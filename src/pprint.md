# Practical case study of common lisp pretty printings.
## Metanote
### 対象読者
* Common LispのPRETTY-PRINT機能に興味のある方。

## Introduction.
Common Lispという言語が持つ特殊な機能の一つにPRETTY-PRINTINGSというものがあります。
これは（主に）リストを見目麗しく（pretty）表示するためのものです。
コードがそのままリストでもあるCommon Lispならではの機能と言えるかもしれません。

ですがこの機能はCommon Lisperにとっても馴染みのあるものではありません。
というのも必要になることが滅多にないからです。

ここでは[以前作ったjsonリーダ](readtable.html)の対になるプリンタを作りながらPRETTY-PRINTINGの各種機能を紹介していきます。

## Definitions.

念の為本稿でサポートするjsonオブジェクトとLispオブジェクトの対応を再掲します。

| json         |  LISP |
| ---          |  ---- |
| null         |  symbol NULL |
| boolean      |  BOOLEAN i.e. T and NIL. |
| object       |  HASH-TABLE |
| array        |  VECTOR |
| number       |  NUMBER |
| string       |  STRING |

## Pretty printer functions.
リードテーブルに登録する関数は第一引数にストリームを、第二引数に文字を受け取るAPIでなければなりませんでした。
同様にPRETTY-PRINTINGを行う関数は第一引数にストリームを、第二引数にリスプオブジェクトを受け取るAPIでなくてはなりません。

## null printer.
手始めに最もシンプルなPRETTY-PRINTERを定義しましょう。
シンボル`NULL`を受け取り`"null"`を出力する関数です。

```lisp
(defun jprint-null (stream exp) (write-string "\"null\"" stream) exp)
```

Common Lispの`PRINT`系関数は表示したオブジェクトを返すよう設計されているので、ここではそれにならい第二引数をそのまま返り値とします。

## true and false printer.
同様に`BOOLEAN`を出力する関数を定義しましょう。

```lisp
(defun jprint-true (stream exp) (write-string "\"true\"" stream) exp)

(defun jprint-false (stream exp) (write-string "\"false\"" stream) exp)
```

## keyword printer.
次にプロパティキーを出力する関数を定義します。

```lisp
(defun jprint-keyword (stream exp)
  (write-char #\" stream)
  (write-string (symbol-name exp) stream)
  (write-char #\" stream)
  exp)
```

## array printer.
これまでは原始的なオブジェクトのプリンタだったためPRETTY-PRINTINGが必要ではありませんでした。
`array`のプリンタはそうは行きません。

ひと処理づつ見ていきましょう。

```lisp
1 (defun jprint-vector (stream exp)
2   (pprint-logical-block (stream nil :prefix "[" :suffix "]")
3     (loop :for i :upfrom 0
4           :initially (write-char #\Space stream)
5                      (pprint-indent :block 3 stream)
6                      (pprint-newline :linear stream)
7           :if (array-in-bounds-p exp i)
8             :do (write (aref exp i) :stream stream)
9           :else
10             :do (loop-finish)
11          :if (array-in-bounds-p exp (1+ i))
12            :do (write-char #\, stream)
13                (write-char #\Space stream)
14                (pprint-newline :linear stream)
15          :else
16            :do (write-char #\Space stream)
17                (pprint-indent :block 0 stream)
18                (pprint-newline :linear stream)))
19  exp)
```
まず全体を論理ブロックでくくります（２）。
`LOOP`で`VECTOR`の中身を走査していきます（３）。
走査をする前に前処理を行います（４）（５）（６）。

PRETTY-PRINTING処理の典型的なひと塊の処理は

1. 終了テスト。
2. 空白出力。
3. 必要ならインデント指定。
4. 改行指定。
5. オブジェクトの出力。

という手順です。

本前処理では、空白出力（４）、インデント指定（５）、改行指定（６）を行っています。

終了テストを行わずに空白出力を行っているので、カラ配列の出力は`[ ]`となります。
`[]`の方が良い場合は終了テストを追加することになります。

インデントの指定はここでは論理ブロックの左端から３スペースと指定しています。
プリフィックスは論理ブロックに入らない点要注意。
本指定で改行が起きると以下のようになります。

```json
[
    hoge ...
```

改行指定では`:LINEAR`を指定しています。
これは通常改行は行わないが、一度論理ブロック内で改行が行われればそれを引き金として改行が起きることを意味します。

上のjsonコードがそうなっているように、改行が起きる場合開きカッコと同じ行にはなりません。

なお、`PPRINT-NEWLINE`による改行は改行直前の空白文字を取り除いてくれます。
手続き上では空白文字を出力していますが上記jsonコード例の開きカッコ右側には空白文字は残りません。

さて、`LOOP`の本処理ですが、まずは終了テストが行われます（７）。
インデックスが配列の内側であれば要素が出力されます（８）。
さもなくば（９）`LOOP`を抜けます（１０）。

次に次要素での終了テストを行います（１１）。
これは配列の最後にコンマを出力させないための処理です。
効率を優先するならこの処理はまるっとなくしてもいいです。
ただしその場合出力は例えば`[ 1, 2, 3, ]`のように末尾にカンマが残る形となります。

次要素も妥当なインデックスなら、カンマを出力し（１２）、空白文字を出力し（１３）、改行を指定します（１４）。
さもなくば（１５）空白文字を出力し（１６）、インデントを指定し（１７）、改行を指定します（１８）。

さて、うまく機能するでしょうか。

```lisp
* (jprint-vector nil #(1 2 3))
[ 1, 2, 3 ]     ; <--- Side effect.
#(1 2 3)        ; <--- Return value.
```

うまく機能しているように見えますが。

```lisp
* (jprint-vector nil #("foo" "bar" "bazz" "hoge" "fuga" "piyo" "asdf" "uiop" "true" "false" "null"))
[
    "foo",
    "bar",
    "bazz",
    "hoge",
    "fuga",
    "piyo",
    "asdf",
    "uiop",
    "true",
    "false",
    "null",
 ]
#("foo" "bar" "bazz" "hoge" "fuga" "piyo" "asdf" "uiop" "true" "false" "null")
```

おっと。閉じカッコに注目です。空白一つ分インデントされてますね。
これは論理ブロックがプリフィックスをブロックに含まないからです。
jsonの流儀に従うために、`PPRINT-LOGICAL-BLOCK`のAPIを使わずに（２）自前で出力するように（３）（２０）修正しましょう。

```lisp
1 (defun jprint-vector (stream exp)
2   (pprint-logical-block (stream nil)
3     (write-char #\[ stream)
4     (loop :for i :upfrom 0
5           :initially (write-char #\Space stream)
6                      (pprint-indent :block 3 stream)
7                      (pprint-newline :linear stream)
8           :if (array-in-bounds-p exp i)
9             :do (write (aref exp i) :stream stream)
10          :else
11             :do (loop-finish)
12          :if (array-in-bounds-p exp (1+ i))
13            :do (write-char #\, stream)
14                (write-char #\Space stream)
15                (pprint-newline :linear stream)
16          :else
17            :do (write-char #\Space stream)
18                (pprint-indent :block 0 stream)
19                (pprint-newline :linear stream))
20    (write-char #\] stream))
21  exp)
```
これでうまくいくはずです。

```lisp
* (jprint-vector nil #("foo" "bar" "bazz" "hoge" "fuga" "piyo" "asdf" "uiop" "true" "false" "null"))
[
    "foo",
    "bar",
    "bazz",
    "hoge",
    "fuga",
    "piyo",
    "asdf",
    "uiop",
    "true",
    "false",
    "null",
]
#("foo" "bar" "bazz" "hoge" "fuga" "piyo" "asdf" "uiop" "true" "false" "null")
```

期待通り閉じカッコ前の空白がなくなりましたね。

このままでもいいのですが少々コードが長いので、`FORMATTER`を使って短くしましょう。

```lisp
(defun jprint-vector (stream exp)
  (pprint-logical-block (stream nil)
    (write-char #\[ stream)
    (loop :for i :upfrom 0
          :initially (funcall (formatter " ~3I~_") stream)
          :if (array-in-bounds-p exp i)
            :do (write (aref exp i) :stream stream)
          :else
            :do (loop-finish)
          :if (array-in-bounds-p exp (1+ i))
            :do (funcall (formatter ", ~_") stream)
          :else
            :do (funcall (formatter " ~0I~_") stream))
    (write-char #\] stream))
  exp)
```
## object printer.
それでは最後に`object`プリンタの定義です。

```lisp
1 (defun jprint-object (stream exp)
2   (pprint-logical-block (stream nil)
3     (write-char #\{ stream)
4     (with-hash-table-iterator (get-it exp)
5       (labels ((rec (count)
6                  (case count
7                    (0)
8                    (1
9                     (multiple-value-call #'put (get-it))
10                     (funcall (formatter " ~0I~_") stream))
11                   (otherwise
12                    (multiple-value-call #'put (get-it))
13                    (funcall (formatter ", ~:@_") stream)
14                    (rec (1- count)))))
15               (put (found? key v)
16                 (declare (ignore found?))
17                 (funcall (formatter "~W: ~W") stream key v)))
18        (funcall (formatter " ~3I~_") stream)
19        (rec (hash-table-count exp))))
20    (write-char #\} stream))
21  exp)
```
`array`プリンタと同様に`PPRINT-LOGICAL-BLOCK`で論理ブロックを決め（２）最初（３）と最後（２０）にカッコを出力します。
本体では`LOOP`に変わり`WITH-HASH-TABLE-ITERATOR`と`LABELS`との組み合わせ（４）（５）で繰り返しを行います。

繰り返しは`HASH-TABLE-COUNT`に対し行われ（１９）、最後の一要素になるまではカンマを出力します（１３）。

## print-json
簡便のために`PRINT-JSON`関数を定義しましょう。

### `*PRINT-JPRINT-DISPATCH*`
まずはjsonをプリントするためのPPRINT-DISPATCH-TABLEを作成しましょう。

```lisp
(defparameter *print-jprint-dispatch*
  (let ((*print-pprint-dispatch* (copy-pprint-dispatch nil)))
    (set-pprint-dispatch '(eql null) 'jprint-null)
    (set-pprint-dispatch '(eql t) 'jprint-true)
    (set-pprint-dispatch '(eql nil) 'jprint-false)
    (set-pprint-dispatch 'keyword 'jprint-keyword)
    (set-pprint-dispatch '(and vector (not string)) 'jprint-vector)
    (set-pprint-dispatch 'hash-table 'jprint-object)
    *print-pprint-dispatch*))
```

### PRINT-JSON
`PRINT-JSON`は`*PRINT-PPRINT-DISPATCH*`を動的に束縛して`WRITE`を呼び出すだけの簡単な関数です。

```lisp
(defun print-json (exp &optional stream)
  (let ((*print-pprint-dispatch* *print-jprint-dispatch*))
    (write exp :stream stream))
  exp)
```

## Behavior.
それでは振る舞いを見てみましょう。

```lisp
(print-json (eval (read-json)))
{
    "Image": {
        "Width":  800,
        "Height": 600,
        "Title":  "View from 15th Floor",
        "Thumbnail": {
            "Url":    "http://www.example.com/image/481989943",
            "Height": 125,
            "Width":  100
        },
        "Animated" : "null",
        "IDs": [116, 943, 234, 38793]
    }
}
{
   "Image": {
               "Width": 800,
               "Height": 600,
               "Title": "View from 15th Floor",
               "Thumbnail": {
                               "Url": "http://www.example.com/image/481989943",
                               "Height": 125,
                               "Width": 100
                            },
               "Animated": "null",
               "IDs": [ 116, 943, 234, 38793 ]
            }
}
#<HASH-TABLE :TEST EQ :COUNT 1 {...}>
```
おっと。インデントが崩れています。
このインデントを崩れているというのは大変心苦しい。
僕自身はこのインデントの方がカッコのスコープが見やすくて良いと思うのですが、残念ながらjson流儀のスタイルはコレとは異なります。

さて、ここでの大きな問題は、論理ブロックより左にインデントすることは不可能であるという点です。

では論理ブロックに変わるインデントブロックを表すために変数を導入しましょう。

```lisp
(defparameter *indent* 4)

(defparameter *nest* 0)
```

まずトップレベル関数である`PRINT-JSON`を、変数を使うように修正します。

```lisp
(defun print-json (exp &optional stream)
  (let ((*print-pprint-dispatch* *print-jprint-dispatch*) (*nest* *nest*))
    (write exp :stream stream))
  exp)
```
次に`JPRINT-OBJECT`から論理ブロックを取り除いて変数に変更します。

```lisp
(defun jprint-object (stream exp)
  (write-char #\{ stream)
  (let ((*nest* (1+ *nest*)))
    (with-hash-table-iterator (get-it exp)
      (labels ((rec (count)
                 (case count
                   (0)
                   (1
                    (multiple-value-call #'put (get-it))
                    (funcall (formatter " ~VI~_") stream
                             (* *indent* (1- *nest*))))
                   (otherwise
                    (multiple-value-call #'put (get-it))
                    (funcall (formatter ", ~:@_") stream)
                    (rec (1- count)))))
               (put (found? key v)
                 (declare (ignore found?))
                 (funcall (formatter "~W: ~W") stream key v)))
        (funcall (formatter " ~VI~_") stream (* *nest* *indent*))
        (rec (hash-table-count exp)))))
  (write-char #\} stream)
  exp)
```
同様に`JPRINT-VECTOR`からも論理ブロックを取り除いて変数に変更します。

```lisp
(defun jprint-vector (stream exp)
  (write-char #\[ stream)
  (let ((*nest* (1+ *nest*)))
    (loop :for i :upfrom 0
          :initially (funcall (formatter " ~VI~_") stream (* *indent* *nest*))
          :if (array-in-bounds-p exp i)
            :do (write (aref exp i) :stream stream)
          :else
            :do (loop-finish)
          :if (array-in-bounds-p exp (1+ i))
            :do (funcall (formatter ", ~_") stream)
          :else
            :do (funcall (formatter " ~VI~_") stream
                         (* *indent* (1- *nest*)))))
  (write-char #\] stream)
  exp)
```
では振る舞いを見てみましょう。

```lisp
(print-json (eval (read-json)))
{
    "Image": {
        "Width":  800,
        "Height": 600,
        "Title":  "View from 15th Floor",
        "Thumbnail": {
            "Url":    "http://www.example.com/image/481989943",
            "Height": 125,
            "Width":  100
        },
        "Animated" : "null",
        "IDs": [116, 943, 234, 38793]
    }
}
{
    "Image": {
        "Width": 800,
        "Height": 600,
        "Title": "View from 15th Floor",
        "Thumbnail": {
            "Url": "http://www.example.com/image/481989943",
            "Height": 125,
            "Width": 100
        },
        "Animated": "null",
        "IDs": [
            116,
            943,
            234,
            38793
        ]
    }
}
#<HASH-TABLE :TEST EQ :COUNT 1 {...}>
```
おっと。配列のインデントがおかしいですね。
これは論理ブロックが一つしかないからです。
`JPRINT-OBJECT`に論理ブロックを追加しましょう（１８）。

```lisp
1 (defun jprint-object (stream exp)
2   (write-char #\{ stream)
3   (let ((*nest* (1+ *nest*)))
4     (with-hash-table-iterator (get-it exp)
5       (labels ((rec (count)
6                  (case count
7                    (0)
8                    (1
9                     (multiple-value-call #'put (get-it))
10                     (funcall (formatter " ~VI~_") stream
11                             (* *indent* (1- *nest*))))
12                   (otherwise
13                    (multiple-value-call #'put (get-it))
14                    (funcall (formatter ", ~:@_") stream)
15                    (rec (1- count)))))
16               (put (found? key v)
17                 (declare (ignore found?))
18                 (pprint-logical-block (stream nil)
19                   (let ((*nest* 0))
10                     (funcall (formatter "~W: ~W") stream key v)))))
21        (funcall (formatter " ~VI~_") stream (* *nest* *indent*))
22        (rec (hash-table-count exp)))))
23  (write-char #\} stream)
24  exp)
```
うまく動くでしょうか。

```lisp
(print-json (eval (read-json)))
{
    "Image": {
        "Width":  800,
        "Height": 600,
        "Title":  "View from 15th Floor",
        "Thumbnail": {
            "Url":    "http://www.example.com/image/481989943",
            "Height": 125,
            "Width":  100
        },
        "Animated" : "null",
        "IDs": [116, 943, 234, 38793]
    }
}
{
    "Image": {
        "Width": 800,
        "Height": 600,
        "Title": "View from 15th Floor",
        "Thumbnail": {
            "Url": "http://www.example.com/image/481989943",
            "Height": 125,
            "Width": 100
        },
        "Animated": "null",
        "IDs": [ 116, 943, 234, 38793 ]
    }
}
#<HASH-TABLE :TEST EQ :COUNT 1 {...}>
```

うまく動いているようです。

## Conclusion.
以上、駆け足ですがPRETTY-PRINTINGSについて見てきました。
結局ここで作ったのはjsonのCode-formatterに相当するものです。

それが７０行そこそこで完成しました。

PRETTY-PRINTINGSが必要になるケースは稀ですが大変強力なものでもあります。
興味がある人は[CLHSのこのあたりを読んで幸せになりましょう](http://clhs.lisp.se/Body/22_bb.htm)。
