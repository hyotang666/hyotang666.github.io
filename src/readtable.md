# Practical case study of common lisp readtable.
## Meta note
### 対象読者
* Common Lispのリードテーブルに興味のある方。

## Introduction.
Common Lispという言語の特徴的な機能の一つにリードテーブルがあります。
リードテーブルを拡張ないしカスタマイズすることにより、Common Lispプログラマはより大胆に言語そのものを拡張できます。

ですがリードテーブルを実践的に拡張するドキュメントはあまり見ないのでここに書き記そうと思います。

本稿ではjsonをリードしCommon Lispオブジェクトを構築するリードテーブルを開発します。

### Disadvantage of readtable.
リードテーブルはストリームから一文字読み込み読み込んだ文字に応じて対応する関数を呼び出すという振る舞いをします。
すなわちストリーム（標準入力）志向であると言えます。
そのため速度面ではどうしても文字列志向のパースと比べて劣ります。

### Advantage of readtable.
上記のストリーム志向であることはそのままリードテーブルの有利な点ともなります。

Common Lispという言語には[`ARRAY-TOTAL-SIZE-LIMIT`](http://www.lispworks.com/documentation/HyperSpec/Body/v_ar_tot.htm)という定数があります。
これは処理系依存の値で、その処理系がアロケートできる配列の最大サイズを示します。
すなわちLisp処理系が作れる文字列には限界があるということです。
ファイルがこのサイズを超える場合、ファイルの文字列を読み込んでからパースするという作戦は取れなくなります。

ストリーム志向であるリードテーブルの場合、これは問題となりません。

## Definitions
本稿でサポートするjsonオブジェクトとLispオブジェクトの対応は以下のようになります。

| json         |  LISP |
| ---          |  ---- |
| null         |  symbol NULL |
| boolean      |  BOOLEAN i.e. T and NIL. |
| object       |  HASH-TABLE |
| array        |  VECTOR |
| number       |  NUMBER |
| string       |  STRING |

## About macro char function.
リードテーブルに登録する関数は第一引数にストリームを、第二引数に対応した文字を受け取るAPI設計になっていなくてはなりません。

## |"-reader|
まずはもっともシンプルなオブジェクトを読み込むリーダを定義しましょう。
ここでは`null`、`boolean`、`string`を読み込む関数を定義します。

```lisp
1 (let ((reader (get-macro-character #\" (copy-readtable nil))))
2   (defun |"-reader| (stream character)
3     (let ((contents (funcall reader stream character)))
4       (cond ((string= "null" contents) ''null)
5             ((string= "true" contents) t)
6             ((string= "false" contents) nil)
7             (t contents)))))
```

処理系規定の`#\"`に紐付けられた関数を取り出し束縛しておきます（１）。
関数定義の外側に束縛を置くこと（クロージャを作ること）により関数呼び出しのたびにリードテーブルへクエリ発行するのを避けられます。

`COPY-READTABLE`はリードテーブルの複製を返す関数ですが、引数が`NIL`の場合処理系の既定リードテーブルを返すという振る舞いをします。

関数の中では、まず束縛しておいた規定READERを使い文字列を読み込みます（３）。

その後読み込んだ文字列の値に対応して任意の値を返します（４）。

`"null"`が来た時の返り値が`''null`となっている点要注目。
リーダはS式を返す関数です。
Lispは`READ`したものを`EVAL`します。
`QUOTE`が一つなら`READ`した結果は`NULL`となり`EVAL`されると（おそらくは）エラーとなります。
通常シンボル`NULL`に値（SYMBOL-VALUE）は設定されていないからです。
ここで生成すべきは`'null`というS式です。

## array reader.
次に配列のリーダを定義します。

### |,-reader|
ですがその前に配列のデリミタを処理するリーダを定義してしまいましょう。

```lisp
1 (defun |,-reader| (stream character)
2   (declare (ignore character))
3   (case (peek-char t stream)
4     ((#\] #\}) (values))
5     ((#\,) nil)
6     (otherwise (read stream t t t))))
```
閉じカッコが来れば値を返さず終了（４）、`#\,`が続くなら`NIL`を（５）、さもなくば再帰的に`READ`を呼びます（６）。

何も難しくはないですね。

### |[-reader|
次に配列を読み込むリーダです。

```lisp
1 (defun |[-reader| (stream character)
2   (declare (ignore character))
3   `(vector ,@(read-delimited-list #\] stream t)))
```

開き括弧である`#\[`は使わないので無視します（２）。

`READ-DELIMITED-LIST`で閉じ括弧までの要素をリストにくくって取り出し`VECTOR`にスプライスして渡します（３）。

上で定義した`|,-reader|`を使っていない点要注目。
`READ-DELIMITED-LIST`は再帰的に`READ`を呼び出します。
その時の動的な`*READTABLE*`に`|,-reader|`が登録されていれば良いのです。

## object reader.
最後にjsonオブジェクトを読み込むリーダを定義します。

### |:-reader|
ですがその前にプロパティと値とを区切るデリミタを処理するリーダを定義してしまいましょう。

```lisp
1 (defun |:-reader| (stream character)
2   (declare (ignore stream character))
3   (values))
```
本デリミタは人間が読みやすくするためだけのシンタックスシュガーでしかないので単に値を返さず終了する関数として定義します（３）。

### |{-reader|
jsonオブジェクトのリーダは少々大きくなります。
ひと処理づつ見ていきましょう。

```lisp
1  (defun |{-reader| (stream character)
2    (declare (ignore character))
3    (let ((contents (read-delimited-list #\} stream t))
4          (package (find-package :keyword))
5          (var (gensym "HASH-TABLE")))
6      `(let ((,var (make-hash-table :test #'eq)))
7         ,@(loop :for (k v) :on contents :by #'cddr
8                 :collect `(setf (gethash ,(intern k package)) ,var) ,v))
9         ,var)))
```

まず`READ-DELIMITED-LIST`でオブジェクトの中身をリストにくくって取り出します（３）。
ここで取り出したリストはプロパティキーと値が交互に現れる属性リストとなります。

配列の時と同様にデリミタを処理する関数が既に`*READTABLE*`にあることを前提としています。

jsonオブジェクトは`HASH-TABLE`で表されます（６）。
jsonの仕様によりプロパティキーが現れる順番に意味がないこと、また、同一のキーが存在してはいけないこととを満たすためです。

属性リストを`LOOP`していき（７）、`HASH-TABLE`にキーバリューペアを登録するS式を生成します（８）。

この時、プロパティキーは`KEYWORD`シンボルに変換しておきます（４）（８）。
`LOOP`の外側で`PACKAGE`を束縛している（４）のは、繰り返しのたびにキーワードパッケージを探すのを避けるためです。

プロパティキーをキーワードシンボルに変換する理由は比較処理を高速に行うためです。
シンボル同士の比較は`EQ`（ポインタイコール）で比較できますが、文字列の比較は`EQUAL`（要素ごとの比較）で比較しなければならないからです。

## number
必要なリーダ関数は以上です。

`number`の読み込みにはCommon Lispの規定リーダを流用します。

## named-readtables.
グローバルな`*READTABLE*`の値を破壊的に変更するのはあまりよい作法ではありません。
影響範囲が広すぎるためです。

リードテーブルを拡張する場合は[`named-readtables`](https://github.com/melisgl/named-readtables)を使用するのが一般的です。

```lisp
1 (named-readtables:defreadtable json
2   (:macro-char #\: '|:-reader| t)
3   (:macro-char #\, '|,-reader|)
4   (:macro-char #\[ '|[-reader|)
5   (:macro-char #\] (get-macro-character #\) (copy-readtable nil)))
6   (:macro-char #\{ '|{-reader|)
7   (:macro-char #\} (get-macro-character #\) (copy-readtable nil)))
8   (:macro-char #\" '|"-reader|))
```
`array`と`object`の閉じカッコには処理系規定のリーダ関数を流用します（５）（７）。

## READ-JSON
簡便のために`READ-JSON`関数を定義しておきましょう。

```lisp
1 (defun read-json (&optional stream errorp return)
2   (let ((*readtable* (named-readtables:find-readtable 'json)))
3     (read stream errorp return)))
```
`*READTABLE*`の値を動的に束縛し（２）、`READ`を呼ぶだけ（３）の簡単な関数です。

## Usage.
`READ-JSON`関数を使うとストリームからjsonコードを読み込み、相当するS式を生成できます。

```lisp
* (read-json)
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
            "Animated" : false,
            "IDs": [116, 943, 234, 38793]
          }
      }
(LET ((#:HASH-TABLE1635 (MAKE-HASH-TABLE :TEST #'EQ)))
  (SETF (GETHASH :|Image| #:HASH-TABLE1635)
          (LET ((#:HASH-TABLE1634 (MAKE-HASH-TABLE :TEST #'EQ)))
            (SETF (GETHASH :|Width| #:HASH-TABLE1634) 800)
            (SETF (GETHASH :|Height| #:HASH-TABLE1634) 600)
            (SETF (GETHASH :|Title| #:HASH-TABLE1634) "View from 15th Floor")
            (SETF (GETHASH :|Thumbnail| #:HASH-TABLE1634)
                    (LET ((#:HASH-TABLE1633 (MAKE-HASH-TABLE :TEST #'EQ)))
                      (SETF (GETHASH :|Url| #:HASH-TABLE1633)
                              "http://www.example.com/image/481989943")
                      (SETF (GETHASH :|Height| #:HASH-TABLE1633) 125)
                      (SETF (GETHASH :|Width| #:HASH-TABLE1633) 100)
                      #:HASH-TABLE1633))
            (SETF (GETHASH :|Animated| #:HASH-TABLE1634) FALSE)
            (SETF (GETHASH :|IDs| #:HASH-TABLE1634) (VECTOR 116 943 234 38793))
            #:HASH-TABLE1634))
  #:HASH-TABLE1635) 
```

現在の実装ではリテラルの`false`はシンボル`FALSE`になってしまう点は要注意。

```lisp
* (read-json)
false
FALSE
```

`JSON.stringify()`でシリアライズされたjsonはちゃんと`null`、`true`、`false`を文字列にしてくれるのでここでは無視します。

同様に、簡便のためデリミタがrequiredになっていない点も要注意。

```lisp
* (read-json)
[1 2 3]
(VECTOR 1 2 3)
```

## Mix with Common Lisp.
上で定義したリードテーブルは純粋にjsonをリードするためだけのテーブルです。
どういう事かというとCommon Lispのコードと共存させることのできないテーブルだということです。

Common Lispコードの中にjsonをリテラルに書きたいという需要に対応するためには少々の変更が必要です。
具体的にはCommon Lispで規定で持っているMACRO-CHARACTERである`#\,`、`#\"`、MACRO-CHARACTERではないもののそのシンタックス上で重要な意味を持つ`#\:`をテーブルから取り除く必要があります。

### Fixed array reader.
上では動的な`*READTABLE*`に`|,-reader|`が設定されていることを前提としていましたが、明示的に使いましょう（４）。
同様に`|"-reader|`も設定します（５）。

```lisp
1 (defun |[-reader| (stream character)
2   (declare (ignore character))
3   (let ((*readtable* (copy-readtable)))
4     (set-macro-character #\, '|,-reader|)
5     (set-macro-character #\" '|"-reader|)
6     `(vector ,@(read-delimited-list #\] stream t))))
```

### Fixed object reader.
同様に`object`のリーダも暗黙の前提を明示的に設定します。

```lisp
1 (defun |{-reader| (stream character)
2   (declare (ignore character))
3   (let ((*readtable* (copy-readtable)))
4     (set-macro-character #\: '|:-reader|)
5     (set-macro-character #\, '|,-reader|)
6     (set-macro-character #\" '|"-reader|)
7     (let ((contents (read-delimited-list #\} stream t))
8           (var (gensym "HASH-TABLE"))
9           (package (find-package :keyword)))
10       `(let ((,var (make-hash-table :test #'eq)))
11         ,@(loop :for (k v) :on contents :by #'cddr
12                 :collect `(setf (gethash ,(intern k package) ,var) ,v))
13         ,var))))
```

### Fixed readtable.
上の修正によりリードテーブルの定義は以下のように変わります。

```lisp
1 (named-readtables:defreadtable json
2   (:macro-char #\[ '|[-reader|)
3   (:macro-char #\] (get-macro-character #\) (copy-readtable nil)))
4   (:macro-char #\{ '|{-reader|)
5   (:macro-char #\} (get-macro-character #\) (copy-readtable nil))))
```

テーブルの定義が変わったため`READ-JSON`の振る舞いも変わってしまいます。
具体的には文字列を読むことができなくなります。

```lisp
* (read-json)
"true"
|"TRUE"|
```

コードのトップレベルに文字列がゴロンと置かれることは皆無といって過言でないのでここではこの変更を受け入れることとします。

### cl-with-json.
上の変更によりjsonテーブルが使うMACRO-CHARACTERは`#\[`のペアと`#\{`のペアのみとなりました。
幸いこの二つはCommon Lisp言語仕様によりユーザのために予約されている文字です。
安心して２つのテーブルを結合させることができます。

Common Lispのコードとjsonとを共存させるためにはそれ用のテーブルを用意する必要があります。

```lisp
1 (named-readtables:defreadtable cl-with-json
2   (:merge :standard json))
```

上記テーブルはCommon Lispのリードテーブルとjsonのリードテーブルとを共存させたテーブルです（２）。

このテーブルを使うには`NAMED-READTABLES:IN-READTABLE`します。

```lisp
* (named-readtables:in-readtable cl-with-json)
#<NAMED-READTABLE CL-WITH-JSON {...}>
```
これによりCommon Lispコードの中にjsonコードをリテラルに書くことが可能となります。

```lisp
* (let ((obj { "key1": "value",
               "key2": [1, 2, 3] }))
    (gethash :key-2 obj))
#(1 2 3)
T
```

## Conclusion
以上、駆け足ですがCommon Lispにおけるリードテーブルのカスタマイズの実践的ケーススタディでした。

見てきた通り大変簡単にカスタマイズ可能となっています。

[Common Lispの中にCのシンタックスを混ぜるという荒業も可能です。](https://github.com/y2q-actionman/with-c-syntax)

Dispatch-macro-characterについては触れられませんでしたが似ようなものです。
興味のある人は[このあたりに目を通すと幸せになれます](http://clhs.lisp.se/Body/c_reader.htm)。
