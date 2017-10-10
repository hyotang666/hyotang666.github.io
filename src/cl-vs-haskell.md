# Common Lisp vs Haskell, chapter 1
## Meta note
### 対象読者
初級〜中級CLer。
ないしCommon Lispに興味のあるHaskeller（いなさそうだなぁ）。

長文なのでお暇な時にどうぞ。

## Introduction
論文を呼んでいると例題コードがHaskellで書かれているものに出会うことがままある。
筆者はCommon Lispにこそ精通している者であるが、Haskellについては無知蒙昧の徒である。
筆者は論文内の例題コードを、「色々難しいことをごちゃごちゃ言ったけど、実装するならこんな感じだよ」という、本文を理解するための補助輪のような存在と認識している。
その補助輪がチンプンカンプンであれば、内容の理解は一層難しくなる。
そこで論文を読めるようにするためにHaskellを読めるようにすることにした。

しかしながら大きな問題がある。
筆者はHaskellという言語をリスペクトしている。
Common Lispを書いていて不満に思う幾つかをHaskellは綺麗（？）に解決しているように見えるからだ。
とはいえ現時点で筆者はHaskellerになる気は毛頭ない。
筆者がHaskellを学ぶのは上記の通り、あくまで手段として、だ。
当然モチベーションは上がらない。
けして、やりたいことがすなわちHaskellというわけではないからだ。
入門書を読んでいても内容がさっぱり頭に入らない。
これではいけない。
そこでHaskellの各種機能を慣れ親しんだCommon Lispに置き換えながら学習することにした。

通常言語Xを学ぶ際に、言語Yに翻訳しながら解釈していくのは悪手である。
言語Xには言語Xの作法があり、言語Yのそれとは相容れない可能性があり、言語Yの知識が言語Xを学ぶことの足を引っ張ることになるのはよくあることである。
しかしながら今回に限りそれは当てはまらない。
筆者はけしてHaskellerになりたくて学び始めたわけではないからだ。
Ruby開発者のMatzがRubyプログラマでないのと同様に、いわばCommon Lisp上に埋め込みHaskellを作る気持ちで、すなわちHaskellという言語を学ぶ対象でなく作る対象と認識することで学習意欲を維持しようという作戦なわけだ。

さて、彼我の言語を見比べると、当然そこには差異がある。
そこで、HaskellになくCommon Lispに特徴的な機能については、Common Lispに無知蒙昧なHaskellerに解説するつもりで説明を試みた。
本稿の該当部分は初心者CLerにとってCommon Lisp学習の一助となろう。

また逆に、Common LispになくHaskellに特徴的な機能については、ライブラリを使って再現できそうなものは積極的にライブラリを使用し、ライブラリも見つからないような機能の中で簡単に実装できそうなものは自前で実装を試みた。
本稿の該当部分は中級CLerにとって興味深い内容となろう。
なお、本章に於けるハイライトは「中置演算を導入する（infix-math）」「リスト内包表記を導入する（incf-cl）」「無限リストを扱う（SERIES）」といったあたりだろうか。

Haskellerにとって本稿は、Common Lispの強力さの一端をうかがい知れる内容となっていようし、また、Haskellの強力さを再認識できる内容ともなっていよう。

上級CLerにとって得られるものは何もないかと思われるので、よほど暇でない限りはご自身のお仕事に戻って、どうぞ。

なお、今回採用したHaskell入門書はいわゆる「すごいH本」である。
本稿は「すごいH本」の内容をCommon Lispに翻訳しながらCLerがHaskellを学ぶその第一章である。

# 1
## 0
まずは簡単な四則演算から。

```haskell
ghci> 2 + 15
17
ghci> 49 * 100
4900
ghci> 1892 - 1472
420
ghci> 5 / 2
2.5
```

```lisp
cl-user> (+ 2 15)
17
cl-user> (* 49 100)
4900
cl-user> (- 1892 1472)
420
cl-user> (/ 5 2)
5/2
cl-user> (float(/ 5 2))
2.5
```
Common Lispはオペレータが前置になっているという点以外はほとんど同じ。
唯一特徴的なのは`/`が分数を返すという点。
実数が欲しい場合は`FLOAT`に渡す必要がある。

Common Lispはオペレータを前置するシンタックスをサポートしているので、これら四則演算関数が二引数関数とは限らない。


```lisp
cl-user> (+)
0
cl-user> (-)
;; ERROR
cl-user> (*)
1
cl-user> (/)
;; ERROR
cl-user> (+ 1)
1
cl-user> (- 1)
-1
cl-user> (* 3)
3
cl-user> (/ 3)
1/3
cl-user> (+ 1 2 3)
6
cl-user> (- 1 2 3)
-4
cl-user> (* 1 2 3)
6
cl-user> (/ 1 2 3)
1/6
```

括弧による優先順位の明示。

```haskell
ghci> (50 * 100) - 4999
1
ghci> 50 * 100 - 4999
1
ghci> 50 * (100 - 4999)
-244950
```

```lisp
cl-user> (- (* 50 100) 4999)
1
cl-user> (* 50 (- 100 4999))
-244950
```
Common Lispは括弧が必須。
おかげで暗黙裏の優先順位を覚える必要がない。
これはトレードオフであり、好みの問題だ。
たとえ優先順位を覚える必要にかられようとも括弧を書きたくない人もおられよう。

なお、Common Lispではそのような構文を導入することもできる。

```lisp
cl-user> (infix-math:$ 50 * 100 - 4999)
1
cl-user> (infix-math:$ 50 * (100 - 4999))
-249950
```

ブール代数。

```haskell
ghci> True && False
False
ghci> True && True
True
ghci> False || True
True
ghci> not False
True
ghci> not (True && True)
False
```

```lisp
cl-user> (and t nil)
NIL
cl-user> (and t t)
T
cl-user> (or nil t)
T
cl-user> (not nil)
T
cl-user> (not(and t t))
NIL
```

Common Lispは汎ブール（Generalized Boolean）をサポートしている。
すなわち、`NIL`以外は全てTrueとなる。

```lisp
cl-user> (not 1)
NIL
```

Common Lispでは前置構文がサポートされているので、ブール代数のオペレータも２引数オペレータではない。

```
cl-user> (and)
T
cl-user> (or)
NIL
cl-user> (and t nil t)
NIL
```

Common Lispは汎ブールがサポートされているので、返り値は`BOOLEAN`ではなく値となる。
すなわち、`AND`なら最後に評価した節の返り値が、`OR`なら最初にtrueに評価された節の返り値が式全体の返り値となる。

```
cl-user> (or nil 1 2)
1
cl-user> (and 1 2 3)
3
```

等号

```haskell
ghci> 5 == 5
True
ghci> 1 == 0
False
ghci> 5 /= 5
False
ghci> 5 /= 4
True
ghci> "hello" == "hello"
True
ghci> 5 + "llama"
-- ERROR
```

多くの言語で`=`は代入のための予約語であるため、等値比較には`==`を使うこととなる。
Common Lispは代入のためのキーワードを持たないので等値比較に`=`を使う。

```lisp
cl-user> (= 1 2)
NIL
cl-user> (= 1 1)
T
```

ただし`=`は数値比較のためのものであって、引数に数値以外が来るとエラーとなる。

```lisp
cl-user> (= 1 nil)
;; ERROR.
```

そのような場合には`EQ`のファミリーを使う。

```lisp
cl-user> (eql 1 nil)
NIL
```

`EQ`ファミリーの特徴は以下の通り。

* EQ

シンボル同士の比較に用いる。
内部的にはポインタ比較なので高速に機能する。

* EQL

シンボル、文字、整数の比較に用いる。

* EQUAL

リスト、パス名、ケースセンシティブな文字列比較に用いる。

* EQUALP

ケースインセンシティブな文字列比較、ハッシュテーブル、構造体、配列の比較に用いる。

文字の比較、文字列比較のための専用オペレータもある。
気をつけなければならないのは`STRING`のファミリーで、これは文字列ではなく、文字列指定子（STRING-DESIGNATOR）を受け付ける。
すなわち、文字でもシンボルでも機能する。

```lisp
;; For character. Not generalized.
cl-user> (char= #\a #\a)
T
cl-user> (char= #\a #\A)
NIL
cl-user> (char-equal #\a #\A)
T
cl-user> (char= #\a nil)
;; ERROR

;; For string-designator. Not generalized.
cl-user> (string= "hoge" "hoge")
T
cl-user> (string= "hoge" "HOGE")
NIL
cl-user> (string-equal "hoge" "HOGE")
T
cl-user> (string= #\a "a")
T
cl-user> (string= 'hoge "HOGE")
T
cl-user> (string= "0" 0)
;; ERROR
```

Common Lispの等値関数群は初心者にとって難解で悪名高いのだが、それに対する反論としてはSICPとCLtL2からの引用で事足りえよう。

> 参照透明性を一旦捨てると、計算オブジェクトが「同じ」であるとは何を意味するかの概念を形式的に捕らえるのは難しくなる。実はわれわれのプログラムがモデル化している実世界の「同じ」の意味もあまりはっきりしない。一般に二つの見かけ上同じなオブジェクトが本当に「同じもの」であるかは、一つのオブジェクトを変えてみて、もう一つのオブジェクトが同じように変わっているかを見て決める。しかし「同じ」オブジェクトを二度観測し、オブジェクトのある性質が一回目と二回目の観測で違っているということ以外に、あるオブジェクトが「変った」ことがどうして分るだろうか。つまり何か*先験的な*「同一」という概念なしに「変化」を決めることは出来ず、変化の効果を観測することなしに同一性を決めることは出来ない。


> オブジェクトの等値性は、唯一定められた正しいアルゴリズムに基づいた概念ではない。等値の述語のもっともらしさは、ある特定のプログラムの文脈においてのみ判断できるものである。EQUALとEQUALP関数はいかなる型の引数も受け付け、名前も非常に汎用に聞こえるが、これらはすべてのアプリケーションに対してもっともらしいわけではない。これらの関数を使用するか否かという決定は、これらの関数の抽象的な性質よりも動作を規定したドキュメントによって成されるべきである。もし、ある場面でEQUALもEQUALPも不適切であると判明した場合には、プログラマはEQUALやEQUALPが「誤って動作する」と非難するのではなく、他の適切な操作関数を作り出すことが望まれる。

## 1.1
関数呼びだし。

```haskell
ghci> succ 8
9
```

```lisp
cl-user> (1+ 8)
9
```

```haskell
ghci> min 9 10
9
ghci> min 3.4 3.2
3.2
ghci> max 100 101
101
```

```lisp
cl-user> (min 9 10)
9
cl-user> (min 3.2 3.4)
3.2
cl-user> (max 100 101)
101
```
Common Lispは前置構文をサポートしているので、`MAX`、`MIN`関数もまた２引数関数ではない。

```lisp
cl-user> (min 1 2 3)
1
cl-user> (max 1 2 3)
3
cl-user> (min 3)
3
cl-user> (max 3)
3
```

括弧による優先順位の明示

```haskell
ghci> succ 9 + max 5 4 + 1
16
ghci> (succ 9) + (max 5 4) + 1
16
```

```lisp
cl-user> (+ (1+ 9) (max 5 4) 1)
16
cl-user> (infix-math:$ 1 + 9 + (5 max 4) + 1)
16
```

```haskell
ghci> succ 9 * 10
100
ghci> succ (9 * 10)
91
```

Common Lispでは括弧は必須なので、曖昧性がない。
これはトレードオフである。

```lisp
cl-user> (* (1+ 9) 10)
100
cl-user> (1+ (* 9 10))
91
```

```haskell
ghci> div 92 10
9
ghci> 92 `div` 10
9
```

Common Lispで上記の`DIV`に相当する関数は全部で４つある。
各々あまりの丸め込み方が異なる。

```lisp
;; Truncated toward negative infinity.
cl-user> (floor 92 10)
9
2
;; Truncated toward positive infinity.
cl-user> (ceiling 92 10)
10
-8
;; Truncated toward zero.
cl-user> (truncate -1 2)
0
-1
;; Rounded to the nearest mathematical integer.
;; If the mathematical quotient is exactly halfway between two integers,
;; (that is, it has the form integer+1/2),
;; then the quotient has been rounded to the even (divisible by two) integer.
cl-user> (round 5 3)
2
-1
```

```haskell
ghci> 92 `div` 10
9
```
Haskellでは簡単に関数を中置に出来るが、Common Lispにそのような機能はない。
数学系のものに限るなら、infix-mathが便利に使える。
また、infix-mathは拡張もできるので、自身で拡張さえすればどのような関数も中置に出来るとは言える。
もっとも「お手軽」とは言えないが。

```lisp
cl-user> (infix-math:$ 92 floor 10)
9
2
```

## 1.2

```haskell
doubleMe x = x + x
```

```lisp
(defun double-me (x)
  (+ x x))
```

```haskell
ghci> :l baby
ghci> doubleMe 9
18
ghci> doubleMe 8.3
16.6
```

```lisp
cl-user> (load "baby")
cl-user> (double-me 9)
18
cl-user> (double-me 8.3)
16.6
```

```haskell
doubleUs x y = x * 2 + y * 2
```

```lisp
(defun double-us (x y)
  (+ (* x 2)
     (* y 2)))
```

```haskell
ghci> doubleUs 4 9
26
ghci> doubleUs 2.3 34.2
73.0
ghci> doubleUs 28 88 + doubleMe 123
478
```

```lisp
cl-user> (double-us 4 9)
26
cl-user> (double-us 2.3 34.2)
73.0
cl-user> (+ (double-us 28 88) (double-me 123))
478
```

```haskell
doubleSmallNumber x = if x > 100
                        then x
                        else x*2
```

```lisp
(defun double-small-number (x)
  (if(> x 100)
    x
    (* x 2)))
```

```haskell
doubleSmallNumber' x = (if x > 100 then x else x*2) + 1
```

```lisp
(defun double-small-number% (x)
  (1+ (if(> x 100)
        x
        (* x 2))))
```

```haskell
conanO'Brien = "It's a-me, Conan O'Brien!"
```

```lisp
(defun |Connan O'Brien|()
  (progn "It's a-me, Conan O'Brien!"))
;; or below.
(setf (symbol-function '|Connan O'Brien|)
      (constantly "It's a-me, Conan O'Brien!"))
```
リテラル文字列を返す関数を書く時には注意が必要。
というのも、ドキュメンテーション文字列と解釈されうるからで、処理系依存でエラーになる場合がある。
よって上記の例では文字列を`PROGN`でラップしてBODYであることを明示している。

## 1.3

```haskell
ghci> let lostNumbers = [4,8,15,16,23,42]
ghci> lostNumbers
[4,8,15,16,23,42]
```

```lisp
cl-user> (defvar *lost-numbers* '(4 8 15 16 23 42))
*LOST-NUMBERS*
cl-user> *lost-numbers*
(4 8 14 15 23 42)
```
上記のようにスペシャル変数名の左右に`*`を付けるのはマナーであってルールではない。
別に`*`が無くても問題はない。

```haskell
ghci> [1,2,3,4] ++ [9,10,11,12]
[1,2,3,4,9,10,11,12]
ghci> "hello" ++ " " ++ "world"
"hello world"
ghci> ['w','o'] ++ ['o','t']
"woot"
```

Haskellに於いて文字列はリストだが、Common Lispに於いて文字列はベクタである。
よってリスト用関数は文字列に適用できない。
ただしリスト、文字列、ベクタをひっくるめて扱うシーケンスという型があり、それ用のオペレータも充実している。
上記`++`に相当するのはCommon Lispでなら`CONCATENATE`である。
第一引数に返り値の型を明示する必要がある。

```lisp
cl-user> (concatenate 'list '(1 2 3 4) '(9 10 11 12))
(1 2 3 4 9 10 11 12)
cl-user> (concatenate 'string "hello" " " "world")
"hello world"
cl-user> (concatenate 'string "wo" '(#\o #\t))
"woot"
cl-user> (concatenate 'list "foo" "bar")
(#\f #\o #\o #\b #\a #\r)
```

リスト専用のものとしては`APPEND`がある。

```lisp
cl-user> (append '(1 2 3 4) '(9 10 11 12))
(1 2 3 4 9 10 11 12)
```

```haskell
ghci> 'A':" SMALL CAT"
"A SMALL CAT"
ghci> 5:[1,2,3,4,5]
[5,1,2,3,4,5]
```
Common Lispに於いて文字列はアトムであってリストではない。

```lisp
cl-user> (cons #\A " SMALL CAT")
(#\A . " SMALL CAT")
```

Common Lispに於いて文字（CHARACTER）は列（SEQUENCE）ではない。
よって文字と文字列の連結は`CONCATENATE`ではできない。

```lisp
cl-user> (concatenate 'string #\A " SMALL CAT")
;; ERROR.
```
`UIOP:STRCAT`を使うのがよい。


```lisp
cl-user> (uiop:strcat #\A " SMALL CAT")
"A SMALL CAT"
```

`CONS`はリストをコンストラクトするためのオペレータである。

```lisp
cl-user> (cons 5 '(1 2 3 4 5))
(5 1 2 3 4 5)
```

```haskell
ghci> "Steve Buscemi" !! 6
'B'
ghci> [9.4,33.2,96.2,11.2,23.25] !! 1
33.2
```
Haskellの`!!`はCommon Lispの`ELT`に相当する。

```lisp
cl-user> (elt "Steve Buscemi" 6)
#\B
cl-user> (elt '(9.4 33.2 96.2 11.2 23.25) 1)
33.2
```
`ELT`は型によるディスパッチを必要とするのでその分遅い。
より具体的な型に特定化されたアクセサがあるので、必要ならそれらを使う。

```lisp
;; String spcific.
cl-user> (char "Steve Buscemi" 6)
#\B
;; Simple string specific.
cl-user> (schar "Steve Buscemi" 6)
#\B
;; Array specific.
cl-user> (aref #(5 4 3) 0)
5
;; Simple vector specific.
cl-user> (svref #(5 4 3) 0)
5
;; List specific.
cl-user> (nth 0 '(1 2 3))
1
```

```haskell
ghci> let b = [[1,2,3,4],[5,3,3,3],[1,2,2,3,4],[1,2,3]]
ghci> b
[[1,2,3,4],[5,3,3,3],[1,2,2,3,4],[1,2,3]]
ghci> b ++ [[1,1,1,1]]
[[1,2,3,4],[5,3,3,3],[1,2,2,3,4],[1,2,3],[1,1,1,1]]
ghci> [6,6,6]:b
[[6,6,6][1,2,3,4],[5,3,3,3],[1,2,2,3,4],[1,2,3]]
ghci> b !! 2
[1,2,2,3,4]
```

```lisp
cl-user> (defvar *b* '((1 2 3 4)(5 3 3 3)(1 2 2 3 4)(1 2 3)))
*B*
cl-user> *b*
((1 2 3 4)(5 3 3 3)(1 2 2 3 4)(1 2 3))
cl-user> (append *b* '((1 1 1 1)))
((1 2 3 4)(5 3 3 3)(1 2 2 3 4)(1 2 3)(1 1 1 1))
cl-user> (cons '(6 6 6) *b*)
((6 6 6)(1 2 3 4)(5 3 3 3)(1 2 2 3 4)(1 2 3))
cl-user> (nth 2 *b*)
(1 2 2 3 4)
```

```haskell
ghci> [3,2,1] > [2,1,0]
True
ghci> [3,2,1] > [2,10,100]
True
ghci> [3,4,2] < [3,4,3]
True
ghci> [3,4,2] > [2,4]
True
ghci> [3,4,2] == [3,4,2]
True
```

Common Lispにリストの比較関数は存在しない。
上記haskellコードと同様のセマンティクスは`SOME`を使うことで再現出来る。

```lisp
cl-user> (some #'> '(3 2 1)'(2 1 0))
T
cl-user> (some #'> '(3 2 1) '(2 10 100))
T
cl-user> (some #'< '(3 4 2) '(3 4 3))
T
cl-user> (some #'> '(3 4 2) '(2 4))
T
cl-user> (equal '(3 4 2) '(3 4 2))
T
```

```haskell
ghci> head [5,4,3,2,1]
5
ghci> tail [5,4,3,2,1]
[4,3,2,1]
ghci> last [5,4,3,2,1]
1
ghci> init [5,4,3,2,1]
[5,4,3,2]
```

```lisp
cl-user> (car '(5 4 3 2 1))
5
cl-user> (cdr '(5 4 3 2 1))
(4 3 2 1)
cl-user> (first '(5 4 3 2 1))
5
cl-user> (rest '(5 4 3 2 1))
(4 3 2 1)
cl-user> (last '(5 4 3 2 1))
(1)
```
Common Lispの`LAST`は最後の*コンス*を返すという仕様である。
要素が欲しいなら`alexandria:lastcar`を使う。

```lisp
cl-user> (alexandria:lastcar '(5 4 3 2 1))
1
```
`LASTCAR`は名前が示す通りリスト専用である。
シーケンスに使いたいなら`alexandria:last-elt`が使える。

```lisp
cl-user> (alexandria:last-elt "foobar")
#/r
```

Common Lispに於いて`init`に相当するのは`BUTLAST`である。

```lisp
cl-user> (butlast '(5 4 3 2 1))
(5 4 3 2)
```
なお、`LAST`と`BUTLAST`はオプショナルな引数を受け付ける。

```lisp
cl-user> (butlast '(5 4 3 2 1) 2)
(5 4 3)
cl-user> (last '(5 4 3 2 1) 2)
(2 1)
```

```haskell
ghci> head []
*** ERROR
```

```lisp
cl-user> (elt () 0)
;; ERROR
cl-user> (car ())
NIL
cl-user> (cdr ())
NIL
cl-user> (last())
NIL
cl-suer> (butlast ())
NIL
```

```haskell
ghci> length [5,4,3,2,1]
5
```

```lisp
cl-user> (length '(5 4 3 2 1))
5
```

```haskell
ghci> null [1,2,3]
False
ghci> null []
True
```

```lisp
cl-user> (null '(1 2 3))
NIL
cl-user> (null ())
T
```

```haskell
ghci> reverse [5,4,3,2,1]
[1,2,3,4,5]
```

```lisp
cl-user> (reverse '(5 4 3 2 1))
(1 2 3 4 5)
```

```haskell
ghci> take 3 [5,4,3,2,1]
[5,4,3]
ghci> take 1 [3,9,3]
[3]
ghci> take 5 [1,2]
[1,2]
ghci> take 0 [6,6,6]
[]
```

Haskellに於ける`take`はCommon Lispに於いて`SUBSEQ`が相当する。
ただし、いささか勝手が異なる。

```lisp
cl-user> (subseq '(5 4 3 2 1) 0 3)
(5 4 3)
cl-user> (subseq '(3 9 3) 0 1)
(3)
cl-user> (subseq '(1 2) 0 5)
;; ERROR
cl-user> (loop :for elt :in '(1 2)
               :repeat 5
               :collect elt)
(1 2)
cl-user> (subseq '(6 6 6) 0 0)
NIL
```
完全に同じセマンティクスのものが欲しいなら、SERAPEUMの`TAKE`が使える。
incf-clにも`TAKE`はあるが、リスト専用である。

```lisp
cl-user> (serapeum:take 3 '(5 4 3 2 1))
(5 4 3)
cl-user> (serapeum:take 1 '(3 9 3))
(3)
cl-user> (serapeum:take 5 '(1 2))
(1 2)
cl-user> (serapeum:take 0 '(6 6 6))
NIL
```

```haskell
ghci> drop 3 [8,4,2,1,5,6]
[1,5,6]
ghci> drop 0 [1,2,3,4]
[1,2,3,4]
ghci> drop 100 [1,2,3,4]
[]
```
Haskellの`drop`はCommon Lispに於いては`NTHCDR`が相当する。
ただし、名前の通り`NTHCDR`はリスト専用である。
文字列にも使いたいならSERAPEUMの`DROP`が使える。
incf-clにも`DROP`はあるが、リスト専用である。

```lisp
cl-user> (nthcdr 3 '(8 4 2 1 5 6))
(1 5 6)
cl-user> (nthcdr 0 '(1 2 3 4))
(1 2 3 4)
cl-user> (nthcdr 100 '(1 2 3 4))
NIL
cl-user> (nthcdr 3 "foobar")
;; ERROR
cl-user> (serapeum:drop 3 "foobar")
"bar"
```

```haskell
ghci> maximum [1,9,2,3,4]
9
ghci> minimum [8,4,2,1,5,6]
1
```
Common Lispに上記相当の関数はない。
`APPLY`でなんとかする。

```lisp
cl-user> (apply #'max '(1 9 2 3 4))
9
cl-user> (max 1 9 2 3 4)
9
cl-user> (apply #'min '(8 4 2 1 5 6))
1
cl-user> (min 8 4 2 1 5 6)
1
```
以下の`sum`、`product`も同様である。
この辺、余計な名前を覚えなくて良い分、Common Lispの方がスッキリしている印象ではあるが、Haskellの関数がデフォルトでカリー化されているという仕様を鑑みれば、まぁ、必要だよなぁ、とも思う。

```haskell
ghci> sum [5,2,1,6,3,2,5,7]
31
ghci> product [6,2,1,2]
24
ghci> product [1,2,5,6,7,9,2,0]
0
```

```lisp
cl-user> (apply #'+ '(5 2 1 6 3 2 5 7))
31
cl-user> (+ 5 2 1 6 3 2 5 7)
31
cl-user> (apply #'* '(6 2 1 2))
24
cl-user> (* 6 2 1 2)
24
cl-user> (apply #'* '(1 2 5 6 7 9 2 0))
0
cl-user> (* 1 2 5 6 7 9 2 0)
0
```
Haskellの`elem`はおよそCommon Lispの`FIND`に相当していると言える。

```haskell
ghci> 4 `elem` [3,4,5,6]
True
ghci> 10 `elem` [3,4,5,6]
False
```

```lisp
cl-user> (find 4 '(3 4 5 6))
4
cl-user> (find 10 '(3 4 5 6))
NIL
```
なお、`FIND`は見つけたものを返すので`NIL`を見つけられない。
そのような場合、`POSITION`を使う。

```lisp
cl-user> (find nil '(1 nil 2))
NIL
cl-user> (position nil '(1 nil 2))
1
```

## 1.4

```haskell
ghci> [1..20]
[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]
ghci> ['a'..'z']
"abcdefghijklmnopqrstuvwxyz"
ghci> ['K'..'Z']
"KLMNOPQRSTUVWXYZ"
```
Common Lispに上記機能はない。
`Ord`という型と、後継者関数`succ`の存在が大きい。
これはHaskellの綺麗さの一旦であろうと思う。
文字列がリストだというのも一役かってそう。
ただ、Haskellerの中でも文字列がリストなのは間違いだという意見もあるらしく、どうなるものやら。

整数のレンジを求めるだけならINCF-CLの`RANGE`が使える。

```lisp
cl-user> (incf-cl:range 1 20)
(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20)
```

文字のレンジから文字列を求めるのは、それ用のユーティリティも見当たらず、必要なら自作するしかない。

```lisp
(defun char-range(x y)
  (let*((code-x(char-code x))
        (code-y(char-code y))
        (string(make-string(1+(- code-y code-x)))))
    (loop :for n :upfrom code-x :to code-y
          :for i :upfrom 0
          :do(setf(char string i)(code-char n)))
    string))
cl-user> (char-range #\a #\z)
"abcdefghijklmnopqrstuvwxyz"
cl-user> (char-range #\K #\Z)
"KLMNOPQRSTUVWXYZ"
```

```haskell
ghci> [2,4..20]
[2,4,6,8,10,12,14,16,18,20]
ghci> [3,6..20]
[3,6,9,12,15,18]
```
Common Lispでは`LOOP`マクロが割と強力なので、`LOOP`でなんとかするCLerが多いのではないかと思われる。

```lisp
cl-user> (loop :for e :upfrom 2 :to 20 :by 2 :collect e)
(2 4 6 8 10 12 14 16 18 20)
cl-user> (incf-cl:range 2 2 20)
(2 4 6 8 10 12 14 16 18 20)
cl-user> (loop :for e :upfrom 6 :to 20 :by 3 :collect e)
(6 9 12 15 18)
cl-user> (incf-cl:range 6 3 20)
(6 9 12 15 18)
```

```haskell
ghci> [1,2,4,8,16..100]
;; Does not work.
```

```lisp
cl-user> (loop :for i :upfrom 1 :for expt = (expt 2 i) :while (< expt 100) :collect expt)
(2 4 8 16 32 64)
```

```haskell
ghci> [20,19..1]
[20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1]
ghci> [20..1]
[]
```
icf-clの`RANGE`は降順をサポートしていない。

```lisp
cl-user> (loop :for i :downfrom 20 :to 1 :collect i)
(20 19 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1)
cl-user> (loop :for i :upfrom 20 :to 1 :collect i)
NIL
```

```haskell
ghci> [13,26..24*13]
[13,26,39,52,65,78,91,104,117,130,143,156,169,182,195,208,221,234,247,260,273,286,299,312]
ghci> take 24 [13,26..]
[13,26,39,52,65,78,91,104,117,130,143,156,169,182,195,208,221,234,247,260,273,286,299,312]
```
無限リストを扱うなら`SERIES`が使える。

```lisp
cl-user> (loop :for i :upfrom 13 :by 13 :repeat 24 :collect i)
(13 26 39 52 65 78 91 104 117 130 143 156 169 182 195 208 221 234 247 260 273 286 299 312)
cl-user> (series:collect (series:scan-range :from 13 :by 13 :length 24))
(13 26 39 52 65 78 91 104 117 130 143 156 169 182 195 208 221 234 247 260 273 286 299 312)
```

```haskell
ghci> take 10 (cycle [1,2,3])
[1,2,3,1,2,3,1,2,3,1]
ghci> take 12 (cycle "LOL ")
"LOL LOL LOL "
```

```lisp
cl-user> (incf-cl:take 10 (incf-cl:cycle '(1 2 3)))
(1 2 3 1 2 3 1 2 3 1)
cl-user> (series:collect(series:subseries(series:series 1 2 3)0 10))
(1 2 3 1 2 3 1 2 3 1)
cl-user> (series:collect 'string (series:subseries (series:series . #.(coerce "LOL " 'list))
                                                   0 12))
"LOL LOL LOL "
cl-user> (coerce (incf-cl:take 12 (incf-cl:cycle(coerce "LOL " 'list)))'string)
"LOL LOL LOL "
```

なお、効率はSERIESの方が良い。

```lisp
(lambda()
  (series:collect 'string (series:subseries (series:series . #.(coerce "LOL " 'list))
                                            0 12)))
(compile nil *)
(time(funcall *))
;; 19,076 processor cycles, 0 byte consed.

(lambda()
  (coerce (incf-cl:take 12 (incf-cl:cycle '#.(coerce "LOL " 'list)))'string))
(compile nil *)
(time(funcall *))
;; 95,076 processor cycles, 0 byte consed.
```
マクロ展開のオーバーヘッドを効率に入れないよう、コンパイルしてから計測している点注意。
上記のコストは完全に実行時のコストである。

```haskell
ghci> take 10 (repeat 5)
[5,5,5,5,5,5,5,5,5,5]
```

```lisp
cl-user> (series:collect(series:subseries(series:series 5)0 10))
(5 5 5 5 5 5 5 5 5 5)
```

```haskell
ghci> replicate 3 10
[10,10,10]
```
Haskellの`replicate`はCommon Lispに於いては`MAKE-LIST`に相当する。

```lisp
cl-user> (make-list 3 :initial-element 10)
(10 10 10)
```

```haskell
ghci> [0.1,0.3 .. 1]
[0.1,0.3,0.5,0.7,0.8999999999999999,0.999999999999999]
```

```lisp
cl-user> (loop :for f :upfrom 0.1 :to 1 :by 0.2 :collect f)
(0.1 0.3 0.5 0.7 0.900000004)
```

## 1.5

```haskell
ghci> [x*2 | x <- [1..10]]
[2,4,6,8,10,12,14,16,18,20]
```
リスト内包表記にはINCF-CLの`LC`が使える。
素の`LOOP`マクロでも多くをまかなえる。

```lisp
cl-user> (loop :for i :upfrom 1 :to 10 :collect (* i 2))
(2 4 6 8 10 12 14 16 18 20)
cl-user> (incf-cl:lc (* x 2)(incf-cl:<- x (incf-cl:range 1 10)))
(2 4 6 8 10 12 14 16 18 20)
```
単純な例では`LOOP`の方が効率がいい。

```lisp
(lambda()(loop :for i :upfrom 1 :to 10 :collect (* i 2)))
(compile nil *)
(time(funcall *))
;; 6,308 processor cycles, 0 bytes consed.

(lambda()(incf-cl:lc (* x 2)(incf-cl:<- x (incf-cl:range 1 10))))
(compile nil *)
(time(funcall *))
;; 11,514 processor cycles, 0 bytes consed.
```

```haskell
ghci> [x*2 | x <- [1..10], x*2 >= 12]
[12,14,16,18,20]
ghci> [x | x <- [50..100], x `mod` 7 == 3]
[52,59,66,73,80,87,94]
```

```lisp
cl-user> (loop :for x :upfrom 1 :to 10
               :for y = (* x 2)
               :when (>= y 12)
               :collect y)
(12 14 16 18 20)
cl-user> (incf-cl:lc (* x 2)(incf-cl:<- x (incf-cl:range 1 10))(>= (* x 2)12))
(12 14 16 18 20)

cl-user> (loop :for x :upfrom 50 :to 100 :when (= 3 (mod x 7)) :collect x)
(52 59 66 73 80 87 94)
cl-user> (incf-cl:lc x (incf-cl:<- x (incf-cl:range 50 100))(= (mod x 7)3))
(52 59 66 73 80 87 94)
```

```haskell
boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x ]
```

```lisp
(defun boom-bangs (xs)
  (loop :for x :in xs
        :when (oddp x)
        :collect (if(< x 10)
                   "BOOM!"
                   "BANG!")))
```

```haskell
ghci> boomBangs [7..13]
["BOOM!","BOOM!","BANG!","BANG!"]
```

```lisp
cl-user> (boom-bangs(loop :for i :upfrom 7 :to 13 :collect i))
("BOOM!" "BOOM!" "BANG!" "BANG!")
```

```haskell
ghci> [ x | x <- [10..20],x /= 13, x /= 15, x /= 19]
[10,11,12,14,16,17,18,20]
```

```lisp
cl-user> (loop :for i :upfrom 10 :to 20
               :unless (find i '(13 15 19))
               :collect i)
(10 11 12 14 16 17 18 20)
cl-user> (incf-cl:lc x (incf-cl:<- x (incf-cl:range 10 20))(not(find x '(13 15 19))))
(10 11 12 14 16 17 18 20)
```

```haskell
ghci> [x+y | x <- [1,2,3], [10,100,1000]]
[11,101,1001,12,102,1002,13,103,1003]
```
素の`LOOP`で書くのが苦しくなってきた。

```lisp
cl-user> (loop :for i :in '(1 2 3)
               :nconc (loop :for j :in '(10 100 1000)
                            :collect (+ i j)))
(11 101 1001 12 102 1002 13 103 1003)
cl-user> (alexandria:map-product #'+ '(1 2 3) '(10 100 1000))
(11 101 1001 12 102 1002 13 103 1003)
cl-user> (incf-cl:lc (+ x y)(incf-cl:<- x '(1 2 3))(incf-cl:<- y '(10 100 1000)))
(11 101 1001 12 102 1002 13 103 1003)
```
複雑な例になると効率差がなくなってくる。

```lisp
(lambda()(loop :for i :in '(1 2 3)
               :nconc (loop :for j :in '(10 100 1000)
                            :collect (+ i j))))
(compile nil *)
(time(funcall *))
;; 11,058 processor cycles, 0 byte consed.

(lambda()(alexandria:map-product #'+ '(1 2 3) '(10 100 1000)))
(compile nil *)
(time(funcall *))
;; 30,191 processor cycles, 0 byte consed.

(lambda() (incf-cl:lc (+ x y)(incf-cl:<- x '(1 2 3))(incf-cl:<- y '(10 100 1000))))
(compile nil *)
(time(funcall *))
;; 13,471 processor cycles, 0 byte consed.
```

```haskell
ghci> [ x*y | x <- [2,5,10], y <- [8,10,11], x*y > 50]
[55,80,100,110]
```

```lisp
cl-user> (remove-if-not (lambda(x)(> x 50))
                        (alexandria:map-product #'* '(2 5 10)'(8 10 11)))
(55 80 100 110)
cl-user> (loop :for x :in '(2 5 10)
               :nconc (loop :for y :in '(8 10 11)
                            :for z = (* x y)
                            :when (> z 50)
                            :collect z))
(55 80 100 110)
cl-user> (incf-cl:lc (* x y)(incf-cl:<- x '(2 5 10))(incf-cl:<- y '(8 10 11))(> (* x y) 50))
(55 80 100 110)
```

```haskell
ghci> let nouns = ["hobo","frog","pope"]
ghci> let adjectives = ["lazy","grouchy","scheming"]
ghci> [adjective ++ " " ++ noun | adjective <- adjectives, noun <- nouns]
["lazy hobo","lazy frog","lazy pope","grouchy hobo","grouchy frog","grouchy pope","scheming hobo","scheming frog","scheming pope"]
```

```lisp
cl-user> (let((nouns '("hobo" "frog" "pope"))
              (adjectives '("lazy" "grouchy" "scheming")))
           (alexandria:map-product (lambda(adjective noun)
                                     (concatenate 'string adjective " " noun))
                                   adjectives
                                   nouns))
("lazy hobo" "lazy frog" "lazy pope" "grouchy hobo" "grouchy frog" "grouchy pope" "scheming hobo" "scheming frog" "scheming pope")
cl-user> (incf-cl:lc(concatenate 'string adjuctive " " noun)
                    (incf-cl:<- adjuctive '("lazy" "grouchy" "scheming"))
                    (incf-cl:<- noun '("hobo" "frog" "pope")))
("lazy hobo" "lazy frog" "lazy pope" "grouchy hobo" "grouchy frog" "grouchy pope" "scheming hobo" "scheming frog" "scheming pope")
```

```haskell
length' xs = sum [1 | _ <- xs]
```
Common Lispに於いて`'`はマクロ文字なので通常シンボル名などには使えない。
代わりに`%`を使うのが作法として定着している。

```lisp
(defun length% (xs)
  (loop :for i :in xs :sum 1))
```

```haskell
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]
ghci> removeNonUppercase "Hahaha! Ahahaha!"
"HA"
ghci> removeNonUppercase "IdontLIKEFROGS"
"ILIKEFROGS"
```

```lisp
(defun remove-non-uppercase (string)
  (remove-if-not #'upper-case-p string))
cl-user> (remove-non-uppercase "Hahaha! Ahahaha!")
"HA"
```

```haskell
ghci> let xxs = [[1,3,5,2,3,1,2,4,5],[1,2,3,4,5,6,7,8,9],[1,2,4,2,1,6,3,1,3,2,3,6]]
ghci> [ [ x | x <- xs, even x ] | xs <- xxs ]
[[2,2,4],[2,4,6,8],[2,4,2,6,2,6]]
```

```lisp
cl-user> (defvar *xxs* '((1 3 5 2 3 1 2 4 5)(1 2 3 4 5 6 7 8 9)(1 2 4 2 1 6 3 1 3 2 3 6)))
*XXS*
cl-user> (mapcar (lambda(xs)(remove-if #'oddp xs)) *xxs*)
((2 2 4) (2 4 6 8) (2 4 2 6 2 6))
```

## 1.6

```haskell
ghci> (1, 3)
(1,3)
ghci> (3, 'a', "hello")
(3,'a',"hello")
ghci> (50, 50.4, "hello", 'b')
(50,50.4,"hello",'b')
```
Common LispにHaskellのタプルに相当する機能はない。
リストで代用が可能だろうか。

```lisp
cl-user> '(1 . 3)
(1 . 3)
cl-user> '(3 . (#\a . ("hello" . nil)))
(3 #\a "hello")
cl-user> '(50 . (50.4 . ("hello" . (#\b . nil))))
(50 50.4 "hello" #\b)
```
型を明示的に指定したければ`DEFTYPE`を使う。

```lisp
(deftype int-tuple()
  '(cons integer integer))
(deftype int-triple()
  '(cons integer (cons integer (cons integer null))))
cl-user> (typep '(1 . 2) 'int-tuple)
T
cl-user> (typep '(1 . (2 . nil)) 'int-tuple)
NIL
```

```haskell
ghci> fst (8, 11)
8
ghci> fst ("Wow", False)
"Wow"
ghci> snd (8, 11)
11
ghci> snd ("Wow", False)
False
```

```lisp
cl-user> (car '(8 . 11))
8
cl-user> (car '("Wow" . false))
"Wow"
cl-user> (cdr '(8 . 11))
11
cl-user> (cdr '("Wow" . false))
FALSE
```
上記の`FALSE`はただのシンボルであり、True値である点要注意。

```haskell
ghci> zip [1,2,3,4,5] [5,5,5,5,5]
[(1,5),(2,5),(3,5),(4,5),(5,5)]
ghci> zip [1..5] ["one", "two", "three", "four", "five"]
[(1,"one"),(2,"two"),(3,"three"),(4,"four"),(5,five)]
ghci> zip [5,3,2,6,2,7,2,5,4,6,6]["im", "a", "turtle"]
[(5,"im"),(3,"a"),(2,"turtle")]
ghci> zip [1..]["apple", "orange", "cherry", "mango"]
[(1,"apple"),(2,"orrange"),(3,"cherry"),(4,"mango")]
```
Haskellの`zip`はCommon Lispに於いては`MAPCAR`でおよそ再現できる。

```lisp
cl-user> (mapcar #'cons '(1 2 3 4 5)'(5 5 5 5 5))
((1 . 5)(2 . 5)(3 . 5)(4 . 5)(5 . 5))
cl-user> (mapcar #'cons (incf-cl:range 1 5)'("one" "two" "three" "four" "five"))
((1 . "one")(2 . "two")(3 . "three")(4 . "four")(5 . "five"))
cl-user> (mapcar #'cons '(5 3 2 6 2 7 2 5 4 6 6)'("im" "a" "turtle"))
((5 . "im")(3 . "a")(2 . "turtle"))
cl-user> (series:collect(series:map-fn 'list #'cons (series:scan-range :from 1)(series:scan '("apple" "orrange" "cherry" "mango"))))
((1 . "apple")(2 . "orrange")(3 . "cherry")(4 . "mango"))
```

```haskell
ghci> let rightTriangles =  [ (a,b,c) | c <- [1..10], a <- [1..c], b <- [1..a], a^2 + b^2 == c^2, a+b+c == 24]
ghci> rightTriangles
[(8,6,10)]
```

```lisp
cl-user>
(loop :for c :upfrom 1 :to 10
      :nconc (loop :for a :upfrom 1 to c
                   :nconc (loop :for b :upfrom 1 to a
                                :when (and (= (+ (expt a 2)
                                                 (expt b 2))
                                              (expt c 2))
                                           (= 24 (+ a b c)))
                                :collect (list a b c))))
((8 6 10))

cl-user>
(incf-cl:lc (list a b c)
            (incf-cl:<- c (incf-cl:range 1 10))
            (incf-cl:<- a (incf-cl:range 1 c))
            (incf-cl:<- b (incf-cl:range 1 a))
            (= (infix-math:$ a infix-math:^ 2 + b infix-math:^ 2)
               (infix-math:$ c infix-math:^ 2))
            (= (infix-math:$ a + b + c) 24))
((8 6 10))
```
本稿では例題コードをREPLに貼り付ければそのまま動くように、必ずパッケージ名をプリフィックスにつけて記してある。
そのため、大変冗長であり、煩わしく見えるかもしれない。
実際に使う場合は各種パッケージをUSEすることになるかと思われるので、プリフィックスを付ける必要はなくなるであろう。

プリフィックスをつけない場合、上記コードは以下のようになる。

```lisp
(lc (list a b c) (<- c (range 1 10)) (<- a (range 1 c)) (<- b (range 1 a))
    (= ($ a ^ 2 + b ^ 2)
       ($ c ^ 2))
    (= ($ a + b + c) 24))
((8 6 10))
```
