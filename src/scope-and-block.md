# Scope, block and extent in Common Lisp.
## Meta note
### 対象読者
* ブロック、スコープという言葉に関心のある人。

## Introduction.
Common Lispという言語でもスコープという言葉は使われます。
ですがブロックという言葉はCommon Lispでは異なる意味を持ちます。
Common Lispという言語で`BLOCK`は特殊形式というオペレータです。
他言語でいうブロックとは少々趣が異なるかと思われます。

代わりといってはなんですがCommon Lispではextentという言葉がscopeと絡めて語られます。
本稿では自分の言葉でこれらを解説してみたいと思います。

## PROGN

`PROGN`はS式を複数受け取り、左から順に評価していき、最後のS式の評価結果を`PROGN`全体の評価結果として返します。
他言語でいうブロックはCommon Lispでは`PROGN`になるかと思われます。

```lisp
* (progn (print :a) (print :b) (print :c))

:A      ; <--- Side effect.
:B      ; <--- Side effect.
:C      ; <--- Side effect.
:C      ; <--- Return value of PROGN.
```

`PROGN`には暗黙理のものがあります。
代表が`LET`の本体です。

## Scope.
スコープとは見える範囲を指す言葉です。

例えば`LET`は局所変数を定義しますが、そのスコープ（見える範囲）はそのボディに限られます。
`LET`のボディは暗黙理の`PROGN`となります。

```lisp
* (let ((a 0))
    (print a)
    (1+ a))

0       ; <--- Side effect.
1       ; <--- Return value of LET.

* a
; Error
```

### Type of scopes.
スコープには二種類あります。
レキシカルなスコープとダイナミックなスコープです。

#### Lexical scope.
`LET`は通常レキシカルなスコープを持ちます。
レキシカルとはこの場合「文字列上の」くらいの意味です。

以下の図では`LET`のボディで`A`を参照する`BOTTOM`を呼び出していますが、`BOTTOM`からは変数`A`が見えないのを表しています。
なぜなら文字列としては`(LET ((A 0)) (BOTTOM))`で閉じているからです。
変数`A`はその文字列としての範囲内でのみ有効です。

![image of lexical scope.](../img/lexical.svg)

#### Dynamic scope.
動的なスコープを持つ変数は通常`DEFVAR`ないし`DEFPARAMETER`で定義します。

```lisp
* (defvar *a* 0)
*A*
```

動的スコープの場合、スコープの解決（＝変数の参照）を実行時に動的に行います。

上の図における`BOTTOM`は`A`に対する参照の解決がコンパイル時に確定できないので、多くの処理系でコンパイルエラーとなります。（コンディションは処理系に依存します。）

参照すべき変数が動的な変数だと分かっている場合はコンパイルエラーとなることなく、変数参照を実行時に行います。

```lisp
* (defun bottom2 ()
    *a*)
BOTTOM2

* (defun bottom3 ()
    (declare (special a))
    a)
BOTTOM3
```

上の例では`BOTTOM2`は事前に動的であると`DEFVAR`により宣言されている変数への参照なのでエラーとなりません。

また、`BOTTOM3`では変数`A`は動的な参照であることを`DECLARE`により宣言してあるので、これもエラーとはなりません。
ただしこの場合も以下のコードはエラーとなります。

```lisp
* (let ((a 0))
    (bottom3))
; Error
```

なぜなら`LET`が作る変数`A`はあくまでレキシカルなスコープを持つ変数だからです。
`BOTTOM3`が実行時に参照する変数`A`はあくまで動的なものです。
`LET`の変数`A`はレキシカルなのでそのスコープはもう閉じてあるので`BOTTOM3`からは見えません。

見えるようにするためには`LET`の側にも宣言が必要となります。

```lisp
* (let ((a 0))
    (declare (special a))
    (bottom3))
0
```

## Extent
エクステントとは値が生きている時間軸的な長さを表す言葉です。

エクステントには２種類あります。
おのおの、動的なエクステントと無限のエクステントとになります。

### indefinite extent.
Common Lispのオブジェクトは通常無限エクステントを持ちます。

そのためレキシカルなスコープを抜けた後も変数への参照が残り続けるということが起こります。

```lisp
* (let ((a 0))
    (lambda () a))
#<FUNCTION (LAMBDA ()) {...}>

* (funcall *)
0
```

上のコード例ではまず`LET`が変数`A`を作り無名関数を返します。
無名関数の中では`A`への参照が保持されます。

変数`A`のレキシカルなスコープはもう閉じています。
ですが返り値である`LAMBDA`は`LET`のレキシカルな（文字列としての）スコープ内で作られたものでレキシカルなスコープ内で変数`A`への参照を保持しています。
この参照はレキシカルなスコープが閉じた後にも残り続けるので、関数呼び出しを行うと正しく`0`を返します。

このレキシカルな変数への参照を閉じ込めた関数のことをクロージャといいます。

### Dynamic extent.
動的なエクステントとはいうなれば一時的なエクステントです。

`WITH-OPEN-FILE`で開いたストリームは`WITH-OPEN-FILE`のレキシカルなスコープ内でのみOPENな状態であり、スコープを抜けると同時にストリームは閉じられます。
ストリームに束縛される変数は、クロージャで包めばスコープを抜けた後でも参照はできます。
ですがそのストリームは閉じられた後のストリームになります。

また変数に宣言をすることで変数を動的エクステントであると宣言することができます。

```lisp
* (let ((a 0))
    (declare (dynamic-extent a))
    (lambda () a))
#<FUNCTION (LAMBDA ()) {...}>
```
上の例では`LET`が作る変数`A`に動的エクステントであるという宣言がされています。
返された無名関数は変数`A`への参照を保持し続けますが、その変数`A`が実行時に有効であるかどうかはわかりません。
動的エクステントであると宣言されているので、GCが値を回収し、実行されるその瞬間には全く関係ない値がそのアドレスに置かれているかもしれません。

## BLOCK
Common Lispという言語で`BLOCK`は特殊形式です。
`BLOCK`の中からは`RETURN-FROM`で値を返すことができます。

```lisp
* (block :a
    (return-from :a 3))
3
```

`BLOCK`はレキシカルなスコープを持ちます。

```lisp
* (defun test () (return-from :a 0))
; Error
```

上記コードはコンパイル時のレキシカルな環境下に`:A`という名前の`BLOCK`がないので多くの処理系でエラーとなります。

レキシカルな環境はクロージャで包むことで渡すことが可能となります。

```lisp
* (defun bottom4 (returner)
    (funcall returner))
BOTTOM4

* (block :a
    (bottom4 (lambda () (return-from :a 0)))
    (print :never))
0
```

上記コードでは`BLOCK`名`:A`を無名関数にクロージャとして包んで`BOTTOM4`に渡しています。
`BOTTOM4`は無名関数を呼び出すだけのものなので、`PRINT`にはけしてたどり着くことはありません。

### Implicit BLOCK.
`BLOCK`には暗黙理に作られるものもあります。

#### defun
関数を定義すると、暗黙理に関数名と同じ名前を持つ`BLOCK`が作られます。

```lisp
* (defun test () (return-from test 0) (print 0))
TEST

* (test)
0
```

#### DO family and LOOP.
`DO`のファミリーと`LOOP`マクロは暗黙理に`NIL`という名前の`BLOCK`を形成します。

`NIL`を名前に持つブロックからは`RETURN`で帰れます。
`RETURN`は単に`(return-from nil ...)`へ展開されるマクロでしかありません。

```lisp
* (dotimes (x 5) (if (oddp x) (return :return) (print x)))

0
:RETURN
```
