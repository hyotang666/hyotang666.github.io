# Common Lisp vs Haskell, Chapter 2
## Meta note
### 対象読者
[前章](archives/cl-vs-haskell.html)を読了済みの者。

## Introduction
本稿は「すごいH本」の内容をCommon Lispに翻訳しながらCLerがHaskellを学ぶその第2章である。
本章ではHaskellの基本的な型を、Common Lispの型システムに翻訳しながら学習する。

Common Lispはその処理系により動的型付け〜ゆるい型付けを行う言語である。
CLtL2からの引用をしよう。

> １つの例外を除いて宣言は完全にオプションであり、正しい宣言は正しいプログラムの意味に影響を与えない。

それに対しHaskellは強い型付けの言語であり、型の機能が言語中枢に分かちがたく結びついている。
ちょうどそれはCommon Lispに於いてリスト機能が言語中枢に分かちがたく結びついているがごとくである。

型に関してはHaskellの方が相当強力であり、それをCommon Lisp上に再現するのは相当難しい。
そのせいもあって本章は前章と比べて大変短いものとなっている。
また、特筆すべきハイライトも皆無といって差し支えない。

# 2
## 2.1

```haskell
ghci> :t 'a'
'a' :: Char
ghci> :t True
True :: Bool
ghci> :t "HELLO!"
"HELLO!" :: [Char]
ghci> :t (True, 'a')
(True, 'a') :: (Bool, Char)
ghci> :t 4 == 5
4 == 5 :: Bool
```
Common Lispでオブジェクトの型を知るには`TYPE-OF`が使えなくもない。

```lisp
cl-user> (type-of #\a)
STANDARD-CHAR
cl-user> (type-of t)
BOOLEAN
cl-user> (type-of "HELLO!")
(SIMPLE-ARRAY CHARACTER (6))
```
列を扱いだすと役に立たなくなる。

```lisp
cl-user> (type-of '(t . #\a))
CONS
cl-user> (type-of '(= 4 5))
CONS
```
型を調べるのとはまた違うが、オブジェクトの中身を調べる`INSPECT`は標準で存在している。

```lisp
cl-user> (inspect '(t . #\a))
;; The object is a CONS.
;; 0. CAR: T
;; 1. CDR: #\a
;; > :q
```

```haskell
addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z
```
Common Lispで関数の型を宣言するには`DECLAIM`を以下のように使う。

```lisp
(declaim(ftype (function(fixnum fixnum fixnum)fixnum) add-three))
(defun add-three(x y z)
  (+ x y z))
```

## 2.3

```haskell
ghci> :t head
head :: [a] -> a
```
`TYPE-OF`が役に立たないのは関数でも同じ。
有益な情報は得られない。

```lisp
cl-user> (type-of #'car)
FUNCTION
```
`DESCRIBE`に渡せば処理系依存で少しはまともな情報が見れる。

```lisp
cl-user> (describe #'car)
;; #<FUNCTION CAR>
;;  [compiled function]
;; Lambda-list: (LIST)
;; Declared type: (FUNCTION (LIST) (VALUES T &OPTIONAL))
;; Documentation:
;;   Return the 1st object in a list.
```
`INSPECT`した場合。

```lisp
cl-user> (inspect #'car)
;; The object is a FUNCTION named CAR.
;; 0. Lambda-list: (LIST)
;; 1. Ftype: (FUNCTION (LIST) (VALUES T &OPTIONAL))
```

## 2.2
### Int
Common Lispに於ける`FIXNUM`に相当。
仕様では１６ビットの符号つき整数が最低限保証されている。
最大値は処理系依存で、それらの値は定数`MOST-POSITIVE-FIXNUM`、`MOST-NEGATIVE-FIXNUM`に登録されている。
### Integer
Common Lispに於ける`INTEGER`と完全に等価と言える。
### Float Double
Common Lispでは浮動小数点数は全部で４つある。
各々`SHORT-FLOAT`、`SINGLE-FLOAT`、`DOUBLE-FLOAT`、`LONG-FLOAT`である。
これらの範囲は処理系依存であるが、`SHORT-FLOAT`が最小固定精度であり、`LONG-FLOAT`が最大固定精度である。
全て`FLOAT`型と解釈される。
### Bool
Common Lispは汎ブールをサポートしているので`NIL`以外の値は全てTrueである。
ただし、`BOOLEAN`という場合は`NIL`か`T`のみを指すこととなる。

```lisp
cl-user> (typep 0 'boolean)
NIL
```
### Char
Common Lispに於ける`CHARACTER`に相当する。
`CHARACTER`に特定化されたベクタが文字列となる。

```lisp
cl-user> (vector #\a)
#(#\a)
cl-user> (stringp *)
NIL
cl-user> (make-array 1 :initial-element #\a :element-type 'character)
"a"
cl-user> (make-string 1 :initial-element #\a)
"a"
```
### Tuple
Common Lispのリストに相当する。
`CONS-TYPE-SPCIFIER`を駆使することで、長さや要素の型を指定できる。

```lisp
cl-user> (typep '(1) '(cons fixnum null))
T
cl-user> (typep '(1) '(cons fixnum fixnum))
NIL
cl-user> (typep '(1 . 2) '(cons fixnum fixnum))
T
cl-user> (typep '(1 "string" #\a) '(cons fixnum (cons string (cons character null))))
T
```
## 2.3
Common Lispには型変数に相当する機能はない。
強いてあげれば`DEFTYPE`の引数をそのようなものとして利用することが出来なくもないといった程度か。
## 2.4
Common Lispに型クラスに相当する機能はない。

