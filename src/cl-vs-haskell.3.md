# Common Lisp vs Haskell, Chapter 3
## Meta note
### 対象読者
[前章](cl-vs-haskell.2.html)を読了済みの者。

## Introduction
本稿は「すごいH本」の内容をCommon Lispに翻訳しながらCLerがHaskellを学ぶその第3章である。
本章では主にパターンマッチを使用したHaskellにおける関数定義構文を見ていき、それをCommon Lisp上に再現していく。
本章に於けるハイライトは、パターンマッチ（trivia）の導入、Haskellに於ける`where`キーワードをマクロで再現、リーダマクロの導入あたりだろうか。
中級CLerにとっては興味深い内容になっているのではないかと思われる。

# 3
## 3.1
### Pattern match

```haskell
lucky :: Int -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"
```
素のCommon Lispで書くと以下のようになる。

```lisp
(declaim(ftype(function(fixnum)string)lucky))
(defun lucy(x)
  (case x
    (7 "LUCKY NUMBER SEVEN!")
    (t "Sorry, you're out of luck, pal!")))
```
triviaを使うと、以下のようにも書ける。

```lisp
(trivia:defun-match lucky(x)
  (7 "LUCKY NUMBER SEVEN!")
  (_ "Sorry, you're out of luck, pal!"))
```

```haskell
sayMe :: Int -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe 4 = "Four!"
sayMe 5 = "Five!"
sayMe x = "Not between 1 and 5"
```
この例に限っては素のCommon Lispで書く方が望ましいといえるかと思う。
`FORMAT`が反則的だが。
`(integer 1 5)`の部分は複合型指定子（Compound type specifier）といって、より特定的な型を指定するのに使えるものである。
Common Lispの`FIXNUM`は\``(ineger ,most-negative-fixnum ,most-positive-fixnum)`であると言える。

```lisp
(declaim(ftype(function(fixnum)string)say-me))
(defun say-me(x)
  (typecase x
    ((integer 1 5)(format nil "~:(~R~)!" x))
    (t "Not betoween 1 and 5")))
;; or
(trivia:defun-match say-me(x)
  (1 "One!")
  (2 "Two!")
  (3 "Three!")
  (4 "Four!")
  (5 "Five!")
  (_ "Not between 1 and 5"))
```

```haskell
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)
```

```lisp
(declaim(ftype(function(integer)integer)factorial))
(defun fuctorial(n)
  (case n
    (0 1)
    (t (* n (factorial (1- n))))))
;; or
(trivia:defun-match factorial(n)
  (0 1)
  (_ (* n (factorial (1- n)))))
```

### Tuple pattern match

```haskell
addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1+ y2)
```

```lisp
(declaim(ftype (function ((cons double-float double-float)(cons double-float double-float))
                         (cons double-float double-float))
               add-vectors))
(trivia:defun-match* add-vectors (c1 c2)
  (((cons x1 y1)(cons x2 y2)) (cons (+ x1 x2)(+ y1 y2))))
```
### List pattern match, and list comprehension

```haskell
ghci> let xs = [(1,3),(4,3),(2,4),(5,3),(5,6),(3,1)]
ghci> [a+b | (a, b) <- xs]
[4,7,6,8,11,4]
ghci> [x*100+3 | (x,3) <- xs]
[103,403,503]
```
incf-clの`LC`は分配束縛はサポートしているが、パターンマッチはサポートしていない。

```lisp
cl-user> (defvar xs '((1 3)(4 3)(2 4)(5 3)(5 6)(3 1)))
cl-user> (incf-cl:lc (+ a b)(incf-cl:<- (a b) xs))
(4 7 6 8 11 4)
cl-user> (incf-cl:lc (+ 3 (* x 100)) (incf-cl:<- (x 3) '((1 3)(4 3)(2 4)(5 3)(5 6)(3 1))))
;; ERROR
;; incf-cl provides binding only, not pattern matching.
cl-user> (incf-cl:lc (+ 3 (* x 100)) (incf-cl:<- (x y) '((1 3)(4 3)(2 4)(5 3)(5 6)(3 1)))
                     (= y 3))
(103 403 503)
```

```haskell
head' :: [a] -> a
haed' [] = error "Can't call head on an empty list, dummy!"
head' (x:_) = x
```

```lisp
(declaim(ftype(function(list)t)head))
(trivia:defun-match head (list)
 (nil (error "Can't call head on an empty list, dummy!"))
 ((cons x _)x))
```

```haskell
tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and "++ show y
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y
```

```lisp
(declaim(ftype(function(list)string)tell))
(trivia:defun-match tell(list)
 (() "The list is empty")
 ((list x)(format nil "The list has one elememt: ~S" x))
 ((list x y)(format nil "The list has two elements: ~S and ~S"x y))
 ((list* x y _)(format nil "This list is long. The first two elements are: ~S and ~S" x y)))
```
### as pattern

```haskell
firstLetter :: String -> String
firstLetter "" = "Empty string, whoops!"
firstLetter all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]
```
Triviaを使う場合、明示的に仮引数を書くことになるので、必要はなさそうだ。

```lisp
(declaim(ftype(function(string)string)first-letter))
(trivia:defun-match first-letter(all)
 ("" "Empty string, whoops!")
 ((trivia:vector* x _)(uiop:strcat "The first letter of " all " is " x)))
```
## 3.2
### guard

```haskell
bmiTell :: Double -> String
bmiTell bmi
   | bmi <= 18.5 = "You're underweight, you emo, you!"
   | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
   | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"
   | otherwise   = "You're a whale, congratulations!"
```
ガードは`COND`で再現できる。

```lisp
(declaim(ftype(function(float)string)bmi-tell))
(defun bmi-tell(bmi)
  (cond
    ((<= bmi 18.5) "You're underweight, you emo, you!")
    ((<= bmi 25.0) "You're supposedly normal. Pffft, I bet you're ugly!")
    ((<= bmi 30.0) "You're fat! Lose some weight, fatty!")
    (t             "You're a whale, congratulations!")))
```
## 3.3
### where

```haskell
bmiTell :: Double -> Double -> String
bmiTell weight height
   | bmi <= 18.5 = "You're underweight, you emo, you!"
   | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
   | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"
   | otherwise   = "You're a whale, congratulations!"
   where bmi = weight / height ^ 2
```
Haskellの`where`はCommon Lispの`LET`にあたるだろうが、筆記順が異なってしまう。

```lisp
(declaim(ftype(function(float float)string)bmi-tell))
(defun bmi-tell(weight height)
  (let((bmi(infix-math:$ weight / height infix-math:^ 2)))
    (cond
      ((<= bmi 18.5) "You're underweight, you emo, you!")
      ((<= bmi 25.0) "You're supposedly normal. Pffft, I bet you're ugly!")
      ((<= bmi 30.0) "You're fat! Lose some weight, fatty!")
      (t             "You're a whale, congratulations!"))))
```
`LAMBDA`を利用することで前方参照のように書くこともできなくはない。

```lisp
(defun bmi-tell(weight height)
  ((lambda(bmi)
     (cond
       ((<= bmi 18.5) "You're underweight, you emo, you!")
       ((<= bmi 25.0) "You're supposedly normal. Pffft, I bet you're ugly!")
       ((<= bmi 30.0) "You're fat! Lose some weight, fatty!")
       (t             "You're a whale, congratulations!")))
   (infix-math:$ weight / height infix-math:^ 2)))
```
マクロを設計するなら以下のようになるだろうか。

```lisp
#+design
(defun bmi-tell(weight height)
  (with-forward-reference
    (cond
      ((<= bmi 18.5) "You're underweight, you emo, you!")
      ((<= bmi 25.0) "You're supposedly normal. Pffft, I bet you're ugly!")
      ((<= bmi 30.0) "You're fat! Lose some weight, fatty!")
      (t             "You're a whale, congratulations!"))
    :where (bmi(infix-math:$ weight / height infix-math:^ 2))))
#+expanded
(defun bmi-tell(weight height)
  (let((bmi(infix-math:$ weight / height infix-math:^ 2)))
    (cond
      ((<= bmi 18.5) "You're underweight, you emo, you!")
      ((<= bmi 25.0) "You're supposedly normal. Pffft, I bet you're ugly!")
      ((<= bmi 30.0) "You're fat! Lose some weight, fatty!")
      (t             "You're a whale, congratulations!"))))
```
また、リーダマクロを利用することで以下のような表記に発展させることも可能だろう。

```lisp
#+design-with-reader-macro
(defun bmi-tell(weight height)
  #{(cond
      ((<= bmi 18.5) "You're underweight, you emo, you!")
      ((<= bmi 25.0) "You're supposedly normal. Pffft, I bet you're ugly!")
      ((<= bmi 30.0) "You're fat! Lose some weight, fatty!")
      (t             "You're a whale, congratulations!"))
    :where (bmi(infix-math:$ weight / height infix-math:^ 2))})
```
実装は後回しにしてもう少し`where`の機能を見て行きたい。

### pattern match with where

```haskell
...
  where bmi = weight / height ^ 2
        (skinny, normal, fat) = (18.5, 25.0, 30.0)
```
それがパターンマッチでなく、ただの分配束縛であるならmetabang-bindの`BIND`などが利用できよう。

```lisp
(bind:bind((bmi (infix-math:$ weight / height ^ 2))
           ((skinny normal fat)'(18.5 25.0 30.0)))
  ...)
```
素のCommon Lispでなら、以下のような展開系が考えられる。

```lisp
((lambda(bmi)
   ((lambda(skinny normal fat)
       ...)
    18.5 25.0 30.0))
 (infix-math:$ weight / height infix-math:^ 2))
```

```haskell
initials :: String -> String -> String
intials firstname lastname = [f] ++ ". " ++ [l] ++ "."
  where (f:_) = firstname
        (l:_) = lastname
```

```lisp
(declaim(ftype(function(string string)string)initials))
(defun initials(firstname lastname)
  (bind:bind((#(f _)firstname)
             (#(l _)lastname))
    (uiop:strcat f ". " l ".")))
```
### Function in where block

```haskell
calcBmis :: [(Double, Double)] -> [Double]
calcBmis xs = [bmi w h | (w, h) <- xs]
    where bmi weight height = weight / height ^ 2
```
関数を束縛する`where`節は、Common Lispでなら`FLET`で書くべきものだろう。

```lisp
(declaim(ftype(function(list)list)calc-bmis))
(defun calc-bmis(list)
  (flet((bmi(weight height)
          (infix-math:$ weight / height infix-math:^ 2)))
    (incf-cl:lc (bmi w h) (incf-cl:<- (w h) list))))
```
`LAMBDA`を利用して前方参照しようとする場合、`FUNCALL`が必要になってしまうのがブサイクである。

```lisp
(defun calc-bmis(list)
  ((lambda(bmi)(incf-cl:lc (funcall bmi w h)(incf-cl:<- (w h)list)))
   (lambda(weight height)(infix-math:$ weight / height infix-math:^ 2))))
```
`MACROLET`を利用すれば何とかならなくもない。

```
(defun calc-bmis(list)
  (macrolet((bmi(w h)`(funcall fun ,w ,h)))
   ((lambda(fun)(incf-cl:lc (bmi w h)(incf-cl:<- (w h)list)))
    (lambda(weight height)(infix-math:$ weight / height infix-math:^ 2)))))
```
だが、まぁ、素直に`FLET`を書けという案件ではある。

## 3.4
### Let

```haskell
cylinder :: Double -> Double -> Double
cylinder r h =
   let sideArea = 2 * pi * r * h
       topArea = pi * r ^ 2
   in  sideArea + 2 * topArea
```

```lisp
(declaim(ftype(function(float float)float)cylinder))
(defun cylinder(r h)
  (let((side-area(infix-math:$ 2 * pi * r * h))
       (top-area (infix-math:$ pi * r infix-math:^ 2)))
    (infix-math:$ side-area + 2 * top-area)))
```

```haskell
ghci> 4 * (let a = 9 in a + 1) + 2
```

```lisp
cl-user> (+ (* 4 (let((a 9))
                   (1+ a)))
            2)
```

```haskell
ghci> [let square x = x * x in (square 5, square 3, square 2)]
```
Common Lispでローカル関数を定義する場合は`FLET`か`LABELS`を使う。

```lisp
cl-user> (flet((square(x)
                 (* x x)))
           `(,(square 5),(square 3),(square 2)))
```
このように見ていくと、Common Lispでは変数と関数の区別があるのが割と厄介だと言える。
Haskellの持つ、変数に見えるかもしれないが無引数の定数関数であるという考え方はものごとをシンプルにしてくれる。
そして、彼らが著しく括弧を嫌うのも、変数参照のたびに括弧で括りたくないと考えれば得心のいくものだ。

```haskell
ghci> (let a = 100; b = 200; c = 300 in a*b*c, let foo="Hey "; bar="there!" in foo ++ bar)
```

```lisp
cl-user> `(,(let((a 100)
                 (b 200)
                 (c 300))
              (* a b c))
           ,(let((foo "Hey")
                 (bar "there!"))
              (uiop:strcat foo bar)))
```

```haskell
ghci> (let(a,b,c) = (1,2,3) in a+b+c) * 100
600
```
Common Lispで分配束縛を行う場合は`DESTRUCTURING-BIND`を使う。

```lisp
cl-user> (destructuring-bind(a b c)'(1 2 3)
           (* (+ a b c)
              100))
600
```

### Let in the list comprehension

```haskell
calcBmis :: [(Double),(Double)] -> [Double]
calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]
```
incf-clの`LC`には上記のようにしてローカル変数を束縛する構文は提供されていない。
素の`LOOP`マクロで割と充分と思う。

```lisp
(declaim(ftype(function(list)list)calc-bmis))
(defun calc-bmis(xs)
  (flet((bmi(w h)(infix-math:$ w / h infix-math:^ 2)))
    (loop :for (w . h) :in xs :collect (bmi w h))))
```

```haskell
calcBmis :: [(Double),(Double)] -> [Double]
calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi > 25.0]
```

```lisp
(declaim(ftype(function(list)list)calc-bmis))
(defun calc-bmis(xs)
  (loop :for (w . h) :in xs
        :for bmi = (infix-math:$ w / h infix-math:^ 2)
        :when (> bmi 25.0)
        :collect bmi))
```
## 3.5
### case

```haskell
describeList :: [a] -> String
desribeList ls = "The list is "
                 ++ case ls of [] -> "empty."
                               [x] -> "a singleton list."
                               xs -> "a longer list."
```

```lisp
(declaim(ftype(function(list)string)descirbe-list))
(defun describe-list(list)
  (uiop:strcat "The list is " (etypecase list
                                (null "empty.")
                                ((cons T null) "a singleton list.")
                                ((cons t t) "a longer list."))))
```

```haskell
describeList :: [a] -> String
desribeList ls = "The list is " ++ what ls
    where what [] = "empty."
          what [x] = "singleton list."
          what xs = "a longer list."
```

```lisp
(declaim(ftype(function(list)string)descirbe-list))
(defun describe-list(list)
  (flet((what(x)
          (etypecase x
            (null "empty.")
            ((cons t null)"singleton list.")
            ((cons t t) "a longer list."))))
    (uiop:strcat "The list is " (waht list))))
```
## Implement where keyword.
さて、`where`の実装だが、metabang-bindに依存して作ってしまおう。

```lisp
(defmacro with-forward-reference(&body body)
  (destructuring-bind(body binds)(split-sequence:split-sequence :where body)
    `(BIND:BIND,binds
       ,@body)))
```
実装は以上。

これは筆者個人のコーディングルールで、他に同様の作法で書いてあるコードを見たことが無いので念の為説明しておくが、大文字で書かれている部分は静的にそのまま埋め込まれるリテラル部で、小文字で書かれてある部分（カンマの後）は動的に生成される部分を表している。
ACLなんかでは動かないかもしれないので要注意。

これで以下のように書けるようになる。

```haskell
bmiTell :: Double -> Double -> String
bmiTell weight height
   | bmi <= 18.5 = "You're underweight, you emo, you!"
   | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
   | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"
   | otherwise   = "You're a whale, congratulations!"
   where bmi = weight / height ^ 2
```

```lisp
(declaim(ftype(function(double-float double-float)string)bmi-tell))
(defun bmi-tell(weight height)
  (with-forward-reference
    (cond
      ((<= bmi 18.5)"You're underweight, you emo, you!")
      ((<= bmi 25.0)"You're supposedly normal. Pffft, I bet you're ugly!")
      ((<= bmi 30.0)"You're fat! Lose some weight, fatty!")
      (T            "You're a whale, congratulations!"))
    :where
    (bmi (infix-math:$ weight / height infix-math:^ 2))))
```

```haskell
bmiTell :: Double -> Double -> String
bmiTell weight height
   | bmi <= skinny = "You're underweight, you emo, you!"
   | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"
   | bmi <= fat    = "You're fat! Lose some weight, fatty!"
   | otherwise   = "You're a whale, congratulations!"
   where bmi = weight / height ^ 2
         (skinny, normal, fat) = (18.5, 25.0, 30.0)
```

```lisp
(declaim(ftype(function(double-float double-float)string)bmi-tell))
(defun bmi-tell(weight height)
  (with-forward-reference
    (cond
      ((<= bmi skinny)"You're underweight, you emo, you!")
      ((<= bmi normal)"You're supposedly normal. Pffft, I bet you're ugly!")
      ((<= bmi fat)"You're fat! Lose some weight, fatty!")
      (T            "You're a whale, congratulations!"))
    :where
    (bmi (infix-math:$ weight / height infix-math:^ 2))
    ((skinny normal fat) '(18.5 25.0 30.0))))
```


```haskell
initials :: String -> String -> String
intials firstname lastname = [f] ++ ". " ++ [l] ++ "."
  where (f:_) = firstname
        (l:_) = lastname
```

```lisp
(declaim(ftype(function(string string)string)initials))
(defun initials(firstname lastname)
  (with-forward-reference
    (uiop:strcat f ". " l ".")
    :where
    (#(f _)firstname)
    (#(l _)lastname)))
```

```haskell
calcBmis :: [(Double, Double)] -> [Double]
calcBmis xs = [bmi w h | (w, h) <- xs]
    where bmi weight height = weight / height ^ 2
```
metabang-bindに依存しているので、ローカル関数を作る場合など、metabang-bindのシンタックスに従う必要がある。
が、自分で実装することを考えれば許容の範囲だろう。


```lisp
(declaim(ftype(function(list)list)calc-bmis))
(defun calc-bmis(xs)
  (with-forward-reference
    (incf-cl:lc (bmi w h)(incf-cl:<- (w . h) xs))
    :where ((:flet bmi(weight height))(infix-math:$ weight / height infix-math:^ 2))))
```

```haskell
describeList :: [a] -> String
desribeList ls = "The list is " ++ what ls
    where what [] = "empty."
          what [x] = "singleton list."
          what xs = "a longer list."
```

```lisp
(declaim(ftype(function(list)string)describe-list))
(defun describe-list(list)
  (with-forward-reference
    (uiop:strcat "The list is " (what list))
    :where
    ((:flet what(x))
     (trivia:match x
       (nil "empty.")
       ((list _) "singleton list.")
       (_ "a longer list.")))))
```

## Reader macro.
リーダマクロを実装するなら以下のような感じか。

```lisp
(defun |#{-reader|(stream character number)
  (declare(ignore character number))
  `(with-forward-reference ,@(read-delimited-list #\} stream t)))

(named-readtables:defreadtable :where-syntax
  (:merge :standard)
  (:dispatch-macro-char #\# #\{ #'|#{-reader|)
  (:syntax-from :standard #\) #\}))
```

利用する場合は以下のようになる。

```lisp
(named-readtables:in-readtable :where-syntax)
(declaim(ftype(function(list)string)describe-list))
(defun describe-list(list)
  #{(uiop:strcat "The list is " (what list))
    :where
    ((:flet what(x))
     (trivia:match x
       (nil "empty.")
       ((list _) "singleton list.")
       (_ "a longer list.")))})
```

## definterface, defcase-of
HaskellコードとCommon Lispコードとで大きな違いがまだある。
Common Lispのコードは、`DEFUN`ないし`TRIVIA:DEFUN-MATCH`のスコープの中で各節を筆記しているのに対し、Haskellのコードではスコープの外で各節を筆記しているように筆者には見える。
実際の実装がどうなっているのかについては知らないのだが、とにかくシンタックス上はそのように見える。

たとえば以下のHaskellコードは、

```haskell
lucky :: Int -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"
```

筆者には以下のように見える。

```lisp
(definterface lucky (Int) String)
(defcase-of(lucky 7)"LUCKY NUMBER SEVEN")
(defcase-of(lucky _)"Sorry, you're out of luck, pal!")
```

各節が別々のスコープを持ったものとして定義されているように見えるのだ。
丁度`DEFGENERIC`と`DEFMETHOD`との関係に似ている。

では、これを再現してみよう。

```lisp
(defconstant +skip+ '#:skip)

(defmacro definterface (name arg* result)
  (check-type name symbol)
  (let((gensyms(loop :for x :in arg*
                     :collect (gensym))))
    `(PROGN (DECLAIM(FTYPE(FUNCTION ,arg* ,result),name))
            (DEFUN ,name ,gensyms
              (LABELS((REC(CLAUSES)
                        (IF(ENDP CLAUSES)
                          (ERROR "Unmatch clause ~S ~S"',name (list ,@gensyms))
                          (LET((RESULT(FUNCALL(CAR CLAUSES),@gensyms)))
                            (IF(EQ +SKIP+ RESULT)
                              (REC(CDR CLAUSES))
                              RESULT)))))
                (REC(OR (GET ',name 'CLAUSES)
                        (ERROR "No pattern match clause in ~S"',name))))))))

(defmacro defcase-of((name&options &rest pattern*) &body body)
  (multiple-value-bind(name options)(parse-name&options name&options)
    (let*((lambda-list(millet:lambda-list name))
          (defalut-clause(loop :repeat (length lambda-list)
                               :collect '_))
          (body`(TRIVIA:MATCH*,lambda-list
                  (,pattern* ,@body)
                  (,defalut-clause +SKIP+))))
      `(ALEXANDRIA:APPENDF(GET ',name 'CLAUSES)
         (LIST (LAMBDA,lambda-list
                 ,(if options
                    `(TRIVIA:LET-MATCH1(LIST ,@options)(LIST ,@lambda-list)
                       ,body)
                    body)))))))

(defun parse-name&options(n&o)
  (if(symbolp n&o)
    (values n&o nil)
    (values (car n&o)(cdr n&o))))
```
これで以下のように書ける。

```lisp
(definterface lucky (fixnum) string)
(defcase-of(lucky 7)"LUCKY NUMBER SEVEN")
(defcase-of(lucky _)"Sorry, you're out of luck, pal!")
```
as-patternにも対応している。

```haskell
firstLetter :: String -> String
firstLetter "" = "Empty string, whoops!"
firstLetter all@(x:xs) = "The first letter of " ++ all ++ "is " ++ [x]
```

```lisp
(definterface first-letter (string)string)
(defcase-of(first-letter "")"Empty string, whoops!")
(defcase-of((first-letter all)(trivia:string* x _))
  (uiop:strcat "The first letter of " all " is " x))
```
