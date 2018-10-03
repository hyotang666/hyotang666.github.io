# Common Lisp vs Haskell, Chapter 7
## Meta note
### 対象読者
[前章](cl-vs-haskell.6.html)を読了済みの者。

## Introduction
本稿は「すごいH本」の内容をCommon Lispに翻訳しながらCLerがHaskellを学ぶその第7章である。
本章ではHaskellの`data`、`class`、`instance`キーワードを、すなわち、代数的データ型（Algebraic Data Type）、型クラスをCommon Lispで実装しながら学ぶ。

初級CLerは着いてこれなくなるかと思う。
こまかいコードは読まず、本文のみをざっと流し読みし雰囲気だけ汲み取り「Common Lispではこういうことができるのだなぁ」と思っていただければ幸い。
気合が入っている方は実力を付けてから、また読み返していただければ良い。

逆に中級CLerはコードをちゃんと読まないと着いてこれなくなるかと思われる。
難しさの度合いとしては、実践Common Lispのバイナリパーサの章と同等以上と捉えていただければ目安として分かりやすかろうと思う。
幸い、原著の各節に沿って、機能が紹介される毎にコードを拡張していくというスタイルを取っているので、一歩づつ読み解いていけば着いてこれないことはないと思う。

上級CLerの方は、ADTの実装や型クラスの実装に興味があるなら大変楽しく読めるかと思う。
そうでないなら「君はそう書くフレンズなんだね」以上の感想にはならないかと思われる。
「型クラスの実装」だとティンと来ないかもしれないが、「型情報を頼りにメソッドディスパッチをコンパイル時に解決することでオーバーヘッド無しにポリモーフィズムの恩恵を得られるようにする」と言えば興味を持つ方もおられるのではあるまいか。

なお、ここで書かれるコードは原著の例題が動けばそれだけで良いとするものなので、汎用的なものとしては欠陥だらけであることを始めに白状しておく。
実際本稿での実装では11章で出てくるApplicative Functorの実装が出来ない。

Haskellerをはじめとする非Lisperで読んでいる方がいれば、本章はCommon Lispが持つマクロの強力さと、その強力さを得るために付き合っていかなければならない醜さとを痛感できる章になっているかと思う。

本章は原著でも最も長い章だけあって、恐ろしく長く濃い。
たっぷりのコーヒーとチョコレート、そして数日に分けなければ読み切れないかもしれないというちょっぴりの覚悟を持ってお読み頂ければ幸い。

# 7
## 7.1

```haskell
data Bool = False | True
```
単にそれっぽくするだけなら、上記HaskellコードはCommon Lispに於いては`DEFTYPE`を使用することで再現できる。

```lisp
(deftype bool()
  '(member false true))
```
これでシンボル`FALSE`とシンボル`TRUE`は`BOOL`という型であるという事を表している。

## 7.2

```haskell
data Shape = Circle Float Float Float |
             Rectangle Float Float Float Float
```
Haskellのdataキーワードに相当するものはCommon Lispには存在しない。
上記Haskellコードと等価なCommon Lispコードは以下のようなものとなる。

```lisp
;; helper
(defun cons-type-specifier(types)
  (if(endp types)
    'null
    `(cons ,(car types),(cons-type-specifier (cdr types)))))

;; type
(deftype shape()
 `(or ,(cons-type-specifier '((eql circle)real real real))
      ,(cons-type-specifier '((eql rectangle) real real real real))))

;; constructors
(declaim(ftype(function(real real real)shape)circle))
(defun circle(a b c)
  (list 'circle a b c))

(declaim(ftype(function(real real real real)shape)rectngle))
(defun rectangle(a b c d)
  (list 'rectangle a b c d))
```
マクロにしてしまおう。

```lisp
(defmacro defdata(name &rest constructor*)
  `(EVAL-WHEN(:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
     (DEFTYPE ,name()
       '(OR ,@(mapcar (lambda(constructor)
                        (cons-type-specifier
                         `((EQL ,(car constructor)) ,@(cdr constructor))))
                      constructor*)))
     ,@(mapcan (lambda(constructor)
                 (let((lambda-list(loop :repeat (length(cdr constructor))
                                        :collect (gensym))))
                   `((DECLAIM(FTYPE(FUNCTION,(cdr constructor),name),(car constructor)))
                     (DEFUN ,(car constructor),lambda-list
                       (LIST ',(car constructor),@lambda-list)))))
               constructor*)
     ',name))
```
これで以下のように書ける。

```lisp
(defdata shape
  (circle real real real)
  (rectangle real real real real))
```

```haskell
ghci> :t Circle
Circle :: Float -> Float -> Float -> Shape
```

```lisp
cl-user> (inspect #'circle)
;; The object is a FUNCTION named CIRCLE.
;; 0. Lambda-list: (#:G611 #:G612 #:G613)
;; 1. Ftype: (FUNCTION (REAL REAL REAL)
;;            (VALUES
;;             (OR
;;              (CONS (MEMBER CIRCLE)
;;                    (CONS REAL (CONS REAL (CONS REAL NULL))))
;;              (CONS (MEMBER RECTANGLE)
;;                    (CONS REAL
;;                          (CONS REAL
;;                                (CONS REAL (CONS REAL NULL))))))
;;             &OPTIONAL))
```

```haskell
area :: Shape -> Float
area (Circle _ _ r) = pi * r ^ 2
area (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)
```

```lisp
(declaim(ftype(function(shape)real)area))
(trivia:defun-ematch area(shape)
  ((list (eq 'circle) _ _ r)
   (infix-math:$ pi * r infix-math:^ 2))
  ((list (eq 'rectangle) x1 y1 x2 y2)
   (infix-math:$ (abs x2 - x1) * (abs y2 - y1))))
```
`DECLAIM`で返り値を`REAL`と宣言してあるので、`DEFUN-EMATCH`でないとコンパイラがクレームをつけてくる点要注意。
`DEFUN-MATCH`は`NIL`が返る可能性があり、`NIL`は`REAL`でないからだ。

ところで、パターンマッチの構文がなんとも煩わしいので、triviaの`DEFPATTERN`でパターンを定義してしまおう。

```lisp
(defun <pattern-matcher>(constructor)
  (when(listp constructor)
    (let((name(car constructor)))
      `((TRIVIA:DEFPATTERN,name(&REST ARGS)
          `(LIST (EQ ',',name) ,@ARGS))))))

(defmacro defdata(name &rest constructor*)
  `(EVAL-WHEN(:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
     (DEFTYPE ,name()
       '(OR ,@(mapcar (lambda(constructor)
                        (cons-type-specifier
                         `((EQL ,(car constructor)) ,@(cdr constructor))))
                      constructor*)))
     ,@(mapcan (lambda(constructor)
                 (let((lambda-list(loop :repeat (length(cdr constructor))
                                        :collect (gensym))))
                   `((DECLAIM(FTYPE(FUNCTION,(cdr constructor),name),(car constructor)))
                     (DEFUN ,(car constructor),lambda-list
                       (LIST ',(car constructor),@lambda-list)))))
               constructor*)
     ,@(mapcan #'<pattern-matcher> constructor*)
     ',name))
```
これで以下のように書ける。
なお、`SHAPE`型の再定義を忘れぬよう。

```lisp
(declaim(ftype(function(shape)real)area))
(trivia:defun-ematch area (shape)
 ((circle _ _ r)(infix-math:$ pi * r infix-math:^ 2))
 ((rectangle x1 y1 x2 y2)(infix-math:$ (abs x2 - x1) * (abs y2 - y1))))
```

```haskell
ghci> map (Circle 10 20) [4,5,6,6]
[Circle 10.0 20.0 4.0, Circle 10.0 20.0 5.0, Circle 10.0 20.0 6.0, Circle 10.0 20.0 6.0]
```

```lisp
cl-user> (mapcar #`(% 'circle 10 20)'(4 5 6 6))
((circle 10 20 4)(circle 10 20 5)(circle 10 20 6)(circle 10 20 6))
```
Common Lispに於いて`FLOAT`と`INTEGER`は明確に区別されるので、ここではそれらを統合した型`REAL`を使っている点要注意。

### Point data type

```haskell
data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)
```

```lisp
(defdata point (point real real))
(defdata shape (circle point real)(rectangle point point))
```

```haskell
area :: Shape -> Float
area (Circle _ r) = pi * r ^ 2
area (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)
```

```lisp
(trivia:defun-ematch area(shape)
  ((circle _ r) (infix-math:$ pi * r infix-math:^ 2))
  ((rectangle (point x1 y1) (point x2 y2))
   (infix-math:$ (abs x2 - x1) * (abs y2 - y1))))
```

```haskell
ghci> area (Rectangle (Point 0 0)(Point 100 100))
10000.0
ghci> area (Circle (Point 0 0) 24)
1809.5574
```

```lisp
cl-user> (area (rectangle (point 0 0)(point 100 100)))
10000
cl-user> (area (circle (point 0 0) 24))
1809.5573684677208d0
```

```haskell
nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x+a)(y+b)r)
nudge (Rectagle (Point x1 y1) (Point x2 y2)) a b
    = Rectangle (Point (x1+a) (y1+ b)) (Point (x2+a) (y2+b))
```

```lisp
(declaim(ftype(function(shape real real)shape)nudge))
(trivia:defun-ematch* nudge(shape a b)
  (((circle (point x y) r) a b)
   (circle (point (+ x a)(+ y b)) r))
  (((rectangle (point x1 y1)(point x2 y2)) a b)
   (rectangle (point (+ x1 a)(+ y1 b))(point (+ x2 a)(+ y2 b)))))
```
単に束縛しなおしているだけの本来は必要のないパターンマッチはオーバーヘッドになるだけなので、以下のようにするほうが望ましかろう。

```
(defun nudge(shape a b)
  (trivia:ematch shape
    ((circle (point x y) r)
     (circle (point (+ x y)(+ y b)) r))
    ((rectangle (point x1 y1)(point x2 y2))
     (rectangle (point (+ x1 a)(+ y1 b))(point (+ x2 a)(+ y2 b))))))
```

### Export Shape as module

```haskell
module Shapes
( Point(..)
, Shape(..)
, area
, nudge
) where
```

```lisp
(defpackage :shapes(:use :cl)
  (:export ; types
           #:shape)
  (:export ; data constructors
           #:point #:circle #:rectangle)
  (:export ; main api
           #:area #:nudge))
```

## 7.3

```haskell
data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     , height :: Float
                     , phoneNumber :: String
                     , flavor :: String } deriving (Show)
```
上記HaskellコードはCommon Lispに於ける`DEFSTRUCT`とおよそ等価である。
以下のようにすれば再現できる。

```lisp
(deftype person()
  (cons-type-specifier '(eql person)'string 'string 'fixnum 'float 'string 'string))
(defstruct(person :named (:type list)(:conc-name nil)(:constructor person)
                  (:copier nil)(:predicate nil))
  (first-name (error "required") :type string)
  (last-name (error "required") :type string)
  (age (error "required") :type fixnum)
  (height (error "required") :type float)
  (phone-number (error "required") :type string)
  (flavor (error "required") :type string))
```
これも`DEFDATA`マクロに統合してしまおう。
マクロの規模が大きくなってきたので、リファクタリングもしておく。

```lisp
;;;; DEFDATA
(defmacro defdata(name &rest constructor*)
  `(EVAL-WHEN(:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
     ,(<deftype> name constructor*)
     ,@(mapcan #`(% '<constructors> name)constructor*)
     ,@(mapcan #'<pattern-matcher> constructor*)
     ',name))

;;; <deftype>
(defun <deftype>(name constructor*)
  (labels((type-name(constructor)
            (cond
              ((list-constructor-p constructor)
               (cons-type-specifier `((eql ,(car constructor)) ,@(cdr constructor))))
              (t (cons-type-specifier
                  `((eql ,(car constructor))
                    ,@(loop :for clause :in (cdr constructor)
                            :collect (or (getf clause :type)
                                         T))))))))
    `(DEFTYPE,name()
       '(OR ,@(mapcar #'type-name constructor*)))))

(defun list-constructor-p(constructor)
  (every #'millet:type-specifier-p (cdr constructor)))

;;; <constructors>
(defun <constructors>(name constructor)
  (cond
    ((list-constructor-p constructor)
     (let((lambda-list(loop :repeat (length(cdr constructor))
                            :collect (gensym))))
       `((DECLAIM(FTYPE(FUNCTION,(cdr constructor),name),(car constructor)))
         (DEFUN ,(car constructor),lambda-list
           (LIST ',(car constructor),@lambda-list)))))
    (t `((DEFSTRUCT(,name :NAMED (:TYPE LIST)
                          (:CONC-NAME NIL)
                          (:COPIER NIL)
                          (:PREDICATE NIL)
                          (:CONSTRUCTOR ,(car constructor)))
           ,@(cdr constructor))))))
```
これで以下のようにも書ける。

```lisp
(defdata person 
  (person (first-name "" :type string)
          (last-name "" :type string)
          (age 0 :type fixnum)
          (height .0 :type Float)
          (phone-number "" :type string)
          (flavor "" :type string)))
```

```haskell
ghci> :t flavor
flavor :: Person -> String
ghci> :t firstName
firstName :: Person -> String
```

```lisp
cl-user> (type-of #'flavor)
FUNCTION
cl-user> (describe #'flavor)
;; Lambda-list: (STRUCTURE)
;; Derived type: (FUNCTION (LIST) (VALUES STRING &OPTIONAL))
cl-user> (inspect #'flavor)
;; The object is a FUNCTION named FLAVOR.
;; 0. Lambda-list: (STRUCTURE)
;; 1. Ftype: (FUNCTION (LIST) (VALUES STRING &OPTIONAL))
> :q
```

```haskell
data Car = Car String String Int deriving (Show)

ghci> Car "Ford" "Mustang" 1967
Car "Ford" "Mustang" 1967

data Car = Car { company :: String
               , model :: String
               , year :: Int
               } deriving (Show)

ghci> Car {company="Ford", model="Mustang", year=1967}
Car {company = "Ford", model = "Mustang", year = 1967}
```
型コンストラクタの名前が衝突するので、ここでは先頭に`.`を付けて回避しておく。

```lisp
cl-user> (defdata .car (.car string string fixnum))
cl-user> (.car "Ford" "Mustang" 1967)
(.CAR "Ford" "Mustang" 1967)
cl-user> (defdata .car (.car (company "" :type string)
                             (model "" :type string)
                             (year 0 :type fixnum)))
cl-user> (.car :company "Ford" :model "Mustang" :year 1967)
(.CAR "Ford" "Mustang" 1967)
```

## 7.4

```haskell
data Maybe a = Nothing | Just a

ghci> Just 3 :: Maybe Int
Just 3

data IntMaybe = INothing | IJust Int
data StringMaybe = SNothing | SJust String
data ShapeMaybe = ShNothing | ShJust Shape
```
上記Haskellにおける型変数は、Common Lispに於ける`DEFTYPE`の引数と解釈できる。
また、無引数の型コンストラクタはそれ自身を表すものであると解釈できるので、Common Lispに於けるキーワードシンボルであると解釈できる。

```lisp
(deftype maybe(a)
  `(or (eql :nothing) ,(cons-type-specifier '(eql just)a)))
(defun just(a)
  (list 'just a))
```
ではこれも`DEFDATA`マクロに統合しよう。

```lisp
;;;; DEFDATA
(defmacro defdata(name lambda-list &rest constructor*)
  `(EVAL-WHEN(:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
     ,(<deftype> name lambda-list constructor*)
     ,@(mapcan #`(% '<constructors> name lambda-list)constructor*)
     ,@(mapcan #'<pattern-matcher> constructor*)
     ',name))

;;; <deftype>
(defun <deftype>(name lambda-list constructor*)
  (labels((optional-lambda-list(list)
            (if(cdr list)
              (list* (car list) '&optional (cdr list))
              list))
          (type-name(args constructor)
            (cond
              ((keywordp constructor)`'(EQL ,constructor))
              (args (comma-type-specifier args constructor))
              ((list-constructor-p constructor)
               `',(cons-type-specifier `((eql ,(car constructor)) ,@(cdr constructor))))
              (t `',(cons-type-specifier
                      `((eql ,(car constructor))
                        ,@(loop :for clause :in (cdr constructor)
                                :collect (or (getf clause :type)
                                             T))))))))
    `(DEFTYPE,name,(optional-lambda-list lambda-list)
       (LIST 'OR ,@(mapcar (lambda(constructor)
                             (type-name lambda-list constructor))
                           constructor*)))))

(defun comma-type-specifier(args constructor)
  ``(cons (eql ,',(car constructor))
         ,,(labels((rec(types)
                    (if(endp types)
                      ''null
                      `(list 'cons ,(let((arg(car types)))
                                      (cond
                                        ((find arg args :test #'eq)arg)
                                        ((millet:type-specifier-p arg)`',arg)
                                        (t
                                          `(list ',(car arg) ,@(cdr arg)))))
                             ,(rec (cdr types))))))
            (rec (cdr constructor)))))

;;; <costructors>
(defun <constructors>(name args constructor)
  (labels((arg-types(lambda-list arg-types &optional acc)
            (if(endp arg-types)
              (nreverse acc)
              (if(find(car arg-types)lambda-list :test #'eq)
                (arg-types lambda-list (cdr arg-types)(push t acc))
                (arg-types lambda-list (cdr arg-types)(push (car arg-types)acc))))))
    (cond
      ((keywordp constructor) nil)
      ((or args (list-constructor-p constructor))
       (let((lambda-list(gensyms(cdr constructor))))
         `(,@(if args
               `((DECLAIM(FTYPE (FUNCTION ,(arg-types args (cdr constructor))
                                          ,(constructor-return-type name))
                                ,(car constructor))))
               `((DECLAIM(FTYPE (FUNCTION,(cdr constructor),name)
                                ,(car constructor)))))
            (DEFUN ,(car constructor),lambda-list
              (LIST ',(car constructor),@lambda-list)))))
      (t `((DEFSTRUCT(,(car constructor)
                       :NAMED (:TYPE LIST) (:CONC-NAME NIL)
                       (:COPIER NIL) (:PREDICATE NIL)
                       (:CONSTRUCTOR ,(car constructor)))
             ,@(cdr constructor)))))))

(defun constructor-return-type(name)
  (list name '*))

(defun gensyms (list)
  (loop :repeat (length list)
        :collect (gensym)))
```
これで以下のようにも書ける。

```lisp
(defdata maybe (a)
  :nothing
  (just a))
```
なお、APIが変ったので注意。
冒頭の`SHAPE`の例は以下のように書かねばならなくなる。

```lisp
(defdata shape ()
  (circle real real real)
  (rectangle real real real real))
```
Haskellのシンタックスからは遠のくが、CLer的には馴染みのあるシンタックスになったかと思う。

### Vector

```haskell
data Vector a = Vector a a a deriving (Show)
```

```lisp
(defdata .vector (a)
  (.vector a a a))
```

```
vplus :: (Num a) => Vector a -> Vector a -> Vector a
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)

dotProd :: (Num a) => Vector a -> Vector a -> a
(Vector i j k) `dotProd` (Vector l m n) = (i*l) + (j*m) + (k*n)

vmult :: (Num a) => Vector a -> a -> Vector a
(Vector i j k) `vmult` m = Vector (i*m) (j*m) (k*m)
```
上記Haskellコードの`(Num a)`に相当する型クラス制約は、少々冗長になるが素のCommon Lispで対応可能である。

```lisp
(declaim(ftype(function((.vector number)(.vector number))(.vector number))vplus))
(trivia:defun-ematch* vplus(v1 v2)
  (((.vector i j k)(.vector l m n))(.vector(+ i l)(+ j m)(+ k n))))

(declaim(ftype(function((.vector number)(.vector number))number)dot-prod))
(trivia:defun-ematch* dot-prod(v1 v2)
  (((.vector i j k)(.vector l m n))(+ (* i l)(* j m)(* k n))))

(declaim(ftype(function((.vector number)number)(.vector number))vmult))
(trivia:defun-ematch* vmult (v m)
  (((.vector i j k) m)(.vector (* i m)(* j m)(* k m))))
```
どうしても冗長なのが気になるなら以下のようにマクロを組めば良い。

```lisp
(defmacro .ftype(name &rest args)
  (multiple-value-bind(args return)(parse-.ftype args)
    `(declaim(ftype(function ,args ,return),name))))

(defun parse-.ftype(args)
  (let((constraint(when(string= '=> (second args))
                    (prog1 (pop args)
		      (pop args))))
       (args(delete '-> args :test (lambda(a b)(and (symbolp b)(string= a b))))))
    (when constraint
      (setf args (subst (car constraint)(cadr constraint) args)))
    (values (butlast args)
            (car(last args)))))

(.ftype vplus (number a) => (.vector a) -> (.vector a) -> (.vector a))
```
気を付けなければいけない点として、上のマクロはHaskellのシンタックスをなぞって模倣しただけのものであり、そのセマンティクスまでは再現できていないという点である。
すなわち、カリー化された関数となるわけではない。

## 7.5
### deriving
### Eq

```haskell
data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     } deriving (Eq)

mikeD = Person {firstName = "Michael", lastName = "Diamond", age = 43}
adRock = Person {firstName = "Adam", lastName = "Horovitz", age = 41}
mca = Person {firstName = "Adam",lastName = "Yauch",age = 44}

ghci> mca == adRock
False
ghci> mikeD == adRock
False
ghci> mikeD == mikeD
True
ghci> mikeD == Person {firstName="Michael", lastName="Diamond", age=43}
True

ghci> let beastieBoys = [mca, adRock, mikeD)
ghci> mikeD `elem` beastieBoys
True
```
Haskellに於ける上記`deriving`キーワードは、Common Lispに於いては現時点では必要ない。
特に指定なくとも`EQUAL`がそのまま使える。

```lisp
(defdata person ()
  (person (first-name "" :type string)
          (last-name "" :type string)
          (age 0 :type fixnum)))
(defvar mike-d (person :first-name "Michael" :last-name "Diamond" :age 43))
(defvar ad-rock (person :first-name "Adam" :last-name "Horovitz" :age 41))
(defvar mca (person :first-name "Adam" :last-name "Yauch" :age 44))

cl-user> (equal mca ad-rock)
NIL
cl-user> (equal mike-d ad-rock)
NIL
cl-user> (equal mike-d mike-d)
T
cl-user> (equal mike-d (person :first-name "Michael" :last-name "Diamond" :age 43))
T

cl-user> (defvar beastie-boys (list mca ad-rock mike-d))
cl-user> (find mike-d beastie-boys)
NIL
cl-user> (find mike-d beastie-boys :test #'equal)
(PERSON "Michael" "Diamond" 43)
```
### Show Read

```haskell
data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     } deriving (Eq, Show, Read)
```
Haskellに於ける上記`Show`、`Read`型クラスインスタンスの指定も、Common Lispに於いては現状必要ない。

```haskell
ghci> mikeD
Person {firstName = "Michael", lastName = "Diamond", age = 43}
ghci> "mikeD is: " ++ Show mikeD
"mikeD is: Person {firstName = \"Michael\", lastName = \"Diamond\", age = 43}"
```
Haskellに於ける`Show`はCommon Lispではおよそ`PRIN1-TO-STRING`が相当する。

```lisp
cl-user> mike-d
(PERSON "Michael" "Diamond" 43)
cl-user> (uiop:strcat "mikeD is: "(prin1-to-string mike-d))
"mikeD is: (PERSON \"Michael\" \"Diamond\" 43)"
```
なお、同様に`PRINC-TO-STRING`、`WRITE-TO-STRING`も存在する。
`PRINC`は人間にとって読みやすい形で表示する。

```lisp
cl-user> (princ :hoge)
HOGE ; <--- side effect
:HOGE ; <--- return value
cl-user> (princ "hoge")
hoge ; <--- side effect
"hoge" ; <--- return value
```

`WRITE`は動的にスペシャル変数の影響を受ける。

```lisp
cl-user> (let((*print-escape* t))
           (write :hoge))
:HOGE
:HOGE
cl-user> (let((*print-escape* nil))
           (write :hoge))
HOGE
:HOGE
```

```haskell
mysteryDude = "Person { firstName = \"Michael\", lastName = \"Diamond\", age = 43}"
ghci> read mysteryDude :: Person
Person {firstName = "Michael", lastName = "Diamond", age = 43}
```
Haskellに於ける`read`はCommon Lispに於ける`READ-FROM-STRING`に相当する。

```lisp
(defvar mystery-dude "#.(person :first-name \"Michael\" :last-name \"Diamond\" :age 43)")
cl-user> (read-from-string mystery-dude)
(PERSON "Michael" "Diamond" 43)
61
```
第ニ返り値の`61`は消費した文字数である。

文字列の先頭に`#.`が指定されている点要注意。
これがなければ以下のような不正なデータ型になってしまう。

```lisp
cl-user> (read-from-string "(person :first-name \"Michael\" :last-name \"Diamond\" :age 43)")
(PERSON :FIRST-NAME "Michael" :LAST-NAME "Diamond" :AGE 43)
59
```

### Order

```haskell
data Bool = Fails | True deriving (Ord)

ghci> True `compare` False
GT
ghci> True > False
True
ghci> True < False
False
```
上記Haskellに於ける`compare`とその背後にある仕組みは、Common Lispには存在しない。

`DEFDATA`マクロに組み込もう。

```lisp
;;;; DEFDATA
(defmacro defdata(name lambda-list &rest constructor*)
  `(eval-when(:compile-toplevel :load-toplevel :execute)
     ,(<deftype> name lambda-list constructor*)
     (SETF(GET ',name 'ADT)T)
     ,@(mapcan #`(% #'<constructors> name lambda-list)constructor*)
     ,@(loop :for c :in constructor*
             :for o :upfrom 0
             :collect (<meta-info-setter> c o lambda-list name))
     ,@(mapcan #'<pattern-matcher> constructor*)
     ',name))

;;;; ADT data structure
(defstruct(adt (:copier nil)(:predicate nil))
  (order (error "required") :type fixnum :read-only t))

;;; <meta-info-setter>
(defun <meta-info-setter>(constructor order lambda-list name)
  (let((c (alexandria:ensure-car constructor)))
    `(SETF (GET ',c 'adt-meta-info)
           (MAKE-ADT :ORDER ,order))))
```
`compare`の実装は後述。

### week

```haskell
data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
             deriving (Eq, Ord, Show, Read, Bounded, Enum)

ghci> Wednesday
Wednesday
ghci> show Wednesday
"Wednesday"
ghci> read "Saturday" :: Day
Saturday

ghci> Saturday == Sunday
False
ghci> Saturday == Saturday
True
ghci> Saturday > Friday
True
ghci> Monday `compare` Wednesday
LT

ghci> minBound :: Day
Monday
ghci> maxBound :: Day
Sunday

ghci> succ Monday
Tuesday
ghci> pred Saturday
Friday
ghci> [Thursday..Sunday)
[Thursday,Friday,Saturday,Sunday)
ghci> [minBound .. maxBound) :: [Day)
[Monday,Tuesday,Wednesday,Thursday,Friday,Saturday,Sunday)
```
こいつも後に譲る。

## 7.6
### Type synonym

```haskell
type String = [Char]
```
上記Haskellコードに於ける型シノニムの機能は、Common Lispに於ける`DEFTYPE`のそれである。

```lisp
(deftype string ()'(vector character))
```
### Phone book

```haskell
type PhoneNumber = String
type Name = String
type phoneBook = [(Name, PhoneNumber)]
```
Haskellに於ける上記コードをCommon Lispに翻訳するには少々迂遠な道程を辿らねばならない。
というのも、同一の型からなるリストを表す簡単な方法が無いからだ。

```lisp
(deftype phone-number()'string)
(deftype name()'string)
(deftype phone-book()
  '(satisfies phone-bookp))
(defun phone-bookp(arg)
  (and (listp arg)
       (ignore-errors (every (lambda(elt)
                               (and (consp elt)
                                    (typep (car elt)'name)
                                    (typep (cdr elt)'phone-number)))
                             arg))))
```

```haskell
inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pnumber pbook = (name, pnumber) `elem` pbook
```

```lisp
(declaim(ftype(function(name phone-number phone-book)boolean)in-phone-book-p))
(defun in-phone-book-p(name pnumber pbook)
  (find (cons name pnumber) pbook :test #'equal))
```

### Complex synonym

```haskell
type AssocList k v = [(k,v)]
```
前節でも述べた通り、Common Lispでは同一の型からなるリストを表す簡単な方法はない。
上記Haskellコードと等価なものはCommon Lispでは書けない。
というのも、`SATISFIES`の引数はシンボルでなくてはならないからだ。
すなわち、引数を渡す手段がない。

Common Lispに於いては、リストというものは様々な型のオブジェクトを任意個格納するためのもの、と割り切っているように見える。
なお、Common Lispに於いては、同様の型のオブジェクトを任意個格納するためのものとして`SIMPLE-VECTOR`が想定されているようだ。
例えば、上記haskellコードは、`VECTOR`を使えばCommon Lispで以下のように書ける。

```lisp
(deftype assoc-vector(k v)
  `(vector (cons ,k ,v)))
```

### Either

```haskell
data either a b = Left a | Right b deriving (Eq, Ord, Read, Show)
```

```lisp
(defdata either (a b)
  (left a)
  (right b))
```

```haskell
data LockerState = Taken | Free deriving (Show, Eq)

type Code = String

type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map = case Map.lookup lockerNumber map of
    Nothing -> Left $ "Locker " ++ show lockerNumber ++ " doesn't exist!"
    Just (state, code) -> if state /= Taken
                             then Right code
                             else Left $ "Locker " ++ shoe lockerNumber ++ " is already taken!"
```

```lisp
(defdata locker-state () :taken :free)
(deftype code () 'string)
(deftype locker-map()'hash-table)

(declaim(ftype(function(fixnum locker-map)(either string code))loker-lookup))
(defun locker-lookup(locker-number map)
  (multiple-value-bind(key exist?)(gethash locker-number map)
    (if exist?
      (destructuring-bind(state . code)key
        (if (not(eq state :taken))
          (right code)
          (left (uiop:strcat "Locker " (princ-to-string locker-number) " is already taken!"))))
      (left(uiop:strcat "Locker " (princ-to-string locker-number) " doesn't exist!")))))
```

## 7.7
### Recursive data type

```haskell
data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)
```
上記Haskellコードのような再帰的な型構造をCommon Lispで表すことは不可能である。
たとえば以下のような直訳が考えられるかと思う。

```lisp
(deftype .list(a)
  `(or null (cons ,a (.list ,a))))
```
実際これで動く処理系もある。（ECL、CLISP）
だが、動かない処理系もある。（CCL、SBCL）

CLHSには以下のようにある。

> Recursive expansion of the type specifier returned as the expansion must terminate, including the expansion of type specifiers which are nested within the expansion.

再帰的な型構造を表せるのは本当に羨ましい。
### Tree 
前節と同じ理由で本節も再現不可能なので省略。

## 7.8
Haskellに於ける型クラスという機能の、筆者個人の解釈を述べる。
正しいかどうかは知らない。

まず、スタート地点として１章で取り上げた等値関数の問題を考える。
すなわち、等値性はアプリケーションによるので、それ用の関数を定義すべきだ、という考えである。
これは正論であるが、デメリットを伴う。
プログラマが行いたいのは、オブジェクトが同じか否かの比較であるだけなのに、`A-EQUAL`, `B-EQUAL`,,, `N-EQUAL`と大量の名前を必要とする。
なんにせよやりたいことは等値比較なので、同じ名前で行いたい。

この問題に対する一つの回答がオブジェクト指向によりもたらされた。
あらゆるデータはオブジェクトであり、 振る舞いはオブジェクトが知っている。
アプリケーションによる特殊な等値比較関数が必要なら、メソッドとしてオブジェクトに持たせればいい。
既存のものが使えるなら、継承すればいい。
これにより、関数の名前は、例えば、`==`一つあればいい。

ただしこれにもデメリットがつきまとう。
メソッドのディスパッチは実行時に行われるので、それがオーバーヘッドとなる。
その問題に対する一つの回答こそ、型クラスの一側面であると筆者は考える。

型というものは、コンパイラに対するヒントである。
型情報が充分コンパイラに与えられていれば、メソッドのディスパッチをコンパイル時に行える。
なお、デメリットは型の情報が正しく指定されていなければならないという点だ。
スタート地点では名前に型情報がプリフィックスとして付いていると考えれば、ぐるっと一周して戻ってきた感はある。
しかしながら型推論を充分行えるコンパイラであれば、型情報の記述は減らせるので、トータルで見れば記述量は減らせる事が出来るのかもしれない。

Common Lispにそのような機能は存在しない。
近いことを行うライブラリとしてはinlined-generic-functionがある。
Inlined-generic-functionは優秀なプロダクトであるが、その欠点はそれが総称関数である点だ。
すなわち、メソッドはクラスの単位でしか作られない。

Common Lispはオブジェクト指向もサポートしている言語である。
CLOSと呼ばれるそれは、もともとあったLispの上にLispを使って作られたものだ。
それ自体はCommon Lispという言語の強力さを表しているが、それ故にCommon Lispには負債が伴ってもいる。
もともとあった型システムと、CLOSのクラスシステムとの互換性が完全でないのだ。

１というデータがある。
これの型は何だろうか。
Common Lispに於いて、１は`BIT`という型になる。
そして`BIT`はクラスではない。
よって`BIT`にスペシャライズドされたメソッドは作れない。
同様に`(unsigned-byte 8)`にスペシャライズドされたメソッドも作れないし、`(cons fixnum fixnum)`にスペシャライズドされたメソッドも作れない。

これまで見てきたように、本章で作ってきた`DEFDATA`というマクロは、古式ゆかしき型システムに依存しているものである。
よって、inlined-generic-functionとは別に自前で作らねばならない。

### draft
ではコードを書き始める前に、想定される仕様の策定を行おう。

#### (define-type-class(name &rest vars)super-classes methods &rest rest)
CLOSに於ける`DEFCLASS`と`DEFGENERIC`とを統合したものに相当するマクロとして`DEFINE-TYPE-CLASS`を考える。
`DEFINE-TYPE-CLASS`は、インターフェイスとなる関数とそのコンパイラマクロに展開されるマクロだ。
また、interfaceにテーブルを紐付ける役割も行う。

コンパイラマクロは、コンパイル時にまず型制約のチェックを行う。
（それは`CHECK-SIGNATURE`で行われる。）
破綻していればエラーとする。
さもなくばインスタンスを探す。
（それは`GET-INSTANCE-LAMBDA`で行われる。）
見つからなければ`WARNING`を発する。
さもなくばインターフェイスを見つかったラムダフォームで置き換える。

インターフェイス関数は、実行時に受け取った引数の型をキーにして、インスタンステーブルからラムダフォームを探してくる。
（これは`GET-INSTANCE-LAMBDA`で行われる。）
見つからなければエラーを発する。
さもなくば見つかったインスタンスに実引数を適用させる。

#### (definstance interface instance-lambda-list &body body)
CLOSに於ける`DEFMETHOD`に相当するマクロとして`DEFINSTANCE`を考える。
`DEFINSTANCE`は、インターフェイス関数にインスタンスラムダを紐付けるマクロだ。
（それは`ADD-INSTANCE`で行われる。） 

#### (get-instance-lambda interface type\*)
型のリストをキーにinterface（シンボル）からラムダフォームを探す。
まず、適応可能な全てのインスタンスを集める。
（それは`COLLECT-INSTANCE`で行われる。）
しかる後に最も特定的なインスタンスを選択する。
（それは`COMPUTE-APPLICABLE-INSTANCE`で行われる。）
interfaceからテーブルを取り出すためには`INSTANCE-TABLE`を使う。
テーブルは、キーに型のリスト、値にラムダフォームが入っているものとする。
見つからなければ`NIL`を返す。

#### (collect-instance type\* interface)
型のリストをキーに、interfaceに登録されている適用可能なインスタンスを全て集める。

#### (compute-applicable-instance list)
最も特定的なインスタンスを返す。

#### (add-instance interface type\* lambda-form)
インターフェイスに紐付けられたテーブルに型リストをキーにしてラムダフォームを登録する。

#### (instance-table interface)
インターフェイスに紐付けられたインスタンステーブルを返す。

#### (check-signature signature type\*)
type\*がsignatureを満たすかチェックする。
満たさなくばエラーを発する。

### implementation
では実装に移ろう。

コンパイル時に何らかのデータを取り扱いたい場合、そのデータはシンボルに紐づけておくのがセオリーだ。
まずは、それらのデータ型から作る。

定義される型クラス名となるシンボルに紐付けられるデータ構造。

```lisp
;;;; TYPE-CLASS OBJECT
(defstruct(type-class(:constructor make-info)(:copier nil)
                     (:predicate nil)(:conc-name type-))
  (name (error "Name is required.") :type symbol :read-only t)
  (direct-superclasses nil :type list)
  (direct-subclasses nil :type list))
```

CLOSで言うところの総称関数、その名前シンボルに紐付けられるデータ構造。

```lisp
;;;; INSTANCE OBJECT
(defstruct(type-class-instance(:constructor instance-info)(:copier nil)
                              (:predicate nil))
  (lambda-list nil :type list :read-only t)
  (return-type nil :type (or list symbol):read-only t)
  (type-class (error "required") :type symbol :read-only t)
  (table nil :type list)
  (default nil :type list :read-only t))
```

CLOSで言うところの総称関数、その名前シンボルの事を上記仕様では`INTERFACE`と呼んでいた。
ラッパがあった方が便利だ。

```lisp
(defun instance-type-class(interface)
  (type-class-instance-type-class(get interface 'instance)))

(defun instance-default(interface)
  (type-class-instance-default(get interface 'instance)))

(defun instance-lambda-list(interface)
  (type-class-instance-lambda-list(get interface 'instance)))

(defun instance-return-type(interface)
  (type-class-instance-return-type(get interface 'instance)))

(defun instance-p(interface)
  (get interface 'instance))
```
`INSTANCE-TABLE`に関してはSETF関数も定義しておく。

```lisp
;;;; INSTANCE-TABLE
(defun instance-table(interface)
  (type-class-instance-table(get interface 'instance)))
(defun (setf instance-table)(new interface)
  (setf(type-class-instance-table(get interface 'instance))new))
```
筆者は、丁寧にリファクタリングされたマクロはBNFに近くなるという印象を持っている。
マクロ`DEFINE-TYPE-CLASS`は大変多くの仕事を請け負うものではあるが、それでも読みにくくはないはずだ。

```Lisp
;;;; DEFINE-TYPE-CLASS
(defmacro define-type-class((name &rest vars)super-classes methods &rest rest)
  ;; trivial syntax checking.
  (assert(every #'symbolp vars))
  ;; as canonicalize
  (map-into vars #'envar vars)
  ;; body
  `(EVAL-WHEN(:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
     (SETF(GET ',name 'TYPE-CLASS)(MAKE-INFO :NAME ',name))
     ,@(when super-classes
         (<type-class-relation-setter> name super-classes))
     ,@(loop
         :for (method lambda-list return-type) :in methods
         :for gensyms = (gensyms lambda-list)
         :do (setf ; as canonicalise
               lambda-list (patternize lambda-list)
               return-type (patternize return-type))
         :collect (<instance-info-setter> method name lambda-list return-type rest)
         :collect (<instance-compiler-macro> method gensyms lambda-list return-type)
         :collect (<instance-interpreter> method gensyms lambda-list))
     ',name))

(defun envar(thing)
  (intern(format nil "?~A"thing)))

(defun patternize(thing)
  (if(millet:type-specifier-p thing)
    thing
    (if(listp thing)
      (trestrul:mapleaf #'patternize thing)
      (envar thing))))
```
スーパークラスが指定されているようならその親子関係を各データ構造に登録する、そのためのフォームを生成する。

```lisp
;;; <type-class-relation-setter>
(defun <type-class-relation-setter>(name super-classes)
  `((SETF (TYPE-DIRECT-SUPERCLASSES(GET ',name 'TYPE-CLASS))',super-classes)
    ,@(loop :for type-class :in super-classes
            :collect
            `(PUSH ',name (TYPE-DIRECT-SUBCLASSES(GET ',type-class 'TYPE-CLASS))))))
```
型クラスは各々メソッドを持っている。
それらメソッドの名前シンボルにメタ情報を登録するフォームを生成する。

```lisp
;;; <instance-info-setter>
(defun <instance-info-setter(method name lambda-list return-type rest)
  `(SETF (GET ',method 'INSTANCE)
         (INSTANCE-INFO :TYPE-CLASS ',name
                        :LAMBDA-LIST ',lambda-list
                        :RETURN-TYPE ',return-type
                        ,@(let((default(find method rest :key #'cadr)))
                            (when default
                              `(:DEFAULT '(LAMBDA,@(cddr default))))))))
```
型クラスに所属している各メソッドに対し働くコンパイラマクロを定義するフォームを生成する。

```lisp
;;; <instance-compiler-macro>
(defun <instance-compiler-macro>(method gensyms lambda-list return-type)
  `(DEFINE-COMPILER-MACRO,method(&WHOLE WHOLE ,@gensyms &ENVIRONMENT ENV)
     (SETF ; as canonicalise. In order to retrieve return type.
       ,@(loop :for gensym :in gensyms
               :append `(,gensym (EXPANDER:EXPAND ,gensym))))
     (LET((INFOS(COMPUTE-RETURN-TYPES (list ,@gensyms) ENV)))
       ,@(when(cdr lambda-list)
           `((CHECK-SIGNATURE ',lambda-list INFOS)))
       (LET((IL(GET-INSTANCE-LAMBDA ',method INFOS)))
         (IF IL
             ,(if(millet:type-specifier-p return-type)
                `(LIST 'THE ',return-type (LIST IL ,@gensyms))
                `(LET((RETURN(SUBSTITUTE-PATTERN ',return-type (UNIFY:UNIFY ',lambda-list (SUBST '_ '* INFOS)))))
                   (IF RETURN
                       (LIST 'THE RETURN (LIST IL ,@gensyms))
                       (LIST IL ,@gensyms))))
             (PROGN (WHEN *COMPILE-FILE-PATHNAME*
                      (WARN "Can not get instance of ~S" WHOLE))
                    ;; In order to avoid expanding macros twice,
                    ;; we should use canonicalized `GENSYMS`.
                    ;; And in order to trick `compiler-macroexpand-1` returns nil
                    ;; as second value, we must destructively modify `WHOLE`.
                    ;; Or `compiler-macroexpand` get into infinite expanding.
                    (RPLACD WHOLE (LIST ,@gensyms))))))))
```
ここでは仕様になかったヘルパー（しかもそれなりに規模の大きなもの）が大量に必要になる。
見通しが甘かったとも言えるが、あまりにがんじがらめの仕様にしてしまうと実装においての工夫のしどころがなくなってしまう。
（特に最適化まわりに関して。）
この辺はさじ加減が難しいところ。

また、これらのヘルパーを実装するにあたり必要な情報をコンパイル時に得られるようにするため、`DEFDATA`マクロにも手を入れなければならない。
具体的には、ADTがリテラルで書かれていた場合、その値から対応する型を特定できねばならない。

ADTに関するメタインフォを格納するデータ構造。

```lisp
(defstruct(adt (:copier nil)(:predicate nil))
  (type-of (error "required") :read-only t :type (or symbol list))
  (types (error "required") :read-only t :type (or symbol list))
  (order (error "required") :type fixnum :read-only t))
```
各種trivialなヘルパー。

```lisp
(defun adv-p(thing)
  (or (when(keywordp thing)
        (get thing 'adt-meta-info))
      (when(and (listp thing)
                (symbolp (car thing)))
        (get (car thing) 'adt-meta-info))))

(defun data-type-of(thing)
  (let((adt(adv-p thing)))
    (if adt
      (let((types(adt-types adt)))
        (if(null types)
          (adt-type-of adt)
          (if(eq 'eql (car types))
            (adt-type-of adt)
            (list* (car (adt-type-of adt))
                   (mapcar (lambda(type value)
                             (if(unify:variablep type)
                               (data-type-of value)
                               type))
                           (adt-types adt)
                           (cdr thing))))))
      (if(not(functionp thing))
        (class-name(class-of thing))
        (let((name(millet:function-name thing)))
          (if name
            (introspect-environment:function-type name)
            'function))))))

(defun data-order(thing)
  (let((adt(adv-p thing)))
    (when adt
      (adt-order adt))))
```
DEFDATAに関する変更は`<meta-info-setter>`のみで良い。
コンストラクタシンボルにメタインフォを登録する、そのためのフォームを生成する。

```lisp
;;; <meta-info-setter>
(defun <meta-info-setter>(constructor order lambda-list name)
  (let((c (alexandria:ensure-car constructor)))
    `(SETF (GET ',c 'adt-meta-info)
           (MAKE-ADT :TYPE-OF ',(if lambda-list
                                  (constructor-return-type name)
                                  name)
                     :TYPES ',(arg-types constructor lambda-list)
                     :ORDER ,order))))

(defun arg-types(constructor args)
  (cond
    ((keywordp constructor) `(eql ,constructor))
    ((or args (list-constructor-p constructor))
     (sublis (mapcar (lambda(elt)
                       (cons elt (Envar elt)))
                     args)
             (cdr constructor)))
    (t (mapcar (lambda(slot)
                 (if(symbolp slot)
                   t
                   (getf slot :type t)))
               (cdr constructor)))))
```
定数フォームから取り出した値をラップしておくための構造体。

```lisp
(defstruct(constant (:constructor wrap-value(value))(:copier nil))
  (value nil :read-only t))
```
内部エラー用コンディション。

```lisp
(define-condition internal-logical-error(cell-error)
  ((datum :initarg :datum :accessor error-datum))
  (:report(lambda(c *standard-output*)
            (format t "INTERNAL LOGICAL ERROR: ~S~%~S trapped with ~S."
                    (type-of c)
                    (cell-error-name c)
                    (error-datum c)))))
(define-condition exhausts-clauses(internal-logical-error)())
(define-condition unexpected-macro(internal-logical-error)())
(define-condition unexpected-quote(internal-logical-error)())
(define-condition unexpected-local-macro(internal-logical-error)())
```

フォームの返り値を求める。
コンパイラマクロフォームの可読性を高めるため、利便性の高いAPIを提供する役割を担う。

```lisp
(defun compute-return-types(var* &optional env)
  (loop :for var :in var*
        :collect (compute-return-type var env)))

```
実際の各処理、特に条件分岐を担当する。

```lisp
(defun compute-return-type(var &optional env)
  (cond
    ((constantp var) ; lisp object.
     (let((value(introspect-environment:constant-form-value var env)))
       (if(adv-p value) ; literal adt.
         (data-type-of value)
         (wrap-value value))))
    ((symbolp var) ; free variable.
     (or (introspect-environment:variable-type var env)
         T))
    ((and (listp var) ; instance call.
          (instance-p (car var)))
     (compute-instance-call-return-type var))
    ((and (listp var)
          (symbolp (car var)))
     (compute-standard-form-return-type var env))
    (t (error 'exhausts-clauses :name 'compute-return-type :datum var))))
```
インスタンスコールフォームから返り値を求める。
ここでユニフィケーションを行えるようにするために、`DEFINE-TYPE-CLASS`マクロでは変数を事前に論理変数化してある。
`COMPUTE-RETURN-TYPE`の可読性を上げるため、利便性の高いAPIを提供する役目を担う。

```lisp
(defun compute-instance-call-return-type(call-form)
  (let((pattern(instance-return-type(car call-form)))
       (environment(unify:unify (instance-lambda-list(car call-form))
                                (subst '_ '* (compute-return-types(cdr call-form))))))
    (substitute-pattern pattern environment)))
```
ユニフィケーションの結果に基づきパターンを置き換える。

```lisp
(defun substitute-pattern(pattern environment)
  (trestrul:asubst-if (lambda(var)
                        (let((return-type (unify:find-variable-value var environment)))
                          (if(typep return-type '(cons (eql values)t))
                            (cadr return-type)
                            return-type)))
                      #'unify:variablep
                      pattern))
```
通常のリスプフォームから返り値を求める。
条件分岐を担う。

```lisp
(defun compute-standard-form-return-type(form env)
  (multiple-value-bind(type localp declaration)(introspect-environment:function-information (car form)env)
    (declare(ignore localp))
    (case type
      ((nil) (warn "Undefined function ~S. ~S"(car form)form))
      (:special-form (special-operator-return-type form env))
      (:macro (error 'unexpected-macro :name 'compute-standard-form-return-type
                     :datum form))
      (:function
        (let((ftype(assoc 'ftype declaration)))
          (if ftype
            (ftype-return-type (cdr ftype))
            (progn (warn "Could not determine type of ~S"form)
                   T)))))))
```
特殊形式フォームから返り値の型を求める。

```lisp
(defun special-operator-return-type(form env)
  (case (car form)
    ((progn progv let let* flet labels lambda setq locally eval-when)
     (compute-return-type(car(last form))env))
    ((the)(canonicalize-return-type(second form)))
    ((unwind-protect multiple-value-prog1 multiple-value-call load-time-value)
     (compute-return-type(second form)env))
    ((tagbody)'null)
    ((function)
     (if(listp (second form))
       (compute-return-type(car(last(second form)))env)
       (introspect-environment:function-type(second form)env)))
    ((if)
     `(or ,(compute-return-type(third form)env)
          ,(compute-return-type(fourth form)env)))
    ((quote)
     (error 'unexpected-quote :datum form :name 'special-operator-return-type))
    ((macrolet symbol-macrolet)
     (error 'unexpected-local-macro :datum form :name 'special-operator-return-type))
    (otherwise ; give up. (go throw catch return-from block)
      t)))
```
`FTYPE`フォームから返り値の型を求める。

```lisp
(defun ftype-return-type(form)
  (if(symbolp form)
    T
    (canonicalize-return-type(third form))))
```
リターンタイプフォームを正規化する。

```lisp
(defun canonicalize-return-type(return-type)
  (flet((ensure-t(thing)
          (if(eq '* thing)
            T
            thing)))
    (if(symbolp return-type)
      (ensure-t return-type)
      (if(eq 'values (car return-type))
        (ensure-t (cadr return-type))
        return-type))))
```
コンパイルせずにソースがロードされた場合、もしくは実行時にならないと型情報を得られない場合など、実行時に呼び出されるべきインタープリタ関数を定義するフォームを生成する。

```lisp
;;;; <instance-interpreter>
(defun <instance-interpreter>(method gensyms lambda-list)
  `(DEFUN,method,gensyms
     ,@(when(cdr lambda-list)
         `((CHECK-SIGNATURE ',lambda-list (LIST ,@(mapcar (lambda(sym)
                                                            `(DATA-TYPE-OF ,sym))
                                                          gensyms)))))
     (LET((INSTANCE(OR (GET-INSTANCE-LAMBDA ',method (LIST ,@(loop :for s :in gensyms
                                                                   :collect `(DATA-TYPE-OF ,s))))
                       (INSTANCE-DEFAULT ',method))))
       (IF INSTANCE
           (LET((DECLARED(INSERT-DECLARE INSTANCE (LIST ,@gensyms))))
             (FUNCALL (COERCE DECLARED 'FUNCTION)
                      ,@gensyms))
           (ERROR "Instance is not found. ~S ~S"',method (LIST ,@gensyms))))))
```
`GET-INSTANCE-LAMBDA`が返したラムダフォームに型情報を付与する。

```lisp
(defun insert-declare(form values)
  (labels((actual-type(type value)
            (if(listp value)
              (rec(cdr type)(cdr value)(list (car type)))
              type))
          (rec(type value acc)
            (if (endp value)
              (nreconc acc type)
              (body (car type)(cdr type)(car value)(cdr value)acc)))
          (body(type type-rest value value-rest acc)
            (if(eq '* type)
              (rec type-rest value-rest (push (class-name(class-of value))acc))
              (rec type-rest value-rest (push type acc))))
            )
    `(,@(subseq form 0 2)
       (DECLARE,@(mapcar (lambda(value var)
                           (if(adv-p value)
                             (let((type(data-type-of value)))
                               (if(listp type)
                                 `(TYPE ,(actual-type type value),var)
                                 `(TYPE ,type ,var)))
                             `(TYPE ,(type-of value),var)))
                         values
                         (cadr form)))
       ,@(subseq form 2))))
```
インスタンスメソッドが持つ型シグネチャと型情報が一致するかチェックする。
`CHECK-TYPE`同様返り値に意味はない。
可読性向上のために利便性の高いAPIを提供する役目を担う。

```lisp
;;;; CHECK-SIGNATURE
(defun check-signature($pattern $type*)
  (labels((rec(pattern type* acc)
            (if(endp pattern)
              (if(endp type*)
                T
                (error "Unmatch length ~S ~S" $pattern $type*))
              (if(endp type*)
                (error "Unmatch length ~S ~S" $pattern $type*)
                (body (car pattern)(cdr pattern)(car type*)(cdr type*)acc))))
          (body(pat pat-rest type type-rest acc)
            (let((seen(assoc pat acc :test #'equal)))
              (if seen
                (if(compatible-type-p type (cdr seen))
                  (rec pat-rest type-rest (progn (pushnew type (cdr seen):test #'equalp)
                                                 acc))
                  (error "Uncompatible type ~S ~S"type seen))
                (rec pat-rest type-rest (push(%check pat type)acc)))))
          )
    (rec (cdr $pattern)(cdr $type*)`(,(%check(car $pattern)(car $type*))))))
```
例えば`FIXNUM`は`INTEGER`のサブタイプである。
サブタイプは受け入れられるべきである。
それらをチェックするための述語。
これは利便性のためのAPIを担う。

```lisp
(defun compatible-type-p(type type*)
  (loop :for t1 :in type*
        :always (%compatible-type-p t1 type)))
```
実際の判定処理。

```lisp
(defun %compatible-type-p(t1 t2)
  (if(constant-p t1)
    (if(constant-p t2)
      (%compatible-type-p (type-of(constant-value t1))
                          (type-of(constant-value t2)))
      (typep (constant-value t1)t2))
    (if(constant-p t2)
      (typep (constant-value t2)t1)
      (subtypep t1 t2))))
```
各シグネチャと型との整合性をチェックする。

```
(defun %check(pattern type)
  (if(listp pattern)
    (if(eq 'function (car pattern))
      (cond
        ((constant-p type)
         (let((ftype(cdr(assoc 'ftype (nth-value 2 (introspect-environment:function-information (constant-value type)))))))
           (unify:unify pattern (subst '_ '* ftype))
           (list pattern ftype)))
        ((typep type '(cons (eql function) t))
         (unify:unify pattern (subst '_ '* type))
         (list pattern type))
        ((or (eq t type)(eq 'function type))
         (warn "Could not match ~S ~S"pattern type)
         (list pattern type))
        (t (error "%CHECK: Unknown type comes.~%TYPE: ~S" type)))
      (progn (unify:unify pattern (subst '_ '* type))
             (list pattern type)))
    (list pattern type)))
```
型情報を基に、インターフェイスからラムダフォームを求める。

```lisp
;;;; GET-INSTANCE-LAMBDA
(defun get-instance-lambda(interface type*)
  (if(every (lambda(x)
              (eq t x))
            type*)
    nil
    (or (compute-applicable-instance(collect-instance type* interface))
        (instance-default interface))))
```
適応可能なインスタンスを集める。

```lisp
;;;; COLLECT-INSTANCE
(defun collect-instance(type* interface)
  (remove-if-not (lambda(ts2)
                   (every #'%compatible-type-p type* ts2))
                 (instance-table interface)
                 :key #'car))
```
最も特定的なインスタンスを求める。

```lisp
;;;; COMPUTE-APPLICABLE-INSTANCE
(defun compute-applicable-instance(list)
  (cdar(sort-instance list)))

(defun sort-instance(list)
  (flet((type<(ts1 ts2)
          (every #'subtypep ts1 ts2)))
    (sort list #'type< :key #'car)))
```
インスタンスを定義する。

```lisp
;;;; DEFISTANCE
(defmacro definstance(interface instance-lambda-list &body body)
  (flet((parse-lambda-list(lambda-list)
          (loop :for elt :in lambda-list
                :collect (car elt) :into vars
                :collect (let((type(cadr elt)))
                           (if(or (adt-p type)
                                  (find-class type nil))
                             type
                             (error "Invalid type")))
                :into types
                :finally (return (values vars types)))))
    (multiple-value-bind(vars types)(parse-lambda-list instance-lambda-list)
      (let((scs(type-direct-superclasses(get(instance-type-class interface)'type-class))))
        (dolist(sc scs)
          (assert(or (get-instance-lambda sc types)
                     (instance-default sc)))))
      `(progn (add-instance ',interface
                            ',types
                            '(lambda,vars,@body))
              ',interface))))
```
インターフェイスにインスタンスを登録する。

```lisp
;;;; ADD-INSTANCE
(defun add-instance(interface type* lambda-form)
  (push(cons type* lambda-form)(instance-table interface)))
```

### type class

```haskell
class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool
    x == y = not (x /= y)
    x /= y = not (x == y)
```

```lisp
(define-type-class(eq a)()
  ((==(a a) boolean)
   (/==(a a) boolean))
  (:default == (x y)
    (not (/== x y)))
  (:default /== (x y)
    (not (== x y))))
```

### Trafic light

```haskell
data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where
    Red == Red = True
    Green == Green = True
    Yellow == Yellow = True
    _ == _ = False
instance (Eq m) => Eq (Maybe m) where
    Just x == Just y = x == y
    Nothing == Nothing = True
    _ == _ = False
```

```lisp
(defdata traffic-light()
  :red :yellow :green)

(definstance == ((a traffic-light)(b traffic-light))
  (trivia:match*(a b)
    ((:red :red)T)
    ((:green :green)T)
    ((:yellow :yellow)T)
    ((_ _) nil)))

(definstance == ((a (maybe *))(b (maybe *)))
  (trivia:match*(a b)
   ((:nothing :nothing)T)
   (((just x)(just y))(== x y))
   ((_ _)nil)))
```

## 7.9
### Yes No

```haskell
class YesNo a where
    yesno :: a -> Bool

instance YesNo Int where
    yesno 0 = False
    yesno _ = True

instance YesNo [a) where
    yesno [) = False
    yesno _ = True

instance YesNo bool where
    yesno = id

instance YesNo (Maybe a) where
    yesno (just _) = True
    yesno Nothing = False

instance YesNo (Tree a) where
    yesno EmptyTree = False
    yesno _ = True

instance YesNo TrafficLight where
    yesno Red = False
    yesno _ = True
```

```lisp
(define-type-class(yes-no a)()
  ((yes-no(a)boolean)))

(definstance yes-no ((a fixnum))
  (not(zerop a)))

(definstance yes-no ((a list))
  a)

(definstance yes-no ((a symbol))
  a)

(definstance yes-no ((a (maybe *)))
  (unless(eq :nothing a)
    T))

(definstance yes-no ((a traffic-light))
  (unless(eq :red a)
    T))
```

### Compare
さて、欠陥だらけではあるが、ひとまず動く`DEFINE-TYPE-CLASS`が手に入っているので、これで後回しにしていたTODOを回収できる。
すなわち、`compare`の実装と、`week`データ型の実装とである。

```haskell
data Bool = Fails | True deriving (Ord)

ghci> True `compare` False
GT
ghci> True > False
True
ghci> True < False
False
```

```lisp
(defdata bool () :false :true)

(define-type-class(ord a)()
  ((ord(a)integer))
  (:default ord(x)
    (data-order x)))

(definstance ord ((a number))
  a)
(definstance ord ((a character))
  (char-code a))

(define-type-class(compare a)(ord)
  ((compare(a a)(member :eq :lt :gt))
   (lt(a a)boolean)
   (gt(a a)boolean)
   (lte(a a)boolean)
   (gte(a a)boolean))
  (:default compare (x y)
    (compare (ord x)(ord y)))
  (:default lt (x y)
    (< (ord x)(ord y)))
  (:default gt (x y)
    (> (ord x)(ord y)))
  (:default lte (x y)
    (<= (ord x)(ord y)))
  (:default gte (x y)
    (>= (ord x)(ord y))))

(definstance compare((a number)(b number))
  (cond
    ((= a b):eq)
    ((< a b):lt)
    (t :gt)))

cl-user> (compare :true :false)
:GT
cl-user> (gt :true :false)
T
cl-user> (lt :true :false)
NIL
```

```haskell
data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
             deriving (Eq, Ord, Show, Read, Bounded, Enum)
```

```lisp
(defdata week ()
  :monday :tuesday :wednesday :thursday :friday :saturday :sunday)

(define-type-class(bounded a)()
  ((min-bound(a)fixnum)
   (max-bound(a)fixnum))
  (:default min-bound(x)
    (cadr(introspect-environment:typexpand-1 x)))
  (:default max-bound(x)
    (car(last(introspect-environment:typexpand-1 x)))))

(define-type-class(enum a)()
  ((succ(a)a)
   (pred(a)a))
  (:default succ (x)
    (cadr (nth (1+(data-order x))
               (cdr(introspect-environment:typexpand-1(data-type-of x))))))
  (:default pred (x)
    (cadr (nth (1-(data-order x))
               (cdr(introspect-environment:typexpand-1(data-type-of x)))))))
```
これでおよそ期待通りの振る舞いをしてくれる。

```haskell
ghci> Wednesday
Wednesday
ghci> show Wednesday
"Wednesday"
ghci> read "Saturday" :: Day
Saturday
```

```lisp
cl-user> :wednesday
:WEDNESDAY
cl-user> (princ-to-string :wednesday)
"WEDNESDAY"
cl-user> (read-from-string ":saturday")
:SATURDAY
```

```haskell
ghci> Saturday == Sunday
False
ghci> Saturday == Saturday
True
ghci> Saturday > Friday
True
ghci> Monday `compare` Wednesday
LT
```

```lisp
cl-user> (eq :saturday :sunday)
NIL
cl-user> (eq :saturday :saturday)
T
cl-user> (gt :saturday :friday)
T
cl-user> (compare :monday :wednesday)
:LT
```

```haskell
ghci> minBound :: Day
Monday
ghci> maxBound :: Day
Sunday
```

```lisp
cl-user> (min-bound 'week)
(EQL :MONDAY)
cl-user> (max-bound 'week)
(EQL :SUNDAY)
```

```haskell
ghci> succ Monday
Tuesday
ghci> pred Saturday
Friday
```

```lisp
cl-user> (succ :monday)
:TUESDAY
cl-user> (pred :saturday)
:FRIDAY
```
欠点は`:deriving`をサポートしていないところだ。

## 7.10
### Functor

```haskell
class Functor f where
    fmap :: (a -> b) -> f a -> f b

instance Functor [] where
  fmap = map
```

```lisp
(define-type-class(functor f)()
  ((fmap((function(a)b)(f a))(f b))))
```
ファンクタも、およそそのまま再現できる。
大きな違いは関数の型を上手く表せない点だろう。

Common Lispのラムダリストはとても強力で、`&REST`や`&KEY`、`&OPTIONAL`無しにどうやってコードを書けば良いんだ、と思ってしまうほどであるが、その代償として、関数の型は上手く表すことが出来なくなってしまっている。

特に関数を受け取る高階関数を書くにあたり、受け取る関数の型を上手く書けないのは割と辛い。
この点はHaskellが非常に羨ましく思える。
カリー化に伴う制約と、ラムダリストの便利さと、天秤にかけるならどちらに傾くだろうか。

### Maybe

```haskell
instance Functor Maybe where
    fmap f (just x) = Just (f x)
    fmap f Nothing = Nothing
```

```lisp
(definstance fmap ((f function)(m (maybe *)))
  (trivia:match m
    ((just x)(just(funcall f x)))
    (:nothing :nothing)))
```

### Tree
### Either

```haskell
instance Functor (Either a) where
  fmap f (Right x) = Right (f x)
  fmap f (Left x) = Left x
```

```lisp
(definstance fmap((f function)(e (either *)))
  (trivia:ematch e
    ((right x)(right(funcall f x)))
    ((left _) e)))
```
