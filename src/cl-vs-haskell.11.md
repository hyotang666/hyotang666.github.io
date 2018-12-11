# Common Lisp vs Haskell, Chapter 11
## Meta note
### 対象読者
[前章](cl-vs-haskell.10.html)を読了済みの者。

## Introduction
本稿は「すごいH本」の内容をCommon Lispに翻訳しながらCLerがHaskellを学ぶその第11章である。

本章では[７章](cl-vs-haskell.7.html)で作ったDSLに手を入れ、Applicativeの実装を行い、学んでいく。

初級者にとって得られるものはほとんど何もないと言って差し支えなかろう。
もっとも本稿の対象読者は本稿シリーズを読了済の者であるので、ここまでを楽しく読めてきている方であるならば本章も楽しく読めるであろうが。

中級者にとっては、MOPを利用してFuncallable Objectを作っているあたりが興味深く見れるかと思う。

上級者にとっては、やはり特に面白く見れる点はなかろうと思う。
Haskellコードを粛々とCommon Lispコードに置き換えていくだけの内容であり、それ自身はCommon Lispの強力さをまざまざと見せつけるものとなってはいるのだが、Common Lisp上級者たればそんなことは百も承知のもので「何を今更」な話でしかなかろうからだ。

# 11
## 11.1

### io action as functor

```haskell
instance Functor IO where
    fmap f action = do
        result <- action
        return (f result)
```
型指定の補助がいるが、上記コードは簡単に移植できる。

```lisp
(define-type-class(functor f a)()
  ((fmap((function(a)b)(functor a))(functor b))))

(deftype io(&optional ret)
  `(function * ,ret))

(definstance fmap ((f (function(a)b))(action (io a)))
  (action result <- action
          (.return (funcall f result))))
```

```haskell
main = do line <- fmap reverse getLine
          putStrLn $ "You said " ++ line ++ " backwards!"
```

```lisp
(defaction main ()
  line <- (fmap #'reverse (get-line))
  (put-string-line (uiop:strcat "You said " line " backwards!")))
```

```haskell
import Data.Char
import Data.List

main = do line <- fmap (intersperse '-' . reverse . map toUpper)
                       getLine
          putStrLn line
```
Haskellの`intersperse`に相当する機能はCommon Lispには存在しないので、自作せねばならない。

```lisp
(defun intersperse (char string)
  (with-output-to-string(*standard-output*)
    (loop :for c :across string
          :for i :upfrom 0
          :when (array-in-bounds-p string (1+ i))
          :do (write-char c)(write-char char)
          :else :do (write-char c))))

(defaction main ()
  line <- (fmap #`(+ #`(% 'intersperse #\-) 'reverse 'string-upcase)
                (get-line))
  (put-string-line line))
```

### function as functor

```haskell
instance Functor ((->) r) where
    fmap f g = (\x -> f (g x))

ghci> fmap (*3) (+100) 1
303
ghci> (*3) `fmap` (+100) $ 1
303
ghci> (*3) . (+100) $ 1
303
ghci> fmap (show . (*3)) (+100) 1
"303"
```

`DEFTYPE`による補助だけでは`IO`としての関数と通常の関数とを区別できないので、MOPを利用して新しく`IO-ACTION`型の関数を導入しよう。

```lisp
(defclass io-action ()
  ((instance :initarg :instance :reader action-of))
  (:metaclass closer-mop:funcallable-standard-class))

(defmethod initialize-instance :after ((c io-action) &key)
  (closer-mop:set-funcallable-instance-function c (action-of c)))

(defmacro defio (name lambda-list declarations &body body)
  `(PROGN (DECLAIM(FTYPE(FUNCTION,declarations IO-ACTION),name))
          (DEFUN,name,lambda-list
            (MAKE-INSTANCE 'IO-ACTION :INSTANCE (LAMBDA(),@body)))))

(defmacro action(&rest exp*)
  (labels((rec(exp* &optional acc)
            (if(endp exp*)
              (nreverse acc)
              (cond
                ((eq 'let (first exp*))
                 `((LET*,(second exp*)
                    ,@(rec (nthcdr 2 exp*)))))
                ((eq '<- (second exp*))
                 `(,@(nreverse acc)
                    (LET((,(first exp*)(MAY-CALL ,(third exp*))))
                      ,@(rec (nthcdr 3 exp*)))))
                (T (rec (cdr exp*)(cons `(MAY-CALL ,(first exp*))acc))))))
          )
    `(MAKE-INSTANCE 'IO-ACTION :INSTANCE (LAMBDA(),@(rec exp*)))))

(defio .return(value)(t)
  value)

(definstance fmap ((f function)(action io-action))
  (action result <- action
          (.return (funcall f result))))
```
これにより、通常の関数と`IO`としての関数とを区別できるようになる。

```lisp
(definstance fmap ((f function)(g function))
  #`(+ f g))

cl-user> (funcall (fmap #`(% '* 3) #`(% '+ 100)) 1)
303
cl-user> (funcall #%(#`(% '* 3) fmap #`(% '+ 100)) 1)
303
cl-user> (funcall #`(+ #`(% '* 3) #`(% '+ 100)) 1)
303
cl-user> (funcall (fmap #`(+ 'princ-to-string #`(% '* 3))
                        #`(% '+ 100))
                  1)
"303"
```

```haskell
ghci> fmap (replicate 3) [1,2,3,4]
[[1,1,1],[2,2,2],[3,3,3][4,4,4]]
ghci> fmap (replicate 3) (Just 4)
Just [4,4,4]
ghci> fmap (replicate 3) (Right "blah")
Right ["blah","blah","blah"]
ghci> fmap (replicate 3) Nothing
Nothing
ghci> fmap (replicate 3) (Left "foo")
Left "foo"
```
必要なインスタンスを都度定義するだけで、まぁ、だいたいは再現できる。

```lisp
(definstance fmap ((f function)(l list))
  (mapcar f l))
cl-user> (fmap #`(% 'replicate 3)'(list 1 2 3 4))
(LIST(1 1 1)(2 2 2)(3 3 3)(4 4 4))

(definstance fmap ((f function)(m (maybe *)))
  (trivia:ematch m
    ((just x)(just (funcall f x)))
    (:nothing :nothing)))
cl-user> (fmap #`(% 'replicate 3) (just 4))
(JUST (4 4 4))
cl-user> (fmap #`(% 'replicate 3) :nothing)
:nothing

(definstance fmap ((f function)(e (either * *)))
  (trivia:match e
    ((right x)(right (funcall f x)))
    (_ e)))
cl-user> (fmap #`(% 'replicate 3) (left "foo"))
(LEFT "foo")
```
現時点で著しく不細工なのは`EITHER`の型指定子だろうか。
型コンストラクタがカリー化されていない点は、おそらく将来、なんとかせねばならなくなるだろう。

## 11.2
### rule 1

```haskell
ghci> fmap id (Just 3)
(Just 3)
ghci> id (Just 3)
(Just 3)
ghci> fmap id [1..5]
[1,2,3,4,5]
ghci> id [1..5]
[1,2,3,4,5]
ghci> fmap id []
[]
ghci> fmap id Nothing
Nothing
```
ファンクタ則もほぼそのまま移植できる。

```lisp
cl-user> (fmap #'identity (just 3))
(JUST 3)
cl-user> (identity (just 3))
(JUST 3)
cl-user> (fmap #'identity (incf-cl:range 1 5))
(1 2 3 4 5)
cl-user> (identity (incf-cl:range 1 5))
(1 2 3 4 5)
cl-user> (fmap #'identity ())
()
cl-user> (fmap #'identity nothing)
:NOTHING
```

### rule 2

### breaking the rule

```haskell
data CMaybe a = CNothing | CJust Int a deriving (Show)

ghci> CNothing
CNothing
ghci> CJust 0 "haha"
CJust 0 "haha"
ghci> :t CNothing
CNothing :: CMaybe a
ghci> :t CJust 0 "haha"
CJust 0 "haha" :: CMaybe [Char]
ghci> CJust 100 [1,2,3]
CJust 100 [1,2,3]

instance Functor CMaybe where
    fmap f CNothing = CNothing
    fmap f (CJust counter x) = CJust (counter+1) (f x)

ghci> fmap (++"ha") (CJust 0 "ho")
CJust 1 "hoha"
ghci> fmap (++"he") (fmap (++"ha") (CJust 0 "ho"))
CJust 2 "hohahe"
ghci> fmap (++"blah") CNothing
CNothing

ghci> fmap id (CJust 0 "haha")
CJust 1 "haha"
ghci> id (CJust 0 "haha")
CJust 0 "haha"
```
これもほぼそのままである。

```lisp
(defdata counter-maybe(a)
  counter-nothing
  (counter-just fixnum a))

cl-user> :counter-nothing
:COUNTER-MAYBE
cl-user> (counter-just 0 "haha")
(COUNTER-JUST 0 "haha")
cl-suer> (counter-just 100 '(1 2 3))
(COUNTER-JUST 100 (1 2 3))

(definstance fmap ((f function)(cm (counter-maybe *)))
  (trivia:match cm
    (counter-nothing counter-nothing)
    ((counter-just counter x)(counter-just (1+ counter)(funcall f x)))))

cl-user> (fmap #`(% 'uiop:strcat _ "ha")(counter-just 0 "ho"))
(COUNTER-JUST 1 "hoha")
cl-user> (fmap #`(% 'uiop:strcat _ "he")(fmap #`(% 'uiop:strcat _ "ha")(counter-just 0 "ho")))
(COUNTER-JUST 2 "hohahe")
cl-user> (fmap #`(% 'uiop:strcat _ "blah") :counter-nothing)
:COUNTER-NOTHING

cl-user> (fmap #'identity (counter-just 0 "haha"))
(COUNTER-JUST 1 "haha")
cl-user> (identity (counter-just 0 "haha"))
(COUNTER-JUST 0 "haha")
```

## 11.3

### applicative

```haskell
class (Functor f) => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b
```
上記コードは現行の実装では再現できない。
現行の実装では各インスタンスは、インタープリタとして振る舞う関数と、それ上に機能するコンパイラマクロとして実装されている。

そして`PURE`のインスタンスは型のコンテクストに依存して決定されることとなる。
コンパイラマクロはこれを上手く捌けるが、インタープリタには出来ない。
コンテクストが決まる前に`PURE`が評価されてしまうからだ。

これに対応するには、コンパイラマクロと関数という二段構えの構造を捨て、代わりにグローバルマクロとローカルマクロの二段構えにすれば良い。

```lisp
;;;; DEFINE-TYPE-CLASS
(defmacro define-type-class((name type-var)super-classes methods &rest rest)
  ;; trivial syntax checking.
  (assert(symbolp name))
  (assert(symbolp type-var))
  (assert(listp super-classes))
  (assert(every #'symbolp super-classes))
  ;; as canonicalize
  (setf type-var (Envar type-var))
  ;; body
  `(EVAL-WHEN(:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
     (SETF(GET ',name 'TYPE-CLASS)(MAKE-INFO :NAME ',name :VAR ',type-var))
     ,@(when super-classes
         (<type-class-relation-setter> name super-classes))
     ,@(loop
         :for (method lambda-list return-type) :in methods
         :for gensyms = (Gensyms lambda-list)
         :do (setf ; as canonicalise
               lambda-list (patternize lambda-list)
               return-type (patternize return-type))
         :collect (<instance-info-setter> method name lambda-list return-type rest)
         :collect (<defmacro> method gensyms lambda-list return-type))
     ',name))

;;; <defmacro>
(defvar *sub-expand* nil)
(defvar *expand-verbose* T)
(defun <defmacro>(method gensyms lambda-list return-type &aux (sub-name(sub-name method)))
  `(DEFMACRO,method(&WHOLE WHOLE ,@gensyms &ENVIRONMENT ENV)
     (IF (EQ *SUB-EXPAND* WHOLE)
         (ERROR "Trap infinite expansion ~S" whole)
         (LET*((*SUB-EXPAND* WHOLE)
               (EXPANDED(LOOP :FOR FORM :IN (LIST ,@gensyms)
                              :COLLECT (EXPANDER:EXPAND FORM ENV)))
               (INFOS(CHECK-SIGNATURE ',lambda-list (COMPUTE-RETURN-TYPES EXPANDED ENV)))
               (IL(GET-INSTANCE-LAMBDA ',method INFOS))
               (MACROS(LOOP :FOR (NAME . REST) :IN IL
                            :COLLECT (CONS (SUB-NAME NAME) REST)))
               (BODY`(,',sub-name
                       ,@(LOOP :FOR FORM :IN (TRESTRUL:ASUBST-IF
                                               #'SUB-NAME
                                               (LAMBDA(X)(FIND X IL :KEY #'CAR :TEST #'EQ))
                                               EXPANDED)
                               :COLLECT (expander:expand
                                          `(MACROLET,MACROS,FORM) env)))))
           (IF IL
              ,(if(millet:type-specifier-p return-type)
                 ``(MACROLET,MACROS (THE ,',return-type ,BODY))
                `(LET((RETURN(SUBSTITUTE-PATTERN ',return-type (TYPE-UNIFY:UNIFY ',lambda-list (ENWILD INFOS)))))
                   (IF(MILLET:TYPE-SPECIFIER-P RETURN)
                     `(MACROLET,MACROS (THE ,RETURN ,BODY))
                     `(MACROLET,MACROS ,BODY))))
              (PROGN (WHEN *EXPAND-VERBOSE*
                       (WARN "Instance is not found. ~S ~S"',method (LIST ,@gensyms)))
                     WHOLE))))))

(defun sub-name(symbol)
  (intern(format nil "%~A"symbol)))

;;;; DEFISTANCE
(defmacro definstance((type-class type) definition)
  `(progn ,@(loop :for (name) :in definition
                  :for signature = (subst type
                                          (type-var (get type-class 'type-class))
                                          (instance-lambda-list name))
                  :when (trestrul:find-leaf-if (complement #'type-unify:variablep)
                                               signature)
                  :collect `(add-instance ',name ',signature ',definition))
          ',type-class))
```
少々トリッキーなので、理屈を解説しよう。
まず、グローバルマクロ`<*>`が呼び出される。
グローバルマクロ`<*>`はインスタンスを特定するために、引数として受け取ったS式を先にマクロ展開させる。
例えばグローバルマクロ`PURE`が展開されるとする。
グローバルマクロ`PURE`は現在の型のコンテクストが分からないのでインスタンスを特定できない。
そこでマクロ展開を諦めて`&WHOLE`を返す。
すなわち、マクロ展開を遅延させる。
グローバルマクロ`<*>`は、今一方の引数から（`<*>`はニ引数マクロである。）返り値が例えばリストであることが判明するとする。
そこでグローバルマクロ`<*>`はリストのコンテクストを生成する。
すなわち、マクロレットフォームを生成する。
マクロレットフォームはローカルマクロ`PURE`を確立する。
グローバルマクロ`<*>`の展開はここで終了し、評価をLispに引き渡す。
Lispはマクロレットフォームのボディ部に評価を進めていき、改めて`PURE`にぶつかる。
その時点でローカルマクロ`PURE`がグローバルマクロ`PURE`をシャドウしているので、この度はローカルマクロ`PURE`が呼び出され、成功裏にリストインスタンスの`PURE`が展開されるという運びとなる。
以上、解説終わり。

型クラスの定義は以下のようになる。

```Lisp
(define-type-class(applicative f)(functor)
  ((pure(a)(functor a))
   (<*>((functor(function(a)b))(functor a))(functor b))))
```

### Maybe is applicative functor

```haskell
instance Applicative Maybe where
    pure = Just
    Nothing <*> _ = Nothing
    (Just f) <*> something = fmap f something
```
上記インスタンス定義は以下のようになる。

```lisp
(definstance(aplicative maybe)
  ((pure(x)
     `(just ,x))
   (<*>(a b)
     (trivia:match*(a b)
        ((nothing _) nothing)
        ((just f)something)`(fmap ,f ,something)))))
```
パターンマッチをマクロ展開時に行っている点要注意。

```haskell
ghci> Just (+3) <*> Just 9
Just 12
ghci> pure (+3) <*> Just 10
Just 13
ghci> pure (+3) <*> Just 9
Just 12
ghci> Just (++"hahaha") <*> Nothing
Nothing
ghci> Nothing <*> Just "woot"
Nothing
```

```lisp
cl-user> (<*> (just (curried-function:section + 3 _)) (just 9))
(JUST 12)
cl-user> (<*> (pure (curried-function:section + 3 _)) (just 10))
(JUST 13)
cl-user> (<*> (pure (curried-function:section + 3 _)) (just 9))
(JUST 12)
cl-user> (<*> (just (curried-function:section uiop:strcat _ "hahaha")) nothing)
NOTHING
cl-user> (<*> nothing (just "woot"))
NOTHING
```
`<*>`が前置になっている点と、関数のカリー化が冗長な点を除けば、ほぼ直訳と言える。

### Applicative style.

```haskell
ghci> pure (+) <*> Just 3 <*> Just 5
Just 8
ghci> pure (+) <*> Just 3 <*> Nothing
Nothing
ghci> pure (+) <*> Nothing <*> Just 5
Nothing
```
`<*>`が左結合である点を明示的に書かねばならない点が非常に不細工であるが、上記コードは以下のようになる。

```lisp
cl-user> (<*> (<*> (pure (curried-function:section + _ _))
                   (just 3))
              (just 5))
(JUST 8)
cl-user> (<*> (<*> (pure (curried-function::section + _ _))
                   (just 3))
              nothing)
NOTHING
cl-user> (<*> (<*> (pure (curried-function::section + _ _))
                   nothing)
              (just 5))
NOTHING
```
不細工なシンタックスに我慢をして付き合う必要はない。
なぜならLispだからだ。

```lisp
(defmacro <*>*(&rest body)
    (labels((rec(body)
              (if(endp (cdr body))
                (car body)
                `(<*> ,(rec(cdr body))
                      ,(car body)))))
      (rec(reverse body))))
```
これで以下のように書けるようになった。

```lisp
cl-user> (<*>* (pure (curried-function:section + _ _))
               (just 3)
               (just 5))
(JUST 8)
cl-user> (<*>* (pure (curried-function::section + _ _))
               (just 3)
               nothing)
NOTHING
cl-user> (<*>* (pure (curried-function::section + _ _))
               nothing
               (just 5))
NOTHING
```

```haskell
(<$>) :: (Functor f) => (a -> b) -> f a -> f b
f <$> x = fmap f x
```
上記関数は、現行の実装ではマクロとして実装せざるを得ない。
なぜなら`FMAP`はマクロであり、マクロは関数より先に展開されるからだ。
ついでに`<*>*`の機能も載せてしまおう。

```lisp
(defmacro <$>(function &rest functors)
    (labels((rec(body)
              (if(endp (cdr body))
                `(fmap ,function ,(car body))
                `(<*> ,(rec(cdr body))
                      ,(car body)))))
      (rec(reverse functors))))
```

```haskell
ghci> (++) <$> Just "johntra" <*> Just "volta"
Just "johntravolta
```

```lisp
cl-user> (<$> (curried-function::section concatenate 'string _ _)
              (just "johntra")
              (just "volta"))
(JUST "johntravolta")
```

### List

```haskell
instance Applicative [] where
    pure x = []
    fs <*> xs = [f x | f <- fs, x <- xs]

ghci> [(*0),(+100),(^2)] <*> [1,2,3]
[0,0,0,101,102,103,1,4,9]
ghci> [(+),(*)] <*> [1,2] <*> [3,4]
[4,5,5,6,3,4,6,8]
ghci> (++) <$> ["ha","heh","hmm"] <*> ["?","!","."]
["ha?","ha!","ha.","heh?","heh!","hmm?","hmm!","hmm."]
ghci> (*) <$> [2,5,10] <*> [8,10,11]
[16,20,22,40,50,55,80,100,110]
ghci> filter (>50) $ (*) <$> [2,5,10] <*> [8,10,11]
[55,80,100,110]
```

```lisp
(definstance(applicative list)
  ((<*>(functor arg)
     `(incf-cl:lc (funcall f x)
                  (incf-cl:<- f ,functor)
                  (incf-cl:<- x ,arg)))
   (pure(x)
     `(list ,x))))

cl-user> (<*> (list (curried-function::section * 0 _)
                    (curried-function::section + 100 _)
                    (curried-function::section expt _ 2))
              '(1 2 3))
(0 0 0 101 102 103 1 4 9)

cl-user> (<$> (curried-function::section concatenate 'string _ _)
              '("ha" "heh" "hmm")
              '("?" "!" "."))
("ha?" "ha!" "ha." "heh?" "heh!" "heh." "hmm?" "hmm!" "hmm.")

cl-user> (<$> (curried-function::section * _ _)
              '(2 5 10)
              '(8 10 11))
(16 20 22 40 50 55 80 100 110)

cl-user> (remove-if-not (curried-function::section > _ 50)
                        (<$> (curried-function::section * _ _)
                             '(2 5 10)
                             '(8 10 11)))
(55 80 100 110)
```

### IO

```haskell
instance Applicative IO where
    pure = return
    a <*> b = do
        f <- a
        x <- b
        return (f x)

myAction :: IO String
myAction = (++) <$> getLine <*> getLine
```

```lisp
(definstance(applicative io)
  ((<*>(functor arg)
     `(action f <- ,functor
              x <- ,arg
              (.return (funcall f x))))
   (pure(x)
     `(.return ,x))))

(defaction my-action ()
  (<$> (curried-function::section concatenate 'string _ _)
       (get-line)
       (get-line)))
```

### Function

```haskell
instance Applicative ((->) r) where
    pure x = (\_ -> x)
    f <*> g = \x -> f x (g x)

ghci> (+) <$> (+3) <*> (*100) $ 5
508
ghci> (\x y z -> [x,y,z]) <$> (+3) <*> (*2) <*> (/2) $ 5
[8.0,10.0,2.5]
```

```lisp
(definstance(applicative function)
  ((pure(x)
     `(constantly ,x))
   (<*>(f g)
     `(lambda(x)
        (funcall (funcall ,f x) (funcall ,g x))))))

cl-user> (funcall (<$> (curried-function::section + _ _)
                       (curried-function::section + _ 3)
                       (curried-function::section * _ 100))
                  5)
508

cl-user> (funcall (<$> (curried-function:section list _ _ _)
                           (curried-function:section + _ 3)
                           (curried-function:section * _ 2)
                           (curried-function:section / _ 2))
                  5)
(8 10 5/2)
```

### Zip List
本節は次章で`newtype`を実装してから行う。

## 11.4

```haskell
liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c
liftA2 f a b = f <$> a <*> b

ghci> liftA2 (:) (just 3) (Just [4])
Just [3,4]
ghci> (:) <$> Just 3 <*> Just [4]
Just [3,4]
```

`LIFT`もまたマクロとして実装せざるを得ない。

```lisp
(defmacro lift(function &rest functor*)
    `(<$> ,function ,@functor*))

cl-user> (lift (curried-function:section cons _ _) (just 3)(just '(4)))
(just (3 4))
cl-user> (<$> (curried-function:section cons _ _)
              (just 3)
              (just '(4)))
(just (3 4))
```

```haskell
sequenceA :: (Applicative f) => [f a] -> f [a]
sequenceA [] = pure []
sequenceA (x:xs) = (:) <$> x <*> sequenceA xs

ghci> sequenceA [Just 3, Just 2, Just 1]
Just [3,2,1]
ghci> sequenceA [Just 3, Nothing, Just 1]
Nothing
ghci> sequenceA [(+3),(+2),(+1)] 3
[6,5,4]
ghci> sequenceA [[1,2,3],[4,5,6]]
[[1,4],[1,5],[1,6],[2,4],[2,5],[2,6],[3,4],[3,5],[3,6]]
ghci> sequenceA [[1,2,3],[4,5,6],[3,4,4],[]]
[]
```
`sequenceA`もまたマクロで実装せねばならない。
本実装では引数はコンパイル時に決定していなければならない。
このような制限はHaskellには存在しないはずだが、本稿では原著の例題さえ再現できればよしとするスタンスのため、ここでは目を瞑ることとする。

```lisp
(defmacro sequence-a(applicative*)
    (trivia:ematch applicative*
      ((null)`(pure nil))
      ((cons x xs)`(<$> (curried-function:section cons _ _)
                        ,x
                        (sequence-a ,xs)))))

cl-user> (sequence-a ((just 3)(just 2)(just 1)))
(just (3 2 1))
cl-user> (sequence-a ((just 3)nothing(just 1)))
NOTHING
cl-user> (funcall (sequence-a ((curried-function:section + _ 3)
                               (curried-function:section + _ 2)
                               (curried-function:section + _ 1)))
                  3)
(6 5 4)
cl-user> (sequence-a ('(1 2 3)'(4 5 6)))
((1 4)(1 5)(1 6)(2 4)(2 5)(2 6)(3 4)(3 5)(3 6))
cl-user> (sequence-a ('(1 2 3)'(4 5 6)'(3 4 4)nil))
NIL
```

```haskell
ghci> sequenceA [(>4),(<10),odd] 7
[True,True,True]
ghci> and $ sequenceA [(>4),(<10),odd] 7
True
```

```lisp
cl-user> (funcall (sequence-a ((curried-function:section > _ 4)
                               (curried-function:section < _ 10)
                              #'oddp))
                  7)
(T T T)

cl-user> (every #'identity
                (funcall (sequence-a((curried-function:section > _ 4)
                                     (curried-function:section < _ 10)
                                     #'oddp))
                         7))
=> T
```

```haskell
ghci> sequenceA [getLine,getLine,getLine]
heyh
ho
woo
["heyh","ho","woo"]
```

```lisp
cl-user> (funcall (sequence-a ((get-line)(get-line)(get-line))))
heyh
ho
woo
("heyh" "ho" "woo")
```
