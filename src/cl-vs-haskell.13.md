# Common Lisp vs Haskell, Chapter 13
## Meta note
### 対象読者
[前章](cl-vs-haskell.12.html)を読了済みの者。

## Introduction
本稿は「すごいH本」の内容をCommon Lispに翻訳しながらCLerがHaskellを学ぶその第13章である。

本稿ではHaskellに於ける型クラス`Monad`と、その周辺のオペレータ、すなわち`do`とリスト内包表記とをCommon Lispに移植しながら学習していく。

本稿はCLerより、むしろHaskellerにとって興味深い内容となっているかもしれない。
というのも、Haskellではブラックボックスになっていて脳内で想像するしかない型クラスの振る舞いが、マクロ展開という形で詳らかにされているからだ。

## 13.3
### Monad

```Haskell
class Monad m where
    return :: a -> m a
    (>>=) :: m a -> (a -> m b) -> m b
    (>>) :: m a -> m b -> m b
    x >> y = x >>= \_ -> y
    fail :: String -> m a
    fail msg = error msg
```

```lisp
(define-type-class(monad m)()
  ((.return(a)(m a))
   (>>=((m a)(function(a)(m b)))(m b))
   (>>((m a)(m b))(m b))
   (fail(string)(m a)))
  (:default >> (x y)
    `(>>= ,x (constantly ,y)))
  (:default fail(msg)
    `(error ,msg)))
```
８章で実装した`.RETURN`とは全く違うものになっている点、要注意。

```haskell
instance Monad Maybe where
    return x = Just x
    Nothing >>= f = Nothing
    Just x >>= f = f x
    fail _ = Nothing

ghci> Just 9 >>= \x -> return (x*10)
Just 90
ghci> Nothing >>= \x -> return (x*10)
Nothing
```

```lisp
(definstance(monad maybe)
  ((.return(x)
     `(just ,x))
   (>>=(a b)
     (alexandria:with-gensyms(x f)
       `(trivia:ematch*(,a ,b)
          ((nothing _)nothing)
          (((just ,x),f)(funcall ,f ,x)))))
   (fail(a)
     (declare(ignore a))
     'nothing)))

cl-user> (>>= (just 9)(lambda(x)(.return (* x 10))))
(just 90)

cl-user> (>>= nothing(lambda(x)(.return (* x 10))))
NOTHING
```

### 13.4

```haskell
type Birds = Int
type Pole = (Birds, Birds)
```

```lisp
(deftype birds()'integer)
(deftype pole()'(cons birds birds))
```

```haskell
landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (left, right)
    | abs ((left + n) - right) < 4 = Just (left + n, right)
    | otherwise                    = Nothing

landRight :: Birds -> Pole -> Maybe Pole
landRight n (left, right)
    | abs (left - (right + n)) < 4 = Just (left, right + n)
    | otherwise                    = Nothing
```

```lisp
(defun land-left(n pole)
  (destructuring-bind(left . right)pole
    (if(< (abs (- (+ left n)
                  right))
          4)
       (just (cons (+ left n) right))
       nothing)))

(defun land-right(n pole)
  (destructuring-bind(left . right)pole
    (if(< (abs (- left
                  (+ right n)))
          4)
       (just (cons left (+ n right)))
       nothing)))
```

```haskell
ghci> return (0, 0) >>= landRight 2 >>= landLeft 2 >>= landRight 2
Just (2,4)
ghci> return (0, 0) >>= landLeft 1 >>= landRight 4 >>= landLeft (-1) >>= landRight (-2)
Nothing
```

Common Lispに於いて、関数の型宣言は通常`DECLAIM`を使って行われる。
また、関数名（シンボル）から宣言された関数型を取り出す機能は言語仕様上には存在しない。
CLtL2には環境オブジェクトに問い合わせを行う幾つかのオペレータが提案されていたのだが、ANSIでは省かれてしまったようだ。
とはいえかつてCLtL2では提案されていたものなので、処理系によってはそれらをサポートしている場合がある。
それら処理系依存のAPIを叩くにはラッパライブラリである`INTROSPECT-ENVIRONMENT`を利用するのが定石だ。
ここでも処理系依存のAPIを直接叩くことはせずに、`INTROSPECT-ENVIRONMENT`経由で叩くこととしている。
もっとも、`INTROSPECT-ENVIRONMENT`はAPIこそラップしてくれているが、振る舞いの共通化までは面倒を見てくれていないので本稿のコードは現時点でSBCLでしか動かないのだが。
さて、そうして取り出した関数型は（SBCLでは）型展開された後のものとなる。
すなわち、`(maybe fixnum)`と返り値を宣言していても、返ってくるのは`(or (member nothing)(cons (member just)(cons fixnum null)))`という型となる。
そして、展開後の型指定子から展開前の型指定子を取り出すのは不可能であり、展開後の型指定子から型シグネチャのチェックを行うのも困難である。

我々の実装に於いて、各インスタンスはマクロであり、各インスタンスマクロは各引数の返り値型を計算し、その型をシグネチャと照らしあわせ実インスタンスを取り出す。
取り出した返り値型から型シグネチャのチェックが出来ないというのは致命的である。

これを避けるため、各種ヘルパーを提供しよう。
組み込みたる`DECLAIM`と重複する機能を提供し、独自のルールを盛り込むのは大変不細工であり、心苦しいのだが、プログラミング言語を作るのではなく、Haskellの機能をCommon Lispで再現するならどうするかという視点で本稿は書かれており、結句、Common Lispの型システムとHaskellの型システムとを同居させねばならなくなってしまっているので、この点については目を瞑ろう。

```lisp
(defmacro function-type (name args return)
  `(PROGN (SETF (GET ',name 'FTYPE)'(FUNCTION ,args ,return))
          ',name))

(defun function-type-of(name)
  (get name 'ftype))

(defun compute-standard-form-return-type(form env)
  (cond
    ((function-type-of(car form))
     (third(function-type-of(car form))))
    ((and (eq 'coerce (car form))
          (constantp (third form)))
     (introspect-environment:constant-form-value(third form)))
    (t
      (multiple-value-bind(type localp declaration)(introspect-environment:function-information (car form)env)
        (declare(ignore localp))
        (case type
          ((nil) (when *return-type-verbose*
                   (warn "Undefined function ~S. ~S"(car form)form)))
          (:special-form (special-operator-return-type form env))
          (:macro (compute-return-type (agnostic-lizard:macroexpand-all (copy-tree form)env)
                                       env))
          (:function
            (let((ftype(assoc 'ftype declaration)))
              (if ftype
                (ftype-return-type (cdr ftype))
                (progn (when *return-type-verbose*
                         (warn "Could not determine type of ~S"form))
                       T)))))))))
```

また、カリー化された関数から返り値の型を取得できねばならない。
これまでは外部ライブラリを利用していたが、フォークする形で本稿用のものをでっちあげよう。

```lisp
;;;; CURRY data structure.
(defclass curry ()
  ((function :initarg :function :reader curried-function)
   (arity :initarg :arity :reader arity)
   (return-type :initarg :return-type :reader return-type))
  (:metaclass c2mop:funcallable-standard-class))

(defmethod initialize-instance :after ((c curry) &key)
  (c2mop:set-funcallable-instance-function c (curried-function c)))

;;;; CURRY
(defmacro curry (op &rest args)
  ;; Trivial syntax check.
  (check-type op (or symbol (cons (eql lambda)T)))
  (when(typep op '(cons (eql lambda)t))
    (assert (every #'symbolp (cadr op)))
    (assert (notany (lambda(elt)
                      (find elt lambda-list-keywords))
                    (cadr op))))
  ;; body
  (if(null args)
    `#',op
    (<Section-Form> op args)))

;;; <Section-Form>
(defun <Section-Form>(op args)
  (let*((gensyms(gensyms(count-if #'underscorep args)))
        (optional-lambda-list(optional-lambda-list gensyms)))
    (if gensyms
      (<Curry-Form> (<Section-Body-Form> op args gensyms) optional-lambda-list
                    (and (symbolp op)
                         (or (third(function-type-of op))
                             (third(introspect-environment:function-type op)))))
      `(,op ,@args))))

(defun underscorep (thing)
  (and (symbolp thing)
       (string= "_" thing)))

(defun optional-lambda-list(lambda-list)
  (mapcar (lambda(x)
            `(,x NIL ,(gensym (format nil "~A-P"x))))
          lambda-list))

;;; <Section-Body-Form>
(defun <Section-Body-Form>(op args gensyms)
  (labels((rec(args gensyms &optional acc)
            (if(endp args)
              (nreverse acc)
              (body(car args)(cdr args)gensyms acc)))
          (body(arg rest gensyms acc)
            (if(underscorep arg)
              (rec rest (cdr gensyms)(push (car gensyms)acc))
              (rec rest gensyms (push arg acc)))))
    `((,op ,@(rec args gensyms)))))

;;; <Curry-Form>
(defun <Curry-Form> (body optional-lambda-list return-type)
  (let((curry (gensym "CURRY")))
    (labels((ENTRY-POINT(list)
              (if(endp list)
                (<BODY-FORM> body)
                `(LABELS((,curry(&OPTIONAL ,@list)
                           (IF ,(caddar list)
                             ,(rec (cdr list))
                             (MAKE-INSTANCE 'CURRYi
                                            :FUNCTION #',curry
                                            :ARITY ,(length list)
                                            :RETURN-TYPE ',return-type
                                            ))))
                   (MAKE-INSTANCE 'CURRY
                                  :FUNCTION #',curry
                                  :ARITY ,(length list)
                                  :RETURN-TYPE ',return-type
                                  ))))
            (REC(list)
              (if(endp list)
                (<body-form> body)
                `(IF,(caddar list)
                   ,(REC (cdr list))
                   ,(ENTRY-POINT list))))
            (<BODY-FORM>(body)
                (if(cdr body)
                  `(LOCALLY ,@body)
                  (car body)))
            )
      (ENTRY-POINT optional-lambda-list))))
```

これで型推論が上手く機能するようになる。

```lisp
(function-type land-right (fixnum pole)(maybe *))
(function-type land-left (fixnum pole)(maybe *))

cl-user> (>>= (>>= (>>= (.return '(0 . 0))
                        (curry land-right 2 _))
                   (curry land-left 2 _))
              (curry land-right 2 _))
(JUST (2 . 4))
```
`>>=`が左結合であることを明示的に書かねばならない点、相変わらず不細工であるが、醜いシンタックスと無理して付き合わなくて良い点もまた、相変わらずである。

```lisp
(defmacro >>=* (&rest expression*)
  (labels((rec(list)
            (if(endp(cdr list))
              (car list)
              `(>>= ,(rec (cdr list))
                    ,(car list)))))
    (rec (reverse expression*))))
```
上記マクロを使えば以下のように書ける。

```lisp
cl-user> (>>=* (.return '(0 . 0))
               (curry land-right 2 _)
               (curry land-left 2 _)
               (curry land-right 2 _))
(JUST (2 . 4))

cl-user> (>>=* (.return '(0 . 0))
               (curry land-left 1 _)
               (curry land-right 4 _)
               (curry land-left -1 _)
               (curry land-right -2 _))
NOTHING
```

```haskell
banana :: Pole -> Maybe Pole
banana _ = Nothing

ghci> return (0, 0) >>= landLeft 1 >>= banana >>= landRight 1
Nothing
```

```lisp
(setf(symbol-function 'banana)(constantly nothing))

cl-user> (>>=* (.return '(0 . 0))
               (curry land-left 1 _)
               #'banana
               (curry land-right 1 _))
NOTHING
```

### 13.5 DO

```haskell
ghci> Just 3 >>= (\x -> Just "!" >>= (\y -> Just (show x ++ y)))
Just "3!"
ghci> Nothing >>= (\x -> Just "!" >>= (\y -> Just (show x ++ y)))
Nothing
ghci> Just 3 >>= (\x -> Nothing >>= (\y -> Just (show x ++ y)))
Nothing
ghci> Just 3 >>= (\x -> Just "!" >>= (\y -> Nothing))
Nothing
```

```lisp
(>>= (just 3)
     (lambda(x)
       (>>= (just "!")
            (lambda(y)
                    (just (format nil "~A~A" x y))))))
(JUST "3!")

(>>= nothing
     (lambda(x)
       (>>= (just "!")
            (lambda(y)
                    (just (format nil "~A~A" x y))))))
NOTHING

(>>= (just 3)
     (lambda(x)
       (>>= nothing
            (lambda(y)
                    (just (format nil "~A~A" x y))))))
NOTHING

(>>= (just 3)
     (lambda(x)
       (declare(ignore x))
       (>>= (just "!")
            (lambda(y)
                    (declare(ignore y))
                    nothing))))
NOTHING
```

```haskell
foo :: Maybe String
foo = do
    x <- Just 3
    y <- Just "!"
    Just (show x ++ y)
```

Haskellに於ける`DO`記法はシンタックスシュガーである。
Common Lispに於いてシンタックスシュガーはマクロで作るべきものである。
`DO`だと名前が衝突するので、ここでは`DOMONAD`とする。

```lisp
(defmacro domonad(&rest expression*)
  (labels((rec(list)
            (let((second(second list)))
              (if(and (symbolp second)
                      (string= '#:<- second))
                `(>>= ,(third list)
                      ,(if(symbolp(first list))
                         `(lambda(,(first list))
                            (declare(ignorable ,(first list)))
                            ,(rec(nthcdr 3 list)))
                         `(lambda(arg)
                            (trivia:match arg
                                          (,(first list),(rec(nthcdr 3 list)))
                                          (_ (fail "Pattern missing."))))))
                (if(cdr list)
                  `(>> ,(first list)
                       ,(rec (cdr list)))
                  (car list))))))
    (rec expression*)))
```
これで以下のように書ける。

```lisp
(defun foo()
  (domonad x <- (just 3)
           y <- (just "!")
           (just (format nil "~A~A" x y))))
```

```haskell
ghci> Just 9 >>= (\x -> Just (x > 8))
Just True

marySue :: Maybe Bool
marySue = do
    x <- Just 9
    Just (x > 8)
```

原著ではここで以下の指摘がある。

> ２つを見比べると、do記法で連鎖させた最後のモナドの結果がdo式全体の結果になる仕組みがよく分かると思います。

Haskellに於いて、プログラマは２つの式の同意性を脳内で想像するしかないが、Common Lispに於いては`MACROEXPAND-1`して実際に見ることができる。

```lisp
cl-user> (>>= (just 9)
              (lambda(x)
                (just (> x 8))))
(JUST T)

cl-user> (macroexpand-1 '(domonad x <- (just 9)
                                  (just (> x 8))))
(>>= (JUST 9)
     (LAMBDA(X)
       (DECLARE(IGNORABLE X))
       (JUST (> X 8))))
```

```haskell
routine :: Maybe Pole
routine = do
    start <- return (0, 0)
    first <- landLeft 2 start
    second <- landRight 2 first
    landLeft 1 second

ghci> routine
Just (3, 2)
```
Haskellに於いて`do`は関数を返すが、我らが`DOMONAD`は単に処理を行う。
これは将来、変更されるかもしれない。

```lisp
cl-user> (domonad start <- (.return '(0 . 0))
                  first <- (land-left 2 start)
                  second <- (land-right 2 first)
                  (land-left 1 second))
(JUST (3 . 2))
```

```haskell
justH :: Maybe Char
justH = do
    (x:xs) <- Just "hello"
    return x

ghci> justH
Just 'h'

wopwop :: Maybe Char
wopwop = do
    (x:xs) <- Just ""
    return x

ghci> wopwop
Nothing
```

```lisp
(defun just-h()
  (domonad (trivia:string* x _) <- (just "hello")
           (.return x)))

cl-user> (just-h)
(JUST #\h)

(defun wopwop()
  (domonad (cons x _) <- (just "")
           (.return x)))

cl-user> (wopwop)
NOTHING
```

我々の実装に於いてパターンマッチは`TRIVIA`に依存している。
`TRIVIA`の`STRING*`マッチャーは空文字列にマッチする点要注意。

```lisp
cl-user> (domonad (trivia:string* x _) <- (just "")
                  (.return x))
(JUST NIL)
```

### List Monad

```haskell
instance Monad [] where
    return x = [x]
    xs >>= f = concat (map f xs)
    fail _ = []
```

```lisp
(definstance(monad list)
  ((.return(x)`(list ,x))
   (>>=(xs f)
     `(mapcan ,f ,xs))
   (fail(x)
     (declare(ignore x))
     NIL)))
```

```haskell
ghci> [3,4,5] >>= \x -> [x,-x]
[3,-3,4,-4,5,-5]
ghci> [] >>= \x -> ["bad","mad","rad"]
[]
ghci> [1,2,3] >>= \x -> []
[]
ghci> [1,2] >>= \n -> ['a','b'] >>= \ch -> return (n, ch)
[(1,'a'),(1,'b'),(2,'a'),(2,'b')]
```

```lisp
cl-user> (>>= '(3 4 5)
              (lambda(x)
                (list x (- x))))
(3 -3 4 -4 5 -5)

cl-user> (>>= nil
              (constantly '("bad" "mad" "rad")))
NIL

cl-user> (>>= '(1 2 3)
              (constantly nil))
NIL

cl-user> (>>= '(1 2)
              (lambda(n)
                (>>= '(#\a #\b)
                     (lambda(c)
                       (.return (cons n c))))))
((1 . #\a)(1 . #\b)(2 . #\a)(2 . #\b))
```

```haskell
listOfTuples :: [(Int, Char)]
listOfTuples = do
    n <- [1,2]
    ch <- ['a','b']
    return (n, ch)
```

```lisp
(defun list-of-cons()
  (domonad n <- '(1 2)
           ch <- '(#\a #\b)
           (.return (cons n ch))))
```

```haskell
ghci> [ (n, ch) | n <- [1,2], ch <- ['a','b'] ]
[(1,'a'),(1,'b'),(2,'a'),(2,'b')]
```

Haskelに於いてリスト内包表記はシンタックスシュガーである。
Common Lispに於いてシンタックスシュガーはマクロで実装されるべきものである。
これまで頼ってきていた`INCF-CL`の`LC`を捨てて、独自の`LC`マクロを用意しよう。

```lisp
(defmacro lc (&rest expression*)
  `(domonad ,@(labels((rec(list &optional acc)
                        (if(endp list)
                          (nreverse acc)
                          (if(and (symbolp(second list))
                                  (string= '#:<- (second list)))
                            (rec (cdddr list)(list* (third list)(second list)(first list)acc))
                            (rec (cdr list)(cons `(guard ,(car list))acc))))))
                (rec (cddr expression*)))
            (.return ,(car expression*))))
```

これで以下のように書ける。

```lisp
cl-user> (lc (cons n c) || n <- '(1 2) c <- '(#\a #\b))
((1 . #\a)(1 . #\b)(2 . #\a)(2 . #\b))
```

`MACROEXPAND-1`していくと以下の通り。

```lisp
cl-user> (macroexpand-1 '(lc (cons n c) || n <- '(1 2) c <- '(#\a #\b)))
(DOMOAD N <- '(1 2) C <- '(#\a #\b) (.RETURN (CONS N C)))

cl-user> (macroexpand-1 *)
(>>= '(1 2)
     (LAMBDA(N)
       (DECLARE(IGNORABLE N))
       (>>= '(#\a #\b)
            (LAMBDA(C)
              (DECLARE(IGNORABLE C))
              (.RETURN (CONS N C))))))

cl-user> (eval +)
(MACROLET ((.RETURN(X)
             `(LIST ,X))
           (%>>=(XS F)
             `(MAPCAN ,F ,XS))
           (FAIL(X)
             (DECLARE(IGNORE X))
             NIL)
           (MZERO()
             NIL)
           (MPLUS(A B)
             `(APPEND ,A ,B)))
  (THE LIST
       (%>>= '(1 2)
             (LAMBDA(N)
               (DECLARE(IGNORABLE N))
               (THE LIST
                    (MAPCAN (LAMBDA(C)
                              (DECLARE(IGNORABLE C))
                              (LIST (CONS N C)))
                            '(#\a #\b)))))))

cl-user> (expander:expand *)
(THE LIST
     (MAPCAN (LAMBDA(N)
               (DECLARE(IGNORABLE N))
               (THE LIST
                    (MAPCAN (LAMBDA(C)
                              (DECLARE(IGNORABLE C))
                              (LIST (CONS N C)))
                            '(#\a #\b))))
             '(1 2)))
```

Haskellではブラックボックスになっており、プログラマが想像するしかない背後の働きが、上記のように我々の実装に於いては`MACROEXPAND`を通して詳らかにされる点は、遅延評価を実装せずマクロで無理やり実装したことによる数少ないメリットの一つであろうかと思う。

また、高級なフォームが展開され低級なフォームになるわけだが、どちらもLispフォームである点に変わりはなく、Lispが「プリティアセンブラ」と言われるのも頷けるものとなっている。

### MonadPlus

```haskell
class Monad m => MonadPlus m where
    mzero :: m a
    mplus :: m a -> m a -> m a

instance MonadPlus [] where
    mzero = []
    mplus = (++)
```

```lisp
(define-type-class(monad+ m)(monad)
  ((mzero()(m a))
   (mplus((m a)(m a))(m a))))

(definstance(monad+ list)
  ((mzero()nil)
   (mplus(a b)`(append ,a ,b))))
```

```haskell
guard :: (MonadPlus m) => Bool -> m ()
guard True = return ()
guard False = mzero
```
Haskellに於いて`guard`は関数であるが、我々の実装に於いてはマクロで実装せねばならない。

```lisp
(defmacro guard(bool)
  `(if ,bool
     (.return nil)
     (mzero)))
```

```haskell
ghci> [1..50] >>= (\x -> guard ('7' `elem` show x) >> return x)
[7,17,27,37,47]

sevensOnly :: [Int]
sevensOnly = do
    x <- [1..50]
    guard ('7' `elem` show x)
    return x

ghci> [ x | x <- [1..50], '7' `elem` show x ]
[7,17,27,37,47]
```

```lisp
cl-user> (>>= (the list(loop :for i :upfrom 1 :to 50 :collect i))
              (lambda(x)
                (>> (guard(find #\7(princ-to-string x)))
                    (.return x))))
(7 17 27 37 47)

cl-user> (domonad x <- (the list(loop :for i :upfrom 1 :to 50 :collect i))
                  (guard (find #\7 (princ-to-string x)))
                  (.return x))
(7 17 27 37 47)

cl-user> (lc x || x <- (the list(loop :for i :upfrom 1 :to 50 :collect i))
             (find #\7 (princ-to-string x)))
(7 17 27 37 47)
```

`LOOP`マクロからは返り値の型推論が不可能なので`THE`により型ヒントを明示してあげなければならない点要注意。

```haskell
type KnightPos = (Int, Int)
```

```lisp
(deftype knight-pos()
  '(cons fixnum fixnum))
```

```haskell
moveKnight :: KnightPos -> [KnightPos]
moveKnight (c, r) = do
    (c', r') <- [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)
                ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)
                ]
    guard (c' `elem` [1..8] && r' `elem` [1..8])
    return (c', r')

ghci> moveKnight (6, 2)
[(8,1),(8,3),(4,1),(4,3),(7,4),(5,4)]
ghci> moveKnight (8, 1)
[(6,2),(7,3)]
```

```lisp
(declaim(ftype(function(knight-pos)(list knight-pos))move-knight))

(defun move-knight(pos)
  (destructuring-bind(c . r)pos
    (domonad (cons c% r%) <- (list (cons (+ c 2)(1- r))
                                   (cons (+ c 2)(1+ r))
                                   (cons (- c 2)(1- r))
                                   (cons (- c 2)(1+ r))
                                   (cons (1+ c)(- r 2))
                                   (cons (1+ c)(+ r 2))
                                   (cons (1- c)(- r 2))
                                   (cons (1- c)(+ r 2)))
             (guard(and (find c% '(1 2 3 4 5 6 7 8))
                        (find r% '(1 2 3 4 5 6 7 8))))
             (.return (cons c% r%)))))

cl-user> (move-knight '(6 . 2))
((8 . 1)(8 . 3)(4 . 1)(4 . 3)(7 . 4)(5 . 4))

cl-user> (move-knight '(8 . 1))
((6 . 2)(7 . 3))
```

```haskell
in3 :: KnightPos -> [KnightPos]
in3 start = do
    first <- moveKnight start
    second <- moveKnight first
    moveKnight second

canReachIn3 :: KnightPos -> KnightPos -> Bool
canReachIn3 start end = end `elem` in3 start

ghci> (6, 2) `canReachIn3` (6, 1)
True
ghci> (6, 2) `canReachIn3` (7, 3)
False
```

```lisp
(defun in3(start)
  (domonad first <- (move-knight start)
           second <- (move-knight first)
           (move-knight second)))

(defun reach-in3-p(start end)
  (find end (in3 start) :test #'equal))

cl-user> (reach-in3-p '(6 . 2)'(6 . 1))
(6 . 1)

cl-user> (reach-in3-p '(6 . 2)'(7 . 3))
NIL
```
