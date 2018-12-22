# Common Lisp vs Haskell, Chapter 12
## Meta note
### 対象読者
[前章](cl-vs-haskell.11.html)を読了済みの者。

## Introduction
本稿は「すごいH本」の内容をCommon Lispに翻訳しながらCLerがHaskellを学ぶその第12章である。

本章では`newtype`に相当する機能を実装し、学んでいく。
また、`monoid`も実装し、学んでいく。

初心者CLerにとっては、中程で紹介されるCommon Lisp組み込み関数`STRING`のファミリーが参考になろうかと思う。

中級以上のCLerにとっては、得られるものが何もなかろうと思う。
ちょっと癖のあるハッキーな`THE`の使い方が面白くは見えるかもしれない。

長文ではあるが、大半はHaskellコードをCommon Lispコードに粛々と移植していくだけの内容なので、中身は大変薄っぺらい。
コーヒーを片手に、ざっと流し読みしていただけたならそれで充分である。

# 12
## NEWTYPE

Haskellに於ける`newtype`キーワードはCommon Lispで作ることは不可能である。
だが、ここで欲しいのはインスタンスをディスパッチする際のキーとしての新しい名前である。
`DEFTYPE`と`THE`を使うことで、近しいことはできよう。

```lisp
(defmacro define-newtype(name lambda-list &body body)
  `(PROGN (SETF (GET ',name 'newtype) T)
          (DEFTYPE ,name ,lambda-list ,@body)
          (DEFMACRO,name(arg)
            `(THE ,',name ,arg))))

(defun denew(form)(third form))
```

これにより前章で後回しにしていた宿題を片付けることが出来る。

```haskell
instance Applicative ZipList where
    pure x = ZipList (repeat x)
    ZipList fs <*> ZipList xs = ZipList (zipWith (\f x -> f x) fs xs
```

```lisp
(define-newtype zip-list(&optional a)
  (declare(ignore a))
  'list)

(definstance(functor zip-list)
  ((fmap(f zl)
     `(zip-list (fmap ,f ,(denew zl))))))

(definstance(applicative zip-list)
  ((pure(x)
     `(series:series ,x))
   (<*>(fs xs)
     `(zip-list (let((fn ,fs))
                  (series:collect(series:map-fn t #'funcall (series:scan fn)
                                                (series:scan ,xs))))))))
```

```haskell
ghci> getZipList $ (+) <$> ZipList [1,2,3] <*> ZipList [100,100,100]
[101,102,103]
ghci> getZipList $ (+) <$> ZipList [1,2,3] <*> ZipList [100,100..]
[101,102,103]
ghci> getZipList $ max <$> ZipList [1,2,3,4,5,3] <*> ZipList [5,3,1,2]
[5,3,3,4]
ghci> getZipList $ (,,) <$> ZipList "dog" <*> ZipList "cat" <*> ZipList "rat"
[('d','c','r'),('o','a','a'),('g','t','t')]
```

```lisp
cl-user> (<$> (curried-function:section + _ _)
              (zip-list '(1 2 3))
              (zip-list '(100 100 100)))
(101 102 103)

cl-user> (<$> (curried-function:section max _ _)
              (zip-list '(1 2 3 4 5 3))
              (zip-list '(5 3 1 2)))
(5 3 3 4)

cl-user> (<$> (curried-function:section list _ _ _)
              (zip-list (coerce "dog" 'list))
              (zip-list (coerce "cat" 'list))
              (zip-list (coerce "rat" 'list)))
((#\d #\c #\r)(#\o #\a #\a)(#\g #\t #\t))
```
我々の実装に於いて`NEWTYPE`は`THE`のフォームに展開される。
よって`getZipList`で中身を取り出すことは通常必要ない。
必要になるのはマクロ展開時に`THE`の型情報を引っぺがして下層型としてインスタンスのディスパッチを行いたい場合のみだ。
その場合、定義された型に合わせてあれこれ名前を用意するのは面倒なので`DENEW`というヘルパーが定義されている。
これは`THE`のフォームから本体を取り出すだけのものだ。

なお、現行、無限リストへの対応はできていない。
しかしながら、この点は不可能ではない。
ただ、`SERIES`が警告を大量に出すのと、効率的な`LOOP`にならないのと、それを解決するには`SERIES`のソースコードをがっつり解析しなければならないことが面倒なだけで、警告と効率を無視して良いのなら簡単に対応はできる。

さて、宿題は一通り終えたので本編に進もう。

## 12.1
### Wrapping type with newtype.

```haskell
newtype CharList = CharList { getCharList :: [Char] } deriving (Eq, Show)

ghci> CharList "this will be shown!"
CharList { getCharList = "this will be shown!"}
ghci> CharList "benny" == CharList "benny"
True
ghci> CharList "benny" == CharList "oisters"
False
```

```lisp
(define-newtype char-list(&optional char)
  (declare(ignore char))
  'list)

cl-user> (char-list (coerce "this" 'list))
(#\t #\h #\i #\s)

cl-user> (equal (char-list (coerce "bunny" 'list))
                (char-list (coerce "bunny" 'list)))
T

cl-user> (equal (char-list (coerce "benny" 'list))
                (char-list (coerce "oister" 'list)))
NIL
```

Common Lispに於いて`THE`フォームは外側のフォームに型を伝えるためだけのものであるので、一度評価されてしまうと、それ以上型の伝播は行えなくなってしまう点要注意。

### Making instance of type class with newtype.

```haskell
newtype Pair b a = Pair { getPair :: (a, b) }

instance Functor (Pair c) where
    fmap f (pair (x, y)) = Pair (f x, y)

ghci> getPair $ fmap (*100) (Pair (2, 3))
(200,3)
ghci> getPair $ fmap reverse (Pair ("london calling", 3))
("gnillac nodnol", 3)
```

```lisp
(define-newtype pair(&optional b a)
  `(cons ,a ,b))

(definstance(functor pair)
  ((fmap(f pair)
     `(trivia:match ,pair
        ((cons x y)(pair (cons (funcall ,f x)y)))))))

cluser> (fmap (curried-function:section * _ 100)
              (pair '(2 . 3)))
(200 . 3)

cl-user> (fmap #'reverse (pair '("london calling" . 3)))
("gnillac nodnol" . 3)
```

パターンマッチをマクロ展開時ではなく実行時に行っている点要注意。
この点、かなり場当たり的であり、非常に不細工である。
下層の実装がどうなっているのか、またパターンマッチを展開時/実行時いずれに行うかを明示せねばならない点、非常によろしくない。
Haskellの美しさが際立つ点である。

## 12.2
### Monoid

```Haskell
class monoid m where
    mempty :: m
    mappend :: m -> m -> m
    mconcat :: [m] -> m
    mconcat = foldr mappend mempty
```

```lisp
(define-type-class(monoid m)()
  ((mempty()m)
   (mappend(m m)m)
   (mconcat((list m))m))
  (:default mconcat(ms)
    `(reduce (lambda(a m)(mappend a m))
             ,ms
             :from-end t
             :initial-value (mempty))))
```
我々の実装に於いて、インスタンスはマクロとして定義されているので、高階関数に渡すにはラムダで包まねばならない。
これもまた、非常に不細工な点である。
Haskellの美しさが際立つ。

## 12.3
### List as monoid.

```haskell
instance Monoid [a] where
    mempty = []
    mappend = (++)

ghci> [1,2,3] `mappend` [4,5,6]
[1,2,3,4,5,6]
ghci> ("one" `mappend` "two") `mappend` "three"
"onetwothree"
ghci> "one" `mappend` ("two" `mappend` "three")
"onetwothree"
ghci> "one" `mappend` "two" `mappend` "three"
"onetwothree"
ghci> "pang" `mappend` mempty
"pang"
ghci> mconcat [[1,2],[3,6],[9]]
```

```lisp
(definstance(monoid list)
  ((mempty()nil)
   (mappend(a b)
     `(funcall (curried-function:section append _ _)
                     ,a
                     ,b))))

cl-user> (mappend '(1 2 3)'(4 5 6))
(1 2 3 4 5 6)
```

Common Lispに於いて`LIST`と`STRING`は別々な型なので、`STRING`用のインスタンスを別途定義せねばならない。

```lisp
(definstance(monoid string)
  ((mempty()"")
   (mappend(a b)
     `(funcall (curried-function:section concatenate 'string _ _)
                     ,a ,b))))

cl-user> (mappend "one" (mappend "two" "three"))
"onetwothree"

cl-user> (mappend (mappend "one" "two")
                  "three")
"onetwothree"
```
Common Lispに於いて、演算子を中置にする機能は外部ライブラリを使わない限り難しいが、代わりと言ってはなんだが前置特有の可変長引数なら簡単に導入できる。

```lisp
(defmacro mappend* (&body form*)
  (labels((rec(form*)
            (if(endp(cdr form*))
              (car form*)
              `(mappend ,(car form*)
                        ,(rec (cdr form*))))))
    (rec form*)))

cl-user> (mappend* "one" "two" "three")
"onetwothree"

cl-user> (mappend "pang" (mempty))
"pang"

cl-user> (mconcat '((1 2)(3 6)(9)))
(1 2 3 6 9)
```

### Product

```haskell
newtype Product a = Product { getProduct :: a }
    deriving (Eq, Ord, Read, Show, Bounded)

instance Num a => Monoid (Product a) where
    mempty = Product 1
    Product x `mappend` Product y = Product (x * y)

ghci> getProduct $ Product 3 `mappend` Product 9
27
ghci> getProduct $ Product 3 `mappend` mempty
3
ghci> getProduct $ Product 3 `mappend` Product 4 `mappend` Product 2
24
ghci> getProduct . mconcat . map Product $ [3,4,2]
24
```

```lisp
(define-newtype product()
   'integer)

(definstance(monoid product)
  ((mempty()`(product 1))
   (mappend(a b)
     `(product (* ,a ,b)))))

cl-user> (mappend (product 3)(product 9))
27

cl-user> (mappend (product 3)(mempty))
3

cl-user> (mappend (product 3)
                  (mappend (product 4)
                                 (product 2)))
24

cl-user> (mappend* (product 3) (product 4) (product 2))
24

cl-user> (mconcat(the(list product)'(3 4 2)))
24
```
Haskellに於いてはリストの各要素に`Product`を`map`して型変換をした挙句`mconcat`に渡しているのに対し、我々の実装では単に`THE`を通して`MCONCAT`に型を伝えているだけな点要注意。

### Sum

```haskell
ghci> getSum $ Sum 2 `mappend` Sum 9
11
ghci> getSum $ mempty `mappend` Sum 3
3
ghci> getSum . mconcat map Sum $ [1,2,3]
6
```

```lisp
(define-newtype sum() 'integer)

(definstance(monoid sum)
  ((mempty()`(sum 0))
   (mappend(a b)
     `(sum (+ ,a ,b)))))

cl-user> (mappend (sum 2)(sum 9))
11

cl-user> (mappend (mempty)(sum 3))
3

cl-user> (mconcat (the (list sum)'(1 2 3)))
6
```

### Any

```haskell
newtype Any = Any { getAny :: Bool }
    deriving (Eq, Ord, Read, Show, Bounded)

instance Monoid Any where
    mempty = Any False
    Any x `mappend` Any y = Any (x || y)

ghci> getAny $ Any True `mappend` Any False
True
ghci> getAny $ mempty `mappend` Any True
True
ghci> getAny . mconcat . map Any $ [False,False,False,True]
Trhe
ghci> getAny $ mempty `mappend` mempty
False
```

```lisp
(define-newtype any() 'boolean)

(definstance(monoid any)
  ((mempty()`(any nil))
   (mappend(a b)
     `(or ,a ,b))))

cl-user> (mappend (any t)(any nil))
T
cl-user> (mappend (mempty)(any t))
T
cl-user> (mconcat (the (list any) '(nil nil nil t)))
T
cl-user> (mappend (the any (mempty))(mempty))
NIL
```
最後の例に見られるように、我々の実装では`getAny`の必要が無い分、コンパイラが型推論のヒントとすべき型情報がなくなってしまうので、代わりに`THE`フォームで`ANY`型である点を伝えてあげる必要が生じる。

### All

```haskell
newtype All = All { getAll :: Bool }
    deriving (Eq, Ord, Read, Show, Bounded)

instance Monoid All where
    mempty = All True
    All x `mappend` All y = All (x && y)

ghci> getAll $ mempty `mappend` All True
True
ghci> getAll $ mempty `mappend` All False
False
ghci> getAll . mconcat . map All $ [True, True, True]
True
ghci> getAll . mconcat . map All $ [True, True, False]
False
```

```lisp
(define-newtype all() 'boolean)

(definstance(monoid all)
  ((mempty()`(all t))
   (mappend(a b)
     `(and ,a ,b))))

cl-user> (mappend (mempty)(all t))
T

cl-user> (mappend (mempty)(all nil))
NIL

cl-user> (mconcat (the (list all) '(t t t)))
T
cl-user> (mconcat (the (list all) '(t t nil)))
NIL
```

### Ordering

```haskell
instance Monoid Ordering where
    mempty = EQ
    LT `mappend` _ = LT
    EQ `mappend` y = y
    GT `mappend` _ = GT

ghci> LT `mappend` GT
LT
ghci> GT `mappend` LT
GT
ghci> mempty `mappend` LT
LT
ghci> mempty `mappend` GT
GT

lengthCompare :: String -> String -> Ordering
lengthCompare x y = (length x `compare` length y) `mappend`
                    (x `compare` y)

ghci> lengthCompare "zen" "ants"
LT
ghci> lengthCompare "zen" "ant"
GT

lengthCompare :: String -> String -> Ordering
lengthCompare x y = (length x `compare` length y) `mappend`
                    (vowels x `compare` vowels y) `mappend`
                    (x `compare` y)
    where vowels = length . filter (`elem` "aeiou")

ghci> lengthCompare "zen" "anna"
LT
ghci> lengthCompare "zen" "ana"
LT
ghci> lengthCompare "zen" "ann"
GT
```

```lisp
(defdata ordering()
  :lt :eq :gt)

(definstance(monoid ordering)
  ((mempty():eq)
   (mappend(a b)
     `(trivia:ematch*(,a ,b)
        ((:lt _):lt)
        ((:eq y)y)
        ((:gt _):gt)))))

cl-user> (mappend :lt :gt)
:LT
cl-user> (mappend :gt :lt)
:GT
cl-user> (mappend (mempty) :lt)
:LT
cl-user> (mappend (mempty) :gt)
:GT
```
Common Lispに於いてリストとストリングは明確に区別されるので、`STRING`向けの`COMPARE`を定義する必要がある。


```lisp
(definstance(compare string)
  ((lt(a b)
     `(string< ,a ,b))
   (lte(a b)
     `(string<= ,a ,b))
   (gt(a b)
     `(string> ,a ,b))
   (gte(a b)
     `(string>= ,a ,b))
   (compare(a b)
     `(cond
        ((string= ,a ,b):eq)
        ((string< ,a ,b):lt)
        (t :gt)))))

(defun length-compare(x y)
  (mappend (compare (length x)(length y))
           (compare x y)))

cl-user> (length-compare "zen" "ants") => :LT
cl-user> (length-compare "zen" "ant") => :GT

(defun length-compare2(x y)
  (flet((vowels(x)
          (count-if (curried-function:section find _ "aeiou")
                    x)))
    (declare(ftype (function(string)fixnum)vowels))
    (mappend* (compare (length x)(length y))
              (compare (vowels x)(vowels y))
              (compare x y))))

cl-user> (length-compare2 "zen" "anna")
:LT
cl-user> (length-compare2 "zen" "ana")
:LT
cl-user> (length-compare2 "zen" "ann")
:GT
```
なお、単に辞書順の比較であるなら、Common Lispは`STRING<`、`STRING<=`、`STRING>`、`STRING>=`、`STRING-LESSP`、`STRING-NOT-LESSP`、`STRING-GREATERP`、`STRING-NOT-GREATERP`を擁している。

```lisp
cl-user> (string< "zen" "ants")
NIL
```

### Maybe monoid.

```haskell
instance Monoid a => Monoid (Maybe a) where
    mempty = nothing
    Nothing `mappend` m = m
    m `mappend` Nothing = m
    Just m1 `mappend` Just m2 = Just (m1 `mappend` m2)

ghci> Nothing `mappend` Just "andy"
Just "andy"
ghci> Just LT `mappend` Nothing
Just LT
ghci> Just (Sum 3) `mappend` Just (Sum 4)
Just (Sum {getSum = 7})
```

```lisp
(definstance(monoid maybe)
  ((mempty()nothing)
   (mappend(a b)
     (trivia:ematch*(a b)
       ((nothing m)m)
       ((m nothing)m)
       (((just m1)(just m2))
        `(just(mappend ,m1 ,m2)))))))

cl-user> (mappend nothing (just "andy"))
(JUST "andy")

cl-user> (mappend (just :lt)nothing)
(JUST :LT)

cl-user> (mappend (just (sum 3))(just (sum 4)))
(JUST 7)
```

```haskell
newtype First a = First { getFirst :: Maybe a }
    deriving (Eq, Ord, Read, Show)

instance Monoid (First a) where
    mempty = First Nothing
    First (Just x) `mappend` _ = First (Just x)
    First Nothing `mappend` x = x

ghci> getFirst $ First (Just 'a') `mappend` First (Just 'b')
Just 'a'
ghci> getFirst $ First Nothing `mappend` First (Just 'b')
Just 'b'
ghci> getFirst $ First (Just 'a) `mappend` First Nothing
Just 'a'
ghci> getFirst . mconcat . map First $ [Nothing, Just 9, Just 10]
Just 9
```

```lisp
(define-newtype 1st(&optional a)
  `(maybe ,a))

(definstance(monoid 1st)
  ((mempty()'(1st nothing))
   (mappend(a b)
     `(trivia:ematch*(,a ,b)
        (((just x)_)(1st(just x)))
        ((nothing x)x)))))

cl-user> (mappend (1st(just #\a))(1st(just #\b)))
(just #\a)

cl-user> (mappend (1st nothing)(1st (just #\b)))
(JUST #\b)

cl-user> (mappend (1st (just #\a))(1st nothing))
(JUST #\a)

cl-user> (mconcat (the (list 1st)'(nothing (just 9)(just 10))))
(JUST 9)
```
