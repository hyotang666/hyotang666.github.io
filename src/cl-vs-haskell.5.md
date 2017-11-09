# Common Lisp vs Haskell, Chapter 5
## Meta note
### 対象読者
[前章](archives/cl-vs-haskell.4.html)を読了済みの者。

## Introduction
本稿は「すごいH本」の内容をCommon Lispに翻訳しながらCLerがHaskellを学ぶその第5章である。
本章のテーマは高階関数である。
デフォルトで関数がカリー化されているHaskellのシンプルで美しい筆記には目を見張るものがある。
また、デフォルトで遅延評価がされていることによる効率の良さも素晴らしい。
さて、それに対してCommon Lispはというと、それなりに健闘しているのではないかと思う。
Haskellはまさに関数型の記述を行うための言語だ。
それに対しCommon Lispはけして関数型の記述を行うためだけの言語ではない。
その割にはそれっぽいアプローチもしっかり取れてしまうあたりマルチパラダイムの面目躍如であろう。
本章のハイライトは、関数合成のためのリーダマクロを導入、SERIESが想定外の大活躍といったあたりか。
中級CLerにとっては面白い内容になっているのではないかと思われる。
割と長文であるし、中身も濃いと思うのでお暇な時にどうぞ。

# 5
## 5.1
### section

```haskell
divideByTen :: (Floating a) => a -> a
devideByTen = (/10)
ghci> devideByTen 200
20.0
ghci> 200 / 10
20.0
ghci> (/10) 200
20.0
```
Common Lispの関数はカリー化されていない。
部分適用された関数が欲しいなら、ALEXANDRIAの`CURRY`、`RCURRY`を使う。
また、合成には同じくALEXANDRIAの`COMPOSE`が使える。

```lisp
(declaim(ftype(function(float)float)/10))
(defun /10 (float)
  (float(/ float 10)))
cl-user> (/10 200)
20.0
cl-user> (float(/ 200 10))
20.0
cl-user> (funcall (alexandria:rcurry (alexandria:compose #'float #'/) 10)200)
20.0
```
少々長ったらしいのでリーダマクロを書いてしまおう。
以下のように書けるものとする。

```lisp
#+design
(funcall #`(% #`(+ 'float '/) _ 10)200)
```
実装は以下の通り。

```lisp
(defun |#`-reader|(stream character number)
  (declare(ignore character number))
  (let((form(read stream t t t)))
    (ecase(car form)
      (and `(alexandria:conjoin ,@(cdr form)))
      (or `(alexandria:disjoin ,@(cdr form)))
      (+ `(alexandria:compose ,@(cdr form)))
      (% (make-form (cdr form))))))

(defun make-form(form)
  (flet((underscorep(x)
          (and (symbolp x)
               (string= '#:_ x))))
    (let((position(position-if #'underscorep form)))
      (if position
        (progn (assert (not (find-if #'underscorep form :start (1+ position))))
               (if (= 1 position)
                 `(alexandria:rcurry ,(car form) ,@(cddr form))
                 `(alexandria:rcurry (alexandria:curry ,(car form)
                                                       ,@(subseq form 1 position))
                                     ,@(nthcdr (1+ position)form))))
        `(alexandria:curry ,@form)))))
```
リードテーブルは以下の通り。

```lisp
(named-readtables:defreadtable :higher-order-syntax
  (:merge :standard)
  (:dispatch-macro-char #\# #\` #'|#`-reader|))
```
## 5.2

```haskell
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)
gchi> applyTwice (+3) 10
16
ghci> applyTwice (++ " HAHA") "HEY"
"HEY HAHA HAHA"
ghci> applyTwice ("HAHA " ++) "HEY"
"HAHA HAHA HEY"
ghci> applyTwice (3:)[1]
[3,3,1]
```

```lisp
(declaim(ftype(function(function t)t)apply-twice))
(defun apply-twice(f x)
  (funcall f(funcall f x)))
cl-user> (apply-twice #`(% '+ 3)10)
16
cl-user> (apply-twice #`(% 'uiop:strcat _ " HAHA")"HEY")
"HEY HAHA HAHA"
cl-user> (apply-twice #`(% 'uiop:strcat "HAHA ") "HEY")
"HAHA HAHA HEY"
cl-user> (apply-twice #`(% 'cons 3)'(1))
(3 3 1)
```

### zipWith

```haskell
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith f xs ys

ghci> zipWith' (+) [4,2,5,6] [2,6,2,3]
[6,8,7,9]
ghci> zipWith' max [6,3,2,1] [7,3,1,5]
[7,3,2,5]
ghci> zipWith' (++) ["foo ", "bar ", "baz "] ["fighters", "hoppers", "aldrin"]
["foo fighters", "bar hoppers", "baz aldrin"]
ghci> zipWith' (zipWith' (*)) [[1,2,3],[3,5,6],[2,3,4]] [[3,2,2],[3,4,5],[5,4,3]]
[[3,4,6],[9,20,30],[10,12,12]]
```
Haskellの`zipWith`はCommon Lispの`MAPCAR`に相当する。

```lisp
cl-user> (mapcar #'+ '(4 2 5 6)'(2 6 2 3))
(6 8 7 9)
cl-user> (mapcar #'max '(6 3 2 1)'(7 3 1 5))
(7 3 2 5)
cl-user> (mapcar #'uiop:strcat '("foo " "bar " "baz ") '("fighters" "hoppers" "aldrin"))
("foo fighters"  "bar hoppers"  "baz aldrin")
cl-user> (mapcar #`(% 'mapcar '*)'((1 2 3)(3 5 6)(2 3 4))'((3 2 2)(3 4 5)(5 4 3)))
((3 4 6)(9 20 30)(10 12 12))
```

### flip

```haskell
flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
    where g x y = f y x
;; or
flip' :: (a -> b -> c) -> b -> a -> c
flip' f y x = f x y

ghci> zip [1,2,3,4,5] "hello"
[(1,'h'),(2,'e'),(3,'l'),(4,'l'),(5,'o')]
ghci> flip' zip [1,2,3,4,5] "hello"
[('h',1),('e',2),('l',3),('l',4),('o',5)]
ghci> zipWith div [2,2..][10,8,6,4,2]
[0,0,0,0,1]
ghci> zipWith (flip' div) [2,2..] [10,8,6,4,2]
[5,4,3,2,1]
```
Haskellの`flip`はINCF-CLにある。

```lisp
cl-user> (map 'list #'cons '(1 2 3 4 5) "hello")
((1 . #\h)(2 . #\e)(3 . #\l)(4 . #\l)(5 . #\o))
cl-user> (map 'list (incf-cl:flip #'cons)'(1 2 3 4 5) "hello")
((#\h . 1)(#\e . 2)(#\l . 3)(#\l . 4)(#\o . 5))
cl-user> (mapcar #'truncate '(2 2 2 2 2)'(10 8 6 4 2))
(0 0 0 0 1)
cl-user> (mapcar (incf-cl:flip #'truncate)'(2 2 2 2 2)'(10 8 6 4 2))
(5 4 3 2 1)
```
## 5.3
### map

```haskell
map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x,xs) = f x : map f xs

ghci> map (+3) [1,5,3,1,6]
[4,8,6,4,9]
ghci> map (++ "!")["BIFF", "BANG", "POW"]
["BIFF!","BANG!","POW!"]
ghci> map (replicate 3) [3..6]
[[3,3,3][4,4,4][5,5,5][6,6,6]]
ghci> map (map (^2)) [[1,2],[3,4,5,6],[7,8]]
[[1,4],[9,16,25,36],[49,64]]
ghci> map fst [(1,2),(3,5),(6,3),(2,6),(2,5)]
[1,3,6,2,2]
```
Haskellの`map`は通常Common Lispの`MAPCAR`に相当する。

```lisp
cl-user> (mapcar #`(% '+ 3)'(1 5 1 3 6))
(4 8 4 6 9)
cl-user> (mapcar #`(% 'uiop:strcat _ "!")'("BIFF" "BANG" "POW"))
("BIFF!" "BANG!" "POW!")
cl-user> (mapcar #`(% 'mapcar #`(% 'expt _ 2))'((1 2)(3 4 5 6)(7 8)))
((1 4)(9 16 25 36)(49 64))
cl-user> (mapcar #'car '((1 2)(3 5)(6 3)(2 6)(2 5)))
(1 3 6 2 2)
```

### filter

```haskell
filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (x:xs)
    | p x       = x : filter p xs
    | otherwise = filter p xs

ghci> filter (>3) [1,5,3,2,1,6,4,3,2,1]
[5,6,4]
ghci> filter (==3) [1,2,3,4,5]
[3]
ghci> filter even [1..10]
[2,4,6,8,10]
ghci> let notNull x = not (null x) in filter notNull [[1,2,3],[],[3,4,5],[2,2],[],[],[]]
[[1,2,3],[3,4,5],[2,2]]
ghci> filter (`elm` ['a'..'z']) "u LaUgH aT mE BeCaUsE I aM diFfeRent"
"uagameasadifeent"
ghci> filter (`elm` ['A'..'Z']) "i LAuGh at you bEcause u R all the same"
"LAGER"
```
Haskellの`filter`はCommon Lispの`REMOVE-IF-NOT`に相当する。

```lisp
cl-user> (remove-if-not #`(% '> _ 3)'(1 5 3 2 1 6 4 3 2 1))
(5 6 4)
cl-user> (remove-if-not #`(% '= 3) '(1 2 3 4 5))
(3)
cl-user> (remove-if #'null '((1 2 3)()(3 4 5)(2 2)()()()))
((1 2 3)(3 4 5)(2 2))
cl-user> (remove-if-not #'lower-case-p "u LaUgH aT mE BeCaUsE I aM diFfeRent")
"uagameasadifeent"
cl-user> (remove-if-not #'upper-case-p "i LAuGh at you bEcause u R all the same")
"LAGER"
```

```haskell
ghci> filter (<15) (filter even [1..20])
[2,4,6,8,10,12,14]
ghci> [x | x <- [1..20], x < 15, even x]
[2,4,6,8,10,12,14]
```

```lisp
cl-user> (remove-if-not #`(% '< _ 15)(remove-if-not #'evenp (incf-cl:range 1 20)))
(2 4 6 8 10 12 14)
cl-user> (incf-cl:lc x (incf-cl:<- x(incf-cl:range 1 20))(< x 15)(evenp x))
(2 4 6 8 10 12 14)
cl-user> (loop :for i :upfrom 1 :to 20 :when (and (< i 15)(evenp i)) :collect i)
(2 4 6 8 10 12 14)
```
なお、`REMOVE-IF-NOT`は新しいリストを作って返すので、上記のようにネストして使うとメモリ効率が悪い。
上記３つのコードの中では、余計な中間リストを作らない`LOOP`が最も効率が良い。

### extra examples about map and filter

```haskell
largestDivisible :: Integer
largestDivisible = head (filter p [100000,99999..])
    where p x = x `mod` 3829 == 0
```

```lisp
(declaim(ftype(function()Integer)largest-divisible))
(defun largest-divisible()
  ((lambda(p)(car(remove-if-not p (alexandria:iota 100000 :start 100000 :step -1))))
   (lambda(x)(zerop(mod x 3829)))))
;; or
(defun largest-divisible()
  (loop :for x :downfrom 100000
        :when (zerop(mod x 3829))
        :do(return x)))
;; or
(defun largest-divisible()
  (series:collect-first (series:choose-if (lambda(x)(zerop(mod x 3829)))
                                          (series:scan-range :from 100000 :by -1))))
```
Common Lispは遅延評価を行わない言語のため、上記コードも`LOOP`の方が効率がよく望ましい。
筆記順をHaskellと同様にしつつ効率を求めるならSERIESが望ましい。


```haskell
ghci> takeWile (/=' ') "elephants know how to party"
"elephants"
ghci> sum (takeWhile (<10000) (filter odd (map (^2) [1..])))
166650
ghci> sum (takeWhile (<10000) [m | m <- [n^2 | n <- [1..]], odd m])
166650
```
Common LispにHaskellの`takeWhile`相当の関数はない。
素のCommon Lispで書くなら以下のようになる。

```lisp
cl-user> (let((string "elephants know how to party"))
           (subseq string 0 (position #\space string)))
"elephants"
cl-user> (loop :for i :upfrom 0
               :for j = (expt i 2)
               :while (< j 10000)
               :when (oddp j)
               :sum j)
166650
```
Seriesを使うなら以下のようになる。

```lisp
cl-user> (series:collect-sum
           (series:until-if (complement #`(% '< _ 10000))
                            (series:choose-if #'oddp
                                              (series:map-fn 'integer
                                                             #`(% 'expt _ 2)
                                                             (series:scan-range :from 0)))))
166650
```
また、INCF-CLとSERAPEUMは各々`TAKE-WHILE`を提供している。
なお、INCF-CLの方はリストにしか使えない。
（METHODで実装されているので拡張は簡単だろうが、自分で書かなくてはならない。）


```haskell
chain :: Integer -> [Integer]
chain 1 = [1]
chain n
    | even n = n : chain (n `div`2)
    | odd n  = n : chain (n * 3 + 1)


numLongChains :: Int
numLongChains = length (filter isLong (map chain [1..100]))
    where isLong xs = length xs > 15
```

```lisp
(declaim(ftype(function(integer)list)chain))
(defun chain(integer)
  (labels((rec(n &optional acc)
            (if(= 1 n)
              (nreconc acc (list 1))
              (if(evenp n)
                (rec (/ n 2)(cons n acc))
                (rec (1+ (* 3 n))(cons n acc))))))
    (rec integer)))

(declaim(ftype(function()integer)num-long-chains))
(defun num-long-chains()
  ((lambda(long-p)
    (length (delete-if-not long-p (mapcar #'chain (incf-cl:range 1 100)))))
   (lambda(xs)(> (length xs)15))))
;; or
(defun num-long-chains()
  (loop :for i :upfrom 1 :to 100
        :when (< 15 (length(chain i)))
        :count it))
```
本例もまた、余計な中間リストを作らない分`LOOP`の方が望ましい。

Seriesで書くなら以下の通り。

```lisp
(series:collect-length(series:choose-if (lambda(l)(> (length l)15))
                                        (series:map-fn 'list #'chain
                                                       (series:scan-range :from 1 :upto 100))))
```

### Applying some arguments to map

```haskell
ghci> let listOfFuns = map (*) [0..]
ghci> (listOfFuns !! 4) 5
20
```
遅延リストに依存したコードなのでSERIESを使わないと再現できない。

```lisp
cl-user> (defvar *list-of-funs* (series:map-fn 'function #`(% 'alexandria:curry '*)
                                               (series:scan-range :from 0)))
*LIST-OF-FUNS*
cl-user> (funcall (series:collect-nth 4 *list-of-funs*)5)
20
```

### lambda expression

```haskell
numLongChains :: Int
numLongChains = length (filter (\xs -> length xs > 15)
                               (map chain [1..100]))
```

```lisp
(declaim(ftype(function()fixnum)num-long-chains))
(defun num-long-chains()
  (length(remove-if-not (lambda(xs)(> (length xs) 15))
                        (mapcar #'chain (incf-cl:range 1 100)))))
```

余計な中間リストが出来る分、非効率的である。
前々節の再掲となるが、 Seriesで書くなら以下の通り。

```lisp
(series:collect-length(series:choose-if (lambda(xs)(> (length xs)15))
                                        (series:map-fn 'list #'chain
                                                       (series:scan-range :from 1 :upto 100))))
```

```haskell
ghci> zipWith (\a b -> (a * 30 + 3) / b) [5,4,3,2,1] [1,2,3,4,5]
[153.0,61.5,31.0,15.75,6.6]
ghci> map (\(a,b) -> a + b) [(1,2),(3,5),(6,3),(2,6),(2,5)]
[3,8,9,8,7]
```

```lisp
cl-user> (mapcar (lambda(a b)(float(infix-math:$ (a * 30 + 3) / b))) '(5 4 3 2 1)'(1 2 3 4 5))
(153.0 61.5 31.0 15.75 6.6)
cl-user> (mapcar (trivia:lambda-match((cons a b)(+ a b))) '((1 . 2)(3 . 5)(6 . 3)(2 . 6)(2 . 5)))
(3 8 9 8 7)
```
引数がドット対でなくプロパリストなら`APPLY`を使って以下のように書ける。

```lisp
cl-user> (mapcar #`(% 'apply '+) '((1 2)(3 5)(6 3)(2 6)(2 5)))
(3 8 9 8 7)
```

## 5.5
### foldl

```haskell
ghci> :t foldl
foldl :: (a -> b -> a) -> a -> [b] -> a

sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs

;; or
sum' = foldl (+) 0

ghci> sum' [3,5,2,1]
11
```
Haskellの`foldl`はCommon Lispでは`REDUCE`に相当する。
なお、Common Lispに於いて`REDUCE`は、必ずしも効率の良いオペレータではないので、代替案も同時に記しておく。

```lisp
(reduce #'+ '(3 5 2 1))
11
;; or
(apply #'+ '(3 5 2 1))
```

### foldr
Haskellの`foldr`はCommon Lispでは`REDUCE`に`:FROM-END`キーワード引数を渡して再現することとなる。

```lisp
;; foldL
(reduce #'cons '(1 2 3 4 5) :initial-value 9)
((((9 . 1) . 2) . 3) . 4 . 5)
1> (cons 9 1)
2> (cons * 2)
3> (cons * 3)
4> (cons * 4)
5> (cons * 5)

;; foldR
(reduce #'cons '(1 2 3 4 5) :initial-value 9 :from-end t)
(1 2 3 4 5 . 9)
1> (cons 5 9)
2> (cons 4 *)
3> (cons 3 *)
4> (cons 2 *)
5> (cons 1 *)
;; or
(multiple-value-call #'list* (values-list '(1 2 3 4 5)) 9)
```

### examples

```haskell
reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []
```

```lisp
(declaim(ftype(function(list)list)reverse%))
(defun reverse%(list)
  (reduce (lambda(x y)(cons y x))list :initial-value nil))
```

```haskell
product' :: (Num a) => [a] -> a
product' = foldl (*) 1
```

```lisp
(declaim(ftype(function(list)integer)product))
(defun product (list)
  (reduce #'* list))
;; or
(apply #'* list)
```

```haskell
filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x acc -> if p x then x : acc else acc) []
```

```lisp
(declaim(ftype(function(function list)list)filter))
(defun filter(pred list)
  (reduce (lambda(x acc)
            (if(funcall pred x)
              (cons x acc)
              acc))
          list
          :initial-value nil :from-end t))
;; or
(remove-if-not pred list)
```

```haskell
last' :: [a] -> a
last' = foldl1 (\_ x -> x)
```

```lisp
(declaim(ftype(function(list)t)last1))
(defun last1(list)
  (reduce (lambda(_ x)(declare(ignore _))x)list))
;; or
(alexandria:lastcar list)
```

### another view.

### reducing infinite list.

```haskell
and' :: [Bool] -> Bool
and' xs = foldr (&&) True xs
```

```lisp
(declaim(ftype(function(list)boolean)and%))
(defun and%(list)
  (reduce (lambda(x y)(and x y))list))
;; or
(every #'identity list)
```
Common Lispは遅延リストをサポートしていないので、`REDUCE`を使ったバージョンは`NIL`に出会っても走査は止まらない。
リストを最後まで舐めつくしてしまう。

Common Lispにはまさにそのためのオペレータ`EVERY`が提供されている。
なお、少々分かりづらいと思うので、念の為説明しておくが、上記`IDENTITY`は`(lambda(x)(not(null x)))`と等価である。
なんなら以下のように書いてもよい。

```lisp
(notany #'null list)
```
個人的には`NOTANY`と`NOTEVERY`は分かりづらいと思っているので、`SOME`、`EVERY`の方が好きだ。

なお、SERIESには`COLLECT-AND`があるので、無限リストが必要になった場合はそれで対応可能である。

### scan

```haskell
ghci> scanl (+) 0 [3,5,2,1]
[0,3,8,10,11]
ghci> scanr (+) 0 [3,5,2,1]
[11,8,3,1,0]
ghci> scanl1 (\acc x -> if x > acc then x else acc) [3,4,5,3,7,9,2,1]
[3,4,5,5,7,9,9,9]
ghci> scanl (flip(:)) [] [3,2,1]
[[],[3],[2,3],[1,2,3]]
```
Common Lispに上記`scan`のファミリーは存在しない。
serapeumが`SCAN`を、INCF-CLが`SCAN*`を提供している。

```lisp
cl-user> (serapeum:scan #'+ '(3 5 2 1) :initial-value 0)
(0 3 8 10 11)
cl-user> (incf-cl:scan* #'+ '(3 5 2 1) :initial-value 0)
(0 3 8 10 11)
```
Haskellの`scanr`はINCF-CLのみが対応可能となっている。

```lisp
cl-user> (incf-cl:scan* #'+ '(3 5 2 1) :initial-value 0 :from-end T)
(11 8 3 1 0)
```
Haskellの`scanl1`は単に`:INITIAL-VALUE`を渡さなければ良い。

```lisp
cl-user> (serapeum:scan (lambda(acc x)(if(< acc x)x acc))'(3 4 5 3 7 9 2 1))
(3 4 5 5 7 9 9 9)
cl-user> (incf-cl:scan* (lambda(acc x)(if(< acc x)x acc))'(3 4 5 3 7 9 2 1))
(3 4 5 5 7 9 9 9)
```
serapeumの`SCAN`はhaskellの`scanl`とは異なる挙動をする。
（狙ってこのような挙動に敢えてしてある可能性があるので、ここではバグとは言わない。）

```lisp
cl-user> (serapeum:scan (incf-cl:flip #'cons) '(3 2 1) :initial-value nil)
(NIL (NIL . 3) ((NIL . 3) . 2) (((NIL . 3) . 2) . 1))
cl-user> (incf-cl:scan* (incf-cl:flip #'cons) '(3 2 1) :initial-value nil)
(NIL (3) (2 3) (1 2 3))
```
Serapeumの方はシーケンスに対応しているのが魅力となる。
リストにしか使わないのであればINCF-CLの方がよろしかろう。
なお、双方とも`:KEY`キーワード引数をサポートしている。

```haskell
sqrtSums :: Int
sqrtSums = length (takeWhile (<1000)(scanl1 (+) (map sqrt [1..]))) + 1
ghci> sqrtSums
131
ghci> sum (map sqrt [1..131])
1005.0942035344083
ghci> sum (map sqrt [1..130])
993.6486803921487
```
Common Lispは遅延リストをサポートしていないので、上記Haskellコードを直訳すると、中間リストが作られまくり効率が著しく悪くなる。
`LOOP`マクロで対応するのが望ましい。

```lisp
(declaim(ftype(function()integer)sqrt-sums))
(defun sqrt-sums()
 (1+ (loop :for i :upfrom 1
           :sum (sqrt i) :into sums
           :while (< sums 1000)
           :count sums)))
cl-user> (sqrt-sums)
131
cl-user> (loop :for i :upfrom 1 :to 131 :sum (sqrt i))
1005.0941
cl-user> (loop :for i :upfrom 1 :to 130 :sum (sqrt i))
993.6486
```

## 5.6

```haskell
ghci> sum (filter (> 10) (map (*2) [2..10]))
80
ghci> sum $ filter (> 10) (map (*2) [2..10])
80
ghci> sum $ filter (> 10) $ map (*2) [2..10]
80

ghci> map ($ 3) [(4+),(10*),(^2), sqrt]
[7.0,30.0,9.0,1.7320508075688772]
```

```lisp
cl-user> (apply #'+ (remove-if-not #`(% '> _ 10) (mapcar #`(% '* 2)(incf-cl:range 2 10))))
80
cl-user> (loop :for i :upfrom 2 :to 10
               :for j = (* 2 i)
               :when (< 10 j)
               :sum j)
80
cl-user> (series:collect-sum(series:choose-if #`(% '> _ 10)
                                              (series:map-fn 'fixnum #`(% '* 2)
                                                             (series:scan-range :from 2 :upto 10))))
80
```
直訳は中間リストが多く作られるので効率が悪い。
`LOOP`マクロなら中間リストは作られないので効率が良い。
シンタックスをHaskellを近づけたいならSERIESが良い。

```lisp
cl-user> (mapcar #`(% 'funcall _ 3) `(,#`(% '+ 4) ,#`(% '* 10) ,#`(% 'expt _ 2) sqrt))
(7 30 9 1.7320508)
```

## 5.7

```haskell
ghci> map (\x -> negate (abs x))[5,-3,-6,7,-3,2,-19,24]
[-5,-3,-6,-7,-3,-2,-19,-24]
ghci> map (negate . abs) [5,-3,-6,7,-3,2,-19,24]
[-5,-3,-6,-7,-3,-2,-19,-24]
ghci> map (\xs -> negate (sum (tail xs)))[[1..5],[3..6],[1..7]]
[-14,-15,-27]
ghci> map (negate . sum . tail)[[1..5],[3..6],[1..7]]
[-14,-15,-27]
```
関数合成はalexandriaの`COMPOSE`で行える。
本章冒頭で導入したリーダマクロでは`+`オペレータが関数合成を担う。

```lisp
cl-user> (mapcar #`(+ '- 'abs)'(5 -3 -6 7 -3 2 -19 24))
(-5 -3 -6 -7 -3 -2 -19 -24)
cl-user> (mapcar #`(+ '- #`(% 'apply '+) 'cdr)
                 (list (incf-cl:range 1 5)(incf-cl:range 3 6)(incf-cl:range 1 7)))
(-14 -15 -27)
```

### compose

```haskell
ghci> sum (replicate 5 (max 6.7 8.9))
====> (sum . replicate 5) (max 6.7 8.9)
====> sum . replicate 5 $ max 6.7 8.9

ghci> replicate 2 . product . map (*3) $ zipWith max [1,2] [4,5]
```

```lisp
cl-user> (funcall #`(+ #`(% 'make-list 2 :initial-element _) #`(% 'apply '*) #`(% 'mapcar #`(% '* _ 3)))
                  (mapcar #'max '(1 2)'(4 5)))
;; or
cl-user> (make-list 2 :initial-element (apply #'* (mapcar #`(% '* 3)(mapcar 'max '(1 2)'(4 5)))))
;; or
cl-user> (make-list 2 :initial-element (loop :for a :in '(1 2)
                                             :for b :in '(4 5)
                                             :for c = (* 3 (max a b)) :then (* c 3 (max a b))
                                             :finally (return c)))
;; or
cl-user> (make-list 2 :initial-element (series:collect-product (series:map-fn 'fixnum #`(% '* 3)
                                                                              (series:map-fn 'fixnum #'max (series:scan '(1 2))(series:scan '(4 5))))))
```
Common Lispで無理に関数合成を駆使しようとすると、かえって読みづらくなる気がする。
Haskellの美しさが際立つ例かと思う。
この例ではSERIESが美しさと効率の両立で健闘しているように見える。

### point free style

```haskell
sum' :: (Num a) => [a] -> a
sum' xs = foldl (+) 0 xs
;; or
sum' = foldl (+) 0

fn x = ceiling (negate (tan (cos (max 50 x))))
;; or
fn = ceiling . negate . tan . cos . max 50
```

```lisp
(declaim(ftype(function(list)fixnum)sum))
(defun sum(list)
  (apply #'+ list))
;; or
(setf (symbol-function 'sum) #`(% 'apply '+))

(defun fn (x)
  (ceiling(- (tan (cos (max 50 x))))))
;; or
(setf (symbol-function 'fn) #`(+ 'ceiling '- 'tan 'cos #`(% 'max 50)))
```

関数を返すコマンドの返り値を`SYMBOL-FUNCTION`に代入すれば、暗黙裏の仮引数に依存出来て明示的にラムダリストを書かなくても済むようになるが、「ポイントフリースタイル」だとかいう仰々しい名前をつけて騒ぐほどのことではないと思う。
ただし、それはCommon Lispに於いては、の話であって、Haskellに於いてはほぼ同じシンタックスで仮引数を無くせるというのは、なかなか面白い現象で、そのような特徴的な現象には名前がついていてしかるべきだろうとも思う。
