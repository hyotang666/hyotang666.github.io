# Common Lisp vs Haskell, Chapter 4
## Meta note
### 対象読者
[前章](archives/cl-vs-haskell.3.html)を読了済みの者。

## Introduction
本稿は「すごいH本」の内容をCommon Lispに翻訳しながらCLerがHaskellを学ぶその第4章である。
本章のテーマは再帰である。
そのため基本ライブラリにあるような機能を敢えて再発明している。
しかしながらCLerにとって再帰は自家薬籠中のものである。
今更再帰を学ぶもない。
そこで、本章では原著をリスペクトし、敢えて再発明再帰コードも掲載するが、それと同時に再発明している機能を再発明せずに素のCommon Lispで書くならどう書くかも同時に記していくこととする。
初心者CLerにとっては、特に末尾再帰への変換の仕方が参考になるかと思われる。
中級以上の方は読み飛ばして、どうぞ。

# 4
## 4.1
### maximum

```haskell
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list!"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)
```

```lisp
(declaim(ftype(function(list)t)maximum))
(defun maximum(list)
  (labels((rec(list acc)
            (if(endp list)
              acc
              (rec(cdr list)(max (car list)acc)))))
    (trivia:match list
      (nil (error "Maximum of empty list!"))
      ((list x)x)
      ((cons x y)(rec y x)))))
;; or
(defun maximum(list)
  (apply #'max list))
```
## 4.2
### replicate

```haskell
replicate' :: Int -> a -> [a]
replicate' n x
    | n <= 0 = []
    | otherwise = x :replicate' (n-1) x
```

```lisp
(declaim(ftype(function(integer t)list)))
(defun replicate(num elt)
  (labels((rec(num &optional acc)
            (if(<= num 0)
              acc
              (rec(1- num)(cons elt acc)))))
    (rec num)))
;; or
(defun replicate(num elt)
  (make-list num :initial-element elt))
```

### take

```haskell
take' :: Int -> [a] -> [a]
take' n _
    | n<=0 = []
take' _ [] = []
take' n (x:xs) = x : take' (x-1) xs
```

```lisp
(declaim(ftype(function(integer list)list)))
(defun take(num list)
  (labels((rec(num list &optional acc)
            (if(or (<= num 0)
                   (endp list))
              (nreverse acc)
              (rec(1- num)(cdr list)(cons(car list)acc)))))
    (rec num list)))
;; or
(defun take(number list)
  (loop :for elt :in list
        :repeat (max 0 number)
        :collect elt))
```

### reverse

```haskell
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]
```

```lisp
(declaim(ftype(function(list)list)reverse%))
(defun reverse%(list)
  (labels((rec(list &optional acc)
            (if(endp list)
              acc
              (rec(cdr list)(cons (car list) acc)))))
    (rec list)))
```

### repeat

```haskell
repeat' :: a -> [a]
repeat' x = x : repeat' x
```
Common Lispは遅延評価をする言語ではないので、こればかりは再帰で書けない点要注意。

```lisp
(declaim(ftype(function(t)series:series)repeat))
(defun repeat(elt)
  (let((list(list elt)))
    (rplacd list list)))
```

### zip

```haskell
zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys
```

```lisp
(declaim(ftype(function(list list)list)zip))
(defun zip(l1 l2)
  (labels((rec(l1 l2 &optional acc)
            (if(or (null l1)(null l2))
              (nreverse acc)
              (rec (cdr l1)(cdr l2)(acons(car l1)(car l2)acc)))))
    (rec l1 l2)))
;; or
(defun zip(l1 l2)
  (mapcar #'cons l1 l2))
;; or
(setf (symbol-function 'zip)(symbol-function 'pairlis))
```
上記２例目と３例目は少々異なる。
`MAPCAR`を使っている２例目は引数の２リストが異なる長さでも機能する。
しかし`PAIRLIS`を使っている３例目は引数の長さが異なっていればエラーを投げる。
また、`MAPCAR`を使っている２例目はリストの順序を保持するが、`PAIRLIS`を使っている３例目は順序が保持されるとは限らない。
（仕様上未定義。）
また、`PAIRLIS`はオプショナルな第三引数も受け取り、指定された場合、結果が連結される。

```lisp
cl-user> (pairlis '(1 2 3) '(a b c))
((3 . C)(2 . B)(1 . A))
cl-user> (pairlis '(4) '(d) *)
((4 . D)(3 . C)(2 . B)(1 . A))
```

### elm

```haskell
elm' :: (Eq a) => a -> [a] -> Bool
elm' a [] = False
elm' a (x:xs)
    | a == x = True
    | otherwise = a `elm` xs
```

```lisp
(declaim(ftype(function(elt list)boolean)elm))
(defun elm(elt list)
  (unless (null list)
    (destructuring-bind(x . xs)list
      (if(eql x elt)
        t
        (elm elt xs)))))
;; or
(setf(symbol-function 'elm)(symbol-function 'member))
```

## 4.3
### quicksort

```haskell
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
    let smallOrEqual = [a | a <- xs, a <= x]
        larger = [a | a <- xs, a > x]
    in quicksort smallerOrEqual ++ [x] ++ quicksort larger
```

```lisp
(declaim(ftype(function(list)list)quicksort))
(defun quicksort(list)
  (unless(null list)
    (bind:bind(((x . xs)list)
               ((:values small-or-equal larger)
                (loop :for elt :in xs
                      :if(<= elt x)
                      :collect elt :into SoE
                      :else :collect elt :into L
                      :finally(return(values SoE L)))))
      (nconc (quicksort small-or-equal)(list x)(quicksort larger)))))
;; or
(setf(symbol-function 'quicksort)(lambda(list)(sort list #'<)))
```

Common Lispの`SORT`は高階関数である。
渡す関数次第で降順、昇順どちらでもいける。

```lisp
cl-user> (sort '(1 3 2 4) #'<)
(1 2 3 4)
cl-user> (sort '(1 3 2 4) #'>)
(4 3 2 1)
```

また、`:KEY`キーワード引数にも対応している。

```lisp
cl-user> (defstruct person name age)
PERSON
cl-user> (defvar persons (list (make-person :name "a" :age 1)
                               (make-person :name "b" :age 2)
                               (make-person :name "c" :age 3)
                               (make-person :name "d" :age 4)))
PERSONS
cl-user> (sort persons #'> :key #'person-age)
(#S(PERSON :NAME "d" :AGE 4) #S(PERSON :NAME "c" :AGE 3)
 #S(PERSON :NAME "b" :AGE 2) #S(PERSON :NAME "a" :AGE 1))
```
