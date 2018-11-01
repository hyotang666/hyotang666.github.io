# Common Lisp vs Haskell, Chapter 10
## Meta note
### 対象読者
[前章](cl-vs-haskell.9.html)を読了済みの者。

## Introduction
本稿は「すごいH本」の内容をCommon Lispに翻訳しながらCLerがHaskellを学ぶその第10章である。
本章ではこれまでの知識を駆使して、簡単な問題を解いていく。
その結果、HaskellコードをCommon Lispコードにへと粛々と書き写していくだけの内容が大半となっている。

初級CLerにとっても特に得られるもののない退屈な内容かと思われるので、読み飛ばして、どうぞ。
ボリュームも少ないので、ザッと目を通すだけならものの数分で済むかと思われる。

# 10
## 10.1
### RPN

```haskell
solveRPN :: String -> Double
```

```lisp
(declaim(ftype(function(string)rational)solve-rpn))
```

```haskell
solveRPN = head . foldl foldingFunction [] . words
    where foldingFunction (x:y:ys) "*" = (y * x):ys
          foldingFunction (x:y:ys) "+" = (y + x):ys
          foldingFunction (x:y:ys) "-" = (y - x):ys
          foldingFunction xs numberString = read numberString:xs
```

```lisp
(defun solve-rpn (string)
  (flet((folding-function(list op)
          (trivia:match*(list op)
            (((list* x y ys) "*")(cons (* y x)ys))
            (((list* x y ys) "+")(cons (+ y x)ys))
            (((list* x y ys) "-")(cons (- y x)ys))
            ((xs number-string)(cons (read-from-string number-string)xs)))))
    (car(reduce #'folding-function (uiop:split-string string)
                :initial-value ()))))
```
３章で導入したリーダマクロを使うなら以下の通り。

```lisp
(defun solve-rpn (string)
  #{(car(reduce #'folding-function (uiop:split-string string)
                :initial-value ()))
    :where
    ((:flet folding-function(list op))
     (trivia:match*(list op)
       (((list* x y ys) "*")(cons (* y x)ys))
       (((list* x y ys) "+")(cons (+ y x)ys))
       (((list* x y ys) "-")(cons (- y x)ys))
       ((xs number-string)(cons (read-from-string number-string)xs))))})
```


## 10.2

```haskell
data Section = Section {getA :: Int, getB :: Int, getC :: Int}
    deriving (Show)

type RoadSystem = [Section]
```

```lisp
(defdata section ()
  (section (get-a 0 :type fixnum)
           (get-b 0 :type fixnum)
           (get-c 0 :type fixnum)))
(deftype road-system ()
  '(trivial-types:proper-list section))
```

```haskell
heathrowToLondon :: RordSystem
heathrowToLondon = [ Section 50 10 30
                   , Section 5 90 20
                   , Section 40 2 25
                   , Section 10 8 0]
```
Haskellのレコード構文で定義された型はインスタンスを作る方法が２種類あるようだ。

```haskell
ghci> Section 50 10 30
ghci> Section {getA=50, getB=10, getC=30}
```
Haskellの`{...}`を使った構文は、いわば型のリテラル表記であると見なすことができ、`{...}`構文を使わずに値を渡している方は動的にコンストラクタで作っているとみなせよう。

```lisp
(declaim(type road-system *heathrow-to-london*))
(defvar *heathrow-to-london* '(#.(section 50 10 30)
                               #.(section 5 90 20)
                               #.(section 40 2 25)
                               #.(section 10 8 0)))
```

```haskell
data Label = A | B | C deriving (Show)
type Path = [(Label, Int)]
```

```lisp
(defdata label () :a :b :c)
(deftype path ()
  '(trivial-types:proper-list (cons label fixnum)))
```
Common Lispでは同一要素のリストを表す型を定義するのが不可能（？）である点、７章で述べた。
よって、ここではtrivial-typesの`PROPER-LIST`を導入しよう。
trivial-typesはこの問題をザックリ割りきっていて、型`PROPER-LIST`は単なる`LIST`へのシノニムに過ぎない。
上記のように複合型指定子として引数を取るが、それは無視される。
コンパイラにとっては何の役にも立たないが、ドキュメントとしては有用である。

```haskell
optimalPath :: ReadSystem -> Path
```

```lisp
(declaim(ftype(function(road-system)path)optimal-path))
```

```haskell
road-step :: (Path, Path) -> Section -> (Path, Path)
road-step (pathA, pathB) (Section a b c) =
  let timeA = sum (map snd pathA)
      timeB = sum (map snd pathB)
      forwardTimeToA = timeA + a
      crossTimeToA = timeB + b + c
      forwardTimeToB = timeB + b
      crossTimeToB = timeA + a + c
      newPathToA = if forwardTimeToA <= crossTimeToA
                       then (A, a):pathA
                       else (C, c):(B, b):pathB
      newPathToB = if forwardTimeToB <= crossTimeToB
                       then (B, b):pathB
                       else (C, c):(A, a):pathA
  in (newPathToA, newPathToB)
```

```lisp
(declaim(ftype(function((cons path path)section)(cons path path))road-step))
(trivia:defun-match* road-step(cons section)
  (((cons path-a path-b)(section a b c))
   (let*((time-a(reduce #'+ path-a :key #'cdr))
         (time-b(reduce #'+ path-b :key #'cdr))
         (forward-time-to-a (+ time-a a))
         (cross-time-to-a (+ time-b b c))
         (forward-time-to-b (+ time-b b))
         (cross-time-to-b (+ time-a a c))
         (new-path-to-a (if (<= forward-time-to-a cross-time-to-a)
                          (acons :a a path-a)
                          (acons :c c (acons :b b path-b))))
         (new-path-to-b (if (<= forward-time-to-b cross-time-to-b)
                          (acons :b b path-b)
                          (acons :c c (acons :a a path-a)))))
     (cons new-path-to-a new-path-to-b))))
```

```haskell
optimalPath :: RoadSystem -> Path
optimalPath roadSystem = 
    let (bestAPath, bestBPath) = foldl roadStep ([],[]) roadSystem
    in if sum (map snd bestAPath) <= sum (map snd bestBPath)
        then reverse bestAPath
        else reverse bestBPath
```

```lisp
(declaim(ftype(function(road-system)path)optimal-path))
(defun optimal-path (road-system)
  (destructuring-bind(best-a-path . best-b-path)(reduce #'road-step road-system
                                                        :initial-value '(() . ()))
    (if(<= (reduce #'+ best-a-path :key #'cdr)
           (reduce #'+ best-b-path :key #'cdr))
      (reverse best-a-path)
      (reverse best-b-path))))
```

```haskell
groupsOf :: Int -> [a] -> [[a]]
groupsOf 0 _ = undefined
groupsOf _ [] = []
groupsOf n xs = take n xs : groupsOf n (drop n xs)
```

```lisp
(declaim(ftype(function (fixnum (trivial-types:proper-list *))
                        (trivial-types:proper-list (trivial-types:proper-list *)))
              groups-of))
(trivia:defun-match* groups-of(n xs)
  ((0 _) :undefined)
  ((_ NIL)nil)
  ((n xs)(cons (incf-cl:take n xs)(groups-of n (nthcdr n xs)))))
;; or
(defun groups-of(n xs)
  (labels((rec(list &optional acc)
            (multiple-value-bind(group rest)(split list)
              (if(endp rest)
                (nreconc acc (list group))
                (rec rest (cons group acc)))))
          (split(list)
            (loop :repeat n
                  :for rest :on list
                  :collect (car rest) :into group
                  :finally (return (values group (cdr rest)))))
         )
    (if(zerop n)
      :undefined
      (unless(null xs)
        (rec xs)))))
```
上記Common Lispコードのうち、triviaの`DEFUN-MATCH*`を使った方は、ほぼHaskellの直訳になっている。
しかしながら、その結果、末尾再帰になっていないので、その点しっかりケアしたい場合、第二のコードのようになる。

```haskell
import Data.List

main = do
    contents <- getContents
    let threes = groupsOf 3 (map read $ lines contents)
        roadSystem = map (\[a,b,c] -> Section a b c)threes
        path = optimalPath roadSystem
        pathString = concat $ map (show . fst) path
        pathTime = sum $ map snd path
    putStrLn $ "The best path to take is: " ++ pathString
    putStrLn $ "Time taken: " ++ show pathTime
```

```lisp
(defun main()
  (let*((contents(get-contents))
        (threes(groups-of 3 (mapcar #'parse-integer (lines contents))))
        (road-system (mapcar (trivia:lambda-match((list a b c)`(section ,a ,b ,c)))
                             threes))
        (path (optimal-path road-system))
        (path-string (apply #'concatenate 'string (mapcar #`(+ 'princ-to-string 'car)
                                                          path)))
        (path-time (apply #'+ (mapcar #'cdr path))))
    (format t "The best path to take is: ~A~%" path-string)
    (format t "Time taken: ~A~%" path-time)))
```
