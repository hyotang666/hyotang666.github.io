# Common Lisp vs Haskell, Chapter 8
## Meta note
### 対象読者
[前章](cl-vs-haskell.7.html)を読了済みの者。

## Introduction
本稿は「すごいH本」の内容をCommon Lispに翻訳しながらCLerがHaskellを学ぶその第8章である。
本章ではHaskellに於けるIOアクションを、Common Lispに翻訳しながら学習していく。

初級CLerにとっては冒頭にあるroswell周りの内容が興味深いものとなろう。

中級以上の者にとっては特に得られる物がないと思われる。
サクッと読み飛ばして、どうぞ。

本稿で実装するモナド周りのコマンドは、その振る舞いのみをそれっぽく真似ただけのものでしかないことを予め言っておく。
よりそれっぽい実装は１３章で行う。

# 8
## 8.2
### Hello world

```haskell
# helloworld.hs
main = putStrLn "Hello, world"
# shell
$ ghc --make helloworld
$ ./helloworld
hello, world
```
実行形式ファイルを作る機能だが、Common Lispはそのための仕様を持たない。
これは、Common Lispが、かつてはLispマシンなども持っていた事と無関係ではないと思う（憶測）。
Common Lispに於けるREPLは、いわば、UNIXの世界に於けるシェルのようなものだ。
（だから筆者は個人的にCommon Lispが比較されるべきなのはPerlやrubyであるよりはbashやdashであるべきなのではないかと思っている。）
不幸なことにLispマシンは今や絶滅（？）し、LispはOSを抽象化したシェルそのものの立場から、シェルから呼び出されるプログラムという立場になってしまった。

UNIXの世界に充分慣れ親しんだ人がLispを学び始めて不便に思うことの一つが、このシェルとの連携に不向きという点だろう。
とはいえ、それが全く出来ないというわけではない。
各処理系は独自に実行形式ファイルを吐く機能を提供しているし、roswellが抽象化レイヤーを提供してくれている。

```lisp
# shell
$ ros init hello
;; hello.ros
(defun main (&rest argv)
  (declare (ignorable argv))
  (write-line "Hello, world"))
# shell
$ ./hello.ros
hello, world
```
上記の呼び出しは非常に遅い。
必要なら、以下のようにしてイメージをダンプしておくべきだろう。

```lisp
$ ros build hello.ros
# ./hello
hello, world
```

```haskell
ghci> :t putStrLn
putStrLn :: String -> IO ()
ghci> :t putStrLn "hello, world"
putStrLn "hello, world" :: IO ()
```
Haskellの`putStrLn`は、Common Lispに於ける`WRITE-LINE`におよそ等しい。
だが、大きな違いがあり、それは`putStrLn`は関数を返す関数である点だ。
再現したいなら自作する必要がある。

```lisp
cl-user> (type-of #'write-line)
FUNCTION
cl-user> (describe #'write-line)
;; #<FUNCTION WRITE-LINE>
;;   [compiled function]
;; Lambda-list: (STRING &OPTIONAL (STREAM *STANDARD-OUTPUT*) &KEY
;;                      (START 0) END)
;; Declared type: (FUNCTION
;;                 (STRING &OPTIONAL (OR STREAM BOOLEAN) &KEY
;;                         (:START (MOD 536870909))
;;                         (:END (OR NULL (MOD 536870909))))
;;                 (VALUES STRING &OPTIONAL))
;; Derived type: (FUNCTION
;;                (STRING &OPTIONAL (OR STREAM BOOLEAN) &KEY (:START T)
;;                        (:END T))
;;                (VALUES STRING &OPTIONAL))

(defun put-string-line(string)
  (lambda()(write-line string)(values)))
```

## 8.3
### IO actions

```haskell
main = do
    putStrLn "Hello, what's your name?"
    name <- getLine
    putStrLn ("Hey " ++ name ++ ", you rock!")
```
Haskellに於ける`do`は、Common Lispに於ける`PROGN`におよそ相当すると言えよう。
しかしながら前節の`putStrLn`と同様の違いがあり、すなわち、`do`は関数を返すが`PROGN`は処理を行う。
再現にこだわらないのであれば、上記Haskellコードは以下のように翻訳できる。
なお、`DEFUN`は暗黙裏に`PROGN`に展開される。

```lisp
(defun main (&rest argv)
  (declare (ignorable argv))
  (write-line "Hello, what's your name?")
  (let((name(read-line)))
    (format t "Hey ~A, you rock!~%" name)))
```

Haskellに於ける`getLine`は、Common Lispに於ける`READ-LINE`におよそ相当する。
前節同様、違いは関数を返すか、処理をするか、である。
Haskellに倣うなら、自作するしか無い。

```lisp
(defun get-line()
  (lambda()(read-line)))
```

再現にこだわるなら、`do`を作らねばならない。
`DO`だと名前が衝突してしまうので、ここでは`ACTION`とした。

```lisp
(defmacro action(&rest exp*)
  (labels((rec(exp* &optional acc)
            (if(endp exp*)
              (nreverse acc)
              (if(eq '<- (second exp*))
                `(,@(nreverse acc)
                   (LET((,(first exp*)(FUNCALL ,(third exp*))))
                     ,@(rec (nthcdr 3 exp*))))
                (rec (cdr exp*)(cons `(FUNCALL ,(first exp*))acc)))))
          )
    `(LAMBDA(),@(rec exp*))))
```
これで以下のように書ける。

```lisp
(setf (symbol-function 'main)
      (action (put-string-line "Hello, what's your name?")
              name <- (get-line)
              (put-string-line (uiop:strcat "Hey " name ", you rock!"))))
```
なんならそれ用のマクロを用意してもいい。

```lisp
(defmacro defaction(name &body body)
  (check-type name symbol)
  `(PROGN (SETF (SYMBOL-FUNCTION ',name)
                (ACTION ,@body))
          ',name))
```
これで以下のように書ける。

```lisp
(defaction main
   (put-string-line "Hello, what's your name?")
   name <- (get-line)
   (put-string-line (uiop:strcat "Hey " name ", you rock!")))
```
### Let in IO action

```haskell
import Data.Char

main = do
    putStrLn "What's your first name?"
    firstName <- getLine
    putStrLn "What's your last name?"
    lastNmae <- getLine
    let bigFirstName = map toUpper firstName
        bigLastName = map toUpper lastName
    putStrLn $ "hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", how are you?"
```
再現にこだわらないなら以下のように翻訳できる。

```lisp
(defun main (&rest argv)
  (declare (ignorable argv))
  (let*((first-name (progn (write-line "What's your first name?")
                           (read-line)))
        (last-name (progn (write-line "What's your last name?")
                          (read-line)))
        (big-first-name (string-upcase first-name))
        (big-last-name (string-upcase last-name)))
    (format t "hey ~A ~A, how are you?~%" big-first-name big-last-name)))
```
`LET`キーワードの実装は以下の通り。

```lisp
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
                    (LET((,(first exp*)(FUNCALL ,(third exp*))))
                      ,@(rec (nthcdr 3 exp*)))))
                (T (rec (cdr exp*)(cons `(FUNCALL ,(first exp*))acc))))))
          )
    `(LAMBDA(),@(rec exp*))))
```
これで以下のよう書ける。

```lisp
(defaction main
   (put-string-line "What's your first name?")
   first-name <- (get-line)
   (put-string-line "What's your last name?")
   last-name <- (get-line)
   let ((big-first-name(string-upcase first-name))
        (big-last-name (string-upcase last-name)))
   (put-string-line(uiop:strcat "hey " big-first-name " " big-last-name ", how are you?")))
```

### reverse

```haskell
main = do
    line <- getLine
    if null line
        then return ()
        else do
            putStrLn $ reverseWords line
            main

reverseWords :: String -> String
reverseWords = unwords . map reverse . words
```
再現しないなら以下のように翻訳できる。

```lisp
(defun main (&rest argv)
  (declare (ignorable argv))
  (let((line(read-line)))
    (unless(string= "" line)
      (write-line (reverse-words line))
      (main))))

(declaim(ftype(function(string)string)reverse-words))
(defun reverse-words(line)
  (format nil "~{~A~^ ~}"(mapcar #'reverse (uiop:split-string line))))
```
Haskellに於ける`return`は、Common Lispに於ける`CONSTANTLY`のようなものに見える。

```lisp
(defun .return(value)
  (lambda()value))
```
再現にこだわるなら、再帰を実現するために以下のような変更を要する。

```lisp
(defmacro defaction(name &body body)
  (check-type name symbol)
  `(PROGN (SETF (SYMBOL-FUNCTION (DEFUN ,name()))
                (ACTION ,@body))
          ',name))

(defun may-call(arg)
  (if(functionp arg)
    (funcall arg)
    arg))

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
    `(LAMBDA(),@(rec exp*))))
```

これで以下のように書ける。

```lisp
(defaction main
   line <- (get-line)
   (if(string= "" line)
     (.return nil)
     (action (put-string-line (reverse-words line))
             (main))))
```

## 8.4
### putStr

```haskell
main = do
    putStr "Hey, "
    putStr "I'm "
    putStrLn "Andy!"
```
Haskellの`putStr`はCommon Lispに於ける`WRITE-STRING`におよそ相当する。
Haskell風に再現するなら以下のようになる。

```lisp
(defun put-string (string)
  (lambda()(write-string string)(force-output)(values)))

(defaction main
  (put-string "Hey ")
  (put-string "I'm ")
  (put-string-line "Andy!"))
```

### putChar

```haskell
main = do
    putChar 't'
    putChar 'e'
    putChar 'h'
```
Haskellの`putChar`はCommon Lispに於ける`WRITE-CHAR`におよそ相当する。
Haskell風に再現するなら以下の通り。

```lisp
(defun put-char(char)
  (lambda()(write-char char)(values)))
```

### print

```haskell
main = do
    print True
    print 2
    print "haha"
    print 3.2
    print [3,4,3]
```
Haskellの`print`はCommon Lispに於ける`PRINT`とおよそ等価である。
Haskell風に再現するなら以下の通り。
名前が衝突するので、`.`が付いていある点要注意。

```lisp
(defun .print (arg)
  (lambda()(print arg)(values)))

(defaction main
  (.print t)
  (.print 2)
  (.print "haha")
  (.print 3.2)
  (.print '(3 4 3)))
```

### when

```haskell
import Control.Monad

main = do
    input <- getLine
    when (input == "SWORDFISH") $ do
        putStrLn input
```
Haskellの`when`はCommon Lispに於ける`WHEN`におよそ相当する。
大きな違いは、Haskellの`when`が関数であるのに対し、Common Lispの`WHEN`は通常マクロである点だ。

再現にこだわらないなら上記Haskellコードは以下のように翻訳できる。

```lisp
(defun main()
  (let((input(read-line)))
    (when(string= "SWORDFISH" input)
      (write-line input))))
```
再現にこだわるなら以下のように書くこととなる。
`MAY-CALL`を導入したおかげで、既存コードに変更はいらない。

```lisp
(defaction main
  input <- (get-line)
  (when (string= input "SWORDFISH")
    (put-string-line input)))
```

### sequence

```haskell
main = do
    rs <- sequence [getLine, getLine, getLine]
    print rs
```
再現しないなら以下の通り。

```lisp
(defun main()
  (let((rs(loop :repeat 3 :collect (read-line))))
    (print rs)))
```
再現にこだわるなら以下の通り。

```lisp
(defun .sequence(ios)
  (lambda()
    (mapcar #'funcall ios)))

(defaction main
  rs <- (.sequence (list (get-line)(get-line)(get-line)))
  (print rs))
```

```haskell
ghci> sequence $ map print [1,2,3,4,5]
1
2
3
4
5
[(),(),(),(),()]
```
HaskellのIOアクションはREPLに打ち込むと呼び出されるが、Common Lisp版にはそのような機能が無いので明示的に`FUNCALL`を呼ぶ必要がある。

```lisp
cl-user> (funcall (.sequence (mapcar #'.print '(1 2 3 4 5))))

1
2
3
4
5
(NIL NIL NIL NIL NIL)
```

### mapM

```haskell
ghci> mapM print [1,2,3]
1
2
3
[(),(),()]

ghci> mapM_ print [1,2,3]
1
2
3
```
再現にこだわらないなら、Haskellの`mapM`はCommon Lispの`MAPCAR`におよそ相当し、Haskellの`mapM_`はCommon Lispの`MAPC`におよそ相当する。

```lisp
cl-user> (mapcar #'print '(1 2 3))
1
2
3
(1 2 3)
cl-user> (mapc #'print '(1 2 3))
1
2
3
(1 2 3)
```
`MAPCAR`はリストの`CAR`部に働き、結果を格納する新しいリストを作って返す。
上記例では`(1 2 3)`が返っているが、これはCommon Lispの`PRINT`は引数を返すという仕様の性である。

`MAPC`はリストの`CAR`部に働き、第一リストを返す。
上記例で`(1 2 3)`が返っているのは`MAPC`の受け取っった引数が`(1 2 3)`だからである。

```lisp
cl-user> (mapcar (lambda(x y)(print (+ x y)))'(1 2 3)'(4 5 6))
5
7
9
(5 7 9)
cl-user> (mapc (lambda(x y)(print (+ x y)))'(1 2 3)'(4 5 6))
5
7
9
(1 2 3)
```
再現にこだわるなら以下のようにすることとなる。
なお、名前はCommon Lispの`MAPCAR`、`MAPC`、`MAPLIST`、`MAPL`にちなんだ。

```lisp
(defun mapmonad (io arg*)
  (.sequence (mapcar io arg*)))

(defun mapm (io arg*)
  (lambda()
    (map nil (lambda(arg)(funcall(funcall io arg)))arg*)))
```

### forever

```haskell
import Control.Monad
import Data.Char

main = forever $ do
    putStr "Give me some input: "
    l <- getLine
    putStrLn $ map toUpper l
```
再現にこだわらないなら以下の通り。

```lisp
(defun main()
  (loop :for l = (progn (write-string "Give me some input: ")
                        (force-output)
                        (read-line))
        :do (write-line (string-upcase l))))
```
再現にこだわるなら以下のようにする。

```lisp
(defun forever (io)
  (lambda()(loop (funcall io))))

(defaction main
  (forever (action (put-string "Give me some input: ")
                   l <- (get-line)
                   (put-string-line (string-upcase l)))))
```

### forM

```haskell
import Control.Monad

main = do
    colors <- forM [1,2,3,4] $ \a -> do
        putStrLn $ "Which color do you associate with the number "
                   ++ show a ++ "?"
        color <- getLine
        return color
    putStrLn "The colors that you associate with 1, 2, 3 and 4 are: "
    mapM putStrLn colors
```
再現にこだわらないなら以下の通り。

```Lisp
(defun main()
  (let((colors (loop :for a :in '(1 2 3 4)
                     :collect (progn (uiop:format! t "Which color do you associate with the number ~D?~%" a)
                                     (read-line)))))
    (write-line "The colors that you associate with 1, 2, 3 and 4 are: ")
    (mapc #'write-line colors)))
```
再現にこだわるなら以下の通り。

```lisp
(defun for-monad(args io)
  (lambda()
    (mapcar (lambda(x)(funcall(funcall io x))) args)))

(defaction main
  colors <- (for-monad '(1 2 3 4)
              (lambda(a)
                (action (put-string-line (uiop:strcat "Which color do you associate with the number " (princ-to-string a) "?"))
                        color <- (get-line)
                        (.return color))))
  (put-string-line "The colors that you associate with 1, 2, 3 and 4 are: ")
  (mapm #'put-string-line colors))
```
