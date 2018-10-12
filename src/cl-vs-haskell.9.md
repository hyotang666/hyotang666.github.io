# Common Lisp vs Haskell, Chapter 9
## Meta note
### 対象読者
[前章](cl-vs-haskell.8.html)を読了済みの者。

## Introduction
本稿は「すごいH本」の内容をCommon Lispに翻訳しながらCLerがHaskellを学ぶその第9章である。
本章ではHaskellに於けるファイル操作、乱数生成、コマンドライン引数の取り扱いを、Common Lispに翻訳しながら学習していく。

初心者CLerにとっては幾つか導入される簡単なマクロの実装が参考になるかと思われる。
また、途中比較的大きな関数が出てくるのだが、そこは行番号を振って丁寧に解説してあるので、そこも参考になるかもしれない。

中級以上のCLerにとっては、特別興味深い点はなかろうと思われるので、読み飛ばして、どうぞ。

比較的長文なので、お暇なときにでも、たっぷりのコーヒーとチョコレートを用意してお読みくだされば幸い。

# 9
## 9.1
### input redirect

```haskell
import Control.Monad
import Data.Char

main = forever $ do
    l <- getLine
    putStrLn $ map toUpper l
```
再現にこだわらずに作るなら以下の通り。

```lisp
(defun main ()
  (loop (write-line(string-upcase(read-line)))))
```
こだわるなら以下の通り。

```lisp
(defaction main
  (forever(action l <- (get-line)
                  (put-string-line(string-upcase l)))))
```

### input stream

```haskell
import Data.Char

main = do
    contents <- getContents
    putStr $ map toUpper contents
```
こだわらないなら以下の通り。

```lisp
(defun main()
  (loop :for contents = (read-line nil nil)
        :while contents
        :do (write-line(string-upcase contents))))
```
さて、こだわった場合の実装だが、これはどう作ったら良いのかさっぱり分からない。
`getContetns`は標準入力から`EOF`まで入力を読み込むアクションだという。
それだけなら簡単だ。

```lisp
(defun get-contents()
  (lambda()
    (loop :for char = (read-char nil nil)
          :while char
          :collect char :into result
          :finally (return (coerce result 'string)))))
```
ところが、それは遅延IOだという。

> getContentsの結果がcontentsに束縛されとき、それは本当の文字列ではなく、最終的には文字列に評価されるプロミス（promise）としてメモリ上に置かれます。contentsにtoUpperをマップするとき、それもまた入力の結果に関数をマップするというプロミスになります。最終的にputStrが呼ばれると、これがさっきのプロミスに対して「やあ、大文字化された行が必要なんだ！」と言います。そのプロミスはまだ入力の行を何も持っていないので、contentsに対し「端末からの入力の状況はどうなってる？」と問い合わせます。それでようやくgetContentsは実際に端末から入力して、何か入力をくれといってきたコードに生成したものを渡すのです。受け取ったコードは渡されたものにtoUpperをマップし、その結果をputStrに渡して、画面に行が出力されます。さらに続けてputStrは「ヘイ、次の行をくれ！カモン！」と言います。これが入力がなくなるまで、つまりEOF文字が入力されるまで繰り返されます。

上記引用の中で、特に筆者が理解出来ないのが、行単位でデータのやり取りが行われる点である。
また暗黙裏の繰り返しが誰の責任で行われているのかという点も分からない。
遅延評価まわりやHaskellの実装についての論文をちゃんと読めば分かるようにもなるだろうが、そこまでやる気はないので、以降本節に関しては再現にこだわらず読み飛ばして行くものとする。

さて、 Haskellの`getContents`関数を実装する代わりに、ここでは`DO-CONTENTS`マクロを作ろう。

```lisp
(defmacro do-contents((var &optional(reader '#'read)(stream '*standard-input*))&body body)
  `(LOOP :FOR ,var = (FUNCALL ,reader ,stream NIL)
         :WHILE ,var
         :DO ,@(mapcar(lambda(form)
                        `(MAY-CALL ,form))
                      body)))
```
これで上記Haskellコードは以下のように翻訳できる。

```lisp
(defaction main
  (do-contents(contents #'read-line)
    (put-string(string-upcase contents))))
```

```haskell
main = do
    contents <- getContents
    putStr (shortLinesOnly contents)

shortLinesOnly :: String -> String
shortLinesOnly = unlines . filter (\line -> length line < 10) . lines
```
Haskellに於ける`lines`、`unlines`はCommon Lispには存在しない。
必要なら自作せねばならない。

```lisp
(defun lines(string)
  (uiop:split-string string :separator #.(string #\newline)))

(defun unlines(string*)
  (format nil "~{~A~^~%~}"string*))
```
これで以下のように書ける。

```lisp
(defun main ()
  (loop :for contents = (read-line nil nil)
        :while contents
        :do (write-string (short-lines-only contents))
        (force-output)))

(declaim(ftype(function(string)string)short-lines-only))
(defun short-lines-only (string)
  (unlines (remove-if-not (lambda(line)
                            (< (length line)10))
                          (lines string))))
```
先の`DO-CONTENTS`マクロを使うなら以下の通り。

```lisp
(defun main()
  (do-contents(contents #'read-line)
    (put-string (short-lines-only contents))))
```

### convert

```haskell
main = interact shortLinesOnly

shortLinesOnly :: String -> String
shortLinesOnly = unlines . filter (\line -> length line < 10) . lines
```
Haskellに於ける`interact`はCommon Lispに存在しない。
必要なら自作するしかない。

```lisp
(defun interact (&optional(function #'identity))
  (lambda()
    (loop :for content = (read-line nil nil)
          :while content
          :do (write-line(funcall function content)))))
```
これで以下のように書ける。

```lisp
(defaction main (interact #'short-lines-only))
```

```haskell
respondPalindromes :: String -> String
respondPalindromes = 
    unlines .
    map (\xs -> if isPal xs then "palindrome" else "not a palindrome") .
    lines

isPal :: String -> Bool
isPal xs = xs == reverse xs
```

```lisp
(declaim(ftype(function(string)string)respond-palindromes))
(defun respond-palindromes(string)
  (unlines (mapcar (lambda(xs)
                     (if(palindromep xs)
                       "palindrome"
                       "not a palindrome"))
                   (lines string))))

(declaim(ftype(function(string)boolean)palindromep))
(defun palindromep(string)
  (string= string (reverse string)))

(defaction main (interact #'respond-palindromes))
```

### file

```haskell
import System.IO

main = do
    handle <- openFile "baabaa.txt" ReadMode
    contents <- hGetContents handle
    putStr contents
    hClose handle
```
Haskellに於ける`openFile`、`hClose`は各々Common Lispに於ける`OPEN`、`CLOSE`におよそ等しい。
違いはこれまでと同様に、関数を返すか処理を行うかである。
必要なら自作するしか無い。

```lisp
(defun open-file(path direction)
  (lambda()
    (open path :direction direction)))

(defun h-close(handle)
  (lambda()(close handle)))
```
上記Haskellコードを直訳するなら以下の通り。

```lisp
(defaction main
  handle <- (open-file "baabaa.txt" :input)
  (let((*standard-input* handle))
    (do-contents(contents #'read-line)
      (put-string-line contents)))
  (h-close handle))
```
再現にこだわらないなら以下の通り。

```lisp
(defun main ()
  (with-open-file(*standard-input* "baabaa.txt")
    (loop :for line = (read-line nil nil)
          :while line
          :do (write-line line))))
```
Common Lispに於いて`OPEN`や`CLOSE`は上級者向けのコマンドである。
というのも、上記のように通常は`WITH-OPEN-FILE`マクロを使うものだからである。

ちなみに、折衷案として以下のようにも書ける。

```lisp
(defun main()
  (with-open-file(*standard-input* "baabaa.txt")
    (funcall(interact))))
```
`INTERACT`が返すIOアクションを`WITH-OPEN-FILE`のスコープ内で`FUNCALL`を用いて強引に呼び出している。
明示的な`FUNCALL`が不細工に見えるかもしれないが、上記のどのコードよりも短い。

```haskell
type FilePath = String
```
Haskellに於いて`FilePath`は`String`へのシノニムのようだが、Common Lispにはそれそのものを表す型`PATHNAME`がある。
型`PATHNAME`は型`STRING`とは明確に区別される。
しかしながらファイルパスを受け付ける関数の多くは文字列でも機能するように仕様で決まっている。
そこで、仕様書では`PATHNAME`と`STRING`とを統合した用語`PATHNAME-DESIGNATOR`というものがしばしば仮引数の名前などに使われる。
残念ながら`PATHNAME-DESIGNATOR`は仕様書で使われる用語でしかなく、そのような型は言語仕様には存在しない。
あれば便利なのでこれらを提供するライブラリがある。
名をTrivial-typesという。
よって、Haskellの`FilePath`はCommon Lispに於いては`TRIVIAL-TYPES:PATHNAME-DESIGNATOR`が最も望ましいと言える。

```hasekell
data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode
```
Haskellに於ける型`IOMode`はCommon Lispでは`OPEN`への引数、ひいては`WITH-OPEN-FILE`への引数として組み合わせて使うこととなる。

Haskellの`ReadMode`は以下のようにしてCommon Lispで再現できる。

```lisp
(open "path")
```
Haskellの`WriteMode`は以下のようにしてCommon Lispで再現できる。

```lisp
(open path" :direction :output :if-does-not-exist :create :if-exists :supersede)
```
Haskellの`AppendMode`は以下のようにしてCommon Lispで再現できる。

```lisp
(open "path" :direction :output :if-exists :append :if-does-not-exist :create)
```
Haskellの`ReadWriteMode`は以下のようにしてCommon Lispで再現できる。

```lisp
(open "path" :direction :io :if-does-not-exist :create)
```
Haskell風に再現するなら自作するしかない。

```lisp
(defdata io-mode()
  :read-mode :write-mode :append-mode :read-write-mode)
```
`IO-MODE`を作った以上は、上記`OPEN-FILE`も作りなおさねばなるまい。

```lisp
(defun open-file(path mode)
  (lambda()
    (apply #'open path (ecase mode
                         (:read-mode)
                         (:write-mode `(:direction :output :if-does-not-exist :create :if-exists :supersede))
                         (:append-mode `(:direction :output :if-exists :append :if-does-not-exist :create))
                         (:read-write-mode `(:direction :io :if-does-not-exist :create :if-exists :supersede))))))
```

### withFile

```haskell
main = do
    withFile "baabaa.txt" ReadMode $ \handle -> do
        contents <- hGetContents handle
        putStr contents
```
Haskellの`withFile`はCommon Lispの`WITH-OPEN-FILE`におよそ等しい。
違いは関数を返すが処理を行うかである。
再現にこだわるなら自作するしかない。

```lisp
(declaim(ftype(function(trivial-types:pathname-designator
                        io-mode
                        function)
                       function)
              with-file))
(defun with-file(file-path io-mode function)
  (coerce `(LAMBDA()
             (WITH-OPEN-FILE(*TERMINAL-IO* ,file-path
                             ,@(ecase io-mode
                                 (:read-mode)
                                 (:write-mode `(:DIRECTION :OUTPUT :IF-DOES-NOT-EXIST :CREATE :IF-EXISTS :SUPERSEDE))
                                 (:append-mode `(:DIRECTION :OUTPUT :IF-EXISTS :APPEND :IF-DOES-NOT-EXIST :CREATE))
                                 (:read-write-mode `(:DIRECTION :IO :IF-DOES-NOT-EXIST :CREATE :IF-EXISTS :SUPERSEDE))))
               (FUNCALL(FUNCALL ,function *TERMINAL-IO*))))
          'function))
```
これで以下のように書ける。

```lisp
(defaction main()
  (with-file "baabaa.txt" :read-mode
    (lambda(handle)
      (action (do-contents(contents #'read-line handle)
                (put-string contents))))))
```

### bracket

```haskell
withFile :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile name mode f = bracket (openFile name mode)
    (\handle -> hClose handle)
    (\handle -> f handle)
```
Haskellに於ける`bracket`はCommon Lispに於ける`UNWIND-PROTECT`におよそ等しい。
大きな違いは`bracket`が関数であるのに対し、`UNWIND-PROTECT`は特殊形式である点だろう。
`WITH-OPEN-FILE`はもちろん`UNWIND-PROTECT`上に実装されている。

```lisp
(macroexpand-1 '(with-open-file(s file-path)(read-line s)))
#+expanded
(LET ((S (OPEN FILE-PATH))
      (#:G1 T))
  (UNWIND-PROTECT (MULTIPLE-VALUE-PROG1 (PROGN (READ-LINE S))
                                        (SETQ #:G1 NIL))
    (WHEN S (CLOSE S :ABORT #:G1))))
```
関数版が欲しいなら実装するしかない。

```lisp
(declaim(ftype(function(function function function)function)bracket))
(defun bracket(prologue epilogue body)
  (lambda()
    (let(handle)
      (unwind-protect(progn (setf handle(funcall prologue))
                            (funcall(funcall body handle)))
      (funcall(funcall epilogue handle))))))
```
これで`WITH-FILE`の実装を以下のようにできる。

```
(defun with-file(name mode f)
  (bracket (open-file name mode)
           (lambda(handle)(h-close handle))
           (lambda(handle)(funcall f handle))))
```

### handle
Haskellで`handle`と呼ばれているものは、Common Lispでは`STREAM`と呼ばれている。

Haskellで、例えば`getContents`が標準入出力に振る舞うのに対し、`hGetContents`は受け取った`handle`に対し振る舞う。
翻ってCommon Lispでは、例えば`READ-LINE`はオプショナルに`STREAM`を引数に取る。
引数の規定値は`*STANDARD-INPUT*`であり、`*STANDARD-INPUT*`の規定値は標準入力である。
例えば`(read-line)`とすれば、標準入力から一行読み込むが、`(read-line stream)`とすれば渡された`stream`から一行読み込むこととなる。

また、Common Lispではスペシャル変数の動的束縛をよく用いる。

```lisp
(let((*standard-input* stream))
  (read-line))
```
上記コードではスペシャル変数`*STANDARD-INPUT*`の値を動的に`stream`の値に束縛し、そのコンテクスト下で`READ-LINE`が呼ばれる。
`READ-LINE`は引数が指定されていないので、規定値である`*STANDARD-INPUT*`から一行読み込むのだが、その`*STANDARD-INPUT*`は`stream`に束縛されているので、結果的に`stream`から一行読み込むこととなる。

このため、Common LispではHaskellの様に標準入出力に振る舞うものと`handle`を受け取るものと２つの関数を用意する必要がない。
それが良いことなのか悪いことなのかについてはここでは議論しない。

```haskell
import System.IO

main = do
    contents <- readFile "baabaa.txt"
    putStr contents
```
Haskellの`readFile`はCommon Lispには存在しない。
こだわらずに作るなら、自作する代わりにuiopの`READ-FILE-STRING`が利用できよう。

```lisp
(defun main ()
  (write-string (uiop:read-file-string "baabaa.txt")))
```

```haskell
import System.IO
import Data.Char

main = do
    contents <- readFile "baabaa.txt"
    writeFile "baabaacaps.txt" (map toUpper contents)
```
Haskellの`writeFile`に相当するものはCommon Lispには存在しない。
素のCommon Lispで書くなら上記Haskellコードは以下のようになるだろう。

```lisp
(defun main()
  (witn-open-file(*standard-input* "baabaa.txt")
    (with-open-file(*standard-output* "baabaacaps.txt"
                    :direction :output
                    :if-does-not-exist :create
                    :if-exists :supersede)
      (loop :for line = (read-line)
            :while line
            :do (write-line(string-upcase line))))))
```
Haskellのコードは非常にシンプルで美しいものとなっている。
繰り返しが暗黙裏に行われている点が愁眉であろう。
これを遅延評価の無いCommon Lispで再現するのは難しい。
よってマクロにして隠蔽してしまおう。

```lisp
(defmacro with-write-file(path &body body)
  `(WITH-OPEN-FILE(*STANDARD-OUTPUT* ,path :DIRECTION :OUTPUT
                   :IF-EXISTS :SUPERSEDE :IF-DOES-NOT-EXIST :CREATE)
     ,@body))
(defmacro with-append-file(path &body body)
  `(WITH-OPEN-FILE(*STANDARD-OUTPUT* ,path :DIRECTION :OUTPUT
                   :IF-EXISTS :APPEND :IF-DOES-NOT-EXIST :CREATE)
     ,@body))
```
これで、これまで作ってきたものと組み合わせて以下のように書ける。

```lisp
(defun main ()
  (with-write-file "baabaacaps.txt"
    (write-string(string-upcase(uiop:read-file-string "baabaa.txt")))))
```

### todo list

```haskell
import System.IO
main = do
    todoItem <- getLine
    appendFile "todo.txt" (todoItem ++ "\n")
```
素のCommon Lispで書くなら以下の通り。

```lisp
(defun main()
  (with-open-file(*standard-output* "todo.txt"
                  :direction :output
                  :if-does-not-exist :create
                  :if-exists :append)
    (loop :for line = (read-line nil nil)
          :while line
          (write-line line))))
```
これまで作ってきたものを駆使するなら以下の通り。

```lisp
(defun main()
  (with-append-file "todo.txt"
    (funcall(interact))))
```

### delete

```haskell
import System.IO
import System.Directory
import Data.List

main = do
    contents <- readFile "todo.txt"
    let todoTasks = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line)
                                    [0..] todoTasks
    putStrLn "There are your TO-DO items:"
    mapM_ putStrLn numberedTasks
    putStrLn "Which one do you want to delete?"
    numberString <- getLine
    let number = read numberString
        newTodoItems = unlines $ delete (todoTasks !! number) todoTasks
    (tempName, tempHandle) <- openTempFile "." "temp"
    hPutStr tempHandle newTodoItems
    hClose tempHandle
    removeFile "todo.txt"
    renameFile tempName "todo.txt"
```

```lisp
0(defun main()
1  (let*((lines(delete ""(uiop:read-file-lines "baabaa.txt"):test #'string=))
2        (alist(loop :for i :upfrom 0
3                    :for line :in lines
4                    :collect (list i line)))
5        (number(prompt-for:prompt-for `(mod ,(length lines))
6                           "There are your TO-DO items:~%~:{~3D - ~A~%~}~%~
7                           Which one do you want to delete?~%>> " alist))
8        (temp-name(loop :for name = (symbol-name(gensym))
9                        :unless (probe-file name)
10                        :return name)))
11    (with-write-file temp-name
12      (loop :for (i line) :in alist
13            :unless (= number i)
14            :do (write-line line)))
15    (delete-file "todo.txt")
16    (rename-file temp-name "todo.txt")))
```
少々規模が大きいので解説する。
まず、`UIOP:READ-FILE-LINES`で指定されたファイルの行のリストを得る。（１）
念の為、空行を消しておく。（１、`DELETE`）
`REMOVE`を使わず`DELETE`を使っているのは`UIOP:READ-FILE-LINES`がフレッシュなリストを返すからだ。
破壊的に編集しても他に影響を及ぼさない。
変数`ALIST`に束縛されるリストの要素は、後のことを考えて`CONS`ではなく`LIST`で括られている。（４）
`PROMPT-FOR`はCLtL2に出てきた架空の関数を実際に作ってみた拙作ライブラリである。（５）
第一引数にリストを受け取った場合、それはCompound-type-specifierと解釈され、ユーザの入力がそれを満たさない場合、クレームを出力し再入力を促す。
ここでは``(mod ,(length lines))`が渡されている。（５）
仮に行数が３だった場合、ユーザの入力が０、１、２のいずれかでない限り再入力を促すこととなる。
`PROMPT-FOR`の第二引数はFormat-controlである。
ここでは（４）で`LIST`を使って要素をくくったおかげで`~:{ ... ~}`というFormat-directiveを使うことが出来る。
Haskellの`openTempFile`に相当する機能はCommon Lispには無いので、`GENSYM`を利用する。（８）
`GENSYM`により生成されたシンボルの名前を`SYMBOL-NAME`で取り出し（８）、`PROBE-FILE`で同名のファイルが存在するか確認し（９）、存在しないようならその名前を返す（１０）。
後は`WITH-WRITE-FILE`に`TEMP-NAME`を渡し（１１）、`ALIST`を`LOOP`し（１２）、ユーザが指定した行番号と異なるなら（１３）出力する（１４）。
後はHaskellと変わらないので説明はいらないだろう。

ところで、tempファイルに書き出してから安全にリネームすることはよくあることだろう。
マクロで隠蔽してしまおう。

```lisp
(defmacro with-safe-write-file(file &body body)
  (let((temp(gensym "TEMP")))
    `(LET((,temp(LOOP :FOR NAME = (SYMBOL-NAME(GENSYM))
                      :UNLESS(PROBE-FILE NAME)
                      :RETURN NAME)))
       (HANDLER-BIND((ERROR(LAMBDA(C)(DECLARE(IGNORE C))
                             (WHEN(PROBE-FILE ,tmep)
                               (DELETE-FILE ,temp)))))
         (WITH-WRITE-FILE ,temp ,@body)
         (WHEN(PROBE-FILE ,file)
           (DELETE-FILE ,file))
         (RENAME-FILE ,temp ,file)))))
```

### cleanup
Haskellの`bracketOnError`はCommon Lispに於ける`HANDLER-BIND`に近しい。

実装するなら以下の通り。

```lisp
(defun bracket-on-error(arg handler body)
  (handler-bind((error(lambda(c)
                        (declare(ignore c))
                        (funcall handler arg))))
    (funcall body arg)))
```

## 9.4
### command line arguments

```haskell
import System.Environment
import Data.List

main = do
    args <- getArgs
    progName <- getProgName
    putStrLn "The arguments are:"
    mapM putStrLn args
    putStrLn "The program name is:"
    putStrLn progName
```
Haskellの`getArgs`に相当する機能はCommon Lispにはない。
しかしながらRoswellスクリプトとして書いているなら、Roswellが面倒を見てくれる。
Rosスクリプトのmain関数は`&REST`でコマンドライン引数を受け取るようになっている。

Haskellの`getProgName`に相当する機能はCommon Lispにはない。
対応するライブラリ等についても筆者は知らない。

```lisp
(defun main (&rest argv)
  (format t "The arguments are:~%~{~A~%~}"argv))
```

## 9.5
### Multi task task list

```haskell
import System.Environment
import System.Directory
import System.IO
import Data.List

dispatch :: String -> [String] -> IO()
dispatch "add" = add
dispatch "view" = view
dispatch "remove" = remove

main = do
    (command:argList) <- getArgs
    dispatch command argList
```

```lisp
(defun dispatch(command)
  (trivia:match command
    ("add" #'add)
    ("view" #'view)
    ("remove" #'.remove)))

(defun main (&rest argv)
  (apply (dispatch (car argv))(cdr argv)))
```

```haskell
add :: [String] -> IO ()
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")
```

```lisp
(declaim(ftype(function(string string)t)add))
(defun add (file-name todo-item)
  (with-append-file file-name
    (write-line todo-item)))
```

```haskell
view :: [String] -> IO ()
view [fileName] = do
    contents <- readFile fileName
    let todoTasks = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line)
                        [0..] todoTasks
    putStr $ unlines numberedTasks
```
素のCommon Lispで書くなら以下の通り。

```lisp
(declaim(ftype(function(string)null)view))
(defun view(file)
  (with-open-file(*standard-input* file)
    (loop :for i :upfrom 0
          :for line = (read-line nil nil)
          :while line
          :do (format t "~&~3D - ~A"i line))))
```
再現にこだわるなら、`DEFACTION`マクロに手を入れて以下のようにしよう。
なお、後方互換性がなくなるので要注意。

```lisp
(defmacro defaction(name lambda-list &body body)
  (check-type name symbol)
  `(PROGN (SETF (SYMBOL-FUNCTION ',name)
                (LAMBDA ,lambda-list
                  (FUNCALL(ACTION ,@body))))
          ',name))
```
これで以下のように書ける。

```lisp
(defaction view(file)
  let ((todo-tasks(uiop:read-file-lines file))
       (numbered-tasks (loop :for i :upfrom 0
                             :for line :in todo-tasks
                             :collect (format nil "~3D - ~A"i line))))
  (put-string(unlines numbered-tasks)))
```
なお、SERIESを使って以下のように書いてもいい。

```lisp
(defun view(file)
  (series:collect-ignore
    (series:map-fn 'null (lambda(i l)(format t "~&~D - ~A"i l))
      (series:scan-range :from 0)
      (series:scan-file file #'read-line))))
```

```haskell
remove :: [String] -> IO ()
remove [fileName, numberString] = do
    contents <- readFile fileName
    let todoTasks = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line)
                        [0..] todoTasks
    putStrLn "There are your TO-DO items:"
    mapM_ putStrLn numberedTasks
    let number = read numberString
        newTodoItems = unlines $ delete (todoTasks !! number) todoTasks
    bracketOnError (openTempFile "." "temp")
        (\(tempName, tempHandle) -> do
            hClose tempHandle
            removeFile tempName)
        (\(tempName, tempHandle) -> do
            hPutStr tempHandle newTodoItems
            hClose tempHandle
            removeFile "todo.txt"
            renameFile tempName "todo.txt")
```

```lisp
(declaim(ftype(function(string string)t).remove))
(defun .remove (file-name number-string)
  (let*((todo-tasks(uiop:read-file-lines file-name))
        (numbered-tasks (loop :for line :in todo-tasks
                              :for i :upfrom 0
                              :collect (list i line)))
        (number(parse-integer number-string))
        (new-todo-items(loop :for task :in todo-tasks
                             :for i :upfrom 0
                             :unless (= i number)
                             :collect task)))
    (format t "There are your TO-DO items:~%~{~D - ~A~%~}" numbered-tasks)
    (with-safe-write-file "todo.txt"
      (mapc #'write-line new-todo-items))))
```

## 9.6
### random

```haskell
ghci> random (mkStdGen 100) :: (Int, StdGen)
(-1352021624,651872571 1655838864)
ghci> random (mdStdGen 100) :: (Int, StdGen)
(-1352021624,651872571 1655838864)
ghci> random (mkStdGen 949494) :: (Int, StdGen)
(539963926, 466647808 1655838864)
ghci> random (mkStdGen 949488) :: (Float, StdGen)
(0.8938442, 1597344447 1655838864)
ghci> random (mkStdGen 949488) :: (Bool, StdGen)
(False, 1485632275 40692)
ghci> random (mkStdGen 949488) :: (Integer, StdGen)
(1691547873, 1597344447 1655838864)
```
Common Lispの`RANDOM`関数は０から第一引数に指定された数までのランダムな値を返す。

```lisp
cl-user> (random 1000)
504
```
Common Lispの`RANDOM`関数は第二引数にランダムシードを受け付ける。
その規定値は`*random-state*`で、本引数は破壊的に変更される。

ランダムシードを作るには`MAKE-RANDOM-STATE`を使う。
`MAKE-RANDOM-STATE`はオプショナルに引数を取り、それが`T`ならフレッシュな`RANDOM-STATE`オブジェクトを返す。
それが`NIL`なら`*random-state*`のコピーを返す。
それが`RANDOM-STATE`ならそのコピーを返す。

```lisp
cl-user> (random 100(make-random-state))
44
cl-user> (random 100(make-random-state))
44
cl-user> (random 100(make-random-state t))
29
```
なお、`RANDOM`の引数は整数かfloatが受け付け可能である。

```lisp
cl-user> (random 100.0(make-random-state))
81.15838
cl-user> (random 100.0d0(make-random-state))
67.00164098613853d0
```

```haskell
threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen =
    let (firstCoin, newGen) = random gen
        (secondCoin, newGen') = (random newGen)
        (thirdCoin, newGen'') = (random newGen')
    in (firstCoin, secondCoin, thirdCoin)
```

```lisp
(declaim(ftype(function(random-state)list)three-coins))
(defun three-coins(state)
  (let((*random-state* (make-random-state state)))
    `(,(oddp(random 100))
      ,(oddp(random 100))
      ,(oddp(random 100)))))
```

### More randoms

```haskell
randoms' :: (RandomGen g, Random a) => g -> [a]
randoms' gen = let (value, newGen) = random gen in value:randoms' newGen
```
Haskellの`randoms`に相当する機能をCommon Lispで作るのは難しい。
というのもCommon Lispは遅延評価をする言語ではないからだ。
SERIESを使えば、無限に乱数を生成するSERIESオブジェクトを返す関数を作ることも可能だろう。

```lisp
(defun randoms (&optional(state *random-state*))
  (series:scan-fn 'fixnum
    (lambda()(random most-positive-fixnum state))
    (lambda(x)
      (declare(ignore x))
      (random most-positive-fixnum state))))
```
どこかで区切ってとり出さねばならないなら、それごと統合して一関数にしてしまうのが簡単だ。

```lisp
(defun randoms (num &optional (*random-state* *random-state*))
  (loop :repeat num :collect (random most-positive-fixnum)))
```

```haskell
ghci> randomR (1,6) (mkStdGen 359353)
(6, 1494289578 40692)
ghci> randomR (1,6) (mkStdGen 35935335)
(3, 1250031057 40692)
```
Haskellに於ける`randomR`はCommon Lispには存在しない。
必要なら自作するしか無い。

```lisp
(defun random-range(min max &optional(*random-state* *random-state*))
  (+ min (random (1+(- max min)))))
```

```haskell
ghci> take 10 $ randomRs ('a','z') (mkStdGen 3) :: [Char]
"ndkxbvmomg"
```
Haskellの`randomRs`もまたCommon Lispには無い。
作るなら、取り出しと統合して以下のようにするのがよろしかろう。

```lisp
(defun random-ranges(length min max
                           &optional(*random-state* *random-state*))
  (loop :repeat length :collect (random-range min max)))
```
文字列にしたいなら以下のようにするしかない。

```lisp
(map 'string #'code-char(random-ranges 10 (char-code #\a)(char-code #\z)))
```

## 9.7
### bytestring

```haskell
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as S

ghci> B.pack [99,97,110]
Chunk "can" Empty
ghci> B.pack [98..120]
Chunk "bcdefghijklmnopqrstuvwx" Empty

ghci> let by = B.pack [98,111,114,116]
ghci> by
Chunk "bort" Empty
ghci> B.unpack by
[98,111,114,116]
```
Common Lispは遅延評価をする言語ではないので、Haskellの`bytestring`に相当するものがそもそもない。
なお、バイトベクタと文字列との変換にはbabelを使う。

```lisp
cl-user> (defvar by (make-array 4 :element-type '(unsigned-byte 8)
                                  :initial-contents '(98 111 114 116)))
BY
cl-user> (babel:octets-to-string *)
"bort"
cl-user> by
#(98 111 114 116)
cl-user> (babel:string-to-octets "hoge")
#(104 111 103 101)
```
