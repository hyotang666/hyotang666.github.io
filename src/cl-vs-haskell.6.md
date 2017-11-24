# Common Lisp vs Haskell, Chapter 6
## Meta note
### 対象読者
[前章](cl-vs-haskell.5.html)を読了済みの者。

## Introduction
本稿は「すごいH本」の内容をCommon Lispに翻訳しながらCLerがHaskellを学ぶその第6章である。
本章ではHaskellのmoduleを、それに相当するCommon Lispのシステムに翻訳していきながら学習する。

冒頭でCommon Lispのシステムについての簡単な解説があるので、初心者CLerにとっては一助となるかもしれない。

また、中間部ではHaskellの代表的な標準モジュールをCommon Lispに翻訳しているので、そのあたりも初心者CLerの一助となろう。

中級CLerにとっては、冒頭で作成するローカルなパッケージニックネームの導入が興味深い内容となっているのではないかと思われる。

# 6

```haskell
import Data.List
```
Common Lispに於いて、Haskellのモジュールに相当するものは、言語仕様には存在しない。
Common Lispが提供するのは`PACKAGE`という名前空間管理機能のみである。
Common Lispに於いて構造化されたソースファイル群を読み込む機能は、デファクトスタンダードとして、asdfが担う。
よって、asdfの`SYSTEM`をHaskellのモジュールと見做せるかと思う。

上記Haskellの`import`におよそ相当する、ソースコードからモジュールをロードする方法として、Common Lispではシステム定義ファイル（\*.asdファイル）を書くこととなる。

```lisp
(in-package :asdf)
(defsystem "my-product"
  :depends-on ("data.list")
  :components ((:file ...)))
```
上記例では"my-product"という名前のシステムを定義し、それは"data.list"というシステムに依存している事を示している。
すなわち、"my-product"をロードするためには手始めに"data.list"をロードしなければならないということを示している。

```haskell
ghci> :m + Data.List
ghci> :m + Data.List Data.Map Data.Set
```
上記で定義したシステムをREPLから呼びだすにはasdfが提供している`LOAD-SYSTEM`を叩くことになる。

```lisp
cl-user> (asdf:load-system :my-product)
```
複数のシステムをまとめてロードしたい場合は`LOAD-SYSTEMS`を使う。

```lisp
cl-suer> (asdf:load-systems :data.list :data.map :data.set)
```

システムがロードされると、そのシステムが定義している`PACKAGE`（名前空間）が、現在駆動中のLispイメージ内に構築される。

```lisp
cl-user> (find-package :alexandria)
NIL
cl-user> (asdf:load-system :alexandria)
T
cl-user> (find-package :alexandria)
#<PACKAGE "ALEXANDRIA.0.DEV">
```

```haskell
import qualified Data.Map
import qualified Data.Map as M
```
Common LispはデフォルトでHaskellの`import qualified`に相当している。
すなわち、パッケージ名をプリフィックスにつけないとシンボルにアクセスできない。

```lisp
cl-user> (iota 3)
;; ERROR
cl-user> (alexandria:iota 3)
(0 1 2)
```
プリフィックスを付けずに`PACKAGE`が提供しているシンボルにアクセス出来るようにするためには、`USE-PACKAGE`や`IMPORT`を使う。


```haskell
import Data.List (num, sort)
```

```lisp
cl-user> (import '(something:num something:sort))
```
ただし、上記コマンド群は現在の`PACKAGE`を破壊的に変更してしまうので、通常自身のアプリケーションのために自前の`PACKAGE`を定義して、それを使うこととなる。

```lisp
(defpackage :my-product(:use :cl :data.list)
  (:import-from :something #:num #:sort)
  ...)
(in-package :my-product)
```

```haskell
import Data.List hiding (nub)
```

Haskellの`hiding`キーワードに相当する機能は、Common Lispに於いては`SHADOW`と呼ばれる。

```lisp
(defpackage hoge(:use Data.List)
  (:shadow :nub))
```

さて、Common LispにはHaskellの`qualified import`の`as`に相当する機能がない。
一応`PACKAGE`はニックネームを持てるのだが、それはグローバルになってしまう。
パッケージローカルにパッケージのニックネームを付ける機能は仕様には存在しない。
できたら便利なケースもあろうかと思われるので作ってみよう。

```lisp
(defmacro qualified-use(package &key as)
  (assert as()"Missing :AS keyword parameter.")
  `(eval-when(:compile-toplevel :load-toplevel :execute)
     ,@(loop :for symbol :being :each :external-symbol :of package
             :for new = (intern(format nil "~A.~A"as symbol))
             :collect `(define-symbol-macro ,new ,symbol)
             :when (macro-function symbol)
             :do (setf (macro-function new)(macro-function symbol))
             :when (and (not(macro-function symbol))
                        (fboundp symbol))
             :do (setf (symbol-function new)(symbol-function symbol))
             :when (ignore-errors(fdefinition`(setf ,symbol)))
             :do (setf (fdefinition`(setf ,new))(fdefinition`(setf ,symbol))))))
```
実装は以上。
少々卑怯なやり方で実現している。

実際のところ、上記`QUALIFIED-USE`はパッケージのニックネームを作らない。
単に新しいシンボルをインターンする。

```lisp
cl-user> (qualified-use :alexandria :as :alex)
cl-user> (alex.iota 3)
(0 1 2)
```

メリットは

* 実装が単純
* 実装が簡単
* ポータブル

デメリットは

* シンボルが（ことと次第では）大量にインターンされる。
* ローカルニックネームを経由して内部シンボルにアクセスすることはできない。

といったあたりか。

既存の言語上に構築するものとしては、現実的な妥協案であろうかと思う。

## 6.2

```haskell
ghci> words "hey these are the words in this sentence"
["hey","these","are","the","words","in","this","sentence"]
ghci> words "hey these         are      the words in this sentence"
["hey","these","are","the","words","in","this","sentence"]
```
haskellの`words`に相当する機能はsplit-sequenceの`SPLIT-SEQUENCE`がある。
`SPLIT-SEQUENCE`は名前の通り、列（SEQUENCE）に対応するものだが、対象が文字列に限定されるならuiopの`SPLIT-STRING`も使える。
ただし、空文字列を結果から除去したいなら`SPLIT-SEQUENCE`の方が望ましい。

```lisp
cl-user> (split-sequence:split-sequence #\space "hey these are the words in this sentence")
("hey" "these" "are" "the" "words" "in" "this" "sentence")
cl-user> (uiop:split-string "hey these are the words in this sentence")
("hey" "these" "are" "the" "words" "in" "this" "sentence")
cl-user> (uiop:split-string "hey these    are    the words in this sentence")
("hey" "these" "" "" "" "are" "" "" "" "the" "words" "in" "this" "sentence")
cl-user> (split-sequence:split-sequence #\space "hey these    are    the words in this sentence")
("hey" "these" "" "" "" "are" "" "" "" "the" "words" "in" "this" "sentence")
cl-user> (split-sequence:split-sequence #\space "hey these    are    the words in this sentence" :remove-empty-subseqs t)
("hey" "these" "are" "the" "words" "in" "this" "sentence")
```

```haskell
ghci> group [1,1,1,1,2,2,2,2,3,3,2,2,2,5,6,7]
[[1,1,1,1],[2,2,2,2],[3,3],[2,2,2],[5],[6],[7]]
ghci> group ["boom","bip","bip","boom","boom"]
[["boom"],["bip","bip"],["boom","boom"]]
```
Haskellの`group`に相当するものはicnf-clが提供している。

```lisp
cl-user> (incf-cl:group '(1 1 1 1 2 2 2 2 3 3 2 2 2 5 6 7))
((1 1 1 1)(2 2 2 2)(3 3)(2 2 2)(5)(6)(7))
cl-user> (incf-cl:group '("boom" "bip" "bip" "boom" "boom"))
(("boom")("bip")("bip")("boom")("boom"))
cl-user> (incf-cl:group '("boom" "bip" "bip" "boom" "boom"):test #'string=)
(("boom")("bip" "bip")("boom" "boom"))
```

```haskell
ghci> sort [5,4,3,7,2,1]
[1,2,3,4,5,7]
ghci> sort ["boom","bip","bip","boom","boom"]
["bip","bip","boom","boom","boom"]
```

```lisp
cl-user> (sort '(5 4 3 7 2 1) #'<)
(1 2 3 4 5 7)
cl-user> (sort '("boom" "bip" "bip" "boom" "boom") #'string<)
```

```haskell
wordDums :: String -> [(String,Int)]
wordNums = map (\ws -> (head ws, length ws)) . group . sort . words
ghci> wordNums "wa wa wee wa"
[("wa",3),("wee",1)]
```

```lisp
(declaim(ftype(function(string)list)word-nums))
(defun word-nums(string)
  (mapcar (lambda(xs)
            (cons (car xs)
                  (length xs)))
          (incf-cl:group (sort (split-sequence:split-sequence #\space string
                                                              :remove-empty-subseqs t)
                               #'string<)
                         :test #'string=)))
cl-user> (word-nums "wa wa wee wa")
(("wa" . 3)("wee" . 1))
```

```haskell
ghci> tails "party"
["party","arty","rty","ty","y",""]
ghci> tails [1,2,3]
[[1,2,3],[2,3],[3],[]]
```
Haskellの`tails`はCommon Lispの`MAPLIST`がおよそ相当する。

```lisp
cl-user> (maplist (lambda(list)
                    (coerce list 'string))
                  (coerce "party" 'list))
("party" "arty" "rty" "ty" "y")
cl-user> (maplist #'identity '(1 2 3))
((1 2 3)(2 3)(3))
```

```haskell
ghci> "hawaii" `isPrefixOf` "hawaii joe"
True
ghci> "haha" `isPrefixOf` "ha"
False
ghci> "ha" `isPrefixOf` "ha"
True
```
Haskellの`isPrefixOf`はCommon Lispではuiopの`STRING-PREFIX-P`が相当する。
uiopは同様に`STRING-SUFFIX-P`も提供している。

```lisp
cl-user> (uiop:string-prefix-p "hawaii" "hawaii joe")
T
cl-user> (uiop:string-prefix-p "haha" "ha")
NIL
cl-user> (uiop:string-prefix-p "ha" "ha")
T
```

```haskell
ghci> any (> 4) [1,2,3]
False
ghci> any (=='F') "Frank Sobotka"
True
ghci> any (\x -> x > 5 && x < 10)[1,4,11]
False
```
Haskellの`any`はCommon Lispでは`SOME`に相当する。
Common Lispは他に同様のものとして`EVERY`、`NOTEVERY`、`NOTANY`を提供している。

```lisp
cl-user> (some #`(% '> _ 4) '(1 2 3))
NIL
cl-user> (some #`(% 'char= #\F) "Frank Sobotka")
T
cl-user> (some #`(% '< 5 _ 10) '(1 4 11))
NIL
```
Common Lispは前置構文をサポートしているので、`<`や`>`も2引数関数ではない点要注意。

```lisp
cl-user> (<)
;; ERROR
cl-user> (< 1)
T
cl-user> (< 1 2)
T
cl-user> (< 1 2 3)
T
```

```haskell
ghci> "art" `isInfixOf` "party"
True
ghci> [1,2] `isInfixOf` [1,3,5]
False
```
Haskellの`isInfixOf`はCommon Lispでは`SEARCH`がそれに相当する。

```lisp
cl-user> (search "art" "party")
1
cl-user> (search '(1 2) '(1 3 5))
NIL
```

### caesar

```haskell
ghci> ord 'a'
97
```
Haskellの`ord`はこの場合Common Lispの`CHAR-CODE`に相当する。

```lisp
cl-user> (char-code #\a)
97
```

```haskell
ghci> chr 97
'a'
```
Haskellの`chr`は、Common Lispの`CODE-CHAR`に相当する。

```lisp
cl-user> (code-char 97)
#\a
```

```haskell
ghci> map ord "abcdefgh"
[97,98,99,100,101,102,103,104]
```
上記HaskellコードはCommon Lispでは`MAP`を使うことで再現できる。
第一引数に返り値の型を明示しなくてはならない点要注意。

```lisp
cl-user> (map 'list #'char-code "abcdefgh")
(97 98 99 100 101 102 103 104)
```

```haskell
encode :: Int -> String -> String
encode offset msg = map (\c -> chr $ ord c + offset) msg

ghci> encode 3 "hey mark"
"kh|#pdun"
ghci> encode 5 "please instruct your men"
"uqjfxj%nsxywzhy%~tzw%rjs"
ghci> encode 1 "to party hard"
"up!qbsuz!ibse"
```

```lisp
(declaim(ftype(function(fixnum string)string)encode))
(defun encode (offset msg)
  (map 'string (lambda(c)
                 (code-char (+ offset (char-code c))))
               msg))
cl-user> (encode 3 "hey mark")
"kh|#pdun"
cl-user> (encode 5 "please instruct your men")
"uqjfxj%nsxywzhy%~tzw%rjs"
cl-user> (encode 1 "to party hard")
"up!qbsuz!ibse"
```

```haskell
decode :: Int -> String -> String
decode shift msg = encode (negate shift) msg

ghci> decode 3 "kh|#pdun"
"hey mark"
```

```lisp
(declaim(ftype(function(fixnum string)string)decode))
(defun decode (shift msg)
  (encode (- shift)msg))
cl-user> (decode 3 "kh|#pdun")
"hey mark"
```

### first to 40

```haskell
ghci> digitToInt '2'
2
ghci> digitToInt 'F'
15
ghci> digitToInt 'z'
;; ERROR
```
Haskellの`digitToInt`はCommon Lispでは`DIGIT-CHAR-P`が相当するが、いささか勝手が異なる。
Common Lispの`DIGIT-CHAR-P`は第一引数に文字を受け取り、それが数と解釈できるなら解釈された数を返す。
数と解釈できない場合は`NIL`を返す。
数と解釈できるか否かの判定はオプショナルな第二引数による。
規定値は`*READ-BASE*`の値が使われる。
Common Lispでは２進数から３６進数まで対応可能である。

```lisp
cl-user> (digit-char-p #\2)
2
cl-user> (digit-char-p #\F)
nil
cl-user> (digit-char-p #\F 16)
15
cl-user> (digit-char-p #\9 3)
NIL
cl-user> (digit-char-p #\z 36)
35
```

```haskell
digitSum :: Int -> Int
digitSum = sum . map digitToInt . show
```

```lisp
(declaim(ftype(function(fixnum)fixnum)digit-sum))
(defun digit-sum(int)
  (apply #'+ (map 'list #'digit-char-p (princ-to-string int))))
```
上記コードは中間リストが作られ効率が悪いので、`LOOP`を使う方が望ましい。

```lisp
(defun digit-sum(int)
  (loop :for c :across (princ-to-string int)
        :sum (digit-char-p c)))
```

```haskell
ghci> find (> 4) [3,4,5,6,7]
Just 5
ghci> find odd [2,4,6,8,9]
Just 9
ghci> find (=='z') "mjolnir"
Nothing
```
Haskellの`find`はCommon Lispの`FIND-IF`に相当する。
Common Lispは`maybe`に相当する機能が無く、また、`FIND-IF`は見つけたものを返す仕様となっているので、`FIND-IF`は`NIL`を見つけられない点要注意。
そのような場合は`POSITION-IF`が代替として使える。

```lisp
cl-user> (find-if #`(% '> _ 4) '(3 4 5 6 7))
5
cl-user> (find-if #'oddp '(2 4 6 8 9))
9
cl-user> (find-if #`(% 'char= #\z) "mjolnir")
NIL
cl-user> (find-if #'null '(1 nil 2))
NIL
cl-user> (position-if #'null '(1 nil 2))
1
```

```haskell
firstTo :: Int -> Maybe Int
firstTo n = find (\x -> digitSum x == n)[1..]

ghci> firstTo 40
Just 49999
ghci> firstTo 27
Just 999
ghci> firstTo 1
Just 1
ghci> firstTo 13
Just 49
```
Common Lispでは無限リストをサポートしていないので`LOOP`でなんとかする。

```lisp
(declaim(ftype(function(fixnum)(or null fixnum))first-to))
(defun first-to(n)
  (loop :for i :upfrom 1
        :when (= n (digit-sum i))
        :return i))
cl-user> (first-to 40)
49999
cl-user> (first-to 27)
999
cl-user> (first-to 1)
1
cl-user> (first-to 13)
49
```
Seriesを使うなら以下の通り。

```lisp
(defun first-to(n)
  (series:collect-first (series:choose-if (lambda(i)
                                            (= n (digit-sum i)))
                                          (series:scan-range :from 1 :type 'fixnum))))
```

## 6.3
### Mapping

```haskell
findKey :: (Eq k) => k -> [(k, v)] -> v
findKey key xs = snd . head . filter (\(k,v) -> key == k) $ xs
```
上記`findKey`に相当するものはCommon LispではALEXANDRIAが提供している`ASSOC-VALUE`となる。

```lisp
cl-user> (alexandria:assoc-value list key)
```

### Map

```haskell
ghci> Map.fromList [(3,"shoes"),(4,"trees"),(9,"bees")]
fromList [(3,"shoes"),(4,"trees"),(9,"bees")]
```
Haskellに於ける`Map`はCommon Lispに於ける`HASH-TABLE`に相当する。
上記`fromList`はALEXANDRIAの提供する`ALIST-HASH-TABLE`に相当する。
なお、ALEXANDRIAは`PLIST-HASH-TABLE`も提供している。

```lisp
cl-user> (alexandria:alist-hash-table '((3 . "shoes")(3 . "trees")(9 . "bees")))
#<HASH-TABLE :TEST EQL :COUNT 3 >
```
気をつけなければいけない点として、キーに重複があった場合、最初のキーが優先される点があげられる。
これはHaskellの`fromList`とは挙動がことなる。

```haskell
ghci> Map.fromList [("MS",1),("MS",2),("MS",3)]
fromList [("MS",3)]
```

```lisp
cl-user> (alexandria:alist-hash-table '(("MS" . 1)("MS" . 2)("MS" . 3))
                                      :test #'equal)
#<HASH-TABLE :TEST EQUAL :COUNT 1>
cl-user> (gethash "MS" *)
1
```

```haskell
phoneBook :: Map.Map String String
phoneBook = Map.fromList $
    [("betty", "555-2938")
    ,("bonnie", "452-2928")
    ,("patsy", "493-2928")
    ,("lucille", "205-2928")
    ,("wendy", "939-8282")
    ,("penny", "853-2492")
    ]
ghci> :t Map.lookup
Map.lookup :: (Ord k) => k -> Map.Map k a -> Maybe a
ghci> Map.lookup "betty" phoneBook
Just "555-2938"
ghci> Map.lookup "grace" phoneBook
Nothing
```
上記`lookup`に相当する機能は、Common Lispでは`GETHASH`になる。
なお、Common Lispは多値をサポートしているので、`GETHASH`はキーが見つかったかどうかのフラグを第二返り値として返す。

```lisp
cl-user> (defvar *phone-book* (alexandria:plist-hash-table '("betty" "555-2938" "bonnie" "452-2928" "patsy" "493-2928" "lucille" "205-2928" "wendy" "939-8282" "penny" "853-2492")
                                                           :test #'equal))
*PHONE-BOOK*
cl-user> (gethash "betty" *phone-book*)
"555-2938"
T
cl-user> (gethash "grace" *phone-book*)
NIL
NIL
```

```haskell
ghci> :t Map.insert
Map.insert :: (Ord k) => k -> a -> Map.Map k a -> Map.Map k a
ghci> Map.lookup "grace" phoneBook
Nothing
ghci> let newBook = Map.insert "grace" "341-9021" phoneBook
ghci> Map.lookup "grace" newBook
Just "341-9021"
```
上記`insert`に相当するものはCommon Lispにはない。
どうしても欲しいなら自作するしかない。
なお、自作にあたってはALEXANDRIAの提供する`COPY-HASH-TABLE`が便利に使える。

```lisp
(defun insert(key value hash-table)
  (let((ht(alexandria:copy-hash-table hash-table)))
    (setf(gethash key ht)value)
    ht))
cl-user> (defvar *new-book* (insert "grace" "341-9021" *phone-book*))
*NEW-BOOK*
cl-user> (gethash "grace" *new-book*)
"341-9021"
T
```

```haskell
ghci> :t Map.size
Map.size :: Map.Map k v -> Int
ghci> Map.size phoneBook
6
ghci> Map.size newBook
7
```
上記`size`に相当するものは、Common Lispに於いては`HASH-TABLE-COUNT`である。

```lisp
cl-user> (hash-table-count *phone-book*)
6
cl-user> (hash-table-count *new-book*)
7
```

```haskell
string2digits :: String -> [Int]
string2digits = map digitToInt . filter isDigit

ghci> string2digits "948-9282"
[9,4,8,9,2,8,2]
```

```lisp
(declaim(ftype(function(string)list)string-to-digits))
(defun string-to-digits(string)
  (mapcar #'digit-char-p (remove-if-not #'digit-char-p string)))

cl-user> (string-to-digits "948-9282")
(9 4 8 9 2 8 2)
```
直訳だと中間リストが出来て効率が悪いので、`LOOP`を使うのが望ましい。

```lisp
(defun string-to-digits(string)
  (loop :for char :across string
        :when (digit-char-p char)
        :collect :it))
```

```haskell
ghci> let intBook = Map.map string2digits phoneBook
ghci> :t intBook
intBook :: Map.Map String [Int]
ghci> Map.lookup "betty" intBook
Just [5,5,5,2,9,3,8]
```
Common Lispには上記のように`HASH-TABLE`に対して`map`する機能は存在しない。
`MAPHASH`という関数は存在するが、これは主に副作用のために使われるものだ。
上記Haskellの`Map.map`に相当するものはALEXANDRIAの`COPY-HASH-TABLE`に`:KEY`キーワード引数を渡すことによって実現できる。

```Lisp
cl-user> (defvar *int-book* (alexandria:copy-hash-table *phone-book* :key #'string-to-digits))
cl-user> (gethash "betty" *int-book*)
(5 5 5 2 9 3 8)
T
```

```haskell
phoneBook = 
    [("betty", "555-2938")
    ,("betty", "342-2492")
    ,("bonnie", "452-2928")
    ,("patsy", "493-2928")
    ,("patsy", "943-2929")
    ,("patsy", "827-9162")
    ,("lucille", "205-2928")
    ,("wendy", "939-8282")
    ,("penny", "853-2492")
    ,("penny", "555-2111")
    ]
```

```lisp
(defvar *phone-book* '(("betty" . "555-2938")
                       ("betty" . "342-2492")
                       ("bonnie" . "452-2928")
                       ("patsy" . "493-2928")
                       ("patsy" . "943-2929")
                       ("patsy" . "827-9162")
                       ("lucille" . "205-2928")
                       ("wendy" . "939-8282")
                       ("penny" . "853-2492")
                       ("penny" . "555-2111")))
```

```haskell
phoneBookToMap :: (Ord k) => [(k,String)] -> Map.Map k String
phoneBookToMap xs = Map.fromListWith add xs
    where add number1 number2 = number1 ++ ", " ++ number2
```
上記Haskellの`Map.fromListWith`に相当するものはCommon Lispにはない。
必要なら自作するしかない。

```lisp
(defun alist-hash-table-with(function alist &rest args)
  (let((ht(apply #'make-hash-table args)))
    (loop :for (key . value) :in alist
          :do (multiple-value-bind(v exist?)(gethash key ht)
                (if exist?
                  (setf(gethash key ht)(funcall function v value))
                  (setf(gethash key ht)value))))
    ht))
```

```lisp
(declaim(ftype(function(list)hash-table)phone-book-to-map))
(defun phone-book-to-map(xs)
  ((lambda(add)(alist-hash-table-with add xs :test #'equal)))
   (lambda(n1 n2)(uiop:strcat n1 ", " n2)))
```

```haskell
ghci> Map.lookup "patsy" $ phoneBookToMap phoneBook
"827-9162, 943-2929, 493-2928"
```

```lisp
cl-user> (gethash "patsy" (phone-book-to-map *phone-book*))
"493-2928, 943-2928, 827-9162"
T
```

```haskell
phoneBookToMap :: (Ord k) => [(k, a)] -> Map.Map k [a]
phoneBookToMap xs = Map.fromListWith (++) $ map (\(k,v) -> (k, [v])) xs

ghci> Map.lookup "patsy" $ phoneBookToMap phoneBook
["827-9162", "943-2929", "493-2928"]
```

```lisp
(declaim(ftype(function(list)hash-table)phone-book-to-map))
(defun phone-book-to-map(alist)
  (alist-hash-table-with #'append (mapcar (trivia:lambda-match((cons k v)(cons k (list v))))
                                          alist)
                         :test #'equal))

cl-user> (gethash "patsy" (phone-book-to-map *phone-book*))
("493-2928" "943-2929" "827-9162")
```

```haskell
ghci> Map.fromListWith max [(2,3),(2,5),(2,100),(3,29),(3,22),(3,11),(4,22),(4,15)]
fromList [(2,100),(3,29),(4,22)]

ghci> Map.fromListWith (+) [(2,3),(2,5),(2,100),(3,29),(3,22),(3,11),(4,22),(4,15)]
fromList [(2,108),(3,62),(4,37)]
```

```lisp
cl-user> (alist-hash-table-with #'max '((2 . 3)(2 . 5)(2 . 100)(3 . 29)(3 . 22)(3 . 11)(4 . 22)(4 . 15)))
#<HASH-TABLE :TEST EQL :COUNT 3>
cl-user> (alexandria:hash-table-alist *)
((4 . 22)(3 . 29)(2 . 100))
```
Common Lispの`HASH-TABLE`は通常中身は見えない。
必要なら`PRINT-OBJECT`メソッドを書けばいい。

```lisp
(defmethod print-object((ht hash-table)stream)
  (print-unreadable-object(ht stream :type t)
    (maphash (lambda(k v)
               (format stream "(~W . ~W)"k v))
             ht))
  ht)

cl-user> (alist-hash-table-with #'+ '((2 . 3)(2 . 5)(2 . 100)(3 . 29)(3 . 22)(3 . 11)(4 . 22)(4 . 15)))
#<HASH-TABLE (2 . 108)(3 . 62)(4 . 37)>
```

## 6.4
### Making module

```haskell
module Geometry
( sphereVolume
, sphereArea
, cubeVolume
, cubeArea
, cuboidArea
, cuboidVolume
) where

sphereVolume :: Float -> Float
sphereVolume radius = (4.0 / 3.0) * pi * (radius ^ 3)

sphereArea :: Float -> Float
spehreArea radius = 4 * pi * (radius ^ 2)

cubeVolume :: Float -> Float
cubeVolume side = cuboidVolume side side side

cubeArea :: Float -> Float
cubeArea side = cuboidArea side side side

cuboidVolume :: Float -> Float -> Float -> Float
cuboidVolume a b c = rectArea a b * c

cuboidArea :: Float -> Float -> Float -> Float
cuboidArea a b c = rectArea a b * 2 + rectArea a c * 2 + rectArea c b * 2

rectArea :: Float -> Float -> Float
rectArea a b = a * b
```
上記Haskellコード冒頭の`module`キーワードは、Common Lispに於いては`DEFPACKAGE`に相当すると言える。
`DEFPACKAGE`の`:EXPORT`節は機能ごとに複数個に分割し、コメントを加えておくのが望ましい。

```lisp
(defpackage :geometry(:use :cl)
  (:export ; SPHERE
           #:sphere-volume #:sphere-area)
  (:export ; CUBE
           #:cube-volume #:cube-Area)
  (:export ; CUBOID
           #:cuboid-volume #:cuboid-area))
(in-package :geometry)

(declaim(ftype(function(float)float)sphere-volume))
(defun sphere-volume(radius)
  (* (/ 4.0 3.0) pi (expt radius 3)))

(declaim(ftype(function(float)float)sphere-area))
(defun sphere-area(radius)
  (* 4 pi (expt radius 2)))

(declaim(ftype(function(float)float)cube-volume))
(defun cube-volume(side)
  (coboid-valume side side side))

(decliam(ftype(function(float)float)cobe-area))
(defun cube-area(side)
  (cuboid-area side side side))

(declaim(ftype(function(float float float)float)cuboid-volume))
(defun cuboid-volume(a b c)
  (rect-area a (* b c)))

(declaim(ftype(function(float float float)float)cuboid-area))
(defun cuboid-area(a b c)
  (+ (rect-area a (* b 2))
     (rect-area a (* c 2))
     (rect-area c (* b 2))))

(declaim(ftype(function(float float)float)rect-area))
(defun rect-area(a b)
  (* a b))
```
### Hierarchical module

```haskell
# Sphere.hs
module Geometry.Sphere
( volume
, area
) where

volume :: Float -> Float
volume radius = (4.0 / 3.0) * pi * (radius ^ 3)

area :: Float -> Float
area radius = 4 * pi * (radius ^ 2)

# Cuboid.hs
module Geometry.Cuboid
( volume
, area
) where

volume :: Float -> Float -> Float -> Float
volume a b c = rectArea a b * c

area :: Float -> Float -> Float -> Float
area a b c = rectArea a b * 2 + rectArea a c * 2 + rectArea c b * 2

# Cube.hs
module Geometry.Cube
( volume
, area
) where

import qualified Geometry.Cuboid as Cuboid

volume :: Float -> Float
volume side = Cuboid.volume side side side

area :: Float -> Float
area side = Cuboid.area side side side
```

```lisp
;; geometry.asd
(in-package :asdf)
(defsystem "geometry"
  :components ((:file "sphere")
               (:file "cuboid")
               (:file "cube" :depends-on ("cuboid"))))

;; sphere.lisp
(defpackage :geometry.sphere(:use :cl)
  (:export #:volume #:area))
(in-package :geometry.sphere)

(declaim(ftype(function(float)float)volume))
(defun volume(radius)
  (* #.(/ 4.0 3.0) pi (expt radius 3)))

(declaim(ftype(function(float)float)area))
(defun area(radius)
  (* 4 pi (expt radius 2)))

;; cuboid.lisp
(defpackage :geometry.cuboid(:use :cl)
  (:export #:volume #:area))
(in-package :geometry.cuboid)

(declaim(ftype(function(float float float)float)volume))
(defun volume(a b c)
  (* (rect-area a b)
     c))

(declaim(ftype(function(float float float)float)area))
(defun area(a b c)
  (+ (rect-area a (* b 2))
     (rect-area a (* c 2))
     (rect-area c (* b 2))))

(declaim(ftype(function(float float)float)rect-area))
(defun rect-area(a b)
  (* a b))

;; cube.lisp
(defpackage :geometry.cube(:use :cl)
  (:export #:volume #:area))
(in-package :geometry.cube)

(qualified-use :geometry.cuboid :as :cuboid)

(declaim(ftype(function(float)float)volume))
(defun volume(side)
  (cuboid.volume side side side))

(declaim(ftype(function(float)float)area))
(defun area(side)
  (cuboid.area side side side))
```

```haskell
import Geometry.Sphere
## or
import qualified Geometry.Sphere as Sphere
import qualified Geometry.Cuboid as Cuboid
import qualified Geometry.Cube as Cube
```

```lisp
(asdf:load-system :geometry)
(use-package :geometry.sphere)
;; or
(qualified-use :geometry.sphere :as :sphere)
(qualified-use :geometry.cuboid :as :cuboid)
(aualified-use :geometry.cube :as :cube)
```
