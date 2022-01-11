# Caveman kills ruby on rails - Chapter 12.5
## Meta info
### 対象読者
* 本連載の読者。
* メソッドコンビネーションの具体例に触れたいCLer

## NOTE
筆者はcavemanを捨て[snooze](https://github.com/joaotavora/snooze)を使うようになった。
詳細は[ここ](why-snooze.html)に記してある。

## Introduction
本稿は[原著](https://book.impress.co.jp/books/1117101135)の各章をCommon Lispに翻訳するシリーズの第12.5章である。

やることが高度化してきており、結果としてルーティングコードが肥大化してきている。
そこでいくつかのヘルパーを導入したい。
だが、それはCavemanの学習という観点からは本質ではないところである。
よって下準備として、本章に詰め込んでしまいたい。

章立てが12.5となっているのは13章の手前ぐらいの意味しかない点、予め断っておく。

## Status-code
ソースコードの中にHTTPステータスコードを埋め込むことはよくある。
だが、そのままだとマジックナンバーとなってしまう。
名前をつけて定数とするほうが健全であろう。

HTTPステータスコードはrfcで定義されていてWebに公開されているので、それをさくっとスクレイピングしてコンパイルしてしまおう。

HTTPクライアントには[DEXADOR](https://github.com/fukamachi/dexador)を、HTMLのパースには[PLUMP](https://github.com/Shinmera/plump)を使う。

```lisp
(defvar *doc*(plump:parse (dexador:get "https://www.w3.org/protocols/rfc2616/rfc2616-sec10.html")))
```

plumpが作るDOMを操作するには、同作者による[clss](https://github.com/Shinmera/CLSS)が便利に使える。

```lisp
(defun constant-forms()
  (mapcon (lambda(list)
            (when(equal "h3"(plump:tag-name (car list)))
              (let*((position(position "h3" (cdr list) :test #'equal :key #'plump:tag-name))
                    (ps(subseq (cdr list) 0 position))
                    (h3(loop :for element :across (plump:children (car list))
                             :when (plump:text-node-p element)
                             :collect (plump:text element) :into result
                             :finally (return (string-trim " "(apply #'concatenate 'string result)))))
                    (code))
                (multiple-value-bind(match-p start)(ppcre:scan "^[0-9][0-9][0-9]" h3)
                  (if (and match-p
                           (not(eql 306 (setf code (parse-integer h3 :junk-allowed t)))))
                    `((defconstant ,(read-from-string
                                      (format nil "+~A+"
                                              (substitute #\- #\space(string-trim " "(ppcre::nsubseq h3 start)))))
                                   ,code
                                   ,(string-trim '(#\newline #\space)
                                                 (remove #\return
                                                         (apply #'concatenate 'string (mapcar #'plump:text ps)))))))))))
          (let*((vector(plump:child-elements(aref(clss:select "body" *doc*)0)))
                (position (position "h3" vector :test #'equal :key #'plump:tag-name)))
            (coerce (subseq vector position)'list))))
```
REPLで叩くとこんな感じ。

```lisp
* (constant-forms)
((DEFCONSTANT +CONTINUE+
   100
   "The client SHOULD continue with its request. This interim response is
    used to inform the client that the initial part of the request has
    been received and has not yet been rejected by the server. The client
    SHOULD continue by sending the remainder of the request or, if the
    request has already been completed, ignore this response. The server
    MUST send a final response after the request has been completed. See
    section 8.2.3 for detailed discussion of the use and handling of this
    status code.")
 ...)
```

最後に適当にthunkを作る。

```lisp
(defun thunk()
  (let((list(constant-forms)))
    (format t "~&~(~S~)" `(in-package :cl-user))
    (format t "~&~(~S~)" `(defpackage #:rfc2616-sec10 (:use :cl)
                          (:nicknames #:status-code)
                          (:export ,@(mapcar #'cadr list))))
    (format t "~&~(~S~)" `(in-package #:rfc2616-sec10))
    (format t "~{~&~%~S~}" list)))
```

## with-authenticity-check
ルーティングの冒頭で典型的に行う条件分岐をマクロに落とし込もう。

```lisp
(defmacro with-authenticity-check((&rest check*)&body body)
  (labels((rec(list)
            (if(endp list)
              `(progn ,@body)
              (body(caar list)(cadar list)(cdr list))))
          (body(key value rest)
            (ecase key
              (:token `(if(not(string= ,value (token)))
                          (throw-code status-code:+forbidden+)
                          ,(rec rest)))
              (:logged-in `(if(not(hermetic:logged-in-p))
                              (throw-code status-code:+unauthorized+)
                              ,(rec rest))))))
    (rec (mapcar #'alexandria:ensure-list check*))))
```

## ensure-let
データベースにアクセスして、オブジェクトが見つからなければ`MYWAY:NEXT-ROUTE`するのはよくあるのでマクロでまとめてしまう。


```lisp
(defmacro ensure-let((&rest bind*)&body body)
  (labels((rec(binds)
            (if(endp binds)
              `(progn ,@body)
              (body(car binds)(cdr binds))))
          (body(bind rest)
            `(let(,bind)
               (if(null ,(car bind))
                 (myway:next-route)
                 ,(rec rest)))))
    (rec bind*)))
```

## update-instance
オブジェクトに`SETF`していくのは醜いしコードも肥大してしまう。
僕達が本当に欲しいのは`CL:MAKE-INSTANCE`と同様のシンタックスで、第一引数に受け取ったオブジェクトを破壊的にアップデートするコマンドだ。
MOPを利用することで以下のように作れる。

```Lisp
(defgeneric update-instance(object &rest args))

(defmethod update-instance((object standard-object)&rest args)
  (loop :with initargs = (loop :for key :in args :by #'cddr :collect key)
        :for slot :in (c2mop:class-slots (class-of object))
        :for keys = (intersection (c2mop:slot-definition-initargs slot) initargs)
        :when (and keys
                   (or (null (cdr keys))
                       (error "Dupcated initargs ~S"keys)))
        :do (let((value(getf args(car keys))))
              (unless (equal "" value)
                (setf (slot-value object (c2mop:slot-definition-name slot))
                      value))))
  object)
```
なお、引数が空文字列だった場合、引数は無視される点要注意。
というのもCavemanアプリでは空っぽの入力フォームは空文字列として渡ってくるからである。

また、キーワード引数が重複している場合、手前のものが採用され、後ろのものはシャドウイングされる点も要注意。
この振る舞いは`CL:MAKE-INSTANCE`の振る舞いに則っている。

## validation
MitoのオブジェクトはCLOSに則っており、継承も当然行える。
あるオブジェクトがバリデーションされる時、継承しているスーパークラスもバリデーションされて欲しい。

これまで個別に関数を作って対応していたが、総称関数にする方が良さそうだ。

さて、総称関数のAPIに関してだが、これまでと同様、第一返り値に破壊変更されたかもしれないオブジェクトを、第二返り値にエラーメッセージを表すドット対のリストを返したい。
問題はスーパークラスで行われる各バリデーション結果と結合して全体の返り値を作らねばならないところである。
このような場合に便利に使えるものとしてCLOSにはメソッドコンビネーションがある。

この度メソッドコンビネーションを自作するに当たっては[仕様](http://clhs.lisp.se/Body/m_defi_4.htm#define-method-combination)を参照した。
例が豊富で大変わかりやすかった。

なお、本メソッドコンビネーションは簡単のために主メソッド以外はサポートしていない。

```
(define-method-combination validate()
                           ((primary (validate) :required t))
  (labels((rec(methods)
            (if(endp (cdr methods))
              `(call-method ,(car methods) nil)
              `(multiple-value-bind(o e),(rec (cdr methods))
                 (values o (append e (nth-value 1 (call-method ,(car methods) nil))))))))
    (rec primary)))

(defgeneric validate(object &key target-slots test)
  (:method-combination validate))
```

## Method-case
ディスパッチャも典型的なコードとなるのでマクロにまとめよう。

```lisp
(defmacro method-case(method &rest clauses)
  (let((var(gensym"VAR")))
    `(let((,var ,method))
       (cond
         ,@(mapcar (lambda(clause)
                     `((string= ,(car clause),var),@(cdr clause)))
                   clauses)
         (t (throw-code status-code:+method-not-allowed+))))))
```
