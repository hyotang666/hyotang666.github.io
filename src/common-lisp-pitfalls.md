# Common Lisp Pitfalls
## Meta info
### 対象読者
複数の処理系で可能な限り可搬的になるようコードを書きたい初級〜中級CLer。
### 現時点での対象処理系
SBCL、CLISP、ECL、CCL

## Introduction
僕がハマってきたピットフォール群をメモ的にコレクションしていきたい。

ピットフォールは主に以下の種類に分けられる。

* 仕様上未定義と定義されている（明示的未定義）。
* 仕様上触れられていない（暗黙裏に未定義）。
* 仕様上明確に定義されているが難解。
* 仕様上明確に定義されているが独自拡張している処理系あり。
* 処理系が仕様に違反。

先頭から順番に読むもよし、気になるオペレータ名で検索をかけるもよし。

なお、記事が追加される場合は先頭に追加していくこととする。
また、項目は重複する可能性があるものとする。

## READ-SEQUENCE
言語仕様では第二引数の型エラーについて触れられていない。
多くの処理系では第二引数がストリームでない場合`TYPE-ERROR`を発するが、そうでない処理系もある。

CCLは第一引数が空シーケンスの場合、第二引数がなんであれ成功裏に0を返す。

```lisp
? (read-sequence #() :not-stream)
=> 0
```

通常このような愚かなコードは書かないが、「引数がストリームでない場合エラーとなる」というようなテストを書いている場合想定外に成功するという形で出会うことがある。

## VALUES as type specifier
通常の型指定子は第一返り値の型を示すものであり、暗黙理に多値が返る可能性があることを示している。


```lisp
(the integer (values 0 1)) ; <--- ok
```

多値の型指定をする場合には`VALUES`型指定子が使える。

```lisp
(the (values integer integer)(values 0 1)) ; <--- ok
```
ただ、上記の場合も暗黙理に第三返り値以降の多値が返る可能性があることを示していることとなる。

```lisp
(the (values integer integer)(values 0 1 2)) ; <--- ok
```
例えば返り値は１つだけでけして多値が返ることはないということを示したいなら`&OPTIONAL`を使って以下のようにする。

```lisp
(the (values integer &optional)(values 1 2)) ; <--- not ok
```

## LOOP
:ON節に非NILアトムが渡ってきた場合、エラーではなくNILとなる。

```lisp
(loop :for a :on 'non-nil-atom :collect a) => NIL
```

## \*MACROEXPAND-HOOK\*
変数\*MACROEXPAND-HOOK\*が受け取る関数のAPIは(expander form env)である。
使い方としては、何らかの処理を行った後、MACRO-FUNCTIONであるEXPANDERにFORMとENVとを渡す形でFUNCALLしてあげれば良い。
例えばCLHSには以下のような例がある。

```lisp
(defun hook (expander form env)
   (format t "Now expanding: ~S~%" form)
   (funcall expander form env)) =>  HOOK 
(defmacro machook (x y) `(/ (+ ,x ,y) 2)) =>  MACHOOK 
(macroexpand '(machook 1 2)) =>  (/ (+ 1 2) 2), true 
(let ((*macroexpand-hook* #'hook)) (macroexpand '(machook 1 2)))
>>  Now expanding (MACHOOK 1 2) 
=>  (/ (+ 1 2) 2), true
```

気をつけなければならないのは、このような書き方では外側にある別な\*MACROEXPAND-HOOK\*関数をシャドウしてしまう点だ。
これにより、内側のフックは機能するが外側のフックが機能せず全体として期待と異なる振る舞いになってしまう場合が起こりうる。
これを避けるためにはクロージャを利用して以下のようにすると良い。

```lisp
(defun hooker(outer-hook)
  (lambda(expander form env)
    ...
    (funcall outer-hook expander form env)))

(let((*macroexpand-hook*(hooker *macroexpand-hook*)))
  ...)
```

HOOKER関数はその時点での外側のフック関数を引数として補足し、フック関数を返す関数である。

変数\*MACROEXPAND-HOOK\*はスペシャル変数、すなわち動的な束縛を行うので、外側の変数を補足することができない点要注意。
以下のコード例は無限ループに陥る。

```lisp
(let((*macroexpand-hook*
       (lambda(expander form env)
         ...
         (funcall *macroexpand-hook* expander form env))))
  ...)
```

LAMBDAの中の\*MACROEXPAND-HOOK\*の値は動的な（レキシカルでない）値なので、LAMBDA自信になる。
よって一度呼び出されると、自分自信を無限再起呼び出しし続けることとなる。

## IGNORE-ERRORS with multiple-value.
マクロ`IGNORE-ERRORS`はformが`ERROR`をシグナルするとそのコンディションを捕まえて`(VALUES NULL CONDITION)`を返す。
formが`ERROR`をシグナルすることがなければformの返り値をそのまま返す。
ここでいう「そのまま」は、formが多値を返したならその多値をそのまま返すという意味である。
よって、たとえば`MULTIPLE-VALUE-BIND`などを用いて「第二返り値があるかないか」だけで失敗か否かをチェックしようとするとformが多値を返したときに混同してしまう。

第二返り値が`CONDITION`型かどうかをチェックしている場合でも、formが成功裏に`CONDITION`を第二返り値として返したら混同してしまう。

`IGNORE-ERRORS`の第二返り値を使いたい場合はめんどくさがらず`HANDLER-CASE`を書くべきである。

## WRITE, \*PRINT-PRETTY\*
割と多くの処理系で`(function hoge)`というリストは表示できない。
`#'hoge`になってしまう。
筆者が調べた限りでは、SBCLは`*PRINT-PRETTY*`を`NIL`に束縛することで期待通り出力できるようだ。
CLISP, ECL, CCLではリスト`(function hoge)`の出力方法は見つけられなかった。
仕様ではこの点については触れられていない。
同様に`(quote hoge)`というリストも割と多くの処理系で表示できない。

```lisp
#+sbcl
(write '(function hoge) :pretty nil)
(FUNCTION HOGE) ; <--- output
#'HOGE		; <--- return

#-sbcl
(write '(function hoge) :pretty nil)
#'HOGE		; <--- output
#'HOGE		; <--- return
```
ドキュメンテーション自動生成ツール開発中に、メソッドの各シグネチャを出力する際、シグネチャが`(function function)`だった場合に`#'FUNCTION`と出力されてしまうという形で出会った。

## STRING family
`STRING`のファミリーは引数に文字列指定子を受け付ける。
すなわち、文字、シンボルも受け付けられる。

```lisp
(string= () "NIL") ; => T
(string= :a #\A) ; => T
```
文字列同士でしか比較をしたくない場合は`EQUAL`、`EQUALP`を使う。

## IMPORT, EXPORT, UNEXPORT, SHADOWING-IMPORT, SHADOW
第一引数はあくまでリストである。
便宜的にシンボル自身も受け付けるが、あくまで基本はリストである。

筆者はこれを反対に覚えてしまいハマった。
具体的には`NIL`を`IMPORT`しようしたが`IMPORT`されなかった。
`NIL`を裸で渡した場合、「シンボルのリストを受け付けたが、中身は空であった」と解釈される。
`NIL`を操作したい場合、必ずリストに括って渡さなければならない。

## CONSTANTP with &WHOLE
`CONSTANTP`は受け取った引数がマクロフォームであった場合、マクロ展開を行う可能性がある。
この点は仕様上明示的に処理系依存とされている。

もしマクロフォームが`&WHOLE`ラムダリストキーワードで受けたフォームを返した場合、無限マクロ展開に陥る。
これを回避するためには`*MACROEXPAND-HOOK*`を束縛し、条件によって大域脱出（`GO`、`RETURN-FROM`、`THROW`）を行えばよい。

## MACROEXPAND-1 with &WHOLE
マクロ展開関数が`&WHOLE`ラムダリストキーワードで受けたフォームを返した場合、直感的には展開が行われていないので第二返り値が`NIL`になりそうなものだが、`T`となる。
仕様では、引数のフォームがマクロフォームであれば第二返り値は`T`となる。

> If form is a macro form, then the expansion is a macro expansion and expanded-p is true.

第二返り値の名前が`EXPANDED-P`であることが、誤解の原因と言える。

## LIST\*
無引数で呼び出した場合の挙動に関しては仕様上触れられていない。
`NIL`が返る処理系とエラーになる処理系とがある。

```lisp
#+(or clisp sbcl ccl)
(list*) => ERROR
#+ecl
(list*) => NIL
```

## \*PRINT-LENGTH\*
プリティプリンタ周りの実装は可搬的でないケースが多い。
構造体の表示に関しては仕様でも触れられていない。
CCLでは型名もスロット名も「リスト内の要素」と解釈されている。
SBCL,ECLでは型名はカウントせず、スロット：値の対を一要素と解釈されている。
CLISPでは構造体自体は言わばアトムであると解釈されている。

```lisp
=> FOO
(defstruct foo a b c d)
(let((*print-length* 2))
  (print(make-foo :a (list 1 2 3 4 5))))

#+clisp
#S(FOO :A (1 2 ...) :B NIL :C NIL :D NIL)

#+(or sbcl ecl)
#S(FOO :A (1 2 ...) :B NIL ...)

#+ccl
#S(FOO :A ...)
```

## READ
これは処理系のバグに相当するが、`+.`や`-.`はAnsiスタンダードでは数ではないとされているが、ECLでは０に解釈される。

```lisp
(read-from-string "+.") => implementation-dependent.
                        ; Symbol +. in spec.
                        ; 0 in ECL.
```

通常問題になることは無いと思われるが、Common LispでCommon Lispのパーザを書き、それをテストしたところ遭遇した。

## BACKQUOTE
バッククォートの実装は処理系依存である。
多くの処理系でバッククォートはマクロに展開され、すなわちコンパイル時に等価なフォームが生成されるが、そうでない処理系も存在する。
具体的にはCCLはフォーム生成をリード時に行う。

```lisp
'`(hoge ,@(cdr '(1 2 3))) => implementation-dependent.
                          ; `(HOGE ,`(CDR '(1 2 3))) in many impls.
                          ; (LIST* 'HOGE (CDR '(1 2 3))) in CCL.
```

## SIGNAL
`SIGNAL`の振る舞いは、受け取ったコンディションを元にハンドラを探し、ハンドラがあればコールしてまわり、どのハンドラもコントロールフロー制御をしなければ最終的に`NIL`を返すというものである。

トップレベルにハンドラがあるかどうかは処理系依存となる。

```lisp
(signal 'error) => implementation-dependent. NIL or invokes debugger.
```
## `*STANDARD-OUTPUT*` `*STANDARD-INPUT*`
多くの処理系では、たとえば`*STANDARD-OUTPUT*`に`*STANDARD-INPUT*`を束縛することはエラーとなるが、そうでない処理系も存在する。
たとえばCCLでは両シンボルは`*TERMINAL-IO*`へのaliasとして機能している。

```lisp
(let((*standard-output* *standard-input*))
  ...)
=> implementation-dependent. Error or works.
```

```lisp
(output-stream-p *standard-input*) => implementation-dependent. T in CCL.
```
通常このような馬鹿げたコードを書くことはないが、「アウトプットストリームを期待している関数にインプットストリームを渡すとエラーになる」という文脈のテストコードを書く際などに、想定外に成功するという形で現れる。

## CASE ECASE CCASE
`NIL`ないし`T`をキーにしたい場合は必ず括弧にくくらねばならない。

```lisp
(case var
  (nil :this-clause-is-never-chosen.)
  ((nil) :ok.)
  (t :this-clause-is-treated-as-default-clause.)
  ((t) :ok.))
```

## LOOP
`:MAXIMIZE`や`:MINIMIZE`が実行されなかった場合の返り値は未定義。

```lisp
(loop :for i :in () :minimize i) => unspecified. NIL or 0.
```

終端チェック節の後に変数束縛節を使うのはinvalid。
期待通り動く処理系とそうでない処理系とがある。

```lisp
(loop :for i :in '(1 1 1 #\1)
      :while (integerp i)
      :for c = (code-char i) ; <--- invalid.
      :do ...)
```

## DEFTYPE
再帰定義は未定義。
上手く動く処理系とそうでない処理系がある。

```lisp
(deftype strings()
  (or null (cons string strings)))
=> STRINGS
(typep :hoge strings)
=> unspecified. Works or infinite loop.
```

マクロとしての`AND`は左から右に評価されるが、型指定子としての`AND`はその限りではない。

```lisp
(typep :hoge '(and integer (satisfies evenp)))
=> unspecified. Works or signals error.
```

## DOCUMENTATION
これは処理系のバグに相当するが、ECLではSETFできない。
仕様ではSETF出来る。

```lisp
;; @ECL
(setf(documentation 'hoge 'function) "docstring")
=> "docstring"
(documentation 'hoge 'function)
=> NIL
```

## MAKE-STRING-INPUT-STREAM WITH-INPUT-FROM-STRING
これは処理系独自拡張になるが、ECLでは文字列指定子（string-designator）が使える。

```lisp
;; @ECL
(with-input-from-string(s :hoge)
  (read s))
=> HOGE ; Error in spec.

(with-input-from-string(s #\c)
  (read s))
=> C ; Error in spec.
```

## SETF FDEFINITION
SETF可能でも、それがSETF Expanderを持つとは限らない。

```lisp
(defstruct foo bar)
=> FOO
(fdefinition '(setf foo-bar)) => unspecified.

(fdefinition '(setf car)) => unspecified.
```

## NIL
これは可搬的なのだが、分かりづらいので。

`NIL`は型名でもある。
型名としての`NIL`は「無」を表す。
そのためあらゆる型のsubtypeである。

```lisp
(subtypep nil nil) => T
```

また、「無」を表すので、けしてどの型でもない。
すなわち自分自身でもない。

```lisp
(typep nil nil) => NIL
```

値としての`NIL`の型名は`NULL`である。

```lisp
(typep nil 'null) => T
```

筆者個人は例えば以下のようなコードを書き、

```lisp
(typep '(0) '(cons (eql 0) nil))
```
`T`を期待するも`NIL`が返ってきて、「何故だ」と悩んだ挙句、「あぁ、`NIL`じゃない、`NULL`だ」となることが、まま、ある。

## SYMBOL
エスケープされた文字を含むシンボルの表示方法はポータブルではない。

```lisp
\#hoge
=> |#HOGE|
; otherwise
=> \#HOGE
```

## PATHNAME
リテラルで書く場合、変な値が入る場合がある。

```lisp
(pathname-version #P"") => :NEWEST
```

これは処理系独自拡張なのだが、シンボルを受け付ける処理系もある。

```lisp
(pathname :/foo/bar/bazz) => #P"/foo/bar/bazz" ; Error in spec.
```
## *
０を掛けた場合、０になるとは限らない。

```lisp
(* 0 0.0) => 0 or 0.0
```

## CONDITION
`PRINC`した場合、メッセージが表示されるとは限らない。

```lisp
(princ (nth-value 1 (ignore-errors (/ 2 0))))
=> unspecified. "Division by zero." or #<DIVISION-BY-ZERO #X123456>
```

## SYMBOL-FUNCTION FDEFINITION
シンボルがマクロや特殊形式の場合、関数オブジェクトが入っているとは限らない。

```lisp
(symbol-function 'when) => unspecified.
```

## CONCATENATE
これは処理系独自拡張なのだが、SEQUENCE-DESIGNATORとして`ARRAY`を受け付ける処理系もある。

```lisp
(concatenate 'array #(1 2 3) #(4 5 6)) => #(1 2 3 4 5 6) ; Error in many impls.
```

## COERCE
シーケンスを配列に出来る処理系とそうでない処理系がある。

```lisp
(coerce '(1 2 3) 'array) => implementation-dependent. #(1 2 3) or signals error.
```

## MAKE-ARRAY
どのような値で初期化されるかは未定義。

```lisp
(make-array 1) => unspecified. #(0) or #(nil)
```

