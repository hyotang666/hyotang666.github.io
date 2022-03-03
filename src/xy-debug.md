# XY-DEBUG, or XY-problem in the debug phase. <br><sub>A case study in common lisp.</sub>
## Meta notes.
### 対象読者

* エラーハンドリングに興味のあるCLer。
* わけのわからないエラーメッセージに憎しみを覚えた経験のある方。
* Common Lispにおけるエラーハンドリングに関心のある他言語ユーザー。

# [XY-PROBLEM]

[XY-PROBLEM]をご存知でしょうか？
wikipediaによると[伽藍とバザール]の著者である[Eric S. Raymond]氏が著作[How To Ask Questions The Smart Way]の「べからず集(Questions Not To Ask)」に於いて示したものらしいです。
[xyproblem.info]による具体例として「ファイルの拡張子を取り出したくて(Y)文字列から最後３つの文字を取り出す方法(X)を質問する」などが挙げられています。
分かりやすい。

## Case in programming.
[XY-PROBLEM]はなにも人と人との対話においてのみ現出する問題ではありません。
プログラムを書いたことのある方なら、この[XY-PROBLEM]には割とよく悩まされているのではないでしょうか。
すなわちエラーメッセージです。

具体例をいくつか示しましょう。

### [`UNBOUND-VARIABLE`]
ある関数を呼び出したところ[`UNBOUND-VARIABLE`]型の[`CONDITION`]が発せられたが、エラーメッセージは「The variable foo is unbound.」というものでしかなく、**どうするべきだったのかについては何も教えてくれない**。

### [`NO-APPLICABLE-METHOD`]
ある総称関数を呼び出したところ[`NO-APPLICABLE-METHOD`]エラーに陥ったが、では**どのようなクラスに特定化されたメソッドが存在しているのかについては何も教えてくれない**。

# Case study in common lisp.
上に見たようなエラーメッセージにおける[XY-PROBLEM]は可能な限り排除するのが良策です。
開発中は関数同士がどのように呼び出し合っているかというプログラムの構造をある程度把握できているのでXというエラーメッセージを目にしてもそのようなエラーが出るということはYに問題があるんだな、と憶測しやすいのですが（将来の自分を含む）第三者はその限りではありません。

丁寧なエラーハンドリングは丁寧なドキュメンテーション以上に（将来の自分を含む）ユーザーに利益をもたらすものです。
ドキュメンテーションを読まないユーザーでもエラーメッセージ自体は読むでしょうから。

以下、Common Lispにおける簡単な対処方を記しておきます。

## Pseudo variable with symbol-macro.
[`ERROR`]を捕捉する場合[`HANDLER-BIND`]なり[`HANDLER-CASE`]なりを使うものですが、これらのフォームがソース上にゴテゴテとあると可読性が著しく損なわれます。
そのような場合、シンボルマクロが便利に使えます。
シンボルマクロは[`DEFINE-SYMBOL-MACRO`]ないし[`SYMBOL-MACROLET`]で定義します。

スペシャル変数があるとします。

```lisp
(defvar *special*)
```

このスペシャル変数はマクロによって束縛を作られるよう設計されているとします。

```lisp
(defmacro with-special ((bind) &body body)
  `(let ((*special* ,bind))
    ,@body))
```

各補助関数群は、そのスコープの外側で上記マクロによってある種の環境が構築済みであると期待しているものとします。

```lisp
(defun helper (something)
  (compute *special* something))
```

（自分を含む）ある人がうっかりマクロでラップするのを忘れて(Y)直接上記`HELPER`を呼び出した場合、エラーメッセージは「The variable \*special\* is unbound.」というもの(X)となり、`WITH-SPECIAL`でコードをラップすべきことまでは伝わりません。

以下のようにリファクタリングすることで適切なエラーメッセージを示しつつソースコードの可読性を保持できます。

まずはスペシャル変数を改名します。
たんに耳あてを取り除くだけです。

```lisp
(defvar special)
```

それに合わせてマクロも修正します。

```lisp
(defmacro with-special ((bind) &body body)
  `(let ((special ,bind))
    ,@body))
```

シンボルマクロで擬似変数を定義します。

```lisp
(define-symbol-macro *special*
  (handler-case special
    (unbound-variable (condition)
      (error "The variable ~S is unbound. ~:@_Hint: Forgot to wrap your code with ~S?"
             (cell-error-name condition)
             'with-special))))
```
補助関数`HELPER`は修正の必要がない点要注目。

副作用として代入や束縛ができなくなっている点要注意。

```lisp
(setf *special* :will-fail)

(let ((*special* :will-fail))
  (helper :something))
```

上記２例はいずれもエラーになります。
前者は`(setf handler-case)`というSETF関数が存在しないため、後者は背後にあるスペシャル変数`SPECIAL`は未束縛なままなためです。

むしろ人によってはこの直接の代入や束縛を難しくさせる副作用にこそ魅力を感じる方もいるかもしれません。

## Listing up applicable methods.
Applicableなメソッドを示すには[`NO-APPLICABLE-METHOD`]を定義し、[closer-mop]でリストアップします。

総称関数`DEMO`があるとします。

```lisp
(defgeneric demo (x))
```

メソッドはシンボルだけ定義されているとします。

```lisp
(defmethod demo ((x symbol)) x)
```
[`NO-APPLICABLE-METHOD`]の実装は以下のようになります。

```lisp
(defmethod no-applicable-method ((this (eql #'demo)) &rest args)
  (handler-case (call-next-method)
    (error (condition)
      (error "~A ~I~:@_Applicable methods: ~2I~_~S"
             condition
             (mapcar (lambda (m) (mapcar #'class-name (c2mop:method-specializers m)))
                     (c2mop:generic-function-methods this))))))
```

エラーメッセージは以下のようになります。（於SBCL。）

```
#<THREAD "main thread" RUNNING {xxxxxxx}>:
  There is no applicable method for the generic function
    #<STANDARD-GENERIC-FUNCTION DEMO (1)>
  when called with arguments
    (666)
See also:
  The ANSI Standard, Section 7,6,6
Applicable methods:
  ((SYMBOL))
```

### Using [fuzzy-match].
同様の事例で、ユーザーが指定したオプションが（タイポなどにより）存在しないというものがあります。
そのような場合も上記同様にサポートされているオプションがエラーメッセージに含まれていると助かります。
ただし、オプションが多い場合、それらを網羅的に列挙するのはかえって不便です。
そのような場合[fuzzy-match]が便利に使えるかもしれません。

[fuzzy-match]の振る舞いは以下のようなものです。

```lisp
* (fuzzy-match:fuzzy-match "sato" '("tanaka" "saito" "yoshida" "watanabe"))
("saito" "watanabe" "tanaka" "yoshida")

* (fuzzy-match:fuzzy-match "abe" '("tanaka" "saito" "yoshida" "watanabe"))
("watanabe")
```

[`FORMAT`]は以下を参考にしてください。

```lisp
* (dolist (name '("sato" "abe"))
    (apply #'format t "Missing ~S. ~:@_Did you mean ~#[~;~S~;~S or ~S~:;~S, ~S or ~S~]?"
           name (fuzzy-match:fuzzy-match name '("tanaka" "saito" "yoshida" "watanabe")))
    (terpri))
Missing "sato". Did you mean "saito", "watanabe" or "tanaka"?
Missing "abe". Did you mean "watanabe"?
NIL
```

（自分自身を含む）ユーザーというものはわがままなもので、親切なエラーメッセージを表示してあげているとさらにつけあがって「そこまで分かっているならいい具合にしてくれよ」と思うものです。
[fuzzy-match]の候補が一つしかない場合は[restart]を提供してもいいかもしれません。

```lisp
(flet ((fail (name possibilities)
         (apply #'error "Missing ~S. ~:@_Did you mean ~#[~;~S~;~S or ~S~:;~S, ~S or ~S~]?"
                name possibilities)))
  (let ((possibilities (fuzzy-match:fuzzy-match name '("tanaka" "saito" "yoshida" "watanabe"))))
    (if (= 1 (length possibilities))
        (restart-case (fail name possibilities)
          (continue () :report (lambda (s) (format s "Use ~S" (car possibilities)))
            (car possibilities)))
        (fail name possibilities))))
```

## Adding error message with [`REINITIALIZE-INSTANCE`].
補足した[`ERROR`]が[`SIMPLE-ERROR`]のサブタイプである場合、エラーメッセージの拡張は[`REINITIALIZE-INSTANCE`]を使う方が便利な場合もあるかもしれません。
その場合は[`HANDLER-BIND`]で補足した[`CONDITION`]を再初期化します。

```lisp
(handler-bind ((simple-error (lambda (e)
                               (reinitialize-instance e
                                 :format-control (concatenate 'string
                                                              (simple-condition-format-control e)
                                                              "Additional message.")
                                 :format-arguments (append (simple-condition-format-arguments e)
                                                           '(additional args))))))
  (the-form-which-may-signal-simple-error))
```

# Conclusion.
エラーメッセージXから原因Yを推測せねばならない状況を筆者は特別に「XY-DEBUG」と名付けています。
開発中にふと「あれ、俺今XY-DEBUGをしたな？」と思った時はエラーハンドリングを見直すよう心がけています。

このようなエラーハンドリングはえてして面倒くさいものですが、自分が書いたエラーメッセージが自分で分かりやすいと思えて助けになったときは「やるじゃん俺」と思えてモチベーションを維持できます。
この精神的作用こそがエラーハンドリングに於いてもっとも重要な副作用かもしれません。

<!-- Links -->
[XY-PROBLEM]:https://en.wikipedia.org/wiki/XY_problem
[伽藍とバザール]:https://www.kinokuniya.co.jp/f/dsg-01-9784904807026
[Eric S. Raymond]:http://www.catb.org/esr/
[How To Ask Questions The Smart Way]:https://d.cxcore.net/Eric%20S%20Raymond/How%20To%20Ask%20Questions%20The%20Smart%20Way.pdf
[xyproblem.info]:https://xyproblem.info/
[`UNBOUND-VARIABLE`]:http://clhs.lisp.se/Body/e_unbo_1.htm
[`CONDITION`]:http://www.lispworks.com/documentation/HyperSpec/Body/e_cnd.htm
[`NO-APPLICABLE-METHOD`]:http://clhs.lisp.se/Body/f_no_app.htm
[`ERROR`]:http://clhs.lisp.se/Body/f_error.htm
[`HANDLER-BIND`]:http://clhs.lisp.se/Body/m_handle.htm
[`HANDLER-CASE`]:http://www.lispworks.com/documentation/HyperSpec/Body/m_hand_1.htm
[`DEFINE-SYMBOL-MACRO`]:http://clhs.lisp.se/Body/m_defi_1.htm
[`SYMBOL-MACROLET`]:http://www.lispworks.com/documentation/HyperSpec/Body/s_symbol.htm
[closer-mop]:https://github.com/pcostanza/closer-mop
[fuzzy-match]:https://github.com/vindarel/fuzzy-match
[`DESTRUCTURING-BIND`]:http://www.lispworks.com/documentation/HyperSpec/Body/m_destru.htm
[`REINITIALIZE-INSTANCE`]:http://www.lispworks.com/documentation/HyperSpec/Body/f_reinit.htm
[`SIMPLE-ERROR`]:http://www.lispworks.com/documentation/HyperSpec/Body/e_smp_tp.htm
[`FORMAT`]:http://www.lispworks.com/documentation/lw50/CLHS/Body/22_cgb.htm
[restart]:http://www.lispworks.com/documentation/HyperSpec/Body/09_adb.htm
