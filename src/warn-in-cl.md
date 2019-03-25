# warnの出力抑制はどうやって実現しているのか
## Introduction
[このような記事](http://nptcl.hatenablog.com/entry/2019/03/24/101432)を目にしたのですが、いろいろと 勘違いなさっているご様子なので僕自身の言葉で解説をしてみようと思う。

## Meta info
### 対象読者

* Common Lisp に於いて警告の出力抑制がどのように実現されているか興味のあるかた。

初心者CLerは学習に、中級CLerは復習に、ベテランCLerと非CLerはひやかしに見ていただければ幸い。

## Three features of condition system.
Common Lispのコンディションシステムは大別して３つのパーツからなる。
各々「シグナラ」、「ハンドラ」、「リスタート」である。
`CL:WARN`はシグナラとリスタートの上に実装されている。
そしてコンディションがシグナルされたときにどのように制御するかを担っているのがハンドラである。

## Typical implementation of CL:SIGNAL
最も原始的なシグナラ関数は`CL:SIGNAL`である。
`CL:SIGNAL`の仕事は以下の４つである。

1. コンディションオブジェクトを作る。
2. コンディションが`*BREAK-ON-SIGNALS*`に登録されているならデバッガに入る。
3. 環境オブジェクトに問い合わせ、対応ハンドラを片っ端からコールして回る。
4. `NIL`を返りす。

典型的な実装は以下のようなものになるだろう。
なお、環境オブジェクトは言語仕様に含まれておらず処理系依存となる。
ここでは簡便のためにスペシャル変数に連想リストで実装することとする。

```lisp
(defvar *handlers* nil)

(defun %signal(datum &rest args)
  (let*((condition(etypecase datum
                    (condition datum)
                    (string (make-condition 'simple-condition
                                            :format-control datum
                                            :format-arguments args))
                    (symbol (apply #'make-condition datum args)))))
    (if(find condition *break-on-signals* :test #'typep)
      (invoke-debugger condition)
      (dolist(handler *handlers*)
        (when(typep condition (car handler))
          (funcall (cadr handler)
                   condition))))))

;; REPL
* (%signal 'error)
NIL
```

## Simple implementation of CL:HANDLER-BIND
ハンドラをコンディションに紐づけて登録する責はマクロ`CL:HANDLER-BIND`が担う。
その仕事は主に以下の2つである。

1. コンディション名とハンドラ関数を紐付ける形で環境オブジェクトを拡張する。
2. 本体を評価する。

効率度外視でいい加減に実装するなら`HANDLER-BIND`は以下のようなものになるかもしれない。

```lisp
(defmacro %handler-bind(handlers &rest body)
  `(call-with-handlers (lambda(),@body)
                       (list ,@(nreverse (mapcar (lambda(handler)
                                                   `(list ',(car handler)
                                                          ,(cadr handler)))
                                                 handlers)))))

(defun call-with-handlers(body handlers)
  (labels((rec(handlers &optional(*handlers* *handlers*))
            (if(endp handlers)
              (funcall body)
              (rec (cdr handlers)(cons (car handlers)
                                       *handlers*)))))
    (rec handlers)))

;; REPL
* (%handler-bind((warning #'print))
    (%signal 'warning))

#<WARNING {*******}>  ; <--- side effect of print.
NIL                   ; <--- return value of %signal.
```

上記コード例では`SIGNAL`が`WARNING`を発する。
環境には`HANDLER-BIND`により`WARNING`にハンドラが束縛されている。
この場合ハンドラは`PRINT`。
よって`PRINT`にコンディションオブジェクトを渡す形で呼び出している。
`PRINT`関数はプログラムフローを制御しないので`SIGNAL`はその処理を終え`NIL`を返す。

プログラムフローを制御する場合は、例えば以下のようになる。

```lisp
;; RPEL
* (block()
    (%handler-bind((warning (lambda(c)
                              (declare(ignore c))
                              (return 1))))
      (%signal 'warning)))
1
```

## Typical implementation of CL:WARN.
`CL:WARN`の仕事は以下の5つである。

1. コンディションオブジェクトを作る。
2. リスタートのためのコンテクストを構築する。
3. シグナルを発する。
4. 警告文を出力する。
5. `NIL`を返す。

実装は以下のようなものになるだろう。

```lisp
(defun %warn (datum &rest args)
  (let((condition(etypecase datum
                   (condition datum)
                   (string (make-condition 'simple-warning
                                           :format-control datum
                                           :format-arguments args))
                   (symbol (apply #'make-condition datum args)))))
    (restart-case(progn (%signal condition)
                        (format *error-output* "~&;; WARNING: ~A~%" condition))
      (muffle-warning()))))

;; REPL
* (%warn "test")
;; WARNING: test                  ; <--- side effect
NIL                               ; <--- return value of %warn

* (%handler-bind((warning (lambda(c)
                            (print c *error-output*)
                            (force-output *error-output*))))
    (%warn "test"))

#<SIMPLE-WARNING "test" {******}> ; <--- side effect of handler.
;; WARNING: test                  ; <--- side effect of %warn.
NIL                               ; <--- return value of %warn.

* (%handler-bind((warning #'muffle-warning))
    (%warn "test"))
NIL                               ; <--- no side effect.
```

元記事の作者さんが勘違いしているらしいことの一つは、出力はハンドラで行うという点だ。
（仕様をちゃんと把握した上で何らかの都合で敢えてこのような実装にしてあるのかどうか文面からは読み取れなかった。）
`CL:WARN`は警告を行うが、元記事の`WARN!`は警告を行わない。

```lisp
* (cl:warn "test")
WARNING: test       ; <--- side effect.
NIL                 ; <--- return value.

* (warn! "test")
NIL                 ; <--- no side effect, just return NIL.
```

`CL:WARN`が行う出力は`CL:WARN`自身が提供するリスタートにより抑制される。
リスタートの名前は`muffle-warning`といい、同名の関数も提供されている。

すなわち警告出力の抑制を行っているのはリスタート機構である。

## Restart
リスタートとは誤解を恐れずに大胆に言ってしまうなら、デバッガへのオプション指定機能であり、デバッガからの復帰エントリポイントの提供機構である。

もっともシンプルな使い方は以下のようなものとなる。

```lisp
;; REPL (case in SBCL)
* (restart-case(error "error")
    (test():test))

debugger invoked on a SIMPLE-ERROR in thread
#<THREAD "main thread" RUNNING {982EF329}>:
  error

Type HELP for debugger help, or (SB-EXT:EXIT) to exit from SBCL.

restarts (invokable by number or by possibly-abbreviated name):
  0: [TEST ] TEST
  1: [ABORT] Exit debugger, returning to top level.

((LAMBDA ()))
   source: (SB-KERNEL:WITH-SIMPLE-CONDITION-RESTARTS 'ERROR NIL "error")
0] 0
:TEST
*
```

上記コード例では、関数`CL:ERROR`によりデバッガが起動され、デバッガにより動的なリスタートオプションが明示され、ユーザがオプションを指定することでエントリポイントに復帰している。

このオプションはユーザからの指定のみならず、プログラムから自動的に指定することもできる。
そのためにはハンドラを利用する。

```lisp
;; REPL
* (handler-bind((error (lambda(c)
                         (let((restart(find-restart 'test c)))
                           (when restart
                             (invoke-restart restart))))))
    (restart-case(error "test")
      (test () :test)))
:TEST
```

上記例ではハンドラがリスタートをinvokeすることによりユーザの手を煩わせることなく自動的にリスタートを選択してみせている。

警告の抑制もこれにより行われている。
では肝心のリスタートの実装がどうなっているのか見ていこう。

## Simple implementation of restart system.
### restart object
リスタートオブジェクトの実装については処理系依存となっている。
言語仕様により唯一要求されていることは`CL:RESTART-NAME`が名前（シンボル）を返さなければならないということのみだ。

ここでは簡便のためにサブセットを実装することとする。

```lisp
(defstruct %restart name function)
```

### restart-bind
リスタートの登録はマクロ`CL:RESTART-BIND`が責を担う。
環境オブジェクトは処理系依存となるので、ここでは`HANDLER-BIND`と同様にスペシャル変数を用意して対応することとする。

```lisp
(defvar *restarts* nil)

(defmacro %restart-bind(restarts &body body)
  `(call-with-restarts (lambda(),@body)
                       (list ,@(mapcar (lambda(restart)
                                         `(make-%restart :name ',(car restart)
                                                         :function ,(cadr restart)))
                                       (reverse restarts)))))

(defun call-with-restarts(body restarts)
  (labels((rec(restarts &optional (*restarts* *restarts*))
            (if(endp restarts)
              (funcall body)
              (rec (cdr restarts) (cons (car restarts) *restarts*)))))
    (rec restarts)))
```

### restart-case
`RESTART-BIND`はリスタートの登録を行うだけで、エントリポイントの提供は行わない。
どういう意味かというと、デバッガから呼ばれてもデバッガに留まり続けるという意味である。

```lisp
;; REPL
* (restart-bind((test(lambda()
                        (print :test *debug-io*)
                        (force-output *debug-io*))))
    (invoke-debugger (make-condition 'error)))

1: [TEST ] TEST
0: [ABORT] Exit debugger, returning to top level.

0] 0

:TEST
0] 0

:TEST
0] 1
*
```

エントリポイントの提供は`CL:RESTART-CASE`が担う。

```lisp
(defmacro %restart-case(form &rest clauses)
  (let((tag(gensym "RESTART-BLOCK")))
    `(block ,tag
            (tagbody
              (%restart-bind,(mapcar(lambda(clause)
                                      `(,(car clause)
                                         (lambda()(go ,(car clause)))))
                               clauses)
                (return-from ,tag ,form))
              ,@(mapcan (lambda(clause)
                          `(,(car clause)(return-from ,tag
                                                      (funcall (lambda,@(cdr clause))))))
                        clauses)))))
```

### find-restart
リスタートを環境から探してくるのは`CL:FIND-RESTART`の責である。

```lisp
(defun %find-restart(name condition)
  (declare(ignore condition)) ; because this is subset.
  (find name *restarts* :key #'%restart-name))
```

### invoke-restart
リスタート時の処理を呼び出すのは`CL:INVOKE-RESTART`の責である。

本来ならユーザからの入力を受け付ける`CL:INVOKE-RESTART-INTERACTIVELY`もあるのだが、ここでは簡便のためサポートしない。

```lisp
(defun %invoke-restart(restart)
  (funcall (%restart-function restart)))
```

### compute-restarts
現在有効なリスタートをリストアップするのは`CL:COMPUTE-RESTARTS`の責である。

```lisp
(defun %compute-restarts(&optional condition)
  (declare(ignore condition)) ; because this is subset.
  *restarts*)
```

### invoke-debugger
デバッガを呼び出すのは`CL:INVOKE-DEBUGGER`の責である。

```lisp
(defun %abort(condition)
  (let((restart(%find-restart '%abort condition)))
    (if restart
      (%invoke-restart restart)
      (error 'program-error))))

(defun %invoke-debugger(condition)
  (%restart-case
    (let((restarts (%compute-restarts condition)))
      (loop :for i :upfrom 0
            :for restart :in restarts
            :do (format *debug-io* "~&~D: [~A]" i (%restart-name restart)))
      (loop (format *debug-io* "~%> ")
            (force-output *debug-io*)
            (print (debugger-eval (read *debug-io*)
                                  restarts)
                   *debug-io*)
            (force-output *debug-io*)))
    (%abort()(values))))

(defun debugger-eval(form restarts)
  (let*((value(eval form))
        (restart(and (typep value '(integer 0 *))
                     (nth value restarts))))
    (if restart
      (%invoke-restart restart)
      value)))
```

これでリスタート機構は完成である。
このリスタート機構に合わせた`MUFFLE-WARNING`も作って、`WARN`もそれに合わせて修正しよう。

```lisp
(defun %muffle-warning(condition)
  (let((restart(%find-restart '%muffle-warning condition)))
    (if restart
      (%invoke-restart restart)
      (error 'program-error))))

(defun %warn (datum &rest args)
  (let((condition(etypecase datum
                   (condition datum)
                   (string (make-condition 'simple-warning
                                           :format-control datum
                                           :format-arguments args))
                   (symbol (apply #'make-condition datum args)))))
    (%restart-case(progn (%signal condition)
                         (format *error-output* "~&;; WARNING: ~A~%" condition))
      (%muffle-warning()))))

;; REPL
* (%warn "test")
;; WARNING: test
NIL

* (%handler-bind((warning #'%muffle-warning))
    (%warn "test"))
NIL
```

## Conclusion
駆け足で見てきたが、これがCommon Lispにおいて警告の出力を抑制する機能のあらましである。
キモはリスタート機構であることが分かるかと思う。
リスタート機構は初心者にとっては謎の機能なのだが、見てきたとおり、サブセットだからなおのことだが、さほど大きくもなく複雑でもない。

ある程度実践的なものを作ろうとすると、コンディションシステムは使い倒すことになるのだが、解説は乏しい。
翻訳されている書籍のなかでは唯一実践Common Lispのみがコンディションシステムについて詳しく触れてくれている。
あとはCLtL2くらいしか無い。
本稿がCommon Lispコンディションシステムについての理解の一助となれば幸い。
