# Fight against cl-opengl 7.
## Metanotes
### 対象読者
[前章](clopengl6.html)読了済みの方。

## Introduction.
前回はElement Buffer Objectを抽象化しました。
今回はシェーダーへの不満点を解消していきます。

## Issues.
現在シェーダープログラムは文字列で持っています。

```lisp
(defparameter *vertex-shader*
  "#version 330 core
  in vec2 xy;
  void main () {
    gl_Position = vec4(xy, 0.0, 1.0);
  }")

(defparameter *fragment-shader*
  "#version 330 core
  out vec4 outColor;
  void main () {
    outColor = vec4(1.0, 1.0, 1.0, 1.0);
  }")
```

書いていて覚える不満点を列挙します。

* versionの指定が面倒。
* インプットの指定が面倒。
* アウトプットとインプットの変数名を合致させなければならないのが面倒。

これまで見てきたように僕達はいまや属性の管理を`VECTOR-CLASS`に投げています。
`VECTOR-CLASS`はvertex-shaderに何をインプットするべきか知っています。
なぜそれをわざわざ人間が指定しなければいけないのでしょうか。

vertex-shaderのアウトプットとfragment-shaderのインプットの変数は名前が合致しなければいけません。
名前さえ合致していればシェーダーは自由に組み合わせられるとみれば疎結合であると言えるかもしれません。
ですが名前を合致させなければならないととらえれば密結合であるとも言えます。

シェーダーが分割されているということはきっと自由に組み合わせたい需要があるのでしょう。
でも今の筆者にはその需要は見えません。

不満を覚えながらコーディングするより不満を解消し気持ちよくコーディングした後に「いやぁやっぱり分割されてあるべきだったわ。めんごめんご」となるほうが精神衛生がよろしゅうございます。

## DEFSHADER
### Syntax design.
こういうふうに書けると嬉しいというシンタックスのデッサンは以下のようなものとなります。

```lisp
(defshader hello-shader 330 (xy)
  (:vertex () "gl_Position = vec4(xy, 1.0, 1.0);")
  (:fragment ((|outColor| :vec4))
    "outColor = vec4(1.0, 1.0, 1.0, 1.0);"))
```

第一引数は定義されるシェーダーの名前です。
これはクラス名になります。

第二引数はversionです。

第三引数は属性を表す`VECTOR-CLASS`名のリストです。
このリストはそのままvertex-shaderのインプットとなります。

第四引数以降はシェーダー定義節です。

節の第一要素は定義されるシェーダー名です。

第二要素はシェーダーの出力リストです。
これはそのまま次のシェーダーの入力に使われます。

第三要素以降は`main`関数の中身になります。
現時点でローカル関数定義はできません。
[YAGNI](https://ja.wikipedia.org/wiki/YAGNI)の精神です。

### Implementation.
実装しましょう。

```lisp
(defgeneric vertex-shader (name)
  (:documentation "Accept class name, return its vertex shader code string."))

(defgeneric fragment-shader (name)
  (:documentation "Accept class name, return its fragment shader code string."))

(defmacro defshader (name version superclasses &body shader*)
  ;; Trivial syntax check.
  (check-type name symbol)
  (check-type version (or symbol integer))
  (assert (and (listp superclasses) (every #'find-class superclasses)))
  (assert (every (lambda (s) (find (car s) '(:vertex :fragment))) shader*))
  ;; binds
  (let ((format
         #.(concatenate 'string "#version ~A core~%" ; version
                        "~{in ~A ~A;~%~}~&" ; in
                        "~{out ~A ~A;~%~}~&" ; out
                        "~@[~{uniform ~A ~A;~%~}~]~&" ; uniforms
                        "void main () {~%~{~A~^~%~}~%}" ; the body.
                        )))
    (labels ((defs (list)
               (loop :for (name type) :in list
                     :collect (change-case:camel-case (symbol-name type))
                     :collect (change-case:camel-case (symbol-name name))))
             (rec (shaders in acc)
               (if (endp shaders)
                   (nreverse acc)
                   (body (car shaders) (cdr shaders) in acc)))
             (body (shader rest in acc)
               (destructuring-bind
                   (type out &rest main)
                   shader
                 (let* ((&uniform
                         (position-if
                           (lambda (x) (and (symbolp x) (string= '&uniform x)))
                           out))
                        (vars (and out (defs (subseq out 0 &uniform)))))
                   (rec rest `',vars
                        (cons
                          (let ((method
                                 (intern (format nil "~A-SHADER" type)
                                         :fude-gl)))
                            `(defmethod ,method ((type (eql ',name)))
                               ,(if (typep main
                                           '(cons
                                              (cons (eql quote)
                                                    (cons symbol null))
                                              null))
                                    `(,method ',(cadar main))
                                    `(format nil (formatter ,format) ',version
                                             ,in ',vars
                                             ',(and &uniform
                                                    (defs
                                                      (subseq out
                                                              (1+ &uniform))))
                                             ',main))))
                          acc))))))
      ;; The body.
      `(progn
        (defclass ,name ,superclasses () (:metaclass vector-class))
        ,@(rec shader*
               `(loop :for c :in (class-list (find-class type))
                      :for slots = (c2mop:class-direct-slots c)
                      :when slots
                        :collect (format nil "vec~D" (length slots))
                        :and :collect (change-case:camel-case
                                        (symbol-name (class-name c))))
               nil)))))
```

マクロの展開形を見てみましょう。

```lisp
* (macroexpand-1 '(defshader hello-rectangle 330 (xy)
                    (:vertex () "gl_Position = vec4(xy, 0.0, 1.0);")
                    (:fragment ((|outColor| :vec4))
                      "outColor = vec4(1.0, 1.0, 1.0, 1.0);")))

(progn
 (defclass hello-rectangle (xy) () (:metaclass vector-class))
 (defmethod vertex-shader ((type (eql 'hello-rectangle)))
   (format nil
           (formatter
            "#version ~A core~%~{in ~A ~A;~%~}~&~{out ~A ~A;~%~}~&~@[~{uniform ~A ~A;~%~}~]~&void main () {~%~{~A~^~%~}~%}")
           '330
           (loop :for c :in (class-list (find-class type))
                 :for slots = (sb-mop:class-direct-slots c)
                 :when slots
                   :collect (format nil "vec~D" (length slots))
                   :and :collect (change-case:camel-case
                                   (symbol-name (class-name c))))
           'nil 'nil '("gl_Position = vec4(xy, 0.0, 1.0);")))
 (defmethod fragment-shader ((type (eql 'hello-rectangle)))
   (format nil
           (formatter
            "#version ~A core~%~{in ~A ~A;~%~}~&~{out ~A ~A;~%~}~&~@[~{uniform ~A ~A;~%~}~]~&void main () {~%~{~A~^~%~}~%}")
           '330 'nil '("vec4" "outColor") 'nil
           '("outColor = vec4(1.0, 1.0, 1.0, 1.0);"))))
```

クラスが定義されシェーダーコードを返すメソッドが二つ定義されているのがわかります。


これで前回の四角形描画関数を書き直すと以下のようになります。

```lisp
(defshader hello-rectangle 330 (xy)
  (:vertex () "gl_Position = vec4(xy, 0.0, 1.0);")
  (:fragment ((|outColor| :vec4))
    "outColor = vec4(1.0, 1.0, 1.0, 1.0);"))

(defparameter *rectangle*
  (concatenate '(array single-float (*))
               (make-instance 'hello-rectangle :x -0.5 :y -0.5) ; Top left.
               (make-instance 'hello-rectangle :x 0.5 :y -0.5) ; Top right.
               (make-instance 'hello-rectangle :x -0.5 :y 0.5) ; Bottom left.
               (make-instance 'hello-rectangle :x 0.5 :y 0.5) ; Bottom right.
               ))

(defun hello-rectangle ()
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :flags '(:shown :opengl)
                           :x 100
                           :y 100
                           :w 800
                           :h 600)
      (sdl2:with-gl-context (context win)
        (with-vao ((vao (:vertices *rectangle*)
                        (:attributes 'hello-rectangle)
                        (:indices (coerce '(0 1 2 2 3 1) '(array (unsigned-byte 8) (*)))
                                  :target :element-array-buffer)
                        (:shader (vertex-shader 'hello-rectangle)
                                 (fragment-shader 'hello-rectangle))))
          (sdl2:with-event-loop (:method :poll)
            (:quit () t)
            (:idle ()
              (with-clear (win (:color-buffer-bit)
                               :color '(0.0 0.0 0.0 1.0))
                (%gl:draw-elements :triangles
                                   (length (indices-of vao))
                                   (foreign-type (array-element-type (indices-of vao)))
                                   0)))))))))
```

マクロ`WITH-VAO`の初期化フォームの中に繰り返し出てくる`HELLO-RECTANGLE`が鬱陶しいですね。
これもまとめてしまいましょう。

```lisp
(defmacro with-shader ((&rest bind*) &body body)
  `(with-vao ,(mapcar (lambda (bind)
                        (destructuring-bind (class &rest clause*) bind
                          `(,class ,@clause*
                                   (:attributes ',class)
                                   (:shader (vertex-shader ',class)
                                            (fragment-shader ',class)))))
                      bind*)
             ,@body))
```

これで以下のように書けるようになりました。

変数名を`DEFSHADER`で定義したクラス名と同名にしなくてはならないという制約を受け入れることでコンパクトにできます。
制約が不満なら下層の`WITH-VAO`を直接使えばいいだけです。
上にあるマクロ定義を見れば分かる通り`WITH-SHADER`マクロは大変薄いラッパでしかありません。

```lisp
(defun hello-rectangle ()
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :flags '(:shown :opengl)
                           :x 100
                           :y 100
                           :w 800
                           :h 600)
      (sdl2:with-gl-context (context win)
        (with-shader ((hello-rectangle (:vertices *rectangle*)
                                       (:indices (coerce '(0 1 2 2 3 1)
                                                         '(array (unsigned-byte 8) (*)))
                                                 :target :element-array-buffer)))
          (sdl2:with-event-loop (:method :poll)
            (:quit () t)
            (:idle ()
              (with-clear (win (:color-buffer-bit)
                               :color '(0.0 0.0 0.0 1.0))
                (%gl:draw-elements :triangles
                                   (length (indices-of hello-rectangle))
                                   (foreign-type (array-element-type
                                                   (indices-of hello-rectangle)))
                                   0)))))))))
```
