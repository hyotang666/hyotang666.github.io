# Fight against cl-opengl 21.
## Metanotes
### 対象読者
[前章](clopengl20.html)読了済みの方。

## Introduction
前章ではテクストの描画が可能になりました。
本章では再度マクロの改築を行います。

## Issues.
コードの規模が肥大してきデバッグが辛くなってまいりました。

原因は幾つかあります。

### Using unsigned integer as name.
まずopenGLに依頼してオブジェクトを作ってもらった場合その名前を返してもらうのですがそれは名前とは名ばかりで実態は整数です。

`texture`を期待しているところに`buffer`がやってきたという場合OpenGLは`invalid-operation`というエラーを投げるのですが文脈（ここでは`texture`を期待しているところに`buffer`がやってきたということ）は剥落してしまっております。

これではデバッグは困難で仮に名前（整数）が表示されても人間にはそれが何を表しているのか分かりづろうございます。

また例えば現在束縛されている`buffer`を問い合わせることは可能ですがその場合も返り値は名前（整数）です。
仮に`1`が返ってきたとしてその数を見て自身のコードのどの`buffer`を表しているのか察するのは困難です。

### Less informations.
不正な値が来たというのはエラーメッセージとして片手落ちで有効な値のリストも同時に示してもらいたいところです。
ところが現在登録されている全`buffer`の名前を取り出すことはできないぽいです。
（有識者の方いらっしゃいましたらご教授願います。）

## Design.
### GL-OBJECT
上記の問題を解決するために`GL-OBJECT`を導入しましょう。

OpenGLから返ってきた生の名前（整数）をそのまま扱うのではなく`GL-OBJECT`にラップして取り扱うのです。
これにより文脈の保持が可能となります。
また然るべき型宣言を行うことでLispコンパイル時に型不整合を検出できるようになります。

### Design.
設計はCondition systemとPackage systemとを足して割ったものを目指します。
より具体的には`CL:HANDLER-BIND`と`CL:IN-PACKAGE`を組み合わせたようなものです。

#### HANDLER-BIND
`CL:HANDLER-BIND`はエラーハンドリングのための関数束縛をLisp環境を拡張するかたちで構築します。

```lisp
(handler-bind ((error (lambda (c) (print c))))
  ...)
```

上記コード例では`ERROR`というコンディションに無名関数を紐付けるかたちでLisp環境を拡張しています。
関数`SIGNAL`がコンディションを発すると動的な現在のLisp環境を内側から外側に向かい検索していき発されたコンディションに紐付けられた全てのハンドラ関数を`FUNCALL`していきます。

#### IN-PACKAGE
関数`MAKE-PACKAGE`で新しいパッケージを作れます。
新しく作られたパッケージはLispが背後のデータベースに登録して管理されます。
現在有効なパッケージは変数`*PACKAGE*`に束縛されておりその値を変更するには`IN-PACKAGE`する必要があります。

#### GL-OBJECT
OpenGLに依頼して作ってもらうオブジェクトはOpenGLが管理するという意味でパッケージ的です。
OpenGLは状態マシンであり現在有効なオブジェクトは例えば`glBindBuffer`関数などで指定する必要がありますが、それはちょうどパッケージシステムにおける`IN-PACKAGE`に対応しているとみなせます。

またOpenGLが管理するオブジェクト（リソース）は終了時に開放する必要があります。
そのような後処理はマクロでくるみ隠蔽してきました。
OpenGL環境が動的に拡張されるという意味において僕達のWITH系マクロは`HANDLER-BIND`的であると言えます。

#### Conclusion.
これらを組み合わせてOpenGLリソースの管理をLisp側でも行うようにコードを改築していくとします。

ひとまず必要なのは`GL-OBJECT`型です。

```lisp
(defstruct gl-object
  (name (alexandria:required-argument :name)
        :type (or character symbol)
        :read-only t)
  (id (alexandria:required-argument :id) :type (unsigned-byte 32) :read-only t))
```

## BUFFER
### BUFFER
`buffer`を管理するための`BUFFER`オブジェクトを`GL-OBJECT`を継承して作ります。

```lisp
(defstruct (buffer (:include gl-object))
  (target :array-buffer :type buffer-target :read-only t)
  (usage :static-draw :type buffer-usage :read-only t))
```

### \*BUFFERS\*
宣言された全`BUFFER`を格納するデータベースです。

```lisp
(defvar *buffers* nil "Dynamic buffer environment.")
```

### \*BUFFER\*
現在有効な`BUFFER`を格納する変数です。

```lisp
(defvar *buffer* :uninitizlied-buffer "Current buffer.")
```

### FIND-BUFFER
`BUFFER`指示子を受け取り環境に問い合わせる関数です。

```lisp
(defun find-buffer (thing)
  (etypecase thing
    (buffer thing)
    (symbol
     (or (find thing *buffers* :key #'buffer-name)
         (error "Missing buffer named ~S. ~S" thing *buffers*)))))
```

### IN-BUFFER
OpenGLの状態を変更しLisp環境も同様に変更するマクロです。
ぶっちゃけマクロである必要は皆無ですが`IN-PACKAGE`がマクロなのでそれに倣いました。
引数は評価されるのでかえって混乱を生むかもしれません。
`USE-xxx`という関数にしたほうがいいかもしれません。

```lisp
(defmacro in-buffer (form)
  (let ((buffer (gensym "BUFFER")))
    `(let ((,buffer (find-buffer ,form)))
       (gl:bind-buffer (buffer-target ,buffer) (buffer-id ,buffer))
       (setf *buffer* ,buffer))))
```

### WITH-BUFFER
`WITH-BUFFER`マクロは以下の通りに改築されます。
全ての変数は`BUFFER`に束縛され環境が拡張されます。

```lisp
(defmacro with-buffer (&whole whole (&rest bind*) &body body)
  (check-bnf:check-bnf (:whole whole)
    ((bind* (var buffer-option*))
     (buffer-option* option-key keyword)
     (option-key (member :target :usage))
     (var symbol)))
  `(destructuring-bind
       ,(mapcar #'car bind*) ; <--- All variables are
       (mapcar ; <--- bound by
         (lambda (bind id)
           (destructuring-bind
               (name &key (target :array-buffer) (usage :static-draw))
               bind
             (make-buffer :id id :name name :target target :usage usage))) ; <--- Buffer object!
         ',bind* (gl:gen-buffers ,(length bind*)))
     (unwind-protect
         (let ((*buffer* *buffer*) ; <--- Current buffer!
               (*buffers* (list* ,@(mapcar #'car bind*) *buffers*))) ; <--- Extend environment!
           ,@body)
       (gl:delete-buffers ; <--- Cleanup!
         (list ,@(mapcar (lambda (bind) `(buffer-id ,(car bind))) bind*))))))
```

## OTHERS
上記と同様の変更を`WITH-PROG`、`WITH-VERTEX-ARRAY`、`WITH-TEXTURES`にも施します。
またそれに合わせて各種関数も変更します。

コードを以下に貼り付けておきます。
本章は以上となります。

```lisp
;;; WITH-PROG

(defvar *progs* nil)

(defvar *prog* :uninitialized-program)

(defstruct (program (:include gl-object)))

(defun find-program (thing)
  (etypecase thing
    (program thing)
    (symbol
     (or (find thing *progs* :key #'program-name)
         (error "Missing program named ~S: ~S" thing *progs*)))))

(defmacro in-shader (form)
  (let ((program (gensym "PROGRAM")))
    `(let ((,program (find-program ,form)))
       (gl:use-program (program-id ,program))
       (setf *prog* ,program))))

(defmacro with-prog (&whole whole (&rest bind*) &body body)
  (check-bnf:check-bnf (:whole whole)
    ((bind* (symbol check-bnf:expression check-bnf:expression))))
  (alexandria:with-unique-names (compile warn vs fs)
    `(let* ((*prog* *prog*)
            ,@(loop :for (name) :in bind*
                    :collect `(,name
                               (make-program :name ',name
                                             :id (gl:create-program))))
            (*progs* (list* ,@(mapcar #'car bind*) *progs*)))
       (unwind-protect
           (progn
            ,@(loop :for (var vertex-shader fragment-shader) :in bind*
                    :collect `(let ((,vs (gl:create-shader :vertex-shader))
                                    (,fs (gl:create-shader :fragment-shader)))
                                (unwind-protect
                                    (labels ((,compile (prog id source)
                                               (gl:shader-source id source)
                                               (gl:compile-shader id)
                                               (,warn
                                                (gl:get-shader-info-log id))
                                               (gl:attach-shader
                                                 (program-id prog) id))
                                             (,warn (log)
                                               (unless (equal "" log)
                                                 (warn log))))
                                      (,compile ,var ,vs ,vertex-shader)
                                      (,compile ,var ,fs ,fragment-shader)
                                      (gl:link-program (program-id ,var))
                                      (,warn
                                       (gl:get-program-info-log
                                         (program-id ,var))))
                                  (gl:delete-shader ,fs)
                                  (gl:delete-shader ,vs))))
            ,@body)
         ,@(mapcar
             (lambda (bind) `(gl:delete-program (program-id ,(car bind))))
             bind*)))))

;;; LINK-ATTRIBUTES

(defun get-attrib-location (program class)
  (let* ((name (change-case:camel-case (symbol-name (class-name class))))
         (loc (gl:get-attrib-location (program-id program) name)))
    (if (minusp loc)
        (error "Not active attribute name ~S in ~S." name program)
        loc)))

(defun link-attributes (class program)
  (labels ((rec (class-list total-length funs)
             (if (endp class-list)
                 (let ((total (apply #'+ total-length)))
                   (loop :for f :in funs
                         :for l :in total-length
                         :do (funcall f total l offset)
                         :sum l :into offset))
                 (let ((slots
                        (length (c2mop:class-direct-slots (car class-list)))))
                   (if (zerop slots)
                       (rec (cdr class-list) total-length funs)
                       (rec (cdr class-list)
                            (cons (the (integer 1 4) slots) total-length)
                            (cons (processer (car class-list)) funs))))))
           (processer (class)
             (lambda (total-length length offset)
               (let* ((location (get-attrib-location program class))
                      (slots
                       (c2mop:class-direct-slots
                         (c2mop:ensure-finalized class)))
                      (type
                       (ecase (c2mop:slot-definition-type (car slots))
                         (single-float :float)))
                      (size (cffi:foreign-type-size type)))
                 #++
                 (uiop:format! *trace-output* "~%Length ~S. Offset ~S."
                               (* total-length size) (* offset size))
                 (gl:vertex-attrib-pointer location length type nil ; As
                                                                    ; normalized-p
                                           (* total-length size)
                                           (* offset size))
                 (gl:enable-vertex-attrib-array location)))))
    (rec (class-list (find-class class)) nil nil)))

;;; WITH-VERTEX-ARRAY

(defstruct (vertex-array (:include gl-object)))

(defvar *vertex-arrays* nil)

(defvar *vertex-array* :uninitialzied-vertex-array)

(defun find-vertex-array (thing)
  (etypecase thing
    (vertex-array thing)
    (symbol
     (or (find thing *vertex-arrays* :key #'vertex-array-name)
         (error "Missing vertex-array named ~S. ~S" thing *vertex-arrays*)))))

(defmacro in-vertex-array (form)
  (let ((vao (gensym "VERTEX-ARRAY")))
    `(let ((,vao (find-vertex-array ,form)))
       (gl:bind-vertex-array (vertex-array-id ,vao))
       (setf *vertex-array* ,vao))))

(defmacro with-vertex-array (&whole whole (&rest bind*) &body body)
  (check-bnf:check-bnf (:whole whole)
    ((bind* (symbol init-form+))
     (init-form+ check-bnf:expression)))
  `(let* ((*vertex-array* *vertex-array*)
          ,@(loop :for (name) :in bind*
                  :collect `(,name
                             (make-vertex-array :name ',name
                                                :id (gl:gen-vertex-array))))
          (*vertex-arrays* (list* ,@(mapcar #'car bind*) *vertex-arrays*)))
     (unwind-protect
         (progn
          ,@(mapcan
              (lambda (bind) `((in-vertex-array ',(car bind)) ,@(cdr bind)))
              bind*)
          ,@body)
       (gl:delete-vertex-arrays
         (list
           ,@(mapcar (lambda (bind) `(vertex-array-id ,(car bind))) bind*))))))

;;; WITH-TEXTURES

(defstruct (texture (:include gl-object))
  (target (alexandria:required-argument :target)
          :type texture-target
          :read-only t))

(defvar *textures* nil)

(defvar *texture* :uninitialized-texture)

(defun find-texture (thing)
  (etypecase thing
    (texture thing)
    ((or character symbol)
     (or (find thing *textures* :key #'texture-name)
         (error "Missing texture named ~S. ~S" thing *textures*)))))

(defmacro in-texture (form)
  (let ((texture (gensym "TEXTURE")))
    `(let ((,texture (find-texture ,form)))
       (gl:active-texture (texture-id ,texture))
       (gl:bind-texture (texture-target ,texture) (texture-id ,texture))
       (setf *texture* ,texture))))

(defmacro with-textures ((&rest bind*) &body body)
  "Each VAR is bound by openGL texture id."
  ;; Trivial syntax check.
  (dolist (b bind*) (the (cons symbol (cons texture-target *)) b))
  (labels ((vname (k v)
             (case k
               ((:texture-wrap-s :texture-wrap-t :texture-wrap-r)
                (type-assert v 'texture-wrapping))
               ((:texture-mag-filter) (type-assert v 'texture-mag-filter))
               ((:texture-min-fileter) (type-assert v 'texture-min-filter))
               (otherwise v)))
           (<option-setters> (params target)
             (destructuring-bind
                 (&key (texture-wrap-s :repeat) (texture-wrap-t :repeat)
                  (texture-min-filter :linear) (texture-mag-filter :linear)
                  &allow-other-keys)
                 params
               (let ((params
                      (list* :texture-wrap-s texture-wrap-s :texture-wrap-t
                             texture-wrap-t :texture-mag-filter
                             texture-mag-filter :texture-min-filter
                             texture-min-filter
                             (uiop:remove-plist-keys
                               '(:texture-wrap-s :texture-wrap-t
                                 :texture-min-filter :texture-mag-filter)
                               params))))
                 (loop :for (k v) :on params :by #'cddr
                       :collect `(gl:tex-parameter ,target
                                                   ,(type-assert k
                                                                 'texture-pname)
                                                   ,(vname k v)))))))
    ;; The body.
    `(destructuring-bind
         ,(mapcar #'car bind*)
         (loop :for (name target) :in ',bind*
               :for id :in (gl:gen-textures ,(length bind*))
               :collect (make-texture :id id :name name :target target))
       (unwind-protect
           (let ((*texture* *texture*)
                 (*textures* (list* ,@(mapcar #'car bind*) *textures*)))
             ,@(mapcan
                 (lambda (b)
                   (destructuring-bind
                       (var target &key params init)
                       b
                     `((in-texture ',var) ,@(<option-setters> params target)
                       ,@(when init
                           `(,init)))))
                 bind*)
             ,@body)
         (gl:delete-textures
           (list
             ,@(mapcar (lambda (bind) `(texture-id ,(car bind))) bind*)))))))

;;;; WITH-VAO

(defmacro indices-of (id)
  (declare (ignore id))
  (error "INDICE-OF is must be inside of WITH-VAO."))

(defun get-uniform-location (program name)
  (let ((location (gl:get-uniform-location (program-id program) name)))
    (assert (not (minusp location)) ()
      "Uniform ~S is not active in program ~S." name program)
    location))

(defun <uniform-binder> (prog)
  (lambda (uniform)
    (etypecase uniform
      (symbol
       `(,uniform
         (get-uniform-location ,prog
                               ,(change-case:camel-case
                                  (symbol-name uniform)))))
      ((cons symbol (cons symbol null))
       `(,(first uniform)
         (get-uniform-location ,prog
                               ,(change-case:camel-case
                                  (symbol-name (second uniform)))))))))

(defun ensure-second (thing)
  (if (listp thing)
      (second thing)
      thing))

(defun <init-buffer> (buf vec)
  `((in-buffer ',buf)
    (gl:buffer-data (buffer-target ,buf) (buffer-usage ,buf) ,vec)))

(defun prog-name (prog bind*)
  (or (and (symbol-package prog) prog) (caar bind*)))

(defun uniform-bind (bind* prog)
  (let* ((uniforms (cdr (assoc :uniform (cdar bind*))))
         (required (uniforms (prog-name prog bind*)))
         (actual (mapcar #'ensure-second uniforms)))
    (assert (null (set-exclusive-or required actual :test #'string=)) ()
      "Mismatch uniforms. ~S but ~S" required actual)
    (mapcar (<uniform-binder> prog) uniforms)))

(defun parse-with-vao-binds (bind* body)
  (let ((refs))
    (labels ((clause (clause bind)
               (or (assoc clause (cdr bind))
                   (error "Missing required cluase ~S in ~S" clause bind)))
             (rec (bind*)
               (if (endp bind*)
                   body
                   (destructuring-bind
                       (prog vs fs)
                       (cdr (clause :shader (car bind*)))
                     (unless prog
                       (setf prog (gensym "PROG")))
                     `((with-prog ((,prog ,vs ,fs))
                         ,(body (assoc :indices (cdar bind*)) prog bind*))))))
             (<may-uniform-bind> (uniforms bind*)
               (if uniforms
                   `((let ,uniforms
                       ,@(rec (cdr bind*))))
                   (rec (cdr bind*))))
             (<body-form> (bind* prog &optional indices-bind ebo-bind ebo-inits)
               (let* ((verts (clause :vertices (car bind*)))
                      (vertices (or (second verts) (gensym "VERTICES")))
                      (vbo
                       `(,(or (cadr (assoc :buffer (cdar bind*)))
                              (gensym "VBO"))
                         ,@(cdddr (assoc :vertices (cdar bind*)))))
                      (uniforms (uniform-bind bind* prog))
                      (attr (second (clause :attributes (car bind*)))))
                 `(with-gl-vector ((,vertices ,(third verts)) ,@indices-bind)
                    (with-buffer ,(list* vbo ebo-bind)
                      (with-vertex-array ((,(caar bind*)
                                           ,@(<init-buffer> (car vbo) vertices)
                                           (in-shader ',prog)
                                           (link-attributes ,attr ,prog)
                                           ,@ebo-inits))
                        ,@(<may-uniform-bind> uniforms bind*))))))
             (body (vec prog bind*)
               (if vec
                   (alexandria:with-unique-names (vector indices ebo)
                     `(let ((,vector ,(second vec)))
                        ,(progn
                          (push (list (prog-name prog bind*) `',vector) refs)
                          (<body-form> bind* prog `((,indices ,vector))
                                       `((,ebo
                                          ,@(uiop:remove-plist-key :size (cddr
                                                                           vec))))
                                       (<init-buffer> ebo indices)))))
                   (<body-form> bind* prog))))
      (values (rec bind*) refs))))

;;;; FONT

(defstruct char-glyph
  (texture (alexandria:required-argument :texture) :type texture :read-only t)
  w
  h
  bearing-x
  bearing-y
  advance)

(defmacro with-glyph (() &body body)
  `(let ((*fonts* (alexandria:copy-hash-table *fonts*))
         (*glyphs* (make-hash-table)))
     (unwind-protect (progn ,@body)
       (loop :for g :being :each :hash-value of *glyphs*
             :collect (texture-id (char-glyph-texture g)) :into textures
             :finally (gl:delete-textures textures))
       (loop :for v :being :each :hash-value of *fonts*
             :when (typep v 'zpb-ttf::font-loader)
               :do (zpb-ttf::close-font-loader v)))))

(defun char-glyph (char font-name &optional (size 16))
  (let ((loader (font-loader font-name)))
    (if (not (zpb-ttf:glyph-exists-p char loader))
        (error "~S is not exist in the font ~S." char font-name)
        (or (gethash char *glyphs*)
            (multiple-value-bind (image w h bearing-x bearing-y advance)
                (font-data char loader size)
              (gl:pixel-store :unpack-alignment 1)
              (let ((texture
                     (make-texture :id (car (gl:gen-textures 1))
                                   :name char
                                   :target :texture-2d)))
                (in-texture texture)
                (gl:tex-image-2d :texture-2d 0 :red w h 0 :red
                                 :unsigned-byte image)
                (gl:tex-parameter :texture-2d :texture-wrap-s :clamp-to-edge)
                (gl:tex-parameter :texture-2d :texture-wrap-t :clamp-to-edge)
                (gl:tex-parameter :texture-2d :texture-min-filter :linear)
                (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
                (setf (gethash char *glyphs*)
                        (make-char-glyph :texture texture
                                         :w w
                                         :h h
                                         :bearing-x bearing-x
                                         :bearing-y bearing-y
                                         :advance advance))))))))

(defun render-text
       (text shader
        &key (x 0) (y 0) (scale 1) (color '(1 1 1)) (font "Ubuntu-M")
        (vertices (error ":VERTICES is required."))
        (color-uniform (error ":COLOR-UNIFORM is required."))
        ((:vertex-array vao) (error ":VERTEX-ARRAY is required."))
        ((:vertex-buffer vbo) (error ":VERTEX-BUFFER is required.")))
  (setf text (map 'list (lambda (c) (char-glyph c font)) text))
  (in-shader shader)
  (apply #'gl:uniformf color-uniform color)
  (gl:active-texture 0)
  (in-vertex-array vao)
  (loop :for glyph :in text
        :for x-pos = (+ x (* (char-glyph-bearing-x glyph) scale))
        :for y-pos
             = (- y
                  (* (- (char-glyph-h glyph) (char-glyph-bearing-y glyph))
                     scale))
        :for w = (* scale (char-glyph-w glyph))
        :for h = (* scale (char-glyph-h glyph))
        :do (loop :for elt
                       :in (list x-pos (+ h y-pos) 0 0 ; first
                                 x-pos y-pos 0 1 ; second
                                 (+ w x-pos) y-pos 1 1 ; third
                                 x-pos (+ h y-pos) 0 0 ; fourth
                                 (+ w x-pos) y-pos 1 1 ; fifth
                                 (+ w x-pos) (+ h y-pos) 1 0)
                  :for i :upfrom 0
                  :do (setf (gl:glaref vertices i) (float elt)))
            (gl:bind-texture (texture-target (char-glyph-texture glyph))
                             (texture-id (char-glyph-texture glyph)))
            (in-buffer vbo)
            (gl:buffer-sub-data (buffer-target vbo) vertices)
            (gl:draw-arrays :triangles 0 6)
            (incf x (* scale (char-glyph-advance glyph)))))
```
