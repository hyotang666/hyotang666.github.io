# Fight against cl-opengl 18.
## Metanotes
### 対象読者。
[前章](clopengl17.html)読了済みの方。

## Introduction.
前章では衝突判定を導入してゲームらしくなりました。
本章では音楽を導入します。

## Ecosystems
Common Lispで音楽を再生しようとする場合かつそれがゲームエンジンでの使用とする場合選択肢は二つです。

### [harmony](https://github.com/Shirakumo/harmony)
本シリーズではこれを採用します。

比較的新しく活発に開発されています。
OSを問わず使えるように設計されておりバックエンドも豊富です。
ただ悪く言うとまだ枯れておらずドキュメントは皆無です。
使いたい場合はソースを読まなければなりません。

### [mixalot](https://github.com/ahefner/mixalot)
こちらは古くあまりメンテナンスされておりません（最新コミットが2015年ころ）。
ただよく言えば枯れているともみなせドキュメントも丁寧です。

harmonyを採用した理由は何よりもチームshirakumoのプロダクトであるという点に尽きます。
またゲームエンジンのために作られたという点も評価できます。

## ENSURE-SOUND
必要な音楽ファイルを本家から導入しましょう。
各ファイルにはキーワードによる名前をつけて管理します。
（不測の事態を招きたくない。）

```lisp
(let ((pathnames (make-hash-table)))
  (flet ((def (name url)
           (let ((pathname
                  (ensure-directories-exist
                    (merge-pathnames
                      (format nil "sounds/~A"
                              (subseq url (1+ (position #\/ url :from-end t))))
                      (asdf:system-source-directory
                        (asdf:find-system :breakout-cl))))))
             (unless (probe-file pathname)
               (uiop:format! *trace-output* "~&Downloading ~S" url)
               (dex:fetch url pathname))
             (setf (gethash name pathnames) pathname))))
    (def :breakout "https://learnopengl.com/audio/in-practice/breakout/breakout.mp3")
    (def :break "https://learnopengl.com/audio/in-practice/breakout/bleep.mp3")
    (def :solid "https://learnopengl.com/audio/in-practice/breakout/solid.wav")
    (def :bounce "https://learnopengl.com/audio/in-practice/breakout/bleep.wav")
    (defun ensure-sound (name)
      (or (gethash name pathnames)
          (error "Unknown file name ~S: ~S" name
                 (alexandria:hash-table-keys pathnames))))))
```

## WITH-HARMONY
harmonyはサーバーです。
リソース管理のためいつもどおりWITH系マクロを定義しましょう。

```lisp
(defmacro with-harmony ((var) &body body)
  `(symbol-macrolet ((,var org.shirakumo.fraf.harmony:*server*))
     (org.shirakumo.fraf.harmony:maybe-start-simple-server)
     (unwind-protect (progn ,@body) (org.shirakumo.fraf.harmony:stop ,var))))
```

## PLAY
簡便のため`ORG.SHIRAKUMO.FRAF.HARMONY:PLAY`関数をラップしましょう。

```lisp
(defun play (name &key repeat (mixer :effect))
  (org.shirakumo.fraf.harmony:play (ensure-sound name)
                                   :name name
                                   :mixer mixer
                                   :if-exists :restart
                                   :repeat repeat))
```

## OBJECTS
ゲームロジックに関係のない副作用目当てのコードを紛れ込ませるのは嫌です。
音楽再生は総称関数の`:BEFORE`メソッドに担わせましょう。

そのためにはオブジェクトを少々変更する必要があります。

```lisp
(defstruct (normal-block (:include blocks)) (brokenp nil :type boolean))

(defstruct (solid-block (:include blocks)))
```

## COLLIDEP
`COLLIDEP`を総称関数に変更します。

```lisp
(defgeneric collidep (subject object)
  (:method ((circle ball) (rect game-object))
    (with-slots (radius x y)
        circle
      (with-slots (w h (rect-x x) (rect-y y))
          rect
        (let* ((center (3d-vectors:vec (+ x radius) (+ y radius)))
               (aabb-half-extents (3d-vectors:vec (/ w 2) (/ h 2)))
               (aabb-center
                (3d-vectors:nv+ (3d-vectors:vec rect-x rect-y)
                                aabb-half-extents))
               (difference
                (3d-vectors:v-
                  (3d-vectors:v+ aabb-center
                                 (3d-vectors:vclamp
                                   (3d-vectors:v- center aabb-center)
                                   (3d-vectors:v- aabb-half-extents)
                                   aabb-half-extents))
                  center)))
          (if (< (3d-vectors:vlength difference) radius)
              (values t (vector-direction difference) difference)
              (values nil :up (3d-vectors:vec 0 0)))))))
  (:method ((ball ball) (block normal-block))
    (unless (normal-block-brokenp block)
      (call-next-method)))
  (:method ((subject ball) (object null)) ; Do nothing
    ))
```

## RESPONSE
衝突応答を担う総称関数です。

```lisp
(defgeneric response (subject object &key)
  (:method :before ((ball ball) (player player) &key) (play :bounce)) ; <--- Side effect!
  (:method ((ball ball) (player player) &key)
    (with-slots (x w)
        player
      (let* ((center-board (+ x (/ w 2)))
             (distance (- (+ (ball-x ball) (ball-radius ball)) center-board))
             (percentage (/ distance (/ w 2)))
             (strength 2)
             (length (3d-vectors:vlength (ball-velocity ball))))
        (3d-vectors:vsetf (ball-velocity ball)
                          (* (car *initial-velocity*) percentage strength)
                          (* -1
                             (abs (- (3d-vectors:vy (ball-velocity ball))))))
        (setf (ball-velocity ball)
                (3d-vectors:v* (3d-vectors:vunit (ball-velocity ball))
                               length)))))
  (:method ((ball ball) (block blocks) &key direction difference)
    (if (find direction '(:left :right))
        (let ((penetration
               (- (ball-radius ball) (abs (3d-vectors:vx difference)))))
          (setf (3d-vectors:vx (ball-velocity ball))
                  (- (3d-vectors:vx (ball-velocity ball))))
          (if (eq :left direction)
              (incf (3d-vectors:vx (ball-velocity ball)) penetration)
              (decf (3d-vectors:vx (ball-velocity ball)) penetration)))
        (let ((penetration
               (- (ball-radius ball) (abs (3d-vectors:vy difference)))))
          (setf (3d-vectors:vy (ball-velocity ball))
                  (- (3d-vectors:vy (ball-velocity ball))))
          (if (eq :up direction)
              (incf (3d-vectors:vx (ball-velocity ball)) penetration)
              (decf (3d-vectors:vx (ball-velocity ball)) penetration)))))
  (:method :before ((ball ball) (block normal-block) &key) (play :break)) ; <--- Side effect!
  (:method ((ball ball) (block normal-block) &key)
    (setf (normal-block-brokenp block) t)
    (call-next-method))
  (:method :before ((ball ball) (block solid-block) &key) (play :solid))) ; <--- Side effect!
```

## CHECK-COLLISION

上記ヘルパーを使うことで`CHECK-COLLISIOIN`は以下の通りの短さになります。

```lisp
(defun check-collision (ball player level)
  (unless (ball-stuckp ball)
    (if (collidep ball player)
        (response ball player)
        (response ball level))))
```

## DRAW
特に必要に迫られたわけではないですが設計がオブジェクト志向的になってきたので`DRAW`も総称関数にしておきます。

```lisp
(defgeneric draw (object model image texture &key)
  (:method (model model-mat image texture &key)
    "Default method to draw."
    (gl:uniform-matrix model 4 model-mat)
    (gl:uniformi image texture)
    (gl:draw-arrays :triangles 0 6))
  (:method ((bg (eql :background)) model image texture &key
            (win (alexandria:required-argument :win)))
    (call-next-method model
     (multiple-value-call #'model-matrix 0 0 (sdl2:get-window-size win)) image
     texture))
  (:method :before ((o blocks) model image texture &key
                    (splite-color
                     (alexandria:required-argument :splite-color)))
    (3d-vectors:with-vec3 (r g b)
        (blocks-color o)
      (gl:uniformf splite-color r g b)))
  (:method ((o game-object) model image texture &key)
    (with-slots (x y w h)
        o
      (call-next-method model (model-matrix x y w h) image texture)))
  (:method ((level array) model image (textures list) &key
            (splite-color (alexandria:required-argument :splite-color)))
    (dotimes (i (array-total-size level) (gl:uniformf splite-color 1 1 1))
      (let ((o (row-major-aref level i)))
        (draw o model image (getf textures (type-of o))
              :splite-color splite-color))))
  (:method ((block normal-block) model image textures &key)
    (unless (normal-block-brokenp block)
      (call-next-method)))
  (:method ((n null) model image texture &key &allow-other-keys)) ; Do nothing.
  )
```

## MAIN
`MAIN`関数は以下の通り。

```lisp
(defun main ()
  (uiop:nest
    (with-harmony (server) ; <--- New!
      (play :breakout))
    (sdl2:with-init (:everything))
    (sdl2:with-window (win :flags '(:shown :opengl)
                           :x 100
                           :y 100
                           :w 800
                           :h 600
                           :title "Breakout-CL"))
    (sdl2:with-gl-context (context win)
      (gl:enable :blend)
      (gl:blend-func :src-alpha :one-minus-src-alpha))
    (fude-gl:with-shader ((splite
                            (:vertices *quads*)
                            (:uniform model projection |spliteColor| image))))
    (fude-gl:with-textures ((background :texture-2d
                                        :init (fude-gl:tex-image-2d
                                                (ensure-image :background)))
                            (block :texture-2d
                                   :init (fude-gl:tex-image-2d
                                           (ensure-image :block)))
                            (block-solid :texture-2d
                                         :init (fude-gl:tex-image-2d
                                                 (ensure-image :block-solid)))
                            (paddle :texture-2d
                                    :init (fude-gl:tex-image-2d
                                            (ensure-image :paddle)))
                            (ball-tex :texture-2d
                                      :init (fude-gl:tex-image-2d
                                              (ensure-image :face)))))
    (let* ((level (level *level1* win))
           (player (make-player win))
           (ball (make-ball player)))
      (gl:uniform-matrix projection 4 (ortho win)))
    (sdl2:with-event-loop (:method :poll)
      (:quit ()
        t))
    (:idle nil (sleep (/ 1 60)))
    (fude-gl:with-clear (win (:color-buffer-bit))
      (move player 0.025 (sdl2:get-window-size win) :ball ball)
      (move ball 0.025 (sdl2:get-window-size win))
      (check-collision ball player level)
      (draw :background model image background :win win)
      (draw level model image `(normal-block ,block solid-block ,block-solid)
            :splite-color |spliteColor|)
      (draw ball model image ball-tex)
      (draw player model image paddle))))
```
