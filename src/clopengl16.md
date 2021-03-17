# Fight against cl-opengl 16.
## Metanotes
### 対象読者
[前章](clopengl15.html)読了済みの方。

## Introduction.
前章では操作可能なプレイヤーを導入しました。
本章ではボールを導入します。

## Ball
### BALL
`BALL`オブジェクトを定義します。

```lisp
(defstruct (ball (:include movable) (:constructor %make-ball))
  radius
  (stuckp t :type boolean))
```

### CONSTRUCTOR
コンストラクタは以下の通り。

```lisp
(defparameter *initial-velocity* (list 100 -350))

(defun make-ball (player)
  (let ((radius 12.5))
    (with-slots (x y w h)
      player
      (%make-ball :x (+ x (/ w (- 2 radius)))
                  :y (+ y (* (- radius) 2))
                  :w (* radius 2)
                  :h (* radius 2)
                  :velocity (apply #'3d-vectors:vec2 *initial-velocity*)
                  :radius radius))))
```

### MOVE
総称関数`MOVE`は以下の通り。
オプショナルにキーワード引数を受け付けるAPIに変わった点要注意。

```lisp
(defgeneric move (subject dt width &key)
  (:method ((player player) (dt float) (width integer) &key ball)
    (with-slots (x w velocity)
        player
      (keypress-case
        (:left
         (let ((new (max 0 (- x (* velocity dt)))))
           (setf x new)
           (when (ball-stuckp ball)
             (setf (ball-x ball) new))))
        (:right
         (let ((new (min (- width w) (+ x (* velocity dt)))))
           (setf x new)
           (when (ball-stuckp ball)
             (setf (ball-x ball) new))))
        (:space (setf (ball-stuckp ball) nil)))))
  (:method ((ball ball) (dt float) (width integer) &key)
    (with-slots (x y w velocity stuckp)
        ball
      (unless stuckp
        (let ((new
               (3d-vectors:v+ (3d-vectors:vec2 x y)
                              (3d-vectors:v* dt velocity))))
          (cond
            ((<= (3d-vectors:vx new) 0)
             (setf (3d-vectors:vx velocity) (- (3d-vectors:vx velocity))
                   (3d-vectors:vx new) 0))
            ((<= width (+ (3d-vectors:vx new) w))
             (setf (3d-vectors:vx velocity) (- (3d-vectors:vx velocity))
                   (3d-vectors:vx new) (- width w)))
            ((<= (3d-vectors:vy new) 0)
             (setf (3d-vectors:vy velocity) (- (3d-vectors:vy velocity))
                   (3d-vectors:vy new) 0)))
          (setf x (3d-vectors:vx new)
                y (3d-vectors:vy new)))))))
```

### MAIN
`MAIN`関数は以下の通り。

```lisp
(defun main ()
  (uiop:nest
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
                            (ball-tex :texture-2d ; <--- New!
                                      :init (fude-gl:tex-image-2d
                                              (ensure-image :face)))))
    (let* ((level (level *level1* win))
           (player (make-player win))
           (ball (make-ball player))) ; <--- New!
      (gl:uniform-matrix projection 4 (ortho win)))
    (sdl2:with-event-loop (:method :poll)
      (:quit ()
        t))
    (:idle nil (sleep (/ 1 30)))
    (fude-gl:with-clear (win (:color-buffer-bit))
      (move player 0.05 (sdl2:get-window-size win) :ball ball) ; <--- Updated!
      (move ball 0.05 (sdl2:get-window-size win)) ; <--- New!
      (draw model
            (multiple-value-call #'model-matrix 0 0 (sdl2:get-window-size win))
            image background)
      (dotimes (i (array-total-size level) (gl:uniformf |spliteColor| 1 1 1))
        (let ((o (row-major-aref level i)))
          (when o
            (with-slots (x y w h type color)
                o
              (3d-vectors:with-vec3 (r g b)
                  color
                (gl:uniformf |spliteColor| r g b))
              (draw model (model-matrix x y w h) image
                    (ecase type (:solid block-solid) (:normal block)))))))
      (with-slots (x y w h) ; <--- New!
          ball
        (draw model (model-matrix x y w h) image ball-tex))
      (with-slots (x y w h)
          player
        (draw model (model-matrix x y w h) image paddle)))))
```
![GIF of the example above.](../img/fude-gl/ball.gif)
