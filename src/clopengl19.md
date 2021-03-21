# Fight against cl-opengl 19.
## Metanotes.
### 対象読者。
[前章](clopengl18.html)を読了済みの方。

## Introduction.
前章では音楽ファイルの再生を導入しました。
本章ではテクストの描画を実装します。

## ecosystem
OpenGLはグラフィックカードとやり取りする低レベルな仕様群です。
フォントの取り扱いなどは埒外です。

OpenGLでフォントの描画をする場合以下のライブラリ一択です。

### [cl-fond](https://github.com/Shirakumo/cl-fond)
本シリーズではこれを不採用とし自作する道を選びます。

不採用とする理由は`cl-fond`の設計にあります。

`cl-fond`では事前に使用する文字セットを文字列にして初期化関数に渡す必要があります。
使用文字数の少ないアルファベット文化圏ではこの設計で問題ないでしょうが日本語話者としては不便きわまりない設計です。

事前に必要な全文字の初期化を行うのではなく実行時にJIT的にフォントを生成することにします。
これにより初期化は必要なくなり、例えばキャラクターのセリフを追加するたびに初期化すべき文字セットの更新をしなければならないというような事態を避けられます。

## INITIALIZE-FONTS
指定されたディレクトリ下の全`ttf`ファイルの`PATHNAME`をリストにくくって返します。

```lisp
(defun initialize-fonts (root)
  (let ((ht (make-hash-table :test #'equal)))
    (uiop:collect-sub*directories root #'identity ; always true.
                                  #'identity ; recurse all directories.
                                  (lambda (dir)
                                    (loop :for pathname
                                               :in (uiop:directory-files dir
                                                                         "*.ttf")
                                          :do (setf (gethash
                                                      (pathname-name pathname)
                                                      ht)
                                                      pathname))))
    ht))
```

## \*FONTS\*
集めた`ttf`ファイルの`PATHNAME`群はグローバル変数に格納しておきます。
（何度も探したくない。）

```lisp
(defparameter *fonts* (initialize-fonts "/usr/share/fonts/"))
```

## FIND-FONT LIST-ALL-FONTS
グローバル変数を直接参照しなくていいように簡単なヘルパを定義します。

```lisp
(defun find-font (name &optional (errorp t))
  (or (values (gethash name *fonts*))
      (and errorp (error "Missing font named: ~S" name))))

(defun list-all-fonts ()
  (loop :for k :being :each :hash-key :of *fonts*
        :collect k))
```

## FONT-LOADER
フォント名から`ZPB-TTF::FONT-LOADER`を取り出します。
オープンした`ZPB-TTF::FONT-LOADER`は`PATHNAME`に変わりグローバル変数に格納しておきます。
（何度もファイルシステムにアクセスしたくない。）

```lisp
(defun font-loader (font-name)
  (let ((loader (find-font font-name nil)))
    (typecase loader
      (zpb-ttf::font-loader loader)
      ((or string pathname)
       (setf (gethash font-name *fonts*) (zpb-ttf::open-font-loader loader)))
      (otherwise
       (error
         "Unknown font. ~S ~:_Eval (fude-gl:list-all-fonts) for supported fonts."
         font-name)))))
```

## CHAR-GLYPH
必要なデータをまとめて管理できるように構造体を定義します。

```lisp
(defstruct char-glyph
  (texture 0 :type (unsigned-byte 32) :read-only t)
  w
  h
  bearing-x
  bearing-y
  advance)
```

## \*GLYPHS\*
生成された`CHAR-GLYPH`オブジェクトはグローバル変数に格納して管理します。

```lisp
(defvar *glyphs*)
```

## WITH-GLYPH
リソース管理のためにWITH系マクロを定義します。

```lisp
(defmacro with-glyph (() &body body)
  `(let ((*fonts* (alexandria:copy-hash-table *fonts*))
         (*glyphs* (make-hash-table)))
     (unwind-protect (progn ,@body)
       (loop :for g :being :each :hash-value of *glyphs*
             :do (gl:delete-textures (list (char-glyph-texture g))))
       (loop :for v :being :each :hash-value of *fonts*
             :when (typep v 'zpb-ttf::font-loader)
               :do (zpb-ttf::close-font-loader v)))))
```

## FONT-DATA
True type fontはベクタ画像であり描画するにはラスタ画像化する必要があります。
ラスタ画像化にはライブラリ[vecto](https://www.xach.com/lisp/vecto/)を使うと便利です。
注意点として`vecto`は`rgba`でラスタ画像化することが挙げられます。
ここで欲しいのは`grayscale`なので簡便のために生成された`rgba`から`alpha`要素だけ抜き出すこととします。
効率は著しく悪いですが実装の詳細には立ち入りたくないので効率が問題になるまではこれでokとします。

```lisp
(defun font-data (char loader size)
  (flet ((non-zero-int (i)
           (if (zerop i)
               1
               i)))
    (let* ((string (string char))
           (bbox (vecto:string-bounding-box string size loader))
           (w
            (ceiling
              (non-zero-int (- (zpb-ttf:xmax bbox) (zpb-ttf:xmin bbox)))))
           (h
            (ceiling
              (non-zero-int (- (zpb-ttf:ymax bbox) (zpb-ttf:ymin bbox))))))
      ;; TODO Implement gray scale rasterizer.
      (vecto:with-canvas (:width w :height h)
        (vecto:set-font loader size)
        (vecto:draw-string 0 (- (zpb-ttf:ymin bbox)) string)
        (values (loop :with vec = (vecto::image-data vecto::*graphics-state*)
                      :with new
                            = (make-array (* w h)
                                          :element-type '(unsigned-byte 8)
                                          :initial-element 0)
                      :for i :upfrom 3 :by 4
                      :while (array-in-bounds-p vec i)
                      :do (setf (aref new (floor i 4)) (aref vec i))
                      :finally (return new))
                w
                h
                (floor (zpb-ttf:xmin bbox))
                (ceiling (zpb-ttf:ymax bbox))
                (ceiling
                  (* (zpb-ttf:advance-width (zpb-ttf:find-glyph char loader))
                     (/ size (zpb-ttf:units/em loader)))))))))
```
## CHAR-GLYPH
文字を受け取り`CHAR-GLYPH`を返す関数です。
初めての文字に出会った場合`CHAR-GLYPH`を生成します。

```lisp
(defun char-glyph (char font-name &optional (size 16))
  (let ((loader (font-loader font-name)))
    (if (not (zpb-ttf:glyph-exists-p char loader))
        (error "~S is not exist in the font ~S." char font-name)
        (or (gethash char *glyphs*)
            (multiple-value-bind (image w h bearing-x bearing-y advance)
                (font-data char loader size)
              (gl:pixel-store :unpack-alignment 1)
              (let ((texture (car (gl:gen-textures 1))))
                (gl:active-texture texture)
                (gl:bind-texture :texture-2d texture)
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
```

## RENDER-TEXT
描画関数は以下の通り。

```lisp
(defun render-text
       (text shader
        &key (x 0) (y 0) (scale 1) (color '(1 1 1)) (font "Ubuntu-M")
        (vertices (error ":VERTICES is required."))
        (color-uniform (error ":COLOR-UNIFORM is required."))
        ((:vertex-array vao) (error ":VERTEX-ARRAY is required."))
        ((:vertex-buffer vbo) (error ":VERTEX-BUFFER is required.")))
  (setf text (map 'list (lambda (c) (char-glyph c font)) text))
  (gl:use-program shader)
  (apply #'gl:uniformf color-uniform color)
  (gl:active-texture 0)
  (gl:bind-vertex-array vao)
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
            (gl:bind-texture :texture-2d (char-glyph-texture glyph))
            (gl:bind-buffer :array-buffer vbo)
            (gl:buffer-sub-data :array-buffer vertices)
            (gl:draw-arrays :triangles 0 6)
            (incf x (* scale (char-glyph-advance glyph)))))
```
