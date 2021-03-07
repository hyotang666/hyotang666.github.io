# Fight against cl-opengl 3
## Metanotes
### 対象読者
[前章](clopengl2.html)読了済みの方。

## Introduction.
前回ではwindowが毎フレーム更新されるようになりました。
本章からOpenGLに描画を依頼するための土台を把握していきます。

## Vertex Buffer Object.
OpenGLの新作法では頂点（vertex）の情報はOpenGL側のバッファに格納して扱います。
これによりデータ送信という重い作業を一度で済ませてしまおうという魂胆です。

典型的な作業順は以下の通りとなります。

1. OpenGLにN個のバッファの作成を依頼。
2. 作られたバッファの中からターゲットとなるバッファを一つ指定。
3. 指定したバッファにデータを送信。
4. 本体処理。
5. バッファの削除をOpenGLに依頼。

Common Lispには`PACKAGE`という名前空間システムがあります。
複数ある名前空間のうち現在有効な名前空間はプログラムの任意の時点で常にひとつです。
その現在有効な名前空間をカレントパッケージと呼びます。
カレントパッケージは変数`*PACKAGE*`に束縛されています。

これと同様にOpenGLでは複数のバッファを管理できますが現在有効なバッファは常にひとつです。
慣れ親しんだLispの文脈に従いこれをカレントバッファと呼ぶことにします（OpenGLの文脈で特別な呼称は存在しないようです（要出典））。
Common Lispでパッケージの切り替えに`IN-PACKAGE`を使うようにOpenGLでバッファの切り替えは`GL:BIND-BUFFER`で行います。

作ったバッファは適切に削除しなければなりません。
ということはWITH系マクロの出番です。

ここでは確保・開放のみならず初期化までを担わせます。

## WITH-BUFFER
### BUFFER-TARGET
OpenGLでは比較的に時間のかかってしまうCPUとのやり取りを削減するためにバッファはまとめて作成します。
バッファには様々な用途がありますが用途の指定は後から行います。

どういう用途があるかはOpenGLの仕様を読みます。
前回同様ここではドキュメントを兼ねた型を定義します。

```lisp
(deftype buffer-target ()
  '(member :array-buffer :element-array-buffer
           :copy-read-buffer :copy-write-buffer
           :pixel-unpack-buffer :pixel-pack-buffer
           :query-buffer :texture-buffer
           :transform-feedback-buffer :uniform-buffer
           :draw-indirect-buffer :atomic-counter-buffer
           :dispatch-indirect-buffer :shader-storage-buffer))
```

### BUFFER-USAGE
OpenGLに送信したデータをどのように使うのかについても指定が必要となります。

オプションについてはOpenGLの仕様を読みます。
上同様これも型を定義します。

```lisp
(deftype buffer-usage () '(member :static-draw :stream-draw :dynamic-draw))
```

### WITH-BUFFER
マクロ`WITH-BUFFER`もこれまでの自作WITH系マクロ同様薄いラッパです。
これまでと違うのは初期化処理も担う点です。

```lisp
(defmacro with-buffer ((&rest bind*) &body body)
  `(destructuring-bind
       ,(mapcar #'car bind*)
       (gl:gen-buffers ,(length bind*))
     (unwind-protect
         (progn
          ,@(mapcan
              (lambda (bind)
                (destructuring-bind
                    (var array
                     &key (target :array-buffer) (usage :static-draw))
                    bind
                  `((gl:bind-buffer (the buffer-target ,target) ,var)
                    (gl:buffer-data (the buffer-target ,target)
                                    (the buffer-usage ,usage) ,array))))
              bind*)
          ,@body)
       (gl:delete-buffers (list ,@(mapcar #'car bind*))))))
```

中程にある`MAPCAN`が初期化処理を埋め込みます。
それを除けばこれまでのWITH系マクロと構造は同じです。
