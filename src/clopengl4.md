# Fight against cl-opengl 4.
## Metanotes
### 対象読者
[前回](clopengl3.html)読了済みの方。

## Introduction.
前回はバッファを抽象化しました。
今回はシェーダーを抽象化します。

## Shaders
OpenGLではシェーダー周りの処理が６段階あります。
そのうちの３つにユーザーは手を入れられます。
各々vertex-shader、geometry-shader、fragment-shaderです。

vertexおよびfragmentシェーダーは必須（required）です。

## GLSL
シェーダープログラムはそれ用のDSLであるGLSLで書く必要があります。

ここでは簡便のため文字列で持つとします。

### [VARJO](https://github.com/cbaggers/varjo)
S式でGLSLを書くコンパイラもあります。

ここで採用しない理由は筆者がGLSLを把握できていないからです。
何かしらエラーが起きたとき調べるヶ所は少ない方が良いとの判断です。

将来的には導入するかもしれません。
メモだけしておきます。

## Workflow
シェーダーを作る一連の典型的な手順は以下になります。

1. OpenGLにシェーダーの作成を依頼。
2. シェーダーにGLSLソースコードを流し込む。
3. シェーダーをコンパイル。
4. OpenGLにプログラムの作成を依頼。
5. プログラムに各シェーダーを順次アタッチ。
6. プログラムをリンクさせる。
7. シェーダーの削除を依頼。
8. これから使うプログラムを指定。
9. 本体処理。
10. プログラムの削除を依頼。

シェーダー、プログラムともに削除が必要なのでやはりWITH系マクロの出番です。

## WITH-PROG
ここでは必須の２シェーダーのみ使うものとします。
[YAGNI](https://ja.wikipedia.org/wiki/YAGNI)の精神です。

```lisp
(defmacro with-prog ((var vertex-shader fragment-shader) &body body)
  (let ((vs (gensym "VERTEX-SHADER")) (fs (gensym "FRAGMENT-SHADER")))
    `(let ((,var (gl:create-program)))
       (labels ((s-compile (id source)
                  (gl:shader-source id source)
                  (gl:compile-shader id)
                  (may-warn (gl:get-shader-info-log id))
                  (gl:attach-shader ,var id))
                (may-warn (log)
                  (unless (equal "" log)
                    (warn log))))
         (unwind-protect
             (let ((,vs (gl:create-shader :vertex-shader))
                   (,fs (gl:create-shader :fragment-shader)))
               (unwind-protect
                   (progn
                    (s-compile ,vs ,vertex-shader)
                    (s-compile ,fs ,fragment-shader)
                    (gl:link-program ,var)
                    (may-warn (gl:get-program-info-log ,var))
                    (gl:use-program ,var))
                 (gl:delete-shader ,fs)
                 (gl:delete-shader ,vs))
               ,@body)
           (gl:delete-program ,var))))))
```

これまでのWITH系マクロと比べると規模が大きくなりますがそれでも基本構造は同じです。
シェーダーの開放のために`UNWIND-PROTECT`がネストしている点と共通するコンパイル処理をローカル関数に抜き出している点が特徴です。
