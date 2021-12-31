# [cl-yesql] hacks or [asdf] extension.
## 対象読者
* [cl-yesql]のユーザ。
* [asdf]の拡張に興味のあるCLer。

## Introduction.
[cl-yesql]を愛用しているのですが、気に食わない点として独自のシステムビルダを使用している点が挙げられます。
できればCommon Lisp界のデファクトスタンダードシステムビルダである[asdf]でビルドしたい。

個人的に[asdf]の魅力の一つは、ファイルやライブラリの依存を一ヶ所にまとめて書ける点にあると思っています。
（なので`package-infered-system`は好みでない。）
この点は特に（将来の自分を含む）第三者がシステムの全体像をざっくり把握したい場合役立ちます。

そんなわけでSQLファイルもasdファイルの`defsystem`フォームに記したい。

たとえば以下のようなディレクトリ構造のプロジェクトがあるとします。

```
+ myproject.asd
+ src
   |--- package.lisp
   |--- main.lisp
   |--- util.lisp
+ sql
   |--- user.sql
   |--- entry.sql
```

`defsystem`フォームはたとえば以下のように書きたい。

```lisp
(defsystem "myproject"
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "util" :depends-on ("package"))
                 (:file "main" :depends-on ("util" "package"))))
               (:module "sql"
                :depends-on ("src")
                :components ((yesql "user")
                             (yesql "entry")))))
```

本稿ではこれを実現するためのコードを備忘録として記しておきます。

## asdf:source-file
まず必要になるのは独自の`component`クラスです。
asdfは様々な`component`クラスを提供してくれています。

cl-yesqlではSQLファイルを書くことになります。
またそれらSQLファイルは`cl-yesql:import`によりLisp環境に関数群としてロードされます。
すなわちSQLファイルは形を変えたLispソースコードとみなせます。

そのようなソースコードファイルは`asdf:source-file`で表します。

```lisp
(defclass yesql (asdf:source-file) ()
  (:default-initargs :type "sql"))
```

`:default-initargs`でファイルの拡張子を`sql`に指定している点要注目。

```lisp
* (asdf:file-type (make-instance 'yesql :name "dummy")) => "sql"
```

## asdf:perform
`asdf:source-file`を継承した`component`クラスを定義した場合、`asdf:compile-op`と`asdf:load-op`に特定化された`asdf:perform`メソッドを各々定義しなくてはなりません。

```lisp
(defmethod asdf:perform ((o asdf:compile-op) (c yesql))
  :TODO)
(defmethod asdf:perform ((o asdf:load-op) (c yesql))
  :TODO)
```

## Compiling with universal.lisp
Lispファイルをコンパイルする場合、[asdf]は`asdf::perform-lisp-compilation`関数を使っています。

問題は我々が扱っているファイルはSQLファイルであってLispファイルではない点です。

[cl-yesql]が依存している埋め込み言語モジュールシステムである[vernacular]では`universal.lisp`ファイルを使いこの問題を解決しています。

大変興味深いので解説します。

### univesal.lisp
`universal.lisp`の中身は大変短いので引用しましょう。

```lisp
#.vernacular/specials:*program-preamble*
#.vernacular/specials:*program*
```

ご覧の通り読み込み時に変数を参照するだけです。

`universal.lisp`ファイルが`cl:compile-file`に渡されると、Lispはファイルの中身を読み込もうとします。

読み込み時評価である`#.`というディスパッチマクロがあるので変数を評価します。
これら変数は動的なスペシャル変数ですので、その次点で束縛されている値が読み込み時評価の結果としてLisp環境に読み込まれます。

すなわち変数に`(defun main () (print :hello))`というリストが束縛されていれば、あたかもファイルに`(defun main () (print :hello))`と書かれてあるかのようにLispは解釈します。

これにより、SQLファイルをLisp関数群としてコンパイルするにあたり、各SQLファイルに対応したLispファイルを作る必要なく、ただ一枚`universal.lisp`ファイルがありさえすれば後は逐次任意のフォームを変数に束縛するだけで事足ります。

賢い。

### compile-op
コンパイルは以下のようになります。

```lisp
(defmethod asdf:perform ((o asdf:compile-op) (c yesql))
  (progv (list (uiop:find-symbol* '#:*program-preamble* '#:vernacular/specials)
               (uiop:find-symbol* '#:*program* '#:vernacular/specials))
         (list :TODO :TODO)
    (asdf::perform-lisp-compilation o c)))
```

パッケージ`vernacular/specials`はasdファイルのロード時には未だ存在していないので、`cl:progv`を使ってメソッド呼び出し時に動的に束縛を作る点、また、`uiop:find-symbol*`でメソッド呼び出し時にシンボルを探している点、要注目。

### load-op
ロードは以下のようになります。

```lisp
(defmethod asdf:perform ((o asdf:load-op) (c yesql))
  (asdf::perform-lisp-load-fasl o c))
```

## asdf:input-files, asdf:output-files and operation object.
コンパイルやロードを行うためには、`component`オブジェクトからしかるべき`pathname`を作れなければいけません。
asdfは入/出力用の`pathname`を取り出すためのメソッド`asdf:input-files`、`asdf:output-files`を提供しています。

既定の振る舞いは以下のようになります。

```lisp
;;;; For compile.
* (asdf:input-files 'asdf:compile-op (asdf:find-component :myproject '("sql" "user")))
=> (#P"/home/hyotang666/.roswell/local-projects/myproject/sql/user.sql")

* (asdf:output-files 'asdf:compile-op (asdf:find-component :myproject '("sql" "user")))
NIL
T

;;;; For load.
* (asdf:input-files 'asdf:load-op (asdf:find-component :myproject '("sql" "user")))
=> (#P"/home/hyotang666/.roswell/local-projects/myproject/sql/user.sql")

* (asdf:output-files 'asdf:load-op (asdf:find-component :myproject '("sql" "user")))
NIL
T
```

通常は難しく考えずこれらメソッドを実装すればよろしゅうございます。

今回われわれがコンパイルしたいのはつまるところLispなので、[asdf]が持つ内部関数を便利に使わせてもらいましょう。
[asdf]に多くの仕事を任せることで、Lisp処理系依存による多くの問題を肩代わりさせられます。

```lisp
(defmethod asdf:output-files ((o asdf:compile-op) (c yesql))
  "Generate output fasl pathnames."
  (asdf::lisp-compilation-output-files o c))
```

上記メソッドの追加により、振る舞いは以下のようになります。

```lisp
;;;; For compile.
* (asdf:input-files 'asdf:compile-op (asdf:find-component :myproject '("sql" "user")))
=> (#P"/home/hyotang666/.roswell/local-projects/myproject/sql/user.sql")

* (asdf:output-files 'asdf:compile-op (asdf:find-component :myproject '("sql" "user")))
(#P"/home/hyotang666/.cache/common-lisp/sbcl-2.2.0-linux-x64/home/hyotang666/.roswell/local-projects/myproject/sql/user.fasl")
T

;;;; For load.
* (asdf:input-files 'asdf:load-op (asdf:find-component :myproject '("sql" "user")))
(#P"/home/hyotang666/.cache/common-lisp/sbcl-2.2.0-linux-x64/home/hyotang666/.roswell/local-projects/myproject/sql/user.fasl")

* (asdf:output-files 'asdf:load-op (asdf:find-component :myproject '("sql" "user")))
NIL
T
```

コンパイルのための出力ファイルとロードのための入力ファイルが変わった点要注目。

さて、ここからが厄介です。
我々は出力ファイルパスの生成に`asdf::lisp-compilation-output-files`を使いました。
そして`asdf::lisp-compilation-output-files`は`asdf:input-files`の振る舞いに依存しています。

ですが、われわれが`asdf::perform-lisp-compilation`経由で`cl:compile-file`に渡したいのは`universal.lisp`へのファイルパスです。
すなわち`asdf:input-files`の返り値をあるときはSQLファイル、あるときは`universal.lisp`と状況によって切り替えたい。
通常そのような場合は、たとえば引数でフラグを渡して切り替えるなどするのですが、`asdf:input-files`は[asdf]の総称関数であって自前のものではないのでAPIシグネチャの変更などはできません。
なら、スペシャル変数を宣言し、その値によって振る舞いを切り替えたいところですが、残念ながら`asdf:input-files`はメモ化されているのでこの手段も取れません。

よって今回は新たに`compile-yesql-op`という`operation`オブジェクトを導入することで対応します。

```lisp
(defclass compile-yesql-op (asdf:compile-op) ())

(defmethod asdf:input-files ((o compile-yesql-op) (c yesql))
  "Return universal file path."
  (list (make-pathname :name "universal"
                       :type "lisp"
                       :defaults (asdf:system-source-directory
                                   (asdf:find-system :vernacular)))))
```

振る舞いは以下のようになります。

```lisp
* (asdf:input-files 'compile-yesql-op (asdf:find-component :myproject '("sql" "user")))
=> (#P"/home/hyotang666/.roswell/lisp/quicklisp/dists/quicklisp/software/vernacular-20211020-git/universal.lisp")

* (asdf:output-files 'compile-yesql-op (asdf:find-component :myproject '("sql" "user")))
(#P"/home/hyotang666/.cache/common-lisp/sbcl-2.2.0-linux-x64/home/hyotang666/.roswell/lisp/quicklisp/dists/quicklisp/software/vernacular-20211020-git/universal.fasl")
T
```

`asdf:input-files`が`universal.lisp`になったのは歓迎ですが、`asdf:output-files`が`universal.fasl`なのはいただけません。
クラスにスロットを追加し、対応しましょう。

```lisp
(defclass compile-yesql-op (asdf:compile-op)
  ((op :accessor op))) ; <--- this!

(defmethod output-files ((o compile-yesql-op) (c yesql))
  (asdf:output-files (op o) c))
```

呼び出しがわ、すなわち`asdf:perform`の実装は以下のようになります。

```lisp
(defmethod asdf:perform ((o asdf:compile-op) (c yesql))
  (progv (list (find-symbol* '#:*program-preamble* '#:vernacular/specials)
               (find-symbol* '#:*program* '#:vernacular/specials))
    (list :TODO :TODO)
    (let ((op (asdf:make-operation 'compile-yesql-op)))
      (setf (op op) o)
      (asdf::perform-lisp-compilation op c))))
```

`operation`オブジェクトの作成には`asdf:make-operation`を使わねばならないとマニュアルにありますので、スロットの値はオブジェクト作成後に`setf`するかたちで行っています。

## Generate lisp functions.
残すはTODOの中身です。

`vernacular/specials:*program-preamble*`は`NIL`に束縛することとします。
ここは`defpackage`やら`in-package`フォームやらに束縛するべきものですが、今回は使いません。

`vernacular/specials:*program*`は`vernacular/lang:expand-module`の返り値をゴニョゴニョした`progn`フォームで束縛することとします。

筆者は[vernacular]というライブラリの振る舞いに明るくありません。
本当はもっといいやり方があるような気がしています。

ですがわずかながらソースを掘り返した結果、[vernacular]は[overlord]というビルドシステムと密接に結びついており、[asdf]との結合がうまく行かなそうでした。

今回は多少無理やりな方法で、やりたいことを実現します。

`vernacular/lang:expand-module`は`cl-yesql/postmodern:module-progn`フォームを生成します。
（"#lang cl-yesql/postmodern"の場合。）

フォームの`cdr`部には`cl-yesql/postmodern:defquery`フォームが詰まっています。
`cl-yesql/postmodern:defquery`マクロは`defun`フォームを生成するマクロです。

`cl-yesql/postmodern:module-progn`マクロを展開させてしまうと[vernacular]の内部に深く入り込む必要が生じてしまうようでした。

`cl-yesql/postmodern:defquery`フォームさえあればわれわれが行いたいことは可能なようですので、思い切って`cl-yesql/postmodern:module-progn`マクロは`cl:progn`で置き換えることとします。

`cl-yesql/postmodern:defquery`フォームの第一引数は`defun`フォームの第一引数となるべきシンボルで、不幸にも`cl-yesql/postmodern`パッケージにインターンされてしまっています。
これを強引に`myproject`パッケージにインターンさせることとします。
これでSQLファイルに書かれたコードは無事Lisp関数となり、すべて`myproject`パッケージにインターンされます。

なお、[vernacular]はこの編のインターン周りをより丁寧に行ってくれているようです。
ですがそのためにコードが著しく複雑になっているようなので筆者は把握するのを諦めました。

`asdf:perform`メソッドは以下のようになります。

```lisp
(defmethod asdf:perform ((o asdf:compile-op) (c yesql))
  (progv (list (uiop:find-symbol* '#:*program-preamble* '#:vernacular/specials)
               (uiop:find-symbol* '#:*program* '#:vernacular/specials))
    (list nil `(progn ,@(loop :for (op name . rest)
                              :in (cdr (uiop:symbol-call '#:vernacular/lang '#:expand-module
                                                         (asdf:component-pathname c)))
                          :collect `(,op ,(intern (symbol-name name) '#:myproject)
                                         ,@rest))))
    (let ((op (asdf:make-operation 'compile-yesql-op)))
      (setf (op op) o)
      (asdf::perform-lisp-compilation op c))))
```

なお、`vernacular/lang:expand-module`はパス名からシステム名とパッケージ名を推測するようで、失敗するとエラーを投げます。
これに対応するためには`overlord:set-package-base`を呼ぶ必要があります。
残念ながら`overlord:set-package-base`はマクロで`uiop:symbol-call`が使えないので、最終手段として`eval`を使います。

```lisp
(defmethod asdf:perform :before ((o asdf:compile-op) (c yesql))
  (eval
  `(,(uiop:find-symbol* '#:set-package-base '#:overlord)
     ,(make-pathname :directory (list :relative (asdf:component-name (asdf:component-parent c))))
     ,(asdf:primary-system-name c))))
```

実装は以上です。
これで冒頭のasdファイルをロードすることができるようになっているはずです。

SQL関数を`myproject`パッケージにインターンさせるため、"sql"モジュールを"src"モジュールに`:depends-on`させるのがコツです。

[cl-yesqlをasdfに統合させるissue](https://github.com/ruricolist/cl-yesql/issues/27)もあるので、こんなの自前で書かなくてもよくなると嬉しいのにな。

今回のコードは[ここ](https://gist.github.com/hyotang666/e54d4be187a9485a67dd24fc6f6a3dbf)にまとめておきます。

<!-- Links -->
[cl-yesql]:https://github.com/ruricolist/cl-yesql
[asdf]:https://gitlab.common-lisp.net/asdf/asdf
[vernacular]:https://github.com/ruricolist/vernacular
[overlord]:https://github.com/ruricolist/overlord
