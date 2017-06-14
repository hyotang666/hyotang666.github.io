# *Dynamic-package*<br><small>あるいはpackage-inferred-systemに期待したもの</small>
## Meta info
### 対象読者
`DEFPACKAGE` `IN-PACKAGE`を書くの面倒臭いなぁと思っているCLer。

## Introduction
[このようなブログ](http://octahedron.hatenablog.jp/entry/2017/05/21/110420)がありました。
ここで触れられている「Package-inferred-systemに期待したもの」は簡単に作れそうだったのでやっつけで作ってみました。

## load-as-package
[これ](https://gist.github.com/hyotang666/bc0d97ce5477c5e360853621f4fc7ed5)は単なる`UIOP:LOAD*`へのラッパで、ファイル名をパッケージ名として暗黙裏にパッケージを作成します。

```lisp
#| a/hoge.lisp
(defun test()
  (format t "Yes!"))
|#

;;;; current-directory is a.

(load-as-package "hoge") ; => T
(find-package :hoge) ; => #<PACKAGE HOGE>
(hoge::test) ; Yes! ; NIL
```

なお、ロードしかしないのでコンパイルはされません。

```lisp
(compiled-function-p #'hoge::test) ; => NIL
```

サブディレクトリのファイルを指定すると、パッケージ名がサブディレクトリを含む名前になります。

```lisp
#| a/b/hoge.lisp
(defun test()
  (format t "This is b/hoge."))
|#

;;;; current-dirctory is a.

(load-as-package "b/hoge") ; => T
(b/hoge::test) ; This is b/hoge. ; NIL
```

以下のように親ディレクトリ下のファイルも読めます。

```lisp
#| ../hoge.lisp
(defun test()
  (format t "Parent."))
|#

;;;; current-directory is a.

(load-as-package "../hoge") ; => T
(../hoge::test) ; Parent. ; NIL
```
上記の通りパッケージ名は`../HOGE`になります。

```lisp
(find-package :../hoge) ; => #<PACKAGE ../HOGE>
```

引数は相対パスであることが想定されています。
絶対パスを渡した際の仕様は未定義です。
なお、現行の実装では期待どおりロードできますが、ファイルがカレントディレクトリ下のものでない場合パッケージ名がえらいことになります。

```lisp
(load-as-package "~/hoge") ; => T
(list-all-packages) ; (#<PACKAGE /HOME/MYNAME/HOGE> ...)
```

さて、これで件のブログで書かれていた機能は再現できたことになります。

ですがこれは「顧客が本当に欲しかったもの」でしょうか？
多分違うのではないでしょうか？

同じファイルをロードしているのにカレントディレクトリ次第でパッケージ名がコロコロ変わるのは嫌な予感しかしません。
これはファイル名のみをパッケージ名として採用する（すなわちディレクトリ名はパッケージ名に加えない）とすることで対応可能ですが、例えば"util.lisp"などといったファイル名は大変よく使われるもので、パッケージ名の衝突を避けられません。

ブログ内では「ディレクトリ構造とパッケージ構造を同じにしたいとき」とありますが、本当に欲しいものはもっと違うものなのではないでしょうか？
それはたとえば単に「暗黙裏に`DEFPACKAGE`してくれるもの」なのではないでしょうか？

## 顧客が本当に欲しかったもの（憶測）
という訳で顧客が本当に欲しかったものを憶測で作ってみました。

[Dynamic-package](https://github.com/hyotang666/dynamic-package)といって`ASDF`の拡張です。

通常CommonLispシステムを書く場合、以下のようなasdファイルを最初に用意することになるかと思われます。

```lisp
(in-package :asdf)
(defsystem :my-product :components ((:file "product-file")))
```
代わりに次のようなasdファイルにします。

```lisp
1 (in-package :asdf)
2 (load-system "dynamic-package")
3 (defsystem :my-product
4   :default-component-class dynamic-package:file
5   :components ((:file "product-file")))
```
`DEFSYSTEM`前に`DYNAMIC-PACKAGE`を`LOAD-SYSTEM`し（２）、`DEFSYSTEM`内に`:DEFAULT-COMPONENT-CLASS DYNAMIC-PACKAGE:FILE`のオプション指定をするだけ（４）です。
これでシステムを通常通りロードできます。

```lisp
(asdf:load-system :my-product) ; => T
(my-product.product-file::something) ; work.
```

`:DEFAULT-COMPONENT-CLASS`を`DYNAMIC-PCKAGE:FILE`に指定したシステムが管理するLispファイルの中では`DEFPACKAGE`も`IN-PACKAGE`もする必要がありません。
パッケージ名は「システム名.ファイル名」から自動的に作られます。
（ディレクトリ構造とは関係ありませんので、接続には'.'が使われています。）
ただし、システム名とファイル名が同名の場合システム名がそのままパッケージ名になります。
（エンドユーザへのインターフェイスパッケージとしてご利用ください。）

シンボルの管理に関しては`IMPORT`, `EXPORT`, `SHADOW`, `SHADOWING-IMPORT`, `USE-PACKAGE`などを明示的に書くことになりますが、これらの関数を直接呼んではいけません。
必ず`EVAL-WHEN`で包まないと上手く機能しません。

そこで`DYNAMIC-PACKAGE`は`CONTROL`という簡易ヘルパーを提供しています。
`CONTROL`は`DEFPACKAGE`と同様のシンタックス（ただしパッケージ名（第一引数）は受け取らない）なので学習コストは皆無と言って過言ではありません。

```lisp
(dynamic-package:control
  (:import-from :foo #:bar)
  (:export #:hoge))
(defun hoge()
  (bar ...))
```

他にも`Dynamic-package`は使用に際して注意しなければならない点がいくつかあります。
主にインポートに絡むシンボルの衝突についてです。
たとえば以下のようなコードを書いて

```lisp
(defun something()
  (let((date(get-universal-time)))
    ...))
```
その後、あるパッケージから`DATE`シンボルをインポートしようとしたら当然衝突します。
これは結構厄介な問題で、というのも、解決策は「衝突しないよう事前にインポートしておく」か「シャドウイングインポートする」かになるのですが、前者の場合は「シンボルを使いたいその場その場でインポートすることによって、そのシンボルが何処から来ているのかを明確にしてドキュメントとして機能するようにする」という部分が捨てられることになりますし、ドキュメントとしての機能を残したいのならそのシンボルを使っているコードごと手前に動かさないといけなくなります。
また、後者の場合は本来必要でないシャドウイングインポートをするという点で論理的美しさに欠けます。

こういったことはこれまではけして起こりませんでした。
シンボルの管理は`DEFPACKAGE`が一括で行っていたからです。
`DEFPACKAGE`を捨てたため`DEFPACKAGE`が解決してくれていた問題が再燃しだしたと言えます。
（ゴン、お前だったのか。。。）

## Conclusion
### About package-inferred-system
僕個人はpackage-inferred-system、というかone-package-per-fileのシステム構築に懐疑的な人間です。
ここでいう「懐疑的」とは、特定の状況下では上手く機能し非常に便利なものだけど銀の弾丸ではないよね、程度の意味合いです。

たとえば、複数のファイルから広く参照されるスペシャルシンボルがある場合、古式ゆかしきone-package-some-filesのアプローチなら単に「specials.lisp」というファイルを作ればいいだけでしたが、one-package-per-fileではどうでしょう？
そのスペシャルシンボルが何らかのモジュールの一部とみなせる場合はそのモジュールを担うファイルに突っ込めばいいだけの話ですが、特定のモジュールに属するとはみなせないようなものの場合どのファイルに入れるべきでしょうか？
最初にロードされるファイルでしょうか？
最初に参照するファイルでしょうか？
個別にファイルを分けるべきでしょうか？
仮に個別にファイルを分けるとして、エクスポートするべきシンボルがそれ一つしかないならどうでしょう？
ハードコアなone-package-per-file信者の方はパッケージを作る道を選ぶのかもしれませんが、僕には鶏をさばくのに牛刀を用いる感があります。

ではどうしているのか？

僕は個人的にはhierarchical-systemsが好きです。
hierarchical-systemsの代表はmcclimでしょうか。
lisp-builderもそうですね。
各種モジュールをファイルで分けるのではなくシステムで分けてしまうというものです。
各モジュールは古式ゆかしきone-package-some-filesで書かれますが、充分に小さなモジュールの場合ファイルは一つで済むので結果的にone-package-per-fileと同じ状況になることもあります。

これは気持ちの問題なのですが、one-package-per-fileをモダンなアプローチと捉えてしまった場合、one-package-some-filesのアプローチを取りづらくなるのではないでしょうか？
one-package-per-fileをone-package-some-filesの特殊な一状況と捉えることで、必要に応じて柔軟に選択できるようになるかと思われます。

### About package
`DEFPACKAGE`を捨てることで`DEFPACKAGE`が解決してくれていた問題が再燃しだしたのは先に見た通りです。
あちこちで`DYNAMIC-PACKAGE:CONTOL`を書いているとゲンナリしてきて一カ所にまとめたくなったりもします。
`DEFPACKAGE`はなんだかんだ言って割と良い解決策なのではないかとも思います。

とはいえ`DEFPACKAGE`が開発されたのは未だ`ASDF`の存在しなかった時代のことです。
`ASDF`はpackage-inferred-systemの提唱にあたり`UIOP:DEFINE-PACKAGE`を提供するに至っています。
深町氏はソースの保守性のために`CL-ANNOT`を使用してのアノテーションをファイルに付けるスタイルを提唱しています。
件のブログのような声が挙がるところを見ても、パッケージ管理のやり方はまだまだ発展途上といえるのかもしれません。
もっとも「発展途上」ならまだいいほうで、実際は、あちらを立てればこちらが立たずになるような「答えのない問題」あるいは「好みの問題」であるような気配がプンプンとしているのですが。

僕はもうCommonLispにドップリ漬かってしまっている人間ですので、システムにしてもパッケージにしても、全く疑問に思わず「そういうもの」として受け止めてしまっている人間ですので、公平な目で判断できないのが辛いところですね。
もっとも、なんらかの言語からCommonLispに移ってきた人はそのなんらかの言語のフィルターを通してCommonLispを見るでしょうから、それもまた公平な視点ではありません。
こういった場合、もっとも公平な目を持つ人というのはいわゆる言語オタクとでもいうような「広く浅く数々の言語を触ってきた人」という事になるでしょう。
当然、そこにも「個人的な趣味」というフィルターは入るでしょうから、複数人の言語オタクを集めて座談会とか開かれれば最高なんですが。

## Appendix
### dynamic-packge
`:DEFAULT-COMPONENT-CLASS`を指定するのではなく、直接使うことも出来ます。

```lisp
(defsystem :my-product
  :components ((dynamic-package:file "a")
               (:file "b" :depends-on ("a"))))
```
上記の例ではファイル"a"はdynamic-packageですが、ファイル"b"はそうではありません。
おそらくはファイルの中で`(in-package :my-product.a)`することとなります。

これによりone-package-per-fileでなくとも`DYNAMIC-PACKAGE`を利用できます。

### SBCL
SBCLは`DEFPACKAGE`を静的な宣言であると解釈します。
そのため、たとえば開発中、リロードしたシステムの`DEFPACKAGE`フォームが変更されていた場合（エクスポートされるシンボルが増えるなど）、処理系はクレームをつけてデバッガに落ちることとなります。
開発中に`DEFPACKAGE`の中身が変わるのは（僕にとっては）割とよくあることなので、これが割と鬱陶しい。

`DYNAMIC-PACKAGE`は`DEFPACKAGE`を使っていませんので、このようなクレームとは無縁です。
暗黙裏にパッケージが作られる`DYNAMIC-PACKAGE`のコンセプトは気に食わない方でも、このクレームを避けるために開発中は`DYNAMIC-PACKAGE`を使い、もうシンボルの構成は変わらないと確信できたあたりで`DEFPACKAGE`に戻して依存を切るという使い方をしたくなる人はいるかもしれません。

ちょうど`DEFCONSTANT`に対する`ALEXANDRIA:DEFINE-CONSTANT`のような感じですね。
