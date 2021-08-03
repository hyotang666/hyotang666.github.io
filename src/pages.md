# *Pages*<br><small>あるいはCommonLisp製github.pages用コンパイラ。</small>

## Meta info
### 対象読者
Github.pagesを使ってのブログ運営をしてみようかしら、と思ってるCLer。

## Introduction
CommonLispの勉強を初めて数年経ち、様々な知見を蓄えてきました。
そのような知見はブログという形で共有されれば有意義であるというのが、大方の見方です。
僕自身もその意見には比較的賛成なのですが、僕は非常にものぐさな人間ですので、これまでそのような発表を一切と言って差し支えないレベルで行ってきませんでした。

まぁ、でも、そろそろなんか書こうかなぁ、と。

2017年現在、技術系ブログは、僕の観測範囲内では大きく４つに別れる印象です。

* qiita
* hatena
* github.pages
* 自サバ

自サバはハードルが高い。
hatenaはべつに技術系に特化したサービスというわけではない。
qiitaはなんかいろいろアレ。
という訳でgithub.pagesが良いかなぁ、と。

Github.pagesでのブログ運営は簡単で、cloneしてきたディレクトリにhtmlを置いてpushしたらいいだけみたいです。
管理にはjekyllを使うのが本家推奨です。

ですがここで大きな問題がありまして、僕はhtml/css/jsに関してズブの素人なのです。
これからブログを運営しながら少しずつ学んでいくとして、それにプラスしてjekyllなどというツールの使い方まで学んでいかなくてはならないというのはいかにもハードルが高い。

そこで慣れ親しんだCommonLispでそれ用のスクリプトを自作すれば、jekyllの使い方を学ばなくて良いという意味合いでハードルが一つ減りますし、やりたいことはmarkdownで記事を書いたらいい具合にhtmlにコンパイルしてくれるというだけのものですので作るのは簡単そうですし、自作するとなれば嫌でも生のhtmlやcssを見ないわけにはいかなくなりますのでその辺の学習も進むだろうと安直に考えたわけです。

なによりCommonLispを書くのは楽しいですからね。

そうしてできたのが[Pages](https://github.com/hyotang666/pages)です。

## How to use
Clone してきたディレクトリに行って、、、

```shell
cd to/your/name.github.io
```

'src'ディレクトリをほって、、、

```shell
mkdir src
```

記事を書いて、、、

```shell
vim src/first-blog.md
```

pagesを呼ぶだけ。

```
pages
```

Pagesはだいたい以下のようにディレクトリを構築します。

```
yourname.github.io/
 +--- archives/
 |    +--- blog.html
 |
 |--- archives.html
 +--- css/
 |    +---css.css
 |
 +--- img/
 +--- indexes/
      +---index.html
 +--- src/
      +--- blog.md
```
srcディレクトリにあるmarkdown（note 拡張子は`md`でなくてはなりません。）からarchives下のhtmlを生成し、それらへのリンクをまとめたarchives.htmlを生成、最後に最新記事をindex.htmlとして生成します。

markdownからhtmlへのコンパイルはmarkdownがindex.htmlより新しいもののみ行われます。
（すなわち、アップデートされたもののみ再コンパイルされます。）
また、archives.htmlは新しい順にソートされた形でコンパイルされます。

## Installation
Rosスクリプトが書かれているのでROSWELL経由でインストールするのが推奨です。

```shell
ros install hyotang666/markup-functions
ros install hyotang666/pages
```

たぶんこれでインストールできるはず（試してない）。

なお、shellから叩きたくない人はREPLから`(PAGES:COMPILE)`を評価していただければ良いです。

現在はmarkdownしか対応していませんし、今後もtexやorgといった他の物に対応する気はありませんが、`PAGES:COMPILE`のキーワード引数`:PATTERN`に拡張子のパターン文字列を（規定値は`"*.md"`）、`:COMPILER`にパスネームを引数に受け取り`*STANDARD-OUTPUT*`に`<body>`の中身を出力する無引数関数（ようするにthunk）を返す関数を渡してあげればそれで動きますので拡張は簡単だろうと思います。

以下にhtmlを受け取ってhtmlへコンパイルする`IDENTITY`的なものの例を書いときますね。

```lisp
(pages:compile :pattern "*.html"
               :compiler (lambda(pathname)
                           (lambda()
                             (write-string (uiop:read-file-string pathname)))))
```

## 作ってみて。
デバッグしやすいように副作用を分離していったらTHUNKの嵐になってちょっと混乱したｗ

関数を返す関数をこんなにも書いたのは初めてかもしれない。

<del>htmlのマークアップにはCL-WHOを使っています。（特に不満がないので。）
深町氏のcl-markupや2017/5のquicklisp-updateで入ってきたCl-whyも気になるっちゃ気になりますが。</del><br>
htmlのマークアップには自作のHTMLgeneratorである[markup-functions](https://github.com/hyotang666/markup-functions)を使っています。
プリティプリントのサポートとコンパイル時のHTML文法チェックに力を入れてあります。

cssのコンパイルにはcl-cssを使っています。

markdownからhtmlへのコンパイルには3bmdを使っています。
<del>（3bmdはclispで動かないバグがあるのでpagesもclispでは動きません。）</del>
(3bmdがclispで動こないバグはPRを送ったので多分動くようになってます。テストはしてません)
2017/5のquicklisp-updateで入ってきたmarkdown.clも気になりますが、まだアルファクオリティとのこで敬遠しました。
cl-markdown？
そんな子いましたかねぇ？

<del>なお[前回の記事](https://hyotang666.github.io/archives/dynamic-package)で書いた`DYNAMIC-PACKAGE`を試験的に使用しています。</del>（試験の結果使いづらいという結論に達したので依存は外しました）

## 今後。
ご覧の通りのへっぽこCSSなので、見た目をもう少し何とかしたいかなぁとは思いつつ、必要充分だよなぁとも思いつつ。
ここから先はデザイナの領域だなぁ、とも思いつつ。

JSは、べつに、いらない、、かなぁ？

今はそんなことより手元のオレオレライブラリ群をちまちまとgithubに上げてはブログを書くという方に専念したほうがよさそげ、かとも思っております。

将来的にアレコレしたくなったときに、このPagesを拡張していくのか、さっさと捨てて作りなおすのか、それともjekyllなどに鞍替えするのか、何もかも未定です。

という訳でPagesの使用は非推奨です。

### 2021年追記
なんだかんだで使い続けてはいます。
リファクタリング等でコードの内容と記事が噛み合わなくなって来たので修正。
