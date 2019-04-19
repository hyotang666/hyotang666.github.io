<!-- {% raw %} -->
# github pages pitfalls
## Meta info
### 対象読者
Github pagesを使用している方。

## Introduction
Github pagesを利用していて陥ったPitfall群をまとめていきます。
追記は先頭にしていきます。

## Does not updated.
記事をPushしたのに反映されないことがあります。

この場合、登録してあるアドレスにメールが来てます。

さもなくばyour-name/your-name.github.ioページのsettingから状況の確認ができます。

## Page build failed: Unknown tag error
記事の中にリキッドタグ（{% ... %}というタグ）があるとこのエラーが起きる場合があります。
本エラーが起きるとビルドがコケて更新が反映されなくなります。

Github pagesはサイトのビルドにJekyllを使用してるようです。
そしてJekyllのテンプレートエンジンが記事内のリキッドタグに反応してしまって、場合によるとこのエラーが起きます。
Github pagesがJekyllの使用を強く推奨しているのは、同じツールを使うことでツール間の齟齬によるエラーを回避できるためでしょう。

本エラーはリキッドタグの周囲を`{% raw %}...{% endraw %}`というリキッドタグで囲めば対応できます。
ただしそのままだと記事内に`{% raw %}...{% endraw %}`という余計な文字列が現れてしまうのでコメントアウトしておきましょう。

HTMLないしMarkdownなら`<!-- {% raw %} -->`というようにします。

<!-- {% endraw %} -->
