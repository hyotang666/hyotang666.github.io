# Escape from the cave, dive into the bed!
## Metanote
### 対象読者
* Common Lispでweb-frameworkを探している/使っている人。

## Introduction.
[caveman]を使っていたのですが[snooze]に乗り換えたのでその理由を記しておきます。

# What is [caveman]?
まずは[caveman]とは何かというお話から。

[caveman]は[ningle]の上に組み立てられた"microでない"web-frameworkです。
"microでない"、は、この場合

* [cl-dbi]/[datafly]/[sxql]によるデータベースサポート。
* [djula]によるhtmlテンプレートエンジンサポート。
* [envy]によるコンフィグレーションサポート。

を含むことを意味します。

これは言葉を変えるなら、上記みっつの機能に不満があるなら、[caveman]を使うより[ningle]上に自分の好むデータベース、テンプレートエンジン、コンフィグレーションを組み込んでいくほうが良いということです。

# What is [ningle]?
次に[ningle]とは何かというお話。

[ningle]は[clack]の上に組み立てられた"micro" web-frameworkです。
"micro"、は、この場合。

* [myway]によるルーティングサポート。

くらいしか[clack]に追加していないことを意味します。

これは言葉を変えるなら、ルーティングに不満があるなら、[ningle]を使うより[clack]上に自分の好むルーティングシステムを組み込んでいくほうが良いということです。

# Altenatives.
## [cl-yesql]
データベースサポートは[cl-yesql]を使うことにしました。

[cl-yesql]は[clojure]の[yesql]から本歌取りしたものです。

なぜ[cl-yesql]か、についてはこの[本家のREADME][yesql-rationale]に理由が記されてあります。
これがまさに膝を打つ内容でした。

## [markup-functions]
htmlテンプレートエンジンは[markup-functions]を使うことにしました。

[djula]の[哲学][djula-philosophy]はある程度共感できましたが、今の自分には必要がないと判断しました。

[DSL]の学習コストが高いという印象が強うございます。

有力な代替案は[cl-who]でしたが、様々な不満があったので自作しました。
不満点については[README][markup-functions]にあるのでそちらをご覧ください。

## [snooze]

[caveman]の主要機能みっつのうち、ふたつを変えるとなると、[caveman]を使う理由は乏しくなります。
[ningle]上に再構築しようかとリサーチをしたところ[snooze]と出会いました。

[snooze]は大変筋が良いように見えましたので思いきって乗り換えてみました。

[snooze]のどこを「筋が良い」と判断したかについては、主に[READMEのここ][snooze-rationale]にございます。

# Conclusion.
今の所、大変満足しております。

今後の課題は[clack]/[lack]のインターフェイスを使い続けるのか思い切って直接[hunchentoot]に依存してしまうかです。

<!-- Links -->

[caveman]:https://github.com/fukamachi/caveman
[snooze]:https://github.com/joaotavora/snooze
[ningle]:https://github.com/fukamachi/ningle
[cl-dbi]:https://github.com/fukamachi/cl-dbi
[datafly]:https://github.com/fukamachi/datafly
[sxql]:https://github.com/fukamachi/sxql
[djula]:https://github.com/mmontone/djula
[envy]:https://github.com/fukamachi/envy
[clack]:https://github.com/fukamachi/clack
[myway]:https://github.com/fukamachi/myway/
[cl-yesql]:https://github.com/ruricolist/cl-yesql
[clojure]:https://clojure.org/
[yesql]:https://github.com/krisajenkins/yesql
[yesql-rationale]:https://github.com/krisajenkins/yesql#rationale
[markup-functions]:https://github.com/hyotang666/markup-functions
[djula-philosophy]:https://mmontone.github.io/djula/djula/Introduction.html#Introduction
[DSL]:https://ja.wikipedia.org/wiki/%E3%83%89%E3%83%A1%E3%82%A4%E3%83%B3%E5%9B%BA%E6%9C%89%E8%A8%80%E8%AA%9E
[cl-who]:https://edicl.github.io/cl-who/
[snooze-rationale]:https://github.com/joaotavora/snooze#rationale
[lack]:https://github.com/fukamachi/lack
[hunchentoot]:https://github.com/edicl/hunchentoot
