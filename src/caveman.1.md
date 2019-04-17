# Caveman kills ruby on rails - Chapter 1
## Meta info
### 対象読者
* 他人の環境に興味のあるCLer

## Introduction
本稿は[原著](https://book.impress.co.jp/books/1117101135)の各章をCommon Lispに翻訳するシリーズの第一章である。
本章では開発環境構築とcavemanの簡単な導入紹介を行う。

## Cavemanの概要
### Cavemanとは
Common Lisp製のWebアプリケーション用フレームワークである。

## Caveman開発環境の構築
### OS
筆者のお気に入りのDAW([samplitude](https://www.magix.com/us/music/samplitude/))はwindowsしかサポートしてないので、OSはwindowsである。
### IDE
windows最強のIDEは[VMware](https://www.vmware.com/products/workstation-player.html)である（次点で[VirtualBox](https://www.virtualbox.org/)）。
筆者はこれに[lubuntu](https://lubuntu.net/)というプラグインを入れて使っている。
### roswell
処理系の管理に[roswell](https://github.com/roswell/roswell)を使用する。
インストール方法は[ここ](https://github.com/roswell/roswell/wiki/Installation)を参照。
lubuntuで`ros setup`を行うと[エラーを吐く](https://github.com/roswell/roswell/issues/351)がインストール自体は無事終わっているらしく（おそらくは）問題ない。
### Editor
筆者は[vim](https://www.vim.org/)を愛用している。
vimとLisp処理系との連携には[vlime](https://github.com/l04m33/vlime)がある。
が、筆者はより原始的な方法を取っている。
[tmux](https://github.com/tmux/tmux/wiki)で端末画面を分割し、[rlwrap](https://github.com/hanslub42/rlwrap)で処理系を直接バチバチ叩くというものだ。

```shell
rlwrap -m ros run
```
これでカーソルキーによる、入力のヒストリ参照ができる。
複雑なコマンドの入力には不向きかもしれないが、Ctl+^でEditorが立ち上がるので、それでなんとかする。

このアプローチのメリットは複雑な操作を覚える必要がないことだ。
デメリットは処理系を直接叩いていることだ。
すなわちリモートの処理系にアクセスするというのができない。
その必要がでてきたなら、vlimeなり[lem](https://github.com/cxxxr/lem)なりを覚えようかと思う。
### caveman
REPLを立ち上げたら[caveman](http://8arrow.org/caveman/)をインストールする。

```lisp
(ql:quickload :caveman2)
```

## アプリケーションの新規作成
以下のコマンドでプロジェクトスケルトンを作る。

```lisp
(caveman2:make-project #P"~/roswell/local-projects/your-app" :author "Your Name")
```
your-appにはお好きな名前をどうぞ。
本シリーズではyou-appで通す。
パスも任意の作りたいところで大丈夫。
ここでは"~/.roswell/local-projects/"に作ることとする。

## Cavemanを動かしてみよう。
アプリケーションはスケルトンが作られただけなので、REPLにロードする必要がある。

```lisp
(ql:quickload :your-app)
```
サーバーを立ち上げるには以下のようにする。

```lisp
(your-app:start)
```
これでlocalhost:5000に適当なブラウザでアクセスすれば良い。

サーバーを終了するには以下のコマンドを評価する。

```lisp
(your-app:stop)
```

## ルーティングの設定
ルーティングはsrc/web.lispに記述する。
デフォルトで以下のコードが書かれている。

```lisp
;;
;; Routing rules

(defroute "/" ()
  (render #P"index.html"))
```
`#P"index.html"`の所在だが、これは`your-app.config:*template-directory*`を参照している。
ここでは~/.roswell/local-projects/your-app/templates/がそれに当たる。

## ビューの作成
`#P"index.html"`の中身を以下のように編集する。

```html
{% extends "layouts/default.html" %}
{% block title %}Welcome to Caveman2{% endblock %}
{% block content %}
<h2>Hello</h2>
<p>Here we go.</p>
{% endblock %}
```

ブラウザをリロードすると変更されているはずだ。

## 変数の表示
上述のdefrouteを以下のように変える。

```lisp
(defroute "/" ()
  (render #P"index.html" '(:message "Hi!")))
```
その上で`#P"index.html"を以下のように変える。

```html
{% extends "layouts/default.html" %}
{% block title %}Welcome to Caveman2{% endblock %}
{% block content %}
<h2>{{message}}</h2>
<p>Here we go.</p>
{% endblock %}
```
htmlファイルの変更はブラウザのリロードだけで反映されたが、Lispソースを改修した場合はプロジェクト自体をリロードしないと変更は反映されない点要注意。

```lisp
(ql:quickload :your-app)
```
プロジェクトのリロード後にブラウザをリロードすれば変更が反映されているはずだ。

## まとめ
* Cavemanは、プログラミング言語Common Lispによるウェブアプリケーション開発のためのフレームワークです。
* Cavemanをインストールするには、roswellをインストールしてから、QL:QUICKLOADでcaveman関連のシステムをインストールします。
* REPLでCAVEMAN2:MAKE-PROJECT関数を評価すると、アプリケーションの骨格を作成できます。
* Cavemanアプリケーションは、ウェブサーバHUNCHENTOOTを動かしながら開発します。
* web.lispにDEFROUTEを追加し、対応するテンプレートを記述するとウェブページができます。
