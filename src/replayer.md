# Common Lisp製Audio File Player "REPLAYER" の紹介。
## Introduction.
Twitterで[このようなやりとり](https://twitter.com/KTakahiro1729/status/1255761609563111427)があってですね。
残念ながらCommon Lispは選ばれなかったっぽいので見切り発車で勝手にCommon Lisp製のAudio File Playerを作り始めてみた訳です。

一通り必要とされているらしい機能は実装できたぽいので簡単に紹介だけ。

## [REPLAYER](https://github.com/hyotang666/replayer)
名前は「REPL」での「PLayer」から。
特に深い意味はありません。

## Installation.
俺々ライブラリを使っているのでroswellでのインストール推奨です。

[roswellのインストールはこちら](https://github.com/roswell/roswell/wiki/Installation)

またデータベースとして[sqlite3](https://sqlite.org/index.html)を使用しているのでそちらも各自インストールしてください。

roswellとsqlite3をインストールし終わったら以下のコマンドを叩きます。

```shell
$ ros install hyotang666/r-iff hyotang666/wav-parser hyotang666/replayer
```

これで準備はおｋ。

## Inside REPL.

quicklispでロードできます。

```lisp
* (ql:quickload :replayer)
```

後は再生したいファイルを関数`PLAY`に渡せばいいだけ。

```lisp
* (replayer:play "~/Music/path/to/file")
```

現時点では`wav`と`mp3`のみサポートされてます。

## [Play list](https://twitter.com/KTakahiro1729/status/1255740954679496704)
`PLAY`は総称関数として実装されています。
リストを渡せば順繰りに再生してくれます。

```lisp
* (replayer:play (uiop:directory-files "~/Music/" "*.wav"))
```

## [Saving play list](https://twitter.com/KTakahiro1729/status/1255740954679496704)
ファイルへのパスはpathnameオブジェクトでも文字列でもOkです。
保存したい場合は以下のようにすると取り回しが楽になるでしょう。

```lisp
* (with-open-file (s "play-list" :direction :output :if-not-exist :create)
    (format s "~{~A~%~}" (uiop:directory-files "~/Music/" "*.wav")))
```

保存したプレイリストを再生したい場合は`UIOP:READ-FILE-LINES`が便利に使えます。

```lisp
* (replayer:play (uiop:read-file-lines "play-list"))
```

保存されたプレイリストはただのtxtファイルなので共有もできますし（ディレクトリ構造が同じであれば。）表計算ソフトで開くこともできます。

## [Tagging](https://twitter.com/KTakahiro1729/status/1255751612691169280)
`TAG`関数を使えばファイルとタグとを紐付けることができます。

```lisp
* (tag "tag" (uiop:directory-files "~/Music/" "*.wav"))
```

### [Tagging example](https://twitter.com/KTakahiro1729/status/1255752412553338880)

```lisp
* (loop :for tag :in (remove "" (ppcre:split "(\\()|(\\))|(,)|(&)|( )" "原村和(小清水亜美), 宮永咲(植田佳奈), 染谷まこ(白石涼子), 片岡優希(釘宮理恵) & 竹井久(伊藤静)"))
        :do (tag tag "~/Music/四角い宇宙で待ってるよ"))
```

## [Filter](https://twitter.com/KTakahiro1729/status/1255753010019356673)

正規表現での検索には現時点では対応していませんが論理式によるタグ検索は可能です。
タグによる検索＆再生には中間オブジェクトとして`TAG`オブジェクトを作成します。

```lisp
* (replayer:play (replayer:make-tag :exp '(and "Death-metal" (not "America"))))
```

上記例ではDeath-metalというタグがついている音楽を再生しますがAmericaというタグがついている音楽は再生されません。

## Shuffle

変数`*shuffle*`はシャッフル再生するか否かを制御します。
既定値は`NIL`です。

## Repeat

変数`*repeat*`はリピートモードを制御します。
`:one`、`:all`、`NIL`のいずれかが入ります。
既定値は`NIL`です。

## Stop
停止させるには`STOP`を呼びます。

```lisp
* (replayer:stop)
```

## Skip
次の曲へ飛ばすには`SKIP`を呼びます。

```lisp
* (replayer:skip)
```

## 紹介を終えて。
ros-scriptをサポートしているのでシェルからの操作もできますが、その紹介はまた気が向いたら。
（[README](https://github.com/hyotang666/replayer) に書いてあります。）

まだまだ機能は足りないとは思うのですが、まぁとりあえずはこんなところで。

自分で使うぶんには割と満足しているので、このあと何をしたらいいのかよく分かってない。
