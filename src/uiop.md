# To query OS distribution with Common Lisp especially UIOP.

## Introduction

[このような記事](https://komi.hatenadiary.com/entry/2020/04/05/235243)を読み、脊髄反射で[このように返信](https://twitter.com/hyotang666/status/1246819542946304000)してしまったのだけど、よく読んでみたら`UIOP:RUN-PROGRAM`要らなくね？ってなったので、自分ならこう書くかなってのをメモ程度に書きとどめておくと同時に`UIOP`の各種機能を紹介していこうと思う。

## 対象読者。

* 元記事作者様。
* `UIOP`？なにそれ？おいしいの？っていう初級CLer。

## Useful UIOP functions.

僕が書くなら`GET-DIST`は以下のようになる。

```lisp
(defun get-dist ()
  (loop :for file :in (uiop:directory-files "/etc/" "*-release")
        :do (loop :for line :in (uiop:read-file-lines file)
                  :if (uiop:string-prefix-p "ID=" line)
                  :do (return-from get-dist (subseq line 3)))))
```

イカれたメンバー達を紹介しよう！

### UIOP:DIRECTORY-FILES

第一引数で指定されたディレクトリにあるファイルpathnameをリストでくくって返すぜ！
オプショナルな第二引数にパターンを渡せばそのパターンにマッチするファイルのみを返してくれるぜ！
ここで言う「ファイル」はいわゆるファイルでディレクトリはファイルに含まれないぜ！

```lisp
* (uiop:directory-files "/etc/" "*-release")

(#P"/etc/lsb-release" #P"/etc/os-release")
```

### UIOP:READ-FILE-LINES

pathnameを受け取ってその各行をリストにくくって返すぜ！

```lisp
* (uiop:read-file-lines (car *))

("DISTRIB_ID=Ubuntu" "DISTRIB_RELEASE=18.04" "DISTRIB_CODENAME=bionic"
 "DISTRIB_DESCRIPTION=\"Ubuntu 18.04.4 LTS\"")
```

このファミリーとして、ファイルの最初の行だけを返す`UIOP:READ-FILE-LINE`、ファイル内の各S式をリストにくくって返す`UIOP:READ-FILE-FORMS`、最初のS式だけを返す`UIOP:READ-FILE-FORM`、ファイルの内容を文字列として返す`UIOP:READ-FILE-STRING`などがあるぜ！

なかでも`UIOP:READ-FILE-STRING`はプロジェクトのREADMEの中身をASDFのLONG-DESCRIPTIONとして読み込むのに使われたりしているから要チェックだ！

### UIOP:STRING-PREFIX-P

第二引数に渡した文字列指定子が第一引数で指定したプリフィックスで始まっているかテストするぜ！

```lisp
* (uiop:string-prefix-p "ID=" (car *))

NIL
```

元記事では`(CL:SEARCH "ID=" i)`という形でテストしてあったけど、`CL:SEARCH`は文字列に含まれるか否かをテストするものだから、元記事の文脈には沿わないバグとなっているぜ！
現に僕の環境で元記事の`GET-DIST`は以下のような返り値となるぜ！

```lisp
* (get-dist)

"TRIB_ID=Ubuntu"
```

これは期待とは異なる振る舞いのはずだ。
僕の環境では"ID="より先に"DISTRIB_ID="が先に現れ、これは`(CL:SEARCH "ID=" ...)`を満足させるので結果このような振る舞いになってしまっているんだ。

せめて`CL:SUBSEQ`に渡す３をハードコーディングせず以下のようにしていればよかったのだけれど。

```lisp
(defun get-dist ()
  (let ((os-data (split (string #\Newline)
                        (system "cat /etc/*-release"))))
    (loop :for i :in os-data
          :for position := (search "ID=" i)
          :if position
          :do (return (subseq i (+ position 3))))))
```

とはいえ上記のコードもけしてパーフェクトとは言い難く、というのも、例えば僕の環境には他に"VERSION_ID=\"18.04\""なんて行もあるからだ。
まがり間違ってこの行が先に現れた場合、返り値は"18.04"となる。
`GET-DIST`という関数の返り値としてこれは不適切だろう。

でも`UIOP:STRING-PREFIX-P`を使えばこんなバグとはおさらばだ！

ちな、ファミリーとして`UIOP:STRING-SUFFIX-P`もあるゾ！

### UIOP:SPLIT-STRING

元記事では`SPLIT`関数が作られてるけど、`UIOP`はすでに同じものをもっているぜ！

使い方はだいたい以下の通りだ！

```lisp
* (uiop:split-string string :separator #.(string #\newline))
```

## TRIVIAL-FEATURES

`CL:*FEATURES*`に入っている値は処理系依存で、これは割とやっかいな問題だ。
MACであるかどうかをある処理系は`:DARWIN`で表し、ある処理系は`:MACOS`で表し、またある処理系は`:MACOSX`で表したりしている。
それは困るということで、ある程度`CL:*FEATURES*`の中身をポータブルにしようという試みがある。
それが`TRIVIAL-FEATURES`だ！

ちな、`TRIVIAL-FEATURES`は世にも珍しい`PACKAGE`を作らないライブラリだ！

## Conclusion.

見てきたように`UIOP`は多くの便利関数を提供してくれているぜ！
他のライブラリと違って`UIOP`は`ASDF`が提供しているものであり、`ASDF`は多くの処理系にバンドルされているものであり、すなわち`UIOP`はインストールしなくても使える（場合が多い）ぜ！

一つ一つの機能は小さく（比較的）把握しやすいので、一通り入門記事は読み終えたけど次は何をしようかな？と迷っているような初級CLerが読み始めるにはおすすめのライブラリだ！

機能自体は小さいのに処理系可搬性のためにクソデカコードになってしまっている関数もあり、その労力には涙と感謝を禁じ得ないぜ！

最後に注意点を。

`UIOP`のコードは読みやすさやメンテナンスのしやすさが重要視されているようで（推測）、効率を追求する場合は他のライブラリを使ったほうがいい場合がある。

例えば`UIOP:WHILE-COLLECTING`はいわゆる`PUSH/REVERSE`（`NREVESRSE`ですらない！）に変換されるマクロだ。
効率を求めるなら`TCONC`で実装されている`CL-UTILITIES:WITH-COLLECTORS`の方がいいだろう。

また、`UIOP`はあくまで`ASDF`のためのものだ。
幾つかのAPIは`ASDF`から使いやすいやすいように実装されている。

例えば上記`UIOP:SPLIT-STRING`はキーワード引数`MAX`を受け付ける。
その振る舞いは何も知らなければ若干奇妙だ。

```lisp
* (uiop:split-string "ototo" :separator "t" :max 2)

("oto" "o")
```

先頭からではなく後ろから切り分ける振る舞いとなっている。
これはファイル名"hoge.fuga.piyo"をファイル名と拡張子とに切り分けるのに使われたり、package-infered-systemのsystem名"hoge/fuga/piyo"からコンポネント名（ここでは"piyo"）を切り分けたりするのに使われるためのものだ。

こういった恣意的なAPIを快く思わない場合も別なライブラリが魅力的に見えることとなろう。

それでも`UIOP`はめちゃくちゃ便利だ。
`ENSURE-LIST`や`IF-LET`のためだけに巨大な`ALEXANDRIA`に依存したくないなんて場合も`UIOP`が使えるぜ！（どちらも`UIOP`にあるのさ！）

さぁ君も`UIOP`をマスターして一歩上のCler（謎）になろう！
