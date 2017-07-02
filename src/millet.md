# *Millet*<br><small>あるいはユーティリティ落ち穂拾い</small>
## Meta info
### 対象読者
処理系依存の小さな機能をポータブルに使いたいCLer

## Introduction
Common Lispの言語仕様は非常に巨大なものだが、それでも実際にコードを書こうとすると様々な機能が足りないこととなる。
それらは処理系独自拡張として提供される。
もちろん処理系独自のものであり、可搬性はない。

そこで、大きな機能については可搬的に書けるようにするためのラッパライブラリが存在する。
たとえばFFIを可搬的にするCFFI、ソケットを可搬的にするUSOCKET、GRAY-STREAMSを可搬的にするTRIVIAL-GRAY-STREAMS、シェルへのアクセスを可搬的にするUIOP:RUN-PROGRAM、WEAK-POINTERを可搬的にするTRIVIAL-GABAGE、マルチスレッドを可搬的にするBORDEAUX-THREADS、MOPを可搬的にするCLOSER-MOPなど。

ところで処理系が独自に提供するものは何も大きな機能ばかりではない。
小さなユーティリティもまた、処理系独自拡張として提供されていることが多い。

Common Lispでユーティリティを扱うライブラリといえばALEXANDRIAだろう。
QUICKLISP経由で最もインストールされているライブラリでもある。
そのALEXANDRIAはポリシーとして

* Public domainである。
* 100% Common Lispである。

の２つを謳っている。

## Issues
よってユーティリティと見做せる充分小さな機能であっても、処理系独自コードに依存しているものはALEXANDRIAには含まれない。
いや、ALEXANDRIAのみならず、多くのユーティリティライブラリ群に含まれていない。
それらの機能は必要とするシステムがそのシステム毎にラッパ関数を書いているのが現状だ。

## Proposal
そこで、そのような小さなどのユーティリティライブラリ群からも顧みられない可哀想な関数群をまとめて可搬的にできるよう務めているのが、本記事で紹介する拙作[millet](https://github.com/hyotang666/millet)である。

## API
### function-name
関数を取り、関数名（シンボル）を返す。

```lisp
(function-name #'car) => CAR
```

### lambda-list
関数を取り、ラムダリストを返す。

```lisp
(lambda-list #'car) => (list)
```
### global-symbol-p
シンボルを取り、グローバルなシンボルかテストする。
定数を含む。

```Lisp
(global-symbol-p '*package*) => T
(global-symbol-p :hoge) => T
```

### special-symbol-p
シンボルを取り、スペシャルシンボルかテストする。
定数は含まれない。

```lisp
(special-symbol-p :hgoe) => NIL
```

### type-expand
型を展開させる。
展開された場合第二値がTとなる。
さもなくばNIL。

```lisp
(deftype function-name ()
  '(or symbol function))
(type-expand 'function-name) => (or symbol function) ; T
```

### type-specifier-p
引数が型指定子かどうかテストする。

```Lisp
(type-specifier-p 'function-name) => T
```

## Conclusion
自分が必要としたものしか加えてないので現時点ではこれだけ。
振る舞いの詳細に関しては[Spec file](https://github.com/hyotang666/millet/blob/master/spec/millet.lisp)か同内容の[Github-wiki](https://github.com/hyotang666/millet/wiki/P_MILLET)を参照されたし。

## Appendix
### Tips for read time conditionalization
リード時に環境に応じて読み込むS式を変更するには通常'`#+`', '`#-`'というディスパッチマクロを利用する。

```lisp
#+clisp
(print "This form is evaluated only in clisp")

#+(or sbcl ecl)
(print "This form is evaluated in sbcl or ecl")

#-(or clisp sbcl ecl)
(print "This form is not evaluated in clisp, sbcl or ecl.")
```

ただし、これには少々厄介な問題がつきまとう。
以下は実践Common Lispからの引用。

> 読み込み時条件分岐の動作でちょっと頭が痛いのは、通過しない条件を書く簡単な方法がないことだ。
> 例えば、fooに別な処理系のサポートを追加するのに`#+`で別な式を追加した場合、同じ機能を`#-`に続く`or`の機能式にも忘れずに追加しなければならない。
> さもないと新しい式が実行された後に`ERROR`フォームが評価されてしまう。

そこで、僕自身はちょいとひと工夫したコードを書いている。
それは以下のようなものだ。

```lisp
#.(or
    #+clisp `(print "Only in clisp")
    #+(or sbcl ecl) `(print "Only in sbcl or ecl")
    `(print "Other implemantations"))
```
この書き方だと`'#-'`を書く必要がない。
最後の節が自動的に規定値となってくれるからだ。

clisp上ではリード時に以下のコードが評価され、、、

```lisp
(or `(print "Only in clisp")
    `(print "Other implementations"))
```

その結果以下のコードがソースに埋め込まれる。

```lisp
(print "Only in clisp")
```

clispでもECLでもsbclでもない処理系では、リード時に以下のコードが評価され、、、

```lisp
(or `(print "Other implementations"))
```

その結果以下のコードがソースに埋め込まれるというわけだ。

```lisp
(print "Other implementations")
```

ただこの書き方も「クォートしなければならない」という欠点がある。
やはり銀の弾丸はないということか。
