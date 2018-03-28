# Profiling issue in SBCL.
## Meta info
### 対象読者
関数のプロファイルに力を入れているSBCLユーザ。

## Introduction
SBCLには強力な、プロファイル用のオペレータ群が存在する。
これは充分強力で、通常不満を覚えることはない。
すなわち、通常でない場合には不満を覚えることもあるという意味だ。
本稿では筆者が出会った「通常でない場合」とその不満点、解決策として開発した成果物/その使い方、使用上の注意点、今後の課題、最後に実装に際しての裏話などについて述べる。

## Issue
筆者がやりたかったのは「「関数を返す関数」が返した関数のプロファイルを取る」である。
ここではわかり易さのため、「関数を返す関数」のことを「ファクトリ」、ファクトリが返した関数のことを「インスタンス」と呼ぶこととする。
すなわち、筆者がやりたかったことは「インスタンスのプロファイルを取る」である。
`SB-PROFILE:PROFILE`はファクトリのプロファイルこそ取れるものの、インスタンスのプロファイルを取ることは出来ない。

## LAMBDA-METER
そこで、それを可能たらしむる小さなライブラリを作った。
名を[`LAMBDA-METER`](https://github.com/hyotang666/lambda-meter) という。

使用に際してはリードテーブルの変更が推奨される。
`LAMBDA-METER`は[`NAMED-READTABLES`](https://github.com/melisgl/named-readtables) をサポートしているので、それを使うか、さもなくば`ENABLE`関数で`*READTABLE*`を変更させればよい。

```lisp
(named-readtables:in-readtable lambda-meter:syntax)
;; or
(lambda-meter:enable)
```

さて、関数を返す関数の例といえば`ADDER`だろう。
以下のような関数である。

```lisp
(defun adder(x)
  (lambda(y)
    (+ x y)))
```

ファクトリ関数`ADDER`が返すインスタンスのプロファイルを取りたい場合、以下のような一行を追加するだけで良い。

```lisp
(defun adder(x)
  #M adder	; <--- This!
  (lambda(y)
    (+ x y)))
```

`LAMBDA-METER`が提供するディスパッチリーダマクロは唯一`#M`である。
これは`#+`、`#-`ファミリーと同様にS式を２つ消費する。

ひとつ目はインスタンスに紐付けられるIDで、これはシンボルでなければならない。
評価はされない。

ふたつ目はプロファイルされるべき関数オブジェクトを生成するフォームである。
よって上記例は以下のように書いても機能する。

```lisp
(defun adder(x)
  (flet((adder(y)
          (+ x y)))
    #M #:adder #'adder))
```

こうするだけで、後は`SB-PROFILE`が提供するAPI群を普通に使えばよい。

```lisp
* (defparameter 2+ (adder 2))
2+

* (defparameter 3+ (adder 3))
3+

* (funcall 2+ 2)
4

* (funcall 3+ 2)
5

* (sb-profile:report)

  seconds  |     gc    | consed | calls |  sec/call  |  name
---------------------------------------------------
     0.000 |     0.000 |      0 |     2 |   0.000000 | ADDER
---------------------------------------------------
     0.000 |     0.000 |      0 |     2 |   0.000000 | Total

estimated total profiling overhead: 0.00 seconds
overhead estimation parameters:
  3.2000003e-8s/call, 9.855999e-6s total profiling, 4.4159997e-6s internal profiling
```

## NOTE
前節の例ではファクトリ自体のプロファイルは取っていなかったので、IDは単にファクトリの名前を使っていたが、ファクトリ自体のプロファイルも取りたい場合、名前の衝突を避ける必要がある。

好きな名前を使えば良いが、ここではアンインターンドシンボルを使うことを推奨しておく。
（根拠は、1.余計な名前を考えなくてよい。2.ファクトリ名と充分区別がつく。3.パッケージを汚さない。等である。）

ただ、IDにファクトリ名以外のシンボルを使う場合、一つだけ注意点がある。
`SB-PROFILE:UNPROFILE`がエラーを投げるようになってしまうのだ。
この問題に対処するため、`LAMBDA-METER`は独自の`UNPROFILE`を提供している。
「独自の」と言っても、中身は単なる`SB-PROFILE:UNPROFILE`へのラッパに過ぎない。
APIは`SB-PROFILE:UNPROFILE`と全く同じなので、学習コストは皆無である。

## Task?
現行の実装では各インスタンスは全て同一のIDで管理されている。
上記の例で言えば`2+`を呼ぼうが`3+`を呼ぼうが「`ADDER`が呼ばれた」と数えている。
将来的には、もしかしたら、各インスタンス毎にプロファイルを取り分けたいと思うことがあるかもしれない。

リーダマクロが`#M`なのは"Meter"の頭文字から。
本当は"Profile"から採りたかったのだが`#P`は既にパスネームに使われてしまっているので。
[`CL-ANNOT`](https://github.com/m2ym/cl-annot) を採用して`@profile`とかしたほうがスマートだったかもしれない。
需要があるようならプラグインとして提供するようにしてもいいかもしれない。
現時点では自分しか使わないのでこれでよしとしている。

"Meter"という言葉は、ライブラリ[`METERING`](http://quickdocs.org/metering/) から。
`METERING`はポータブルなプロファイラーで、[`SLIME`](https://github.com/slime/slime) が持っているパッケージを独立させたものらしい。

## sb-profile
割と素朴な実装だったので[ソース](https://github.com/sbcl/sbcl/blob/master/src/code/profile.lisp) を読むのはそこまで苦痛ではなかった。
グローバルにハッシュテーブルがポツンとあり、そこに関数名をキーに`PROFILE-INFO`オブジェクトをバリューにして登録する。
プロファイルされる関数は、パッケージをアンロックして`FDEFINITION`を書き換える。
新しい`FDEFINITION`は単なるラッパで、処理時間やコンシングに関する内部状態を管理しつつ本来の関数を呼びだす。

`RESET`はハッシュテーブルをマップしていくだけ。

`REPORT`はソートが含まれるが、適したオブジェクトさえハッシュテーブルに入っていればノータッチでおｋ。

`UNPROFILE`はラッパを捨てて`FDEFINITION`に本来の関数を登録し直すだけ。
ちなみにこれがエラーの原因。

## Conclusion
関数を返す関数を実装することは、頻度としてはあまりない事と思う。
ましてや返された関数のプロファイルを取りたいと思うことをや。

しかしながら、[`OPTIMA`](https://github.com/m2ym/optima) や[`ESRAP`](https://github.com/nikodemus/esrap) の例に見られる通り、遅延評価やカリーイングを採用したHaskellに代表されるような関数型言語で実装されたアルゴリズムをCommon Lispに輸入しようという場合、いきおいファクトリが大量に必要になる場合もある。
それで充分速度が出ていれば（プロファイルを取る必要がなければ）問題とはならないのだが、そうでない場合には`LAMBDA-METER`が助けとなろう。

そうでなくとも、例えば[`CLACK`](https://github.com/fukamachi/clack) を使っているユーザなど、特定のクロージャ（無名関数）のプロファイルを取りたい場合は他にも考えられるだろう。
