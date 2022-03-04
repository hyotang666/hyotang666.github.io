# Restarter or tiny tips for restart feature in common lisp.

## Meta notes.

### 対象読者
* Common Lispの[restart]機構に関心のある方。

# Confirmation.

## Typical restart.
典型的な[restart]は、例えば以下のようなものになるかと思われます。

```lisp
(restart-case (the-form-that-may-fails)
  (ignore () nil))
```

## Typical restart function.
上の[restart]をプログラムから呼び出したい場合、[`handler-bind`]を使用して[`condition`]に紐づけたハンドラ内で[restart]を探して呼び出すという処理を行うと思います。

```lisp
(handler-bind ((error (lambda (condition)
                        (let ((restart (find-restart 'ignore condition))) ; <--- restartを探して、
                          (when restart                                   ; <--- もしあったら、
                            (invoke-restart restart))))))                 ; <--- 呼び出す。
  ...)
```

関数にしてしまう事例も多かろうと存じます。

```lisp
(defun ignore (condition)
  (let ((restart (find-restart 'ignore condition)))
    (when restart
      (invoke-restart restart))))
```

# Issue.
## Package lock violation.
処理系によってはパッケージはロックされていたりします。
上のリスタート関数`IGNORE`の定義は少なくとも[SBCL]においてはパッケージがロックされているためエラーとなります。

ましてや[restart]の名前を[`delete`]や[`replace`]にしたい場合、Common Lispの通常の関数名と衝突するのでそのような名前のリスタート関数は定義できません。

## Increasing exported symbols.
名前衝突を回避しようと試みる場合、リスタート名が不自然なものとなったり過剰に長くなったりするおそれがあります。

また、それらの名前は通常エクスポートされるものです。
エクスポートされるシンボルが増えるのは（将来の自分を含む）エンドユーザーにとって学習コスト増という負担になります。

# Proposal.
## Restarter.
典型的なリスタート関数はどうせ同じ処理を行うのですからリスタート関数を作って返す関数を定義すれば良いのではないでしょうか。

```lisp
(defun restarter (restart-name &rest args)
  (lambda (condition)
    (let ((restart (find-restart restart-name condition)))
      (when restart
        (apply #'invoke-restart restart args)))))
```

上記関数`RESTARTER`さえあればリスタート関数を定義する必要はなくなります。
先の例を`RESTARTER`を使う形で書き直すと以下のようになります。

```lisp
(handler-bind ((error (restarter 'ignore)))
  ...)
```
## Keyword symbol as a restart name.
リスタート名はシンボルである必要がありますが、それはただのIDとしてしか使われません。
なら、キーワードシンボルでも構わないとは思いませんか？

キーワードシンボルを使えばパッケージからエクスポートされるシンボルを減らせます。
それはそのまま（自分を含む）エンドユーザーの学習コストの減少に直結します。

たとえば[babel]のように、たとえ頻繁に使うシンボルは[`string-to-octets`]と[`octets-to-string`]の２種だとしても、パッケージからエクスポートされているシンボルが合計３３個もあると、もうその時点で「うげぇ」となって学習に対するモチベーションが下がることがあるのは賢明な読者の皆様におかれましてはご承知のとおりのことと存じます。
「学習コストの減少」にはそのような心理的抵抗（苦手意識）の減少も重要であると考えます。

Common Lispの仕様がリスタートには対応するリスタート関数が定義されてあるものという設計になっているのが、ミスリーディングしているように思えます。
上に見たように`RESTARTER`関数さえあればリスタート関数群は必要ありません。

なお、賢明な読者諸兄の中にはキーワードシンボルを使うと複数の異なるライブラリ間でリスタート名の衝突が起こりうる点を懸念される御仁もいらっしゃるかもしれません。
ですがリスタートはリスタート名のみで求まるものかというとさにあらず、[`condition`]とセットで求まるものでございます。
[`condition`]を適切に定義すればたとえリスタート名が衝突してもリスタート節自体は適切に選択しうるものと愚考する次第でございます。

# Conclusion.
[`muffle-warning`]のようにリスタートが存在しないなら[`program-error`]とするようなものにも対応するためにはやはりリスタート関数自体を自前で書く必要は残りますが、それでも多くの場合は`RESTARTER`関数で対応できるかと思われます。

[alexandria]あたりが導入してくれると嬉しい。

<!-- Links -->
[restart]:http://www.lispworks.com/documentation/HyperSpec/Body/09_adb.htm
[`handler-bind`]:http://clhs.lisp.se/Body/m_handle.htm
[`condition`]:http://clhs.lisp.se/Body/e_cnd.htm
[SBCL]:http://www.sbcl.org/
[`delete`]:http://clhs.lisp.se/Body/f_rm_rm.htm
[`replace`]:http://clhs.lisp.se/Body/f_replac.htm
[babel]:https://github.com/cl-babel/babel
[`string-to-octets`]:https://github.com/cl-babel/babel/blob/master/src/strings.lisp#L251-L301
[`octets-to-string`]:https://github.com/cl-babel/babel/blob/master/src/strings.lisp#L221-L235
[`muffle-warning`]:http://www.lispworks.com/documentation/HyperSpec/Body/f_abortc.htm
[`program-error`]:http://clhs.lisp.se/Body/e_progra.htm#program-error
[alexandria]:https://gitlab.common-lisp.net/alexandria/alexandria
