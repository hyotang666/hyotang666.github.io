# *Resignal-bind*<br><small>あるいはよりよいエラーメッセージを求めて</small>
## Meta info
### 対象読者
エラーメッセージに対して「分かりにくい！」「で、どうしろと？」などと思った経験のあるCLer。

長文ですのでお暇な時にビールでも片手にどうぞ。

## Introduction
エラーメッセージが分かりにくい大きな理由はスコープの狭さに起因している。
通常コーリー（呼びだされ側）よりコーラー（呼び出した側）の方が多くのコンテクストを保持しているが、それら多くの情報を利用せずコーリーだけにエラー処理をさせていると分かりにくいエラーメッセージが出来上がる。

具体例を見てみよう。
以下のようなコードを考える。

```lisp
(defun something (arg)
  (let((temp(helper arg)))
    (etypecase temp
      (symbol (procedure-for-symbol temp))
      (string (procedure-for-string temp)))))
```

ここでは、コーラーを`SOMETHING`、コーリーを`ETYPECASE`とする。
`ETYPECASE`のスコープからはエラーメッセージの作成には`TEMP`しか利用できない。
例えば`HELPER`からの返り値が整数０だった場合、エラーメッセージは以下のようなものになるだろう。

> 0 is must be one of SYMBOL or STRING.

多くのスタイルガイド等では`TYPECASE`より`ETYPECASE`が推奨されている。
それは半分正しいが半分正しくない。
`ETYPECASE`は使うべきだが使うべきでない。
どういうことかというと、CLAUSEが満たされなかった場合、暗黙裏に`NIL`を返すよりはエラーを発するほうが良いが、多くの場合`ETYPECASE`の第一引数以上の情報（コンテクスト）が周囲に存在するはずなので自前でエラーを書く方が良い。
例えば以下のようになろうか。

```lisp
(defun something(arg)
  (let((temp(helper arg)))
    (typecase temp
      (symbol (procedure-for-symbol temp))
      (string (procedure-for-string temp))
      (t (error "SOMETHING: (helper ~S) is evaluated to be ~S~%It must be one of symbol or string."
                arg temp)))))
```

エラーの原因は次の３つのどれかである。

* おかしな`ARG`が来た結果`HELPER`がおかしな値を返した。
* 正しい`ARG`に対し`HELPER`がおかしな値を返した。
* `TYPECASE`に必要なCLAUSEが抜けている。

いずれの場合であれ前者より後者の方が察しがつきやすい。

*NOTE!* - 上記のエラーメッセージでは`SOMETHING`というプリフィックスをつけてある。
このようなエラーメッセージを作るのは、CLtL2では非推奨とされている。
コンディションが何処から発せられたかの表示はデバッガが担うべき仕事であり、エラーメッセージには加えられるべきではないというのがその論拠である。
しかしながら、肝心のデバッガの振る舞いが処理系依存であり、表示する処理系もあれば表示しない処理系もあり、表示しない処理系に於いて表示させるためにはなんらかのコマンドを叩くというひと手間が必要になる上、その「何らかのコマンド」も各処理系によって異なるという現実に対応するには、たとえ重複して表示されることとなろうともプリフィックスがあったほうが便利だというのが筆者の考えである。
これは好みの問題なので、異論反論は大いにあろうと思われる。

さて、（一画面に収まるという）わかり易さのために、例では`ETYPECASE`というマクロを取り扱ったが、モノが関数になっても理屈は同じである。

例えば`PROCEDURE-FOR-SYMBOL`がエラーを投げるとしよう。
`PROCEDURE-FOR-SYMBOL`の中身は次のようなものとする。

```lisp
(defun procedure-for-symbol(symbol)
  (char(symbol-name symbol)0))
```

本関数は引数にシンボルを期待している。
シンボル以外が引数でくるとエラーとなる。
だが、それはコーラーの`SOMETHING`が`TYPECASE`で場合分けしているので、通常問題ないと思われるかもしれない。
だが話はそんなに甘くない。
これは引数が`||`というシンボルだった場合エラーとなる。
想定されるエラーメッセージは以下のようなものである。

> 0 is invalid index for "".

これは分かりにくかろう。
頑張ってエラーハンドリングするなら以下のようなコードになろう。

```lisp
(defun procedure-for-symbol(symbol)
  (handler-case(char(symbol-name symbol)0)
    (type-error(c)(error c))
    (error()(error "Empty name symbol is invalid. ~S"symbol))))
```

この場合も`PROCEDURE-FOR-SYMBOL`に分かるのは、引数`SYMBOL`が`||`であったということだけである。
そのようなシンボルがどうして渡ってきたのかについては知るよしもない。
そのへんの情報まで取り扱いたいなら、コーラーの側でケアしてあげなければならない。

すると、例えば`SOMETHING`のコードは以下のようなものとなろう。

```lisp
(defun something(arg)
  (let((temp(helper arg)))
    (symbol (handler-case (procedure-for-symbol temp)
              (error()(error "SOMETHING: (helper ~S) is evaluated to be ~S.~%Empty name symbol is invalid."
                             arg temp))))
    (string (procedure-for-string temp))
    (t (error "SOMETHING: (helper ~S) is evaluated to be ~S.~%It must be one of symbol or string."
              arg temp))))
```

さて、仮に`PROCEDURE-FOR-SYMBOL`の中身が次のようなものだとする。

```lisp
(defun procedure-for-symbol(symbol)
  (let((char(handler-case(char(symbol-name symbol)0)
              (type-error(c)(error c))
              (error()(error "Empty name symbol is invalid. ~S"symbol)))))
    (subroutine char)))
```

`SUBROUTINE`もまたなんらかの場合エラーを発するとする。
コーラーの`SOMETHING`から見て、それらのエラーに区別がつかないのはいかにもまずい。
そこで状況に合わせて細かくコンディションを定義し、コーラーから区別がつくようにするのがセオリーである。

以下のコードでは`SOBROUTINE-ERROR`と`EMPTY-NAME-SYMBOL`というコンディションが定義済みであるとする。

```lisp
(defun something(arg)
  (let((temp(helper arg)))
    (typecase temp
      (symbol (handler-case(procedure-for-symbol temp)
                (subroutine-error()(error "Blah blah"))
                (empty-name-symbol()(error "Hoge hoge"))))
      (string (procedure-for-string temp))
      (t (error "Fuga fuga")))))
```

## Issues
前節で見てきたように細かくエラーハンドリングしようとする場合、あるコンディションを受け取って別なコンディションにして投げ直すという処理を書くことが多くなる。
その場合、時にスロットの値を受け継ぎたい事がある。

以下のようなコンディションが定義されているとしよう。

```lisp
(define-condition low-level (simple-type-error)())
(define-condition top-level (low-level)())
```

`LOW-LEVEL`コンディションが発せられた場合、それを補足して`TOP-LEVEL`コンディションに変えて投げ直したいとする。
これは以下のような恐ろしく冗長なコードとなる。

```lisp
(handler-case(something ...)
  (low-level(c)
    (error 'top-level
           :format-control (simple-condition-format-control c)
           :format-arguments (simple-condition-format-arguments c)
           :expected-type (type-error-expected-type c)
           :datum (type-error-datum c))))
```

そこで、この苦痛を少しでも和らげるべく開発されたのが、本記事で紹介する拙作[RESIGNAL-BIND](https://github.com/hyotang666/resignal-bind)である。

## Proposal
前節末尾のコードは`RESIGNAL-BIND`を使用すると以下のように書ける。

```lisp
(resignal-bind((low-lovel()'top-level))
  (something ...))
```

これで「LOW-LEVELコンディションが投げられたら補足して、TOP-LEVELコンディションに変えて投げ直してくれ。 なお、共通するスロットがあったらいい具合に引き継いどいて。」とLispに指示することを意味する。

シンタックスは以下の通り。

```syntax
(resignal-bind (bind*) &body body)

bind := (condition-type-specifier (var?) make-condition-arguments+)

condition-type-specifier := [condition-name | compound-condition-type-specifier]
condition-name := symbol
compound-condition-type-specifier := [(and condition-type-specifier+)
                                      | (or condition-type-specifier+)
                                      | (not condition-type-specifier)]

var := symbol

make-condition-arguments := condition-name-form arguments
condition-name-form := form ; which evaluated to be condition-name
argumetns := {initarg value}*
initarg := keyword
value := T

body := implicit-progn
```
より詳細な仕様については[Specファイル](https://github.com/hyotang666/resignal-bind/blob/master/spec/resignal-bind.lisp)か、同内容の[Github-wiki](https://github.com/hyotang666/resignal-bind/wiki/S_resignalHbind)を参照されたし。

## Conclusion
読者諸兄の中には「そこまで神経質なエラーハンドリングする？」とお疑いの方もいらっしゃる事と思う。
筆者自身からして「ここまで神経質なエラーハンドリングは書かないよねぇ。。？」と思ってもいる。

しかしながら自分が作っているシステムの、自分で書いたエラーメッセージに対して、自分で「わっかんねぇよ！」「で、どうしろと？」「お前どこだよ？」などと思ってしまった時は諦めて神経質なくらい書くことにしている。

これにはメリットもあり、自分の書いたエラーメッセージのおかげでエラー箇所が容易に特定できスムーズにデバッグ等対応出来た場合、「俺スゲェェェ！」と自画自賛でき脳内麻薬がじゅるじゅる出てモチベーションの維持に絶大な効力を発揮することとなる。

## Appendix
### Tips
エラーハンドリングのコードはアルゴリズムそのものとは、極論すれば無関係なものであり、そのようなコードで溢れ返ればコードの見通しが著しく悪くなる。
そのような場合には`MACROLET`が有用である。

例えば以下のように書けば、少しはスッキリすることだろう。

```lisp
(macrolet((!(form)
            `(RESIGNAL-BIND((SUBROUTINE()'ERROR "Blah blah")
                            (EMPTY-NAME-SYMBOL()'ERROR "Hoge hoge"))
               ,form)))
  (defun something(arg)
    (let((temp(helper arg)))
      (typecase temp
        (symbol (!(procedure-for-symbol temp)))
        (string (procedure-for-string temp))
        (t (error "Fuga fuga"))))))
```

なお、筆者の「自分ルール」に於いて、「コンディションを受けてコンディションを投げる」は'`!`'、「NILならコンディションを投げる」は'`?!`'、「コンディションを受けたら`RETURN-FROM`する」は'`!?`'となっている。

また、エラーハンドリングコードを`MACROLET`を利用してメインロジックの外側に出してしまうというアプローチは`SPLIT-SEQUENCE`のソースで初めて出会って以降、好んで真似させてもらっている方法である。

### Behavior of SIGNAL
`SIGNAL`はコンディション指定子を受け取り、ハンドラがあるか探し、ハンドラが有ればコールし、なければ黙って`NIL`を返すというものである。

```lisp
(signal 'error) => NIL
```

多くの処理系では上記のようにトップレベルで`SIGNAL`を呼べば`NIL`が返る。
ただし、そうでない処理系もある。
具体的には（僕の知る限りでは）ECLがそうである。
ECLで上記フォームを評価するとデバッガに入る。

ではECLは仕様に反しているのか？
そうとは言えない。
というのも仕様は「ハンドラが無ければ`NIL`を返す」と言っているだけであり、「トップレベルにはけしてハンドラはない」とはどこにも書かれていないからである。

