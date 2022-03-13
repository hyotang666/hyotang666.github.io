# Related-methods

## Introduction.
ツイッターにて、以下のような[thread]がありました。

> 最近のコーディングは型推論の情報によるエディタの支援がすっごい優秀で助かるのだけど、あの行き当たりばったりでなんとかなっちゃう快適性を前置記法のLisp系言語に持ち込めないだろうか…。

> 関数の戻り値がオブジェクトだった時に、ライブラリのドキュメントを漁って「どういうオブジェクトを返しているのか？」「そのオブジェクトに対してどんな操作ができるのか？」を真面目に調べないといけないのが辛い。

以下、Common Lispに限定して話を進めます。

## Can do or not can do?
できるかできないかの話だけで考えるとできないと思います。

### Rationale.
Common Lispがその言語仕様として採用したオブジェクトシステムであるCLOSはC++に代表されるようなオブジェクトシステムがクラスベースすなわちクラスがメソッドを管理する設計であるのと異なり総称関数がメソッドを管理する設計となっております。
総称関数名をタイプしたときにどのようなメソッドがあるか/その総称関数はどのような引数の型を期待しているのかを表示するのはできるでしょうが、それはそもそも総称関数（その存在）を分かっている/知っている場合の話で、つまり、'関数の戻り値がオブジェクトだった時に、〜中略〜「そのオブジェクトに対してどんな操作ができるのか？」を真面目に調べないといけない'のは変わらないでしょう。

## Goods and bads.

### Goods.
'関数の戻り値がオブジェクトだった時に、ライブラリのドキュメントを漁って〜中略〜真面目に調べないといけない'部分は`cl:describe`や`cl:inspect`を使ってLispに直接尋ねれば事足りる場合も多かろうとは存じます。

### Bads.
'関数の戻り値がオブジェクトだった時に、〜中略〜「そのオブジェクトに対してどんな操作ができるのか？」を真面目に調べないといけない'の部分は素のCommon Lispでは対応できない部分でしょう。

### Alternative.
まぁ、無いなら作ればいいだけの話ではあります。
ツイ主さんがおっしゃる'行き当たりばったりでなんとかなっちゃう快適性'にはほど遠いでしょうが（というのもその'快適性'はシンタックスに負うところが大きく、変数宣言時にまずクラス名を書くような言語ではその時点でメソッドのリストが得られ表示できるでしょうが、まず関数名を書かねばならないCommon Lispではそうとはいかないからです。）幸いCommon LispではインクリメンタルにREPLと対話しながら開発するものですので'関数の戻り値がオブジェクトだった時'にオブジェクト名から関連する総称関数をリストアップできれば'ライブラリのドキュメントを漁って〜中略〜真面目に調べないといけない'つらみは軽減されるのではないでしょうか。

## The fruit.
というわけで出来上がったのがこちら`RELATED-METHODS`でございます。

### Usage.

必須引数として型名を渡すとカレントパッケージがエクスポートしているシンボルを舐めて関連している総称関数名をリストアップしてくれます。

```lisp
* *package*
#<PACKAGE "COMMON-LISP-USER">

* (related-methods 'stream)
NIL
```

`cl-user`パッケージは一切シンボルをエクスポートしていないので当然結果は`NIL`です。

カレントパッケージ意外を使いたい場合はキーワード引数`:PACKAGE`で指定できます。

```lisp
;; COMMON-LISPパッケージを検索。
* (related-methods 'stream :package :cl)
(SLOT-MISSING COMPUTE-APPLICABLE-METHODS DOCUMENTATION SHARED-INITIALIZE
              UPDATE-INSTANCE-FOR-REDEFINED-CLASS NO-APPLICABLE-METHOD
              FIND-METHOD UPDATE-INSTANCE-FOR-DIFFERENT-CLASS PRINT-OBJECT
              DESCRIBE-OBJECT CHANGE-CLASS SLOT-UNBOUND)

;; TRIVIAL-GRAY-STREAMSを指定。
* (related-methods 'stream :package :trivial-gray-streams)
(STREAM-WRITE-STRING TRIVIAL-GRAY-STREAMS:STREAM-READ-SEQUENCE
                     TRIVIAL-GRAY-STREAMS:STREAM-FILE-POSITION
                     STREAM-ADVANCE-TO-COLUMN
                     TRIVIAL-GRAY-STREAMS:STREAM-WRITE-SEQUENCE) 
```

込み入った場合エクスポートされているシンボルだけでなく、インターナルな総称関数もチェックしたい場合があるかもしれません。
そのような場合キーワード引数`:TARGET`で指定できます。

```lisp
* (related-methods 'stream :package :cl-store :target :internal)
(CL-STORE::INTERNAL-RESTORE-OBJECT)
```

キーワード引数`:TARGET`は`:EXTERNAL`、`:INTERNAL`、`:INHERITED`、`:ALL`を受け付けます。
各々、そのパッケージにインターンされておりかつエクスポートされているシンボル、そのパッケージにインターンされておりかつエクスポートされていないシンボル、そのパッケージに存在しているが他所のパッケージからインポートされてきたシンボル、そのパッケージ内でアクセス可能な全てのシンボルを意味します。

```lisp
;; External. (The default.)
* (related-methods 'stream :package :cl-store :target :external)
(CL-STORE:STORE-REFERRER CL-STORE:CHECK-MAGIC-NUMBER
                         CL-STORE:BACKEND-RESTORE-OBJECT CL-STORE:BACKEND-STORE
                         CL-STORE:STORE CL-STORE:GET-NEXT-READER
                         CL-STORE:RESTORE CL-STORE:BACKEND-RESTORE
                         CL-STORE:REFERRERP CL-STORE:INTERNAL-STORE-OBJECT
                         CL-STORE:SERIALIZABLE-SLOTS-USING-CLASS
                         CL-STORE:BACKEND-STORE-OBJECT
                         CL-STORE:STORE-BACKEND-CODE) 

;; Internal. (再掲)
* (related-methods 'stream :package :cl-store :target :internal)
(CL-STORE::INTERNAL-RESTORE-OBJECT)

;; Inherited.
* (related-methods 'stream :package :cl-store :target :inherited)
(SLOT-MISSING COMPUTE-APPLICABLE-METHODS DOCUMENTATION SHARED-INITIALIZE
              UPDATE-INSTANCE-FOR-REDEFINED-CLASS NO-APPLICABLE-METHOD
              FIND-METHOD UPDATE-INSTANCE-FOR-DIFFERENT-CLASS PRINT-OBJECT
              DESCRIBE-OBJECT CHANGE-CLASS SLOT-UNBOUND)

;; All.
* (related-methods 'stream :package :cl-store :target :all)
(CL-STORE::INTERNAL-RESTORE-OBJECT CL-STORE:STORE-REFERRER
                                   CL-STORE:CHECK-MAGIC-NUMBER
                                   CL-STORE:BACKEND-RESTORE-OBJECT
                                   CL-STORE:BACKEND-STORE CL-STORE:STORE
                                   CL-STORE:GET-NEXT-READER CL-STORE:RESTORE
                                   CL-STORE:BACKEND-RESTORE CL-STORE:REFERRERP
                                   CL-STORE:INTERNAL-STORE-OBJECT
                                   CL-STORE:SERIALIZABLE-SLOTS-USING-CLASS
                                   CL-STORE:BACKEND-STORE-OBJECT
                                   CL-STORE:STORE-BACKEND-CODE SLOT-MISSING
                                   COMPUTE-APPLICABLE-METHODS DOCUMENTATION
                                   SHARED-INITIALIZE
                                   UPDATE-INSTANCE-FOR-REDEFINED-CLASS
                                   NO-APPLICABLE-METHOD FIND-METHOD
                                   UPDATE-INSTANCE-FOR-DIFFERENT-CLASS
                                   PRINT-OBJECT DESCRIBE-OBJECT CHANGE-CLASS
                                   SLOT-UNBOUND) 
```

あるパッケージにおいて別なパッケージのシンボルがインポートされず、パッケージプリフィックスをつける形で直接参照されている場合も想定されます。
そのようなシンボルはパッケージにとって`:EXTERNAL`でも`:INTERNAL`でも`:INHERITED`でもありません。
ですが、使われているのは事実で、できれば複数パッケージに渡り検索をしたい場合もあることでしょう。
そのような場合、キーワード引数`:PACKAGE`に`NIL`を渡すとLispランタイム上に存在する全てのパッケージから関連メソッドをリストアップできます。
ただし結果は多すぎるかもしれません。

```lisp
* (length (related-methods 'stream :package nil))
236
```

これはクラス`T`を受け付けるメソッドやクラス`STANDARD-OBJECT`を受け付けるメソッドまでもが対象になっているからです。
無視したいクラスはキーワード引数`:IGNORE`にリスト形式で指定できます。

```lisp
* (related-methods 'stream :package nil :ignore '(t standard-object))
(CL-STORE:BACKEND-STORE CL-STORE:GET-NEXT-READER CL-STORE:BACKEND-RESTORE
                        UIOP/RUN-PROGRAM:SLURP-INPUT-STREAM
                        UIOP/RUN-PROGRAM:VOMIT-OUTPUT-STREAM PRINT-OBJECT)
```

これで少しは楽になるのではないでしょうか。
もっとも関数の検索はできないので、最終的にはやはりドキュメントなりソースなり読み回さなきゃならなくなるでしょうけど。

### The code.
コードは以下のとおり。

やっていることは単純で、パッケージのシンボルを舐めてまわり、そのシンボルが総称関数名であるならその総称関数が管理しているメソッドのスペシャライザーを舐めてまわり、引数で指定された型が一つでもスペシャライザーを満たすようであればそのようなメソッドを持つ総称関数は指定された型に関連性のある総称関数だと判断して総称関数名（シンボル）を集積しているだけです。
Lispが環境志向であるおかげで、パッケージのシンボルを舐めるのが容易であること、またMeta Object Protocolのおかげで、関連するメソッドの検索が容易だったのが幸いしました。
LispのことはLispに訊け、というのもまたLispという言語の大きな魅力の一つかと思います。

```lisp
(defun related-methods
       (type &key (package *package*) (target :external) (ignore))
  (declare (type (member :external :internal :inherited :all) target)
           (type list ignore))
  (labels ((subtype (type specializer)
             (if ignore
                 (and (not (typep specializer 'c2mop:eql-specializer))
                      (not (find (class-name specializer) ignore))
                      (subtypep type specializer))
                 (subtypep type specializer)))
           (related? (type gf)
             (loop :for method :in (c2mop:generic-function-methods gf)
                   :thereis (loop :for specializer
                                       :in (c2mop:method-specializers method)
                                  :thereis (subtype type specializer))))
           (targetp (symbol target)
             (or (eq :all target)
                 (eq target
                     (nth-value 1
                                (find-symbol (symbol-name symbol)
                                             (or package
                                                 (symbol-package symbol)))))))
           (ensure-list (package)
             (if (null package)
                 (list-all-packages)
                 (list package))))
    (delete-duplicates
      (uiop:while-collecting (collect)
        (dolist (p (ensure-list package))
          (do-symbols (symbol p)
            (when (and (fboundp symbol)
                       (typep (symbol-function symbol)
                              'standard-generic-function)
                       (targetp symbol target)
                       (related? type (symbol-function symbol)))
              (collect symbol))))))))
```

<!-- Links -->

[thread]:https://twitter.com/AkiteruMiho/status/1500048765298028553

