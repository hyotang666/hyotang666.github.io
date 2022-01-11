# Caveman kills ruby on rails - Chapter 4
## Meta info
### 対象読者
* Common Lispにおけるデータベース周りに興味のある人。

## NOTE
筆者はcavemanを捨て[snooze](https://github.com/joaotavora/snooze)を使うようになった。
詳細は[ここ](why-snooze.html)に記してある。

## Introduction
本稿は[原著](https://book.impress.co.jp/books/1117101135)の各章をCommon Lispに翻訳するシリーズの第4章である。
本章ではCommon Lispにおけるデータベース取り扱いの基礎を修めていく。

## 4.1 Basic of database and model
### database
ここでは[sqlite3](https://sqlite.org/index.html)を使う。
理由はCavemanがデフォルトで使用しているデータベースがこれだからである。

### Mito
[Mito](https://github.com/fukamachi/mito)というライブラリを使う。
これはCommon Lisp用のORMである。

なぜMitoを使うのか。
データベース周りに関して、Cavemanはデフォルトで[Datafly](https://github.com/fukamachi/datafly)をサポートしており、わざわざMitoを入れなくともデータベースは使えるのに？

理由はさまざまあるが、最も大きなものは「Mitoはまだしもドキュメントがあるから」である。
[抽象化の漏れ](http://pikapika.to/~yf/momoka.cgi?op=readmsg&id=848)というものがあるが、ドキュメントレベルでもこれは存在する。
dataflyのドキュメントは「既にデータベースをバリバリ使っている人」向けのものであり、今までデータベースを使った経験が無い僕のような人間には不向きなものなのである。
Mitoには幸い[tutorial](https://lispcookbook.github.io/cl-cookbook/databases.html)があったので、データベースにド素人な僕でも少しは進められそうだった。

という訳でyour-app.asdの`:depends-on`に`"mito"`を追加しておこう。

```lisp
(defsystem "your-app"
  ...
  :depends-on ("clack"
               ...
               ;; for DB
               "mito" ; <--- This!
               "datafly"
               "sxql")
  ...
  )
```

また、Mitoがデータベースとのコネクションを保持する変数とDataflyが保持する変数とは異なるので、src/db.lispを以下のように編集しておく。

```lisp
(defmacro with-connection (conn &body body)
  `(let ((mito.connection:*connection* ,conn)) ; <--- This!
     ,@body))
```

### Settings of database
Modelを記述するためのファイルを別に用意しよう。

your-app.asdを以下のように編集する。

```lisp
(defsystem "your-app"
  ...
  :components ((:module "src"
                :components
                ((:file "main" :depends-on ("config" "view" "db"))
                 ...
                 (:file "model" :depends-on ("db")) ; <--- This!
                 (:file "config"))))
  ...)
```

src/下にmodel.lispファイルを作り以下のようにする。

```lisp
(defpackage :your-app.model(:use :cl :your-app.db))
(in-package :your-app.model)
```

Cavemanのデータベースはデフォルトではインメモリである。
（READMEに文言がないのは不親切だと思う。）
これを変更するためにsrc/config.lispを以下のように変更する。

```lisp
(defconfig :common
  `(:databases ((:maindb :sqlite3 :database-name
      "yourApp" ; <--- This!
      ))))
```

当初:database-nameを"your-app"としたのだが、後にデータベースにアクセスしようとしたところCANTOPENと怒られてしまった。
そこで"your_app"と名前を変えてみたところ、うまく動いてくれた。
どうやらダッシュで区切られた単語は勝手に先頭が大文字化されるらしい。
こちらがコードで指定した名前は"your-app"だが、登録されるファイル名は"your-App"となる。
そのくせ`MITO:CONNECT-TOPLEVEL`に"your-app"という:database-nameを指定した場合は件の暗黙理のケース変換が行われずケースセンシティブに"your-app"というファイルが探され、結果エラーとなっているらしい。
ここでは安全のため記号を排したキャメルケースでデータベース名を定義している。

なお、データベースファイルはカレントディレクトリに作られる。
これが嫌な場合は:DATABASE-NAMEに絶対パスを指定しておけば良い。
`*APPLICATION-ROOT*`という変数が作られているし、`DEFCONFIG`への引数は`:DATABASE-NAME`だし（`DATABASE-PATH`ではない）、プロジェクトディレクトリには"db"ディレクトリもあるので、いい具合にやってくれるのかと思いきやそんなことはない。
ドキュメントが無いのは不親切だと思う。

## 4.2 Creating table
### define user table
Mitoを使ってModelを定義するには`CL:DEFCLASS`の構文を使用することができる。

この`CL:DEFCLASS`は完全に同じなのではなく、`:METACLASS`拡張の指定により微妙に異なるものになっている。
READMEにbnfがあるので、少しは参考になる。

ここで「少しは」としているのは完全には参考にならないからだ。
具体的には:COL-TYPEの引数KEYWORDにどのようなものがあるのか説明がどこにもない。
MitoはCL-DBIの上に作られている。
CL-DBIは各データベースライブラリの構文を共通化させるラッパライブラリだ。
mysqlではdatetimeだがpostgresqlではtimestampとなるような異なりを吸収してくれているわけだ。
では、一体何で統一するのか。
:datetimeで統一するのか、:timestampで統一するのか、それとも:dateのような新しいキーで統一するのか。
一切の解説がない。
有志の先達によるリストが[tutorial](https://lispcookbook.github.io/cl-cookbook/databases.html#fields-types)にあるが、こういうのは本来公式が提供すべきものであると思う。

ではmodel.lispに以下のコードを追加しよう。

```lisp
(defclass user()
  ((number :col-type :integer
           :initarg :number
           :reader number-of)
   (name :col-type (:varchar 64)
         :initarg :name
         :reader name-of)
   (full-name :col-type (or (:varchar 128) :null)
              :initarg :full-name
              :reader full-name-of)
   (email :col-type (or :null :text)
          :initarg :email
          :accessor email-of)
   (birthday :col-type (or :null :date)
             :initarg :birthday
             :reader birthday-of)
   (sex :col-type :integer
        :initarg :sex
        :initform 1
        :reader sex-of)
   (administrator :col-type :boolean
                  :initarg :administrator
                  :initform nil
                  :accessor administratorp
                  )
   )
  (:metaclass mito:dao-table-class))
```

上記のコードはUSERテーブルクラスをLispイメージに対して定義しただけで、未だデータベースにはテーブルが作られていない点要注意。

データベース上にテーブルを作るためにsrc/model.lispに以下のコードを追加する。

```lisp
(with-connection(db)
 (mito:ensure-table-exists 'user))
```

## 4.3 Store
### Create and update record.
`CL:MAKE-INSTANCE`でオブジェクトを作り、`MITO:INSERT-DAO`総称関数でデータベースに記録する。

また、`MIDO:CREATE-DAO`総称関数はこれらをまとめてやってくれる。

### Seed data.
シードデータをデータベースに登録する関数を作成しておく。
ここではsrc/model.lispに追加することとする。

```lisp
(defun seeds()
  (let((names #("Taro" "Jiro" "Hana" "John" "Mike" "Sophy" "Bill" "Alex" "Mary" "Tom"))
       (fnames #("佐藤" "鈴木" "高橋" "田中"))
       (gnames #("太郎" "次郎" "花子")))
    (with-connection(db)
      (dotimes(x 10)
        (mito:create-dao 'user
                         :number (+ x 10)
                         :name (aref names x)
                         :full-name (format nil "~A ~A"(aref fnames (rem x 4))
                                            (aref gnames (rem x 3)))
                         :email (format nil "~A@example.com"(aref names x))
                         :birthday "1981-12-01"
                         :sex (nth (rem x 3)'(1 1 2))
                         :administratorp (zerop x))))))
```

### rebuild
以下のようなヘルパー関数を用意しておけばよい。

```lisp
(defun rebuild()
  (with-connection(db)
    (mito:recreate-table 'user))
  (seeds))
```

## 4.4 find
### ids
Railsにおけるidsメソッドのようなものなどない。
自作するとしたら以下のようになるだろう。

```lisp
(defun ids()
  (your-app.db:with-connection(your-app.db:db)
    (mapcar #'mito:object-id (mito:retrieve-dao 'your-app.model::user))))
```

### find-dao
カラムからレコードを取り出すには以下のようにする。

```lisp
(your-app.db:with-connection(your-app.db:db)
  (mito:find-dao 'your-app.model::user :id 3))
; ===> #<USR {12341234123}>
```

中身を見たい場合は`CL:DESCRIBE`を使用する。
ただし、中身の表示のされ方は処理系依存な点要注意。

```lisp
(describe *) ; case in SBCL
  [standard-object]

Slots with :INSTANCE allocation:
  CREATED-AT                = @yyyy-mm-ddThh:mm:ss.ms+tz
  UPDATED-AT                = @yyyy-mm-ddThh:mm:ss.ms+tz
  SYNCED                    = T
  ID                        = 3
  NUMBER                    = 12
  NAME                      = "Hana"
  FULL-NAME                 = "高橋 花子"
  EMAIL                     = "Hana@example.com"
  BIRTHDAY                  = @yyyy-mm-ddThh:mm:ss.ms+tz
  SEX                       = 2
  ADMINISTRATOR             = NIL
```

帰ってきたオブジェクトは`DEFCLASS`フォームで指定したREADERないしACCESSORでスロット参照が可能である。

```lisp
(email-of **)
; ---> "Hana@example.com"
```

先程はidで検索をかけたが、もちろんid以外でもできる。

```lisp
(your-app.db:with-connection(your-app.db:db)
  (mito:find-dao 'your-app.model::user :name "Taro"))
```

この場合は最初に一致するレコードを返すこととなる。

複数のカラム指定ももちろんできる。

```lisp
(your-app.db:with-connection(your-app.db:db)
  (mito:find-dao 'your-app.model::user :sex 1 :administrator 0))
```

上記例に見るように、MitoにおけるBOOLEANの取り扱いは中途半端である。
`DEFCLASS`フォームの`:COL-TYPE`への:BOOLEANの指定はシンタックスエラーにはならない。
`MITO:CREATE-DAO`に渡した`NIL`ないし`T`は正しくsqlite3におけるBOOLEAN型に変換され格納される。
またidなどを利用してデータベースからレコードを取り出した場合も、sqlite3におけるBOOLEAN型は正しく`T`ないし`NIL`に変換される。
しかしながら`MITO:FIND-DAO`などでスロット値による検索をする場合、`T`や`NIL`の指定はエラーとなる。

```lisp
(your-app.db:with-connection(your-app.db:db)
  (mito:find-dao 'your-app.model::user :administrator nil))
; ERROR
```

先程の例に見るように、BOOLEANの値を指定したい場合は０ないし１で指定する。
動作確認はしていないが、sqlite3、mysql、postgresqlのドキュメントを読む限り、これら３データベースは共通して０をFALSEに１をTRUEとして扱う。
ソースコードの読みやすさのためには以下のような定数を定義しておいたほうがいいかもしれない。

```lisp
(defconstant +true+ 1)
(defconstant +false+ 0)
```

もっと言えば`:BOOLEAN`型は使わないほうがいいのかもしれない。

なお対応するレコードが見つからなかった場合NILになる。

また、指定されたカラムがレコードに存在しない場合はエラーとなる。

一致するレコードを、最初の一つではなくすべて取り出したい場合は`MITO.DAO:RETRIEVE-DAO`を使用する。

```lisp
(mito:retrieve-dao 'user :administrator +false+)
; ---> (#<USER {111}> #<USER {222}> #<USER {333}>
        #<USER {444}> #<USER {555}> #<USER {666}>
        #<USER {777}> #<USER {888}> #<USER {999}>)
```

### Lazy loading
RailsにおけるLazy loadingはサポートされていないように見える。

### Query
複雑なクエリを組み立てたい場合は`MITO.DAO:SELECT-DAO`と[sxql](https://github.com/fukamachi/sxql)とを組み合わせて使う。

前節の`MITO:RETRIEVE-DAO`は以下のコードと等価である。

```lisp
(mito:select-dao 'user
  (sxql:where '(:= :administrator 0)))
```

:andによる条件の重ね合わせ例。

```lisp
(mito:select-dao 'user
  (sxql:where '(:and (:= :name "Taro")
                     (:< :number 20))))
```

SXQL:ORDER-BYによるソート例。

```lisp
(mito:select-dao 'user
  (sxql:where '(:= :sex 2))
  (sxql:order-by :number))
```

:DESC指定による降順ソート例。

```lisp
(mito:select-dao 'user
  (sxql:where '(:= :sex 2))
  (sxql:order-by (:desc :number)))
```

:ORによる検索例。

```lisp
(mito:select-dao 'user
  (sxql:where `(:or ,@(mapcar (lambda(num)
                                `(:= :number ,num))
                              '(15 17 19)))))
```

:<=による範囲指定例。

```lisp
(mito:select-dao 'user
  (sxql:where `(:and (:<= 12 :number)
                     (:<= :number 14))))
```

### Unsupported methods
Railsにおけるファインダーメソッドに相当するものはない。

集計用のメソッド群もない。

## Summary
* データベースの設定はsrc/config.lispで行います。
* データベースの中にテーブルを作成するには、`MITO:ENSURE-TABLE-EXISTS`を叩きます。
* 本書では、開発用のデータベースに初期データを投入するためにシードデータを使います。
* モデルを定義するのには`CL:DEFCLASS`構文を利用します。
* モデルを使ってレコードを保存するには`MITO:CREATE-DAO`総称関数を使います。
* モデルを使ってレコードを取り出すには`MITO:FIND-DAO`関数を使います。
* 複雑なクエリの構築にはsxqlを使用します。
