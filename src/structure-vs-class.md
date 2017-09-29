# Structure vs Class, case in Common Lisp.

## Metanote.
対象読者。

* Common Lisp初心者。
* 長文ですのでお暇な時に。

## Introduction
Common Lispにはユーザが定義できる合成型として構造体とクラスの２種がサポートされている。
これはしばしば初心者をして「どちらを使えばいいのか」疑問に思わせることとなる。
もちろん２種サポートされているのは各々に違いがあるからなのだが、初心者には違いが伝わりにくかろう。
ここにまとめておく。

## 定義フォーム
１スロットしか持たない最小の定義を考える。

```lisp
; structure
(defstruct struct slot)
; class
(defclass my-class()(slot))
```
フォーム（syntax）は大差無いがセマンティクスは大きくことなる。
構造体はこのフォームだけで、Constructor, Predicate, Copier, Accessorが定義（インターン）される。
クラスでは何も定義されない。
構造体で暗黙裏に多くのシンボルがインターンされるのは、時に名前衝突を引き起こし、バグの原因ともなる。
それを反面教師としたのか、クラスでは暗黙裏のシンボルインターンは皆無となっている。
ただ、その結果ユーザが自分で多くのものを書かねばならないので、正直クラスはめんどくさい。
これはトレードオフだ。

## Constructor.
コンストラクタを見よう。
構造体はオプションで指定しなければ、MAKE-XXXというコンストラクタが作られる。
クラスは`MAKE-INSTANCE`でインスタンスを作る。

`MAKE-INSTANCE`で総称的にインスタンスを作れるのはとても便利であるが、実は多くの処理系では構造体も`MAKE-INSTANCE`で作れる。
（僕が知っている限りではCLISP,CCL,ECLが可能。）
ただしそれはANSI外だ。
可搬的ではない。（SBCLはできない）
でも、それができたら嬉しい。
そこで僕は[可搬的にできるようにしている。](https://github.com/hyotang666/structure-ext/tree/master/make-instance)
CLOSER-MOPが担当してくれると嬉しいのだが、そもそも`MAKE-INSTANCE`で構造体をコンストラクトするのはANSI外なので難しいかもしれない。

## Initform.
構造体はスロットが初期化されなかった場合`NIL`で初期化される。
クラスは初期化されず、未束縛となる。
未束縛のスロットを参照すると`SLOT-UNBOUND`型のエラーとなる。
スロットが初期化されているかどうかチェックするためには`SLOT-BOUNDP`を使用する。
構造体でスロットが初期化されたか否か知る方法はデフォルトではない。
それが重要になるなら、

* 初期化されたかどうかのフラグを格納するスロットを用意する。
*  `(defvar +unbound+ '#:unbound)`などしてグローバルに未束縛であることを示すシンボルを用意し、それで初期化しておき、スロット値が`+unbound+`と`eq`かどうかで確認する。

のいずれかを取らねばならない。

## Initarg
構造体では自動的にINITARGがインターンされるが、クラスでは指定しない限りINITARGは導入されない。

```lisp
(make-instance 'my-class :slot 0) ; ERROR
(defclass my-class()
  ((slot :initarg :slot))) ; <--- Specify initarg.
(make-instance 'my-class :slot 0) ; ok
```
クラスでは指定しなければならないので、筆記量が増えるが、自由に柔軟な名前が使用できる。
ただ、構造体でも実はオプションで指定可能である。

```lisp
(defstruct(struct (:constructor make-struct (&key((:arg slot)))))
  slot)
(make-struct :arg 0) ; ok
```
上記のオプション指定で:argをINITARGとして使えるようになる。
よほどの事がない限りINITARGとスロット名を異なるものにしたいとは思わないと思うので、クラスのめんどくささだけが際立つ印象がある。

## Printed notation.
構造体はリーダブルにプリントされるがクラスはアンリーダブルである。

```lisp
(make-struct)
#S(STRUCT :SLOT NIL)
(make-instance 'my-class)
#<MY-CLASS #x12345678>
```
構造体をアンリーダブルにするにはその構造体へスペシャライズドされた`PRINT-OBJECT`メソッドを書き、その中で`PRINT-UNREADABLE-OBJECT`マクロを使えば良い。
クラスをリーダブルにするには、同様に`PRINT-OBJECT`メソッドを書き、リーダマクロを書けばいい。
この場合もひと手間多い分クラスのほうがめんどくさい。

## Predicate
構造体では自動的に作られるが、クラスでは手で書かねばならない。
また、その場合多くの処理系では`TYPEP`で事足りるが、処理系依存らしい。

```lisp
(defclass subclass(my-class))
(defvar subclass (make-instance 'subclass))
(typep subclass 'my-class) ; implementation dependent
```
ソースはCLXのソースコード。
アレグロではダメらしい。
CLXは古いプロダクツなので（mcclimの開発が活発になった影響で最近はCLXのメンテナンスも活発になってきたみたいだが）今は違うかもしれないのだが。

## Copier
構造体では自動的にCOPY-XXXという複製関数が作られるが、クラスでは作られない。
なお構造体には`COPY-STRUCTURE`という関数があるので、正直Copierはいらないとも思うのだが。
また、構造体のCopierは浅いコピーしか行わない。
例えばスロットに格納されているリストを破壊的に変更した場合。コピーした/されたインスタンスで変更が共有されてしまう。
これはピットフォールになりがちだ。
クラスなら自前で用意しなければならないので、そのようなバグは踏まないだろうし、踏んだなら自分が間抜けなだけなのだが、構造体は勝手に作ってくれているものなので理解が正しく及んでいないとはまりかねない。

## Accessor
構造体では自動的にSTRUCT-SLOTというアクセサが定義される。
クラスでは指定しない限りアクセサは作られない。
作らなかった場合は`SLOT-VALUE`という低レベルな総称関数でアクセスする。

```lisp
(defvar s (make-struct :slot 0))
(struct-slot s) => 0
(defvar c (make-instance 'my-class :slot 0))
(my-class-slot c) ; ERROR
(slot-value c 'slot) => 0
(defclass my-class()
  ((slot :initarg :slot :accessor my-class-slot))) ; <--- specify accessor
(setq c (make-instance :slot 0))
(my-class-slot c) => 0
```
クラスでは柔軟な命名が可能だが構造体ではそうはいかない。
構造体で可能なのは

* プリフィックスを変更する。
* プリフィックスをつけない。

のいずれかである。

```lisp
(defstruct(struct (:conc-name structure-))
  slot)
(setq s (make-struct :slot 0))
(struct-slot s) ; ERROR
(structure-slot s) => 0

(defstruct(struct (:conc-name nil))
  slot)
(setq s (make-struct :slot 0))
(struct-slot s) ; ERROR
(slot s) => 0
```
例えば構造体のアクセサにサフィックスを付けたいなどの場合、自前で関数を書く必要がある。

```lisp
(defstruct struct slot)
(defun slot<=struct(struct)
  (struct-slot struct))
```
多くの処理系で構造体のアクセサは通常インライン展開される。
自前で上記のようなアクセサを定義するならその辺もケアせねばならない。
また上記関数はアクセサではなくただのリーダだ。
SETFableではない。
`SETF`できるようにするにはそれもまた自前で書かねばならない。

```lisp
(defun(setf slot<=struct)(new-value struct)
  (setf(struct-slot struct)new-value))
```

## Shared slot
クラスでは、各インスタンスで共有されるスロットをオプション指定することが可能となっている。
構造体にそのようなオプションはない。

```lisp
(defclass 'my-class()
  ((slot :allocate class)))
```
ただ、`SYMBOL-PLIST`を利用することで構造体でも同様の機能をエミュレートすることは可能だ。

```lisp
(defstruct struct)
(defun struct-slot(struct)
  (etypecase struct
    (struct (get 'struct 'slot))))
(defun (setf struct-slot)(new-value struct)
  (etypecase struct
    (struct (setf(get 'struct 'slot)new-value))))
```

## Type option
構造体はTYPEオプションを持つ。
これにより構造体のsyntaxでリストかベクタを作れるようになる。

```lisp
(defstruct(struct (:type list))
  slot)
(make-struct :slot 0)
=> (0)
```
これをクラスで再現するのはとても難しいと思う。
やってやれないことはないとは思うが、労多くして益少なしといったところだろう。
ぶっちゃけ構造体でも普通いらない。
頑張って妄想をたくましくして考えられるストーリーは次のようなものか。
例えば`LET`などに展開されるマクロを書いているとする。
ユーザが指定する元のシンボルと、そのシンボル名を元に作られたマクロ上で使われる`GENSYM`されたシンボルと、初期化フォームとをまとめて持たねばならないとする。
安易に考えてリストでくくっていたのだが、取り出す関数が`CADDR`だの`THIRD`だのになってしまい、何を意味するものかソースの字面から判然としなくなる。
構造体なら意味のある名前を持つアクセサができるので、メンテナビリティのためにも構造体にしたい。
だが、各スロットを各々引数として受け取る補助関数を作ってしまった後だ。
これまではリストだったので`APPLY`に渡せばいいだけだったが、構造体では`APPLY`に渡せない。
ソースコードは可能な限り変えたくない。
そのような状況だろうか？

普通いらないオプションなのでクラスで再現できないとしても問題になることはあるまい。

## inheritance
Common Lispの構造体は単一なら継承できる。
クラスなら多重継承も可能である。

## Bench
クラスを作るのは著しく遅い。

```lisp
(time(make-struct))
; 7,885 processor cycles
(time(make-instance 'my-class))
; 23,297,308 processor cycles
```
三千倍のコストである。
もちろん処理系依存だが。
SBCLだとキャッシュが効いて２回め以降の呼び出しは高速になる。

```lisp
(time(make-instance 'my-class))
; 9,120 processor cycles
```

アクセサも遅い。

```lisp
(defvar s (make-struct))
(time(struct-slot s))
; 6,023 processor cycles
(defvar c (make-instance 'my-class :slot nil))
(time(my-class-slot c))
; 406,847 processor cycles
```

## Conclusion
オブジェクトを大量に作っては捨て作っては捨てするようなコードなら構造体のほうが速度面で大いに有利となろう。
ユーザに提供し、継承を通して拡張を提供するならクラスのほうが柔軟でよかろう。

基本は構造体でコードを書き始め、多重継承が必要となったらクラスに書き換えるのがよかろう。
僕は[DEFSTRUCTと同じsyntaxでありながらDEFCLASSとDEFMETHODの式に展開されるマクロを用意している。](https://github.com/hyotang666/structure-ext/tree/master/as-class)
DEFSTRUCTとの違いは、:TYPE, :INITIAL-OFFSET, :NAMEDオプションがINVALIDなのと:INCLUDEオプションを複数受け取れることだけだ。
これにより構造体からクラスに変更する場合でも、単にコマンド名を変更すればいいだけとなる。
また、多重継承が必要とならない内は通常のCommon Lispの機能だけを使うこととなるので依存は増えない。
