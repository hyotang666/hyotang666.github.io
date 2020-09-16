# Why common lisp package is difficult.
## Meta note.
### 対象読者
* Common Lispのpackageシステムわかりづらい初心者CLer

## Reason.
Common LispのPackageシステム（名前空間管理システム）は他言語の同機能とは設計が異なります。
ですので他言語の名前空間管理システムへのメンタルモデルのままCommon LispのPackageシステムを解釈しようとすると歪みが生じます。
これが初学者をしてCommon LispのPackageシステムを難解なものとしている理由となります。

## Namespace model.
多くの言語が採用している名前空間管理モデルは一番外側にグローバルな名前空間があり、その内側に名前空間を区切るというものです。

!["Namespace example1."](../img/packages/namespace1.svg)

グローバルな名前空間には言語により予約されたキーワードがあります。

!["Namespace example2."](../img/packages/namespace2.svg)

名前空間から予約語は見えます。

!["Namespace example3."](../img/packages/namespace3.svg)

他の名前空間は見えません。

!["Namespace example4."](../img/packages/namespace4.svg)

## Package system model.
他言語の名前空間モデルがウィンドウの分割に相当するなら、Common LispのPackageシステムはタブウィンドウに相当します。
Package（名前空間）は全て並列に存在します。

!["Package example1."](../img/packages/package1.svg)

`IN-PACKAGE`でタブを切り替えると見えるシンボルが変わります。

!["Package example2."](../img/packages/package2.svg)

上に見てきたようにCommon Lispにはグローバルな名前空間というものが存在しません。
よって予約語というものも存在しません。

Common Lispという言語が提供するシンボルは全て`COMMON-LISP`パッケージに所属しています。

!["Package example3."](../img/packages/package3.svg)

言語仕様によりLisp Runtimeを立ち上げたときに最初に入っている名前空間（package）は`CL-USER`であると決まっています。
そして`CL-USER`パッケージは`COMMON-LISP`パッケージを使用（USE）してあるものと定められています。

これはすなわち`COMMON-LISP`パッケージを使用（USE）しなければ`CAR`も`CDR`もできないパッケージを作ることが可能であることを意味します。
これを他言語で例えるなら、`if`による条件分岐も`for`によるループもできない名前空間を作るに相当します。

その場合でも、もちろん、明示的にパッケージ名をプリフィックスとして指定すれば参照は可能ですが。

## Conclusion.
ハマりどころは他にもあるのですがそもそものモデルの違いを解説している記事は見たことがなかったので書いてみました。
初学者の方の一助となれば幸い。
