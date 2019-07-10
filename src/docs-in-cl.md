# Docs in Common Lisp
## Meta notes
### 対象読者
* ドキュメンテーション文字列の扱いについて関心のあるCLer。

## Documentation string
Common Lispには、例えば関数定義構文にドキュメンテーション文字列を挿入できる機能がある。

たとえば以下のようにする。

```lisp
(defun example(x)
  "This is documentation."
  x)
```

ドキュメンテーション文字列は、たとえば`DESCRIBE`経由で参照できる。

```lisp
* (describe 'example)

COMMON-LISP-USER::EXAMPLE
  [symbol]

EXAMPLE names a compiled function:
  Lambda-list: (X)
  Derived type: (FUNCTION (T) (VALUES T &OPTIONAL))
  Documentation:
    This is documentation.
  Source form:
    (LAMBDA (X) "This is documentation." (BLOCK EXAMPLE X))
```

`DESCRIBE`の欠点は出力内容が処理系依存なことである。
（なお、上記出力はSBCLのもの。）

余計な出力が必要ないなら`DOCUMENTATION`でドキュメンテーション文字列のみを取り出せる。

```lisp
* (documentation 'example 'function)
"This is documentation."
```

## Issue
個人的にこの機能はなかなか良い機能だと思ってはいるのだが、それがうまく機能しているかというと少々疑問がある。

そもそもエンドユーザーとしてドキュメンテーション文字列に期待するのはどのような情報だろうか？

僕が期待するものは引数の型情報、返り値の型情報、副作用があるかないか、コンディションを投げることがあるかなどであり、これは要するにHYPERSPECのような内容である。

当然それは一行やそこらで収まるものではない。
僕が見てきた中で最長のものは[`DRAKMA:HTTP-REQUEST`](https://github.com/edicl/drakma/blob/master/request.lisp#L189)のそれで、実に243行に渡る。

そのような長いドキュメンテーション文字列はソースコードを少々読みにくくさせる。
特に問題なのが、最悪の場合ラムダリストがエディタの画面外に行ってしまう点である。
（もっとも、`DRAKMA:HTTP-REQUEST`のラムダリストはそれだけで45行もあるのだが。）

こういった点を苦々しく思っているのは僕だけではないようで、なんとかしようと試みている例は幾つかある。

## Solution 1
[`CL-ANNOT`](https://github.com/m2ym/cl-annot#user-content-annotation-doc)はアノテーションを使うことでドキュメンテーション文字列を関数定義フォームの外側へ出せるようにしている。

```lisp
@doc
"This is documentation"
(defun example(x)
  x)
```

欠点はドキュメンテーション文字列がフォームに束縛される点である。
すなわち、別ファイルに分けることができない。

## Solution 2
[`DOCUMENTATION-UTILS`](https://github.com/Shinmera/documentation-utils)はマクロで後からドキュメンテーション文字列を`SETF`するようにしている。

```lisp
(defun example(x)
  x)
(docs:define-docs
  (example "This is documentation"))
```

同様のことは[`INTROSPECT-ENVIRONMENT`](https://github.com/Bike/introspect-environment/blob/master/doc.lisp)も独自のマクロで行っている。

欠点はポータビリティに欠ける点である。
総称関数`DOCUMENTATION`への`SETF`は言語仕様上できるはずなのだが、少なくともECLはそれに違反しており`SETF`できない。

## Conclusion
個人的には関数定義フォームへのドキュメンテーション文字列は`ASDF`でいうところのショートデスクリプションであるべきなのではないかと思っている。

そして、長さが必要なら（`ASDF`でいうところのロングデスクリプションは）総称関数`DOCUMENTATION`への`:AROUND`メソッドとして定義するのがスマートではなかろうか。

```lisp
(defun example(x)
  "This is documentation title"
  x)

(defmethod documentation :around ((name (eql 'example))(type (eql 'function)))
  (format nil "~@[~A~2%~]~A"
	  (call-next-method)
	  "This is documentation body"))
```
