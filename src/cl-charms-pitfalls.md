# cl-charms pitfalls.
## Meta notes.
### 対象読者
* cl-charmsを使って日本語を表示したい方。

## Introduction.
[cl-charms](https://github.com/HiTECNOLOGYs/cl-charms)を使って日本語を表示しようとしたら文字化けを起こしていました。
解決したので備忘録として記しておきます。

## Reason.
原因はlocaleの初期化をしていないからでした。
[ncurses](https://linux.die.net/man/3/ncurses)のマニュアルによるとlocaleの設定がなされていない場合は後方互換製のためエンコーディングをiso-8859-1とみなすようです。

## How to initialize.
[lem-setlocale/cffi](https://github.com/cxxxr/lem/tree/master/lib/setlocale)を使います。
これは[lem](https://github.com/cxxxr/lem)のサブシステムです。

lem自体はquicklispに登録されていないので、[roswell](https://github.com/roswell/roswell)を使ってインストールしておきましょう。

```lisp
* (lem-setlocale/cffi:setlocale lem-setlocale:+lc-all+ "en_US.UTF-8")
```

上記コードでlocaleの初期化が可能です。

システムのlocaleを取り出したい場合は[system-locale](https://github.com/Shinmera/system-locale)が便利です。

## Pitfalls.
### Missing cffi.
2020/09/29現時点でlem-setlocale/cffiのasdファイルにはcffiが`:depends-on`に存在しません。
よって`lem-setlocale/cffi`のみをロードしようとするとエラーとなります。
事前に`cffi`のロードが必要です。

### Must setlocale before compile cl-charms.
cl-charmsをコンパイルする前に`SETLOCALE`しなければなりません。

少々お行儀が悪いですがメソッドを追加して対応するのが簡単だと思います。

```lisp
(defsystem "your-app"
  :depends-on
  (
   "cffi"
   "lem-setlocale/cffi"
   )
  ...)

;; lem-setlocale/cffiをロード語にSETLOCALEを呼ぶ。
(defmethod perform :after ((o load-op) (c (eql (find-system "lem-setlocale/cffi"))))
  (symbol-call "LEM-SETLOCALE/CFFI" "SETLOCALE"
               (symbol-value (find-symbol* "+LC-ALL+" "LEM-SETLOCALE/CFFI"))
               "en_US.UTF-8"))

;; lem-setlocale/cffiのロード後にcl-charmsがロードされるように指定。
(defmethod component-depends-on ((o load-op) (c (eql (find-system "lem-setlocale/cffi"))))
  (append (call-next-method) '((load-op "cl-charms"))))
```

## 最後に。
lem-setlocale/cffiが独立したライブラリになってくれると嬉しい。
