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
[cl-setlocale](https://github.com/shamazmazum/cl-setlocale)を使います。

```lisp
* (cl-setlocale:set-all-to-native)
```

上記コードでlocaleの初期化が可能です。

システムのlocaleを取り出したい場合は[system-locale](https://github.com/Shinmera/system-locale)が便利です。

## Pitfalls.
### Must setlocale before compile cl-charms.
cl-charmsをコンパイルする前に`SETLOCALE`しなければなりません。

少々お行儀が悪いですがメソッドを追加して対応するのが簡単だと思います。

```lisp
(defsystem "your-app"
  :depends-on
  (
   "cl-setlocale"
   )
  ...)

;; cl-setlocaleをロード語にSET-ALL-TO-NATIVEを呼ぶ。
(defmethod perform :after ((o load-op) (c (eql (find-system "cl-setlocale"))))
  (symbol-call "CL-SETLOCALE" "SET-ALL-TO-NATIVE"))

;; cl-setlocaleのロード後にcl-charmsがロードされるように指定。
(defmethod component-depends-on ((o load-op) (c (eql (find-system "cl-setlocale"))))
  (append (call-next-method) '((load-op "cl-charms"))))
```
