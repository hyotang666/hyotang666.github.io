# Common Lisp for data pre process.
大量のデータを取り扱ったことのある人なら前処理の重要性をご存知でしょう。
というのも多くの場合データを使っての処理そのものよりデータの正規化に時間が取られるものだからです。

データの正規化が困難なのは壊れ方には無限の可能性があるからです。
整数を期待しているフィールドに整数が入っていない場合、では、何が入っているのか。
それは無限の可能性があります。
値が無い（null）なのかもしれない。
文字列が入っているのかもしれない。
全角の整数が入っているのかもしれない。

壊れ方に無限の可能性があるならそれらへの対処方も無限に必要です。
対処法が無限に必要な場合それらを事前にプログラムへとコーディングしておくことは不可能です。
壊れたデータは単に無視すればいいという状況なら話は簡単ですが、そうとも限りません。
全てのデータを必ず処理しなければいけない場合、壊れたデータを人間が確認し手動で対応する必要が出てきます。

全体のわずか0.3%データが破損しているとしましょう。
千件中3件しか発生しない例外です。
ですがデータが1000万件あれば3万件のデータが破損していることになります。
3万件のデータを手動処理するのは大変です。

壊れたデータはログに書き出しておいて後で手動処理をするとしても、手動処理に大変な時間がかかるのは変えられません。
書捨てのスクリプトでログ出しした破損データ群をまとめて処理もできます。
ですがその場合でも「あるフィールドを正規化した結果他のフィールド値が壊れた」などということだって起こりえます。
データがcsvなどであればそれは簡単に起こります。

このような困難な問題でもCommon Lispなら柔軟に対応できます。

## Step 0: Define function.
話を簡単にするために標準入力から値を読み込み、それが整数なら標準出力に書き出すという処理を取り上げます。

最初の関数は以下のようになります。

```lisp
* (defun parse-int ()
    (print (parse-integer (read-line)))
    (terpri)
    (force-output))
=> PARSE-INT

* (loop (parse-int))
0       ; <--- input.

0       ; <--- output.
1       ; <--- input.

1
2

2
3
```

整数以外の入力が来るとエラーとなります。

```lisp
* (parse-int)
:hoge

debugger invoked on a SB-INT:SIMPLE-PARSE-ERROR in thread
#<THREAD "main thread" RUNNING {1001570343}>:
  junk in string ":hoge"

Type HELP for debugger help, or (SB-EXT:EXIT) to exit from SBCL.

restarts (invokable by number or by possibly-abbreviated name):
  0: [ABORT] Exit debugger, returning to top level.

(PARSE-INTEGER ":hoge" :START 0 :END NIL :RADIX 10 :JUNK-ALLOWED NIL)
0] 
```

## Step 1: Specify restart.
Common Lispにはリスタートという機能があります。
リスタートを使えばエラーが起きた際の復帰方法を指定できます。

ここではユーザが値を指定できるようにしてみましょう。

```lisp
* (defun parse-int ()
    (print (restart-case (parse-integer (read-line))
             (use-value (value)
                 :interactive (lambda ()
                                (format *query-io* "Specify use value> ")
                                (force-output *query-io*)
                                (list (read *query-io*)))
               value)))
    (terpri)
    (force-output))
=> PARSE-INT

* (loop (parse-int))
0       ; <--- input.

0       ; <--- output.
1

1
:hoge   ; <--- invlid input.

debugger invoked on a SB-INT:SIMPLE-PARSE-ERROR in thread
#<THREAD "main thread" RUNNING {1001570343}>:
  junk in string ":hoge"

Type HELP for debugger help, or (SB-EXT:EXIT) to exit from SBCL.

restarts (invokable by number or by possibly-abbreviated name):
  0: [USE-VALUE] USE-VALUE
  1: [ABORT    ] Exit debugger, returning to top level.

(PARSE-INTEGER ":hoge" :START 0 :END NIL :RADIX 10 :JUNK-ALLOWED NIL)
0] use-value            ; <--- リスタートを指定。
Specify use value> 2    ; <--- 正しい値を入力。

2
3

3
```

これで壊れたデータが来てもユーザが適切な値を入力さえすればプログラムを中断しなくても良くなりました。

## Step2: Redefine and/or reload.
データの壊れ方には無限の可能性があると申し上げましたが、その実えてして壊れ方には一定のパターンがあるものです。
壊れたデータが来てデバッガに落ちるたびに手入力をしていたあなたはダブルクォートでくるまれた整数が多いのに気づきました。

`READ-LINE`して取ってきた行を`PARSE-INTEGER`に渡すのではなく、`READ-FROM-STRING`に渡して結果の型で条件分岐すればよさそうです。

関数定義コードは以下のようになるでしょう。

```lisp
(defun parse-int ()
  (let ((value (read-from-string (read-line))))
    (print (restart-case (etypecase value
                           (integer value)
                           (string (parse-integer value)))
             (use-value (value)
                 :interactive (lambda ()
                                (format *query-io* "Specify use value> ")
                                (force-output *query-io*)
                                (list (read *query-io*)))
               value)))
    (terpri)
    (force-output)))
```

このような場合でもCommon Lispでは駆動中のループ処理を止めることなく関数の再定義が可能です。

```lisp
* (loop (parse-int))
0       ; <--- input.
                                                        
0       ; <--- output.
"1"     ; <--- invalid input.

debugger invoked on a SB-INT:SIMPLE-PARSE-ERROR in thread
#<THREAD "main thread" RUNNING {1001570343}>:
  junk in string "\"1\""

Type HELP for debugger help, or (SB-EXT:EXIT) to exit from SBCL.

restarts (invokable by number or by possibly-abbreviated name):
  0: [USE-VALUE] USE-VALUE
  1: [ABORT    ] Exit debugger, returning to top level.

(PARSE-INTEGER "\"1\"" :START 0 :END NIL :RADIX 10 :JUNK-ALLOWED NIL)

;; 関数を再定義。
(defun parse-int ()
  (let ((value (read-from-string (read-line))))
    (print (restart-case (etypecase value
                           (integer value)
                           (string (parse-integer value)))
             (use-value (value)
                 :interactive (lambda ()
                                (format *query-io* "Specify use value> ")
                                (force-output *query-io*)
                                (list (read *query-io*)))
               value)))
    (terpri)
    (force-output)))

WARNING: redefining COMMON-LISP-USER::PARSE-INT in DEFUN
PARSE-INT       ; <--- DEFUNの返り値。
0] (parse-int)  ; <--- 変更した定義で期待通り振る舞うか確認。
"1"             ; <--- わざと不正値を入力。

1               ; <--- 期待通りの出力を確認。

NIL             ; <--- PARSE-INTの返り値。
0] use-value    ; <--- リスタートを指定。
Specify use value> 1    ; <--- 今回は正しい値を手入力。

1
"2"             ; <--- 次に来た同様の不正値。

2               ; <--- エラーにならず正しく処理。
3

3
```
## Conclusion
上に見てきたようにCommon Lispではエラーが起きるとデバッガに入ります。
デバッガでは通常のREPLでできることは何でもできます。
ソースコードを書き換えた上でのライブラリの再コンパイル/再ロードも可能です。

大量のデータを処理する場合でも、とにかく壊れたデータが来たときは手入力できるように設計。
手入力を繰り返すことでパターンが見えてきたらそのパターンに対処できるようソースを改変。
プログラムでは対応できないコーナーケースのみ手入力で対応と進めていけます。

あなたのコード（ここでいう`PARSE-INT`）はどんどん堅牢に成長していきます。
もし新たに1000万件のデータが渡されたとしても人間が手入力しなければならないケースはほとんどないことでしょう。

実行中のループを止めることなく（一時的にしか止めず）関数の再定義が可能なのはCommon Lispの強みです。
この強みは見てきた通りデータの前処理という困難なタスクで力を発揮します。

