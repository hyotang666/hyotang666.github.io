# ojilang-cl
## Meta Note
### 対象読者
* 暇人
* ojilangファン

## Introduction
[ojilang](https://twitter.com/grethlen/status/1142414723121655808)設計にチャレンジしてみた。

## entry point
ゼロから言語を設計することは難しい。
そこで、むしろ既存の言語を架空の（かつ理想の）ojilangに変換するコードを書けば、おのずとojilangが出来上がるのではあるまいか。

## Common Lisp
Common Lispは25の特殊形式を持つ。
すなわち、25特殊形式すべてをojilangに変換できれば、事実上すべてのCommon Lispコードはojilangに変換できることとなる。

## Atom
アトムはそのまま出力することとする。

```lisp
(ojilang 0) => 0
```

## Function call
関数呼び出しは以下のように変換される。

```lisp
(ojilang '(+ 1 2)) => "今度+しようよ😍❓1❓2❓どうカナ💦" 

(ojilang '(print :hello-world)) => "今度PRINTしようよ😍❓:HELLO-WORLD❓どうカナ💦" 
```

なお、これはあくまで現時点での変換である。
（すなわちアルファクオリティである。）
ojilangとして自然なシンタックスになるように、安定版になるまでは頻繁に変更されることとなろう。

現行の設計では、ある程度ネストに耐えうるようになっている。

```lisp
(ojilang '(+ (- 1 2) 3)) => "今度+しようよ😍❓今度-しようよ😍❓1❓2❓どうカナ💦❓3❓どうカナ💦" 
```

## IF
条件分岐は以下のようになる。

```lisp
(ojilang '(if :a :b :c))

"暇ができたらさ、:Aﾉｼ
大丈夫なら:B❤️
えっ❓違う❓❓💦💦
じゃあ:Cナンチャッテ（笑）" 
```

## LET
ローカル変数宣言は以下のように変換される。

```lisp
(ojilang '(let(a)a))

"ヤッホー(^з<)🎵
Aチャン、
ﾁｭｯ😘❤️ ❤️ 
A
(^_^)v" 
```

## BLOCK
ブロック宣言は以下のように変換される。

```lisp
(ojilang '(block :a (return-from :a 0)))

"おじさんも:Aに行くの、すっごくスキ（＞＿＜）なんだよ❗
今度一緒に:Aに行こうよ❗
0ってダメかな❓
楽しみだね🥰 🥰 
なんだか、ドキドキ❤️ しちゃうね(^_^)" 
```

## TAGBODY
タグによるGOは以下のように変換される。

```lisp
(ojilang '(tagbody :start (go :start)))

"最近連絡くれないけど、忙しいのかな❓
:STARTとか好きだったよね❓
そろそろおじさんと:STARTとか行こうよ😝
今日も素敵な夢が見られますように❤️ " 
```

## ojilang-cl
[現行バージョン（1.2.0）](https://github.com/hyotang666/ojilang-cl/blob/master/src/ojilang-cl.lisp)では、都合１１の特殊形式をサポートしている。
１１しかなくとも、簡単なコードなら必要充分である。
たとえばfizzbazzは以下のように変換される。

```lisp
(ojilang '(dotimes(x 100)
            (cond
              ((and (zerop (rem x 3))
                    (zerop (rem x 5)))
               (print :fizzbazz))
              ((zerop(rem x 3))(print :fizz))
              ((zerop(rem x 5))(print :bazz))
              (t (print x)))))

"おじさんもNILに行くの、すっごくスキ（＞＿＜）なんだよ❗
ヤッホー(^з<)🎵
Xチャン、0
ﾁｭｯ😘❤️ ❤️ 
今度DECLAREしようよ😍❓今度TYPEしようよ😍❓UNSIGNED-BYTE❓X❓どうカナ💦❓どうカナ💦
最近連絡くれないけど、忙しいのかな❓
そろそろおじさんと#:G1741とか行こうよ😝
#:G1740とか好きだったよね❓
最近連絡くれないけど、忙しいのかな❓
暇ができたらさ、暇ができたらさ、今度ZEROPしようよ😍❓今度REMしようよ😍❓X❓3❓どうカナ💦❓どうカナ💦ﾉｼ
大丈夫なら今度ZEROPしようよ😍❓今度REMしようよ😍❓X❓5❓どうカナ💦❓どうカナ💦❤️
えっ❓違う❓❓💦💦
じゃあNILナンチャッテ（笑）ﾉｼ
大丈夫なら今度PRINTしようよ😍❓:FIZZBAZZ❓どうカナ💦❤️
えっ❓違う❓❓💦💦
じゃあ暇ができたらさ、今度ZEROPしようよ😍❓今度REMしようよ😍❓X❓3❓どうカナ💦❓どうカナ💦ﾉｼ
大丈夫なら今度PRINTしようよ😍❓:FIZZ❓どうカナ💦❤️
えっ❓違う❓❓💦💦
じゃあ暇ができたらさ、今度ZEROPしようよ😍❓今度REMしようよ😍❓X❓5❓どうカナ💦❓どうカナ💦ﾉｼ
大丈夫なら今度PRINTしようよ😍❓:BAZZ❓どうカナ💦❤️
えっ❓違う❓❓💦💦
じゃあこれはTだけど、大丈夫❓
今度PRINTしようよ😍❓X❓どうカナ💦ナンチャッテ（笑）ナンチャッテ（笑）ナンチャッテ（笑）
今日も素敵な夢が見られますように❤️ 
早く会いたいよ〜🥺 
突然だけど、Xチャン、今度1+しようよ😍❓X❓どうカナ💦
やっぱりおじさんとはイヤかなσ(^_^);汗汗💦💦
NIL
待っててね❗❓
#:G1741とか好きだったよね❓
暇ができたらさ、今度>=しようよ😍❓X❓100❓どうカナ💦ﾉｼ
大丈夫ならNIL❤️
えっ❓違う❓❓💦💦
じゃあそろそろおじさんと#:G1740とか行こうよ😝ナンチャッテ（笑）
今度一緒にNILに行こうよ❗
早く会いたいよ〜🥺 
NIL
待っててね❗❓ってダメかな❓
楽しみだね🥰 🥰 
今日も素敵な夢が見られますように❤️ 
(^_^)v
なんだか、ドキドキ❤️ しちゃうね(^_^)" 
```

現行では`DECLARE`がサポートされていないので、関数であるかのように扱われている点要注意。

## TODO （やるとは言っていない）
1. 残る特殊形式の設計。
2. 充分整合が取れるようになったら、パーズ（すなわちojilangからS式生成）できるようにする。

## Conclusion
地獄絵図。
