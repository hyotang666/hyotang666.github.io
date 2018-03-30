# Generalized zundoko function in Common Lisp
## Meta Info
### 対象読者
広く暇を持て余したプログラマ。

## Zundoko function version 1.
[ズンドコ関数](https://twitter.com/kumiromilk/status/707437861881180160) をCommon Lispで書いてみました。

```lisp
(defun zundoko(&optional(memory 0))
  (if(= #B11110 memory)
    (Prin1 :kiyoshi!)
    (zundoko(shift memory(print-zundoko(random 2))))))

(defun print-zundoko(n)
  (ecase n
    (1 (prin1 :zun))
    (0 (PRIN1 :doko)))
  n)

(defun shift(acc it)
  (ldb(byte 5 0)(dpb it(byte 1 0)(ash acc 1))))

* (zundoko)
:DOKO:DOKO:ZUN:DOKO:DOKO:ZUN:ZUN:ZUN:ZUN:ZUN:ZUN:ZUN:DOKO:KIYOSHI!
:KIYOSHI!
```

出力されるのは「:ZUN」と「:DOKO」のみなので、これは０と１、２つの数があれば表すのに充分であるということです。

Common Lispで２進数を書く場合、`#B`ディスパッチマクロを使ってリテラルに書くことが出来ます。
`30`というマジックナンバーが書かれているより、メモリの状態が分かりやすかろうと思われます。

Common Lispは仮想ビットベクタをサポートしています。
仮想ビットベクタとは、正数を事実上のビットベクタと見なす、というものです。
いにしえのC言語では、`char`で変数を宣言して、それをビットベクタと見なし、各種フラグを格納するのに使うということがありました。
丁度それと似たようなものです。

そしてCommon LispはBIGNUMをサポートする言語でもあります。
すなわち、メモリが許す限り、いくらでも大きな正数を持てます。
それはつまり、メモリが許す限り、事実上無限のビットベクタがあると見なすことができるということでもあります。

おかげでちょっと厄介なのが、ビットシフトして、オーバーフローを起こすことが出来ないという点です。
それをシミュレートしているのが関数`SHIFT`です。

## Zundoko function version 2.
前節のバージョンでは、キヨシのズンドコ節は表わせますが、ドリフのズンドコ節は表わせません。
（すなわち、「ズンズンズンズンドコ」でなく「ズンズンズンズンズンズンドコ」と出力したら「志村！」と出力する。）

早速対応しましょう。

```lisp
(defun shift(acc it &key(max (integer-length acc)))
  (ldb(byte max 0)(dpb it(byte 1 0)(ash acc 1))))

(defun zundoko(result finally &optional(memory 0))
  (if(= result memory)
    (prin1 finally)
    (zundoko result finally (shift memory (print-zundoko(random 2))
                                   :max(integer-length result)))))

* (zundoko #B1111110 :shimura)
:DOKO:DOKO:DOKO:ZUN:DOKO:ZUN:ZUN:DOKO:ZUN:DOKO:DOKO:DOKO:ZUN:DOKO:DOKO:ZUN:DOKO:DOKO:DOKO:ZUN:DOKO:ZUN:DOKO:DOKO:ZUN:DOKO:DOKO:ZUN:ZUN:ZUN:DOKO:ZUN:ZUN:DOKO:DOKO:ZUN:DOKO:DOKO:DO:DOKO:ZUN:ZUN:ZUN:DOKO:DOKO:ZUN:ZUN:DOKO:ZUN:DOKO:ZUN:DOKO:DOKO:DOKO:DOKO:ZUN:DOKO:DOKO:ZUN:DOKO:ZUN:DOKO:ZUN:ZUN:DOKO:DOKO:ZUN:ZUN:ZUN:ZUN:ZUN:ZUN:DOKO:SHIMURA
:SHIMURA
```

## Zundoko function version 3.
前節のバージョンでキヨシのズンドコ節のみならずドリフのズンドコ節にも対応できるようになりましたが、これでは広末涼子の大好きを表すことが出来ません。
（すなわち、「とっても」と「大好きよ」をランダムに出力し、「とってもとってもとってもとってもとってもとっても大好きよ」の出力が得られたら「ダーリンILoveYouダーリン」と出力する。）

早速対応しましょう。

```lisp
(defun zundoko (sequence &optional finally)
  (let*((length(length sequence))
        (vector(coerce (remove-duplicates sequence) 'vector))
        (result 0))
    (labels((MAIN(memory)
              (if(= result memory)
                (when finally (write finally)NIL)
                (MAIN(shift memory (PRINT-ZUNDOKO(random 2))
                            :max length))))
            (PRINT-ZUNDOKO(n)
              (write(aref vector n))
              n)
            (SETUP()
              (map nil (lambda(elt)
                         (setf result (shift result (position elt vector)
                                             :max length)))
                   sequence))
            (START()
              (loop :with acc = 0
                    :repeat length
                    :do (setf acc (shift acc (PRINT-ZUNDOKO(random 2))
                                         :max length))
                    :finally (return acc)))
          )
      (SETUP)
      (MAIN (START)))))

* (zundoko '(とっても とっても とっても とっても とっても とっても 大好きよ) '#:ダーリンILoveYouダーリン)
大好きよ大好きよ大好きよとってもとっても大好きよ大好きよとってもとってもとっても大好きよ大好きよとっても大好きよとっても大好きよ大好きよとってもとってもとっても大好きよ大好きよとっても大好きよとってもとってもとっても大好きよとっても大好きよ大好きよとっても大好きよ大好きよとってもとっても大好きよとってもとってもとってもとってもとってもとっても大好きよ#:ダーリンILOVEYOUダーリン
NIL
```

## Zundoko function version 4.
前節のバージョンでは広末涼子の大好きまで表わすことが出来るようになりましたが、まだ足りません。
今のままではアントニオ猪木の締めを表すことが出来ません。
（すなわち、「１！」、「２！」、「３！」をランダムに出力し、「１！２！３！」と出力されたら「ダーッ！」と出力する。）

早速対応しましょう。

```lisp
(defun shift(memory num &key (max (integer-length memory)) (unit 0))
  (ldb(byte (* max unit) 0)(dpb num(byte unit 0)(ash memory unit))))

(defun zundoko (sequence &optional finally)
  (let*((length(length sequence))
        (vector(remove-duplicates(coerce sequence 'vector)))
        (byte(length vector))
        (result 0))
    (labels((MAIN(memory)
              (if(= result memory)
                (when finally (write finally)NIL)
                (MAIN(shift memory (PRINT-ZUNDOKO(random byte))
                            :max length :unit byte))))
            (PRINT-ZUNDOKO(n)
              (write(aref vector n))
              n)
            (SETUP()
              (map nil (lambda(elt)
                         (setf result (shift result (position elt vector)
                                             :max length :unit byte)))
                   sequence))
            (START()
              (loop :with acc = 0
                    :repeat length
                    :do (setf acc (shift acc (PRINT-ZUNDOKO(random byte))
                                         :max length :unit byte))
                    :finally (return acc)))
          )
      (SETUP)
      (MAIN (START)))))

* (zundoko "123" "だーっ！")
#\3#\2#\2#\3#\2#\1#\2#\1#\3#\2#\2#\1#\2#\2#\2#\1#\2#\3"だーっ！"
NIL
```

これで今話題の[ポプテピピック](https://renidentia991.bitbucket.io/pptp.html) も引数を渡すだけで再現できますね！

```lisp
(defvar *|pop'n destruction|* "
　　　　　　　　　　　　　　　　　　　　　　　　　-------　､　ｒへ------､
　　　　　　　　　　　　　　　　　　 ＿__､+''\"~　八 　　　　　~''＜ つ　　　＼
　　　　　　　　　　　　　　 _､+''\"~　 ./ ,:　　　　＾ 　 丶　　　　　ヽく＿＿　∧
　　　　　　 , -r-　　　　 / 　 　 　 / / -､　　　　'´￣!＼‘,　‘:,　 |　　　 ＼/
　　　　　／ .∧　　 ＼..ﾞ|/＿,,.｡*'/ /|,/＼＿_／^ヽ /,,_　`! 　 ',　|
　　　 ／　　　∧　　　 ＼　〈〉.　,　.|　ｙ''\"ヽ______ 7´⌒ヽ _|　　ト;′
　　 〈　　　　　 ∧ （竹） .∨...　｜｜ ,r　ヽ 三三/　　‘:, Y!　 .|,′
　　　∨ 　 　 　 ∧　　　　∨...　|　.|八 °,!三三{.　° ﾉ 八　/---､
.　　　 ∨ 　 　 　 ∧　　　　∨....∨（.＾ 　　 ､_,､_, `　　　,.ｨ^!./＿　　',
　　　　 ∨　　 ／三ﾐメ、　　∨　冫≧=‐------‐ｧ=≦/ .|/　　 } 　 !
.　　　 　 ∨三三三三圦__.＼＼/　＼|ｲ `''＜:＼/:::::／　　|　　（＿__）
　　　 〈〉　　 ノ　ゞ三ｼ.　＼　＼;　　 { /　　　 ｀¨ﾍ⌒こフ　.∨ 〔､､､_）
　　　　　　　）　　　 　 　 　 ＼八　　`^i､_ __　　　 ＼＼　　 .∨ ｀¨´
　　　　 ＜ﾍ＿/三＼＿／｜＼､.＼　/::/　 ）　　　　 `'´　＿ ,∨､
　　　　|＼三三三三:／　　|.　　＼/^ゞク≦---‐=≦::::::「:::::::::|::::/
　　　　　　　　 　 　 　 　 　 　 　 /:::::::/:::::::/:::::::::;′:::::::::|::::::::::|:人")

* (zundoko '(ポ プ テ ピピック) *|pop'n destruction|*)
プピピックポポププテポピピックピピックピピックプポピピックテプポピピックポテプテポピピックピピックピピックピピックピピックポピピックピピックピピックピピックテテピピックポプピピックプポピピックピピックピピックピピックポポピピックピピックポテポピピックポテププピピックポテポピピックポプピピックポテポポテピピックプテピピックテポポプポテテポピピックプポププテプポテプポポテテピピックピピックポテププテププピピックプポピピックテプテテプテポテププテプピピックポププポプテピピック"
　　　　　　　　　　　　　　　　　　　　　　　　　-------　､　ｒへ------､
　　　　　　　　　　　　　　　　　　 ＿__､+''\"~　八 　　　　　~''＜ つ　　　＼
　　　　　　　　　　　　　　 _､+''\"~　 ./ ,:　　　　＾ 　 丶　　　　　ヽく＿＿　∧
　　　　　　 , -r-　　　　 / 　 　 　 / / -､　　　　'´￣!＼‘,　‘:,　 |　　　 ＼/
　　　　　／ .∧　　 ＼..ﾞ|/＿,,.｡*'/ /|,/＼＿_／^ヽ /,,_　`! 　 ',　|
　　　 ／　　　∧　　　 ＼　〈〉.　,　.|　ｙ''\"ヽ______ 7´⌒ヽ _|　　ト;′
　　 〈　　　　　 ∧ （竹） .∨...　｜｜ ,r　ヽ 三三/　　‘:, Y!　 .|,′
　　　∨ 　 　 　 ∧　　　　∨...　|　.|八 °,!三三{.　° ﾉ 八　/---､
.　　　 ∨ 　 　 　 ∧　　　　∨....∨（.＾ 　　 ､_,､_, `　　　,.ｨ^!./＿　　',
　　　　 ∨　　 ／三ﾐメ、　　∨　冫≧=‐------‐ｧ=≦/ .|/　　 } 　 !
.　　　 　 ∨三三三三圦__.＼＼/　＼|ｲ `''＜:＼/:::::／　　|　　（＿__）
　　　 〈〉　　 ノ　ゞ三ｼ.　＼　＼;　　 { /　　　 ｀¨ﾍ⌒こフ　.∨ 〔､､､_）
　　　　　　　）　　　 　 　 　 ＼八　　`^i､_ __　　　 ＼＼　　 .∨ ｀¨´
　　　　 ＜ﾍ＿/三＼＿／｜＼､.＼　/::/　 ）　　　　 `'´　＿ ,∨､
　　　　|＼三三三三:／　　|.　　＼/^ゞク≦---‐=≦::::::「:::::::::|::::/
　　　　　　　　 　 　 　 　 　 　 　 /:::::::/:::::::/:::::::::;′:::::::::|::::::::::|:人"
```
