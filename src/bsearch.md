# Sequencial saerch faster than binary search in SBCL
## Meta note
### 対象読者
* Common Lisp Evangelist.
* SBCL Evangelist.
## Introduction
SBCLに於いて線形探索が２分探索より速かったというお話。

## Theory
線形探索はO(n)で２分探索はO(log n)なので２分探索のほうが速いのは皆様ご承知の通り。
大胆な発言が許されるなら、要素数が３を超えたら２分探索のほうが理屈上は速くなります。

## Common Lisp comunity
他の言語は知りませんが、Common Lispという言語とその文化圏では割と速度や効率が重要視される傾向にあるように見えます。
速いということや効率の良さを謳ったライブラリも散見されます。
ところがそんなライブラリのソースコードを覗いてみると割と線形探索が多様されており、２分探索をほとんど見ないように思います。
ALEXANDRIAのような主要なユーティリティライブラリにも２分探索アルゴリズムは含まれておりません。

## bsearch
「それ、２分探索にしたらそれだけでもっとはやくなるんじゃないの？」
というわけでさくっと関数を書いてみました。

```lisp
(defun bsearch(item vector &key (key #'identity)(test #'eql)(start 0)(end (length vector)) (compare #'<)(default nil))
  (declare (type fixnum start end)
	   (type function key test compare)
	   (type simple-vector vector)
	   (dynamic-extent key test compare)
	   )
  (assert(<= 0 start end (length vector)))
  (labels((ON-THE-NODE(center %)
	    (declare(type fixnum center %))
	    (if(zerop center)
	      (REACHED-LEAF % (+ center start))
	      (CHECK-CONTENTS(+ center start))))
	  (REACHED-LEAF(% index)
	    (let((i(if(zerop %) ; (= end start)
		     (1+ index)
		     index)))
	      (declare(type fixnum i))
	      (DETERMINE-RETURN-VALUE i (svref vector i))))
	  (DETERMINE-RETURN-VALUE(index target)
	    (if (funcall test item (funcall key target))
	      (values target index)
	      (values default nil)))
	  (CHECK-CONTENTS(index)
	    (let*((target(svref vector index))
		  (elt (funcall key target)))
	      (if (funcall test item elt)
		(values target index)
		(REC elt index))))
	  (REC(elt index)
	    (if(funcall compare item elt)
	      (setf end index)
	      (setf start index))
	    (multiple-value-call #'ON-THE-NODE (floor (- end start)2)))
	  )
    (declare(inline REACHED-LEAF DETERMINE-RETURN-VALUE CHECK-CONTENTS REC))
    (multiple-value-call #'ON-THE-NODE (floor (- end start) 2))))
```

## Bench
ベンチは以下のようにして取りました。

```lisp
(defun vec(size)
  (make-array size :initial-contents (loop :for i :below size :collect i)))

(defun bench(vector)
  (let((size(length vector)))
    (print :find)
    (time(dotimes(x 10000)
	   (find (random size)vector)))
    (print :bsearch)
    (time(dotimes(x 10000)
	   (bsearch (random size)vector)))))
```

CCLでは以下の通り。
（重要なところのみ抽出してあります）

```lisp
? (bench(vec 10))
:FIND
took 5 milliseconds
:BSEARCH
took 30 milliseconds

? (bench(vec 100))
:FIND
took 19 milliseconds
:BSEARCH
took 52 milliseconds

? (bench(vec 1000))
:FIND
took 169 milliseconds
:BSEARCH
took 75 milliseconds

? (bench(vec 10000))
:FIND
took 1,640 milliseconds
:BSEARCH
took 98 milliseconds
```

２分探索はそれ専用のオーバーヘッドを伴うので、要素数が少ないうちは線形探索のほうが速かったりもしますが、要素数が増えるに従い速度差に開きが出てきています。
これは想定どおりの結果です。

## SBCL
さて問題はSBCLです。

```lisp
* (bench(vec 10))
:FIND
0.001 seconds
:BSEARCH
0.008 seconds

* (bench(vec 100))
:FIND
0.002 seconds
:BSEARCH
0.008 seconds

* (bench(vec 1000))
:FIND
0.001 seconds
:BSEARCH
0.011 seconds

* (bench(vec 10000))
:FIND
0.002 seconds
:BSEARCH
0.014 seconds

* (bench(vec 100000))
:FIND
0.001 seconds
:BSEARCH
0.018 seconds

* (bench(vec 1000000))
:FIND
0.001 seconds
:BSEARCH
0.027 seconds
```
驚くべきことにどれだけ要素数を増やしても線形探索に敵いません。
それどころかむしろ差が開いていく一方です。

## Limitation
いろいろ試してみたところ、EQxxxファミリー以外の比較関数を指定したり、:keyパラメタを指定したりすると遅くなるようです。

### Specifing :TEST

```lisp
(defun bench(vector)
  (let((size(length vector)))
    (print :find)
    (time(dotimes(x 10000)
	   (find (random size)vector :test #'=)))
    (print :bsearch)
    (time(dotimes(x 10000)
	   (bsearch (random size)vector :test #'=)))))

* (bench(vec 10))
:FIND
0.006 seconds
:BSEARCH
0.006 seconds

* (bench(vec 100))
:FIND
0.032 seconds
:BSEARCH
0.008 seconds

* (bench(vec 1000))
:FIND
0.291 seconds
:BSEARCH
0.018 seconds

* (bench(vec 10000))
:FIND
2.738 seconds
:BSEARCH
0.016 seconds
```
### Specifying :KEY.

```lisp
(defun bench(vector)
  (let((size(length vector)))
    (print :find)
    (time(dotimes(x 10000)
	   (find (code-char(random size))vector :key #'code-char)))
    (print :bsearch)
    (time(dotimes(x 10000)
	   (bsearch (code-char(random size))vector :key #'code-char
		    :compare #'char<)))
    ))

* (bench(vec 10))
:FIND
0.007 seconds
:BSEARCH
0.006 seconds

* (bench(vec 100))
:FIND
0.030 seconds
:BSEARCH
0.008 seconds

* (bench(vec 1000))
:FIND
0.283 seconds
:BSEARCH
0.011 seconds

* (bench(vec 1000))
:FIND
1.858 seconds
:BSEARCH
0.007 seconds
```

## Conclusion
SBCLの列関数は謎に速い。
SBCLのソースコードにまで潜り込んであれこれ見てみたのですが、なぜこんなに速くなるのか分からずじまいでした。

これまで筆者はシーケンスが、例えば文字しか含まないというような場合、:TESTで`CHAR=`を指定するとかしてたんですが、SBCLに限って言えば速度面に於いてマイナスの効果が出てしまうものであるようです。

## Appendix
いろいろ試している最中、ローカル関数を使っているのがいけないのでは？と思い、別バージョンも書いてみました。

```lisp
(defun bsearch2(item vector &key (key #'identity)(test #'eql)(start 0)(end (length vector)) (compare #'<)(default nil))
  (declare (type fixnum start end)
	   (type function key test compare)
	   (dynamic-extent key test compare)
	   (type simple-vector vector))
  (assert(<= 0 start end (length vector)))
  (prog(center % index target elt)
    REC
    (multiple-value-setq(center %)(floor(- end start)2))
    ON-THE-NODE
    (setq index (+ center start))
    (if(zerop center)
      (go REACHED-LEAF)
      (go CHECK-CONTENTS))
    REACHED-LEAF
    (when(zerop %)
      (incf index))
    (setq target (svref vector index)
	  elt (funcall key target))
    (if (funcall test item elt)
      (return (values target index))
      (return (values default nil)))
    CHECK-CONTENTS
    (setq target(svref vector index)
	  elt(funcall key target))
    (if (funcall test item elt)
      (return(values target index))
      (progn
	(if(funcall compare item elt)
	  (setf end index)
	  (setf start index))
	(go REC)))))
```

結論から言うと、SBCLでは関係ないようです。
`INLINE`宣言が正しく機能しているようで、速度差は現れませんでした。

しかし、CCLでは有効なようです。
CCLはローカル関数の`INLINE`化をしないようで、以下のように速くなりました。

```lisp
(defun bench(vector)
  (let((size(length vector)))
    (print :find)
    (time(dotimes(x 10000)
	   (find (code-char(random size))vector :key #'code-char)))
    (print :bsearch)
    (time(dotimes(x 10000)
	   (bsearch (code-char(random size))vector :key #'code-char
		    :compare #'char<)))
    (print :bsearch2)
    (time(dotimes(x 10000)
	   (bsearch2 (code-char(random size))vector :key #'code-char
		    :compare #'char<)))))

? (bench(vec 100))
:FIND
took 33 milliseconds
:BSEARCH
took 55 milliseconds
:BSEARCH2
took 26 milliseconds
```

