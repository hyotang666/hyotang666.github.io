# ゼロから作るDeep Learning.Common Lispで学ぶディープラーニングの理論と実装(2)
源著の内容をCommon Lispに移植しながら学んでいくシリーズです。
詳細は源著でお読みください。
ここでは移植したCommon Lispコードについての解説や注意点を記していきます。

# 2 Perceptron
## 2.1 What is
## 2.2 Simple logicals
### 2.2.1 AND gate.
### 2.2.2 NAND gate and OR gate.
## 2.3 Perceptron implementation.
### 2.3.1 Naive implementation.
weightやbiasといったスロットを持つ関数ということで、ここではcloser-mopを使い`FUNCALLABLE-STANDARD-CLASS`を定義することとする。
とはいえ、その理由は「せっかくあるから使ってみるか」くらいのものでしかなく、正直LET-OVER-LAMBDAでいいような気もする。

```lisp
(defclass perceptron ()
  ((w1 :initarg :w1 :accessor w1)
   (w2 :initarg :w2 :accessor w2)
   (bias :initarg :bias :accessor bias))
  (:metaclass c2mop:funcallable-standard-class))

(defmethod initialize-instance :after ((o perceptron) &key function)
  (c2mop:set-funcallable-instance-function o function))
```
次にperceptronを定義する構文をマクロとして定義する。
`WITH-SLOTS`があることで暗黙理にシンボルがインジェクトされるのは、通常は良くないスタイルだがここでは目を瞑る。
というのも&RESTで受けるinitargsで、キーワードとはいえ同名のシンボルを使って初期値を指定するからだ。

```lisp
(defmacro define-perceptron ((name &rest initargs) lambda-list &body body)
  `(progn
     (setf (symbol-function ',name)
             (make-instance 'perceptron ,@initargs :function
                            (lambda ,lambda-list
                              (with-slots (w1 w2 bias) (symbol-function ',name)
                                (declare (ignorable w1 w2 bias))
                                ,@body))))
     ',name))
```
上で定義した構文を使い、各種perceptronを定義していく。
名前の衝突を避けるためバーティカルバーで左右を挟んである点要注意。

```lisp
(define-perceptron (|and| :w1 0.5 :w2 0.5 :bias 0.7) (x1 x2)
  (if (<= (+ (* x1 w1) (* x2 w2)) bias)
    0
    1))

(define-perceptron (|nand| :w1 -0.5 :w2 -0.5 :bias -0.7) (x1 x2)
  (if (<= (+ (* x1 w1) (* x2 w2)) bias)
    0
    1))

(define-perceptron (|or| :w1 0.5 :w2 0.5 :bias 0.4) (x1 x2)
  (if (<= (+ (* x1 w1) (* x2 w2)) bias)
    0
    1))

(define-perceptron (|xor|) (x1 x2)
  (|and| (|nand| x1 x2)
         (|or| x1 x2)))
```

### 2.3.2 Weight and Bias.

```lisp
* (defvar *x* (numcl:asarray '(0 1)))
*X*

* (defvar *w* (numcl:asarray '(0.5 0.5)))
*W*

* (defvar *b* -0.7)
*B*

* (numcl:* *w* *x*)
#(0.0 0.5)

* (numcl:sum (numcl:* *w* *x*))
0.5

* (numcl:+ (numcl:sum (numcl:* *w* *x*)) *b*)
-0.19999999
```

### 2.3.3 Implementation with weight and bias.
前節で定義したコードをnumcl対応版に変更する。

```lisp
(defclass perceptron ()
  ((weight :initarg :weight :accessor weight)
   (bias :initarg :bias :accessor bias))
  (:metaclass c2mop:funcallable-standard-class))

(defmacro define-perceptron ((name &rest initargs) lambda-list &body body)
  `(progn
     (setf (symbol-function ',name)
             (make-instance 'perceptron ,@initargs :function
                            (lambda ,lambda-list
                              (with-slots (weight bias) (symbol-function ',name)
                                (declare (ignorable weight bias))
                                ,@body))))
     ',name))

(define-perceptron (|and| :weight (numcl:asarray #(0.5 0.5)) :bias -0.7) (x1 x2)
  (if (<= (numcl:+ (numcl:sum (numcl:* weight (numcl:asarray (vector x1 x2))))
                   bias)
          0)
    0
    1))

(define-perceptron (|nand| :weight (numcl:asarray #(-0.5 -0.5)) :bias 0.7) (x1 x2)
  (if (<= (numcl:+ (numcl:sum (numcl:* weight (numcl:asarray (vector x1 x2))))
                   bias)
          0)
    0
    1))

(define-perceptron (|or| :weight (numcl:asarray #(0.5 0.5)) :bias -0.2) (x1 x2)
  (if (<= (numcl:+ (numcl:sum (numcl:* weight (numcl:asarray (vector x1 x2))))
                   bias)
          0)
    0
    1))
```

## 2.4 Limitation of perceptron
### 2.4.1 XOR gate.
### 2.4.2 Linear and Non-Linear.
## 2.5 Multi layered perceptron.
### 2.5.1 Combine gates.
### 2.5.2 Implementation of XOR gate.
`XOR`は既存のperceptronを利用して定義できるので通常の`DEFUN`で定義できる。

```lisp
(defun |xor| (x1 x2)
  (|and| (|nand| x1 x2) (|or| x1 x2)))
```

## 2.6 From NAND to computer.
## 2.7 Summary.
本章で学んだこと。

* Closer-mopを使うことで`FUNCALLABLE-STANDARD-CLASS`を継承したクラスを定義できる。
* DSLを作ってからその言語でアプリを書けるというのがCommon Lispの強みの一つである。
