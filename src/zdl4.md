# ゼロから作るDeep Learning.Common Lispで学ぶディープラーニングの理論と実装(4)
原著の内容をCommon Lispに移植しながら学んでいくシリーズです。
詳細は原著でお読みください。
ここでは移植したCommon Lispコードについての解説や注意点を記していきます。

# 4 Learning.
## 4.1 Learning from data.
### 4.1.1 Data driven.
### 4.1.2 Train data and test data.
## 4.2 Loss functions.
### 4.2.1 Sum squared error.

```lisp
(defun sum-squared-error (array teacher)
  (numcl:* 0.5 (numcl:sum (numcl:expt (numcl:- array teacher) 2))))

* (sum-squared-error (numcl:asarray '(0.1 0.05 0.6 0.0 0.05 0.1 0.0 0.1 0.0 0.0))
                     (numcl:asarray '(0 0 1 0 0 0 0 0 0 0)))
0.0975
```

### 4.2.2 Cross entropy error

```lisp
(defun cross-entropy-error (array teacher)
  (numcl:- (numcl:sum (numcl:* teacher (numcl:log (numcl:+ array 1e-7))))))

* (cross-entropy-error (numcl:asarray '(0.1 0.05 0.6 0.0 0.05 0.1 0.0 0.1 0.0 0.0))
                       (numcl:asarray '(0 0 1 0 0 0 0 0 0 0)))
0.5108254

* (cross-entropy-error (numcl:asarray '(0.1 0.05 0.1 0.0 0.05 0.1 0.0 0.6 0.0 0.0))
                       (numcl:asarray '(0 0 1 0 0 0 0 0 0 0)))
2.3025842
```

### 4.2.3 Mini batch.

```lisp
* (defparameter *mnist* (cl-mnist:load-mnist :normalize t :one-hot-label t :flatten t :slurp t))
*MNIST*

;; 訓練データ数。
* (getf *mnist* :train-images-count)
60000

;; 一画像のバイト数。
* (getf *mnist* :train-images-size)
784

;; 一ラベルのバイト数。
* (getf *mnist* :train-labels-size)
1

;; :ONE-HOT-LABELをTRUE指定したので取り出すとBIT-VECTORとなる。
* (funcall (getf *mnist* :train-labels))
#*0000010000
```

SLURPERはデフォルトでは先頭から順次読み込みますが、引数を与えることでランダムアクセスも可能です。

```lisp
* (defparameter index (loop :repeat 10 :collect (random 60000)))
INDEX

* index
(39788 19943 49314 45683 17770 21816 14657 32591 2225 47617)

* (let* ((index (car index))
         (offset (getf *mnist* :train-labels-offset))
         (size (getf *mnist* :train-labels-size))
         (file-position (+ offset (* size index)))
         (slurper (getf *mnist* :train-labels)))
    (funcall slurper file-position))
#*1000000000
```
### 4.2.4 Implementation of cross entropy error.

Numclは開発途上のライブラリで、機能にはまだまだ不備があります。
具体的にはARGMAXは持ちません。
いい加減なものですが、実装していきます。

```lisp
(defun argmax (array &key axes)
  (flet ((doit (&key size pred reader)
           (do ((champion 0)
                (index 0)
                (size (funcall size))
                (current-position 0 (1+ current-position)))
             ((funcall pred current-position size)
              index)
             (let ((challenger (funcall reader current-position)))
               (when (= challenger (max challenger champion))
                 (setf champion challenger
                       index current-position))))))
    (cond
      ((null axes)
       (doit :size (lambda () (array-total-size array))
             :pred (lambda (position size) (not (< position size)))
             :reader (lambda (position) (row-major-aref array position))))
      ((and (= 2 (array-rank array))
            (zerop axes))
       (loop :for col :below (array-dimension array 1)
             :collect (doit :size (lambda () (array-dimension array 0))
                            :pred (lambda (row size) (not (< row size)))
                            :reader (lambda (row) (aref array row col)))))
      ((and (= 2 (array-rank array))
            (= 1 axes))
       (loop :for row :below (array-dimension array 0)
             :collect (doit :size (lambda () (array-dimension array 1))
                            :pred (lambda (col size) (not (< col size)))
                            :reader (lambda (col) (aref array row col)))))
      (t (error "NIY")))))
```
以下の実装も、もっと綺麗に書けそうな気がします。
（とくに`ASARRAY`を使っているところ）

```lisp
(defun cross-entropy-error (y teach)
  (when (= 1 (array-rank y))
    (setf teach (numcl:reshape teach (list 1 (numcl:size teach)))
          y (numcl:reshape y (list 1 (numcl:size y)))))
  (when (= (numcl:size y) (numcl:size teach))
    (setf teach (argmax teach :axes 1)))
  (let ((batch-size (nth 0 (numcl:shape y))))
    (numcl:/
      (numcl:-
        (numcl:sum
          (numcl:log
            (numcl:+
              (numcl:asarray
                (mapcar (lambda (i j) (numcl:aref y i j))
                        (coerce (numcl:arange batch-size) 'list) teach))
              1e-7))))
      batch-size)))
```

### 4.2.5 Why set loss function?
## 4.3 Numerical Differential.
### 4.3.1 Differential.

```lisp
(defun numerical-diff (function number)
  (let ((h 1e-4))
    (numcl:/ (numcl:- (funcall function (numcl:+ number h))
                      (funcall function (numcl:- number h)))
             (numcl:* 2 h))))
```
### 4.3.2 Examples.
REPLに於いて自由シンボルの`*`は直前の返り値を表します。
同様に`**`は二つ前の返り値を表します。
言語仕様で`***`まで定義されています。

```lisp
* (lambda (x) (numcl:+ (numcl:* 0.01 (numcl:expt x 2)) (numcl:* 0.1 x)))
#<FUNCTION (LAMBDA (X)) {537D075B}>

* (numerical-diff * 5)
0.2002716

* (numerical-diff ** 10)
0.3004074
```

### 4.3.3 Partial differential.

```lisp
* (numerical-diff (lambda (x) (numcl:+ (numcl:* x x) (numcl:expt 4.0 2.0))) 3.0)
5.9890747

* (numerical-diff (lambda (x) (numcl:+ (expt 3.0 2.0) (numcl:* x x))) 4.0)
8.0013275
```

## 4.4 Gradient.

```lisp
(defun numerical-gradient (function input)
  (let ((h 1e-4)
        (grad (numcl:zeros-like input)))
    (dotimes (index (numcl:size input))
      (let ((tmp-value (row-major-aref input index)))
        (setf (row-major-aref input index) (numcl:+ tmp-value h))
        (let ((fxh1 (funcall function input)))
          (setf (row-major-aref input index) (numcl:- tmp-value h))
          (let ((fxh2 (funcall function input)))
            (setf (row-major-aref grad index) (numcl:/ (numcl:- fxh1 fxh2) (numcl:* 2 h))
                  (row-major-aref input index) tmp-value)))))
    grad))

* (numerical-gradient (lambda (x) (numcl:+ (numcl:expt (aref x 0) 2) (numcl:expt (aref x 1) 2)))
                      (numcl:asarray '(3.0 4.0)))
#(5.9890747 8.0013275)

* (numerical-gradient (lambda (x) (numcl:+ (numcl:expt (aref x 0) 2) (numcl:expt (aref x 1) 2)))
                      (numcl:asarray '(0.0 2.0)))
#(0.0 3.9982796)

* (numerical-gradient (lambda (x) (numcl:+ (numcl:expt (aref x 0) 2) (numcl:expt (aref x 1) 2)))
                      (numcl:asarray '(3.0 0.0)))
#(5.993843 0.0)
```

### 4.4.1 Gradient descent.
```lisp
(defun gradient-descent (function &key initial-value (learning-rate 0.01) (step 100))
  (let ((x initial-value))
    (dotimes (i step x)
      (let ((grad (numerical-gradient function x)))
        (setf x (numcl:- x (numcl:* learning-rate grad)))))))

* (gradient-descent (lambda (x) (numcl:+ (numcl:expt (numcl:aref x 0) 2)
                                         (numcl:expt (numcl:aref x 1) 2)))
                    :initial-value (numcl:asarray '(-3.0 4.0))
                    :learning-rate 0.1)
#(-6.116103e-10 8.156569e-10)
```

### 4.4.2 Gradient for neural network.
原著は分かりやすさが重視され、また各節のコードが共に存在できるようにクラスが定義されています。

ここではCommon Lispらしく、次々と改良を施していくスタイルで行きます。
中身が書き換わることで古いコード例は動かなくなります。
中身を変える必要がないコードは再掲しません。

```lisp
;; １レイヤーのネットワークを返すコンストラクタ。
(defun make-simple-net (&optional (weight (numcl:normal 0.0d0 1.0d0 '(2 3) 'single-float)))
  (list (make-layer weight
                    (numcl:zeros (second (numcl:shape weight)))
                    'softmax)))

;; PredictはForwardのaliasでしかない。
;; 以下のようにすることで呼び出しコストを軽減する。
;; 呼び出されたPredictが間接的にForwardを呼ぶのではなく、
;; Predictの呼び出しがそのままForwardの呼び出しとなる。
(setf (symbol-function 'predict) #'forward)

(defun loss (net input teach)
  (cross-entropy-error (predict net input)
                       teach))

* (let* ((net (make-simple-net (numcl:asarray '((0.47355232 0.9977393 0.84668094)
                                                (0.85557411 0.03563661 0.69422093)))))
         (input (numcl:asarray '(0.6 0.9)))
         (p (predict net input))
         (teach (numcl:asarray '(0 0 1))))
    (values p
            (argmax p)
            (loss net input teach)
            (numerical-gradient (lambda (w) (declare (ignore w))
                                  (loss net input teach))
                                (weight (car net)))))
#(1.0541481 0.63071656 1.1328074)
2
0.9280684
#2A((0.21904707 0.14334917 -0.3629923) (0.32931566 0.21487474 -0.54478645))
```

## 4.5 Implementation of learning algorithm.
### 4.5.1 Two layered neural network.

```lisp
;; コンストラクタ。
(defun make-net (&key sizes activators (weight-init 0.01))
  (loop :for (size . rest) :on sizes
        :for activator :in activators
        :while rest
        :collect (make-layer
                   (numcl:* weight-init
                            (numcl:asarray
                              (numcl:normal 0.0d0 1.0d0 (list size (car rest))
                                            'single-float)))
                   (numcl:zeros (car rest))
                   activator)))

(defun accuracy (net input teach)
  (let* ((y (numcl:asarray (argmax (predict net input) :axes 1)))
         (teach (numcl:asarray (argmax teach :axes 1))))
    (numcl:/ (numcl:= y teach)
             (float (nth 0 (numcl:shape input))))))

(defun update-params (net input teach &optional (rate 1))
  (let ((f (lambda (weight)
             (declare (ignore weight))
             (loss net input teach))))
    (dolist (layer net net)
      (setf (weight layer)
            (numcl:- (weight layer)
                     (numcl:* rate (numerical-gradient f (weight layer))))
            (bias layer)
            (numcl:- (bias layer)
                     (numcl:* rate (numerical-gradient f (bias layer))))))))
```

### 4.5.2 Implementation of mini batch learning.
Slurperが返す配列はnumcl配列でない点要注意。

また、2020年７月現在、numclのバックエンドはCommon Lispのみです。
これは配列の演算が逐次的に行われることを意味します。
よって`NUMERICAL-GRADIENT`は恐ろしく遅くなります。
筆者は以下のコードを最後まで走らせることを諦めました。
効率的な実装が次章ででます。

```lisp
* (let ((net (make-net :sizes '(784 50 10) :activators '(sigmoid softmax))))
    (loop :repeat 1
          :for index := (random (getf *mnist* :train-images-count))
          :for image := (numcl:asarray
                          (funcall (getf *mnist* :train-images)
                                   (+ (getf *mnist* :train-images-offset)
                                      (* (getf *mnist* :train-images-size)
                                         index))))
          :for label := (numcl:asarray
                          (funcall (getf *mnist* :train-labels)
                                 (+ (getf *mnist* :train-labels-offset)
                                    (* (getf *mnist* :train-labels-size)
                                       index))))
          :do (update-params *net* image label 0.1)
          :collect (loss *net* image label)))
```

### 4.5.3 Evaluate by test data.
## 4.6 Summary.
