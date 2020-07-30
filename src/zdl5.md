# ゼロから作るDeep Learning.Common Lispで学ぶディープラーニングの理論と実装(5)
原著の内容をCommon Lispに移植しながら学んでいくシリーズです。
詳細は原著でお読みください。
ここでは移植したCommon Lispコードについての解説や注意点を記していきます。

# 5 Back propagation.
## 5.1 Computational graph.
### 5.1.1 With computational graph.
### 5.1.2 Local computation.
### 5.1.3 Why.
## 5.2 Chain rule.
### 5.2.1 Back propagation for computational graph.
### 5.2.2 What is.
### 5.2.3 Chain rules and computational graph.
## 5.3 Back propagation.
### 5.3.1 Back propagation for add layer.
### 5.3.2 Back propagation for multiply layer.
### 5.3.3 Example.
### 5.4 Implementation of simple layer.
#### 5.4.1 Implementation of multiply layer.
`FORWARD`と`BACKWARD`は本来メソッドとして実装したいところです。
しかしながらCommon Lispのメソッドはシグネチャが共通の構造を持たねばならないという制約があります。
すなわち、あるクラスへのメソッドは引数が２つだが別なクラスへのメソッドは引数が１つである、というような定義をするのは難しいのです。
不可能ではないのですが、その場合は引数全てを&RESTで受けて本体内で分配するという手段をとらねばなりません。

ここではForward、Backward両関数をスロットに格納することにします。
こうすることで各関数は自由なラムダリストを持てます。

```lisp
;; 抽象クラス。
;; 各ConcreteクラスがForward、Backwardスロットを定義しなければエラー。
(defstruct layer
  (forward (error "Forward slot is required.") :type function)
  (backward (error "Backward slot is required.") :type function))

;; 表示が長くなりすぎないように。
(defmethod print-object ((o layer) stream)
  (print-unreadable-object (o stream :type t)))

;; インターフェースとしてのFORWARD関数。
(defun forward (layer &rest args)
  (apply (layer-forward layer) layer args))

;; インターフェースとしてのBACKWARD関数。
(defun backward (layer &rest args)
  (apply (layer-backward layer) layer args))

(defstruct 
    (*-layer
     ;; LAYER 構造体を継承。
     (:include layer)
     ;; 独自のコンストラクタを定義。
     (:constructor make-*-layer
      (&key x y
       ;; &AUX 経由で継承先のスロット値を指定。
       ;; &KEYと異なりAPIとしては公開されないのでエンドユーザーは初期値を指定できない。
       &aux
       (forward (lambda (this x y)
                  (setf (*-layer-x this) x
                        (*-layer-y this) y)
                  (numcl:* x y)))
       (backward (lambda (this x)
                   (values (numcl:* x (*-layer-y this))
                           (numcl:* x (*-layer-x this))))))))
  x
  y)

* (let* ((apple 100)
         (apple-num 2)
         (tax 1.1)
         (*-apple-layer (make-*-layer))
         (*-tax-layer (make-*-layer))
         ;; Forward.
         (apple-price (forward *-apple-layer apple apple-num))
         (price (print (forward *-tax-layer apple-price tax)))
         ;; Backward.
         (dprice 1))
    (multiple-value-bind (dapple-price dtax) (backward *-tax-layer dprice)
      (multiple-value-bind (dapple dapple-num) (backward *-apple-layer dapple-price)
        (format t "~S ~S ~S" dapple dapple-num dtax))))
220.0 2.2 110.0 200
NIL
```

### 5.4.2 Implementation of add layer.

```lisp
(defstruct
    (+-layer (:include layer)
     (:constructor make-+-layer
      (&aux
       (forward
         (lambda (this x y) (declare (ignore this)) (numcl:+ x y)))
       (backward
         (lambda (this x) (declare (ignore this)) (values x x)))))))

* (let* ((apple 100)
         (apple-num 2)
         (orange 150)
         (orange-num 3)
         (tax 1.1)
         ;; Layers
         (*-apple-layer (make-*-layer))
         (*-orange-layer (make-*-layer))
         (apple+orange (make-+-layer))
         (*-tax-layer (make-*-layer))
         ;; Forward.
         (apple-price (forward *-apple-layer apple apple-num))
         (orange-price (forward *-orange-layer orange orange-num))
         (all-price (forward apple+orange apple-price orange-price))
         (price (forward *-tax-layer all-price tax))
         ;; Backward.
         (dprice 1))
    (multiple-value-bind (dall-price dtax) (backward *-tax-layer dprice)
      (multiple-value-bind (dapple-price dorange-price) (backward apple+orange dall-price)
        (multiple-value-bind (dorange dorange-num) (backward *-orange-layer dorange-price)
          (multiple-value-bind (dapple dapple-num) (backward *-apple-layer dapple-price)
            (format t "~%~S~%~S ~S ~S ~S ~S"
                    price
                    dapple-num dapple
                    dorange dorange-num
                    dtax))))))
715.0
110.0 2.2 3.3000002 165.0 650
```

## 5.5 Implementation of activation layer.

### 5.5.1 ReLU layer.

```lisp
(defstruct
    (relu-layer (:include layer)
     (:constructor make-relu-layer
      (&key mask
       &aux
        (forward
          (lambda (this x)
            (let ((mask (numcl:<= x 0)))
              (setf (relu-layer-mask this) mask)
              (numcl:* mask x))))
        (backward
          (lambda (this x) (numcl:* (relu-layer-mask this) x))))))
  mask)

;; Print/Read 同一性を保護。
(defmethod print-object ((o relu-layer) stream)
  (if (not *print-readably*)
    (call-next-method)
    (format stream "#.~S"
            `(make-relu-layer :mask ,(relu-layer-mask o)))))
```

### 5.5.2 Sigmoid layer.

```lisp
(defstruct
    (sigmoid-layer (:include layer)
     (:constructor make-sigmoid-layer
      (&key out
       &aux
        (forward
          (lambda (this x)
            (setf (sigmoid-layer-out this)
                  (numcl:/ 1 (numcl:+ 1 (numcl:exp (numcl:- x)))))))
        (backward
          (lambda (this x)
            (numcl:* x
                     (numcl:- 1.0 (sigmoid-layer-out this))
                     (sigmoid-layer-out this)))))))
  out)

(defmethod print-object ((o sigmoid-layer) stream)
  (if (not *print-readably*)
    (call-next-method)
    (format stream "#.~S"
            `(make-sigmoid-layer :out ,(sigmoid-layer-out o)))))
```
## 5.6 Implementation of Affine/Softmax layer.
### 5.6.1 Affine layer.

### 5.6.2 Batch version.

```lisp
* (let ((x.w (numcl:asarray '((0 0 0) (10 10 10))))
        (b (numcl:asarray '(1 2 3))))
    (numcl:+ x.w b))
#2A((1 2 3) (11 12 13))

* (let ((dy (numcl:asarray '((1 2 3) (4 5 6)))))
    (numcl:sum dy :axes 0))
#(5 7 9)
```
原著ではネットワークオブジェクトがParameterをハッシュで持っていましたが、ここでは`LAYER`そのものに持たせることとします。
変数のスコープを小さく保つのが目的です。
（巨大なグローバル変数を持ちたくない）

```lisp
(defstruct
  (affine-layer (:include layer)
   (:constructor make-affine-layer
    (&key weight bias input dw db
     &aux
     (forward
       (lambda (this input)
         (setf (affine-layer-input this) input)
         (numcl:+ (dot input (affine-layer-weight this))
                  (affine-layer-bias this))))
     (backward
       (lambda (this dout)
         (let ((dx (dot dout (numcl:transpose (affine-layer-weight this)))))
           (setf (affine-layer-dw this)
                   (dot (numcl:transpose (affine-layer-input this))
                        dout)
                 (affine-layer-db this)
                   (numcl:sum dout :axes 0))
           dx))))))
  weight
  bias
  input
  dw
  db)

(defmethod print-object ((o affine-layer) stream)
  (if (not *print-readably*)
    (call-next-method)
    (format stream "#.~S"
            `(make-affine-layer :weight ,(affine-layer-weight o)
                                :bias ,(affine-layer-bias o)
                                :input ,(affine-layer-input o)
                                :dw ,(affine-layer-dw o)
                                :db ,(affine-layer-db o)))))
```

### 5.6.3 Softmax with loss layer.

```lisp
(defstruct
    (softmax-with-loss-layer (:include layer)
     (:constructor make-softmax-with-loss-layer
      (&key loss y teach
       &aux
        (forward
          (lambda (this x teach)
            (setf (softmax-with-loss-layer-teach this) teach
                  (softmax-with-loss-layer-y this) (softmax x)
                  (softmax-with-loss-layer-loss this)
                    (cross-entropy-error (softmax-with-loss-layer-y this)
                                         teach))))
        (backward
          (lambda (this &optional (x 1))
            (numcl:/ (numcl:- (softmax-with-loss-layer-y this)
                              (softmax-with-loss-layer-teach this))
                     (nth 0 (numcl:shape (softmax-with-loss-layer-teach this)))))))))
  loss
  y
  teach)

(defmethod print-object ((o softmax-with-loss-layer) stream)
  (if (not *print-readably*)
    (call-next-method)
    (format stream "#.~S"
            `(make-softmax-with-loss-layer :loss ,(softmax-with-loss-loss o)
                                           :y ,(softmax-with-loss-y o)
                                           :teach ,(softmax-with-loss-teach o)))))
```

## 5.7 Implementation of back propagation.
### 5.7.1 Overall of learnings of neural network.
### 5.7.2 Implementation of neural network which supports back propagation.

PREDICTは出力LAYERへの入力値を返す関数です。

Haskellなら`swap`を使いたいところ。

`BUTLAST`で毎回中間リストがアロケートされるのも非効率ですが、ここでは保守性を取ることとします。

```lisp
(defun predict (network input)
  (reduce (lambda (input layer) (forward layer input))
          (butlast network)
          :initial-value input))
```
`PROPAGATE`は`PREDICT`の反対のようなものです。
本関数の目的は引数`NETWORK`が保持する各`LAYER`オブジェクトのスロット値を破壊変更することです。
なので返り値は`NETWORK`とします。

```lisp
(defun propagate (network dout)
  (reduce #'backward network :initial-value dout :from-end t)
  network)
```

`LOSS`も同様に`LAYER`の破壊変更が目的なので`NETWORK`を返します。

```lisp
(defun loss (network input label)
  (forward (car (last network)) (predict network input) label)
  network)
```

```lisp
(defun accuracy (network input label)
  (when (/= 1 (array-rank input))
    (setf label (numcl:asarray (argmax label :axes 1))))
  (numcl:/ (numcl:sum (numcl:= (numcl:asarray (argmax (predict network input)
                                                      :axes 1))
                               label))
           (float (nth 0 (numcl:shape input)))))

(defun compute-numerical-gradient (network input label)
  (flet ((loss-w (weight)
           (declare (ignore weight))
           (loss network input label)))
    (loop :for layer :in network
          :collect (numerical-gradient #'loss-w
                                       (affine-layer-weight layer))
          :collect (numerical-gradient #'loss-w
                                       (affine-layer-bias layer)))))

(defun gradient (network input label)
  (loop :for layer :in (propagate (loss network input label) 1)
        :when (affine-layer-p layer)
        :collect (affine-layer-dw layer)
        :and :collect (affine-layer-db layer)))
```

### 5.7.3 Gradient checks of back propagation.

```lisp
(defun make-net (specs)
  (flet ((constructor (name)
           (uiop:find-symbol* (format nil "MAKE-~A-LAYER" name) *package*)))
    (loop :for (main shape activator) :in specs
          :for size := nil
          ;; Trivial syntax-check.
          :do (check-type main symbol)
              (check-type shape (cons integer (cons integer null)))
              (check-type activator symbol)
              (when size
                (assert (= size (car shape))))
              (setf size (cadr shape))
          ;; The body.
          :collect (funcall (constructor main)
                            :weight (numcl:normal 0.0d0 1.0d0 shape
                                                  'single-float)
                            :bias (numcl:zeros (cadr shape)))
          :collect (funcall (constructor activator)))))
```

```lisp
* (defparameter *mnist* (cl-mnist:load-mnist :flatten t
                                             :normalize t
                                             :one-hot-label t
                                             :slurp t))
*MNIST*

* (defparameter *net* (make-net '((affine (784 50) relu)
                                  (affine (50 10) softmax-with-loss))))
*NET*

* (gradient *net*
            (numcl:asarray (loop :repeat 2 :collect (funcall (getf *mnist* :train-images))))
            (numcl:asarray (loop :repeat 2 :collect (funcall (getf *mnist* :train-labels)))))

```
### 5.7.4 Learnings with back propagation.

```lisp
(defparameter *learning-rate* 0.01)

(defun learn (network input label)
  (loop :for layer :in (propagate (loss network input label) 1)
        :when (affine-layer-p layer)
        :do (setf (affine-layer-weight layer)
                    (numcl:- (affine-layer-weight layer)
                             (numcl:* *learning-rate* (affine-layer-dw layer)))
                  (affine-layer-bias layer)
                    (numcl:- (affine-layer-bias layer)
                             (numcl:* *learning-rate* (affine-layer-db layer)))))
  network)
```

## 5.8 Summary
