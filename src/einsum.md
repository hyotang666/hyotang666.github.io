# Summary of numcl:einsum.
## Meta notes.
### 対象読者
* numcl、特にeinsumに興味のあるプログラマ。
* einsumのユースケース、あるいはチートシートが欲しいかた。
* numclのauthor。

### What is NOT.
* numclのtutorialではない。
* Common Lispのtutorialではない。
* einsumの解説ではない。
* 線形代数の解説ではない。

## Introduction
### 今北産業

1. 英詩を生成するプログラムを作りたい。
2. 自然言語処理が必要。
3. 自然言語処理の流行りはDeep learning。
4. Deep learningには線形代数が必要。
5. Common Lispの線形代数ライブラリはほとんどが死んでいて、2020年現在生きていて（僕の目から見て）最も筋の良いライブラリは[numcl](https://github.com/numcl/numcl)。
6. Numclは開発途上でまだまだ機能が足りない。具体的には`dot`関数がない。
7. `Dot`関数なしにMatrix * Vectorをどうやるのか分からない。
8. ソースコードに目を通したところ抽象度の高い関数のヘルパとして`einsum`関数が共通して使われているのを発見。
9. `einsum`関数もnumpy由来らしい。
10. `einsum`関数の振る舞いを把握したらMatrix * Vectorも実装できそうとあたりをつけて調査　<- イマココ

筆者自信は線形代数にもnumpyにも、もちろんnumpyのeinsumにも暗い人間である。
ここでは

* [numpyの公式ドキュメント](https://numpy.org/doc/stable/reference/generated/numpy.einsum.html)におけるexample。
* einsumで検索して出てきたありがたいtutorial系ブログのexample。

をnumclで再現する形でnumcl:einsumの振る舞いを学んでいきたい。

## https://numpy.org/doc/stable/reference/generated/numpy.einsum.html
### Note
本節はnumpyコードとnumclコードが交互に示されます。

そのままの移植では結果が異なるものについてはあれこれ試した式を残してあります。

また、本節末尾にはnumclがサポートしていない（と思しき再現方法の分からなかった）numpyコードをまとめて置いてあります。

### Example

```python
>>> a = np.arange(25).reshape(5,5)
```

```lisp
* (defparameter a (numcl:reshape (numcl:arange 25) '(5 5)))
A
```

```python
>>> b = np.arange(5)
```

```lisp
* (defparameter b (numcl:arange 5))
B
```

```python
>>> c = np.arange(6).reshape(2,3)
```

```lisp
* (defparameter c (numcl:reshape (numcl:arange 6) '(2 3)))
C
```

### Trace of a matrix:
互換性のないspecがさっそく登場。

```python
>>> np.einsum('ii', a)
60
```

```lisp
* (numcl:einsum '(ii) a)
#(0 6 12 18 24)

* (numcl:einsum '(ii ->) a)
60
```

### Extract the diagonal (requires explicit form):

```python
>>> np.einsum('ii->i', a)
array([ 0,  6, 12, 18, 24])
```

```lisp
* (numcl:einsum '(ii -> i) a)
#(0 6 12 18 24)
```

```python
>>> np.diag(a)
array([ 0,  6, 12, 18, 24])
```

```lisp
* (numcl:diag a)
#(0 6 12 18 24)
```

### Compute a matrix transpose, or reorder any number of axes:

```python
>>> np.einsum('ji', c)
array([[0, 3],
       [1, 4],
       [2, 5]])
```

```lisp
* (numcl:einsum  '(ji) c)
#2A((0 3) (1 4) (2 5))
```

```python
>>> c.T
array([[0, 3],
       [1, 4],
       [2, 5]])
```

```lisp
* (numcl:transpose c)
#2A((0 3) (1 4) (2 5))
```

### Vector inner products:

```python
>>> np.einsum('i,i', b, b)
30
```

```lisp
* (numcl:einsum '(i i) b b)
#(0 1 4 9 16)

* (numcl:einsum '(i i ->) b b)
30
```

```python
>>> np.inner(b,b)
30
```

```lisp
* (numcl:inner b b)
30
```

### Matrix vector multiplication:

```python
>>> np.einsum('ij,j', a, b)
array([ 30,  80, 130, 180, 230])
```

```lisp
* (numcl:einsum '(ij j) a b) 
#2A((0 1 4 9 16) (0 6 14 24 36) (0 11 24 39 56) (0 16 34 54 76) (0 21 44 69 96))

* (numcl:einsum '(ij j ->) a b) 
650

* (numcl:einsum '(ij j -> i) a b)
#(30 80 130 180 230)

* (numcl:einsum '(ij j -> j) a b) 
#(0 55 120 195 280)
```

```python
>>> np.einsum('i...->...', a)
array([50, 55, 60, 65, 70])
```

```lisp
* (numcl:einsum '(ij -> j) a)
#(50 55 60 65 70)
```

### Broadcasting and scalar multiplication:
Broadcastingは実験的な機能とnumclのドキュメントにはあります。

```python
>>> np.einsum('..., ...', 3, c)
array([[ 0,  3,  6],
       [ 9, 12, 15]])
```

```lisp
* (numcl:einsum '(ij -> (cl:+ @1 (cl:* 3 $1)) -> ij) c)
#2A((0 3 6) (9 12 15))
```

```python
>>> np.multiply(3, c)
array([[ 0,  3,  6],
       [ 9, 12, 15]])
```

```lisp
* (numcl:* 3 c)
#2A((0 3 6) (9 12 15))
```

### Vector outer product:

```python
>>> np.einsum('i,j', np.arange(2)+1, b)
array([[0, 1, 2, 3, 4],
       [0, 2, 4, 6, 8]])
```

```lisp
* (numcl:einsum '(i j) (numcl:+ (numcl:arange 2) 1) b)
#2A((0 1 2 3 4) (0 2 4 6 8))
```

```python
>>> np.outer(np.arange(2)+1, b)
array([[0, 1, 2, 3, 4],
       [0, 2, 4, 6, 8]])
```


```lisp
* (numcl:outer (numcl:+ (numcl:arange 2) 1) b)
#2A((0 1 2 3 4) (0 2 4 6 8))
```

### Tensor contraction:

```python
>>> a = np.arange(60.).reshape(3,4,5)
```

```lisp
* (defparameter a (numcl:reshape (numcl:arange 60) '(3 4 5)))
```

```python
>>> b = np.arange(24.).reshape(4,3,2)
```

```lisp
* (defparameter b (numcl:reshape (numcl:arange 24) '(4 3 2)))
```

```python
>>> np.einsum('ijk,jil->kl', a, b)
array([[ 4400.,  4730.],
       [ 4532.,  4874.],
       [ 4664.,  5018.],
       [ 4796.,  5162.],
       [ 4928.,  5306.]])
```


```lisp
* (numcl:einsum '(ijk jil -> kl) a b)
#2A((4400 4730) (4532 4874) (4664 5018) (4796 5162) (4928 5306))
```

### Unsupported?

```python
np.einsum(a, [0,0])
60
>>> np.trace(a)
60
>>> np.einsum(a, [0,0], [0])
array([ 0,  6, 12, 18, 24])
>>> np.einsum(a, [0,1], b, [1])
array([ 30,  80, 130, 180, 230])
>>> np.dot(a, b)
array([ 30,  80, 130, 180, 230])
>>> np.einsum('...j,j', a, b)
array([ 30,  80, 130, 180, 230])
>>> np.einsum(c, [1,0])
array([[0, 3],
       [1, 4],
       [2, 5]])
>>> np.einsum(3, [Ellipsis], c, [Ellipsis])
array([[ 0,  3,  6],
       [ 9, 12, 15]])
>>> np.einsum(b, [0], b, [0])
30
>>> np.einsum(np.arange(2)+1, [0], b, [1])
array([[0, 1, 2, 3, 4],
       [0, 2, 4, 6, 8]])
>>> np.einsum(a, [0,Ellipsis], [Ellipsis])
array([50, 55, 60, 65, 70])
>>> np.einsum(a, [0,1,2], b, [1,0,3], [2,3])
array([[ 4400.,  4730.],
       [ 4532.,  4874.],
       [ 4664.,  5018.],
       [ 4796.,  5162.],
       [ 4928.,  5306.]])
>>> np.tensordot(a,b, axes=([1,0],[0,1]))
array([[ 4400.,  4730.],
       [ 4532.,  4874.],
       [ 4664.,  5018.],
       [ 4796.,  5162.],
       [ 4928.,  5306.]])
>>> np.einsum('ki,...k->i...', a, b)
array([[10, 28, 46, 64],
       [13, 40, 67, 94]])
>>> np.einsum('k...,jk', a, b)
array([[10, 28, 46, 64],
       [13, 40, 67, 94]])
>>> np.einsum('ii->i', a)[:] = 1
>>> a
array([[ 1.,  0.,  0.],
       [ 0.,  1.,  0.],
       [ 0.,  0.,  1.]])
```

## https://rockt.github.io/2018/04/30/einsum
本節はチートシートとして使えると思われ。

### Note
参照ブログでは`torch.einsum`が使われているのですが、numpyのeinsumとは実装が違ったりするんですかね？（調べてない。）
本節のコードは全てそのままの移植で動いたのでpythonコードは省略してあります。

### Matrix transpose.

```lisp
* (numcl:einsum '(ij -> ji) (numcl:reshape (numcl:arange 6) '(2 3)))
#2A((0 3) (1 4) (2 5))
```

### Sum

```lisp
* (numcl:einsum '(ij ->) (numcl:reshape (numcl:arange 6) '(2 3)))
15
```

### Columns sum

```lisp
* (numcl:einsum '(ij -> j) (numcl:reshape (numcl:arange 6) '(2 3)))
#(3 5 7)
```

### Row sum

```lisp
* (numcl:einsum '(ij -> i) (numcl:reshape (numcl:arange 6) '(2 3)))
#(3 12)
```

### Matrix vector multiplication

```lisp
* (numcl:einsum '(ik k -> i) (numcl:reshape (numcl:arange 6) '(2 3)) (numcl:arange 3))
#(5 14)
```

### Matrix matrix multiplication

```lisp
* (numcl:einsum '(ik kj -> ij) (numcl:reshape (numcl:arange 6) '(2 3)) (numcl:reshape (numcl:arange 15) '(3 5)))
#2A((25 28 31 34 37) (70 82 94 106 118))
```

## Dot product
### Vector

```lisp
* (numcl:einsum '(i i ->) (numcl:arange 3) (numcl:arange 3 6))
14
```

### Matrix

```lisp
* (numcl:einsum '(ij ij ->) (numcl:reshape (numcl:arange 6) '(2 3)) (numcl:reshape (numcl:arange 6 12) '(2 3)))
145
```

### Hadamard product

```lisp
* (numcl:einsum '(ij ij -> ij) (numcl:reshape (numcl:arange 6) '(2 3)) (numcl:reshape (numcl:arange 6 12) '(2 3)))
#2A((0 7 16) (27 40 55))
```

### Outer product

```lisp
* (numcl:einsum '(i j -> ij) (numcl:arange 3) (numcl:arange 3 7))
#2A((0 0 0 0) (3 4 5 6) (6 8 10 12))
```

### Batch matrix multiplication

```lisp
* (numcl:einsum '(ijk ikl -> ijl) (numcl:reshape (numcl:arange 30) '(3 2 5)) (numcl:reshape (numcl:arange 45) '(3 5 3)))
#3A(((90 100 110) (240 275 310))
    ((1290 1350 1410) (1815 1900 1985))
    ((3990 4100 4210) (4890 5025 5160)))
```
