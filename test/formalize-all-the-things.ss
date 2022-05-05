(import (only (chezscheme) record-case))

(define (monus x y) (max 0 (- x y)))

(define (seq x y) y)

(define (it a b) b)

(define (mapMaybe c d
e
f) (record-case f ((just) (g) (list 'just (e g)))
((nothing) () f)))

(define (testMapMaybe) (mapMaybe (list) (list)
(lambda (h) (+ 1 h))
(list 'just 5)))

(define (fst i) (record-case i ((_\x2C;_) (j k) j)))

(define (snd l) (record-case l ((_\x2C;_) (m n) n)))

(define (mapInl o p
q
r
s) (record-case s ((inl) (t) (list 'inl (r t)))
((inr) (u) s)))

(define (mapInr v w
x
y
z) (record-case z ((inl) (a1) z)
((inr) (b1) (list 'inr (y b1)))))

(define (¬_) (list))

(define (sym) (list))

(define (trans) (list))

(define (cong) (list))

(define (subst c1 d1
e1
f1
g1
h1) h1)

(define 
  (≤-refl i1)
  (if (= 0 i1) (list '≤-zero (list))
(let ((k1 (- i1 1))) (list '≤-suc (list)
(list)
(≤-refl k1)))))

(define 
  (≤-trans l1 m1
n1
o1
p1)
  (record-case 
    o1
    ((≤-zero) (q1) (list '≤-zero (list)))
    ((≤-suc) 
      (r1 s1 t1)
      (record-case 
        p1
        ((≤-suc) (u1 v1 w1) (list '≤-suc (list)
(list)
(≤-trans (list) (list)
(list)
t1
w1)))))))

(define (≤-antisym) (list))

(define (So) (list))

(define 
  (≤-dec x1 y1
z1)
  (if 
    (= 0 x1)
    (list '≤-zero (list))
    (let ((b2 (- x1 1))) (list '≤-suc (list)
(list)
(≤-dec b2 (list)
(list))))))

(define (_≤_) (list))

(define (z≤-refl c2) (record-case c2 ((Ord.constructor) (d2 e2 f2
g2) e2)))

(define (z≤-trans h2) (record-case h2 ((Ord.constructor) (i2 j2 k2
l2) k2)))

(define (z≤-antisym) (list))

(define (_≥_) (list))

(define 
  (Ord-Nat)
  (list 
    'Ord.constructor
    (list)
    (lambda (m2) (≤-refl m2))
    (lambda 
      (n2)
      (lambda (o2) (lambda (p2) (lambda (q2) (lambda (r2) (≤-trans (list) (list)
(list)
q2
r2))))))
    (list)))

(define 
  (Ord-⊤)
  (list 
    'Ord.constructor
    (list)
    (lambda (s2) (list))
    (lambda (t2) (lambda (u2) (lambda (v2) (lambda (w2) (lambda (x2) (list))))))
    (list)))

(define (Ord-A y2) (record-case y2 ((TDO.constructor) (z2 a3) z2)))

(define (tri b3) (record-case b3 ((TDO.constructor) (c3 d3) d3)))

(define 
  (triNat e3 f3)
  (if 
    (= 0 e3)
    (if (= 0 f3) (list 'equal (list)
(list)
(list))
(list 'less (list)
(list)
(≤-dec 0 (list)
(list))))
    (let 
      ((d4 (- e3 1)))
      (if 
        (= 0 f3)
        (list 'greater (list)
(list)
(≤-dec 0 (list)
(list)))
        (let 
          ((p4 (- f3 1)))
          (let 
            ((q4 (triNat d4 p4)))
            (record-case 
              q4
              ((less) (r4 s4 t4) (list 'less (list)
(list)
(list '≤-suc (list)
(list)
t4)))
              ((equal) (u4 v4 w4) (list 'equal (list)
(list)
(list)))
              ((greater) (x4 y4 z4) (list 'greater (list)
(list)
(list '≤-suc (list)
(list)
z4))))))))))

(define (testTriNat) (triNat 3 5))

(define (TDO-Nat) (list 'TDO.constructor (Ord-Nat)
(lambda (a5) (lambda (b5) (triNat a5 b5)))))

(define 
  (\x5B;\x5D;∞-refl c5 d5
e5)
  (record-case 
    e5
    ((-∞) () (list '-∞-≤ (list)))
    ((\x5B;_\x5D;) (f5) (list '\x5B;\x5D;-≤ (list)
(list)
((z≤-refl d5) f5)))
    ((+∞) () (list '+∞-≤ (list)))))

(define 
  (\x5B;\x5D;∞-trans g5 h5
i5
j5
k5
l5
m5)
  (let 
    ((n5 (seq m5 (list '+∞-≤ (list)))))
    (record-case 
      l5
      ((-∞-≤) (o5) (list '-∞-≤ (list)))
      ((\x5B;\x5D;-≤) 
        (p5 q5 r5)
        (record-case 
          i5
          ((\x5B;_\x5D;) 
            (s5)
            (record-case 
              j5
              ((\x5B;_\x5D;) 
                (t5)
                (record-case 
                  m5
                  ((\x5B;\x5D;-≤) 
                    (u5 v5 w5)
                    (record-case 
                      k5
                      ((\x5B;_\x5D;) 
                        (x5)
                        (list '\x5B;\x5D;-≤ (list)
(list)
((((((z≤-trans h5) s5) t5) x5) r5) w5)))
                      (else n5)))
                  ((+∞-≤) (y5) (list '+∞-≤ (list)))))
              (else n5)))
          (else n5)))
      (else n5))))

(define (\x5B;\x5D;∞-antisym) (list))

(define 
  (Ord-\x5B;\x5D;∞ z5 a6
b6)
  (list 
    'Ord.constructor
    (list)
    (lambda (c6) (\x5B;\x5D;∞-refl (list) a6
c6))
    (lambda 
      (d6)
      (lambda 
        (e6)
        (lambda (f6) (lambda (g6) (lambda (h6) (\x5B;\x5D;∞-trans (list) a6
d6
e6
f6
g6
h6))))))
    (list)))

(define (-∞-≤-I i6 j6) (list '-∞-≤ (list)))

(define (+∞-≤-I k6 l6) (list '+∞-≤ (list)))

(define (\x5B;\x5D;-≤-I m6 n6
o6
p6) (list '\x5B;\x5D;-≤ (list)
(list)
p6))

(define 
  (testBST)
  (list 'node 3
(list 'leaf (list '-∞-≤ (list)))
(list 'leaf (list '+∞-≤ (list)))))

(define 
  (lookup q6 r6
s6
t6
u6
v6)
  (record-case 
    v6
    ((leaf) (w6) (list 'nothing))
    ((node) 
      (x6 y6 z6)
      (let 
        ((a7 (((tri r6) u6) x6)))
        (record-case 
          a7
          ((less) 
            (b7 c7 d7)
            (mapMaybe 
              (list)
              (list)
              (lambda (e7) (list 'left (list)
(list)
(list)
e7))
              (lookup (list) r6
(list)
(list)
u6
y6)))
          ((equal) (f7 g7 h7) (list 'just (list 'here (list)
(list)
(list)
(list))))
          ((greater) 
            (i7 j7 k7)
            (mapMaybe 
              (list)
              (list)
              (lambda (l7) (list 'right (list)
(list)
(list)
l7))
              (lookup (list) r6
(list)
(list)
u6
z6))))))))

(define 
  (insert m7 n7
o7
p7
q7
r7
s7
t7)
  (record-case 
    r7
    ((leaf) (u7) (list 'node q7
(list 'leaf s7)
(list 'leaf t7)))
    ((node) 
      (v7 w7 x7)
      (let 
        ((y7 (((tri n7) q7) v7)))
        (record-case 
          y7
          ((less) 
            (z7 a8 b8)
            (list 
              'node
              v7
              (insert (list) n7
(list)
(list)
q7
w7
s7
(list '\x5B;\x5D;-≤ (list)
(list)
b8))
              x7))
          ((equal) (c8 d8 e8) r7)
          ((greater) 
            (f8 g8 h8)
            (list 
              'node
              v7
              w7
              (insert (list) n7
(list)
(list)
q7
x7
(list '\x5B;\x5D;-≤ (list)
(list)
h8)
t7))))))))

(define (testLookup) (lookup (list) (TDO-Nat)
(list)
(list)
3
(testBST)))

(define 
  (testInsert)
  (insert (list) (TDO-Nat)
(list)
(list)
4
(testBST)
(list '-∞-≤ (list))
(list '+∞-≤ (list))))

(define 
  (fromList i8 j8
k8)
  (record-case 
    k8
    ((\x5B;\x5D;) () (list 'leaf (list '-∞-≤ (list))))
    ((_∷_) 
      (l8 m8)
      (insert 
        (list)
        j8
        (list)
        (list)
        l8
        (fromList (list) j8
m8)
        (list '-∞-≤ (list))
        (list '+∞-≤ (list))))))

(define 
  (testFromList)
  (fromList (list) (TDO-Nat)
(list '_∷_ 1
(list '_∷_ 2
(list '_∷_ 3
(list '\x5B;\x5D;))))))

(define 
  (_++_ n8 o8
p8)
  (record-case o8 ((\x5B;\x5D;) () p8)
((_∷_) (q8 r8) (list '_∷_ q8
(_++_ (list) r8
p8)))))

(define 
  (flatten s8 t8
u8
v8
w8)
  (record-case 
    w8
    ((leaf) (x8) (list '\x5B;\x5D;))
    ((node) 
      (y8 z8 a9)
      (_++_ 
        (list)
        (flatten (list) (list)
(list)
(list)
z8)
        (list '_∷_ y8
(flatten (list) (list)
(list)
(list)
a9))))))

(define (testFlatten) (flatten (list) (list)
(list)
(list)
(testInsert)))

(define (sort b9 c9
d9) (flatten (list) (list)
(list)
(list)
(fromList (list) c9
d9)))

(define 
  (test1)
  (sort 
    (list)
    (TDO-Nat)
    (list 
      '_∷_
      5
      (list 
        '_∷_
        3
        (list 
          '_∷_
          9
          (list 
            '_∷_
            1
            (list 
              '_∷_
              2
              (list 
                '_∷_
                10
                (list '_∷_ 7
(list '_∷_ 4
(list '_∷_ 8
(list '_∷_ 6
(list '\x5B;\x5D;)))))))))))))