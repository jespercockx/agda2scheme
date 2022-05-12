(import (only (chezscheme) record-case))

(define (monus x y) (max 0 (- x y)))

(define (seq x y) y)

(define (it b) b)

(define (mapMaybe e f) (record-case f ((just) (g) (list 'just (e g)))
((nothing) () f)))

(define (testMapMaybe) (mapMaybe (lambda (h) (+ 1 h)) (list 'just 5)))

(define (fst i) (let ((j (list-ref i 0)) (k (list-ref i 1))) j))

(define (snd l) (let ((m (list-ref l 0)) (n (list-ref l 1))) n))

(define (mapInl r s) (record-case s ((inl) (t) (list 'inl (r t)))
((inr) (u) s)))

(define (mapInr y z) (record-case z ((inl) (a1) z)
((inr) (b1) (list 'inr (y b1)))))

(define (subst h1) h1)

(define (≤-refl i1) (if (= 0 i1) (list '≤-zero)
(let ((j1 (- i1 1))) (list '≤-suc (≤-refl j1)))))

(define 
  (≤-trans n1 o1)
  (record-case 
    n1
    ((≤-zero) () (list '≤-zero))
    ((≤-suc) (s1) (let ((v1 (list-ref o1 1))) (list '≤-suc (≤-trans s1 v1))))))

(define (≤-dec w1) (if (= 0 w1) (list '≤-zero)
(let ((z1 (- w1 1))) (list '≤-suc (≤-dec z1)))))

(define (z≤-refl a2) (let ((c2 (list-ref a2 0)) (d2 (list-ref a2 1))) c2))

(define (z≤-trans f2) (let ((h2 (list-ref f2 0)) (i2 (list-ref f2 1))) i2))

(define 
  (Ord-Nat)
  (list 
    (lambda (k2) (≤-refl k2))
    (lambda (l2) (lambda (m2) (lambda (n2) (lambda (o2) (lambda (p2) (≤-trans o2 p2))))))))

(define 
  (Ord-⊤)
  (list (lambda (q2) (list)) (lambda (r2) (lambda (s2) (lambda (t2) (lambda (u2) (lambda (v2) (list))))))))

(define (Ord-A w2) (let ((x2 (list-ref w2 0)) (y2 (list-ref w2 1))) x2))

(define (tri z2) (let ((a3 (list-ref z2 0)) (b3 (list-ref z2 1))) b3))

(define 
  (triNat c3 d3)
  (if 
    (= 0 c3)
    (if (= 0 d3) (list 'equal)
(list 'less (≤-dec 0)))
    (let 
      ((e3 (- c3 1)))
      (if 
        (= 0 d3)
        (list 'greater (≤-dec 0))
        (let 
          ((f3 (- d3 1)))
          (let 
            ((g3 (triNat e3 f3)))
            (record-case 
              g3
              ((less) (j3) (list 'less (list '≤-suc j3)))
              ((equal) () (list 'equal))
              ((greater) (p3) (list 'greater (list '≤-suc p3))))))))))

(define (testTriNat) (triNat 3 5))

(define (TDO-Nat) (list (Ord-Nat) (lambda (q3) (lambda (r3) (triNat q3 r3)))))

(define 
  (\x5B;\x5D;∞-refl t3 u3)
  (record-case 
    u3
    ((-∞) () (list '-∞-≤))
    ((\x5B;_\x5D;) (v3) (list '\x5B;\x5D;-≤ ((z≤-refl t3) v3)))
    ((+∞) () (list '+∞-≤))))

(define 
  (\x5B;\x5D;∞-trans x3 y3
z3
a4
b4
c4)
  (let 
    ((d4 (seq c4 (list '+∞-≤))))
    (record-case 
      b4
      ((-∞-≤) () (list '-∞-≤))
      ((\x5B;\x5D;-≤) 
        (h4)
        (let 
          ((i4 (list-ref y3 1)))
          (let 
            ((j4 (list-ref z3 1)))
            (record-case 
              c4
              ((\x5B;\x5D;-≤) 
                (m4)
                (let 
                  ((n4 (list-ref a4 1)))
                  (list '\x5B;\x5D;-≤ ((((((z≤-trans x3) i4) j4) n4) h4) m4))))
              ((+∞-≤) () (list '+∞-≤))))))
      (else d4))))

(define 
  (Ord-\x5B;\x5D;∞ q4)
  (list 
    (lambda (s4) (\x5B;\x5D;∞-refl q4 s4))
    (lambda 
      (t4)
      (lambda 
        (u4)
        (lambda (v4) (lambda (w4) (lambda (x4) (\x5B;\x5D;∞-trans q4 t4
u4
v4
w4
x4))))))))

(define (-∞-≤-I) (list '-∞-≤))

(define (+∞-≤-I) (list '+∞-≤))

(define (\x5B;\x5D;-≤-I f5) (list '\x5B;\x5D;-≤ f5))

(define (testBST) (list 'node 3
(list 'leaf (list '-∞-≤))
(list 'leaf (list '+∞-≤))))

(define 
  (lookup h5 k5
l5)
  (record-case 
    l5
    ((leaf) (m5) (list 'nothing))
    ((node) 
      (n5 o5 p5)
      (let 
        ((q5 (((tri h5) k5) n5)))
        (record-case 
          q5
          ((less) (t5) (mapMaybe (lambda (u5) (list 'left u5)) (lookup h5 k5
o5)))
          ((equal) () (list 'just (list 'here)))
          ((greater) (a6) (mapMaybe (lambda (b6) (list 'right b6)) (lookup h5 k5
p5))))))))

(define 
  (insert d6 g6
h6
i6
j6)
  (record-case 
    h6
    ((leaf) (k6) (list 'node g6
(list 'leaf i6)
(list 'leaf j6)))
    ((node) 
      (l6 m6 n6)
      (let 
        ((o6 (((tri d6) g6) l6)))
        (record-case 
          o6
          ((less) (r6) (list 'node l6
(insert d6 g6
m6
i6
(list '\x5B;\x5D;-≤ r6))
n6))
          ((equal) () h6)
          ((greater) (x6) (list 'node l6
m6
(insert d6 g6
n6
(list '\x5B;\x5D;-≤ x6)
j6))))))))

(define (testLookup) (lookup (TDO-Nat) 3
(testBST)))

(define (testInsert) (insert (TDO-Nat) 4
(testBST)
(list '-∞-≤)
(list '+∞-≤)))

(define 
  (fromList z6 a7)
  (record-case 
    a7
    ((\x5B;\x5D;) () (list 'leaf (list '-∞-≤)))
    ((_∷_) (b7 c7) (insert z6 b7
(fromList z6 c7)
(list '-∞-≤)
(list '+∞-≤)))))

(define 
  (testFromList)
  (fromList (TDO-Nat) (list '_∷_ 1
(list '_∷_ 2
(list '_∷_ 3
(list '\x5B;\x5D;))))))

(define 
  (_++_ e7 f7)
  (record-case e7 ((\x5B;\x5D;) () f7)
((_∷_) (g7 h7) (list '_∷_ g7
(_++_ h7 f7)))))

(define 
  (flatten m7)
  (record-case 
    m7
    ((leaf) (n7) (list '\x5B;\x5D;))
    ((node) (o7 p7 q7) (_++_ (flatten p7) (list '_∷_ o7
(flatten q7))))))

(define (testFlatten) (flatten (testInsert)))

(define (sort s7 t7) (flatten (fromList s7 t7)))

(define 
  (test1)
  (sort 
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