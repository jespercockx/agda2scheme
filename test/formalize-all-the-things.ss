(import (only (chezscheme) record-case))

(define (monus) (lambda (x) (lambda (y) (max 0 (- x y)))))

(define (seq) (lambda (x) (lambda (y) y)))

(define (\x5B;\x5D;) (list '\x5B;\x5D;))

(define (_∷_) (lambda (a) (lambda (b) (list '_∷_ a
b))))

(define (it) (lambda (a) (lambda (b) b)))

(define (just) (lambda (a) (list 'just a)))

(define (nothing) (list 'nothing))

(define 
  (mapMaybe)
  (lambda (a) (lambda (b) (lambda (c) (lambda (d) (record-case d ((just) (e) ((just) (c e)))
((nothing) () d)))))))

(define (testMapMaybe) (((((mapMaybe) (list)) (list)) (lambda (a) (+ 1 a))) ((just) 5)))

(define (fst) (lambda (a) (record-case a ((_\x2C;_) (b c) b))))

(define (snd) (lambda (a) (record-case a ((_\x2C;_) (b c) c))))

(define (_\x2C;_) (lambda (a) (lambda (b) (list '_\x2C;_ a
b))))

(define (inl) (lambda (a) (list 'inl a)))

(define (inr) (lambda (a) (list 'inr a)))

(define 
  (mapInl)
  (lambda 
    (a)
    (lambda (b) (lambda (c) (lambda (d) (lambda (e) (record-case e ((inl) (f) ((inl) (d f)))
((inr) (g) e))))))))

(define 
  (mapInr)
  (lambda 
    (a)
    (lambda (b) (lambda (c) (lambda (d) (lambda (e) (record-case e ((inl) (f) e)
((inr) (g) ((inr) (d g))))))))))

(define (tt) (list 'tt))

(define (¬_) (list))

(define (sym) (list))

(define (trans) (list))

(define (cong) (list))

(define (subst) (lambda (a) (lambda (b) (lambda (c) (lambda (d) (lambda (e) (lambda (f) f)))))))

(define (≤-zero) (lambda (a) (list '≤-zero a)))

(define (≤-suc) (lambda (a) (lambda (b) (lambda (c) (list '≤-suc a
b
c)))))

(define (≤-refl) (lambda (a) (if (= 0 a) ((≤-zero) (list))
(let ((c (- a 1))) ((((≤-suc) (list)) (list)) ((≤-refl) c))))))

(define 
  (≤-trans)
  (lambda 
    (a)
    (lambda 
      (b)
      (lambda 
        (c)
        (lambda 
          (d)
          (lambda 
            (e)
            (record-case 
              d
              ((≤-zero) (f) ((≤-zero) (list)))
              ((≤-suc) 
                (g h i)
                (record-case 
                  e
                  ((≤-suc) (j k l) ((((≤-suc) (list)) (list)) ((((((≤-trans) (list)) (list)) (list)) i) l))))))))))))

(define (≤-antisym) (list))

(define (So) (list))

(define 
  (≤-dec)
  (lambda 
    (a)
    (lambda 
      (b)
      (lambda 
        (c)
        (if (= 0 a) ((≤-zero) (list))
(let ((e (- a 1))) ((((≤-suc) (list)) (list)) ((((≤-dec) e) (list)) (list)))))))))

(define (_≤_) (list))

(define (≤-refl) (lambda (a) (record-case a ((Ord.constructor) (b c d
e) c))))

(define (≤-trans) (lambda (a) (record-case a ((Ord.constructor) (b c d
e) d))))

(define (≤-antisym) (list))

(define (_≥_) (list))

(define 
  (Ord-Nat)
  (((((Ord.constructor) (list)) (lambda (a) ((≤-refl) a))) 
     (lambda (b) (lambda (c) (lambda (d) ((((≤-trans) (list)) (list)) (list)))))) 
    (list)))

(define 
  (Ord-⊤)
  (((((Ord.constructor) (list)) (lambda (a) (list))) 
     (lambda (b) (lambda (c) (lambda (d) (lambda (e) (lambda (f) (list))))))) 
    (list)))

(define (less) (lambda (a) (lambda (b) (lambda (c) (list 'less a
b
c)))))

(define (equal) (lambda (a) (lambda (b) (lambda (c) (list 'equal a
b
c)))))

(define (greater) (lambda (a) (lambda (b) (lambda (c) (list 'greater a
b
c)))))

(define (Ord-A) (lambda (a) (record-case a ((TDO.constructor) (b c) b))))

(define (tri) (lambda (a) (record-case a ((TDO.constructor) (b c) c))))

(define 
  (triNat)
  (lambda 
    (a)
    (lambda 
      (b)
      (if 
        (= 0 a)
        (if (= 0 b) ((((equal) (list)) (list)) (list))
((((less) (list)) (list)) ((((≤-dec) 0) (list)) (list))))
        (let 
          ((z (- a 1)))
          (if 
            (= 0 b)
            ((((greater) (list)) (list)) ((((≤-dec) 0) (list)) (list)))
            (let 
              ((l1 (- b 1)))
              (let 
                ((m1 (((triNat) z) l1)))
                (record-case 
                  m1
                  ((less) (n1 o1 p1) ((((less) (list)) (list)) ((((≤-suc) (list)) (list)) p1)))
                  ((equal) (q1 r1 s1) ((((equal) (list)) (list)) (list)))
                  ((greater) (t1 u1 v1) ((((greater) (list)) (list)) ((((≤-suc) (list)) (list)) v1))))))))))))

(define (testTriNat) (((triNat) 3) 5))

(define (TDO-Nat) (((TDO.constructor) (Ord-Nat)) (triNat)))

(define (-∞) (list '-∞))

(define (\x5B;_\x5D;) (lambda (a) (list '\x5B;_\x5D; a)))

(define (+∞) (list '+∞))

(define (-∞-≤) (lambda (a) (list '-∞-≤ a)))

(define (\x5B;\x5D;-≤) (lambda (a) (lambda (b) (lambda (c) (list '\x5B;\x5D;-≤ a
b
c)))))

(define (+∞-≤) (lambda (a) (list '+∞-≤ a)))

(define 
  (\x5B;\x5D;∞-refl)
  (lambda 
    (a)
    (lambda 
      (b)
      (lambda 
        (c)
        (record-case 
          c
          ((-∞) () ((-∞-≤) (list)))
          ((\x5B;_\x5D;) (d) ((((\x5B;\x5D;-≤) (list)) (list)) (((≤-refl) b) d)))
          ((+∞) () ((+∞-≤) (list))))))))

(define 
  (\x5B;\x5D;∞-trans)
  (lambda 
    (a)
    (lambda 
      (b)
      (lambda 
        (c)
        (lambda 
          (d)
          (lambda 
            (e)
            (lambda 
              (f)
              (lambda 
                (g)
                (let 
                  ((h (seq g ((+∞-≤) (list)))))
                  (record-case 
                    f
                    ((-∞-≤) (i) ((-∞-≤) (list)))
                    ((\x5B;\x5D;-≤) 
                      (j k l)
                      (record-case 
                        c
                        ((\x5B;_\x5D;) 
                          (m)
                          (record-case 
                            d
                            ((\x5B;_\x5D;) 
                              (n)
                              (record-case 
                                g
                                ((\x5B;\x5D;-≤) 
                                  (o p q)
                                  (record-case 
                                    e
                                    ((\x5B;_\x5D;) 
                                      (r)
                                      ((((\x5B;\x5D;-≤) (list)) (list)) (((((((≤-trans) b) m) n) r) l) q)))
                                    (else h)))
                                ((+∞-≤) (s) ((+∞-≤) (list)))))
                            (else h)))
                        (else h)))
                    (else h)))))))))))

(define (\x5B;\x5D;∞-antisym) (list))

(define 
  (Ord-\x5B;\x5D;∞)
  (lambda 
    (a)
    (lambda 
      (b)
      (lambda 
        (c)
        (((((Ord.constructor) (list)) (lambda (d) ((((\x5B;\x5D;∞-refl) (list)) b) d))) 
           (lambda (e) (lambda (f) (lambda (g) ((((((\x5B;\x5D;∞-trans) (list)) b) e) f) g))))) 
          (list))))))

(define (-∞-≤-I) (lambda (a) (lambda (b) ((-∞-≤) (list)))))

(define (+∞-≤-I) (lambda (a) (lambda (b) ((+∞-≤) (list)))))

(define 
  (\x5B;\x5D;-≤-I)
  (lambda (a) (lambda (b) (lambda (c) (lambda (d) ((((\x5B;\x5D;-≤) (list)) (list)) d))))))

(define (leaf) (lambda (a) (list 'leaf a)))

(define (node) (lambda (a) (lambda (b) (lambda (c) (list 'node a
b
c)))))

(define (testBST) ((((node) 3) ((leaf) ((-∞-≤) (list)))) ((leaf) ((+∞-≤) (list)))))

(define (here) (lambda (a) (lambda (b) (lambda (c) (lambda (d) (list 'here a
b
c
d))))))

(define (left) (lambda (a) (lambda (b) (lambda (c) (lambda (d) (list 'left a
b
c
d))))))

(define (right) (lambda (a) (lambda (b) (lambda (c) (lambda (d) (list 'right a
b
c
d))))))

(define 
  (lookup)
  (lambda 
    (a)
    (lambda 
      (b)
      (lambda 
        (c)
        (lambda 
          (d)
          (lambda 
            (e)
            (lambda 
              (f)
              (record-case 
                f
                ((leaf) (g) (nothing))
                ((node) 
                  (h i j)
                  (let 
                    ((k ((((tri) b) e) h)))
                    (record-case 
                      k
                      ((less) 
                        (l m n)
                        (((((mapMaybe) (list)) (list)) ((((left) (list)) (list)) (list))) 
                          (((((((lookup) (list)) b) (list)) (list)) e) i)))
                      ((equal) (o p q) ((just) (((((here) (list)) (list)) (list)) (list))))
                      ((greater) 
                        (r s t)
                        (((((mapMaybe) (list)) (list)) ((((right) (list)) (list)) (list))) 
                          (((((((lookup) (list)) b) (list)) (list)) e) j))))))))))))))

(define 
  (insert)
  (lambda 
    (a)
    (lambda 
      (b)
      (lambda 
        (c)
        (lambda 
          (d)
          (lambda 
            (e)
            (lambda 
              (f)
              (lambda 
                (g)
                (lambda 
                  (h)
                  (record-case 
                    f
                    ((leaf) (i) ((((node) e) ((leaf) g)) ((leaf) h)))
                    ((node) 
                      (j k l)
                      (let 
                        ((m ((((tri) b) e) j)))
                        (record-case 
                          m
                          ((less) 
                            (n o p)
                            ((((node) j) 
                               (((((((((insert) (list)) b) (list)) (list)) e) k) g) 
                                 ((((\x5B;\x5D;-≤) (list)) (list)) p))) 
                              l))
                          ((equal) (q r s) f)
                          ((greater) 
                            (t u v)
                            ((((node) j) k) 
                              (((((((((insert) (list)) b) (list)) (list)) e) l) 
                                 ((((\x5B;\x5D;-≤) (list)) (list)) v)) 
                                h))))))))))))))))

(define (testLookup) (((((((lookup) (list)) (TDO-Nat)) (list)) (list)) 3) (testBST)))

(define (testInsert) (((((((((insert) (list)) (TDO-Nat)) (list)) (list)) 4) (testBST)) ((-∞-≤) (list))) ((+∞-≤) (list))))

(define 
  (fromList)
  (lambda 
    (a)
    (lambda 
      (b)
      (lambda 
        (c)
        (record-case 
          c
          ((\x5B;\x5D;) () ((leaf) ((-∞-≤) (list))))
          ((_∷_) 
            (d e)
            (((((((((insert) (list)) b) (list)) (list)) d) ((((fromList) (list)) b) e)) ((-∞-≤) (list))) ((+∞-≤) (list)))))))))

(define (testFromList) ((((fromList) (list)) (TDO-Nat)) (((_∷_) 1) (((_∷_) 2) (((_∷_) 3) (\x5B;\x5D;))))))

(define 
  (_++_)
  (lambda 
    (a)
    (lambda (b) (lambda (c) (record-case b ((\x5B;\x5D;) () c)
((_∷_) (d e) (((_∷_) d) ((((_++_) (list)) e) c))))))))

(define 
  (flatten)
  (lambda 
    (a)
    (lambda 
      (b)
      (lambda 
        (c)
        (lambda 
          (d)
          (lambda 
            (e)
            (record-case 
              e
              ((leaf) (f) (\x5B;\x5D;))
              ((node) 
                (g h i)
                ((((_++_) (list)) ((((((flatten) (list)) (list)) (list)) (list)) h)) 
                  (((_∷_) g) ((((((flatten) (list)) (list)) (list)) (list)) i)))))))))))

(define (testFlatten) ((((((flatten) (list)) (list)) (list)) (list)) (testInsert)))

(define 
  (sort)
  (lambda (a) (lambda (b) (lambda (c) ((((((flatten) (list)) (list)) (list)) (list)) ((((fromList) (list)) b) c))))))

(define 
  (test1)
  ((((sort) (list)) (TDO-Nat)) 
    (((_∷_) 5) (((_∷_) 3) (((_∷_) 9) (((_∷_) 1) (((_∷_) 2) (((_∷_) 10) (((_∷_) 7) (((_∷_) 4) (((_∷_) 8) (((_∷_) 6) (\x5B;\x5D;)))))))))))))

(define 
  (Ord.constructor)
  (lambda (a) (lambda (b) (lambda (c) (lambda (d) (list 'Ord.constructor a
b
c
d))))))

(define (TDO.constructor) (lambda (a) (lambda (b) (list 'TDO.constructor a
b))))