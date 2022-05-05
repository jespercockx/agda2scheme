(import (only (chezscheme) record-case))

(define (monus x y) (max 0 (- (force x) (force y))))

(define (seq x y) (begin (force x) y))

(define (id a) (force a))

(define (not b) (record-case (force b) ((true) () (list 'false))
((false) () (list 'true))))

(define (ite c d
e
f) (record-case (force d) ((true) () (force e))
((false) () (force f))))

(define (loop) (loop))

(define (test1) (ite (delay (list)) (delay (list 'false))
(delay (loop))
(delay (list 'true))))

(define (one) (list 'suc (delay (list 'zero))))

(define (two) (list 'suc (delay (one))))

(define (three) (list 'suc (delay (two))))

(define (pred g) (record-case (force g) ((zero) () (force g))
((suc) (h) (force h))))

(define (_+_ i j) (record-case (force i) ((zero) () (force j))
((suc) (k) (list 'suc (delay (_+_ k j))))))

(define 
  (twice l)
  (record-case 
    (force l)
    ((zero) () (force l))
    ((suc) (m) (list 'suc (delay (list 'suc (delay (twice m))))))))

(define (pow2 n) (record-case (force n) ((zero) () (list 'suc n))
((suc) (o) (twice (delay (pow2 o))))))

(define (consume p) (record-case (force p) ((zero) () (force p))
((suc) (q) (consume q))))

(define (test2) (consume (delay (pow2 (delay (twice (delay (twice (delay (twice (delay (three))))))))))))

(define (head r s
t) (record-case (force t) ((con) (u v w) (force v))))

(define (tail x y
z) (record-case (force z) ((con) (a1 b1 c1) (force c1))))

(define 
  (map d1 e1
f1
g1
h1)
  (record-case 
    (force h1)
    ((nil) () (force h1))
    ((con) 
      (i1 j1 k1)
      (list 
        'con
        i1
        (delay ((force g1) j1))
        (delay (map (delay (list)) (delay (list))
(delay (list))
g1
k1))))))

(define 
  (test3)
  (head 
    (delay (list))
    (delay (list))
    (delay 
      (tail 
        (delay (list))
        (delay (list))
        (delay 
          (map 
            (delay (list))
            (delay (list))
            (delay (list))
            (delay (lambda (l1) (list 'suc l1)))
            (delay 
              (list 
                'con
                (delay (list 'suc (delay (list 'suc (delay (list 'zero))))))
                (delay (list 'zero))
                (delay 
                  (list 
                    'con
                    (delay (list 'suc (delay (list 'zero))))
                    (delay (list 'suc (delay (list 'zero))))
                    (delay 
                      (list 
                        'con
                        (delay (list 'zero))
                        (delay (list 'suc (delay (list 'suc (delay (list 'zero))))))
                        (delay (list 'nil))))))))))))))

(define (z123\x27;\x23;\x7C;H\x5C;x65llo) (list 'zero))

(define (test4) (z123\x27;\x23;\x7C;H\x5C;x65llo))

(define (fie m1) (list 'suc m1))

(define (foe n1) (list 'suc (delay (fie n1))))

(define 
  (fun)
  (_+_ 
    (delay (fie (delay (list 'suc (delay (list 'suc (delay (list 'zero))))))))
    (delay (foe (delay (list 'suc (delay (list 'suc (delay (list 'zero))))))))))