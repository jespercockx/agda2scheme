(import (only (chezscheme) record-case))

(define (monus x y) (max 0 (- (force x) (force y))))

(define (seq x y) (begin (force x) y))

(define (id a) (force a))

(define (not b) (record-case (force b) ((true) () (list 'false))
((false) () (list 'true))))

(define (ite d e
f) (record-case (force d) ((true) () (force e))
((false) () (force f))))

(define (loop) (loop))

(define (test1) (ite (delay (list 'false)) (delay (loop))
(delay (list 'true))))

(define (one) (list 'suc (list 'zero)))

(define (two) (list 'suc (one)))

(define (three) (list 'suc (two)))

(define (pred g) (record-case (force g) ((zero) () (force g))
((suc) (h) h)))

(define (_+_ i j) (record-case (force i) ((zero) () (force j))
((suc) (k) (list 'suc (_+_ (delay k) j)))))

(define 
  (twice l)
  (record-case (force l) ((zero) () (force l))
((suc) (m) (list 'suc (list 'suc (twice (delay m)))))))

(define 
  (pow2 n)
  (record-case (force n) ((zero) () (list 'suc (force n)))
((suc) (o) (twice (delay (pow2 (delay o)))))))

(define (consume p) (record-case (force p) ((zero) () (force p))
((suc) (q) (consume (delay q)))))

(define (test2) (consume (delay (pow2 (delay (twice (delay (twice (delay (twice (delay (three))))))))))))

(define (head t) (let ((v (list-ref (force t) 1)) (w (list-ref (force t) 2))) v))

(define (tail z) (let ((b1 (list-ref (force z) 1)) (c1 (list-ref (force z) 2))) c1))

(define 
  (map g1 h1)
  (record-case 
    (force h1)
    ((nil) () (force h1))
    ((con) (j1 k1) (list 'con ((force g1) (delay j1))
(map g1 (delay k1))))))

(define 
  (test3)
  (head 
    (delay 
      (tail 
        (delay 
          (map 
            (delay (lambda (l1) (list 'suc (force l1))))
            (delay 
              (list 
                'con
                (list 'zero)
                (list 
                  'con
                  (list 'suc (list 'zero))
                  (list 'con (list 'suc (list 'suc (list 'zero)))
(list 'nil)))))))))))

(define (z123\x27;\x23;\x7C;H\x5C;x65llo) (list 'zero))

(define (test4) (z123\x27;\x23;\x7C;H\x5C;x65llo))

(define (fie m1) (list 'suc (force m1)))

(define (foe n1) (list 'suc (fie n1)))

(define 
  (fun)
  (_+_ 
    (delay (fie (delay (list 'suc (list 'suc (list 'zero))))))
    (delay (foe (delay (list 'suc (list 'suc (list 'zero))))))))