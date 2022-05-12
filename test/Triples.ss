(import (only (chezscheme) record-case))

(define (monus x y) (max 0 (- (force x) (force y))))

(define (seq x y) (begin (force x) y))

(define (if_then_else_ b c
d) (if (force b) (force c)
(force d)))

(define (id f) (force f))

(define 
  (filter h i)
  (record-case 
    (force i)
    ((\x5B;\x5D;) () (force i))
    ((_::_) 
      (j k)
      ((if_then_else_ 
         (delay ((force h) (delay j)))
         (delay (lambda (l) (list '_::_ j
(force l))))
         (delay (lambda (m) (force m)))) 
        (delay (filter h (delay k)))))))

(define 
  (_++_ o p)
  (record-case (force o) ((\x5B;\x5D;) () (force p))
((_::_) (q r) (list '_::_ q
(_++_ (delay r) p)))))

(define 
  (_>>=_ u v)
  (record-case 
    (force u)
    ((\x5B;\x5D;) () (force u))
    ((_::_) (w x) (_++_ (delay ((force v) (delay w))) (delay (_>>=_ (delay x) v))))))

(define (range y z) (go (delay (monus (delay (+ 1 (force z))) y)) y))

(define 
  (go c1 d1)
  (if 
    (= 0 (force c1))
    (list '\x5B;\x5D;)
    (let ((e1 (delay (- (force c1) 1)))) (list '_::_ (force d1)
(go e1 (delay (+ 1 (force d1))))))))

(define 
  (fst f1)
  (let ((g1 (list-ref (force f1) 0)) (h1 (list-ref (force f1) 1)) (i1 (list-ref (force f1) 2))) g1))

(define 
  (snd j1)
  (let ((k1 (list-ref (force j1) 0)) (l1 (list-ref (force j1) 1)) (m1 (list-ref (force j1) 2))) l1))

(define 
  (trd n1)
  (let ((o1 (list-ref (force n1) 0)) (p1 (list-ref (force n1) 1)) (q1 (list-ref (force n1) 2))) q1))

(define 
  (alltriples r1)
  (_>>=_ 
    (delay (range (delay 1) r1))
    (delay 
      (lambda 
        (s1)
        (_>>=_ 
          (delay (range (delay 1) s1))
          (delay 
            (lambda 
              (t1)
              (_>>=_ 
                (delay (range (delay 1) t1))
                (delay 
                  (lambda 
                    (u1)
                    (list '_::_ (list (force u1) (force t1)
(force s1))
(list '\x5B;\x5D;))))))))))))

(define 
  (pythagorean v1)
  (let 
    ((w1 (list-ref (force v1) 0)) (x1 (list-ref (force v1) 1)) (y1 (list-ref (force v1) 2)))
    (= (+ (* w1 w1) (* x1 x1)) (* y1 y1))))

(define (triples z1) (filter (delay (lambda (a2) (pythagorean a2))) (delay (alltriples z1))))

(define 
  (sumall b2)
  (record-case 
    (force b2)
    ((\x5B;\x5D;) () 0)
    ((_::_) 
      (c2 d2)
      (let 
        ((e2 (list-ref c2 0)) (f2 (list-ref c2 1)) (g2 (list-ref c2 2)))
        (+ (+ (+ (sumall (delay d2)) e2) f2) g2)))))

(define (test1) (sumall (delay (triples (delay 200)))))