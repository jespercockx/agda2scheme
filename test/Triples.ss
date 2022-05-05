(import (only (chezscheme) record-case))

(define (monus x y) (max 0 (- (force x) (force y))))

(define (seq x y) (begin (force x) y))

(define (if_then_else_ a b
c
d) (if (force b) (force c)
(force d)))

(define (id e f) (force f))

(define 
  (filter g h
i)
  (record-case 
    (force i)
    ((\x5B;\x5D;) () (force i))
    ((_::_) 
      (j k)
      ((if_then_else_ 
         (delay (list))
         (delay ((force h) (delay j)))
         (delay (lambda (l) (list '_::_ j
(force l))))
         (delay (lambda (m) (force m)))) 
        (delay (filter (delay (list)) h
(delay k)))))))

(define 
  (_++_ n o
p)
  (record-case 
    (force o)
    ((\x5B;\x5D;) () (force p))
    ((_::_) (q r) (list '_::_ q
(_++_ (delay (list)) (delay r)
p)))))

(define 
  (_>>=_ s t
u
v)
  (record-case 
    (force u)
    ((\x5B;\x5D;) () (force u))
    ((_::_) 
      (w x)
      (_++_ 
        (delay (list))
        (delay ((force v) (delay w)))
        (delay (_>>=_ (delay (list)) (delay (list))
(delay x)
v))))))

(define (range y z) (go (delay (list)) (delay (list))
(delay (monus (delay (+ 1 (force z))) y))
y))

(define 
  (go a1 b1
c1
d1)
  (if 
    (= 0 (force c1))
    (list '\x5B;\x5D;)
    (let 
      ((e1 (delay (- (force c1) 1))))
      (list '_::_ (force d1)
(go (delay (list)) (delay (list))
e1
(delay (+ 1 (force d1))))))))

(define (fst f1) (record-case (force f1) ((triple) (g1 h1 i1) g1)))

(define (snd j1) (record-case (force j1) ((triple) (k1 l1 m1) l1)))

(define (trd n1) (record-case (force n1) ((triple) (o1 p1 q1) q1)))

(define 
  (alltriples r1)
  (_>>=_ 
    (delay (list))
    (delay (list))
    (delay (range (delay 1) r1))
    (delay 
      (lambda 
        (s1)
        (_>>=_ 
          (delay (list))
          (delay (list))
          (delay (range (delay 1) s1))
          (delay 
            (lambda 
              (t1)
              (_>>=_ 
                (delay (list))
                (delay (list))
                (delay (range (delay 1) t1))
                (delay 
                  (lambda 
                    (u1)
                    (list 
                      '_::_
                      (list 'triple (force u1)
(force t1)
(force s1))
                      (list '\x5B;\x5D;))))))))))))

(define 
  (pythagorean v1)
  (record-case (force v1) ((triple) (w1 x1 y1) (= (+ (* w1 w1) (* x1 x1)) (* y1 y1)))))

(define 
  (triples z1)
  (filter (delay (list)) (delay (lambda (a2) (pythagorean a2)))
(delay (alltriples z1))))

(define 
  (sumall b2)
  (record-case 
    (force b2)
    ((\x5B;\x5D;) () 0)
    ((_::_) (c2 d2) (record-case c2 ((triple) (e2 f2 g2) (+ (+ (+ (sumall (delay d2)) e2) f2) g2))))))

(define (test1) (sumall (delay (triples (delay 200)))))