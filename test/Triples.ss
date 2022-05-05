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
         (delay ((force h) j))
         (delay (lambda (l) (list '_::_ j
l)))
         (delay (lambda (m) (force m)))) 
        (delay (filter (delay (list)) h
k))))))

(define 
  (_++_ n o
p)
  (record-case 
    (force o)
    ((\x5B;\x5D;) () (force p))
    ((_::_) (q r) (list '_::_ q
(delay (_++_ (delay (list)) r
p))))))

(define 
  (_>>=_ s t
u
v)
  (record-case 
    (force u)
    ((\x5B;\x5D;) () (force u))
    ((_::_) 
      (w x)
      (_++_ (delay (list)) (delay ((force v) w))
(delay (_>>=_ (delay (list)) (delay (list))
x
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
      ((f1 (delay (- (force c1) 1))))
      (list '_::_ d1
(delay (go (delay (list)) (delay (list))
f1
(delay (+ 1 (force d1)))))))))

(define (fst g1) (record-case (force g1) ((triple) (h1 i1 j1) (force h1))))

(define (snd k1) (record-case (force k1) ((triple) (l1 m1 n1) (force m1))))

(define (trd o1) (record-case (force o1) ((triple) (p1 q1 r1) (force r1))))

(define 
  (alltriples s1)
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
              (_>>=_ 
                (delay (list))
                (delay (list))
                (delay (range (delay 1) u1))
                (delay 
                  (lambda 
                    (v1)
                    (list 
                      '_::_
                      (delay (list 'triple v1
u1
t1))
                      (delay (list '\x5B;\x5D;)))))))))))))

(define 
  (pythagorean w1)
  (record-case 
    (force w1)
    ((triple) 
      (x1 y1 z1)
      (= (+ (* (force x1) (force x1)) (* (force y1) (force y1))) (* (force z1) (force z1))))))

(define 
  (triples a2)
  (filter (delay (list)) (delay (lambda (b2) (pythagorean b2)))
(delay (alltriples a2))))

(define 
  (sumall c2)
  (record-case 
    (force c2)
    ((\x5B;\x5D;) () 0)
    ((_::_) 
      (d2 e2)
      (record-case 
        (force d2)
        ((triple) (f2 g2 h2) (+ (+ (+ (sumall e2) (force f2)) (force g2)) (force h2)))))))

(define (test1) (sumall (delay (triples (delay 200)))))