(import (only (chezscheme) record-case))

(define (monus) (lambda (x) (lambda (y) (max 0 (- x y)))))

(define (\x5B;\x5D;) (list '[]))

(define (_::_) (lambda (a) (lambda (b) (list '_::_ a
b))))

(define 
  (filter)
  (lambda 
    (a)
    (lambda 
      (b)
      (lambda 
        (c)
        (record-case 
          c
          (([]) () c)
          ((_::_) (d e) (let ((f (b d))) (if f (((_::_) d) ((((filter) (list)) b) e))
((((filter) (list)) b) e)))))))))

(define 
  (testfilter)
  ((((filter) (list)) (lambda (a) (< a 5))) (((_::_) 2) (((_::_) 4) (((_::_) 6) (((_::_) 8) (((_::_) 10) (\x5B;\x5D;))))))))

(define 
  (_++_)
  (lambda (a) (lambda (b) (lambda (c) (record-case b (([]) () c)
((_::_) (d e) (((_::_) d) ((((_++_) (list)) e) c))))))))

(define (test++) ((((_++_) (list)) (((_::_) 1) (((_::_) 2) (\x5B;\x5D;)))) (((_::_) 3) (((_::_) 4) (\x5B;\x5D;)))))

(define 
  (_>>=_)
  (lambda 
    (a)
    (lambda 
      (b)
      (lambda 
        (c)
        (lambda 
          (d)
          (record-case c (([]) () c)
((_::_) (e f) ((((_++_) (list)) (d e)) (((((_>>=_) (list)) (list)) f) d)))))))))

(define 
  (testbind)
  (((((_>>=_) (list)) (list)) (((_::_) 1) (((_::_) 2) (\x5B;\x5D;)))) 
    (lambda (a) (((_::_) (+ 1 a)) (((_::_) (+ 2 a)) (\x5B;\x5D;))))))

(define (range) (lambda (a) (lambda (b) (((((go) (list)) (list)) (((monus) (+ 1 b)) a)) a))))

(define 
  (go)
  (lambda 
    (a)
    (lambda 
      (b)
      (lambda 
        (c)
        (lambda (d) (if (= 0 c) (\x5B;\x5D;)
(let ((f (- c 1))) (((_::_) d) (((((go) (list)) (list)) f) (+ 1 d))))))))))

(define (fst) (lambda (a) (record-case a ((triple) (b c d) b))))

(define (snd) (lambda (a) (record-case a ((triple) (b c d) c))))

(define (trd) (lambda (a) (record-case a ((triple) (b c d) d))))

(define (triple) (lambda (a) (lambda (b) (lambda (c) (list 'triple a
b
c)))))

(define 
  (alltriples)
  (lambda 
    (a)
    (((((_>>=_) (list)) (list)) (((range) 1) a)) 
      (lambda 
        (b)
        (((((_>>=_) (list)) (list)) (((range) 1) b)) 
          (lambda 
            (c)
            (((((_>>=_) (list)) (list)) (((range) 1) c)) (lambda (d) (((_::_) ((((triple) d) c) b)) (\x5B;\x5D;))))))))))

(define (testalltriples) ((alltriples) 5))

(define (cartesian) (lambda (a) (record-case a ((triple) (b c d) (= (+ (* b b) (* c c)) (* d d))))))

(define (testcartesian) ((cartesian) ((((triple) 3) 4) 5)))

(define (triples) (lambda (a) ((((filter) (list)) (cartesian)) ((alltriples) a))))

(define 
  (sumall)
  (lambda 
    (a)
    (record-case 
      a
      (([]) () 0)
      ((_::_) (b c) (record-case b ((triple) (d e f) (+ (+ (+ ((sumall) c) d) e) f)))))))

(define (test1) ((sumall) ((triples) 200)))