(import (only (chezscheme) record-case))

(define 
  (if_then_else_)
  (lambda (a) (lambda (b) (lambda (c) (lambda (d) (if (force b) (force c)
(force d)))))))

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
          (force c)
          (([]) () (force c))
          ((_::_) 
            (d e)
            (((((if_then_else_) (delay (list))) (delay ((force b) d))) 
               (delay (((_::_) d) (delay ((((filter) (delay (list))) b) e))))) 
              (delay ((((filter) (delay (list))) b) e)))))))))

(define 
  (_++_)
  (lambda 
    (a)
    (lambda 
      (b)
      (lambda 
        (c)
        (record-case 
          (force b)
          (([]) () (force c))
          ((_::_) (d e) (((_::_) d) (delay ((((_++_) (delay (list))) e) c)))))))))

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
          (record-case 
            (force c)
            (([]) () (force c))
            ((_::_) 
              (e f)
              ((((_++_) (delay (list))) (delay ((force d) e))) 
                (delay (((((_>>=_) (delay (list))) (delay (list))) f) d))))))))))

(define 
  (range)
  (lambda (a) (lambda (b) (((((go) (delay (list))) (delay (list))) (delay (((_-_) (delay (+ 1 (force b)))) a))) a))))

(define 
  (go)
  (lambda 
    (a)
    (lambda 
      (b)
      (lambda 
        (c)
        (lambda 
          (d)
          (if 
            (= 0 (force c))
            (\x5B;\x5D;)
            (let 
              ((e (delay (- (force c) 1))))
              (((_::_) d) (delay (((((go) (delay (list))) (delay (list))) e) (delay (+ 1 (force d)))))))))))))

(define (fst) (lambda (a) (record-case (force a) ((triple) (b c d) (force b)))))

(define (snd) (lambda (a) (record-case (force a) ((triple) (b c d) (force c)))))

(define (trd) (lambda (a) (record-case (force a) ((triple) (b c d) (force d)))))

(define (triple) (lambda (a) (lambda (b) (lambda (c) (list 'triple a
b
c)))))

(define 
  (triples)
  (lambda 
    (a)
    ((((filter) (delay (list))) 
       (delay (lambda (b) (= (+ (* ((snd) b) ((snd) b)) (* ((fst) b) ((fst) b))) (* ((trd) b) ((trd) b)))))) 
      (delay 
        (((((_>>=_) (delay (list))) (delay (list))) (delay (((range) (delay 1)) a))) 
          (delay 
            (lambda 
              (c)
              (((((_>>=_) (delay (list))) (delay (list))) (delay (((range) (delay 1)) c))) 
                (delay 
                  (lambda 
                    (d)
                    (((((_>>=_) (delay (list))) (delay (list))) (delay (((range) (delay 1)) d))) 
                      (delay (lambda (e) (((_::_) (delay ((((triple) e) d) c))) (delay (\x5B;\x5D;))))))))))))))))

(define (test1) ((triples) (delay 100)))