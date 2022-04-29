(import (only (chezscheme) record-case))

(define (add) (lambda (m) (lambda (n) (+ (force m) (force n)))))

(define (sub) (lambda (m) (lambda (n) (- (force m) (force n)))))

(define (mul) (lambda (m) (lambda (n) (* (force m) (force n)))))

(define (quot) (lambda (m) (lambda (n) (div (force m) (force n)))))

(define (rem) (lambda (m) (lambda (n) (mod (force m) (force n)))))

(define (iff) (lambda (b) (lambda (x) (lambda (y) (if (force b) (force x)
(force y))))))

(define (eq) (lambda (x) (lambda (y) (= (force x) (force y)))))

(define (plus3) (lambda (a) (((add) (delay 3)) a)))

(define (pred) (lambda (a) ((((iff) (delay (((eq) (delay 0)) a))) (delay 0)) (delay (((sub) a) (delay 1))))))

(define (test1) ((pred) (delay (((add) (delay 1)) (delay ((pred) (delay ((plus3) (delay 40)))))))))

(define (twice) (lambda (a) (((mul) (delay 2)) a)))

(define 
  (pow2)
  (lambda 
    (a)
    ((((iff) (delay (((eq) (delay 0)) a))) (delay 1)) 
      (delay (let ((b (delay (((sub) a) (delay 1))))) ((twice) (delay ((pow2) b))))))))

(define 
  (consume)
  (lambda 
    (a)
    ((((iff) (delay (((eq) (delay 0)) a))) (delay 0)) (delay (let ((b (delay (((sub) a) (delay 1))))) ((consume) b))))))

(define (test2) ((consume) (delay ((pow2) (delay 24)))))