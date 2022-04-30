(import (only (chezscheme) record-case))

(define (plus3) (lambda (a) (+ 3 (force a))))

(define (pred) (lambda (a) (if (= 0 (force a)) 0
(- (force a) 1))))

(define (test1) ((pred) (delay (+ 1 ((pred) (delay ((plus3) (delay 40))))))))

(define (twice) (lambda (a) (* 2 (force a))))

(define (pow2) (lambda (a) (if (= 0 (force a)) 1
(let ((b (delay (- (force a) 1)))) ((twice) (delay ((pow2) b)))))))

(define (consume) (lambda (a) (if (= 0 (force a)) 0
(let ((b (delay (- (force a) 1)))) ((consume) b)))))

(define (test2) ((consume) (delay ((pow2) (delay 24)))))