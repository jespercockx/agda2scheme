(import (only (chezscheme) record-case))

(define (add) (lambda (m) (lambda (n) (+ (force m) (force n)))))

(define (sub) (lambda (m) (lambda (n) (- (force m) (force n)))))

(define (mul) (lambda (m) (lambda (n) (* (force m) (force n)))))

(define (quot) (lambda (m) (lambda (n) (div (force m) (force n)))))

(define (rem) (lambda (m) (lambda (n) (mod (force m) (force n)))))

(define (iff) (lambda (b) (lambda (x) (lambda (y) (if (force b) (force x)
(force y))))))

(define (eq) (lambda (x) (lambda (y) (= (force x) (force y)))))

(define (Level) (begin (display "encountered axiom: Level\n") (exit 1)))

(define (lzero) (begin (display "encountered axiom: lzero\n") (exit 1)))

(define (lsuc) (begin (display "encountered axiom: lsuc\n") (exit 1)))

(define (_⊔_) (begin (display "encountered axiom: _⊔_\n") (exit 1)))