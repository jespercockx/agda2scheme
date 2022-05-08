(import (only (chezscheme) record-case))

(define (monus x y) (max 0 (- x y)))

(define (seq x y) y)

(define (not a) (if a #f
#t))

(define (f b) (record-case b ((c) (e) (list 'c (not e)))))

(define (map j k) (record-case k ((\x5B;\x5D;) () k)
((_∷_) (m n) (list '_∷_ (j m)
(map j n)))))

(define 
  (test1)
  (map (lambda (o) (f o)) (list '_∷_ (list 'c #t)
(list '_∷_ (list 'c #f)
(list '\x5B;\x5D;)))))