(import (only (chezscheme) record-case))

(define (Level) (begin (display "encountered axiom: Level\n") (exit 1)))

(define (lzero) (begin (display "encountered axiom: lzero\n") (exit 1)))

(define (lsuc) (begin (display "encountered axiom: lsuc\n") (exit 1)))

(define (_⊔_) (begin (display "encountered axiom: _⊔_\n") (exit 1)))