#lang racket
(require racket/control)

(provide handler perform)

; <<<<<<<<<<<<<<<<<<EVIDENCE STACK>>>>>>>>>>>>>>>>>>>
(define evidence-stack '())

(define (push-evidence! w)
	(set! evidence-stack (cons w evidence-stack))
)

(define (get-evidence)
    (car evidence-stack)
)

; removes one entty from the top of the evidence stack
; does *NOT* return the popped evidence
(define (pop-evidence!)
	(set! evidence-stack (cdr evidence-stack))
)
; <<<<<<<<<<<<<<<<</EVIDENCE STACK>>>>>>>>>>>>>>>>>>>



; <<<<<<<<<<<<<<<<<EVIDENCE GLOBAL>>>>>>>>>>>>>>>>>>

(define evidence (hash))


(define (handler l h)
	(lambda (v)
		(let* [
			(old-evidence evidence)
			(m (make-continuation-prompt-tag))
		]
		(set! evidence (hash-set evidence l (list m h old-evidence)))
		(map display (list "perform" " " l " ... START"))
		(displayln "")
		(reset0-at m (v '()))
		(map display (list "perform" " " l " ... END"))
		(displayln "")
		(set! evidence old-evidence)
		)
	)
)

(define (perform l op)
	(lambda (v)
	  	(map display (list "perform" l op v))
		(displayln "")
		(let* [
			(ev (hash-ref evidence l))
			(m (car ev))
			(h (cadr ev))
			(f (hash-ref h op))
		]
		(shift0-at m k (f v k))
)))
