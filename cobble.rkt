#lang racket

(define (perform l op)
    (lambda (w v)
        (let* [
            (ev (hash-ref w l))
            (m (car ev))
            (handler (cadr ev))
            (handler-f (hash-ref handler op))
            (resumption-args ...)
        ]
            0
        )
    )
)
