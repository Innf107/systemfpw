#lang racket
(define main (lambda (x) 
        (main
            0
        )
    ))

(println (main 0))