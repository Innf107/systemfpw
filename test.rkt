#lang racket
(require racket/control)

(+ 1 (let [(m1 (make-continuation-prompt-tag 'm1))]
  (prompt-at m1
    (+ 10
      (let [(m2 (make-continuation-prompt-tag 'm2))]
        (prompt-at m2
          (+ 
              100
              (control-at m1 k 1000))
        )
      )
))))

