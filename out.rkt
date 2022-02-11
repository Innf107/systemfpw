#lang racket

(require racket/control)
(require "./cobble.rkt")
(define f (lambda (k) 
        ((handler
            'reader
            (hash 
                'ask
                (lambda (x k) 
                    (k
                        2
                    )
                )
            )
        )
            (lambda (_) 
                (k
                    0
                )
            )
        )
    ))
(define main (lambda (_) 
        (f
            ((handler
                'reader
                (hash 
                    'ask
                    (lambda (x k) 
                        (k
                            1
                        )
                    )
                )
            )
                (lambda (_) 
                    ((handler
                        'evil
                        (hash 
                            'evil
                            (lambda (x k) 
                                k
                            )
                        )
                    )
                        (lambda (_) 
                            (let* [
                                (_ ((perform
                                    'reader
                                    'ask
                                )
                                    0
                                ))
                            ]
                            
                                (let* [
                                    (_ ((perform
                                        'evil
                                        'evil
                                    )
                                        0
                                    ))
                                ]
                                
                                    ((perform
                                        'reader
                                        'ask
                                    )
                                        0
                                    )
                                )
                            )
                        )
                    )
                )
            )
        )
    ))

(println (main '()))
