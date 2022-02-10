#lang racket

(require racket/control)
(define inc (lambda (w_0 _) 
        (let* [
            (x ((lambda (w_1 v_2) 
                (let* [
                    (ev_4 (hash-ref 
                        w_1
                        'State
                    ))
                    (m_5 (car ev_4))
                    (h_6 (cadr ev_4))
                    (f_7 (hash-ref 
                        h_6
                        'get
                    ))
                ]
                
                    (shift0-at m_5 k_3
                        (f_7
                            v_2
                            (lambda (w_8 x_9) 
                                (k_3
                                    x_9
                                )
                            )
                        )
                    )
                )
            )
                w_0
                0
            ))
        ]
        
            ((lambda (w_10 v_11) 
                (let* [
                    (ev_13 (hash-ref 
                        w_10
                        'State
                    ))
                    (m_14 (car ev_13))
                    (h_15 (cadr ev_13))
                    (f_16 (hash-ref 
                        h_15
                        'put
                    ))
                ]
                
                    (shift0-at m_14 k_12
                        (f_16
                            v_11
                            (lambda (w_17 x_18) 
                                (k_12
                                    x_18
                                )
                            )
                        )
                    )
                )
            )
                w_0
                (+ 
                    x
                    1
                )
            )
        )
    ))
(define test (lambda (w_19 _) 
        (let* [
            (_ (inc
                w_19
                0
            ))
        ]
        
            (let* [
                (_ (inc
                    w_19
                    0
                ))
            ]
            
                (let* [
                    (_ (inc
                        w_19
                        0
                    ))
                ]
                
                    (lambda (w_20 s) 
                        s
                    )
                )
            )
        )
    ))
(define main (lambda (w_21 _) 
        (((lambda (w_24 body_22) 
            (let* [
                (m_23 (make-continuation-prompt-tag 'm_23))
                (w_25 (hash-set 
                    w_24
                    'State
                    (list 
                        m_23
                        (hash 
                            'get
                            (lambda (x k) 
                                (lambda (w_26 y) 
                                    ((k
                                        w_26
                                        y
                                    )
                                        w_26
                                        y
                                    )
                                )
                            )
                            'put
                            (lambda (x k) 
                                (lambda (w_27 y) 
                                    ((k
                                        w_27
                                        0
                                    )
                                        w_27
                                        x
                                    )
                                )
                            )
                        )
                        w_24
                    )
                ))
            ]
            
                (reset0-at m_23
                    (body_22
                        w_25
                        0
                    )
                )
            )
        )
            w_21
            test
        )
            w_21
            2
        )
    ))

(println (main (hash) 0))
