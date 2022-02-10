#lang racket

(require racket/control)
(define f (lambda (w_0 k) 
        (println k)
        ((lambda (w_3 body_1) 
            (let* [
                (m_2 (make-continuation-prompt-tag 'm_2))
                (w_4 (hash-set 
                    w_3
                    'reader
                    (list 
                        m_2
                        (hash 
                            'ask
                            (lambda (x k) 
                                (k
                                    w_0
                                    2
                                )
                            )
                        )
                        w_3
                    )
                ))
            ]
            
                (reset0-at m_2
                    (body_1
                        w_4
                        0
                    )
                )
            )
        )
            w_0
            (lambda (w_5 _) 
                (k
                    w_5
                    0
                )
            )
        )
    ))
(define main (lambda (w_6 _) 
        (f
            w_6
            ((lambda (w_9 body_7) 
                (let* [
                    (m_8 (make-continuation-prompt-tag 'm_8))
                    (w_10 (hash-set 
                        w_9
                        'reader
                        (list 
                            m_8
                            (hash 
                                'ask
                                (lambda (x k) 
                                    (k
                                        w_6
                                        1
                                    )
                                )
                            )
                            w_9
                        )
                    ))
                ]
                
                    (reset0-at m_8
                        (body_7
                            w_10
                            0
                        )
                    )
                )
            )
                w_6
                (lambda (w_11 _) 
                    ((lambda (w_14 body_12) 
                        (let* [
                            (m_13 (make-continuation-prompt-tag 'm_13))
                            (w_15 (hash-set 
                                w_14
                                'evil
                                (list 
                                    m_13
                                    (hash 
                                        'evil
                                        (lambda (x k) 
                                            k
                                        )
                                    )
                                    w_14
                                )
                            ))
                        ]
                        
                            (reset0-at m_13
                                (body_12
                                    w_15
                                    0
                                )
                            )
                        )
                    )
                        w_11
                        (lambda (w_16 _) 
                            (let* [
                                (_ ((lambda (w_17 v_18) 
                                    (let* [
                                        (ev_20 (hash-ref 
                                            w_17
                                            'reader
                                        ))
                                        (m_21 (car ev_20))
                                        (h_22 (cadr ev_20))
                                        (f_23 (hash-ref 
                                            h_22
                                            'ask
                                        ))
                                    ]
                                    
                                        (shift0-at m_21 k_19
                                            (f_23
                                                v_18
                                                (lambda (w_24 x_25) 
                                                    (k_19
                                                        x_25
                                                    )
                                                )
                                            )
                                        )
                                    )
                                )
                                    w_16
                                    0
                                ))
                            ]
                            
                                (let* [
                                    (_ ((lambda (w_26 v_27) 
                                        (let* [
                                            (ev_29 (hash-ref 
                                                w_26
                                                'evil
                                            ))
                                            (m_30 (car ev_29))
                                            (h_31 (cadr ev_29))
                                            (f_32 (hash-ref 
                                                h_31
                                                'evil
                                            ))
                                        ]
                                        
                                            (shift0-at m_30 k_28
                                                (f_32
                                                    v_27
                                                    (lambda (w_33 x_34) 
                                                        (k_28
                                                            x_34
                                                        )
                                                    )
                                                )
                                            )
                                        )
                                    )
                                        w_16
                                        0
                                    ))
                                ]
                                
                                    ((lambda (w_35 v_36) 
                                        (let* [
                                            (ev_38 (hash-ref 
                                                w_35
                                                'reader
                                            ))
                                            (m_39 (car ev_38))
                                            (h_40 (cadr ev_38))
                                            (f_41 (hash-ref 
                                                h_40
                                                'ask
                                            ))
                                        ]
                                        
                                            (shift0-at m_39 k_37
                                                (f_41
                                                    v_36
                                                    (lambda (w_42 x_43) 
                                                        (k_37
                                                            x_43
                                                        )
                                                    )
                                                )
                                            )
                                        )
                                    )
                                        w_16
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

(println (main (hash) 0))
