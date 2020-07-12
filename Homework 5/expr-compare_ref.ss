#lang racket
(provide (all-defined-out))


(define (Transsec x y)
  (cond
            [(equal? 1 (length x ))  (if (equal? x y) x
                                         (list ( string->symbol (string-append (symbol->string (car x)) "!" (symbol->string  (car y)) )) ) ) ] 
            [else   (append  (Transsec (list (car x)) (list (car y)))  (Transsec (cdr x) (cdr y)) )]
  )    
)

(define (indexing x y) (- (length x) (length y)))
(define (checkmem li target)
  (cond
    [(symbol? li) (member li target)]
     [(equal? '() li) '()]
    [(list? li)
     (if (equal? 1(length li))
         (checkmem (car li) target)
         (and (checkmem (car li) target) (checkmem (cdr li) target)))]
    [else #f]
     )
  )  

(define (TransBody original target  ksymbol)
   (cond [(symbol? original)
             (if (member  original target)
                        (list-ref  ksymbol (indexing target (member  original target )))
                         original   ) ]

         [(number? original)  original]

         [(equal? '() original)  '()]
         ;;not symbol must be least list, list then check if lambda length 3
         [else  (cond
                    [( and (equal? 3(length original)) (or (equal? 'lambda (car original) )(equal? 'λ (car original) )))
                     ;;if second term do no
                     (if (and (not(equal? '() (car (cdr original))))(or (equal? (car (cdr original)) target)
                             ;;check every symbol is in target
                             ( checkmem (car (cdr original)) target   ))
                          )
                          original
                         ( if  (member (car original) target)
                               (append  (list (list-ref  ksymbol (indexing target (member (car original) target) ))) (TransBody (cdr original) target ksymbol ))
                               (append (list (car original))  (TransBody (cdr original) target ksymbol ) ) 
                               )
                         )
                     ]

                    [ (equal? 1 (length original))
                 ;;length1
                          (cond  [(list? (car original))  (list (TransBody (car original) target ksymbol))] ;length is one
                                 [else (if  (member (car original) target)
                                         (append (list (list-ref  ksymbol (indexing target (member (car original) target) )))) original )
                                 ]
                           )
                     ]
                ;;length not 1 and not another lambda 
                     [else   (if  (member (car  original) target) ;;length is not one check first element
                                  (append  (list (list-ref  ksymbol (indexing target (member (car original) target) ))) (TransBody (cdr original) target ksymbol ))
;if list length not one, first check if first one satisfy else check if it can form a new lambda (first lambda length 3 ),
                     ; if new lambda has same second term do now, esle change to other 
                                  (cond
                                    [(list? (car original))( append (list (TransBody (car original) target ksymbol)) (TransBody (cdr original) target ksymbol ))] 
                                    [else (append (list (car original))  (TransBody (cdr original) target ksymbol ) ) ]    )
                               )     
                      ]
               )
        ]                          
   )
)

(define(prolambda x y)  ; x y must be three element
      (let ([secx (car (cdr x))]    [secy   (car (cdr y))]
            [thx (car (cdr (cdr x)))]   [thy (car (cdr (cdr y)))] )

           (if (equal? (length secx) (length secy))
                   (cond  [ (not (equal? secx  secy))
                               (list  'λ  (Transsec secx secy)
                                   (if (and (symbol? thx) (symbol? thy))
                                       (let ([passin (listlow (TransBody thx secx  (Transsec secx secy))  (TransBody thy secy  (Transsec secx secy)))])
                                       (if (equal? 1 (length passin )) (car passin) passin)
                                         )

                                       (listlow (TransBody thx secx  (Transsec secx secy))  (TransBody thy secy  (Transsec secx secy)))
                                   )                                 
                              )
                          ] 
                          [else (list 'λ  (listlow (cdr x) (cdr y))) ]
                   )
                   (let ([newx  (list 'λ secx thx)] [newy (list 'λ secy thy  )])
                   (list 'if '% newx newy))
             )
       )
 )

 

(define (listlow x y) ; x y must not same
  (cond  
         [ (and (boolean? x) (boolean? y) )
              (if (equal? x y ) x (if (equal? x #t) '%  '(not %)))]
         [ (and (equal? x '()) (equal? y '()) )      x  ]
         [ (and (pair? x) (pair? y) )
           
           (cond [ (or (equal? 'quote (car x)) (equal? 'quote (car y)) ) (list 'if  '% x y)     ];;;remove
                 [ (and (and (or  (or (equal? 'λ (car x)) (equal? 'lambda (car x)) )
                                  (or (equal? 'λ (car y))(equal? 'lambda (car y)) ))
                             (equal? 3 (length x )))
                        (not (equal? (car (cdr x))  (car (cdr y)))))

                    (prolambda x y)   ]
                 
                 [ (and (list? (car x)) (list? (car y)))
                   (append (list (listlow (car x) (car y) ))  (listlow (cdr x) (cdr y)))  ]
                 
                 [(if (equal? (car x) (car y)) 
                      ( if  (equal? 'lambda (car x)) ( append (list 'λ ) (listlow (cdr x) (cdr y)))
                            ( append (list (car x)) (listlow (cdr x) (cdr y)))
                            )
                      
                      (if  (and (or  (or (equal? 'λ (car x)) (equal? 'λ (car y))) (or (equal? 'lambda (car x)) (equal? 'lambda (car y)) ) )
                                (and (equal? 3 (length x)) (equal? 3 (length y))))
                            (append (list 'λ)  (listlow (cdr x) (cdr y)))
                            (append (list (list 'if  '%  (car x) (car y)) )  (listlow (cdr x) (cdr y)))))  ]   ;;remove 
                 
                 [else (list 'if '% x y) ])
          ]
         [( and (symbol? x) (symbol? y))   (if (equal? x y) (list x) (list 'if '% x y) )]
         [else  (list 'if '% x y ) ] 
  )
)
(define (listcom x y)
  (cond [(not (equal? (length x) (length y)))  (list 'if '% x y) ]
        [(equal? 0 (length x)) x]
        [ (or (equal? 'quote  (car x)) (equal? 'quote  (car y) ) ) (list 'if  '% x y)     ]
        [(if (equal? (car x) (car y))  
             ( cond
                [(and (or (equal? 'lambda (car x)) (equal? 'λ (car x)) )
                      (not (equal? (car (cdr x))  (car (cdr y)))  ))
                     (prolambda x y) ]
                
                [else
                 (if (and (equal? 3 (length x)) (or (equal? 'lambda (car x)) (equal? 'λ (car x)) ))
                     (append  (list 'λ) (listcom (cdr x) (cdr y)))
                 (append  (list(car x)) (listcom (cdr x) (cdr y)) )) ]

              )             
             (cond 
                  [ (or (equal? (car x) 'if) (equal? (car y) 'if ))    (list 'if '% x y)   ]
                  
                  [ (and (or (and (equal? 'lambda (car x))  (equal? 'λ (car y)) )   (and (equal? 'lambda (car y))  (equal? 'λ (car x)) ))
                         (equal? 3 (length x)))
                      (if (equal? (car (cdr x))  (car (cdr y)))  (append (list 'λ) (listcom (cdr x) (cdr y)))
                     (prolambda x y)) ]
                  
                  [ (and (or (equal? (car x) 'lambda) (equal? (car y) 'lambda )) (and (equal? 3(length x))(equal? 3 (length y))) )

                    (cond [(equal? (car x) 'lambda)   (list  'if '%  (append (list 'λ) (cdr x))  y)]
                          [else (list 'if '%  x  (append(list 'λ) (cdr y)))]   )
                  ]

                  [ (or (or (equal? (car x) 'lambda) (equal? (car y) 'lambda ))  (or (equal? (car x) 'λ) (equal? (car y) 'λ )) )
                        (if (equal? 3 (length x)) (list  'if '% x y)
                            (if (equal? 1 (length x)) (list (list 'if '% (car x) (car y)))
                            (list (list 'if '% (car x) (car y)) (listcom (cdr x) (cdr y))))) ]

                  [else (append  (list (listlow (car x) (car y)))  (listcom (cdr x) (cdr y)))]))
         ]
        
        [else (list 'if '% x y)])
  )


(define (expr-compare x y)
 (  cond [(and (list? x) (list? y))
                 (if (equal? x y )
                     (if (equal? 'lambda (car x)) (append (list 'λ) (cdr x)) x )
                     (listcom x y))]
         [ (and (boolean? x) (boolean? y) )
              (if (equal? x y ) x (if (equal? x #t) '%  '(not %)))]
         [ #t  (if (equal? x y ) x (list 'if '% x y))]

     ) 
)


(define (test-expr-compare x y)
  (and (equal? (eval x) (eval (list 'let '((% #t)) (expr-compare x y)))) (equal? (eval y) (eval (list 'let '((% #f)) (expr-compare x y))))))

; (define test-expr-x '(list 3 'q (+ (if #t 1 2) ((lambda (a) (* a a)) 2))) )
; (define test-expr-y '(list 3 'q (+ (if #t 1 2) ((lambda (a) (- a a)) 2))) )

(define test-expr-x '(list 3 (+ (if #t 1 2) 'quote ((lambda (a) (* a a)) 2))) )
(define test-expr-y '(list 3 (+ (if #t 1 2) 'quote ((lambda (a) (- a a)) 2))) )

; (define test-expr-x '(list 9 (+ (if #t 4 8) 'quote (lambda (b) (- g g)) )) )
; (define test-expr-y '(list 9 (+ (if #t 4 8) 'quote (lambda (b) (/ g g)) )) )

(define test-expr-x (apply + (cons 3 ((lambda (a b) (list a b)) 1 2))))
(define test-expr-y (apply + (cons 2 ((lambda (a c) (list a c)) 1 2))))

; (define test-expr-x 12)
; (define test-expr-y 12)

; (define test-expr-x (apply + (cons 3 ((lambda (a b) (list a b)) 1 2))) )
; (define test-expr-y (apply + (cons 2 ((lambda (a c) (list a c)) 1 2))) )

; (test-expr-compare test-expr-x test-expr-y)

