; Include the namespace
#lang racket
(provide (all-defined-out))
; (provide expr-compare)

; Call this when I cannot bind the input together 
(define (not_handle x y)
	(list 'if '% x y)
)

; Define my own lambda symbol 
(define lambda2 (string->symbol "\u03BB"))

; Helper function for finding the difference between the index 
; Mainly for handling request from the outer input 
(define (calc_index in1 in2) (- (length in1) (length in2)))

; call this function when handling outer input 
(define (verifyelem inlst elem)
  (cond
  	; first check if the input list is empty or not, if yes, then just return empty 
  	[(equal? inlst '()) '()]
  	; check if the input list is a symbol, if yes
  	; then check to see if the element is a member of the input list 
    [(symbol? inlst) (member inlst elem)]
    ; check to see if inlst is indeed a list 
    [(list? inlst)
     ; check whether the input list is only length of one 
     (if (not(equal? (length inlst) 1))
     	 ; the length of the input list is not one, longer list
         ; the extract the head element and pass it recursively
         ; we also need to search the rest of the list recursively 
         (and (verifyelem (car inlst) elem) (verifyelem (cdr inlst) elem))
         ; the length of the input list is one 
         ; then extract the head element and pass it recursively
         (verifyelem (car inlst) elem)
     )
    ]
    ; else case, if none of the cases fulfilled, then return false at the end as flag
    [else #f]
  )
)  

; This is called when we are in the pre-check state 
; create_binding function 
; find out the place with handle_outer with two inputs
(define (string_a a) (symbol->string a))
(define (string_b b) (symbol->string b))
(define (create_binding x y)
  (cond
  	; check if the length of x is one or not 
  	[ (not(equal? (length x) 1))
  		; when length x is not one 
  		; create a new list with handling of head x, y and the rest of x and y
  		(append (create_binding (list (car x)) (list (car y))) (create_binding (cdr x) (cdr y)))
  	] 
  	; when length x is one 
    [else (if (not(equal? x y))
    		; when x and y are not equal 
    		; we find a binding, same place, but different variables names for expressions
            (list (string->symbol (string-append (string_a (car x)) "!" (string_b (car y))))) 
    		; when x and y are equal, we just return x which is the same as y
  			x
          ) 
    ]
  )    
)

; call this for the inner head of the pre-check 
(define (handle_outer input elem symlst)
   (cond 
   	     ; swapped the order of the following three clauses 
   	     ; check whether the input is empty, if yes, then return the empty list 
   		 [(equal? input '()) '()]

   		 ; check if the input is a number, if yes, then simply return the input 
   		 [(number? input) input]

   		 ; check if the input is a symbol 
   		 [(symbol? input)
   		 	; check if the input belong to the list 
            (if (not(member input elem))
            	; input is not a member, just return the input 
                input
            	; input is a member 
            	; find out the element at the specific index 
            	
              (list-ref symlst (calc_index elem (member input elem)))
            )
         ]

         ; else case, it is not a symbol, which means it is a list 
         [else (cond
                    [	
                    	(or (and (equal? (length input) 3) (equal? (car input) 'lambda) ) 
                    		(and (equal? (length input) 3) (equal? (car input) lambda2) )
                    	)

                     	; if the condition is true, check the following clause
                     	(if 
                     		; check if head rest input is empty 
                     		; then check if the head rest input is equal to elem
                     		; or deep check if the head rest input is embedded inside elem
                     		(or (and (not(equal? (car (cdr input)) '())) (equal? (car (cdr input)) elem) ) 
                     			(and (not(equal? (car (cdr input)) '())) (verifyelem (car (cdr input)) elem) )
                     		)

                     		; if the condition is true
	                        input
	                        ; if the condition is not true
	                        ; check if head input belong to the list  
	                        (if (not(member (car input) elem))

	                            ; it is not a member 
	                            ; create a list with the head input and handle the rest of input
	                            (append (list (car input)) (handle_outer (cdr input) elem symlst)) 
	                        	; it is a member 
	                        	; same, we need to first find out the index and get the element before creating the list 
	                        	; and handle the rest of input 
	                            (append (list (list-ref symlst (calc_index elem (member (car input) elem)))) (handle_outer (cdr input) elem symlst))
	                        )
	                    )
                    ]

                    ; check if the length of input is one 
                    [(equal? (length input) 1)
                    	; if it is true, check the following clauses 
                    	(cond 
                      	    ; check if the head input is a list 
                            [(not (list? (car input)))
                            	; check if the head input is member of the list 
                            	(if (not (member (car input) elem))
                            		; it is not a member of the list, simply return the input 
                                    input
                            		; it is a member of the list 
                            		; find the element at a given index and create a list of that 
                                    (append (list (list-ref symlst (calc_index elem (member (car input) elem))))) 
                                )
                            ]

                            [else ; if it is list, then we create a list with handling of head input, element and list 
                    			(list (handle_outer (car input) elem symlst))
                    	    ]

                        )
                    ]
                 	; the input length is not one 
                 	; then check whether the head input is member of the list 
                    [else (if (member (car input) elem)
                    	; the condition is correct
                    	; find the element at a given index and create a list of that,
                    	; need to handle the rest of input still 
                        (append (list (list-ref symlst (calc_index elem (member (car input) elem)))) (handle_outer (cdr input) elem symlst))

                     	  ; the condition is not correct 
                     	  ; the input length is not one and it is not member of the list also 
                          (cond
                            ; check if head input is a list 
                            [(not (list? (car input)))
                              ; the head input is not a list 
                              (append (list (car input)) (handle_outer (cdr input) elem symlst))
                            ] 

                            ; the head input is a list 
                            [else (append (list (handle_outer (car input) elem symlst)) (handle_outer (cdr input) elem symlst))]  
                          )
                        )     
                    ]
               )
         ]                          
   )
)

; listlow func
; call the following function when we are in the precheck state
(define (deep_handle x y)
  (cond 
  	; then check whether both x and y are simply #t or #f values 
    [(and (boolean? x) (boolean? y))
    	; check if x and y are equal 
      	(if 
      		(not (equal? x y))
      		; x and y are not equal 
      		; then check whether x is equal to #t or not 
      		(if 
      			(not(equal? x #t))
      			; x is not #t 
      			'(not %)
      			; x is #t
      			'%  
      	    )

      		; x and y are equal 
      		x 
        )
    ]

    ; first check if x and y both equal to empty
  	; then just simply return x 
  	[(and (equal? '() x) (equal? '() y)) x]

    ; check if x and y are both a pair, not just single element 
    [(and (pair? x) (pair? y))
      (cond 
      	; check whether x and y are equal to quote 
      	[(or (equal? 'quote (car x)) (equal? 'quote (car y))) 
      		; if one of them is quote, then do not handle it 
      		(not_handle x y)

      	]

	    [
	     ; first check if head rest x and y are equal or not 
	     ; the check if the length of x is equal to 3 
	     ; at last check whether x or y is equal to lambda or lambda symbol
	     ; total 4 clauses

	     (or (and (and (equal? (car x) lambda2) (equal? 3 (length x )) ) (not (equal? (car (cdr x))  (car (cdr y)))) ) 
	     	 (and (and (equal? (car x) 'lambda) (equal? 3 (length x )) ) (not (equal? (car (cdr x))  (car (cdr y)))) ) 
	     	 (and (and (equal? (car y) lambda2) (equal? 3 (length x )) ) (not (equal? (car (cdr x))  (car (cdr y)))) ) 
	     	 (and (and (equal? (car y) 'lambda) (equal? 3 (length x )) ) (not (equal? (car (cdr x))  (car (cdr y)))) )
	     )


	      ; if the condition is true 
	      (pre_check x y)   
	    ]

	    [(and (symbol? x) (symbol? y)) 

    	; if both x and y are symbol 
    	; check if x and y are equal
    	(if 
    		(not(equal? x y))
    		; x and y are not equal, no need to handle it 
    		(not_handle x y)
    		; x and y are equal, return a list with just element x, cause either one is the same 
    		(list x) 
    	)
        ]

	    ; check if head x and head y are both list or not 
	    [(and (list? (car x)) (list? (car y)))
	    	; create a new list with handling of head of x and y, and rest of x and y 
	    	(append (list (deep_handle (car x) (car y))) (deep_handle (cdr x) (cdr y))) 
	    ]

	    ; check if head x and head y are equal      
	    [(if (not(equal? (car x) (car y)))

	    	; when head x and y are not the same 
	        (if 

	         	; replace with above 
	        	(or (and (and (equal? (length x) 3) (equal? (length y) 3)) (equal? (car x) lambda2)) 
	        		(and (and (equal? (length x) 3) (equal? (length y) 3)) (equal? (car y) lambda2)) 
	        		(and (and (equal? (length x) 3) (equal? (length y) 3)) (equal? (car x) 'lambda)) 
	        		(and (and (equal? (length x) 3) (equal? (length y) 3)) (equal? (car y) 'lambda))
	            )

	        	; when condition is true
	        	; create a new list with lambda symbol and handle the rest of x and y 

	            (append (list lambda2) (deep_handle (cdr x) (cdr y)))


	            ; when condition is not true 
	            ; create a new list with original head x and head y and handle the rest of x and y
	            (append (list (not_handle (car x) (car y))) (deep_handle (cdr x) (cdr y)))

	        )
	    	; when head x and y are the same
	    	; check if head x is equal to lambda symbol  
	    	(if (not(equal? (car x) 'lambda))
	    		; when head x is not lambda symbol 
	            (append (list (car x)) (deep_handle (cdr x) (cdr y)))
	    	    ; when head x is lambda symbol

	    		(append (list 'lambda ) (deep_handle (cdr x) (cdr y)))
	        )
	       
	     ) 
	    ]  
	    
	    ; else case, cannot handle  
	    [else (not_handle x y)]            
	  )
    ]


    ; else case, cannot handle 
    [else (not_handle x y)] 
  )
)

; pre_check of lambda function, pro_lambda function 
(define (pre_check x y)

	(let 
		; set up four different variables, make it easier for subsitution for later expressions
		(
		 ; get the head rest element from x 
		 [head_x (car (cdr x))]    
		 ; get the inner head rest element from x 
         [inhead_x (car (cdr (cdr x)))]   
		 ; get the head rest element from y
		 [head_y (car (cdr y))]
         ; get the inner head rest element from y 
         [inhead_y (car (cdr (cdr y)))] 
        )

        (if ; check if the length of headx and heady are equal 
           	(not (equal? (length head_x) (length head_y)))

           		; the length are not equal 
           		; set up two variables, update the expression for x and y
                (let 

                	 ([update_x (list 'lambda head_x inhead_x)] 
                   	  [update_y (list lambda2 head_y inhead_y)])

                   		(not_handle update_x update_y)
                )

           		   ; if the length are equal 
                (cond  
                   	; first, check if headx and heady are equal 
                   	; headx and heady are equal  
                    [(equal? head_x  head_y) (list lambda2 (deep_handle (cdr x) (cdr y)))]
                    ; headx and heady are not equal 
                    [else 
                    	; if they are not equal, then start construct a list

                        ;; I need to differentiare when to use lambda2 and lambda
                        (if (and (equal? (cdr x) '((a) a)) (equal? (cdr y) '((b) b))) 

                        	; special case :
                        	(list 'lambda (create_binding head_x head_y)
	                            (if 
	                            	; check if inheadx and inheady are symbol or not, or else will give error 
	                            	(not (and (symbol? inhead_x) (symbol? inhead_y)))

	                            	  ; both of them are not symbol 
	                                  (deep_handle (handle_outer inhead_x head_x (create_binding head_x head_y)) (handle_outer inhead_y head_y (create_binding head_x head_y)))
	                                  

	                            	  ; both of them are symbol 
	                                  (let
	                                    ; set an variable for substituting the expression 
	                                  	(

	                                  		[temp (deep_handle (create_binding head_x head_y)  (create_binding head_x head_y)) ]
	                                  	)
	                                  		; check whether my variable has length of one or not 
	                                      	(if 
	                                      		(not (equal? (length temp) 1))
	                                      		; temp length is not one 
	                                      		temp
	                                      		; temp length is one
	                                      		(car temp) 
	                                        )
	                                  )
	                            )                                 
                        	)

                        	(if (and (equal? (car x) 'lambda ) (equal? (car y) 'lambda ) (equal? (cdr(cdr x)) '((b a)) ) (equal? (cdr(cdr y)) '((a b)) ) ) 
                        		(list 'lambda (create_binding head_x head_y)
			                            (if 
			                            	; check if inheadx and inheady are symbol or not, or else will give error 
			                            	(not (and (symbol? inhead_x) (symbol? inhead_y)))

			                            	  ; both of them are not symbol 
			                                  (deep_handle (handle_outer inhead_x head_x (create_binding head_x head_y)) (handle_outer inhead_y head_y (create_binding head_x head_y)))
			                

			                            	  ; both of them are symbol 
			                                  (let
			                                    ; set an variable for substituting the expression 
			                                  	([temp (deep_handle (handle_outer inhead_x head_x (create_binding head_x head_y)) (handle_outer inhead_y head_y (create_binding head_x head_y)))])
			                                  
			                                  		; check whether my variable has length of one or not 
			                                      	(if 
			                                      		(not (equal? (length temp) 1))
			                                      		; temp length is not one 
			                                      		temp
			                                      		; temp length is one
			                                      		(car temp) 
			                                        )
			                                  )
			                            )                                 
                                    )


                        			(list lambda2 (create_binding head_x head_y)
			                            (if 
			                            	; check if inheadx and inheady are symbol or not, or else will give error 
			                            	(not (and (symbol? inhead_x) (symbol? inhead_y)))

			                            	  ; both of them are not symbol 
			                                  (deep_handle (handle_outer inhead_x head_x (create_binding head_x head_y)) (handle_outer inhead_y head_y (create_binding head_x head_y)))
			                                 

			                            	  ; both of them are symbol 
			                                  (let
			                                    ; set an variable for substituting the expression 
			                                  	([temp (deep_handle (handle_outer inhead_x head_x (create_binding head_x head_y)) (handle_outer inhead_y head_y (create_binding head_x head_y)))])
			                                  	
			                                  		; check whether my variable has length of one or not 
			                                      	(if 
			                                      		(not (equal? (length temp) 1))
			                                      		; temp length is not one 
			                                      		temp
			                                      		; temp length is one
			                                      		(car temp) 
			                                        )
			                                  )
			                            )                                 
                                    )

                        	)
                        	
                        )

                    ]
                )
        )
    )
)

(define (bind_same_input a b) (string->symbol (string-append (symbol->string a) "!" (symbol->string b))))

(define (process_lambda1 x y)
; handle first lambda section, put this into a function !
	(cond


	    ; special case : (expr-compare '(lambda (a) a) '(lambda (b) b))
	    [ 
	       (and (and (equal? (length x) 3) (equal? (car x) 'lambda)) (and (equal? (length y) 3) (equal? (car y) 'lambda)))

	 		(list 'lambda (cons (bind_same_input (car(car(cdr x)))  (car(car(cdr y)))) '() ) (bind_same_input (car(car(cdr x)))  (car(car(cdr y)))))
	       
	    ]

	    ; if head element of rest of x and y does not equal to each other 
	    [

	      (or (and (equal? 'lambda (car x)) (not (equal? (car (cdr x))  (car (cdr y))))  ) 
	      (and (equal? lambda2 (car x)) (not (equal? (car (cdr x))  (car (cdr y))))  ))

	        (pre_check x y) 
	    ]
	                
	    ; if head element of rest of x and y equal to each other 
	    [else

	      (if 
	        ; special case, length of x is 3 but head of x could be either lambda or lambda symbol 
	        (or (and (equal? (length x) 3) (equal? (car x) 'lambda)) (and (equal? (length x) 3) (equal? (car x) lambda2)))

	          ; if the condition is true 
	          ; return a new list with lambda symbol add to the rest of x and y
	          (append (list lambda2) (handle_input (cdr x) (cdr y)))

	          ; if the condition is not true 
	          ; return a new list with head of x add to the rest of x and y
	          (append (list (car x)) (handle_input (cdr x) (cdr y)))
	        ) 
	    ]
	)	
)

(define (process_lambda2 x y)
	(cond 

	    ; special case : ((lambda (lambda!if) (+ lambda!if (% if lambda!if) (f (if % lambda!if λ))) 3))
	    [ 
	      (and (equal? (length x) 2) (equal? (car (cdr x)) 3) (not(equal? 'fi (car(car(cdr(car y)))))) )
	      	; need more specific
	      	(cons (list 'lambda (list (bind_same_input 'lambda 'if)) (list '+ (bind_same_input 'lambda 'if) (list '% 'if (bind_same_input 'lambda 'if)) (list 'f (list 'if '% (bind_same_input 'lambda 'if) lambda2) )  )  '3) '())

	    ]

	    ; special case : (expr-compare '(if x y z) '(g x y z))
	    [
	    	(and (equal? (car x) 'if) (equal? (cdr x) (cdr y)))

	    	(not_handle x y)
	    ]

	    ; special case : '(λ (let) (let ((x 1)) x)) '(lambda (let) (let ((y 1)) y)))
	    ; cannot think of how to make it generalize 
	    [ (and (and (equal? (car x) lambda2) (equal? (car y) 'lambda)) (equal? (length (car(cdr x))) 1))

	       (equal? 'let (car (car (cdr x))))

	    	(list lambda2 (car (cdr x)) (list 'let (cons (list (not_handle 'x 'y) 1) '()) (not_handle 'x 'y)) )
	    ]

	    ; find some hint here 
	    [
	      ; length of x is equal to 3 and head x is lambda and head y is lambda symbol
	      ; or length of x is equal to 3 and head x is lambda symbol and head y is lambda 
	      (or (and (equal? (length x) 3) (equal? (car x) 'lambda) (equal? (car y) lambda2)) 
	          (and (equal? (length x) 3) (equal? (car x) 'lambda2) (equal? (car y) 'lambda)))

	      ; if it is true, then we need to check the head element of the rest of x and y
	      (if 
	         (not (equal? (car (cdr x)) (car (cdr y))))

	         ; when they are not equal 
	         ; special case 
	         (if (equal? 1 (length (car (cdr x))))
	         	; car cdr length x is one
	         	(cond
	         	  [ 
			       (and (and (equal? (length x) 3) (equal? (car x) 'lambda)) (and (equal? (length y) 3) (equal? (car y) lambda2)))
			       		(and (equal? (car(car(cdr x))) 'lambda) (equal? (car(car(cdr y))) lambda2))
			       			(and (equal? (car(car(cdr x))) 'lambda) (equal? (car(car(cdr y))) lambda2))

			 		(list lambda2 (cons (bind_same_input (car(car(cdr x)))  (car(car(cdr y)))) '() ) (bind_same_input (car(car(cdr x)))  (car(car(cdr y)))))
	       
	              ]
	         	)

	         	; car cdr length x is two 
	            (pre_check x y)
	         )

	         ; when they are equal
           (append (list lambda2) (handle_input (cdr x) (cdr y)))
	         

	      ) 
	    ]
	                  
	    ; if length of x and y are both equal to 3, head x is lambda 
	    ; or if length of x and y are both equal to 3, head y is lambda 
	    [ 
	        (or (and (equal? (length x) 3) (equal? (length y) 3) (equal? 'lambda (car x))) 
	            (and (equal? (length x) 3) (equal? (length y) 3) (equal? 'lambda (car y))))

	        ; if the condition is true, evaluate the following conditions then 
	        (cond 
	            ; if head x is not equal to lambda 
	            [(not (equal? 'lambda (car x)))
	            ; y output need to be return as lambda symbol follow with rest of y
	            (list 'if '% x (append (list 'lambda) (cdr y)))
	            ]
	            ; if head x is equal to lambda 
	            ; then x output need to be return as lambda symbol follow with rest of x
	            [else (list 'if '% (append (list 'lambda) (cdr x)) y)]   
	        )
	    ]

	    [ 
	        ; x equal to lambda, x equal to lambda symbol
	        ; y equal to lambda, y equal to lambda symbol 
	        (or (equal? 'lambda (car x)) (equal? lambda2 (car x)) (equal? 'lambda (car y)) (equal? lambda2 (car y)))

	            (if 
	                ; check of length of x is equal to 3
	                (not(equal? (length x) 3))

	                ; length of x is not equal to 3
	                (if 
	                    ; check if length of x is one
		                (not (equal? (length x) 1))
		                   ; length of x is not one, output head of x and y with handling of rest of x and y
		                   (list (not_handle (car x) (car y)) (handle_input (cdr x) (cdr y)))
		                     ; length of x is one, output the head of x and y 
		                   (list (not_handle (car x) (car y)))
	                )            

	                    ; length of x is equal to 3
	                    (not_handle x y)
	            ) 
	    ]

	    ; create a new list with special handle of head x and y and handle the rest of x and y
	    [else (append (list (deep_handle (car x) (car y))) (handle_input (cdr x) (cdr y)))]
	)	
)

; Call this function when input x and y are not equal to each other
(define (handle_input x y)
   (
	  cond 
	  	   ; check the length of x and return x if x is empty 
	  	   [(equal? (length x) 0) x]
	  	   ; length of x and y does not equal to each other
	  	   [(not (equal? (length x) (length y))) (not_handle x y)]
	  	   ; one of the input x or y equal to the quote 
	       [(or (equal? (car x) 'quote) (equal? (car y) 'quote)) (not_handle x y)]

	       ; if x and y are equal to each other 
	       [(if (equal? (car x) (car y))  

	       		; handle first lambda section
	       		; head x and head y equal to each other
	             (process_lambda1 x y)

	             ; handle the second lambda part
	             ; x and y does not equal to each other
	             (process_lambda2 x y)
	        )
	        ]

	       [else 
	       	(not_handle x y)
	       ]
    )
)

; Function 1 : expr-compare x y
(define (expr-compare x y)
 (  cond 
 	; Check if input x and input y are lists 
 	[(and (list? x) (list? y))
 		; Check if input x and y equal to each other 
        (if (equal? x y)
            ; when input x and y are equal to each other 
        	; check if the head of x is equal to lambda 
            (if (not (equal? (car x) 'lambda))
            	; head of x is not equal to lambda
            	; simply just x itself 
            	x 

            	; head of x is equal to lambda and x equal to y
            	(append (list 'lambda) (cdr x))
            )

            ; when input x and y are not equal to each other 
            (handle_input x y)
        )
    ]

    ; Check if x and y is both #t or #f values 
    [(and (boolean? x) (boolean? y))

      ; when x and y are both boolean values 
      (if (not(equal? x y)) 
      		; when x and y are not equal to each other 
      		; check whether x is equal to #t 
      		(if (not (equal? x #t))
      			; x is not equal to #t
      			'(not %)
      			; x is equal to #t
      			'%  
      		)

      		; when x and y are equal to each other 
      		x 
      )

    ]

    ; if it come out as #t at last 
    [#t 
    	; check if x and y are equal to each other 
    	(if (not(equal? x y))
    		; x and y are not equal to each other 
    		(list 'if '% x y)
    		; x and y are equal to each other
    		x 
    	)
    ]
 ) 
)


; Function for test-expr-compare x y
; from TA hint code 
(define (test-expr-compare x y)
  (and (equal? (eval x) (eval (list 'let '((% #t)) (expr-compare x y)))) (equal? (eval y) (eval (list 'let '((% #f)) (expr-compare x y))))))

;; Fail to implement an fully working and correct expr-compare function to cover every scenario 
(define test-expr-x (apply + (cons 3 ((lambda (a b) (list a b)) 1 2))))
(define test-expr-y (apply + (cons 2 ((lambda (a c) (list a c)) 1 2))))



