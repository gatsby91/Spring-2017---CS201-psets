#lang racket



; Please do not modify lines above this one.

; ****************************************************************
; CS 201 HW #4  DUE 11:59 pm Wednesday, March 1, 2017
; using the submit command on the Zoo.
; ****************************************************************
; Name:
; Email address:
; ****************************************************************
; ** problem 0 ** (1 easy point)
; Please modify the following definition to reflect the number of
; hours you spent on this assignment.  (Must be non-zero to earn credit.)

(define hours 10)

; ****************************************************************
; Please DO NOT use require or mutators (set! and its relatives)
; in your programs.  Otherwise you may use any Racket constructs.

; You may write auxiliary procedure(s) in addition to
; the one(s) specified in the problems.  Please include
; a comment for each one explaining its input and results.

; Topics:
; Racket: deep recursion on a recursively defined data structure.
; Computer Science: Boolean functions, expressions, environments,
; truth tables, logical equivalence, tautology, contradiction,
; contingency, simplification.

; ****************************************************************
; We define a table as a list of entries,
; where each entry is given by the following structure.

(struct entry (key value) #:transparent)

; Recall that a struct defines a constructor, selector(s), and a type predicate.
; In this case, the constructor is entry, the selectors are
; entry-key and entry-value, and the type predicate is entry?.

; Here are two examples of tables.

(define table1
  (list
   (entry "second" 2)
   (entry "first" 1)
   (entry "fifth" 5)))

(define table2
  (list
   (entry 'x 0)
   (entry 'z 1)
   (entry 'y 1)
   (entry 'z 0)))

; ****************************************************************
; ** problem 1 ** (5 points)
; Write a procedure to deal with tables as follows.

; (lookup key table)

; returns #f if no entry in the table has a key equal? to key
; otherwise, returns the value of the first entry whose key is equal? to key.

; Examples
; (lookup "first" table1) => 1
; (lookup "third" table1) => #f
; (lookup 'z table2) => 1
; ****************************************************************

(define (lookup key table)
  (if (empty? table) #f
      (if (equal? (entry-key (first table)) key) (entry-value (first table))
          (lookup key (rest table)) )))

; ****************************************************************
; ** problem 2 ** (5 points)
; Write three procedures to compute Boolean functions as follows.

; (f-not val)
; (f-and lst)
; (f-or lst)

; (f-not val) takes a single Boolean value represented by 0 or 1
; and returns the negation (NOT) of it.

; (f-and lst) takes a list lst of Boolean values represented by 0 or 1
; and returns the conjunction (AND) of them all.  If lst is empty, then
; the value returned should be 1.

; (f-or lst) takes a list lst of Boolean values represented by 0 or 1
; and returns the disjunction (OR) of them all.  If lst is empty, then
; the value returned should be 0.

; Examples
; (f-not 0) => 1
; (f-and '()) => 1
; (f-and '(1 1 0 1)) => 0
; (f-or '()) => 0
; (f-or '(1)) => 1
; ****************************************************************

(define (f-not val)
  (if (= val 1) 0 1))

(define (f-and lst)
  (cond
    ((empty? lst) 1)
    ((equal? (first lst) 1) (f-and (rest lst)))
    (else (equal? (first lst) 0) 0))) 

(define (f-or lst)
  (cond
    ((empty? lst) 0)
    ((equal? (first lst) 0) (f-or (rest lst)))
    (else (equal? (first lst) 1) 1))) 


; ****************************************************************
; Our representation of Boolean expressions will use the following
; struct definitions, for the operations of NOT, AND, OR.

(struct e-not (arg) #:transparent)
(struct e-and (args) #:transparent)
(struct e-or (args) #:transparent)

; These define constructors, selectors, and type predicates as follows
; e-not, e-not-arg, e-not?
; e-and, e-and-args, e-and?
; e-or, e-or-args, e-or?

; A Boolean expression is defined recursively as follows.
; 1) the constants 0 and 1 are Boolean expressions
; 2) any identifier is a Boolean expression, where the variable x
; is represented by the symbol 'x
; 3) if <E> is any Boolean expression, its negation (NOT) is represented
; by (e-not <E>).
; 4) if <E1>, ..., <En> are any Boolean expressions, their conjunction (AND)
; is represented by (e-and (list <E1> ... <En>)).  If the list is empty,
; then the AND expression is equivalent to the constant 1.
; 5) if <E1>, ..., <En> are any Boolean expressions, their disjunction (OR)
; is represented by (e-or (list <E1> ... <En>)).  If the list is empty,
; then the OR expression is equivalent to the constant 0.

; Some examples of Boolean expressions:

; The expression 0'
(define exp1 (e-not 0))

; The expression (x + y)
(define exp2 (e-or (list 'x 'y)))

; The expression (x * y * z)
(define exp3 (e-and (list 'x 'y 'z)))

; The expression (w * (x' + 0 + y)))
(define exp4 (e-and (list 'w (e-or (list (e-not 'x) 0 'y)))))

; The expression (x + x')
(define exp5 (e-or (list 'x (e-not 'x))))

; ****************************************************************
; ** problem 3 ** (10 points)
; Write a procedure

; (boolean-exp? val)

; (boolean-exp? val) takes an arbitrary Racket value val
; and tests to see whether it is a Boolean expression according
; to the definition above, returning #t if so and #f if not.

; The predicate (symbol? arg) will test whether its argument
; is an identifier.

; Hint: deep recursion on the structure of Boolean expressions.

; Examples
; (boolean-exp? 0) => #t
; (boolean-exp? 2) => #f
; (boolean-exp? exp1) => #t
; (boolean-exp? (e-and (list 'x "hi!"))) => #f
; (boolean-exp? (e-and (list 'x 0 'y))) => #t
; (boolean-exp? (e-or (list 'x (e-and (list 'y #t))))) => #f
; ****************************************************************
(define (boolean-exp? val)
  (cond
    ((equal? 1 val) #t)
    
    ((equal? 0 val) #t)
    
    ((symbol? val) #t)
    
    ((list? val) (cond
                   ((empty? val) #t)
                   ((and (boolean-exp? (first val)) (not (empty? (rest val)))) (boolean-exp? (rest val)))
                   (else (boolean-exp? (first val)))))
    
    ((e-not? val) (cond ((equal? (e-not-arg val) 1) #t)
                        ((equal? (e-not-arg val) 0) #t)
                        ((symbol? (e-not-arg val)) #t)
                        ((e-and? (e-not-arg val)) (boolean-exp? (e-not-arg val)))
                        ((e-or? (e-not-arg val)) (boolean-exp? (e-not-arg val)))
                        ((e-not? (e-not-arg val)) (boolean-exp? (e-not-arg val)))
                        (else #f) ))
    
    ((e-and? val) (cond ((empty? (e-and-args val)) #t)
                        ((equal? (first (e-and-args val)) 1) (boolean-exp? (rest (e-and-args val))))
                        ((equal? (first (e-and-args val)) 0) (boolean-exp? (rest (e-and-args val))))
                        ((symbol? (first (e-and-args val))) (boolean-exp? (rest (e-and-args val))))
                        ((and (e-not? (first (e-and-args val))) (boolean-exp? (first (e-and-args val)))) (boolean-exp? (rest (e-and-args val))))
                        ((and (e-or? (first (e-and-args val))) (boolean-exp? (first (e-and-args val)))) (boolean-exp? (rest (e-and-args val))))
                        ((and (e-and? (first (e-and-args val))) (boolean-exp? (first (e-and-args val)))) (boolean-exp? (rest (e-and-args val))))
                        (else #f)))
    
    ((e-or? val) (cond ((empty? (e-or-args val)) #t)
                        ((equal? (first (e-or-args val)) 1) (boolean-exp? (rest (e-or-args val))))
                        ((equal? (first (e-or-args val)) 0) (boolean-exp? (rest (e-or-args val))))
                        ((symbol? (first (e-or-args val))) (boolean-exp? (rest (e-or-args val))))
                        ((and (e-not? (first (e-or-args val))) (boolean-exp? (first (e-or-args val)))) (boolean-exp? (rest (e-or-args val))))
                        ((and (e-or? (first (e-or-args val))) (boolean-exp? (first (e-or-args val)))) (boolean-exp? (rest (e-or-args val))))
                        ((and (e-and? (first (e-or-args val))) (boolean-exp? (first (e-or-args val)))) (boolean-exp? (rest (e-or-args val))))
                        (else #f)))
    (else #f)))

    
                                                             
  
; ****************************************************************
; ** problem 4 ** (10 points)
; Write a procedure

; (all-vars bexp)

; that takes a Boolean expression bexp 
; and makes a list containing all the variables
; that occur in bexp.  The list should not contain duplicates,
; and should have the variables in the order of their
; first appearance in bexp (scanning left to right.)

; Hint: deep recursion on the structure of Boolean expressions.

; Note that there is a Racket procedure: remove-duplicates

; Examples

; (all-vars 0) =>'()
; (all-vars 'x) => '(x)
;(all-vars (e-not (e-and (list 'x (e-or (list 'y (e-not 'z) 'x)))))) => '(x y z)
; (all-vars (e-and (list 1 (e-or (list 0 (e-not 'u)))))) => '(u)
; (all-vars (e-and (list (e-or (list 'x 'y)) (e-or (list (e-not 'y) (e-not 'x)))))) => '(x y)
; (all-vars (e-or (list 'c 'b 'a (e-and (list 'a 'b 'c))))) => '(c b a)
;*************************************************

; given a list of lists, delist said list: '(x y (a b) => '(x y a b)
(define (de-list lst)
  (cond
    ((empty? lst) '())
    ((and (symbol? (first lst)) (not(empty? (rest lst)))) (cons (first lst) (de-list (rest lst))))
    ((and (list? (first lst)) (not(empty? (rest lst)))) (append (de-list (first lst)) (de-list (rest lst))))
    ((symbol? (first lst)) (cons (first lst) '()))
    (else (list? (first lst)) (de-list (first lst)))))

(define (all-vars val)
  (remove-duplicates (de-list(isolate-variables val))))

; give a list of ALL variables from a boolean expression
(define (isolate-variables val)
  (cond
    ((symbol? val) (cons val '()))

    ((list? val) (cond
                   ((empty? val) '())
                   ((and (or (e-and? (first val))
                             (e-or? (first val))
                             (e-not? (first val))
                             (symbol? (first val)))
                         (not (empty? (rest val)))) (cons (isolate-variables (first val)) (isolate-variables (rest val))))
                   ((not (empty? (rest val))) (isolate-variables (rest val)))
                   (else (cons (isolate-variables (first val)) '() ))))      ;(not(equal? (isolate-variables (first val)) #f)) 
                   
                   
    ((e-not? val) (cond
                    ((symbol? (e-not-arg val)) (cons (e-not-arg val) '()))
                    ((e-and? (e-not-arg val)) (isolate-variables (e-not-arg val)))
                    ((e-or? (e-not-arg val)) (isolate-variables (e-not-arg val)))
                    (else (e-not? (e-not-arg val)) (isolate-variables (e-not-arg val)))))
    
    ((e-and? val) (cond  ; do we have to accommodate (e-and (list 'x (list 'x 'y))))?
                    ((empty? (e-and-args val)) (e-and-args val))
                    ((symbol? (first (e-and-args val))) (cons (first (e-and-args val)) (isolate-variables (rest (e-and-args val)))))
                    ((e-and? (first (e-and-args val))) (cons (isolate-variables (first (e-and-args val))) (isolate-variables (rest (e-and-args val)))))  ;(not (equal? (all-vars (first (e-and-args val))) #f))
                    ((e-or? (first (e-and-args val))) (cons (isolate-variables (first (e-and-args val))) (isolate-variables (rest (e-and-args val)))))
                    ((e-not? (first (e-and-args val))) (cons (isolate-variables (first (e-and-args val))) (isolate-variables (rest (e-and-args val)))))
                    (else (isolate-variables (rest (e-and-args val)))) ))
    
    ((e-or? val) (cond
                    ((empty? (e-or-args val)) (e-or-args val))
                    ((symbol? (first (e-or-args val))) (cons (first (e-or-args val)) (isolate-variables (rest (e-or-args val)))))
                    ((e-and? (first (e-or-args val))) (cons (isolate-variables (first (e-or-args val))) (isolate-variables (rest (e-or-args val)))))  ;(not (equal? (all-vars (first (e-and-args val))) #f))
                    ((e-or? (first (e-or-args val))) (cons (isolate-variables (first (e-or-args val))) (isolate-variables (rest (e-or-args val)))))
                    ((e-not? (first (e-or-args val))) (cons (isolate-variables (first (e-or-args val))) (isolate-variables(rest (e-or-args val)))))
                    (else (isolate-variables (rest (e-or-args val)))) ))
    (else '())))
    
    
; ****************************************************************
; We represent an environment as table each entry of which
; has a key that is a Racket symbol and a value that is 0 or 1,
; which specifies the truth value of that variable in the environment.
; For example:

(define environ1
  (list
   (entry 'x 0) (entry 'y 1) (entry 'z 0)))
  
(define environ2
  (list
   (entry 'u 0) (entry 'x 1) (entry 'w 1) (entry 'y 0) (entry 'z 1)))

; ****************************************************************
; ** problem 5 ** (10 points)
; Write a procedure 

; (eval-in-env bexp env)

; that takes a Boolean expression bexp and an environment env
; (represented as described above) and returns 0 or 1 giving 
; the value of the expression in the environment.

; If the Boolean expression contains variables that do not
; occur in the environment, (eval-in-env bexp env) should
; return the string: "variable unspecified in environment".
; (You may want to check for this condition first.)

; Hint: deep recursion on the structure of Boolean expressions.

; Examples

; (eval-in-env 1 environ1) => 1
; (eval-in-env (e-or (list 0 0 0)) '()) => 0
; (eval-in-env 'x environ1) => 0
; (eval-in-env 'x environ2) => 1
; (eval-in-env (e-not 'z) environ1) => 1
; (eval-in-env (e-or (list 'y (e-not 'x))) environ2) => 0
; (eval-in-env (e-and (list (e-or (list 'y 'x)) (e-or (list 'w 'y)))) environ2) => 1
; (eval-in-env exp5 environ1) => 1
; (eval-in-env (e-and (list 'x 'y 'z)) (list (entry 'x 1) (entry 'z 0))) => "variable unspecified in environment"
; ****************************************************************
;
(define (eval-in-env bexp env)
  (let* ((vars-list (all-vars bexp))
         (no-unspec-vars? (check-unspec-vars vars-list env)))
    (if (equal? no-unspec-vars? #t) (base-case bexp env) "variable unspecified in environment")))
   
;take a list and return #f is lookup fail
(define (check-unspec-vars lst env)
   (if (empty? lst) #t
        (if (equal? (lookup (first lst) env) #f) #f (check-unspec-vars (rest lst) env)))) 

;return the final result but does not check for unspecified variables
(define (base-case bexp env)
  (cond
    ((empty? bexp) '())
   
    ((e-or? bexp) ; ;(base-case (e-or (list 'x 'y 'z (e-not 'z))) environ1)
     (cond
       ((list? (e-or-args bexp)) (cond
                                      ((number? (first (e-or-args bexp))) (f-or (cons (first (e-or-args bexp)) (base-case (rest (e-or-args bexp)) env))))
                                      ((symbol? (first (e-or-args bexp)))  (f-or (cons (lookup (first (e-or-args bexp)) env) (base-case (rest (e-or-args bexp)) env))))
                                      (else (or (list? (first (e-or-args bexp)))
                                           (e-not? (first (e-or-args bexp)))
                                           (e-and? (first (e-or-args bexp)))
                                           (e-or? (first (e-or-args bexp)))) (f-or (cons (base-case (first (e-or-args bexp)) env) (base-case (rest (e-or-args bexp)) env))))))
       (else #f)))
    
    ((e-and? bexp)
     (cond
       ((list? (e-and-args bexp)) (cond
                                      ((number? (first (e-and-args bexp))) (f-and (cons (first (e-and-args bexp)) (base-case (rest (e-and-args bexp)) env))))
                                      ((symbol? (first (e-and-args bexp)))  (f-and (cons (lookup (first (e-and-args bexp)) env) (base-case (rest (e-and-args bexp)) env))))
                                      (else (or (list? (first (e-and-args bexp)))
                                           (e-not? (first (e-and-args bexp)))
                                           (e-and? (first (e-and-args bexp)))
                                           (e-or? (first (e-and-args bexp)))) (f-and (cons (base-case (first (e-and-args bexp)) env) (base-case (rest (e-and-args bexp)) env))))))
       (else #f)))
     
    ((e-not? bexp)
     (cond
       ((symbol? (e-not-arg bexp)) (f-not (lookup (e-not-arg bexp) env))) ;(eval-in-env (e-not 'z) environ1) => 1
       ((e-not? (e-not-arg bexp)) (f-not (base-case (e-not-arg bexp) env)))   ;(eval-in-env (e-not (e-not 'z)) environ1) => 0
       ((or (e-or? (e-not-arg bexp)) (e-and? (e-not-arg bexp))) (f-not (base-case (e-not-arg bexp) env)))
       (else (number? (e-not-arg bexp)) (f-not (e-not-arg bexp))) ))

    ((list? bexp)
    (cond
      ((not (empty? (rest bexp))) (cond ;more than 1 element in list
                                    ((symbol? (first bexp)) (cons (lookup (first bexp) env) (base-case (rest bexp) env)))
                                    (else (or (e-or? (first bexp))
                                         (e-not? (first bexp))
                                         (e-and? (first bexp))
                                         (list? (first bexp))) (cons (base-case (first bexp) env) (base-case (rest bexp) env))) ))
      ((empty? (rest bexp)) (cond ; just 1 element in list
                              ((symbol? (first bexp)) (cons (lookup (first bexp) env) '()))
                                    (else (or (e-or? (first bexp))
                                         (e-not? (first bexp))
                                         (e-and? (first bexp))
                                         (list? (first bexp))) (cons (base-case (first bexp) env) '() ))))
      (else #f) ))

    ((number? bexp) bexp)
    (else (symbol? bexp) (lookup bexp env)))) 

; ****************************************************************
; We define a truth table as represented by the following struct

(struct tt (vars rows) #:transparent)

; whose fields contain the following
; (1) a (possibly empty) list of n distinct variables, and
; (2) a table containing an entry for each row of the truth table:
; the key of an entry is a list of n 0's and 1's, and the value is the
; Boolean value (0 or 1) of the function on that row of the table.

; Note that the entries in a truth table should be in increasing order of
; their keys, considered as binary numbers.

; Examples of truth tables for the functions given by
; x', (x * y), (a NAND b), (u XOR v)

(define tt-not (tt '(x)
                   (list
                    (entry '(0) 1)
                    (entry '(1) 0))))

(define tt-and (tt '(x y)
                   (list 
                    (entry '(0 0) 0)
                    (entry '(0 1) 0)
                    (entry '(1 0) 0)
                    (entry '(1 1) 1))))
                    
 (define tt-nand (tt '(a b)
                   (list
                    (entry '(0 0) 1)
                    (entry '(0 1) 1)
                    (entry '(1 0) 1)
                    (entry '(1 1) 0))))
  
(define tt-xor (tt '(u v)
                   (list
                    (entry '(0 0) 0)
                    (entry '(0 1) 1)
                    (entry '(1 0) 1)
                    (entry '(1 1) 0))))

; Here is a truth table for a function of three arguments a, b, c.

(define tt-f1 (tt '(a b c)
                  (list
                   (entry '(0 0 0) 0)
                   (entry '(0 0 1) 0)
                   (entry '(0 1 0) 1)
                   (entry '(0 1 1) 1)
                   (entry '(1 0 0) 0)
                   (entry '(1 0 1) 1)
                   (entry '(1 1 0) 0)
                   (entry '(1 1 1) 1))))

; ****************************************************************
; ** problem 6 ** (10 points)
; Write a procedure 

; (all-keys n)

; that takes a non-negative integer n and creates the list of all 
; lists of n 0's or 1's in the *specific order* required for
; a truth table.  In other words, the lists, interpreted as
; binary numbers, should be in increasing order.

; Hint: if a recursive call gives the correct answer
; for (all-keys 2), what needs to happen to it
; to give the correct answer for (all-keys 3)?
; (Compare bit-strings from lecture and all-subsets from hw #2.)

; Use let or let* to avoid recomputing the recursive call!

; Examples
; (all-keys 0) => '(())
; (all-keys 1) => '((0) (1))
; (all-keys 2) => '((0 0) (0 1) (1 0) (1 1))
; (all-keys 3) => '((0 0 0) (0 0 1) (0 1 0) (0 1 1) (1 0 0) (1 0 1) (1 1 0) (1 1 1))
; ****************************************************************

(define (all-keys-aux lst)
  (append  (map (lambda (lst) (cons 0 lst)) lst) (map (lambda (lst) (cons 1 lst)) lst))) 


(define (all-keys n)
  (if (= n 0) '(())
     (if (= n 1)
      '((0) (1))
       (all-keys-aux (all-keys (- n 1)))))) 

; ****************************************************************
; ** problem 7 ** (10 points)
; Write a procedure

; (truth-table bexp)

; that takes a Boolean expression bexp and returns the truth table for bexp
; where the variables for the table are extracted from bexp using all-vars, 
; and the function value for each row is obtained by evaluating bexp 
; in the corresponding environment.  Notice that all-vars specifies
; the order of variables for the truth table.

; Examples:

;> (truth-table exp1)
;(tt '() (list (entry '() 1)))

;> (truth-table exp2)
;
;(tt '(x y)
; (list (entry '(0 0) 0)
;       (entry '(0 1) 1)
;       (entry '(1 0) 1)
;       (entry '(1 1) 1)))
;

;> (truth-table exp2)
;
;(tt '(x y)
; (list (entry '(0 0) 0)
;       (entry '(0 1) 1)
;       (entry '(1 0) 1)
;       (entry '(1 1) 1)))
;

;>  (truth-table exp4)
;(tt
; '(w x y)
; (list
;  (entry '(0 0 0) 0)
;  (entry '(0 0 1) 0)
;  (entry '(0 1 0) 0)
;  (entry '(0 1 1) 0)
;  (entry '(1 0 0) 1)
;  (entry '(1 0 1) 1)
;  (entry '(1 1 0) 0)
;  (entry '(1 1 1) 1)))
; ****************************************************************


;get the list of unique variables in Boolean expression
;get the length of the list of unique variables
;create the keys using the length

;given ONE set of keys, and a list of variable, create an accroding environment.
; example: (create-env '(x y) '(0 0) => (environ (list (entry 'x 0)  (entry 'y 0))) 
(define (create-env var-lst entry-lst);  entries = all entry-lsts    variable-list = var-lst
  (cond ((empty? var-lst) '())
    ((empty? (rest var-lst)) (list (entry  (first var-lst) (first entry-lst))))
      (else (cons (entry (first var-lst) (first entry-lst)) (create-env (rest var-lst) (rest entry-lst)) ))))
      
;use create-env to evaluate the Boolean value returned for each row
;(eval-in-env bexp (create-env var-lst entry-lst))    ;reminder: (eval-in-env (e-not 'z) environ1) => 1

;given all entries, return a list of according Boolean value
;example: (create-Boolean-value-list '((0 0 0) (0 0 1) (0 1 0) (0 1 1) (1 0 0) (1 0 1) (1 1 0) (1 1 1)) '(w x y) exp4) => '(0 0 0 0 1 1 0 1)
(define (create-Boolean-value-list all-entry-lsts var-lst bexp)
  (cond ((empty? var-lst) '())
    ((empty? (rest all-entry-lsts)) (cons  (eval-in-env bexp (create-env var-lst (first all-entry-lsts))) '()))
      (else (cons  (eval-in-env bexp (create-env var-lst (first all-entry-lsts) )) (create-Boolean-value-list (rest all-entry-lsts) var-lst bexp) )) ))
     

;given all entries and all Boolean values, create the table minus the list of var
;example: (create-rows '((0 0 0) (0 0 1) (0 1 0) (0 1 1) (1 0 0) (1 0 1) (1 1 0) (1 1 1))  '(0 0 0 0 1 1 0 1))
; => ; (list
;  (entry '(0 0 0) 0)
;  (entry '(0 0 1) 0)
;  (entry '(0 1 0) 0)
;  (entry '(0 1 1) 0)
;  (entry '(1 0 0) 1)
;  (entry '(1 0 1) 1)
;  (entry '(1 1 0) 0)
;  (entry '(1 1 1) 1)))

(define (create-rows entries bool-values)
  (if (empty? (rest entries)) (list (entry (first entries) (first bool-values)))
      (cons (entry (first entries) (first bool-values)) (create-rows (rest entries) (rest bool-values)))))

;(struct tt (vars rows) #:transparent)

(define (truth-table bexp)
  (let*
      [(variable-list (all-vars bexp))
       (all-entries (all-keys (length variable-list)))
       (all-bool-values (create-Boolean-value-list all-entries variable-list bexp))]
    (tt variable-list 
    (create-rows all-entries all-bool-values)) ))
       
 

; ****************************************************************
; ** problem 8 ** (10 points)
; Write two procedures

; (classify bexp)
; (equivalent? bexp1 bexp2)

; (classify bexp) takes a Boolean expression and classifies it, returning one of the
; identifiers: 'tautology, 'contradiction, or 'contingent.
; The expression is a tautology if it is true in every environment,
; a contradiction if it is false in every environment, and contingent
; if it is true in some environments and false in some environments.

; (equivalent? bexp1 bexp2) takes two Boolean expressions and returns #t if
; they are logically equivalent, and #f if they are not logically equivalent.
; Two expressions are logically equivalent if, for every environment that
; assigns Boolean values to ALL the variables that appear in either expression,
; the two expressions have the same value.

; For example, the expression 'a is not equivalent to the expression 'b,
; because in the environment (list (entry 'a 0) (entry 'b 1)),
; the first expression takes the value 0, but the second expression takes the value 1.

; These procedures will be tested on expressions with few enough
; variables that generating truth tables WILL BE a feasible approach.

; Examples
; (classify 0) => 'contradiction
; (classify (e-or (list 'x (e-not 'x)))) => 'tautology
; (classify exp2) => 'contingent
; (classify exp3) => 'contingent
; (classify (e-and '())) => 'tautology

; (equivalent? 0 (e-and (list 'a 0))) => #t
; (equivalent? 'a 'b) => #f
; (equivalent? (e-not (e-or (list 'a 'b 'c))) (e-and (list (e-not 'c) (e-not 'b) (e-not 'a)))) => #t
; ****************************************************************

(define (classify bexp)
(cond
 ((number? bexp) (if (= bexp 1) 'tautology 'contradiction))
 ((and (e-and? bexp) (equal? (e-and-args bexp) '())) 'tautology)
 ((and (e-or? bexp) (equal? (e-or-args bexp) '())) 'contradiction)
 (else
  (let*
      [(variable-list (all-vars bexp))
       (all-entries (all-keys (length variable-list)))
       (all-bools (create-Boolean-value-list all-entries variable-list bexp))]
    (cond ((and (member? 0 all-bools) (member? 1 all-bools)) 'contigent)
          ((and (member? 0 all-bools) (not(member? 1 all-bools))) 'contradiction)
          (else 'tautology)))))) ;(else (and (not(member? 0 all-bools)) (member? 1 all-bools)) 'tautology) 
    

(define (member? n lst)
  (cond ((empty? lst) #f)
      (else (if (equal? n (first lst)) #t (member? n (rest lst))))))
      

(define (equivalent? bexp1 bexp2)
  (cond
    ((symbol? bexp1)
     (cond
       ((symbol? bexp2) (if (equal? bexp1 bexp2) #t #f))
       ((number? bexp2) #f)
       (else (or (e-not? bexp2) (e-or? bexp2) (e-and? bexp2))
       (cond
         ((> (length (all-vars bexp2)) 1) #f)
         (else
          (let*
              [(variable-list (all-vars bexp2))
               (all-entries (all-keys (length variable-list)))
               (all-bools (create-Boolean-value-list all-entries variable-list bexp2))]
            (if (or (equal? all-bools '(0 1))  (equal? all-bools '(1 0))) #t #f)))))
         ))
       
     ((number? bexp1)
      (cond
        ((number? bexp2) (if (equal? bexp1 bexp2) #t #f ))
        (else (or (e-not? bexp2) (e-or? bexp2) (e-and? bexp2)) (if (and
                                                                    (or (equal? (classify bexp2) 'contradiction) (equal? (classify bexp2) 'tautology))
                                                                    (equal? (classify bexp1) (classify bexp2))) #t #f))))
     (else (let*
              [(variable-list1 (all-vars bexp1))
               (all-entries1 (all-keys (length variable-list1)))
               (all-bools1 (create-Boolean-value-list all-entries1 variable-list1 bexp1))
               (variable-list2 (all-vars bexp2))
               (all-entries2 (all-keys (length variable-list2)))
               (all-bools2 (create-Boolean-value-list all-entries2 variable-list2 bexp2))]
               
             (if  (equal? all-bools1 all-bools2) #t #f))) ))
        

; ****************************************************************
; ** problem 9 ** (20 points)
; Write a procedure

; (find-exp tt)

; This procedure takes a truth table
; and returns a Boolean expression 
; for the given truth table.

; You may choose to use the sum-of-products algorithm
; from lecture, or some other method, but it must
; return a Boolean expression with the given truth table.

; Examples
; (find-exp tt-and) => (e-or (list (e-and '(x y))))

; (find-exp tt-nand) => (e-or
;                        (list
;                         (e-and (list (e-not 'a) (e-not 'b)))
;                         (e-and (list (e-not 'a) 'b))
;                         (e-and (list 'a (e-not 'b)))))

; (find-exp tt-xor) =>(e-or (list (e-and (list (e-not 'u) 'v)) (e-and (list 'u (e-not 'v))))

; (find-exp tt-f1) => (e-or
;                      (list
;                       (e-and (list (e-not 'a) 'b (e-not 'c)))
;                       (e-and (list (e-not 'a) 'b 'c))
;                       (e-and (list 'a (e-not 'b) 'c))
;                       (e-and '(a b c))))

; ****************************************************************

;> (entry-key (first (rest (tt-rows (tt '(x y) (list (entry '(0 0) 0) (entry '(0 1) 1) (entry '(1 0) 1) (entry '(1 1) 0)))))))
;'(0 1)
;> (entry-value (first (rest (tt-rows (tt '(x y) (list (entry '(0 0) 0) (entry '(0 1) 1) (entry '(1 0) 1) (entry '(1 1) 0)))))))
;1
;> 
;
;(struct tt (vars rows) #:transparent)


(define (find-exp tt)
  (e-or (product-lists (tt-vars tt) (tt-rows tt))))
       

(define (product-list var-list entry-keys) ; product: x * y'.  after knowing the boolean value for this entry is 1
                                           ; example: (product-list '(x y) '(1 1 )) => '(x y)
  (cond
    ((empty? (rest entry-keys)) (if (equal? (first entry-keys) 1) (list (first var-list)) (list (e-not (first var-list))))) ; > concern: (product-list '(x y) '(1 1 )) => '(x y)
    ((equal? 1 (first entry-keys)) (cons (first var-list) (product-list (rest var-list) (rest entry-keys))))
    (else (equal? 0 (first entry-keys)) (cons (e-not (first var-list)) (product-list (rest var-list) (rest entry-keys)))))
    )

;iterate thru a list of entries.  when an entry with value 1, perform product-list and add (e-and:
;example: (product-lists '(x y) (list (entry '(0 0) 0) (entry '(0 1) 1) (entry '(1 0) 1) (entry '(1 1) 0)))

(define (product-lists var-list entry-list)
  (cond
    ((empty? (rest entry-list)) (if (equal? (entry-value (first entry-list)) 1) (list (e-and (product-list var-list (entry-key (first entry-list))))) (list)))
    ((equal? (entry-value (first entry-list)) 1) (cons (e-and (product-list var-list (entry-key (first entry-list)))) (product-lists var-list (rest entry-list) )))
    (else (product-lists var-list (rest entry-list)))))

    
             

  


; ****************************************************************
; ** problem 10 ** (9 points)
; Write a procedure

; (simplify bexp)

; that takes a Boolean expression bexp and returns
; an equivalent Boolean expression that is
; simplified as much as possible using the following rules:

; x + 0 -> x
; 0 + x -> x
; x + 1 -> 1
; 1 + x -> 1
; x * 0 -> 0
; 0 * x -> 0
; x * 1 -> x
; 1 * x -> x
; 0' -> 1
; 1' -> 0
; (x')' -> x

; Also note that
; (e-or '()) should be simplified to 0
; (e-and '()) should be simplified to 1
; (e-or (list bexp)) should be simplified to bexp
; (e-and (list bexp)) should be simplified to bexp
;that is to say:
;> (truth-table (e-and (list (e-or (list 'x 'y)))))
;(tt '(x y) (list (entry '(0 0) 0) (entry '(0 1) 1) (entry '(1 0) 1) (entry '(1 1) 1)))
;> (truth-table  (e-or (list 'x 'y)))
;(tt '(x y) (list (entry '(0 0) 0) (entry '(0 1) 1) (entry '(1 0) 1) (entry '(1 1) 1)))
;> 


; Examples
; (simplify 0) => 0
; (simplify 'x) => 'x
; (simplify (e-not (e-not 'x))) => 'x
; (simplify (e-not 'y)) => (e-not 'y)
; (simplify (e-or (list 0 (e-and (list 1 (e-not 0) 1))))) => 1
; (simplify (e-and (list 'x 1))) => 'x
; (simplify (e-or (list 0 'z 0))) => 'z
; (simplify (e-or (list (e-and (list 'x 1)) (e-or (list (e-not 1) 'z))))) => (e-or '(x z))

;;(simplify (e-and (list (e-or (list 'x 1)) (e-not 'x) (e-or (list (e-not 1) 'z)))))
; ****************************************************************

(define (remove-all n ls)
  (cond
    ((empty? ls) '())
    ((equal? (first ls) n) (remove-all n (rest ls)))
    (else (cons (first ls)  (remove-all n (rest ls))))))

(define (simplify bexp)
  (cond
    ((empty? bexp) '())

    ((symbol? bexp) bexp)

    ((number? bexp) bexp)  
    
    ;e-not cases
    ((e-not? bexp) (cond ((number? (e-not-arg bexp)) (eval-in-env bexp '()))
     (else (if (equivalent? bexp (e-not (e-not 'x))) (first (all-vars bexp)) bexp))))

    ;e-and cases
    ((e-and? bexp) (cond
                     ((empty? (e-and-args bexp)) 1) ;(e-and '())
                     
                     ((list? (e-and-args bexp)) (cond
                                                  ((= 1 (length (e-and-args bexp))) (simplify (first (e-and-args bexp))))  ;(e-and (list bexp)) should be simplified to bexp
                                                  ((member? 0 (e-and-args bexp)) (simplify 0))
                                                  ((member? 1 (e-and-args bexp)) (simplify (e-and (remove-all 1 (e-and-args bexp))))) ;(simplify (e-and (list 'x 1))) => 'x
                                                  ((equal? (simplify (first (e-and-args bexp))) (first (e-and-args bexp))) (e-and (cons (first (e-and-args bexp)) (simplify (rest (e-and-args bexp)))))) 
                                                  (else (simplify (e-and (cons (simplify (first (e-and-args bexp))) (simplify (rest (e-and-args bexp))))))))))); (e-and (list 'x 'y)) or
                                                  ;(e-and (list (e-and (list 'x 'y)) 'z))))
     ; e- or cases
    ((e-or? bexp) (cond
                     ((empty? (e-or-args bexp)) 0) ;(e-or '())
                     
                     ((list? (e-or-args bexp)) (cond
                                                  ((= 1 (length (e-or-args bexp))) (simplify (first (e-or-args bexp))))  ;(e-or (list bexp)) should be simplified to bexp
                                                  ((member? 1 (e-or-args bexp)) (simplify 1))
                                                  ((member? 0 (e-or-args bexp)) (simplify (e-or (remove-all 0 (e-or-args bexp))))) ;(simplify (e-or (list 'x 1))) => 'x
                                                  ((equal? (simplify (first (e-or-args bexp))) (first (e-or-args bexp))) (e-or (cons (first (e-or-args bexp)) (simplify (rest (e-or-args bexp)))))) 
                                                  
                                                  (else (simplify (e-or (list (simplify (first (e-or-args bexp))) (simplify (rest (e-or-args bexp))))))))))); (e-or (list 'x 'y)) or
    ;list                                            ;(e-or (list (e-or (list 'x 'y)) 'z))))
    (else (list? bexp) (cond
                   ((empty? (rest bexp)) (cond
                                           ((symbol? (first bexp)) (list (first bexp)))
                                           (else (list (simplify (first bexp))))))
                   (else (cond
                           ((symbol? (first bexp)) (cons (first bexp) (simplify (rest bexp))))
                           (else (cons (simplify (first bexp)) (simplify (rest bexp))))))))))


;(simplify (e-or (list (e-and (list 'x 1)) (e-or (list (e-not 1) 'z)))))
;(simplify (e-or (cons (simplify (first (e-or-args bexp))) (simplify (rest (e-or-args bexp))))))
;(simplify (e-or (cons (simplify (e-and '(x 1))) (simplify (list (e-or (list (e-not 1) 'z)))))))
;
;(simplify (e-and '(x 1))) => 'x   (simplify (list (e-or (list (e-not 1) 'z)))) => '(z)

;(simplify (e-or '(x z)))
;(simplify (e-and (list (e-or (list 'x 1)) (e-not 'x) (e-or (list (e-not 1) 'z)))))

;(simplify (e-and (list (e-or (list 'x 1)) (e-not 'x) (e-or (list (e-not 1) 'z)))))
                                                  
                                                  
                                                  
                                                  
                                                  
                     

; **************** end of hw #4 *********************************