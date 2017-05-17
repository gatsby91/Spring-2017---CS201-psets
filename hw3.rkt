#lang racket
(provide hours
	 ins ins-cst ins-csy ins-nst ins-nsy ins-hdir
         tm-sub1
         i-lookup
         conf conf-cst conf-ltape conf-csy conf-rtape
         halted? change-state write-symbol
	 normalize
         shift-head-left shift-head-right
         next-config
         tm-xx
         make-tm-stepper
         halts?)

; Please do not modify the lines above this comment.
; ****************************************************************
; CS 201a HW #3  DUE by Wednesday, February 15, 11:59 pm
; using the submit command on the Zoo.
; ****************************************************************
; Name:
; Email address:
; ****************************************************************

; Unless the problem specifies otherwise:
; ** You may solve the problem using any method and any Racket constructs 
; (EXCEPT mutators, that is, set! and its relatives.)
; ** You may write auxiliary procedure(s) in addition to the one(s) 
; specified in the problem.  (Please include a comment for each one 
; specifying what it does and giving one or more examples of it.)
; ** Please make your code as clear and readable as possible.

; The topics of this assignment are:
; a simulator for Turing machines and writing Turing machine programs.

; ****************************************************************
; ** problem 0 ** (1 easy point)
; Modify the following definition to reflect 
; the number of hours you spent on this assignment.
; (Any nonzero number is OK, but an accurate estimate
; helps calibrate the workload.)

(define hours 20)

; ****************************************************************
; Turing machines were described in the lectures; 
; see also the lecture notes on the course web page.

; Here is a top-level procedure to simulate a Turing machine 
; starting from a given configuration until either it halts 
; or it has executed n steps, whichever is first.
; The procedure returns the list of the successive configurations 
; of the computation, starting with the initial one.
; The length of the list of configurations is one more than 
; the number of steps taken by the machine.

(define (simulate tm config n) 
  (cond
    ((<= n 0) (list config))
    ((halted? tm config) (list config))
    (else
     (cons config
           (simulate 
            tm (next-config tm config) (- n 1))))))

; tm is a representation of a Turing machine
; config is a representation of a configuration of the machine
; n is the maximum number of steps to simulate

; The procedures halted? and next-config will be
; written by you in the problems below; you will then
; have a complete Turing machine simulator.

; ****************************************************************
; Turing machine representation.

; A Turing machine is represented as a list of instructions, 
; where each instruction is a 5-tuple, represented as a struct
; defined as follows:

(struct ins (cst csy nst nsy hdir) #:transparent)


; The fields represent the following components of an instruction:
; cst is the current state in the instruction
; csy is the current symbol in the instruction 
; nst is the new state in the instruction
; nsy is the new symbol in the instruction 
; dir is the move direction for the head

; The entries for fields cst and nst are Racket symbols,
; the entries for fields csy and nsy are Racket symbols or nonnegative integers,
; and the entry for hdir must be either the symbol 'L or the symbol 'R
; representing a move to the left or right, respectively.

; Example
; > (define i1 (ins 'q1 0 'q3 1 'L))
; creates an instruction with
; current state 'q1, current symbol 0,
; new state 'q3, new symbol 1,
; and move direction 'L,
; and names it i1.

; Because we've made ins "transparent",
; when the value of i1 is printed, its field values
; are printed.
; > i1
; (ins 'q1 0 'q3 1 'L)

; We can access the components of i1 via the structure selectors
; below (which are automatically defined by the evaluation of struct.)

; (ins-cst i1) => 'q1
; (ins-csy i1) => 0
; (ins-nst i1) => 'q3
; (ins-nsy i1) => 1
; (ins-hdir i1) => 'L

; Example (from lecture):
; We define a Turing machine that when started in state 'q1
; on the leftmost of a string of 0's and 1's,
; changes all the 0's to 1's and all the 1's to 0's 
; and then returns the head to the leftmost symbol and halts.

; Note that the blank symbol is 'b.

(define tm1 
  (list
   (ins 'q1 0 'q1 1 'R)
   (ins 'q1 1 'q1 0 'R)
   (ins 'q1 'b 'q2 'b 'L)
   (ins 'q2 0 'q2 0 'L)
   (ins 'q2 1 'q2 1 'L)
   (ins 'q2 'b 'q3 'b 'R)))
 

; In state q1, the machine moves to the right, changing 0 to 1 and
; 1 to 0, until the first blank.  It then changes to state q2
; and moves left, keeping 0 as 0 and keeping 1 as 1, until the
; first blank.  It then moves one square right and halts in q3,
; because no instructions are defined with current state q3.

; ****************************************************************
; ** problem 1 (15 points)
; Define (in the format just given) a Turing machine named

; tm-sub1

; that takes a non-empty input string of 0's and 1's representing a 
; positive integer (greater than zero) in binary (base 2) notation, 
; and produces an output string that is the binary representation
; of the input number minus 1.

; Note that we guarantee that no input number will begin with 0,
; and require that the only output number that begins with 0
; is a single 0.

; When the machine halts, the head should be scanning the leftmost symbol 
; of the output.

; Your machine *may* use additional tape symbols 
; but the output should contain no
; symbols other than 0, 1 and blank.
; When the machine halts, all the symbols
; except the output should be blank.

; Examples of the behavior of tm-sub1
; input     =>  output
; 1         =>  0
; 10        =>  1
; 1100      =>  1011
; 10000     =>  1111
; 101100    =>  101011 

; The initial state of your machine should be 'q1 -- other states 
; may be named with Racket symbols of your choice.

; You'll be able to run your Turing machine once you get
; your simulator (in the other problems) working.  The behavior
; of your Turing machine will be tested using our simulator.

; ****************************************************************

(define tm-sub1 
  (list
   (ins 'q1 1 'q1 1 'R)
   (ins 'q1 0 'q1 0 'R)
   (ins 'q1 'b 'q2 'b 'L)
   
   ;if the last digit is 1
   (ins 'q2 1 'q3 0 'L)
   (ins 'q3 0 'q3 0 'L)
   (ins 'q3 1 'q3 1 'L)
   (ins 'q3 'b 'q5 'b 'R) 

   ;if the last digit is 0
   (ins 'q2 0 'q4 1 'L)
   
   ;(simulate tm-sub1 (conf 'q1 '() 1 '(1 1 0)) 100)
      ;sub case: if the last digit is 1 and the number is 1, then halt
      ;(ins 'q4 'b 'q8 'b 'R)
      ;(ins 'q8 1 'q9 0 'L)
      ;(ins 'q9 'b 'q9 'b 'R)

   (ins 'q4 0 'q4 1 'L)
   (ins 'q4 1 'q6 0 'L)
   (ins 'q6 0 'q6 0 'L)
   (ins 'q6 1 'q6 1 'L)
   (ins 'q6 'b 'q7 'b 'R)
   (ins 'q7 0 'q7 'b 'R)
   (ins 'q7 1 'q8 1 'R) 
   ))

(define tm-xx 
  (list

   ;read till reaching a blank
   (ins 'q1 1 'q1 1 'R)
   (ins 'q1 0 'q1 0 'R)
   ; write x at blank, then go back
   (ins 'q1 'x 'q1 'x 'R) 
   (ins 'q1 'b 'q2 'x 'L)
   (ins 'q2 'x 'q2 'x 'L)


   ; subtraction cases:
   
   ;if the last digit is 1
   (ins 'q2 1 'q3 0 'L)
   (ins 'q3 0 'q3 0 'L)
   (ins 'q3 1 'q3 1 'L)
   (ins 'q3 'b 'q1 'b 'R) ;restart the cycle of subtracting for when last digit is 1; use 'q5 instead of 'q1 otherwise
   

   ;if the last digit is 0
   (ins 'q2 0 'q4 1 'L)
   
      ;sub case: if the last digit is 1 and the number is 1, then halt
      (ins 'q4 'b 'q8 'b 'R)
      (ins 'q8 1 'q9 'b 'L)
      (ins 'q9 'b 'q9 'b 'R)
      (ins 'q9 'x 'q10 'b 'R)

   (ins 'q4 0 'q4 1 'L)
   (ins 'q4 1 'q6 0 'L)
   (ins 'q6 0 'q6 0 'L)
   (ins 'q6 1 'q6 1 'L)
   (ins 'q6 'b 'q7 'b 'R)
   (ins 'q7 0 'q7 'b 'R)
   (ins 'q7 1 'q1 1 'R) ;restart the cycle of subtracting for when last digit is 0.  use q11 instead of q1 otherwise
   ))



(define tm-copy
  (list
(ins 'q1 0 'q1 0 'R) ;move right over 0’s
(ins 'q1 1 'q1 1 'R) ;move right over 1’s
(ins 'q1 'b 'q2 'c 'L) ;change first blank to c, move left to q2
(ins 'q2 0 'q2 0 'L) ;move left over 0’s
(ins 'q2 1 'q2 1 'L) ;move left over 1’s
(ins 'q2 'b 'q3 'b 'R) ;at first blank, move right to q3
(ins 'q3 0 'q4 'd 'R) ;change 0 to d, move right to q4
(ins 'q3 1 'q5 'e 'R) ;change 1 to e, move right to q5
(ins 'q3 'c 'q7 'c 'L) ;c means copying is over, move left to q7
(ins 'q4 0 'q4 0 'R) ;move right over 0’s
(ins 'q4 1 'q4 1 'R) ;move right over 1’s
(ins 'q4 'c 'q4 'c 'R) ;move right over c
(ins 'q4 'b 'q6 0 'L) ;replace first blank with 0, move left to q6
(ins 'q5 0 'q5 0 'R) ;move right over 0’s
(ins 'q5 1 'q5 1 'R) ;move right over 1’s
(ins 'q5 'c 'q5 'c 'R) ;move right over c
(ins 'q5 'b 'q6 1 'L) ;replace first blank with 1, move left to q6
(ins 'q6 0 'q6 0 'L) ;move left over 0’s
(ins 'q6 1 'q6 1 'L) ;move left over 1’s
(ins 'q6 'c 'q6 'c 'L) ;move left over c
(ins 'q6 'd 'q3 0 'R) ;change d back into 0, move right to q3
(ins 'q6 'e 'q3 1 'R) ;change e back into 1, move right to q3
(ins 'q7 0 'q7 0 'L) ;move left over 0’s
(ins 'q7 1 'q7 1 'L) ;move left over 1’s
(ins 'q7 'b 'q8 'b 'R) ;at first blank, move right and halt (q8)
))
   
        

; ****************************************************************
; ** problem 2 (10 points)
; Write the following procedure.
; Remember to use the instruction selectors:
; ins-cst, ins-csy, ins-nst, ins-nsy, ins-hdir

; (i-lookup st sy tm)

; If there is an instruction in the Turing machine tm
; that has ins-cst equal to the state st and ins-sy
; equal to the symbol sy, that instruction is returned
; as the value of (i-lookup st sy tm).  If there is
; no such instruction, the value returned is #f.

; You may assume that *at most one* instruction will match,
; that is, the machine tm is deterministic.

; Examples
; (equal? (i-lookup 'q1 1 tm1) (ins 'q1 1 'q1 0 'R)) => #t
; (equal? (i-lookup 'q2 'b tm1) (ins 'q2 'b 'q3 'b 'R)) => #t
; (i-lookup 'q3 1 tm1) => #f
; ****************************************************************

(define (i-lookup st sy tm)
  (if (not (empty? tm))
      (if (and (equal? (ins-cst (first tm)) st) (equal? (ins-csy (first tm)) sy )) (first tm)
          (i-lookup st sy (rest tm)))
    #f))

;(i-lookup 'q1 1 tm1)



; ****************************************************************
; Representation of a Turing machine configuration.
; We represent a Turing machine configuration using the following structure:

(struct conf (cst ltape csy rtape) #:transparent)

; where the fields are as follows
; cst is the current state of the machine,
; ltape is a list of the symbols on the tape to the left of the head
; csy is the current symbol on the tape (located at the read/write head)
; rtape is a list of the symbols on the tape to the right of the head

; We reserve the symbol 'b for the blank.

; For example, we define the following two configurations:

(define config1 (conf 'q3 '(0 0) 1 '(1)))
(define config2 (conf 'q6 '(1 b) 0 '(b b)))

; Note that the selectors are
; conf-cst, conf-ltape, conf-csy, conf-rtape

; config1 represents the Turing machine configuration

;   --------------------------
;   .. | 0 | 0 | 1 | 1 |  | ..
;   --------------------------
;                ^
;                q3

; in which the non-blank symbols on the tape are 0011,
; and the machine is in state q3 with the read/write head
; scanning the leftmost 1.

; config2 represents the Turing machine configuration

;   ------------------------------
;   .. |   | 1 |  | 0 |   |   | ..
;   ------------------------------
;                   ^
;                   q6

; in which the symbols 1, blank, 0, are on the tape, surrounded
; by blanks, and the machine is in state q6 with the read/write
; head scanning the 0.

; A configuration is *normalized* if the following two conditions hold.
; (1) either ltape is the empty list or its leftmost symbol is not 'b, and
; (2) either rtape is the empty list or its rightmost symbol is not 'b.

; Of the two configurations above, config1 is NORMALIZED, 
; but config2 is not (because its rtape list is not empty and ends with 'b).

; Note that tape squares that are not explicitly represented are
; assumed to contain blanks.  A normalized configuration
; to represent the machine in state q1 with all tape squares
; blank is thus (conf 'q1 '() 'b '()), where the current symbol
; is 'b, and the ltape and rtape lists are both empty.

; ****************************************************************
; ** problem 3 ** (10 points)
; Write one procedure

; (normalize config)

; takes a Turing machine configuration config and returns an equivalent 
; *normalized* configuration. That is, the same Turing machine configuration 
; is represented by the input configuration and the output configuration, 
; and the output configuration is normalized, as defined above.

(struct conf (cst ltape csy rtape) #:transparent)
(define config1 (conf 'q3 '(0 0) 1 '(1)))

; Examples
; (normalize config1) => (conf 'q3 '(0 0) 1 '(1))
; (normalize config2) => (conf 'q6 '(1 b) 0 '()))
; (normalize (conf 'q3 '(b 0) 'b '(1 1 0 b b))) => (conf 'q3 '(0) 'b '(1 1 0))
; (normalize (conf 'q6 '(b b 0 b) 1 '(b 0 b b))) => (conf 'q6 '(0 b) 1 '(b 0))
; (normalize (conf 'q4 '(b b b) 'b '(b b b))) => (conf 'q4 '() 'b '())
; ****************************************************************

(define (normalize config)
  (let ((new-config (struct-copy conf config [ltape (new-ltape (conf-ltape config))]
                           [rtape (new-rtape (conf-rtape config))])))
    (if (not(equal? new-config #f)) new-config #f)))
  
(define (new-ltape ls)
  (cond
    ((empty? ls) ls)
    ((if (equal? (first ls) 'b) (new-ltape (rest ls)) ls ))))

(define (new-rtape ls)
  (reverse (new-ltape (reverse ls)))) ; alternatives?
        
; Note that the selectors are
; conf-cst, conf-ltape, conf-csy, conf-rtape

     
; ****************************************************************
; ** problem 4 (9 points)
; Write the following three procedures.

; (halted? tm config)
; is given a normalized configuration config and
; returns #t if the Turing machine tm is halted 
; in machine configuration config, otherwise returns #f.
; (The Turing machine tm is halted if it has no instruction
; matching the current state and current symbol in the configuration config.)

; (change-state nst config)
; takes a normalized configuration config and returns 
; a normalized configuration
; in which the state of the machine is changed to nst.

; (write-symbol nsy config) 
; takes a normalized configuration config and
; returns a normalized configuration in which the symbol scanned by 
; the read/write head has been replaced by nsy.


; Examples
; (halted? tm1 (conf 'q1 '(0 1 1) 'b '())) => #f
; (halted? (list (ins 'q1 'b 'q2 'b 'R)) (conf 'q2 '() 'b '())) => #t
; (change-state 'q2 (conf 'q1 '(0) 1 '())) => (conf 'q2 '(0) 1 '())
; (change-state 'q13 (conf 'q4 '(0 1 1) 'b '())) => (conf 'q13 '(0 1 1) 'b '())
; (write-symbol 1 (conf 'q5 '(0) 0 '(1 1))) => (conf 'q5 '(0) 1 '(1 1))
; (write-symbol 'b (conf 'q3 '(1) 1 '())) => (conf 'q3 '(1) 'b '())
; ****************************************************************

;take config state and symbol and see if there is a matching pair in tm.  lookup is false = halted
 (define (halted? tm config)
  (if (equal? (i-lookup (conf-cst config) (conf-csy config) tm) #f) #t #f))

(define (change-state nst config)
  (let ((new-config (struct-copy conf config [cst nst])))
    (if (not(equal? new-config #f)) new-config #f)))

(define (write-symbol nsy config)
  (let ((new-config (struct-copy conf config [csy nsy])))
    (if (not(equal? new-config #f)) new-config #f)))

; ****************************************************************
; ** problem 5 ** (10 points)
; Write two procedures

; (shift-head-left config)
; takes a normalized configuration config 
; and returns a normalized configuration 
; in which the position of the read/write head 
; has been moved one tape square to the left.

; (shift-head-right config)
; takes a normalized configuration config 
; and returns a normalized configuration 
; in which the position of the read/write head 
; has been moved one tape square to the right.

; Examples
; (shift-head-left (conf 'q5 '() 'b '())) => (conf 'q5 '() 'b '())
; (shift-head-left (conf 'q6 '(0 0) 1 '(1 1))) => (conf 'q6 '(0) 0 '(1 1 1))
; (shift-head-left (conf 'q7 '() 0 '(1 1 0))) => (conf 'q7 '() 'b '(0 1 1 0))
; (shift-head-right (conf 'q2 '() 'b '())) => (conf 'q2 '() 'b '())
; (shift-head-right (conf 'q9 '() 0 '(1 1 1))) => (conf 'q9 '(0) 1 '(1 1))
; (shift-head-right (conf 'q8 '(1 0 1 1) 'b '())) => (conf 'q8 '(1 0 1 1 b) 'b '())
; ****************************************************************

;helper function for helper function to return new ltape for shift-head-left
(define (remove-last lst)
    (if (null? (rest lst))
        '()
        (cons (first lst) (remove-last (rest lst)))))

;helper function to create new csy, ltape, and rtape for a config that gets returned by shift-head-left

(define (shift-ltape-left current-ltape current-csy)
  (cond
    ((empty? current-ltape) current-ltape)
    (else (remove-last current-ltape))))

(define (shift-csy-left current-csy current-ltape)
  (cond
    ((empty? current-ltape) 'b)
    (else (last current-ltape)) ))

(define (shift-rtape-left current-csy current-rtape)
  (cond
  ((and (equal? current-csy 'b) (empty? current-rtape)) current-rtape)
  (else (cons current-csy current-rtape))))

(define (shift-head-left config)
  (let ((new-config (struct-copy conf config [ltape (shift-ltape-left  (conf-ltape config) (conf-csy config))]
                                             [csy (shift-csy-left (conf-csy config) (conf-ltape config))]
                                             [rtape (shift-rtape-left (conf-csy config) (conf-rtape config))])))
    (if (not(equal? new-config #f)) new-config #f)))

;helper function to create new csy, ltape, and rtape for a config that gets returned by shift-head-RIGHT
(define (shift-rtape-right current-csy current-rtape)
  (cond
    ((empty? current-rtape) current-rtape)
    (else (remove (first current-rtape) current-rtape))))

(define (shift-csy-right current-csy current-rtape)
  (cond
    ((empty? current-rtape) 'b)
    (else (first current-rtape)) ))

(define (shift-ltape-right current-csy current-ltape)
  (cond
  ((and (equal? current-csy 'b) (empty? current-ltape)) current-ltape)
  ((empty? current-ltape) (list current-csy))
  (else (reverse (cons current-csy (reverse current-ltape))))))


(define (shift-head-right config)
  (let ((new-config (struct-copy conf config [rtape (shift-rtape-right (conf-csy config) (conf-rtape config))]
                                             [csy (shift-csy-right (conf-csy config) (conf-rtape config))]
                                             [ltape (shift-ltape-right (conf-csy config) (conf-ltape config))])))
    (if (not(equal? new-config #f)) new-config #f)))


    
  
; ****************************************************************
; ** problem 6 ** (15 points)
; Write a procedure 

; (next-config tm config)
; takes a Turing machine tm and a normalized configuration config
; and returns the normalized NEXT configuration 
; for the Turing machine tm in the configuration config.
; If there is no applicable instruction, the configuration
; returned should be just the input configuration.

; Hint: get your procedures
; halted?, i-lookup, write-symbol, shift-head-left, shift-head-right
; working and combine them appropriately.

; Examples
; (next-config tm1 (conf 'q1 '() 0 '(0 1))) => (conf 'q1 '(1) 0 '(1))
; (next-config tm1 (conf 'q1 '(1) 0 '(1))) => (conf 'q1 '(1 1) 1 '())
; (next-config tm1 (conf 'q1 '(1 1 0) 'b '())) => (conf 'q2 '(1 1) 0 '()))
; (next-config tm1 (conf 'q2 '() 'b '(1 1 0))) => (conf 'q3 '() 1 '(1 0))
; (next-config tm1 (conf 'q3  '() 1 '(1 0))) => (conf 'q3 '() 1 '(1 0))
; ****************************************************************

(define (next-config tm config)
  (if (halted? tm config) config
      
      (let*
          ((instruct  (i-lookup (conf-cst config) (conf-csy config) tm))
          (unshifted-config (struct-copy conf config
                                             [cst (ins-nst instruct)]
                                             [csy (ins-nsy instruct)]))
          (shifted-config (if (equal? (ins-hdir instruct) 'R) (shift-head-right unshifted-config) (shift-head-left unshifted-config))))
          (if (not(equal? shifted-config #f)) shifted-config #f))))
       
; ****************************************************************
; If your procedures are working, then you should
; be able to run the following example, which
; shows the successive normalized configurations 
; of Turing machine tm1 when run from the given configuration.

;> (simulate tm1 (conf 'q1 '() 1 '(1 0)) 20)
;(list
; (conf 'q1 '() 1 '(1 0))
; (conf 'q1 '(0) 1 '(0))
; (conf 'q1 '(0 0) 0 '())
; (conf 'q1 '(0 0 1) 'b '())
; (conf 'q2 '(0 0) 1 '())
; (conf 'q2 '(0) 0 '(1))
; (conf 'q2 '() 0 '(0 1))
; (conf 'q2 '() 'b '(0 0 1))
; (conf 'q3 '() 0 '(0 1)))

; ****************************************************************
; ** problem 7 ** (15 points)
; Define (in the given representation) a Turing machine named

; tm-xx

; which takes as input a positive binary number
; (with no leading zeroes)
; and produces as output a string of x's of
; length equal to the binary number.

; You *may* use additional tape symbols.  The initial state should
; be 'q1, and the initial configuration has the read/write head on
; the leftmost symbol of the input.  When the machine halts,
; the only non-blank symbols on the tape should be the output string,
; and the read/write head should be positioned on the leftmost
; symbol of the output.

; NOTE: you can still do this problem if your simulator is not working, 
; assuming you understand Turing machines and the representation of them 
; defined above.

; Examples
; input  => output
; 1      => x
; 11     => xxx
; 1010   => xxxxxxxxxx
; ****************************************************************

;see above
  
; ****************************************************************
; ** problem 8 ** (10 points)
; Define a Racket procedure

; (make-tm-stepper n)

; that takes a non-negative integer n and returns
; a Turing machine in the given representation with the
; following behavior.
; Its tape alphabet consists of 'b, 0, 1 and
; no other symbols.
; When started (in state 'q1) on a completely blank
; tape, the machine runs for exactly n steps and halts.
; When the machine halts, the tape may have any contents.

; Try to make your output machines have as few states as possible.

; You may want to look at the built-in procedures
; string->symbol, number->string, and string-append.

;> (string->symbol "Apple")
;'Apple

;> (number->string 3.0)
;"3.0"

;> (string-append "Apple" "Banana")
;"AppleBanana"

; Examples
; (let [(tm11 (make-tm-stepper 11))] (length (simulate tm11 (conf 'q1 '() 'b '()) 20))) => 12 
; (let [(tm100 (make-tm-stepper 100))] (length (simulate tm100 (conf 'q1 '() 'b '()) 200))) => 101 
; ****************************************************************

(define (make-tm-stepper n)
  (reverse (final-ins-list (make-prelim-tm (+ 1 n)))))
    
;create  prelim list                                                              
(define (make-prelim-tm n)
  (if (= n 2)  (list (ins 'q2 'b 'q1 'b 'R))
      (let* [(current-state (string->symbol (string-append "q" (number->string n))))
             (next-state (string->symbol (string-append "q" (number->string (- n 1)))))]
        (cons (ins current-state 'b next-state 'b 'R) (make-prelim-tm (- n 1)))) ) )

;switch the state of each ins in tm
(define (switch-ins-state instruct)
  (let* ((new-state (ins-nst instruct))
        (current-state (ins-cst instruct))
        (new-ins (struct-copy ins instruct [cst new-state] [nst current-state])))
     (if (not(equal? new-ins #f)) new-ins #f)))
    
;recursively switch the states for all ins in tm
(define (final-ins-list ins-list)
  (if (empty? ins-list) '()
                (cons (switch-ins-state (first ins-list)) (final-ins-list (rest ins-list)))))
  
                   
; ****************************************************************
; ** problem 9 ** (5 points)
; Define a Racket procedure

; (halts? tm config)

; that takes as input a Turing machine tm
; and a configuration config and returns
; #t if the given Turing machine would eventually
; halt if started in the given configuration,
; #f if the given Turing machine would never
; eventually halt if started in the given configuration,
; and '? if the procedure cannot decide.

; Try to avoid returning '? if you can.

; Examples
; (halts? tm1 (conf 'q1 '() 1 '(1 0 0 1))) => #t
; (halts? (list (ins 'q1 'b 'q2 'b 'R) (ins 'q2 'b 'q1 'b 'L)) (conf 'q1 '() 'b '())) => #f
; ****************************************************************

(define (halts? tm config)
  "???")  ;IMPOSSIBLE TO BUILD!!!!

; *************** end of hw3.rkt *********************************





