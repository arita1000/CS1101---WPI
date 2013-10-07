;; Students:  Hoang Ngo & Marilda Bozdo
;; Usernames: hmngo     & mbozdo
;; ----------------------------------------

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ############################################### PART 1 ######################################################
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Problem 1: Provide data definitions for a river system. 
;; For each river in the hierarchy, you should record the following information:                      
;;        the name of the river, the pH of the water, the DO in parts per million, 
;;        and a list of the tributaries (rivers) that feed into the river.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct river (name pH DO tributaries))
;; A river is a (make-river String Number Number ListOfRiver) where
;;   name        is the name of the river
;;   pH          is the level of pH of the water which ranges from 0 to 14
;;   DO          is the level of dissolved oxygen and ranges from 0 ppm to 12 ppm
;;   tributaries is a list of the rivers that feed into the given river 


;; ListOfRiver is one of:
;;       - empty
;;       - (cons River ListOfRiver)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Problem 2: Provide an example of a river system that starts with a single river and               
;;            consists of at least two levels in the hierarchy below that. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define River1 
			 (make-river "Missouri" 7 6 
      			        (list (make-river "Jefferson" 7.5 9 
            			                    (list (make-river "Beaverhead" 9 11  empty) 
                  			                    (make-river "Big Hole"   9 6.5 empty)))
                    			(make-river "Sun" 14 12 empty)
                    			(make-river "Yellowstone" 10 7 (list (make-river "Gardner" 5  11 empty) 
                        			                                 (make-river "Sheilds" 13 12 empty)
                              			                           (make-river "Boulder" 1  1  empty)))
                    			(make-river "Madison"  1 12 empty)
                  				(make-river "Gallatin" 2 6  empty))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Problem 3: Provide the templates for your data definitions.                                      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; fun-for-river: River...-> ...

#;(define (fun-for-river a-river....)
       (... (river-name a-river)
            (river-pH a-river)
            (river-DO a-river)
            (fun-for-lor (river-tributaries a-river)...)))


;; fun-for-lor: ListOfRiver ... -> ....

#;(define (fun-for-lor alor ....)
    (cond [(empty? alor) ..... ]
          [(cons?  alor) (fun-for-river (first alor)....)
                         (fun-for-lor (rest alor).....)]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Problem 4: Develop a function lower-ph-than that consumes a river system and a number.
;;             The function produces a list of the names of each river in the given system that has 
;; a pH value lower than the given pH.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ---------------------------------------------- MAIN FUNCTION -------------------------------------------------
;; Signature: lower-ph-than: river number -> ListOfString
;; interp: consumes a river system and a pH value and 
;;         produces a list of the names of each river that has a pH lower than the given pH value
 
(define (lower-ph-than a-river limit)
  (if (< (river-pH a-river) limit)
      (cons (river-name a-river)
            (lower-ph-than/list (river-tributaries a-river) limit))
      (lower-ph-than/list (river-tributaries a-river) limit)))


;; --------------------------------------------- HELPER FUNCTIONS -----------------------------------------------
;; Signature: lower-ph-than/list: ListOfRiver number -> ListOfString
;; Interp: consumes a list of rivers and a pH values and 
;;         produces a list of names of the rivers in the list that have a lower pH value than the given value
    
(define (lower-ph-than/list alor limit)
  (cond [(empty? alor) empty]
        [(cons? alor) (append (lower-ph-than (first alor) limit)
                              (lower-ph-than/list (rest alor) limit))]))

(check-expect (lower-ph-than River1 9) 
              (list "Missouri" "Jefferson" "Gardner" "Boulder" "Madison" "Gallatin"))
(check-expect (lower-ph-than/list empty 7) 
              empty)
(check-expect (lower-ph-than (make-river "Johnson" 12 6 (list (make-river "Buna" 5 6 empty))) 7) 
              (list "Buna"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Problem 5: Develop a function healthy? that consumes a river system and produces a boolean. 
;;             The function returns true if every river in the system has a pH between 6.5 and 8.5,    
;;             and a DO of at least 6ppm.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ------------------------------------------------ CONSTANTS ---------------------------------------------------
(define LOWER-BOUND-PH 6.5)
(define UPPER-BOUND-PH 8.5)
(define LOWER-BOUND-DO 6)

;; ---------------------------------------------- MAIN FUNCTION -------------------------------------------------
;; Signature: healthy? : river -> boolean
;; interp: consumes a river and 
;;         produces true if each river in the system has pH between 6.5 and 8.5 and DO at least 6ppm
;;                  false otherwise

(define (healthy? a-river)
  (and (>  (river-pH a-river) LOWER-BOUND-PH)               ;; pH condition
       (<  (river-pH a-river) UPPER-BOUND-PH)
       (>= (river-DO a-river) LOWER-BOUND-DO)               ;; DO condition
       (all-healthy? (river-tributaries a-river))))
    

;; --------------------------------------------- HELPER FUNCTIONS -----------------------------------------------
;; Signature: all-healthy? : ListOfRiver -> Boolean
;; interp: consumes a list of rivers and 
;;         produces true if each river in the system has pH between 6.5 and 8.5 and DO at least 6ppm
;;                  false otherwise

(define (all-healthy? alor)
  (cond [(empty? alor) true]
        [(cons? alor) (and (healthy? (first alor))
                           (all-healthy? (rest alor)))]))             
       
(check-expect (healthy? River1) 
              false)

(check-expect (healthy?
               (make-river "Missouri" 7 6 
                           (list (make-river "Jefferson"   7.5 9 
                                             (list (make-river "Beaverhead" 6.6 11  empty) 
                                                   (make-river "Big Hole"   7.5 6.5 empty)))
                                 (make-river "Sun"         7   12 empty)
                                 (make-river "Yellowstone" 6.9 7 
                                             (list (make-river "Gardner" 7.6 11 empty) 
                                                   (make-river "Sheilds" 8.3 12 empty)
                                                   (make-river "Boulder" 8.4 7  empty)))
                                 (make-river "Madison"     6.7 12 empty)
                                 (make-river "Gallatin"    6.8 6  empty)))) 
              true)

(check-expect (healthy? 
               (make-river "Missouri" 8.5 6 
                           (list (make-river "Jefferson" 7.5 9 
                                             (list (make-river "Beaverhead" 6.6 11 empty) 
                                                   (make-river "Big Hole" 7.5 6.5 empty)))
                                 (make-river "Sun" 7 12 empty)
                                 (make-river "Yellowstone" 6.9 7 
                                             (list (make-river "Gardner" 7.6 11 empty) 
                                                   (make-river "Sheilds" 8.3 12 empty)
                                                   (make-river "Boulder" 8.4 7 empty)))
                                 (make-river "Madison" 6.7 12 empty)
                                 (make-river "Gallatin" 6.8 6 empty))))
              false)

(check-expect (healthy? 
              (make-river "Missouri" 7 5 
                           (list (make-river "Jefferson" 7.5 9 
                                             (list (make-river "Beaverhead" 6.6 11 empty) 
                                                   (make-river "Big Hole" 7.5 6.5  empty)))
                                 (make-river "Sun" 7 12 empty)
                                 (make-river "Yellowstone" 6.9 7 (list (make-river "Gardner" 7.6 11 empty) 
                                                                       (make-river "Sheilds" 8.3 12 empty)
                                                                       (make-river "Boulder" 8.4 7 empty)))
                                 (make-river "Madison" 6.7 12 empty)
                                 (make-river "Gallatin" 6.8 6 empty)))) 
              false)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Problem 6: Develop a function lower-all-ph consumes a river system and produces a river system. 
;;             The river system that is produced is the same as the original, 
;;             except that the pH of all the rivers in the system have been lowered by 0.1.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ------------------------------------------------ CONSTANTS ---------------------------------------------------
(define SUBTRAHEND-PH .1)


;; ---------------------------------------------- MAIN FUNCTION -------------------------------------------------
;; Signature: lower-all-ph : river -> river
;; Interp: consumes a river and 
;;         produces another river the same as the first one but with pH lowered by 0.1

(define (lower-all-ph a-river)
     (make-river (river-name a-river) 
                 (- (river-pH a-river) SUBTRAHEND-PH) 
                 (river-DO a-river)
                 (lower-all-ph/list (river-tributaries a-river))))


;; --------------------------------------------- HELPER FUNCTIONS -----------------------------------------------
;; Signature: lower-all-ph/list : ListOfRiver -> ListOfRiver
;; Interp: consumes a list of river and
;;         produces another list of river the same as the first one but with pH lowered by 0.1

   ; better solution
(define (lower-all-ph/list alor)
  (map lower-all-ph alor))

;(define (lower-all-ph/list alor)
;  (cond [(empty? alor) empty ]
;        [(cons? alor) (cons (lower-all-ph (first alor))
;                       (lower-all-ph/list (rest alor)))]))

(check-expect (lower-all-ph River1) (make-river "Missouri" 6.9 6 (list (make-river "Jefferson" 7.4 9 
    (list (make-river "Beaverhead" 8.9 11 empty) (make-river "Big Hole" 8.9 6.5 empty)))
        (make-river "Sun" 13.9 12 empty)
        (make-river "Yellowstone" 9.9 7 (list (make-river "Gardner" 4.9 11 empty) 
                                             (make-river "Sheilds" 12.9 12 empty)
                                             (make-river "Boulder" 0.9 1 empty)))
        (make-river "Madison" 0.9 12 empty)
        (make-river "Gallatin" 1.9 6 empty))))
        

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ########################################## PART 2 ################################################
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Borrower is a (make-borrower String String String Number Number)
(define-struct borrower (name country business requested percent-raised))
;; interp:  represents a borrower for a microlending institution
;;          requested is the amount of the requested loan (in US dollars)
;;          percent-raised is the percentage of the loan already raised
;;           (i.e. a number in the range 0..100 inclusive)

(define BORROWER-1 (make-borrower "Cow"       "Moo land" "Moo"   10    50))
(define BORROWER-2 (make-borrower "Super cow" "Utopia"   "Moo"   10001 50))
(define BORROWER-3 (make-borrower "Woc"       "Woc land" "Cow"   100   25))
(define BORROWER-4 (make-borrower "Cup"       "Cup land" "Straw" 100   75))
(define BORROWER-5 (make-borrower "Super cup" "Cup land" "Ice"   10000 55))


;; a ListOfBorrower is one of
;;    - empty
;;    - (cons Borrower ListOfBorrower)

(define BORROWER-LIST-1 (cons BORROWER-1 (cons BORROWER-2 (cons BORROWER-3 empty))))
(define BORROWER-LIST-2 (cons BORROWER-2 (cons BORROWER-2 (cons BORROWER-3 empty))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Problem 7: Using map and/or filter,     
;;             redefine the function find-by-country that you wrote for Homework 4                            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; find-by-country :  String ListOfBorrower -> ListOfBorrower
;; interp. consumes the name of a country and a list of borrowers and 
;;         produces a list of the borrowers who are from that country

(check-expect (find-by-country "Nowhere" empty) 
              empty)

; BORROWER-1: "Moo land" | BORROWER-2: "Utopia" | BORROWER-3: "Woc land"
(check-expect (find-by-country "Nowhere" (cons BORROWER-1 (cons BORROWER-2 (cons BORROWER-3 empty)))) 
              empty)	                         

; BORROWER-1: "Moo land" | BORROWER-2: "Utopia" | BORROWER-3: "Woc land" | BORROWER-4: "Cup land"
(check-expect (find-by-country "Utopia"  (cons BORROWER-1 (cons BORROWER-2 (cons BORROWER-4 empty)))) 
              (cons BORROWER-2 empty))

(define (find-by-country country alob)
  (local [(define (is-country? aborrower)
            (string=? (borrower-country aborrower) country))]
    (filter is-country? alob)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Problem 8: Using map and/or filter, define a new function list-all-businesses                            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; list-all-businesses :  ListOfBorrower -> ListOfString
;; interp. consumes a list of borrowers and
;;         produces a list of the names of all businesses for all borrowers in the list 
;;                  (it's OK for the list of businesses to contain duplicates)

(check-expect (list-all-businesses empty)
              empty)

(check-expect (list-all-businesses BORROWER-LIST-1)
              (list "Cow" "Super cow" "Woc"))

(check-expect (list-all-businesses BORROWER-LIST-2)
              (list "Super cow" "Super cow" "Woc"))


(define (list-all-businesses alob)
  (map borrower-business alob))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Problem 9: Using map and/or filter, define a new function names-large-loans                              
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; names-large-loans :  ListOfBorrower Number -> ListOfString
;; interp. consumes a list of borrowers and the amount for a loan and
;;         produces a list of the names of the borrowers who are requesting more than the given loan amount

(check-expect (names-large-loans empty 30) 
              empty)

(check-expect (names-large-loans BORROWER-LIST-1 100) 
              (list "Super cow"))

(check-expect (names-large-loans BORROWER-LIST-2 99999999) 
              empty)


(define (names-large-loans alob loan)
  (local [(define (large-loan? aborrower)
            (> (borrower-requested aborrower) loan))]
    (map borrower-name (filter large-loan? alob))))
