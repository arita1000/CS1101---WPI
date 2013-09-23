;; Students:  Hoang Ngo &  Marilda Bozdo
;; Usernames: hmngo     &  mbozdo
;; ------------------------------------------------

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 1: Develop data definitions for Borrower and ListOfBorrower.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct borrower (name country kind-of-business requested-loan percentage-raised))
;; borrower is (make-borrower String String String Number Number)
;; interp. (make-borrower name country kind-of-business requested-loan percentage-raised) is a borrower 
;;         waiting for loan with
;;            name              as the name of the borrower
;;            country           as the country the borrower is from
;;            kind-of-business  as the kind of business the funds are needed for
;;            requested-loan    as the amount of requested loan
;;            percentage-raised as the percentage raised so far

(define B1 (make-borrower "Cow"       "Moo land" "Moo"   10    50))
(define B2 (make-borrower "Super cow" "Utopia"   "Moo"   10001 50))
(define B3 (make-borrower "Woc"       "Woc land" "Cow"   100   25))
(define B4 (make-borrower "Cup"       "Cup land" "Straw" 100   75))

;; ListOfBorrower is one of
;;      - empty
;;      - (cons borrower ListOfBorrower)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 2: Provide an example of ListOfBorrower. Your example should contain at least three borrowers.       ;;                                                                                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cons B1 (cons B2 (cons B3 empty)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 3: Write the template(s) for your data definitions in Problem 1  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Template for borrower
(define (fn-for-borrower a-borrower)
  (.....(borrower-nam		    a-borrower) 
        (borrower-country     	    a-borrower)
        (borrower-kind-of-business  a-borrower)
        (borrower-requested-loan    a-borrower)
        (borrower-percentage-raised a-borrower)))


; Template for ListOfBorrower
(define (fn-for-lob alob)
  (cond [(empty? alob) (...)]
        [(cons?  alob) (... (first alob)
                            (fn-for-lob (rest alob)))]))
                            

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 4: Write a function count-by-sector that consumes a list of borrowers and 
;; 							     the name of a type of business and 
;; produces the number of borrowers in the list whose loans are for the given kind of business.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ------------------------------------------- HELPER FUNCTIONS ------------------------------------------------

;; signature: sector=?: borrower string -> boolean
;; interp: return true if the borrower's kind of business is the same as the given one
;;         	  false otherwise

(check-expect (sector=? B1 "Moo") true)		;; B1's kind of business: "Moo"
(check-expect (sector=? B2 "Cow") false)	;; B2's kind of business: "Moo"

(define (sector=? a-borrower type-of-business)
  (string=? (borrower-kind-of-business a-borrower) 
  	    type-of-business))


;; ---------------------------------------------- MAIN FUNCTION ------------------------------------------------

;; Signature: count-by-sector : ListOfBorrower String -> Natural
;; Purpose: consume a list of borrower and the kind of business then
;;          return the number of borrowers whose loans are the same as the kind of business

(check-expect (count-by-sector empty "cow") 0)
(check-expect (count-by-sector (cons B1 empty) "Woc") 0)				;; B1: "Moo"
(check-expect (count-by-sector (cons B1 (cons B3 empty)) "Cow") 1)			;; B1: "Moo" B3: "Cow"
(check-expect (count-by-sector (cons B4 (cons B3 (cons B1 (cons B2 empty)))) "Moo") 2)  ;; B1, B2: "Moo" B3: "Cow" B4: "Straw"

(define (count-by-sector alob type-of-business)
  (cond
      [(empty? alob) 0]
      [(cons?  alob) (if (sector=? (first alob) type-of-business)
                         (+ 1 
                            (count-by-sector (rest alob) type-of-business))
                         (count-by-sector (rest alob) type-of-business))]))
                  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 5: Write a function find-by-country that consumes the name of a country and a list of borrowers and 
;; returns the list of borrowers who are from that country.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ------------------------------------------- HELPER FUNCTIONS ----------------------------------------------- 

;; Signature: country=? string borrower -> boolean
;; Interp: Return true if the given country is the same as the country of the borrower
;;	 	  false otherwise

(check-expect (country=? "Moo land" B1) true)		;; B1: "Moo land"
(check-expect (country=? "Albania"  B2) false)		;; B2: "Utopia"

(define (country=? country a-borrower)
  (string=? country (borrower-country a-borrower)))
  
  
;; ------------------------------------------- MAIN FUNCTION ---------------------------------------------------

;; Signature: find-by-country : String ListOfBorrower -> ListOfBorrower
;; Purpose: consume the name of country and a list of borrowers
;;          return the list of borrowers who are from that country

(check-expect (find-by-country "Nowhere" empty) empty)
(check-expect (find-by-country "Nowhere" (cons B1 (cons B2 (cons B3 empty)))) empty)		; B1: Moo land | B2: Utopia | B3: Woc land
(check-expect (find-by-country "Utopia"  (cons B1 (cons B2 (cons B4 empty)))) (cons B2 empty))	; B1: Moo land | B2: Utopia | B3: Woc land | B4: Cup land

(define (find-by-country country alob)
	(cond
		[(empty? alob) empty]
		[(cons?  alob) (if (country=? country (first alob))
				   (cons (first alob) 
					 (find-by-country country (rest alob)))
				   (find-by-country country (rest alob)))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 6: Write a function any-large-loans? that consumes a list of borrowers and produces true           ;;
;; if any of the borrowers in the list are requesting loans in excess of $10,000.                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; HELPER FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Signature: large-loan? : borrower -> boolean
;; Interp: consumes a borrower and produces 
;;         true if the borrower's requested loan is bigger than $ 10000
;;         false otherwise

(define (large-loan? a-borrower)
  (> (borrower-requested-loan a-borrower) 10000))

(check-expect (large-loan? B1) false)		; B1: $10
(check-expect (large-loan? B2) true)		; B2: $10001

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MAIN FUNCTION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Signature: any-large-loans? : ListOfBorrower -> Boolean
;; interp: consumes a list of borrowers and produces
;;         true if any of the borrowers is requesting a loan more than $10000
;;         false otherwise 

(define (any-large-loans? alob)
  (cond [(empty? alob) false]
        [(cons? alob) (or (large-loan? (first alob))
                          (any-large-loans? (rest alob)))]))

(check-expect (any-large-loans? empty) false)
(check-expect (any-large-loans? (cons B1 (cons B2 empty))) true)		; B2: $10001
(check-expect (any-large-loans? (cons B2 (cons B1 (cons B3 empty)))) true)	; B2: $10001
(check-expect (any-large-loans? (cons B1 (cons B3 (cons B4 empty)))) false)	; B1: $10 | B2: $100 | B3: $100


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 7: Write a function funds-needed that consumes a list of borrowers and 
;; produces the total amount of money that these borrowers are still seeking 
;; (ie, the sum of the amounts requested but not yet raised across all borrowers).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ------------------------------------------- HELPER FUNCTIONS ----------------------------------------------- 

;; Signature: fund-still-seeking : Borrower -> Number
;; Interp: Consumes a borrower and produces the amount of money he needs

(check-expect (fund-still-seeking B1) 5)
(check-expect (fund-still-seeking B2) 5000.5)

(define (fund-still-seeking a-borrower)
  (* (/ (- 100 (borrower-percentage-raised a-borrower)) 
  	100) 
     (borrower-requested-loan a-borrower))) 
     
     
;; -------------------------------------------- MAIN FUNCTION --------------------------------------------------

;; Signature: funds-needed : ListOfBorrower -> Number
;; Purpose: consume a list of borrowers and
;;	    return the total money that these borrowers are seeking

(check-expect (funds-needed empty) 0)
(check-expect (funds-needed (cons B1 (cons B2 (cons B3 empty))))	   (+ 5 5000.5 75))
(check-expect (funds-needed (cons B1 (cons B2 (cons B3 (cons B4 empty))))) (+ 5 5000.5 75 25))

(define (funds-needed alob)
	(cond [(empty? alob) 0]
	      [(cons?  alob) (+  (fund-still-seeking (first alob))
				 (funds-needed (rest alob)))]))
