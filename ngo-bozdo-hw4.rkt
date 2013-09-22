;; Students:  Hoang Ngo & Marilda Bozdo
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

(define B1 (make-borrower "Cow"       "Moo land" "Moo"   10   .5))
(define B2 (make-borrower "Super cow" "Utopia"   "Moo"   10001 .5))
(define B3 (make-borrower "Woc"       "Woc land" "Cow"   100  .25))
(define B4 (make-borrower "Cup"       "Cup land" "Straw" 100  .75))

;; ListOfBorrower is one of
;;      - empty
;;      - (cons borrower ListOfBorrower)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 2: Provide an example of ListOfBorrower. Your example should contain at least three borrowers.       ;;                                                                                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cons (make-borrower "Cow" "Moo land" "Moo" 10 .5)(cons (make-borrower "Super cow" "Utopia"   "Moo"   1000 .5)(cons (make-borrower "Woc"       "Woc land" "Cow"   100  .25) empty)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 3: Write the template(s) for your data definitions in Problem 1  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (fn-for-borrower a-borrower)
  (.....(borrower-name a-borrower) 
        (borrower-country a-borrower)
        (borrower-kind-of-business a-borrower)
        (borrower-requested-loan a-borrower)
        (borrower-percentage-raised a-borrower)))


(define (fn-for-lob alob)
  (cond [(empty? alob) .... ]
        [(cons? alob) (..( fn-for-borrower (first alob))
                           (fn-for-lob (rest alob)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 4: Write a function count-by-sector that consumes a list of borrowers and 
;; 							     the name of a type of business and 
;; produces the number of borrowers in the list whose loans are for the given kind of business.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;; MAIN FUNCTION;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Signature: count-by-sector : ListOfBorrower String -> Natural
;; Purpose: consume a list of borrower and the kind of business then
;;          return the number of borrowers whose loans are the same as the kind of business

(check-expect (count-by-sector empty "cow") 0)
(check-expect (count-by-sector (cons B1 empty) "Woc") 0)
(check-expect (count-by-sector (cons B1 (cons B3 empty)) "Cow") 1)
(check-expect (count-by-sector (cons B4 (cons B3 (cons B1 (cons B2 empty)))) "Moo") 2)

(define (count-by-sector alob type-of-business)
  (cond
      [(empty? alob) 0]
      [(cons?  alob) (if (sector? (first alob) type-of-business)
                         (+ 1 
                         		(count-by-sector (rest alob) type-of-business))
                         (count-by-sector (rest alob) type-of-business))]))
                         
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;HELPER FUNCTION;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; signature: sector?: borrower string -> boolean
;; interp: consumes a borrower and a type of business and produces true if the kind of businnes of the borrower
;;         is the same as type of business and 
;;         false otherwise

(define (sector? a-borrower type-of-bussiness)
  (string=? (borrower-kind-of-business a-borrower) type-of-bussiness))

(check-expect (sector? B1 "Moo") true)
(check-expect (sector? B2 "Cow") false)
                         
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 5: Write a function find-by-country that consumes the name of a country and a list of borrowers and 
;; returns the list of borrowers who are from that country.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MAIN FUNCTION;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Signature: find-by-country : String ListOfBorrower -> ListOfBorrower
;; Purpose: consume the name of country and a list of borrowers
;;          return the list of borrowers who are from that country

(check-expect (find-by-country "Nowhere" empty) empty)
(check-expect (find-by-country "Nowhere" (cons B1 (cons B2 (cons B3 empty)))) empty)
(check-expect (find-by-country "Utopia"  (cons B1 (cons B2 (cons B4 empty)))) (cons B2 empty))

(define (find-by-country country alob)
	(cond
			[(empty? alob) empty]
			[(cons?  alob) (if (country? country (first alob))
					   (cons (first alob) 
						 (find-by-country country (rest alob)))
					   (find-by-country country (rest alob)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; HELPER FUNCTION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Signature: country? string borrower -> boolean
;; Interp: consume a name of country and a borrower and 
;;         produce true if the name of the country is the same as the country of the borrower and
;;         false otherwise

(define (country? country a-borrower)
  (string=? country (borrower-country a-borrower)))

(check-expect (country? "Moo land" B1) true)
(check-expect (country? "Albania" B2) false)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 6: Write a function any-large-loans? that consumes a list of borrowers and produces true           ;;
;; if any of the borrowers in the list are requesting loans in excess of $10,000.                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; HELPER FUNCTION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Signature: large-loan? borrower -> boolean
;; Interp: consumes a borrower and produces 
;;         true if the borrower's requested loan is bigger than $ 10000
;;         false otherwise

(define (large-loan? a-borrower)
  (> (borrower-requested-loan a-borrower) 10000))

(check-expect (large-loan? B1) false)
(check-expect (large-loan? B2) true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MAIN FUNCTION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Signature: any-large-loans?: ListOfBorrower -> Boolean
;; interp: consumes a list of borrowers and produces
;;         true if any of the borrowers is requesting a loan more than $10000
;;         false otherwise 

(define (any-large-loans? alob)
  (cond [(empty? alob) false]
        [(cons? alob) (if (large-loan? (first alob))
                          true
                         (any-large-loans? (rest alob)))]))

(check-expect (any-large-loans? empty) false)
(check-expect (any-large-loans? (cons B1 (cons B2 empty))) true)
(check-expect (any-large-loans? (cons B2 (cons B1 (cons B3 empty)))) true)
(check-expect (any-large-loans? (cons B1 (cons B3 (cons B4 empty)))) false)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 7: Write a function funds-needed that consumes a list of borrowers and 
;; produces the total amount of money that these borrowers are still seeking 
;; (ie, the sum of the amounts requested but not yet raised across all borrowers).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MAIN FUNCTION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Signature: funds-needed : ListOfBorrower -> Number
;; Purpose: consume a list of borrowers and
;;					return the total money that these borrowers are seeking

(check-expect (funds-needed empty) 0)
(check-expect (funds-needed (cons B1 (cons B2 (cons B3 empty)))) (+ 5 5000.5 75))
(check-expect (funds-needed (cons B1 (cons B2 (cons B3 (cons B4 empty))))) (+ 5 5000.5 75 25))


(define (funds-needed alob)
	(cond [(empty? alob) 0]
	      [(cons?  alob) (+  (funds-for-one (first alob))
				          (funds-needed (rest alob)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; HELPER FUNCTION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Signature: funds-for-one: Borrower -> Number
;; Interp: Consumes a borrower and produces the amount of money he needs

(define (funds-for-one a-borrower)
  (*(- 1 (borrower-percentage-raised a-borrower)) (borrower-requested-loan a-borrower))) 

(check-expect (funds-for-one B1) 5)
(check-expect (funds-for-one B2) 5000.5)
