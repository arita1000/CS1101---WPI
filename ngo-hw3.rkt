;; Students: Hoang Ngo
;; Username: hmngo
;; --------------------------------

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 1: A storm can be any one of the following:
;;
;;- hurricane: Information required for a hurricane consists of the name of the hurricane, 
;;             the category (a number between 1 and 5, inclusive), the maximum sustained winds in miles per hour, 
;;             the velocity of the storm in miles per hour, and the storm's heading (for example, NNW).
;;- thunderstorm: The definition of a thunderstorm consists of the number of inches of rainfall, 
;;                the maximum wind gust in miles per hour, the velocity of the storm in miles per hour, and its heading.
;;- fire: A fire is represented by the number of square miles it covers, the number of days it has been raging, 
;;        and the number of people displaced by the fire.
;;        
;;Develop a data definition for each type of storm described above, and a data definition for an itemization for storms. 
;;Provide at least one example for each kind of storm.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct hurricane (name category max-wind storm-velocity storm-destination))
;; A hurricane is (make-hurricane String Natural Number Number String)
;; interp. (make-hurricane name category max-wind storm-velocity storm-destination) is a hurricane with
;;         name as its name
;;         category as its category (1 to 5, inclusive)
;;         max-wind as its maximum winds in miles per hour
;;         storm-velocity as the storm's velocity
;;         storm-destination as the storm's heading

(define H1 (make-hurricane "Crazy" 4 100 900 "Nowhere"))

(define-struct thunderstorm (amount-rainfall max-wind-gust storm-velocity storm-destination))
;; A thunderstorm is (make-thunderstorm Number Number Number String)
;; interp. (make-thunderstorm amount-rainfall max-wind-gust storm-velocity storm-destination) is a thunderstorm that has
;;         amount-rainfall as its number of inches of rainfall
;;         max-wind-gust as its maximum wind gust in miles per hour
;;         storm-velocity as the storm's velocity in miles per hour
;;         storm-destination as the storm's heading

(define T1 (make-thunderstorm 60 320 203 "Somewhere"))

(define-struct fire (cover-area total-raging-day no-people-displaced))
;; A fire is (make-fire Number Natural Natural)
;; interp. (make-fire cover-area total-raging-day no-people-displaced) is a fire that has
;;         cover-area as the number of square miles it covers
;;         total-raging-day as the number of days it has been raging
;;         no-people-displaced as the number of people displaced by the fire

(define F1 (make-fire 1000 230 1939290))

;; A storm is one of
;;                   - hurricane
;;                   - thunderstorm
;;                   - fire


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 2: Provide the template for each data definition you made in Problem 1 (including the itemization).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Template for hurricane

(define (fun-for-hurricane a-hurricane)
    (... (hurricane-amount-rainfall   a-hurricane)
         (hurricane-category          a-hurricane)
         (hurricane-max-wind          a-hurricane)
         (hurricane-storm-velocity    a-hurricane)
         (hurricane-storm-destination a-hurricane)))

;; Template for thunderstorm

(define (fun-for-thunderstorm a-thunderstorm)
    (... (thunderstorm-amount-rainfall   a-thunderstorm)
         (thunderstorm-max-wind-gust     a-thunderstorm)
         (thunderstorm-storm-velocity    a-thunderstorm)
         (thunderstorm-storm-destination a-thunderstorm)))

;; Template for fire

(define (fun-for-fire a-fire)
    (... (fire-cover-area        a-fire)
         (fire-total-raging-day  a-fire)
         (fire-storm-destination a-fire)))
         
;; Template for storm

(define (fun-for-storm a-storm)
    (cond 
        [(hurricane? a-storm) (...(hurricane-name              a-storm)
                                  (hurricane-category          a-storm)
                                  (hurricane-max-wind          a-storm)
                                  (hurricane-storm-velocity    a-storm)
                                  (hurricane-storm-destination a-storm))]
        [(thunderstorm? a-storm)  (...(thunderstorm-amount-rainfall   a-storm)
                                      (thunderstorm-max-wind-gust     a-storm)
                                      (thunderstorm-storm-velocity    a-storm)
                                      (thunderstorm-storm-destination a-storm))]
        [(fire? a-storm) (...(fire-cover-area        a-storm)
                             (fire-total-raging-day  a-storm)
                             (fire-storm-destination a-storm))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 3: Write a function high-impact? that consumes a storm and produces a boolean. 
;;            The function returns true if the storm is a category 4 or 5 hurricane, 
;;            a thunderstorm with more than 3 inches of rainfall and winds exceeding 60mph, or
;;            a fire covering at least 50 square miles.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Signature: high-impact?: storm -> boolean
;; Purpose: return true if the storm is a category 4 or 5 hurricane
;;                                      the thunderstorm with more than 3 inches of rainfall and winds exceeding 60mph
;;                                      a fire covering at least 50 square miles

(check-expect (high-impact? (make-hurricane "n1" 3 90 90 "nowhere")) false)
(check-expect (high-impact? (make-hurricane "n2" 4 34 34 "nowhere")) true)
(check-expect (high-impact? (make-hurricane "n3" 5 24 57 "nowhere")) true)
(check-expect (high-impact? (make-thunderstorm 2 50 354 "nowhere")) false)
(check-expect (high-impact? (make-thunderstorm 3 50 354 "nowhere")) false)
(check-expect (high-impact? (make-thunderstorm 3 60 543 "nowhere")) false)
(check-expect (high-impact? (make-thunderstorm 4 60 354 "nowhere")) false)
(check-expect (high-impact? (make-thunderstorm 4 70 234 "nowhere")) true)
(check-expect (high-impact? (make-fire 40 454 236)) false)
(check-expect (high-impact? (make-fire 50 456 765)) true)
(check-expect (high-impact? (make-fire 60 645 347)) true)

(define (high-impact? a-storm)
    (cond 
        [(hurricane? a-storm)    (>= (hurricane-category a-storm) 4)]
        [(thunderstorm? a-storm) (and (> (thunderstorm-amount-rainfall a-storm) 3)
                                	  (> (thunderstorm-max-wind-gust   a-storm) 60))]
        [(fire? a-storm)         (>= (fire-cover-area a-storm) 50)]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 4: Write a function change-heading that consumes a storm and a heading and produces a storm. 
;;            The storm is returned unchanged if the given storm is a fire. 
;;            Otherwise, the storm that's produced is a storm the same as the original, 
;;            except that the heading has been changed to the given heading.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Signature: change-heading : Storm String -> Storm
;; Purpose: consume a storm and return
;;          an unchanged storm if the given storm is a fire
;;          a storm the same as the original with a new heading

(check-expect (change-heading F1 "somewhere") F1)
(check-expect (change-heading T1 "nowhere") (make-thunderstorm 60 320 203 "nowhere"))
(check-expect (change-heading H1 "somewhere") (make-hurricane "Crazy" 4 100 900 "somewhere"))

(define (change-heading a-storm heading)
    (cond 
        [(fire? a-storm) a-storm]
        [(hurricane? a-storm) (make-hurricane (hurricane-name           a-storm)
        									  (hurricane-category       a-storm)
        									  (hurricane-max-wind       a-storm)
        									  (hurricane-storm-velocity a-storm)
        									  heading)]
        [(thunderstorm? a-storm) (make-thunderstorm (thunderstorm-amount-rainfall a-storm)
        											(thunderstorm-max-wind-gust   a-storm)
        											(thunderstorm-storm-velocity  a-storm)
        											heading)]))
        

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 5: Develop a function character-count that consumes a list-of-string and 
;;            counts the total number of characters in all strings in the list.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define LOS1 empty)
(define LOS2 (cons "paper" empty))
(define LOS3 (cons "paper" (cons "pen" (cons "idiosyncrasy" empty))))

;; Signature: character-count : ListOfString -> Natural
;; Purpose: return the total number of characters in all string of the ListOfString

(check-expect (character-count LOS1) 0)
(check-expect (character-count LOS2) 5)
(check-expect (character-count LOS3) 20)

(define (character-count a-los)
    (cond 
        [(empty? a-los) 0]
        [(cons? a-los) (+ (string-length (first a-los))
                          (character-count (rest a-los)))]))
                          

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 6: Develop a function all-contain-numbers? that consumes a ListOfString and 
;; produces true if every string in the list contains at least one numeric character. 
;; Otherwise, the function produces false. (Hint: check the DrRacket help desk for various string functions). 
;; Here are two sample test cases (you may need additional test cases):
;; (check-expect (all-contain-numbers? (cons "CS1101" (cons "A1" (cons "32" empty)))) true)
;; (check-expect (all-contain-numbers? (cons "CS1101" (cons "A-one" (cons "32" empty)))) false)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; --------------------------------------------- MAIN FUNCTION -----------------------------------------------------
;; Signature: all-contain-number? : ListOfString -> Boolean
;; Purpose: return true if a ListOfString contains at least one numeric character
;;                 false otherwise

(check-expect (all-contain-number? empty) false)
(check-expect (all-contain-number? (cons "CS1101" (cons "A1" (cons "32" empty)))) true)
(check-expect (all-contain-number? (cons "CS1101" (cons "A-one" (cons "32" empty)))) false)

(define (all-contain-number? a-los)
    (cond 
        [(empty? a-los) false]
        [(cons? a-los)  (if (almost-end? a-los)
                            (has-number? (first a-los))
                            (and (has-number? (first a-los))
                                 (all-contain-number? (rest a-los))))]))
                                     
                           

;; -------------------------------------------- HELPER FUNCTIONS ---------------------------------------------------
;; Signature: has-number? : String -> Boolean
;; Purpose: return true if the string has a numeric character
;;                 false otherwise

(check-expect (has-number? "abc")     false)
(check-expect (has-number? "abc-()5") true)
(check-expect (has-number? "a43")     true)
(check-expect (has-number? "243")     true)

(define (has-number? str)
    (or (string-contains? "0" str)
        (string-contains? "1" str)
        (string-contains? "2" str)
        (string-contains? "3" str)
        (string-contains? "4" str)
        (string-contains? "5" str)
        (string-contains? "6" str)
        (string-contains? "7" str)
        (string-contains? "8" str)
        (string-contains? "9" str)))

;; Signature: almost-end? : ListOfString -> boolean
;; Purpose: consume a list having 2 elements at least and
;;          return true if the ListOfString contains only 2 elements left
;;                 false otherwise

(check-expect (almost-end? (cons "asd" empty)) true)
(check-expect (almost-end? (cons "asd" (cons "asd" empty))) false)

(define (almost-end? a-los)
  (empty? (rest a-los)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 7: Write a data definition to represent a list of natural numbers (call it ListOfNatural). 
;; Then develop a function lengths-of-strings that consumes a ListOfString and produces a ListOfNatural. 
;; The function produces a list of the lengths of each of the strings in the given ListOfString.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A ListOfNatural is one of
;;       - empty
;;       - (cons Natural empty)

(define (fun-for-lon a-lon)
    (cond 
        [(empty? a-lon) ...]
        [(cons? a-lon) (... (first (a-lon))
                            (fun-for-lon (rest a-lon)))]))
                            
;; Signature: lengths-of-strings : ListOfString -> ListOfNatural
;; Purpose: consume a ListOfString and 
;;          return a ListOfNatural that contains the length of all elements in ListOfString separately

(check-expect (lengths-of-strings empty) empty)
(check-expect (lengths-of-strings (cons "a" (cons "abc" (cons "Hahah" (cons "sajsd0" empty)))))
              (cons 1 (cons 3 (cons 5 (cons 6 empty)))))

(define (lengths-of-strings a-los)
    (cond 
        [(empty? a-los) empty]
        [(cons? a-los) (cons (string-length (first a-los))
                             (lengths-of-strings (rest a-los)))]))
