;; Students:  Hoang Ngo & Marilda Bozdo
;; Usernames: hmngo     & mbozdo
;; --------------------------------------

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

(define-struct hurricane (name category max-winds velocity heading))
;; Signature: a hurricane is a (make-hurricane string natural number number string)
;; Interp: (make-hurricane name category max-winds velocity heading) is a hurricane with
;;          name as the name of the hurricane
;;          category as a hurricane's category (a number between 1 and 5 inclusive)
;;          max-winds as the maximum sustained wind in miles per hour
;;          velocity as the velocity of the hurricane in miles per hour
;;          heading as the place where the hurricane is heading

(define H1 (make-hurricane "Katrina" 5 175 150 "South Florida"))

(define-struct thunderstorm (amount-rainfall max-wind-gust velocity heading))
;; Signature: a thunderstorm is a (make-thunderstorm number number number string)
;; Interp: (make-thunderstorm amount-rainfall max-wind-gust velocity heading) is a thunderstorm with
;;           amount-rainfall as the number of inches of rainfall
;;           max-wind-gust as the maximum wind gust in miles per hour
;;           velocity as the velocity of the thunderstorm in miles per hour
;;           heading as the place where the thunderstorm is heading

(define T1 (make-thunderstorm 5 50 80 "New York"))

(define-struct fire (cover-area duration n-people-displaced))
;; Signature: a fire is a (make-fire number number number)
;; Interp: (make-fire cover-area duration n-people-displaced) is a fire with
;;          cover-area as the number of square miles it covers
;;          duration as the number of days it has been raging
;;          n-people-displaced as the number of people displaced by fire

(define F1 (make-fire 100 10 50))

;; Storm is one of
;;      - hurricane
;;      - thunderstorm
;;      - fire


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 2: Provide the template for each data definition you made in Problem 1 (including the itemization).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Template for hurricane 

(define (fun-for-hurricane a-hurricane)
  (... (hurricane-name      a-hurricane)
       (hurricane-category  a-hurricane)
       (hurricane-max-winds a-hurricane)
       (hurricane-velocity  a-hurricane)
       (hurricane-heading   a-hurricane)))

;; Template for thunderstorm

(define (fun-for-thunderstorm a-thunderstorm)
  (... (thunderstorm-amount-rainfall a-thunderstorm)
       (thunderstorm-max-wind-gust   a-thunderstorm)
       (thunderstorm-velocity        a-thunderstorm)
       (thunderstorm-heading         a-thunderstorm)))

;; Template for fire

(define (fun-for-fire a-fire)
  (... (fire-cover-area         a-fire)
       (fire-duration           a-fire)
       (fire-n-people-displaced a-fire)))

;; Template for storm

(define (fun-for-storm a-storm ...)
  (cond 
        [(hurricane? a-storm) (... (hurricane-name      a-storm)
                                   (hurricane-category  a-storm)
                                   (hurricane-max-winds a-storm)
                                   (hurricane-velocity  a-storm)
                                   (hurricane-heading   a-storm))]
        [(thunderstorm? a-storm) (... (thunderstorm-amount-rainfall a-storm)
                                      (thunderstorm-max-wind-gust   a-storm)
                                      (thunderstorm-velocity        a-storm)
                                      (thunderstorm-heading         a-storm))]
        [(fire? a-storm) (... (fire-cover-area         a-storm)
                              (fire-duration           a-storm)
                              (fire-n-people-displaced a-storm))]))     


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 3: Write a function high-impact? that consumes a storm and produces a boolean. 
;;            The function returns true if the storm is a category 4 or 5 hurricane, 
;;            a thunderstorm with more than 3 inches of rainfall and winds exceeding 60mph, or
;;            a fire covering at least 50 square miles.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Signature: high-impact? storm -> boolean
;; Interp: return true if the storm is
;;                      - a hurricane with category 4 or 5
;;                      - a thunderstorm with more than 3 inches of rainfall and winds exceeding 60mph
;;                      - a fire covering at least 50 square miles
;;                false otherwise

(define (high-impact? a-storm)
  (cond 
        [(hurricane? a-storm)    (>= (hurricane-category a-storm) 4)]
        [(thunderstorm? a-storm) (and (> (thunderstorm-amount-rainfall a-storm) 3) 
                                      (> (thunderstorm-max-wind-gust   a-storm) 60))]
        [(fire? a-storm)         (>= (fire-cover-area a-storm) 50)]))

;; Test cases:
(check-expect (high-impact? (make-hurricane "Umbridge" 3 90 90 "California")) false)
(check-expect (high-impact? (make-hurricane "Ursula" 4 34 34 "New York")) true)
(check-expect (high-impact? (make-hurricane "Katrina" 5 175 150 "South Florida")) true)
(check-expect (high-impact? (make-thunderstorm 2 50 354 "California")) false)
(check-expect (high-impact? (make-thunderstorm 3 50 354 "New York")) false)
(check-expect (high-impact? (make-thunderstorm 3 60 543 "Texas")) false)
(check-expect (high-impact? (make-thunderstorm 4 60 354 "Massachusetts")) false)
(check-expect (high-impact? (make-thunderstorm 4 70 234 "Mexico")) true)
(check-expect (high-impact? (make-fire 40 454 236)) false)
(check-expect (high-impact? (make-fire 50 456 765)) true)
(check-expect (high-impact? (make-fire 60 645 347)) true)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 4: Write a function change-heading that consumes a storm and a heading and produces a storm. 
;;            The storm is returned unchanged if the given storm is a fire. 
;;            Otherwise, the storm that's produced is a storm the same as the original, 
;;            except that the heading has been changed to the given heading.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Signature: change-heading: storm string -> storm
;; Interp: consume a storm and a heading and
;;         return an unchanged storm if the given storm is a fire
;;         otherwise produce a storm same as original with a changed heading

(check-expect (change-heading F1 "Texas") F1)
(check-expect (change-heading T1 "Boston") (make-thunderstorm   5 50 80 "Boston"))
(check-expect (change-heading H1 "Philadelphia") (make-hurricane  "Katrina" 5 175 150 "Philadelphia"))

(define (change-heading a-storm heading)
  (cond [(hurricane? a-storm) (make-hurricane (hurricane-name      a-storm) 
                                              (hurricane-category  a-storm) 
                                              (hurricane-max-winds a-storm) 
                                              (hurricane-velocity  a-storm)
                                              heading)]
        [(thunderstorm? a-storm) (make-thunderstorm  (thunderstorm-amount-rainfall a-storm)
                                                     (thunderstorm-max-wind-gust   a-storm)
                                                     (thunderstorm-velocity        a-storm)
                                                     heading)]
        [(fire? a-storm) a-storm]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 5: Develop a function character-count that consumes a list-of-string and 
;;            counts the total number of characters in all strings in the list.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; a ListOfString is one of
;;     - empty
;;     - (cons String ListOfString)

(define LOS1 empty)
(define LOS2 (cons "paper" empty))
(define LOS3 (cons "paper" (cons "pen" (cons "idiosyncrasy" empty))))

;; Signature: character-count: list-of-string->number
;; Interp: consumes a list of string and produces the total number of characters 
;;         in all strings of the list

(check-expect (character-count LOS1) 0)
(check-expect (character-count LOS2) 5)
(check-expect (character-count LOS3) 20)

(define (character-count los)
  (cond 
        [(empty? los) 0] 
        [(cons?  los) (+ (string-length (first los))
                         (character-count (rest los)))]))


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
;; Purpose: return true if every ListOfString contains at least one numeric character
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
;; Purpose: consume a list having 1 element at least and
;;          return true if the ListOfString contains only no elements left
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
