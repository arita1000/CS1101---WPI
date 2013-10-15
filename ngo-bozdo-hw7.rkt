;; Students:  Hoang Ngo & Marilda Bozdo
;; Usernames: hmngo     & mbozdo
;; --------------------------------------------

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 1: Provide a data definition for a person. 
;;		  Each person has a name, an email address, and a list of friends (persons).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; a person is (make-person string string ListOfPerson)
(define-struct person (name email friend-list))


;; ListOfPerson is one of
;;      - empty
;;      - (cons person ListOfPerson)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 2: Define a variable for a social network called network that will hold a list of persons 
;;            (it should initially be empty).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define NETWORK empty)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 3: Write a function create-person that consumes a person's name and email address and 
;;                    returns a new person with the given name and email (and no friends). 
;;            The new person should also be added to the network. 
;;            You may assume the named person does not already exist in the network.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; --------------------------------------------------- CONSTANTS ---------------------------------------------------------
(define INIT-FRIEND-LIST empty)


;; ------------------------------------------------- MAIN FUNCTION -------------------------------------------------------
;; create-person: string string -> person
;; Interp. consume a person's name and email
;;         return a new person
;; EFFECT: add a person with given info into NETWORK

(define (create-person name email)
      (begin
        (set! NETWORK (cons (make-person name email INIT-FRIEND-LIST) NETWORK))
        (first NETWORK)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 4: Use create-person to populate the social network network with at least 5 different persons. 
;;            (You should "define" these new persons so you can refer to them in later exercises.)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define NATE  (create-person "Nate"   "nate@wpi.edu"))
(define JULIA (create-person "Julia" "julia@wpi.edu"))
(define PETER (create-person "Peter" "peter@wpi.edu"))
(define ERIK  (create-person "Erik"   "erik@wpi.edu"))
(define LUKE  (create-person "Luke"   "luke@wpi.edu"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 5: Write a function list-names-in-network that doesn't consume anything and 
;;            produces a list of the names of all people in the network. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Signature: list-names-in-network:    ->ListOfString
;; interp: the function should consume nothing and produce the list of all people in the network

(define (list-names-in-network)
  (map person-name NETWORK))

(check-expect (list-names-in-network) (list "Luke" "Erik" "Peter" "Julia" "Nate"))

      

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 6: Write a function list-all-names that doesn't consume anything and 
;;            produces a list of the names of all people in the network 
;;            (same signature and purpose as the previous problem). 
;;            This time, you must use accumulator-style programming to solve the problem.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Signature: list-all-names:    ->ListOfString
;; interp: the function should consume nothing and produce the list of all people in the network
(define (list-all-names)
  (name-accum NETWORK empty))

;; Signature: name-accum: ListOfPeople ListOfString -> ListOfString
;; interp: consume a list of people and produce all their names  by remembering the list of names so far

(define (name-accum alop names-so-far)
  (cond [(empty? alop) names-so-far]
        [(cons? alop) (name-accum (rest alop) (cons (person-name (first alop)) names-so-far))]))


(check-expect (list-all-names) (list "Nate" "Julia" "Peter" "Erik" "Luke"))
                                                       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 7: Write a function friend that consumes two persons, and 
;;            makes them each a friend of the other. 
;; (A person A is a friend of person B if person A's friend list contains B and vice versa.) 
;; You may assume that the given persons are not already friends, and that both persons exist in the network. 
;; Your function should return void.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 8: Write a function find-person that consumes the name of a person and 
;;                returns that person if the person exists in the network, or returns the string "not found" otherwise.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ------------------------------------------------- MAIN FUNCTION -------------------------------------------------------
;; find-person : string -> person
;; Interp. consume the name of a person and
;;         return the person who has that name in NETWORK or
;;                "not found" if there is no person having that name

(define (find-person name)
      (find-person-in-list name NETWORK))
            
            
;; ------------------------------------------------ HELPER FUNCTIONS -----------------------------------------------------
;; find-person-in-list : string ListOfPerson -> person
;; Interp. consume name of a person and a list of people
;;         return that person if that person exists in the list
;;         return "not found" if that person doesn't exist in the list

(define (find-person-in-list name alop)
      (cond 
            [(empty? alop) (error "not found")]
            [(cons?  alop) (if (string=? name (person-name (first alop)))
                               (first alop)
                               (find-person-in-list name (rest alop)))]))
                                           

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 9: Write a function most-social that consumes nothing and 
;;                      returns the person with the most friends (resolve ties arbitrarily). 
;;                      Return "empty network" if the network doesn't contain any persons. 
;;            You may use the built-in Racket function length.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 10: Write a function change-email that consumes the name of a person and a new email address and 
;;             changes that person's email address to the new one. 
;;             Of course, the change should also show up in all occurrences of the named person in any of the friends lists
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; change-email : string string -> void
;; Interp. change email of a given person to a new one
;; EFFECT: change email of one person in NETWORK

(define (change-email name new-email)
      (set-person-email! (find-person name) new-email))
