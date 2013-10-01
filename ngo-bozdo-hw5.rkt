;; Students:  Hoang Ngo & Marilda Bozdo
;; Usernames: hmngo     & mbozdo
;; ----------------------------------------

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 1: Write the data definition(s) needed for this binary search tree.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Data definition for BST

;; A BST is either
;;      - 'unknown
;;      - (make-book title authors cost n-sold isbn ltbt rtbt) in which
;;            title & authors       is a string
;;            cost  & n-sold & isbn is a number
;;            ltbt  & rtbt          is a BST
;;            ltbt contains books that have smaller ISBN than current one's
;;            rtbt contains books that have bigger  ISBN than current one's


; Data definition for book

(define-struct book (title authors cost n-sold isbn ltbt rtbt))
;; a book is (make-book String ListOfAuthor Number Number Number book-node book-node)
;; interp. (make-book title authors cost n-sold isbn ltbt rtbt) is a book that has
;;        title   as the title of the book
;;        authors as the list of authors of the book
;;        cost    as the cost of the book
;;        n-sold  as the number of copies sold
;;        ltbt    as a BST
;;        rtbt    as a BST


; Data definition for ListOfAuthor

;; ListOfAuthor is one of
;;      - empty
;;      - (cons String ListOfAuthor)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 2: Provide an example of binary search tree containing at least 5 books. 
;;            Make sure you construct your example so that the items in the tree are 
;;            ordered according to the binary search tree property, on the ISBN number.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define BST1 (make-book "A Christmas Carol" (cons "Charles Dickens" empty) 50 10000 10                  ; Root 0
                    (make-book "Oliver Twist"  (cons "Charles Dickens" empty) 50 20000 6                ; 0. Left branch 1
                               'unknown 
                               'unknown)
                    (make-book "Great Expectations" (cons "Charles Dickens" empty) 50 50000 15          ; 0. Right branch 1
                            (make-book "David Copperfield" (cons "Charles Dickens" empty) 50 20000 11   ; R1. Left branch 2
                                        'unknown 
                                        'unknown)
                            (make-book "Bleak House" (cons "Charles Dickens" empty) 50 90000 20         ; R1. Right branch 2
                                        'unknown 
                                        'unknown))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 3: Write the template(s) for the data definition(s) in Problem 1.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Template for book

(define (fun-for-book abook)
    (... (book-title                    abook)
         (fun-for-author (book-authors  abook))
         (book-cost                     abook)
         (book-n-sold                   abook)
         (book-isbn                     abook)
         (fun-for-bst (book-ltbt        abook)
         (fun-for-bst (book-rtbt        abook)))))

       
; Template for ListOfAuthor

(define (fun-for-author loa)
    (cond
        [(empty? loa) (...)]
        [(cons?  loa) (... (first loa)
                           (fun-for-author (rest loa)))]))
                           
                           
; Template for BST

(define (fun-for-bst abst)
    (cond 
        [(symbol? abst) (...)]
        [(book?   abst) (... (book-title                    abook)
                             (fun-for-author (book-authors  abook))
                             (book-cost                     abook)
                             (book-n-sold                   abook)
                             (book-isbn                     abook)
                             (fun-for-bst (book-ltbt        abook))
                             (fun-for-bst (book-rtbt        abook)))]))
   
   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 4: Write a function increase-price that consumes a binary search tree of books and 
;;	a number representing a per cent increase and 
;;            produces a binary search tree the same as the original 
;;	except that the cost of each book in the tree has been increased by the given percentage.                            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Signature: increase-price: binary-search-tree number -> binary-search-tree
;; interp: consumes a binary search tree of books and a percentage and produces 
;;  a binary search tree same as the original but with cost of book increased by the percentage given 


(define (increase-price abst percentage)
    (cond 
        [(symbol? abst) 'unknown ]
        [(book?   abst) (make-book 
                        (book-title abst)
                        (book-authors  abst)
                        (* (book-cost abst)(+ 1 percentage))
                        (book-n-sold abst)
                        (book-isbn abst)
                    (increase-price (book-ltbt abst) percentage)
                    (increase-price (book-rtbt abst) percentage))]))


(check-expect (increase-price 'unknown 0.2) 'unknown)
(check-expect (increase-price BST1 0.1) (make-book "A Christmas Carol" (cons "Charles Dickens" empty) 55 10000 10                  
                                         (make-book "Oliver Twist"  (cons "Charles Dickens" empty) 55 20000 6 'unknown 'unknown)
                                          (make-book "Great Expectations" (cons "Charles Dickens" empty) 55 50000 15          
                                          (make-book "David Copperfield" (cons "Charles Dickens" empty) 55 20000 11 'unknown 'unknown)
                                           (make-book "Bleak House" (cons "Charles Dickens" empty) 55 90000 20 'unknown 'unknown))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 5: Write a function copies-sold which consumes a binary search tree and an ISBN number, 
;;            and returns the number of copies sold for the book with the given ISBN. 
;;            If a book with the given ISBN doesn't exist in the tree, the function should return -1. 
;; Your function should be written efficiently, 
;;                  such that it performs as few comparisons as is necessary to find the correct ISBN number in the tree.                           
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Signature: copies-sold: binary-search-tree number -> number
;;interp: consumes a binary search tree and an ISBN and produces the number of copies sold for the book with the given ISBN

(define (copies-sold abst ISBN)
    (cond 
        [(symbol? abst) -1]
        [(book?   abst) 
         (cond
          [(= (book-isbn abst) ISBN) (book-n-sold abst)]
          [(< (book-isbn abst) ISBN) (copies-sold (book-rtbt abst) ISBN)]
          [(> (book-isbn abst) ISBN) (copies-sold (book-ltbt abst) ISBN)])]))
          

(check-expect (copies-sold BST1 11) 20000) 
(check-expect (copies-sold 'unknown 21) -1)   
(check-expect (copies-sold BST1 10) 10000)
(check-expect (copies-sold BST1 20) 90000)
(check-expect (copies-sold BST1 70) -1)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 6: Write a function author-of-book? which consumes a binary search tree,
;;                                                            an ISBN number, 
;;                                                            and the name of an author. 
;;            The function returns true if one of the authors of the book with the given ISBN number has the given name. 
;;            If a book with the given ISBN doesn't exist in the tree, the function should return false. 
;; Your function should be written efficiently, 
;; such that it performs as few comparisons as is necessary to find the correct ISBN number in the tree.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ---------------------------------------------- HELPER FUNCTIONS -------------------------------------------------------
;; is-isbn? : number book -> boolean
;; Purpose: return true if the book's isbn is the same as the given isbn
;;                 false otherwise

(check-expect (is-isbn? 12 BST1) false)     ; BST1: 10, 6, 15, 11, 20
(check-expect (is-isbn? 10 BST1) true)

(define (is-isbn? isbn abook)
    (= isbn
       (book-isbn abook)))
      
      
;; has-author? : string book -> boolean
;; Purpose: return true if the book's list of authors has the given author
;;                 false otherwise

(check-expect (has-author? "Stephen Crane"   BST1) false)       ; BST1: (cons "Charles Dickens" empty)
(check-expect (has-author? "Charles Dickens" BST1) true)

(define (has-author? author abook)
    (member author (book-authors abook)))
                 
                          
;; -------------------------------------------------- MAIN FUNCTION ------------------------------------------------------
;; author-of-book? : BST number string -> boolean
;; Purpose: find the book that has the given isbn first (take advantage of short-circuit evaluation), 
;;              and return false if couldn't find. If found, then
;;          find the given author in the book's list of authors, 
;;              return true if found and false otherwise

(check-expect (author-of-book? 'unknown 2  "ABC")              false)           ; Empty tree
(check-expect (author-of-book? BST1     99 "ABC")              false)           ; non-existed & too big isbn
(check-expect (author-of-book? BST1     1  "ABC")              false)           ; non-existed & too small isbn
(check-expect (author-of-book? BST1     2  "Charles Dickens")  false)           ; non-existed isbn & existed author
(check-expect (author-of-book? BST1     6  "Charles Dickens")  true)            ; book of left branch
(check-expect (author-of-book? BST1     20 "Charles Dickens")  true)            ; book of right branch

(define (author-of-book? abst isbn author)
    (cond
        [(symbol? abst) false]
        [(book?   abst) (cond 
                             [(< isbn (book-isbn abst)) (author-of-book? (book-ltbt abst) isbn author)]
                             [(> isbn (book-isbn abst)) (author-of-book? (book-rtbt abst) isbn author)]
                             [else (has-author? author abst)])]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 7: Write a function add-new-book. 
;;            The function consumes a binary search tree, an ISBN, title, a list of authors, and a price
;;                         and adds a new book with the given information to the binary search tree. 
;;            The new book has zero copies sold. Make sure that the tree that is produced is a binary search tree. 
;; You may assume that the ISBN number of the book to be added does not already exist in the given tree. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define INIT-COPIES-SOLD 0)

;; ---------------------------------------------- HELPER FUNCTIONS -------------------------------------------------------
;; insert : string ListOfAuthor number number book -> book
;; Purpose: insert a new book with given information to a binary search tree

(define (insert title authors price isbn abook)
    (if (< isbn (book-isbn abook))
        (cond
            [(symbol? (book-rtbt abook)) (add-to-tree "left"
                                                      (make-book title authors price INIT-COPIES-SOLD isbn 
                                                                'unknown 'unknown)
                                                      abook)]
            [(book?   (book-rtbt abook)) (add-to-tree "left"
                                                      (insert title authors price isbn 
                                                              (book-ltbt abook))
                                                      abook)])
        (cond
            [(symbol? (book-ltbt abook)) (add-to-tree "right"
                                                      (make-book title authors price INIT-COPIES-SOLD isbn 
                                                                'unknown 'unknown)
                                                      abook)]
            [(book?   (book-ltbt abook)) (add-to-tree "right"
                                                      (insert title authors price isbn 
                                                              (book-rtbt abook))
                                                      abook)])))

;; add-to-tree : String book book -> book
;; Purpose: make a new book based on the right given book that has
;;              the left given book added to either left branch or right branch of the binary search tree
;;          depending on the value of the string "left" or "right"

(define (add-to-tree branch book1 book2)
    (cond
        [(string=? "left" branch) (make-book (book-title                   book2)
                                             (book-authors                 book2)
                                             (book-cost                    book2)
                                             (book-n-sold                  book2)
                                             (book-isbn                    book2)
                                             book1                                      ;; Add to left
                                             (book-rtbt                    book2))]
        [(string=? "right" branch) (make-book (book-title                   book2)
                                              (book-authors                 book2)
                                              (book-cost                    book2)
                                              (book-n-sold                  book2)
                                              (book-isbn                    book2)
                                              (book-ltbt                    book2)
                                              book1)]))                                 ;; Add to right
        

;; -------------------------------------------------- MAIN FUNCTION ------------------------------------------------------
;; add-new-book : BST Number string ListOfAuthor Number -> BST
;; Purpose: add a new book with given information to proper branch of a binary search tree

(check-expect (add-new-book BST1 7 "The Old Curiosity Shop" (cons "Charles Dickens" empty) 50)
              (make-book "A Christmas Carol" (cons "Charles Dickens" empty) 50 10000 10
                        (make-book "Oliver Twist"  (cons "Charles Dickens" empty) 50 20000 6 
                                   'unknown 
                                   (make-book "The Old Curiosity Shop" (cons "Charles Dickens" empty) 50 0 7 
                                             'unknown 
                                             'unknown))
                         (make-book "Great Expectations" (cons "Charles Dickens" empty) 50 50000 15
                                   (make-book "David Copperfield" (cons "Charles Dickens" empty) 50 20000 11 
                                              'unknown 
                                              'unknown)
                                   (make-book "Bleak House" (cons "Charles Dickens" empty) 50 90000 20 
                                              'unknown 
                                              'unknown))))
                                        
(check-expect (add-new-book BST1 25 "The Old Curiosity Shop" (cons "Charles Dickens" empty) 50)
              (make-book "A Christmas Carol" (cons "Charles Dickens" empty) 50 10000 10
                          (make-book "Oliver Twist" (cons "Charles Dickens" empty) 50 20000 6 
                                     'unknown 
                                     'unknown)
                          (make-book "Great Expectations" (cons "Charles Dickens" empty) 50 50000 15
                                     (make-book "David Copperfield" (cons "Charles Dickens" empty) 50 20000 11 
                                                'unknown 
                                                'unknown)
                                     (make-book "Bleak House" (cons "Charles Dickens" empty) 50 90000 20 
                                                'unknown 
                                                (make-book "The Old Curiosity Shop" (cons "Charles Dickens" empty) 50 0 25 
                                                           'unknown 
                                                           'unknown)))))
                                         
(check-expect (add-new-book BST1 12  "The Old Curiosity Shop" (cons "Charles Dickens" empty) 50)
              (make-book "A Christmas Carol" (cons "Charles Dickens" empty) 50 10000 10
                         (make-book "Oliver Twist" (cons "Charles Dickens" empty) 50 20000 6 
                                    'unknown 
                                    'unknown)
                         (make-book "Great Expectations" (cons "Charles Dickens" empty) 50 50000 15
                                    (make-book "David Copperfield" (cons "Charles Dickens" empty) 50 20000 11 
                                               'unknown 
                                               (make-book "The Old Curiosity Shop" (cons "Charles Dickens" empty) 50 0 12 
                                                          'unknown 
                                                          'unknown))
                                    (make-book "Bleak House" (cons "Charles Dickens" empty) 50 90000 20 
                                               'unknown 
                                               'unknown))))


(define (add-new-book abst isbn title authors price)
    (cond 
        [(symbol? abst) (make-book title authors price INIT-COPIES-SOLD isbn 'unknown 'unknown)]
        [(book?   abst) (insert title authors price isbn abst)]))
        
         
