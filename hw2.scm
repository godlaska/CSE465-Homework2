#lang scheme
; ---------------------------------------------
; DO NOT REMOVE OR CHANGE ANYTHING UNTIL LINE 26
; ---------------------------------------------

; zipcodes.scm contains all the US zipcodes.
; This file must be in the same folder as hw2.scm file,
; and you should not modify it. Your code
; should work for other instances of this file.
(require "zipcodes.scm")

; Helper function
(define (mydisplay value)
	(display value)
	(newline)
)

; Helper function
(define (line func)
        (display "--------- ")
        (display func)
        (display " ------------")
        (newline)
)

; ================ Solve the following functions ===================
; Return a list with only the negatives items
(define (negatives lst)
	(if (null? lst)
      '() ; returns an empty list if null
      (if (> 0 (car lst)) ; checks if the first element of the list is negative
          (cons (car lst) (negatives (cdr lst))) ; if true, adds the odd element to the list and removes it
          (negatives (cdr lst)) ; if false, removes the element from the list
      )
  )
)

(line "negatives")
(mydisplay (negatives '()))  ; -> ()
(mydisplay (negatives '(-1)))  ; -> (-1)
(mydisplay (negatives '(-1 1 2 3 4 -4 5)))  ; -> (-1 -4)
(mydisplay (negatives '(1 1 2 3 4 4 5)))  ; -> ()
(line "negatives")
; ---------------------------------------------

; Returns true if the two lists have identical structure
; in terms of how many elements and nested lists they have in the same order
(define (struct lst1 lst2)
  (if (null? lst1) ; returns true if both are empty lists, else false
      (null? lst2) ; if lst1 is empty, go to this if statement
      (if (null? lst2) ; else condition
      #f ; returns false if lst1 has elements but lst2 doesn't
      (if (and (list? (car lst1)) (list? (car lst2))) ; both elements are lists
          (and (struct (car lst1) (car lst2)) ; recursively checks first elem list structure
               (struct (cdr lst1) (cdr lst2))) ; checks rest of the lists
                ; ELSE BRANCH
               (if (and (not (list? (car lst1))) (not (list? (car lst2)))) ; both elements are not lists
                   (struct (cdr lst1) (cdr lst2)) ; checks rest of the list
                   #f))) ; returns false if any conditions fail, true otherwise
  )
)

(line "struct")
(mydisplay (struct '(a b c (c a b)) '(1 2 3 (a b c))))  ; -> #t
(mydisplay (struct '(a b c d (c a b)) '(1 2 3 (a b c))))  ; -> #f
(mydisplay (struct '(a b c (c a b)) '(1 2 3 (a b c) 0)))  ; -> #f
(line "struct")
; ---------------------------------------------

; Returns a list of two numeric values. The first is the smallest
; in the list and the second is the largest in the list. 
; lst -- contains numeric values, and length is >= 1.
(define (minAndMax lst)
  (list (minLst lst) (maxLst lst))
)

; --- HELPER FUNCTIONS ---

; defines a function that returns the max value of a given list
(define (maxLst lst)
  (if (= 1 (length lst))
      (car lst) ; if there's only one element left, grabs that
      (max (car lst) (maxLst (cdr lst))) ; else recursively compares the list, removing the last element
  )
)

; defines a function that returns the min value of a given list
(define (minLst lst)
  (if (= 1 (length lst))
      (car lst) ; if there's only one element left, grabs that
      (min (car lst) (minLst (cdr lst))) ; else recursively compares the list, removing the last element
  )
)
; ------------------------

(line "minAndMax")
(mydisplay (minAndMax '(1 2 -3 4 2)))  ; -> (-3 4)
(mydisplay (minAndMax '(1)))  ; -> (1 1)
(line "minAndMax")
; ---------------------------------------------

; Returns a list identical to the first list, while having all elements
; that are inside nested loops taken out. So we want to flatten all elements and have
; them all in a single list. For example '(a (a a) a))) should become (a a a a)
(define (flatten lst)
  (if (null? lst)
      '() ; return empty list
      (if (list? (car lst)) ; check if current element is a list
          (append (flatten (car lst)) (flatten (cdr lst))) ; if the element is a list, recursively flatten the list and then remove it 
          (cons (car lst) (flatten (cdr lst))) ; if not a list, add to constructing list
      )
  )
)

(line "flatten")
(mydisplay (flatten '(a b c)))  ; -> (a b c)
(mydisplay (flatten '(a (a a) a)))  ; -> (a a a a)
(mydisplay (flatten '((a b) (c (d) e) f)))  ; -> (a b c d e f)
(line "flatten")
; ---------------------------------------------

; The paramters are two lists. The result should contain the cross product
; between the two lists: 
; The inputs '(1 2) and '(a b c) should return a single list:
; ((1 a) (1 b) (1 c) (2 a) (2 b) (2 c))
; lst1 & lst2 -- two flat lists.
(define (crossproduct lst1 lst2)
  (if (null? lst1)
      '() ; return empty list
      (cons (crossproduct-helper (car lst1) lst2)  ; Pair (car lst1) with every element of lst2
            (crossproduct (cdr lst1) lst2))  ; Recursively pair with the rest of lst1
  )
)

; --- HELPER FUNCTION ---
(define (crossproduct-helper lst1Elem lst2)
  (if (null? lst2)
      '() ; return empty list
      (cons (list lst1Elem (car lst2)) ; creates a pair of element from lst1 with lst2
            (crossproduct-helper lst1Elem (cdr lst2))) ; recursively pairs the rest of lst2 elements
  )
)
; ------------------------

(line "crossproduct")
(mydisplay (crossproduct '(1 2) '(a b c)))
(line "crossproduct")
; ---------------------------------------------

; Returns the first latitude and longitude of a particular zip code.
; if there are multiple latitude and longitude pairs for the same zip code,
; the function should only return the first pair. e.g. (53.3628 -167.5107)
; zipcode -- 5 digit integer
; zips -- the zipcode DB- You MUST pass the 'zipcodes' function
; from the 'zipcodes.scm' file for this. You can just call 'zipcodes' directly
; as shown in the sample example
(define (getLatLon zipcode zips)
  (if (null? zips)
      '()
      (let ((currentZip (car zips))) ; gets the first elem in zipcodes, which is the zipcode
        (if (equal? zipcode (car currentZip))
            ; list-ref information provided by https://groups.csail.mit.edu/mac/ftpdir/scheme-7.4/doc-html/scheme_8.html
            (list (list-ref currentZip 4) (list-ref currentZip 5)) ; gets the 4th and 5th element from the list which are lat and lon
            (getLatLon zipcode (cdr zips)) ; recurses through until correct zip code is found (if found)
        )
      )
  )
)

(line "getLatLon")
(mydisplay (getLatLon 45056 zipcodes))
(line "getLatLon")
; ---------------------------------------------

; Returns a list of all the place names common to two states.
; placeName -- is the text corresponding to the name of the place
; zips -- the zipcode DB
(define (getCommonPlaces state1 state2 zips)
  (define places1 (getStatePlaces state1 zips))
  (define places2 (getStatePlaces state2 zips))
  (define (find-common places1 result)
    (cond
      ((null? places1) result)
      ((place-in-list? (car places1) places2)
       (find-common (cdr places1) (cons (car places1) result)))
      (else (find-common (cdr places1) result))))
  (find-common places1 '()))

; ChatGPT provided assistance in this method's coding, namely the loops and
; and recursion logic.

; Helper function to check if the state in the entry matches the given state
(define (state-matches? entry state)
  (equal? (caddr entry) state))  ; The state is the third element in the entry

; Helper function to extract the place name from an entry
(define (get-place entry)
  (cadr entry))  ; The place name is the second element in the entry

; Helper function to filter the zip database by state and extract place names
(define (getStatePlaces state zips)
  (define (loop entries result)
    (cond
      ((null? entries) result)
      ((state-matches? (car entries) state)
       (loop (cdr entries) (cons (get-place (car entries)) result)))
      (else (loop (cdr entries) result))))
  (loop zips '()))

; Helper function to check if a place is in the list
(define (place-in-list? place places)
  (cond
    ((null? places) #f)
    ((equal? place (car places)) #t)
    (else (place-in-list? place (cdr places)))))

(line "getCommonPlaces")
(mydisplay (getCommonPlaces "OH" "MI" zipcodes))
(line "getCommonPlaces")
; ---------------------------------------------

; Returns the number of zipcode entries for a particular state.
; state -- state
; zips -- zipcode DB
(define (zipCount state zips)
 

(line "zipCount")
(mydisplay (zipCount "OH" zipcodes))
(line "zipCount")
; ---------------------------------------------

; Some sample predicates
(define (POS? x) (> x 0))
(define (NEG? x) (< x 0))
(define (LARGE? x) (>= (abs x) 10))
(define (SMALL? x) (not (LARGE? x)))

; Returns a list of items that satisfy a set of predicates.
; For example (filterList '(1 2 3 4 100) '(EVEN?)) should return the even numbers (2 4 100)
; (filterList '(1 2 3 4 100) '(EVEN? SMALL?)) should return (2 4)
; lst -- flat list of items
; filters -- list of predicates to apply to the individual elements

(define (filterList lst filters)


(line "filterList")
(mydisplay (filterList '(1 2 3 11 22 33 -1 -2 -3 -11 -22 -33) (list POS?)))
(mydisplay (filterList '(1 2 3 11 22 33 -1 -2 -3 -11 -22 -33) (list POS? even?)))
(mydisplay (filterList '(1 2 3 11 22 33 -1 -2 -3 -11 -22 -33) (list POS? even? LARGE?)))
(line "filterList")
; ---------------------------------------------