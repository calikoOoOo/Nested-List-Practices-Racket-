;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname nested) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))

;; A (nested-listof X) is one of:
;; * empty
;; * (cons (nested-listof X) (nested-listof X))
;; * (cons X (nested-listof X))
;; Requires: X itself is not a list type

;; a NotAList is an Any that is not a list type

;; question a)

;; nested-listof-X-template: (nested-listof X) -> Any
(define (nested-listof-X-template nlon)
  (cond [(empty? nlon) ...]
        [(list? (first nlon)) ...]
        [else ...]))

;; question b)

;; (nested-count lnon) produces the number of NotAList in the nested list
;; Examples:

(check-expect (nested-count '((1 2 3 4) "cat" (34 abc))) 7)
(check-expect (nested-count '((1 2 3 4 5))) 5)
(check-expect (nested-count '((3 4 5) 1 (34) (23))) 6)

;; nested-count: (nested-listof X) -> Nat
(define (nested-count nlon)
  (cond [(empty? nlon) 0]
        [(list? (first nlon)) (+ (nested-count (first nlon))
                                 (nested-count (rest nlon)))]
        [(list? nlon) (add1 (nested-count (rest nlon)))]))

;; Tests:
(check-expect (nested-count '(((1234 12) 89) 23)) 4)
(check-expect (nested-count '()) 0)

;; question c)

;; (nested-sum nlon) consumes a nested list of Numbers and produces
;; the sum of all the numbers in the nested list
;; Examples:

(check-expect (nested-sum '((1 2 3 4) 3)) 13)
(check-expect (nested-sum '((1 2 3 4))) 10)
(check-expect (nested-sum '(2 (40 5) 3)) 50)

;; nested-sum: (nested-listof Num) -> Num
(define (nested-sum nlon)
  (local [(define sum 0)
          ;; (sum-up nlon sum) produces the sum of all the numbers in a
          ;; nested list
          ;; sum-up: (nested-listof Num) Num -> Num
          (define (sum-up nlon sum)
            (cond [(empty? nlon) sum]
                  [(list? (first nlon)) (+ sum (nested-sum (first nlon))
                                           (nested-sum (rest nlon)))]
                  [else (sum-up (rest nlon) (+ sum (first nlon)))]))]
    (cond [(empty? nlon) sum]
          [else (sum-up nlon sum)])))

;; Tests:
(check-expect (nested-sum '(3 4 5 6 7)) 25)
(check-expect (nested-sum '()) 0)

;; question d)

;; (nested-member? x nlon) consumes a value and a nested list of NotAList
;; to produce true if the value is present in the nested list and false otherwise
;; Examples:

(check-expect (nested-member? 'ball '((towel) (pizza balloon) ball)) true)
(check-expect (nested-member? 3 '((1 2 4) 5 6)) false)
(check-expect (nested-member? "Ball" '((Flamingo "Ball"))) true)

;; nested-member? NotAList (nested-listof NotAList) -> Bool
(define (nested-member? x nlon)
  (cond [(empty? nlon) false]
        [(list? (first nlon)) (or (nested-member? x (first nlon))
                                  (nested-member? x (rest nlon)))]
        [(equal? x (first nlon)) true]
        [else (nested-member? x (rest nlon))]))

;; Tests:
(check-expect (nested-member? 2 '()) false)

;; question e)

;; (nested-ref nlon k) consumes a nested list of NotAList and a
;; natural number to produce the kth element that is NotAList in
;; the nested list
;; Examples:

(check-expect (nested-ref '((1 2 3) 4 5 (6)) 1) 2)
(check-expect (nested-ref '((dough cookies) 1 2 3 "cat") 5) "cat")
(check-expect (nested-ref '(1 2 3) 0) 1)

;; nested-ref: (nested-listof NotAList) Nat -> NotAList
(define (nested-ref nlon k)
  (cond [(and (= k 0) (not (list? (first nlon)))) (first nlon)]
        [(and (= k 0) (list? (first nlon))) (nested-ref (first nlon) k)]
        [(and (list? (first nlon)) (> k (nested-count (first nlon))))
         (nested-ref (rest nlon) (- k (nested-count (first nlon))))]
        [(and (list? (first nlon)) (< k (nested-count (first nlon))))
              (nested-ref (first nlon) k)]
        [else (nested-ref (rest nlon) (sub1 k))]))

;; Tests:
(check-expect (nested-ref '((1) 2 3) 0) 1)
          
;; question f)

;; (nested-filter pred? nlon) consumes a predicate function and a nested list
;; and produces the nested list with only the values that satisfy the predicate
;; function
;; Examples:

(check-expect (nested-filter even? '((1 (2 3 4) 5))) '(((2 4))))
(check-expect (nested-filter string? '((1 "cat" 4 5))) '(("cat")))
(check-expect (nested-filter zero? '((0) (2 3 4) 5)) '((0) ()))

;; nested-filter: (X -> Bool) (nested-listof X) -> (nested-listof X)
(define (nested-filter pred? nlon)
  (cond [(empty? nlon) empty]
        [(list? (first nlon))
         (cons (nested-filter pred? (first nlon)) (nested-filter pred? (rest nlon)))]
        [(pred? (first nlon)) (cons (first nlon) (nested-filter pred? (rest nlon)))]
        [else (nested-filter pred? (rest nlon))]))

;; Tests:
(check-expect (nested-filter even? '()) '())

;; question g)

;; (nested-cleanup nlon) consumes a nested list and produces the nested list
;; without the empty lists in it, if the nested list itself is empty, then
;; produce false
(check-expect (nested-cleanup '((0) ())) '((0)))
(check-expect (nested-cleanup '("cat" 1 2 3)) '("cat" 1 2 3))
(check-expect (nested-cleanup '(() ())) false)

;; nested-cleanup: (nested-listof X) -> (Anyof (nested-listof X) false)
(define (nested-cleanup nlon)
  (cond [(empty? nlon) false]
        [(and (not (list? (first nlon))) (false? (nested-cleanup (rest nlon))))
         (cons (first nlon) empty)]
        [(not (list? (first nlon))) (cons (first nlon) (nested-cleanup (rest nlon)))]
        [(list? (first nlon)) (cond [(and (false? (nested-cleanup (first nlon)))
                                          (false? (nested-cleanup (rest nlon))))
                                     false]
                                    [(false? (nested-cleanup (rest nlon)))
                                     (cons (nested-cleanup (first nlon)) empty)]
                                    [(false? (nested-cleanup (first nlon)))
                                     (nested-cleanup (rest nlon))]
                                    [else (cons (nested-cleanup (first nlon))
                                                (nested-cleanup (rest nlon)))])]))

;; Tests:
(check-expect (nested-cleanup '()) false)
(check-expect (nested-cleanup '((1) 2 3 4)) '((1) 2 3 4))
(check-expect (nested-cleanup '(1 2 3 4)) '(1 2 3 4))
(check-expect (nested-cleanup '(() a)) '(a))