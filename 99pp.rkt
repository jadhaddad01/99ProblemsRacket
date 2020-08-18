#lang racket
;;Extra methods
(define (append lst1 lst2) ;;append 2 lists function
  (foldr cons lst2 lst1))




;;1rst problem: Find the last box of a list.
(define (lastElemList lst) ;; we can only input 1 list
  (if (list? lst)
      (cond
        [(null? lst) '()] ;;if list is empty we return nothing
        [(null? (cdr lst)) (car lst)] ;;if there is only one item in the list we return that item
        [else (lastElemList (cdr lst))] ;;if there are more than one items in the list we redo the function with the same list minus the first element
      )

      '() ;;return null if not a list
  )
)
;;Examples:
          ;;(lastElemList '()) --> '()
          ;;(lastElemList '(1)) --> 1
          ;;(lastElemList '(1 2 3)) --> 3
          ;;(lastElemList '(1 2 1 a)) --> 'a


;;2nd problem: Find the last but one box of a list.
(define (twoLastElemList lst) ;; we can only input 1 list
  (if (list? lst)
      (cond
        [(null? lst) '()] ;;if list is empty we return nothing
        [(null? (cdr lst)) '()] ;;if only one element is present we return null
        [(null? (cddr lst)) (list (car lst) (cadr lst))] ;;if there is only two items in the list we return these items
        [else (twoLastElemList (cdr lst))] ;;if there are more than one items in the list we redo the function with the same list minus the first element
      )

      '() ;;return null if not a list
  )
)
;;Examples:
          ;;(twoLastElemList '()) --> '()
          ;;(twoLastElemList '(1)) --> '()
          ;;(twoLastElemList '(1 2)) --> '(1 2)
          ;;(twoLastElemList '(1 2 3 4 5)) --> '(4 5)


;;3rd problem: Find the K'th element of a list. (First element is 1)
(define (kElem lst k)
  (if (and (number? k) (list? lst))
      (cond
        [(< k 1) '()] ;;if k is less than 1 we return nothing
        [(null? lst) '()] ;;if nothing is in the list we return nothing / logically if k is bigger than the length of the list it will return null
        [(= 1 k) (car lst)] ;;if we get to 1 then we return the first element of the list
        [else (kElem (cdr lst) (- k 1))] ;;we dimish k by 1 and the list by its first elem
      )

      '() ;;return null if lst not a list OR k not a number
  )
)
;;Examples:
          ;; (kElem '() 1) --> '()
          ;; (kElem '(1 2 3) -5) --> '()
          ;; (kElem '(1 2 3) 6) --> '()
          ;; (kElem '(1 2 3 4 5) 2) --> 2


;;4th problem: Find the number of elements of a list.
(define (lengthAlternateFunction lst)
  (if (list? lst)
      (cond
        [(null? lst) 0] ;;if list is empty we return 0
        [else (+ 1 (lengthAlternateFunction (cdr lst)))] ;;add 1 and remove the first element of list and restart
      )

      0 ;;return 0 if not a list
  )
)
;;Examples:
          ;; (lengthAlternateFunction '()) --> 0
          ;; (lengthAlternateFunction 'a) --> 0
          ;; (lengthAlternateFunction '(1 2 3)) --> 3
          ;; (lengthAlternateFunction '(1 2 3 4 5)) --> 5


;;5th problem: Reverse a list.
(define (reverseList lst)
  (if (list? lst)
      (cond
        [(null? lst) '()]
        [else (append (reverseList (cdr lst)) (cons (car lst) '()))]
      )

      '() ;;return null if not a list
  )
)
;;Examples:
          ;; (reverseList '()) --> '()
          ;; (reverseList '(1)) --> '(1)
          ;; (reverseList '(1 2 3)) --> '(3 2 1)
          ;; (reverseList '(1 2 3 4 5)) --> '(5 4 3 2 1)


;;6th problem: Find out whether a list is a palindrome. (can be read forward or backwards)
(define (palindrome lst))