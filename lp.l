;;; singleton-p: detects singleton list
(defun singleton-p (lst)
  (cond
   ( (null lst) nil )
   ( (null (cdr lst) ) t )
   ( t () )
  )
)

;;; rac: car backwards 
(defun rac (lst)
  (cond
   ( (singleton-p lst)
     (car lst) )
     (t
      (rac (cdr lst) )
      )
     )
   )

;;; rdc: cdr backwards
(defun rdc (lst)
  (cond
   ( (singleton-p lst)
     () )
   (t
    (cons (car lst) (rdc (cdr lst) ) )
    )
   )
  )

;;; snoc: cons backwards
(defun snoc (lo ll)
  (cond
   ( (null ll)
     (list lo) )
   (t
    (cons (car ll) (snoc lo (cdr ll) ) )
     )
    )
   )

;;; palindrome-p
(defun palindrome-p (lst)
  (cond
   ( (null lst)
     t
   )
   ( (singleton-p lst)
     t
   )
   ( (equal (car lst) (rac lst) )
     (palindrome-p (cdr (rdc lst) ) )
   )
   ( t nil)
   )
 )

;;; select: returns the element of the list in a given position
(defun select (i lst)
  (cond
   ( (null lst) nil)
   ( (equal i 0) (car lst) )
   (t
    (select (- i 1) (cdr lst) )
    )
   )
  )

;;; pick: uses select to pick a random element from the list
(defun pick (lst)
  (select (random (length lst) ) lst)
)

;;; sum
(defun sum (n1)
  (cond
   ( (null n1) 0)
   (t
    ( + (car n1) (sum (cdr n1) ) ) 
    )
   )
 )

;;; product
(defun product (n1)
  (cond
   ( (null n1) 1)
   (t
    (* (car n1) (product (cdr n1) ) )
   )
 )
)

;;; iota
(defun iota (n)
  (cond
   ( ( = n 0) () )
   ( t ( snoc n ( iota ( - n 1) ) ) )
  )
)

;;; duplicate: generates a list containing n instances of a lisp object
(defun duplicate (n lo)
  ( cond
    ( ( equal n 0 ) nil )
    ( ( equal n 1 ) ( list lo ) )
    ( t ( cons lo (duplicate ( - n 1 ) lo ) ) )
   )
  )

;;; factorial
(defun factorial (n)
  (product (iota n) )
  )

;;; power: raises n to the xth power
(defun power (n x)
  (product (duplicate n x) )
)

;;; filter-in: returns the set of elements which are true with respect to p
(defun filter-in (pred-p lst)
  (cond
   ( (null lst) nil)
   ( (funcall pred-p (car lst) )
    (cons (car lst) (filter-in pred-p (cdr lst) ) ) )
   ( t (filter-in pred-p (cdr lst) ) )
   )
 )

;;; filter-out: returns the set of elements which are false with respect to p
(defun filter-out (pred-p lst)
  (cond
   ( (null lst) nil)
   ( (funcall pred-p (car lst) )
     (filter-out pred-p (cdr lst) ) )
   (t (cons (car lst) (filter-out pred-p (cdr lst) ) ) )
   )
)
   
;;; take-from: returns lst with all occurrences of lo removed from it
(defun take-from (lo lst)
  (cond
   ( (null lst) nil)
   ( (equal lo (car lst) ) (take-from lo (cdr lst) ) )
   (t
     (cons (car lst) (take-from lo (cdr lst) ) ) 
    )
   )
 )

;;; random-permutation: returns a random permutation of lst
(defun random-permutation (lst)
  (cond
   ( (null lst) () )
   ( t
     (setf rand-elem (pick lst) )
     (setf remainder (take-from rand-elem lst ) )
     (cons rand-elem (random-permutation remainder) )
     )
   )
 )
     

