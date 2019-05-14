;;; Ken-Ken Generator and Solver
;;; Author: Emma Brunell
;;; Spring 2019

(load "lp.l")
(load "simple_viewer_agent.l")


; ---------------------------------------------------
; Global Variables
; ---------------------------------------------------
(setf *coords* '() )
(setf *cages* '() )
(setf *board* '() )
(setf *full-board* '() )
(setf *size* '() )
(setf *targets* '() )
(setf *generated-game* '() )
(setf *written-sols* '() )

; -----------------------------------------------------
; Task 1: generate an nxn latin square of a given size 
; -----------------------------------------------------

; create a randomized latin square, list in order from bottom to top
(defmethod latin-square (size &aux result sublist x y)
  (setf result '() )
  (setf *size* size)
  (loop for line from 1 to size do
	(setf sublist '() )
	(loop for col from 1 to size do
	      (setf x (- size line) )
	      (setf y (+ 1 (mod (+ x col) size) ) )
	      (setf sublist (push y sublist) )
	      finally (push sublist result)
	)
   )
  (setf *board* (random-permutation result) )
  *board*
)

; a random size for the latin square from size n=3 to n=6
(defmethod random-size ()
  (+ 3 (random 5) )
)

; -----------------------------------------------------------------------------------
; Task 2: Create various sizes/shapes of cages that will fit within the latin square
; -----------------------------------------------------------------------------------

; set up the grid coordinates for the given nxn size
(defmethod grid-coords (square &aux size coord)
  (setf size (length (car square) ) )
  (setf coord '() )
  (loop for row from 1 to size do
	(loop for col from 1 to size do
	      (push (cons col row) coord)
	)
  )
  (setf *coords* (reverse coord))
)

; determine if cells are neighbors with each other
(defmethod get-neighbors (cell &aux neighbors)
  (setf neighbors
	(list (north-neighbor cell) (south-neighbor cell)
	      (west-neighbor cell) (east-neighbor cell)
	)
  )
  (take-from 'nil neighbors)
)

; get the neighbor above a cell
(defmethod north-neighbor (cell &aux neighbor y)
  (setf y (+ 1 (cdr cell) ) )
  (setf neighbor (cons (car cell) y) )
  (cond
   ( (member neighbor *coords* :test 'equal) neighbor)
   (t nil)
  )
)

; get the neighbor below a cell
(defmethod south-neighbor (cell &aux neighbor y)
  (setf y (- (cdr cell) 1 ) )
  (setf neighbor (cons (car cell) y) )
  (cond
   ( (member neighbor *coords* :test 'equal) neighbor)
   (t nil)
  )
)

; get the neighbor to the left of a cell
(defmethod west-neighbor (cell &aux neighbor x)
  (setf x (- (car cell) 1 ) )
  (setf neighbor (cons x (cdr cell) ) )
  (cond
   ( (member neighbor *coords* :test 'equal) neighbor)
   (t nil)
  )
)

; get the neighbor to the right of a cell
(defmethod east-neighbor (cell &aux neighbor x)
  (setf x (+ 1 (car cell) ) )
  (setf neighbor (cons x (cdr cell) ) )
  (cond
   ( (member neighbor *coords* :test 'equal) neighbor)
   (t nil)
  )
)

; randomly generate polyominoes up to size n that will fit within the square
(defmethod polyominoes (coords &aux avail p1 p2 p3 cage cages)
  (setf avail coords)
  (setf cages '() )
  (loop for i in *coords* do
	(setf cage '() )
	(setf p1 (pick (get-neighbors i) ) )
	(if (member p1 avail :test #'equal)
	    (progn  (and (push p1 cage)
		     (setf avail (remove p1 avail :test #'equal) )
		     )
		    (setf p2 (pick (get-neighbors p1) ) )
	            (if (member p2 avail :test #'equal)
	              (if (not (eq p2 nil) )
		        (progn (push p2 cage)
			       (setf avail (remove p2 avail :test #'equal) )
			       (if (not (eq p2 nil) )
				   (and
				    (setf p3 (pick (get-neighbors p2) ) )
				    (if (member p3 avail :test #'equal)
					(progn
					  (if (not (eq p3 nil) )
					      (progn (push p3 cage)
						     (setf avail (remove p3 avail :test #'equal) )
					       )
					    )
					  )
				      )
				    )
				 )
			    )
			)
		     )
	       )
	)
	(push cage cages)
  )
  (loop for i in avail do
	(push (list i) cages)
  )
  (setf *cages* (take-from 'nil cages) )
)


; -----------------------------------------------------------------------------------
; Task 3: Populate cages with Latin Square Contents
; -----------------------------------------------------------------------------------

; finds the contents of one coordinate with respect to the existing board
(defmethod get-square (coord &aux x y pos)
  (setf x (- (car coord) 1) )
  (setf y (- (cdr coord) 1) )
  (setf pos (nth y (nth x *board*) ) )
  pos
)

; relate each square content with its appropriate cage coordinate
(defmethod populate-cages (&aux cages cage)
  (setf cages '())
  (loop for i in *cages* do
	(cond
	  ( (eq (length i) 1 )
	   (and
	    (setf cage (list (get-square (car i) ) (car i) ) )
	    (push cage cages)
	   )
	  )
	  ( (eq (length i) 2 )
	    (and
	     (setf cage (list (list (get-square (car i) ) (car i) )
			      (list (get-square (cadr i) ) (cadr i) ) ) )
	     (push cage cages)
	     )
	  )
	  ( (eq (length i) 3 )
	    (and
	     (setf cage (list (list (get-square (car i) ) (car i) )
			      (list (get-square (cadr i) ) (cadr i) )
			      (list (get-square (caddr i) ) (caddr i) ) ) )
	     (push cage cages)
	     )
	  )
	)
  )
  (setf *full-board* cages)
)

; -----------------------------------------------------------------------------------
; Task 4: Mathematical relationships
; -----------------------------------------------------------------------------------

; choose a random operator out of +, -, *, and /
(defmethod random-op (size &aux ops)
  (setf ops '() )
  (cond
   ( (eq 2 size) (setf ops '(+ - * /) ) )
   ( t (setf ops '(+ - *) ) )
  )
  (pick ops)
)

; provide a simplified numeric list of numbers in cage form
(defmethod condense-cages (&aux cages cage)
  (setf cages '())
  (loop for i in *cages* do
	(cond
	  ( (eq (length i) 1 )
	   (and
	    (setf cage (list (get-square (car i) ) ) )
	    (push cage cages)
	   )
	  )
	  ( (eq (length i) 2 )
	    (and
	     (setf cage (list (get-square (car i) )
			      (get-square (cadr i) ) ) )
	     (push cage cages)
	     )
	  )
	  ( (eq (length i) 3 )
	    (and
	     (setf cage (list (get-square (car i) )
			      (get-square (cadr i) )
			      (get-square (caddr i) ) ) )
	     (push cage cages)
	     )
	  )
       )
    )
  cages
)

; apply the random operator to each cage
(defmethod apply-op (&aux op c target targets)
  (setf c (condense-cages) )
  (setf *targets* '() )
  (loop for i in c do
      (cond
       ( (eq (length i) 1)
	   (setf i (cons '~ i) )
	 )
	 ( t
	   (setf i (cons (valid-op i) i) )
	 )
	 )
      (setf op (car i) )
      (cond
       ( (not (eq op '~) ) (setf target (list (eval i) op ) ) )
       (t (setf target (list (cadr i) op) ) )
      )
      (push target *targets*)
      )
  (setf *targets* (reverse *targets*) )
  (place-targets *targets*)
)

; check to make sure the operator applied to the cage will create a valid target number
(defmethod valid-op (cage &aux op res)
  (setf op (random-op (length cage) ) )
  (setf res (apply op (sort cage '>) ) )
  (cond
   ( (and (> res 0) (integerp res) ) op )
   ( t (valid-op cage) )
  )
)

; combine targets and unpopulated cages together to create a solvable full board
(defmethod place-targets (targets &aux ii)
  (setf ii 0)
  (loop for i in (reverse *cages*) do
       (setf i (push (nth ii targets) i) )
       (push i *generated-game*)
       (setf ii (+ 1 ii) )
  )
  *generated-game*
)

; ----------------------------------------------------------------------------------
; Task 5: Display Generated Game
; ----------------------------------------------------------------------------------

(defmethod display-generated (&aux alphabet letter ii to-paint to-write)
  (setf ii 0)
  (setf to-paint '() to-write '() )
  (setf alphabet '(a b c d e f g h i j k l m n o p q r s t u v w x y z) )
  (send (init (list *size* 'show 0) ) )
  (sleep 1)
  (loop for i in (reverse *cages*) do
	(setf letter (pick alphabet) )
	(setf alphabet (remove letter alphabet) )
	(cond
	 ( (eq (length i) 1)
	   (push (list (cdar i) (caar i) letter ) to-paint)
	 )
	 ( (eq (length i) 2)
	   (and  (push (list (cdar i) (caar i) letter) to-paint)
	   (push (list (cdadr i) (caadr i) letter) to-paint ) )
	 )
	 ( (eq (length i) 3)
	   (and (push (list (cdar i) (caar i) letter) to-paint)
		(and (push (list (cdadr i) (caadr i) letter) to-paint)
		     (push (list (cdr (nth 2 i) ) (car (nth 2 i) ) letter) to-paint) ) )
         )
	 )
       (push (select-square i (nth ii *targets*) ) to-write)
       (setf ii (+ 1 ii) )
       )
  (send (paint to-paint) )
  (sleep 1)
  (send (write-target to-write) )
  (sleep 1)
)

; select the box to write the target number in for each cage
; chosen by leftmost square in each cage
(defmethod select-square (cage target &aux result)
  (setf result '() )
  (cond
   ( (eq (length cage) 1)
     (setf result (list (cdar cage) (caar cage) target) )
   )
   ( (eq (length cage) 2)
      (setf result (get-leftmost-two (caar cage) (cdar cage) (caadr cage) (cdadr cage) target ) )
   )
   ( (eq (length cage) 3)
      (setf result (get-leftmost-three (caar cage) (cdar cage) (caadr cage) (cdadr cage)
				       (caaddr cage) (cdaddr cage) target ) )
   )
   )
  result
)

(defmethod get-leftmost-two (car-s1 cdr-s1 car-s2 cdr-s2 target)
  (cond
   ( (< car-s1 car-s2 )
     (list cdr-s1 car-s1 target)
   )
   ( (> car-s1 car-s2)
     (list cdr-s2 car-s2 target)
   )
   ( (= car-s1 car-s2)
     (cond
      ( (> cdr-s1 cdr-s2)
	(list cdr-s1 car-s1 target)
      )
      ( t (list cdr-s2 car-s2 target) )
     )
    )
  )
)

(defmethod get-leftmost-three (car-s1 cdr-s1 car-s2 cdr-s2 car-s3 cdr-s3 target)
   (cond
    ( (< car-s1 car-s2 )
     (cond
      ( (< car-s1 car-s3 )
	(list cdr-s1 car-s1 target)
      )
      ( (= car-s1 car-s3)
	(cond
	 ( (> cdr-s1 cdr-s3)
	   (list cdr-s1 car-s1 target)
	 )
	 ( t (list cdr-s3 car-s3 target) )
	)
      )
      (t (list cdr-s3 car-s3 target) )
      )
     )
    ( (< car-s2 car-s3)
     (list cdr-s2 car-s2 target)
    )
    ( (= car-s1 car-s2)
      (cond
      ( (> cdr-s1 cdr-s2)
	(list cdr-s1 car-s1 target)
      )
      ( t (list cdr-s2 car-s2 target) )
      )
    )
    ( (= car-s2 car-s3)
	(cond
	 ( (> cdr-s2 cdr-s3)
	   (list cdr-s2 car-s2 target)
	 )
	 ( t (list cdr-s3 car-s3 target) )
	)
      )
    (t (list cdr-s3 car-s3 target) )
  )
)

(defmethod generation-demo ()
  (latin-square 4)
  (grid-coords *board*)
  (format t "Latin Square:")
  (print *board*) (terpri) (terpri)
  (polyominoes *coords*)
  (format t "Unpopulated Cages:")
  (loop for i in (reverse *cages*) do (print i) )
  (terpri) (terpri)
  (populate-cages)
  (format t "Populated Cages:")
  (loop for i in *full-board* do (reverse (print i) ) )
  (terpri) (terpri)
  (format t "Empty Cages w/ Target Number and Operator:")
  (apply-op)
  (loop for i in (reverse *generated-game*) do (print i) )
  (display-generated)
)


; ----------------------------------------------------------------------------------
; Task 6: Goal State
; ----------------------------------------------------------------------------------

; check to see if the solution has been achieved
(defmethod goal-p (full-lst &aux condensed-lst)
  (setf condensed-lst (condense-full-board full-lst) )
  (cond
   ( (and
      (row-p condensed-lst)
      (and
       (row-p (rotate condensed-lst) )
       (target-p full-lst)
       )
      )
     t
   )
  )   
)

; checks if number is not repeated in each row
(defmethod row-p (lst &aux result)
  (setf result '() )
  (loop for i in lst do
	(push (check-row i) result)
  )
  (setf result (filter-out 'null result) )
  (cond
   ( (equal (length result) *size*) t)
   (t nil)
  )
)

(defmethod check-row-num (lst)
  (cond
   ( (null lst) t)
   ( (member (car lst) (cdr lst) )
     (cond
      ( (eq (car lst) 0) (check-row-num (cdr lst) ) )
      (t nil)
      )
   )
   ( t (check-row-num (cdr lst) ) )
  )
)

(defmethod check-row (lst)
  (cond
   ( (null lst) t)
   ( (member (car lst) (cdr lst) ) nil)
   ( t (check-row (cdr lst) ) )
  )
)

(defun member-nested (el lst)
    (cond
       ( (null lst) nil)
       ( (equal el (car lst) ) t)
       ( (consp (car lst) ) (or (member-nested el (car lst) )
                            (member-nested el (cdr lst) ) ) )
       (t (member-nested el (cdr lst) ) )
   )
)

(defmethod remove-nested (a l)
  (cond
   ( (null l) nil)
   ( (atom (car l) )
     (cond
      ( (equal a (car l) )
	(remove-nested a (cdr l) )
      )
      (t (cons (car l) (remove-nested a (cdr l) ) ) )
     )
    )
    (t (cons (remove-nested a (car l) )
             (remove-nested a (cdr l) )
       )
    )
  )
)

(defmethod get-pos (lst)
  (cond
   ( (null lst) nil)
   ( t (cons (caar lst) (get-pos (remove (car lst) lst) ) ) ) )
)

; checks if target number has been achieved
; ( (num op) ( (goal (x . y) ) (goal (x . y) ) ) )
; empty example: ( (10 +) ( (0 (1 . 2) ) (5 (1 . 1) ) ) )
;'( (30 *) ( (5 (1 . 4) ) (6 (1 . 3) ) (1 (1 . 2) ) ) )
(defmethod target-p (lst &aux num op cage-list result)
  (setf result '() )
  (loop for i in lst do
	(setf num (caar i) )
	(setf op (cadar i) )
	(cond
	 ( (eq (length (cadr i) ) 1)
	   (cond
	    ( (eq num (nth 0 (caadr i) ) ) (push t result) )
	    ( t (push nil result) )
	    )
	   )
	 ( (eq (length (cadr i) ) 2)
	   (cond
	    ( (eq num (eval (list op (nth 0 (caadr i) )(nth 0 (nth 1 (cadr i) ) ) ) ) ) (push t result) )
	    ( (eq num (eval (list op (nth 0 (nth 1 (cadr i) ) )(nth 0 (caadr i) ) ) ) ) (push t result) )
	    ( t (push nil result) )
	    )
	   )
	 ( (eq (length (cadr i) ) 3)
	   (cond
	    ( (eq num (eval (list op (nth 0 (caadr i) )
				     (nth 0 (nth 1 (cadr i) ) )
				     (nth 0 (nth 2 (cadr i) ) ) ) ) )
	      (push t result)
	    )
	    ( t (push nil result) )
	    )
	 )
      )
  )
  (setf result (filter-out 'null result) )
  (cond
   ( (equal (length result) (length lst) ) t)
   (t nil)
  )
)

(defmethod condense-full-board (lst &aux condensed)
  (setf condensed (create-empty-list) )
  (loop for i in lst do
      (cond
	 ( (eq (length (cadr i) ) 1)
	   (setf (nth (get-y (cadr i) ) (nth (get-x (cadr i) ) condensed ) ) (nth 0 (caadr i) ) )
	 )
	 ( (eq (length (cadr i) ) 2)
	   (and
	    (setf (nth (get-y (cadr i) ) (nth (get-x (cadr i) ) condensed ) ) (nth 0 (caadr i) ) )
	    (setf (nth (get-y (cdadr i) ) (nth (get-x (cdadr i) ) condensed ) ) (nth 0 (cadadr i) ) )
	   )
	   
	 )
	 ( (eq (length (cadr i) ) 3)
	   (and
	    (setf (nth (get-y (cadr i) ) (nth (get-x (cadr i) ) condensed ) ) (nth 0 (caadr i) ) )
	    (and
	     (setf (nth (get-y (cdadr i) ) (nth (get-x (cdadr i) ) condensed ) ) (nth 0 (cadadr i) ) )
	     (setf (nth (get-y (cddadr i) ) (nth (get-x (cddadr i) ) condensed ) )
		   (nth 0 (car (cddadr i) ) ) )
	    )
	   )
	 )
      )
  )
  condensed
)

(defmethod get-x (lst &aux coords)
  (setf coords (nth 1 (car lst) ) )
  (- (car coords) 1)
)

(defmethod get-y (lst &aux coords)
  (setf coords (nth 1 (car lst) ) )
  (- (cdr coords) 1)
)

(defmethod create-empty-list (&aux inner lst)
  (setf lst '() )
  (dotimes (n *size*)
    (setf inner '() )
    (dotimes (n *size*)
      (push 0 inner)
     )
    (push inner lst)
   )
  lst
)

; ----------------------------------------------------------------------------------
; Task 7: Solver
; ----------------------------------------------------------------------------------

; unknown is denoted by a 0
(defmethod generated-to-solvable (board &aux solvable to-sol)
  (setf solvable '() to-sol '() )
  (loop for i in (reverse board) do
      (cond
	 ( (eq (length (cdr i) ) 1)
	   (and (push (list (car i) (list (list (caar i) (cadr i) ) ) ) solvable )
		(and (push (list (cdadr i) (caadr i) (caar i) ) to-sol)
		     (push (cons (caadr i) (cdadr i) ) *written-sols* )
		 )
	   )
	 )
	 ( (eq (length (cdr i) ) 2)
	   (push (list (car i) (list (list 0 (nth 0 (cdr i) ) )
				     (list 0 (nth 1 (cdr i) ) ) ) )
	    solvable)
	 )
	 ( (eq (length (cdr i) ) 3)
	   (push (list (car i) (list
				(list 0 (nth 0 (cdr i) ) )
				(list 0 (nth 1 (cdr i) ) )
				(list 0 (nth 2 (cdr i) ) ) ) )
	    solvable)
	 )
      )
  )
  (if (not (equal to-sol nil) )
      (progn (send (write-solution to-sol) )
	     (sleep 1)
       )
      
  )
  solvable
)

(defmethod solver (board &aux fl ref)
  (setf board (one-left-in-row board) )
  (setf board (one-left-in-col board) )
  (loop for i in board do
	(setf fl (get-factors i) )
	(if (not (cage-completed (cadr i) (car i) ) )
	    (loop for ii in (cadr i) do
		  (setf ref (refine-factors fl ii (condense-full-board board) ) )
		  (if (not (equal ref nil) )
		  (setf i (set-square ref ii i) ) )
	    )
       )
  )
  (cond
   ( (eq (length (flatten (condense-full-board board) ) ) (expt *size* 2) )
     (if (goal-p board) (write-dfs (condense-full-board board)) )
   )
   (t (find-solution board 0) )
  )
)

(defmethod find-solution (board iter &aux to-sol)
  (format t "Find solution: ~A~%" (condense-full-board board))
  (setf to-sol '() )
  (loop for i in board do
	(loop for ii in (cadr i) do
	      (if (typep ii 'cons)
		  (if (equal (type-of (car ii) ) 'cons )
		      (if (equal (length (car ii) ) 1)
			  (and 
			   (setf i (set-square (caar ii) ii i) )
			   (and 
			    (push (list (cdadr ii) (caadr ii) (car ii) ) to-sol)
			    (push (cons (caadr ii) (cdadr ii) ) *written-sols* )
			    )
			  )
		      )
		)
	     )
	)
  )
  (if (not (equal to-sol nil) ) (progn (send (write-solution to-sol) ) (sleep 1) ) )
  (format t "Find solution check goalp: ~A~%" (condense-full-board board))
  (cond
   ( (eq (length (flatten (condense-full-board board) ) ) (expt *size* 2) )
     (if (goal-p board) (write-dfs (condense-full-board board) ) )
   )
   (t (eliminate-options board iter) )
  )
  board
)

(defmethod eliminate-options (board iter &aux x y m n to-sol)
  (format t "Eliminate options: ~A~%" (condense-full-board board))
  (setf x 1)
  (setf to-sol '() )
  (loop for i in (condense-full-board board) do
	(setf y 1)
	(loop for ii in i do
	      (if (equal (type-of ii) 'cons)
		  (loop for iii in ii do
			(cond
			 ( (> (count iii i) 0)
			   (and
			    (setf board (set-condensed-square (remove iii ii) (cons x y) board) )
			    (if (not (typep (remove iii ii) 'cons) )
				(progn
				  (push (list y x (remove iii ii) ) to-sol)
				  (push (cons x y) *written-sols*) )
			     )
			   )
			 )
		        )
		   )
	  )
	 (setf y (+ 1 y) )
	)
	(setf x (+ 1 x) )
  )
  (setf m 1)
  (loop for i in (rotate (condense-full-board board) ) do
	(setf n 1)
	(loop for ii in i do
	      (if (equal (type-of ii) 'cons)
		  (loop for iii in ii do
			(cond
			 ( (> (count iii i) 0)
			   (and
			    (setf board (set-condensed-square (remove iii ii) (cons n m) board) )
			    (if (not (typep (remove iii ii) 'cons) )
				(progn
				  (push (list m n (remove iii ii) ) to-sol)
				  (push (cons n m) *written-sols*) )
			      )
				  
			   )
			 )
		    )
		 )
	      )
	(setf n (+ 1 n) )
      )
  (setf m (+ 1 m) )
  )
  (if (not (equal to-sol nil) ) (progn (send (write-solution to-sol) ) (sleep 1) ) )
  (format t "Eliminate options check goalp: ~A~%" (condense-full-board board))
  (cond
   ( (eq (length (flatten (condense-full-board board) ) ) (expt *size* 2) )
     (if (goal-p board) (write-dfs (condense-full-board board) ) )
   )
   (t (find-only-option board iter) )
  )      
)

(defmethod find-only-option (board iter &aux x y m n to-sol)
  (format t "Find only option: ~A~%" (condense-full-board board))
  (print (condense-full-board board) )
  (setf to-sol '() )
  (setf x 1)
  (loop for i in (condense-full-board board) do
	(setf y 1)
	(loop for ii in i do
	      (if (equal (type-of ii) 'cons)
		  (loop for iii in ii do
			(cond
			 ( (= (count iii (flatten i) ) 1)
			   (and 
			    (setf board (set-condensed-square iii (cons x y) board) )
			    (if (not (typep iii 'cons) )
				(progn
				  (push (list y x iii ) to-sol)
				  (push (cons x y) *written-sols*) )
			     )
			   )
			 )
		        )
		  )
		)
	     (setf y (+ 1 y) )
       )
 (setf x (+ 1 x) )
 )
 (setf m 1)
 (loop for i in (rotate (condense-full-board board) ) do
	(setf n 1)
	(loop for ii in i do
	      (if (equal (type-of ii) 'cons)
		  (loop for iii in ii do
			(cond
			 ( (= (count iii (flatten i) ) 1)
			   (and 
			    (setf board (set-condensed-square iii (cons n m) board) )
			    (if (not (typep iii 'cons) )
				(progn
				  (push (list m n iii) to-sol)
				  (push (cons n m) *written-sols*) )
			     )
			   )
			 )
		    )
		 )
	      )
	(setf n (+ 1 n) )
      )
  (setf m (+ 1 m) )
  )
  (if (not (equal to-sol nil) ) (progn (send (write-solution to-sol) ) (sleep 1) ) )
 (format t "Find only option goalp: ~A~%" (condense-full-board board))
  (cond
   ( (eq (length (flatten (condense-full-board board) ) ) (expt *size* 2) )
     (if (goal-p board) (write-dfs (condense-full-board board) ) )
   )
   (t (find-only-option-lists board iter) )
  )
)

(defmethod find-only-option-lists (board iter &aux x y m n to-sol)
  (format t "Find only option lists: ~A~%" (condense-full-board board))
  (setf x 1)
  (setf to-sol '() )
  (loop for i in (condense-full-board board) do
	(setf y 1)
	(if (only-list-p i)
	    (loop for j in i do
		  (loop for k in j do
			(cond
			 ( (= (count k (flatten i) ) 1)
			     (and
			      (setf board (set-condensed-square k (cons x y) board) )
			      (if (not (typep k 'cons) )
				  (progn
				    (push (list y x k) to-sol)
				    (push (cons x y) *written-sols*) )
				  )
			    )
			     )
			)
		   )
	      (setf y (+ 1 y) )
	    )
         )
    (setf x (+ 1 x) )
  )
  (setf m 1)
  (loop for i in (rotate (condense-full-board board) ) do
	(setf n 1)
	(if (only-list-p i)
	    (loop for j in i do
		  (loop for k in j do
			(cond
			 ( (= (count k (flatten i) ) 1)
			     (and
			      (setf board (set-condensed-square k (cons n m) board) )
			      (if (not (typep k 'cons) )
			      (progn 
				(push (list m n k) to-sol)
				(push (cons n m) *written-sols*) ) )
			     )
			    )
			 )
		   )
	      (setf n (+ 1 n) )
	    )
         )
    (setf m (+ 1 m) )
    )
   (if (not (equal to-sol nil) ) (progn (send (write-solution to-sol) ) (sleep 1) ) )
  (format t "Find only option lists goalp: ~A~%" (condense-full-board board) )
   (cond
   ( (eq (length (flatten (condense-full-board board) ) ) (expt *size* 2) )
     (if (goal-p board) (write-dfs (condense-full-board board) ) )
   )
   ( (> iter 4) (do-search (searchboard board) ) )
   (t (find-solution board (+ 1 iter) ) )
  )
)

; converts heuristic search board into a board useable by the dfs do-search method
(defmethod searchboard (board)
  (loop for i in board do
	(cond
	 ( (eq (length (cadr i) ) 2)
	   (if (typep (nth 0 (caadr i) ) 'cons) (setf (nth 0 (caadr i) ) 0) )
	  )
	)
	(cond
	 ( (eq (length (cadr i) ) 2)
	   (if (typep (nth 0 (cadadr i) ) 'cons) (setf (nth 0 (cadadr i) ) 0 ) )
	 )
	)
	(cond
	 ( (eq (length (cadr i) ) 3)
	   (if (typep (nth 0 (caadr i) ) 'cons) (setf (nth 0 (caadr i) ) 0) )
	  )
	)
	(cond
	 ( (eq (length (cadr i) ) 3)
	   (if (typep (nth 0 (cadadr i) ) 'cons) (setf (nth 0 (cadadr i) ) 0) )
	 )
	)
        (cond
	 ( (eq (length (cadr i) ) 3)
	   (if (typep (car (nth 2 (cadr i) ) ) 'cons) (setf (car (nth 2 (cadr i) ) ) 0) )
	 )
	)
    )
  board
)

(defmethod only-list-p (lst)
  (cond
   ( (null lst) t)
   ( (typep (car lst) 'integer) nil)
   ( t (only-list-p (cdr lst) ) )
  )
)

(defmethod set-condensed-square (num square board)
  (cond
   ( (typep num 'cons)
     (cond 
      ( (equal (length num) 1) (setf num (car num) ) )
      )
   )
  )
  (loop for i in board do
	(loop for ii in (cadr i) do
	      (cond
	       ( (equal (cdr ii) (list square) )
		 (setf ii (setf (car ii) num) )
	       )
	     )
	)
  )
  board
)

(defmethod solve (board)
  (cond
   ( (goal-p board) t)
   (t (solver board) )
 )
)
; searches for a solution using a modified depth first search
(defmethod do-search (board &aux temp factor-list refined)
  (format t "Do DFS: ~A~%" (condense-full-board board) )
  (cond
   ( (not (member-nested 0 (condense-full-board board) ) )
     (if (goal-p board) (write-dfs (condense-full-board board) ) nil)
   )
   (t (let ((state board))
   (setf state (one-left-in-row state) )
   (setf state (one-left-in-col state) )
   (loop for i in state do
      (let ( (factor-list (get-factors i) ) )
      (if (not (cage-completed (cadr i) (car i) ) )
	  (loop for ii in (cadr i) do
	      (let ((refined (refine-factors factor-list ii (condense-full-board state) ) ) )
	      (if (eq (car ii) 0 )
		  (loop for j in refined do
			(cond
			 ( (rc-valid? j ii i (condense-full-board state) )
			   (and (setf i (set-square j ii i) )
			          (let ( (result (do-search state) ) )
				    (when result (write-dfs result) ) )
				  )
			   )
			 )
		      )
		  )
	       )
	  )
      )	     
   ) ) ) ) )
  (condense-full-board board)
)

(defmethod write-dfs (board &aux x y to-sol)
  (setf x 1)
  (setf to-sol '() )
  (loop for i in board do
	(setf y 1)
	(loop for j in i do
	      (if (not (member (cons x y) *written-sols* :test #'equal) )
		  (push (list y x j) to-sol) )
	      (setf y (+ 1 y) )
	)
   (setf x (+ 1 x) )
   )
   (if (not (equal to-sol nil) ) (progn (send (write-solution to-sol) ) (sleep 1) ) )
)

; removes factors from factor-list that cannot be in a given position
(defmethod refine-factors (lst square state &aux x y cols)
  (setf x (nth 0 (cadr square) ) )
  (setf y (cdadr square) )
  (setf cols (rotate state) )
  (loop for i in lst do
       (cond
	( (member i (nth (- x 1) state) ) (setf lst (remove i lst) ) )
	( (member i (nth (- y 1) cols) ) (setf lst (remove i lst) ) )
	)
  )
  lst	 
)

; gets the possible factors for each cage
(defmethod get-factors (cage &aux lst)
  (cond
   ( (equal (cadar cage) '+) (setf lst (get-addition cage) ) )
   ( (equal (cadar cage) '-) (setf lst (get-subtraction cage) ) )
   ( (equal (cadar cage) '*) (setf lst (get-multiplication cage) ) )
   ( (equal (cadar cage) '/) (setf lst (get-division cage) ) )
  )
  lst
)

(defmethod get-addition (cage &aux p)
  (setf p (addition-partitions (caar cage) ) )
  (loop for i in p do
      (cond ( (> (length i) 3) (setf p (remove i p) ) ) )
      (cond ( (not (equal (length (cadr cage) ) (length i) ) ) (setf p (remove i p) ) ) )
      (loop for ii in i do
	 (cond ( (> ii *size*) (setf p (remove i p) ) ) )
      )
  )
  (setf p (remove-duplicates (flatten p) ) )
  p
)

(defmethod get-subtraction (cage &aux p)
  (setf p '() )
  (loop for i from 1 to *size* do
	(loop for j from 1 to *size* do
	      (cond
	       ( (equal (eval (- i j) ) (caar cage) ) (and (push i p) (push j p) ) )
	      )
	)
  )
  (setf p (remove-duplicates p) )
  p
)

(defmethod get-multiplication (cage &aux f)
  (setf f '() )
  (loop for i from 1 to *size* do
      (cond
	 ( (equal (mod (caar cage) i) 0) (push i f) )
      )
  )
  f
)

(defmethod get-division (cage &aux f)
  (setf f '() )
  (loop for i from 1 to *size* do
	(loop for j from 1 to *size* do
	      (cond
	       ( (equal (eval (/ i j) ) (caar cage) ) (and (push i f) (push j f) ) )
	       )
	 )
  )
  (setf f (remove-duplicates f) )
  f
)

; source: https://stackoverflow.com/questions/38170662/integer-partitions-in-common-lisp
(defun addition-partitions (n)
  (let ( (result (list) ) )
    (labels ( (%partitions (n largest-number acc)
               (cond  ( (< n 1) (push (reverse acc) result) )
                     (t (loop for l from largest-number downto 1
                              do (loop for i from (floor n l) downto 1
                                       do (%partitions
                                           (- n (* l i) )
                                           (1- l)
                                           (append (make-list i :initial-element l)
                                                   acc) ) ) ) ) ) ) )
      (%partitions n n '())
      (nreverse result) ) )
)

; sets a square on the board at a certain position
(defmethod set-square (num square cage)
  (cond
   ( (eq (length (cadr cage) ) 2)
     (cond
      ( (equal square (nth 0 (cadr cage) ) ) (setf (nth 0 (caadr cage) ) num) )
      ( t (setf (nth 0 (cadadr cage) ) num) )
     )
   )
   ( (eq (length (cadr cage) ) 3)
     (cond
      ( (equal square (nth 0 (cadr cage) ) ) (setf (nth 0 (caadr cage) ) num) )
      ( (equal square (nth 1 (cadr cage) ) ) (setf (nth 0 (cadadr cage) ) num) )
      (t (setf (car (nth 2 (cadr cage) ) ) num) )
     )
   )
  )
  cage
)

; checks to see if a guess would be valid
(defmethod rc-valid? (num square cage state &aux x y cols)
  (setf x (- (caadr square) 1) )
  (setf y (- (cdadr square) 1) )
  (setf (nth y (nth x state) ) num)
  (setf cols (rotate state) )
  (cond
   ( (and (check-row-num (nth x state) ) (check-row-num (nth y cols) ) ) t)
   (t nil)
  )
)

; checks to see if a cage has already been solved
(defmethod cage-completed (cage target)
  (cond
   ( (eq (length cage) 1 ) t)
   ( (eq (length cage) 2)
     (cond 
      ( (and (not (eq (caar cage) 0) ) (not (eq (caadr cage) 0) ) ) (cage-valid? cage target 2) )
      ( t nil )
    )
  )
   ( (eq (length cage) 3)
     (cond
      ( (and (not (eq (caar cage) 0) ) (and (not (eq (caadr cage) 0 ) )
					     (not (eq (car (nth 2 cage ) ) 0) ) ) )
	(cage-valid? cage target 3)
      )
      (t nil)
      )
    )
  )
)

(defmethod cage-valid? (cage target size &aux num op)
  (setf num (car target) )
  (setf op (nth 1 target) )
  (cond
   ( (eq size 2)
     (cond
      ( (eq num (eval (list op (caar cage) (caadr cage) ) ) ) t)
      ( (eq num (eval (list op (caadr cage) (caar cage) ) ) ) t)
      (t nil)
     )
   )
   ( (eq size 3)
     (cond
      ( (eq num (eval (list op (caar cage) (caadr cage) (car (nth 2 cage) ) ) ) ) t)
      (t nil)
     )
   )
  )
)
					;
;checks to see if all but one number is filled in, in a row
(defmethod one-left-in-row (state &aux condensed ii)
  (setf condensed (condense-full-board state) )
  (setf ii 1)
  (loop for i in condensed do
      (cond
	 ( (member 0 i)
	   (cond
	    ( (eq (count 0 i) 1) (setf state (solve-row state ii (+ 1 (position 0 i) ) i) ) )
	   )
	 )	 
      )
    (setf ii (+ ii 1) )
  )
  state
) 

; checks to see if all but one number is filled in, in a column
(defmethod one-left-in-col (state &aux condensed ii)
  (setf condensed (condense-full-board state) )
  (setf condensed (rotate condensed) )
  (setf ii 1)
  (loop for i in condensed do
     (cond
         ( (member 0 i)
	    (cond
	     ( (eq (count 0 i) 1) (setf state (solve-row state (+ 1 (position 0 i) ) ii i) ) )
	    )
	 )
     )
     (setf ii (+ ii 1) )
   )
  state
)

; rotates rows --> columns
(defmethod rotate (lst)
  (apply #'mapcar #'list lst))

; solves a row if applicable
(defmethod solve-row (state x y row &aux pos temp ii to-sol)
  (setf pos (cons x y) )
  (setf temp state)
  (setf ii 0)
  (setf to-sol '() )
  (loop for i in state do
      (cond
	 ( (eq (length (cadr i) ) 2 )
	   (cond
	    ( (equal pos (cadar (nth 1 i) ) )
	      (and (setf (caar (nth 1 (nth ii temp) ) ) (find-missing row) )
		   (push (list y x (find-missing row) ) to-sol)
	      )
	    )
	    ( (equal pos (car (cdadr (nth 1 i) ) ) ) 
	      (and (setf (caadr (nth 1 (nth ii temp) ) ) (find-missing row) )
		   (push (list y x (find-missing row) ) to-sol)
	      )
	    )
	   )
	 )
	 ( (eq (length (cadr i) ) 3 )
	   (cond
	    ( (equal pos (cadar (nth 1 i) ) ) 
	      (and (setf (caar (nth 1 (nth ii temp) ) ) (find-missing row) )
		   (push (list y x (find-missing row) ) to-sol)
	      )
	    )
	    ( (equal pos (car (cdadr (nth 1 i) ) ) ) 
	      (and (setf (caadr (nth 1 (nth ii temp) ) ) (find-missing row) )
		   (push (list y x (find-missing row) ) to-sol)
	      )
	    )
	    ( (equal pos (cadr (nth 2 (nth 1 i) ) ) )
	      (and (setf (car (nth 2 (nth 1 (nth ii temp) ) ) ) (find-missing row) )
		   (push (list y x (find-missing row) ) to-sol)
	      )
	    )
	   )
	 )
      )
   (setf ii (+ 1 ii) )
   )
   (if (not (equal to-sol nil) ) (progn (send (write-solution to-sol) ) (sleep 1) ) )
  temp
)

; locates the number in the column/row to be filled in by process of elimination
(defmethod find-missing (missing &aux lst result)
  (setf lst '() )
  (dotimes (n *size*)
    (push (+ 1 n) lst)
  )
  (loop for i in missing do
	(setf lst (remove i lst) )
  )
  (car lst)
)

(defmethod solver-demo ()
  (generation-demo)
  (terpri) (terpri)
  (solve (generated-to-solvable *generated-game*) )
)

(defmethod demo (size &aux solved)
  (setf *coords* '() )
  (setf *cages* '() )
  (setf *board* '() )
  (setf *full-board* '() )
  (setf *size* '() )
  (setf *targets* '() )
  (setf *generated-game* '() )
  (setf *written-sols* '() )
  (latin-square size)
  (grid-coords *board*)
  (format t "Latin Square:")
  (print *board*) (terpri) (terpri)
  (polyominoes *coords*)
  (populate-cages)
  (format t "Empty Cages w/ Target Number and Operator:")
  (apply-op)
  (loop for i in (reverse *generated-game*) do (print i) )
  (display-generated)
  (terpri) (terpri)
  (setf solved (solve (generated-to-solvable *generated-game*) ) )
  solved
)









