;;;; File: simple_viewer_agent.l
;;;; Line: Lisp API for doing just a few very simple visual things
;;;; More: See the Simple Viewer Agent guide and report

;; THE FEATURED FUNCTIONALITY -- a messenger and four compilers

( defun send (command)
  ( write-to-view-file command )
  ( sleep 1)
)

( defun draw ( triples )
  ( concatenate 'string "DRAW " ( list-to-string-proper ( flatten triples ) ) )
)

( defun paint ( triples )
  ( concatenate 'string "PAINT " ( list-to-string-proper ( flatten triples ) ) )
)

( defun mode ( mode )
  ( concatenate 'string "MODE " ( list-to-string-proper ( list mode ) ) )
)

( defun init ( triple )
  ( concatenate 'string "INIT " ( list-to-string-proper triple ) )
)

( defun write-target ( triples )
  (concatenate 'string "WRITE " (list-to-string-proper (flatten triples) ) )
)

( defun write-solution ( triples )
  (concatenate 'string "SOLUTION " (list-to-string-proper (flatten triples) ) )
)
;; INFRASTRUCTURE FOR THE FEATURED FUNCTIONALITY

; simply create a string from the list, with the parentheses removed
( defun list-to-string-proper ( the-list )
  ( string-right-trim ")" ( string-left-trim "(" ( write-to-string the-list) ) )
)

; write a command to the designated output file
( defun write-to-view-file ( command )
  ( with-open-file
    ( *standard-output* "view.text" :direction :output :if-exists :supersede )
    ( format t "~A~%" command )
  )
)

;; UTILITIES

;; return a list of pairs representing the locations on an nxn grid
( defun locations (n)
  ( mapcan
    ( lambda (list1 list2) ( mapcar #'list list1 list2 ) )
    ( mapcar #'duplicate ( iota n ) ( duplicate n n ) )
    ( duplicate ( iota n ) n )
  )
)

;; return a list of triples for rendering an entire nxn grid in some color
( defun color-grid (n c)
  ( mapcar #'snoc ( duplicate c ( * n n ) ) ( locations n ) )
)

;; selected list processors

( defun snoc ( o l )
  ( cond 
    ( ( null l ) 
      ( list o ) 
    ) 
    ( t 
      ( cons ( car l ) ( snoc o ( cdr l ) ) ) 
    ) 
  ) 
) 

( defun duplicate ( o n )
  ( cond
    ( ( = n 0 )
      ()
    )
    ( t
      ( cons o ( duplicate o ( - n 1 ) ) )
    )
  )
)

( defun iota ( n )
  ( cond
    ( ( = n 0 )
      ()
    )
    ( t
      ( snoc n ( iota ( - n 1 ) ) )
    )
  )
)

( defun take-from ( x l )
  ( filter-out ( lambda ( y ) ( equal x y ) ) l )
)

( defun pick ( l )
  ( nth ( random ( length l ) ) l )
)

( defun random-permutation ( l )
  ( cond
    ( ( null l )
      ()
    )
    ( t
      ( setf element ( pick l ) )
      ( setf remainder ( take-from element l ) )
      ( cons element ( random-permutation remainder ) )
    )
  )
)
  
( defun flatten (l)
  ( cond
    ( ( null l )
      ()
    )
    ( ( atom ( car l ) )
      ( cons ( car l ) ( flatten ( cdr l ) ) )
    )
    ( t
      ( append ( flatten ( car l ) ) ( flatten ( cdr l ) ) )
    )
  )
)
