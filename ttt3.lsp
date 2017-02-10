(load "two_lisp_ditties.lsp")
;;Loading file two_lisp_ditties.lsp
;;Loaded file two_lisp_ditties.lsp


;(load "ttt3.lsp")
;(demo-random-random)
;(demo-random-human)







;;;; TASK 3 - modeling human and random players
; The root of the player hierarchy.
;
( defclass player ()
  ( ( name :accessor player-name :initarg :name :initform 'emanon ) )
)

; A random machine player is a player.
;
( defclass random-machine-player ( player ) () )

( defmethod display ( ( p random-machine-player ) )
  ( format t "RANDOM MACHINE PLAYER ...~%" )
  ( format t "name = ~A~%" ( player-name p ) )
  ( format t "~%" )
   nil
)

;A human player is a player.
;
( defclass human-player ( player ) () )

( defmethod display ( ( p human-player ) )
  ( format t "HUMAN PLAYER ...~%" )
  ( format t "name = ~A~%" ( player-name p ) )
  ( format t "~%" )
   nil
)

; A move making method for a random machine player.
;
( defmethod make-move ( ( p random-machine-player ) ( report t ) &aux move )
  ( if report ( format t "BEGIN RANDOM PLAYER MOVE ...~%" ) )
  ( setf move ( select *avail* ) )
  ( if report ( format t "randomly selecting ~A for my move~%" move ) )
  ( setf *avail* ( remove move *avail* ) )
  ( if report ( format t "END RANDOM PLAYER MOVE ...~%" ) )
  move
)

;A move making method for a human player.
;
( defmethod make-move ( ( p human-player ) ( report t ) &aux move )
  ( if report ( format t "BEGIN HUMAN PLAYER MOVE ...~%" ) )
  ( format t "Please select a move from ~A~%" *avail* )
  ( setf move ( read ) )
  ( cond
    ( ( not ( member move *avail* ) )
      ( make-move p )
    )
    ( t
       ( setf *avail* ( remove move *avail* ) )
       move
     )
   )
   ( if report ( format t "END HUMAN PLAYER MOVE~%" ) )
   move
)

; A generic play method. The *play-so-far* variable will be used by the heuristic machine
;
( defmethod generic-play ( ( x player ) ( o player ) ( report t ) &aux move)
  ( setf *avail* '(nw n ne w c e sw s se) )
  ( setf *play-so-far* () )
  ( dolist ( player '(x o x o x o x o x) )
    ( if ( or report ( equal ( type-of o ) 'human-player-machine ) )
      ( visualize *play-so-far* )

    )
    ( cond
      ( ( eq player 'x )
        ( setf move ( make-move x report ) )
      )
      ( ( eq player 'o )
        ( setf move ( make-move o report ) )
      )
    )
    ( setf *play-so-far* ( snoc move *play-so-far* ) )
    ( if ( game-over-p *play-so-far* ) ( return nil ) )
  )
    *play-so-far*
)

; Predicate to determine if the game is over or not.
;
( defmethod game-over-p ( ( play list ) )
  ( cond
    ( ( line-p ( odd play ) ) 'w )
    ( ( line-p ( even play ) ) 'l )
    ( ( = ( length play ) 9 ) 'd )
    ( t nil )
  )
)

( defmethod odd ( ( l list ) )
  ( cond
    ( ( null 1 ) () )
    ( ( null ( cdr l ) ) ( list ( car l ) ) )
    ( t ( cons ( car l ) ( odd ( cddr l ) ) ) )
  )
)

( defmethod even ( ( l list ) )
  ( cond
    ( ( null 1 ) () )
    ( ( null ( cdr l ) ) () )
    ( t ( cons ( cadr l ) ( even ( cddr l ) ) ) )
  )
)

( defmethod line-p ( ( l list ) )
  ( cond
    ( ( < ( length l ) 3 )
      nil
    )
    ( ( = ( length l ) 3 )
      ( line ( first l ) ( second l ) ( third l ) )
    )
    ( ( = ( length l ) 4 )
      ( or
        ( line ( first l ) ( second l ) ( third l ) )
        ( line ( first l ) ( second l ) ( fourth l ) )
        ( line ( first l ) ( third l ) ( fourth l ) )
        ( line ( second l ) ( third l ) ( fourth l ) )
      )
    )
    ( ( = ( length l ) 5 )
      ( or
        ( line ( first l ) ( second l ) ( third l ) )
        ( line ( first l ) ( second l ) ( fourth l ) )
        ( line ( first l ) ( second l ) ( fifth l ) )
        ( line ( first l ) ( third l ) ( fourth l ) )
        ( line ( first l ) ( third l ) ( fifth l ) )
        ( line ( first l ) ( fourth l ) ( fifth l ) )       
        ( line ( second l ) ( third l ) ( fourth l ) )
        ( line ( second l ) ( third l ) ( fifth l ) )
        ( line ( second l ) ( fourth l ) ( fifth l ) )       
        ( line ( third l ) ( fourth l ) ( fifth l ) )       
      )
    )
  )
)

;Two random machine players play one random game.
;
( defmethod demo-random-random (&aux p x o)
  ( setf x ( make-instance 'random-machine-player ) )
  ( setf o ( make-instance 'random-machine-player ) )
  ( setf p ( generic-play x o t ) )
  ( format t "~A~%" p )
  ( visualize p )
  ( format t "~A~%" ( analyze p ) )
  nil
)

; A random machine player plays one game agianst a human player.
;
( defmethod demo-random-human (&aux p x o)
  ( setf x ( make-instance 'random-machine-player ) )
  ( setf o ( make-instance 'human-player ) )
  ( setf p ( generic-play x o t ) )
  ( format t "~A~%" p )
  ( visualize p )
  ( format t "~A~%" ( analyze p ) )
  nil
)



(defmethod stats ((n number) (demo t) &aux w l d p result)
	(if demo (format t "BEGIN GATHERING STATISTICS ...~%"))
	(setf w 0 l 0 d 0)
	(dotimes (i n)
		(setf p (play))
		(if demo (format t "~A~%" p))
		(if demo (visualize p))
		(setf result (analyze p))
		(if demo (format t "~A~%" result))
		(cond
			((eq result 'w) (setf w (+ w 1)))
			((eq result 'l) (setf l (+ l 1)))
			((eq result 'd) (setf d (+ d 1)))
		)
	)
	(setf results (mapcar #'probability (list w l d) (list n n n)))
	(if demo (format t "END GATHERING STATISTICS~%"))
	(mapcar #'list '(w l d) results)
)

(defmethod probability ((special integer) (total integer))
	(/ (float special) (float total))
)

(defmethod demo (&aux p)
	(setf p (play))
	(format t "~A~%" p)
	(visualize p)
	(format t "~A~%" (analyze p))
	nil
)



;simulate the play of a game
(defmethod play (&aux play avail move)
	(setf play())
	(setf avail '(nw n ne w c e sw s se))
	(dolist (player '(x o x o x o x o x))
		(cond
			(	(eq player 'x)
				(setf move (select avail))
				(setf avail (remove move avail))
				(setf play (snoc move play))
			)
			(	(eq player 'o)
				(setf move (select avail))
				(setf avail (remove move avail))
				(setf play (snoc move play))
			)
		)
	)
	play
)

(defmethod visualize ((list list) &aux avail spot)
	(setf avail '(nw n ne w c e sw s se))
	(dolist (x avail)
		(cond
			(
				(or (null (position x list)) (>= (position x list) (length list)))
				(format t "~A" (concatenate 'string "--" " "))
			)
			(
				t
				(setf spot (position x list))
				(cond
					(
						(= 0 (mod (+ spot 1) 2))
						(format t "~A" (concatenate 'string "O" (write-to-string (/ (+ spot 1) 2)) " "))
					)
					(
						t
						(format t "~A" (concatenate 'string "X" (write-to-string (/ (+ spot 2) 2)) " "))
					)
				)
			)
		)
		(if (= 0 (mod (+ (position x avail) 1) 3))
			(terpri)
		)
	)
)

(defmethod analyze ((list list) &aux x-list o-list win-list result)
	(setf x-list '())
	(setf o-list '())
	(setf win-list '((nw n ne) (w c e) (sw s se) (nw w sw) (n c s) (ne e se) (nw c se) (ne c sw)))
	(setf result 'D)
	(block b0
		(dolist (element list)
			(cond
				(
					(eq (mod (+ (position element list) 1) 2) 0)
					(setf o-list (snoc element o-list))
				)
				(
					t
					(setf x-list (snoc element x-list))
				)
			)
			(if (> (+ (position element list) 1) 5)
				(if (eq T (is-list-present win-list o-list))
					(progn
						(setf result 'L)
						(return-from b0)
					)
				)
			)
			(if (> (+ (position element list) 1) 4)
				(if (eq T (is-list-present win-list x-list))
					(progn
						(setf result 'W)
						(return-from b0)
					)
				)
			)
		)
	)
	result
)

(defmethod line (p1 p2 p3 &aux temp val)
	(setf temp (eq p1 p2))
	(cond
		(
			(not temp)
			(setf val temp)
		)
		(
			t
			(setf val (eq p2 p3))
		)
	)
	val
)


(defmethod list-contains (lst sym &aux val)
	(setf val T)
	(if (null (find sym lst))
		(setf val nil)
	)
	val
)


(defmethod is-list-present (big-list small-list &aux val)
	(setf val nil)
	(block b1
		(dolist (temp-list big-list)
			(block b2
				(dolist (ele temp-list)
					(cond
						(
							(list-contains small-list ele)
							(setf val T)
						)
						(
							t
							(setf val nil)
							(return-from b2)
						)
					)
				)
			)
			(if (eq val T)
				(return-from b1)
			)
		)
	)
	val
)