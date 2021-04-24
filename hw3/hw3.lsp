;
; CS161 Hw3: Sokoban
; 
; *********************
;    READ THIS FIRST
; ********************* 
;
; All functions that you need to modify are marked with 'EXERCISE' in their header comments.
; Do not modify a-star.lsp.
; This file also contains many helper functions. You may call any of them in your functions.
;
; *Warning*: The provided A* code only supports the maximum cost of 4999 for any node.
; That is f(n)=g(n)+h(n) < 5000. So, be careful when you write your heuristic functions.
; Do not make them return anything too large.
;
; For Allegro Common Lisp users: The free version of Allegro puts a limit on memory.
; So, it may crash on some hard sokoban problems and there is no easy fix (unless you buy 
; Allegro). 
; Of course, other versions of Lisp may also crash if the problem is too hard, but the amount
; of memory available will be relatively more relaxed.
; Improving the quality of the heuristic will mitigate this problem, as it will allow A* to
; solve hard problems with fewer node expansions.
; 
; In either case, this limitation should not significantly affect your grade.
; 
; Remember that most functions are not graded on efficiency (only correctness).
; Efficiency can only influence your heuristic performance in the competition (which will
; affect your score).
;  
;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; General utility functions
; They are not necessary for this homework.
; Use/modify them for your own convenience.
;

;
; For reloading modified code.
; I found this easier than typing (load "filename") every time. 
;
(defun reload()
  (load "hw3.lsp")
  )

;
; For loading a-star.lsp.
;
(defun load-a-star()
  (load "a-star.lsp"))

;
; Reloads hw3.lsp and a-star.lsp
;
(defun reload-all()
  (reload)
  (load-a-star)
  )

;
; A shortcut function.
; goal-test and next-states stay the same throughout the assignment.
; So, you can just call (sokoban <init-state> #'<heuristic-name>).
; 
;
(defun sokoban (s h)
  (a* s #'goal-test #'next-states h)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; end general utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We now begin actual Sokoban code
;

; Define some global variables
(setq blank 0)
(setq wall 1)
(setq box 2)
(setq keeper 3)
(setq star 4)
(setq boxstar 5)
(setq keeperstar 6)

; Some helper functions for checking the content of a square
(defun isBlank (v)
  (= v blank)
  )

(defun isWall (v)
  (= v wall)
  )

(defun isBox (v)
  (= v box)
  )

(defun isKeeper (v)
  (= v keeper)
  )

(defun isStar (v) ;goal
  (= v star)
  )

(defun isBoxStar (v)
  (= v boxstar)
  )

(defun isKeeperStar (v)
  (= v keeperstar)
  )

;
; Helper function of getKeeperPosition
;
(defun getKeeperColumn (r col)
  (cond ((null r) nil)
	(t (if (or (isKeeper (car r)) (isKeeperStar (car r)))
	       col
	     (getKeeperColumn (cdr r) (+ col 1))
	     );end if
	   );end t
	);end cond
  )

;
; getKeeperPosition (s firstRow)
; Returns a list indicating the position of the keeper (c r).
; 
; Assumes that the keeper is in row >= firstRow.
; The top row is the zeroth row.
; The first (right) column is the zeroth column.
;
(defun getKeeperPosition (s row)
  (cond ((null s) nil)
	(t (let ((x (getKeeperColumn (car s) 0)))
	     (if x
		 ;keeper is in this row
		 (list x row)
		 ;otherwise move on
		 (getKeeperPosition (cdr s) (+ row 1))
		 );end if
	       );end let
	 );end t
	);end cond
  );end defun

;
; cleanUpList (l)
; returns l with any NIL element removed.
; For example, if l is '(1 2 NIL 3 NIL), returns '(1 2 3).
;
(defun cleanUpList (L)
  (cond ((null L) nil)
	(t (let ((cur (car L))
		 (res (cleanUpList (cdr L)))
		 )
	     (if cur 
		 (cons cur res)
		  res
		 )
	     );end let
	   );end t
	);end cond
  );end 

;;================================================

; EXERCISE: Modify this function to return true (t)
; iff s is a goal state of the game.

; (neither any boxes nor the keeper is on a non-goal square)
;
; Currently, it always returns NIL. If A* is called with
; this function as the goal testing function, A* will never
; terminate until the whole search space is exhausted.

; Base on the data structure of the state, Check every row and every eleemnts within each row recursively. 
; If detect a box (2), so the box should not in the goal, returns nil. 
; 

(defun goal-test (s)
  	(cond
	  ((null s) t); finish searching, do not find anything not satisfy, return true
	  ((atom s) 
	   	(cond 
		   ((isBox s) nil)
		   ((isKeeper s) nil) ; if find any keeper or box bit, it is not goal state.
		   (t T)
		      )) ; if find any keeper or box bit, it is not goal state.
	  (t (and (goal-test (car s)) (goal-test (cdr s )))) ;if current square is legal, recursion. It has to be all squares are legal.
	  );end cond
  );end defun

;;================================================
; EXERCISE: Modify this function to return the list of 
; sucessor states of s.
;
; This is the top-level next-states (successor) function.
; Some skeleton code is provided below.
; You may delete them totally, depending on your approach.
; 
; If you want to use it, you will need to set 'result' to be 
; the set of states after moving the keeper in each of the 4 directions.
; A pseudo-code for this is:
; 
; ...
; (result (list (try-move s UP) (try-move s DOWN) (try-move s LEFT) (try-move s RIGHT)))
; ...
; 
; You will need to define the function try-move and decide how to represent UP,DOWN,LEFT,RIGHT.
; Any NIL result returned from try-move can be removed by cleanUpList.
; 
;
;;;;; Helper functionS for next-state
; helper function to get square content
; takes argument state(s), row number (r), column number(c) -> integer content of state S at square (r,c)
; if the square is outside the scope of the problem, return the value of a wall.

; take row out first, then the square
; define a function which can take out nth element from a list, from campuswire #202 

(defun helper_nth (n lst)
	(cond
	  ((> n 0) (car (nthcdr n lst)))
	  ((= n 0) (car lst))
	  (t wall) ; < 0 , return value of wall
	)
)

; get size of the row
;(defun get-num-col (s)
;	(cond ((null s) 0)
;	      (t (length (car s))))
; )
; get size of the state
;(defun get-num-row (s)
;  	(cond ((null s) 0)
;	      (t (length s))
;))

(defun get-square (s r c)
	(if (or (< r 0) (< c 0)) wall
		;(> r (- (get-num-row s) 1)) (> c (- get-num-col s) 1))
	(let* (
		(row (helper_nth r s))
		(square (helper_nth c row)))
		(cond
			((null square) wall); if outside, return the value of wall. (1)
			(t square)
		); end cond
	); end let*
;	wall) ; end if, for any r, c <0, return wall
)
); end defun

; takes arguemnt (s r c v), v is a square content (integer).   -> New state s'
; This function should return a new state S' that obtained by setting the square (r, c) to value v.
; The function should not modify the input state! 
; top left corner (0, 0)
; helper function, take a list as argument, return a new list with a element changed. 

(defun set-element (L index val)
  	(cond
	 	 ((null L) nil)
		 ((= index 0) (cons val (cdr L)));replace the first element
		 (t (cons (car L) (set-element (cdr L) (- index 1) val)) ) ;iterative that replace the nth element
	 )

 ); end defun

; return a new states
(defun set-square (s r c v)
  	;(let* 
	 ; (row (helper_nth r s))
	 ; (square (helper_nth c row))
	 ; (new_row (set-element row c v))
	  (cond
		((null s) nil)
		;((= r 0) (cons new_row (cdr s)))
		((= r 0) (cons (set-element (car s) c v) (cdr s)))
		(t (cons (car s) (set-square (cdr s) (- r 1) c v)))
	   ); end cond
	; )
 )

; takes in a state S and move direction D. 
; Return the state that is the result of moving the keeper in state S in direction D. 
; If the move is invalid -> NIL. (e.g., a wall in that direction)
; in a word, retruns the result if the move is successful.
; note that update the content of every square to the right value. 

; Helper function to check if the move is valid, if not, returns nil.
; Below condition is invalid:
; 1. wall in the direction
; 2. Box cannot move in that direction
; check two adjacent square
(defun check_move (dir_sq_val box_next_sq_val)
	(cond 
	  ((isWall dir_sq_val) nil) ;if the direction square is wall, then it is invalid. 
	  ; the box in the direction, i.e., the keeper is pushing the box
	  ; However, it is invalid if the next square in the direction next to box is wall or a box.
	  ; or we could say it has to be blank (0) or a goal (4). 
	  ((isBox dir_sq_val) (OR (isBlank box_next_sq_val) (isStar box_next_sq_val))); either works, if both false, return nil
	  ; if it is a boxstar, then same as what we have in the last line,
	  ((isBoxStar dir_sq_val)  (OR (isBlank box_next_sq_val) (isStar box_next_sq_val)));
	  (t T)
	  );end cond
 ); end defun

(defun move_and_set (S row col row_1 col_1 row_2 col_2)
  	(let ((k_val  (get-square S row col))
	      (sq1_value (get-square S row_1 col_1))
		(sq2_value (get-square S row_2 col_2)))
  	 ;if the position is keeper, and with checking done, the move is valid, then continue
  	(if  (check_move (get-square S row_1 col_1) (get-square S row_2 col_2))
		; then
		(cond	;Two main cases.
		  	; 1. Keeper move from a blank, the previous square (k_val here) resest to blank
		  	((isKeeper k_val)
			 	(cond
				  ;; 1.1 Not pushing
				  ;;; move to a blank -> change 2 squares
				  ((isBlank sq1_value) (set-square (set-square S row_1 col_1 keeper) row col blank) )
				  ;;; move to a goal -> change 2 squares
				  ((isStar sq1_value) (set-square (set-square S row_1 col_1 keeperstar) row col blank) )
				  ;; 1.2  pushing a box -> change 3 squares, sq1 -> keeper
				  ;;; box in the blank (box), move to a blank -> sq2 box
				  ((and (isBox sq1_value) (isBlank sq2_value)) 
				   	(set-square 
					  (set-square 
					    (set-square s row_2 col_2 box) row_1 col_1 keeper) row col blank))
				  ;;; box in the blank, move to goal -> sq2 boxstar
				  ((and (isBox sq1_value) (isStar sq2_value))
				   	(set-square
					  (set-square 
					    (set-square S row_2 col_2 boxstar) row_1 col_1 keeper) row col blank))
				  ;;; box in the goal, move to blank. sq1 -> keeperstar
			          ((and (isBoxStar sq1_value) (isBlank sq2_value))
				   	(set-square 
					  (set-square
					    (set-square S row_2 col_2 box ) row_1 col_1 keeperstar) row col blank))
				  ;;; box in the goal, move to another goal. sq1-> keepstar
				  ((and (isBoxStar sq1_value) (isStar sq2_value)) 
				   	(set-square 
					  (set-square
					    (set-square S row_2 col_2 boxstar ) row_1 col_1 keeperstar) row col blank))
				); end cond
				);end case1
			;; 2. Keeper move from a goal, the previous square that the keeper stay is reset to goal (k_val -> star)
			((isKeeperStar k_val)
			 	(cond
				  ; NOt pushing
				 ((isBlank sq1_value) (set-square (set-square S row_1 col_1 keeper) row col star))
				 ((isStar sq1_value) (set-square (set-square S row_1 col_1 keeperstar) row col star))
				 ; pushing a box
				 ;; box in blank
				 ;; blank -> blank
				 ((and (isBox sq1_value) (isBlank sq2_value)) 
				  	(set-square 
					  (set-square
					    (set-square S row_2 col_2 box ) row_1 col_1 keeper) row col star))
				 ;; blank -> goal
				 ((and (isBox sq1_value) (isStar sq2_value)) 
				  	(set-square 
					  (set-square
					    (set-square S row_2 col_2 boxstar ) row_1 col_1 keeper) row col star))
				 ;; box in a goal
				 ;;; goal -> blank
				 ((and (isBoxStar sq1_value) (isBlank sq2_value)) 
				  	(set-square 
					  (set-square
					    (set-square S row_2 col_2 box ) row_1 col_1 keeperstar) row col star))
				 ;;; goal -> goal
				 ((and (isBoxStar sq1_value) (isStar sq2_value)) 
				  	(set-square 
					  (set-square
					    (set-square S row_2 col_2 boxstar ) row_1 col_1 keeperstar) row col star))		
				  ); end cond 
				 ) ;end case2
		);end cond
	 ); end if
	); end let	
 )


(defun try-move (S D row col)
  	(cond 
	  ((equal D 'UP) ;row-1
		 (move_and_set S row col (- row 1) col (- row 2) col)
	   ) ; end move up case
	  ((equal D 'DOWN) ;row+1
		(move_and_set S row col (+ row 1) col (+ row 2) col)		 
	   ) ; end move down case
	  ((equal D 'LEFT) ;colmun-1
		(move_and_set S row col row (- col 1) row (- col 2))
	    ); end move left case
	  ((equal D 'RIGHT) ;colmun+1
		(move_and_set S row col row (+ col 1) row (+ col 2))		 
	   ) ;end move right case
	 );end cond
 ); end defun



(defun next-states (s)
  (let* ((pos (getKeeperPosition s 0)) ; (c r)
	 (x (car pos)) ;keeper's column
	 (y (cadr pos)) ;row
	 ;x and y are now the coordinate of the keeper in s.
	 (result (list (try-move s 'UP y x) (try-move s 'DOWN y x) (try-move s 'LEFT y x) (try-move s 'RIGHT y x)))
	 )
    (cleanUpList result);end
   );end let
  ); end defun


;;==============================================

; EXERCISE: Modify this function to compute the trivial 
; admissible heuristic.
; Heuristic Function that returns the constant 0, trivial admissible

(defun h0 (s)
 0)


;; ============================================


; EXERCISE: Modify this function to compute the 
; number of misplaced boxes in s.

; helper function to count number of box in each row (list)
; it return number of boxes (char 2) in current row


;; Method 1 for h1 Function, by using count function to get total number of atom 2 in the state
(defun count_box_row (L) ;argument: a list (row)
  (cond 
      ((null L) 0)
      (t (count box L))
      ); end cond
); end defun

(defun h1_method1 (s)
 (cond 
    ((null s) 0)
    ((atom s) (if (isBox s) 1 0))
    (t (+ (count_box_row (car s)) (h1_method1 (cdr s))))
    )
 )

;; Method 2 for h1 function, normally iterative each square and add the number of boxes.
(defun h1_method2 (s)
	(cond 
	  	((null s) 0)
		((atom s) ;square
		 	(cond
			  ((isBox s) 1) ;if the square is box, add 1
			      (t 0)) ;otherwise, return 0
			); end cond 
		(t (+ (h1_method2 (car s)) (h1_method2 (cdr s)))); recursion check each square within each row. Return the total number eventually. 
	);end cond
);end defun

; Return # of boxes which are not on goal positions in the given state. Use count funciton. i.e., # of 2 in s
; Note that #of goals ≥ ( #of boxes + 1)
; This function is admissible. It never overestimate the cost of reaching the goal. i.e., the number of moves needed to terminate the game

(defun h1 (s)
  	(or (h1_method2 s) (h1_method1 s));both method works, i implement both of them.
)

;; ==========================================

; EXERCISE: Change the name of this function to h<UID> where
; <UID> is your actual student ID number. Then, modify this 
; function to compute an admissible heuristic value of s. 
; 
; This function will be entered in the competition.
; Objective: make A* solve problems as fast as possible.
; The Lisp 'time' function can be used to measure the 
; running time of a function call.
;
; takes in state and returns an integer 

; note that h1 returns number of boxes. 
; we can make a new function to get position of these boxes.





; my baisc idea is to get the mehantan distance for each box and goal. 

(defun h405346443 (s)
	(let* (
	      (boxes (cleanUpList (get-items s 2)))
	     ; (goals (cleanUpList (getGoalPosition s 0 0)))
	     ; (single_keeper (getKeeperPosition s 0)))
	     )
	  (cond
	    ((null boxes) 0)
	    ((have_cornered s boxes) 1500)
	    (t (h1 s))
	    );if any box is in corner, assign a high heuristic value, never check it since it cannot lead to goal state. 
	    )
)
 
;;;;;;;Check corner
; A helper function to deduct if a box is in the corner. i.e., there is two wall arrounding the box, this box cannot be moved anymore.
; So if it is in the corner, it is call "cornered", this function returns true. 
; input format (r, c)
(defun isInCorner (s box_pos)
	(let ((up (get-square s (- (car box_pos) 1) (cadr box_pos)))
		(down (get-square s (+ (car box_pos) 1) (cadr box_pos)))
	     	(right (get-square s (car box_pos)  (+ (cadr box_pos) 1)))
		(left (get-square s (car box_pos)  (- (cadr box_pos) 1)))
	      )(cond
		  	; Four cornered cases
			((AND (isWall up)   (isWall left)) t)
			((AND (isWall down) (isWall left)) t)
			((AND (isWall up)   (isWall right)) t)
			((AND (isWall down) (isWall right)) t)
			(t nil) ;otherwise, it is not cornered, return nil.
		); end cond
	); end let
)

; function take the list of boxes coordinate  
;, check if any of them are in "cornered" state, once funnd, assign a high arbitary heuristic value. 
(defun have_cornered (s boxes)
	(cond
	  ((null boxes) '()) ;no boxes postion, means no boxes
	  ((atom (car boxes)) ; it means it is a single list (r, c)
		(isInCorner s boxes)) 
	  ;check if the box is cornered. If returns nil, it is not, go to next step. It returns t, the function can stop cause we just need one. 
	  (t  (or (have_cornered s (car boxes)) (have_cornered s (cdr boxes))))
))


;;;;;;;Get box
; function to read box coordinate postion and return (c r).
; Note that start from r = 0, c =0
; note that use cleanupList
(defun getBoxPosition (s row col)
    (cond 
      ((null s) '())
         ((atom s) 
            (if (isBox s)
                (list row col)
                '()))
         (t (if (atom (car s))
                (cons (getBoxPosition (car s) row col) (getBoxPosition (cdr s) row (+ col 1)))
                (append (getBoxPosition (car s) row col) (getBoxPosition (cdr s) (+ row 1) col)); otherwise,next row
            ); end if
         ); end t
    ); end cond
    
)
;;;;;;;;Get goal

; function to get goal postion like getBoxPosition
(defun getGoalPosition (s row col)
    ( cond ((null s) '())
         ((atom s) 
            (if (isStar s)
                (list row col) '())
         )
         (t (if (atom (car s))
                (cons (getGoalPosition (car s) row col) (getGoalPosition (cdr s) row (+ col 1)))
                (append (getGoalPosition (car s) row col) (getGoalPosition (cdr s) (+ row 1) col)); otherwise, next row
            ); end if
         ); end t
    ); end cond
); end defun

;;;;;;;;;;




  
 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Some predefined problems.
 | Each problem can be visualized by calling (printstate <problem>). For example, (printstate p1).
 | Problems are ordered roughly by their difficulties.
 | For most problems, we also provide a number which indicates the depth of the optimal solution.
 | These numbers are located at the comments of the problems. For example, the first problem below has optimal solution depth 6.
 | As for the solution depth, any admissible heuristic must make A* return an optimal solution. So, the depths of the optimal solutions provided could be used for checking whether your heuristic is admissible.
 |
 | Warning: some problems toward the end are quite hard and could be impossible to solve without a good heuristic!
 | 
 |#
;(6)
(setq p1 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 4 0 4 1)
	   (1 1 1 1 1 1)));h1: 1

;(15)
(setq p2 '((1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 1) 
	   (1 0 0 0 0 0 1) 
	   (1 0 0 2 1 4 1) 
	   (1 3 4 0 1 0 1)
	   (1 1 1 1 1 1 1)));h1: 1

;(13)
(setq p3 '((1 1 1 1 1 1 1 1 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 2 0 3 4 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 4 0 1 0 0 0 1)
	   (1 1 1 1 1 1 1 1 1)));h1: 1

;(17)
(setq p4 '((1 1 1 1 1 1 1)
	   (0 0 0 0 0 1 4)
	   (0 0 0 0 0 0 0)
	   (0 0 1 1 1 0 0)
	   (0 0 1 0 0 0 0)
	   (0 2 1 0 0 4 0)
	   (0 3 1 0 0 0 0))); h1:1

;(12)
(setq p5 '((1 1 1 1 1 1)
	   (1 1 0 0 1 1)
	   (1 0 0 0 0 1)
	   (1 4 2 2 4 1)
	   (1 0 0 0 4 1)
	   (1 1 3 1 1 1)
	   (1 1 1 1 1 1)));h1:2

;(13)
(setq p6 '((1 1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 4 1)
	   (1 4 0 0 2 2 3 1)
	   (1 0 0 1 0 0 4 1)
	   (1 1 1 1 1 1 1 1)))

;(47)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
	   (0 0 1 1 1 1 4 0 0 3)
	   (0 0 0 0 0 1 0 0 0 0)
	   (0 0 0 0 0 1 0 0 1 0)
	   (0 0 1 0 0 1 0 0 1 0)
	   (0 2 1 0 0 0 0 0 1 0)
	   (0 0 1 0 0 0 0 0 1 4)))

;(22)
(setq p8 '((1 1 1 1 1 1)
	   (1 4 0 0 4 1)
	   (1 0 2 2 0 1)
	   (1 2 0 1 0 1)
	   (1 3 4 0 4 1)
	   (1 1 1 1 1 1)))

;(34)
(setq p9 '((1 1 1 1 1 1 1 1 1) 
	   (1 1 1 0 0 1 1 1 1) 
	   (1 0 0 0 0 0 2 0 1) 
	   (1 0 1 0 0 1 2 0 1) 
	   (1 0 4 4 4 1 3 0 1) 
	   (1 1 1 1 1 1 1 1 1)))

;(59)
(setq p10 '((1 1 1 1 1 0 0)
	    (1 4 0 0 1 1 0)
	    (1 3 2 0 0 1 1)
	    (1 1 0 2 0 0 1)
	    (0 1 1 0 2 0 1)
	    (0 0 1 1 0 0 1)
	    (0 0 0 1 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 1 1)))

;(?)
(setq p11 '((0 0 1 0 0 0 0)
	    (0 2 1 4 0 4 0)
	    (0 2 0 4 0 0 0)	   
	    (3 2 1 1 1 4 0)
	    (0 0 1 4 0 0 0))) ;h1:3

;(41) 
;run: 579s
(setq p12 '((1 1 1 1 1 0 0 0)
	    (1 0 0 4 1 0 0 0)
	    (1 2 1 0 1 1 1 1)
	    (1 4 0 0 0 0 0 1)
	    (1 0 0 5 0 5 0 1)
	    (1 0 5 0 1 0 1 1)
	    (1 1 1 0 3 0 1 0)
	    (0 0 1 1 1 1 1 0))) ;h1:1

;(?)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
	    (1 3 0 0 1 0 0 4 4 1)
	    (1 0 2 0 2 0 0 4 4 1)
	    (1 0 2 2 2 1 1 4 4 1)
	    (1 0 0 0 0 1 1 4 4 1)
	    (1 1 1 1 1 1 0 0 0 0))) ;h1:5

;(?)
(setq p14 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 4 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)	    
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
	    )) ; h1:2

;(?)
(setq p15 '((0 0 1 1 1 1 1 1 1 0)
	    (1 1 1 0 0 1 1 1 1 0)
	    (1 0 0 2 0 0 0 1 1 0)
	    (1 3 2 0 2 0 0 0 1 0)
	    (1 1 0 2 0 2 0 0 1 0)
	    (0 1 1 0 2 0 2 0 1 0)
	    (0 0 1 1 0 2 4 0 1 0)
	    (0 0 0 1 1 1 1 0 1 0)
	    (0 0 0 0 1 4 1 0 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 0 1 4 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 1 1 1 1 1))); h1:8

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Utility functions for printing states and moves.
 | You do not need to understand any of the functions below this point.
 |#

;
; Helper function of prettyMoves
; from s1 --> s2
;
(defun detectDiff (s1 s2)
  (let* ((k1 (getKeeperPosition s1 0))
	 (k2 (getKeeperPosition s2 0))
	 (deltaX (- (car k2) (car k1)))
	 (deltaY (- (cadr k2) (cadr k1)))
	 )
    (cond ((= deltaX 0) (if (> deltaY 0) 'DOWN 'UP))
	  (t (if (> deltaX 0) 'RIGHT 'LEFT))
	  );end cond
    );end let
  );end defun

;
; Translates a list of states into a list of moves.
; Usage: (prettyMoves (a* <problem> #'goal-test #'next-states #'heuristic))
;
(defun prettyMoves (m)
  (cond ((null m) nil)
	((= 1 (length m)) (list 'END))
	(t (cons (detectDiff (car m) (cadr m)) (prettyMoves (cdr m))))
	);end cond
  );

;
; Print the content of the square to stdout.
;
(defun printSquare (s)
  (cond ((= s blank) (format t " "))
	((= s wall) (format t "#"))
	((= s box) (format t "$"))
	((= s keeper) (format t "@"))
	((= s star) (format t "."))
	((= s boxstar) (format t "*"))
	((= s keeperstar) (format t "+"))
	(t (format t "|"))
	);end cond
  )

;
; Print a row
;
(defun printRow (r)
  (dolist (cur r)
    (printSquare cur)    
    )
  );

;
; Print a state
;
(defun printState (s)
  (progn    
    (dolist (cur s)
      (printRow cur)
      (format t "~%")
      )
    );end progn
  )

;
; Print a list of states with delay.
;
(defun printStates (sl delay)
  (dolist (cur sl)
    (printState cur)
    (sleep delay)
    );end dolist
  );end defun
