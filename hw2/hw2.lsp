; Head Info: BFS function. Compute breadth first search on a TREE and return a list of terminal nodes in the order (left-to-right). Check shallowest node first.
; Solution:  First check base cases, if the TREE is null, return nil. If it is an atom, then it has only one node, return it as a list.
; If the first element is atom, then it is the shallowest node, take it as the first element of the input and recursive call the BFS on the rest. 
; Otherwise, if the first element is a list, we may want to check other subtree that if there is other shallower node. Move the element to the end of tree and append with the rest list. Then recursively call BFS function on them. Take the TREE as a queue. 

(defun BFS (TREE)
  	(cond 
	  ((null TREE) NIL)
	  ((atom TREE) (list TREE)) ; ’a -> (a)
	  ((atom (car TREE)) (append (list (car TREE)) (BFS (cdr TREE)))); if head element is an atom, 
	  (t (BFS (append (cdr TREE) (car TREE)))) ; If the head element is a list, 
	  )
)
;Test cases:
;(BFS '((W X)(Y Z))) -> (W X Y Z)
;(BFS '((A (B)) C (D))) -> (C A D B)

;;*************************************

;Head Info: DFS function. Compute Depth first search on a TREE. It takes a single argument TREE that is the list representation of the tree, and returns a single, top-level list of the terminal nodes in the order they would be visited by a right-to-left depth-first search. 
;Solution: Check base cases first. If the TREE is null, return nil. If it is an atom, return a list. 
; if the subtree is atom, then the search has reached the maximum depth of this subtree. Take the element and expand to the end of the output list, then recursively call DFS on the rest list. 
; Since we need right-to-left order, 

; Test cases: 
; (dfs '((A (B)) C (D))) -> (D C B A)

(defun DFS (TREE)
	(cond
	  ((NULL TREE) NIL)
	  ((atom TREE) (list TREE)) ; 'a -> (a)
	  (t (append (DFS (cdr TREE)) (DFS (car TREE))));right-to-left order, move the first element to the end of the output list. Recursively call them as input.
	 )
)
;;*************************************
; Helper function for DFID. The logic is similary as DFS, but the order is left-to-right. 
; And cutoff if the depth is maxed out. 
(defun DFID_H (TREE LIMIT)
	(cond
	  ((or (< LIMIT 0) (NULL TREE)) NIL)
	  ((atom TREE) (list TREE))
	  (t (append (DFID_H (car TREE) (- LIMIT 1)) (DFID_H (cdr TREE) LIMIT)))
	 )
 )
;Head info: top-level function, called DFID, take two arguments, the list representation of the tree - TREE, and an integer representing the maximum depth of the tree - LIMIT
; and returns a single top-level list of the terminal nodes in the order that they would be visited by a left-to-right depth-first iterative-deepening search.
; In short, DFID is combination of BFS+DFS. Base Case: if the TREE is null, return nil. And if the initial depth LIMIT is smaller than 0, return nil. 
; Recursively call the helper function DFID_H, depth increments until it reaches the maximum depth. 
(defun DFID (TREE LIMIT)
	(cond
	  ((or (< LIMIT 0) (NULL TREE)) NIL)
	  (t (append (DFID TREE (- LIMIT 1)) (DFID_H TREE LIMIT)))
	 )
 )


;;*************************************

; These functions implement a depth-first solver for the missionary-cannibal
; problem. In this problem, three missionaries and three cannibals are trying to
; go from the east side of a river to the west side. They have a single boat
; that can carry two people at a time from one side of the river to the
; other. There must be at least one person in the boat to cross the river. There
; can never be more cannibals on one side of the river than missionaries. If
; there are, the cannibals eat the missionaries.

; In this implementation, a state is represented by a single list
; (missionaries cannibals side). side represents which side the boat is
; currently on, and is T if it is on the east side and NIL if on the west
; side. missionaries and cannibals represent the number of missionaries and
; cannibals on the same side as the boat. Thus, the initial state for this
; problem is (3 3 T) (three missionaries, three cannibals, and the boat are all
; on the east side of the river) and the goal state is (3 3 NIL).

; The main entry point for this solver is the function MC-DFS, which is called
; with the initial state to search from and the path to this state. It returns
; the complete path from the initial state to the goal state: this path is a
; list of intermediate problem states. The first element of the path is the
; initial state and the last element is the goal state. Each intermediate state
; is the state that results from applying the appropriate operator to the
; preceding state. If there is no solution, MC-DFS returns NIL.

; To call MC-DFS to solve the original problem, one would call (MC-DFS '(3 3 T)
; NIL) -- however, it would be possible to call MC-DFS with a different initial
; state or with an initial path.

; Examples of calls to some of the helper functions can be found after the code.

; FINAL-STATE takes a single argument s, the current state, and returns T if it
; is the goal state (3 3 NIL) and NIL otherwise.
;  (missionaries cannibals side)
; Done
(defun final-state (s)
	(equal s '(3 3 NIL)) ; return t if it is the goal state.
)

; NEXT-STATE returns the state that results from applying an operator to the
; current state. It takes three arguments: the current state (s), a number of
; missionaries to move (m), and a number of cannibals to move (c). 

; It returns a list containing the state that results from moving that number of missionaries
; and cannibals from the current side of the river to the other side of the
; river.
; 1. If applying this operator results in an invalid state (because there
; are more cannibals than missionaries on either side of the river, 
; 2. or because it would move more missionaries or cannibals than are on this side of the
; river) it returns NIL.
;
; NOTE that next-state returns a list containing the successor state (which is
; itself a list); the return should look something like ((1 1 T)).
; all inputs are legal, no need to validate it. 
; (current-state, #of miss, #of cann)
(defun next-state (s m c)
  (let* (
	(n_miss (car s)) ;number of missionaries at boat side
    	(n_cann (cadr s)) ;number of cannibals at boat side
	(side (third s)) ;boat on left or right
	(new_miss (+ m (- 3 n_miss))) ;new numer after move
    	(new_cann (+ c (- 3 n_cann)))
	)
  	(cond
	  ((or (> m n_miss) (> c n_cann)) NIL) ; check if the action is valid. If person moved is more than total, i.e., outnumbers, returns nil
	  ((and (> new_cann new_miss) (> new_miss 0)) NIL) ;if there is missionaries, and # of cannibals are more than missionaries in one side, it is not legal since miss would be eaten
	  ((and (> (- n_cann c) (- n_miss m)) (> (- n_miss m) 0)) NIL) ;check the opposite side of the boat after moved. 
	  (t (list (list new_miss new_cann (not side)))) ;return next state, since the boat goes to other side, boolean it, 
	)
	)
 )


; SUCC-FN returns all of the possible legal successor states to the current
; state. It takes a single argument (s), which encodes the current state, and
; returns a list of each state that can be reached by applying legal operators
; to the current state.
; Directly append it, nil would not matter in append function. But note that it matters in list function. 
; The order doesn't not matter
; a boat can only care two person. 5 possible actions. 
; Done
(defun succ-fn (s)
  	(append
	  (next-state s 0 1) ;5 possible legal successor states
	  (next-state s 1 0)
	  (next-state s 1 1)
	  (next-state s 0 2)
	  (next-state s 2 0)
	)
 )

; ON-PATH checks whether the current state is on the stack of states visited by
; this depth-first search. It takes two arguments: the current state (s) and the
; stack of states visited by MC-DFS (states). It returns T if s is a member of
; states and NIL otherwise.
(defun on-path (s states)
  	(cond
	  ((NULL states) NIL) ;base case, return nil if the state is null
	  ((equal s (car states)) t)
	  (t (on-path s (cdr states)))
	)
 )

; MULT-DFS is a helper function for MC-DFS. It takes two arguments: a stack of
; states from the initial state to the current state (path), and the legal
; successor states from the current state (states).
; MULT-DFS does a depth-first search on each element of states in
; turn. If any of those searches reaches the final state, MULT-DFS returns the
; complete path from the initial state to the goal state. Otherwise, it returns
; NIL. 
; Note that the path should be ordered as: (S_n ... S_2 S_1 S_0)
(defun mult-dfs (states path)
  	(cond
	  ((null states) NIL) ;return nil if the state is null
	  ((mc-dfs (car states) path) (mc-dfs(car states) path)) ;
	  (t (mult-dfs (cdr states) path)) ;call mult-dfs on the rest of states
	 ) 
  )

; MC-DFS does a depth first search from a given state to the goal state. It
; takes two arguments: a state (S) and the path from the initial state to S
; (PATH).  
; 1. If S is the initial state in our search, PATH should be NIL. MC-DFS
; performs a depth-first search starting at the given state. It returns the path
; from the initial state to the goal state, if any, or NIL otherwise.
;
; 2. MC-DFS is responsible for checking if S is already the goal state, as well as for
; ensuring that the depth-first search does not revisit a node already on the
; search path.
(defun mc-dfs (s path)
	(cond
	  ((final-state s) (reverse (append (list s) path))) ;if the initial state is the goal state, return it. Or if the final state has been reached, append to path. And reverse finally. 
	  ((on-path s path) NIL) ;if S is already in the path, return nil. Avoid infinite loop.
	  (t (mult-dfs (succ-fn s) (append (list s) path)))
	 )
 )
; CL-USER> (mc-dfs '(3 3 T) NIL)
; ((3 3 T) (1 1 NIL) (3 2 T) (0 3 NIL) (3 1 T) (2 2 NIL) (2 2 T) (3 1 NIL) (0 3 T) (3 2 NIL) (0 2 T) (3 3 NIL))
; Checked. 
