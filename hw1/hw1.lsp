;Q1: TREE-CONTAINS
;Header info: N is a number and TREE is an ordered TREE
;The function is boolean function, it returns T if element N appears in TREE, otherwise returns NIL.
;Solution: First return NIL if the tree is null. If the tree is a list, compare its second element m to N. If equals, returns T.
;If not, check if N > m, then the function should check the right subtree. If N < m, check the left subtree. 
(defun TREE-CONTAINS (N TREE)
  (cond 
    ((null TREE) nil)
    ((atom TREE) (= N TREE))
    ((= N (cadr TREE)) t)
    ((< N (cadr TREE)) (TREE-CONTAINS N (car TREE)))
    ((> N (cadr TREE)) (TREE-CONTAINS N (third TREE)) )
   )	
)

;Q2: TREE-MIN
;Header info: takes one argument TREE, returns the min number appearing in the ordered tree TREE. 
;Solution: Recursivelt check First element of the list, if the argumnet is atom and a number, it will be the min number since the the TREE is an ordered tree. 
;The minimum number will be the most significant number of the list. So the function returns it. 
(defun TREE-MIN (TREE)
 (cond
    ((numberp TREE) TREE)
    (t (TREE-MIN (car TREE)))
  )
 )
;Q3 TREE-ORDER: 
;Header info: The function takes one argument TREE, and returns an pre-ordered list of the numbers appearing in the ordered tree TREE.
;Solution: if the argument itself is a atom number, then transform it to list. 
; Recursively check the list, orders the middle, left, right tree. Append and sort them in the order of middle, left, right.
(defun TREE-ORDER (TREE)
	(cond
	  ((numberp TREE)(list TREE))
	  (t (append (TREE-ORDER (cadr TREE)) (TREE-ORDER (car TREE)) (TREE-ORDER (third TREE))))
	)
 )

;Q4: SUB-LIST
;Header Info: takes a list L and two non-negative integers START and LEN, and returns the sub-list of L starting at position START and having length LEN. Assume that the first element of L has position 0.
;Solution: First if the L is null or LEN == 0, return nil. Recursively call the function with the rest elements of the list as input, START -= 1. Finally when START == 0,
; concrete a list starting with the first element of L, the rest of list as input, recursively call the funtion, with LEN -= 1. The function will end when LEN reaches 0.
(defun SUB-LIST (L START LEN)
 	(cond 
	  ((or (null L)(= LEN 0) ) nil)
	  ((= START 0) (cons (car L)(SUB-LIST (cdr L) 0 (- LEN 1))))
	  (t (SUB-LIST (cdr L) (- START 1) LEN))
	  )
 )

;Test case: 
;(SUB-LIST '(a b c d) 0 3) returns (a b c)
;(SUB-LIST '(a b c d) 3 1) returns (d)
;(SUB-LIST '(a b c d) 2 0) return s NIL
;----------------
;Q5: SPLIT-LIST
;Header Info: takes a list L, and returns a list of two lists L1 and L2, in that order, such that L is the result of appending L1 and L2
;and Length of L1 minus length of L2 is 0 or 1.
;Solution: Check if the length of the list L is odd or even. If even, the the length of two splitted list will be identical. If odd, then the length of 
;L1 is greater than L2's by 1. Use list function to make a new list. 
;Return a list that the first element is list L1 whose first element is by calling SUB-LIST (with argument L, START == 0, LEN == half of the length of L rounded up.)
;Return L2 as the second list element, call function SUB-LIST (with argument L, START as half of the length of L rounded up, LEN == half of the length of L rounded down).

(defun SPLIT-LIST (L)
  	(let* ((l_length (length L)))
	  (cond
	    	((evenp l_length) (list (SUB-LIST L 0 (/ l_length 2)) (SUB-LIST L (/ l_length 2) (/ l_length 2))))
		((oddp l_length) (list (SUB-LIST L 0 (/ (+ 1 l_length) 2)) (SUB-LIST L (/ (+ 1 l_length) 2) (/ (- l_length 1) 2))))
	  )
	)
)

;Test case: 
;(SPLIT-LIST '(a b c d)) returns ((a b) (c d))
;(SPLIT-LIST '(a b c d e)) returns ((a b c) (d e))
;(SPLIT-LIST '(a b c d e f)) returns ((a b c) (d e f))
;------------
;Q6: BTREE-HEIGHT
;Header info: takes a binary tree TREE, and returns the height of TREE. Note that the height of a binary tree is defined as the length of the longest path from the root node to the farthest leaf node.
; Solution: Return nil if the TREE is null. If the argument is atom, return 0.
; Recursively call the function itself on the left subtree and right subtree, at each level of the tree, add 1. Finally, Compare them and return the greater one. 
(defun BTREE-HEIGHT (TREE)
  	(cond
	  ((null TREE) nil)
	  ((atom TREE) 0)
	  (t (let* ((left_height (+ 1 (BTREE-HEIGHT (car TREE)))) (right_height (+ 1 (BTREE-HEIGHT(cadr TREE)))))
	       (if (> left_height right_height) left_height right_height)
	      )
	  )
	 )
)

;Q7: LIST2BTREE
;Header info: takes a non-empty list of atoms LEAVES, and returns a binary tree such that The tree leaves are the elements of LEAVES;
; For any internal (non-leaf) node in the tree, the number of leaves in its left branch minus the number of leaves in its right branch is 0 or 1.
; Solution: If the list has only one element (length == 1), return the element as atom.
; If the list has multiple elements, use function SPLIT-LIST to split the list. Use the output as the input for recursively LIST2BTREE call. (first and second element)

(defun LIST2BTREE (LEAVES)
	(cond
	  ((= (length LEAVES) 1)(car LEAVES))
	  (t (list (LIST2BTREE (car (SPLIT-LIST LEAVES))) (LIST2BTREE (cadr(SPLIT-LIST LEAVES)))))
	  )
)


;Q8: BTREE2LIST
;Header info: takes a binary tree TREE as input, and returns a list of atoms 
;Solution: first return nil if the tree is null. If the argument is a atom, transofrm to a list.
;Use append to concatenates list arguments into one list. Recursively call the function with the first element of the list and the rest as the input.
(defun BTREE2LIST (TREE)
	(cond
	  ((null TREE) nil)
	  ((atom TREE)(list TREE))
	  (t (append (BTREE2LIST (car TREE)) (BTREE2LIST (cdr TREE))))
	)
)

;Q9: IS-SAME
;Header info: takes two LISP expressions E1 and E2 whose atoms are all numbers, and checks whether the expressions are identical. 
;Solution: First, check if both arguments are null, return T. Then check if one of them is null but other is not, return nil.
;If they both are atom, use = to compare them. If they both are list, recursively call the function check if the first element of them are the same, if yes, the rest of 
; E1 and E2 would be the input to call IS-SAME so that the each element of two list would be checked. If any of the elements are not identical, return nil. 

(defun IS-SAME(E1 E2)
  	(cond 
	  ((and (null E1)(null E2)) t)
	  ((or (null E1)(null E2)) nil)
	  ((and (atom E1)(atom E2)) (= E1 E2))
	  ((or (atom E1)(atom E2)) nil)
	  ((IS-SAME (car E1)(car E2)) (IS-SAME (cdr E1)(cdr E2)))
	  (t nil)
	)
)

