;;;;;;;;;;;;;;
; Homework 4 ;
;;;;;;;;;;;;;;


; EXERCISE: Modify this function to decide satisifiability of delta.
; If delta is satisfiable, sat? returns a list of n integers that represents a model of delta,  
; otherwise it returns NIL. (See spec for details.)
; param n: number of variables in delta
; param delta: a CNF represented as a list of lists


(defun sat? (n delta)
  	(let* (solution '())
	   (backTrack-Search n delta '())
	 ); end let
); end defun

;;;;;

; backtrack search DFS, it returns solutions list if it find. 
; If any variable cannot be sign, it means the search could never visit the leaves node, NO SOLUTION. Return nil. 
(defun backTrack-Search (n delta solution)
  (if (= (length solution) n) solution    
      (let* ((var (+ (length solution) 1))
             (var-assign-T (append solution (list (abs var)))) ; append the new assignment to the solution list
             (var-assign-F (append solution (list (- var)))))
       (cond 
	 ; Left child node.
	 ; Assign True value to the variablw
          ((check-CNF n delta var-assign-T) ;check if the assignment valid.
            	(let* ((pruned-delta (prun-clause delta var-assign-T))
		       ; if not succeed, here returns nil. Then we visit the right child node.
		       (T-flag (backTrack-Search n pruned-delta var-assign-T)) )
              (if  T-flag T-flag
		; if the result is null, we tried to assign the value as false. 
			 (if  (check-CNF n delta var-assign-F)
                      			(let* ((pruned-delta (prun-clause delta var-assign-F)) 
			     			(F-flag (backTrack-Search n pruned-delta var-assign-F )))
					  	; If not succeed, here returns nil. 
						; If the negative assignment does not work as well, then it has no solution for the CNF.
						(if F-flag F-flag nil)); end let
					 ; If the negative value can valid the delta, it means we have no way to assign this variable value to make delta valid. So no solution.
                                       	nil ); end if
		); end if
	      ); end let
	      )
	   ; Right child node.
 	   ; Assign False value to the variable
          ((check-CNF n delta var-assign-F) ;check if the assignment valid
            	(let* ((pruned-delta (prun-clause delta var-assign-F))
		  	(F-flag (backTrack-Search n pruned-delta var-assign-F )))
              (if F-flag F-flag nil); end if
	     )
	   )
          ; if both values assignment does not work for the validity of the delta, it means no solution. We have to assign value for each variables. 
	  (t nil)
	  ) ; end cond
	); end let  
    ); end if
  ); end defun


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Evaluation Part during assignment ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Evaluation Procedure:
; 1. check clause (it has to be all clauses to be true) vaildity with current variable assignment. 
; 2. check literal in each single clause (it just need one literal to be true, then the whole clause will be also true )
; 3. Once found any literal existing in solution list, Returns true.
; We cannot know the clause is false or not until all variable inside has been assigned. 
; 

; helper function to check the CNF with the current solution list
(defun check-CNF (n CNF solution)
  	(if (or (> (length solution) n) (= (length solution) 0) ( > (abs (car solution)) n ) (= (car solution) 0)) nil
	(cond 
		((null CNF) t)
		((not (check-clause (car CNF) solution)) nil) ;if any clause is false, then the whole delta is not ture. 
		; if it cannot 
		(t (check-CNF n (cdr CNF) solution))
	  )
	); end if
)

; (check-CNF '((-1 -2 3) (-1) (-2 -3)) '(-1 -2 3) )
; helper function to check each clause. 
; If any of the literals inside the clause are ture, then that claus

; Three case during evaluation: if the clause is false, check next one; If the clause already has a satisfible assignment in solution list, it is T, we prun it;
; if we still don't have enough information to deduce the current clause is t or f, set it to t and check the next clause.

(defun check-clause (clause solution)
	(cond
	  ((null clause) nil)
	  ; Case1: satisfied
	  ((isMember (car clause) solution) t) ;satisfied clause
	  ;if there is no assignment for the variables in the clause yet, we don't have enough information, return true.
	  ; Case2: The clause is not satisfied yet, not enough information so far
	  ((check-not-assigned (car clause) solution) t)
	 (t (check-clause (cdr clause) solution)) ;Recursion
	  ); end cond
); end defun

; helper function to detece clauses that are not satisfied yet. 
(defun check-not-assigned (literal solution)
	(cond
	  	((null literal) nil)
	  	((and (not (isMember literal solution)) (not (isMember (- literal) solution) ) ) t)
	  )
 )
;;;;;;;;;;;;;;;;;;;;;;;
; Improve Performance ;
;;;;;;;;;;;;;;;;;;;;;;;
; once we proud the clauses has been satisfied, we don't need to check them again, since we know they are always true with the current solution.
; So no matter what values we assign for the rest variables, they are still true. So pruning them can prompt performance. 
; Return the new delta list
(defun prun-clause (delta solution)
  	(cond
	  ((null delta) delta)
	  (t (if (prun-helper (car delta) solution) 
		; just found that I don't have to use remove function to prun.........
		; The below will also work since we just remove the first element here.
		; (prun-clause (my-remove (car delta) delta n))
		(prun-clause (cdr delta) solution) ;if the clause has been satisfied, prun from the delta. i.e, we don't need the first element here.
		; otherwise:
		  (cons
		      (car delta) (prun-clause (cdr delta) solution)); stay the checking clause, if it has not been satisfied. Recursion to prun the satisfied clause. 
		); end if	     			
	  ); end t
	); end cond
 ); end defun


; helper function to check true clause and prun. It happens after variable assign. 
(defun prun-helper (clause solution)
  	(cond
	 	 ((null clause) nil) ;if clause is null, nil. 
		 ((isMember (car clause) solution) t)
		 (t (prun-helper (cdr clause) solution))
	)
)
;;The performance could be further improved by prunning the literals in clauses that which has been evaluated. 
;; I stopped here since the requirement of the spec has been satisifed and I need time to review for midterms. :)
(defun remove-literal (literal clause)
	(cond
	  ((null delta) nil)
	  ((= (car delta) literal) (remove-literal literal (cdr clause)))
	  (t (cons (car clause) (remove-literal literal (cdr clause))))
	 )

 )


;;;;;;;;;;;;;;;;;;;;;;;
; Helper Function     ;
;;;;;;;;;;;;;;;;;;;;;;;
; From hw3
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

; Helper function to check if an literal is 
; it will return nil if the item is not in the list.
; At the beginning I use equal which made the performance bad. (8 mins- 12 mins).
; Note that if use equal here, the performance will be much lower.
; The reason is that = compare the numerical values, But Using eql or equal, you test whether their values are indistinguishable.
; While testing numbers for numerical equailty, we should use = instead of equal.
; Above Reference: https://www.gnu.org/software/emacs/manual/html_node/elisp/Comparison-of-Numbers.html
(defun isMember (item L)
  	(if L
      		(if (= item (car L)) t  ;if the item is member of the list, return t
          	(isMember item (cdr L)) ;otherwise, keep check next element. 
		)
      	)
)


;(defun my-remove (item L n)
;	(cond
;	  ((null L) '())
;	  ((not (isMember item L)) L);if item not in the list, directly return the list, no need to remove
	  ;((equal item (car L)) (cdr L)) ;base case. if the first element is the item
;	  (t (let (size (length L))
;	       (if (not (equal item (car L)))
;		 (my-remove item (cdr L) (+ n 1)) ; if not equal, move the pointer
;		 (append (butlast L (+ (- size n) 1)) (cdr L)) ;if equal, remove the item at n index. 
;		 ) ;end if 
;	     ); end let
;	  ); end t
;	); end cond
; ); end defun


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Functions that help you parse CNF from files in folder cnfs/
; You need not modify any functions in this section
; Usage (solve-cnf <path-to-file>)
; e.g., (time (solve-cnf "./cnfs/f1/sat_f1.cnf"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun split-line (line)
  (if (equal line :eof)
      :eof
      (with-input-from-string (s line) (loop for x = (read s nil) while x collect x))))

(defun read-cnf (filename)
  (with-open-file (in filename)
    (loop for line = (split-line (read-line in nil :eof)) until (equal line :eof)
      if (equal 'p (first line)) collect (third line)      ; var count
      if (integerp (first line)) collect (butlast line)))) ; clause

(defun parse-cnf (filename)
  (let ((cnf (read-cnf filename))) (list (car cnf) (cdr cnf))))

; Following is a helper function that combines parse-cnf and sat?
(defun solve-cnf (filename)
  (let ((cnf (parse-cnf filename))) (sat? (first cnf) (second cnf))))
