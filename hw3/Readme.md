
- Play a variant of game called Sokoban
- defining search space for practical problem
- good heuristic to make program efficient

Helper Function

- `getKeeperPosition` -> Return (column row) 
- `cleanUpList` -> Remove nil elements of a list

---

- `goal-test`: check a state if it is a goal state. 

  -  (neither any boxes nor the keeper is on a non-goal square)

  -  the keeper cannot walk into a wall or into a box

## New lisp function

- `abs`: number -> absolute value

```lisp
 (abs 0) =>  0
 (abs 12/13) =>  12/13
 (abs -1.09) =>  1.09
 (abs #c(5.0 -5.0)) =>  7.071068
 (abs #c(5 5)) =>  7.071068
 (abs #c(3/5 4/5)) =>  1 or approximately 1.0
 (eql (abs -0.0) -0.0) =>  true
```

- `min` & `max`: 返回 list里的最值

```lisp
 (max 3) =>  3 
 (min 3) =>  3
 (max 6 12) =>  12 
 (min 6 12) =>  6
 (max -6 -12) =>  -6 
 (min -6 -12) =>  -12
 (max 1 3 2 -7) =>  3 
 (min 1 3 2 -7) =>  -7
 (max -2 3 0 7) =>  7 
 (min -2 3 0 7) =>  -2
 (max 5.0 2) =>  5.0 
 (min 5.0 2) => 2
```

- `floor` : number &optional division => quotient, remainder       
  - division 默认1
  - 单个argument (整数， 小数)

```lisp
(floor 3/2) =>  1, 1/2
(floor 10) => 10, 0
(floor 10.3 2) => 5, 0.3000002
(floor 3/4) => 0, 3/4
(floor 2) => 2, 0
```

- `ceiling`: *number**divisor* => quotient (numeric value), remainder (numeric value)

  function returns two values, the first is result of dividing number by divisor and truncating toward positive infinity. Second result remainder that satisfies equation: quotient * divisor + remainder = number.

  - | number  | number                           |
    | ------- | -------------------------------- |
    | divisor | non-zero real number, default is |

```lisp
(ceiling 10) => 10, 0
(ceiling 10.3 2) => 6, -1.6999998
(ceiling 3/4) => 1, -1/4
```

- `butlast`: function returns the argument list copy without N last elements. See LAST.
  - syntax: *list n*(optional) => list



```lisp
(butlast '(1 2 3)) => (1 2)
(butlast '(1 2 3) 0) => (1 2 3)
(butlast '(1 2 3) 1) => (1 2)
(butlast '(1 2 3) 2) => (1)
(butlast '(1 2 3) 3) => NIL
(butlast '(1 2 3) 4) => NIL
```

- `nthcdr`： n list => tail
  - **Description:**Returns the [*tail*](http://clhs.lisp.se/Body/26_glo_t.htm#tail) of *list* that would be obtained by calling [**cdr**](http://clhs.lisp.se/Body/f_car_c.htm#cdr) *n* times in succession. 去除前面n个元素返回
  - **Examples:**

```lisp
 (nthcdr 0 '()) =>  NIL
 (nthcdr 3 '()) =>  NIL
 (nthcdr 0 '(a b c)) =>  (A B C)
 (nthcdr 2 '(a b c)) =>  (C)
 (nthcdr 4 '(a b c)) =>  ()
 (nthcdr 1 '(0 . 1)) =>  1
```

- `count`function counts specified elements in sequence. Return value is number of occurancesf or NIL if no occurance is not found. See also COUNT-IF, POSITION, POSITION-IF, FIND, FIND-IF and MEMBER.

> Symbol type: function
>
> **count***item**sequence**test*(keyword)*from-end*(keyword)*start*(keyword)*end*(keyword)*key*(keyword)*test*(keyword)*test-not*(keyword) => integer
>
> Argument description:
>
> | item     | an item to be found                                         |
> | -------- | ----------------------------------------------------------- |
> | sequence | a sequence to be searched                                   |
> | test     | function key and item comparison                            |
> | from-end | direction of search, default is NIL - forward               |
> | start    | starting position for search, default is 0                  |
> | end      | final position for search, default is NIL - end of sequence |
> | key      | function for extracting value before test                   |
> | test     | function for comparison of two values                       |
> | test-not | function for comparison of two values                       |

```lisp
(count #\s "Some sequence") => 1
(count #\s "Some sequence" :key #'char-downcase) => 2
(count #\s "Some sequence" :key #'char-downcase :start 1) => 1
(count #\x "Some sequence") => 0
(count '(1 2) #(9 3 (1 2) 6 7 8)) => 0
(count '(1 2) #(9 3 (1 2) 6 7 8) :test #'equal) => 1
(count 1 #(0 1 0 0 0 1 0) :from-end t) => 2
```

- `nth`: nth n list 返回列表第 n-1 个元素

## Quesiton

Use `count` in h1, count the atom 2 in the state. 

```lisp
;check number of box in a list
(defun count_box_row (l)
      (cond 
      ((null l) 0)
      (t (count 2 l))
      ); end cond
); end defun

(defun count_box (s)
     (cond 
    ((null s) 0)
    (t (+ (count_box_row (car s)) (count_box (cdr s))))
    )
 )

```



```lisp
(printstates (a* p1))
(sokoban p1 #'h1) 
; time function can use to get execution time
(time (sokoban p1 #'h1))
```

