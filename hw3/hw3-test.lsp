(setq s2 '((1 1 1 1 1)
	   (1 0 0 4 1)
	   (1 0 2 3 1)
	   (1 0 0 0 1)
	   (1 0 0 4 1)
	   (1 1 1 1 1)
	   ))

(setq s3 '((1 1 1 1 1)
	  (1 0 0 6 1)
	  (1 0 2 0 1)
	  (1 0 0 0 1)
	  (1 4 0 4 1)
	  (1 1 1 1 1)))

(setq s4 '((1 1 1 1 1)
	   (1 0 2 4 1)
	   (1 0 0 0 1)
	   (1 0 5 3 1)
	   (1 1 1 1 1)))
(print "s2")
(print (next-states s2))
(print "s3")
(print (next-states s3))
(print "s4")
(print (next-states s4))


(setq s1 '((1 1 1 1 1)
	   (1 4 0 0 1)
	   (1 0 2 0 1)
	   (1 0 3 0 1)
	   (1 0 0 0 1)
	   (1 1 1 1 1)
	   )) ;; in spec to test next-states

(setq testgoal '((1 1 1 1 1)
	   (1 6 0 0 1)
	   (1 0 5 0 1)
	   (1 0 5 0 1)
	   (1 0 0 0 1)
	   (1 1 1 1 1)));TEST GOAL

