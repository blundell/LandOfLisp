(defun say-hello ()
	(print "Please type your name:")
	(let ((name (read)))
		(print "Nice to meet you, ")
		(print name)))

(defun add-five ()
		(print "Please enter a number:")
		(let ((num (read)))
			(print "When I add five I get")
			(print (+ num 5))))

(defun say-hello-proper ()
		(princ "Please type your name:")
		(let ((name (read-line)))
			(princ "Nice to meet you, ")
			(princ name)))

(defun simple-repl ()
		(princ "Type a line of Lisp (don't hurt me):")
		(let ((code (read-line)))
			(princ "ship it!")
			(eval code)))