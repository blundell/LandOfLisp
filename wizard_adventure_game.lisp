;; chapter 5

(defparameter *nodes* '((living-room (you are in the living-room. 
	                            a wizard is snoring loud on the couch.))
                        (garden (you are in a beautiful garden.
                        	    there is a well in front of you.))
                        (attic (you are in the attic.
                        	    there is a giant welding torch in the corner.))))

(defparameter *edges* '((living-room 
							(garden west door) 
							(attic upstairs ladder))
					   (garden 
					   		(living-room east door))
					   (attic
					   		(living-room downstairs ladder))))

(defparameter *objects* '(whiskey bucket frog chain))

(defparameter *object-locations* '(
					(whiskey living-room)
					(bucket living-room)
					(chain garden)
					(frog garden)))

(defparameter *location* 'living-room)

(defun describe-location (location nodes)
	(cadr (assoc location nodes)))

(defun describe-path (edge)
	`(there is a ,(caddr edge) going ,(cadr edge) from here.))

(defun describe-paths (location edges)
	(apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

(defun objects-at (location objects object-locations)
	(labels ((at-location-p (object)
				(eq (cadr (assoc object object-locations)) location)))
	(remove-if-not #'at-location-p objects)))


(defun describe-objects (location objects object-location)
	(labels ((describe-object (object)
				`(you see a ,object on the floor.)))
	 (apply #'append (mapcar #'describe-object (objects-at location objects object-location)))))

(defun look ()
	(append 
		(describe-location *location* *nodes*)
		(describe-paths *location* *edges*)
		(describe-objects *location* *objects* *object-locations*)))

(defun walk (direction)
	(let ((next (find direction
					(cdr (assoc *location* *edges*))
					:key #'cadr)))
	(if next
		(progn (setf *location* (car next))
				(look))
		'(you cannot go that way))))


(defun pickup (object)
	(cond ((member object (objects-at *location* *objects* *object-locations*))
			(push (list object 'body) *object-locations*)
				`(you are now carrying the ,object))
			(t '(you cannot get that.))))

(defun inventory ()
	(cons 'items- (objects-at 'body *objects* *object-locations*)))

;; chapter 6
(defparameter *allowed-commands* '(look walk pickup inventory))

(defun game-repl ()
	(print "You can 'look' 'walk' 'pickup' and see 'inventory' ( or 'quit' phht.)")
	(print "What now?")
	(let ((cmd (game-read)))
		(unless (eq (car cmd) 'quit)
			(game-print (game-eval cmd))
			(game-repl)))
	(print "LOL WIMP")
	)
	

(defun game-read ()
	(let ((cmd (read-from-string
					(concatenate 'string "(" (read-line) ")"))))
	(flet ((quote-it (x)
				(list 'quote x)))
		(cons (car cmd) (mapcar #'quote-it (cdr cmd))))))

(defun game-eval (sexp)
	(if (member (car sexp) *allowed-commands*)
		(eval sexp)
		'(i do not know that command.)))

(defun tweak-text (list caps literature)
	(when list
	(let ((item (car list))
		  (rest (cdr list)))
	(cond 
		((eql item #\space) (cons item (tweak-text rest caps literature)))
		((member item '(#\! #\? #\.)) (cons item (tweak-text rest t literature)))
		((eql item #\") (tweak-text rest caps (not literature)))
		(literature (cons item (tweak-text rest nil literature)))
		(caps (cons (char-upcase item) (tweak-text rest nil literature)))
		(t (cons (char-downcase item) (tweak-text rest nil nil)))))))

(defun game-print (list)
	(princ (coerce (tweak-text (coerce (string-trim "()"
													(prin1-to-string list))
										'list)
								t
								nil)
							'string))
	(fresh-line))










