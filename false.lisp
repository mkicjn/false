(defparameter *FALSE-stack* nil)

(defun F-reset () (setf *FALSE-stack* nil))

(defun F-pop () (pop *FALSE-stack*))

(defun F-push (a)
  (if (null (car *FALSE-stack*))
    (setf *FALSE-stack* `(,a ,@(cdr *FALSE-stack*)))
    (push a *FALSE-stack*))
  *FALSE-stack*)

(defun F-push-all (&rest args) (mapc #'F-push args))

(defparameter *FALSE-dictionary*
  '((#\+ 'F+)
    (#\- 'F-)
    (#\* 'F*)
    (#\/ 'F/)
    (#\  'F-space)
    (#\$ 'F-dup)
    (#\% 'F-drop)
    (#\\ 'F-swap)
    (#\@ 'F-rot)
    ))

#| to-do:
	Stack: slashed-o
	Bitwise operators: 	_ & | ~
	Comparison:		= >
	Lambdas:		[ ] ! ? #
	Variables:		a-z : ;
	I/O:			^ , " . eszett
	Comments:		{ }
|#

(defun char->digit (c) (- (char-int c) (char-int #\0)))

(defun argswap (fun x y) (funcall fun y x))

(defun F-integer (c)
  (F-push (+ (if (typep (car *FALSE-stack*) 'fixnum) (* 10 (F-pop)) 0) (char->digit c))))

(defun F-nilcull () (when (null (car *FALSE-stack*)) (F-pop)))

(defun F-space () (when (typep (car *FALSE-stack*) 'fixnum) (push nil *FALSE-stack*)))

(defmacro FALSE-rearr (to-pop &rest pushing)
    `(let ,(loop for i from 1 to to-pop
		 	       collect `(,(intern (write-to-string i)) (F-pop)))
            (F-push-all ,@pushing)))

(defmacro FALSE-arithmetic (fun)
  `(progn (F-nilcull) (F-push (argswap ,fun (F-pop) (F-pop)))))

(defun F-dup () (F-push (car *FALSE-stack*)))
(defun F-drop () (F-pop) *FALSE-stack*)
(defun F-swap () (FALSE-rearr 2 |1| |2|))
(defun F-rot () (FALSE-rearr 3 |2| |1| |3|))

(defun F+ () (FALSE-arithmetic #'+))
(defun F- () (FALSE-arithmetic #'-))
(defun F* () (FALSE-arithmetic #'*))
(defun F/ () (FALSE-arithmetic #'/))

(defmacro FALSE-parse (arg-string)
  (flet ((FALSE-fun (ch)
		    (let ((fun (assoc ch *FALSE-dictionary*)))
		      (if fun
			`(progn (F-nilcull) (funcall ,(cadr fun)) (F-space))
			`(funcall #'F-integer ,ch)))))
    `(progn (F-space) ,@(mapcar #'FALSE-fun (coerce arg-string 'list))
	    (F-nilcull) *FALSE-stack*)))
