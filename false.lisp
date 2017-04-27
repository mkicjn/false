(defparameter *FALSE-stack* nil)
(defparameter *FALSE-lambda-mode* nil)
(defparameter *FALSE-current-lambda* nil)

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
    (#\> 'F>)
    (#\= 'F=)
    (#\  'F-space)
    (#\$ 'F-dup)
    (#\% 'F-drop)
    (#\\ 'F-swap)
    (#\@ 'F-rot)
    (#\O 'F-pick)
    (#\! 'F-exec)
    (#\? 'F-cond)
    ))

;;; to-do:
;;;	Stack:			'
;;;	Bitwise operators: 	_ & | ~
;;;	Lambdas:		? #
;;;	Variables:		a-z : ;
;;;	I/O:			^ , " . eszett
;;;	Comments:		{ }

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
  `(progn (F-push (argswap ,fun (F-pop) (F-pop)))))

(defun F-dup () (F-push (car *FALSE-stack*)))
(defun F-drop () (F-pop) *FALSE-stack*)
(defun F-swap () (FALSE-rearr 2 |1| |2|))
(defun F-rot () (FALSE-rearr 3 |2| |1| |3|))
(defun F-pick () (F-push (nth (F-pop) *FALSE-stack*)))
(defun F-exec () (funcall (F-pop)) *FALSE-stack*)
(defun F-cond () (F-swap) (if (not (= (F-pop) 0)) (funcall (F-pop)) (F-pop)) *FALSE-stack*)

(defun F+ () (FALSE-arithmetic #'+))
(defun F- () (FALSE-arithmetic #'-))
(defun F* () (FALSE-arithmetic #'*))
(defun F/ () (FALSE-arithmetic #'/))
(defun F> () (F-push (if (argswap #'> (F-pop) (F-pop)) -1 0)))
(defun F= () (F-push (if (= (F-pop) (F-pop)) -1 0)))

(defmacro FALSE-encapsulate (&rest args) `(progn (F-space) ,@args (F-nilcull)))

(defmacro FALSE-parse (arg-string)
  (labels ((FALSE-fun (ch) (cond
		      ((equal ch #\[) (setf *FALSE-lambda-mode* t) nil)
		      ((equal ch #\]) 
		       (setf *FALSE-lambda-mode* nil)
		       `(F-push ,(lambda ()
				   (eval `(FALSE-encapsulate
					    ,@(remove-if #'null (mapcar #'FALSE-fun *FALSE-current-lambda*)))))))
		      (*FALSE-lambda-mode* (setf *FALSE-current-lambda*
						 `(,@*FALSE-current-lambda* ,ch)) nil)
		      (t (let ((fun (assoc ch *FALSE-dictionary*)))
			   (if fun
			     `(progn (F-nilcull) (funcall ,(cadr fun)) (F-space))
			     `(funcall #'F-integer ,ch)))))))
    `(progn (F-space) ,@(remove-if #'null (mapcar #'FALSE-fun (coerce arg-string 'list)))
	    (F-nilcull) *FALSE-stack*)))
