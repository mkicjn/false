(defparameter *FALSE-stack* nil)
(defparameter *FALSE-lambda-mode* nil)
(defparameter *FALSE-current-lambda* nil)
(defparameter *FALSE-char-mode* nil)
; To-do: Make all values local to FALSE-parse

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
    (#\~ 'F~)
    (#\_ 'F_)
    (#\& 'F-and)
    (#\| 'F-or)
    (#\  'F-space)
    (#\$ 'F-dup)
    (#\% 'F-drop)
    (#\\ 'F-swap)
    (#\@ 'F-rot)
    (#\O 'F-pick)
    (#\! 'F-exec)
    (#\? 'F-cond)
    (#\# 'F-while)
    (#\: 'F-set)
    (#\; 'F-acc)
    ))

;;; to-do:
;;;	I/O:			^ , " . eszett
;;;	Comments:		{ }

(defun char->digit (c) (- (char-int c) (char-int #\0)))

(defun argswap (fun x y) (funcall fun y x))

(defun F-integer (c)
  (F-push (+ (if (typep (car *FALSE-stack*) 'fixnum) (* 10 (F-pop)) 0) (char->digit c))))

(defun F-nilcull () (when (null (car *FALSE-stack*)) (F-pop)))

(defun F-space () (when (typep (car *FALSE-stack*) 'fixnum) (push nil *FALSE-stack*)))

(defun lowercase-p (ch) (and (>= (char-int ch) (char-int #\a))
			     (<= (char-int ch) (char-int #\z))))

(defmacro FALSE-rearr (to-pop &rest pushing)
    `(let ,(loop for i from 1 to to-pop
		 	       collect `(,(intern (write-to-string i)) (F-pop)))
            (F-push-all ,@pushing)))

(defmacro FALSE-arithmetic (fun)
  `(progn (F-push (argswap ,fun (F-pop) (F-pop)))))

(defun integer->bitarray (arg)
  (if (>= arg 0)
  (coerce (cdr (butlast (loop for i = (expt 2 16) then (/ i 2)
				      for n = arg then (if (>= n (* i 2)) (- n (* i 2)) n)
				      for r = (floor (/ n i))
				      collect r into a
				      when (< i 1) return a)))
  'bit-vector)
  (integer->bitarray (+ (expt 2 16) arg))))

(defun bitarray->integer (b)
  (let ((unsigned (reduce (lambda (x y) (+ (ash x 1) y)) b)))
    (if (< unsigned (expt 2 15)) unsigned (- (- (expt 2 15) (mod unsigned (expt 2 15)))))))

(defmacro FALSE-bitwise (fun)
  `(F-push (bitarray->integer
	     (funcall ,fun
		      (integer->bitarray (F-pop))
		      (integer->bitarray (F-pop))))))

(defun F-dup () (F-push (car *FALSE-stack*)))
(defun F-drop () (F-pop) *FALSE-stack*)
(defun F-swap () (FALSE-rearr 2 |1| |2|))
(defun F-rot () (FALSE-rearr 3 |2| |1| |3|))
(defun F-pick () (F-push (nth (F-pop) *FALSE-stack*)))
(defun F-exec () (funcall (F-pop)) *FALSE-stack*)
(defun F-cond () (F-swap) (if (not (= (F-pop) 0)) (funcall (F-pop)) (F-pop)) *FALSE-stack*)

(defun F-while () (let ((fun (F-pop)) (con (F-pop)))
  (loop with c = -1
	until (= c 0)
	do (funcall con)
	do (setf c (F-pop))
	when (= c -1) do (funcall fun)
	)))

(defun F+ () (FALSE-arithmetic #'+))
(defun F- () (FALSE-arithmetic #'-))
(defun F* () (FALSE-arithmetic #'*))
(defun F/ () (FALSE-arithmetic #'/))
(defun F> () (F-push (if (argswap #'> (F-pop) (F-pop)) -1 0)))
(defun F= () (F-push (if (= (F-pop) (F-pop)) -1 0)))
(defun F_ () (F-push -1) (F*))
(defun F~ () (F-push (bitarray->integer
			(bit-not (integer->bitarray (F-pop))))))
(defun F-and () (FALSE-bitwise #'bit-and))
(defun F-or () (FALSE-bitwise #'bit-ior))
(defun F-set () (set (intern (coerce `(,(F-pop)) 'string)) (F-pop)))
(defun F-acc () (F-push (eval (intern (coerce `(,(F-pop)) 'string)))))

(defmacro FALSE-encapsulate (&rest args) `(progn (F-space) ,@args (F-nilcull)))

(defun FALSE-fun (ch) (cond
			((equal ch #\') (setf *FALSE-char-mode* t) nil)
			(*FALSE-char-mode* (setf *FALSE-char-mode* nil)
					   `(F-push (char-int ,ch)))
			((equal ch #\[) (setf *FALSE-current-lambda* nil)
					(setf *FALSE-lambda-mode* t) nil)
			((equal ch #\]) 
			 (setf *FALSE-lambda-mode* nil)
			 `(F-push (lambda ()
				    (eval (FALSE-encapsulate
					     ,@(remove-if #'null (mapcar #'FALSE-fun *FALSE-current-lambda*)))))))
			(*FALSE-lambda-mode* (setf *FALSE-current-lambda*
						   `(,@*FALSE-current-lambda* ,ch)) nil)
			(t (let ((fun (assoc ch *FALSE-dictionary*)))
			     (if fun
			       `(progn (F-nilcull) (funcall ,(cadr fun)) (F-space))
			       (if (lowercase-p ch) `(F-push ,ch)
			       `(funcall #'F-integer ,ch)))))))

(defmacro FALSE-parse (arg-string)
  `(FALSE-encapsulate ,@(remove-if #'null (mapcar #'FALSE-fun (coerce arg-string 'list)))))

(defun FALSE-REPL ()
  (F-reset)
  (format t "Enter an empty line to quit.~%CL-FALSE> ")
  (finish-output)
  (loop for input = (read-line)
	until (equalp input "")
	do (eval `(FALSE-parse ,input))
	do (format t "~{~A ~}~%~%CL-FALSE> " (reverse *FALSE-stack*))
	do (finish-output)))

(FALSE-REPL)
