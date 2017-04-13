;;; Global stack definition
(defparameter *forth-stack* nil)
;;; Miscellaneous functions
(defun _/ (x y) (floor (/ x y)))
;;; Core stack manipulation functions
(defun forth-pop () (pop *forth-stack*))
(defun forth-push (n) (push n *forth-stack*))
(defun forth-push-all (&rest nums) (mapc #'forth-push nums))
;;; Stack manipulation macro
(defmacro forth-rearr (to-pop &rest pushing)
  `(let ,(loop for i from 1 to to-pop
	       collect `(,(intern (write-to-string i)) (forth-pop)))
     (funcall #'forth-push-all ,@pushing) "ok"))
;;; Instruction evaluation macro
(defmacro as-forth (&rest args)
  `(progn ,@(loop for thing in args
		  for num = (typep thing 'fixnum)
		  for arith = (member thing '(+ - * / mod))
		  when num collect `(forth-push ,thing)
		  when arith collect `(forth-arith #',thing)
		  unless (or num arith) collect `(,thing))))
;;; Manually defined Forth functions
(defun clearstack () (setq *forth-stack* nil) "ok")
(defun cr () (terpri) "ok")
(defun dot () (princ (forth-pop)) "ok")
(defun dot-s () (princ *forth-stack*) "ok")
(defun drop () (forth-pop) "ok")
;;; Macro-assisted Forth function definitions
(defun swap () (forth-rearr 2 |1| |2|))
(defun dup () (forth-rearr 1 |1| |1|))
(defun over () (forth-rearr 2 |2| |1| |2|))
(defun rot () (forth-rearr 3 |2| |1| |3|))
;;; Basic Forth arithmetic handling functions
(defun forth-arith (op) (swap)
  (forth-push (funcall op (forth-pop) (forth-pop))) "ok")
(defun /mod ()
  (let ((n1 (forth-pop)) (n2 (forth-pop)))
    (forth-push (mod n2 n1)) (forth-push (_/ n2 n1))) "ok")
;;; Test data
(loop for i from 1 to 4 do (forth-push i))
