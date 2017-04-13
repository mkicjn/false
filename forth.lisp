; Global stack definition
(defparameter *forth-stack* nil)
; Core stack manipulation functions
(defun forth-pop () (pop *forth-stack*))
(defun forth-push (n) (push n *forth-stack*))
(defun forth-push-all (&rest nums) (mapc #'forth-push nums))
; Stack manipulation macro
(defmacro forth-rearr (to-pop &rest pushing)
  `(let ,(loop for i from 1 to to-pop
	       collect `(,(intern (write-to-string i)) (forth-pop)))
     (funcall #'forth-push-all ,@pushing) "ok"))
; Manually defined functions
(defun clearstack () (setq *forth-stack* nil) "ok")
(defun cr () (terpri) "ok")
(defun dot () (princ (forth-pop)) "ok")
(defun dot-s () (princ *forth-stack*) "ok")
(defun drop () (forth-pop) "ok")
; Macro-defined functions
(defun swap () (forth-rearr 2 |1| |2|))
(defun dup () (forth-rearr 1 |1| |1|))
(defun over () (forth-rearr 2 |2| |1| |2|))
(defun rot () (forth-rearr 3 |2| |1| |3|))
; Test data
(loop for i from 1 to 4 do (forth-push i))
