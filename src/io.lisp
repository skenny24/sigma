(in-package :sigma)
; saving and restoring graphs
#+LISPWORKS (ql:quickload "cl-store")
; save graph for later session 
; add all existing vars to global vars here
#+LISPWORKS (defun save-graph (filespec)
	      (cl-store:store cg filespec)
	      t)

; reload saved graph
#+LISPWORKS (defun load-graph (filespec)
	      (setf cg (cl-store:restore filespec))
	      t)

#+LISPWORKS (defun save-sigma-session (filespec)
	      (in-package :cl-user)
	      (hcl:save-current-session filespec)
	      t)

#+LISPWORKS (defun load-sigma-session (filespec)
;; need to find way to load binary from within session
	      t)

;defaults to loading session into file "lispinit.mem"
#+CLISP (defun save-sigma-session ()
	  (EXT:SAVEINITMEM))
