;;; The following lines added by ql:add-to-init-file:
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

;;; Specify where your sigma source code lives: REPLACE THIS EXAMPLE WITH YOUR OWN DIRECTORY
(pushnew #p"/home/user/sigma-release/Sigma38/src/" asdf:*central-registry*)

; load libraries
(ql:quickload :cl-store) 
;;; LOAD SIGMA PACKAGE
(ql:quickload :sigma)

;;; SET PACKAGE TO SIGMA 
(defadvice (capi::interactive-pane-top-loop set-sigma-package :before)
  (top-level istream)
   (setf *package* (find-package "SIGMA")))

