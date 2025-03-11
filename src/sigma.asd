(in-package :cl-user)

(defpackage :sigma-asd
  (:use :cl :asdf))

(in-package :sigma-asd)
; check if should compile graphical features use +LISPWORKS instead?
;(if (member :capi *features*)
#+LISPWORKS (defsystem :sigma
    :serial t
    :components ((:file "packages")
		 (:file "sigma")
		 (:file "templates")
		 (:file "sigmaUI")
		 (:file "graph-compilation")
		 (:file "graph-runtime")
		 (:file "deciding")
		 (:file "learning")
		 (:file "appraisal")
		 (:file "printCLI")
		 (:file "regressionCLI")
		 (:file "debugging-tools")
		 (:file "multicore")
		 (:file "multicoreCLI")
		 (:file "ultimatum-softv4")
		 (:file "ultimatum-forwardv16")
		 (:file "shift-reduce-dependency")
		 (:file "tests")
		 (:file "regression")
		 (:file "io")		 
))

#+CLISP (defsystem :sigma
    :serial t
    :components ((:file "packages")
		 (:file "sigma")
		 (:file "templates")
		 (:file "graph-compilation")
		 (:file "graph-runtime")
		 (:file "deciding")
		 (:file "learning")
		 (:file "appraisal")
		 (:file "printCLI")
		 (:file "regressionCLI")
		 (:file "debugging-tools")
		 (:file "ultimatum-softv4")
		 (:file "ultimatum-forwardv16")
		 (:file "shift-reduce-dependency")
		 (:file "tests")
))

