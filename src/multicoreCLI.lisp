;; run an instance of the function, each thread will die when its call to this
;; function returns.
(defun process-data (funcname param count mailbox &optional funcfile)
  (sleep (random 1.0))
  (setq cstr (concatenate 'string (write-to-string count) "_" (write-to-string param)))
  (mp:mailbox-send mailbox (wrapper cstr funcname param funcfile)))

;; Pass mailbox from "processor" to vraious "generators".
(defun run-parallel (mfunc params &optional funcfile)
"run jobs in parallel, argument 1 is the function to run, argument 2 is the list of parameters to run the function on and the 3rd optional argument is a file to be compiled if the function to be run is not in sigma.lisp"
  (defvar mbox (mp:make-mailbox))
  (defvar idx 0)
  (dolist (param params)
    (mp:process-run-function
     (format nil "Generator ~d." idx) ()
     'process-data
     mfunc
     param
     idx
     mbox
     funcfile)
    (incf idx)))

(defun wrapper (packagename funcname funcarg &optional funcfile)
  (setf *pkg* (make-package packagename))
  (use-package "CL-USER" *pkg*)
  (setq *package* *pkg*)
  (compile-file "./sigma.lisp" :load t :in-memory t)
  (if funcfile
      (compile-file funcfile :load t :in-memory t))
  (intern funcname *package*)
  (funcall (intern funcname) funcarg))



