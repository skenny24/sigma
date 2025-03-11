;; Copyright (c) 1987--2015 LispWorks Ltd. All rights reserved.

(in-package "CL-USER")

(load-all-patches)

;;; Where we are going to save the application (except on Cocoa)

(defvar *delivered-image-name* "~/hello")

;;; Load the application code.
;;; We compile the file to the temporary directory
;;;  because the current directory is not necessarily writable. 

(compile-file (current-pathname "parallelsig") 
              :output-file :temp
              :load t)

;;; On Cocoa it is a little more complicated, because we need to
;;; create an application bundle. We set *DELIVERED-IMAGE-NAME* to the
;;; path that CREATE-MACOS-APPLICATION-BUNDLE returns. We avoid
;;; copying the LispWorks file type associations by passing
;;; :DOCUMENT-TYPES NIL.

;;; Note: As an alternative to CREATE-MACOS-APPLICATION-BUNDLE you
;;; could use the bundle creation code that is supplied with LispWorks
;;; in (example-file "configuration/macos-application-bundle.lisp")
;;; However, that should only be necessary if you want to modify that
;;; bundle creation code.

#+cocoa
(setq *delivered-image-name*
        (create-macos-application-bundle "~/Hello.app"   
                                         :document-types nil
                                         :identifier "com.example.Hello"
                                         ))

;;; Deliver the application

(deliver 'hello-world *delivered-image-name* 0 :multiprocessing t)
