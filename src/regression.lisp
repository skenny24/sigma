(in-package :sigma)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Regression testing for Sigma Architecture
;;; This is a basic testing framework which only test the execution of the functions and see
;;; the functions throw any kind of exceptions.
;;; I believe in the future it will be more complex and will be able to determine
;;; the correct out put it should get when we run an example.
;;;
;;; In order to run this interface - 
;;;
;;; CL-USER> (regression-testing)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Parameter which stores the example list
;(defparameter *example-list* '(TEST-RULE-ONE TEST-EPISODIC)); test-color-silver))
(defparameter *example-list* '(test-rule-one test-color-silver test-print-eight-puzzle test-ultimatum-soft test-ultimatum-forward test-rw-al test-rl-rl test-slam test-shift-reduce test-episodic)) 
; list of files in sigma package that should be compiled for separate listener instances
(defparameter *sigfiles* '("sigma.lisp" "sigmaUI.lisp" "graph-compilation.lisp" "graph-runtime.lisp" "deciding.lisp" "learning.lisp" "appraisal.lisp" "printCLI.lisp" "ultimatum-softv4.lisp" "ultimatum-forwardv16.lisp" "shift-reduce-dependency.lisp" "tests.lisp" "regression.lisp" "debugging-tools.lisp" "templates.lisp"))
; sigma home used for compilation of temp regression-testing file
(defvar *regoutfile* (merge-pathnames *sigmahome* "regression-testing.lisp"))
;parameter with predefined colors as the number of examples grow the color list should also grow
(defparameter *font-colors* '(:black :red :green :blue :lemonchiffon4 :magenta4 :burlywood4 :red3 :green3 :blue3 :red4 :green4 :blue4))
(defvar color-examples '())
;we want the default color to be black
(defvar current-color :black)
;exception variable will store the value of any exception which was caught and will  be used to display it to the user
(defparameter *exception* nil)
(defvar exception)
;all the listeners for the examples
(defvar list-ex '())

;initializing the variables
(setq exception *exception*)

;list of directories we have to search for the files
;(defparameter example-directory (list #P"Dropbox/GVHA/" #P"GVHA/" #P"GVHA/dinesh/"))
(defparameter example-directory '())

;if you want to add more paths add to this variable
;Please put #P before your path name and / after pathname
(defparameter example (list :test-ultimatum-forward #P"Theory of Mind/ultimatum forward v16.lisp" :test-ultimatum-soft #P"Theory of Mind/ultimatum-soft v4.lisp"
                            :test-shift-reduce #P"NLP/shift-reduce-dependency.lisp"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Main interface macro which will display the Testing screen
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(capi:define-interface regression ()
  ()
  (:panes 
   (header capi:title-pane
           :text "Regression Testing"
           :font (graphics-ports:make-font-description
                  :family "times"
                  :size 18
                  :weight :medium
                  :slant :roman))
   
   (test-all-examples capi:push-button
                      :text "Test all examples"
                      :selection-callback 'test-all-examples)

   (tae-info capi:title-pane
             :text "This will test all the examples of Sigma Architecture")
   
; manual testing of the examples
   (manual-title capi:display-pane
                 :text "Manual Testing"
                 :font (gp:make-font-description
                        :family "times"
                        :size 18
                        :weight :medium
                        :slant :roman))
   ;example
   (examples capi:push-button-panel
             :items *example-list* 
             :layout-class 'capi:grid-layout
             :layout-args '(:columns 2)
             :selection-callback 'test-one-example
             :print-function 
             #'(lambda (x)
                 (format nil "~S" x))  
             :title "Examples"
             :title-position :frame    
             )
        
   (listener-tab capi:listener-pane
                 :title "Listener"
                 )

   (info-tab capi:collector-pane
             :enabled :read-only
             :title "Update"
             :title-position :frame
             :change-callback 'change-color
             :background :grey95
             :horizontal-scroll t
             :visible-max-width 800
             )  
   )  
; layout of the interface as seen by the user
  (:layouts  
   
   (column-left capi:column-layout
;                '(header tae-info test-all-examples manual-title examples info-tab)
                '(header tae-info test-all-examples manual-title examples)
                :gap 25
                :visible-min-width (* (capi:screen-width (capi:convert-to-screen)) 0.40)  
                :visible-max-width (* (capi:screen-width (capi:convert-to-screen)) 0.40)
                )
   
   (listener-layout capi:grid-layout
                    '(listener-tab)
                    :x-ratios '(90 0) 
                    :columns 3
                    :orientation :row
                    )
   
   (row-main capi:row-layout
             '(column-left listener-layout)
             :reader row-main)   
   
   (test-all-listeners capi:grid-layout
                       '()
                       :columns 2
                       :orientation :row
                       :y-uniform-size-p t
                       :reader test-all-listeners
                       )
   
   (main-layout  capi:switchable-layout
                 '(row-main test-all-listeners)
                 :reader main-layout
                 :visible-child 'row-main
                 )        
   
   (tabs capi:tab-layout
         '(main-layout)
         :items '(row-main test-all-listeners)
         :print-function #'(lambda (item)
                             (ecase item
                               (row-main "Main Layout")
                               (test-all-listeners "Test all")))
         :selection-callback #'(lambda (item interface)
                                 (setf (current-view interface) item))
         :reader tabs
         )
   )
  
  (:default-initargs
   :layout 'tabs
   :title "Regression Testing"
   :width (* (capi:screen-width (capi:convert-to-screen)) 0.9)
   :height (* (capi:screen-height (capi:convert-to-screen)) 0.9)
   )
  )

(defmethod current-view ((interface regression))
  (let ((child (capi:switchable-layout-visible-child (main-layout interface))))
    (unless (symbolp child)
      (setf child (capi:capi-object-name child)))
    child)
  )

;;; Change the view.
(defmethod (setf current-view) (new-view (interface regression))
  (setf (capi:switchable-layout-visible-child (main-layout interface))
        (ecase new-view
          (row-main (row-main interface))
          (test-all-listeners (test-all-listeners interface))))
    )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This section handles testing of all the examples,decisions
;;; and prints working memory.
;;; When the user clicks on the test-all-example button a thread
;;; is created which runs in the background performing test on
;;; examples to see if there is any bug which throws an exception
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun test-all-examples (data interface)
  (declare (ignore data))
  (setq from-regression t)
  (setq current-color :black)
  ;change the view 
  (setf (current-view interface) 'test-all-listeners)
  (setf (capi:choice-selected-item (tabs interface)) 'test-all-listeners)
  (format info-stream "~%~%Testing all Examples~%")
  (do-read-write) ; prep regression file for compiling
  (dolist (ex *example-list*)
    (capi:interactive-pane-execute-command (cdr (assoc ex list-ex))
                                           (concatenate 'string "(format t \"~%Running Example " (symbol-name ex) " \") 
                                                         (in-package " (symbol-name ex) ")(progn
                                        (compile-file-if-needed (merge-pathnames \"./regression-testing.lisp\" 
                                                                                                  \"" (namestring *sigmahome*) "\")
                                                                                 :load t
                                                                                 :in-memory t
                                                                                 )
                                                                           
                                                           (setq info-stream (sigma:get-info-stream))
                                                          (if (not (fboundp '" (symbol-name ex)  "))
                                                               (compile-example-file '" (symbol-name ex) "))
                                                             (progn    
                                                               (" (symbol-name ex) ")
                                                               (format t \" Running example \")
                                                               (info-stream-func '" (symbol-name ex) ")
                                                               (sleep 10)
                                                               (reset-from-regression)
                                                               ))"
                                                        )
                                           )
    )
  )

(defun test-one-example (data interface)
  (declare (ignore data))
  (setq from-regression t)
  (setq current-color :black)
  (format info-stream "~%~%Testing all Examples~%")
  (setf ex data)
  (setq trace-stream (capi:interactive-pane-stream (slot-value interface 'listener-tab)))
    (capi:interactive-pane-execute-command (slot-value interface 'listener-tab)
                                           (concatenate 'string "(format t \"~%Running Example " (symbol-name ex) " \") 
                                                           (setq info-stream (sigma:get-info-stream))
                                                          (if (not (fboundp '" (symbol-name ex)  "))
                                                               (compile-example-file '" (symbol-name ex) "))
                                                             (progn    
                                                               (" (symbol-name ex) ")
                                                               (format t \" Running example \")
                                                               (info-stream-func '" (symbol-name ex) ")

                                                               (reset-from-regression)
                                                               )"
                                                        )
                                           )
    )
  ;)

;this function to be called from other packages to disply message in Main stream
(defun info-stream-func(msg)
  (format info-stream "~%~%Finished running example ~S" msg)
)

(defun reset-from-regression()
  (setq from-regression nil)
  )

;Change color basically changes the color of the text when the new examples is executed.
;The implementation of this part was a bit tricky since there is no direct way to color text in collector pane
(defun change-color (pane point old-len new-len)
  (declare (ignore pane old-len))
  (editor:with-point ((start point))
    (editor:character-offset start new-len)
    (editor:put-text-property start point
                              'editor:face
                              (editor:make-face current-color :foreground current-color :if-exists t))
    )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Compile file basically looks for the example file if they are not found in the main program
;;; It look for corresponding files and compile them in the given folders
;;; If still cannot be found it prompt to select the file.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun compile-example-file (ex)
  (let* ((filename (getf example (values (intern (symbol-name ex)  "KEYWORD"))))
         (example-paths '())
         )
    (dolist (ep example-directory)
      (setq example-paths (append example-paths (list (merge-pathnames ep (user-homedir-pathname)))))
      )
    (if filename
        (progn
          (dolist (ex-path example-paths)
            (if (probe-file (merge-pathnames filename ex-path))
                (progn
                  (compile-file (merge-pathnames filename ex-path) :load t :in-memory t) ;:output-file (merge-pathnames fasl-directory (user-homedir-pathname)))
                  (return t)
                  )
              (progn
                (multiple-value-bind (fname exist)
                    (capi:prompt-for-file "Select file"
                                  :pathname (user-homedir-pathname)
                                  :filters '("Lisp Source Files" "*.lisp;*.lsp"))
                  (when exist
                    (compile-file fname :load t :in-memory t) ;:output-file (merge-pathnames fasl-directory (user-homedir-pathname)))          
                    (return t)
                    )
                  )
                (return nil)
                )
              )
            )
          )
      (progn
        (multiple-value-bind (fname exist)
            (capi:prompt-for-file "Select file"
                                  :pathname (user-homedir-pathname)
                                  :filters '("Lisp Source Files" "*.lisp;*.lsp"))
          (when exist
            (compile-file fname :load t :in-memory t)     
            (return-from compile-example-file t)
            )
          )        
        )
      )
    )
  )


(defun create-example-listeners (interface)
  (dolist (ex *example-list*)
    (unless (find-package ex)
      (make-package ex)
      )
    (setq list-ex (acons ex (make-instance 'capi:listener-pane :title (format nil "~S" ex)) list-ex))
    (with-slots (test-all-listeners) interface   
      (setf (capi:layout-description test-all-listeners)
            (append
             (capi:layout-description test-all-listeners)
             (list (cdr (assoc ex list-ex)))
             )))    
    )
  )

; when abort button is clicked
(defun abort-process (data interface)
  (declare (ignore data))
  (mp:process-kill (mp:find-process-from-name "testing"))
  (capi:execute-with-interface interface 'capi:destroy interface)  
  (format info-stream "~%~%The testing was aborted")
  )
;updating the progress bar
(defun update-pb(pb val)
  (capi:execute-with-interface (capi:element-interface pb) (lambda () (setf (capi:range-slug-start pb) val)))
  )

(defun get-info-stream()
  info-stream
  )


;when the regression testing window closes
(defun close-regression (interface)
  (declare (ignore interface))
  (setq list-ex '())
  (when (mp:find-process-from-name "testing") (mp:process-kill (mp:find-process-from-name "testing")))
  (when (mp:find-process-from-name "progress-bar") (mp:process-kill (mp:find-process-from-name "progress-bar")))
  (when (mp:find-process-from-name "update-process") (mp:process-kill (mp:find-process-from-name "update-process")))
  (setq trace-stream t)
  (setq from-regression nil)
  )

(defun regression-testing()
  (if (capi:locate-interface 'regression)
      (capi:find-interface 'regression)
    (progn 
      (setq current-color :black) 
      (setq regression-interface (make-instance 'regression
                                                :destroy-callback 'close-regression))
      (create-example-listeners regression-interface)
      (setq color-examples (pairlis *example-list* (nthcdr (abs (- (list-length *font-colors*) (list-length *example-list*))) *font-colors*)))
      (capi:display regression-interface)
      )
    )
  )

(defun read-write (infname regout)
  (with-open-file (stream1 infname)
    (read-line stream1 nil)
    (do ((line (read-line stream1 nil)
               (read-line stream1 nil)))
        ((null line))
      (princ line regout)
      (princ #\Newline regout))))
;      (pprint-newline :fill str))))
;      (princ "~&" str))))

(defun do-read-write ()
  (with-open-file (regstream *regoutfile* :direction :output :if-exists :supersede :if-does-not-exist :create)
    (dolist (sf *sigfiles*)
      (let ((sfile (merge-pathnames *sigmahome* sf)))
      (read-write sfile regstream)))))
