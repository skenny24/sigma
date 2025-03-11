;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Interface for running instances of a function in parallel over a list of parameters
;;;
;;; Usage:
;;;
;;; CL-USER> (run-parallel path-to-code function-name arg-list)
;;;
;;; Example:
;;; CL-USER> (run-parallel "~/sigma/test.lisp" 'runmodel '(1 0 2.5 3))
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sigma)
(defparameter *parallel-font-colors* '(:black :red :green :blue :lemonchiffon4 :magenta4 :burlywood4 :red3 :green3 :blue3 :red4 :green4 :blue4))
(defvar parallel-color-examples '())
;we want the default color to be black
(defvar curr-color :black)
;exception variable will store the value of any exception which was caught and will  be used to display it to the user
(defparameter *parallel-exception* nil)
(defvar parallel-exception)
;all the listeners for the examples
(defvar parallel-list-ex '())

;initializing the variables
(setq parallel-exception *parallel-exception*)
(defparameter *func-file* nil)
(defparameter *main-func* "")
(defparameter *parameter-list* '())
(defvar proccnt)
(defvar procstr)
(defvar exstr)
(defvar cnt)
(defvar *mbox* (mp:make-mailbox))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Main interface macro which will display the Testing screen
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(capi:define-interface parallel ()
  ()
  (:panes 
   (header capi:title-pane
           :text "multiprocessing"
           :font (graphics-ports:make-font-description
                  :family "times"
                  :size 18
                  :weight :medium
                  :slant :roman))
   
   (run-jobs capi:push-button
                      :text "run all jobs"
                      :selection-callback 'run-jobs)

   (tae-info capi:title-pane
             :text "This will run an instance of your function for each input parameter")

   (listener-tab capi:listener-pane
                 :title "Listener"
                 )
   (info-tab capi:collector-pane
             :enabled :read-only
             :title "Update"
             :title-position :frame
             :change-callback 'parallel-change-color
             :background :grey95
             :horizontal-scroll t
             :visible-max-width 800
             )  
   ) 

; layout of the interface as seen by the user
  (:layouts  
   
   (column-left capi:column-layout
                '(header tae-info run-jobs info-tab)
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
   :title "Parallel Testing"
   :width (* (capi:screen-width (capi:convert-to-screen)) 0.9)
   :height (* (capi:screen-height (capi:convert-to-screen)) 0.9)
   )
  )

(defmethod current-view ((interface parallel))
  (let ((child (capi:switchable-layout-visible-child (main-layout interface))))
    (unless (symbolp child)
      (setf child (capi:capi-object-name child)))
    child)
  )

;;; Change the view.
(defmethod (setf current-view) (new-view (interface parallel))
  (setf (capi:switchable-layout-visible-child (main-layout interface))
        (ecase new-view
          (row-main (row-main interface))
          (test-all-listeners (test-all-listeners interface))))
    )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This section handles spawing of the instances/jobs
;;; and prints working memory.
;;; When the user clicks on the 'run all' button a thread
;;; is created which runs the job in the background
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun run-jobs (data interface)
  (declare (ignore data))
  (setq proccnt 0)
  (setq from-parallel t)
  (setq curr-color :black)
  ;change the view 
  (setf (current-view interface) 'test-all-listeners)
  (setf (capi:choice-selected-item (tabs interface)) 'test-all-listeners)
  (format info-stream "~%~%Running all Jobs~%")
  ;go through the list again and print call the thread individually
  (dolist (ex *parameter-list*)
    (setq exstr (concatenate 'string (write-to-string proccnt) "_" (write-to-string ex)))
    (capi:interactive-pane-execute-command (cdr (nth proccnt parallel-list-ex))
                                           (concatenate 'string "(format t \"~%Running Instance " exstr " \") 
                                                         (in-package " exstr ")
                                                         (compile-file-if-needed (merge-pathnames \"./regression-testing.lisp\" 
                                                                                                  (hcl:get-working-directory))
                                                                                 :load t
                                                                                 :in-memory t
                                                                                 )
                                                         
                                                         (progn
                                                           (setq info-stream (sigma::parallel-get-info-stream))
                                                          (if (not (fboundp '" exstr  "))
                                                               (sigma::compile-user-src-file ))
                                                             (progn    
                                                               (sigma::savedat(" (symbol-name *main-func*) " " (write-to-string ex) "))
                                                               (format t \" Running instance \")
                                                               (sigma::parallel-info-stream-func '" exstr ")
                                                               (sleep 10)
                                                               (sigma::reset-from-parallel)
                                                               ))"
                                                        )
                                           )
    (setq proccnt (+ proccnt 1)))
  (setq proccnt 0))

;this function to be called from other packages to disply message in Main stream
(defun parallel-info-stream-func(msg)
  (format info-stream "~%~%Finished running job ~S" msg)
)

(defun savedat(thisout)
  (mp:mailbox-send *mbox* thisout))

(defun reset-from-parallel()
  (setq from-parallel nil)
  )

;Change color basically changes the color of the text when the new examples is executed.
;The implementation of this part was a bit tricky since there is no direct way to color text in collector pane
(defun parallel-change-color (pane point old-len new-len)
  (declare (ignore pane old-len))
  (editor:with-point ((start point))
    (editor:character-offset start new-len)
    (editor:put-text-property start point
                              'editor:face
                              (editor:make-face curr-color :foreground curr-color :if-exists t))
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Callbacks for executing the instances manually and running the specific
;;; decisions and printing the working memory.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;calling examples 
(defun parallel-callback-function(data interface)
  (capi:apply-in-pane-process (slot-value interface 'listener-layout) #'(setf capi:layout-x-ratios) '(90 0) (slot-value interface 'listener-layout))
  ;remove all the listener panes and only keep the main
  (with-slots (listener-layout) interface   
    (setf (capi:layout-description listener-layout)
          (list (car
           (capi:layout-description listener-layout)))          
          ))
  
  (setq from-parallel t)
  (setq trace-stream (capi:interactive-pane-stream (slot-value interface 'listener-tab)))
  ;(setq curr-color :black)        
  (setq curr-color (cdr (assoc data parallel-color-examples)))
  (format info-stream "~%~%Runnning example ~S" data)
  (if (not (fboundp data))
      (progn
        (if (compile-user-src-file)
            (progn
              ;making sure that the compiled file is the right one
              (if (fboundp data)
                  (parallel-progress-dialog data ())
                (format info-stream "~% Please make sure you have compiled the right file for the example ~S" data)
                )
              )
          (format info-stream "~%Please select the file where the example ~S is located" data)
          )
        )
    (parallel-progress-dialog data () )
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; prompt user for for file containing function to apply
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun compile-user-src-file ()
  (if *func-file*
            (compile-file *func-file* :load t :in-memory t)     
            (return-from compile-user-src-file t))
            )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Progress dialog thread which creates a progress bar and runs the functions
;;; in the background.
;;; If you dont want the progress bar you can always comment it and it will only run the thread
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun parallel-progress-dialog(funct arguments &key (start 0) (end 100))
  (let* ((progress-bar (make-instance 'capi:progress-bar
                                      :start start
                                      :end end
                                      :external-min-width 30
                                      :external-min-height 30))
         (update-progress-bar (mp:process-run-function "update process"
                                                       ()
                                                       (lambda ()                                                      
                                                         (dotimes (i end)
                                                           (sleep 0.5)
                                                           (parallel-update-pb progress-bar (mod i end)) ;(setf (capi:range-slug-start progress-bar) (mod i end))
                                                           )                                                                                                
                                                         )
                                                       ))
         (buttons (make-instance 'capi:push-button-panel
                                 :items '("Abort")
                                 :selection-callback 'parallel-abort-process
                                 ))
         (row-layout (make-instance 'capi:column-layout
                                    :description (list progress-bar buttons)
                                    :visible-min-width 300
                                    :title "Please wait..."
                                    :title-position :frame
                                    ))         
         (progress-bar-thread (mp:process-run-function "progress-bar"
                                                       ()
                                                       (lambda ()
                                                         (capi:contain row-layout)
                                                         )                                  
                                                       ))
         )         
    (mp:ensure-process-cleanup
     (lambda (process)
       (declare (ignore process))
       (unwind-protect
           (progn
             (format info-stream "~&~%Function ~S completed successfully" funct)
             (capi:execute-with-interface (capi:element-interface progress-bar) (lambda () (setf (capi:range-slug-start progress-bar) end)))
             (mp:process-kill update-progress-bar)
             (sleep 1.5)
             (capi:execute-with-interface (capi:element-interface row-layout) (lambda() (capi:quit-interface row-layout)))
             (mp:process-kill progress-bar-thread)
             (setq from-parallel nil)
             (setq trace-stream t)
             ))
       )
     (apply #'mp:process-run-function "testing"
            ()
            funct
            arguments)
     )
    nil)                                      
  )

(defun create-parallel-listeners (interface mainfunc fargs)
  (setq *parameter-list* fargs)
  (setq *main-func* mainfunc)
  (setq cnt 0)
  (dolist (ex *parameter-list*)
    (setq procstr (concatenate 'string (write-to-string cnt) "_" (write-to-string ex)))
    (unless (find-package procstr)
      (make-package procstr)
      )
    (setq parallel-list-ex (acons procstr (make-instance 'capi:listener-pane :title (format nil "~S" ex)) parallel-list-ex))
    (with-slots (test-all-listeners) interface   
      (setf (capi:layout-description test-all-listeners)
            (append
             (capi:layout-description test-all-listeners)
             (list (cdr (assoc procstr parallel-list-ex)))
             )))    
        (setq cnt (+ 1 cnt)))
  (setq cnt 0))

; when abort button is clicked
(defun parallel-abort-process (data interface)
  (declare (ignore data))
  (mp:process-kill (mp:find-process-from-name "testing"))
  (capi:execute-with-interface interface 'capi:destroy interface)  
  (format info-stream "~%~%The testing was aborted")
  )
;updating the progress bar
(defun parallel-update-pb(pb val)
  (capi:execute-with-interface (capi:element-interface pb) (lambda () (setf (capi:range-slug-start pb) val)))
  )

(defun parallel-get-info-stream()
  info-stream
  )


;when the parallel testing window closes
(defun close-parallel (interface)
  (declare (ignore interface))
  (setq parallel-list-ex '())
  (when (mp:find-process-from-name "testing") (mp:process-kill (mp:find-process-from-name "testing")))
  (when (mp:find-process-from-name "progress-bar") (mp:process-kill (mp:find-process-from-name "progress-bar")))
  (when (mp:find-process-from-name "update-process") (mp:process-kill (mp:find-process-from-name "update-process")))
  (setq trace-stream t)
  (setq from-parallel nil)
  )

(defun run-parallel(mfunc margs &optional funcfile)
"run jobs in parallel, argument 1 is the function to run, argument 2 is the list of parameters to run the function on and the 3rd optional argument is a file to be compiled if the function to be run is not in sigma.lisp"
(if funcfile
  (setq *func-file* funcfile))
  (if (capi:locate-interface 'parallel)
      (capi:find-interface 'parallel)
    (progn 
      (setq curr-color :black) 
      (setq *parallel-interface* (make-instance 'parallel
                                                :destroy-callback 'close-parallel))
      (setq trace-stream (capi:interactive-pane-stream (slot-value *parallel-interface* 'listener-tab)))
      (setq info-stream (capi:collector-pane-stream (slot-value *parallel-interface* 'info-tab)))
      (create-parallel-listeners *parallel-interface* mfunc margs)
      (setq parallel-color-examples (pairlis *parameter-list* (nthcdr (abs (- (list-length *parallel-font-colors*) (list-length *parameter-list*))) *parallel-font-colors*)))
      (capi:display *parallel-interface*)
      )
    )
  *mbox*)

