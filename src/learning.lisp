(in-package :sigma)
; Learning
; --------
; Function for controlling what learning is turned on
; Given a call, only those forms of learning explicitly mentioned will be on
; except that gradient descent must always be on if diachronic prediction is
(defun learn (&optional ons)
  (dolist (on ons)
    (unless (member on '(:gd :gradient-descent :e :episodic :open :no-normal :action-model :am :perception-model :pm :rl :reinforcement-learning))
      (error "Unrecognized keyword ~S in (LEARN ~S).~&Known keywords are :gd :gradient-descent :e :episodic :open :no-normal :action-model :am :perception-model :pm :rl :reinforcement-learning." on ons)
      )
    )
  (setq episodic-learning (if (or (member ':e ons)
                                  (member ':episodic ons))
                              t
                            nil))
  (when (and episodic-learning (not track-time))
    (error "Can't turn on episodic learning unless TRACK-TIME is set to T before INIT is run.")
    )
  (setq automatic-action-models (if (or (member ':am ons)
                                        (member ':action-model ons))
                                    t
                                  nil))
  (setq automatic-perception-models (if (or (member ':pm ons)
                                            (member ':perception-model ons))
                                        t
                                      nil))
  (setq automatic-reinforcement-learning (if (or (member ':rl ons)
                                                 (member ':reinforcement-learning ons))
                                             t
                                           nil))
  (setq diachronic-prediction (if (or automatic-action-models
                                      automatic-perception-models
                                      automatic-reinforcement-learning
                                      episodic-learning)
                                  t
                                nil))
  (setq learn-via-gradient-descent (if (or (member ':gd ons)
                                           (member ':gradient-descent ons)
                                           automatic-action-models
                                           automatic-perception-models
                                           episodic-learning
                                           automatic-reinforcement-learning)
                                       t
                                     nil))
  (setq learn-open (member ':open ons))
  (setq learn-no-normal (member ':no-normal ons))
  (when episodic-learning
      (init-temporal-conditional)
      )
  (when learn-via-gradient-descent
    (setq feedback-to-functions t))
  (or learn-via-gradient-descent episodic-learning) ; Return T if any form of learning is turned on
  )

; -----------------------------------------------------------
; Episodic learning

; Initialize temporal predicates for episodic memory
(defun init-temporal-conditional nil
  (let ((pred (predicate-from-name temporal-predicate-name))
        )
    (setf (predicate-function pred) `((,smoothing-parameter *) (,(* 2 smoothing-parameter) 0)))
    (setf (predicate-learning-rate pred) .00000002)
    (compile-predicate-function pred)
    ; Temporal predicate and conditional for episodic memory
    (when episodic-learning
      (predicate temporal-predicate-name-episodic
                 :arguments (list (list 'value temporal-predicate-name distribution-symbol))
                 :function temporal-predicate-name
                 :learning-rate .0002)
      )
    )
  )

; For closed-world arguments, make them open-world by replacing ! with %
(defun convert-argument-open-world (arg)
  (if (eq (third arg) best-symbol)
      (list (first arg) (second arg) distribution-symbol)
    (copy-seq arg))
  )

; Define episodic memory predicates and conditionals for a newly defined predicate
(defun init-predicate-em (pred)
  (let* ((pname (predicate-name pred)) ; Name of predicate
         (pargs (predicate-arguments pred)) ; Predicate's argument list
         (opargs (mapcar #'convert-argument-open-world pargs)) ; Open world version of predicate arguments
         pelements ; List of elements for arguments to use in defining pattern
         lname ; Name of learning predicate
         sname ; Name of selection predicate
         rname ; Name of retrieval predicate
         epname ; Name of episodic memory version of predicate (open world)
         ep ; Episodic memory version of predicate (open world)
         )
    ; Create STATE predicate if doesn't already exist
    (unless (predicate-from-name 'state t)
      (system-predicate 'state :world 'closed :arguments (if multiagent '((agent agent) (state state)) '((state state))))
      )
    (setq epname (concat-symbols (list pname episodic-suffix) nil))
    (setq ep (predicate epname :arguments opargs)) ; Create episodic predicate
    (setq pelements (mapcar #'argument-to-element pargs))
    ; Episodic Learning
    (setq lname (concat-symbols (list temporal-predicate-name epname 'learn) t)) ; Episodic learning function predicate
    (predicate lname ; Episodic learning predicate
               :arguments (cons (list temporal-predicate-name temporal-predicate-name) opargs)
               :function 1
               :learning-rate .2
               :episodic t
               )
    (apply #'conditional (concat-symbols (list lname '*c)) ; Create conditional for learning episodic memory
           :conditions (list '(state (state (state)))
                               (list temporal-predicate-name (list 'value (list temporal-predicate-name)))
                               (cons pname pelements))
           :condacts (list (append (list lname `(,temporal-predicate-name (,temporal-predicate-name))) pelements))
;           :episodic t ; Just used for printing EM
           nil)
    ; Episodic Selection
    (setq sname (concat-symbols (list temporal-predicate-name epname 'select) t)) ; Episodic selection function predicate
    (predicate sname ; Episodic selection predicate
               :arguments (cons (list temporal-predicate-name temporal-predicate-name) opargs)
               :function lname
               :learning-rate 0
               )
    (apply #'conditional (concat-symbols (list sname '*c)) ; Create conditional for learning episodic memory
           :conditions (list '(state (state (state)))
                              (cons (concat-symbols (list pname prediction-suffix)) pelements))
           :condacts (list (list temporal-predicate-name-episodic (list 'value (list temporal-predicate-name)))
                           (append (list sname `(,temporal-predicate-name (,temporal-predicate-name))) pelements))
;           :episodic t ; Just used for printing EM
           nil)
    ; Episodic Retrieval
    (setq rname (concat-symbols (list temporal-predicate-name epname 'retrieve) t)) ; Episodic retrieval function predicate
    (predicate rname ; Episodic retrieval predicate
               :arguments (cons (list temporal-predicate-name temporal-predicate-name) opargs)
               :function lname
               :learning-rate 0
               )
    (apply #'conditional (concat-symbols (list rname '*c)) ; Create conditional for learning episodic memory
           :conditions (list '(state (state (state)))
                              (list temporal-predicate-name-episodic (list 'value (list temporal-predicate-name))))
           :condacts (list (cons epname pelements)
                           (append (list rname `(,temporal-predicate-name (,temporal-predicate-name))) pelements))
           :map t
;           :episodic t ; Just used for printing EM
           nil)
    (setf (predicate-em pred) (predicate-function-node (predicate-from-name lname)))
    (setf (predicate-em-predicate pred) ep)
    )
  )

; -----------------------------------------------------------
; Learn (gradient descent)

; Determine whether to trace learning for a node
(defun trace-gdl (node)
  (and trace-gdl
       (or (not (consp trace-gdl)) ; If its a symbol then assume true for all nodes
           (member (node-number node) trace-gdl)
           (member (node-name node) trace-gdl)
           (member (node-function-name node) trace-gdl)
           )
       )
  )

; Initialize a weighted gradient with the learning rate (i.e., the weight)
; D-length is the length of the normalization dimension, when provided
; lr is the learning rate
(defun initialize-weighted-gradient (variables lr)
  (make-constant-discrete-plm (number-list (length variables)) variables lr 0)
  )

; Compute integral over all dimensions of a PLM
; Just return value
(defun integral-over-all-dimensions-plm (plm)
  (dotimes (i (length (plm-variables plm)))
      (setq plm (integral-plm plm i))
      )
  (region-constant (plm-origin plm))
  )

; Compute integral over all dimensions of a PLM except for the state dimension if there is one
; Just return value
(defun integrate-out-all-but-state-dimension-plm (plm)
  (dotimes (i (length (plm-variables plm)))
    (unless (eq (stype-name (svariable-type (aref (plm-variables plm) i))) 'state)
      (setq plm (integral-plm plm i))
      )
    )
  plm
  )

; Compute average over all dimensions of a PLM
; Just return value
(defun average-over-all-dimensions-plm (plm)
  (dotimes (i (length (plm-variables plm)))
      (setq plm (average-plm plm i))
      )
  (region-constant (plm-origin plm))
  )

; Compute integral of function-message product at a conditional factor function node (a single number)
; Multiply incoming message times factor function and then integrate out all dimensions
; Currently assumes there are no universal variables or that it is okay to integrate over them
; Currently uses normalized incoming message rather than raw version
(defun integral-function-message-product (mf map nf tgdl)
  (let (f v)
    (setq f (combine-plms mf map nf 'product))
    (when tgdl
      (format trace-stream "~&Product of existing factor function and raw update function: ")
      (pplm f)
      )
    (setq v (integral-over-all-dimensions-plm f))
    (when tgdl
      (format trace-stream "~&Result of integrating over dimensions of product: ~S" v)
      )
    v)
  )

; Normalize over all dimensions of a PLM
; Intended to be used when no normalization dimension is provided
(defun normalize-over-all-dimensions-plm (plm &optional destructive)
  (let ((integral plm))
    (dotimes (i (length (plm-variables plm)))
      (setq integral (integral-plm integral i))
      )
    (transform-plm #'scale-function plm (/ 1 (region-constant (plm-origin integral))) nil destructive)
    )
  )

; Determing the volume of the span of the parents of a learned function
(defun parent-span (node)
  (let ((volume 1)
        (nd (node-normal node))
        (nvs (node-variables node))
        )
    (dotimes (d (length nvs))
      (unless (or (and (numberp nd) (= d nd)) (member d nd))
        (setq volume (* volume (stype-span (svariable-type (aref nvs d)))))
        )
      )
    volume)
  )

; Determing the volume of the span of a function's dimensions
(defun function-span (node)
  (let ((volume 1)
        (nvs (node-variables node))
        (nn (node-normal node))
        )
    (dotimes (d (length nvs))
      (when (member d nn)
        (setq volume (* volume (stype-span (svariable-type (aref nvs d)))))
        )
      )
    volume)
  )

; Compute the weighted gradient to be used in updating the node's function
; Assumes there is only one message into the node that has the same dimensions/variables as the node
(defun compute-weighted-gradient (node smooth)
  (let* ((link (car (node-links node)))
         (update (link-var-content link)) ; Feedback to use in updating
         (map (link-map link))
         (normal (node-normal node)) ; Index of the variable to normalize over, if any
         (nf (node-function node)) ; Previous factor function
         (vs (plm-variables nf))
         (tgdl (trace-gdl node))
         ifmp ; Value to divide by in scaling update
         epg-sub ; Subtractor for exponential-product gradient
         (lr (if (node-learning-rate node)
                 (node-learning-rate node)
               (if learning-rate-fraction-of-smoothing-parameter
                   (* learning-rate-fraction-of-smoothing-parameter smooth)
                 learning-rate)))
         (alr (if arousal (* arousal lr) lr))
         (wg (initialize-weighted-gradient vs alr))  ; Initialize plm with learning rate
         average ; Average function for subtraction
         ifmp-vars
         max-update ; Maximum value(s) of update function along normalization dimension(s)
         (predicate-argument-unique-positions (init-vector (length vs))) ; Which variables (by position) are unique arguments for GDL in predicate
         up ; unique position
         (pred (node-predicate node)) ; Predicate for node
         )
    (setf (plm-piecewise-constant wg) t)
    (when tgdl
      (format trace-stream "~&~%~%Gradient descent on node ~A with learning rate ~A" (node-name node) lr)
      (when arousal
        (format trace-stream " (and arousal rate of ~A, for an effective learning rate ~A)" arousal alr)
        )
      (format trace-stream ": ")
      (format trace-stream "~&Existing factor function: ")
      (pplm nf)
      (format trace-stream "~&~%Raw update function: ")
      (pplm update)
      (format trace-stream "~&~%")
      )
    (cond ((or (eq gdl-divide 'newer) ; Integrate over all variables that are unique in the conditional
               exponential-product-gradient ; Always use this with the exponential gradient
               )
;           (when (= (node-number node) 110) (break))
           (setq ifmp (combine-plms update map nf 'product))
           (when tgdl
             (format trace-stream "~&Product of existing factor function and raw update function: ")
             (pplm ifmp)
             )
           (setq ifmp-vars (plm-variables ifmp))
           ; Determine positions of unique variables from predicate
           (when pred
             (dolist (an (predicate-unique-for-gdl pred))
               (setq up (position-of-predicate-argument an pred))
               (when up
                 (setf (aref predicate-argument-unique-positions up) t)
                 )
               )
             )
           (dotimes (d (length ifmp-vars))
             (when (or (svariable-unique (aref ifmp-vars d))
                       (and pred
                            (aref predicate-argument-unique-positions d)
                            )
                       )
               (setq ifmp (integral-plm ifmp d))
               (setf (aref (plm-active ifmp) d) t) ; Mark integrated out dimension as active so that division works right
               )
             )
           (setq update (combine-plms update map ifmp 'divide-0))
           (when tgdl
             (format trace-stream "~&Divisor function for normalization (integral over unique dimensions of product): ")
             (pplm ifmp)
             (format trace-stream "~&Update function after normalization via integral over product: ")
             (pplm update)
             )
           )
          ((and (eq gdl-divide 'new) normal) ; Integrate over just the normal variable
           (setq ifmp (combine-plms update map nf 'product))
           (when tgdl
             (format trace-stream "~&Product of existing factor function and raw update function: ")
             (pplm ifmp)
             )
           (if (numberp normal)
               (progn
                 (setq ifmp (integral-plm ifmp normal))
                 (setf (aref (plm-active ifmp) normal) t) ; Mark integrated out dimension as active so that division works right
                 )
             (dolist (n normal)
               (setq ifmp (integral-plm ifmp n))
               (setf (aref (plm-active ifmp) n) t) ; Mark integrated out dimension as active so that division works right
               ))
           (setq update (combine-plms update map ifmp 'divide-0))
           (when tgdl
             (format trace-stream "~&Divisor function for normalization (integral over normal dimension of product): ")
             (pplm ifmp)
             (format trace-stream "~&Update function after normalization via integral over product: ")
             (pplm update)
             )
           )
          (gdl-divide ; Integrate over all variables
           (setq ifmp (integral-function-message-product update map nf tgdl))
           (unless (= ifmp 0) ; Only divide if not by 0
             (setq update (transform-plm #'scale-function update (/ 1 ifmp) nil t))
             )
           (when tgdl
             (if (= ifmp 0)
                 (format trace-stream "~&Update function not divided by integral because it is 0")
               (format trace-stream "~&Update function after division by integral (~S): " ifmp)
               )
             (pplm update)
             )
           )
          (normal ; Old normalization
           (if (numberp normal)
               (setq update (normalize-plm update normal t))
             (dolist (n normal)
               (setq update (normalize-plm update n t))
               ))
           (when tgdl
             (format trace-stream "~&Update function normalized over variable(s) ~S (dimension(s) ~S): "
                     (if (numberp normal)
                         (svariable-name (aref (node-variables node) normal))
                       (mapcar #'(lambda (n) (svariable-name (aref (node-variables node) n))) normal))
                     normal)
             (pplm update)
             )
           )
          (t (error "~&~%No way to normalize gradient in learning!"))
          )
    (when (and gradient-subtract-average ; Subtract the function's average from the function
               (not exponential-product-gradient) ; Never do with exponential-product gradient
               )
      (if normal
          (progn
            (setq average (if (numberp normal) (average-plm update normal) (average-plm-ds update normal)))
            (when tgdl
              (format trace-stream "~&Average to be subtracted from function:")
              (pplm average)
              )
            (setq update (combine-plms update nil average 'difference))
            )
        (progn
          (setq average (average-over-all-dimensions-plm update))
          (when tgdl
              (format trace-stream "~&Average to be subtracted from function: ~S" average)
              )
          (setq update (transform-plm #'add-to-function update (- average) nil t))
          ))
      (when tgdl
        (format trace-stream "~&Update function after average subtracted: ")
        (pplm update)
        )
      )
    ; When doing exponential product gradient, subtract divisor from update and multiply result by original factor function
    (when exponential-product-gradient
      (setq epg-sub (combine-plms update nil nf 'product))
      (when tgdl
        (format trace-stream "~&Unintegrated subtractor for exponential-product gradient: ")
        (pplm epg-sub)
        )
      (setq epg-sub (normalize-plm epg-sub normal t))
      (when tgdl
        (format trace-stream "~&Integrated subtractor (over child/normal variable(s)) for exponential-product gradient: ")
        (pplm epg-sub)
        )
      (setq update (combine-plms update nil epg-sub 'difference))
      (when tgdl
        (format trace-stream "~&Update function after subtraction: ")
        (pplm update)
        )
      (setq update (combine-plms nf nil update 'product))
      (when tgdl
        (format trace-stream "~&Update function multiplied times existing factor function: ")
        (pplm update)
        )
      )
    ; Multiply gradient by learning rate
    (setq update (combine-plms update map wg 'product))
    (when tgdl
      (format trace-stream "~&Update function discounted by learning rate of ~S: " alr)
      (pplm update)
      )
    ; Optionally limit maximum value in gradient
    (when max-gdl-increment
      (setq max-update update)
      (if (listp normal)
          (dolist (n normal)
            (setq max-update (maximize-plm max-update n))
            )
        (setq max-update (maximize-plm max-update normal)))
      (setq update (combine-plms max-update nil update 'gdl-scale))
      (when tgdl
        (format trace-stream "~&Max values in update function over normalization dimensions: ")
        (pplm max-update)
        (format trace-stream "~&Update function limited to maximum increment of ~S: " max-gdl-increment)
        (pplm update)
        )
      )
    ; When doing exponential product gradient, exponentiate the result (which currently is always a constant value)
    (when exponential-product-gradient
      (setq update (transform-plm #'exponentiate-constant-function update nil nil t))
      (when tgdl
        (format trace-stream "~&Update function exponentially transformed: ")
        (pplm update)
        )
      )
  update)
  )

; Span (product) across multipe stypes
(defun stype-spans (ts)
  (let ((prod 1))
    (dolist (tx ts)
      (setq prod (* prod (stype-span tx)))
      )
    prod)
  )

(defun update-via-gradient (node)
  (let (new
        (normald (node-normal node)) ; The dimension to normalize over, if any
        min-new ; Smallest value in weighted gradient
        (tgdl (trace-gdl node))
        wg ; Weighted gradient
        smooth ; The value to use for smoothing
        smoothed ; Whether have done smooothing
        surprise-function
        (vector (node-vector node)) ;is a vector being learnt 
        )
    (setq smooth
          (if (node-smoothing-parameter node) ; Override
              (node-smoothing-parameter node)
            (if (and adaptive-smoothing normald)
                (/ .1 (+ (node-changes node) (if (numberp normald)
                                                 (stype-span (svariable-type (aref (node-variables node) normald)))
                                               (stype-spans (mapcar #'(lambda (x) (svariable-type (aref (node-variables node) x))) normald)))))
              smoothing-parameter)))
    (if vector (setq wg (compute-vector-gradient node smooth)) 
      (setq wg (compute-weighted-gradient node smooth))
      )
    (unless (plm-empty wg)
      (setf (node-changes node) (1+ (node-changes node)))
      (setq new (combine-plms (node-function node) nil wg (if exponential-product-gradient 'product 'sum)))
      (setq min-new (plm-extreme new #'< infinity))
      (when tgdl
        (format trace-stream "~&Updated function (unnormalized): ")
        (pplm new)
        )
      (cond (vector ;only perform vector normalization, no need for smoothing and resmoothing 
             ;(setq new (normalize-plm new normald t)) ;vector learning normalization required multiple iterations for learning, not normalizing learns quicker 
             (when tgdl
               (format trace-stream "~&Vector normalized updated function: ")
               (pplm new)
               )
             )
            (t ;default gradient descent case applies here if not processing vectors
             ; Need to shift whole gradient function up so that is above parameter (smoothing)
             (when (< min-new smooth)
               (setq smoothed t)
               (setq new (smooth-plm new smooth)) ; Shift up values below threshold
               (when tgdl
                 (format trace-stream "~&Smoothed (~S) unnormalized updated function: " smooth)
                 (pplm new)
                 )
               )
             (when normald ; There are normalization dimensions
               ; Both here and below, must be able to use subtractive when no normalization dimension as well
               (if (and gdl-subtractive-normalization (not exponential-product-gradient))
                   (setq new (subtractive-normalize-plm new smooth normald))
                 (setq new (normalize-plm new normald t)))
               )
             (when tgdl
               (format trace-stream "~&Normalized ")
               (when smoothed
                 (format trace-stream "smoothed ")
                 )
               (format trace-stream "updated function: ")
               (pplm new)
               )
             ; If normalization creates very small numbers (more than a factor of 10 smaller than parameter), redo smoothing and normalization
             (setq min-new (plm-extreme new #'< infinity))
             (when (< min-new (* smooth .1)) ; Need to shift whole gradient function up so that is above parameter (smoothing)
               (setq new (smooth-plm new smooth))
               (when tgdl
                 (format trace-stream "~&Resmoothed updated function: ")
                 (pplm new)
                 )
               (if normald ; There is a normalization dimension
                   (setq new (normalize-plm new normald t))
                 (setq new (normalize-over-all-dimensions-plm new t)))
               (when tgdl
                 (format trace-stream "~&Renormalized resmoothed updated function: ")
                 (pplm new)
                 )
               )
             )
            )
      ; Constrain any regions that were 0 in the original function to remain 0
      (setq new (combine-plms (node-restriction node) nil new 'product))
      ; Update the surprise function (unless :no-surprise or not computing)
      (when (and (node-predicate node) (predicate-surprise-predicate (node-predicate node)))
        (setq surprise-function (hd-plms (node-function node) new (unique-dimensions (node-function node))))
        (setq surprise-function (strip-and-reorder-plm
                                 (build-smap (node-variables (predicate-perception (node-surprise-predicate node))) (plm-variables surprise-function))
                                 (node-variables (predicate-perception (node-surprise-predicate node))) surprise-function))
        (setq surprise-function (remove-unneeded-slices surprise-function))
        (when trace-attention
          (when surprise-function
            (format trace-stream "~&~%~S Surprise Map (Hellinger distance):~&" (predicate-name (node-predicate node)))
            (print-smart surprise-function t trace-stream)
            )
          )
        (setf (predicate-perception-temp-mem (node-surprise-predicate node)) surprise-function) ; Local surprise function
        )
      ; Update node function
      (setf (node-function node) new)
      ; Store the new function into all of the other CFF nodes that share the same function
      (dolist (shared (node-shared-functions node))
        (setf (node-function shared) new)
        )
      (when tgdl
        (format trace-stream "~&Function constrained by original zeros: ")
        (pplm new)
        )
      (when save-message-state
        (setf (aref (graph-changes cg) (node-number node)) t)
        (when debug-descendants
          (format trace-stream "~&Changing ~S Function" (node-name node))
          )
        (dolist (shared (node-shared-functions node))
          (setf (aref (graph-changes cg) (node-number shared)) t)
          (when debug-descendants
            (format trace-stream "~&Changing ~S Function" (node-name shared))
            )
          )
        )
      )
    )
  )

; Update all factor functions via their gradients
(defun update-functions-via-gradient ()
  (dolist (n (graph-nodes cg))
    (when (and (function-nodep n)
               (node-function-name n)
               (not (member (node-function-name n) do-not-learn))
               )
      (unless (or (not (link-var-content (car (node-links n)))) ; Link not active towards function
                  (plm-empty (link-var-content (car (node-links n)))) ; Empty message to function
                  (equal (node-learning-rate n) 0) ; Learning rate is 0
                  )
        (update-via-gradient n)
        )
      )
    )
  )

; -----------------------------------------------------------
; Create a Naive Bayes classifier for a UCI ML dataset and process data

; Define an attribute
(defun define-attribute (attribute value &optional category unsupervised)
  (if (eq (car value) 'symbolic) ; symbolic attribute
      (new-type attribute :constants (cdr value))
    (new-type attribute :numeric t :discrete (eq (car value) 'discrete) :min (cadr value) :max (caddr value)))
  (predicate attribute :perception t :world 'open :arguments `((,attribute ,attribute %)))
  (when (and category
             unsupervised
             (not (and (eq (car value) 'discrete)
                       (= (cadr value) 0)))
             )
    (error "The unsupervised category type ~S must be discrete numeric and start at 0!" attribute)
    )
  )
 
; Create a conditional with a uniform prior on the category
; Arguments are the name of the category and the number of values
(defun category-prior (cat-name nvs)
  (conditional (intern (concatenate 'string "PRIOR-" (format nil "~S" cat-name)))
               :condacts `((,cat-name (,cat-name (,cat-name))))
               :function-variable-names `(,cat-name)
               :normal cat-name
               :function `((,(/ 1.0 nvs) *))
               )
  )

; Generate random (in [0,1]) vector of length
(defun random-vector (length)
  (let ((v (init-vector length)))
    (dotimes (i length)
      (setf (aref v i) (random 1.0))
      )
    v)
  )

; Normalize vector
(defun normalize-vector (v)
  (let ((sum (reduce #'+ v)))
    (map 'vector #'(lambda (x) (/ x sum)) v)
    )
  )

; Generate a (normalized) random distribution in a vector
(defun random-distribution (length)
  (normalize-vector (random-vector length))
  )
(defun rd (length) (random-distribution length))

; Create a conditional with a uniform likelihood for the category and an attribute
; If the learning is unsupervised (i.e., if the optional types are specified) use a random likelihood function instead
(defun likelihood (att-name cat-name nvs atype ctype &optional unsupervised)
  (let (ca-pname pfun)
    (setq pfun (if unsupervised ; Generate random function for unsupervised learning
                   (let (fn d)
                     (dotimes (c (if center-discrete-numeric-on-integer (+ (stype-max ctype) 1/2) (stype-max ctype)))
                       (unless (stype-discrete atype)
                         (error "Unsupervised learning with continuous attribute (~S) is not yet supported!" att-name)
                         )
                       (setq d (random-distribution nvs))
                       (dotimes (a nvs)
                         (setq fn (cons (list (aref d a)
                                              c
                                              (if (stype-constants atype) (get-symbol-name a atype) (+ a (stype-min atype))))
                                        fn))
                         )
                       )
                     (reverse fn))
                 1))
    (setq ca-pname (predicate-name (predicate (concat-symbols (list cat-name att-name) t)
                                              :arguments `((,cat-name ,(stype-name ctype)) (,att-name ,(stype-name atype) %))
                                              :function pfun)))
    (conditional (intern (concatenate 'string "LIKELIHOOD" (format nil "-~S" cat-name) (format nil "-~S" att-name)))
                 :condacts `((,cat-name (,cat-name (,cat-name)))
                             (,att-name (,att-name (,att-name)))
                             (,ca-pname (,cat-name (,cat-name)) (,att-name (,att-name))))
                 )
    )
  )

; Define a data set with an initially uniform naive Bayes classifier for supervised learning
; If unsupervised is non-nil do clustering with a new category predicate
; avs is a list, for each of which the car is the name of the attribute and the cdr is its values
; The values can be a list of symbols or list within a list of (discrete/continuous :min :max)
; Assumes the category is the last attribute provided unless the first element of a sublist is * (category-symbol)
(defun define-data-set (avs &optional unsupervised trace)
  (let (type cat-type cat-pred)
    (init)
    (setq trace-gdl trace)
    (setq data-set-attributes nil) ; Reinitialize list of attributes (including category) in data set
    (setq data-set-category nil)
    ; Define types and predicates for attributes and category
    (dolist (av avs)
      (if (eq (car av) category-symbol)
          (progn
            (setq av (cdr av))
            (setq data-set-category (car av))
            (define-attribute (car av) (cadr av) t unsupervised)
            )
        (define-attribute (car av) (cadr av))
        )
      (setq data-set-attributes (cons (car av) data-set-attributes))
      )
    (unless data-set-category ; When no attribute is explicitly marked as the category
      (setq data-set-category (car data-set-attributes)) ; Assume it is the last category
      )
    (unless unsupervised
      (setq cat-pred (predicate-from-name data-set-category))
      (setf (predicate-function cat-pred) 1)
      (compile-predicate-function cat-pred)
      )
    (setq data-set-attributes (reverse data-set-attributes)) ; Yield attributes in order presented
    (setq cat-type (type-from-name data-set-category))
    ; Create conditionals and predicates for attribute-category pairs
    (dolist (av avs)
      (unless (or (eq (car av) category-symbol) (eq (car av) data-set-category)) ; Not the category
        (setq type (type-from-name (if (eq (car av) category-symbol) (cadr av) (car av))))
        (likelihood (car av) data-set-category (stype-span type) type cat-type unsupervised)
        )
      )
    )
  )

; Naive Bayes gradient-descent learning from data files

; Data stream for reading in instances
(defvar data-stream)

; Set things up to track results of testing
(defstruct gd-test-results attribute current true-positives true-negatives false-positives false-negatives)
(defvar gd-test-results)
(defun init-gd-test-results (att)
  (make-gd-test-results :attribute att :true-positives 0 :true-negatives 0 :false-positives 0 :false-negatives 0)
  )
(defun total-gd-test-results (tr)
  (+ (gd-test-results-true-positives tr) (gd-test-results-true-negatives tr)
     (gd-test-results-false-positives tr) (gd-test-results-false-negatives tr)
     )
  )

; Find test-result structure for an attribute
(defun find-gd-test-results (att)
  (find att gd-test-results :key #'gd-test-results-attribute)
  ); Update gd-test results (to be used within post-d)
; Only works if attribute uses T and F as its two values
(defun update-gd-test-results (atts)
       (let (tr current best)
         (dolist (att atts)
           (setq tr (find-gd-test-results att))
           (setq current (gd-test-results-current tr))
           (setq best (best-in-plm (vnp att)))
           (if (eq current t)
               (if (eq best t)
                   (setf (gd-test-results-true-positives tr)
                         (+ (gd-test-results-true-positives tr) 1))
                 (setf (gd-test-results-false-negatives tr)
                       (+ (gd-test-results-false-negatives tr) 1)))
             (if (eq best t)
                 (setf (gd-test-results-false-positives tr)
                       (+ (gd-test-results-false-positives tr) 1))
               (setf (gd-test-results-true-negatives tr)
                     (+ (gd-test-results-true-negatives tr) 1))))
           )
         )
       )


; Set up file to process one instance per decision
(defun setup-gd (file-name &optional test omit not-in-data)
  (let* (category-name)
    (setq data-stream (open file-name))
;    (setq pre-d '())
    (setq post-d `((update-gd-test-results ',omit)))
;    (setq post-t '())
    (learn (unless test '(:gd)))
    (when not-in-data ; assumes category is first in not-in-data, if exists
      (setq category-name (car not-in-data))
      )
    (when test
      (when category-name
        (setq pre-d `((format trace-stream "~&~%") (ppvn ',category-name)))
        )
      (setq gd-test-results (mapcar #'init-gd-test-results omit))
      )
    (setq perceive-list `((unless (listen data-stream)
                            (format trace-stream "*** No more instances in file. ***")
                            (throw 'decide t)
                            )
                          (instance (read-line data-stream) ',omit ',not-in-data) ; Set up perception for a test instance
                          )
          )
    t)
  )

; Create evidence for an instance specified as a comma-delimited string (one line from data set)
; If omit is provided, it is a list of attributes for which evidence shouldn't be specified
; The attributes in not-in-data are not even in the data
(defun instance (inst-s &optional omit not-in-data)
  (let (value comma-loc)
    (format trace-stream "~&~%*** Instance:")
    (empty-pers) ; Empty perceptual memories before adding the evidence
    (dolist (att-name data-set-attributes)
      (multiple-value-setq (value comma-loc) (read-from-string inst-s))
      (if (member att-name omit)
          (progn
            (setf (gd-test-results-current (find-gd-test-results att-name)) value)
            (format trace-stream " [~S:~S]" att-name value)
            )
        (unless (member att-name not-in-data) ; Create evidence for the attribute-value pair
          (format trace-stream " ~S:~S" att-name value)
          (perceive `((,att-name (,att-name ,(if (stype-discrete (type-from-name att-name)) value (list value (+ value epsilon2)))))))
          )
        )
      (when (< comma-loc (length inst-s)) ; There is another entry in the string
        (setq inst-s (subseq inst-s (+ comma-loc 1))) ; Move to next entry in the string
        )
      )
    )
  )

; -----------------------------------------------------------
; Dynamically extend a constant type

(defun check-for-new-constants (args pname) 
  (dolist (arg args)
    (let* ((pred (predicate-from-name pname))
           (constants (stype-constants (type-from-predicate-argument (car arg) pred)))
           checked-type
           )
      (when (and 
             constants ; there are constants
             (not (star-element (cadr arg))) ; the constant is not a star-element                
             (not (member (cadr arg) constants)) ; the constant is new
             )          
        (setf checked-type (type-from-predicate-argument (car arg) pred))
        (setf (stype-constants checked-type) (append (stype-constants checked-type) (list (cadr arg)))) ;update the type 
        (setf  (stype-max checked-type) (+  (stype-max checked-type) 1) )  ;update the number of constants for the type    
        (find-functions-using-type checked-type (cadr arg))   ; check the functions that use this type        
        )
      )
    )
  )

;the function types are explicitly stated; if new function types are introduced, this function needs to be updated.
(defun find-functions-using-type (checked-type new-constant)
  (let (nvs)
    (dolist (n (graph-nodes cg))
      (when (factor-nodep n)
        (setf nvs (node-variables n))
        (dotimes (i (length nvs)) 
          (when (eq (stype-name checked-type) (stype-name (svariable-type (aref nvs i)))) 
            (case (node-subtype n)
              (function (update-function-factors n checked-type new-constant))
              (beta (update-beta-factors n))
              (wm (update-wm-factors n))
              )
            )
          )
        )
      )
    )
  )

(defun update-function-factors (node checked-type new-constant)
  (let (condi condi-vars condi-normal p-vars (param 1) func)
    (setf condi (conditional-from-name (node-conditional-name node)))
    (setf condi-vars (conditional-function-variable-names condi))
    (setf condi-normal (conditional-normal condi))
    (setq p-vars (node-variables (conditional-last-memory condi)))
    
    (dotimes (i (length p-vars)) 
      (if (string-equal (svariable-name (aref p-vars i)) (stype-name checked-type)) (update-max-slice (aref (plm-slices (node-function node)) i)))
      )
      
    ;setting the conditional probability 
    (if condi-normal
        (setf param (/ param (if (symbolp condi-normal)
                                 (stype-span (type-from-name condi-normal))
                               (stype-spans (mapcar #'type-from-name condi-normal)))))
      (setf param (/ 1 (length (stype-constants checked-type))))   ; if there is normal defined, how do we do this?   
      )
   
    ;create the function
    (setf func `(,param))
    (dolist (cv condi-vars)
      (if (eq (stype-name checked-type) cv)
          (setf func (append func `(,new-constant)))
        (setf func (append func '(*)))
        ) 
      )

    (setf (node-function node) (update-region (cpt-region condi-vars func p-vars t) (node-function node)))
    (setf (aref (graph-changes cg) (node-number node)) t)
  )
)

(defun update-max-slice (slices)
  (let (max-slice)
    (dotimes (i (length slices)) (if 
                                     (or
                                      (eq max-slice nil)
                                      (> (slice-location (nth i slices)) (slice-location max-slice))
                                      ) 
                                     (setf max-slice (nth i slices))
                                   )
      )
    (setf (slice-location max-slice) (+ (slice-location max-slice) 1))
 )
)

(defun update-beta-factors (node) 
  (let (beta-node-vs beta-function)
    (setf beta-node-vs (node-variables node))
    (setq beta-function (init-plm beta-node-vs
                                  1
                                  0 (init-vector (length beta-node-vs) t)))
    (setf (node-function node) beta-function)
    (setf (aref (graph-changes cg) (node-number node)) t)
    )
  )

(defun update-wm-factors (node)
  (let (vars)
    (setf vars (node-variables node))
    (setf (node-function node)
          (init-plm vars
                    1
                    0 (init-vector (length vars) t)))
    (setf (aref (graph-changes cg) (node-number node)) t)
    )
  )
; -------------------------------------------------------------