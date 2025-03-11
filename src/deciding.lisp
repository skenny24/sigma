(in-package :sigma)
;-----------------------------------------------------------------
; Decision-making processes
; * creating and modifying WM values
; * perception
; * choosing
; * adding/removing states
;----------------------------------------------------------------
; Run a sequence of decisions, each with a max of max-messages or messages if specified
(defun decide (&optional cycles messages)
  (let (break ; Send messages but then break before first decision
        continue ; Continue after break, skipping initialization and message sending, and just doing decision
        return-value ; Result of decisions
        pre-time ; Time before measurement starts
        run-result ; Result of executing run
        )
    (setq have-run nil) ; Mark that the next run will be the initial one (for a new decision)
    ; Initialize goals at very beginning of run
    (unless (graph-goals-set cg)
      (dolist (goal (graph-goal-list cg))
        (apply #'evidence (list goal))
        )
      (setf (graph-goals-set cg) t)
      )
    (when (eq cycles 'd) ; Do decision after a (d 0)
      (setq cycles 1)
      (setq continue t)
      )
    (unless cycles (setq cycles max-decisions))
    (when (zerop cycles)
      (setq cycles 1)
      (setq break t)
      )
    (setq return-value
          (catch 'decide
            (dotimes (i cycles)
              (unless continue
                (setq decision-count (+ decision-count 1))
                (when trace-performance
                  (setq global-decision-statistics
                        (cons (make-decision-statistics :number decision-count :messages 0 :run-time 0 :decision-time 0 :init-time 0 :learn-time 0)
                              global-decision-statistics))
                  )
                (when trace-decisions (format trace-stream "~&~%<<< Decision ~S >>>~%" decision-count))
                ; Increment time
                (when (or track-time episodic-learning)
                  (evidence `((,temporal-predicate-name (value ,decision-count))))
                  )
                ; Handle perception
                (when perceive-list
                  (perceive-list) ; Execute perceive forms to update perceive functions in WMFNs
                  )
                (update-global-appraisal-variables)
                (update-perception-memories)
                (setq run-result (run (if messages messages max-messages) t)) ; Pass messages until quiescence (or messages or max-messages)
                (when track-graph-messages
                  (setq global-graph-messages (append global-graph-messages (list (graph-message-array))))
                  )
                (when (eq run-result interrupt-symbol) (return interrupt-symbol))
                (when (break-on-positive-messages)
                  (setq break t)
                  )
                )
              ; Make decisions and learn
              (cond (break
                     (format trace-stream "~&~%<<< Break Before Decision >>>~%")
                     (return)
                     )
                    (t
                     (when trace-preferences (print-preferences))
                     (when pre-d
                       (pre-d) ; Execute pre-decision forms
                       )
                     (when trace-performance (setq pre-time (get-internal-run-time)))
                     ; Intitialize variables for impasse detection
                     (cond (multiagent
                            (setq add-new-state (init-vector (graph-agents cg)))
                            (dotimes (i (length bottom-state))
                              (setf (aref delete-lower-states i) (1+ (aref bottom-state i)))
                              )
                            )
                           (t
                            (setq add-new-state nil)
                            (setq delete-lower-states (+ bottom-state 1))
                            )
                       )
                     ; Copy prediction perceptions to original predicates
                     (when diachronic-prediction (copy-diachronic-perception))
                     ; Actually make decisions and change working memory
                     (change-wm)
                     ; Add and delete states for impasses as appropriate
                     (when detect-impasses
                       (adjust-states)
                       ; Rewipe operators based on any changes made during state adjustment
                       (rewipe-operators-after-selection)
                       )
                     (when trace-performance
                       (setf (decision-statistics-decision-time (car global-decision-statistics))
                             (- (get-internal-run-time) pre-time))
                       )
                     (when trace-performance (setq pre-time (get-internal-run-time)))
                     ; Update conditional functions when doing gradient-descent learning
                     (when learn-via-gradient-descent
                       (update-functions-via-gradient)
                       )
                     (when trace-performance
                       (setf (decision-statistics-learn-time (car global-decision-statistics))
                             (- (get-internal-run-time) pre-time))
                       )
                     (compute-goal-progress) ; Compute goal progress
                     (when action-list
                       (action-list) ; Execute list of actions
                       )
                     (when post-d
                       (post-d) ; Execute post-decision forms
                       )
                     (when (haltp)
                       (return halt-symbol)
                       )
                     )
                    )
              )
            )
          )
    (when trace-performance (print-global-decision-statistics))
    return-value)
  )

(defun d (&optional cycles messages) (decide cycles messages))

; Rewipe operators based on any changes made during adjust-states
(defun rewipe-operators-after-selection nil
  (let* ((selected-pred (graph-selected-predicate cg))
         (selected-wm (predicate-wm selected-pred))
         )
    (when (and selected-pred (not (plm-full selected-operator-wipe)))
    ; Wipe out any operator for a state which has had a unique predicate change
    ; Do this again after selected predicate is processed in case operators reselected
      (when (and trace-wm-changes
                 (or (atom trace-wm-changes) (member 'selected trace-wm-changes))
                 )
        (format trace-stream "~&PLM to use for wiping out operators in changed SELECTED WM: ")
        (print-plm selected-operator-wipe t)
        )
      (setf (node-function selected-wm)
            (remove-unneeded-slices (combine-plms selected-operator-wipe nil (node-function selected-wm) 'product)))
      (when (and trace-wm-changes
                 (or (atom trace-wm-changes) (member 'selected trace-wm-changes))
                 )
        (format trace-stream "~&SELECTED WM after wiping out operators in changed SELECTED WM with adjusted states: ")
        (print-plm (node-function selected-wm) t)
        )
      )
    )
  )

; Copy perception function from *next predicate to original one
(defun copy-diachronic-perception nil
  (dolist (pred (graph-predicates cg))
    (when (and (predicate-perception pred)
               (predicate-predict pred)
               (not (plm-e= (node-function (predicate-perception pred))
                            (node-function (predicate-perception (predicate-predict pred)))))
               )
      (setf (node-function (predicate-perception pred)) (copy-a-plm (node-function (predicate-perception (predicate-predict pred)))))
      (when save-message-state
        (setf (aref (graph-changes cg) (node-number (predicate-perception pred))) t)
        )
      )
    )
  )

; Compute progress and difference on goals set in predicates based on the Bhattacharyya coefficient
; Also compute attention when there is also a surprise predicate
(defun compute-goal-progress nil
  (dolist (pred (graph-predicates cg))
    (let ((goal-pred (predicate-goal-predicate pred))
          (progress-pred (predicate-progress-predicate pred))
          (difference-pred (predicate-difference-predicate pred))
          (surprise-pred (predicate-surprise-predicate pred))
          (attention-pred (predicate-attention-predicate pred))
          pred-function ; Function used for predicate in computing progress
          goal-function ; Predicates goal function
          divisor-function ; Used to scale progress and difference by total mass of goal (so total is fraction of complete goal achievement)
          constraint-function ; Used in determing which portions of difference to ignore depending on parts of goals don't care about (i.e., parts that are 0)
          progress-function difference-function attention-function goal-based-function
          bc-plm ; BC coefficient
          hd-plm ; HD coefficient
          unique-ds ; Unique dimensions of predicate
          nvs
          )
      (when progress-pred
        (setq unique-ds (unique-dimensions-predicate pred)) 
        (setq pred-function (if (open-world pred) (vn-posterior pred) (node-function (predicate-wm pred))))
        (setq goal-function (node-function (predicate-wm goal-pred)))
        (when trace-attention
          (when goal-function
            (format trace-stream "~&~%~S Goal Function:~&" (predicate-name pred))
            (print-smart goal-function t trace-stream)
            )
          (when pred-function
            (format trace-stream "~&~%~S Working Memory Function:~&"(predicate-name pred))
            (print-smart pred-function t trace-stream)
            )
          )
        (setq divisor-function (summarize-plm goal-function (all-but-agent-and-state-dimensions-plm goal-pred) 'integral #'vector-sum))
        ; Compute similarity/progress
        (setq bc-plm (bc-plms pred-function goal-function unique-ds))     
        ; Compute difference
        (setq hd-plm (hd-from-bc-plm bc-plm))
        (when trace-attention
          (when bc-plm
            (format trace-stream "~&~%~S Progress (Bhattacharyya Coefficient):~&" (predicate-name pred))
            (print-smart bc-plm t trace-stream)
            )
          (when hd-plm
            (format trace-stream "~&~%~S Difference (Hellinger Distance):~&" (predicate-name pred))
            (print-smart hd-plm t trace-stream)
            )
          )
         ; Constrain differences to 0 when no goal in region
        (setq constraint-function (integral-plm goal-function unique-ds))
        (setq hd-plm (combine-plms hd-plm nil constraint-function 'constrain-by-0))
        (when trace-attention
          (when constraint-function
            (format trace-stream "~&~%~S Constraint function that enables ignoring 0s in the Goal:~&" (predicate-name pred))
            (print-smart constraint-function t trace-stream)
            )
          (when hd-plm
            (format trace-stream "~&~%~S Asymmetric difference ignoring 0s in the Goal:~&" (predicate-name pred))
            (print-smart hd-plm t trace-stream)
            )
          (when divisor-function
            (format trace-stream "~&~%~S Goal mass (for scaling progress and difference):~&" (predicate-name pred))
            (print-smart divisor-function t trace-stream)
            )
          )
        ; Set up progress function
        (setq nvs (node-variables (predicate-perception progress-pred)))
        (setq progress-function
              (strip-and-reorder-plm (build-smap nvs (plm-variables bc-plm))
                                     nvs
                                     (combine-plms bc-plm ; Could be cheaper
                                                   nil
                                                   divisor-function
                                                   'divide-0)))
        (setq progress-function (remove-unneeded-slices progress-function))
        (when trace-attention
          (when progress-function
            (format trace-stream "~&~%~S Progress Map:~&" (predicate-name pred))
            (print-smart progress-function t trace-stream)
            )
          )
        (setf (predicate-perception-temp-mem progress-pred) progress-function) ; Local progress function
        ; Set up difference function
        (setq nvs (node-variables (predicate-perception difference-pred)))
        (setq difference-function
              (strip-and-reorder-plm (build-smap nvs (plm-variables hd-plm))
                                     nvs
                                     (combine-plms hd-plm ; Could be cheaper
                                                   nil
                                                   divisor-function
                                                   'divide-0)))
        (setq difference-function (remove-unneeded-slices difference-function))
        (when trace-attention
          (when difference-function
            (format trace-stream "~&~%~S Difference Map:~&" (predicate-name pred))
            (print-smart difference-function t trace-stream)
            )
          )
        (setf (predicate-perception-temp-mem difference-pred) difference-function) ; Local difference function
        )
      ; Compute attention
      (when attention-pred
        ; Use normalized difference or progress function, if exists, depending on whether predicate is closed or open world
        (when (and (closed-world pred) difference-function)
          (setq goal-based-function difference-function)
          (when (and (predicate-arguments difference-pred) (not (predicate-no-normalize difference-pred)))
            (setq goal-based-function (normalize-plm goal-based-function))
            )
          (when trace-attention
            (when goal-based-function
              (format trace-stream "~&~%~S Difference Map (Normalized):~&" (predicate-name pred))
              (print-smart goal-based-function t trace-stream)
              )
            )
          )
        (when (and (open-world pred) progress-function)
          (setq goal-based-function progress-function)
          (when (and (predicate-arguments progress-pred) (not (predicate-no-normalize progress-pred)))
            (setq goal-based-function (normalize-plm goal-based-function))
            )
          (when trace-attention
            (when goal-based-function
              (format trace-stream "~&~%~S Progress Map (Normalized):~&" (predicate-name pred))
              (print-smart goal-based-function t trace-stream)
              )
            )
          )
        (if surprise-pred
            (let ((surprise-function (predicate-perception-temp-mem surprise-pred)))
              (setq surprise-function (normalize-plm surprise-function))
              (when trace-attention
                (when surprise-function
                  (format trace-stream "~&~%~S Surprise Map (Hellinger distance) (Normalized):~&" (predicate-name pred))
                  (print-smart surprise-function t trace-stream)
                  )
                )
              (setq attention-function (if goal-based-function
                                           (combine-plms surprise-function nil goal-based-function 'por)
                                         surprise-function))
              )
          (setq attention-function goal-based-function))
        (setq attention-function (remove-unneeded-slices attention-function))
        (when trace-attention
          (when attention-function
            (format trace-stream "~&~%~S Attention Map ~S:~&"
                    (predicate-name pred)
                    (if surprise-pred
                        (if progress-pred
                            (if (closed-world pred)
                                "Surprise POR Difference"
                              "Surprise POR Progress")
                          "Surprise")
                      (if (closed-world pred)
                          "Difference"
                        "Progress")))
            (print-smart attention-function t trace-stream)
            )
          )
        (setf (predicate-perception-temp-mem attention-pred) attention-function) ; Local attention function
        )
      )
    )
  )

; Run some number of trials
(defun trials (&optional trials continue)
  (let (decide-result)
    (unless continue ; Start anew
      (setq trial-count 0)
      ; Execute forms to be executed before all trials
      (when pre-ts (pre-ts))
      )
    (unless trials (setq trials 1))
    (catch 'interrupt-trials
      (dotimes (ti trials)
        (setq trial-count (+ trial-count 1))
        (when trace-trials
          (format trace-stream "~&~%>>> Trial ~S <<<~%" trial-count)
          )
        (when (get-state-predicate)
          (evidence '((state (state 0))))
          )
        ; Update temperature
        (setq temperature (funcall temperature-schedule))
        (setq temperature (max temperature temperature-minimum)) ; Avoid overflows when exponentiate the inverse
        (setq one-over-temperature (/ 1 temperature))
        ; Execute pre-trial forms
        (when pre-t (pre-t))
        ; Run until explicit halt (or hit max-decisions)
        (setq decide-result (d))
        (when (eq decide-result interrupt-symbol)
          (throw 'interrupt-trials interrupt-symbol)
          )
        (empty-wms)
        ; Execute post-trial forms
        (when post-t (post-t))
        )
      )
    )
  )
(defun ts (&optional trials) (trials trials))
; Get the predicate STATE if it is defined

(defun get-state-predicate nil
  (find 'state (graph-predicates cg) :key #'predicate-name)
  )


; -----------------------------------------------------------
; Create and modify working memory
; Perception


; Create a working memory factor node (WMFN) for a predicate
(defun init-wm (pred &optional constant)
  (let* (wm-node
         per-node
         (arguments (predicate-arguments pred))
         (rank (length arguments))
         (vars (init-vector rank))
         element type
         unique ; Variable is unique
         unique-args ; Predicate's unique argument(s)
         )
    ; Create the variables for the WMFN
    (dotimes (i rank)
      (setq unique nil)
      (setq element (elt arguments i))
      (setq type (type-from-name (argument-type-name element)))
      (setq unique-args (predicate-unique pred))
      (setq unique (if (symbolp unique-args)
                       (if (eq (argument-name element) unique-args) t nil)
                     (if (member (argument-name element) unique-args) t nil)))
      (setf (aref vars i)
            (make-svariable :name (convert-to-wm-variable-name (argument-name element))
                           :type type
                           :unique unique
                           :select (when unique (predicate-select pred))
                           ))
      )
    (setf (predicate-wm-variables pred) vars)
    ; Create the factor node
    (when (or open-world-wmfns
              (closed-world pred)
              )
      (setq wm-node
            (init-factor-node
             (concat-symbols (list (predicate-name pred) 'wm-fn) t)
             'wm
             vars
             nil ; Start off with no associated variable nodes
             t nil t nil nil nil)
            )
      (setf (node-assumption wm-node) t)
      (setf (node-function wm-node)
            (init-plm vars
                      (if constant constant (if (closed-world pred) 0 1))
                      0 (init-vector (length vars) t)))
      (when (state-predicate pred)
        (if multiagent
            (dotimes (ai (graph-agents cg))
              (delete-lower-states-node pred wm-node 1 ai)
              )
          (delete-lower-states-node pred wm-node 1))
        )
      (setf (predicate-wm pred) wm-node)
      (setf (node-predicate wm-node) pred)
      )
    ; Create a perception factor node if this is a perception predicate
    (when (predicate-perception pred)
      (setq per-node
            (init-factor-node
             (concat-symbols (list (predicate-name pred) 'per-fn) t)
             'perception
             vars
             nil ; Start off with no associated variable nodes
             t nil t nil nil nil)
            )
      (setf (node-assumption per-node) t)
      (setf (node-function per-node)
            (init-plm vars
                      (if constant constant (if (closed-world pred) 0 1))
                      0 (init-vector (length vars) t)))
      (when (state-predicate pred)
        (if multiagent
            (dotimes (ai (graph-agents cg))
              (delete-lower-states-node pred per-node 1 ai)
              )
          (delete-lower-states-node pred per-node 1))
        )
      (setf (predicate-perception pred) per-node)
      (setf (node-predicate per-node) pred)
      )
    wm-node)
  )

; Make a unique change, including zeroing out everything competing with the change when appropriate
(defun update-wm (r pred &optional blank-r)
  (let* ((pfn (predicate-wm pred)) ; WMFN
         (pf (node-function pfn)) ; WM function
         vs ; WM variables
         si ; Index of state in WM
         sd ; State dimension in new region
         ai ; Index of agent dimension (if any)
         ad ; Agent dimension (if any)
         ms ; State location below which should delete everything
         rcp ; Whether regions change PLM
         )
    (when (and (not open-world-wmfns) (open-world pred))
      (error "There is no WMFN for predicate ~S in UPDATE-WM." (predicate-name pred))
      )
    (setq rcp (update-function r pfn (predicate-name pred) blank-r))
    (when (and rcp (closed-world pred)) ; Changing a closed world predicate
      (setq vs (plm-variables pf)) ; Predicate WM variables
      (setq si (stype-index 'state vs)) ; State index
      (when si ; There is a state argument
        (setq sd (aref (region-dimensions r) si)) ; The state dimension in the region
        (when multiagent
          (setq ai (predicate-agent-arg-index pred)) ; Agent index
          (setq ad (aref (region-dimensions r) ai)) ; The agent dimension in the region
          )
        (setq ms (+ (max (if center-discrete-numeric-on-integer (+ (dimension-min sd) 1/2) (dimension-min sd)) base-level-state) 1)) ; Delete all below this maximum
        ; Possibly modify maximum based on delete-lower-states
        (if multiagent
            (do ((i (if center-discrete-numeric-on-integer (+ (dimension-min ad) 1/2) (dimension-min ad)) (+ i 1))) ; Cycle through all agents in region
                ((e>= i (dimension-max ad) t) nil)
              (when (<= ms (aref bottom-state i))
                (setf (aref delete-lower-states i) (min ms (aref delete-lower-states i)))
                )
              )
          (when (<= ms bottom-state) ; Single agent
            (setq delete-lower-states (min ms delete-lower-states)) ; Mark to delete all states lower than this and previous highest
            )
          )
        )
      )
    )
  )

; Empty a working memory
(defun empty-wm (pred)
  (empty-node-function (predicate-wm pred) (if (open-world pred) 1 0))
  (when (and save-message-state debug-descendants)
    (format trace-stream "~&Changing ~S WM" (predicate-name pred))
    )
  )

; Empty all working memories
(defun empty-wms ()
  (dolist (pred (graph-predicates cg))
    (when (and (or open-world-wmfns (closed-world pred))
               (predicate-wm pred))
      (empty-wm pred)
      )
    )
  )

; Create a region from a wme for a predicate
(defun wme-region (wme weight pred &optional evidence)
  (let (dps r span etype)
    (dolist (element wme)
      (setq etype (type-from-predicate-argument (element-argument-name element) pred))
      (unless etype
        (error "When creating a WM region, element argument ~S is not defined in predicate ~S."
               (element-argument-name element) (predicate-name pred))
        )
      (setq span (compute-span (element-content element) etype t))
      (setq dps (cons (cons (element-wm-index element pred)
                            (list (make-slice :location (car span))
                                  (make-slice :location (cadr span)))) dps))
      )
    (setq r (make-discrete-region dps (predicate-wm-variables pred) nil weight 0))
    (when evidence
      (setf (region-evidence r) t)
      )
    r)
  )

; Given a set of arguments for a wme to be added, generate args for the pre-inverse to be added
; The multiple arguments stay the same, but the unique ones expand to full scope of variable/dimension
(defun inverse-args (args pred)
  (let (eaname ia type multiple)
    (dolist (arg args)
      (setq eaname (element-argument-name arg))
      (setq type (type-from-predicate-argument eaname pred))
      (setq multiple (multiple-from-predicate-argument eaname pred))
      (if multiple
          (setq ia (cons arg ia))
        (setq ia (cons (list eaname (if (and (stype-numeric type) (stype-discrete type) center-discrete-numeric-on-integer)
                                        (list (+ (stype-min type) 1/2) (+ (stype-max type) 1/2))
                                      (list (stype-min type) (stype-max type)))
                             )
                       ia
                       )))
      )
    (reverse ia))
  )

; Create an inverse region for wiping out non-selected values
(defun inverse-region (pred fn positive args)
  (let (unique-ds ; Unique dimensions of function
        ias ; Inverses of args
        )
    (setq unique-ds (unique-dimensions (node-function fn) t))
    (when unique-ds
      (setq ias (inverse-args args pred))
      )
    (if (and unique-ds (e= positive 1))
        (wme-region ias 0 pred)
      nil)
    )
  )

; Add a single wme to WM of a predicate
(defun add-wme (pred-name positive args)
  (let ((pred (predicate-from-name pred-name t)) ; Predicate from predicate name
        wmfn ; The WM factor node for the predicate
        )
    (unless pred
      (error "Attempt to add a working memory element for the non-existant predicate ~S!" pred-name)
      )
    (when (and (not open-world-wmfns) (open-world pred))
      (error "There is no WMFN for predicate ~S in ADD-WME" pred-name)
      )
    (setq wmfn (predicate-wm pred))
    ; Update the WM factor function
    (if positive  
        ; Set new value to default weight (1 usually)
        ; Set all values for non-multiple variables (that aren't no-normalize) with a value of 1 for args to 0 as default
        (update-wm (wme-region args positive pred latch-evidence) pred (unless (predicate-no-normalize pred) (inverse-region pred wmfn positive args)))
      ; If closed world, set new value to 0
      (update-wm (wme-region args 0 pred) pred)) ; Just set value to 0 for now, fix later
    ; Update link contents and add messages from WM factor to WM variable nodes to queue when don't already exist
    (when save-message-state
      (setf (aref (graph-changes cg) (node-number wmfn)) t)
      )
    )
  )

; Handle evidence (adding it to appropriate WMs)
(defun evidence (es &optional empty-wms)
  (unless (graph-initialized cg)
    (init-graph)
    )
  (when empty-wms (empty-wms)) ; Useful when starting a new trial with the same rules
  (dolist (e es)
    (when (and (not open-world-wmfns)
               (open-world (predicate-from-name (car e)))
               )
      (error "Evidence cannot be provided for open-world predicates such as ~S when OPEN-WORLD-WMFNS is nil" (car e))
      )
    (let ((mode (cadr e))
          positive arguments)
      (cond ((or (eq mode negation-symbol) (e= mode 0)) ; Negated evidence
             (setq arguments (cddr e))
             )
            ((or (numberp mode) ; Positive valued evidence
                 (and (listp mode) ; Linear function evidence
                      (numberp (car mode)) ; Check this is a linear function rather than an argument
                      )
                 )
              (setq positive mode)
              (setq arguments (cddr e))
              )
             (t ; Positive Boolean evidence
              (setq positive 1)
              (setq arguments (cdr e))
              )
             )
      ; check whether there are new constants introduced in the evidence
      (when extend-type-constants-by-evidence (check-for-new-constants arguments (car e))) 
      (add-wme (car e) positive arguments)
      )
    )
  )

;-----------------------------------------------------------------
; Perception

; Handle perception
(defun perceive (ps &optional empty-wms)
  (unless (graph-initialized cg)
    (init-graph)
    )
  (when empty-wms (empty-wms) (empty-pers)) ; Useful when starting a new trial with the same rules
  (dolist (p ps)
    (let ((pred (predicate-from-name (car p)))
          pvs ; WM Node variables
          pf ; The perceptual function for a predicate
          ir ; Inverse region
          (original-p p) ; Save for printout
          (value 1) ; Default constant for region specified by percept percept
          )
      (unless (or (predicate-perception pred)
                  (and (predicate-predict pred)
                       (predicate-perception (predicate-predict pred))
                       )
                  )
        (error "Attempt to perceive for predicate ~S, which is not a perception predicate" (car p))
        )
      (setq pf (predicate-perception-temp-mem pred))
      (setq pvs (node-variables (if (predicate-perception pred)
                                    (predicate-perception pred)
                                  (predicate-perception (predicate-predict pred)))))
      ; Extract value to assign to region if there is one
      (when (and (> (length p) 1)
                 (numberp (cadr p)))
        (setq value (cadr p))
        (setq p (cdr p)) ; Set up so that the cdr below does the right thing
        )
      ; Check whether new constants are introduced
      (when extend-type-constants-by-evidence (check-for-new-constants (cdr p) (car original-p))) 
      (unless pf ; Need to initialize a new percept function for this predicate
        (setq pf (setf (predicate-perception-temp-mem pred) (make-constant-discrete-plm (number-list (length pvs)) pvs (if (closed-world pred) 0 1) 0)))
        )
      (unless (predicate-no-normalize pred)
        (setq ir (inverse-region pred (if (predicate-perception pred)
                                    (predicate-perception pred)
                                  (predicate-perception (predicate-predict pred)))
                                 value (cdr p)))
        (when ir
          (setq pf (update-region ir pf)) ; Zero out appropriate inverse region
          )
        )
      (setf (predicate-perception-temp-mem pred) (update-region (wme-region (cdr p) value pred) pf)) ; Update perception function with percept
      (when trace-perception
        (format trace-stream "~&~%Perception: ~S" original-p)
        )
      )
    )
  )

; Update perception factor nodes
(defun update-perception-memories nil
  (let (perpred pernode)
    (dolist (pred (graph-predicates cg))
      (setq perpred (if (and diachronic-prediction (predicate-predict pred))
                        (predicate-predict pred)
                      pred))
      (when (and (predicate-perception perpred)
                 (predicate-perception-temp-mem pred)
                 (not (plm-e= (node-function (predicate-perception perpred)) (predicate-perception-temp-mem pred)))
                 )
        (setq pernode (predicate-perception perpred))
        (setf (node-function pernode) (predicate-perception-temp-mem pred))
        (when (state-predicate perpred)
          (if multiagent
              (dotimes (ai (graph-agents cg))
                (delete-lower-states-node perpred pernode (+ (aref bottom-state ai) 1) ai)
                )
            (delete-lower-states-node perpred pernode (+ bottom-state 1)))
          )
        (setf (predicate-perception-temp-mem pred) nil)
        (when trace-perception
          (format trace-stream "~&~%Updated ~S perceptual function: " (predicate-name perpred)) (pplm (node-function pernode))
          )
        (when save-message-state
          (setf (aref (graph-changes cg) (node-number pernode)) t)
          )
        )
      )
    )
  )

; Empty one perceptual memory
(defun empty-per (pred &optional zero)
  (empty-node-function (predicate-perception pred) (if (and (not zero) (open-world pred)) 1 0))
  (when (and save-message-state debug-descendants)
    (format trace-stream "~&Changing ~S PERCEPTION" (predicate-name pred))
    )
  )

; Empty all perceptual memories
(defun empty-pers ()
  (dolist (pred (graph-predicates cg))
    (when (predicate-perception pred)
      (empty-per pred)
      )
    )
  )


; Maximize over selection dimension in a PLM
; If according-to-plm provided, use it to find the index and whether best or expected, rather than the plm itself
; This is to deal with the fact that a selection predicate may receive a change message from a non-selection prediction WMVN
(defun maximize-selection-dimension (plm &optional omit-maximals according-to-plm)
  (let ((max-plm plm)
        (test-plm (if according-to-plm according-to-plm plm))
        selection-ds)
    (dotimes (i (plm-rank plm))
      (if (best-variable (aref (plm-variables test-plm) i))
          (progn
            (setq max-plm (maximize-plm max-plm i omit-maximals selection-ds))
            (setq selection-ds (cons i selection-ds))
            )
        (if (expected-variable (aref (plm-variables test-plm) i))
            (setq max-plm (expected-value-plm max-plm i (not omit-maximals)))))
      )
    max-plm)
  )

; Create a weight vector (of length n+1) with a constant zeroth element and all else 0
(defun constant-weight-vector (n c)
  (let ((v (init-vector (+ n 1) 0)))
    (setf (aref v 0) c)
    v)
  )

; Set the function in a region to a constant weight vector
(defun set-constant-region (r c)
  (assign-function (constant-weight-vector (region-rank r) c) r)
  )

; Compute difference of max and min specified in a list
(defun max-min (pair)
  (- (cadr pair) (car pair))
  )

; Compute the span of a list of maximals
; Each maximal is (min max) so subtract max from min and add
(defun sum-differences (pairs)
  (apply #'+ (mapcar #'max-min pairs))
  )

; Return the point across the maximal subregions of the dimension
; corresponding to a point in the maximal span
(defun point-in-maximal-region (point dmaxs)
  (let (result min max span)
    (dolist (dmax dmaxs) ; dmax is a maximal subregion along dimension (min max)
      (setq min (car dmax)) ; Minimum of maximal subregion
      (setq max (cadr dmax)) ; Maximum of maximal subregion
      (setq span (- max min)) ; Span is length of maximal subregion
      (when (< point span) ; If the point is less than the span
        (setq result (+ min point)) ; The result we want is offset from the min by point
        (return result)
        )
      (setq point (- point span)) ; Reduce point by span
      )
    (unless result
      (error "Didn't find maximal point ~S in maximals ~S." point dmaxs)
      )
    result)
  )

; Determine if there is more than one maximal element
(defun multiple-maximals (mspan v)
  (if (stype-discrete (svariable-type v)) ; Discrete numeric or symbolic variable
      (> mspan 1.1) ; Give a bit of slop
    (> mspan epsilon21)) ; A bit of slop bigger than size of continous regions changed
  )

; Generate an impasse (returning a list of impasse regions)
; sd is the state dimension and ad is the agent dimension
; maximals is the list of operator maximals
; empty is true if the change message for the (selected) predicate is empty
(defun generate-impasse (sd ad maximals type empty)
  (let* ((ip (predicate-from-name 'impasse)) ; Impasse predicate
         (vs (plm-variables (node-function (predicate-wm ip)))) ; Impasse variables
         (ivs (initial-slice-vector vs))
         (si (stype-index 'state vs)); Index of state variable
         (oi (stype-index (graph-operator-type-name cg) vs)) ; Index of operator variable
         (ii (stype-index 'impasse vs)) ; Index of impasse variable
         (ai (predicate-agent-arg-index ip)) ; Index of agent variable
         maxrs ; Regions for maximals
         dps
         (impasse-type (type-from-name 'impasse))
         (sms (+ (if empty base-level-state (dimension-min sd)) 1)); State min slice + 1 (or 2 if change message is empty)
         (stype-index (position type (stype-constants impasse-type))) ; Index of impasse type
         )
    (if maximals
        (dolist (m maximals)
          (setq dps (list (list si ; State dimension bounds
                                (make-slice :location sms)
                                (make-slice :location (+ sms 1))
                                )
                          (list oi ; Operator dimension bounds
                                (make-slice :location (car m))
                                (make-slice :location (cadr m)))
                          (list ii ; Impasse dimension bounds
                                (make-slice :location stype-index)
                                (make-slice :location (+ stype-index 1)))
                          ))
          (when multiagent
            (setq dps (cons (list ai ; Agent dimension bounds
                                  (copy-slice (dimension-min-slice ad))
                                  (copy-slice (dimension-max-slice ad))
                                  )
                            dps)
                  )
            )
          (setq maxrs (cons (make-discrete-region dps vs ivs 1 0) maxrs))
          )
      (progn
        (setq dps (list (list si ; State dimension bounds
                              (make-slice :location sms)
                              (make-slice :location (+ sms 1))
                              )
                        (list ii ; Impasse dimension bounds
                              (make-slice :location stype-index)
                              (make-slice :location (+ stype-index 1)))
                        ))
        (when multiagent
          (setq dps (cons (list ai ; Agent dimension bounds
                                (copy-slice (dimension-min-slice ad))
                                (copy-slice (dimension-max-slice ad))
                                )
                          dps)
                )
          )
        (setq maxrs (cons (make-discrete-region dps vs ivs 1 0) maxrs))
        ))
    maxrs)
  )

; Are two lists of two elements e=?
(defun maximal-contained (x y)
  (and (e>= (car x) (car y) t)
       (e<= (cadr x) (cadr y) t)
       )
  )

; Is current selection one of the maximal changes?
; ms and pms are current and previous maximals for regioin
; us is the dimensions over which maximizing
; pr is 
(defun selection-still-maximal (ms pms us pr)
  (let ((found (not (region-e-empty pr))) ; Whether all current selections found in maximal changes
        msi pmsi)
    (when found
      (dolist (ui us)
        (setq msi (aref ms ui))
        (setq pmsi (aref pms ui))
        ; The only time there should be multiple maximals in the previously selected region is if it is a constant 0 (which has been ruled out above)
        (when (> (length pmsi) 1)
          (error "Not a single unique maximal for previous maximals along dimension ~S in region." ui)
          )
        (when (zerop (length pmsi))
          (setq found nil)
          ;(error "No maximal found for selection dimension ~S in selection-still-maximal" ui)
          )
        (unless (setq found (and found (member (car pmsi) msi :test #'maximal-contained)))
          (return found)
          )
        )
      )
    found)
  )

; Is there a NONE impasse for agent a at state s?
; Under assumption that the impasse function will never be huge, walk through all regions here
; rather than the earlier approach of finding the origin region for a and then s and then NONE
(defun impasse-none (a s)
  (let* ((ip (predicate-from-name 'impasse t))
         (impasse-fun (node-function (predicate-wm ip)))
         (ivs (plm-variables impasse-fun))
         (si (stype-index 'state ivs))
         (ai (predicate-agent-arg-index ip))
         (ii (stype-index 'impasse ivs))
         (iarray (plm-array impasse-fun))
         (n (position 'none (stype-constants (type-from-name 'impasse))))
         ir
         none
         )
    (dotimes (i (array-total-size iarray))
      (setq ir (row-major-aref iarray i))
      (when (and (or (not a) (and (e<= (region-min ir ai) a t) (e< a (region-max ir ai)))) ; Region is for agent a (or no agent)
                 (and (e<= (region-min ir si) s t) (e< s (region-max ir si))) ; Region is for state s
                 (and (e<= (region-min ir ii) n t) (e< n (region-max ir ii))) ; Region is for impasse type NONE
                 (e> (region-constant ir) 0) ; Region is non-empty
                 )
        (return (setq none t))
        )
      )
    none)
  )

; Does impasse match?
(defun impasse-matches-region (a ai s si n ii ir)
  (and (or (not ai) ; There is no agent
            (and (e<= (region-min ir ai) a t) (e> (region-max ir ai) a)) ; Agent within region
            )
       (e<= (region-min ir si) s t) (e> (region-max ir si) s) ; State within region
       (e<= (region-min ir ii) n t) (e> (region-max ir ii) n) ; Impasse type is within regions
       )
  )

; Is there a newly generated none impasse (in impasse-regions) for agent a at state s?
(defun new-impasse-none (a s)
  (let* ((ip (predicate-from-name 'impasse t))
         (ivs (node-variables (predicate-wm ip)))
         (si (stype-index 'state ivs))
         (ai (predicate-agent-arg-index ip))
         (ii (stype-index 'impasse ivs))
         (n (position 'none (stype-constants (type-from-name 'impasse))))
         none
         )
    (dolist (ir impasse-regions)
      (when (impasse-matches-region a ai s si n ii ir)
        (return (setq none t))
        )
      )
    none)
  )

; Create list of dimensions of selection variables in vector of variables
(defun selection-variable-list (vs)
  (let (svs)
    (dotimes (d (length vs))
      (when (selection-variable (aref vs d))
        (setq svs (cons d svs))
        )
      )
    svs)
  )

; Determine if should detect impasses at state (based on detect-impasses predicate)
(defun detect-impasses-state (smin &optional agent)
  (let* ((dip (graph-detect-impasses-predicate cg)) ; Detect-impasses predicate
         (vn (predicate-outgoing-vn dip)) ; Shared outgong vn for detect-impasses predicate if it exiss   
         )
    (if vn
        (eq (value-in-function-state dip (vn-posterior dip) smin 'value agent) 'true)
      t)
    )
  )

; Return a maximal subregion (either first or random) from a region
; Multiple variables keep whole extent
; Pick a maximal value along selection variables
; Assign a constant value of 1 to the region
; Detect impasses as appropriate
(defun maximal-subregion (predicate prev-max r)
  (let* ((pf (node-function (predicate-wm predicate))) ; Predicate WM node function
         (vs (predicate-wm-variables predicate)) ; Predicate WM variables
         (rank (length vs))
         (new-r (copy-region-full r)) ; Copy of original region
         (ds (region-dimensions new-r)) ; Dimensions of region
         (ms (region-maximals new-r)) ; Maximals for region
         (tm trace-maximals)
         (te trace-empty)
         (pname (predicate-name predicate)) ; Name of predicate
         (selected (eq pname 'selected))
         (us (selection-variable-list vs)) ; Positions of unique/selection variables
         (opi (when selected (car us))) ; When this is the selected predicate only one selection variable (operator)
         (pmr (region-point (region-mins-0 r) prev-max)) ; Region in prev-max plm containing min-point of change region r
         (pms (region-maximals pmr)) ; Maximals for region in prev-max
         (mspans (init-vector rank)) ; Total length of maximal span along selection dimensions
         (mvals (init-vector rank)) ; Chosen maximal values for selection dimensions
         (si (stype-index 'state vs)) ; Index of state dimension, if any
         sd ; State dimension, if any
         impasse ai ad an maximal-point
         spans-state ; Whether region spans full state dimension
         dls ; Value for delete-lower-states, whether multiagent or not
         double-none ; Whether generating a none impasse when there is already one at the level above
         )
    ; Compute total length of maximal spans for selection variables
    (dolist (i us)
      (setf (aref mspans i) (sum-differences (aref ms i)))
      )
    (when si (setq sd (aref ds si))) ; State dimension, if any
    (when (and multiagent selected detect-impasses)
      (setq ai (predicate-agent-arg-index predicate)) ; Index of agent dimension, if any
      (when ai
        (setq ad (aref ds ai)) ; Multiagent dimension, if any
        (setq an (region-min r ai)) ; Agent number for this region (assumes shattered so one agent per region)
        )
      )
    (when (and trace-wm-changes
               (or (atom trace-wm-changes) (member pname trace-wm-changes))
               (or (not sd)
                   (<= (dimension-max sd) ; State upper bound on region is less than or equal to
                       (+ (if an ; There is an agent dimension on this predicate
                              (aref bottom-state an)
                            (if (numberp bottom-state) ; There is only one agent
                                bottom-state
                              (reduce #'max bottom-state)))
                            1)) ; Upper bound on bottom-state state
                   )
               )
      (setq trace-maximals t)
      (setq trace-empty t)
      (format trace-stream "~&Computing maximal subregion for region (with selection variables maximized): ")
      (print-region r pf t trace-stream)
      (format trace-stream "~&Previous maximal subregion (with selection variables maximized): ")
      (print-region pmr pf t trace-stream)
      (setq trace-maximals tm)
      (setq trace-empty te)
      )
    (when sd ; There is a state dimension
      (setq spans-state (and (= (dimension-min sd) 0)
                             (= (dimension-max sd) max-state)
                             ))
      (setq dls (if an (aref delete-lower-states an) delete-lower-states))
      )
    ; Detect impasses
    (when (and detect-impasses ; Impasse detection is turned on
               selected ; Only impasse on the operator in selected (which does have a state dimension)
               (< (dimension-min sd) dls) ; When impasse detection not obviated by a higher change in the state hierarchy
               (detect-impasses-state (dimension-min sd))
               )
      (cond ((and (region-e-empty r) ; No maximal choice (maximal value is 0)
                  (region-e-empty pmr) ; Nothing currently selected
                  )
             (if (or (impasse-none (when ai (region-min r ai)) (region-min r si)) ; A none impasse above (keeps from generating an endless hierarchy of none impasses)
                     (new-impasse-none (when ai (region-min r ai)) (region-min r si)) ; A new none impasse above in impasse-regions
                     )
                 (setq double-none t)
               (progn
                 (setq impasse 'none)
                 (setf (aref ms opi) nil)
                 ))
             )
            ((and (not (region-e-empty pmr)) ; Operator is selected
                  (not (region-e-empty r)) ; There is an operator to select (ensuring they aren't all at 0)
                  (selection-still-maximal ms pms us pmr) ; Region shouldn't change (current selection still maximal)
                  )
             (setq impasse 'no-change) ; Then this is a no-change impasse
             (setf (aref ms opi) (aref (region-maximals pmr) opi))
             )
            ((and (multiple-maximals (aref mspans opi) (aref vs opi)) ; There is more than one maximal (none of which are previously selected)
                  (not (region-e-empty r)) ; There is an operator to select (ensuring they aren't all at 0)
                  (setq maximal-point (region-mins r))
                  (setf (aref maximal-point (+ opi 1)) (caar (aref ms opi)))
                  (e< (linear-value (extract-function r) maximal-point) 1) ; The maximal value is not "best"
                  )
             (setq impasse 'tie) ; Then this is a tie impasse
             )
            )
      )
    (cond (double-none
           nil) ; make no change
          (impasse
           (setq impasse-regions (append impasse-regions (generate-impasse sd ad (aref ms opi) impasse spans-state))) ; generate impasse regions
           (when trace-impasses
             (format trace-stream "~&>>> ~S impasse in ~S region " impasse pname)
             (if (eq impasse 'none)
                 (setq trace-maximals nil)
               (setq trace-maximals t))
             (setq trace-empty t)
             (print-region (if (eq impasse 'no-change) pmr r) pf symbolic-trace trace-stream)
             (setq trace-maximals tm)
             (setq trace-empty te)
             )
           (when (or (>= (dimension-max sd) ; State upper bound on region is equal to
                          (min (+ (if an (aref bottom-state an) bottom-state) 1) ; Upper bound on bottom state
                               (if an (aref delete-lower-states an) delete-lower-states))) ; State at which to start deleting    
                     )
             ; Set flag(s) concerning need for a new state
             (if multiagent
                 (setf (aref add-new-state an) t)
               (setq add-new-state t) ; Set flag true
               )
             )
           nil) ; Return nil if there is no change in value (an impasse)
          ((region-e-empty r) ; Signal that nothing is to be selected
           'empty
           )
          (t
           (cond ((selection-still-maximal ms pms us pmr) ; Region shouldn't change (current selection still maximal)
                  (setq new-r nil) ; Mark no change region
                  )
                 ((or (and selected (eq operator-best-select 'random))
                      (and (not selected) (eq non-operator-best-select 'random))
                      )
                  (dolist (ui us)
                    (setf (aref mvals ui) (point-in-maximal-region (random (aref mspans ui)) (aref ms ui))) ; Point in region corresponding to point in span
                    )
                  )
                 (t ; Select first among maximal subregions
                  (dolist (ui us)
                    (setf (aref mvals ui) (caar (aref ms ui)))
                    )
                  )
                 )
           (when new-r
             ; Restrict region along dimension to interval starting at random maximal point
             (dolist (ui us)
               (setf (dimension-min-slice (aref ds ui)) (make-slice :location (aref mvals ui)))
               (setf (dimension-max-slice (aref ds ui)) (make-slice :location (+ (aref mvals ui) (if (stype-discrete (svariable-type (aref vs ui))) 1 epsilon2))))
               )
             (set-constant-region new-r 1)
             (when (and trace-wm-changes
                        (or (atom trace-wm-changes) (member pname trace-wm-changes)))
               (format trace-stream "~&New maximal subregion: ")
               (setq trace-maximals nil)
               (setq trace-empty t)
               (print-region new-r pf t trace-stream)
               (setq trace-maximals tm)
               (setq trace-empty te)
               )
             )
           new-r) ; Return new region if there is a change (which is nil if existing is to be kept)
          )
    )
  )

; Ensure that a predicate WM with selection variables has only one value per combination of multiple variables
; First set entire region to 0 and then add one element with maximum value at 1
(defun change-wm-selection (predicate prev-max changes-max)
  (let* ((crarray (plm-array changes-max))
         changed-regions
         (pname (predicate-name predicate))
         point cr zero-region ds d v
         (rank (plm-rank changes-max))
         (vs (predicate-wm-variables predicate))
         (piecewise-constant (plm-piecewise-constant changes-max))
         )
    (dotimes (i (array-total-size crarray))
      (setq cr (row-major-aref crarray i))
      (when (or (eq pname 'selected) ; Always call for selected so can detect impasses
                (not (region-e-empty cr piecewise-constant)) ; Make changes if change region is non-empty
                )
        (setq point (maximal-subregion predicate prev-max cr)) ; Pick one maximal subregion from region
        (when point ; If select a point (no impasse)
          (setq zero-region (set-constant-region cr 0))
          (when (and trace-wm-changes
                     (or (atom trace-wm-changes) (member pname trace-wm-changes))
                     )
            (format trace-stream "~&Region zeroed before unique changes in predicate ~S: " pname)
            (setq ds (region-dimensions cr))
            (dotimes (i rank)
              (setq d (aref ds i))
              (setq v (aref vs i))
              (format trace-stream "~S: " (svariable-name v))
              (print-dimension (dimension-min d) (dimension-max d) v t)
              (format trace-stream "; ")
              )
            )
          (if (eq point 'empty)
              (update-wm zero-region predicate) ; Whole region should be zeroed
            (update-wm point predicate zero-region)) ; Set all of region r in pred-wm to 0 and then maximal point to 1
          (setq changed-regions (cons cr changed-regions))
          )
        )
      )
    changed-regions)
  )

; Create an empty region for selected-operator-wipe corresponding to state span of unique change region (of another predicate)
; uc is a unique region that has been changed
; uc-vars is the vector of variables for the changed region
(defun selected-region-from-unique-change (uc uc-vars uc-pred)
  (let ((selected-vars (plm-variables selected-operator-wipe)) ; Variables in selected predicate
        wipe-region ; Region of selected predicate to wipe
        wr-si ; State index in wipe-region
        uc-si ; State index in unique change
        wr-sd ; State dimension in wipe region
        wr-ai ; agent index in wipe-region
        uc-ai ; agent index in unique change
        wr-ad ; agent dimension in wipe region
        )
    (setq wipe-region (make-spanning-region selected-vars nil 0 0))
    ; Restrict state dimension to state(s) in unique change
    (setq wr-si (stype-index 'state selected-vars))
    (setq uc-si (stype-index 'state uc-vars))
    (setq wr-sd (aref (region-dimensions wipe-region) wr-si))
    (setf (dimension-min-slice wr-sd) (make-slice :location (region-min uc uc-si)))
    (setf (dimension-max-slice wr-sd) (make-slice :location (region-max uc uc-si)))
    ; When multiagent, restrict state dimension to agent(s) in unique change
    (when multiagent
      (setq wr-ai (predicate-agent-arg-index (predicate-from-name 'selected)))
      (setq uc-ai (predicate-agent-arg-index uc-pred))
      (setq wr-ad (aref (region-dimensions wipe-region) wr-ai))
      (setf (dimension-min-slice wr-ad) (make-slice :location (region-min uc uc-ai)))
      (setf (dimension-max-slice wr-ad) (make-slice :location (region-max uc uc-ai)))
      )
    wipe-region)
  )

; Update WM based on actions
(defun change-predicate-wm (predicate)
  (let* (prev-max ; Previous WM with unique variables maxed out
         changes-max ; Changes with unique variables maxed out
         (pred-wm (predicate-wm predicate)) ; Predicate WMFN node
         (wm-plm (node-function pred-wm)) ; WM function
         (wm-vars (predicate-wm-variables predicate)) ; Variables in WM function
         (changes (wm-change-message predicate)) ; Change PLM
         (pred-name (predicate-name predicate))
         (selected (eq pred-name 'selected))
         unique-changes ; Unique regions changed
         sv ; Selection variable
         changed-negative ; Whether actually make any negative changes
         ai ; Agent variable index
         (tm trace-maximals)
         )
    (when (and trace-wm-changes
               (or (atom trace-wm-changes) (member pred-name trace-wm-changes))
               )
      (format trace-stream "~&~%")
      )
    ; Shatter PLMs on agent dimension if this is the selected predicate and are multiple agents and we're detecting impasses
    (when (and selected multiagent detect-impasses)
      (setq ai (predicate-agent-arg-index predicate))
      (setq wm-plm (shatter-plm wm-plm ai))
      (setf (node-function pred-wm) wm-plm) ; Put shattered plm back into WMFN so maximal-regions can find it
      (when changes
        (setq changes (shatter-plm changes ai))
        )
      )
    (when (and selected (not (plm-full selected-operator-wipe)))
      ; Wipe out any operator for a state which has a unique predicate change
      ; Need to do this after all predicates other than selected are processed but
      ; before selected is processed to avoid generating impasses for wiped operators
      ; Assumes selected is the last predicate processed
      (when (and trace-wm-changes
                 (or (atom trace-wm-changes) (member pred-name trace-wm-changes))
                 )
        (format trace-stream "~&SELECTED WM before wiping out operators: ")
        (print-plm wm-plm t)
        (format trace-stream "~&PLM to use for wiping out operators in SELECTED upon unique state changes: ")
        (print-plm selected-operator-wipe t)
        )
      ; Wipe out operators based on unique state changes
      (setq wm-plm
            (remove-unneeded-slices (combine-plms selected-operator-wipe nil wm-plm 'product)))
      (when (and (and trace-wm-changes (or (symbolp trace-wm-changes) (member 'selected trace-wm-changes)))
                 (or (atom trace-wm-changes) (member pred-name trace-wm-changes))
                 )
        (format trace-stream "~&SELECTED WM after wiping out operators (but before changing SELECTED WM): ")
        (print-plm wm-plm t)
        )
      )
    (when changes ; There is a change message
      (when (and trace-wm-changes
                 (or (atom trace-wm-changes) (member pred-name trace-wm-changes))
                 )
        (format trace-stream "~&~S WM before updating via actions: " pred-name)
        (print-plm wm-plm symbolic-trace)
        (format trace-stream "~&~%Change message: ")
        (print-plm changes symbolic-trace)
        (format trace-stream "~&")
        )
      (setq sv (selection-variables wm-plm))
      (cond (sv ; There are selection variables in the predicate
             (when (member (predicate-select predicate) '(prob-match boltzmann))
               (if (> (length sv) 1)
                   (error "Probability matching and Boltmann selection only defined for a single selection variable, not the multiple in predicate ~S" pred-name)
                 (setq sv (car sv)))
               )
             (setq prev-max (maximize-selection-dimension wm-plm)) ; Maximize over selection variables in WM 
             (setq changes-max
                   (case (predicate-select predicate)
                     ((prob-match) (prob-match-plm changes sv))
                     ((boltzmann) (prob-match-plm changes sv #'boltzmann-function))
                     (t (maximize-selection-dimension changes nil wm-plm))
                     ))
             (when (and trace-wm-changes
                        (or (atom trace-wm-changes) (member pred-name trace-wm-changes))
                        )
               (format trace-stream "~&Change message with unique variables maximized: ")
               (setq trace-maximals t)
               (print-plm changes-max t)
               (setq trace-maximals tm)
               (format trace-stream "~&")
               )
             (multiple-value-setq (prev-max changes-max) (share-slices prev-max changes-max))
             ; Add best positive value (retaining existing value if still maximal)
             (setq unique-changes (change-wm-selection predicate prev-max changes-max))
             (when (or unique-changes changed-negative)
               (setq wm-plm (remove-unneeded-slices wm-plm))
               )
             ; When unique changes to state, and there are operators, set appropriate part of selected-operator-wipe to 0
             (when (graph-operator-type-name cg) ; there is an operator type
               (setq wm-vars (plm-variables wm-plm))
               (when (and (not selected) ; Not relevant for changes in the selected predicate
                          unique-changes ; Only do when there are unique changes
                          (stype-index 'state wm-vars) ; Only do when the predicate includes the state
                          )
                 (dolist (uc unique-changes)
                   (setq selected-operator-wipe (update-region (selected-region-from-unique-change uc wm-vars predicate) selected-operator-wipe pred-name))
                   )
                 )
               )
             )
            (t (setf (node-function pred-wm) changes))
            )
      ; When some change has been made to WM function for predicate
      (when save-message-state
        (setf (aref (graph-changes cg) (node-number (predicate-wm predicate))) t)
        (when debug-descendants
          (format trace-stream "~&Changing ~S WM" (predicate-name predicate))
          )
        )
      (when (and trace-wm-changes
                 (or (atom trace-wm-changes) (member pred-name trace-wm-changes))
                 )
        (cond ((plm-empty (node-function (predicate-wm predicate)))
               (format trace-stream "~&~S WM Empty After Changes~&" pred-name)
               )
              (t
               (format trace-stream "~&~S WM After Changes: " pred-name)
               (print-plm (node-function (predicate-wm predicate)) t)
               (format trace-stream "~&")
               )
              )
        )
      )
    )
  )

; Update WM based on actions
(defun change-wm ()
  (let ((selected-pred (graph-selected-predicate cg))
        selected-wm
        selected-vars ; Variables in selected predicate
        )
    ; If selection predicate defined, initialize function used to wipe out operators for states that have been changed via unique predicates
    (when selected-pred
      (setq selected-wm (predicate-wm selected-pred))
      (setq selected-vars (predicate-wm-variables selected-pred)) ; Variables in selected predicate
      (setq selected-operator-wipe (make-constant-discrete-plm (number-list (length selected-vars)) selected-vars 1 0))
      (setf (plm-piecewise-constant selected-operator-wipe) t)
      )
    (dolist (predicate (graph-predicates cg))
      (when (or open-world-wmfns (closed-world predicate))
        (change-predicate-wm predicate) ; Make changes to standard predicate
        ; When this is the selection predicate and there are some state changes, wipe corresponding operators
        ; This wipes out operators selected on this cycle (and before episodic memory is changed for the selected predicate)
        (when (and (eq (predicate-name predicate) 'selected) (not (plm-full selected-operator-wipe)))
          ; Wipe out any operator for a state which has had a unique predicate change
          ; Do this again after selected predicate is processed in case operators reselected
          (setf (node-function selected-wm)
                (remove-unneeded-slices (combine-plms selected-operator-wipe nil (node-function selected-wm) 'product)))
          (when (and trace-wm-changes
                     (or (atom trace-wm-changes) (member 'selected trace-wm-changes))
                     )
            (format trace-stream "~&SELECTED WM after wiping out operators (and changing SELECTED WM): ")
            (print-plm (node-function selected-wm) t)
            )
          )
        )
      )
    )
  )

; -----------------------------------------------------------
; Compute probability match (including Boltzmann) choice across one dimension of a PLM

; Return a region for a discrete point
; Handle case where we're centering on integers
(defun discrete-point (x)
  (let ((imin (floor x)))
    (list imin (+ imin 1)))
  )

; Compute probability matching choice over dimensions of a PLM, yielding a new PLM
; Given an optional transform to use before choice
; Doesn't actually eliminate the dimensions from the PLM, but makes them inactive
; Meaning is ambiguous for multiple dimensions, so works for one
(defun prob-match-plm (p d &optional transform)
  (let* ((rank (plm-rank p))
         rarray sizev sizev-1 size np rarrayn sizeln sizen
         integrals; Cumulative integrals by summary region
         rands ; Random numbers by summary region
         rintegral ; Integral of region
         discrete ; Whether dimension d is discrete
         r ; Region
         temp-sum ii ii0
        bp bpoint brand
        destructive-normalize
        )
    (setq discrete (stype-discrete (svariable-type (aref (plm-variables p) d))))
    (when trace-summarize
      (format trace-stream "~&~%PROBABILITY MATCHING")
      (format trace-stream "~&~%P: ") (print-plm p symbolic-trace trace-stream)
      (format trace-stream "~&~%DIMENSION: ~S" d)
      )
    (setq bp p)
    ; Convert incoming PLM to a normalized Boltzmann form (where functions are divided by temperature and exponentiated)
    (when (eq transform #'boltzmann-function)
      (setq bp (transform-plm transform p))
      (when trace-summarize
        (format trace-stream "~&~%BOLTZMANN TRANSFORM: ") (print-plm bp symbolic-trace trace-stream)
        )
      (when trace-summarize
        (format trace-stream "~&~%NORMALIZED BOLTZMANN TRANSFORM: ") (print-plm bp symbolic-trace trace-stream)
        )
      (setq destructive-normalize t)
      )
    (setq bp (normalize-plm bp d destructive-normalize))
    ; Initialize what would normally be done in LET, but needed to wait until bp was defined to be safe
    (setq rarray (plm-array bp))
    (setq sizev (dimension-sizes-v bp))
    (setq sizev-1 (vector-1 sizev))
    (setq size (array-total-size rarray))
    (setq np (init-summarize-plm bp d))
    (setq rarrayn (plm-array np))
    (setq sizeln (dimension-sizes np))
    (setq sizen (array-total-size rarrayn))
    (setq integrals (make-array sizeln :initial-element 0))
    (setq rands (make-array sizeln))
    ; Set random numbers by summary regions
    (dotimes (i sizen)
      (setf (row-major-aref rands i) (random 1.0))
      )
    ; Traverse regions of original PLM, determining probability matching points
    (setq ii (make-list rank :initial-element 0))
    (dotimes (i size)
      (setf r (row-major-aref rarray i))
      (setq rintegral (aref (integral-region r (list d)) 0)) ; Constant in region's integration along d
      ; Zero index along d
      (setq ii0 (copy-seq ii))
      (setf (nth d ii0) 0)
      (when (and (not (aref (region-maximals (apply #'aref rarrayn ii0)) d)) (> rintegral 0)) ; Haven't already found point for region and there is a positive integral over the region
        (setq temp-sum (+ rintegral (apply #'aref integrals ii0))) ; Add integral to running total
        (setq brand (apply #'aref rands ii0))
        (when (< brand temp-sum) ; Random number is less than cumulative integral after region
          (setq bpoint (+ (region-min r d) (* (/ (- brand (apply #'aref integrals ii0)) rintegral) (region-span r d))))
          (setf (aref (region-maximals (apply #'aref rarrayn ii0)) d) (list (if discrete (discrete-point bpoint) (continuous-point bpoint))))
          )
        (setf (apply #'aref integrals ii0) temp-sum)
        )
      (setq ii (next-index-list ii sizev-1 rank))
      )
    ; Copy integrals to region-constants of summary regions (exact values don't matter, but zero versus positive does)
    (dotimes (i sizen)
      (setf (region-constant (row-major-aref rarrayn i)) (row-major-aref integrals i))
      )
    ; Set selection dimension inactive
    (setf (aref (plm-active np) d) nil)
    (when (plm-piecewise-constant p)
      (setf (plm-piecewise-constant np) t)
      )
    np)
  )

;----------
; Determine if region would change any values in PLM if used to update it
(defun region-changes-plm (ur p)
  (let (differ
        (rarray (plm-array p))
        r
        )
    (dotimes (i (array-total-size rarray))
      (setq r (row-major-aref rarray i))
      (when (and (regions-overlap ur r)
                 (not (vector-e= (extract-function ur) (extract-function r)))
                 )
        (return (setq differ t))
        )
      )
    differ)
  )

; Determine whether any of the regions in the list alter the function
(defun regions-change-plm (urs p)
  (let (differ
        )
    (dolist (ur urs)
      (when (region-changes-plm ur p)
        (return (setq differ t))
        )
      )
    differ)
  )

; Create a list of regions from two regions where the values correspond to the second only where not overlapped by the first
; The predicate should be unique and the first region should cover a subrange of the second along the unique dimensions (ds)
(defun overlay-region (r1 r2 ds)
  (let ((rs (list r1))
        tr)
    (when (numberp ds) (setq ds (list ds))) ; Make sure there is always a list here
    (dolist (d ds)
      ; See if need to add an r2-based region that spans the scope before r1 starts
      (unless (e= (region-min r1 d) (region-min r2 d) t)
        (setq tr (copy-region-full r2))
        (setf (dimension-max-slice (aref (region-dimensions tr) d))
              (make-slice :location (region-min r1 d)))
        (setq rs (cons tr rs))
        )
      ; See if need to add an r2-based region that spans the scope after r1 ends
      (unless (e= (region-max r1 d) (region-max r2 d) t)
        (setq tr (copy-region-full r2))
        (setf (dimension-min-slice (aref (region-dimensions tr) d))
              (make-slice :location (region-max r1 d)))
        (setq rs (cons tr rs))
        )
      )
    rs)
  )

; Make a unique change, including zeroing out everything competing with the change when appropriate
(defun update-function (r node pred-name &optional blank-r)
  (let* ((pf (node-function node)) ; WM function
         (rcp (regions-change-plm (if blank-r (overlay-region r blank-r (unique-dimensions pf)) (list r)) pf))
         )
    (when rcp
      (when blank-r
        (setf pf (update-region blank-r pf pred-name))
        )
      (setf pf (update-region r pf pred-name))
      )
    (setf (node-function node) pf)
    rcp)
  )

; Shatter a PLM along a discrete dimension, yielding regions for each domain element
(defun shatter-plm (p d)
  (let* ((ptype (svariable-type (aref (plm-variables p) d)))
         (new-slices (init-vector (plm-rank p)))
         )
    (unless (stype-discrete ptype)
      (format trace-stream "~&~%Attempt to shatter a continuous dimension ~S of PLM: " d)
      (pplm p)
      (error "")
      )
    (dotimes (i (stype-span ptype))
      (setf (aref new-slices d) (cons (make-slice :location (+ i 1)) (aref new-slices d)))
      )
    (setf new-slices (sort-slice-list-array new-slices))
    (setq p (apply-slices new-slices nil p))
    p)
  )

; Modify two PLMs so that they have the same slices
(defun share-slices (p1 p2)
  (let ((s1 (plm-slices p1))
        (s2 (plm-slices p2)))
    (values (apply-slices s2 nil p1) (apply-slices s1 nil p2))
    )
  )

; Create an empty PLM with all active variables
(defun empty-plm (vars)
  (let ((p (init-plm vars 0 0 (init-vector (length vars) t))))
    (setf (plm-piecewise-constant p) t)
    p)
  )

; Create a full PLM with all active variables
(defun full-plm (vars)
  (let ((p (init-plm vars 1 0 (init-vector (length vars) t))))
    (setf (plm-piecewise-constant p) t)
    p)
  )

; -----------------------------------------------------------
; Add/remove states

; Find index of type in variable sequence
(defun stype-index (type vs)
  (position type vs :key #'(lambda (v) (stype-name (svariable-type v))))
  )

; Find highest slice in a list
(defun highest-slice (slices)
  (let (highest)
    (setq highest (car slices))
    (setq slices (cdr slices))
    (dolist (s slices)
      (when (e> (slice-location s) (slice-location highest)) ; New highest slice
        (setq highest s)
        )
      )
    highest)
  )

; Find lowest slice in a list
(defun lowest-slice (slices)
  (let (lowest)
    (setq lowest (car slices))
    (if (and (slice-index lowest) (= (slice-index lowest) 0))
        lowest
      (progn
        (setq slices (cdr slices))
        (dolist (s slices)
          (when (e< (slice-location s) (slice-location lowest)) ; New highest slice
            (setq lowest s)
            )
          )
        lowest))
    )
  )

; Find second highest slice in a list (upper bound on latest state)
(defun second-highest-slice (slices)
  (let (highest second)
    (setq highest (car slices))
    (setq slices (cdr slices))
    (dolist (s slices)
      (cond ((e> (slice-location s) (slice-location highest)) ; New highest slice
             (setq second highest)
             (setq highest s)
             )
            ((or (not second) ; There isn't already a second highest slice
                 (e> (slice-location s) (slice-location second))) ; Slice is higher than current second highest
             (setq second s)
             )
            )
      )
    second)
  )

; Fill impasse state for open-world predicates
(defun fill-open-world-state (bottom-state &optional agent-index)
  (let (pwm ; Predicate WM FN node
        p ; Function in pwm
        r ; Full region to use in modifying p
        bs ; Bottom state to use
        )
    (if agent-index
        (setq bs (aref bottom-state agent-index))
      (setq bs bottom-state)
      )
    (dolist (sp (graph-state-predicates cg)) ; Process every open-world predicate that has the state as an argument
      (unless (or (member (predicate-name sp) '(impasse selected)) ; Skip these predicates (which are now closed world, but keep for future in case)
                  (closed-world sp) ; Predicate is closed world
                  )
        (setq pwm (predicate-wm sp)) ; Predicate WM FN node
        (setq p (node-function pwm)) ; PLM in predicate's WM
        (setq r (make-spanning-region (plm-variables p) nil 1 0)) ; Create a full region for entire PLM
        ; Create a new restricted dimension for state
        ; Need to create a new dimension rather than modify old because dimension is shared by PLM
        (setf (aref (region-dimensions r) (stype-index 'state (plm-variables p)))
              (make-dimension :min-slice (make-slice :location bs)
                              :max-slice (make-slice :location (+ bs 1))
                              :weight 0)
              )
      ; Create a new restricted dimension for agent if there is one
      ; Need to create a new dimension rather than modify old because dimension is shared by PLM
        (when agent-index
          (setf (aref (region-dimensions r) (predicate-agent-arg-index sp))
                (make-dimension :min-slice (make-slice :location agent-index)
                                :max-slice (make-slice :location (+ agent-index 1))
                                :weight 0)
                )
          )
        (setq p (update-region r p))
        (when save-message-state
          (setf (aref (graph-changes cg) (node-number pwm)) t)
          )
        )
      )
    )
  )

; Cloning state at one higher location in state dimension
(defun clone-state (bottom-state) ; &optional agent-index
  (let (slice ; Slice to be moved
        loc ; Location of the slice
        pwm ; Predicate WM FN node
        p ; Function in pwm
        sd ; State dimension in p
        )
    (dolist (sp (graph-state-predicates cg)) ; Process every predicate that has the state as an argument
      (unless (or (member (predicate-name sp) '(impasse selected)) ; Skip these predicates
                  (and (not open-world-wmfns) (open-world sp)) ; Skip open-world predicates when don't have WMFNs
                  )
        (setq pwm (predicate-wm sp)) ; Predicate WM FN node
        (setq p (node-function pwm)) ; PLM in predicate's WM
        (setq sd (stype-index 'state (plm-variables p))) ; State dimension in PLM
        (setq slice (second-highest-slice (aref (plm-slices p) sd))) ; Upper (smaller) slice for previous state
        (setq loc (slice-location slice))
        (when (e> loc 0) ; There is  a substate in the WM function
          (when (e= loc bottom-state t) ; When there is content in the next highest state
            (setf (slice-location slice) (+ loc 1)) ; Create new state and copy previous to new state
            (when save-message-state
              (setf (aref (graph-changes cg) (node-number pwm)) t)
              )
            )
          )
        )
      )
    )
  )
; Get name of agent (when symbolic) or number
(defun agent-name (agent-index)
  (let ((at (graph-agent-type cg)))
    (if (stype-constants at)
        (nth agent-index (stype-constants at))
      agent-index)
    )
  )

; Get agent number from name (or number)
(defun agent-number (agent-name)
  (constant-index agent-name (graph-agent-type cg))
  )

; Delete lower states in a PLM than snum
; sp is the predicate, and p is the PLM
(defun delete-lower-states-function (sp p snum &optional agent-index)
  (let (empty-r ; Empty region for deleting lower states
        ai ; agent-index
        agent-type-min) ; Minimum value for agent type
    (setq empty-r (make-spanning-region (plm-variables p) nil 0 0)) ; Create an empty region for entire PLM
    ; Create a new restricted dimension for state
    ; Need to create a new dimension rather than modify old because dimension is shared by PLM
    (setf (aref (region-dimensions empty-r) (stype-index 'state (plm-variables p)))
          (make-dimension :min-slice (make-slice :location (if center-discrete-numeric-on-integer (- snum 1/2) snum))
                          :max-slice (make-slice :location (stype-max (type-from-name 'state)))
                          :weight 0)
          )
    ; Create a new restricted dimension for agent if there is one
    ; Need to create a new dimension rather than modify old because dimension is shared by PLM
    (when agent-index
      (setq ai (predicate-agent-arg-index sp))
      (setq agent-type-min (stype-min (svariable-type (aref (plm-variables p) ai))))
      (setf (aref (region-dimensions empty-r) ai)
            (make-dimension :min-slice (make-slice :location (+ agent-type-min agent-index))
                            :max-slice (make-slice :location (+ agent-type-min agent-index 1))
                            :weight 0)
            )
      )
    (setq p (update-region empty-r p)) ; Delete all of the information in the lower states (merging with empty region at end)
    )
  )

; Delete all states at snum and below and a node
; sp is the predicate for the node
(defun delete-lower-states-node (sp node snum &optional agent-index)
  (setf (node-function node) (delete-lower-states-function sp (node-function node) snum agent-index))
  (when (and save-message-state (graph-changes cg)) ; Don't do this if the graph-changes vector has not yet been defined
    (setf (aref (graph-changes cg) (node-number node)) t)
    )
  )

; Delete all states at snum and below
(defun delete-lower-states (snum &optional agent-index)
  (let (bs ; Bottom state (for agent)
        pn ; Perception node
        )
    (if agent-index
        (setq bs (aref bottom-state agent-index))
      (setq bs bottom-state)
      )
    (when trace-states
      (if (= snum bs)
          (progn
            (format trace-stream "~&Removing state ~S" snum)
            (when multiagent
              (format trace-stream " for agent ~S" (agent-name agent-index))
              )
            )
        (when (< snum bs)
          (format trace-stream "~&Removing states ~S to ~S" snum bs)
          (when multiagent
            (format trace-stream " for agent ~S" (agent-name agent-index))
            )
          )
        )
      )
    (if agent-index
        (setf (aref bottom-state agent-index) (- snum 1))
      (setq bottom-state (- snum 1))
      )
    (dolist (sp (graph-state-predicates cg)) ; Process every predicate that has the state as an argument
      (unless (and (not open-world-wmfns) (open-world sp)) ; Skip if open world and not WMFN
        (delete-lower-states-node sp (predicate-wm sp) snum agent-index)
        )
      (setq pn (predicate-perception sp))
      (when pn
        (delete-lower-states-node sp pn snum agent-index)
        )
      )
    )
  )

; Add and delete states for impasses as appropriate
(defun adjust-states nil
  (let (impasse-predicate impasse-state-index impasse-agent-index impasse-state-dim bs blank-r impasse-type-index impasse-type-dim impasse-type impasse-fun impasse-vars)
    (cond (multiagent
           ; Delete lower states in hierarchy as appropriate
           (dotimes (ai (graph-agents cg))
             (when (aref delete-lower-states ai)
               (delete-lower-states (aref delete-lower-states ai) ai)
               )
             )
           ; Move the bottom state for each agent to which adding a new state
           (dotimes (i (length add-new-state))
             (when (aref add-new-state i)
               (setf (aref bottom-state i)
                     (+ (aref bottom-state i) 1))
               (when trace-states
                 (format trace-stream "~&Adding state ~S for agent ~S" (aref bottom-state i) (agent-name i))
                 )
               (when open-world-wmfns
                 (fill-open-world-state bottom-state i)
                 )
               (evidence `((state (state ,(aref bottom-state i)))))
               )
             )
           )
          (t
           ; Delete lower states in hierarchy as appropriate
           (when delete-lower-states
             (delete-lower-states delete-lower-states)
             )
           ; Move the bottom state
           (when add-new-state
             (setq bottom-state (+ bottom-state 1))
             (when trace-states
               (format trace-stream "~&Adding state ~S" bottom-state)
               )
             (when open-world-wmfns
               (if impasse-copy-state
                   (clone-state bottom-state)
                 (fill-open-world-state bottom-state))
               )
             (evidence `((state (state ,bottom-state))))
             )
           )
          )
    (setq impasse-predicate (predicate-from-name 'impasse t))
    (when impasse-regions
      (setq impasse-state-index (stype-index 'state (predicate-wm-variables impasse-predicate)))
      (when multiagent
        (setq impasse-agent-index (predicate-agent-arg-index impasse-predicate))
        )
      (when impasse-predicate
        ; When saving message state, mark that the impasse predicate has been changed
        (when save-message-state
          (setf (aref (graph-changes cg) (node-number (predicate-wm impasse-predicate))) t)
          )
        (when debug-descendants
          (format trace-stream "~&Changing ~S WM" (predicate-name impasse-predicate))
          )
        )
      )
    (setq impasse-fun (node-function (predicate-wm impasse-predicate)))
    ; Add impasse regions to WM
    (dolist (ir impasse-regions)
      (setq bs (if multiagent (aref bottom-state (region-min ir impasse-agent-index)) bottom-state))
      (when (e<= (region-min ir impasse-state-index) bs t) ; Only update if region overlaps with active states
        (when (and (e> (region-span ir impasse-state-index) 1) ; Region covers more than one state
                   (e> (region-max ir impasse-state-index) (+ bs 1)) ; State upper bound exceeds bottom state
                   )
          (setq impasse-state-dim (aref (region-dimensions ir) impasse-state-index))
          (setf (dimension-min-slice impasse-state-dim) (make-slice :location (+ bs 1))) ; Cut off region at upper bound of bottom state
          )
        (setq blank-r (empty-region (copy-region-full ir)))
        (setq impasse-type-index (stype-index 'impasse (node-variables (predicate-wm impasse-predicate))))
        (setq impasse-type-dim (aref (region-dimensions blank-r) impasse-type-index))
        (setq impasse-type (type-from-name 'impasse))
        (setf (dimension-min-slice impasse-type-dim) (make-slice :location (stype-min impasse-type)))
        (setf (dimension-max-slice impasse-type-dim) (make-slice :location (stype-max impasse-type)))
        ; Wipe operators in regions for which a new impasse has been generated for the state above
        (when (region-changes-plm ir impasse-fun)
          (setq impasse-vars (plm-variables impasse-fun))
          (setq selected-operator-wipe (update-region (selected-region-from-unique-change ir impasse-vars impasse-predicate) selected-operator-wipe 'impasse))
          )
        (update-wm ir impasse-predicate blank-r)
       )
      )
    (setq impasse-regions nil)
    )
  )


