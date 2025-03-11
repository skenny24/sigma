; Apply a summarization operation to a PLM (or a list of two PLMs)
(defun apply-summarization (pn s)
  (let (d op location ps)
    (setq op
          (case (car s)
            ((constant) #'extract-value-plm)
            ((expected) #'expected-value-plm)
            ((max argmax) #'maximize-plm)
            (t (error "Summarization operator ~S not one of CONSTANT, EXPECTED, MAX or ARGMAX in find-plm." (car s)))
            ))
    (when (eq (car s) 'constant)
      (if (cddr s)
          (setq location (caddr s))
        (error "No location provided for CONSTANT summarization in APPLY-SUMMARIZATION."))
;      (format trace-stream "~&~S: ~S~&" (cadr s) (caddr s))
      )
    (if (listp pn)
        (dolist (p pn)
          (setq d (position (cadr s) (plm-variables (cdr p)) :key #'svariable-name))
          (when d
            (setq ps (cons (apply op (apply-summarization-arguments (cdr p) d location)) ps))
            )
          )
      (progn
        (setq d (position (cadr s) (plm-variables pn) :key #'svariable-name))
        (unless d ; No position was found
          (error "Could not find dimension ~S to summarize over via ~A in ~S." (cadr s) op pn)
          )
        (setq pn (apply op (apply-summarization-arguments pn d location)))
        )
      )
    pn)
  )

; Return a region descriptor for a conditional function
(defun function-region (r p)
  (let (fun specs dims dim dt vars)
    (if (region-function-constantp r)
        (setq fun (region-constant r))
      (setq fun (coerce (extract-function r) 'list)))
    (setq dims (region-dimensions r))
    (setq vars (plm-variables p))
    (dotimes (d (region-rank r))
      (setq dim (aref dims d))
      (setq dt (svariable-type (aref vars d)))
      (if (dimension-full-scope (dimension-min dim) (dimension-max dim) dt)
          (push '* specs)
        (if (stype-constants dt)
            (if (= (region-span r d) 1)
                (push (get-symbol-name (dimension-min dim) dt) specs)
              (push (list (get-symbol-name (dimension-min dim) dt) (get-symbol-name (1- (dimension-max dim)) dt)) specs))
          (if (stype-discrete dt)
              (if (= (region-span r d) 1)
                  (push (if center-discrete-numeric-on-integer (+ (dimension-min dim) 1/2) (dimension-min dim)) specs)
                (push (list (if center-discrete-numeric-on-integer (+ (dimension-min dim) 1/2) (dimension-min dim))
                            (if center-discrete-numeric-on-integer (+ (dimension-max dim) 1/2) (dimension-max dim))) specs))
            (push (list (dimension-min dim) (dimension-max dim)) specs))))
      )
    (if (and (numberp fun) (= fun 0))
        nil
      (cons fun (reverse specs))))
  )

(defvar post-automatic nil)
; Execute post-automatic forms
(defun post-automatic nil
  (mapc #'eval post-automatic)
  t)

; Initialize a graph after it is defined
(defun init-graph nil
  (init-descendants)
  (when (or automatic-action-models automatic-perception-models automatic-reinforcement-learning)
    (create-models)
    (post-automatic)
    )
  (post-process-conditionals)
  (setf (graph-initialized cg) t)
  (init-link-counts)
  (when (get-state-predicate)
    (evidence '((state (state 0))))
    )
  )

; Initialize
(defun init (&optional operators agents center-discrete)
  (when reset-parameters-in-init
    (reset-parameters))
  (setq trial-count 0)
  (setq center-discrete-numeric-on-integer center-discrete)
  (setq cg (create-graph))
  (init-checks)
  (setq epsilon21 (* epsilon2 1.1))
  (setq relative-epsilon (* .1 epsilon2)) ; Initial value before defining types
  (new-type 'boolean :constants (list boolean-false boolean-true))
  (new-type 'flag :constants '(true))
  (new-type 'state :numeric t :discrete t :min 0 :max max-state)
  (when track-time
    (new-type temporal-predicate-name :numeric t :discrete t :min 0 :max max-time)
    (system-predicate temporal-predicate-name :world 'closed :arguments '((value time !)))
    )
  (cond (agents
         (let (agent-type)
           (setq multiagent t)
           (setq agent-type (if (consp agents) ; Take a list of agent names
                                (new-type 'agent :constants agents)
                              (if (and (integerp agents) (> agents 0)) ; Take a number of agents
                                  (new-type 'agent :numeric t :discrete t :min 0 :max (+ agents 1))
                                (error "Agents specification of ~S is neither a list or a positive integer." agents))))
           (setf (graph-agent-type cg) agent-type)
           (setf (graph-agents cg) (stype-max agent-type))
           (setq bottom-state (init-vector (graph-agents cg) base-level-state))
           (setq delete-lower-states (init-vector (graph-agents cg)))
           )
         )
        (t
         (setq multiagent nil)
         (setq bottom-state base-level-state)
         (setq delete-lower-states nil)
         )
        )
  (when operators
    (init-operators 'symbols operators) ; Defaults to detect-impasses nil
    )
  (system-predicate halt-predicate-name :world 'closed)
  (setf (graph-node-vector cg) nil)
  (setf (graph-goals-set cg) nil)
  (setq message-count 0)
  (setq decision-count 0)
  (setq cycle-message-counts nil)
  (setq impasse-regions nil)
  (setq perceive-list nil)
  (setq action-list nil)
  (setq pre-run nil)
  (setq post-run nil)
  (setq pre-d nil)
  (setq post-d nil)
  (setq pre-t nil)
  (setq post-t nil)
  (setq pre-ts nil)
  (setq post-automatic nil)
  (setq global-decision-statistics nil)
  (setq global-graph-messages nil)
  (setq have-run nil)
  (setq define-system-predicate nil)
  t)

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

; A list with n stars
(defun stars (n)
  (make-list n :initial-element '*)
  )

; Create reinforcement learning
(defun create-reinforcement-learning nil
  (let (unique-state-predicates ; All of the unique close-world state predicates
        regular-variables ; The variables from regular (closed-world unique state) predicates that define the context for the learned functions
        types ; Types of variables
        arguments ; Arguments names for variables
        prediction-variables ; The variables from prediction (*next) predicates that define the context for the learned functions
        regular-argument-list ; Arguments from regular predicates to use in created predicates
        predicate-name-list ; Name of predicate that goes with each argument
        prediction-argument-list ; Arguments from prediction predicates to use in created predicates
        condition-list ; List of conditions to be used in prediction
        variablization ; A list containing a pattern, a list of variables, and a list of types for a predicate
        (context-variable-count 0) ; The count of how many context variables there are
        pred ; Predicate to use in conditional
        operator-variable ; Variable name used in selected predicate
        regular-predicate-arguments ; Arguments from the regular (closed-wold unique state) predicates
        prediction-predicate-arguments ; Arguments from the prediction (*next) predicates
        psp
        projected*next-value-variable ; Variable used for the value in projected*next
        reward-value-variable ; Variable used for the value in reward
        q-pred ; Q predicate
        selected-pattern ; Pattern for selected predicate
        regular-condition-list ; List of conditions for regular (closed-world state selection) predicates
        implicit-q-condition ; Pattern for use of implicit Q values as a condition
        )
    ; Determine all of the closed world unique state predicates that aren't no-models
    (dolist (predicate (graph-predicates cg))
      (when (and (closed-world predicate)
                 (not (predicate-no-models predicate))
                 (predicate-unique predicate)
                 (state-predicate predicate)
                 (not (member (predicate-name predicate) '(selected impasse))) ; Ignore selected and impasse predicates
                 )
        (setq unique-state-predicates (cons predicate unique-state-predicates))
        )
      )
    ; Create condition lists plus the variables and types to be used in the conditionals
    (dolist (usp unique-state-predicates)
      ; Regular predicate
      (setq variablization (variablize-predicate usp context-variable-count))
      (setq condition-list (cons (car variablization) condition-list))
      (setq regular-variables (append (cadr variablization) regular-variables))
      (setq types (append (caddr variablization) types))
      (setq arguments (append (cadddr variablization) arguments))
      (setq context-variable-count (+ (length (cadr variablization)) context-variable-count))
      (setq regular-predicate-arguments (append (convert-arguments-predicate-name (remove 'state (cdar variablization) :key #'car) (predicate-name usp)) regular-predicate-arguments))
      (setq predicate-name-list (append (nth 4 variablization) predicate-name-list))
      (setq regular-condition-list condition-list)
      ; Prediction predicate
      (setq psp (predicate-predict usp))
      (setq variablization (variablize-predicate psp context-variable-count))
      (setq condition-list (cons (car variablization) condition-list))
      (setq prediction-variables (append (cadr variablization) prediction-variables))
      (setq context-variable-count (+ (length (cadr variablization)) context-variable-count))
      (setq prediction-predicate-arguments (append (convert-arguments-predicate-name (remove 'state (cdar variablization) :key #'car) (predicate-name usp)) prediction-predicate-arguments))
      )
    (setq prediction-predicate-arguments (remove 'state prediction-predicate-arguments :key #'car))
    ; Create argument lists for predicates
    (do ((cns predicate-name-list (cdr cns))
         (cas arguments (cdr cas))
         (cts types (cdr cts)))
        ((null cns))
      (setq regular-argument-list (cons (list (concat-symbols (list (car cns) (car cas)) t) (car cts)) regular-argument-list))
      )
    (do ((cns predicate-name-list (cdr cns)) ; Argument names for both regular and *next state taken from the 
         (cas arguments (cdr cas))
         (cts types (cdr cts)))
        ((null cns))
      (setq prediction-argument-list (cons (list (concat-symbols (list (car cns) (car cas)) t) (car cts)) prediction-argument-list))
      )
    ; When it exists, add the selected predicate to the condition list
    (when (graph-selected-predicate cg)
      (setq variablization (variablize-predicate (predicate-from-name 'selected) context-variable-count))
      (setq selected-pattern (car variablization))
      (setq condition-list (cons selected-pattern condition-list))
      (setq operator-variable (caadr variablization)) ; Assumes operator variable is the first/only variable listed (since state is ignored)
      (setq context-variable-count (+ (length (cadr variablization)) context-variable-count))
      )

    ; Create types and predicates
    (new-type 'utility :numeric t :discrete t :min 0 :max 20)
    ; Predicate PROJECTED
    (setq pred (predicate 'projected :arguments (append regular-argument-list '((value utility %))) :function 1))
    (setq variablization (variablize-predicate pred context-variable-count regular-predicate-arguments))
    (setq context-variable-count (+ (length (cadr variablization)) context-variable-count))
    ; Predicate PROJECTED*NEXT
    (setq pred (predicate (concat-symbols (list 'projected prediction-suffix)) :arguments (append prediction-argument-list '((value utility %))) :function 'projected))
    (setq variablization (variablize-predicate pred context-variable-count prediction-predicate-arguments))
    (setq condition-list (cons (car variablization) condition-list)) 
    (setq context-variable-count (+ (length (cadr variablization)) context-variable-count))
    (setq projected*next-value-variable (caadr (assoc 'value (cdar variablization))))
    ; Predicate REWARD
    (setq pred (predicate 'reward :perception t :arguments (append prediction-argument-list '((value utility %)))
                          :function (list (append '(0) (stars (length prediction-argument-list)) '((0 20)))
                                          (append '(.1) (stars (length prediction-argument-list)) '((0 10))))))
    (setq variablization (variablize-predicate pred context-variable-count prediction-predicate-arguments))
    (setq condition-list (cons (car variablization) condition-list)) 
    (setq context-variable-count (+ (length (cadr variablization)) context-variable-count))
    (setq reward-value-variable (caadr variablization))
    ; Predicate Q
    (setq q-pred (predicate 'q :arguments (append regular-argument-list '((operator operator) (value utility %))) :function 1))
    (setq variablization (variablize-predicate q-pred context-variable-count))
    (setq context-variable-count (+ (length (cadr variablization)) context-variable-count))

    ; Add implicit Q to conditions of backup conditionals
    (setq implicit-q-condition (append `(q (operator (,operator-variable))) regular-predicate-arguments '((value (q (:filter (* 0 .05)))))))
    (setq condition-list (cons implicit-q-condition condition-list))

    (setq regular-condition-list (reverse regular-condition-list))
    (setq condition-list (reverse condition-list))

    ; Create conditionals
    ;;; Conditional Q-C (retrieve Q values for current state)
    (conditional 'q-c
                 :conditions (cons '(state (state (s))) regular-condition-list)
                 :condacts (list (car (variablize-predicate q-pred context-variable-count regular-predicate-arguments)))
                 )

  ;;; Conditional SELECT-OPERATOR (select operator based on Q value)
  (conditional 'select-operator
               :conditions (append (cons '(state (state (s))) regular-condition-list)
                                   (list implicit-q-condition))
               :actions (list selected-pattern)
               )

    ;;; Conditional BACKUP-PROJECTED
    (conditional 'backup-projected
                 :conditions (cons '(state (state (s))) condition-list)
                 :actions (list (cons 'projected
                                      (append regular-predicate-arguments
                                              `((value (,projected*next-value-variable (:coefficient .95 :offset ,reward-value-variable :pad 0 :apply-coefficient-to-offset t))))
                                              ))
                                )
                 )

    ;;; Conditional BACKUP-Q
    (conditional 'backup-q
                 :conditions (cons '(state (state (s))) condition-list)
                 :actions (list (cons 'q (append regular-predicate-arguments `((operator (,operator-variable)) (value (,projected*next-value-variable (:coefficient .95 :offset ,reward-value-variable :pad 0 :apply-coefficient-to-offset t)))))))
                 )

    )
  )

; Create an alpha network for a pattern
(defun create-alpha-network (pp c in out)
  (let* ((p-name (car pp)) ; Predicate name for pattern
         (cond-name (conditional-name c))
         (pred (predicate-from-name p-name)) ; Predicate for pattern
         (open (open-world pred))
         (p-args (predicate-arguments pred)) ; Arguments of the predicate
         (wmfn (predicate-wm pred)) ; Predicate WM FN
         (wmvs (predicate-wm-variables pred)) ; Vector of predicate WM's variables
         iwmvs ; Vector of variables to use in a transformed alpha path
         wmvn ; Starts with WM VN, updated to variable node after constant tests
         reduced-wmvn ; Variable node at the output of a constant test
         shared-wmvn ; VN to share after a filter function
         wmvn-vs ; Variables in variable node prior to constant test
         (reduced-vs (copy-seq wmvs)) ; Copy of predicate WM's variables, to be updated as go through elements
         (wmvs-rank (length wmvs)) ; Rank of predicate
         evnum ; Index of element in WM FN
         ev ; WM variable corresponding to the element
         e-arg-name ; Argument name
         e-content ; Argument contents
         e-type ; Argument type
         e-rest ; Stuff after arguent name and content
         c-vn ; Name of variable in element, if it is a variable test
         c-v ; Variable for element, if it is a variable test
         c-varns ; List of variable names in pattern
         c-vars ; List of variables in pattern
         c-vars-v ; Vector of variables in pattern
         (c-vars-i (init-vector wmvs-rank)) ; Vector for mapping from predicate variables to conditional variables
         (c-vars-d (init-vector wmvs-rank)) ; Boolean vector for which predicate variables used in delta factor
         (c-vars-e (init-vector wmvs-rank)) ; Boolean vector for which predicate variables have pattern elements (nil means ignored by pattern)
         (original-pp pp) ; Used for error message
         negated ; Whether pattern is negated
         delta-vs ; Variables for delta node
         aff
         affines
         affine-node
         action-offset-variable
         coef coef-position
         (filters (init-vector wmvs-rank)) ; Filters on elements of pattern
         (not-equals (init-vector wmvs-rank)) ; Not-equal tests on elements of pattern
         (variable-exists (init-vector wmvs-rank)) ; Which WM variables have conditional variables
         filter-fn filter-node
         afilter ; There is a filter along at least one dimension
         linear-filter ; There is a linear filter
         transform-node ; Transform factor node (for negated actions)
         explicit-node ; Explicit factor node (for making function explicit along a dimension in an action)
         outgoing-vn ; Shared link for finding WM VN for condacts
         share ; Conditions for sharing VN from conditions/condacts with actions
         exponential ; Whether this is an exponential condition
         action ; Whether pattern is for an action
         new-action-var ; Whether variable is a newly introduced action variable
         explicit-var ; Name of variable that is newly introduced in action (and not used elsewhere in action) and marked as :explicit
         beta-not-equal-vars ; Not-equal variable pairs when other variable is in a previous condition
         )
    ; Generate error if can't find predicate name among predicates
    (unless pred (error "Predicate ~S in conditional ~S is undefined." p-name cond-name))
    ; Strip off predicate name from predicate pattern
    (setq pp (cdr pp))
    ; Determine if pattern is negated (negation must come before alpha variable if exists)
    (when (and (listp pp) (eq (car pp) negation-symbol))
      (setq negated t)
      (setq pp (cdr pp))
      )
    ; Determine if pattern is an exponentiated condition
    (when (and (listp pp) (eq (car pp) exponential-symbol)
               out (not in))
      (setq exponential t)
      (setq pp (cdr pp))
      )
    (setq share (or (not (predicate-persistent pred)) ; Conditions for sharing VN from conditions/condacts with actions
                    (and (not (predicate-select pred))
                         (or (predicate-universal pred)
                             (predicate-cumulative pred)
                             )
                         )
                    ))
    ; Deal with sharing
    (cond ((or (and in
                    (or out ; Condact sharing
                        (and open-actions-like-condacts open) ; Sharing open-world actions when behave like condacts
                        )
                    )
               (and out open-conditions-like-condacts) ; Treat open-world conditions like condacts for connectivity
               )
           ; If there is a WM VN attached to the WM FN, use it; otherwise create a new one
           (setq outgoing-vn (predicate-outgoing-vn pred))
           (if outgoing-vn
               (progn
                 (setq wmvn outgoing-vn)
                 (setf (node-pattern-type outgoing-vn) 'condact)
                 )
             (progn
               (setq wmvn (create-wm-variable-node pred wmfn
                                                   (concat-symbols `(,p-name shared wm-vn) t)
                                                   ; condacts-change-wm
                                                   nil
                                                   wmvs t t nil out (predicate-exponential pred))
                     )
               )
             )
           )
          ((and in (not out)) ; Actions
           (setq wmvn (add-action pred wmfn ; Will return a list of two nodes for closed-world selection predicate
                                  (if share 'shared 'action)
                                  (concat-symbols (list cond-name p-name) t)
                                  (concat-symbols (elements-constants pp) t)
                                  negated (predicate-exponential pred) wmvs c
                                  share
                                  (predicate-persistent pred)
                                  open))
           (setq action t)
           )
          ((and (not in) out) ; Conditions
           (setq wmvn (add-condition pred wmfn
                                             (concat-symbols `(,p-name shared wm-vn) t)
                                             (concat-symbols (list cond-name p-name) t)
                                             (concat-symbols (elements-constants pp) t)
                                             wmvs c (predicate-exponential pred)
                                             share)
                 )
           )
          )
    ; If condact is negated, error
    (when (and in out negated)
      (error "Negation is not allowed in condacts, as used in pattern ~S of conditional ~S." pp (conditional-name c))
      )
    ; If action is negated, insert transform (inversion or negation) factor and variable node
    (when (and in (not out) negated)
      (setq iwmvs (transform-variables wmvs)) ; Replace WM vars with transformed/inverse vars
      (setq reduced-wmvn
            (init-variable-node
             (concat-symbols (list cond-name p-name 'action-neg-wm-vn) t)
             'inversion nil nil iwmvs t (pattern-type in out)))
      (setq transform-node (create-transform-factor c (if (predicate-no-normalize pred) 'negate 'invert) iwmvs wmvs reduced-wmvn wmvn wmvn in out nil))
      (when (and (eq p-name 'selected) ; Action for selected
                 in ; With next, detect that this is an action
                 (not out)
                 )
        (setf (graph-negative-preferences cg) (cons transform-node (graph-negative-preferences cg)))
        )
      (setq wmvn reduced-wmvn) ; Make new variable node the one to build on for rest of alpha path
      (setq wmvs iwmvs) ; Use inverted vars for rest of alpha path
      (setq reduced-vs (copy-seq iwmvs)) ; Copy negated vars for use in alpha path
      )
    ; If pattern is exponentiated, insert transform/exponentiate factor and variable node
    (when exponential
      (setq iwmvs (transform-variables reduced-vs)) ; Replace WM vars with transformed vars
      (setq reduced-wmvn
            (init-variable-node
             (concat-symbols (list cond-name p-name 'EV) t)
             'exponential nil nil iwmvs t (pattern-type in out)))
      (setq transform-node (create-transform-factor c 'exponentiate iwmvs reduced-vs reduced-wmvn wmvn wmvn in out nil))
      (when (predicate-unique pred)
        (setf (node-normalize transform-node) t)
        )
      (setq wmvn reduced-wmvn) ; Make new variable node the one to build on for rest of alpha path
      (setq wmvs iwmvs)
;      (setq wmvs (transform-variables wmvs)) ; Use transformed vars for rest of alpha path [Use this form rather than previous if after dolist on elements]
      (setq reduced-vs iwmvs) ; Transformed vars for use in rest of alpha path
      )
    ; Process elements of pattern, handling constant and variable tests (and affine transforms)
    (dolist (element pp)
      (setq e-rest (element-rest element)) ; Stuff after element argument and content
      (when (and e-rest ; Syntax error if stuff after argument and content, and
                 (or (not (weight-list (car e-rest))) ; First part of rest is not a weight vector, or
                     (cdr e-rest))) ; There is even more after the weight vector
        (error "Extra stuff in element ~S of pattern ~S in conditional ~S"
               element original-pp (conditional-name c)))
      (setq evnum (element-wm-index element pred)) ; Index of element in predicate WM
      (unless evnum
        (error "Argument ~S not defined in predicate ~S, but used in conditional ~S"
               (car element) p-name (conditional-name c))
        )
      (setq ev (aref wmvs evnum)) ; WM variable corresponding to the element
      (setq wmvn-vs (node-variables wmvn)) ; Variables in variable node prior to constant test
      (setq e-arg-name (element-argument-name element)) ; Argument name
      (setq e-content (element-content element)) ; Argument contents
      (setq e-type (type-from-predicate-argument e-arg-name pred)) ; Argument type
      (cond ((variable-element e-content) ; Process a variable element
             (setf (aref variable-exists evnum) t)
             ; Get name of variable from element
             (setq c-vn (car e-content))
             ; Get existing conditional variable for name if it exists
             (setq c-v (variable-from-name c-vn (conditional-variables c)))
             ; Signal error if type of condtional variable not same as type of pattern argument
             (when (and c-v (not (equal (stype-name e-type) (stype-name (svariable-type c-v)))))
               (error "Type mismatch in conditional ~S; Variable ~S used for both types ~S and ~S!"
                      cond-name (svariable-name c-v) (stype-name e-type) (stype-name (svariable-type c-v)))
               )
             (unless c-v ; Conditional variable does not already exist, so create it
               (setq c-v (make-svariable :name c-vn
                                         :type e-type
                                         :unique (variable-unique-in-conditional c-vn c)
                                         ))
               (setf (conditional-variables c) (cons c-v (conditional-variables c)))
               (when action
                 (setq new-action-var t)
                 )
               )
             (cond ((member c-vn c-varns) ; Conditional variable already used in pattern (within pattern variable equality testing)
                    (setf (aref c-vars-i (position e-arg-name p-args :key #'car)) c-vn) ; Signal that predicate variable maps to existing pattern variable
                    (setq new-action-var nil)
                    )
                   (t ; Conditional variable is new for this pattern
                    (setq c-varns (cons c-vn c-varns)) ; List of conditional variable names in pattern
                    (setq c-vars (cons c-v c-vars)) ; List of conditional variables in pattern 
                    (setf (aref c-vars-i (position e-arg-name p-args :key #'car)) c-v)
                    )
                   )
             (setf (aref c-vars-d evnum) t)
             (setf (aref c-vars-e evnum) t)
             (dolist (variable-modifier (cdr e-content))
               (cond ((numberp variable-modifier) ; There is an offset (special case of an affine transform)
                      ; Convert offset into affine
                      (setq aff (make-affine :offset variable-modifier :to c-vn :from c-vn))
                      (setq affines (cons aff affines))
                      )
                     ((affine-listp variable-modifier) ; An affine transform
                      (setq coef-position (position ':coefficient variable-modifier))
                      (when coef-position
                        (setq coef (nth (+ coef-position 1) variable-modifier))
                        (when (= coef 0)
                          (error "Coefficient in affine transform in conditional ~S must not be 0."
                                 (conditional-name c))
                          )
                        )
                      (setq aff (apply #'make-affine variable-modifier))
                      (setf (affine-to aff) c-vn)
                      (if (affine-from aff)
                          (when (and in (not out)) ; An action
                            (error "Use the condition variable ~S directly in action ~S of conditional ~S instead of using a new action variable (~S) in conjunction with :FROM ~S."
                                   (affine-from aff) pp (conditional-name c) (affine-to aff) (affine-from aff))
                            )
                        (setf (affine-from aff) (affine-to aff)) ; "From" defaults to "to"
                        )
                      (unless (eq e-type ; Type of :to variable
                                  (svariable-type (variable-from-name (affine-to aff) c-vars))
                                  )
                        (error "Mismatched variable types ~S and ~S in affine transform ~S in conditional ~S."
                               (stype-name e-type)
                               (stype-name (svariable-type (variable-from-name (affine-to aff) c-vars)))
                               aff (conditional-name c))
                        )
                      (when (and (affine-offset aff) (symbolp (affine-offset aff))) ; A variable is used as the offset
                        (if (and in (not out)) ; This is an action
                            (setq action-offset-variable (variable-from-name (affine-offset aff) (conditional-variables c)))
                          (error "Variables not currently allowed as offsets other than in actions: Pattern ~A in conditional ~S." original-pp (conditional-name c))
                          )
                        )
                      (setq affines (cons aff affines))
                      )
                     ((filter-listp variable-modifier) ; A filter on the variable
                      (setf (aref filters evnum) (create-element-plm ev (cdr variable-modifier) 0)) ; Add filter in its place in the vector
                      (setq afilter t)
                      (unless (e= (filter-coef variable-modifier) 0)
                        (setq linear-filter t)
                        )
                      )
                     ((eq variable-modifier ':explicit)
                      (if new-action-var
                          (setq explicit-var (svariable-name ev))
                        (error "Attempt to generate an explicit distribution over variable (~S) that isn't a new action variable in pattern ~S of conditional ~S"
                               c-vn original-pp (conditional-name c))
                        )
                      )
                     ((not-equal-listp variable-modifier) ; A not-equal (<>) test on a prior variable
                      (unless (stype-discrete e-type)
                        (error "Argument ~S is not discrete in a not-equal (<>) test in pattern ~S in conditional ~S"
                               e-arg-name original-pp cond-name)
                        )
                      (when (member (cadr variable-modifier) c-varns) ; This is a within-pattern not-equal (<>) test
                        (unless (eq e-type (type-from-predicate-argument (argument-from-variable (cadr variable-modifier) pp) pred))
                          (error "Variables ~S (~S) and ~S (~S) are not of the same type in a not-equal (<>) test in pattern ~S in conditional ~S"
                                 c-vn (stype-name e-type) (cadr variable-modifier)
                                 (stype-name (type-from-predicate-argument (argument-from-variable (cadr variable-modifier) pp) pred))
                                 original-pp cond-name)
                          )
                        (unless (not in)
                          (error "Within-pattern not-equal (<>) test in an action or condact (~S) in conditional ~S"
                                 original-pp cond-name)
                          )
                        )
                      (if (member (cadr variable-modifier) c-varns)
                          (setf (aref not-equals evnum) (cadr variable-modifier)) ; Unequal to a variable in the same pattern
                        (setq beta-not-equal-vars (cons (list c-vn (cadr variable-modifier)) beta-not-equal-vars))) ; Unequal to a variable in a previous pattern
                      )
                     ((eq variable-modifier not-equal-symbol)
                      (error "Not-equal symbol (<>) is not first element of a list after variable ~S in pattern ~S of conditional ~S" c-vn original-pp cond-name)
                      )
                     )
               )
             )
            ((and (or in out) (constant-element e-content)) ; Process a constant/filter element if messages are passing
             (setf (aref filters evnum) (create-element-plm ev (if (filter-listp e-content) ; A filter
                                                                   (cdr e-content) ; Remove :filter from front
                                                                 (list (list e-content 1 0))) ; Turn into filter data
                                                            )
                   )
             (setf (aref c-vars-e evnum) t)
             (setq afilter t)
             (when (and (filter-listp e-content)
                        (not (e= (filter-coef e-content) 0)))
               (setq linear-filter t)
               )
             ; Remove WM variable for current constant from list
             (setq reduced-vs (remove ev reduced-vs))
             )
            )
      )
    (when (or afilter ; There is at least one variable with a filter
              (position nil c-vars-e)) ; or there is at least one WM variable not mentioned in the pattern
      (setq filter-fn (create-filter-function wmvs filters)) ; Create filter function
      (dotimes (i wmvs-rank)
        (unless (aref variable-exists i) ; There is no conditional variable for this WM variable
          (setq reduced-vs (remove (aref wmvs i) reduced-vs)) ; Don't include variable in resulting variable node
          )
        )
      ; If sharing filters, and this is a condition, see if there is an existing one to share
      (when (and share-condition-tests out (not in))
        (setq shared-wmvn (shared-filter-wmvn wmvn filter-fn reduced-vs))
        )
      ; If there is a shared filter, simply pick up with its subsequent variable node
      (if shared-wmvn
          (setq wmvn shared-wmvn)
        (progn
          ; Create variable node for output from filter test (and make it WM variable for next iteration)
          (setq reduced-wmvn
                (init-variable-node
                 (concat-symbols (list cond-name p-name 'fiv) t)
                 'filter nil nil reduced-vs t (pattern-type in out)))
          ; Create factor node for filter test
          (setq filter-node
                (init-factor-node (concat-symbols (list cond-name p-name 'fif) t)
                                  'filter
                                  wmvn-vs                  
                                  (list wmvn reduced-wmvn)
                                  (list wmvn) in out nil nil c))
          (setf (node-function filter-node) filter-fn)
          ; Mark node has having a linear filter if linear-filter is true
          (when linear-filter
            (setf (node-linear-filter filter-node) t)
            )
          ; Set things up to invert open-world predicates through filters
          ; Always do for messages through condacts to WM
          ; Do for open-world actions if open-actions-like-condacts it T
          ; Do for messages through condacts from WM if all-condact-filters-pad-1 is T
          ; Do for open-world conditions of open-conditions-like-condacts and all-condact-filters-pad-1 are both T   
          (when (and open ; Open world
                     (or (and in out) ; condact
                         (and in open-actions-like-condacts) ; Open-world action to be treated like incoming part of condact
                         (and out open-conditions-like-condacts) ; Open-world condition to be treated like outgoing part of condact
                         )
                     )
            (setf (node-inverse-function filter-node) (transform-plm #'invert-function-variant filter-fn))
            (setf (node-subsubtype filter-node)
                  (cond ((and in out) ; condact
                         (if all-condact-filters-pad-1 'inverse-both 'inverse-in))
                        ((and in open-actions-like-condacts) ; Open-world action to be treated like incoming part of condact
                         'inverse-in)
                        ((and out open-conditions-like-condacts) ; Open-world condition to be treated like outgoing part of condact
                         'inverse-out)))
            )
          ; Set things up for processing after filter test(s)
          (setq wmvn reduced-wmvn)
          ))
      )
    ; If condition is negated, insert transform/inversion factor and variable node
    (when (and out (not in) negated)
      (setq iwmvs (transform-variables reduced-vs)) ; Replace WM vars with transformed/inverse vars
      (setq shared-wmvn (if share-condition-tests (shared-negation-wmvn wmvn) nil))
      (if shared-wmvn
          (setq wmvn shared-wmvn)
        (progn
          (setq reduced-wmvn
                (init-variable-node
                 (concat-symbols (list cond-name p-name 'wm-neg-vn) t)
                 'inversion nil nil iwmvs t (pattern-type in out)))
          (setq transform-node (create-transform-factor c 'invert iwmvs reduced-vs reduced-wmvn wmvn wmvn in out nil))
          (setq wmvn reduced-wmvn) ; Make new variable node the one to build on for rest of alpha path
          ))
      (setq wmvs (transform-variables wmvs)) ; Use inverted vars for rest of alpha path
      (setq reduced-vs iwmvs) ; Negated vars for use in rest of alpha path
      )
    (when explicit-var
      (setq reduced-wmvn
            (init-variable-node (concat-symbols (cons (conditional-name c) (cons p-name (cons 'XV c-varns))) t)
                                'explicit nil nil reduced-vs t (pattern-type in out)))
      (setq explicit-node (init-factor-node (concat-symbols (cons (conditional-name c) (cons p-name (cons 'XF c-varns))) t)
                                              'explicit reduced-vs (list wmvn reduced-wmvn) (list wmvn) in out nil t c))
      (setf (node-function explicit-node) (list ':explicit (position explicit-var reduced-vs :key #'svariable-name)))
      (setq wmvn reduced-wmvn)
      )
    (when c-vars ; If there are any variables create a delta factor
      ; Reverse the lists so get in good reading order for people (original order)
      (setq c-varns (reverse c-varns))
      (setq c-vars (reverse c-vars))
      ; Convert list of variables to vector of variables
      (setq c-vars-v (coerce c-vars 'vector))
      ; Create variables for delta node (paired previous/WM and pattern vars)
      (setq delta-vs (delta-variables (length (node-variables wmvn)) c-vars-d c-vars-i wmvs))
      (setq shared-wmvn (if (and share-condition-tests out (not in)) (shared-delta-wmvn wmvn c-vars-v) nil))
      (if shared-wmvn
          (setq wmvn shared-wmvn)
        (progn
          ; Create new memory for pattern after delta
          (setq reduced-wmvn
                (init-variable-node (concat-symbols (cons (conditional-name c) (cons p-name (cons 'ADV c-varns))) t)
                                    'delta nil nil c-vars-v t (pattern-type in out)))
          ; The new memory is currently the last memory in the conditional
          (setf (conditional-last-memory c) reduced-wmvn)
          ; Create delta factor
          (create-delta-factor c delta-vs c-varns reduced-wmvn wmvn wmvn in out 'match pred not-equals)
          (setq wmvn reduced-wmvn)
          ))
      )
    (when affines ; There is at least one variable with a transform
      ; Convert :from and :to fields from pattern variable name to pattern variable numbers
      (dolist (aff affines)
        (setf (affine-from aff) (position (affine-from aff) c-varns))
        (setf (affine-to aff) (position (affine-to aff) c-varns))
        )
      ; If there is an offset variable in an action, add it to the variables
      (when action-offset-variable
        (setq c-vars-v (extend-vector c-vars-v action-offset-variable))
        )
      ; Create variable node after transform factor node
      (setq reduced-wmvn
            (init-variable-node
             (concat-symbols (list cond-name p-name 'av) t)
             'affine nil nil c-vars-v t (pattern-type in out)))
      ; Create transform factor node
      (setq affine-node
            (init-factor-node (concat-symbols (list cond-name p-name 'af) t)
                              'affine
                              c-vars-v
                              (list wmvn reduced-wmvn)
                              (list wmvn) in out nil nil c))
      (setf (node-region-pad affine-node) (if (affine-pad aff) (affine-pad aff) (if open 1 0)))
      (when (predicate-unique pred)
        (setf (node-normalize affine-node) t)
        )
      (setf (node-function affine-node) affines)
      (setq wmvn reduced-wmvn)
      )
    (unless (eq (node-subtype wmvn) 'wm)
      (setf (node-subtype wmvn) 'alpha)
      )
    (setf (node-subsubtype wmvn) (if in (if out 'condact 'action) 'condition))
    (setf (conditional-alpha-memories c) (cons wmvn (conditional-alpha-memories c)))
    (cons wmvn beta-not-equal-vars))
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
          (error "No maximal found for selection dimension ~S in selection-still-maximal" ui)
          )
        (unless (setq found (and found (member (car pmsi) msi :test #'maximal-contained)))
          (return found)
          )
        )
      )
    found)
  )