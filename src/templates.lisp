(in-package :sigma)
; Create an action-model conditional (and predicate) for each closed-world unique state predicate
; Create a perception-model conditional (and predicate) for each perceptual predicate
; Create reinforcement learning (RL) predicates and conditionals
(defun create-models nil
  (when automatic-action-models
    (create-action-models)
    )
  (when automatic-perception-models
    (create-perception-models)
    )
  (when automatic-reinforcement-learning
    (create-reinforcement-learning)
    )
  )

; Create action models
(defun create-action-models nil
  (let (unique-state-predicates ; All of the unique close-world state predicates
        action-function-variables ; The variables to include in the action function
        condition-list ; List of conditions to be used in prediction
        variablization ; A list containing a pattern, a list of variables, and a list of types for a predicate
        action-type-list ; A list of the names of the types of the variables the perception predicate
        action-condact ; New condact for action function
        (action-variable-count 0) ; The count of how many variables have been created for the action conditional
        pred ; Predicate to use in conditional
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
      (setq variablization (variablize-predicate usp action-variable-count))
      (setq condition-list (cons (car variablization) condition-list))
      (setq action-function-variables (append (cadr variablization) action-function-variables))
      (setq action-variable-count (+ (length (cadr variablization)) action-variable-count))
      (setq action-type-list (append (caddr variablization) action-type-list))
      )
    ; When it exists, add the selected predicate to the condition list
    (when (graph-selected-predicate cg)
      (setq variablization (variablize-predicate (predicate-from-name 'selected) action-variable-count))
      (setq condition-list (append condition-list (list (car variablization)))) ; Put selected pattern at end of condition list
      (setq action-function-variables (append (cadr variablization) action-function-variables)) ; Put operator variable after other condition variables (and before condact vars)
      (setq action-variable-count (+ (length (cadr variablization)) action-variable-count))
      (setq action-type-list (append (caddr variablization) action-type-list))
      )
    ; Create conditionals
    (dolist (usp unique-state-predicates)
;     (when (not (member (predicate-name usp) no-preds)) ; Don't create for dependent predicates
      (setq variablization (variablize-predicate usp action-variable-count (universal-arguments usp (cdr (assoc (predicate-name usp) condition-list)))))
      (setq pred (predicate (make-name-symbol 'action-)
                            :arguments (reverse (mapcar #'list
                                                        (append (cadr variablization) action-function-variables)
                                                        (append (caddr variablization) action-type-list)))
                            :unique (unique-model-arguments (car variablization) (predicate-unique usp))
                            :function (if (predicate-action-function usp) (predicate-action-function usp) 1)))
      (setf (graph-action-predicate cg) pred)
      (setq action-condact (cons (predicate-name pred) (reverse (mapcar #'(lambda (v) (list v (list v))) (append (cadr variablization) action-function-variables)))))
      (conditional (concat-symbols (list (predicate-name usp) 'prediction) t)
                   :conditions (if (predicate-from-name 'state t) (cons '(state (state (s))) condition-list) condition-list)
                   :condacts (append (list (cons (concat-symbols (list (caar variablization) prediction-suffix)) (cdar variablization))) (list action-condact))
                   )
;        )
      )
    t)
  )

; Create perception models
(defun create-perception-models nil
  (let (perception-function-variables ; The variables to include in the perception function
        condact-list ; List of condacts to be used in prediction (perception learning)
        variablization ; A list containing a pattern, a list of variables, and a list of types for a predicate
        perception-type-list ; A list of the names of the types of the variables the perception predicate
        perception-condact ; New condact for perception function
        (perception-variable-count 0) ; The count of how many variables have been created for the perception conditional
        pred ; Predicate to use in conditional
        )
    ; Create condition and condact lists plus the variables and types to be used in the conditionals
    (dolist (predicate (graph-predicates cg))
      (when (and (closed-world predicate)
                 (not (predicate-no-models predicate))
                 (predicate-unique predicate)
                 (state-predicate predicate)
                 (not (member (predicate-name predicate) '(selected impasse))) ; Ignore selected and impasse predicates
                 )
        (setq variablization (variablize-predicate (predicate-predict predicate) perception-variable-count))
        (setq condact-list (cons (car variablization) condact-list))
        (setq perception-function-variables (append (cadr variablization) perception-function-variables))
        (setq perception-variable-count (+ (length (cadr variablization)) perception-variable-count))
        (setq perception-type-list (append (caddr variablization) perception-type-list))
        )
      )
    ; Create conditionals, one for each perception predicate
    (dolist (p (graph-predicates cg))
      (when (and (predicate-perception p)
                 (not (member (predicate-name p) '(time state)))
                 (not (predicate-no-models p))
                 (not (closed-world p))
                 (not (predicate-prediction p))
                 )
        (setq variablization (variablize-predicate p perception-variable-count))
        (setq pred (predicate (make-name-symbol 'perception-)
                              :arguments (mapcar #'list
                                                 (append (cadr variablization) perception-function-variables)
                                                 (append (caddr variablization) perception-type-list))
                              :unique (unique-model-arguments (car variablization) (predicate-unique p))
                              :automated-perception t
                              :function (if (predicate-perception-function p) (predicate-perception-function p) 1)))
        (setf (graph-perception-predicate cg) pred)
        (setq perception-condact (cons (predicate-name pred) (mapcar #'(lambda (v) (list v (list v))) (append (cadr variablization) perception-function-variables))))
        (conditional (concat-symbols (list (predicate-name p) 'perception-prediction) t)
                     :conditions (if (predicate-from-name 'state t) '((state (state (s)))) nil)
                     :condacts (append (cons (cons (caar variablization) (cdar variablization)) condact-list) (list perception-condact))
                     )
        )
      )
    t)
  )

; -----------------------------------------------------------
; Automatically create conditionals based on predicates for action models and perceptual prediction (perception and map functions)
; Gradient descent with diachronic processing takes care of the rest

; List of (closed-world unique state) predicates for which shouldn't learn because they depend on other predicates
(defun no-prediction-predicates nil
  (let (no-preds ; List of predicate names for which not to do prediction
        dwmfn ; Descendant wmfn node
        pred ; Predicate for descendant wmfn node
        )
    (dolist (n (graph-nodes cg))
      (when (wm-fnp n)
        (dolist (d (node-descendant-links n))
          (setq dwmfn (node-from-number (descendant-link-to d)))
          (setq pred (node-predicate dwmfn))
          (when (and (wm-fnp dwmfn)
                     (closed-world pred)
                     (predicate-unique pred)
                     (state-predicate pred)
                     (not (= (node-number n) (descendant-link-to d))) ; Ignore dependance on self
                     (not (and (predicate-predict pred)
                               (eq (predicate-name (node-predicate n)) (predicate-name (predicate-predict pred))) ; Ignore dependance on prediction predicate
                               ))
                     )
            (setq no-preds (adjoin (predicate-name pred) no-preds))
            )
          )
        )
      )
    (remove 'selected no-preds))
  )

; Create a list containing a variablized pattern, the list of variables, the list of types, and the list of arguments, for a predicate
; Retain-args is a list argument variable pairs where want to retain the variables
(defun variablize-predicate (predicate variable-count &optional retain-args)
  (let (var variables elements arguments preds types an found (pname (predicate-name predicate)))
    (dolist (arg (predicate-arguments predicate))
      (setq an (argument-name arg))
      (cond ((eq an 'state)
             (setq elements (cons '(state (s)) elements))
             )
            ((setq found (assoc an retain-args))
             (setq elements (cons (list an (cadr found)) elements))
             )
            (t
             (setq var (concat-symbols (list an variable-count) t))
             (setq variables (cons var variables))
             (setq types (cons (argument-type-name arg) types))
             (setq elements (cons (list an (list var)) elements))
             (setq arguments (cons an arguments))
             (setq preds (cons pname preds))
             )
            )
      )
    (list (cons pname (reverse elements)) (reverse variables) (reverse types) (reverse arguments) (reverse preds))
    )
  )
; Convert an argument with an original name to one with predicate name prepended to argument name
(defun convert-argument-predicate-name (arg pname)
  (cons (concat-symbols (list pname (car arg)) t) (cdr arg))
  )

; Convert argument names of a list of arguments
(defun convert-arguments-predicate-name (args pname)
  (mapcar #'(lambda (arg) (convert-argument-predicate-name arg pname)) args)
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
    (setq q-pred (predicate 'q :arguments (append regular-argument-list `((operator ,(graph-operator-type-name cg)) (value utility %))) :function 1))
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
