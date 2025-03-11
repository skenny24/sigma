(in-package :sigma)
; Solve ultimatum game by backwards propagating information across decisions in subgoals.
; But do problem solving forward in this version
(defun ultimatum-forward nil
  (init nil '(a b))

  (setq post-d '((ppfn 'offer-choice nil '((argmax wm-offer)))
                 (ppfn 'accept-choice nil '((max wm-state) (argmax wm-acceptance)))
                 (ppfn 'impasse nil '((max wm-operator) (argmax wm-type)))
                 (ppfn 'selected nil '((argmax wm-operator)))
                 (print-operators 'a)
                 (print-operators 'b)
                 (ppwm 'money)
                 (ppwm 'evaluation)
                 )
        )

  (new-type 'quantity :numeric t :discrete t :min 0 :max 4)

  (predicate 'offer :world 'open :arguments '((agent agent) (doer agent) (offer quantity)))
  (predicate 'accept :world 'open :arguments '((agent agent) (doer agent) (acceptance boolean)))

  (init-operators 'predicates '(offer accept) t)

  (predicate 'money :world 'closed :no-normalize t
             :arguments '((agent agent) (owner agent) (state state) (operator operator) (quantity quantity %)))
  (predicate 'offer-choice :world 'closed :arguments '((agent agent) (doer agent) (state state) (offer quantity !)))
  (predicate 'accept-choice :world 'closed :arguments '((agent agent) (doer agent) (state state) (acceptance boolean !)))
  (predicate 'completed :world 'closed :arguments '((agent agent) (state state) (operator operator)))
  (predicate 'evaluation :world 'closed :no-normalize t :arguments '((agent agent) (state state) (operator operator %)))
  (predicate 'b-offer-probs :world 'open :exponential t :arguments '((acceptance boolean %)))

;;; "Default" conditionals for evalution operators
  ; Make evaluation operators best in subgoal for tied operators
  (conditional 'evaluate-best
               :conditions '((state (agent (a)) (state (s)))
                             (impasse (agent (a)) (state (s)) (type tie) (operator (o)))
                             (evaluate-operator (operator (eo)) (evaluate (o))))
               :actions '((selected (agent (a)) (state (s)) (operator (eo))))
               :function 1
               )

  ; Make operator to be evaluated best in no-change for evaluation operator
  (conditional 'evaluation-operator-best
               :conditions '((state (agent (a)) (state (s)))
                             (impasse (agent (a)) (state (s)) (type no-change) (operator (eo)))
                             (evaluate-operator (operator (eo)) (evaluate (o))))
               :actions '((selected (agent (a)) (state (s)) (operator (o))))
               :function 1
               )


  ; Reject evaluation operators when there is an evaluation for their operator
  (conditional 'evaluate-reject
               :conditions '((state (agent (a)) (state (s)))
                             (completed (agent (a)) (state (s)) (operator (eo))))
               :actions '((selected - (agent (a)) (state (s)) (operator (eo))))
               :function 1
               )

  ; Copy down a chosen offer from state above
  ; Should one of these conditionals be created automatically for each state predicate when detecting impasses?
  (conditional 'copy-offer-choice
               :conditions '((state (agent (a)) (state (s)))
                             (impasse (agent (a)) (state (s)))
                             (offer-choice (agent (a)) (doer (b)) (state (s -1)) (offer (offer))))
               :actions '((offer-choice (agent (a)) (doer (b)) (state (s)) (offer (offer))))
               )

;;; Conditionals for ultimatum game
  ; Make offer operators acceptable for agent a
  (conditional 'acceptable-offer
               :conditions '((state (agent a) (state (s)))
                             (offer (agent a) (doer a) (operator (o)))
                             (impasse - (agent a) (state (s)) (type tie)))
               :actions '((selected (agent a) (state (s)) (operator (o))))
               :function .1)

  ; Make accept operators acceptable for agent b if there is an offer-choice available for a
  (conditional 'acceptable-accept
               :conditions '((state (agent (a)) (state (s)))
                             (offer-choice (agent (b)) (doer a) (state (s)))
                             (accept (agent (a)) (doer b) (operator (o)))
                             (impasse - (agent (a)) (state (s)) (type tie)))
               :actions '((selected (agent (a)) (state (s)) (operator (o))))
               :function .1)

  ; Reject offer operators for agent a when there is an offer-choice
  (conditional 'reject-offer
               :conditions '((state (agent a) (state (s)))
                             (offer (agent a) (doer a) (operator (o)))
                             (offer-choice (agent a) (doer a) (state (s))))
               :actions '((selected - (agent a) (state (s)) (operator (o))))
               )

  ; Reject accept operators for agent b if there is an accept-choice
  (conditional 'reject-accept
               :conditions '((state (agent (a)) (state (s)))
                             (accept (agent (a)) (doer b) (operator (o)))
                             (accept-choice (agent (a)) (doer b) (state (s))))
               :actions '((selected - (agent (a)) (state (s)) (operator (o))))
               )


  ; Reject any accept operator for agent A at base level
  (conditional 'base-reject-accept-A
               :conditions '((state (agent a) (state 0))
                             (accept (agent a) (doer (b)) (operator (o))))
               :actions '((selected - (agent a) (state 0) (operator (o))))
               )

  ; Apply operator offer
  (conditional 'apply-offer
               :conditions '((state (agent a) (state (s)))
                             (selected (agent a) (state (s)) (operator (o)))
                             (offer (agent a) (doer a) (offer (offer)) (operator (o))))
               :actions '((offer-choice (agent a) (doer a) (state (s)) (offer (offer))))
               )

  ; Simulate communicate offer from A to B
  (conditional 'communicate-offer
               :conditions '((offer-choice (agent a) (doer a) (state 0) (offer (offer))))
               :actions '((offer-choice (agent b) (doer a) (state 0) (offer (offer))))
               )

  ; Apply operator offer
  (conditional 'apply-accept
               :conditions '((state (agent (a)) (state (s)))
                             (selected (agent (a)) (state (s)) (operator (o)))
                             (accept (agent (a)) (doer (b)) (acceptance (acc)) (operator (o))))
               :actions '((accept-choice (agent (a)) (doer (b)) (state (s)) (acceptance (acc))))
               )

  ; Simulate communicate accept from B to A
  (conditional 'communicate-accept
               :conditions '((accept-choice (agent b) (doer b) (state 0) (acceptance (acc))))
               :actions '((accept-choice (agent a) (doer b) (state 0) (acceptance (acc))))
               )

  ; Return signal that accept operator has been evaluated once there is an accept-choice
  (conditional 'evaluate-accept-done
               :conditions '((state (agent (a)) (state (s)))
                             (money (agent (a)) (owner b) (state (s)) (operator (o)))
                             (impasse (agent (a)) (state (s)) (type no-change) (operator (eo)))
                             (evaluate-operator (operator (eo)) (evaluate (o)))
                             (accept (agent (a)) (doer b) (acceptance (acc)) (operator (o))))
               :actions '((completed (agent (a)) (state (s -1)) (operator (eo))))
               )

  ; Money for b in (a)'s model
  (conditional 'money-*-b
               :conditions '((state (agent (a)) (state (s)))
                             (accept-choice (agent (a)) (doer b) (state (s)) (acceptance (acc)))
                             (accept (agent (a)) (doer b) (acceptance (acc)) (operator (o)))
                             (offer-choice (agent (a)) (doer a) (state (s)) (offer (offer))))
               :actions '((money (agent (a)) (owner b) (state (s)) (operator (o)) (quantity (money))))
               :function-variable-names '(offer acc money)
               :function '((1 0 true 0) (1 1 true 1) (1 2 true 2) (1 3 true 3)
                           (1 * false 0))
               )

  ; Money for a in a's model
  (conditional 'money-a-a
               :conditions '((state (agent a) (state (s)))
                             (accept-choice (agent a) (doer b) (state (s)) (acceptance (acc)))
                             (accept (agent a) (doer b) (acceptance (acc)) (operator (o)))
                             (offer-choice (agent a) (doer a) (state (s)) (offer (offer))))
               :actions '((money (agent a) (owner a) (state (s)) (operator (o)) (quantity (money))))
               :function-variable-names '(offer acc money)
               :function '((1 0 true 3) (1 1 true 2) (1 2 true 1) (1 3 true 0)
                           (1 * false 0))
               )

  ; Return money to evaluation space
  (conditional 'return-money
               :conditions '((state (agent (a)) (state (s)))
                             (impasse (agent (a)) (state (s)) (type no-change) (operator (eo)))
                             (evaluate-operator (operator (eo)) (evaluate (o)))
                             (money (agent (a)) (owner (own)) (state (s)) (operator (o)) (quantity (money)))
                             )
               :actions '((money (agent (a)) (owner (own)) (state (s -1)) (operator (o)) (quantity (money))))
               :function-variable-names '(money)
               :function '((.1 0) (.4 1) (.7 2) (1 3))
               )

  ; Determine normalized probability for b's action
  (conditional 'bs-probs
               :conditions '((state (agent a) (state (s)))
                             (impasse (agent a) (state (s)) (type tie) (operator (o)))
                             (money (agent a) (owner b) (state (s)) (operator (o)) (quantity (money)))
                             (accept (agent a) (doer b) (acceptance (acc)) (operator (o))))
               :condacts '((b-offer-probs (acceptance (acc))))
               )

  ; Return value of b's choice for b
  (conditional 'evaluation-accept-b
               :conditions '((state (agent b) (state (s)))
                             (impasse (agent b) (state (s)) (type tie) (operator (o)))
                             (accept (agent b) (doer b) (acceptance (acc)) (operator (o)))
                             (impasse (agent b) (state (s 1)) (type none))
                             (money (agent b) (owner b) (state (s)) (operator (o))))
               :actions '((selected (agent b) (state (s -1)) (operator (o))))
               )

  ; Return expected value of b's choice for a (probability of b choosing times value for a)
  (conditional 'evaluation-accept-a
               :conditions '((state (agent a) (state (s)))
                             (impasse (agent a) (state (s)) (type tie) (operator (o)))
                             (accept (agent a) (doer b) (acceptance (acc)) (operator (o)))
                             (impasse (agent a) (state (s 1)) (type none))
                             (money (agent a) (owner a) (state (s)) (operator (o)))
                             (b-offer-probs (acceptance (acc))))
               :actions '((evaluation (agent a) (state (s)) (operator (o))))
               )

  ; Combine a's evaluations on b's choices into an evaluation of a's choice
  (conditional 'combine-accept-evaluation
               :conditions '((state (agent a) (state (s)))
                             (impasse (agent a) (state (s)) (type tie) (operator (ao)))
                             (impasse (agent a) (state (s 1)) (type none))
                             (evaluation (agent a) (state (s)) (operator (ao)))
                             (impasse (agent a) (state (s -1)) (type no-change) (operator (eo)))
                             (evaluate-operator (operator (eo)) (evaluate (o)))
                             (offer (agent a) (doer a) (operator (o))))
               :actions '((evaluation (agent a) (state (s -2)) (operator (o))))
               )

  ; Return signal that offer operator has been evaluated once there are no more accept operators to evaluate
  (conditional 'evaluate-offer-done
               :conditions '((state (agent a) (state (s)))
                             (impasse (agent a) (state (s)) (type tie) (operator (ao)))
                             (impasse (agent a) (state (s 1)) (type none))
                             (impasse (agent a) (state (s -1)) (type no-change) (operator (eo)))
                             (evaluate-operator (operator (eo)) (evaluate (o)))
                             (offer (agent a) (doer a) (operator (o))))
               :actions '((completed (agent a) (state (s -2)) (operator (eo))))
               )

  ; Return evaluations on offer operators once they have all been evaluated
  (conditional 'return-offer-evaluations
               :conditions '((state (agent a) (state (s)))
                             (impasse (agent a) (state (s)) (type tie) (operator (o)))
                             (impasse (agent a) (state (s 1)) (type none))
                             (offer (agent a) (doer a) (operator (o)))
                             (evaluation (agent a) (state (s)) (operator (o))))
               :actions '((selected (agent a) (state (s -1)) (operator (o))))
               )

  t)

(defun test-ultimatum-forward ()
  (ultimatum-forward)
  (d 93)
  (ppfn 'offer-choice nil '((constant wm-state 0) (argmax wm-offer)) info-stream)
  (ppfn 'accept-choice nil '((max wm-state) (argmax wm-acceptance)) info-stream)
  (print-global-decision-statistics info-stream)
  )

; Print out states in hierarchy for agent a
(defun print-operators (agent)
  (let ((agent-index (position agent (stype-constants (type-from-name 'agent))))
        op)
    (format trace-stream "~&~%")
    (dotimes (s (+ (- (aref bottom-state agent-index) base-level-state) 1))
      (setq op (operator-in-state (+ s base-level-state) agent))
      (when op
        (format trace-stream "~&")
        (pid op)
        )
      )
    )
  )
