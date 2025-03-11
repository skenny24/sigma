(in-package :sigma)
; TESTING
; Semantic memory (naive Bayes categorization)

(defun cat-init nil
  (init)
  (new-type 'id :constants '(i1 i2 i3))
  (new-type 'type :constants '(walker table dog human))
  (new-type 'color :constants '(silver brown white))
  (new-type 'i04 :numeric t :discrete t :min 0 :max 5)
  (new-type 'weight :numeric t :min 0 :max 500)

  (predicate 'object :world 'closed :perception t :arguments '((state state) (id id)))
  (predicate 'concept :perception t :arguments '((id id) (value type %)))
  (predicate 'concept2 :world 'closed :arguments '((id id) (value type !)))
  (predicate 'color :perception t :arguments '((id id) (value color %)))
  (predicate 'color2 :world 'closed :arguments '((id id) (value color !)))
  (predicate 'alive :perception t :arguments '((id id) (value boolean %)))
  (predicate 'mobile :perception t :arguments '((id id) (value boolean %)))
  (predicate 'legs :perception t :arguments '((id id) (value i04 %)))
  (predicate 'weight :perception t :arguments '((id id) (value weight %)))
  (predicate 'weight2 :world 'closed :arguments '((id id) (value weight $)))

  ; Function predicates
  (predicate 'concept-f
             :arguments '((concept type %)) 
             :function 1)
  (predicate 'concept-color
             :arguments '((concept type) (color color %))
             :function '((.95 walker silver) (.05 walker brown)
                         (.05 table silver) (.95 table brown)
                         (.05 dog silver) (.7 dog brown) (.25 dog white)
                         (.5 human brown) (.5 human white)))
  (predicate 'concept-legs
             :arguments '((concept type) (legs i04 %))
             :function '((1 walker 4)
                         (.9 table 4) (.1 table 3)
                         (.01 dog 1) (.02 dog 2) (.1 dog 3) (.87 dog 4)
                         (.01 human 0) (.02 human 1) (.97 human 2)))
  (predicate 'concept-alive
             :arguments '((concept type) (alive boolean %))
             :function '((1 walker false)
                         (1 table false)
                         (.05 dog false) (.95 dog true)
                         (.05 human false) (.95 human true)))
  (predicate 'concept-mobile
             :arguments '((concept type) (mobile boolean %))
             :function '((1 walker true)
                         (.95 table false) (.05 table true)
                         (.05 dog false) (.95 dog true)
                         (.05 human false) (.95 human true)))
  (predicate 'concept-weight
             :arguments '((concept type) (weight weight %))
             :function '(((-2/15 0 2/75) walker (5 10)) ((4/15 0 -1/75) walker (10 20))
                         ((-2/1881 0 2/1881) table (1 20)) ((5/198 0 -1/3960) table (20 100))
                         ((-2/7301 0 2/7301) dog (1 50)) ((3/149 0 -1/7450) dog (50 150))
                         ((-2/59451 0 2/59451) human (1 150)) ((16/1995 0 -1/49875) human (150 400))))

  (setq pre-d '((ppwm 'concept 'array) (ppwm 'color 'array) (ppwm 'alive 'array) (ppwm 'mobile 'array) (ppwm 'legs 'array) (ppwm 'weight)))
  (setq post-d '((ppfn 'concept2 nil '((constant wm-id i1) (argmax wm-value)))
                 (ppfn 'color2 nil '((constant wm-id i1) (argmax wm-value)))
                 (ppfn 'weight2 nil '((constant wm-id i1) (argmax wm-value)))))
  )

(defun cat-conds nil

  ; Prior on concept  
  (conditional 'concept
               :conditions '((object (state (state)) (id (id))))
               :condacts '((concept (id (id)) (value (concept)))
                           (concept-f (concept (concept))))
               )

  ; Conditional distribution on attributes given concept

  (conditional 'concept-color*join
               :conditions '((object (state (state)) (id (id))))
               :condacts '((concept (id (id)) (value (concept)))
                           (color (id (id)) (value (color)))
                           (concept-color (concept (concept)) (color (color))))
               )
  
  (conditional 'concept-legs*join
               :conditions '((object (state (state)) (id (id))))
               :condacts '((concept (id (id)) (value (concept)))
                           (legs (id (id)) (value (legs)))
                           (concept-legs (concept (concept)) (legs (legs))))
               )

  (conditional 'concept-alive*join
               :conditions '((object (state (state)) (id (id))))
               :condacts '((concept (id (id)) (value (concept)))
                           (alive (id (id)) (value (alive)))
                           (concept-alive (concept (concept)) (alive (alive))))
               )
  
  (conditional 'concept-mobile*join
               :conditions '((object (state (state)) (id (id))))
               :condacts '((concept (id (id)) (value (concept)))
                           (mobile (id (id)) (value (mobile)))
                           (concept-mobile (concept (concept)) (mobile (mobile))))
               )

  (conditional 'concept-weight*join
               :conditions '((object (state (state)) (id (id))))
               :condacts '((concept (id (id)) (value (concept)))
                           (weight (id (id)) (value (weight)))
                           (concept-weight (concept (concept)) (weight (weight))))
               )

  (conditional 'color-action
               :conditions '((object (state (state)) (id (id)))
                             (color (id (id)) (value (color))))
               :actions '((color2 (id (id)) (value (color)))))

  (conditional 'weight-action
               :conditions '((object (state (state)) (id (id)))
                             (weight (id (id)) (value (weight))))
               :actions '((weight2 (id (id)) (value (weight)))))

  (conditional 'concept-action
               :conditions '((object (state (state)) (id (id)))
                             (concept (id (id)) (value (concept))))
               :actions '((concept2 (id (id)) (value (concept)))))

  t)
  
; Color silver
(defun color-silver nil
  (cat-init)
  (cat-conds)
  (setq perceive-list '((perceive '((object (state 0) (id i1))
                                    (color (id i1) (value silver))
                                    ))))
  t)

(defun test-color-silver ()
  (color-silver)
  (d 1)
  (when from-regression
    (ppwm 'concept 'array info-stream) (ppwm 'color 'array info-stream) (ppwm 'alive 'array info-stream) (ppwm 'mobile 'array info-stream)
    (ppwm 'legs 'array info-stream) (ppwm 'weight nil info-stream)
    (ppfn 'concept2 nil '((constant wm-id i1) (argmax wm-value)) info-stream)
    (ppfn 'color2 nil '((constant wm-id i1) (argmax wm-value)) info-stream)
    (ppfn 'weight2 nil '((constant wm-id i1) (argmax wm-value)) info-stream)
    (print-global-decision-statistics info-stream)
    )
  )

; Color white
(defun color-white nil
  (cat-init)
  (cat-conds)
  (setq perceive-list '((perceive '((object (state 0) (id i1))
                                    (color (id i1) (value white))
                                    ))))
  t)

; Four legs and alive
(defun alive-true-legs-4 nil
  (cat-init)
  (cat-conds)
  (setq perceive-list '((perceive '((object (state 0) (id i1))
                                    (alive (id i1) (value true))
                                    (legs (id i1) (value 4))
                                    ))))
  t)

; Concept is human
(defun concept-human nil
  (cat-init)
  (cat-conds)
  (setq perceive-list '((perceive '((object (state 0) (id i1))
                                    (concept (id i1) (value human))
                                    ))))
  t)

; Weight is 200 pounds
(defun weight-200 nil
  (cat-init)
  (cat-conds)
  (setq perceive-list '((perceive '((object (state 0) (id i1))
                                    (weight (id i1) (value (200 201)))
                                    ))))
  t)

; Something specified for concept and all attributes
(defun cat-all nil
  (cat-init)
  (cat-conds)
  (setq perceive-list '((perceive '((object (state 0) (id i1))
                                    (alive (id i1) (value true))
                                    (legs (id i1) (value 4))
                                    (color (id i1) (value brown))
                                    (mobile (id i1) (value false))
                                    (weight (id i1) (value 100))
                                    (concept (id i1) (value dog)) 
                                    ))))
  t)

; Something specified for concept and all attributes except legs
(defun no-legs nil
  (cat-init)
  (cat-conds)
  (setq perceive-list '((perceive '((object (state 0) (id i1))
                                    (alive (id i1) (value true))
                                    (color (id i1) (value brown))
                                    (mobile (id i1) (value false))
                                    (weight (id i1) (value 100))
                                    (concept (id i1) (value dog))
                                    ))))
  t)

; Something specified for all attributes except legs
(defun no-concept-legs nil
  (cat-init)
  (cat-conds)
  (setq perceive-list '((perceive '((object (state 0) (id i1))
                                    (alive (id i1) (value true))
                                    (color (id i1) (value brown))
                                    (mobile (id i1) (value false))
                                    (weight (id i1) (value 100))
                                    ))))
  t)

; Something specified for concept and all attributes except weight
(defun no-weight nil
  (cat-init)
  (cat-conds)
  (setq perceive-list '((perceive '((object (state 0) (id i1))
                                    (alive (id i1) (value true))
                                    (legs (id i1) (value 4))
                                    (color (id i1) (value brown))
                                    (mobile (id i1) (value false))
                                    (concept (id i1) (value dog))
                                    ))))
  t)

; Something specified for all attributes except weight
(defun no-concept-weight nil
  (cat-init)
  (cat-conds)
  (setq perceive-list '((perceive '((object (state 0) (id i1))
                                    (alive (id i1) (value true))
                                    (legs (id i1) (value 4))
                                    (color (id i1) (value brown))
                                    (mobile (id i1) (value false))
                                    ))))
  t)

; Simplified semantic memory with just one attribute
(defun cat1 nil
  (init)
  (new-type 'id :constants '(i1 i2 i3))
  (new-type 'type :constants '(walker table dog human))
  (new-type 'color :constants '(silver brown white))

  (predicate 'concept :perception t :arguments '((id id) (value type %)))
  (predicate 'color :perception t :arguments '((id id) (value color %)))
  (predicate 'object :perception t :world 'closed :arguments '((state state) (id id)))

  ; Function predicates
  (predicate 'concept-f :arguments '((value type %))
             :function 1)
  (predicate 'concept-color :arguments '((concept type) (color color %))
             :function '((.95 walker silver) (.05 walker brown)
                               (.05 table silver) (.95 table brown)
                               (.05 dog silver) (.7 dog brown) (.25 dog white)
                               (.5 human brown) (.5 human white)))

  (setq pre-d '((ppwm 'concept) (ppwm 'color)))

  ; Prior on concept  
  (conditional 'concept
               :conditions '((object (state (state)) (id (id))))
               :condacts '((concept (id (id)) (value (concept)))
                           (concept-f (value (concept))))
               )

  ; Conditional distribution on attributes given concept
  (conditional 'concept-color*join
               :conditions '((object (state (state)) (id (id))))
               :condacts '((concept (id (id)) (value (concept)))
                           (color (id (id)) (value (color)))
                           (concept-color (concept (concept)) (color (color))))
               )

  t)

; Color silver
(defun color-silver1 nil
  (cat1)
  (setq perceive-list '((perceive '((object (state 0) (id i1))
                                    (color (id i1) (value silver))
                                    ))))
  t)

; Test whether negative domain elements are okay
; Simplified semantic memory with one attribute (negative number of legs)
(defun cat1n nil
  (init)
  (new-type 'id :constants '(i1 i2 i3))
  (new-type 'type :constants '(walker table dog human))
  (new-type 'n04 :numeric t :discrete t :min -4 :max 1)

  (predicate 'concept :perception t :arguments '((id id) (value type %)))
  (predicate 'legs :perception t :arguments '((id id) (value n04 %)))
  (predicate 'object :perception t :world 'closed :arguments '((state state) (id id)))

  ; Prior on concept  
  (conditional 'concept
               :conditions '((object (state (state)) (id (id))))
               :condacts '((concept (id (id)) (value (concept))))
               :function-variable-names '(concept)
               :function '((.25 *)))

  ; Conditional distribution on attributes given concept
  
  (conditional 'concept-legs
               :conditions '((object (state (state)) (id (id))))
               :condacts '((concept (id (id)) (value (concept)))
                           (legs (id (id)) (value (legs))))
               :function-variable-names '(concept legs)
               :function '((1 walker -4)
                               (.9 table -4) (.1 table -3)
                               (.01 dog -1) (.02 dog -2) (.1 dog -3) (.87 dog -4)
                               (.01 human 0) (.02 human -1) (.97 human -2)))
  )

; Color silver
(defun legs-2 nil
  (cat1n)
  (setq perceive-list '((perceive '((object (state 0) (id i1))
                                    (legs (id i1) (value -2))
                                    ))))
  t)

; Simplified semantic memory with two attributes
(defun cat2 nil
  (init)
  (new-type 'id :constants '(i1 i2 i3))
  (new-type 'type :constants '(walker table dog human))
  (new-type 'color :constants '(silver brown white))
  (new-type 'i04 :numeric t :discrete t :min 0 :max 5)

  (predicate 'concept :perception t :arguments '((id id) (value type %)))
  (predicate 'color :perception t :arguments '((id id) (value color %)))
  (predicate 'legs :perception t :arguments '((id id) (value i04 %)))
  (predicate 'object :perception t :world 'closed :arguments '((state state) (id id)))

  ; Prior on concept  
  (conditional 'concept
               :conditions '((object (state (state)) (id (id))))
               :condacts '((concept (id (id)) (value (concept))))
               :function-variable-names '(concept)
               :function '((.25 *)))

  ; Conditional distribution on attributes given concept

  (conditional 'concept-color
               :conditions '((object (state (state)) (id (id))))
               :condacts '((concept (id (id)) (value (concept)))
                           (color (id (id)) (value (color))))
               :function-variable-names '(concept color)
               :function '((.95 walker silver) (.05 walker brown)
                           (.05 table silver) (.95 table brown)
                           (.05 dog silver) (.7 dog brown) (.25 dog white)
                           (.5 human brown) (.5 human white)))
  
  (conditional 'concept-legs
               :conditions '((object (state (state)) (id (id))))
               :condacts '((concept (id (id)) (value (concept)))
                           (legs (id (id)) (value (legs))))
               :function-variable-names '(concept legs)
               :function '((1 walker 4)
                           (.9 table 4) (.1 table 3)
                           (.01 dog 1) (.02 dog 2) (.1 dog 3) (.87 dog 4)
                           (.01 human 0) (.02 human 1) (.97 human 2)))
  )

; Color silver
(defun color-silver2 nil
  (cat2)
  (setq perceive-list '((perceive '((object (state 0) (id i1))
                                    (color (id i1) (value silver))
                                    ))))
  t)

; Simplified semantic memory with two attributes (just condacts)
(defun ssm nil
  (init)
  (new-type 'type :constants '(walker table dog human))
  (new-type 'color :constants '(silver brown white))
  (new-type 'i04 :numeric t :discrete t :min 0 :max 5)

  (predicate 'concept :perception t :arguments '((value type %)))
  (predicate 'color :perception t :arguments '((value color %)))
  (predicate 'legs :perception t :arguments '((value i04 %)))

  (setq pre-d '((ppwm 'concept 'array) (ppwm 'color 'array) (ppwm 'legs 'array)))

  ; Prior on concept  
  (conditional 'concept
               :condacts '((concept (value (concept))))
               :function-variable-names '(concept)
               :function '((.25 *)))

  ; Conditional distribution on attributes given concept

  (conditional 'concept-color
               :condacts '((concept (value (concept)))
                           (color (value (color))))
               :function-variable-names '(concept color)
               :function '((.95 walker silver) (.05 walker brown)
                           (.05 table silver) (.95 table brown)
                           (.05 dog silver) (.7 dog brown) (.25 dog white)
                           (.5 human brown) (.5 human white)))
  
  (conditional 'concept-legs
               :condacts '((concept (value (concept)))
                           (legs (value (legs))))
               :function-variable-names '(concept legs)
               :function '((1 walker 4)
                           (.9 table 4) (.1 table 3)
                           (.01 dog 1) (.02 dog 2) (.1 dog 3) (.87 dog 4)
                           (.01 human 0) (.02 human 1) (.97 human 2)))
  )

; Color silver
(defun ssm-color-silver nil
  (ssm)
  (setq perceive-list '((perceive '((color (value silver))))))
  t)

; Simple program for testing episodic memory
; 0: No object
; 1: walker silver false true 4 10
; 2: human white true true 2 150
; 3: human brown true true 2 125
; 4: dog silver true true 4 50
; 5: silver
; 6: not alive, 4 legs
; 7: not alive, 2 legs
; 8: dog brown
; 9: walker, silver, alive
; 10: not alive
; Each episode is learned for the time slot that is one greater than the episode number

(defparameter *ep-per* '((perceive '((concept (value walker)) (color (value silver)) (alive (value false)) (mobile (value true)) (legs (value 4)) (weight (value 10))))
                         (perceive '((concept (value human)) (color (value white)) (alive (value true)) (mobile (value true)) (legs (value 2)) (weight (value 150))))
                         (perceive '((concept (value human)) (color (value brown)) (alive (value true)) (mobile (value true)) (legs (value 2)) (weight (value 125))))
                         (perceive '((concept (value dog)) (color (value silver)) (alive (value true)) (mobile (value true)) (legs (value 4)) (weight (value 50))))
                         (perceive '((concept (value walker)) (color (value *)) (alive (value *)) (mobile (value *)) (legs (value *)) (weight (value *))))
                         (perceive '((concept (value *)) (color (value silver)) (alive (value *)) (mobile (value *)) (legs (value *)) (weight (value *))))
                         (perceive '((concept (value *)) (color (value *)) (alive (value false)) (mobile (value *)) (legs (value 4)) (weight (value *))))
                         (perceive '((concept (value *)) (color (value *)) (alive (value false)) (mobile (value *)) (legs (value 2)) (weight (value *))))
                         (perceive '((concept (value dog)) (color (value brown)) (alive (value *)) (mobile (value *)) (legs (value *)) (weight (value *))))
                         (perceive '((concept (value walker)) (color (value silver)) (alive (value true)) (mobile (value *)) (legs (value *)) (weight (value *))))
                         (perceive '((concept (value *)) (color (value *)) (alive (value false)) (mobile (value *)) (legs (value *)) (weight (value *))))
                         )
  )
(defvar ep-per)

(defun episodic nil
  (init)
  (setq gdl-subtractive-normalization nil) ; Necessary so that give divisive normalization, and thus exponential rather than linear decay in time
  (setq exponential-product-gradient nil)
  (setq threshold-for-dense-connection 15) ; Makes it easier to understand messages at temporal variable
;  (setq trace-gdl '(time-cff time-concept*episodic-learn))
  (learn '(:e))
  (setq trace-perception t)
  (setq ep-per *ep-per*)
  (setq perceive-list '((eval (pop ep-per))))
  (setq pre-d '((format trace-stream "~&~%Temporal prior:") (pa 'time*episodic)
                (format trace-stream "~&~%Episodic retrieval:")
                (format trace-stream "~&  Time[~S]: " (best-in-plm (vn-posterior (predicate-from-name 'time*episodic)))) (ppvn 'time*episodic)
                (format trace-stream "~&  Concept[~S]: " (best-in-plm (vn-posterior (predicate-from-name 'concept*episodic)) 0)) (ppvn 'concept*episodic)
                (format trace-stream "~&  Color[~S]: " (best-in-plm (vn-posterior (predicate-from-name 'color*episodic)) 0)) (ppvn 'color*episodic)
                (format trace-stream "~&  Alive[~S]: " (best-in-plm (vn-posterior (predicate-from-name 'alive*episodic)) 0)) (ppvn 'alive*episodic)
                (format trace-stream "~&  Mobile[~S]: " (best-in-plm (vn-posterior (predicate-from-name 'mobile*episodic)) 0)) (ppvn 'mobile*episodic)
                (format trace-stream "~&  Legs[~S]: " (best-in-plm (vn-posterior (predicate-from-name 'legs*episodic)) 0)) (ppvn 'legs*episodic)
                (format trace-stream "~&  Weight[~S]: " (best-in-plm (vn-posterior (predicate-from-name 'weight*episodic)) 0)) (ppvn 'weight*episodic)))

;  (setq post-d '((format trace-stream "~&~%Concept episodic memory:") (pa 'time-concept*episodic-learn)))

  (new-type 'type :constants '(walker table dog human))
  (new-type 'color :constants '(silver brown white))
  (new-type 'i04 :numeric t :discrete t :min 0 :max 5)
  (new-type 'weight :numeric t :min 0 :max 500)

  (predicate 'concept :world 'closed :perception t :arguments '((state state) (value type !)))
  (predicate 'color :world 'closed :perception t :arguments '((state state) (value color !)))
  (predicate 'alive :world 'closed :perception t :arguments '((state state) (value boolean !)))
  (predicate 'mobile :world 'closed :perception t :arguments '((state state) (value boolean !)))
  (predicate 'legs :world 'closed :perception t :arguments '((state state) (value i04 !)))
  (predicate 'weight :world 'closed :perception t :arguments '((state state) (value weight !)))
  
  t)

(defun test-episodic nil
  (episodic)
  (d 12)
  )

; Replay a sequence of episodes starting from a retrieved one
(defparameter *ep-per-replay* '((perceive '((concept (value walker)) (color (value silver)) (alive (value false)) (mobile (value true)) (legs (value 4)) (weight (value 10))))
                                nil
                                (perceive '((concept (value human)) (color (value white)) (alive (value true)) (mobile (value true)) (legs (value 2)) (weight (value 150))))
                                nil
                                (perceive '((concept (value human)) (color (value brown)) (alive (value true)) (mobile (value true)) (legs (value 2)) (weight (value 125))))
                                nil
                                (perceive '((concept (value dog)) (color (value silver)) (alive (value true)) (mobile (value true)) (legs (value 4)) (weight (value 50))))
                                nil
                                (perceive '((concept (value walker)) (color (value *)) (alive (value *)) (mobile (value *)) (legs (value *)) (weight (value *))))
                                nil
                                (perceive '((concept (value *)) (color (value silver)) (alive (value *)) (mobile (value *)) (legs (value *)) (weight (value *))))
                                nil
                                (perceive '((concept (value *)) (color (value *)) (alive (value false)) (mobile (value *)) (legs (value 4)) (weight (value *))))
                                nil
                                (perceive '((concept (value *)) (color (value *)) (alive (value false)) (mobile (value *)) (legs (value 2)) (weight (value *))))
                                nil
                                (perceive '((concept (value dog)) (color (value brown)) (alive (value *)) (mobile (value *)) (legs (value *)) (weight (value *))))
                                nil
                                (perceive '((concept (value walker)) (color (value silver)) (alive (value true)) (mobile (value *)) (legs (value *)) (weight (value *))))
                                nil
                                (perceive '((concept (value *)) (color (value *)) (alive (value false)) (mobile (value *)) (legs (value *)) (weight (value *))))
                                )
  )

(defun episodic-replay nil
  (init '(replay))
  (setq gdl-subtractive-normalization nil) ; Necessary so that give divisive normalization, and thus exponential rather than linear decay in time
  (setq exponential-product-gradient nil)
  (setq threshold-for-dense-connection 15) ; Makes it easier to understand messages at temporal variable
;  (setq trace-gdl '(time-cff time-concept*episodic-learn))
  (learn '(:e))
  (setq trace-perception t)
  (setq ep-per *ep-per-replay*)
  (setq perceive-list '((eval (pop ep-per))))
  (setq pre-d '((format trace-stream "~&~%Temporal prior:") (pa 'time*episodic-cff)
                (format trace-stream "~&~%Episodic retrieval:")
                (format trace-stream "~&  Time[~S]: " (best-in-plm (vn-posterior (predicate-from-name 'time*episodic)))) (ppvn 'time*episodic)
                (format trace-stream "~&  Concept[~S]: " (best-in-plm (vn-posterior (predicate-from-name 'concept*episodic)) 0)) (ppvn 'concept*episodic)
                (format trace-stream "~&  Color[~S]: " (best-in-plm (vn-posterior (predicate-from-name 'color*episodic)) 0)) (ppvn 'color*episodic)
                (format trace-stream "~&  Alive[~S]: " (best-in-plm (vn-posterior (predicate-from-name 'alive*episodic)) 0)) (ppvn 'alive*episodic)
                (format trace-stream "~&  Mobile[~S]: " (best-in-plm (vn-posterior (predicate-from-name 'mobile*episodic)) 0)) (ppvn 'mobile*episodic)
                (format trace-stream "~&  Legs[~S]: " (best-in-plm (vn-posterior (predicate-from-name 'legs*episodic)) 0)) (ppvn 'legs*episodic)
                (format trace-stream "~&  Weight[~S]: " (best-in-plm (vn-posterior (predicate-from-name 'weight*episodic)) 0)) (ppvn 'weight*episodic)))

  (setq post-d '((format trace-stream "~&~%Replay Episode:") (ppfn 'time*replay)
                 (ppfn 'selected nil '((max wm-state) (argmax wm-operator)))))

  (new-type 'type :constants '(walker table dog human))
  (new-type 'color :constants '(silver brown white))
  (new-type 'i04 :numeric t :discrete t :min 0 :max 5)
  (new-type 'weight :numeric t :min 0 :max 500)

  (predicate 'concept :world 'closed :perception t :arguments '((state state) (value type !)))
  (predicate 'color :world 'closed :perception t :arguments '((state state) (value color !)))
  (predicate 'alive :world 'closed :perception t :arguments '((state state) (value boolean !)))
  (predicate 'mobile :world 'closed :perception t :arguments '((state state) (value boolean !)))
  (predicate 'legs :world 'closed :perception t :arguments '((state state) (value i04 !)))
  (predicate 'weight :world 'closed :perception t :arguments '((state state) (value weight !)))

  (predicate 'time*replay :world 'closed :arguments '((value time !)))

  ; Start replay at episode 1 when see episode 6 (not alive, 4 legs)
  (conditional 'start-replay
               :conditions '((time (value 13))
                             (time*episodic (value (t))))
               :actions '((time*replay (value (t))))
               )

  ; Propose a replay step whenever there is a replay time
  (conditional 'propose-replay
               :conditions '((state (state (s)))
                             (time*replay (value (t))))
               :actions '((selected (state (s)) (operator replay)))
               )

  ; Access content of episodic memory for replay step
  (conditional 'replay-access
               :conditions '((state (state (s)))
                             (selected (state (s)) (operator replay))
                             (time*replay (value (t))))
               :actions '((time*episodic (value (t))))
               )

  ; Increment replay time
  (conditional 'replay-increment
               :conditions '((state (state (s)))
                             (selected (state (s)) (operator replay))
                             (time*replay (value (t))))
               :actions '((time*replay (value (t 1))))
               )
  (when open-world-wmfns
    (evidence '((time*episodic .01)))
    )
  
  t)

(defun test-episodic-replay nil
  (episodic-replay)
  (d 25)
  )

; Program for learning a random sequence of episodes

; Get a random element from a discrete type (form its name)
(defun random-element (type-name)
  (let* ((type (type-from-name type-name))
         (span (stype-span type)))
    (unless type
      (error "Type unknown in attempt to generate random element")
      )
    (unless (or (stype-discrete type)
                (floatp (stype-span type))
                )
      (setq span (coerce span 'float))
      )
    (if (stype-constants type)
        (nth (random span) (stype-constants type))
      (random span)))
  )

; Create one random episode
; With probability (1 - prob-value) specify no specific value
(defun random-episode (predicates prob-value)
  (unless (and (>= prob-value 0) (<= prob-value 1))
    (error "Specified PROB-VALUE in RANDOM-EPISODE not in [0,1]")
    )
  (let (ep)
    (dolist (p predicates)
      (let (pep)
        (dolist (a (predicate-arguments p))
          (unless (eq (argument-name a) 'state)
            (push (list (argument-name a)
                        (if (< (random 1.0) prob-value)
                            (random-element (argument-type-name a))
                          '*)) pep)
            )
          )
        (push (cons (predicate-name p) (reverse pep)) ep)
        )
      )
    (list 'perceive (list 'quote (reverse ep)))
    )
  )

; Create the arguments for a random closed-world discrete selection state predicate
(defun random-cwdss-arguments (typev min-args max-args)
  (let ((args (list '(state state)))
        (nargs (+ (random (- max-args min-args)) min-args))
        (rank (length typev))
        )
    (dotimes (i (1- nargs))
      (push (list (gentemp "ARG") (stype-name (aref typev (random rank)))) args)
      )
    (push (list (gentemp "ARG") (stype-name (aref typev (random rank))) '!) args)
    (reverse args))
  )

; Create a random closed-world discrete selection state predicate
(defun random-cwdss-predicate (typev min-args max-args)
  (predicate (gentemp "PRED") :world 'closed :perception t :arguments (random-cwdss-arguments typev min-args max-args))
  )

; Generate a vector of types
(defun vector-of-types (max-domain)
  (let ((typev (init-vector max-domain)))
    (dotimes (i max-domain)
      (setf (aref typev i) (new-type (gentemp "TYPE") :numeric t :discrete t :min 0 :max (1+ i)))
      )
    typev)
  )

; Create a random set of episodic predicates
(defun random-episodic-predicates (min-preds max-preds min-args max-args max-domain)
  (let ((npreds (+ (random (- max-preds min-preds)) min-preds))
        (vts (vector-of-types max-domain))
        preds
        )
    (dotimes (i npreds)
      (push (random-cwdss-predicate vts min-args max-args) preds)
      )
    preds)
  )

; Global variable for tracking randomly generated episodic predicates
(defvar episodic-predicates)

; Create a list of random episodes
(defun random-episodes (nepisodes min-preds max-preds min-args max-args max-domain prob-value)
  (let (eps)
    (setq episodic-predicates (random-episodic-predicates min-preds max-preds min-args max-args max-domain))
    (dotimes (i nepisodes)
      (push (random-episode episodic-predicates prob-value) eps)
      )
    eps)
  )

; Learn a series of random episodes
(defun episodic-random (nepisodes)
  (init)
  (setq gdl-subtractive-normalization nil) ; Necessary so that give divisive normalization, and thus exponential rather than linear decay in time
  (learn '(:e))
  (setq trace-perception t)
  (setq ep-per (random-episodes nepisodes 1 4 1 3 4 .8))
  (setq perceive-list '((eval (pop ep-per))))
  (setq pre-d '((format trace-stream "~&~%Temporal prior:") (pa 'time*episodic-cff)))
  (dotimes (i (+ nepisodes 1))
    (d 1)
    )
  (dolist (p episodic-predicates)
    (format trace-stream "~&~%Episodic memory for predicate ~S:" (predicate-name p))
    (pa (concat-symbols `(time- ,(predicate-name p) *episodic-learn)) nil '((constant state 0)))
    )
  (dolist (timing (reverse (mapcar #'decision-statistics-run-time global-decision-statistics)))
    (format t "~S," timing)
    )
  )

; Program for simple linked production
(defun init-rule (&optional reverse)
  (init)
  (new-type 'id :constants '(i1 i2 i3 i4 i5 i6 i7))
  
  (predicate 'next :world 'closed :arguments '((id id) (value id)))

  (setq post-d '((ppfn 'next nil 'array)))
  
  (conditional 'trans
               :conditions '(
                             (next (id (a)) (value (b)))
                             (next (id (b)) (value (c)))
                             )
               :actions '((next (id (a)) (value (c))))
               )

  (when reverse
    (conditional 'reverse
                 :conditions '(
                               (next (id (a)) (value (b)))
                               )
                 :actions '((next (id (b)) (value (a))))
                 )
    )
  t)

; One possible match
(defun rule-one (&optional reverse)
  (init-rule reverse)
  (evidence '((next (id i1) (value i2))
              (next (id i2) (value i3))
              ))
  t)

(defun test-rule-one nil
  (rule-one)
  (d 1)
  (ppfn 'next nil 'array info-stream)
  )

; Two possible matches
(defun rule-two (&optional reverse)
  (init-rule reverse)
  (evidence '((next (id i1) (value i2))
              (next (id i2) (value i3))
              (next (id i4) (value i5))
              (next (id i5) (value i6))
              ))
  t)

; Program for simple linked production
(defun init-rule2 (&optional reverse)
  (init)
  (new-type 'id :constants '(i1 i2 i3 i4 i5 i6 i7))
  
  (predicate 'next :world 'closed :arguments '((id id) (value id) (unique boolean !)))

  (setq post-d '((ppwm 'next)))
  
  (conditional 'trans
               :conditions '(
                             (next (id (a)) (value (b)) (unique true))
                             (next (id (b)) (value (c)) (unique true))
                             )
               :actions '((next (id (a)) (value (c)) (unique true)))
               )

  (when reverse
    (conditional 'reverse
                 :conditions '(
                               (next (id (a)) (value (b)) (unique true))
                               )
                 :actions '((next (id (b)) (value (a)) (unique-true)))
                 )
    )
  t)

; One possible match
(defun rule-one2 (&optional reverse)
  (init-rule2 reverse)
  (evidence '((next (id i1) (value i2) (unique true))
              (next (id i2) (value i3) (unique true))
              ))
  t)

; Two possible matches
(defun rule-two2 (&optional reverse)
  (init-rule2 reverse)
  (evidence '((next (id i1) (value i2) (unique true))
              (next (id i2) (value i3) (unique true))
              (next (id i4) (value i5) (unique true))
              (next (id i5) (value i6) (unique true))
              ))
  t)

; Simple test for constraint processing with the two color problem
(defun constraint (&optional learn)
  (let (function)
    (init)
    (if learn
        (progn
          (learn '(:gd))
          (setq function 1)
          )
      (progn
        (learn)
        (setq function '((1 red blue) (1 blue red) (0 red red) (0 blue blue)))
        )
      )
    
    (new-type 'cell :constants '(c1 c2 c3 c4))
    (new-type 'color :constants '(red blue))
    
    (predicate 'assign :perception t :arguments '((cell cell) (color color %)))

    ; Function predicates
    (predicate 'constraint12 :arguments '((color1 color) (color2 color)))
    (predicate 'constraint13 :arguments '((color1 color) (color3 color)))
    (predicate 'constraint24 :arguments '((color2 color) (color4 color)))
    (predicate 'constraint34 :arguments '((color3 color) (color4 color)))

    (conditional 'c12*join
                 :condacts '(
                             (assign (cell c1) (color (color1)))
                             (assign (cell c2) (color (color2)))
                             (constraint12 (color1 (color1)) (color2 (color2)))
                             )
                 )
    
    (conditional 'c13
                 :condacts '(
                             (assign (cell c1) (color (color1)))
                             (assign (cell c3) (color (color3)))
                             (constraint13 (color1 (color1)) (color3 (color3)))
                             )
                 )
    
    (conditional 'c24
                 :condacts '(
                             (assign (cell c2) (color (color2)))
                             (assign (cell c4) (color (color4)))
                             (constraint24 (color2 (color2)) (color4 (color4)))
                             )
                 )
    
    (conditional 'c34
                 :condacts '(
                             (assign (cell c3) (color (color3)))
                             (assign (cell c4) (color (color4)))
                             (constraint34 (color3 (color3)) (color4 (color4)))
                             )
                 )

    ; Function conditionals

    (conditional 'c12*function
                 :condacts '((constraint12 (color1 (color1)) (color2 (color2))))
                 :function-variable-names '(color1 color2)
                 :normal '(color1 color2)
                 :function function
                 )

    (conditional 'c13*function
                 :condacts '((constraint13 (color1 (color1)) (color3 (color3))))
                 :function-variable-names '(color1 color3)
                 :normal '(color1 color3)
                 :function function
                 )

    (conditional 'c24*function
                 :condacts '((constraint24 (color2 (color2)) (color4 (color4))))
                 :function-variable-names '(color2 color4)
                 :normal '(color2 color4)
                 :function function
                 )

    (conditional 'c34*function
                 :condacts '((constraint34 (color3 (color3)) (color4 (color4))))
                 :function-variable-names '(color3 color4)
                 :normal '(color3 color4)
                 :function function
                 )

    )
  t)

; Test constraint program
(defun ct nil
  (constraint)
  (setq post-d '((ppwm 'assign)))
  (setq perceive-list '((perceive '(
                                   (assign (cell c1) (color blue))
                                   )
                                 )
                       ))
  (d 1)
  )

; Assign evidence
(defun ce (type)
  (if (eq type 'blue)
      (setq perceive-list '((perceive '(
                                        (assign (cell c1) (color blue))
                                        (assign (cell c2) (color red))
                                        (assign (cell c3) (color red))
                                        (assign (cell c4) (color blue))
                                        )
                                      )))
    (setq perceive-list '((perceive '(
                                        (assign (cell c1) (color red))
                                        (assign (cell c2) (color blue))
                                        (assign (cell c3) (color blue))
                                        (assign (cell c4) (color red))
                                        )
                                      ))))
  )

; Test what messages go into function nodes when full results in evidence
(defun cl (iterations)
  (constraint t)
  (setq post-d '((format trace-stream "~&") (pcfs)))
  (dotimes (i iterations)
    (ce 'blue)
    (d 1)
    (ce 'red)
    (d 1)
    )
  )


; Eight puzzle

; Print Eight Puzzle board as an array
(defun pepb (&optional state stream)
  (unless stream (setq stream trace-stream))
  (unless state (setq state 0))
  (pa 'board t `((constant wm-state ,state) (argmax wm-tile)) stream)
  )

; A program for the Eight Puzzle based on a 2D board
; No subgoaling
(defun ep7i (&optional el) ; Whether to do episodic learning
  (init '(left right up down))
  (when el (learn '(:e)))

  (setq pre-d '(
                )
        )

  (setq post-d '((ppfn 'selected)
                 (format trace-stream "~&~%")
                 (pepb)
                 (when episodic-learning (print-episodic-memory))
                 )
        )

  (new-type 'dimension :numeric t :min 0 :max 3)
  (new-type 'tile :numeric t :discrete t :min 0 :max 9)

  (predicate 'board :world 'closed :arguments '((state state) (x dimension) (y dimension) (tile tile !)))
  (predicate 'operator :world 'closed :persistent nil :arguments '((id operator %) (state state) (x dimension) (y dimension)))
  (predicate 'goal :world 'closed :arguments '((state state) (x dimension) (y dimension) (tile tile !)))
  (predicate 'success :world 'closed :arguments '((state state)))

  (conditional 'left
               :conditions '((state (state (s)))
                             (board (state (s)) (x (x)) (y (y)) (tile (t)))
                             (board (state (s)) (x (x -1)) (y (y)) (tile 0))
                             )
               :actions '(
                          (operator (state (s)) (id left) (x (x)) (y (y)))
                           )
               )

  (conditional 'right
               :conditions '((state (state (s)))
                             (board (state (s)) (x (x)) (y (y)) (tile (t)))
                             (board (state (s)) (x (x 1)) (y (y)) (tile 0))
                             )
               :actions '(
                          (operator (state (s)) (id right) (x (x)) (y (y)))
                           )
               )

  (conditional 'up
               :conditions '((state (state (s)))
                             (board (state (s)) (x (x)) (y (y)) (tile (t)))
                             (board (state (s)) (x (x)) (y (y -1)) (tile 0))
                             )
               :actions '(
                          (operator (state (s)) (id up) (x (x)) (y (y)))
                           )
               )

  (conditional 'down
               :conditions '((state (state (s)))
                             (board (state (s)) (x (x)) (y (y)) (tile (t)))
                             (board (state (s)) (x (x)) (y (y 1)) (tile 0))
                             )
               :actions '(
                          (operator (state (s)) (id down) (x (x)) (y (y)))
                           )
               )

  (conditional 'acceptable ; Make all operators on the current state acceptable
               :conditions '((state (state (s)))
                             (operator (id (o)) (state (s)) (x (x)) (y (y)))
                             )
               :actions '((selected (state (s)) (operator (o))))
               :function .1
               )

  (conditional 'move-left ; Move tile left (and blank right)
               :conditions '((state (state (s)))
                             (selected (state (s)) (operator left))
                             (operator (id left) (state (s)) (x (x)) (y (y)))
                             (board (state (s)) (x (x)) (y (y)) (tile (t)))
                             (board (state (s)) (x (x -1)) (y (y)) (tile 0))
                             )
               :actions '((board (state (s)) (x (x)) (y (y)) (tile 0))
                          (board - (state (s)) (x (x -1)) (y (y)) (tile 0))
                          (board (state (s)) (x (x -1)) (y (y)) (tile (t)))
                          (board - (state (s)) (x (x)) (y (y)) (tile (t)))
                          )
               )

  (conditional 'move-right ; Move tile right (and blank left)
               :conditions '((state (state (s)))
                             (selected (state (s)) (operator right))
                             (operator (id right) (state (s)) (x (x)) (y (y)))
                             (board (state (s)) (x (x)) (y (y)) (tile (t)))
                             (board (state (s)) (x (x 1)) (y (y)) (tile 0))
                             )
               :actions '((board (state (s)) (x (x)) (y (y)) (tile 0))
                          (board - (state (s)) (x (x 1)) (y (y)) (tile 0))
                          (board (state (s)) (x (x 1)) (y (y)) (tile (t)))
                          (board - (state (s)) (x (x)) (y (y)) (tile (t)))
                          )
               )

  (conditional 'move-up ; Move tile up (and blank down)
               :conditions '((state (state (s)))
                             (selected (state (s)) (operator up))
                             (operator (id up) (state (s)) (x (x)) (y (y)))
                             (board (state (s)) (x (x)) (y (y)) (tile (t)))
                             (board (state (s)) (x (x)) (y (y -1)) (tile 0))
                             )
               :actions '((board (state (s)) (x (x)) (y (y)) (tile 0))
                          (board - (state (s)) (x (x)) (y (y -1)) (tile 0))
                          (board (state (s)) (x (x)) (y (y -1)) (tile (t)))
                          (board - (state (s)) (x (x)) (y (y)) (tile (t)))
                          )
               )

  (conditional 'move-down ; Move tile down (and blank up)
               :conditions '((state (state (s)))
                             (selected (state (s)) (operator down))
                             (operator (id down) (state (s)) (x (x)) (y (y)))
                             (board (state (s)) (x (x)) (y (y)) (tile (t)))
                             (board (state (s)) (x (x)) (y (y 1)) (tile 0))
                             )
               :actions '((board (state (s)) (x (x)) (y (y)) (tile 0))
                          (board - (state (s)) (x (x)) (y (y 1)) (tile 0))
                          (board (state (s)) (x (x)) (y (y 1)) (tile (t)))
                          (board - (state (s)) (x (x)) (y (y)) (tile (t)))
                          )
               )

  (conditional 'left-best ; Make operators best that move tile into goal location
               :conditions '((state (state (s)))
                             (operator (id left) (state (s)) (x (x)) (y (y)))
                             (board (state (s)) (x (x)) (y (y)) (tile (t)))
                             (goal (state (s)) (x (x -1)) (y (y)) (tile (t)))
                             )
               :actions '((selected (state (s)) (operator left)))
               )

  (conditional 'goal-reject ; Reject an operator that moves a tile from its desired location
               :conditions '((state (state (s)))
                             (operator (id (o)) (state (s)) (x (x)) (y (y)))
                             (goal (state (s)) (x (x)) (y (y)) (tile (t)))
                             (board (state (s)) (x (x)) (y (y)) (tile (t)))
                             )
               :actions '((selected - (state (s)) (operator (o))))
               )

  (conditional 'previous-reject-left-right ; Reject left operator when previous is right
               :conditions '((state (state (s)))
                             (selected (state (s)) (operator right))
                             (operator (state (s)) (id right) (x (px)) (y (py)))
                             (operator (state (s)) (id left) (x (x)) (y (y)))
                             )
               :actions '((selected - (state (s)) (operator left))
                          )
               )

  (conditional 'previous-reject-right-left ; Reject right operator when previous is left
               :conditions '((state (state (s)))
                             (selected (state (s)) (operator left))
                             (operator (state (s)) (id left) (x (px)) (y (py)))
                             (operator (state (s)) (id right) (x (x)) (y (y)))
                             )
               :actions '((selected - (state (s)) (operator right))
                          )
               )

  (conditional 'previous-reject-up-down ; Reject up operator when previous is down
               :conditions '((state (state (s)))
                             (selected (state (s)) (operator down))
                             (operator (state (s)) (id down) (x (px)) (y (py)))
                             (operator (state (s)) (id up) (x (x)) (y (y)))
                             )
               :actions '((selected - (state (s)) (operator up))
                          )
               )

  (conditional 'previous-reject-down-up ; Reject down operator when previous is up
               :conditions '((state (state (s)))
                             (selected (state (s)) (operator up))
                             (operator (state (s)) (id up) (x (px)) (y (py)))
                             (operator (state (s)) (id down) (x (x)) (y (y)))
                             )
               :actions '((selected - (state (s)) (operator down))
                          )
               )

  (conditional 'goal-test
               :conditions '((state (state (s)))
                             (board (state (s)) (x (x1)) (y (y1)) (tile 1))
                             (goal (state (s)) (x (x1)) (y (y1)) (tile 1))
                             (board (state (s)) (x (x2)) (y (y2)) (tile 2))
                             (goal (state (s)) (x (x2)) (y (y2)) (tile 2))
                             (board (state (s)) (x (x3)) (y (y3)) (tile 3))
                             (goal (state (s)) (x (x3)) (y (y3)) (tile 3))
                             (board (state (s)) (x (x4)) (y (y4)) (tile 4))
                             (goal (state (s)) (x (x4)) (y (y4)) (tile 4))
                             (board (state (s)) (x (x5)) (y (y5)) (tile 5))
                             (goal (state (s)) (x (x5)) (y (y5)) (tile 5))
                             (board (state (s)) (x (x6)) (y (y6)) (tile 6))
                             (goal (state (s)) (x (x6)) (y (y6)) (tile 6))
                             (board (state (s)) (x (x7)) (y (y7)) (tile 7))
                             (goal (state (s)) (x (x7)) (y (y7)) (tile 7))
                             (board (state (s)) (x (x8)) (y (y8)) (tile 8))
                             (goal (state (s)) (x (x8)) (y (y8)) (tile 8))
                             )
               :actions '((success (state (s)))
                          )
               )

  (conditional 'halt
               :conditions '((success (state 0)))
               :actions '((halt))
               )

  t)

; Initialize board and goal for 2d 8P 
(defun ep7e nil
; Board
; 123
; 8 5
; 746
  (evidence '((board (state 0) (x 0) (y 0) (tile 1))
              (board (state 0) (x 1) (y 0) (tile 2))
              (board (state 0) (x 2) (y 0) (tile 3))
              (board (state 0) (x 0) (y 1) (tile 8))
              (board (state 0) (x 1) (y 1) (tile 0))
              (board (state 0) (x 2) (y 1) (tile 5))
              (board (state 0) (x 0) (y 2) (tile 7))
              (board (state 0) (x 1) (y 2) (tile 4))
              (board (state 0) (x 2) (y 2) (tile 6))
; Goal
; 123
; 8 4
; 765
              (goal (state 0) (x 0) (y 0) (tile 1))
              (goal (state 0) (x 1) (y 0) (tile 2))
              (goal (state 0) (x 2) (y 0) (tile 3))
              (goal (state 0) (x 0) (y 1) (tile 8))
              (goal (state 0) (x 1) (y 1) (tile 0))
              (goal (state 0) (x 2) (y 1) (tile 4))
              (goal (state 0) (x 0) (y 2) (tile 7))
              (goal (state 0) (x 1) (y 2) (tile 6))
              (goal (state 0) (x 2) (y 2) (tile 5))
              )
            )
  )

; Initialize board and goal for 2d 8P
(defun ep7e2 nil
; Board
; 123
; 845
; 7 6
  (evidence '((board (state 0) (x 0) (y 0) (tile 1))
              (board (state 0) (x 1) (y 0) (tile 2))
              (board (state 0) (x 2) (y 0) (tile 3))
              (board (state 0) (x 0) (y 1) (tile 8))
              (board (state 0) (x 1) (y 1) (tile 4))
              (board (state 0) (x 2) (y 1) (tile 5))
              (board (state 0) (x 0) (y 2) (tile 7))
              (board (state 0) (x 1) (y 2) (tile 0))
              (board (state 0) (x 2) (y 2) (tile 6))
; Goal
; 123
; 8 4
; 765
              (goal (state 0) (x 0) (y 0) (tile 1))
              (goal (state 0) (x 1) (y 0) (tile 2))
              (goal (state 0) (x 2) (y 0) (tile 3))
              (goal (state 0) (x 0) (y 1) (tile 8))
              (goal (state 0) (x 1) (y 1) (tile 0))
              (goal (state 0) (x 2) (y 1) (tile 4))
              (goal (state 0) (x 0) (y 2) (tile 7))
              (goal (state 0) (x 1) (y 2) (tile 6))
              (goal (state 0) (x 2) (y 2) (tile 5))
              )
            )
  )

; Initialize board and goal for 2d 8P
(defun ep7e3 nil
; Board
; 123
; 67
; 548
  (evidence '((board (state 0) (x 0) (y 0) (tile 1))
              (board (state 0) (x 1) (y 0) (tile 2))
              (board (state 0) (x 2) (y 0) (tile 3))
              (board (state 0) (x 0) (y 1) (tile 6))
              (board (state 0) (x 1) (y 1) (tile 7))
              (board (state 0) (x 2) (y 1) (tile 0))
              (board (state 0) (x 0) (y 2) (tile 5))
              (board (state 0) (x 1) (y 2) (tile 4))
              (board (state 0) (x 2) (y 2) (tile 8))
; Goal
; 123
; 8 4
; 765
              (goal (state 0) (x 0) (y 0) (tile 1))
              (goal (state 0) (x 1) (y 0) (tile 2))
              (goal (state 0) (x 2) (y 0) (tile 3))
              (goal (state 0) (x 0) (y 1) (tile 8))
              (goal (state 0) (x 1) (y 1) (tile 0))
              (goal (state 0) (x 2) (y 1) (tile 4))
              (goal (state 0) (x 0) (y 2) (tile 7))
              (goal (state 0) (x 1) (y 2) (tile 6))
              (goal (state 0) (x 2) (y 2) (tile 5))
              )
            )
  )

(defun ep7 (&optional el)
;  (learn)
  (ep7i el)
  (ep7e)
  t)

(defun ep72 (&optional el)
;  (learn)
  (ep7i el)
  (ep7e2)
  t)

(defun ep73 (&optional el)
;  (learn)
  (ep7i el)
  (ep7e3)
  t)

; A program for the Eight Puzzle based on a 2D board
; No subgoaling
(defun ep8i (&optional el) ; Whether to do episodic learning
  (init)
  (setq gdl-subtractive-normalization nil)
  (when el
    (learn '(:e))
;    (setq trace-gdl '(time-selected*episodic-learn time-cff))
    )

  (when episodic-learning
    (setq pre-d '((format trace-stream "~&~%Operator retrieved from episodic memory:~%")
                   (ppvn 'selected*episodic)))
    )

  (setq post-d '((ppfn 'selected nil '((max wm-state) (argmax wm-operator)))
                 (format trace-stream "~&~%")
                 (pepb)
                 (when episodic-learning
                   (format trace-stream "~&~%Episodic memory:~%")
;                   (print-episodic-memory)
;                   (pplm 'time-board*next)
                   (pa 'time)
                   (format trace-stream "~&~%")
;                   (parray 'time-selected*episodic-learn nil '((constant state 0) (argmax operator)))
                   (parray 'time-selected*episodic-learn nil '((constant wm-state 0)))
                   (parray 'time-selected*episodic-learn nil '((constant wm-state 0) (argmax wm-operator)))
                   )
                 )
        )

  (new-type 'dimension :numeric t :min 0 :max 3)
  (new-type 'tile :numeric t :discrete t :min 0 :max 9)

  (predicate 'board :world 'closed :arguments '((state state) (x dimension) (y dimension) (tile tile !)))
  (predicate 'goal :world 'closed :arguments '((state state) (x dimension) (y dimension) (tile tile !)))
  (predicate 'success :world 'closed :arguments '((state state)))

  (init-operators 'type 'tile)

  (conditional 'left-acceptable
               :conditions '((state (state (s)))
                             (board (state (s)) (x (x)) (y (y)) (tile (t)))
                             (board (state (s)) (x (x -1)) (y (y)) (tile 0))
                             )
               :actions '((selected (state (s)) (operator (t))))
               :function .1
               )

  (conditional 'right-acceptable
               :conditions '((state (state (s)))
                             (board (state (s)) (x (x)) (y (y)) (tile (t)))
                             (board (state (s)) (x (x 1)) (y (y)) (tile 0))
                             )
               :actions '((selected (state (s)) (operator (t))))
               :function .1
               )

  (conditional 'up-acceptable
               :conditions '((state (state (s)))
                             (board (state (s)) (x (x)) (y (y)) (tile (t)))
                             (board (state (s)) (x (x)) (y (y -1)) (tile 0))
                             )
               :actions '((selected (state (s)) (operator (t))))
               :function .1
               )

  (conditional 'down-acceptable
               :conditions '((state (state (s)))
                             (board (state (s)) (x (x)) (y (y)) (tile (t)))
                             (board (state (s)) (x (x)) (y (y 1)) (tile 0))
                             )
               :actions '((selected (state (s)) (operator (t))))
               :function .1
               )

  (conditional 'move-left ; Move tile left (and blank right)
               :conditions '((state (state (s)))
                             (selected (state (s)) (operator (t)))
                             (board (state (s)) (x (x)) (y (y)) (tile (t)))
                             (board (state (s)) (x (x -1)) (y (y)) (tile 0))
                             )
               :actions '((board (state (s)) (x (x)) (y (y)) (tile 0))
                          (board (state (s)) (x (x -1)) (y (y)) (tile (t)))
                          )
               )

  (conditional 'move-right ; Move tile right (and blank left)
               :conditions '((state (state (s)))
                             (selected (state (s)) (operator (t)))
                             (board (state (s)) (x (x)) (y (y)) (tile (t)))
                             (board (state (s)) (x (x 1)) (y (y)) (tile 0))
                             )
               :actions '((board (state (s)) (x (x)) (y (y)) (tile 0))
                          (board (state (s)) (x (x 1)) (y (y)) (tile (t)))
                          )
               )

  (conditional 'move-up ; Move tile up (and blank down)
               :conditions '((state (state (s)))
                             (selected (state (s)) (operator (t)))
                             (board (state (s)) (x (x)) (y (y)) (tile (t)))
                             (board (state (s)) (x (x)) (y (y -1)) (tile 0))
                             )
               :actions '((board (state (s)) (x (x)) (y (y)) (tile 0))
                          (board (state (s)) (x (x)) (y (y -1)) (tile (t)))
                          )
               )

  (conditional 'move-down ; Move tile down (and blank up)
               :conditions '((state (state (s)))
                             (selected (state (s)) (operator (t)))
                             (board (state (s)) (x (x)) (y (y)) (tile (t)))
                             (board (state (s)) (x (x)) (y (y 1)) (tile 0))
                             )
               :actions '((board (state (s)) (x (x)) (y (y)) (tile 0))
                          (board (state (s)) (x (x)) (y (y 1)) (tile (t)))
                          )
               )

  (conditional 'goal-left-best ; Make left operator best that moves tile into goal location
               :conditions '((state (state (s)))
                             (board (state (s)) (x (x)) (y (y)) (tile (t)))
                             (board (state (s)) (x (x -1)) (y (y)) (tile 0))
                             (goal (state (s)) (x (x -1)) (y (y)) (tile (t)))
                             )
               :actions '((selected (state (s)) (operator (t))))
               )

  (conditional 'goal-right-best ; Make right operator best that moves tile into goal location
               :conditions '((state (state (s)))
                             (board (state (s)) (x (x)) (y (y)) (tile (t)))
                             (board (state (s)) (x (x 1)) (y (y)) (tile 0))
                             (goal (state (s)) (x (x 1)) (y (y)) (tile (t)))
                             )
               :actions '((selected (state (s)) (operator (t))))
               )

  (conditional 'goal-up-best ; Make up operator best that moves tile into goal location
               :conditions '((state (state (s)))
                             (board (state (s)) (x (x)) (y (y)) (tile (t)))
                             (board (state (s)) (x (x)) (y (y -1)) (tile 0))
                             (goal (state (s)) (x (x)) (y (y -1)) (tile (t)))
                             )
               :actions '((selected (state (s)) (operator (t))))
               )

  (conditional 'goal-down-best ; Make down operator best that moves tile into goal location
               :conditions '((state (state (s)))
                             (board (state (s)) (x (x)) (y (y)) (tile (t)))
                             (board (state (s)) (x (x)) (y (y 1)) (tile 0))
                             (goal (state (s)) (x (x)) (y (y 1)) (tile (t)))
                             )
               :actions '((selected (state (s)) (operator (t))))
               )

  (conditional 'goal-reject ; Reject an operator that moves a tile from its desired location
               :conditions '((state (state (s)))
                             (goal (state (s)) (x (x)) (y (y)) (tile (t)))
                             (board (state (s)) (x (x)) (y (y)) (tile (t)))
                             )
               :actions '((selected - (state (s)) (operator (t))))
               )

  (conditional 'goal-test
               :conditions '((state (state (s)))
                             (board (state (s)) (x (x1)) (y (y1)) (tile 1))
                             (goal (state (s)) (x (x1)) (y (y1)) (tile 1))
                             (board (state (s)) (x (x2)) (y (y2)) (tile 2))
                             (goal (state (s)) (x (x2)) (y (y2)) (tile 2))
                             (board (state (s)) (x (x3)) (y (y3)) (tile 3))
                             (goal (state (s)) (x (x3)) (y (y3)) (tile 3))
                             (board (state (s)) (x (x4)) (y (y4)) (tile 4))
                             (goal (state (s)) (x (x4)) (y (y4)) (tile 4))
                             (board (state (s)) (x (x5)) (y (y5)) (tile 5))
                             (goal (state (s)) (x (x5)) (y (y5)) (tile 5))
                             (board (state (s)) (x (x6)) (y (y6)) (tile 6))
                             (goal (state (s)) (x (x6)) (y (y6)) (tile 6))
                             (board (state (s)) (x (x7)) (y (y7)) (tile 7))
                             (goal (state (s)) (x (x7)) (y (y7)) (tile 7))
                             (board (state (s)) (x (x8)) (y (y8)) (tile 8))
                             (goal (state (s)) (x (x8)) (y (y8)) (tile 8))
                             )
               :actions '((success (state (s)))
                          )
               )

  (conditional 'halt
               :conditions '((success (state 0)))
               :actions '((halt))
               )
  t)

(defun ep8 (&optional el)
;  (learn)
  (ep8i el)
  (ep7e)
  t)

(defun ep82 (&optional el)
;  (learn)
  (ep8i el)
  (ep7e2)
  t)

; See when episodically learned operators are retrieved
(defun ep82e (&optional retrieval-cycles)
  (ep8i t)
  (ep7e2)
  (d 7)
  (learn)
  (conditional 'suggest-episodic-operator
               :conditions '((selected*episodic (state (s)) (operator (o))))
               :actions '((selected (state (s)) (operator (o))))
               )
  (ep7e2)
  (evidence '((success - (state *)) (halt -)))
  (when retrieval-cycles
    (d retrieval-cycles)
    )
  )

(defun test-eight-puzzle (&optional el)
  (ep82 el)
  (d 10)
  (when episodic-learning
    (format info-stream "~&~%")
    (pa 'time-board*episodic-learn nil '((constant wm-state 0) (argmax wm-tile)))
    (pa 'time-selected*episodic-learn nil '((constant wm-state 0)))
    )
  )

(defun test-print-eight-puzzle ()
  (test-eight-puzzle)
  (pa 'board t))

(defun ep83 (&optional el)
;  (learn)
  (ep8i el)
  (ep7e3)
  t)

; A program for the Eight Puzzle based on a 2D board
; Gradient ascent based on comparison of board and goal
(defun ep9i (&optional el) ; Whether to do episodic learning
  (init)
  (setq gdl-subtractive-normalization nil)
  (when el
    (learn '(:e))
;    (setq trace-gdl '(time-selected*episodic-learn time-cff))
    )

  (setq post-d '((ppfn 'selected nil '((max wm-state) (argmax wm-operator)))
                 (format trace-stream "~&~%State 0:~&")
                 (pepb 0)
                 (when (> bottom-state 0)
                   (ppwm 'operator-evaluation)
                   )
                 (when (> bottom-state 1)
                   (format trace-stream "~&~%State 2:~&")
                   (pepb 2)
                   (ppwm 'state-evaluation)
                   )
                 (when episodic-learning
                   (format trace-stream "~&~%Episodic memory:~%")
                   (pa 'time)
                   (format trace-stream "~&~%")
                   (parray 'time-selected*episodic-learn)
                   (format trace-stream "~&~%Operator retrieved from episodic memory:~%")
                   (ppvn 'selected*episodic)
                   )
                 )
        )

  (new-type 'dimension :numeric t :min 0 :max 3)
  (new-type 'tile :numeric t :discrete t :min 0 :max 9)

  (init-operators 'type 'tile t)

  (predicate 'board :world 'closed :arguments '((state state) (x dimension) (y dimension) (tile tile !)))
  (predicate 'board*e :arguments '((state state) (x dimension %) (y dimension %) (tile tile %)))
  (predicate 'goal :world 'closed :arguments '((state state) (x dimension) (y dimension) (tile tile !)))
  (predicate 'goal*e :arguments '((state state) (x dimension %) (y dimension %) (tile tile %)))
  (predicate 'success :world 'closed :arguments '((state state)))
  (predicate 'state-evaluation :arguments '((state state)))
  (predicate 'operator-evaluation :world 'closed :no-normalize t :arguments '((state state) (operator operator %)))
  (predicate 'completed :world 'closed :arguments '((state state) (operator operator)))
  (predicate 'simulated :world 'closed :arguments '((state state) (operator operator) (status flag !))) ; Flag added so can't chain on it until decision

  (conditional 'left-acceptable
               :conditions '((state (state (s)))
                             (board (state (s)) (x (x)) (y (y)) (tile (t)))
                             (board (state (s)) (x (x -1)) (y (y)) (tile 0))
                             (operator (tile (t)) (operator (o)))
                             )
               :actions '((selected (state (s)) (operator (o))))
               :function .1
               )

  (conditional 'right-acceptable
               :conditions '((state (state (s)))
                             (board (state (s)) (x (x)) (y (y)) (tile (t)))
                             (board (state (s)) (x (x 1)) (y (y)) (tile 0))
                             (operator (tile (t)) (operator (o)))
                             )
               :actions '((selected (state (s)) (operator (o))))
               :function .1
               )

  (conditional 'up-acceptable
               :conditions '((state (state (s)))
                             (board (state (s)) (x (x)) (y (y)) (tile (t)))
                             (board (state (s)) (x (x)) (y (y -1)) (tile 0))
                             (operator (tile (t)) (operator (o)))
                             )
               :actions '((selected (state (s)) (operator (o))))
               :function .1
               )

  (conditional 'down-acceptable
               :conditions '((state (state (s)))
                             (board (state (s)) (x (x)) (y (y)) (tile (t)))
                             (board (state (s)) (x (x)) (y (y 1)) (tile 0))
                             (operator (tile (t)) (operator (o)))
                             )
               :actions '((selected (state (s)) (operator (o))))
               :function .1
               )

  (conditional 'move-left ; Move tile left (and blank right)
               :conditions '((state (state (s)))
                             (selected (state (s)) (operator (o)))
                             (operator (tile (t)) (operator (o)))
                             (board (state (s)) (x (x)) (y (y)) (tile (t)))
                             (board (state (s)) (x (x -1)) (y (y)) (tile 0))
                             )
               :actions '((board (state (s)) (x (x)) (y (y)) (tile 0))
                          (board (state (s)) (x (x -1)) (y (y)) (tile (t)))
                          )
               )

  (conditional 'move-right ; Move tile right (and blank left)
               :conditions '((state (state (s)))
                             (selected (state (s)) (operator (o)))
                             (operator (tile (t)) (operator (o)))
                             (board (state (s)) (x (x)) (y (y)) (tile (t)))
                             (board (state (s)) (x (x 1)) (y (y)) (tile 0))
                             )
               :actions '((board (state (s)) (x (x)) (y (y)) (tile 0))
                          (board (state (s)) (x (x 1)) (y (y)) (tile (t)))
                          )
               )

  (conditional 'move-up ; Move tile up (and blank down)
               :conditions '((state (state (s)))
                             (selected (state (s)) (operator (o)))
                             (operator (tile (t)) (operator (o)))
                             (board (state (s)) (x (x)) (y (y)) (tile (t)))
                             (board (state (s)) (x (x)) (y (y -1)) (tile 0))
                             )
               :actions '((board (state (s)) (x (x)) (y (y)) (tile 0))
                          (board (state (s)) (x (x)) (y (y -1)) (tile (t)))
                          )
               )

  (conditional 'move-down ; Move tile down (and blank up)
               :conditions '((state (state (s)))
                             (selected (state (s)) (operator (o)))
                             (operator (tile (t)) (operator (o)))
                             (board (state (s)) (x (x)) (y (y)) (tile (t)))
                             (board (state (s)) (x (x)) (y (y 1)) (tile 0))
                             )
               :actions '((board (state (s)) (x (x)) (y (y)) (tile 0))
                          (board (state (s)) (x (x)) (y (y 1)) (tile (t)))
                          )
               )

  (conditional 'goal-test
               :conditions '((state (state (s)))
                             (board (state (s)) (x (x1)) (y (y1)) (tile 1))
                             (goal (state (s)) (x (x1)) (y (y1)) (tile 1))
                             (board (state (s)) (x (x2)) (y (y2)) (tile 2))
                             (goal (state (s)) (x (x2)) (y (y2)) (tile 2))
                             (board (state (s)) (x (x3)) (y (y3)) (tile 3))
                             (goal (state (s)) (x (x3)) (y (y3)) (tile 3))
                             (board (state (s)) (x (x4)) (y (y4)) (tile 4))
                             (goal (state (s)) (x (x4)) (y (y4)) (tile 4))
                             (board (state (s)) (x (x5)) (y (y5)) (tile 5))
                             (goal (state (s)) (x (x5)) (y (y5)) (tile 5))
                             (board (state (s)) (x (x6)) (y (y6)) (tile 6))
                             (goal (state (s)) (x (x6)) (y (y6)) (tile 6))
                             (board (state (s)) (x (x7)) (y (y7)) (tile 7))
                             (goal (state (s)) (x (x7)) (y (y7)) (tile 7))
                             (board (state (s)) (x (x8)) (y (y8)) (tile 8))
                             (goal (state (s)) (x (x8)) (y (y8)) (tile 8))
                             )
               :actions '((success (state (s)))
                          )
               )

  ; Make evaluation operators best in subgoal for tied operators
  (conditional 'evaluate-best
               :conditions '((state (state (s)))
                             (impasse (state (s)) (type tie) (operator (o)))
                             (evaluate-operator (operator (eo)) (evaluate (o)))
                             )
               :actions '((selected (state (s)) (operator (eo))))
               )

  ;Reject completed evaluation operators
  (conditional 'evaluate-reject
               :conditions '((state (state (s)))
                             (impasse (state (s)) (type tie) (operator (o)))
                             (evaluate-operator (operator (eo)) (evaluate (o)))
                             (completed (state (s)) (operator (eo)))
                             )
               :actions '((selected - (state (s)) (operator (eo))))
               )

  ; Make operator to be evaluated best in no-change for evaluation operator
  (conditional 'evaluation-operator-best
               :conditions '((state (state (s)))
                             (impasse (state (s)) (type no-change) (operator (eo)))
                             (evaluate-operator (operator (eo)) (evaluate (o))))
               :actions '((selected (state (s)) (operator (o))))
               )

  ; Copy down board for use in evaluation
  (conditional 'evaluation-board
               :conditions '((state (state (s)))
                             (impasse (state (s)) (type no-change) (operator (eo)))
                             (evaluate-operator (operator (eo)))
                             (impasse (state (s -1)) (type tie))
                             (board (state (s -2)) (x (x)) (y (y)) (tile (t)))
                             (board - (state (s)))
                             )
               :actions '((board (state (s)) (x (x)) (y (y)) (tile (t))))
               )

  ; Copy down goals for use in evaluation
  (conditional 'evaluation-goal
               :conditions '((state (state (s)))
                             (impasse (state (s)) (type no-change) (operator (eo)))
                             (evaluate-operator (operator (eo)))
                             (impasse (state (s -1)) (type tie))
                             (goal (state (s -2)) (x (x)) (y (y)) (tile (t)))
                             )
               :actions '((goal (state (s)) (x (x)) (y (y)) (tile (t))))
               )

  ; Convert board into all unique for evaluation
  (conditional 'board-unique
               :conditions '((board (state (s)) (x (x)) (y (y)) (tile (t))))
               :condacts '((board*e (state (s)) (x (x)) (y (y)) (tile (t))))
               )

  ; Convert goal into all unique for evaluation
  (conditional 'goal-unique
               :conditions '((goal (state (s)) (x (x)) (y (y)) (tile (t))))
               :condacts '((goal*e (state (s)) (x (x)) (y (y)) (tile (t))))
               )

  ; Mark that simulated operator has run
  (conditional 'operator-simulated
               :conditions '((state (state (s)))
                             (selected (state (s)) (operator (o)))
                             (impasse (state (s)) (type no-change) (operator (eo)))
                             (evaluate-operator (evaluate (eo)) (operator (o)))
                             )
               :actions '((simulated (state (s)) (operator (o)) (status true)))
               )
               

  ; Compute an evaluation for a state by comparing with goal and summarizing everything out
  (conditional 'evaluate-state
               :conditions '((state (state (s)))
                             (board*e (state (s)) (x (x)) (y (y)) (tile (t)))
                             (goal*e (state (s)) (x (x)) (y (y)) (tile (t)))
                             )
               :actions '((state-evaluation (state (s))))
               )

  ; When operator simulated, return evaluation
  (conditional 'return-state-evaluation
               :conditions '((state (state (s)))
                             (simulated (state (s)) (operator (o)) (status true))
                             (state-evaluation (state (s)))
                             (impasse (state (s)) (type no-change) (operator (eo)))
                             (evaluate-operator (evaluate (o)) (operator (eo)))
                             (selected (state (s -1)) (operator (eo)))
                             )
               :actions '((operator-evaluation (state (s -1)) (operator (o))))
               )

  ; When operator simulated, return completion
  (conditional 'return-completion
               :conditions '((state (state (s)))
                             (simulated (state (s)) (operator (o)) (status true))
                             (impasse (state (s)) (type no-change) (operator (eo)))
                             (evaluate-operator (evaluate (o)) (operator (eo)))
                             (selected (state (s -1)) (operator (eo)))
                             )
               :actions '((completed (state (s -1)) (operator (eo))))
               )

  ; Return evaluations on offer operators once they have all been evaluated
  (conditional 'return-operator-evaluations
               :conditions '((state (state (s)))
                             (impasse (state (s)) (type tie) (operator (o)))
                             (impasse (state (s 1)) (type none))
                             (operator-evaluation (state (s)) (operator (o)))
                             )
               :actions '((selected (state (s -1)) (operator (o)))
                          )
               )

  ; Return evaluations on offer operators once they have all been evaluated
  (conditional 'select-randomly-after-impasse
               :conditions '((state (state (s)))
                             (impasse (state (s)) (type tie) (operator (o)))
                             (impasse (state (s 1)) (type none))
                             )
               :actions '((detect-impasses - (state (s -1)) (value true))
                          )
               )

  (conditional 'halt
               :conditions '((success (state 0)))
               :actions '((halt))
               )
  t)

(defun ep9 (&optional el)
;  (learn)
  (ep9i el)
  (ep7e)
  t)

(defun ep92 (&optional el)
;  (learn)
  (ep9i el)
  (ep7e2)
  t)

(defun ep93 (&optional el)
;  (learn)
  (ep9i el)
  (ep7e3)
  t)

;-----------
; A program for the Eight Puzzle based on a 2D board
; Gradient ascent based on automated comparison of board and goal
(defun ep10i (&optional el) ; Whether to do episodic learning
  (init)
  (setq compute-progress t)
  (setq trace-attention t)
  (setq gdl-subtractive-normalization nil)
  (when el
    (learn '(:e))
;    (setq trace-gdl '(time-selected*episodic-learn time-cff))
    )

  (setq post-d '((ppfn 'selected nil '((argmax wm-operator)))
                 (format trace-stream "~&~%State 0:~&")
                 (pepb 0)
                 (when (> bottom-state 0)
                   (ppwm 'operator-evaluation)
                   )
                 (when (> bottom-state 1)
                   (format trace-stream "~&~%State 2:~&")
                   (pepb 2)
                   (format t "State evaluation: ")
                   (pa (integrate-out-all-but-state-dimension-plm (node-function (perception-node-from-name 'board*progress))))
                   )
                 (when episodic-learning
                   (format trace-stream "~&~%Episodic memory:~%")
                   (pa 'time)
                   (format trace-stream "~&~%")
                   (parray 'time-selected*episodic-learn)
                   (format trace-stream "~&~%Operator retrieved from episodic memory:~%")
                   (ppvn 'selected*episodic)
                   )
                 )
        )

  (new-type 'dimension :numeric t :min 0 :max 3)
  (new-type 'tile :numeric t :discrete t :min 0 :max 9)

  (init-operators 'type 'tile t)

  (predicate 'board :world 'closed :arguments '((state state) (x dimension) (y dimension) (tile tile !))
             :goal '((board*goal (state 0) (x 0) (y 0) (tile 1))
                     (board*goal (state 0) (x 1) (y 0) (tile 2))
                     (board*goal (state 0) (x 2) (y 0) (tile 3))
                     (board*goal (state 0) (x 0) (y 1) (tile 8))
;                    (board*goal (state 0) (x 1) (y 1) (tile 0))
                     (board*goal (state 0) (x 2) (y 1) (tile 4))
                     (board*goal (state 0) (x 0) (y 2) (tile 7))
                     (board*goal (state 0) (x 1) (y 2) (tile 6))
                     (board*goal (state 0) (x 2) (y 2) (tile 5))
                     ))
; Goal
; 123
; 8 4
; 765

  (predicate 'success :world 'closed :arguments '((state state)))
  (predicate 'operator-evaluation :world 'closed :no-normalize t :arguments '((state state) (operator operator %)))
  (predicate 'completed :world 'closed :arguments '((state state) (operator operator)))
  (predicate 'simulated :world 'closed :arguments '((state state) (operator operator) (status flag !))) ; Flag added so can't chain on it until decision

  (conditional 'left-acceptable
               :conditions '((state (state (s)))
                             (board (state (s)) (x (x)) (y (y)) (tile (t)))
                             (board (state (s)) (x (x -1)) (y (y)) (tile 0))
                             (operator (tile (t)) (operator (o)))
                             )
               :actions '((selected (state (s)) (operator (o))))
               :function .1
               )

  (conditional 'right-acceptable
               :conditions '((state (state (s)))
                             (board (state (s)) (x (x)) (y (y)) (tile (t)))
                             (board (state (s)) (x (x 1)) (y (y)) (tile 0))
                             (operator (tile (t)) (operator (o)))
                             )
               :actions '((selected (state (s)) (operator (o))))
               :function .1
               )

  (conditional 'up-acceptable
               :conditions '((state (state (s)))
                             (board (state (s)) (x (x)) (y (y)) (tile (t)))
                             (board (state (s)) (x (x)) (y (y -1)) (tile 0))
                             (operator (tile (t)) (operator (o)))
                             )
               :actions '((selected (state (s)) (operator (o))))
               :function .1
               )

  (conditional 'down-acceptable
               :conditions '((state (state (s)))
                             (board (state (s)) (x (x)) (y (y)) (tile (t)))
                             (board (state (s)) (x (x)) (y (y 1)) (tile 0))
                             (operator (tile (t)) (operator (o)))
                             )
               :actions '((selected (state (s)) (operator (o))))
               :function .1
               )

  (conditional 'move-left ; Move tile left (and blank right)
               :conditions '((state (state (s)))
                             (selected (state (s)) (operator (o)))
                             (operator (tile (t)) (operator (o)))
                             (board (state (s)) (x (x)) (y (y)) (tile (t)))
                             (board (state (s)) (x (x -1)) (y (y)) (tile 0))
                             )
               :actions '((board (state (s)) (x (x)) (y (y)) (tile 0))
                          (board (state (s)) (x (x -1)) (y (y)) (tile (t)))
                          )
               )

  (conditional 'move-right ; Move tile right (and blank left)
               :conditions '((state (state (s)))
                             (selected (state (s)) (operator (o)))
                             (operator (tile (t)) (operator (o)))
                             (board (state (s)) (x (x)) (y (y)) (tile (t)))
                             (board (state (s)) (x (x 1)) (y (y)) (tile 0))
                             )
               :actions '((board (state (s)) (x (x)) (y (y)) (tile 0))
                          (board (state (s)) (x (x 1)) (y (y)) (tile (t)))
                          )
               )

  (conditional 'move-up ; Move tile up (and blank down)
               :conditions '((state (state (s)))
                             (selected (state (s)) (operator (o)))
                             (operator (tile (t)) (operator (o)))
                             (board (state (s)) (x (x)) (y (y)) (tile (t)))
                             (board (state (s)) (x (x)) (y (y -1)) (tile 0))
                             )
               :actions '((board (state (s)) (x (x)) (y (y)) (tile 0))
                          (board (state (s)) (x (x)) (y (y -1)) (tile (t)))
                          )
               )

  (conditional 'move-down ; Move tile down (and blank up)
               :conditions '((state (state (s)))
                             (selected (state (s)) (operator (o)))
                             (operator (tile (t)) (operator (o)))
                             (board (state (s)) (x (x)) (y (y)) (tile (t)))
                             (board (state (s)) (x (x)) (y (y 1)) (tile 0))
                             )
               :actions '((board (state (s)) (x (x)) (y (y)) (tile 0))
                          (board (state (s)) (x (x)) (y (y 1)) (tile (t)))
                          )
               )

  (conditional 'goal-test
               :conditions '((state (state (s)))
                             (board (state (s)) (x (x1)) (y (y1)) (tile 1))
                             (board*goal (state (s)) (x (x1)) (y (y1)) (tile 1))
                             (board (state (s)) (x (x2)) (y (y2)) (tile 2))
                             (board*goal (state (s)) (x (x2)) (y (y2)) (tile 2))
                             (board (state (s)) (x (x3)) (y (y3)) (tile 3))
                             (board*goal (state (s)) (x (x3)) (y (y3)) (tile 3))
                             (board (state (s)) (x (x4)) (y (y4)) (tile 4))
                             (board*goal (state (s)) (x (x4)) (y (y4)) (tile 4))
                             (board (state (s)) (x (x5)) (y (y5)) (tile 5))
                             (board*goal (state (s)) (x (x5)) (y (y5)) (tile 5))
                             (board (state (s)) (x (x6)) (y (y6)) (tile 6))
                             (board*goal (state (s)) (x (x6)) (y (y6)) (tile 6))
                             (board (state (s)) (x (x7)) (y (y7)) (tile 7))
                             (board*goal (state (s)) (x (x7)) (y (y7)) (tile 7))
                             (board (state (s)) (x (x8)) (y (y8)) (tile 8))
                             (board*goal (state (s)) (x (x8)) (y (y8)) (tile 8))
                             )
               :actions '((success (state (s)))
                          )
               )

  ; Make evaluation operators best in subgoal for tied operators
  (conditional 'evaluate-best
               :conditions '((state (state (s)))
                             (impasse (state (s)) (type tie) (operator (o)))
                             (evaluate-operator (operator (eo)) (evaluate (o)))
                             )
               :actions '((selected (state (s)) (operator (eo))))
               )

  ;Reject completed evaluation operators
  (conditional 'evaluate-reject
               :conditions '((state (state (s)))
                             (impasse (state (s)) (type tie) (operator (o)))
                             (evaluate-operator (operator (eo)) (evaluate (o)))
                             (completed (state (s)) (operator (eo)))
                             )
               :actions '((selected - (state (s)) (operator (eo))))
               )

  ; Make operator to be evaluated best in no-change for evaluation operator
  (conditional 'evaluation-operator-best
               :conditions '((state (state (s)))
                             (impasse (state (s)) (type no-change) (operator (eo)))
                             (evaluate-operator (operator (eo)) (evaluate (o))))
               :actions '((selected (state (s)) (operator (o))))
               )

  ; Copy down board for use in evaluation
  (conditional 'evaluation-board
               :conditions '((state (state (s)))
                             (impasse (state (s)) (type no-change) (operator (eo)))
                             (evaluate-operator (operator (eo)))
                             (impasse (state (s -1)) (type tie))
                             (board (state (s -2)) (x (x)) (y (y)) (tile (t)))
                             (board - (state (s)))
                             )
               :actions '((board (state (s)) (x (x)) (y (y)) (tile (t))))
               )

  ; Copy down goal for use in evaluation
  (conditional 'evaluation-goal
               :conditions '((state (state (s)))
                             (impasse (state (s)) (type no-change) (operator (eo)))
                             (evaluate-operator (operator (eo)))
                             (impasse (state (s -1)) (type tie))
                             (board*goal (state (s -2)) (x (x)) (y (y)) (tile (t)))
                             )
               :actions '((board*goal (state (s)) (x (x)) (y (y)) (tile (t))))
               )

  ; Mark that simulated operator has run
  (conditional 'operator-simulated
               :conditions '((state (state (s)))
                             (selected (state (s)) (operator (o)))
                             (impasse (state (s)) (type no-change) (operator (eo)))
                             (evaluate-operator (evaluate (eo)) (operator (o)))
                             )
               :actions '((simulated (state (s)) (operator (o)) (status true)))
               )

  ; When operator simulated, return evaluation
  (conditional 'return-state-evaluation
               :conditions '((state (state (s)))
                             (simulated (state (s)) (operator (o)) (status true))
                             (board*progress (state (s)))
                             (impasse (state (s)) (type no-change) (operator (eo)))
                             (evaluate-operator (evaluate (o)) (operator (eo)))
                             (selected (state (s -1)) (operator (eo)))
                             )
               :actions '((operator-evaluation (state (s -1)) (operator (o))))
               )

  ; When operator simulated, return completion
  (conditional 'return-completion
               :conditions '((state (state (s)))
                             (simulated (state (s)) (operator (o)) (status true))
                             (impasse (state (s)) (type no-change) (operator (eo)))
                             (evaluate-operator (evaluate (o)) (operator (eo)))
                             (selected (state (s -1)) (operator (eo)))
                             )
               :actions '((completed (state (s -1)) (operator (eo))))
               )

  ; Return evaluations on offer operators once they have all been evaluated
  (conditional 'return-operator-evaluations
               :conditions '((state (state (s)))
                             (impasse (state (s)) (type tie) (operator (o)))
                             (impasse (state (s 1)) (type none))
                             (operator-evaluation (state (s)) (operator (o)))
                             )
               :actions '((selected (state (s -1)) (operator (o)))
                          )
               )

  ; Return evaluations on offer operators once they have all been evaluated
  (conditional 'select-randomly-after-impasse
               :conditions '((state (state (s)))
                             (impasse (state (s)) (type tie) (operator (o)))
                             (impasse (state (s 1)) (type none))
                             )
               :actions '((detect-impasses - (state (s -1)) (value true))
                          )
               )

  (conditional 'halt
               :conditions '((success (state 0)))
               :actions '((halt))
               )
  t)

(defun ep102 (&optional el)
;  (learn)
  (ep10i el)
; Board
; 123
; 845
; 7 6
  (evidence '((board (state 0) (x 0) (y 0) (tile 1))
              (board (state 0) (x 1) (y 0) (tile 2))
              (board (state 0) (x 2) (y 0) (tile 3))
              (board (state 0) (x 0) (y 1) (tile 8))
              (board (state 0) (x 1) (y 1) (tile 4))
              (board (state 0) (x 2) (y 1) (tile 5))
              (board (state 0) (x 0) (y 2) (tile 7))
              (board (state 0) (x 1) (y 2) (tile 0))
              (board (state 0) (x 2) (y 2) (tile 6))))
  t)


; A program for testing state adjacency
(defun sa nil
  (init)
  (new-type 'value :constants '(a b))

  (predicate 'test :world 'closed :arguments '((state state) (value value !)))
  (predicate 'result :world 'closed :arguments '((state state) (value value)))

  (conditional 'test-sa-condition
               :conditions '((test (state (state)) (value (value)))
                             (test (state (state 1)) (value (value)))
                             )
               :actions '((result (state (state)) (value (value)))
                          )
               )

;  (conditional 'test-sa-action
;               :conditions '((test (state (state)) (value (value))))
;               :actions '((result (state (state -4)) (value (value))))
;               )

  (evidence '((test (state 0) (value a))
              (test (state 1) (value a))
              (test (state 2) (value b))
              (test (state 3) (value b))
              )
            )
  t)

; A program for detecting overlap of 8P tiles
(defun o1i nil
  (init)

  (new-type 'dimension :numeric t :min 0 :max 3)
  (new-type 'tile :numeric t :discrete t :min 0 :max 9)

  (predicate 'board :world 'closed :arguments '((state state) (x dimension) (y dimension) (tile tile)))
  (predicate 'overlap :world 'closed :arguments '((state state)))

  (conditional 'overlap
               :conditions '((board (state (s)) (x (x)) (y (y)) (tile 1))
                             (board (state (s)) (x (x)) (y (y)) (tile 2))
                             )
               :actions '((overlap (state (s)))
                          )
               )
  )

; Tiles not overlapping
(defun o1e nil
  (evidence '((board (state 0) (x 0) (y 0) (tile 1))
              (board (state 0) (x 1) (y 0) (tile 2))
              )
            )
  )

; Tiles  overlapping
(defun o1e2 nil
  (evidence '((board (state 0) (x 0) (y 0) (tile 1))
              (board (state 0) (x .5) (y 0) (tile 2))
              )
            )
  )

(defun o1 nil
  (o1i)
  (o1e)
  t)

(defun o12 nil
  (o1i)
  (o1e2)
  t)

; Test affine (with a tetromino), but with empty space actually empty
(defun test-affine nil
  (init)
  (new-type 'c4 :numeric t :discrete nil :min 0 :max 4)
  (predicate 'tetromino :world 'closed :arguments '((x c4) (y c4) (present boolean !)))

  (conditional 'rotate-90
               :conditions '((tetromino (x (x)) (y (y)) (present true)))
               :actions '((tetromino (x (y (:coefficient -1 :offset 4))) (y (x)) (present true)))
               )
  (conditional 'rotate-90-cond
               :conditions '((tetromino (x (x (:from y :coefficient -1 :offset 4))) (y (y (:from x))) (present true)))
               )

  (conditional 'reflect-x
               :conditions '((tetromino (x (x)) (y 1) (present true)))
               :actions '((tetromino (x (x (:coefficient -1 :offset 4))) (y 1) (present true)))
               )
  (conditional 'reflect-x-cond
               :conditions '((tetromino (x (x (:coefficient -1 :offset 4))) (y 1) (present true)))
               )

  (conditional 'scale-half
               :conditions '((tetromino (x (x)) (y 1) (present true)))
               :actions '((tetromino (x (x (:coefficient 1/2))) (y 1) (present true)))
               )
  (conditional 'scale-half-cond
               :conditions '((tetromino (x (x (:coefficient 1/2))) (y 1) (present true)))
               )

  (conditional 'scale-double
               :conditions '((tetromino (x (x)) (y 1) (present true)))
               :actions '((tetromino (x (x (:coefficient 2))) (y (y)) (present true)))
               )

  (conditional 'translate-2
               :conditions '((tetromino (x (x)) (y 1) (present true)))
               :actions '((tetromino (x (x 2)) (y 1) (present true)))
               )
  (conditional 'translate-2-cond
               :conditions '((tetromino (x (x 2)) (y 1) (present true)))
               )

  (conditional 'translate-2a
               :conditions '((tetromino (x (x)) (y 1) (present true)))
               :actions '((tetromino (x (x (:offset 2))) (y 1) (present true)))
               )
  (conditional 'translate-2a-cond
               :conditions '((tetromino (x (x (:offset 2))) (y 1) (present true)))
               )

  (conditional 'translate-minus-2
               :conditions '((tetromino (x (x)) (y 1) (present true)))
               :actions '((tetromino (x (x -2)) (y 1) (present true)))
               )
  (conditional 'translate-minus-2a
               :conditions '((tetromino (x (x)) (y 1) (present true)))
               :actions '((tetromino (x (x (:offset -2))) (y 1) (present true)))
               )

  (conditional 'scale-half-in-place
               :conditions '((tetromino (x (x)) (y (y)) (present true)))
               :actions '((tetromino (x (x (:coefficient 1/2 :offset 1))) (y (y)) (present true)))
               )
  (conditional 'scale-half-in-place-cond
               :conditions '((tetromino (x (x (:coefficient 1/2 :offset 1))) (y (y)) (present true)))
               )

; Z tetrimino
  (evidence '((tetromino (x (1/2 5/2)) (y 1) (present true))
              (tetromino (x (3/2 7/2)) (y 2) (present true))
              )
            )
  )

(defun xta nil
  (test-affine)
  (d 0)
  (g)
  nil
  )

; Test affine (with a tetromino), for just rotate in condition
(defun test-rotate-cond nil
  (init)
  (new-type 'c4 :numeric t :discrete nil :min 0 :max 4)
  (predicate 'tetromino :world 'closed :arguments '((x c4) (y c4) (present boolean !)))

  (conditional 'rotate-90-cond
               :conditions '((tetromino (x (x (:from y :coefficient -1 :offset 4))) (y (y (:from x))) (present true)))
               )

; Z tetrimino
  (evidence '((tetromino (x (1/2 5/2)) (y 1) (present true))
              (tetromino (x (3/2 7/2)) (y 2) (present true))
              )
            )
  )

(defun xtrc nil
  (test-rotate-cond)
  (d 0)
  (g)
  nil
  )

; Test affine (with a tetromino), variant without :from in action
(defun test-rotate-variant nil
  (init)
  (new-type 'c4 :numeric t :discrete nil :min 0 :max 4)
  (predicate 'tetromino :world 'closed :arguments '((x c4) (y c4) (present boolean !)))

  (conditional 'rotate-90
               :conditions '((tetromino (x (x)) (y (y)) (present true)))
               :actions '((tetromino (x (y (:coefficient -1 :offset 4))) (y (x)) (present true)))
               )

; Z tetrimino
  (evidence '((tetromino (x (1/2 5/2)) (y 1) (present true))
              (tetromino (x (3/2 7/2)) (y 2) (present true))
              )
            )
  )

(defun xtrv nil
  (test-rotate-variant)
  (d 0)
  (g)
  nil
  )

; Test operations on multiple tetrominos
(defun mt nil
  (init)
  (new-type 'c :numeric t :discrete nil :min 0 :max 7)
  (new-type 'd :numeric t :discrete t :min 0 :max 10)
  (predicate 'field :world 'closed :arguments '((ts d) (x c) (y c)))
  (predicate 'overlap :world 'closed :arguments '((os d) (x c) (y c)))
  (predicate 'collision :world 'closed :arguments '((os d) (value Boolean !)))
  (predicate 'union :world 'closed :arguments '((x c) (y c)))
  (predicate 'left-edge :world 'closed :arguments '((x c) (y c)))

  (setq post-d '((ppfn 'field) (ppfn 'overlap) (ppfn 'collision) (ppfn 'union) (ppfn 'left-edge)))

  ; Rotate Z tetromino in 0 and add it to 3, shifting it 2 down so overlaps slightly with original z
  (conditional 'add-rotate-shift-3
               :conditions '((field (ts 0) (x (x)) (y (y))))
               :actions '((field (ts 3) (x (y (:coefficient -1 :offset 4))) (y (x (:offset 2))))
                          )
               )
  
  ; Scale Z tetronimon add add it to 4, shifting it to the right
  (conditional 'add-invert-scale-shift-4
               :conditions '((field (ts 0) (x (x)) (y (y))))
               :actions '((field (ts 4) (x (x (:coefficient -1/2 :offset 6))) (y (y)))
                          )
               )

  ; Delete the tetromino in 1
  (conditional 'delete-1
               :actions '((field - (ts 1))
                          )
               )

  ; Compute overlap
  (conditional 'overlap-0-2
               :conditions '((field (ts 0) (x (x)) (y (y)))
                             (field (ts 2) (x (x)) (y (y)))
                             )
               :actions '((overlap (os 0) (x (x)) (y (y)))
                          )
               )

  ; Compute overlap
  (conditional 'overlap-0-3
               :conditions '((field (ts 0) (x (x)) (y (y)))
                             (field (ts 3) (x (x)) (y (y)))
                             )
               :actions '((overlap (os 1) (x (x)) (y (y)))
                          )
               )

  ; Compute overlap
  (conditional 'overlap-2-3
               :conditions '((field (ts 2) (x (x)) (y (y)))
                             (field (ts 3) (x (x)) (y (y)))
                             )
               :actions '((overlap (os 2) (x (x)) (y (y)))
                          )
               )

  ; Determine if there is any collision from overlap
  (conditional 'collision
               :conditions '((overlap (os (o)) (x (x)) (y (y))))
               :actions '((collision (os (o)) (value true)))
               )

  ; Compute union of all planes in field
  (conditional 'union
               :conditions '((field (ts (ts)) (x (x)) (y (y))))
               :actions '((union (x (x)) (y (y))))
               )

  ; Detect left edge of unioned image
  (conditional 'left-edge
              :conditions '((union (x (x)) (y (y)))
                            (union - (x (x (:offset -.0001))) (y (y)))
                            )
              :actions '((left-edge (x (x)) (y (y))))
              )

  ; Three Z tetriminos
  (evidence '((field (ts 0) (x (1/2 5/2)) (y 1))
              (field (ts 0) (x (3/2 7/2)) (y 2))

              (field (ts 1) (x (1/2 5/2)) (y 3))
              (field (ts 1) (x (3/2 7/2)) (y 4))

              (field (ts 2) (x (1/2 5/2)) (y 5))
              (field (ts 2) (x (3/2 7/2)) (y 6))

              )
            )
  )

(defun mt2 nil
  (init)
  (new-type 'c :numeric t :discrete nil :min 0 :max 7)
  (new-type 'd :numeric t :discrete t :min 0 :max 10)
  (predicate 'field :world 'closed :arguments '((ts d) (x c) (y c)))
  (predicate 'image :world 'closed :arguments '((x c) (y c)))
  (setq post-d '((ppfn 'field)))
  (conditional 'rotate-90
               :conditions '((field (ts 0) (x (x)) (y (y))))
               :actions '((field (ts 1) (x (y (:coefficient -1 :offset 4))) (y (x))))
               )

  (conditional 'rotate-90-cond
               :conditions '((field (ts 0) (x (x (:from y :coefficient -1 :offset 4))) (y (y (:from x)))))
               :actions '((field (ts 2) (x (x)) (y (y))))
               )

  (conditional 'rotate-90-condact
               :condacts '((image (x (x (:from y :coefficient -1 :offset 4))) (y (y (:from x)))))
               )

  (conditional 'reflect-x
               :conditions '((field (ts 0) (x (x)) (y (y))))
               :actions '((field (ts 3) (x (x (:coefficient -1 :offset 4))) (y (y))))
               )

  (conditional 'scale-half-in-place
               :conditions '((field (ts 0) (x (x)) (y (y))))
               :actions '((field (ts 4) (x (x (:coefficient 1/2 :offset 1))) (y (y))))
               )

  ; Two Z tetriminos
  (evidence '((field (ts 0) (x (1/2 5/2)) (y 1))
              (field (ts 0) (x (3/2 7/2)) (y 2))

              (image (x (1/2 5/2)) (y 1))
              (image (x (3/2 7/2)) (y 2))
              )
            )
  )

; Determine which object is above and below the other
(defun ab nil
  (init)
  (setq unique-weights-only nil)
  (new-type 'c :numeric t :discrete nil :min 0 :max 7)
  (new-type 'd :numeric t :discrete t :min 0 :max 10)
  (predicate 'field :world 'closed :arguments '((ts d) (x c) (y c)))
  (predicate 'below :world 'closed :arguments '((ts d !)))
  (predicate 'above :world 'closed :arguments '((ts d !)))

  (setq post-d '((ppfn 'below) (ppfn 'above)))

  (conditional 'below
               :conditions '((field (ts (ts)) (x (x)) (y (y (:filter (* 0 .1))))))
               :actions '((below (ts (ts))))
               )

  (conditional 'above
               :conditions '((field (ts (ts)) (x (x)) (y (y (:filter (* 1 -.1))))))
               :actions '((above (ts (ts))))
               )

  ; Two Z tetriminos
  (evidence '((field (ts 0) (x (1/2 5/2)) (y 1))
              (field (ts 0) (x (3/2 7/2)) (y 2))

              (field (ts 1) (y (0 2)) (x 1))
              (field (ts 1) (y (1 3)) (x 2))
              )
            )
  )

; Line detector
(defun line-detector nil
  (init)
  (new-type 'c4 :numeric t :discrete nil :min 0 :max 4)
  (predicate 'tetromino :world 'closed :arguments '((x c4) (y c4)))
  (predicate 'edge :world 'closed :arguments '((x c4) (y c4)))

  (setq post-d '((ppfn 'edge)))

  (conditional 'left-edge
               :conditions '((tetromino (x (x)) (y (y)))
                             (tetromino - (x (x -.00001)) (y (y)))
                             )
               :actions '((edge (x (x)) (y (y))))
               )

; Z tetrimino
  (evidence '((tetromino (x (1/2 5/2)) (y 1))
              (tetromino (x (3/2 7/2)) (y 2))
              )
            )
  )

; Line detector with noise
(defun line-detector-noise nil
  (init)
  (new-type 'c4 :numeric t :discrete nil :min 0 :max 4)
  (predicate 'object :world 'open :perception t :arguments '((x c4) (y c4)))
  (predicate 'edge :world 'open :perception t :arguments '((x c4) (y c4)))



  (conditional 'left-edge
               :conditions '((object (x (x)) (y (y)))
                             (object - (x (x -.00001)) (y (y)))
                             )
               :actions '((edge (x (x)) (y (y))))
               )

;  (conditional 'smooth-forward
;               :conditions '((edge (x (x)) (y (y))))
;               :actions '((edge (x (x)) (y (yd .2))))
;               :function-variable-names '(y yd)
;               :function .2
;               )

;  (conditional 'smooth-backward
;               :conditions '((edge (x (x)) (y (y))))
;               :actions '((edge (x (x)) (y (yd -.2))))
;               :function-variable-names '(y yd)
;               :function .2
;               )

  (conditional 'smooth
               :condacts '((edge (x (x)) (y (y)))
                           (edge (x (x)) (y (yd .2))))
               :function-variable-names '(y yd)
               :function .9
               )

; Object
  (setq perceive-list '((perceive '((object .01 (x (0 4)) (y (0 2)))
                                    (object .01 (x (0 1)) (y (2 3)))
                                    (object .99 (x (1 2)) (y (2 2.4)))
                                    (object .01 (x (1 1.0001)) (y (2.4 2.6)))
                                    (object .99 (x (1.0001 2)) (y (2.4 2.6)))
                                    (object .99 (x (1 2)) (y (2.6 3)))
                                    (object .01 (x (2 4)) (y (2 3)))
                                    (object .01 (x (0 4)) (y (3 4)))
                                    ))))
  )

(defun xldn nil
  (line-detector-noise)
  (d 0)
  (g)
  nil
  )

; Noisy line smoothing
(defun smooth nil
  (init)
  (new-type 'c9 :numeric t :discrete nil :min 0 :max 9)
  (predicate 'edge :world 'open :perception t :arguments '((x c9) (occupied Boolean %)))
  (predicate 'edge2 :world 'closed :arguments '((x c9)))

;  (conditional 'smooth-1
;               :condacts '((edge (x (x)) (occupied (o)))
;                           (edge (x (x .1)) (occupied (o))))
;               :function .5
;              )

  (conditional 'smooth-2
               :condacts '((edge (x (x)) (occupied (o)))
                           (edge (x (x .2)) (occupied (o))))
;               :function .25
              )

  (conditional 'smooth-3
               :condacts '((edge (x (x)) (occupied (o)))
                           (edge (x (x .3)) (occupied (o))))
;               :function .1
              )

  (conditional 'smooth-4
               :condacts '((edge (x (x)) (occupied (o)))
                           (edge (x (x .4)) (occupied (o))))
;               :function .05
              )

  (conditional 'result
               :conditions '((edge (x (x)) (occupied true)))
               :actions '((edge2 (x (x))))
               )

;  (conditional 'smooth
;               :condacts '((edge (x (x)))
;                           (edge (x (xo .2))))
;               :function-variable-names '(x xo)
;               :function .9
;               )

; edge
  (setq perceive-list '((perceive '((edge .05 (x (0 2)) (occupied true))
                                    (edge .95 (x (0 2)) (occupied false))
                                    (edge .95 (x (2 4.4)) (occupied true))
                                    (edge .05 (x (2 4.4)) (occupied false))
                                    (edge .05 (x (4.4 4.6)) (occupied true))
                                    (edge .95 (x (4.4 4.6)) (occupied false))
                                    (edge .95 (x (4.6 7)) (occupied true))
                                    (edge .05 (x (4.6 7)) (occupied false))
                                    (edge .05 (x (7 9)) (occupied true))
                                    (edge .95 (x (7 9)) (occupied false))
                                    ))))
  t)

(defun xs nil
  (smooth)
  (d 0)
  (g)
  nil
  )

; Noisy line smoothing
(defun smooth2 nil
  (init)
  (new-type 'c4 :numeric t :discrete nil :min 0 :max 9)
  (predicate 'edge :world 'open :perception t :arguments '((x c4)))
  (predicate 'edge2 :world 'open :arguments '((x c4)))

  (conditional 'direct
               :conditions '((edge (x (x))))
               :actions '((edge2 (x (x))))
               )
  (conditional 'smooth-right-1
               :conditions '((edge (x (x))))
               :actions '((edge2 (x (x .1))))
               :function .5
               )
  (conditional 'smooth-left-1
               :conditions '((edge (x (x))))
               :actions '((edge2 (x (x -.1))))
               :function .5
               )
  (conditional 'smooth-right-2
               :conditions '((edge (x (x))))
               :actions '((edge2 (x (x .2))))
               :function .25
               )
  (conditional 'smooth-left-2
               :conditions '((edge (x (x))))
               :actions '((edge2 (x (x -.2))))
               :function .25
               )
  (conditional 'smooth-right-3
               :conditions '((edge (x (x))))
               :actions '((edge2 (x (x .3))))
               :function .1
               )
  (conditional 'smooth-left-3
               :conditions '((edge (x (x))))
               :actions '((edge2 (x (x -.3))))
               :function .1
               )
  (conditional 'smooth-right-4
               :conditions '((edge (x (x))))
               :actions '((edge2 (x (x .4))))
               :function .05
               )
  (conditional 'smooth-left-4
               :conditions '((edge (x (x))))
               :actions '((edge2 (x (x -.4))))
               :function .05
               )

; edge
  (setq perceive-list '((perceive '((edge .05 (x (0 2)))
                                    (edge .95 (x (2 4.4)))
                                    (edge .05 (x (4.4 4.6)))
                                    (edge .95 (x (4.6 7)))
                                    (edge .05 (x (7 9)))))))
  )

(defun xs2 nil
  (smooth2)
  (d 0)
  (pwm)
  nil
  )

; Noisy line smoothing
(defun smooth3 nil
  (init)
  (new-type 'c9 :numeric t :discrete nil :min 0 :max 9)
  (predicate 'edge :world 'open :perception t :arguments '((x c9) (occupied Boolean %)))
  (predicate 'edge2 :world 'closed :arguments '((x c9)))

;  (conditional 'smooth-1-r
;               :conditions '((edge (x (x)) (occupied (o))))
;               :actions '((edge (x (x .1)) (occupied (o)))
;               :function .2
;              )
;  (conditional 'smooth-1-l
;               :conditions '((edge (x (x)) (occupied (o))))
;               :actions '((edge (x (x -.1)) (occupied (o))))
;               :function .2
;              )

  (conditional 'smooth-2-r
               :conditions '((edge (x (x)) (occupied (o))))
               :actions '((edge (x (x .2)) (occupied (o))))
               :function .1
              )
  (conditional 'smooth-2-l
               :conditions '((edge (x (x)) (occupied (o))))
               :actions '((edge (x (x -.2)) (occupied (o))))
               :function .1
              )

  (conditional 'smooth-3-r
               :conditions '((edge (x (x)) (occupied (o))))
               :actions '((edge (x (x .3)) (occupied (o))))
               :function .05
              )
  (conditional 'smooth-3-l
               :conditions '((edge (x (x)) (occupied (o))))
               :actions '((edge (x (x -.3)) (occupied (o))))
               :function .05
              )

  (conditional 'smooth-4-r
               :conditions '((edge (x (x)) (occupied (o))))
               :actions '((edge (x (x .4)) (occupied (o))))
               :function .025
              )
  (conditional 'smooth-4-l
               :conditions '((edge (x (x)) (occupied (o))))
               :actions '((edge (x (x -.4)) (occupied (o))))
               :function .025
              )

  (conditional 'result
               :conditions '((edge (x (x)) (occupied true)))
               :actions '((edge2 (x (x))))
               )

;  (conditional 'smooth
;               :condacts '((edge (x (x)))
;                           (edge (x (xo .2))))
;               :function-variable-names '(x xo)
;               :function .9
;               )

; edge
  (setq perceive-list '((perceive '((edge .05 (x (0 2)) (occupied true))
                                    (edge .95 (x (0 2)) (occupied false))
                                    (edge .95 (x (2 4.4)) (occupied true))
                                    (edge .05 (x (2 4.4)) (occupied false))
                                    (edge .05 (x (4.4 4.6)) (occupied true))
                                    (edge .95 (x (4.4 4.6)) (occupied false))
                                    (edge .95 (x (4.6 7)) (occupied true))
                                    (edge .05 (x (4.6 7)) (occupied false))
                                    (edge .05 (x (7 9)) (occupied true))
                                    (edge .95 (x (7 9)) (occupied false))))))
  )

(defun xs3 nil
  (smooth3)
  (d 0)
  (g)
  nil
  )

; Tetris
(defun tetris nil
  (init)
  (new-type 'c4 :numeric t :discrete nil :min 0 :max 4)
  (new-type 'c8 :numeric t :discrete nil :min 0 :max 8)
  (new-type 'c12 :numeric t :discrete nil :min 0 :max 12)
  (predicate 'tetromino :world 'closed :arguments '((x c4) (y c4)))
  (predicate 'occupied :world 'closed :arguments '((x c8) (y c12)))
  (predicate 'free :world 'closed :arguments '((x c8) (y c12)))
  (predicate 'left :world 'closed :arguments '())

;  (conditional 'rotate-90
;               :conditions '((tetromino (x (x)) (y (y))))
;               :actions '((tetromino (x (x (:from y :coefficient -1 :offset 4))) (y (y (:from x)))))
;               )

;  (conditional 'z-space
;               :conditions '((occupied (x (x)) (y (y)))
;                             (occupied (x (x 1)) (y (y 1)))
;                             (occupied (x (x 2)) (y (y 1)))
;                             (occupied - (x (x)) (y (y -1)))
;                             (occupied - (x (x 1)) (y (y)))
;                             (occupied - (x (x 2)) (y (y)))
;                             )
;               :actions '((free (x (x)) (y (y))))
;               )

;  (conditional 'add-object
;               :conditions '((tetromino (x (x)) (y (y))))
;               :actions '((occupied (x (x 2)) (y (y 4))))
;               )

  (conditional 'left-of-1
               :conditions '((tetromino (x (0 1)) (y (y))))
               :actions '((left))
               )

; Z tetrimino
  (evidence '((tetromino (x (1/2 5/2)) (y 1))
              (tetromino (x (3/2 7/2)) (y 2))
              )
            )

; Board with bottom row filled and one gap of two in row above
  (evidence '((occupied (x (0 8)) (y 11))
              (occupied (x (0 2)) (y 10))
              (occupied (x (4 8)) (y 10))
              )
            )
  )

(defun xt nil
  (tetris)
  (d 0)
  nil
  )

; Program to test filters (generalization on constant tests)
(defun filter nil
  (init)
  (new-type 'c4 :numeric t :discrete nil :min 0 :max 4)
  (predicate 'object :world 'closed :arguments '((x c4) (y c4 !) (z c4)))

  (conditional 'test-filter
               :conditions '((object (x (x (:filter ((1 3) .1 0) (3 .2 0)))) (y (y (:filter ((1 3) .4 1.1) (3 .6 1.2)))) (z (z)))
                             (object (x (:filter ((1 2) 1 0))) (y (:filter (3 .4 .2) (2 .3 .5))))
                             )
               )

  (evidence '((object 2 (x (0 4)) (y (0 4)) (z (0 4)))))
  )

; 3D mental imagery
(defun 3dmt nil
  (init)
  (new-type 'c :numeric t :discrete nil :min 0 :max 7)
  (new-type 'd :numeric t :discrete t :min 0 :max 10)
  (predicate 'field :world 'closed :arguments '((ts d) (x c) (y c) (z c)))
  (predicate 'overlap :world 'closed :arguments '((os d) (x c) (y c) (z c)))
  (predicate 'collision :world 'closed :arguments '((os d) (value Boolean !)))
  (predicate 'union :world 'closed :arguments '((x c) (y c) (z c)))
  (predicate 'left-edge :world 'closed :arguments '((x c) (y c) (z c)))

  (setq post-d '((ppfn 'field) (ppfn 'overlap) (ppfn 'collision) (ppfn 'union) (ppfn 'left-edge)))

  ; Rotate Z tetromino in 0 and add it to 3, shifting it 2 down so overlaps slightly with original z
  (conditional 'add-rotate-shift-3
               :conditions '((field (ts 0) (x (x)) (y (y)) (z (z))))
               :actions '((field (ts 3) (x (y (:coefficient -1 :offset 4))) (y (x (:offset 2))) (z (z)))
                          )
               )
  
  ; Scale Z tetronimo and add it to 4, shifting it to the right
  (conditional 'add-invert-scale-shift-4
               :conditions '((field (ts 0) (x (x)) (y (y)) (z (z))))
               :actions '((field (ts 4) (x (x (:coefficient -1/2 :offset 6))) (y (y)) (z (z)))
                          )
               )

  ; Delete the tetromino in 1
  (conditional 'delete-1
               :actions '((field - (ts 1) (x (x)) (y (y)) (z (z)))
                          )
               )

  ; Compute overlap
  (conditional 'overlap-0-2
               :conditions '((field (ts 0) (x (x)) (y (y)) (z (z)))
                             (field (ts 2) (x (x)) (y (y)) (z (z)))
                             )
               :actions '((overlap (os 0) (x (x)) (y (y)) (z (z)))
                          )
               )

  ; Compute overlap
  (conditional 'overlap-0-3
               :conditions '((field (ts 0) (x (x)) (y (y)) (z (z)))
                             (field (ts 3) (x (x)) (y (y)) (z (z)))
                             )
               :actions '((overlap (os 1) (x (x)) (y (y)) (z (z)))
                          )
               )

  ; Compute overlap
  (conditional 'overlap-2-3
               :conditions '((field (ts 2) (x (x)) (y (y)) (z (z)))
                             (field (ts 3) (x (x)) (y (y)) (z (z)))
                             )
               :actions '((overlap (os 2) (x (x)) (y (y)) (z (z)))
                          )
               )

  ; Determine if there is any collision from overlap
  (conditional 'collision
               :conditions '((overlap (os (o)) (x (x)) (y (y)) (z (z))))
               :actions '((collision (os (o)) (value true)))
               )

  ; Compute union of all planes in field
  (conditional 'union
               :conditions '((field (ts (ts)) (x (x)) (y (y)) (z (z))))
               :actions '((union (x (x)) (y (y)) (z (z))))
               )

  ; Detect left edge of unioned image
  (conditional 'left-edge
              :conditions '((union (x (x)) (y (y)) (z (z)))
                            (union - (x (x (:offset -.0001))) (y (y)) (z (z)))
                            )
              :actions '((left-edge (x (x)) (y (y)) (z (z))))
              )

  ; Three Z tetriminos
  (evidence '((field (ts 0) (x (1/2 5/2)) (y 1) (z 1))
              (field (ts 0) (x (3/2 7/2)) (y 2) (z 1))

              (field (ts 1) (x (1/2 5/2)) (y 3) (z 1))
              (field (ts 1) (x (3/2 7/2)) (y 4) (z 1))

              (field (ts 2) (x (1/2 5/2)) (y 5) (z 1))
              (field (ts 2) (x (3/2 7/2)) (y 6) (z 1))

              )
            )
  )

; Naive Bayes gradient descent learning from data files

; Datasets for learning

(defvar dataset-datapaths)
(setq dataset-datapaths
      '(
        ; Simple symbolic concept
        (ad ad "Learn ad.txt")

        ; UCI Balloon datasets
        (a-s balloon "../data/adult-stretch.data.txt") ; Adult or Stretch
        (a+s balloon "../data/adult+stretch.data.txt") ; Adult and Stretch
        (y-s balloon "../data/yellow-small.data.txt") ; Yellow and Small or ? (second clause missing in description)
        (y-s+a-s balloon "../data/yellow-small+adult-stretch.data.txt") ; (Yellow and Small) or (Adult and Stretch)

        ; Simple logical tests
        (and logical "and.txt")
        (or logical "or.txt")
        (xor logical "xor.txt")

        ; Object (like in semantic and episodic memories) test
        (simple object "object.txt")
        (simple-test object "object test.txt")
        ))

(defvar dataset-definitions)
(setq dataset-definitions
      '(
        (ad a ((a (symbolic b c)) (d (symbolic e f g h))))
        (balloon inflated ((color (symbolic yellow purple)) (size (symbolic large small))
                          (act (symbolic stretch dip)) (age (symbolic adult child))
                          (inflated (symbolic t f))))
        (logical output ((input1 (symbolic t f)) (input2 (symbolic t f)) (output (symbolic t f))))
        (object concept ((color (symbolic silver brown white)) (legs (discrete 0 5)) (alive (symbolic t f))
                              (mobile (symbolic t f)) (weight (continuous 0 500)) (concept (symbolic walker table dog human))))
        ))

(defun setup-dataset (datapath category &optional unsupervised test)
  (setup-gd datapath test (if test (list category) nil) (if unsupervised '(category) nil))
  (when test
    (setq post-d
          (append `((format trace-stream "~&~%~S: " (best-in-plm (vnp ',category))) (ppvn ',category))
                    post-d))
    )
  t)

; Learn from dataset via gradient descent learning
(defun gdl (train test &optional unsupervised training-cycles trace lr-fraction-sp)
  (let (path-triad definition-triad dataset datapath category definition)
    ; Training
    (setq path-triad (assoc train dataset-datapaths))
    (unless path-triad
      (error "Unknown training dataset version: ~S!" train)
      )
    (setq dataset (cadr path-triad))
    (setq datapath (merge-pathnames (caddr path-triad) dataset-path))
    (setq definition-triad (assoc dataset dataset-definitions))
    (unless definition-triad
      (error "Unknown dataset: ~S!" dataset)
      )
    (setq category (cadr definition-triad))
    (setq definition (caddr definition-triad))
    (when unsupervised
      ; Category needs to go at end of list so doesn't disturb order of data read on line
      (setq definition (append definition `((* category (discrete 0 ,unsup-categories)))))
      )
    (define-data-set definition unsupervised trace)
    (setq learning-rate-fraction-of-smoothing-parameter lr-fraction-sp)
    (setq pre-t `((format trace-stream "~&~%   >>> Training Cycle <<<")
                  (setup-dataset ',datapath ',category ',unsupervised)
                  (setq post-t '((close data-stream)))
                  ))
    (unless (eq (trials training-cycles) interrupt-symbol)
      ; Testing
      (setq pre-t '())
      (format trace-stream "~&~%   >>> Testing <<<")
      (setq path-triad (assoc test dataset-datapaths))
      (unless path-triad
        (error "Unknown testing dataset version: ~S!" test)
        )
      (setq datapath (merge-pathnames (caddr path-triad) dataset-path))
      (setup-dataset datapath category unsupervised t)
      (when (equal (stype-constants (type-from-name category)) '(t f))
        (setq post-t `((close data-stream) (print-gd-test-results '(,category))))
        )
      (trials 1 t)
      (format trace-stream "~&~%") (pcfs)
      )
    )
  )

; Very simple form of distribution learning
(defun simple-gdl (ntypes)
  (init)
  (learn '(:gd))
  (setq learning-rate .001)
  (format trace-stream "Setting learning rate to .001")
  (new-type 'type :numeric t :discrete t :min 0 :max ntypes)
  (predicate 'concept :perception t :arguments '((value type %)) :function 1)
  ; Prior on concept  
  (conditional 'concept
               :condacts '((concept (value (concept))))
               )
  )

; Return a random number in 0-((length ps) - 1) biased according to ps
; PS should be a list of probabilities that sum to 1
(defun br4 (ps)
  (let ((r (random 1.0))
        (min 0))
    (dotimes (i (length ps))
      (setq min (+ min (nth i ps)))
      (when (< r min) (return i))
      )
    )
  )

; Run times trials according to the distribution in ps
; PS should be a list of probabilities that sum to 1
(defun sgdl (ps times)
  (let ((fs (init-vector (length ps) 0))
        tn
        )
    (simple-gdl (length ps))
    (dotimes (i times)
      (setq tn (br4 ps))
      (setf (aref fs tn) (+ (aref fs tn) 1))
      (perceive `((concept (value ,tn))))
      (d 1)
      )
    (format trace-stream "~&") (ppf 'concept)
    (dotimes (tt (length ps))
      (setf (aref fs tt) (/ (aref fs tt) (* times 1.0)))
      )
    fs)
  )

; Multiagent 1D Grid
(defun ma-grid2 nil
  (init nil '(a b c))
  (init-operators 'symbols '(left right) t) ; Detect impasses
  (setq pre-run '(
                  )
        )
  (setq pre-d '(
                )
        )
  (setq post-d '((ppfn 'selected)
                 (ppfn 'impasse)
                 (ppfn 'location)
                 )
        )
  (new-type 'c6 :numeric t :discrete t :min 0 :max 6)
  (predicate 'location :world 'closed :arguments '((agent agent) (state state) (x c6 !)))

  ; Select operator
  (conditional 'select-operator
               :conditions '((location (agent (a)) (state 0) (x (x))))
               :actions '((selected (agent (a)) (state 0) (operator (o))))
               :function-variable-names '(a x o)
               :function '((.2 a * left) (.8 a * right)
                           (.8 b * left) (.2 b * right)
                           (.1 c * *)
                           )
               )

  ; Move left
  (conditional 'move-left
               :conditions '((selected (agent (a)) (state 0) (operator left))
                             (location (agent (a)) (state 0) (x (x)))
                             )
               :actions '((location (agent (a)) (state 0) (x (x -1)))
                          )
               )

  ; Move right
  (conditional 'move-right
               :conditions '((selected (agent (a)) (state 0) (operator right))
                             (location (agent (a)) (state 0) (x (x)))
                             )
               :actions '((location (agent (a)) (state 0) (x (x 1)))
                          )
               )

  ; Prefer left for an agent if reach a tie impasse on it
  ; This is to test removal of reflective states with multiple agents
  (conditional 'tie-left-best
               :conditions '((impasse (agent (a)) (state 1) (type tie)))
               :actions '((selected (agent (a)) (state 0) (operator left)))
               :function 1
               )

  (evidence '((location (agent a) (state 0) (x 2))
              (location (agent b) (state 0) (x 3))
              (location (agent c) (state 0) (x 4))
              )
            )
  )

; Test reuse of same variable within a pattern
; Simple test with a single reuse of one variable
(defun vr nil
  (init)
  (predicate 'test :world 'closed :arguments '((a boolean) (b boolean)))
  (conditional 'vrc
               :conditions '((test (a (v)) (b (v))))
               )
  (evidence '((test (a true) (b *))))
  )

; Complex test with multiple reused variables and other variables
(defun vr2 nil
  (init)
  (new-type 'tri :constants '(x y z))
  (predicate 'test :world 'closed :arguments '((a boolean) (b boolean) (c boolean) (d tri) (e tri) (f tri)))
  (conditional 'vrc
               :conditions '((test (a (v)) (b (v)) (c (m)) (d (n)) (e (n)) (f (n))))
               )
  (evidence '((test (a true) (b *) (c false) (d x) (e *) (f *))))
  )

; Test to see if error will be signalled when try to reuse a variable in an action
(defun vr3 nil
  (init)
  (predicate 'test :world 'closed :arguments '((a boolean) (b boolean)))
  (conditional 'vrc
               :actions '((test (a (v)) (b (v))))
               )
  (evidence '((test (a true) (b *))))
  )

; Test for what happens when there is a linear function over the combined variables
(defun vr4 nil
  (init)
  (new-type 'c03 :discrete nil :min 0 :max 3)
  (predicate 'test :world 'closed :arguments '((a c03) (b c03 !)))
  (conditional 'vrc
               :conditions '((test (a (v)) (b (v))))
               )
  (evidence '((test (1 0 3) (a *) (b *))))
  )

; Test use of constants in open-world condacts
(defun oc nil
  (init)
  (predicate 'o :arguments '((a boolean) (b boolean %)))

  (conditional 'oc1
               :condacts '((o (a true) (b (b))))
               :function-variable-names '(b)
               :function-default 1
               :function '((.5 true) (.3 false))
               )

  (conditional 'oc2
               :condacts '((o (a false) (b (b))))
               :function-variable-names '(b)
               :function-default 1
               :function '((.2 true) (.7 false))
               )
  )

; Test selecting expected value
(defun ev nil
  (init)
  (new-type 'ten :numeric t :min 0 :max 10)
  (predicate 'expect1 :world 'closed :arguments '((expect ten %)))
  (predicate 'expect2 :world 'closed :arguments '((expect ten $)))

  (conditional 'expect
               :conditions '((expect1 (expect (e))))
               :actions '((expect2 (expect (e))))
               )

  (evidence '((expect1 (expect *))))
  t)


; Test use of variable offset, apply-coefficient-to-offset and using both filters and an affine
(defun vo nil
  (init)
  (setq pre-run '(
                  )
        )
  (setq pre-d '(
                )
        )
  (setq post-d '(
                 )
        )

  (new-type 'utility :numeric t :discrete t :min 0 :max 10)

  (predicate 'utility :perception t :arguments '((value utility %)))
  (predicate 'combined :arguments '((value utility %)))
  (predicate 'reward :perception t :arguments '((value utility %)))

  ; Variable offset in action
  (conditional 'vo
               :conditions '((utility (value (u)))
                             (reward (value (r)))
                             (combined (value (x 1 (:filter (4 1 .5)))))
                             )
               :actions '((combined (value (u (:offset r :pad 0))))
                          (combined (value (u (:coefficient .5 :offset r :pad 0))))
                          (combined (value (u (:coefficient .5 :offset r :pad 0 :apply-coefficient-to-offset t)))))
               )

  (setq perceive-list '((perceive '((utility (value 3))
                                    (reward (value 2))))))
  t
  )

; Test negative domain values
(defun nd nil
  (init)
  (setq pre-run '(
                  )
        )
  (setq pre-d '(
                )
        )
  (setq post-d '(
                 )
        )

  (new-type 'utility :numeric t :discrete t :min -5 :max 5)

  (predicate 'utility :perception t :arguments '((value utility %)))
  (predicate 'combined :arguments '((value utility %)))
  (predicate 'reward :perception t :arguments '((value utility %)))

  ; Variable offset in action
  (conditional 'vo
               :conditions '((utility (value (u)))
                             (reward (value (r)))
                             )
               :actions '((combined (value (u (:offset r :pad 0))))
                          (combined (value (u (:offset -2 :pad 0))))
                          (combined (value (u (:offset -8 :pad 0 :coefficient .5 :apply-coefficient-to-offset t))))
                          )
               )

  (setq perceive-list '((perceive '((utility (value 1))
                                    (reward (value 3))))))
  )

; 1D Grid for reinforcement learning (RL)

; Return currently selected location
(defun current-location ()
  (let (v)
    (setq v (value-in-state 'location base-level-state 'x))
    (when center-discrete-numeric-on-integer
      (setq v (+ v 1/2))
      )
    v)
  )

; Assign a fixed reward based on location
(defun assign-reward (rewards)
  (let ((cl (current-location)))
    (eval `(perceive (quote ((reward .1 (x *) (value *)) ; Empty WM of any previous rewards
                             (reward (x ,cl) (value ,(aref rewards cl))))))) ; Add reward for current state
    )
  )

; 1D Grid for RL

; Fixed vector of rewards for use in assign-reward
(defparameter rewards7 (vector 0 0 0 0 9 0 0 0))

(defun init-rl-grid7 (learn &optional start load-evidence model-free)
  (init '(left right) nil t) ; center-discrete-numeric-on-integer
;  (setq learning-rate-fraction-of-smoothing-parameter nil)
  (operator-selection 'boltzmann)
  (when learn
    (learn '(:gd))
    (setq perceive-list '((assign-reward rewards7) perceive-list))
    )
  (when (eq learn :dp)
    (setq diachronic-prediction t)
    )

  (setq post-d '((ppfn 'selected)
                 (ppfn 'location)
                 (when diachronic-prediction
                   (ppvn 'location*next)
                   )
                 )
        )

  (setq post-t '((format trace-stream "~&~%PROJECTED FF (EV):~&")
                 (pa 'projectedc nil '((expected u)))
                 (format trace-stream "~&~%Q FF (EV):~&")
                 (pa 'qc nil '((expected q)))
                 (format trace-stream "~&~%REWARD FF (EV):~&")
                 (pa 'rewardc nil '((expected r)))
                 (when diachronic-prediction
                   (when (member 'transition (graph-conditionals cg) :key #'conditional-name)
                     (format trace-stream "~&~%TRANSITION FF:~&")
                     (pa 'transition)
                     )
                   (when (member 'location-prediction (graph-conditionals cg) :key #'conditional-name)
                     (format trace-stream "~&~%LOCATION-PREDICTION FF:~&")
                     (pa 'location-prediction)
                     )
                   )
                 (format trace-stream "~&~%")
                 )
        )

  (new-type 'location :numeric t :discrete t :min 0 :max 8)
  (new-type 'utility :numeric t :discrete t :min 0 :max 10)

  (predicate 'location :world 'closed :arguments '((state state) (x location !)))
  (unless diachronic-prediction
    (predicate (concat-symbols (list 'location prediction-suffix)) :arguments '((state state) (x location %)))
    )
  (predicate 'projected :arguments '((x location) (value utility %)))
  (predicate 'reward :perception t :arguments '((x location) (value utility %)))
  (predicate 'q :arguments '((operator operator) (x location) (value utility %)))

  ; Q value for operator given present state
  (conditional 'qc
               :conditions '((state (state (s)))
                             (location (state (s)) (x (x))))
               :condacts '((q (operator (o)) (x (x)) (value (q))))
               :function-variable-names '(x o q)
               :normal 'q
               :function '((.1 * * *)
                           (1 (0 2) left 0) (0 (0 2) left (1 10)) ; Don't go left from 1 (or 0)
                           (1 (6 8) right 0) (0 (6 8) right (1 10)) ; Don't go right from 6 (or 7)
                           )
               )

  ; Select operator based on Q value
  (conditional 'select-operator
               :conditions '((state (state (s)))
                             (location (state (s)) (x (x)))
                             (q (operator (o)) (x (x)) (value (q (:filter (* 0 .1))))))
               :actions '((selected (state (s)) (operator (o))))
               )

  ; Transition function
  (unless (or model-free automatic-action-models)
    (conditional 'transition
                 :conditions '((state (state (s)))
                               (location (state (s)) (x (x)))
                               (selected (state (s)) (operator (o))))
                 :condacts `((,(concat-symbols (list 'location prediction-suffix)) (state (s)) (x (nx))))
                 :function-variable-names '(x o nx)
                 :normal 'nx
                 :function (if diachronic-prediction
                               '((.125 * * *))
                             '((1 0 left 0) (1 1 left 0) (1 2 left 1) (1 3 left 2) (1 4 left 3) (1 5 left 4) (1 6 left 5) (1 7 left 6)
                               (1 7 right 7) (1 6 right 7) (1 5 right 6) (1 4 right 5) (1 3 right 4) (1 2 right 3) (1 1 right 2) (1 0 right 1))
                             )
                 )
    )

  ; Backup utility to q
  (conditional 'backup
               :conditions `((state (state (s)))
                             (location (state (s)) (x (x)))
                             (selected (state (s)) (operator (o)))
                             (,(concat-symbols (list 'location prediction-suffix)) (state (s)) (x (nx)))
                             (reward (x (nx)) (value (r)))
                             (projected (x (nx)) (value (p))))
               :actions '((q (operator (o)) (x (x)) (value (p (:coefficient .95 :offset r :pad 0 :apply-coefficient-to-offset t))))
                          (projected (x (x)) (value (p (:coefficient .95 :offset r :pad 0 :apply-coefficient-to-offset t)))))
               )

  ; Projected future utility
  (conditional 'projectedc
               :condacts '((projected (x (x)) (value (u))))
               :function-variable-names '(x u)
               :normal 'u
               :function '((.1 * *) (1 0 0) (0 0 (1 10)) (1 4 0) (0 4 (1 10)) (1 7 0) (0 7 (1 10)))
               )

  ; State reward (local)
  (conditional 'rewardc
               :condacts '((reward (x (x)) (value (r))))
               :function-variable-names '(x r)
               :normal 'r
               :function '((1 0 0) (0 0 (1 10)) (1 7 0) (0 7 (1 10)) (.1 (1 7) *))
               )

  ; Move left
  (conditional 'move-left
               :conditions '((state (state (s)))
                             (selected (state (s)) (operator left))
                             (location (state (s)) (x (x))))
               :actions '((location (state (s)) (x (x -1)))
;                          (location - (state (s)) (x (x)))
                          )
               )

  ; Move right
  (conditional 'move-right
               :conditions '((state (state (s)))
                             (selected (state (s)) (operator right))
                             (location (state (s)) (x (x))))
               :actions '((location (state (s)) (x (x 1)))
;                          (location - (state (s)) (x (x)))
                          )
               )

  ; Don't move to 0
  (conditional 'no-zero
               :conditions '((state (state (s)))
                             (location (state (s)) (x 1)))
               :actions '((selected - (state (s)) (operator left)))
               )

  ; Don't move to 7
  (conditional 'no-seven
               :conditions '((location (state (s)) (x 6)))
               :actions '((selected - (state (s)) (operator right)))
               :function 0
               )

  ; Halt when reach location 4
  (conditional 'halt
               :conditions '((location (state 0) (x 4)))
               :actions '((halt))
               )

  (when load-evidence (rle7 start nil))
  nil)

; Brief init
(defun brief-init-rl-grid7 (learn)
  (learn learn)
  (setq center-discrete-numeric-on-integer t)
  )

; Top level function to call for reinforcement learning
; If initial is true, initialize graph
; Start is the starting location in the grid
; Repeat is the number of trials to run
; Model-free determines whether include conditionals for action models
(defun rl-grid7 (learn &optional initial start repeat model-free)
  (if initial
      (init-rl-grid7 learn start (= repeat 0) model-free)
    (brief-init-rl-grid7 learn)
    )
  (if (= repeat 0)
      (d 0)
    (rlr7 start repeat initial))
  )

(defun test-rl-mb-7 ()
  (rl-grid7 :gd t 1 30)
  (format info-stream "~&~%PROJECTED FF (EV):~&")
  (pa 'projectedc nil '((expected u)) info-stream)
  (format info-stream "~&~%Q FF (EV):~&")
  (pa 'qc nil '((expected q)) info-stream)
  (format info-stream "~&~%REWARD FF (EV):~&")
  (pa 'rewardc nil '((expected r)) info-stream)
  (format info-stream "~&~%")
  (print-global-decision-statistics info-stream)
  )
(defun test-rl-dp-7 ()
  (rl-grid7 :dp t 1 35)
  (format info-stream "~&~%PROJECTED FF (EV):~&")
  (pa 'projectedc nil '((expected u)) info-stream)
  (format info-stream "~&~%Q FF (EV):~&")
  (pa 'qc nil '((expected q)) info-stream)
  (format info-stream "~&~%REWARD FF (EV):~&")
  (pa 'rewardc nil '((expected r)) info-stream)
  (format info-stream "~&~%")
  (format info-stream "~&~%TRANSITION FF:~&")
  (pa 'transition nil nil info-stream)
  (print-global-decision-statistics info-stream)
  )
(defun test-rl-mf-7 ()
  (rl-grid7 :dp t 1 40 t)
  (format info-stream "~&~%PROJECTED FF (EV):~&")
  (pa 'projectedc nil '((expected u)) info-stream)
  (format info-stream "~&~%Q FF (EV):~&")
  (pa 'qc nil '((expected q)) info-stream)
  (format info-stream "~&~%REWARD FF (EV):~&")
  (pa 'rewardc nil '((expected r)) info-stream)
  (format info-stream "~&~%")
  (print-global-decision-statistics info-stream)
  )

; Load evidence
(defun rle7 (&optional start continue)
  (unless start (setq start 2))
  (if open-world-wmfns
      (evidence `((location (state 0) (x ,start))
              (projected .1 (x (1 7))) (projected 1 (x 0) (value 0)) (projected 1 (x 7) (value 0))
              (q .1)) continue)
    (evidence `((location (state 0) (x ,start))) continue))
  )

; Continue with new evidence (use this function to explore more evidence, with initial nil)
; Start is the location at which to start (or a list with a sequence of start locations)
; Repeat is the number of times to repeat (list of) start location(s)
; Initial says that this is the first run with these conditionals
(defun rlr7 (&optional start repeat initial)
  (unless repeat (setq repeat 1))
  (when (numberp start) (setq start (list start)))
  (catch 'interrupt-rl
    (dotimes (i repeat)
      (dolist (s start)
        (rle7 s (not initial))
        (format trace-stream "~&~&Start location: ~S" s)
        (when (eq (trials 1 (not initial)) interrupt-symbol)
          (throw 'interrupt-rl interrupt-symbol)
          )
        (setq initial nil)
        )
      )
    )
  )

; 1D Grid for RL

; Fixed vector of rewards for use in assign-reward
(defparameter rewards8 (vector 0 0 0 0 9 0 0 0))

(defun test-rl-print (stream)
  (format stream "~&~%PROJECTED FF (EV):~&")
  (pa 'projected nil '((expected wm-value)) stream)
  (format stream "~&~%Q FF (EV):~&")
  (pa 'q nil '((expected wm-value)) stream)
  (format stream "~&~%REWARD FF (EV):~&")
  (pa 'reward nil '((expected wm-value)) stream)
  (format stream "~&~%")
  )

(defun init-rl-grid8 (learn &optional start load-evidence model-free)
  (init '(left right) nil t) ; center-discrete-numeric-on-integer
;  (setq learning-rate-fraction-of-smoothing-parameter nil)
;  (setq trace-gdl '(projected-c))
  (operator-selection 'boltzmann)
  (when learn
    (learn '(:gd))
    (setq perceive-list '((assign-reward rewards7) perceive-list))
    )
  (when (eq learn :dp)
    (setq diachronic-prediction t)
    )
  (when (eq learn :am)
    (learn '(:am))
    )
  (setq post-d '((ppfn 'selected)
                 (ppfn 'location)
                 (when diachronic-prediction
                   (ppvn 'location*next)
                   )
                 )
        )

  (setq post-t '((test-rl-print trace-stream)
                 (when (and diachronic-prediction
                            (member 'transition (graph-conditionals cg))
                            )
                   (format trace-stream "~&~%TRANSITION FF:~&")
                   (pa (concat-symbols (list 'location- 'selected- 'location prediction-suffix)) nil nil trace-stream)
                   )
                 (format trace-stream "~&~%")
                 )
        )

  (new-type 'location :numeric t :discrete t :min 0 :max 8)
  (new-type 'utility :numeric t :discrete t :min 0 :max 10)

  (predicate 'location :world 'closed :arguments '((state state) (x location !)))
  (unless diachronic-prediction
    (predicate (concat-symbols (list 'location prediction-suffix)) :arguments '((state state) (x location %)))
    )
  (predicate 'projected
             :arguments '((x location) (value utility %))
             :function '((.1 * *) (1 0 0) (0 0 (1 10)) (1 4 0) (0 4 (1 10)) (1 7 0) (0 7 (1 10)))
             )
  (predicate 'projected*next
             :arguments '((x location) (value utility %))
             :function 'projected
             )
  (predicate 'reward :perception t
             :arguments '((x location) (value utility %))
             :function '((1 0 0) (0 0 (1 10)) (1 7 0) (0 7 (1 10)) (.1 (1 7) *))
             )
  (predicate 'q
             :arguments '((x location) (operator operator) (value utility %))
             :function '((.1 * * *)
                         (1 (0 2) left 0) (0 (0 2) left (1 10)) ; Don't go left from 1 (or 0)
                         (1 (6 8) right 0) (0 (6 8) right (1 10)) ; Don't go right from 6 (or 7)
                         )
             )

  ; Function predicates
  (unless automatic-action-models
    (predicate (concat-symbols (list 'location- 'selected- 'location prediction-suffix))
               :arguments '((location location) (operator operator) (next-location location %))
               :function (if diachronic-prediction
                             1
                           '((1 0 left 0) (1 1 left 0) (1 2 left 1) (1 3 left 2) (1 4 left 3) (1 5 left 4) (1 6 left 5) (1 7 left 6)
                             (1 7 right 7) (1 6 right 7) (1 5 right 6) (1 4 right 5) (1 3 right 4) (1 2 right 3) (1 1 right 2) (1 0 right 1))
                           )
               )
    )

  ; Q value for operator given present state
  (conditional 'q-c
               :conditions '((state (state (s)))
                             (location (state (s)) (x (x))))
               :condacts '((q (operator (o)) (x (x)) (value (q))))
               )

  ; Select operator based on Q value
  (conditional 'select-operator
               :conditions '((state (state (s)))
                             (location (state (s)) (x (x)))
                             (q (operator (o)) (x (x)) (value (q (:filter (* 0 .1))))))
               :actions '((selected (state (s)) (operator (o))))
               )

  ; Transition function
  (unless (or model-free automatic-action-models)
    (conditional 'transition
                 :conditions '((state (state (s)))
                               (location (state (s)) (x (x)))
                               (selected (state (s)) (operator (o))))
                 :condacts `((,(concat-symbols (list 'location prediction-suffix)) (state (s)) (x (nx)))
                             (,(concat-symbols (list 'location- 'selected- 'location prediction-suffix)) (location (x)) (operator (o)) (next-location (nx))))
                 )
    )

  ; Backup utility to projected
  (conditional 'backup-projected
               :conditions `((state (state (s)))
                             (location (state (s)) (x (x)))
                             (selected (state (s)) (operator (o)))
                             (,(concat-symbols (list 'location prediction-suffix)) (state (s)) (x (nx)))
                             (reward (x (nx)) (value (r)))
                             (projected*next (x (nx)) (value (p))))
               :actions '((projected (x (x)) (value (p (:coefficient .95 :offset r :pad 0 :apply-coefficient-to-offset t)))))
               )

  ; Backup utility to q
  (conditional 'backup-q
               :conditions `((state (state (s)))
                             (location (state (s)) (x (x)))
                             (selected (state (s)) (operator (o)))
                             (,(concat-symbols (list 'location prediction-suffix)) (state (s)) (x (nx)))
                             (reward (x (nx)) (value (r)))
                             (projected*next (x (nx)) (value (p))))
               :actions '((q (operator (o)) (x (x)) (value (p (:coefficient .95 :offset r :pad 0 :apply-coefficient-to-offset t)))))
               )

  ; Move left
  (conditional 'move-left
               :conditions '((state (state (s)))
                             (selected (state (s)) (operator left))
                             (location (state (s)) (x (x))))
               :actions '((location (state (s)) (x (x -1)))
                          )
               )

  ; Move right
  (conditional 'move-right
               :conditions '((state (state (s)))
                             (selected (state (s)) (operator right))
                             (location (state (s)) (x (x))))
               :actions '((location (state (s)) (x (x 1)))
                          )
               )

  ; Don't move to 0
  (conditional 'no-zero
               :conditions '((state (state (s)))
                             (location (state (s)) (x 1)))
               :actions '((selected - (state (s)) (operator left)))
               )

  ; Don't move to 7
  (conditional 'no-seven
               :conditions '((state (state (s)))
                             (location (state (s)) (x 6)))
               :actions '((selected - (state (s)) (operator right)))
               )

  ; Halt when reach location 4
  (conditional 'halt
               :conditions '((location (state 0) (x 4)))
               :actions '((halt))
               )

  (if diachronic-prediction
      (setq do-not-learn nil)
    (setq do-not-learn `(transition*function ,(concat-symbols (list 'location prediction-suffix))))
    )

  (when load-evidence (rle8 start nil))
  nil)

; Top level function to call for reinforcement learning
; If initial is true, initialize graph
; Start is the starting location in the grid
; Repeat is the number of trials to run
; Model-free determines whether include conditionals for action models
(defun rl-grid8 (learn &optional initial start repeat model-free)
  (if initial
      (init-rl-grid8 learn start (= repeat 0) model-free)
    (rle8 start nil))
  (if (= repeat 0)
      (d 0)
    (rlr8 start repeat initial))
  )

(defun test-rl-mb ()
  (rl-grid8 :gd t 1 30)
  (test-rl-print info-stream)
  (print-global-decision-statistics info-stream)
  )
(defun test-rl-dp ()
  (rl-grid8 :dp t 1 35)
  (test-rl-print info-stream)
  (format info-stream "~&~%TRANSITION FF:~&")
  (pa (concat-symbols (list 'location- 'selected- 'location prediction-suffix)) nil nil info-stream)
  (print-global-decision-statistics info-stream)
  )
(defun test-rl-am ()
  (rl-grid8 :am t 1 35)
  (test-rl-print info-stream)
  (format info-stream "~&~%TRANSITION FF:~&")
  (pa (predicate-name (graph-action-predicate cg)) nil nil info-stream) ; This way of finding the transition function is fragile
  (print-global-decision-statistics info-stream)
  )
(defun test-rl-mf ()
  (rl-grid8 :dp t 1 40 t)
  (test-rl-print info-stream)
  (print-global-decision-statistics info-stream)
  )

; Load evidence
(defun rle8 (&optional start continue)
  (unless start (setq start 2))
  (evidence `((location (state 0) (x ,start))) continue)
  (if open-world-wmfns
      (evidence `((location (state 0) (x ,start))
                  (projected .1 (x (1 7))) (projected 1 (x 0) (value 0)) (projected 1 (x 7) (value 0))
                  (q .1)) continue)
    (evidence `((location (state 0) (x ,start))) continue))
  )

; Continue with new evidence (use this function to explore more evidence, with initial nil)
; Start is the location at which to start (or a list with a sequence of start locations)
; Repeat is the number of times to repeat (list of) start location(s)
; Initial says that this is the first run with these conditionals
(defun rlr8 (&optional start repeat initial)
  (unless repeat (setq repeat 1))
  (when (numberp start) (setq start (list start)))
  (catch 'interrupt-rl
    (dotimes (i repeat)
      (dolist (s start)
        (rle8 s (not initial))
        (format trace-stream "~&~&Start location: ~S" s)
        (when (eq (trials 1 (not initial)) interrupt-symbol)
          (throw 'interrupt-rl interrupt-symbol)
          )
        (setq initial nil)
        )
      )
    )
  )

; Assign a fixed reward based on location
(defun assign-reward9 (rewards)
  (let ((cl (current-location)))
    (eval `(perceive (quote ((reward .1 (location-x *) (value *)) ; Empty WM of any previous rewards
                             (reward (location-x ,cl) (value ,(aref rewards cl))))))) ; Add reward for current state
    )
  )

; 1D Grid for RL with automatic RL structure generation

(defun init-rl-grid9 (&optional start load-evidence)
  (init '(left right) nil t) ; center-discrete-numeric-on-integer
  (operator-selection 'boltzmann)
  (learn '(:rl :am))
  (setq perceive-list '((assign-reward9 rewards7) perceive-list))
  (setq post-d '((ppfn 'selected)
                 (ppfn 'location)
                 (when diachronic-prediction
                   (ppvn 'location*next)
                   )
                 )
        )

  (setq post-t '((test-rl-print trace-stream)
                 (when (and diachronic-prediction
                            (member 'transition (graph-conditionals cg))
                            )
                   )
                 (format trace-stream "~&~%")
                 )
        )

  (new-type 'location :numeric t :discrete t :min 0 :max 8)

  (predicate 'location :world 'closed :arguments '((state state) (x location !)))

  ; Move left
  (conditional 'move-left
               :conditions '((state (state (s)))
                             (selected (state (s)) (operator left))
                             (location (state (s)) (x (x))))
               :actions '((location (state (s)) (x (x -1)))
                          )
               )

  ; Move right
  (conditional 'move-right
               :conditions '((state (state (s)))
                             (selected (state (s)) (operator right))
                             (location (state (s)) (x (x))))
               :actions '((location (state (s)) (x (x 1)))
                          )
               )

  ; Don't move to 0
  (conditional 'no-zero
               :conditions '((state (state (s)))
                             (location (state (s)) (x 1)))
               :actions '((selected - (state (s)) (operator left)))
               )

  ; Don't move to 7
  (conditional 'no-seven
               :conditions '((state (state (s)))
                             (location (state (s)) (x 6)))
               :actions '((selected - (state (s)) (operator right)))
               )

  ; Halt when reach location 4
  (conditional 'halt
               :conditions '((location (state 0) (x 4)))
               :actions '((halt))
               )

  (when load-evidence (rle8 start nil))
  nil)

; Brief init
(defun brief-init-rl-grid9 ()
  (setq center-discrete-numeric-on-integer t)
  )
; Load evidence
(defun rle9 (&optional start continue)
  (unless start (setq start 2))
  (evidence `((location (state 0) (x ,start))) continue)
  (if open-world-wmfns
      (evidence `((location (state 0) (x ,start))
                  (projected .1 (location-x (1 7))) (projected 1 (location- 0) (value 0)) (projected 1 (location-x 7) (value 0))
                  (q .1)) continue)
    (evidence `((location (state 0) (x ,start))) continue))
  )

; Continue with new evidence (use this function to explore more evidence, with initial nil)
; Start is the location at which to start (or a list with a sequence of start locations)
; Repeat is the number of times to repeat (list of) start location(s)
; Initial says that this is the first run with these conditionals
(defun rlr9 (&optional start repeat initial)
  (unless repeat (setq repeat 1))
  (when (numberp start) (setq start (list start)))
  (catch 'interrupt-rl
    (dotimes (i repeat)
      (dolist (s start)
        (rle9 s (not initial))
        (format trace-stream "~&~&Start location: ~S" s)
        (when (eq (trials 1 (not initial)) interrupt-symbol)
          (throw 'interrupt-rl interrupt-symbol)
          )
        (setq initial nil)
        )
      )
    )
  )

; Top level function to call for reinforcement learning
; If initial is true, initialize graph
; Start is the starting location in the grid
; Repeat is the number of trials to run
; Model-free determines whether include conditionals for action models
(defun rl-grid9 (&optional initial start repeat)
  (if initial
      (init-rl-grid9 start (= repeat 0))
    (brief-init-rl-grid9)
    )
  (if (= repeat 0)
      (d 0)
    (rlr9 start repeat initial))
  )

(defun test-rl-rl ()
  (rl-grid9 t 1 50)
  ; Print to listener
  (format trace-stream "~&~%TRANSITION FF:~&")
  (pa (predicate-name (graph-action-predicate cg))) ; This way of finding the transition function is fragile
  (print-global-decision-statistics trace-stream)
  ; Print to main regression testing window
  (format info-stream "~&~%TRANSITION FF:~&")
  (pa (predicate-name (graph-action-predicate cg)) nil nil info-stream) ; This way of finding the transition function is fragile
  (test-rl-print info-stream)
  (print-global-decision-statistics info-stream)
  )

; Program for 2 stage/step POMDP based in 1D corridor, adapted from what was automatically generated by Nicole's code

; Execute operator with some noise
(defun execute-pomdp-operator nil
  (let ((operator (operator-in-state base-level-state))
        (location (value-in-state 'x base-level-state 'x))
        (rand (random 1.0))
        )
    (when operator
      ; Make correct move 80% of time and no move 20%
      (case operator
        (left (when (< rand .8) (setq location (max (- location 1) 0))))
        (right (when (< rand .8) (setq location (min (+ location 1) 7))))
        )
      ; Add new location to WM and flush operator
      (evidence `((x (x ,location)) (selected 0 (state 0) (operator ,operator))))
      )
    )
  )

(defun init-pomdp nil
  (let ((observation-fn '((0 7 5) (0 7 4) (0 7 3) (0 7 2) (0 7 1) (0 7 0) (.1 7 6) (0.9 7 7)
                          (0 6 4) (0 6 3) (0 6 2) (0 6 1) (0 6 0) (.1 6 7) (.1 6 5) (0.8 6 6)
                          (0 5 7) (0 5 3) (0 5 2) (0 5 1) (0 5 0) (.1 5 6) (.1 5 4) (0.8 5 5)
                          (0 4 7) (0 4 6) (0 4 2) (0 4 1) (0 4 0) (.1 4 5) (.1 4 3) (0.8 4 4)
                          (0 3 7) (0 3 6) (0 3 5) (0 3 1) (0 3 0) (.1 3 4) (.1 3 2) (0.8 3 3)
                          (0 2 7) (0 2 6) (0 2 5) (0 2 4) (0 2 0) (.1 2 3) (.1 2 1) (0.8 2 2)
                          (0 1 7) (0 1 6) (0 1 5) (0 1 4) (0 1 3) (.1 1 2) (.1 1 0) (0.8 1 1)
                          (0 0 7) (0 0 6) (0 0 5) (0 0 4) (0 0 3) (0 0 2) (.1 0 1) (0.9 0 0)))
        (transition-fn '((1 7 7 NONE) (.2 7 7 LEFT) (0.8 7 6 LEFT) (1.0 7 7 RIGHT)
                         (1 6 6 NONE) (.2 6 6 LEFT) (0.8 6 5 LEFT) (.2 6 6 RIGHT) (0.8 6 7 RIGHT)
                         (1 5 5 NONE) (.2 5 5 LEFT) (0.8 5 4 LEFT) (.2 5 5 RIGHT) (0.8 5 6 RIGHT)
                         (1 4 4 NONE) (.2 4 4 LEFT) (0.8 4 3 LEFT) (.2 4 4 RIGHT) (0.8 4 5 RIGHT)
                         (1 3 3 NONE) (.2 3 3 LEFT) (0.8 3 2 LEFT) (.2 3 3 RIGHT) (0.8 3 4 RIGHT)
                         (1 2 2 NONE) (.2 2 2 LEFT) (0.8 2 1 LEFT) (.2 2 2 RIGHT) (0.8 2 3 RIGHT)
                         (1 1 1 NONE) (.2 1 1 LEFT) (0.8 1 0 LEFT) (.2 1 1 RIGHT) (0.8 1 2 RIGHT)
                         (1 0 0 NONE) (1.0 0 0 LEFT) (.2 0 0 RIGHT) (0.8 0 1 RIGHT)))
        (utility-fn '((0.1 7) (0.1 6) (0.1 5) (0.5 4) (1.0 3) (3 2) (1.0 1) (0.5 0)))
        )
        
    (init '(left right none))
    
    (setq pre-d '((pprefs)))
    (setq perceive-list '((execute-pomdp-operator)))
    (setq post-d '((format trace-stream "~&~%   >>> ~S(~S)" (operator-in-state base-level-state) (value-in-state 'x base-level-state 'x))))
    
    (new-type 'location :numeric t :discrete t :min 0 :max 8)
    
    (predicate 'x-1 :arguments '((x location %)))
    (predicate 'x-2 :arguments '((x location %)))
    (predicate 'x-3 :arguments '((x location %)))
    (predicate 'operator-1 :arguments '((name operator)))
    (predicate 'operator-2 :arguments '((name operator)))
    (predicate 'x :perception t :world 'closed :arguments '((state state) (x location !))) ; Actual location

    ; Function predicates
    (predicate 'observation :arguments '((x location) (x-1 location)))
    (predicate 'transition12 :arguments '((x-1 location) (operator operator) (x-2 location)))
    (predicate 'transition23 :arguments '((x-2 location) (operator operator) (x-3 location)))
    
    ; Noise in observing current state
    (CONDITIONAL 'OBSERVATION
                 :CONDITIONS '((X (state 0) (X (X)))) ; True state
                 :CONDACTS '((X-1 (X (X1))) ; Observed state
                             (observation (x (x)) (x-1 (x1))))
                 )
    
    (CONDITIONAL 'TRANSITION-STEP-1
                 :CONDACTS '((X-1 (X (X1)))
                             (X-2 (X (X2)))
                             (OPERATOR-1 (NAME (O)))
                             (transition12 (x-1 (x1)) (operator (o)) (x-2 (x2))))
                 )
    
    (CONDITIONAL 'TRANSITION-STEP-2
                 :CONDACTS '((X-2 (X (X2)))
                             (X-3 (X (X3)))
                             (OPERATOR-2 (NAME (O)))
                             (transition23 (x-2 (x2)) (operator (o)) (x-3 (x3))))
                 )
    
    (CONDITIONAL 'ACCEPTABLE
                 :CONDITIONS '((OPERATOR-1 (NAME (O))))
                 :ACTIONS '((SELECTED (state 0) (OPERATOR (O))))
                 )
    
    (CONDITIONAL 'GOAL-TEST
                 :CONDITIONS '((x (state 0) (x 2)))
                 :ACTIONS '((HALT))
                 )

    ; Function conditionals
    (CONDITIONAL 'OBSERVATION*function
                 :CONDACTS '((observation (x (x)) (x-1 (x1))))
                 :FUNCTION-VARIABLE-NAMES '(X X1)
                 :FUNCTION observation-fn
                 )

    (CONDITIONAL 'TRANSITION-STEP-1*function
                 :CONDACTS '((transition12 (x-1 (x1)) (operator (o)) (x-2 (x2))))
                 :FUNCTION-VARIABLE-NAMES '(X1 X2 O)
                 :FUNCTION transition-fn
                 )

    (CONDITIONAL 'UTILITY-STEP-1*function
                 :CONDACTS '((X-2 (X (X2))))
                 :FUNCTION-VARIABLE-NAMES '(X2)
                 :FUNCTION utility-fn
                 )

    (CONDITIONAL 'TRANSITION-STEP-2*function
                 :CONDACTS '((transition23 (x-2 (x2)) (operator (o)) (x-3 (x3))))
                 :FUNCTION-VARIABLE-NAMES '(X2 X3 O)
                 :FUNCTION transition-fn
                 )

    (CONDITIONAL 'UTILITY-STEP-2*function
                 :CONDACTS '((X-3 (X (X3))))
                 :FUNCTION-VARIABLE-NAMES '(X3)
                 :FUNCTION utility-fn
                 )
    
    (evidence '((x (state 0) (x 6))))
    )
  )

; Single stage POMDP (and, alternatively, random walk) for exploring learning open-world transition function in presence of both action and perception noise
; Learning does not yet work right in the POMDP, but it does for the random walk

; True location in world
(defvar pomdp1-location)

; Perceive results of operator with some noise
; Correct-prob is probability that peak is at correct location
; Correct-mass is how much of the probability mass is at the correct location
(defun perceive-pomdp1-location (&optional correct-prob correct-mass)
  (unless correct-prob (setq correct-prob .8))
  (unless correct-mass (setq correct-mass .8))
  (let ((rand (random 1.0))
        location ; Perceived location
        1-cm
        )
    (setq 1-cm (- 1 correct-mass))
    (setq location pomdp1-location)
    ; Perceive new location with correct-prob of getting right (and otherwise on one side)
    (cond ((= pomdp1-location 0)
           (when (>= rand correct-prob) (setq location 1))
           )
          ((= pomdp1-location 7)
           (when (>= rand correct-prob) (setq location 6))
           )
          (t
           (when (>= rand correct-prob)
             (if (< (random 1.0) .5)
                 (setq location (1- pomdp1-location))
               (setq location (1+ pomdp1-location)))
             )
           )
          )
    ; Zero out all perception for predicate x
    (perceive '((x 0)))
    ; Generate noisy perceptions based on correct-mass
    (perceive `((x ,correct-mass (state 0) (x ,location)))) ; Correct-mass at location
    ; Divide incorrect mass among adjacent locations when they exist
    (cond ((= location 0)
           (perceive `((x ,1-cm (state 0) (x 1))))
           )
          ((= location 7)
           (perceive `((x ,1-cm (state 0) (x 6))))
           )
          (t
           (perceive `((x ,(/ 1-cm 2) (state 0) (x ,(1- location)))
                       (x ,(/ 1-cm 2) (state 0) (x ,(1+ location)))
                       )
                     )
           )
          )
      )
  )

; Execute operator with some noise
(defun execute-pomdp1-operator (&optional correct-prob)
  (unless correct-prob (setq correct-prob .8))
  (let ((operator (operator-in-state base-level-state))
        (rand (random 1.0))
        )
    (when operator
      ; Make correct move action-noise percent of time
      (case operator
        (left (when (< rand correct-prob) (setq pomdp1-location (max (- pomdp1-location 1) 0))))
        (right (when (< rand correct-prob) (setq pomdp1-location (min (+ pomdp1-location 1) 7))))
        )
      )
    )
  )

; Create a random transition function
(defun random-transition-fn (nlocs ops)
  (let (tf ; transition-function
        rv) ; Random vector
    (when center-discrete-numeric-on-integer
      (setq nlocs (+ nlocs 1/2))
      )
    (dotimes (l nlocs)
      (dolist (op ops)
        (setq rv (normalize-vector (random-vector nlocs)))
        (dotimes (next-l nlocs)
          (setq tf (cons (list (aref rv next-l) l op next-l) tf))
          )
        )
      )
    (reverse tf))
  )

; POMDP
; Just execute this init and then run for a number of decisions (or call rd1 to run trials)
; This has not yet been updated for sigma25
(defun init-pomdp1 (&optional learn-transition perception-prob perception-mass action-prob)
  (let ((transition-fn '((1 6 NONE 6) (.2 6 LEFT 6) (0.8 6 LEFT 5) (.2 6 RIGHT 6) (0.8 6 RIGHT 7)
                         (1 5 NONE 5) (.2 5 LEFT 5) (0.8 5 LEFT 4) (.2 5 RIGHT 5) (0.8 5 RIGHT 6)
                         (1 4 NONE 4) (.2 4 LEFT 4) (0.8 4 LEFT 3) (.2 4 RIGHT 4) (0.8 4 RIGHT 5)
                         (1 3 NONE 3) (.2 3 LEFT 3) (0.8 3 LEFT 2) (.2 3 RIGHT 3) (0.8 3 RIGHT 4)
                         (1 2 NONE 2) (.2 2 LEFT 2) (0.8 2 LEFT 1) (.2 2 RIGHT 2) (0.8 2 RIGHT 3)
                         (1 1 NONE 1) (.2 1 LEFT 1) (0.8 1 LEFT 0) (.2 1 RIGHT 1) (0.8 1 RIGHT 2)
                         (1 0 NONE 0) (1 0 LEFT 0) (.2 0 RIGHT 0) (0.8 0 RIGHT 1)))
        (utility-fn '((0.05 7) (0.05 6) (0.05 5) (0.25 4) (.5 3) (1 2) (.5 1) (0.25 0)))
        )
        
    (init '(left right none))
    (setq do-not-learn '(utility)) ; Don't conver utility function over location into frequency of occurrence of location
    (when learn-transition
      (learn '(:gd :open))
      (setq diachronic-prediction t)
      )
    (operator-selection 'boltzmann)
    (setq pomdp1-location 6) ; Intialize true location if not running trials
    (setq pre-t '((setq pomdp1-location 6))) ; Initialize true location on each trial
    (setq pre-run '((format trace-stream "~&~%True location: ~S" pomdp1-location)
                    (when (= pomdp1-location 2) (halt))
                    ))
    (setq perceive-list `((perceive-pomdp1-location ,perception-prob ,perception-mass)))
    (setq action-list `((execute-pomdp1-operator ,action-prob)))
    (setq pre-d '((format trace-stream "~&~%Distribution over next perceived location: ") (ppvn 'x*next)
                  (format trace-stream "~&~%Distribution calculated over current location: ") (ppvn 'x)
                  (format trace-stream "~&") (pprefs)
                  ))
    (setq post-d '((format trace-stream "~&~%Best guess as to current location: ~S" (nonstate-value 'x*selected))
                   (format trace-stream "~&Selected action: ~S" (operator-in-state base-level-state))
;                   (unless (plm-full (node-function (predicate-wm (predicate-from-name 'selected*next))))
;                       (format trace-stream "~&Next action: ~S" (value-in-state 'selected*next 0 'operator)))
                   ))
    (when learn-transition
      (setq post-t (append post-d '((format trace-stream "~&~%Transition function: ") (pcf 'transition*function))))
      )
    
    (new-type 'location :numeric t :discrete t :min 0 :max 8)
    
    (predicate 'x :perception t :arguments '((state state) (x location %)))
    (predicate 'x*selected :world 'closed :arguments '((x location !))) ; Left state off here so that updating it doesn't flush operator
    (unless learn-transition
      (predicate 'x*next :arguments '((state state) (x location %)))
      (predicate 'selected*next :arguments '((state state) (operator operator %)))
      )

    ; Function predicates
    (predicate 'transition :arguments '((x location) (operator operator) (x*next location %)))

    (conditional 'transition-c
                 :condacts '((x (state 0) (x (x1)))
                             (selected*next (state 0) (operator (o)))
                             (x*next (state 0) (x (x2)))
                             (transition (x (x1)) (operator (o)) (x*next (x2)))
                             )
                 )
    
    (conditional 'acceptable
                 :conditions '((selected*next (state 0) (operator (o))))
                 :actions '((selected (state 0) (operator (o))))
                 )

    (conditional 'x*selected
                 :conditions '((x (state 0) (x (x))))
                 :actions '((x*selected (x (x))))
                 )

    ; Function conditionals
        
    (conditional 'utility*function
                 :condacts '((x*next (state 0) (x (x2))))
                 :function-variable-names '(x2)
                 :function utility-fn
                 )

    (conditional 'transition*function
                 :condacts '((transition (x (x1)) (operator (o)) (x*next (x2))))
                 :function-variable-names '(x1 o x2)
                 :normal 'x2
                 :function (if learn-transition
                               (if (eq learn-transition 'random)
                                   (random-transition-fn (stype-max (type-from-name 'location)) (stype-constants (type-from-name 'operator)))
                                 '((.125 * * *)))
                             transition-fn)
                 )

    t)
  )

; Run trials of pomdp1
(defun rd1 (&optional trials learn-transition)
  (unless trials (setq trials 1))
  (init-pomdp1 learn-transition)
  (trials trials)
  )

; Random walk
; Just execute this init and then run for a number of decisions (or call rrw to run trials)
(defun init-rw (&optional learn-transition perception-prob perception-mass action-prob)
  (let ((transition-fn '((1 6 NONE 6) (.2 6 LEFT 6) (0.8 6 LEFT 5) (.2 6 RIGHT 6) (0.8 6 RIGHT 7)
                         (1 5 NONE 5) (.2 5 LEFT 5) (0.8 5 LEFT 4) (.2 5 RIGHT 5) (0.8 5 RIGHT 6)
                         (1 4 NONE 4) (.2 4 LEFT 4) (0.8 4 LEFT 3) (.2 4 RIGHT 4) (0.8 4 RIGHT 5)
                         (1 3 NONE 3) (.2 3 LEFT 3) (0.8 3 LEFT 2) (.2 3 RIGHT 3) (0.8 3 RIGHT 4)
                         (1 2 NONE 2) (.2 2 LEFT 2) (0.8 2 LEFT 1) (.2 2 RIGHT 2) (0.8 2 RIGHT 3)
                         (1 1 NONE 1) (.2 1 LEFT 1) (0.8 1 LEFT 0) (.2 1 RIGHT 1) (0.8 1 RIGHT 2)
                         (1 0 NONE 0) (1 0 LEFT 0) (.2 0 RIGHT 0) (0.8 0 RIGHT 1)))
        )
        
    (init '(left right none))
    (when learn-transition
        (if (eq learn-transition ':am)
            (learn '(:am))
          (progn
            (learn '(:gd))
            (setq diachronic-prediction t)
            ))
        )
    (setq pomdp1-location 6) ; Intialize true location if not running trials
    (setq pre-t '((setq pomdp1-location 6))) ; Initialize true location on each trial
    (setq pre-run '((format trace-stream "~&~%True location: ~S" pomdp1-location)
                    (format trace-stream "~&Perceived location distribution: ") (pppfn 'x)
                    (when (= pomdp1-location 2) (halt))
                    ))
    (setq perceive-list `((perceive-pomdp1-location ,perception-prob ,perception-mass)))
    (setq action-list `((execute-pomdp1-operator ,action-prob)))
    (setq post-run '((format trace-stream "~&~%Distribution calculated over next location: ") (ppvn 'x*next)))
    (setq post-d '((format trace-stream "~&~%Distribution over current location:") (parray 'x t '((constant wm-state 0)))
                   (format trace-stream "~&~%Best guess as to current location: ") (pa 'x*selected)
                   (format trace-stream "~&~%Selected action: ~S" (operator-in-state base-level-state))
                   ))
    (when learn-transition
      (setq post-t (append post-d '((format trace-stream "~&~%Transition function: ") (parray (predicate-name (car (graph-predicates cg))) nil nil trace-stream))))
      )
    
    (new-type 'location :numeric t :discrete t :min 0 :max 8)
    
    (predicate 'x :world 'closed :perception t :arguments '((state state) (x location %)))
    (predicate 'x*selected :world 'closed :arguments '((x location !))) ; Used to capture current best estimate on location for printing

    (unless learn-transition
      (predicate 'x*next :arguments '((state state) (x location %)))
      )
    (unless automatic-action-models
      ; Function predicate
      (predicate 'transition
                 :arguments '((x1 location) (operator operator) (x2 location %))
                 :function (if learn-transition
                               '((.125 * * *))
                             transition-fn))

      (conditional 'transition-c
                   :conditions '((x (state 0) (x (x1)))
                                 (selected (state 0) (operator (o))))
                   :condacts '((x*next (state 0) (x (x2)))
                               (transition (x1 (x1)) (operator (o)) (x2 (x2))))
                   )
      )
    
    (conditional 'acceptable
                 :actions '((selected (state 0) (operator left))
                            (selected (state 0) (operator right))
                            (selected (state 0) (operator none))
                            )
                 )

    (conditional 'x*selected
                 :conditions '((x*next (state 0) (x (x)))
                               )
                 :actions '((x*selected (x (x))))
                 )

    ; The following is necessary to terminate the operator because x*selected doesn't change when we see ourselves in the same location (mostly for NONE)
    (conditional 'reject-selected-operator
                 :conditions '((selected (state 0) (operator (o))))
                 :actions '((selected - (state 0) (operator (o))))
                 )
#|
    ; Function conditionals
    (unless automatic-action-models
      (conditional 'transition*function
                   :condacts '((transition (x1 (x1)) (operator (o)) (x2 (x2))))
                   :function-variable-names '(x1 o x2)
                   :normal 'x2
                   :function (if learn-transition
                                 '((.125 * * *))
                               transition-fn)
                   )
      )
|#
    t)
  )

; Run trials of random walk
(defun rrw (&optional trials learn-transition perception-prob perception-mass action-prob)
  (unless trials (setq trials 1))
  (init-rw learn-transition perception-prob perception-mass action-prob)
  (trials trials)
  )

(defun test-rw-al ()
  (rrw 100 :am .9 .9 .9)
  (format info-stream "~&~%Transition function: ") (parray (predicate-name (car (graph-predicates cg))) nil nil info-stream)
  (print-global-decision-statistics info-stream)
  )

; Test program for impasses
; First get NONE at state 0, then a TIE at state 1, then resolve TIE by selecting operator a,
; then NO-CHANGE on operator a, then obviate the no-change by selection operator b for state 0,
; then apply operator b, and halt.

(defun ti nil
  (init)
  (init-operators 'symbols '(a b) t) ; Detect impasses
  (setq post-d '((ppfn 'selected) (ppfn 'impasse)))

  (predicate 'test :world 'closed :arguments '((state state) (test boolean !)))

  (conditional 'none-tie
               :conditions '((impasse (state 1) (type none)))
               :actions '((selected (state 1) (operator *)))
               :function .1
               )

  (conditional 'resolve-tie
               :conditions '((impasse (state 2) (type tie)))
               :actions '((selected (state 1) (operator a)))
               )

  (conditional 'obviate-no-change
               :conditions '((selected (state 1) (operator a))
                             (impasse (state 2) (type no-change)))
               :actions '((selected (state 0) (operator b)))
               )

  (conditional 'apply-b
               :conditions '((selected (state (s)) (operator b)))
               :actions '((test (state (s)) (test true)))
               )

  (conditional 'halt
               :conditions '((test (state 0) (test true)))
               :actions '((halt))
               )

  (evidence '((test (state 0) (test false))))
  )

; Test program for multiagent impasses
(defun tmi nil
  (init nil 3)
  (init-operators 'symbols '(a b) t) ; Detect impasses
  (setq post-d '((ppfn 'selected) (ppfn 'impasse)))

  (predicate 'test :world 'closed :arguments '((state state) (agent agent) (test boolean !)))

  (conditional 'none-tie
               :conditions '((impasse (state 1) (agent (a)) (type none)))
               :actions '((selected (state 1) (agent (a)) (operator *)))
               :function .1
               )

  (conditional 'resolve-tie
               :conditions '((impasse (state 2) (agent (a (:filter (0 1 0) (2 1 0)))) (type tie)))
               :actions '((selected (state 1) (agent (a)) (operator a)))
               )

  (conditional 'obviate-no-change
               :conditions '((selected (state 1) (agent (a)) (operator a))
                             (impasse (state 2) (agent (a)) (type no-change)))
               :actions '((selected (state 0) (agent (a)) (operator b)))
               )

  (conditional 'apply-b
               :conditions '((selected (state (s)) (agent (a)) (operator b)))
               :actions '((test (state (s)) (agent (a)) (test true)))
               )

  (conditional 'halt
               :conditions '((test (state 0) (agent (a)) (test true)))
               :actions '((halt))
               )

  (evidence '((test (state 0) (agent *) (test false))))
  )

; Test multiple actions
(defun tma nil
  (init)
  (predicate 'test :world 'closed :arguments '((x boolean) (y boolean)))

  (conditional 'test
               :conditions '((test (x (a)) (y (b))))
               :actions '((test (x (a)) (y (b)))
                          (test (x (b)) (y (a))))
               :function .5)
  (evidence '((test (x true) (y false))))
  )

; Test filters with negative results
; Should floor outgoing messages at 0
(defun tf nil
  (init)
  (new-type 'symmetric :numeric t :min -10 :max 10)
  (predicate 'test :world 'closed :arguments '((test symmetric !)))
  (predicate 'act :world 'closed :arguments '((test symmetric !)))

  (conditional 'pass-through
               :conditions '((test (test (t (:filter (* -.5 .1))))))
               :actions '((act (test (t))))
               )

  (conditional 'summarize
               :conditions '((test (test (:filter (* 0 .1)))))
               )

  (conditional 'summarize-1
               :conditions '((test (test (:filter (* -.5 .1)))))
               )

  (conditional 'summarize+1
               :conditions '((test (test (:filter (* .5 .1)))))
               )
  (evidence '((test (test *))))
  )

; Variation on SLAM (simple localization) that uses true perception and diachronic processing to handle representation of two locations

; True location in world
(defvar diachronic-location)
(defvar diachronic-map (vector 'wall 'door1 'none 'none 'door2 'wall))

; Perceive results of operator with some noise
; Correct-prob is probability that peak is at correct location
; Correct-mass is how much of the probability mass is at the correct location
(defun perceive-diachronic-object (&optional correct-prob)
  (unless correct-prob (setq correct-prob 1.0))
  (let ((rand (random 1.0))
        location ; Perceived location
        )
    (setq location diachronic-location)
    ; Perceive new location with correct-prob of getting right (and otherwise on one side)
    (cond ((= diachronic-location 0)
           (when (>= rand correct-prob) (setq location 1))
           )
          ((= diachronic-location 5)
           (when (>= rand correct-prob) (setq location 4))
           )
          (t
           (when (>= rand correct-prob)
             (if (< (random 1.0) .5)
                 (setq location (1- diachronic-location))
               (setq location (1+ diachronic-location)))
             )
           )
          )
    ; Generate object perception
    (perceive `((observed 1 (state 0) (value ,(aref diachronic-map location)))))
    )
  )

; Execute operator with some noise
(defun execute-diachronic-operator (&optional correct-prob)
  (unless correct-prob (setq correct-prob 1))
  (let ((operator (operator-in-state base-level-state))
        (rand (random 1.0))
        )
    (when operator
      ; Make correct move action-noise percent of time
      (case operator
        (left (when (< rand correct-prob) (setq diachronic-location (max (- diachronic-location 1) 0))))
        (right (when (< rand correct-prob) (setq diachronic-location (min (+ diachronic-location 1) 5))))
        )
      )
    )
  )

(defun diachronic-slam (&optional initial-loc perception-prob action-prob attention)
  (init '(left right stay))
  (when attention
    (setq trace-attention t)
    (setq compute-attention t)
    (setq compute-surprise t)
    )
  (setq detect-impasses nil)
  (learn '(:pm :no-normal))
  (new-type 'loc :numeric t :discrete t :min 0 :max 6)
  (new-type 'obj :constants '(wall door1 door2 none))

  (predicate 'location :world 'closed :replace t :arguments '((state state) (value loc %)))
  (predicate 'observed :perception t :arguments '((state state) (value obj %)))

  ; Function predicates
  (predicate 'transition
             :arguments '((loc loc) (operator operator) (locn loc %))
             :learning-rate 0
             :function '((1 5 RIGHT 5) (1 0 LEFT 0) (1 5 STAY 5) (1 4 STAY 4) (1 3 STAY 3) (1 2 STAY 2) (1 1 STAY 1) (1 0 STAY 0) 
                         (0.8 5 LEFT 4) (0.8 4 LEFT 3) (0.8 3 LEFT 2) (0.8 2 LEFT 1) (0.8 1 LEFT 0)
                         (0.8 4 RIGHT 5) (0.8 3 RIGHT 4) (0.8 2 RIGHT 3) (0.8 1 RIGHT 2) (0.8 0 RIGHT 1)
                         (0.2 0 RIGHT 0) (0.2 1 RIGHT 1) (0.2 2 RIGHT 2) (0.2 3 RIGHT 3) (0.2 4 RIGHT 4) 
                         (0.2 1 LEFT 1) (0.2 2 LEFT 2) (0.2 3 LEFT 3) (0.2 4 LEFT 4) (0.2 5 LEFT 5))
             )

  (setq pre-run '((when trace-attention
                    (format trace-stream "~&Perceived surprise function: ~&")
                    (pppfn (predicate-name (car (graph-predicates cg)))) ; Hack leverages that surprise predicate is last created
                    )
                  (format trace-stream "~&~%True location: ~S" diachronic-location)
                  ))
  (setq perceive-list `((perceive-diachronic-object ,perception-prob)))
  (setq action-list `((execute-diachronic-operator ,action-prob)))
  
  (setq pre-d '(
                (format trace-stream "~&~%Distribution over next location: ~&") (ppvn 'location*next)
                ))

  (setq post-d '((format trace-stream "~&~%Distribution over current location: ") (parray 'location nil '((constant wm-state 0)))
;                 (format trace-stream "~&~%Learned map function: ") (parray 'locn-observed) (parray 'locn-observed nil '((argmax obj)))
                 (let ((perception-predicate-name (predicate-name (find-if #'predicate-automated-perception (graph-predicates cg)))))
                       (format trace-stream "~&~%Learned map function: ")
                       (parray perception-predicate-name)
                       (parray perception-predicate-name nil '((argmax wm-value-1)))
                       )
                 (format trace-stream "~&~%Action selected: ") (parray 'selected nil '((constant wm-state 0) (argmax wm-operator)))
                 (format trace-stream "~&~%")
                 (when trace-attention
                   (format trace-stream "~&Learned surprise function: ~&")
                   (ppf (predicate-name (car (graph-predicates cg))) t) ; Hack leverages that surprise predicate is last created
                   )
                 ))

  (conditional 'loc-command-locn ; this conditional for action.
               :conditions '((state (state (s)))
                             (location (state (s)) (value (loc)))
                             (selected (state (s)) (operator (o)))
                             )
               :condacts '((location*next (state (s)) (value (locn)))
                           (transition (loc (loc)) (operator (o)) (locn (locn))))
               )

    (conditional 'acceptable
                 :actions '((selected (state 0) (operator left))
                            (selected (state 0) (operator right))
                            (selected (state 0) (operator stay))
                            )
                 )

    (conditional 'reject
                 :conditions '((selected (state 0) (operator (o))))
                 :actions '((selected - (state 0) (operator (o))))
                 )

    (setq diachronic-location (if initial-loc initial-loc 3))

    (evidence `((location .1 (state 0)) (location .5 (state 0) (value ,diachronic-location))
                (selected (state 0) (operator stay))
                ))
   )


(defun dsl (&optional initial-loc perception-prob action-prob)
  (diachronic-slam initial-loc perception-prob action-prob)
  )

(defun test-slam (&optional attention)
  (diachronic-slam nil nil nil attention)
  (d 300)
;  (format info-stream "~&~%Learned map function: ") (parray 'locn-observed nil nil info-stream) (parray 'locn-observed nil '((argmax obj)) info-stream)
  (format info-stream "~&~%Learned map function: ")
  (let ((perception-predicate-name (predicate-name (find-if #'predicate-automated-perception (graph-predicates cg)))))
    (parray perception-predicate-name nil nil info-stream)
    (parray perception-predicate-name nil '((argmax wm-value-1)) info-stream)
    )
  (print-global-decision-statistics info-stream)
  )

;; TEST SYMBOLIC TYPE EXTENSION
;; CALLING (test-extend-type) CREATES A POS TAGGER MODEL, TRAINS IT WITH WORDS THAT ARE NOT DEFINED AND FINALLY TESTS IT.

; All symbolic
(defun test-extend-type ()
  (pos-tagger-model)
  (train-pos-model)
  (test-pos-model)
)

; With a discrete numeric variable
(defun test-extend-type1 ()
  (pos-tagger-model1)
  (train-pos-model1)
  (test-pos-model1)
)

; With a continuous numeric variable
(defun test-extend-type2 ()
  (pos-tagger-model2)
  (train-pos-model1)
  (test-pos-model1)
)

; Using perception rather than evidence
(defun test-extend-type3 ()
  (pos-tagger-model)
  (train-pos-model3)
  (test-pos-model3)
)

; Define an attribute
(defun define-attribute-pos (attribute value)
  (new-type attribute :constants value)
  (predicate (intern (concatenate 'string (symbol-name attribute)"-PRED"))  :world 'open :perception t :arguments `((,attribute ,attribute %)))
  )

(defun pos-tagger-model ()
  (init)
  (setf extend-type-constants-by-evidence t)
  (define-attribute-pos 'pos '(n v))
  (define-attribute-pos 'prior-pos '(a n))
  (define-attribute-pos 'word '(add be unknown))

  ; Function predicates
  (predicate 'prior-pos-word-pos :arguments '((prior-pos prior-pos %) (word word %) (post-pos pos %)))
  (predicate 'word-pos :arguments '((word word %) (pos pos %)))

  
  (CONDITIONAL 'POS-PRIOR-POS-WORD
               :CONDACTS '(
                           (PRIOR-POS-PRED (PRIOR-POS (PRIOR-POS)))
                           (WORD-PRED (WORD (WORD)))
                           (POS-PRED (POS (POS)))
                           (prior-pos-word-pos (prior-pos (prior-pos)) (word (word)) (post-pos (pos)))
                           )
               )



  (CONDITIONAL 'POS-WORD
               :CONDACTS '(
                           (WORD-PRED (WORD (WORD)))
                           (POS-PRED (POS (POS)))
                           (word-pos (word (word)) (pos (pos)))
                           )
               )

  ; Function conditionals

  (CONDITIONAL 'WORD*function
               :CONDACTS '((WORD-PRED (WORD (WORD))))
               :FUNCTION-VARIABLE-NAMES '(WORD)
               :NORMAL 'WORD
               :FUNCTION 1/3
               )

  (CONDITIONAL 'prior-pos-word-pos*function
               :CONDACTS '(
                           (prior-pos-word-pos (prior-pos (prior-pos)) (word (word)) (post-pos (pos)))
                           )
               :FUNCTION-VARIABLE-NAMES '(PRIOR-POS WORD POS)
               :NORMAL 'POS
               :FUNCTION 1/2
               )

  (CONDITIONAL 'word-pos*function
               :CONDACTS '((word-pos (word (word)) (pos (pos))))
               :FUNCTION-VARIABLE-NAMES '(WORD POS)
               :NORMAL 'POS
               :FUNCTION '((0.6 ADD V) (0.4 ADD N) (0.9 UNKNOWN N) (0.1 UNKNOWN V) (0.1 BE N) (0.9 BE V))
               )

  )

(defun train-pos-model nil
  (learn '(:gd))

  ;Introduce word "HAVE"
  (perceive '((POS-PRED (POS N)) (PRIOR-POS-PRED (PRIOR-POS A)) (WORD-PRED (WORD HAVE))) t)
  (d 1)
  (format trace-stream "~&") (pcfs)

  ;NOTHING NEW IS INTRODUCED
  (perceive '((POS-PRED (POS V)) (PRIOR-POS-PRED (PRIOR-POS N)) (WORD-PRED (WORD HAVE))) t)
  (d 1)
  (format trace-stream "~&") (pcfs)
  
  ;INTRODUCE word "LET" and pos "A"
  (perceive '((POS-PRED (POS A)) (PRIOR-POS-PRED (PRIOR-POS A)) (WORD-PRED (WORD LET))) t)
  (d 1)
  (format trace-stream "~&") (pcfs)
  )

(defun test-pos-model nil 
  (learn)

  (setq pre-d '((ppwm 'pos-pred)))

  (perceive '((PRIOR-POS-PRED (PRIOR-POS A)) (WORD-PRED (WORD HAVE))) t)
  (d 1)
  )

(defun train-pos-model3 nil
  (learn '(:gd))

  ;Introduce word "HAVE"
  (setq perceive-list '((perceive '((POS-PRED (POS N)) (PRIOR-POS-PRED (PRIOR-POS A)) (WORD-PRED (WORD HAVE))) t)))
  (d 1)
  (format trace-stream "~&") (pcfs)

  ;NOTHING NEW IS INTRODUCED
  (setq perceive-list '((perceive '((POS-PRED (POS V)) (PRIOR-POS-PRED (PRIOR-POS N)) (WORD-PRED (WORD HAVE))) t)))
  (d 1)
  (format trace-stream "~&") (pcfs)
  
  ;INTRODUCE word "LET" and pos "A"
  (setq perceive-list '((perceive '((POS-PRED (POS A)) (PRIOR-POS-PRED (PRIOR-POS A)) (WORD-PRED (WORD LET))) t)))
  (d 1)
  (format trace-stream "~&") (pcfs)
  )

(defun test-pos-model3 nil 
  (learn)

  (setq pre-d '((ppwm 'pos-pred)))

  (setq perceive-list '((perceive '((PRIOR-POS-PRED (PRIOR-POS A)) (WORD-PRED (WORD HAVE))) t)))
  (d 1)
  )


(defun pos-tagger-model1 ()
  (init)
  (setf extend-type-constants-by-evidence t)
  (define-attribute-pos 'pos '(n v))
  (new-type 'prior-pos :numeric t :discrete t :min 0 :max 2)
  (predicate 'prior-pos-pred  :world 'open :perception t :arguments '((value prior-pos %)))
    
  (define-attribute-pos 'word '(add be unknown))

  (CONDITIONAL 'WORD
               :CONDACTS '((WORD-PRED (WORD (WORD))))
               :FUNCTION-VARIABLE-NAMES '(WORD)
               :NORMAL 'WORD
               :FUNCTION '((1/3 *)) 
               )
  


  (CONDITIONAL 'POS-PRIOR-POS
               :CONDACTS '(
                          (PRIOR-POS-PRED (VALUE (PRIOR-POS)))
                          (POS-PRED (POS (POS)))
                          )
              :FUNCTION-VARIABLE-NAMES '(PRIOR-POS POS)
              :NORMAL 'PRIOR-POS
              :FUNCTION '((1/2 * *))
               )
 
  (CONDITIONAL 'POS-WORD
               :CONDACTS '(
                           (WORD-PRED (WORD (WORD)))
                           (POS-PRED (POS (POS)))
                           )
               :FUNCTION-VARIABLE-NAMES '(WORD POS)
               :NORMAL 'POS
               :FUNCTION '((0.6 ADD V) (0.4 ADD N) (0.9 UNKNOWN N) (0.1 UNKNOWN V) (0.1 BE N) (0.9 BE V))
               )

  )

(defun train-pos-model1 nil
  (learn '(:gd))

  ;Introduce word "HAVE"
  (perceive '((POS-PRED (POS N)) (PRIOR-POS-PRED (VALUE 0)) (WORD-PRED (WORD HAVE))) t)
  (d 1)
  ;(pfs)

  ;NOTHING NEW IS INTRODUCED
  (perceive '((POS-PRED (POS V)) (PRIOR-POS-PRED (VALUE 1)) (WORD-PRED (WORD HAVE))) t)
  (d 1)
  ;(pfs)
  
  ;INTRODUCE word "LET" and pos "A"
  (perceive '((POS-PRED (POS A)) (PRIOR-POS-PRED (VALUE 0)) (WORD-PRED (WORD LET))) t)
  (d 1)
  ;(pfs)
  )

(defun test-pos-model1 nil 
  (learn)

  (setq pre-d '((ppwm 'pos-pred)))

  (perceive '((PRIOR-POS-PRED (VALUE 0)) (WORD-PRED (WORD HAVE))) t)
  (d 1)
  )

(defun pos-tagger-model2 ()
  (init)
  (setf extend-type-constants-by-evidence t)
  (define-attribute-pos 'pos '(n v))
  (new-type 'prior-pos :numeric t :min 0 :max 2)
  (predicate 'prior-pos-pred  :world 'open :perception t :arguments '((value prior-pos %)))
    
  (define-attribute-pos 'word '(add be unknown))

  (CONDITIONAL 'WORD
               :CONDACTS '((WORD-PRED (WORD (WORD))))
               :FUNCTION-VARIABLE-NAMES '(WORD)
               :NORMAL 'WORD
               :FUNCTION '((1/3 *)) 
               )
  


  (CONDITIONAL 'POS-PRIOR-POS
               :CONDACTS '(
                          (PRIOR-POS-PRED (VALUE (PRIOR-POS)))
                          (POS-PRED (POS (POS)))
                          )
              :FUNCTION-VARIABLE-NAMES '(PRIOR-POS POS)
              :NORMAL 'PRIOR-POS
              :FUNCTION '((1/2 * *))
               )
 
  (CONDITIONAL 'POS-WORD
               :CONDACTS '(
                           (WORD-PRED (WORD (WORD)))
                           (POS-PRED (POS (POS)))
                           )
               :FUNCTION-VARIABLE-NAMES '(WORD POS)
               :NORMAL 'POS
               :FUNCTION '((0.6 ADD V) (0.4 ADD N) (0.9 UNKNOWN N) (0.1 UNKNOWN V) (0.1 BE N) (0.9 BE V))
               )

  )

; Test incrementally storing graded values in closed-world universal predicates
(defun tg ()
  (init)
  (predicate 'test1 :world 'closed :arguments '((arg boolean)))
  (predicate 'test2 :world 'closed :arguments '((arg boolean)))
  (predicate 'test3 :world 'closed :arguments '((arg boolean)))

  (conditional 'test-graded
               :conditions '((test1 (arg (a))))
               :actions '((test2 (arg (a))))
               )
  (conditional 'test-cond
               :conditions '((test2 (arg (a))))
               )
  (evidence '((test1 .7 (arg true))))
  (d 1)
  (ppfn 'test1 nil 'array)
  (ppfn 'test2 nil 'array)
  (evidence '((test1 - (arg true)) (test1 .2 (arg false))))
  (d 1)
  (ppfn 'test1 nil 'array)
  (ppfn 'test2 nil 'array)
  )

; Test use of exponentiation in a condition
(defun tec ()
  (init)
  (predicate 'test1 :world 'closed :arguments '((arg boolean)))
  (predicate 'test2 :world 'closed :arguments '((arg boolean)))

  (conditional 'test-exponentiate-condition
               :conditions '((test1 ^ (arg (a))))
               :actions '((test2 (arg (a))))
               )

  (evidence '((test1 .2 (arg false)) (test1 .7 (arg true))))
  (d 1)
  (ppfn 'test2 nil 'array)
  )

; Test use of probability matching in a decision
(defun tpm nil
  (init '(a b))
  (operator-selection 'prob-match)
;  (setq trace-wm-changes t)

  ; Function predicates
  (predicate 'operator :arguments '((operator operator %)))

  (conditional 'pmatch
               :conditions '((operator (operator (o))))
               :actions '((selected (state 0) (operator (o))))
               )

  ; Function conditionals
  (conditional 'operator*function
               :condacts '((operator (operator (o))))
               :function-variable-names '(o)
               :function '((.9 a) (.1 b)))
  (d 1)
  (format trace-stream "~&") (parray 'selected nil '((constant wm-state 0) (argmax wm-operator)))
  )

; Test adding a conditional after graph has already been initialized
; Simple linked production
(defun tca ()
  (init)
  (new-type 'id :constants '(i1 i2 i3 i4 i5 i6 i7))
  
  (predicate 'next :world 'closed :arguments '((id id) (value id)))

  (setq post-d '((ppfn 'next nil 'array)))
  
  (conditional 'trans
               :conditions '(
                             (next (id (a)) (value (b)))
                             (next (id (b)) (value (c)))
                             )
               :actions '((next (id (a)) (value (c))))
               )

  (evidence '((next (id i1) (value i2))
              (next (id i2) (value i3))
              ))
  (d 1)

  (conditional 'reverse
               :conditions '(
                             (next (id (a)) (value (b)))
                             )
               :actions '((next (id (b)) (value (a))))
               )
  (d 1)
  t)

; Simple program for early chunking experiments
(defun chunk nil
  (init)
  (learn '(:gd))
  (setq diachronic-prediction t)
  (predicate 'in1 :world 'closed :arguments '((state state) (a boolean) (b boolean !)))
  (predicate 'in2 :world 'closed :arguments '((state state) (a boolean) (b boolean !)))
  (predicate 'out :world 'closed :arguments '((state state) (a boolean) (b boolean !)))

  (conditional 'in1-out
               :conditions '((in1 (state (s)) (a (a)) (b (b))))
               :actions '((out (state (s)) (a (a)) (b (b))))
               )

  (conditional 'in2-out
               :conditions '((in2 (state (s)) (a (a)) (b (b))))
               :actions '((out (state (s)) (a (a)) (b (b))))
               )

  (evidence '((in1 (state 0) (a true) (b false))
              (in2 (state 0) (a false) (b true)))
            )

  (create-chunk (predicate-from-name 'out))
  (pcs)

  t)

; Simple program to test deletion of operator (and not replacing it with anything)
(defun ado nil
  (init '(a b))

  (setq post-d '((format trace-stream "~&~%Selected: ") (parray 'selected nil '((constant wm-state 0) (argmax wm-operator)))))

  (conditional 'acceptable
               :actions '((selected (state 0) (operator a)))
               )

  (conditional 'reject
               :conditions '((selected (state 0) (operator a)))
               :actions '((selected - (state 0) (operator a)))
               )
  t)

(defun ado2 nil
  (init)
  (predicate 'boolean :arguments '((temp boolean)))
  (init-operators 'predicates '(boolean) t)

  (setq post-d '((format trace-stream "~&~%Selected: ") (parray 'selected nil '((constant wm-state 0) (argmax wm-operator)))
                 (format trace-stream "~&~%Impasse: ") (parray 'impasse nil '((max wm-operator) (argmax wm-type)))
                 ))

  (conditional 'acceptable
               :actions '((selected (state 0) (operator 0)))
               )

  (conditional 'reject
               :conditions '((selected (state 0) (operator 0))
                             (impasse (state 1) (type no-change) (operator 0))
                             )
               :actions '((selected - (state 0) (operator 0)))
               )
  t)

(defun ado3 nil
  (init nil '(a b))
  (predicate 'boolean :arguments '((temp boolean)))
  (init-operators 'predicates '(boolean) t)

  (setq post-d '((format trace-stream "~&~%Selected: ") (parray 'selected nil '((constant wm-state 0) (argmax wm-operator)))
                 (format trace-stream "~&~%Impasse: ") (parray 'impasse nil '((max wm-operator) (argmax wm-type)))
                 ))

  (conditional 'acceptable
               :actions '((selected (state 0) (agent a) (operator 0)))
               )

  (conditional 'reject
               :conditions '((selected (state 0) (agent a) (operator 0))
                             (impasse (state 1) (agent a) (type no-change) (operator 0))
                             )
               :actions '((selected - (state 0) (agent a) (operator 0)))
               )
  t)

; Try a closed-world (persistent) condact
(defun pec nil
  (init)
  (new-type 'abc :constants '(a b c))
  (predicate 'owd :perception t :arguments '((state state) (value abc %)))
  (predicate 'cwd :world 'closed :arguments '((state state) (value abc %)))

  (conditional 'ow-cw
               :conditions '((owd (state 0) (value (x))))
               :condacts '((cwd (state 0) (value (x))))
               )
  (perceive '((owd .1 (state 0) (value a)) (owd .3 (state 0) (value b)) (owd .6 (state 0) (value c))))
  t)

; Test perception
(defun tper nil
  (init)

  (setq perceive-list '((perceive '((open (arg true))
                                    (closed (arg false))))))

  (predicate 'open :world 'open :perception t :arguments '((arg boolean %)))
  (predicate 'closed :world 'closed :perception t :arguments '((arg boolean !)))

  (conditional 'perception
               :conditions '((closed (arg (ca))))
               :condacts '((open (arg (oa))))
               :actions '((closed (arg (ca))))
               )
  t)

; Test condition ordering
(defun tco nil
  (init)

  (new-type 'sym :constants '(a b c))
  (new-type 'disc :numeric t :discrete t :min 0 :max 1000)
  (new-type 'cont :numeric t :min 0 :max 1000)

  (predicate 's :world 'closed :arguments '((s sym)))
  (predicate 'd :world 'closed :arguments '((d disc)))
  (predicate 'c :world 'closed :arguments '((c cont)))
  (predicate 'sd :world 'closed :arguments '((s sym) (d disc)))
  (predicate 'sc :world 'closed :arguments '((s sym) (c cont)))
  (predicate 'dc :world 'closed :arguments '((d disc) (c cont)))
  (predicate 'sdc :world 'closed :arguments '((s sym) (d disc) (c cont)))

  (conditional 'c1
               :conditions '((sdc (s (s)) (d (d)) (c (c)))
                             (sc (s (s)) (c (c)))
                             (dc (d (d)) (c (c)))
                             (s (s (s)))
                             )
               :actions '((d (d (d))))
               )
  
  (evidence '((sdc (s a) (d 10) (c 500))
              (sdc (s b) (d 100) (c 100))
              (sdc (s c) (d 40) (c 10))
              (sdc (s a) (d 30) (c 70))
              (sc (s a) (c 500))
              (sc (s b) (c 100))
              (sc (s a) (c 70))
              (dc (d 100) (c 100))
              (dc (d 10) (c 500))
              (dc (d 30) (c 70))
              (s (s a)) (s (s b)) (s (s c))
              )
            )
  t)

; Test sharing in conditions
(defun tcs nil
  (init)
  (predicate 'x :world 'closed :arguments '((a boolean) (b boolean)))
  (conditional 'c1
               :conditions '((x (a true))
                             (x (a false))
                             (x - (a false))
                             (x (a true) (b false))
                             (x (a (a)))
                             (x (b (b)))
                             (x (a (a)) (b (b)))
                             )
               )
  (conditional 'c2
               :conditions '((x (a true))
                             (x (b false))
                             (x - (a false))
                             (x (a true) (b false))
                             (x (a (a)))
                             (x (a (a)))
                             (x (b (a)))
                             (x (a (a)) (b (b)))
                             )
               )
  t)

; Test multiple unique dimensions 
(defun tmud nil
  (init)
  (learn '(:gd))
  (setq post-d '((pa 'y) (terpri) (pcfs)))
  (predicate 'x :perception t :arguments '((a boolean) (b boolean %) (c boolean %)))
  (predicate 'y :world 'closed :arguments '((a boolean) (b boolean !) (c boolean !)))

  ; Function predicates
  (predicate 'z :arguments '((a boolean) (b boolean %) (c boolean %)))

  (conditional 'c1
               :conditions '((x (a (a)) (b (b)) (c (c))))
               :condacts '((z (a (a)) (b (b)) (c (c))))
               :actions '((y (a (a)) (b (b)) (c (c))))
               )

  ; Function conditionals

  (conditional 'z*function
               :condacts '((z (a (a)) (b (b)) (c (c))))
               :function-variable-names '(a b c)
               :normal '(a b)
               :function 1)

  (perceive '((x .1 (a *) (b *) (c *)) (x .7 (a true) (b true) (c true))))
  (d 1)
  t)

; Variant of tmud created to test setting portions of functions to be learned to 0 initially
(defun tmud2 nil
  (init)
  (learn '(:gd))
  (setq post-d '((pa 'y) (terpri) (pcfs)))
  (predicate 'x :perception t :arguments '((a boolean) (b boolean %) (c boolean %)))
  (predicate 'y :world 'closed :arguments '((a boolean) (b boolean !) (c boolean !)))

  ; Function predicates
  (predicate 'z :arguments '((a boolean) (b boolean %) (c boolean %)))

  (conditional 'c1
               :conditions '((x (a (a)) (b (b)) (c (c))))
               :condacts '((z (a (a)) (b (b)) (c (c))))
               :actions '((y (a (a)) (b (b)) (c (c))))
               )

  ; Function conditionals

  (conditional 'z*function
               :condacts '((z (a (a)) (b (b)) (c (c))))
               :function-variable-names '(a b c)
               :normal '(a b)
               :function '((1 * * *) (0 true true true))
               )

  (perceive '((x .1 (a *) (b *) (c *)) (x .7 (a true) (b true) (c true))))
;  (d 1)
  t)

; Test new variable in actions
(defun nav nil
  (init)
  (new-type 'm :numeric t :min 0 :max 10)
  (new-type 'n :numeric t :discrete t :min 0 :max 10)
  (predicate 'x :arguments '((a boolean)))
  (predicate 'y :perception t :arguments '((b m %)))
  (predicate 'z :perception t :arguments '((c n %)))

  (conditional 'nav
               :conditions '((x (a (a))))
               :actions '((y (b (b :explicit)))
                          (z (c (c :explicit)))
                          )
               :function .2
               )

  (when open-world-wmfns
    (evidence '((y .001) (z .001)))
    )
  t)

; Test negated actions for no-normalize predicates
; Should multiply by -1 and add in FAN
(defun tna nil
  (init)
  (predicate 'p :no-normalize t :arguments '((x boolean %)))

  (conditional 'tnp
               :actions '((p (x true))
                          (p (x false))
                          )
               )

  (conditional 'tnn
               :actions '((p - (x true))
                          )
               :function 2
               )
  (d 0)
  (pwm)
  t)

; Test row-major initialization of discrete functions
(defun integer-list (n)
  (let (l)
    (dotimes (i n)
      (setq l (cons (- n i) l))
      )
    l)
  )
(defun trmi nil
  (init)
  (new-type 'large :numeric t :discrete t :min 0 :max 1000)
  (predicate 'x
             :arguments '((a boolean) (b boolean) (c boolean))
             :function '(row-major 8 7 6 5 4 3 2 1))
  (predicate 'y
             :arguments '((a large) (b large))
             :function (cons 'row-major (integer-list 1000000)))
  (conditional 'z
               :conditions '((x (a (a)) (b (b)) (c (c))))
               :function-variable-names '(a b c)
               :function '(row-major 7 6 5 4 3 2 1 8))
  t
  )

; Test within-condition variable equality tests
(defun twve nil
  (init)
  (predicate 'test :world 'closed :arguments '((a boolean) (b boolean)))
  (conditional 'test-c
               :conditions '((test (a (x)) (b (x))))
               )
  (evidence '((test (a true) (b *)) (test (a false) (b true))))
  t
  )

; Test variable inequality tests
(defun twvne nil
  (init)
  (predicate 'test :world 'closed :arguments '((a boolean) (b boolean)))
  (predicate 'test2 :world 'closed :arguments '((a boolean)))
  (predicate 'test3 :world 'closed :arguments '((a boolean)))
  (predicate 'test4 :world 'closed :arguments '((a boolean)))
  (predicate 'test5 :world 'closed :arguments '((a boolean)))

  ; Test within-pattern not-equal
  (conditional 'within
               :conditions '((test (a (a)) (b (b (<> a)))))
               )

  ; Test across-pattern not-equal
  (conditional 'across-condition
               :conditions '((test2 (a (a)))
                             (test4 (a (y (<> a))))
                             )
               :actions '((test5 (a (y))))
               )
  (conditional 'across-action
               :conditions '((test2 (a (a)))
                             )
               :actions '((test3 (a (y (<> a)))))
               )


  (evidence '((test (a true) (b *)) (test (a false) (b true))
              (test2 (a false))
              (test4 (a *))))
  t
  )

; Test creating action model with universal variables
(defun tamuv nil
  (init)
  (learn '(:am))
  (predicate 'test :world 'closed :arguments '((state state) (universal boolean) (unique boolean !))
             :action-function '((1 true true true) (.5 false false false) (.5 false false true)))
  (ig)
  (pcs)
  )

; Test a contextualized offset
(defun tcoff nil
  (init)
;  (setq trace-affine t)
  (new-type 'integer :numeric t :discrete t :min 0 :max 10)
  (predicate 'context :world 'closed :arguments '((context boolean) (a integer)))
  (predicate 'offset :world 'closed :arguments '((context boolean) (a integer)))
  (predicate 'result :world 'closed :arguments '((context boolean) (a integer)))
  (conditional 'tco-c
               :conditions '((context (context (c)) (a (ci)))
                             (offset (context (c)) (a (oi)))
                             )
               :actions '((result (context (c)) (a (ci (:offset oi :coefficient 1/2 :apply-coefficient-to-offset t))))
                          )
               )
  
  (evidence '((context (context true) (a 2)) (context (context false) (a 5))
              (offset (context true) (a 2)) (offset (context false) (a 3))))
  t)

; Test closed-world perception
(defun tcwp nil
  (init)
  (learn '(:pm :am))
  (predicate 'test :world 'closed :perception t :replace t :arguments '((state state) (a boolean %) (b boolean %)))
  (setq perceive-list '((perceive '((test (b true))))))
  (evidence '((test (b true))))
  t)

; Constant test with integrate-universal-within-unique
(defun tiuwu (iuwu)
  (init)
  (setq integrate-universal-in-unique iuwu)
  (predicate 'test :world 'closed :arguments '((state state) (a boolean) (b boolean !)))
  (conditional 'tiuwu
               :conditions '((test (state 0) (a true) (b (b))))
               :actions '((test (state 0) (a false) (b (b))))
               )
  (evidence '((test (state 0) (a true) (b true)) (test (state 0) (a false) (b false))))
  (d 0)
  (ppwm 'test)
  )

(defun run-distributed-negate-single-sentence ()

(distributed-negate)

(setf trace-gdl t)
;single sentence
(evidence '( 
            (Current 1 (word 0)) (Cooccuring-Words 1 (word 0)) (Cooccuring-Words 1 (word 3)) (Cooccuring-Words 1 (word 9)) (Skip-Gram-Words 1 (word 3) (position 0) ) (Skip-Gram-Words 1 (word 9) (position 1)) 
            )
          t) 
(d 1) 


)

(defun run-distributed-negate-two-sentences ()

(distributed-negate)

(setf trace-gdl t)
;two sentences
(evidence '( 
            (Current 1 (word 0)) (Cooccuring-Words 1 (word 0)) (Cooccuring-Words 1 (word 3)) (Cooccuring-Words 1 (word 9)) (Skip-Gram-Words 1 (word 3) (position 0) ) (Skip-Gram-Words 1 (word 9) (position 1)) 
            )
          t) 
(d 1) 
(evidence '( 
            (Current 1 (word 1)) (Cooccuring-Words 1 (word 1)) (Cooccuring-Words 1 (word 3)) (Cooccuring-Words 1 (word 9)) (Skip-Gram-Words 1 (word 3) (position 0) ) (Skip-Gram-Words 1 (word 9) (position 1)) 
            )
          t) 
(d 1) 

)

(defun run-distributed-negate()

(distributed-negate)

(dotimes (i 10)
;first sentence
(evidence '( 
            (Current 1 (word 2)) (Cooccuring-Words 1 (word 2)) (Cooccuring-Words 1 (word 4)) (Cooccuring-Words 1 (word 7))  (Skip-Gram-Words 1 (word 0) (position 0) ) (Skip-Gram-Words 1 (word 4) (position 1)) 
            )
          t) 
(d 1) 
(evidence '( 
            (Current 1 (word 4))  (Skip-Gram-Words 1 (word 2) (position 0) ) (Skip-Gram-Words 1 (word 7) (position 1)) 
            )
          ) 
(d 1) 
(evidence '( 
            (Current 1 (word 7))  (Skip-Gram-Words 1 (word 4) (position 0) ) (Skip-Gram-Words 1 (word 1) (position 1)) 
            )
          ) 
(d 1) 

;second sentence
(evidence '( 
             (Current 1 (word 3))  (Cooccuring-Words 1 (word 3)) (Cooccuring-Words 1 (word 5)) (Cooccuring-Words 1 (word 9))  (Skip-Gram-Words 1 (word 0) (position 0) ) (Skip-Gram-Words 1 (word 5) (position 1) ) 
            )
          t) 
(d 1) 
(evidence '( 
            (Current 1 (word 5))  (Skip-Gram-Words 1 (word 3) (position 0) ) (Skip-Gram-Words 1 (word 9) (position 1)) 
            )
          ) 
(d 1) 
(evidence '( 
            (Current 1 (word 9))  (Skip-Gram-Words 1 (word 5) (position 0) ) (Skip-Gram-Words 1 (word 1) (position 1)) 
            )
          ) 
(d 1) 

;third sentence
(evidence '( 
              (Current 1 (word 2)) (Cooccuring-Words 1 (word 2)) (Cooccuring-Words 1 (word 5)) (Cooccuring-Words 1 (word 6))   (Skip-Gram-Words 1 (word 0) (position 0) ) (Skip-Gram-Words 1 (word 5) (position 1) ) 
            )
          t) 
(d 1) 
(evidence '( 
            (Current 1 (word 5))  (Skip-Gram-Words 1 (word 2) (position 0) ) (Skip-Gram-Words 1 (word 6) (position 1)) 
            )
          ) 
(d 1) 
(evidence '( 
            (Current 1 (word 6))  (Skip-Gram-Words 1 (word 5) (position 0) ) (Skip-Gram-Words 1 (word 1) (position 1)) 
            )
          ) 
(d 1)


;fourth sentence
(evidence '( 
            (Current 1 (word 3))  (Cooccuring-Words 1 (word 3)) (Cooccuring-Words 1 (word 4)) (Cooccuring-Words 1 (word 8))   (Skip-Gram-Words 1 (word 0) (position 0) ) (Skip-Gram-Words 1 (word 4) (position 1) ) 
            )
          t) 
(d 1) 
(evidence '( 
            (Current 1 (word 4))  (Skip-Gram-Words 1 (word 3) (position 0) ) (Skip-Gram-Words 1 (word 8) (position 1)) 
            )
          ) 
(d 1) 
(evidence '( 
            (Current 1 (word 8))  (Skip-Gram-Words 1 (word 4) (position 0) ) (Skip-Gram-Words 1 (word 1) (position 1)) 
            )
          ) 
(d 1)

)

(evidence '( 
            (Test-Word 1 (word 2))  
            )
          t) 
(d 1)



)
(defun distributed-negate ()
  (let (  (environmental-vectors  '((0.2 0 0) (-0.68 0 1) (-0.44 0 2) (-0.57 0 3) (0.87 0 4) 
                   (-0.29 1 0) (-0.66 1 1) (-0.46 1 2) (0.34 1 3) (-0.67 1 4) 
               (0.25 2 0) (-0.76 2 1) (-0.62 2 2) (-0.7 2 3) (0.07 2 4) 
               (-0.79 3 0) (0.57 3 1) (-0.3 3 2) (-0.64 3 3) (0.76 3 4) 
               (0.28 4 0) (-0.07 4 1) (0.22 4 2) (0.52 4 3) (0.69 4 4) 
               (0.22 5 0) (-0.74 5 1) (-0.08 5 2) (-0.23 5 3) (0.45 5 4) 
               (0.16 6 0) (-0.62 6 1) (0.97 6 2) (-0.10 6 3) (0.05 6 4) 
               (-0.30 7 0) (0.74 7 1) (-0.76 7 2) (0.74 7 3) (-0.41 7 4) 
               (-0.36 8 0) (0.39 8 1) (0.47 8 2) (-0.35 8 3) (-0.20 8 4)
               (0.82 9 0) (-0.72 9 1) (-0.69 9 2) (0.44 9 3) (-0.77 9 4) 
               ))
         (sequence-vectors  '( 
                   (0.39 0 0) (-0.4 0 1) (0.07 0 2) (-0.42 0 3) (-0.03 0 4) 
                   (0.35 1 0) (0.78 1 1) (0.97 1 2) (0.85 1 3) (0.52 1 4) 
                   ))        
         
  )
         
  (init)
  (setf fan-constant 0)
  ;(setf debug-init-descendants t)
  (learn '(:gd))
  (setf adaptive-smoothing nil)
  (setf learning-rate 1)
  (setf smoothing-parameter 0.000000000000001)
  (setf gradient-subtract-average nil)
  ;(setf trace-gdl t)
  ;(setf trace-transform t)
  ;(setq do-not-learn '(other-environment before-bigram-environment-to-lexical before-bigram-environment-to-lexical predicate))
  
  (new-type 'local :numeric t :discrete t :min 0 :max 10) 
  (new-type 'environment :numeric t :discrete t :min 0 :max 5) 
  (new-type 'position :numeric t :discrete t :min 0 :max 2)
  
  (predicate 'Cooccuring-Words :no-normalize t :world 'closed :arguments '((word local %)))
  (predicate 'Skip-Gram-Words :world 'closed :arguments '((word local %) (position position)))
  (predicate 'Current :world 'closed :arguments '((word local %)))
  (predicate 'Context-Vector  :arguments '((distributed environment []))) ;should normalize this ; need to have vector here
  (predicate 'Skip-Gram-Matrix :no-normalize t :arguments '((position position %) (distributed environment %) ) ) ;if I don't keep position as a distribution variable, ordering vector is calculated by max rather than sum. Hence,this is not a vector
  (predicate 'Ordering-Vector  :arguments '((distributed environment []))) ;should normalize this ; need to have vector here
  (predicate 'Meaning-Vector  :no-normalize t :arguments '((word local) (distributed environment [])) :function 1 ) ;need to have vector over here ; why making this vector changes the fan node from *sum* to *por* -> node 62

  (predicate 'Current-Environmental-Vector :no-normalize t :arguments '((distributed environment []))) 
  (predicate 'Sentence-Vector :no-normalize t :arguments '((distributed environment []))) 
  ;(predicate 'Environmental-Vectors :no-normalize t :learning-rate 0 :arguments '((word local) (distributed environment)) :function environmental-vectors) 
  ;(predicate 'Sequence-Vectors :no-normalize t :learning-rate 0 :arguments '((position position ) (distributed environment)) :function sequence-vectors) 
  (predicate 'Environmental-Vectors :learning-rate 0 :arguments '((word local) (distributed environment [])) :function environmental-vectors) 
  (predicate 'Sequence-Vectors :learning-rate 0 :arguments '((position position ) (distributed environment [])) :function sequence-vectors) 

  ;these predicates are needed for cosine calculations
  (predicate 'Lexical-Vectors :learning-rate 0 :arguments '( (word local) (distributed environment [])) :function 'meaning-vector )
  (predicate 'Test-Word :world 'closed :arguments '((word local %)))
  (predicate 'l-current :no-normalize t :arguments '((word local %)))

   

   ;if the extraction is done via a constant in the conditional argument for lexical vectors, max operation is applied in FIF nodes and negative values become 0.
   ;Using the predicate test-word with parameterized lexical-vectors arguments (two lexical-arguments: one just for the test word and second for all the lexical vectors
   (conditional 'Cosine-Values
                :conditions '(
                              (Test-Word (word (w)))
                              (Lexical-Vectors (word (w)) (distributed (d)))
                              ;(Lexical-Vectors (word 2) (distributed (d)))
                              (Lexical-Vectors (word (x)) (distributed (d)))
                              )
                :condacts '((l-current (word (x)))
                           )

               )

 
   (conditional 'current-environmental-vector
             
               :conditions '((Current (word (w)))
                             (Environmental-Vectors (word (w)) (distributed (d)))
                             )
               :actions '((Current-Environmental-Vector (distributed (d))))          
               )
                       
  (conditional 'sentence-co-occurence-vector
               :conditions '((Cooccuring-Words (word (w)))
                             (Environmental-Vectors (word (w)) (distributed (d)))
                             )
               :actions '((Sentence-Vector (distributed(d))))            
               )

  (conditional 'negated-co-occurence-for-the-current-word
               :conditions '(
                             (Current-Environmental-Vector (distributed (d)))
                             )
               :actions '((Context-Vector - (distributed(d))))
               )
   
  (conditional 'co-occurence
               :conditions '(
                             (Sentence-Vector (distributed (d)))
                             )
               :actions '((Context-Vector  (distributed(d))))       
               )
   
  (conditional 'skip-gram
               :conditions '((Skip-Gram-Words (word (w)) (position (p)))
                             (Environmental-Vectors (word (w)) (distributed (d)))
                             )
               :actions '((Skip-Gram-Matrix (distributed(d)) (position (p))))                      
               )

  (conditional 'ordering
               :conditions '((Skip-Gram-Matrix (distributed (d)) (position (p)))
                             (Sequence-Vectors (position (p)) (distributed (d)))
                             )
               :actions '((Ordering-Vector (distributed(d))))             
               )

  (conditional 'context
               :conditions '(
                             (Context-Vector (distributed (d)))
                             (Current (word (w)))                           
                             )
               :actions '((Meaning-Vector (word (w)) (distributed (d))))  
               )
  (conditional 'word-order
               :conditions '(
                             (Ordering-Vector (distributed (d)))
                             (Current (word (w)))                           
                             )
               :actions '((Meaning-Vector (word (w)) (distributed (d))))  
               )
  t)  
)

(defun random-walk0()
  (init '(left right none))
  (conditional 'acceptable
               :actions '((selected (operator left))
                          (selected (operator right))
                          (selected (operator none))
                          )
               )
  (d 1)
)

(defun random-walk1()
  (init '(left right none))
  (setq post-d '((ppfn 'selected)))
  (conditional 'acceptable
               :actions '((selected (operator *))                          
                          )
               )
  (d 1)
)

(defun random-walk2()
  (init '(left right none))
  (operator-selection 'boltzmann)
  (setq post-d '((ppfn 'selected)))
  (conditional 'acceptable
               :actions '((selected (operator *))                          
                          )
               )
  (d 1)
)

(defun random-walk3()
  (init '(left right none))
  (operator-selection 'boltzmann)
  (new-type '1D-grid :numeric t :discrete t :min 1 :max 8)
  (predicate 'location :world 'closed :arguments '((x 1D-grid !)))

  (setq post-d '(
                   (ppfn 'selected)
                   (ppfn 'location)
                   ))

  (conditional 'move-left
               :conditions '(
                             (selected (operator left))
                             (location (x (value)))
                             )
               :actions '(
                          (location (x (value -1)))
                          )             
               )

  (conditional 'move-right
               :conditions '(
                             (selected  (operator right))
                             (location (x (value)))
                             )
               :actions '(
                          (location (x (value 1)))
                          )             
               )

  (conditional 'acceptable
               :actions '((selected (operator *))                         
                          )
               )

  (evidence '((location (x 4)) ))
  (d 5)
)

;-----------
; Test program for parallel visual search for yellow via top-down attentional input (via a goal for an open-world predicate)
; It also checks for visual surprise and for what happens to surprise when a blank screen intervenes
(defun visual-search (&optional blackout no-attention)
  (init)
  (learn '(:gd))
    (unless no-attention
      (setq compute-surprise t)
      (setq compute-progress t)
      (setq compute-attention t)
      (setq trace-attention t)
      )
;  (setq trace-gdl t)
  (setq perceive-list '((perceive '((image (x 0) (y 1) (color yellow))
                                    (image (x 1) (y 0) (color yellow))
                                    (image (x 2) (color blue)) (image (x 3) (color red))
                                    (image (y 2) (color red)) (image (y 3) (color blue))
                                    (image (x 0) (y 0) (color green))
                                    (image (x 1) (y 1) (color green))))))
  (new-type 'color :constants '(red yellow green blue black))
  (new-type 'dimension :numeric t :discrete t :min 0 :max 4)
  (predicate 'image :perception t :arguments '((x dimension) (y dimension) (color color %)) :function 1
             :goal '((image*goal (color yellow))))
  (predicate 'result :world 'closed :arguments '((x dimension) (y dimension) (color color !)))

  (conditional 'compare
               :conditions '((image (x (x)) (y (y)) (color (color)))
                             (image*goal (x (x)) (y (y)) (color (color)))
                             )
               :actions '((result (x (x)) (y (y)) (color (color))))
               )
  (d 2)
  (pimage)
  (pgoal)
  (d 20)
  (when blackout
    (format T "~&~%*** Changing perception of entire image to black.")
    (setq perceive-list '((perceive '((image (color black))))))
    (d 3)
    (format T "~&~%*** Results after changing perception to black (and 3 decisions)")
    (pimage)
    (pgoal)
    (pppfns)
    )
  (format T "~&~%*** Changing perception of location 0,3 to green.")
  (setq perceive-list '((perceive '((image (x 0) (y 1) (color yellow))
                                    (image (x 1) (y 0) (color yellow))
                                    (image (x 2) (color blue)) (image (x 3) (color red))
                                    (image (y 2) (color red)) (image (y 3) (color blue))
                                    (image (x 0) (y 0) (color green))
                                    (image (x 1) (y 1) (color green))
                                    (image (x 0) (y 3) (color green))))))
  (d 3)
  (format T "~&~%*** Results after change in perception (and 3 decisions)")
  (pimage)
  (pgoal)
  (pppfns)
  )
