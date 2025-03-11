; 1D Grid for RL
; Supports RL, action learning, perceptual learning and episodic learning

; True location in world
(defvar vr-location)
; Action executed in world
(defvar vr-action)
; True map of world
(defvar vr-map (vector 'wall 'none 'door1 'none 'none 'door2 'none 'wall))

; Perceive results of operator with some noise
; Correct-prob is probability that peak is at correct location
; Correct-mass is how much of the probability mass is at the correct location
(defun perceive-vr-object (&optional correct-prob)
  (unless correct-prob (setq correct-prob 1.0))
  (let ((rand (random 1.0))
        location ; Perceived location
        )
    (setq location vr-location)
    ; Perceive new location with correct-prob of getting right (and otherwise on one side)
    (cond ((= vr-location 0)
           (when (>= rand correct-prob) (setq location 1))
           )
          ((= vr-location 7)
           (when (>= rand correct-prob) (setq location 6))
           )
          (t
           (when (>= rand correct-prob)
             (if (< (random 1.0) .5)
                 (setq location (1- vr-location))
               (setq location (1+ vr-location)))
             )
           )
          )
    ; Generate object perception
    (perceive `((observed 1 (state 0) (object ,(aref vr-map location)))))
    (format trace-stream "~&~%Perceived object: ~S" (aref vr-map location))
    )
  )

; Perceive current location
(defun perceive-vr-location nil
  (perceive `((location (state 0) (x ,vr-location))))
  )

; Perceive current action
(defun perceive-vr-action nil
  (perceive `((action (state 0) (action ,vr-action))))
  )

; Execute operator with some noise
(defun execute-vr-operator (&optional correct-prob)
  (unless correct-prob (setq correct-prob 1))
  (let ((operator (operator-in-state base-level-state))
        (rand (random 1.0))
        )
    (when operator
      (setq vr-action operator)
      ; Make correct move action-noise percent of time
      (case operator
        (left (when (< rand correct-prob) (setq vr-location (max (- vr-location 1) 0))))
        (right (when (< rand correct-prob) (setq vr-location (min (+ vr-location 1) 7))))
        )
      )
    )
  )

(defun test-vr-print (stream)
  (format stream "~&~%PROJECTED FF (EV):~&")
  (pa 'projected nil '((expected wm-value)) stream)
  (format stream "~&~%Q FF (EV):~&")
  (pa 'q nil '((expected wm-value)) stream)
  (format stream "~&~%REWARD FF (EV):~&")
  (pa 'reward nil '((expected wm-value)) stream)
  (format stream "~&~%")
  )

; Fixed vector of rewards for use in vr-assign-reward
(defparameter rewards (vector 0 0 0 0 9 0 0 0))

; Assign a fixed reward based on location
(defun vr-assign-reward (rewards)
  (eval `(perceive (quote ((reward .1 (location-x *) (value *)) ; Empty WM of any previous rewards
                           (reward (location-x ,vr-location) (value ,(aref rewards vr-location))))))) ; Add reward for current state
  )

; Initialize everything for virtual robot
(defun init-vr-grid (&optional start load-evidence learn perception-prob action-prob)
  (unless perception-prob (setq perception-prob 1.0))
  (unless action-prob (setq action-prob 1.0))
  (init '(left stay right) nil t) ; center-discrete-numeric-on-integer
  (operator-selection 'boltzmann)
  (learn learn)

  (setq pre-run '((format trace-stream "~&~%True location: ~S" vr-location)
                  ))
  (setq perceive-list `((perceive-vr-object ,perception-prob) (perceive-vr-action)))
  ; Need to perceive rewards when doing RL
  (when automatic-reinforcement-learning
    (setq perceive-list (cons '(vr-assign-reward rewards) perceive-list))
    )
  ; When not doing perception modeling, need to directly perceive where you are
  ; The perception model only gives you an approximation because the same object is in multiple locations
  (unless automatic-perception-models
    (setq perceive-list (cons '(perceive-vr-location) perceive-list))
    )
  (setq action-list `((execute-vr-operator ,action-prob)))
  
  (setq pre-d '(
;                (break)
;                (WHEN (AND (= TRIAL-COUNT 3) (= VR-LOCATION 4) (EQ (operator-IN-STATE 0) (QUOTE RIGHT))) (break))
                (format trace-stream "~&~%Distribution over next location: ~&") (ppvn 'location*next)
                ))

  (setq post-d '((when automatic-perception-models
                   (format trace-stream "~&~%Distribution over current location: ")
                   (parray 'location nil '((constant wm-state 0)))
                   (format trace-stream "~&~%Learned map function: ")
                   (parray (predicate-name (graph-perception-predicate cg)))
                   (parray (predicate-name (graph-perception-predicate cg)) nil '((argmax wm-object-1)))
                   )
                 (format trace-stream "~&~%Action selected: ")
                 (parray 'selected nil '((constant wm-state 0) (argmax wm-operator)))
                 (format trace-stream "~&~%")
                 ; Stop one decision after goal
                 (when (and automatic-reinforcement-learning vr-halt) (halt))
                 (when (and automatic-reinforcement-learning (= vr-location 4)) (setq vr-halt t))
                 ))

  (setq post-t '((when automatic-reinforcement-learning
                   (test-vr-print trace-stream)
                   (when (and diachronic-prediction
                              (member 'transition (graph-conditionals cg))
                              )
                     )
                   (format trace-stream "~&~%")
                   )
                 )
        )

  (new-type 'location :numeric t :discrete t :min 0 :max 8)
  (new-type 'obj :constants '(wall door1 door2 none))

  (predicate 'location :world 'closed :replace t :perception t :arguments '((state state) (x location %)))
  (predicate 'action :world 'closed :perception t :arguments '((state state) (action operator)))
  (predicate 'observed :perception t :arguments '((state state) (object obj %)))

  ; Include an explicit action model if not creating them automatically
  (unless automatic-action-models
    (predicate 'transition
               :arguments '((loc location) (operator operator) (locn location %))
               :function `((1 7 RIGHT 7) (1 0 LEFT 0) (1 7 STAY 7) (1 6 STAY 6) (1 5 STAY 5) (1 4 STAY 4) (1 3 STAY 3) (1 2 STAY 2) (1 1 STAY 1) (1 0 STAY 0) 
                           (,action-prob 7 LEFT 6) (,action-prob 6 LEFT 5) (,action-prob 5 LEFT 4) (,action-prob 4 LEFT 3) (,action-prob 3 LEFT 2)
                           (,action-prob 2 LEFT 1) (,action-prob 1 LEFT 0)
                           (,action-prob 6 RIGHT 7) (,action-prob 5 RIGHT 6) (,action-prob 4 RIGHT 5) (,action-prob 3 RIGHT 4) (,action-prob 2 RIGHT 3) (,action-prob 1 RIGHT 2)
                           (,action-prob 0 RIGHT 1)
                           (,(- 1 action-prob) 0 RIGHT 0) (,(- 1 action-prob) 1 RIGHT 1) (,(- 1 action-prob) 2 RIGHT 2) (,(- 1 action-prob) 3 RIGHT 3)
                           (,(- 1 action-prob) 4 RIGHT 4) (,(- 1 action-prob) 5 RIGHT 5) (,(- 1 action-prob) 6 RIGHT 6)
                           (,(- 1 action-prob) 1 LEFT 1) (,(- 1 action-prob) 2 LEFT 2) (,(- 1 action-prob) 3 LEFT 3) (,(- 1 action-prob) 4 LEFT 4)
                           (,(- 1 action-prob) 5 LEFT 5) (,(- 1 action-prob) 5 LEFT 5) (,(- 1 action-prob) 6 LEFT 6))
               )

    (setq post-automatic
          (cons '(conditional 'transition-c ; Action/transmission model
                              :conditions '((state (state (s)))
                                            (location (state (s)) (x (loc)))
                                            (selected (state (s)) (operator (o)))
                                            )
                              :condacts '((location*next (state (s)) (x (locn)))
                                          (transition (loc (loc)) (operator (o)) (locn (locn))))
                              )
                post-automatic))
    )

  ; Include an explicit perception model if not creating them automatically
  (unless automatic-perception-models
    (predicate 'perception
               :arguments '((loc location) (object obj %))
               :function `((,perception-prob 0 wall) (,perception-prob 1 none) (,perception-prob 2 door1) (,(+ perception-prob (/ (- 1 perception-prob) 2)) 3 none)
                           (,(+ perception-prob (/ (- 1 perception-prob) 2)) 4 none) (,perception-prob 5 door2) (,perception-prob 6 none) (,perception-prob 7 wall)
                           (,(- 1 perception-prob) 0 none) (,(/ (- 1 perception-prob) 2) 1 wall) (,(/ (- 1 perception-prob) 2) 1 door1)
                           (,(- 1 perception-prob) 2 none) (,(/ (- 1 perception-prob) 2) 3 door1) (,(/ (- 1 perception-prob) 2) 4 door2)
                           (,(- 1 perception-prob) 5 none) (,(/ (- 1 perception-prob) 2) 6 door2) (,(/ (- 1 perception-prob) 2) 6 wall)
                           (,(- 1 perception-prob) 7 none))
               )

    (setq post-automatic
          (cons '(conditional 'perception-c
                              :conditions '((state (state (s)))
                                            (location (state (s)) (x (loc)))
                                            (selected (state (s)) (operator (o)))
                                            )
                              :condacts '((observed (state (s)) (object (obj)))
                                          (location*next (state (s)) (x (locn)))
                                          (perception (object (obj)) (loc (locn))))
                              )
                post-automatic))
    )

  (conditional 'acceptable
               :actions '((selected (state 0) (operator left))
                          (selected (state 0) (operator right))
                          (selected (state 0) (operator stay))
                          )
               )

  (when load-evidence (rle start nil))
  nil)

; Brief init
(defun brief-init-vr-grid ()
  (setq center-discrete-numeric-on-integer t)
  )
; Load evidence
(defun rle (&optional start continue)
  (unless start (setq start 1))
  (setq vr-location start)
  (setq vr-action 'stay)
  (evidence `((location 1 (state 0) (x ,vr-location)) ; No misperception of initial location
              (selected (state 0) (operator stay))
              ) continue)
  )

; Used to delay for one decision halting after reach goal
(defvar vr-halt)

; Continue with new evidence (use this function to explore more evidence, with initial nil)
; Start is the location at which to start (or a list with a sequence of start locations)
; Repeat is the number of times to repeat (list of) start location(s)
; Initial says that this is the first run with these conditionals
(defun rlr (&optional start repeat initial)
  (unless repeat (setq repeat 1))
  (when (numberp start) (setq start (list start)))
  (if automatic-reinforcement-learning
      (catch 'interrupt-rl
        (dotimes (i repeat)
          (dolist (s start)
            (rle s (not initial))
            (format trace-stream "~&~&Start location: ~S" s)
            (setq vr-halt nil)
            (when (eq (trials 1 (not initial)) interrupt-symbol)
              (throw 'interrupt-rl interrupt-symbol)
              )
            (setq initial nil)
            )
          )
        )
    (progn ; Just doing random walk for "repeat" decisions
      (rle (car start) (not initial))
      (format trace-stream "~&~&Start location: ~S" (car start))
      (d repeat)
      ))
  )

; Top level function to call for reinforcement learning
; If initial is true, initialize graph
; Start is the starting location in the grid
; Repeat is the number of trials to run
; Model-free determines whether include conditionals for action models
(defun vr-grid (&optional initial start repeat learn perception-prob action-prob)
  (if initial
      (init-vr-grid start (= repeat 0) learn perception-prob action-prob)
    (brief-init-vr-grid)
    )
  (if (= repeat 0)
      (d 0)
    (rlr start repeat initial))
  )

; SLAM
(defun vr-slam (&optional decisions perception-prob action-prob)
  (vr-grid t 4 (if decisions decisions 400) '(:pm) perception-prob action-prob)
  )

; RL
(defun vr-rl (&optional trials perception-prob action-prob)
  (vr-grid t '(0 7) (if trials trials 60) '(:rl) perception-prob action-prob)
  )

; Learn action models
(defun vr-am (&optional decisions perception-prob action-prob)
  (vr-grid t 4 (if decisions decisions 400) '(:am) perception-prob action-prob)
  (format trace-stream "~&~%TRANSITION FF:~&")
  (pa (predicate-name (graph-action-predicate cg))) ; This way of finding the transition function is fragile
  )

; Combine RL and action learning
(defun vr-rl-am (&optional trials perception-prob action-prob)
  (vr-grid t '(0 7) (if trials trials 60) '(:rl :am) perception-prob action-prob)
  (format trace-stream "~&~%TRANSITION FF:~&")
  (pa (predicate-name (graph-action-predicate cg))) ; This way of finding the transition function is fragile
  )

; Combine action learning and SLAM
; Doesn't learn anything because has no constraint
(defun vr-am-slam (&optional decisions perception-prob action-prob)
  (vr-grid t 4 (if decisions decisions 400) '(:am :pm) perception-prob action-prob)
  (format trace-stream "~&~%TRANSITION FF:~&")
  (pa (predicate-name (graph-action-predicate cg))) ; This way of finding the transition function is fragile
  )

; Combine RL and SLAM
(defun vr-rl-slam (&optional trials perception-prob action-prob)
  (vr-grid t '(0 7) (if trials trials 60) '(:rl :pm) perception-prob action-prob)
  )
