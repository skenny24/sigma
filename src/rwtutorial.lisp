(defun random-walk-1()
  (init '(left right none))
  (conditional 'acceptable
               :actions '((selected (operator left))
                          (selected (operator right))
                          (selected (operator none))
                          )
               )
  (d 1)
)

(defun random-walk-2()
  (init '(left right none))
  (operator-selection 'boltzmann)
  (setq post-d '((pwmb 'selected 'array)))
  (conditional 'acceptable
               :actions '((selected (operator *))                          
                          )
               )
  (d 1)
)

(defun random-walk-3()
  (init '(left right none))
  (operator-selection 'boltzmann)
  (new-type '1D-grid :numeric t :discrete t :min 1 :max 8)
  (predicate 'location :world 'closed :arguments '((x 1D-grid !)))

  (setq post-d '(
                   (pwmb '(selected location) 'array)
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

(defun random-walk-4()
  (init '(left right none))
  (operator-selection 'boltzmann)
  (setf max-fraction-pa-regions 0) ;setting it 0 changes the print functions from a region based representation to a more explicit representation.
  (new-type '1D-grid :numeric t :discrete t :min 1 :max 8)
  (predicate 'location :world 'closed :arguments '((x 1D-grid !))) 
  (setq pre-t '((evidence '((location (x 4)) ))))
  (setq post-d '(
                 (pwmb '(selected location) 'array)
                   ))

  (conditional 'move-left
               :conditions '(
                             (selected (state 0) (operator left))
                             (location (x (value)))
                             )
               :actions '(
                          (location (x (value -1)))
                          )             
               )

 (conditional 'move-right
               :conditions '(
                             (selected (state 0) (operator right))
                             (location (x (value)))
                             )
               :actions '(
                          (location (x (value 1)))
                          )             
               )

 (conditional 'halt-at-location-1
               :conditions '(                          
                             (location (x 1))
                             )
               :actions '(
                          (halt)
                          )             
               )

 (conditional 'halt-at-location-7
               :conditions '(                          
                             (location (x 7))
                             )
               :actions '(
                          (halt)
                          )             
               )

  (conditional 'acceptable
               :actions '((selected (operator *))                         
                          )
               )

  (trials 1)
)

; True location in world
(defvar 1d-grid-location)

; Perceive results of operator with some noise
; Correct-prob is probability that peak is at correct location
; Correct-mass is how much of the probability mass is at the perceived location
(defun perceive-location (&optional correct-prob correct-mass)
  (unless correct-prob (setq correct-prob 1.0))
  (unless correct-mass (setq correct-mass 1.0))
  (let ((rand (random 1.0))
        location ; Perceived location
        1-cm
        )
    (setq 1-cm (- 1 correct-mass))
    (setq location 1d-grid-location)
    ; Perceive new location with correct-prob of getting right (and otherwise on one side)
    (cond ((= 1d-grid-location 1)
           (when (>= rand correct-prob) (setq location 2))
           )
          ((= 1d-grid-location 7)
           (when (>= rand correct-prob) (setq location 6))
           )
          (t
           (when (>= rand correct-prob)
             (if (< (random 1.0) .5)
                 (setq location (1- 1d-grid-location))
               (setq location (1+ 1d-grid-location)))
             )
           )
          )
    ; Zero out all perception for predicate location
    (perceive '((location 0)))
    ; Generate noisy perceptions based on correct-mass
    (perceive `((location ,correct-mass (x ,location)))) ; Correct-mass at location
    ; Divide incorrect mass among adjacent locations when they exist
    (cond ((= location 1)
           (perceive `((location ,1-cm  (x 2))))
           )
          ((= location 7)
           (perceive `((location ,1-cm (x 6))))
           )
          (t
           (perceive `((location ,(/ 1-cm 2)  (x ,(1- location)))
                       (location ,(/ 1-cm 2)  (x ,(1+ location)))
                       )
                     )
           )
          )
      )
  )

; Execute operator with some noise
(defun execute-operator (&optional correct-prob)
  (unless correct-prob (setq correct-prob 1.0))
  (let ((operator (operator-in-state base-level-state))
        (rand (random 1.0))
        )
    (when (and operator (not (haltp)))
       (format trace-stream "~&~&Current location: ~S~%" 1d-grid-location)
      ; Make correct move action-noise percent of time
      (case operator
        (left (when (< rand correct-prob) (setq 1d-grid-location (max (- 1d-grid-location 1) 1))))
        (right (when (< rand correct-prob) (setq 1d-grid-location (min (+ 1d-grid-location 1) 7))))
        )
      (format trace-stream "~&~&Operator:~S Next location: ~S~%" operator 1d-grid-location)
      )    
    )
  )

(defun random-walk-5 (&optional perception-prob perception-mass action-prob)
  (init '(left right none))
  (operator-selection 'boltzmann)
  (setf max-fraction-pa-regions 0)
  (new-type '1D-grid :numeric t :discrete t :min 1 :max 8)
  (predicate 'location :perception t :arguments '((x 1D-grid %))) 

  (setq perceive-list `((perceive-location ,perception-prob ,perception-mass)))
  (setq action-list `((execute-operator ,action-prob)))

  (setq pre-t '((setf 1d-grid-location 4) ))
  (setq post-d '(
                 (pwmb '(location selected) 'array)                
                   ))

  (conditional 'halt-1
               :conditions '(                          
                             (location (x 1))
                             )
               :actions '(
                          (halt)
                          )             
               )

  (conditional 'halt-7
               :conditions '(                          
                             (location (x 7))
                             )
               :actions '(
                          (halt)
                          )             
               )

  (conditional 'acceptable
               :actions '((selected (operator *))                         
                          )
               )

  (trials 1)
)

(defun random-walk-6 (&optional perception-prob perception-mass action-prob)
  (init '(left right none))
  (operator-selection 'boltzmann)
  (setf max-fraction-pa-regions 0)
  (new-type '1D-grid :numeric t :discrete t :min 1 :max 8)
  (predicate 'location :perception t :arguments '((x 1D-grid %)))
  (predicate 'location-selected :world 'closed :arguments '((x 1D-grid !)))

  (setq perceive-list `((perceive-location ,perception-prob ,perception-mass)))
  (setq action-list `((execute-operator ,action-prob)))

  (setq pre-t '((setf 1d-grid-location 4) ))
  (setq post-d '(
                 (pwmb '(location location-selected selected) 'array)                         
                   ))

  (conditional 'select-location
               :conditions '((location (x (location))))
               :actions '((location-selected (x (location))))
               )
   (conditional 'halt-1
               :conditions '(                          
                             (location-selected (x 1))
                             )
               :actions '(
                          (halt)
                          )             
               )

  (conditional 'halt-7
               :conditions '(                          
                             (location-selected (x 7))
                             )
               :actions '(
                          (halt)
                          )             
               )

  (conditional 'acceptable
               :actions '((selected (operator *))                         
                          )
               )

  (trials 1)
)

(defun perceive-object ()
  (case 1d-grid-location
    (1 (perceive '((object (object dog)))))
    (2 (perceive '((object (object human)))))
    (3 (perceive '((object (object walker)))))
    (4 (perceive '((object (object table)))))
    (5 (perceive '((object (object dog)))))
    (6 (perceive '((object (object table)))))
    (7 (perceive '((object (object walker)))))                                                
    )      
)

(defun random-walk-7(&optional perception-prob perception-mass action-prob)
  (init '(left right none))
  (operator-selection 'boltzmann)
  (setf max-fraction-pa-regions 0)
  (new-type '1D-grid :numeric t :discrete t :min 1 :max 8)
  (new-type 'obj-type :constants '(walker table dog human))

  (predicate 'location :perception t :arguments '((x 1D-grid %)))
  (predicate 'location-selected :world 'closed :arguments '((x 1D-grid !)))
  (predicate 'object :perception t :arguments '((object obj-type %)))
  (predicate 'object-perceived :world 'closed :arguments '( (location 1D-grid) (object obj-type !)))


  (setq pre-t '((setf 1d-grid-location 4) )) 
  (setq pre-run '((when (or (= 1d-grid-location 1) (= 1d-grid-location 7)) (halt))))
  (setq post-d '(
                 (pwmb '(location object location-selected object-perceived selected) 'array)                       
                   ))


  (setq perceive-list `((perceive-location ,perception-prob ,perception-mass)
                        (perceive-object)                    
                        ))

  (setq action-list `((execute-operator ,action-prob)))

  (conditional 'perceived-objects
               :conditions '(
                             (object (object (obj)))
                             (location (x (loc)))
                             )
               :actions '((object-perceived (object (obj)) (location (loc))))
               )

 (conditional 'select-location
               :conditions '((location (x (location))))
               :actions '((location-selected (x (location))))
               )

 (conditional 'acceptable
              :actions '((selected (operator *))                         
                          )
               )
 (setf 1d-grid-location 4)
  (trials 1)
)

(defun random-walk-8 (number-of-decisions &optional perception-prob perception-mass action-prob)
  (init '(left right none))
  (learn '(:gd))
  (setf learning-rate 0.01)
  (setf max-gdl-increment 0.2)
  (operator-selection 'boltzmann)
  (setf max-fraction-pa-regions 0)
  (setf MAX-DECISIONS number-of-decisions)
  (new-type '1D-grid :numeric t :discrete t :min 1 :max 8)
  (new-type 'obj-type :constants '(walker table dog human))

  (predicate 'location :perception t :arguments '((x 1D-grid %)))
  (predicate 'location-selected :world 'closed :arguments '((x 1D-grid !)))
  (predicate 'object :perception t :arguments '((object obj-type %)))
  (predicate 'map :arguments '( (location 1D-grid) (object obj-type %)) :function 1)


  (setq pre-t '((setf 1d-grid-location 4) )) 
  (setq post-run `( (when (= decision-count ,number-of-decisions) (halt))))

  (setq post-d '(
                 (format trace-stream "~&~&Current location: ~S~%" 1d-grid-location)
                 (pwmb '(location object location-selected selected) 'array)     
                 (format trace-stream "~&~&Current Map ~%")
                 (ppf 'map 'array)       
                 ))


  (setq perceive-list `((perceive-location ,perception-prob ,perception-mass)
                        (perceive-object)              
                        ))

  (setq action-list `((execute-operator ,action-prob)))


  (conditional 'perceived-objects
               :conditions '(
                             (object (object (obj)))
                             (location (x (loc)))
                             )
               :condacts '(                           
                           (map (object (obj)) (location (loc)))
                           )
               )

  (conditional 'select-location
               :conditions '((location (x (location))))
               :actions '((location-selected (x (location))))
               )

  (conditional 'acceptable
               :actions '((selected (operator *))                         
                          )
               )
  (trials 1)

)

(defun random-walk-9 (number-of-decisions &optional perception-prob perception-mass action-prob)
  (init '(left right none))
  (learn '(:gd))
  (setf learning-rate 0.01)
  (setf max-gdl-increment 0.2)
  (operator-selection 'boltzmann)
  (setf max-fraction-pa-regions 0)
  (setf MAX-DECISIONS number-of-decisions)
  (new-type '1D-grid :numeric t :discrete t :min 1 :max 8)
  (new-type 'obj-type :constants '(walker table dog human))

  (predicate 'location :perception t :arguments '((x 1D-grid %)))
  (predicate 'location-selected :world 'closed :arguments '((x 1D-grid !)))
  (predicate 'object :perception t :arguments '((object obj-type %)))
  (predicate 'map :arguments '( (location 1D-grid) (object obj-type %)) :function 1)


  (setq pre-t '((setf 1d-grid-location 4) )) 
  (setq post-run `( (when (= decision-count ,number-of-decisions) (halt))))

  (setq post-d '(
                 (format trace-stream "~&~&Current location: ~S~%" 1d-grid-location)
                 (pwmb '(location object location-selected selected) 'array)               
                 (ppf 'map 'array)                       
                 ))


  (setq perceive-list `((perceive-location ,perception-prob ,perception-mass)
                        (perceive-object)                      
                        ))

  (setq action-list `((execute-operator ,action-prob)))

  (conditional 'perceived-objects
               :conditions '(
                             (object (object (obj)))                           
                             )
               :condacts '(   
                           (location (x (loc)))
                           (map (object (obj)) (location (loc)))
                           )
               )

  (conditional 'select-location
               :conditions '((location (x (location))))
               :actions '((location-selected (x (location))))
               )

  (conditional 'acceptable
               :actions '((selected (operator *))                         
                          )
               )

  (trials 1)
)

(defun perceive-object-features (perceive-object)
  (let ( 
        (rand (random 1.0))
        (rand2 (random 1.0))
        (rand3 (random 4))
        object
        )
    (case rand3 
      (0 (setf object 'dog))
      (1 (setf object 'human))
      (2 (setf object 'table))
      (3 (setf object 'walker))
      )
    (if (not perceive-object)  (format trace-stream "~&~&Correct object: ~S~%" object )) 
    (if perceive-object 
        (perceive `((object 1 (object ,object)))) 
       (perceive '((object 1 (object  *))))
      )

    (cond

     ((eq object 'dog)
      (perceive '((alive 1  (value true))))
      (cond
       ( (< rand 0.7) (perceive '((color 1  (value brown))))) 
       ( (< rand 0.95) (perceive '((color 1 (value white))))) 
       ( (< rand 1) (perceive '((color 1  (value silver)))))             
       )
      (cond
       ((< rand2 0.8) (perceive '((legs 1  (value 4))))) 
       ((< rand2 1) (perceive '((legs 1 (value 3)))))          
       )           
      )

     ((eq object 'walker)
      (perceive '((alive 1  (value false))))
      (cond
       ( (< rand 0.95) (perceive '((color 1  (value silver))))) 
       ( (< rand 1) (perceive '((color 1  (value brown)))))             
       )
      (cond
       ( (< rand2 1) (perceive '((legs 1  (value 1)))))           
       )
      )

     ((eq object 'human)
      (perceive '((alive 1  (value true))))
      (cond
       ((< rand 0.5) (perceive '((color 1  (value brown))))) 
       ((< rand 1) (perceive '((color 1 (value white)))))             
       )
      (cond
       ((< rand2 0.97) (perceive '((legs 1  (value 2))))) 
       ((< rand2 0.99)  (perceive '((legs 1  (value 1)))))   
       ((< rand2 1.0)  (perceive '((legs 1  (value 0)))))
       )
      )

     ((eq object 'table)
      (perceive '((alive 1  (value false))))
      (cond
       ((< rand 0.4) (perceive '((color 1  (value brown))))) 
       ((< rand 1) (perceive '((color 1  (value silver)))))             
       )
      (cond
       ((< rand2 0.9) (perceive '((legs 1  (value 4))))) 
       ((< rand2 1) (perceive '((legs 1  (value 3)))))             
       )
      )
     )  
    )
  )

(defun random-walk-10(number-of-decisions &optional number-of-test-decisions)
  (init)
  (unless number-of-test-decisions (setf number-of-test-decisions 10))
  (setf MAX-DECISIONS number-of-decisions)
  (learn '(:gd))
  (setf max-gdl-increment 0.2)
  (setf learning-rate 0.01)
  (setf max-fraction-pa-regions 0)

  (new-type 'obj-type :constants '(walker table dog human))
  (new-type 'color :constants '(silver brown white))
  (new-type 'i04 :numeric t :discrete t :min 0 :max 5)


  (predicate 'object :perception t :arguments '((object obj-type %)))

  (predicate 'legs :perception t :arguments '( (value i04 %)))
  (predicate 'color :perception t :arguments '((value color %)))
  (predicate 'alive :perception t :arguments '((value boolean %)))

  ; Function predicates
  (predicate 'object-prior
             :arguments '((object obj-type %))
             :function 1)

  (predicate 'object-color
             :arguments '((object obj-type) (color color %))
             :function 1)

  (predicate 'object-legs
             :arguments '((object obj-type) (legs i04 %))
             :function 1)

  (predicate 'object-alive
             :arguments '((object obj-type) (alive boolean %))
             :function 1)

  (setq post-run `((when (= decision-count ,number-of-decisions) (halt))))


  (setq perceive-list `(                      
                        (perceive-object-features t)                                           
                        ))


  (conditional 'perceived-objects
               :condacts '(  
                           (object (object (obj)))   
                           (object-prior (object (obj)))
                           )
               )

  (conditional 'object-color*join

               :condacts '((object (object (obj))) 
                           (color (value (color)))
                           (object-color (object (obj)) (color (color))))
               )

  (conditional 'object-legs*join
               :condacts '((object (object (obj)))
                           (legs (value (legs)))
                           (object-legs (object (obj)) (legs (legs))))
               )
  (conditional 'object-alive*join                
               :condacts '((object (object (obj)))
                           (alive (value (alive)))
                           (object-alive (object (obj)) (alive (alive))))
               ) 

  (format trace-stream "~%~&~&********************Training Phase ~%")
  (trials 1)

  (learn)
  (setq post-run `((when (= decision-count (+ ,number-of-decisions ,number-of-test-decisions)) (halt))))
  (setf post-t '(
                 (ppfs 'array) 
                 )
        )

  (setq perceive-list `(
                        (perceive-object-features nil)                                           
                        ))
  (setq post-d '(               
                 (format trace-stream "~&~&Guess object: ~S~%" (best-in-plm (vnp 'object))) 
                 ))
  (format trace-stream "~%~&~&******************Testing Phase ~%")
  (trials 1)
  )

(defun perceive-object-features-grid (perceive-object one-object-per-location)
  (let ( (rand0 (random 1.0))
         (rand (random 1.0))
         (rand2 (random 1.0))
         object
         )

    (if one-object-per-location
        (case 1d-grid-location
          (1 (setf object 'dog))
          (2 (setf object 'human))
          (3 (setf object 'walker))
          (4 (setf object 'table))
          (5 (setf object 'dog))
          (6 (setf object 'table))
          (7 (setf object 'walker))                                              
          ) 
 
      (case 1d-grid-location
        (1 
         (cond 
          ((< rand0 0.5)  (setf object 'dog) )
          ((< rand0 1)  (setf object 'human) )
          )
         )
        (2
         (cond
          ((< rand0 0.5) (setf object 'human) )
          ((< rand0 1)  (setf object 'table) )
          )
         )  
        (3
         (cond
          ((< rand0 0.5)  (setf object 'walker) )
          ((< rand0 1)  (setf object 'human) )
          ) 
         )
        (4 
         (cond
          ((< rand0 0.5)  (setf object 'table) )
          ((< rand0 1)  (setf object 'walker) )
          )
         )
        (5
         (cond
          ((< rand0 0.5)  (setf object 'dog) )
          ((< rand0 1)  (setf object 'walker))
          ) 
         )
        (6
         (cond
          ((< rand0 0.5) (setf object 'table) )
          ((< rand0 1)  (setf object 'dog) )
          )
         )
        (7 
         (cond
          ((< rand0 0.5)  (setf object 'walker) )
          ((< rand0 1) (setf object 'table) )
          )  
         )
        )
      )

    (unless perceive-object  (format trace-stream "~&~&Correct object: ~S~%" object )) 
    (if perceive-object 
        (perceive `((object 1 (object ,object))))
       (perceive '((object 1 (object  *))))
      )

    (cond
     ((eq object 'dog)
      (perceive '((alive 1  (value true))))
      (cond
       ( (< rand 0.7) (perceive '((color 1  (value brown))))) 
       ( (< rand 0.95) (perceive '((color 1 (value white))))) 
       ( (< rand 1) (perceive '((color 1  (value silver)))))             
       )
      (cond
       ((< rand2 0.8) (perceive '((legs 1  (value 4))))) 
       ((< rand2 1) (perceive '((legs 1 (value 3)))))   
       )

      )
     ((eq object 'walker)
      (perceive '((alive 1  (value false))))
      (cond
       ( (< rand 0.95) (perceive '((color 1  (value silver))))) 
       ( (< rand 1) (perceive '((color 1  (value brown)))))             
       )
      (cond
       ( (< rand2 1) (perceive '((legs 1  (value 1)))))           
       )
      )
     ((eq object 'human)
      (perceive '((alive 1  (value true))))
      (cond
       ((< rand 0.5) (perceive '((color 1  (value brown))))) 
       ((< rand 1) (perceive '((color 1 (value white)))))             
       )
      (cond
       ((< rand2 0.97) (perceive '((legs 1  (value 2))))) 
       ((< rand2 0.99)  (perceive '((legs 1  (value 1)))))   
       ((< rand2 1.0)  (perceive '((legs 1  (value 0)))))
       )
      )
     ((eq object 'table)
      (perceive '((alive 1  (value false))))
      (cond
       ((< rand 0.4) (perceive '((color 1  (value brown))))) 
       ((< rand 1) (perceive '((color 1  (value silver)))))             
       )
      (cond
       ((< rand2 0.9) (perceive '((legs 1  (value 4))))) 
       ((< rand2 1) (perceive '((legs 1  (value 3)))))             
       )
      )
     )  
    )
  )

(defun random-walk-11(number-of-decisions &optional perception-prob perception-mass action-prob)
  (init '(left right none))
  (setf MAX-DECISIONS number-of-decisions)
  (learn '(:gd))
  (setf max-gdl-increment 0.2)
  (setf learning-rate 0.01)
  (operator-selection 'boltzmann)
  (setf max-fraction-pa-regions 0)
  (new-type '1d-grid :numeric t :discrete t :min 1 :max 8)
  (new-type 'obj-type :constants '(walker table dog human))
  (new-type 'color :constants '(silver brown white))
  (new-type 'i04 :numeric t :discrete t :min 0 :max 5)

  (predicate 'location :perception t :arguments '((x 1D-grid %)))
  (predicate 'location-selected :world 'closed :arguments '((x 1D-grid !)))
  (predicate 'object :perception t :arguments '((object obj-type %)))
  (predicate 'map :arguments '( (location 1D-grid) (object obj-type %)) :function 1)

  (predicate 'legs :perception t :arguments '( (value i04 %)))
  (predicate 'color :perception t :arguments '((value color %)))
  (predicate 'alive :perception t :arguments '((value boolean %)))

  ; Function predicates

  (predicate 'object-color
             :arguments '((object obj-type) (color color %))
             :function 1)
  (predicate 'object-legs
             :arguments '((object obj-type) (legs i04 %))
             :function 1)
  (predicate 'object-alive
             :arguments '((object obj-type) (alive boolean %))
             :function 1)

  (setq pre-t '((setf 1d-grid-location 4) )) 
  (setq post-run `((when (= decision-count ,number-of-decisions) (halt))))


  (setq pre-d '(
                (format trace-stream "~&~&Current location: ~S~%" 1d-grid-location)
                ))

  (setq perceive-list `(
                        (perceive-location ,perception-prob ,perception-mass)
                        (perceive-object-features-grid t t)                                           
                        ))

  (setq action-list `((execute-operator ,action-prob)))

  (conditional 'perceived-objects
               :condacts '(  
                           (object (object (obj)))   
                           (location (x (loc)))
                           (map (object (obj)) (location (loc)))
                           )
               )

  (conditional 'object-color*join

               :condacts '((object (object (obj))) 
                           (color (value (color)))
                           (object-color (object (obj)) (color (color))))
               )

  (conditional 'object-legs*join
               :condacts '((object (object (obj)))
                           (legs (value (legs)))
                           (object-legs (object (obj)) (legs (legs))))
               )
  (conditional 'object-alive*join                
               :condacts '((object (object (obj)))
                           (alive (value (alive)))
                           (object-alive (object (obj)) (alive (alive))))
               ) 

  (conditional 'select-location
               :conditions '((location (x (location))))
               :actions '((location-selected (x (location))))
               )

  (conditional 'acceptable
               :actions '((selected (operator *))                         
                          )
               )
  (format trace-stream "~%~&~&********************Training Phase ~%")
  (trials 1)
  
  (learn)
  (setq post-run `((when (= decision-count (+ ,number-of-decisions 10)) (halt))))
  (setf post-t '( 
                 (ppfs 'array)
                 )
        )
  (setq perceive-list `(
                        (perceive-location ,perception-prob ,perception-mass)
                        (perceive-object-features-grid nil t)                                           
                        ))
  (setq post-d '(
                 (format trace-stream "~&~&Guess object: ~S~%" (best-in-plm (vnp 'object))) 
                 (format trace-stream "~&~&Guess location: ~S~%" (best-in-plm (vnp 'location)))
                 ))
  (format trace-stream "~%~&~&********************Testing Phase ~%")
  (trials 1)
  )

(defun perceive-location*next (&optional correct-prob correct-mass)
  (unless correct-prob (setq correct-prob 1.0))
  (unless correct-mass (setq correct-mass 1.0))
  (let ((rand (random 1.0))
        location ; Perceived location
        1-cm
        )
    (setq 1-cm (- 1 correct-mass))
    (setq location 1d-grid-location)
    (format trace-stream "~&~&Perceive Current location: ~S~%" 1d-grid-location)
    ; Perceive new location with correct-prob of getting right (and otherwise on one side)
    (cond ((= 1d-grid-location 1)
           (when (>= rand correct-prob) (setq location 2))
           )
          ((= 1d-grid-location 7)
           (when (>= rand correct-prob) (setq location 6))
           )
          (t
           (when (>= rand correct-prob)
             (if (< (random 1.0) .5)
                 (setq location (1- 1d-grid-location))
               (setq location (1+ 1d-grid-location)))
             )
           )
          )
    ; Zero out all perception for predicate location
    (perceive '((location*next 0)))
    ; Generate noisy perceptions based on correct-mass
    (perceive `((location*next ,correct-mass (x ,location)))) ; Correct-mass at location
    ; Divide incorrect mass among adjacent locations when they exist
    (cond ((= location 1)
           (perceive `((location*next ,1-cm  (x 2))))
           )
          ((= location 7)
           (perceive `((location*next ,1-cm (x 6))))
           )
          (t
           (perceive `((location*next ,(/ 1-cm 2)  (x ,(1- location)))
                       (location*next ,(/ 1-cm 2)  (x ,(1+ location)))
                       )
                     )
           )
          )
      )
  )



(defun random-walk-12(number-of-decisions &optional perception-prob perception-mass action-prob)
  (init '(left right none))
  (learn '(:am))
  (setf learning-rate 0.05)
  (operator-selection 'boltzmann)
  (setf max-fraction-pa-regions 0)
  (setf max-decisions 10000)
  (new-type '1D-grid :numeric t :discrete t :min 1 :max 8)

  (predicate 'location :world 'closed :perception t :arguments '((state state) (x 1D-grid !)))

  (setq pre-t '((setf 1d-grid-location 4) )) 
  (setq post-run `( (when (= decision-count ,number-of-decisions) (halt))))
  
  (setf post-t '( 
                 (ppfs 'array)
                 )
        )

  (setq perceive-list `((perceive-location*next ,perception-prob ,perception-mass)
                        ))

  (setq action-list `((execute-operator ,action-prob)))

  (conditional 'acceptable
              :actions '((selected (operator *))                         
                          )
               )

  (trials 1)

)

(defun random-walk-13(number-of-decisions &optional perception-prob perception-mass action-prob)
  (init '(left right none))
  (learn '(:pm :am))
  (setf learning-rate 0.01)
  (operator-selection 'boltzmann)
  (setf max-fraction-pa-regions 0)
  (setf max-decisions 10000)
  (new-type '1D-grid :numeric t :discrete t :min 1 :max 8)
  (new-type 'obj-type :constants '(walker table dog human))

  (predicate 'location :world 'closed :perception t  :arguments '((state state) (x 1D-grid !)))
  (predicate 'object :perception t :arguments '( (object obj-type %)))


  (setq pre-t '((setf 1d-grid-location 4) )) 
  (setq post-run `( (when (= decision-count ,number-of-decisions) (halt))))

  (setq perceive-list `((perceive-location*next ,perception-prob ,perception-mass)
                        (perceive-object)                
                        ))
  (setf post-t '( 
                 (ppfs 'array)
                 )
        )

  (setq action-list `((execute-operator ,action-prob)))

  (conditional 'acceptable
              :actions '((selected (operator *))                         
                          )
               )

  (trials 1)

)

; Assign a fixed reward based on location
(defun assign-reward-rw (rewards-rw)
  (let ((cl 1d-grid-location))
    (eval `(perceive (quote ((reward .1 (location-x *) (value *)) ; Empty WM of any previous rewards
                             (reward (location-x ,cl) (value ,(aref rewards-rw (- cl 1)))))))) ; Add reward for current state
    )
  )


; Fixed vector of rewards for use in assign-reward
(defparameter rewards-rw (vector 0 9 0 0 0 0 0 ))

(defun test-rl-print-tutorial (stream)
  (format stream "~&~%PROJECTED FF (EV):~&")
  (pa 'projected nil '((expected wm-value)) stream)
  (format stream "~&~%Q FF (EV):~&")
  (pa 'q nil '((expected wm-value)) stream)
  (format stream "~&~%REWARD FF (EV):~&")
  (pa 'reward nil '((expected wm-value)) stream)
  (format stream "~&~%")
  )

; 1D Grid for RL with automatic RL structure generation
(defun random-walk-14(number-of-decisions &optional perception-prob perception-mass action-prob)
  (init '(left right none))
  ;(init-temporal-conditional)
  (learn '(:pm :am :rl))
  (setf learning-rate 0.05)
  (operator-selection 'boltzmann)
  (setf max-fraction-pa-regions 0)
  (setf max-decisions 10000)
  (new-type '1D-grid :numeric t :discrete t :min 1 :max 8)
  (new-type 'obj-type :constants '(walker table dog human))


  (predicate 'location :world 'closed :perception t  :arguments '((state state) (x 1D-grid !)))
  (predicate 'object :perception t :arguments '((state state) (object obj-type %)))


  (setq pre-t '((setf 1d-grid-location 4) )) 
  (setq post-run `( (when (= decision-count ,number-of-decisions) (halt))))

  (setq post-d '(
                 (format trace-stream "~&~&Perceived object post-d: ~S~%" (best-in-plm (vnp 'object) 0))               
                   ))
  (setq post-t '(
                 (test-rl-print-tutorial trace-stream)
                 ))

  (setq perceive-list `(
                        (assign-reward-rw rewards-rw)
                        (format trace-stream "~&~&Perceived object at perception: ~S~%" (best-in-plm (vnp 'object) 0))
                        (perceive-location*next ,perception-prob ,perception-mass)
                        (perceive-object)                  
                        ))

  (setq action-list `((execute-operator ,action-prob)))

  (conditional 'acceptable
              :actions '((selected (operator *))                         
                          )
               )

  (trials 1)

)



(defun tutorial-test()

(random-walk-1) ;first model
(random-walk-2) ;operator-selection rule
(random-walk-3) ;internal map
(random-walk-4) ;trials
(random-walk-5 0.8 0.8) ;some probability on location 1 or location 7 makes the model halt
(random-walk-6 0.8 0.8) ;location selection addresses the problem above
(random-walk-7 0.8 0.8) ;object-perceived does not learn the map
(random-walk-8 200 0.8 0.8) ;learning is on, learning the map
(random-walk-9 500 0.6 0.6) ;modification slam
(random-walk-10 300) ;semantic memory
(random-walk-11 300 0.8 0.8) ;semantic memory on a grid, each location has only one probable object
(random-walk-12 300 0.8 0.8) ;action modeling
(random-walk-13 300 0.8 0.8) ;perception modeling
(random-walk-14 500 0.9 0.9) ;reinforcement learning

)