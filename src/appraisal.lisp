(in-package :sigma)
; Progress and similarity/difference measures between two piecewise-constant unique PLMs

; Compute the within-integral portion of the Bhattacharyya coefficient over two constant values
(defun inner-bc-constants (p q)
    (sqrt (* p q))
  )

; Compute the Bhattacharyya coefficient for all pairs of regions in two PLMS
(defun bc-plms (pp qp ds)
  (unless (and (plm-piecewise-constant pp) (plm-piecewise-constant qp))
    (error "Attempt to compute the Bhattacharyya coefficient on a piecewise-linear PLM; Must be piecewise-constant")
    )
  (integral-plm (combine-plms pp nil qp 'inner-bc) ds)
  )

; Compute the Hellinger difference from the Bhattacharyya coefficient for one constant function
(defun hd-from-bc (bc &optional r piecewise-constant)
  (declare (ignore r piecewise-constant))
  (sqrt (- 1 (min bc 1))) ; In case bc ended up a smidgen over 1, reduce it back to 1 and compute difference
  )

; Compute the Hellinger difference from the Bhattacharyya coefficient for one PLM
(defun hd-from-bc-plm (bcp)
  (transform-plm #'hd-from-bc bcp)
  )

; Compute the Hellinger difference for all pairs of regions in two plms
(defun hd-plms (pp qp ds)
  (unless (and (plm-piecewise-constant pp) (plm-piecewise-constant qp))
    (error "Attempt to compute the Hellinger difference on a piecewise-linear PLM; Must be piecewise-constant")
    )
  (hd-from-bc-plm (bc-plms pp qp ds))
  )

; Compute the Bhattacharyya difference from the Bhattacharyya coefficient for one constant function
(defun bd-from-bc (bc &optional r piecewise-constant)
  (declare (ignore r piecewise-constant))
  (- (log bc))
  )

; Compute the Bhattacharyya difference from the Bhattacharyya- oefficient for one PLM
(defun bd-from-bc-plm (bcp)
  (transform-plm #'bd-from-bc bcp)
  )

; Return a list of the unique dimensions of a predicate
(defun unique-dimensions-predicate (pred)
  (let ((args (predicate-arguments pred)))
    (mapcar #'(lambda (arg-name) (position arg-name args :key #'car)) (predicate-unique pred))
    )
  )

; Return a list of all of the dimension numbers except for the state and agent
(defun all-but-agent-and-state-dimensions-plm (pred)
  (let (ds
        (count 0))
    (dolist (arg (predicate-arguments pred))
      (unless (member (car arg) '(state agent))
        (setq ds (cons count ds))
        )
      (setq count (1+ count))
      )
    ds)
  )

; Is a function 0?
(defun vector-0 (f)
  (every #'(lambda (x) (e= x 0)) f)
  )

; Return 0 if second function is 0 otherwise first function
(defun constrain-by-0-function (f1 f2 &optional piecewise-constant)
  (if piecewise-constant
      (if (e= f2 0) 0 f1)
    (if (vector-0 f2) (copy-seq f2) (copy-seq f1)))
  )

; Convert any form of unique argument to selection
(defun make-unique-argument-select (arg)
  (if (> (length arg) 2)
      (list (car arg) (cadr arg) '!)
    arg)
  )
(defun make-unique-arguments-select (args)
  (mapcar #'make-unique-argument-select args)
  )

; Yield an argument list by removing all of the unique arguments from a predicate's argument list
; Make all of the rest, other than state and agent arguments, distribution
(defun meta-arguments (pred)
  (let ((unique (predicate-unique pred))
        meta-arguments
        )
    ; Extract out universal arguments
    (dolist (arg (predicate-arguments pred))
      (unless (member (car arg) unique)
        (setq meta-arguments (if (member (car arg) '(state agent)) ; Keep these arguments universal
                                 (cons (list (car arg) (cadr arg)) meta-arguments)
                               (cons (list (car arg) (cadr arg) '%) meta-arguments)))
        )
      )
    (setq meta-arguments (nreverse meta-arguments))
    meta-arguments)
  )

; Yield a variable map from an appraisal function to its regular predicate function
(defun meta-map (pred)
  (let* ((unique (predicate-unique pred))
         (appraisal-index 0) ; Index in appraisal function's variables
         (predicate-index 0) ; Index in predicate fucntion's variables
         (vfactor (init-vector (- (length (predicate-arguments pred)) (length unique))))
        )
    ; Extract out universal arguments
    (dolist (arg (predicate-arguments pred))
      (unless (member (car arg) unique)
        (setf (aref vfactor appraisal-index) predicate-index)
        (setq appraisal-index (1+ appraisal-index))
        )
      (setq predicate-index (1+ predicate-index))
      )
    (make-smap :vfactor vfactor))
  )


; Print out link directions with messages that are not marked as piecewise constant
(defun npc nil
  (dolist (l (graph-links cg))
    (dotimes (i 2)
      (when (and (aref (link-contents l) i) (not (plm-piecewise-constant (aref (link-contents l) i))))
        (format t "~S->~S~%" (node-number (aref (link-nodes l) i)) (node-number (aref (link-nodes l) (- 1 i))))
        )
      )
    )
  )

; Is a region's function constant?
(defun region-function-variablep (r)
  (find-if #'(lambda (x) (not (zerop x))) (region-dimensions r) :key #'dimension-weight)
  )

; Determine if a PLM is piecewise constant
(defun plm-piecewise-constantp (plm)
  (let ((rarray (plm-array plm))
        (piecewise-constant t))
    (dotimes (i (array-total-size rarray))
      (when (region-function-variablep (row-major-aref rarray i))
        (setq piecewise-constant nil)
        (return)
        )
      )
    piecewise-constant)
  )


; Make variables active in new PLM if active in either of old ones
; Assumes new active is already a copy of second old one
(defun update-combine-active (a1 map a2 an)
  (let (mapv d)
    (when map
      (setq mapv (smap-vfactor map))
      )
    (dotimes (i (length a1))
      (setq d (if mapv (aref mapv i) i))
      (when (and (aref a1 i) (not (aref a2 d)))
        (setf (aref an d) t)
        )
      )
    )
  )

; Summarize a global meta-predicate function to yield a total appraisal value
; Divisor is the number of active predicates of this type
(defun global-summarized-predicate-plm (plm divisor)
  (let* (d ; Dimension along which to summarize
         (ovs (plm-variables plm)) ; Variables in original PLM
         (nlength (1- (length ovs))) ; Length of new variable vector
         (nvs (init-vector nlength)) ; Variables for result PLM
        )
    (if (= nlength 0)
        (progn
          (setq d 0)
          (setq nvs (vector))
          )
      (if (eq (stype-name (svariable-type (aref (plm-variables plm) 0))) 'predicate) ; Assumes nlength can only be 1 if not 0
          (progn
            (setq d 0)
            (setq nvs (vector (aref ovs 1)))
            )
        (progn
          (setq d 1)
          (setq nvs (vector (aref ovs 0)))
          )))
    (strip-and-reorder-plm nil nvs (transform-plm #'scale-function (integral-plm plm d) (/ 1.0 divisor)))
    )
  )

; Update global appraisal variables (both distributions and single values)
(defun update-global-appraisal-variables nil
  (when (graph-surprise-distribution-predicate cg)
    (setf (predicate-perception-temp-mem (graph-surprise-distribution-predicate cg)) (global-meta-predicate-plm 'surprise t)) ; Global surprise distribution
    (setf (predicate-perception-temp-mem (graph-surprise-predicate cg))
          (global-summarized-predicate-plm (predicate-perception-temp-mem (graph-surprise-distribution-predicate cg)) (length (graph-surprise-predicates cg)))) ; Global surprise
    )
  (when (graph-progress-distribution-predicate cg)
    (setf (predicate-perception-temp-mem (graph-progress-distribution-predicate cg)) (global-meta-predicate-plm 'progress t)) ; Global progress distribution
    (setf (predicate-perception-temp-mem (graph-progress-predicate cg))
          (global-summarized-predicate-plm (predicate-perception-temp-mem (graph-progress-distribution-predicate cg)) (length (graph-progress-predicates cg)))) ; Global progress
    )
  (when (graph-difference-distribution-predicate cg)
    (setf (predicate-perception-temp-mem (graph-difference-distribution-predicate cg)) (global-meta-predicate-plm 'difference t)) ; Global difference distribution
    (setf (predicate-perception-temp-mem (graph-difference-predicate cg))
          (global-summarized-predicate-plm (predicate-perception-temp-mem (graph-difference-distribution-predicate cg)) (length (graph-difference-predicates cg)))) ; Global difference
    )
  (when (graph-attention-distribution-predicate cg)
    (setf (predicate-perception-temp-mem (graph-attention-distribution-predicate cg)) (global-meta-predicate-plm 'attention t)) ; Global attention distribution
    (setf (predicate-perception-temp-mem (graph-attention-predicate cg))
          (global-summarized-predicate-plm (predicate-perception-temp-mem (graph-attention-distribution-predicate cg)) (length (graph-attention-predicates cg)))) ; Global attention
    )
  )

;------------
; Enable remove-unneeded slices with an absolute comparison of region functions for attention

; Determine if constants are equal for two regions
(defun constant-equal (r1 r2 &optional absolute)
  (e= (region-constant r1) (region-constant r2) absolute))

; Determine if the weights along a dimension are equal for two regions
(defun weight-equal (r1 r2 d &optional absolute)
  (e= (region-weight r1 d) (region-weight r2 d) absolute arousal))

; Determine if the functions defined by two regions are equal
(defun function-equal (r1 r2 &optional absolute)
  (if (and (constant-equal r1 r2 absolute)
           (eq (region-exponential r1) (region-exponential r2))
           )
      (dotimes (d (region-rank r1) t)
        (unless (weight-equal r1 r2 d absolute)
          (return nil)
          )
        )
    nil)
  )


; Compute exponential of (* function mult) (actually exponentiating mult times the constant while ignore dimension weights)
(defun exponentiate-constant-times-function (f r mult &optional piecewise-constant)
  (if piecewise-constant
      (exp (* f mult))
    (let (nf)
      r ; dummy so no warning about r not being used
      (setq nf (init-vector (length f) 0))
      (setf (aref nf 0) (exp (* (aref f 0) mult)))
      nf))
  )

; Create a list counting up to 1- number
(defun rank-list (n)
  (let (l)
    (dotimes (i n)
      (setq l (cons i l))
      )
    (nreverse l)
    )
  )

; Recur on average-regions-in-span
(defun average-regions-in-span-recur (ur rarray i sizev starting-indices rank regions-included sum-of-functions)
  (cond ((< i rank)
         (let ((sis (copy-seq starting-indices)))
           (do ((si sis (region-after-index-list si i (1- (aref sizev i))))) ; Destructive
               ((or (null si) (e> (region-max (apply #'aref rarray si) i) (region-max ur i))) nil)
             (multiple-value-setq (regions-included sum-of-functions) (average-regions-in-span-recur ur rarray (1+ i) sizev si rank regions-included sum-of-functions))
             )
           )
         )
        ((= i rank)
         (let ((r (apply #'aref rarray starting-indices)))
;           (format t "~S" starting-indices)
           (when (within-region r nil ur)
             (setq regions-included (1+ regions-included))
             (setq sum-of-functions (vector-sum sum-of-functions (extract-function r)))
             )
           )
         )
        )
  (values regions-included sum-of-functions)
  )

; Avearge PLM regions within span of specified region
(defun average-regions-in-span (ur p sizev starting-indices)
  (let (regions-included sum-of-functions average-function)
;    (format t "~&~S ~S: " (region-mins ur) (region-maxs ur))
    (multiple-value-setq (regions-included sum-of-functions)
        (average-regions-in-span-recur ur (plm-array p) 0 sizev starting-indices (plm-rank p) 0 (init-vector (1+ (plm-rank p)) 0)))
    (setq average-function (init-vector (length sum-of-functions)))
    (dotimes (i (length sum-of-functions))
      (setf (aref average-function i) (/ (aref sum-of-functions i) regions-included))
      )
    average-function)
  )

; Eliminate slices from a needed dimensional vector in afun if not already in pfun
(defun remove-moot-slices-d (ndv asl psl)
  (let* (ploc ; Location of slice in pfun
         (alength (length asl))
         (plength (length psl))
         (result (init-vector plength))
         (i 0)
         )
    (if (= plength alength) ; Already have same slices
        ndv
      (progn
        (dolist (ps psl)
          (setq ploc (slice-location ps))
          (do ((asc asl (cdr asc)))
              ((null asc) (error "Couldn't find original slice in appraisal slices in REMOVE-MOOT-SLICES-D"))
            (when (e= ploc (slice-location (car asc)) t)
              (setf (aref result i) (aref ndv (slice-index (car asc)))) ; Assign old needed to new
              (setq asl (cdr asc))
              (setq i (1+ i))
              (return)
              )
            )
          )
        result)
      )
    )
  )

; Eliminate slices from afun needed if already not in pfun
(defun remove-moot-slices-ds (needed aslices pslices)
  (let* ((length (length needed))
         (nn (init-vector (length needed)))
         )
    (dotimes (i length)
      (setf (aref nn i) (remove-moot-slices-d (aref needed i) (aref aslices i) (aref pslices i)))
      )
    nn)
  )

; Maximize out all unique dimensions
(defun maximize-unique-dimensions (p)
  (dolist (d (unique-dimensions p))
    (setq p (maximize-plm p d))
    )
  p)

; Given a PLM, scale values in universal regions so that largest value is equal to max
(defun scale-max-plm (p max); 
  (let (mp
        maximum ; Result of maximizing out unique dimensions
        )
    (when trace-transform
      (format trace-stream "~&~%SCALE TO MAX:")
      (format trace-stream "~&~%P: ") (print-smart p symbolic-trace trace-stream)
      )
    (setq maximum (maximize-unique-dimensions p))
    (setf (plm-active maximum) (init-vector (plm-rank maximum) t))
    (setq mp (combine-plms p nil maximum 'divide-0))
    (setq mp (transform-plm #'scale-function mp max nil t))
    (when trace-transform
      (format trace-stream "~&~%SCALE TO MAX RESULT: ") (print-smart mp symbolic-trace trace-stream)
      )
    mp)
  )

; Apply attention function to a predicate function
(defun apply-attention (pred pfun)
  (let* ((afun (node-function (predicate-perception (predicate-attention-predicate pred)))) ; Attention map for predicate
         (rank (plm-rank pfun))
         sizev ; Sizes of dimensions
         needed ; Slices needed based on attention
         (pred-name (predicate-name pred))
         )
    (unless (predicate-no-normalize (predicate-attention-predicate pred))
      (setq afun (scale-max-plm afun attention-exponential-numerator))
      (when trace-attention
        (format trace-stream "~&~%~S Attention Map ~S (Scaled):~&"
                (predicate-name pred)
                (if (predicate-surprise-predicate pred)
                    (if (predicate-progress-predicate pred)
                        (if (closed-world pred)
                            "Surprise POR Difference"
                          "Surprise POR Progress")
                      "Surprise")
                  (if (closed-world pred)
                      "Difference"
                    "Progress")))
        (parray afun nil nil trace-stream)
        )
      )
    (setq afun (transform-plm #'exponentiate-constant-function afun))
    (when trace-attention
      (format trace-stream "~&~%~S Attention Map (Exponentially Transformed but Unnormalized):" pred-name)
      (pa afun nil nil trace-stream)
      )
    (setq afun (normalize-plm afun (rank-list (plm-rank afun))))
    (when trace-attention
      (format trace-stream "~&~%~S Attention Map (Exponentially Transformed and Normalized):" pred-name)
      (pa afun nil nil trace-stream)
      )
    (setq afun (combine-plms afun (meta-map pred) pfun 'product nil nil t))
    (when trace-attention
      (format trace-stream "~&~%~S Product of Attention Map and Node Function:" pred-name)
      (pa afun nil nil trace-stream)
      )
    (setq sizev (dimension-sizes-v afun))
    (setq needed (slices-needed afun rank sizev (dimension-multipliers sizev) t))
    (setq needed (remove-moot-slices-ds needed (plm-slices afun) (plm-slices pfun))) ; Remove from needed slices already not in pfun
    (setf (plm-removed-unneeded-slices pfun) nil) ; Force a new removal
    (normalize-plm (remove-unneeded-slices pfun needed 'average nil t))
    )
  )


;--------------
; Update global meta-predicates

; Create a CPT entry from a constant region of a PLM
(defun region-cpt-entry (r rank active)
  (let (cpt)
    (dotimes (j rank)
      (when (aref active j)
        (push (list (region-min r j) (region-max r j)) cpt))
      )
    (cons (region-constant r) (reverse cpt)))
  )

; Convert a meta-predicate to a CPT with all dimensions but agent (if exists) summarized out, and original predicate added as first dimension
; For use in updating a global meta-predicate
(defun meta-predicate-plm-to-cpt (plm pred-args original-pred-name)
  (let (cpt
        entry
        array
        (rank (plm-rank plm))
        (active (plm-active plm))
        )
    (dotimes (d rank)
      (when (and (aref active d)
                 (not (and multiagent
                           (eq (caar pred-args) 'agent)
                           ))
                 )
        (setq plm (integral-plm plm d))
        )
      (setq pred-args (cdr pred-args))                 
      )
    (setq array (plm-array plm))
    (setq active (plm-active plm))
    (dotimes (i (array-total-size array))
      (setq entry (region-cpt-entry (row-major-aref array i) rank active))
      (setq cpt (cons (cons (car entry) (cons original-pred-name (cdr entry)))  cpt))
      )
    cpt)
  )

; Combine open/progress and closed/difference based global attention distributions into one based on worlds of predicates
; Assumes that predicates in graph-predicates and predicate names in predicate type are the same and in the same order
(defun combine-open-closed-world-attention-distributions (att-open att-closed)
  (let ((predicate-index (position 'predicate (plm-variables att-open) :key #'(lambda (v) (stype-name (svariable-type v)))))
        oarray
        carray
        (worlds (mapcar #'predicate-world (graph-predicates cg)))
        )
    ; Shatter the two distributions along the predicate dimension so that can easily track predicate world
    (setq att-open (shatter-plm att-open predicate-index))
    (setq att-closed (shatter-plm att-closed predicate-index))
    ; Get region arrays
    (setq oarray (plm-array att-open))
    (setq carray (plm-array att-closed))
    (if multiagent
        (progn
          )
      (progn
        (dotimes (i (length worlds))
          (when (eq (car worlds) 'open)
            (setf (region-constant (aref carray i)) (region-constant (aref oarray i)))
            )
          (setq worlds (cdr worlds))
          )
        (remove-unneeded-slices att-closed)))
    )
  )

; Compute global attention distribution from global surprise, progress and distribution rather than summarizing over predicate attention functions
(defun compute-global-attention-distribution nil
  (let* ((predicate-worlds (mapcar #'predicate-world (graph-attention-predicates cg)))
         (open (member 'open predicate-worlds))
         (closed (member 'closed predicate-worlds)))
    (if (graph-surprise-distribution-predicate cg)
        (if (graph-progress-distribution-predicate cg)
            (if open
                (if closed
                    (combine-open-closed-world-attention-distributions
                     (combine-plms (predicate-perception-temp-mem (graph-surprise-distribution-predicate cg)) nil (predicate-perception-temp-mem (graph-progress-distribution-predicate cg)) 'por)
                     (combine-plms (predicate-perception-temp-mem (graph-surprise-distribution-predicate cg)) nil (predicate-perception-temp-mem (graph-difference-distribution-predicate cg)) 'por))
                  (combine-plms (predicate-perception-temp-mem (graph-surprise-distribution-predicate cg)) nil (predicate-perception-temp-mem (graph-progress-distribution-predicate cg)) 'por))
              (when closed
                (combine-plms (predicate-perception-temp-mem (graph-surprise-distribution-predicate cg)) nil (predicate-perception-temp-mem (graph-difference-distribution-predicate cg)) 'por)
                ))
          (predicate-perception-temp-mem (graph-surprise-distribution-predicate cg)))
      (if (graph-progress-distribution-predicate cg)
          (if open
              (if closed
                  (combine-open-closed-world-attention-distributions
                   (predicate-perception-temp-mem (graph-progress-distribution-predicate cg))
                   (predicate-perception-temp-mem (graph-difference-distribution-predicate cg)))
                (predicate-perception-temp-mem (graph-progress-distribution-predicate cg)))
            (when closed
              (predicate-perception-temp-mem (graph-difference-distribution-predicate cg))
              ))))
    )
  )

; Create a CPT for full global meta-predicate
(defun global-meta-predicate-plm (mp-type &optional use-temp-perception-memory)
  (let ((pnames (stype-constants (graph-predicate-type cg)))
        pred-access
        global-pred
        cpt
        mp-pred
        vs
        )
    (case mp-type
      ((attention)
       (setq pred-access #'predicate-attention-predicate)
       (setq global-pred (graph-attention-distribution-predicate cg))
       )
      ((surprise)
       (setq pred-access #'predicate-surprise-predicate)
       (setq global-pred (graph-surprise-distribution-predicate cg))
       )
      ((progress)
       (setq pred-access #'predicate-progress-predicate)
       (setq global-pred (graph-progress-distribution-predicate cg))
       )
      ((difference)
       (setq pred-access #'predicate-difference-predicate)
       (setq global-pred (graph-difference-distribution-predicate cg))
       )
      )
    (if (eq mp-type 'attention) ; Combine distribution from surprise and progress/difference rather than summarizing over *attention
        (compute-global-attention-distribution)
      (progn
        (dolist (pname pnames) ; Summarize appropriately over functions for each predicate
          (setq mp-pred (apply pred-access (list (predicate-from-name pname))))
          (when mp-pred ; The predicate has this type of appraisal
            (setq cpt (if use-temp-perception-memory
                          (if (predicate-perception-temp-mem mp-pred)
                              (append (meta-predicate-plm-to-cpt (predicate-perception-temp-mem mp-pred) (predicate-arguments mp-pred) pname) cpt)
                            nil)
                        (append (meta-predicate-plm-to-cpt (node-function (predicate-perception mp-pred)) (predicate-arguments mp-pred) pname) cpt)))
            )
          )
        (setq vs (node-variables (predicate-perception global-pred))) 
        (cpt-function-array-plm (variable-names vs) cpt vs 0)
        ))
    )
  )

(defun pimage ()
  (format t "~&~%Perceptual image: ~&")
  (pa (node-function (predicate-perception (predicate-from-name 'image))) nil '((argmax wm-color)))
  )
(defun pgoal ()
  (format t "~&~%Search image: ~&")
  (pa (node-function (predicate-wm (predicate-goal-predicate (predicate-from-name 'image)))) nil '((argmax wm-color)))
  )

; Generate a visual field with random colors
(defun vf-colors (n colors change-x change-y change-color)
  (let ((cs (length colors))
        vf)
    (dotimes (x n)
      (dotimes (y n)
        (if (and (= x change-x) (= y change-y))
            (setq vf (cons `(image (x ,x) (y ,y) (color ,change-color)) vf))
          (setq vf (cons `(image (x ,x) (y ,y) (color ,(nth (random cs) colors))) vf)))
        )
      )
    vf)
  )

; Program for testing surprise and attention with a large visual field (change happens in location 0,0)
(defun large-surprise (n &optional no-attention)
  (let* ((pes (vf-colors n '(red yellow green blue) 0 0 'blue))
         (pes-l (list pes)) ; For use with perceive in an apply form
         )
    (init)
    (learn '(:gd))
    (unless no-attention
      (setq compute-surprise t)
      (setq compute-attention t)
      (setq trace-attention t)
      )
;    (setq trace-gdl t)
    (setq perceive-list `((apply #'perceive ',pes-l)))
    (new-type 'color :constants '(red yellow green blue))
    (new-type 'dimension :numeric t :discrete t :min 0 :max n)
    (predicate 'image :perception t :arguments '((x dimension) (y dimension) (color color %)) :function 1)
    (predicate 'result :world 'closed :arguments '((x dimension) (y dimension) (color color !)))
    
    (d 2)
    (d 20)
    (format T "~&~%*** Changing perception of location 0,0 to green.")
    (setq pes (cons `(image (x 0) (y 0) (color green)) (remove-if #'(lambda (x) (and (equal (cadr x) '(x 0)) (equal (caddr x) '(y 0)))) pes)))
    (setq pes-l (list pes))
    (setq perceive-list `((apply #'perceive ',pes-l)))
    (d 3)
    )
  )

; Generate a visual function with random colors
(defun vfun-colors (n colors)
  (let ((cs (length colors))
        vf)
    (dotimes (x n)
      (dotimes (y n)
          (setq vf (cons (list 1 x y (nth (random cs) colors)) vf))
        )
      )
    vf)
  )

; Program for testing goals and attention with a large visual field
(defun large-goal (n &optional no-attention)
  (let* (pfn ; Predicate function node
         )
    (init)
    (learn nil)
    (setq compute-surprise nil)
    (if no-attention
        (progn
          (setq compute-progress nil)
          (setq compute-attention nil)
          (setq trace-attention nil)
          )
      (progn
        (setq compute-progress t)
        (setq compute-attention t)
        (setq trace-attention nil)
        )
      )
    (new-type 'color :constants '(red yellow green blue))
    (new-type 'dimension :numeric t :discrete t :min 0 :max n)
    (predicate 'location :world 'closed :perception t :arguments '((x dimension !) (y dimension !)))
    (predicate 'lcolor :world 'closed :arguments '((color color !)))
    (predicate 'image :perception t :arguments '((x dimension) (y dimension) (color color %))
               :function 1 :goal '((image*goal (x 0) (y 0))) :learning-rate 0)
    (setq pfn (predicate-function-node (predicate-from-name 'image)))
    (predicate 'result :world 'closed :arguments '((x dimension) (y dimension) (color color !)))

    (conditional 'location-color1
                 :conditions '((location (x (x)) (y (y)))
                               (image (x (x)) (y (y)) (color (c))))
                 :actions '((lcolor (color (c))))
                 )

    (conditional 'location-color2
                 :conditions '((location (x (x)) (y (y)))
                               (image (x (x)) (y (y)) (color (c))))
                 :actions '((lcolor (color (c))))
                 )


    (conditional 'location-color3
                 :conditions '((location (x (x)) (y (y)))
                               (image (x (x)) (y (y)) (color (c))))
                 :actions '((lcolor (color (c))))
                 )

    (conditional 'location-color4
                 :conditions '((location (x (x)) (y (y)))
                               (image (x (x)) (y (y)) (color (c))))
                 :actions '((lcolor (color (c))))
                 )

    (conditional 'location-color5
                 :conditions '((location (x (x)) (y (y)))
                               (image (x (x)) (y (y)) (color (c))))
                 :actions '((lcolor (color (c))))
                 )

    (conditional 'location-color6
                 :conditions '((location (x (x)) (y (y)))
                               (image (x (x)) (y (y)) (color (c))))
                 :actions '((lcolor (color (c))))
                 )

    (conditional 'location-color7
                 :conditions '((location (x (x)) (y (y)))
                               (image (x (x)) (y (y)) (color (c))))
                 :actions '((lcolor (color (c))))
                 )


    (evidence '((location (x 0) (y 0))))
    
    (d 1)
    (ppwm 'lcolor)

    ; Replace original uniform function with a complex one
    (setf (node-function pfn) (cpt-function-array-plm '(wm-x wm-y wm-color) (vfun-colors n '(red yellow green blue)) (node-variables pfn) 0))
    (setf (aref (graph-changes cg) (node-number (predicate-function-node (predicate-from-name 'image)))) t)

    (d 1)
    (ppwm 'lcolor)
    (message-statistics)
    )
  )

;--------------
; A program for exploring needs/drives/urges

(defun needs nil
  (init)
  (setq trace-attention t)
  (predicate 'hunger :world 'closed :arguments '((exists boolean %)) :goal '((hunger*goal (exists false))))
  (predicate 'thirst :world 'closed :arguments '((exists boolean %)) :goal '((thirst*goal (exists false))))
  (evidence '((hunger .3 (exists true)) (hunger .7 (exists false))
              (thirst .8 (exists true)) (thirst .2 (exists false))))
  (d 2)
  (pppfns)
  )

;--------------
; A program for testing mixture of open-world and closed-world appraisal predicates

(defun mixed-world-attention nil
  (init)
  (setq trace-attention t)
  (setq perceive-list '((perceive '((thirst .8 (exists true)) (thirst .2 (exists false))))))
  (predicate 'hunger :world 'closed :arguments '((exists boolean %)) :goal '((hunger*goal (exists false))))
  (predicate 'thirst :perception t :arguments '((exists boolean %)) :goal '((thirst*goal (exists false))))
  (conditional 'fake :conditions '((thirst (exists true))))
  (evidence '((hunger .3 (exists true)) (hunger .7 (exists false))))
  (d 2)
  (pppfns)
  )