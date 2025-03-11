(in-package :sigma)
; functions for sum product algorithm, queues, message initialization & messages processing 

; Queues
; ------

; Initialize a queue
(defun init-queue (&optional head)
  (let ((qhead (list head)))
    (make-queue :head qhead :tail qhead)
    )
  )

; Is a queue not empty?
(defun queue-empty (q)
  (not (queue-not-empty q))
  )

; Is a queue not empty?
(defun queue-not-empty (q)
  (cdr (queue-head q))
  )

; Length of a queue (without head)
(defun queue-length (q)
  (length (cdr (queue-head q)))
  )

; Messages in depth queues
(defun depth-queues-length nil
  (reduce #'+ (aref (graph-queues cg) depth-queues-index) :key #'queue-length)
  )

; Total messages in queue
(defun length-queues nil
  (+ (depth-queues-length)
     (queue-length (aref (graph-queues cg) other-queue-index))
     )
  )

; Get element off of front of queue
; queue-list will point to the element in front of the one to be removed (head, if the first element)
; (cdr queue-list) is the list starting with the element to be removed.
; (cadr queue-list) is the element to be removed.
; (cddr queue-list), if it exists, is the list that should follow queue-list after removal;
; otherwise queue-list becomes the tail of the new queue
(defun pop-queue (q)
  (let ((queue-list (queue-head q))
        element)
    (when (cdr queue-list) ; There is an item to be removed
      (unless (cddr queue-list) ; Removing the last item from the queue
        (setf (queue-tail q) queue-list))
      (setq element (cadr queue-list))
      (rplacd queue-list (cddr queue-list)))
    element)
  )

; Add an element to the back of a queue
(defun add-to-queue (element q &optional front)
  (let ((new-cdr (list element)))
    (cond (front ; Place at front of queue
           (rplacd new-cdr (cdr (queue-head q)))
           (rplacd (queue-head q) new-cdr)
           )
          (t ; Place at back of queue
           (rplacd (queue-tail q) new-cdr)
           (setf (queue-tail q) new-cdr)
           )
          )
    q)
  )

; Messages
; --------

; Retrieve a message from "from" and "to" node numbers
(defun message-from-numbers (from-num to-num)
  (let* ((from-node (node-from-number from-num))
         (to-node (node-from-number to-num))
         (link (link-from-nodes from-node to-node))
         )
    (if link
        (make-message :index (if (variable-nodep from-node) 0 1)
                      :link link
                      )
      nil)
    )
  )

; Retrieve a message function from "from" and "to" node numbers
(defun message-function-from-numbers (from-num to-num)
  (let ((from-node (node-from-number from-num))
         (to-node (node-from-number to-num))
         )
    (aref (link-contents (link-from-nodes from-node to-node))
          (if (variable-nodep from-node) 0 1))
    )
  )
(defun mffn (from-num to-num)
  (message-function-from-numbers from-num to-num)
  )
; Determine if a message is empty
(defun message-empty (m)
  (plm-empty (aref (link-contents (message-link m)) (message-index m))))

; Determine if a message is full
(defun message-full (m)
  (plm-full (aref (link-contents (message-link m)) (message-index m))))

; Determine if a message is uniform
(defun message-uniform (m)
  (plm-uniform (aref (link-contents (message-link m)) (message-index m))))

; Combine the input messages functionally via op
(defun combine-messages (fn op)
  (let (result vc (nf (node-function fn)) pred)
    (when (and (plm-p nf) ; If there is a function stored in the node, then include it in the combination
               (not (plm-empty nf)) ; Optimization that assumes 0 is the identify element for all of the combination operations
               )
      ; Eliminate lower states when it is part of a FAN for a state predicate
      (when (eq (node-subtype fn) 'combine)
        (setq pred (node-predicate fn))
        )
      (when (and pred (state-predicate pred))
        (if multiagent
            (dotimes (ai (graph-agents cg))
              (delete-lower-states-function pred nf (+ (aref bottom-state ai) 1) ai)
              )
          (delete-lower-states-function pred nf (+ bottom-state 1)))
        )
      (setq result nf)
      )
    (dolist (l (node-links fn))
      (setq vc (link-var-content l))
      (when vc ; Link is incoming to factor node
        (when (eq op 'por) ; Scale incoming message to <=1 if this is a probabilistic or
          (setq vc (scale-to-one-plm vc))
          )
        (if result
            (setq result (combine-plms vc nil result op))
          (setq result vc)
          )
        )
      )
    result)
  )

; Transform a message at a transform factor node
; The function argument here is the name of an executable lisp function to be applied to all regions on message from in-link
(defun transform-message (function in-link out-link &optional normalize)
  (let ((p (aref (link-contents in-link) 0))) ; Incoming PLM
    (setq p (transform-plm function p)) ; Outgoing PLM (with incoming variables)
    (setf (plm-variables p) ; Replace incoming variables with outgoing variables
          (plm-variables (aref (link-contents out-link) 1)))
    (when normalize
      (setq p (normalize-plm p nil t))
      (when trace-transform
        (format trace-stream "Result of transform (normalized): " p)
        )
      )
    p)
  )

; -----------------------------------------------------------
; Compute affine transform of PLM in incoming message

; Remove all slices outside the dimensions min and max
(defun remove-out-of-bound-slices (p d)
  (let* ((type (svariable-type (aref (plm-variables p) d))) ; Dimension structure for d
        (min (stype-min type))
        (max (stype-max type))
        (rank (plm-rank p))
        (sizev (dimension-sizes-v p))
        (needed (init-needed-vector rank sizev t))
        (needed-d (aref needed d))
        )
    (dolist (sl (aref (plm-slices p) d)) ; Process all slices along the dimension
      (when (or (e> (slice-location sl) max)
                (e< (slice-location sl) min)
                )
        (setf (aref needed-d (slice-index sl)) nil)
        )
      )
    (remove-unneeded-slices p needed))
  )

; Compute a vector mapping old dimensions to new for affine transformations
(defun affine-dimensions (rank affines)
  (let ((affine-map (init-vector rank)))
    (dotimes (i rank) ; Initialize map to identity
      (setf (aref affine-map i) i)
      )
    (dolist (aff affines) ; Process affine transforms
      (setf (aref affine-map (affine-from aff)) (affine-to aff))
      )
    affine-map)
  )

; Find the smallest maximal point along the specified dimension
; Assumes that the dimension is independent of the others, so can just maximize over all of them, ending with the one of interest
(defun smallest-maximal-point (p d)
  ; First maximize out all other dimensions
  (dotimes (i (length (plm-variables p)))
    (unless (= i d)
      (setq p (maximize-plm p i))
      )
    )
  ; Lastly maximize out dimension of interest and find smallest maximal point
  (reduce #'min (aref (region-maximals (plm-origin (maximize-plm p d))) d) :key #'car)
  )

; Translate one dimension of a PLM based on an offset specified by another dimension
; Allows offsets to be set on a region-by-region basis
(defun translate-plm (p d o &optional default invert)
  (let ((maximal-plm p)
        ;(variables (plm-variables p))
        )
    (setq maximal-plm (maximize-plm p o))
    (cpt-function-array-plm (variable-names (plm-variables p))
                            (plm-cpt (combine-plms p nil maximal-plm 'translate nil (cons d o) t nil invert))
                            (plm-variables p)
                            (if default default 0)
                            center-discrete-numeric-on-integer
                            t
                            )
    )
  )


; String for printing the offset
(defun offset-print-string (offset)
  (if offset
      (if (and (numberp offset) (< offset 0))
          (format nil " - ~S" (- offset))
        (format nil " + ~S" offset))
    "")
  )

; String for printing an affine transform
(defun affine-print-string (vn coef off apply)
  (cond (apply
         (format nil "~S*(~S~A)" coef vn (offset-print-string off))
         )
        (coef
         (cond ((= coef -1)
                (if off
                    (format nil "~S - ~S" off vn)
                  (format nil "-~S" vn))
                )
               ((and off (< coef 0))
                (format nil "~S - ~S*~S" off (- coef) vn)
                )
               (t
                (if off
                    (format nil "~S*~S~A" coef vn (offset-print-string off))
                  (format nil "~S*~S" coef vn))
                )
               )
         )
        (off
         (format nil "~S~A" vn (offset-print-string off))
         )
        (t
         (format nil "~S" vn)
         )
        )
  )        

; Map a list of dimension numbers
(defun map-dns (dns imapv)
  (let (mds)
    (dolist (d dns)
      (setq mds (cons (position d imapv) mds))
      )
    (nreverse mds))
  )

; Compute affine transform of PLM
; Affines is a list of affine transformations
(defun affine-plm (p affines in-map out-map variables new-vs old-vs &optional invert region-pad)
  (let ((np (copy-a-plm p)) ; Result function
        (rot-out-map (when out-map (make-smap :vfactor (copy-seq (smap-vfactor out-map))))) ; New out-map to change with rotations
        d rot-d in-d out-rot-d temp-d offset coef new-loc type second-slice imapv mapped-dns variable-offset-position
        (discretize (init-vector (length (plm-variables p)))) ; Track which dimensions need to be rediscretized at end
        v-ne ov-ne variable-offset print-offset
        )
    (when trace-affine
      (format trace-stream "~&~%PLM at beginning of affine transform: ") (print-plm p symbolic-trace)
      )
    (when in-map
      (setq imapv (smap-vfactor in-map))
      )
    (dolist (aff affines) ; Process the affine transforms for the corresponding dimensions
      (setq coef (affine-coefficient aff)) ; Coefficient for this dimension
      (when (and coef (= coef 1)) (setq coef nil)) ; Noop
      (setq d (affine-from aff)) ; Affine dimension in factor node
      (setq rot-d (affine-to aff)) ; Dimension to which d is rotated in factor node
      (when invert ; Swap to and from dimensions
        (setq temp-d d) (setq d rot-d) (setq rot-d temp-d)
        )
      (cond ((listp d) ; Incoming variables need to be combined in outgoing message
             (setq mapped-dns (map-dns d imapv))
;             (factor-combined-dimensions p np (if imapv mapped-dns d))
	     (setq np (factor-combined-dimensions p np (if imapv mapped-dns d)))
             (setq np (diagonalize-plm np mapped-dns))
             (when trace-affine
               (format trace-stream "~&~%PLM after combining factor node dimensions ~S (variable node dimensions ~S) and diagonalizing: " d mapped-dns)
               (pplm np)
               )
             (setq d (car d)) ; Use the first dimension to actually do the rotation/swap
             )
            ((listp rot-d) ; Incoming variable needs to be duplicated in outgoing message
             (error "Reuse of variables within a single action or condact not currently allowed.")
             )
            )
      ; in-d is the dimension in the incoming message from the variable node, rather than in the factor node
      (setq in-d d)
      (when in-map
        (setq in-d (position d imapv))
        )
      ; out-rot-d is the dimension that is rotated to in the outgoing message to the variable node, rather than in the factor node
      (setq out-rot-d rot-d)
      (when out-map
        (setq out-rot-d (position rot-d (smap-vfactor out-map)))
        )
      ; Process a rotation for the dimension
      (unless (= d rot-d)
        (setf (aref (plm-variables np) in-d) (aref new-vs out-rot-d))
        (setf (aref (smap-vfactor rot-out-map) out-rot-d) in-d) ; Create map for strip-and-reorder-plm
        )
      (setq offset (affine-offset aff)) ; Offset for this dimension
      (setq print-offset offset)
      (when invert ; If message is coming from evidence (i.e., from WM), invert the offset and coefficient
        (when (and offset (not (symbolp offset))) ;VOLKAN - can't invert if variable offset
          (setq offset (* offset -1)) ; Invert the offset
          )
        (when (and offset (symbolp offset))                 
          (setf (aref (smap-vfactor rot-out-map) (position offset old-vs :key #'svariable-name)) (position offset new-vs :key #'svariable-name))  
          )
        (when coef
          (setq coef (/ 1 coef))
          (when (and offset (not (symbolp offset)) (< coef 0))
            (setq offset (* -1 offset))
            )
          )
        )   
      ; Handle specification of offset by variable (using the smallest maximal point)
      ; (when (and offset (symbolp offset)) ; The offset is a variable name
      ; (setq variable-offset-position (position offset old-vs :key #'svariable-name))
      ; (setq offset (smallest-maximal-point p variable-offset-position)) ; Set offset to smallest maximal
      ; (setq np (maximize-plm np variable-offset-position)) ; Wipe out offset dimension from message

      (when trace-affine
        (setq print-offset (if offset print-offset variable-offset))
        (format trace-stream "~&~%Transform variable: ~S = " (svariable-name (aref variables rot-d)))
        (format trace-stream (affine-print-string (svariable-name (aref old-vs in-d)) coef print-offset (affine-apply-coefficient-to-offset aff)))
        )
      (when (and offset (symbolp offset)) ; The offset is a variable name
        (setq variable-offset offset)
        (setq variable-offset-position (position offset old-vs :key #'svariable-name))
        ; If not applying coefficient to offset, apply coefficient to variable before translating
        ;(unless (affine-apply-coefficient-to-offset aff)
        (when (and coef (not (affine-apply-coefficient-to-offset aff)))
          (when trace-affine
            (format trace-stream "~&~%<<< Begin subsidiary affine transform for coefficient >>>")
            )
         (if invert
              (setq np (affine-plm np (list (make-affine :from (affine-from aff) :to (affine-from aff) :coefficient coef :pad (affine-pad aff)))
                               out-map out-map variables new-vs new-vs invert region-pad)) ; use in-map for out-map and old-vs for new-vs because not yet stripping offset
            (setq np (affine-plm np (list (make-affine :from (affine-from aff) :to (affine-from aff) :coefficient coef :pad (affine-pad aff)))
                              in-map in-map variables old-vs old-vs invert region-pad)) ; use in-map for out-map and old-vs for new-vs because not yet stripping offset
            )
          (when trace-affine
            (format trace-stream "~&~%PLM after ~S = " (svariable-name (aref old-vs in-d)))
            (format trace-stream (affine-print-string (svariable-name (aref old-vs in-d)) coef nil nil))
            (format trace-stream ": ")
            (print-plm np symbolic-trace)
            (format trace-stream "~&~%<<< End subsidiary affine transform for coefficient >>>")
            )
          )
        ; Translate by variable offset
        (setq np (translate-plm np in-d variable-offset-position region-pad invert)) ;translate over in-d rather than rot-d
        (when trace-affine
          (format trace-stream "~&~%PLM after ~S = " (svariable-name (aref old-vs in-d)))
          (format trace-stream (affine-print-string (svariable-name (aref old-vs in-d)) nil print-offset nil))
          (format trace-stream ": ")
          (print-plm np symbolic-trace)
          )
        ; If applying coefficient to offset, apply coefficient after translating
        (when (and coef (affine-apply-coefficient-to-offset aff))
        ;(when (affine-apply-coefficient-to-offset aff)
          (when trace-affine
            (format trace-stream "~&~%<<< Begin subsidiary affine transform for coefficient >>>")
            )
         (if invert
              (setq np (affine-plm np (list (make-affine :from (affine-from aff) :to (affine-from aff) :coefficient coef :pad (affine-pad aff)))
                               out-map out-map variables new-vs new-vs invert region-pad)) ; use in-map for out-map and old-vs for new-vs because not yet stripping offset
            (setq np (affine-plm np (list (make-affine :from (affine-from aff) :to (affine-from aff) :coefficient coef :pad (affine-pad aff)))
                              in-map in-map variables old-vs old-vs invert region-pad)) ; use in-map for out-map and old-vs for new-vs because not yet stripping offset
            )
          (when trace-affine
            (format trace-stream "~&~%PLM after ~S = " (svariable-name (aref old-vs in-d)))
            (format trace-stream (affine-print-string (svariable-name (aref old-vs in-d)) coef nil nil))
            (format trace-stream ": ")
            (print-plm np symbolic-trace)
            (format trace-stream "~&~%<<< End subsidiary affine transform for coefficient >>>")
            )
          )
        (setq np (maximize-plm np variable-offset-position)) ; Wipe out offset dimension from message
        )
      (when (and offset (not (symbolp offset)) (affine-apply-coefficient-to-offset aff))
        (setq offset (* coef offset))
        )
      (setq type (svariable-type (aref new-vs out-rot-d))) ; Type of outgoing variable dimension
      ; Transform the slices for a dimension in the new PLM
      (dolist (s (aref (plm-slices np) in-d))
        ; Transform each slice (assumes already copied so no destructive side effects)
        (setq new-loc (slice-location s))
        (when (and coef (not (and offset (symbolp offset))))
          (setq new-loc (* new-loc coef))
          )
        (when (and offset (not (symbolp offset)))
          (setq new-loc (+ new-loc offset))
          )
        (setf (slice-location s) new-loc)
        )
      (when trace-affine
        (format trace-stream "~&~%PLM after affine transforms: ") (print-plm np symbolic-trace)
        )
      ; When coefficient is negative, reverse the order of the regions along the dimension
      (when (and coef (not (and offset (symbolp offset))) (< coef 0))
        (setq np (reverse-plm np in-d))
        (when trace-affine
          (format trace-stream "~&~%PLM after reversing dimension ~S: " in-d) (print-plm np symbolic-trace)
          )
        )
      (setq second-slice (extreme-slice (aref (plm-slices np) in-d) #'<)) ; Second slice from beginning (is actually first slice??)
      ; Pad front of PLM
      (when (> (slice-location second-slice) (stype-min type))
        (setq np (factor-plm np in-d (make-slice :location (stype-min type)) region-pad))
        (when trace-affine
          (format trace-stream "~&~%PLM after padding front: ") (print-plm np symbolic-trace)
          )
        )
      ; Ensure there is a slice at the type minimum
      (when (< (slice-location second-slice) (stype-min type))
        (setq np (factor-plm np in-d (make-slice :location (stype-min type))))
        (when trace-affine
          (format trace-stream "~&~%PLM after adding min slice: ") (print-plm np symbolic-trace)
          )
        )
      (setq second-slice (extreme-slice (aref (plm-slices np) in-d) #'>)) ; Second slice from end (is actually last slice??)
      ; Pad end of plm
      (when (< (slice-location second-slice) (stype-max type))
        (setq np (factor-plm np in-d (make-slice :location (stype-max type)) region-pad))
        (when trace-affine
          (format trace-stream "~&~%PLM after padding rear: ") (print-plm np symbolic-trace)
          )
        )
      ; Ensure there is a slice at the type maximum
      (when (> (slice-location second-slice) (stype-max type))
        (setq np (factor-plm np in-d (make-slice :location (stype-max type))))
        (when trace-affine
          (format trace-stream "~&~%PLM after adding max slice: ") (print-plm np symbolic-trace)
          )
        )
      (setq np (remove-out-of-bound-slices np in-d))
      (when trace-affine
        (format trace-stream "~&~%PLM after removing out-of-bound slices: ") (print-plm np symbolic-trace)
        )
      ; If dimension is discrete and there is a non-integral coefficient, rediscretize along the dimension
      (when (and (stype-discrete type) coef (not (integerp coef)))
        (setf (aref discretize rot-d) t)
        )
      )
    ; Take care of any within predicate not-equal (<>) tests
    (dolist (aff affines)
      (when (affine-not-equal aff)
        (setq v-ne (position (affine-from aff) (smap-vfactor out-map))) ; Variable from which to remove not-equals
        (setq ov-ne (position (affine-not-equal aff) (smap-vfactor out-map))) ; Variable to which v-ne is not equal
        (setq np (inverse-diagonalize-plm np (list v-ne ov-ne)))
        (when trace-affine
          (format trace-stream "~&~%PLM after not-equal test (~S <> ~S): " (svariable-name (aref new-vs v-ne)) (svariable-name (aref new-vs ov-ne)))
          (print-plm np symbolic-trace)
          )
        )
      )
;    (setq np (remove-unneeded-slices np))
    (when trace-affine
      (format trace-stream "~&~%PLM after removing unneeded slices: ") (print-plm np symbolic-trace)
      )
    (dotimes (i (length discretize))
      (when (aref discretize i)
        (setq np (discretize-dimension np i))
        (when trace-affine
          (format trace-stream "~&~%PLM after rediscretizing slices for dimension ~S: " i) (print-plm np symbolic-trace)
          )
        )
      )
      (setq np (strip-and-reorder-plm rot-out-map new-vs np))
    (when (plm-piecewise-constant p)
      (setf (plm-piecewise-constant np) t)
      )
    np)
  )

; Compute special case of affine transform for ADF delta nodes
; Only changes variables
; Ignores actual affines, just paying attention to mapping from variable node variables to factor node variables
(defun affine-delta-plm (p variables mapv)
  (let* ((rank (plm-rank p))
         (np (make-plm :rank rank
                       :active (copy-seq (plm-active p))
                       :slices (plm-slices p)
                       :array (plm-array p)
                       :piecewise-constant (plm-piecewise-constant p)
                       :removed-unneeded-slices (plm-removed-unneeded-slices p)))
         (nvs (init-vector rank))
         )
    (dotimes (i rank)
      (setf (aref nvs i) (aref variables (aref mapv i)))
    )
    (setf (plm-variables np) nvs)
    np)
  )

;------------------------------------------------
;           combine and summarize PLMs
;------------------------------------------------

; Compute pointwise combination (product, sum, difference or max) of two regions and assign as function of new region (rnew)
; For product, computes quadratic product and then approximates as linear using
; slopes evaluated at three points in new region (min, mid, max)
; map determines how the dimensions of r1 match up with r2.
(defun combine-regions (p1 r1 map p2 r2 rnew np op &optional evidence argument pc1 pc2 piecewise-constant invert)
  (let* ((r2-rank (region-rank r2))
         wv ; New function
         (mapv (if map (smap-vfactor map) nil))
         (r1-f (region-weights r1 r2-rank mapv piecewise-constant)) ; Function in first region
         (r2-f (region-weights r2 r2-rank nil piecewise-constant)) ; Function in second region
         )
    (case op
      ((sum) ; Compute sum of two non-empty regions
       (setq wv (vector-sum r1-f r2-f piecewise-constant))
       )
      ((difference) ; Compute difference of two regions
       (setq wv (vector-difference r1-f r2-f piecewise-constant))
       )
      ((product) ; Compute products of two non-empty regions
       (setq wv (product-functions (region-weights r1 r2-rank mapv pc1) (region-weights r2 r2-rank nil pc2) r2-rank rnew pc1 pc2))
       )
      ((max) ; Compute max of two regions
       (setq wv (max-functions r1-f r2-f rnew piecewise-constant))
       )
      ((por) ; Compute probabilistic or of two regions
       (setq wv (por-functions r1-f r2-f r2-rank rnew piecewise-constant))
       )
      ((divide) ; Divide regions in first PLM by constants in second (doesn't handle full functions in second)
       (setq wv (divide-functions r1-f r2-f piecewise-constant))
       )
      ((divide-0) ; Divide regions in first PLM by constants in second (doesn't handle full functions in second), returning 0 if second is 0
       (setq wv (divide-0-functions r1-f r2-f piecewise-constant))
       )
      ((gdl-scale) ; Scale regions in second PLM by constants in first (doesn't handle full functions in second, and extracts constant from first)
       (cond (piecewise-constant
              (setq wv (gdl-scale (if (numberp r1-f) r1-f (aref r1-f 0)) (if (numberp r2-f) r2-f (aref r2-f 0))))
              )
             (t
              (setq wv (init-vector (length r1-f) 0))
              (setf (aref wv 0) (gdl-scale (if (numberp r1-f) r1-f (aref r1-f 0)) (if (numberp r2-f) r2-f (aref r2-f 0))))
              wv
              )
             )
       )
      ; *** Paul's new addition
      ((inner-bc) ; Within-integral aspect of Bhattacharyya-coefficient
      (setq wv (inner-bc-constants r1-f r2-f))
       )
      ((constrain-by-0) ; Constrain first region to 0 when second is
       (setq wv (constrain-by-0-function r1-f r2-f piecewise-constant))
       )
      ((compute-subtractor) ; Compute subtractor for subtractive normalization
       (setq wv (compute-subtractor r1-f r2-f piecewise-constant))
       )
      ; *** Paul's new addition
      ((translate) ; Translate region in first PLM by offset from second
       (let (dimension offset d type)
         (setq wv r1-f) ; The first argument is the original PLM
         (setq dimension (car argument)) ; (car argument) is the dimension along which to translate
         (setq offset (reduce #'min (aref (region-maximals r2) (cdr argument)) :key #'car)) ; (cdr argument) is the offset dimension
         (when center-discrete-numeric-on-integer
           (setq offset (+ offset 1/2))
           )
         (if invert (setq offset (* -1 offset))) ;VOLKAN
         (setq d (aref (region-dimensions rnew) dimension)) ; Region dimension along which to offset
         (setq type (svariable-type (aref (plm-variables p1) dimension))) ; Type of variable along dimension to offset
         (setf (dimension-min-slice d) (make-slice :location (max (stype-min type) (+ (dimension-min d) offset))))
         (setf (dimension-max-slice d) (make-slice :location (min (stype-max type) (+ (dimension-max d) offset))))
         (when (or (e>= (dimension-min d) (stype-max type))
                   (e<= (dimension-max d) (stype-min type))
                   )
           (setf (region-bad rnew) t)
           )
         )
       )
      (t (error "Unknown combination operator for PLMs: ~S" op))
      )
    (assign-function wv rnew piecewise-constant)
    ; Propagate region evidence to new region
    (when evidence
      (if (or (region-evidence r1) (region-evidence r2))
          (setf (region-evidence rnew) t)
        (setf (region-evidence rnew) nil))
      )
    (when (eq trace-combine 'region)
      (format trace-stream "~&~%R1: ") (print-region r1 p1 t trace-stream)
      (format trace-stream "~&R2: ") (print-region r2 p2 t trace-stream)
      (format trace-stream "~&RNEW: ") (print-region rnew np t trace-stream)
      )
    wv) ; Return the vector for debugging purposes
  )

; Create a new PLM that is the combination of two others
; op specifies which operation to perform
; The first plm may contain a subset of the variables of the second one
; If map exists, determines how to map variables of first PLM onto second
; Destructive-p1 is used to allow normalization (by division by p2) to be destructive
(defun combine-plms (p1 map p2 op &optional evidence argument keep-unneeded-slices destructive-p1 invert)
  (let* (np p1rarray nprarray p2rarray
            civ ; Slice cross-index vector
            nprank nr
            p1sizeone
            p1-origin
            cv ; Current index vector
            sizev-1 ; Dimension sizes -1
;            non-empty
            pc1
            pc2
            px
            piecewise-constant
            non-empty1
            non-empty2
            density-plm1
            density-plm2
            )
    (unless (plm-type-check p1 map p2)
      (error "Attempt to ~S two PLMs not of the same type: " op) (pplm p1) (pplm p2)
      )
    ; Under the right circumstances, swap p1 and p2
    (when (and (eq op 'product)
               (null map)
               (= (plm-rank p1) (plm-rank p2))
               (> (plm-size p1) (plm-size p2))
               )
      (setq px p1)
      (setq p1 p2)
      (setq p2 px)
      )
    (setq p1sizeone (= (plm-size p1) 1))
    (setq p1-origin (plm-origin p1))
    (setq pc1 (plm-piecewise-constant p1))
    (setq pc2 (plm-piecewise-constant p2))
    (setq piecewise-constant (and pc1 pc2))
    (cond ((and (eq op 'product) ; This is a product, and one constant region covers entire domain (or there is no domain because no variables)
                p1sizeone
                (region-is-constant p1-origin)
                )
           (setq np (if (zerop (region-constant p1-origin))
                        (empty-plm (plm-variables p2))
                      (if (= (region-constant p1-origin) 1)
                          p2
;                      (remove-unneeded-slices
                        (transform-plm #'scale-function p2 (region-constant p1-origin))
;                       )
                      )))
           )
          ((eq op 'product)
;           ((and sparse-product
;                 (setq non-empty (check-for-sparse-product p1 op))
;                 )
           (when trace-combine
             (format trace-stream "~&~%PRODUCT(SPARSE):")      
             (format trace-stream "~&~%P1: ") (print-plm p1 symbolic-trace trace-stream)
             (when map
               (format trace-stream "~&~%MAP: vfactor: ~S fvar: ~S omitted: ~S"
                       (smap-vfactor map) (smap-fvar map) (smap-omitted map)
                       )
               )
             (format trace-stream "~&~%P2: ") (print-plm p2 symbolic-trace trace-stream)
             )
           (multiple-value-setq (non-empty1 density-plm1) (plm-non-empty p1))  ;VOLKAN Sparse_Update - New function
           (multiple-value-setq (non-empty2 density-plm2) (plm-non-empty p2)) ;VOLKAN Sparse_Update - New function
           (if (< density-plm1 density-plm2)  ;VOLKAN Sparse_Update - New function
               (setq np (sparse-product-plms p1
;                                         non-empty
                                         ;(plm-non-empty p1)
                                         non-empty1
                                         map p2 evidence argument))
             (setq np (sparse-product-plms-reverse p1  ;VOLKAN Sparse_Update - New function
                                         non-empty2
                                         map p2 evidence argument))
             )
           (when trace-combine
             (format trace-stream "~&~%~S RESULT: " op) (print-plm np symbolic-trace trace-stream)
             )
           np)
          ((and destructive-p1 (member op '(divide divide-0)))
           (setq civ (cross-index-slice-vector (plm-slices p2) nil (plm-slices p1)))
           (setq np p1)
           (setq nprarray (plm-array np))
           (setq nprank (plm-rank np))
           (setq cv (init-vector nprank 0))
           (setq sizev-1 (vector-1 (array-dimensions-v nprarray)))
           (setq p2rarray (plm-array p2))
           (dotimes (i (array-total-size nprarray))
             (setq nr (row-major-aref nprarray i))
             (combine-regions np nr nil p2 (apply #'aref p2rarray (index-list-from-cross-index cv civ nil))
                              nr np op evidence argument pc1 pc2 piecewise-constant invert)
             (setq cv (next-index-vector cv sizev-1 nprank))
             )
           (setq np p1)
           )
          (t
           (when trace-combine
             (format trace-stream "~&~%~S:" op)
             (format trace-stream "~&~%P1: ") (print-plm p1 symbolic-trace trace-stream)
             (when map
               (format trace-stream "~&~%MAP: vfactor: ~S fvar: ~S omitted: ~S"
                       (smap-vfactor map) (smap-fvar map) (smap-omitted map)
                       )
               )
             (format trace-stream "~&~%P2: ") (print-plm p2 symbolic-trace trace-stream)
             )
           (setq p1rarray (plm-array p1))
           (setq np (if p1sizeone (copy-a-plm p2) (apply-slices (plm-slices p1) map (copy-a-plm p2))))
           (update-combine-active (plm-active p1) map (plm-active p2) (plm-active np)) ; Paul's new addition
           (setq nprarray (plm-array np))
           (unless p1sizeone
             (setq civ (cross-index-slice-vector (plm-slices p1) map (plm-slices np)))
             (setq nprank (plm-rank np))
             (setq cv (init-vector nprank 0))
             (setq sizev-1 (vector-1 (array-dimensions-v nprarray)))
             )
           (dotimes (i (array-total-size nprarray))
             (combine-regions p1 (if p1sizeone
                                     p1-origin
                                   (apply #'aref p1rarray (index-list-from-cross-index cv civ map)))
                              map np (row-major-aref nprarray i) (row-major-aref nprarray i) np op evidence argument pc1 pc2 piecewise-constant invert)
             (unless p1sizeone
               (setq cv (next-index-vector cv sizev-1 nprank))
               )
             )
           (unless keep-unneeded-slices
;             (setq np (remove-unneeded-slices np))
             )
           (when trace-combine
             (format trace-stream "~&~%~S RESULT: " op) (print-plm np symbolic-trace trace-stream)
             )
           np)
        )
    (setf (plm-piecewise-constant np) piecewise-constant)
    np)
  )

; Debug function for multiplying two plms with same variables
(defun product-plms (p1 p2)
  (combine-plms p1 nil p2 'product)
  )

; Summarize over one dimension of a region
(defun summarize-region (r ds op &optional argument piecewise-constant vector)
  (case op
    ((integral)
     (integral-region r ds piecewise-constant vector)) 
    ((extract-value)
     (extract-value-region r ds argument piecewise-constant))
    ((span-above-threshold)
     (span-above-threshold-region r ds argument piecewise-constant))
    ((volume)
     (region-volume-function r ds piecewise-constant)
     )
    (t (error "Unknown region summarization operator for a PLM: ~S" op))
    )
  )

; Summarize over one dimension of a PLM, yielding a new PLM
; region-op specifies how to summarize over a region, while combine-op says how to combine summarizations across regions
; Doesn't actually eliminate the dimension from the PLM, but makes it inactive
; If there is a region-argument, provide it to the region-op
(defun summarize-plm (p din region-op combine-op &optional region-argument vector) 
  (let* ((ds (if (consp din) din (list din)))
         (np (init-summarize-plm p ds))
         (rarray (plm-array p))
         (rarrayn (plm-array np))
         (rank (plm-rank p))
         (sizev (dimension-sizes-v p))
         (sizev-1 (vector-1 sizev))
         (size (array-total-size rarray))
         (sizeln (dimension-sizes np))
         (sizen (array-total-size rarrayn))
         (summaries (make-array sizeln))
         r ; Region
         rsummary ; Region integral
         ii ii0
         (piecewise-constant (plm-piecewise-constant p))
         )
    (when trace-summarize
      (format trace-stream "~&~%~S" region-op)
      (format trace-stream "~&~%P: ") (print-plm p symbolic-trace trace-stream)
      (format trace-stream "~&~%~S DIMENSION(S): ~S" region-op ds)
      )
    ; Compute the summazation over the regions in the PLM as function vectors in the variable (vector) summaries
    (setq ii (make-list rank :initial-element 0))
    (dotimes (i size)
      ; Zero the index along ds
      (setq ii0 (copy-seq ii))
      (dolist (d ds)
        (setf (nth d ii0) 0)
        )
      (setf r (row-major-aref rarray i))
      (setq rsummary (summarize-region r ds region-op region-argument piecewise-constant vector)) 
      (setf (apply #'aref summaries ii0) (if (apply #'aref summaries ii0) (apply combine-op rsummary (list (apply #'aref summaries ii0) piecewise-constant)) rsummary))
      (setq ii (next-index-list ii sizev-1 rank))
      )
    ; Assign functions to new regions
    (dotimes (i sizen)
      (assign-function (row-major-aref summaries i) (row-major-aref rarrayn i) piecewise-constant)
      )
    ; Make summarised dimensions inactive
    (dolist (d ds)
      (setf (aref (plm-active np) d) nil)
      )
    (when trace-summarize
      (format trace-stream "~&~%~S RESULT: " region-op) (print-plm np symbolic-trace trace-stream)
      )
    (when (plm-piecewise-constant p)
      (setf (plm-piecewise-constant np) t)
      )
    np)
  )

; Determine if a vector is sorted from smallest to largest
(defun in-order (v)
  (let ((in-order t))
    (dotimes (i (1- (length v)))
      (unless (< (aref v i) (aref v (1+ i)))
        (return (setq in-order nil))
        )
      )
    in-order)
  )

; Compute affine transform of PLM in incoming message
; Affines is a list of affine transformations
(defun affine-message (affines in-link out-link variables &optional other-in-link) ;optional added by VOLKAN
  (let ((in-link-var-content (link-var-content in-link)) ;VOLKAN
        (invert  (member (link-var-node in-link) ; Source variable node for message
                            (node-evidence (link-fact-node in-link)) ; Transform factor node
                            ))
        temp-plm
        temp-map
        )
    (when trace-affine
      (format trace-stream "~&~%~% *** Affine transform at node ~S ***" (node-name (aref (link-nodes in-link) 1)))
      )
    (if (and other-in-link invert (eq (nth 0 (array-dimensions (plm-array (link-var-content other-in-link)))) 1))
        in-link-var-content
      (progn
        (when (and invert other-in-link)      
          ; VOLKAN NEXT LINE TRIES TO EXTRACT THE OFFSET VARIABLE AS THE LAST VARIABLE TO BE USED IN THE OUTER PRODUCT: This is an assumption, need to find a way to extract the offset variable
          (setf temp-plm (full-plm  (coerce (list (aref (plm-variables (link-var-content other-in-link)) (1- (length (plm-variables (link-var-content other-in-link)))) )) 'vector)))
          (setf temp-plm (outer-plms in-link-var-content temp-plm 'product))
          (setf temp-map (build-smap  (plm-variables (link-var-content other-in-link)) (plm-variables temp-plm)))
          (setf in-link-var-content (combine-plms (link-var-content other-in-link)  temp-map temp-plm 'product))
          )
    
  ; Check if can use optimized implementation at delta nodes (no variable mapping in either direction and same number of variables)
        (if (and (eq (node-subsubtype (aref (link-nodes in-link) fact-index)) 'match)
                 (in-order (smap-vfactor (link-map out-link)))
                 (in-order (smap-vfactor (link-map in-link)))
                 (= (length (node-variables (aref (link-nodes in-link) var-index)))
                    (length (node-variables (aref (link-nodes out-link) var-index))))
                 )
            (affine-delta-plm in-link-var-content variables (smap-vfactor (link-map out-link))) ;in-link-var-content VOLKAN
          (affine-plm in-link-var-content affines
                    ;(if other-in-link (link-map out-link) (link-map in-link)) (link-map out-link)
                      (link-map in-link) (link-map out-link)
                      variables (node-variables (aref (link-nodes out-link) var-index)) (plm-variables in-link-var-content);(node-variables (aref (link-nodes in-link) var-index))
              ; If message is coming from evidence (i.e., from WM), invert transform
                      invert
                    ;(member (link-var-node in-link) ; Source variable node for message
                    ;        (node-evidence (link-fact-node in-link)) ; Transform factor node
                     ;       )
                      (node-region-pad (link-fact-node in-link)))
          )
        )
      )
    )
  )

; Return maximum value in region
(defun maximum-in-region (r)
  (let (d+1
        (f (extract-function r))
        (max (region-constant r)))
    (dotimes (d (region-rank r))
      (setq d+1 (1+ d))
      (when (numberp (aref f d+1))
        (unless (zerop (aref f d+1))
          (setq max (+ max (* (aref f d+1)
                              (if (< (aref f d+1) 0)
                                  (region-min r d)
                                (- (region-max r d)
                                   (if (region-discrete r d) 1 epsilon2)))))) ; Region open at top
          )
        )
      )
    max)
  )

; Return maximum value in PLM
(defun maximum-in-plm (plm)
  (let ((max 0)
        val
        (rarray (plm-array plm)))
    (dotimes (i (array-total-size rarray))
      (setq val (maximum-in-region (row-major-aref rarray i)))
      (when (> val max) (setq max val))
      )
    max)
  )

; Transform a PLM so that if largest functional value is >1, the PLM is uniformaly scaled to be <=1
(defun scale-to-one-plm (plm)
  (let ((max plm)
        (new plm))
#|    (dotimes (i (length (plm-variables plm)))
      (setq max (maximize-plm max i))
      )
    (setq max (region-constant (plm-origin max)))|#
    (setq max (maximum-in-plm plm))
    (when (> max 1)
        (setq new (transform-plm #'scale-function plm (/ 1.0 max)))
        )
    new)
  )


; -----------------------------------------------------------
; Compute the outer combination of two PLMs

; Create a PLM in which to store the product of two PLMs (via an array)
; This function generates all of the regions but doesn't set the functions
; The variables in the first PLM are always a (not necessarily proper) subset
; of those in the second
; The product is always as big as the second PLM
(defun init-outer-plm (p1 p2)
  (let* (nv ; New variable vector
         na ; New active vector
         ns ; New slice vector
         (v1 (plm-variables p1))
         (v2 (plm-variables p2))
         (a1 (plm-active p1))
         (a2 (plm-active p2))
         (s1 (plm-slices p1))
         (s2 (plm-slices p2))
         (r1 (plm-rank p1))
         (r2 (plm-rank p2))
         (nr (+ r1 r2))
         ir
         )
    (setq nv (init-vector nr))
    (setq na (init-vector nr))
    (setq ns (init-vector nr))
    (dotimes (i r1) ; Copy variables and active from p1
      (setf (aref nv i) (aref v1 i))
      (setf (aref na i) (aref a1 i))
      (setf (aref ns i) (aref s1 i))
      )
    (dotimes (i r2) ; Copy variables and active from p2
      (setq ir (+ i r1))
      (setf (aref nv ir) (aref v2 i))
      (setf (aref na ir) (aref a2 i))
      (setf (aref ns ir) (aref s2 i))
      )
    (init-plm-with-slices nv 0 0 na ns)
    )
  )

; Generate a weight vector (with the constant oin the 0 position and everything else shifted over) for an outer product
(defun outer-region-weights (r rank offset)
  (let ((vf (init-vector (+ rank 1) 0)))
    (setf (aref vf 0) (region-constant r))
    (dotimes (d (region-rank r))
      (setf (aref vf (+ d offset)) (region-weight r d))
      )
    vf)
  )

; Compute outer combination (product) of two regions and assign as function of new region (rnew)
(defun outer-combine-regions (p1 r1 p2 r2 rnew np op)
  (let* ((nr-rank (region-rank rnew))
         (wv (init-vector (+ nr-rank 1) 0)) ; 0th element is the constant
         wps
         )
    (cond ((eq op 'product) ; Compute products of two non-empty regions
           ; Multiply two linear functions to achieve a quadratic function via outer product
           (setq wps (outer-product (outer-region-weights r1 nr-rank 1)
                                    (outer-region-weights r2 nr-rank (+ (region-rank r1) 1))))
           ; Compute slopes for the variables
           (dotimes (d nr-rank)
             (setf (aref wv (+ d 1)) (slope-approximation-points wps (region-mins rnew) (region-mids rnew) (region-maxs rnew) (+ d 1)))
             )
           ; Compute constant
           (setf (aref wv 0) 0) ; Baseline for computing constant
           (setf (aref wv 0) (constant-approximation-points wps wv (region-mins rnew) (region-mids rnew) (region-maxs rnew)))
           )
          (t (error "Unknown outer combination operator for PLMs: ~S" op))
          )
    ; Assign weights to new region
    (assign-function wv rnew)
    (when (eq trace-combine 'region)
      (format trace-stream "~&~%R1: ") (print-region r1 p1 t trace-stream)
      (format trace-stream "~&R2: ") (print-region r2 p2 t trace-stream)
      (format trace-stream "~&RNEW: ") (print-region rnew np t trace-stream)
      )
    wv) ; Return the vector for debugging purposes
  )

; Create a new PLM that is the outer combination of two others
; op specifies which operation to perform
; The first plm may contain a subset of the variables of the second one
; If map exists, determines how to map variables of first PLM onto second
; Recursive version
(defun outer-plms (p1 p2 op)
  (let* ((np (init-outer-plm p1 p2))
         (rank1 (plm-rank p1)) (rank2 (plm-rank p2))
         r1 r2 rn
         (rarray1 (plm-array p1)) (rarray2 (plm-array p2)) (rarrayn (plm-array np))
         (sizev1 (dimension-sizes-v p1)) (sizev2 (dimension-sizes-v p2))
         (sizev1-1 (vector-1 sizev1)) (sizev2-1 (vector-1 sizev2))
         indices1 indices2 indicesn ; Lists of indexs for the three PLMs
         (total-size1 (array-total-size rarray1)) (total-size2 (array-total-size rarray2))
         )
    (when trace-combine
      (format trace-stream "~&~%~S:" op)
      (format trace-stream "~&~%P1: ") (print-plm p1 symbolic-trace trace-stream)
      (format trace-stream "~&~%P2: ") (print-plm p2 symbolic-trace trace-stream)
      )
    (setq indices1 (make-list rank1 :initial-element 0))
    (dotimes (i1 total-size1)
      (setq r1 (apply #'aref rarray1 indices1))
      (setq indices2 (make-list rank2 :initial-element 0))
      (dotimes (i2 total-size2)
        (setq r2 (apply #'aref rarray2 indices2))
        (setq indicesn (append indices1 indices2))
        (setq rn (apply #'aref rarrayn indicesn))
        (outer-combine-regions p1 r1 p2 r2 rn np op) ; Combine the regions
        (setq indices2 (next-index-list indices2 sizev2-1 rank2))
        )
      (setq indices1 (next-index-list indices1 sizev1-1 rank1))
      )
    (when trace-combine
      (format trace-stream "~&~%~S RESULT: " op) (print-plm np symbolic-trace trace-stream)
      )
    (when (and (plm-piecewise-constant p1) (plm-piecewise-constant p2))
      (setf (plm-piecewise-constant np) t)
      )
    np)
  )

; Debug function for multiplying two plms with same variables
(defun product-outer-plms (p1 p2)
  (outer-plms p1 p2 'product)
  )

; -----------------------------------------------------------
; Summarize across one dimension of a PLM
; This only handles cases where you simply aggregate summarizations of the regions
; It doesn't handle more complicated cases where need to retain other results across the summarization, such as maximals

; Create an empty PLM with all of the regions needed for the summarization over multiple dimensions (via arrays)
; If one dimension is provided as a number, convert it to a list
(defun init-summarize-plm (p ds)
  (when (numberp ds) (setq ds (list ds)))
  (let ((slices (copy-seq (plm-slices p))))
    (dolist (d ds)
      (setf (aref slices d) (list (make-slice :location (stype-min (svariable-type (aref (plm-variables p) d))) :index 0) ; Dimension of summarizaiton only has extreme slices
                                  (make-slice :location (stype-max (svariable-type (aref (plm-variables p) d))) :index 1)
                                  ))
      )
    (init-plm-with-slices (plm-variables p) 0 0 (copy-seq (plm-active p)) slices))
  )

; Convert region volume into a function for use in summarize-region
(defun region-volume-function (r ds &optional piecewise-constant)
  (if piecewise-constant
      (region-volume-ds r ds)
    (let ((rw (init-vector (1+ (region-rank r)) 0)))
      (setf (aref rw 0) (region-volume-ds r ds))
      rw))
  )

; -----------------------------------------------------------
; Compute integral across one dimension of a PLM

; For a linear function, compute the function obtained by the value of an integral along dimension d at point n of region r
; n is a (1D) vaue along dimension d
(defun integral-point-linear (r d n)
  (let* ((rw (extract-function r))
         (lrw (length rw))
         (rwnew (init-vector lrw))
         (d1 (+ d 1))
         )
    ; Multiply all weights by the value along the dimension being integrated
    ; The weight for the integrated dimension is not correct, but reset later
    (dotimes (i lrw)
      (setf (aref rwnew i) (* n (aref rw i)))
      )
    ; Instantiate dimension d and move over to part of constant
    ; The weight for it is currently the original weight times the value
    ; Need to multiply by the value once more (for x^2) and divide by 2
    (setf (aref rwnew 0) (+ (aref rwnew 0) (/ (* n (aref rwnew d1)) 2)))
    (setf (aref rwnew d1) 0)
    rwnew)
  )

; For an exponential function, compute the function obtained by the value of an integral along dimension d at point n of region r
; n is a (1D) value along dimension d
(defun integral-point-exponential (r d n)
  (let* ((rw (extract-function r))
         (lrw (length rw))
         (rwnew (init-vector lrw 0))
         (d1 (+ d 1))
         (dw (aref rw d1)) ; coefficient for dimension d
         )
    ; Copy old weights into new weight vector
; Deleted because we're assuming the other weights are all 0 (and depend on this below)
;    (dotimes (i lrw)
;      (setf (aref rwnew i) (aref rw i))
;      )
    ; Update constant
;    (setf (aref rwnew 0)
;          (+ (aref rwnew 0)
;             (if (e= dw 0) ; If weight for dimension is 0
;                 (if (e= n 0) ; If point is 0
;                     (- infinity) ; Set "log" to negative infinity
;                   (log n) ; Integral is original function times variable value (n), so add log(n) to constant
;                   )
;               (+ (log (/ 1 dw)) (* dw n))))) ; Otherwise, add weight times point plus log of one over weight
;    (setf (aref rwnew d1) 0)
    ; The following only works when there are no weights on dimensions other than on the one being integrated
    ; We're returning an an unexponentiated function with just a constant
    (setf (aref rwnew 0)
          (* (exp (aref rw 0))
             (if (e= dw 0) ; If weight for dimension is 0
                 n
               (* (/ 1 dw) (exp (* dw n))))
             ))
    rwnew)
  )

; Compute the integral over dimension d for region r
; Yields the definite integral from the minimum of d to the maximum of d
(defun integral-region (r ds &optional piecewise-constant vector) 
  (let ((rw (extract-function r piecewise-constant)))
    (if piecewise-constant ; Function is constant
       (if vector 
           (dolist (d ds)
             (setf rw (* (expt rw 2) (- (region-max r d) (region-min r d)))) 
            )
          (if (region-exponential r) ; Function is exponential
              (dolist (d ds)
                (setq rw (* (exp rw) (- (region-max r d) (region-min r d))))
                )
            (if (zerop rw) ; Function is 0
                (setq rw 0)
              (dolist (d ds)
                (setq rw
                      (if (= rw 1) ; Function is 1
                          (- (region-max r d) (region-min r d))
                        (* rw (- (region-max r d) (region-min r d)))))
                ))) 
          )
      (if (region-exponential r)
          (dolist (d ds)
            (setq rw (vector-difference (integral-point-exponential r d (region-max r d)) ; Compute definite integral
                                        (integral-point-exponential r d (region-min r d))))
            )
        (dolist (d ds)
          (setq rw (vector-difference (integral-point-linear r d (region-max r d)) ; Compute definite integral
                                      (integral-point-linear r d (region-min r d)))))
        )
      )
    rw)
  )

; Integrate over one dimension of a PLM, yielding a new PLM
(defun integral-plm (p ds &optional vector) 
  (if vector (transform-plm #'sqrt-function (summarize-plm p ds 'integral #'vector-sum nil vector)) (summarize-plm p ds 'integral #'vector-sum)) 
)

; -----------------------------------------------------------
; Compute (approximate) maximization across one dimension of a PLM

; Returns true no matter what the argument
(defun arg-true (dummy)
  (or dummy (not dummy)))

; Return maximum function for a region r along a dimension d
(defun maximize-region (r d &optional piecewise-constant)
  (if piecewise-constant
      (region-constant r)
    (let* ((d+1 (+ d 1))
           (f-new (extract-function r))
           )
      ; Add weighted value for d to constant and set d's weight to 0
      ; Use smallest value in region if weight is negative, otherwise use largest
      ; If use largest subtract 1 for discrete variables and epsilon2 for continuous
      (when (numberp (aref f-new d+1))
        (unless (zerop (aref f-new d+1))
          (setf (aref f-new 0) (+ (aref f-new 0) (* (aref f-new d+1)
                                                    (if (< (aref f-new d+1) 0)
                                                        (region-min r d)
                                                      (- (region-max r d)
                                                         (if (region-discrete r d) 1 epsilon2)))))) ; Region open at top
          (setf (aref f-new d+1) 0)
          )
        )
      f-new))
  )

; Return element(s) of dimension d in region r that generate(s) the maximum function value
(defun maximal-elements-region (r d)
  (let ((w (region-weight r d)))
    (cond ((or (not (numberp w)) ; If there is no weight on the dimension (so function does not vary over d)
               (e= w 0)) ; or the weight is (epsilon equal to) 0
           (list (region-min r d) (region-max r d))) ; The entire region is maximal (f doesn't depend on d)
          ((> w 0) ; Weight is positive (return a small region just below maximum, since top of region is open)
           (list (- (region-max r d) (if (region-discrete r d) 1 epsilon2))
                 (region-max r d))) ; Highest value is at high end
          (t ; Weight is negative (return small region starting at minimum, since bottom of region is closed)
           (list (region-min r d) (+ (region-min r d) (if (region-discrete r d) 1 epsilon2)))) ; Highest value is at small end
          )
    )
  )

; Compute weighted average of two numbers
(defun average (n1 n2 w1 w2)
  (when (equal (+ w1 w2) 0)
    (error "~&Trying to compute weighted average with 0 weights")
    )
  (/ (+ (* w1 n1) (* w2 n2)) (+ w1 w2)))

; Compute weighted average of two vectors
(defun vector-average (v1 v2 w1 w2)
  (let* ((rank (length v1))
         (va (init-vector rank))
         )
    (dotimes (i rank)
      (setf (aref va i) (average (aref v1 i) (aref v2 i) w1 w2))
      )
    va)
  )

; Determine which function, if either, dominates a region
; Find extreme points for each function and compare
(defun dominates-region (f1 f2 r &optional piecewise-constant)
  (if piecewise-constant
      (if (> f1 f2)
          f1
        (if (> f2 f1)
            f2
          nil))
    (let* ((minp1 (region-mins r)) ; Maximum point in region along all dimensions
           (maxp1 (region-maxs r)) ; Minimum point in region along all dimensions
           (minp2 minp1) ; Copies
           (maxp2 maxp1)
           d+1 temp f1min f1max f2min f2max
           )
      (if (and (function-constantp f1) (function-constantp f2)) ; Special case where both functions are constant
          (cond ((> (aref f1 0) (aref f2 0)) f1)
                ((> (aref f2 0) (aref f1 0)) f2)
                (t nil)
                )
        (progn
          (dotimes (d (region-rank r))
            (setq d+1 (1+ d))
      ; If the coefficient of a dimension is negative, swap the min and max points along that dimension
            (when (< (aref f1 d+1) 0)
              (setq temp (aref minp1 d+1))
              (setf (aref minp1 d+1) (aref maxp1 d+1))
              (setf (aref minp1 d+1) temp)
              )
            (when (< (aref f2 d+1) 0)
              (setq temp (aref minp2 d+1))
              (setf (aref minp2 d+1) (aref maxp2 d+1))
              (setf (aref minp2 d+1) temp)
              )
            (setq f1min (linear-value f1 minp1))
            (setq f1max (linear-value f1 maxp1))
            (setq f2min (linear-value f2 minp2))
            (setq f2max (linear-value f2 maxp2))
            )
          (cond ((or (and (e> f1min f2min)
                          (e>= f1max f2max)
                          )
                     (and (e>= f1min f2min)
                          (e> f1max f2max)
                          )
                     )
                 f1)
                ((or (and (e> f2min f1min)
                          (e>= f2max f1max)
                          )
                     (and (e>= f2min f1min)
                          (e> f2max f1max)
                          )
                     )
                 f2)
                (t nil)
                )
          ))
      ))
  )

; Compute the (approximate) maximization over two regions
(defun maximum-region (rnew fnew fold d nr extents ei &optional omit-maximals prev-max-ds piecewise-constant)
  (let ((maxf fold) ; Cumulate maximum function
        dominates ; Result of checking region in iteration against cumulative for domination
        rnew-extent ; Extent of new region along d
        )
    (setq dominates (dominates-region fnew fold rnew piecewise-constant))
    (cond ((eq dominates fnew) ; If function for region dominates current max
           (setq maxf fnew) ; Set maximum function to function for region
           (unless omit-maximals
             (setf (aref (region-maximals nr) d)
                   (list (maximal-elements-region rnew d))) ; Set dimension's maximal element to that of the region
             )
           ; Copy maximals for previous maximized dimensions
           (dolist (md prev-max-ds)
             (setf (aref (region-maximals nr) md) (aref (region-maximals rnew) md))
             )
           )
          ((null dominates) ; If neither function dominates
           (if piecewise-constant
               (setq maxf fold)
             (progn
               (setq rnew-extent (dimension-extent d rnew))
               (setf (apply #'aref extents ei) (+ (apply #'aref extents ei) rnew-extent))
               (setq maxf (vector-average fold fnew (apply #'aref extents ei) rnew-extent))
               ))
           ; The following is right if both regions have same constant value, but not sure
           ; what should do if really two crossing line segments
           (unless omit-maximals
             (setf (aref (region-maximals nr) d) ; Add region's maximal element to list
                   (cons (maximal-elements-region rnew d) (aref (region-maximals nr) d)))
             )
             ; Do we need to do anything about copying maximals from other dimensions here, as is done above?
           )
          )
    maxf)
  )

; Maximize over one dimension of a PLM, yielding a new PLM
; Doesn't actually eliminate the dimension from the PLM, but makes it inactive
; prev-max-ds is the previous dimensions over which maximized, and thus ones from which want to copy maximals
(defun maximize-plm (p d &optional omit-maximals prev-max-ds)
  (let* ((np (init-summarize-plm p d))
         (rarray (plm-array p))
         (rarrayn (plm-array np))
         (rank (plm-rank p))
         (sizev (dimension-sizes-v p))
         (sizev-1 (vector-1 sizev))
         (size (array-total-size rarray))
         (sizeln (dimension-sizes np))
         (sizen (array-total-size rarrayn))
         (maximums (make-array sizeln)) ; Stores maximums as function vectors
         (extents (make-array sizeln :initial-element 0))
         r ; Region
         rmaximum ; Region maximum
         ii ii0
         (piecewise-constant (plm-piecewise-constant p))
         rms ; Region maximals
         )
    (when trace-summarize
      (format trace-stream "~&~%MAX")
      (format trace-stream "~&~%P: ") (print-plm p t trace-stream)
      (format trace-stream "~&~%MAX DIMENSION: ~S" d)
      )
    ; Compute the maximization over the regions in the PLM as function vectors in the variable (vector) maximums
    (setq ii (make-list rank :initial-element 0))
    (dotimes (i size)
      (setf r (row-major-aref rarray i))
      (setq rmaximum (maximize-region r d piecewise-constant))
      ; Zero the index along d
      (setq ii0 (copy-seq ii))
      (setf (nth d ii0) 0)
      ; Maximize region
      (setf (apply #'aref maximums ii0) (if (apply #'aref maximums ii0)
                                            (maximum-region r rmaximum (apply #'aref maximums ii0) d (apply #'aref rarrayn ii0) extents ii0 omit-maximals prev-max-ds piecewise-constant)
                                         (progn
                                           (unless omit-maximals
                                             (setq rms (region-maximals (apply #'aref rarrayn ii0)))
                                             ; Copy maximals for previous maximized dimensions
                                             (dolist (md prev-max-ds)
                                               (setf (aref rms md) (aref (region-maximals r) md))
                                               )
                                            ; Set dimension's maximal element to that of the region
                                           (setf (aref rms d)
                                                 (list (maximal-elements-region r d)))
                                           )
                                         rmaximum)))
      (setq ii (next-index-list ii sizev-1 rank))
      )
    ; Assign functions to new regions
    (dotimes (i sizen)
      (assign-function (row-major-aref maximums i) (row-major-aref rarrayn i) piecewise-constant)
      )
    (setf (aref (plm-active np) d) nil)
    (when trace-summarize
      (format trace-stream "~&~%MAX RESULT: ") (print-plm np t trace-stream)
      )
    (when (plm-piecewise-constant p)
      (setf (plm-piecewise-constant np) t)
      )
    np)
  )

; -----------------------------------------------------------
; Compute value of function at one point along specified dimension

; Extract function of region if locations are within region along dimension ds
(defun extract-value-region (r ds locations &optional piecewise-constant)
  (let ((in-region t))
    (do ((dl ds (cdr dl))
         (ll locations (cdr ll)))
        ((or (null dl) (null ll)) nil)
      (unless (and (e>= (car ll) (region-min r (car dl)) t)
                   (e< (car ll) (region-max r (car dl)))
                   )
        (setq in-region nil)
        (return)
        )
      )
    (when in-region
      (extract-function r piecewise-constant)
      )
    )
  )

; Combine extracted functions for regions
(defun extract-value-combine (f1 f2 &optional piecewise-constant)
  piecewise-constant ; Dummy to avoid warning message
  (if f1 f1 f2)
  )

; Find function at specified location over one dimension of a PLM, yielding a new PLM
; Doesn't actually eliminate the dimension from the PLM, but makes it inactive
(defun extract-value-plm (p ds locations)
  (when (numberp locations) (setq locations (list locations)))
  (summarize-plm p ds 'extract-value #'extract-value-combine locations)
  )

; -----------------------------------------------------------
; Normalize a PLM
; If a specific dimension is mentioned, normalize along that one
; Otherwise if there is a single unique dimension, normalize along it
; Otherwise signal an error

; Assumes that weights on the other dimensions are all 0, so that can normalize by dividing by a constant

; Find the non-multiple dimensions (error if none)
(defun unique-dimensions (p &optional no-error-if-no-unique)
  (let (uds) ; Unique dimensions
    (dotimes (i (plm-rank p))
      (when (non-multiple-variable (aref (plm-variables p) i))
        (setq uds (cons i uds))
        )
      )
    (when (and (not no-error-if-no-unique) ; Error if there is no unique dimension
               (not uds) ; There is no unique dimension
               )
      (error "No unique dimension in PLM with variables ~S in call to unique-dimensions." (plm-variables p))
      )
    uds)
  )

; Given a PLM, normalize it along specified (or unique) dimension(s)
(defun normalize-plm (p &optional nds destructive vector); 
  (let (ip
        (ds (if nds nds (unique-dimensions p)))
        integral ; Result of integration out unique dimensions
        )
    (when trace-transform
      (format trace-stream "~&~%NORMALIZE:")
      (format trace-stream "~&~%P: ") (print-plm p symbolic-trace trace-stream)
      (format trace-stream "~&~%NORMALIZE DIMENSION(S): ~S" ds)
      )
    (setq integral (integral-plm p ds vector)) 
    (setf (plm-active integral) (init-vector (plm-rank integral) t))
    (setq ip (combine-plms p nil integral 'divide-0 nil nil nil destructive))
    (when trace-transform
      (format trace-stream "~&~%NORMALIZE RESULT: ") (print-plm ip symbolic-trace trace-stream)
      )
    ip)
  )

; -----------------------------------------------------------
; Compute the span along the dimension(s) specified that is above the smoothing paramter
; Used in subtractive normalization of a PLM

; Determine minimum point in region as a vector
; This is not the value at the point, but just the point at the dimension mins
(defun minimum-point (r)
  (let* ((rank (region-rank r))
         (mp (init-vector (+ rank 1)))
         )
    (setf (aref mp 0) 1) ; Value in point for constant (location 0) is always 1
    (dotimes (i rank)
      (setf (aref mp (+ i 1)) (region-min r i))
      )
    mp)
  )

; Determine maximum point in region as a vector
; This is not the value at the point, but just the point at the dimension maxs
(defun maximum-point (r)
  (let* ((rank (region-rank r))
         (mp (init-vector (+ rank 1)))
         )
    (setf (aref mp 0) 1) ; Value in point for constant (location 0) is always 1
    (dotimes (i rank)
      (setf (aref mp (+ i 1)) (region-max r i))
      )
    mp)
  )

; Determine extreme value in a region
(defun region-extreme (r relation)
  (let* ((min (minimum-point r))
         (max (maximum-point r))
         (rank (region-rank r))
         (fun (region-weights r rank nil))
         (min-val (linear-value fun min))
         (max-val (linear-value fun max))
        )
    (if (funcall relation min-val max-val) min-val max-val)
    )
  )

; Determine if the function in the region is above the smoothing parameter
(defun region-above-threshold-p (r smooth)
  (e> (region-extreme r #'<) smooth)
  )

; Compute volume of region along dimensions
(defun region-volume-ds (r ds)
  (if (numberp ds)
      (region-span r ds)
    (let ((volume 1))
      (dotimes (i (length ds))
        (setq volume (* volume (region-span r (nth i ds))))
        )
      volume))
  )

; Compute span-above threshold for a region
; If the region is above threshold, get volume along dimensions, otherwise 0
(defun span-above-threshold-region (r ds smooth &optional piecewise-constant)
  (if piecewise-constant
      (if (e> (region-constant r) smooth)
          (region-volume-ds r ds)
        0)
    (let ((wv (init-vector (1+ (length (region-dimensions r))) 0)))
      (when (region-above-threshold-p r smooth)
        (setf (aref wv 0) (region-volume-ds r ds))
        )
      wv))
  )

; Compute the span along the dimension(s) specified that is above the smoothing paramter
(defun span-above-threshold-plm (p ds smooth)
  (summarize-plm p ds 'span-above-threshold #'vector-sum smooth)
  )

; -----------------------------------------------------------
; Subtractive normalize a PLM
; If a specific dimension is mentioned, normalize along that one
; Otherwise if there is a single unique dimension, normalize along it
; Otherwise signal an error

; Assumes that weights on the other dimensions are all 0, so that get a constant sum over normalization dimension

; Destructively threshold a PLM based on whether regions in another function are above threshold
; Assumes constant functions for use in subtractive normalization
(defun threshold-plm-destructive (testp modifyp threshold)
  (let* ((trarray (plm-array testp))
         (mrarray (plm-array modifyp))
         (tsize (array-total-size trarray))
         (msize (array-total-size mrarray))
         )
    (unless (= tsize msize)
      (error "Test and modify arrays not of same size in THRESHOLD-PLM-DESTRUCTIVE: ~S versus ~S" tsize msize)
      )
    (dotimes (i tsize)
      (when (<= (region-constant (row-major-aref trarray i)) threshold)
        (setf (region-constant (row-major-aref mrarray i)) 0)
        )
      )
    modifyp)
  )

; Determine subtractor region from integrals and spans
(defun compute-subtractor (ifun sfun &optional piecewise-constant)
  (if piecewise-constant
      (let (wv)
        (setq wv (if (zerop sfun) 0 (/ (- ifun 1) sfun)))
        wv)
    (let ((wv (init-vector (length ifun) 0)))
      (setf (aref wv 0) ; Set contant to to subtractor
            (if (zerop (aref sfun 0))
                0
              (/ (- (aref ifun 0) 1)
                 (aref sfun 0))))
      wv))
  )

; Given a PLM, subtractively normalize it along specified (or unique) dimension(s)
(defun subtractive-normalize-plm (p smooth &optional nds)
  (let* ((ip (copy-a-plm p))
        (d (if nds nds (unique-dimensions p)))
        (integral (integral-plm p d)) ; Result of integrating out unique dimensions
        (span (span-above-threshold-plm p d smooth))
        sp ; Subtractor PLM
        (btp (transform-plm #'boolean-threshold-function p smooth)) ; Boolean threshold version of p (which has a 0 value for any region <= smooth)
        )
    (when trace-transform
      (format trace-stream "~&~%SUBTRACTIVE NORMALIZE:")
      (format trace-stream "~&~%P: ") (print-plm p symbolic-trace trace-stream)
      (format trace-stream "~&~%BOOLEAN THRESHOLD P (~S): " smooth) (pplm btp)
      (format trace-stream "~&~%SUBTRACTIVE NORMALIZE DIMENSION(S): ~S" d)
      (format trace-stream "~&~%INTEGRAL ALONG NORMALIZE DIMENSION(S): ") (pplm integral)
      (format trace-stream "~&~%SPAN ALONG NORMALIZE DIMENSION(S): ") (pplm span)
      )
    (setq sp (combine-plms integral nil span 'compute-subtractor nil nil t)) ; Keep "unneeded" slices
    (when trace-transform
          (format trace-stream "~&~%SUBTRACTOR ALONG NORMALIZE DIMENSION(S): ") (pplm sp)
          )
    (setq sp (product-plms sp btp))
    (when trace-transform
          (format trace-stream "~&~%FULL THRESHOLDED SUBTRACTOR: ") (pplm sp)
          )
    (setq ip (combine-plms p nil sp 'difference)) 
    (when trace-transform
      (format trace-stream "~&~%SUBTRACTIVE NORMALIZE RESULT: ") (print-plm ip symbolic-trace trace-stream)
      )
    (remove-unneeded-slices ip))
  )

; -----------------------------------------------------------
; Compute expected value of a PLM
; If a specific dimensions are mentioned, compute expectations along them
; Otherwise if there are unique dimensions, compute expectations along them
; Otherwise signal an error

; As with normalize-plm, assumes that weights on the other dimensions are all 0, so that can normalize by dividing by a constant

; A continuous point starting at x and going for epsilon2
(defun continuous-point (x)
  (list x (+ x epsilon2))
  )

; Return element in region r (along dimension that has been eliminated) that generates the expected function value
(defun expected-element-region (r)
  (continuous-point (region-constant r))
  )

; Compute the expected value of a region along a dimension
; Does this need to be extended for exponential functions?
(defun expected-value-region (r d)
  (let (ct ; Coefficient term
        (wv (init-vector (1+ (region-rank r)) 0))
        )
    (setq ct (* (/ (region-constant r) 2)
                (- (expt (region-max r d) 2)
                   (expt (region-min r d) 2))
                ))
    (unless (e= (region-weight r d) 0) ; There is a coefficient for this dimension
      (setq ct (+ ct (* (/ (region-weight r d) 3)
                        (- (expt (region-max r d) 3)
                           (expt (region-min r d) 3)
                           )
                        )
                  ))
      )
    (setf (aref wv 0) ct)
    wv)
  )

; Compute the expected value of the non-multiple variable of a plm
; Only appropriate when the non-multiple dimension is numeric and there is only one of them
; If assign-maximals is true, put the expected value into the maximals for the dimension
; Eventually generalize summarize-plm to handle this?
(defun expected-value-plm (p &optional d assign-maximals)
  (let* ((rarray (plm-array p))
         (rank (plm-rank p))
         (sizev (dimension-sizes-v p))
         (sizev-1 (vector-1 sizev))
         (size (array-total-size rarray))
         (np (init-summarize-plm p d))
         (rarrayn (plm-array np))
         (sizeln (dimension-sizes np))
         (sizen (array-total-size rarrayn))
         (expecteds (make-array sizeln))
         r nr ; Regions
         rexpected ; Region integral
         ii ii0 ; Index list
         )
    (unless d (setq d (car (unique-dimensions p))))
    ; Error message if dimension is non-numeric
    (unless (stype-numeric (svariable-type (aref (plm-variables p) d))) ; When dimension not numeric
      (pplm p)
      (error "Attempt to compute expected value along non-numeric dimension ~S of PLM."
             (svariable-name (aref (plm-variables p) d))
             )
      )
    (when trace-summarize
      (format trace-stream "~&~%EXPECTED VALUE")
      (format trace-stream "~&~%P: ") (print-plm p symbolic-trace trace-stream)
      (format trace-stream "~&~%EXPECTED VALUE DIMENSION: ~S" d)
      )
    ; Compute the expected value over the regions in the PLM as function vectors in the variable (vector) expecteds
    (setq ii (make-list rank :initial-element 0))
    (dotimes (i size)
      ; Set the dth index to 0
      (setq ii0 (copy-seq ii))
      (setf (nth d ii0) 0)
      (setf r (row-major-aref rarray i))
      (setq rexpected (expected-value-region r d))
      (setf (apply #'aref expecteds ii0) (if (apply #'aref expecteds ii0) (apply #'vector-sum rexpected (list (apply #'aref expecteds ii0))) rexpected))
      (setq ii (next-index-list ii sizev-1 rank))
      )
    ; Assign functions to new regionsh (and maximals if not omitted)
    (dotimes (i sizen)
      (setq nr (row-major-aref rarrayn i))
      (assign-function (row-major-aref expecteds i) nr)
      ; Set up the maximals with the expected point
      (when assign-maximals
        (setf (aref (region-maximals nr) d) (list (expected-element-region nr)))
        )
      )
    ; Make summarised dimension inactive
    (setf (aref (plm-active np) d) nil)
    (when trace-summarize
      (format trace-stream "~&~%EXPECTED VALUE RESULT: ") (print-plm np symbolic-trace trace-stream)
      )
    (when (plm-piecewise-constant p)
      (setf (plm-piecewise-constant np) t)
      )
    np)
  )

; -----------------------------------------------------------
; Make PLM's functional value explicit along the designated variable
; I.e., set the appropriate region (starting at the fraction along the dimension specified by the functional value) along the variable to 1 and everything else to 0

; Create an explicit cpt entry for one region
(defun explicit-cpt-entry (r nd rank vs)
  (let (cpt position vt lv)
    (dotimes (j rank)
      (setq vt (svariable-type (aref vs j)))
      (if (= j nd)
          (progn
            (setq lv (linear-value (extract-function r) (region-mids r)))
            (when (> lv 1)
              (error "Attempt to create an explicit representation of a functional value greater than 1: ~S" lv)
              )
            (setq position (* lv (stype-span vt)))
            (when (stype-discrete vt)
              (setq position (floor position))
              (when (= position (stype-max vt))
                (setq position (1- position)) ; Let a fraction of 1 still yield the highest discrete value
                )
              )
            (setq position (+ (stype-min vt) position)) ; start at min, rather than necessarily at 0
            (push (if (stype-discrete vt)
                      (list position (1+ position))
                    (progn
                      (setq position (min position (- (stype-max vt) epsilon2))) ; Make sure position leaves room for width of continuous region
                      (list position (+ position epsilon2)))) cpt)
            )
        (push (list (region-min r j) (region-max r j)) cpt))
      )
    (cons 1 (reverse cpt)))
  )

; Make PLM's functional value explicit along the designated variable
(defun explicit-plm (p nd)
  (let* (ep
         (rarray (plm-array p))
         (rank (plm-rank p))
         (size (array-total-size rarray))
         cpt-list ; List of region descriptions to turn into a PLM
         (vs (plm-variables p))
         )
    (when trace-transform
      (format trace-stream "~&~%EXPLICIT:")
      (format trace-stream "~&~%P: ") (print-plm p symbolic-trace trace-stream)
      (format trace-stream "~&~%EXPLICIT DIMENSION: ~S" nd)
      )
    ; Generate a list of explicit cpt entries, one per region
    (dotimes (i size)
      (setq cpt-list (cons (explicit-cpt-entry (row-major-aref rarray i) nd rank vs) cpt-list))
      )
    (setq ep (cpt-function-array-plm (variable-names vs) cpt-list vs 0))
    (when trace-transform
      (format trace-stream "~&~%EXPLICIT RESULT: ") (print-plm ep symbolic-trace trace-stream)
      )
    (setq ep (remove-unneeded-slices ep))
    (when trace-transform
      (format trace-stream "~&~%EXPLICIT RESULT (WITH UNNEEDED SLICES REMOVED): ") (print-plm ep symbolic-trace trace-stream)
      )
    (when (plm-piecewise-constant p)
      (setf (plm-piecewise-constant ep) t)
      )
    ep)
  )

; -----------------------------------------------------------
; Smooth a PLM

; Destructively smooth a piecewise constant PLM
(defun smooth-plm (p minimum)
  (let ((rarray (plm-array p))
        r)
    (dotimes (i (array-total-size rarray))
      (setq r (row-major-aref rarray i))
      (when (< (region-constant r) minimum)
        (setf (region-constant r) minimum)
        )
      )
    p)
  )

; -----------------------------------------------------------
; Determine extreme value in a PLM

; Determine extreme value in PLM given relation and largest possible value in other direction
(defun plm-extreme (p relation sofar)
  (let ((nrarray (plm-array p))
        r-ex)
    (dotimes (i (array-total-size nrarray))
      (setq r-ex (region-extreme (row-major-aref nrarray i) relation))
      (when (funcall relation r-ex sofar)
        (setq sofar r-ex)
        )
      )
    sofar)
  )

; -----------------------------------------------------------
; Average a PLM
; Create a new PLM from an existing one, replacing every region value with the average
; value for dimension along which are averaging

; Given a PLM, average it along specified dimension
(defun average-plm (p d)
  (let (ip)
    (when trace-average
      (format trace-stream "~&Function to average (over dimension ~A): " d)
      (pplm p)
      )
    (setq ip (combine-plms (integral-plm p d) nil (summarize-plm p d 'volume #'vector-sum) 'divide))
    (when trace-average
      (format trace-stream "~&Average function: ")
      (pplm ip)
      )
    ip)
  )

; Average a PLM over a list of dimensions
(defun average-plm-ds (p ds)
  (dolist (d ds)
    (setq p (average-plm p d))
    )
  p)

; -----------------------------------------------------------
; Compute transform of a PLM
; I.e., apply a function to values within each region

; Invert value
; For values in [0,1] get 1-v, for values >1 get 0
(defun invert-value (value)
  (if (e> value 1) ; Handle fact that the weight may be greater than 1
      0
    (- 1 value))
  )

; Invert a function (yielding 1-f)
; Subtracts constant from one and multiplies coefficients by -1.
; Yields a constant 0 if the value of the function can be >1 anywhere in region
(defun invert-function (f r &optional piecewise-constant)
  (if piecewise-constant
      (if (> f 1) 0 (- 1 f))
    (let* ((rank (length f))
           (nf (init-vector rank 0))
           d+1)
      (unless (or (> (linear-value f (region-mins r)) 1)
                  (> (linear-value f (region-maxs r)) 1)
                  )
        (setf (aref nf 0) (- 1 (aref f 0)))
        (dotimes (d (- rank 1))
          (setq d+1 (+ d 1))
          (setf (aref nf d+1) (- (aref f d+1)))
          )
        )
      nf))
  )

; Variation on invert-function that acts as if all positive regions are 1
; To do this completely correctly, may need to check if there is an epsilon subregion
; at the edge that is 0, split it off and make it 1 in the inverse.
; But this isn't done at the moment.
(defun invert-function-variant (f r &optional piecewise-constant)
  (if piecewise-constant
      (if (e= f 0) 1 0)
    (let* ((rank (length f))
           (nf (init-vector rank 0)))
      (if (region-e-empty r piecewise-constant)
          (setf (aref nf 0) 1)
        (setf (aref nf 0) 0)
        )
      nf))
  )

; Multiply function by scaling factor
(defun scale-function (f r scale &optional piecewise-constant)
  (if piecewise-constant
      (if (or (zerop f) (zerop scale))
          0
        (if (= f 1)
            scale
          (if (= scale 1)
              f
            (* scale f))))
    (let* ((rank (length f))
           (nf (init-vector rank 0)))
      r ; dummy so no warning about r not being used
      (dotimes (d rank)
        (setf (aref nf d) (* scale (aref f d)))
        )
      nf))
  )

; Negate function (multiply by -1)
(defun negate-function (f r &optional piecewise-constant)
  (scale-function f r -1 piecewise-constant)
  )
    
;; Add value to function
(defun add-to-function (f r value &optional piecewise-constant)
  (if piecewise-constant
      (if (zerop f)
          value
        (if (zerop value)
            f
          (+ f value)))
    (let ((nf (copy-seq f)))
      r ; dummy so no warning about r not being used
      (setf (aref nf 0) (+ (aref f 0) value))
      nf))
  )

; Compute exponential of function (just marking as exponential)
(defun exponentiate-function (f nr)
  (setf (region-exponential nr) t)
  f)

; Compute exponential of function (actually exponentiating the constant while ignore dimension weights)
(defun exponentiate-constant-function (f r &optional piecewise-constant)
  (if piecewise-constant
      (exp f)
    (let (nf)
      r ; dummy so no warning about r not being used
      (setq nf (init-vector (length f) 0))
      (setf (aref nf 0) (exp (aref f 0)))
      nf))
  )

; Compute exponential of (* function 10) (actually exponentiating 10 times the constant while ignore dimension weights)
; If function already normalized, get exponentials of 0-10
(defun exponentiate-constant-times10-function (f r &optional piecewise-constant)
  (if piecewise-constant
      (exp (* f 10))
    (let (nf)
      r ; dummy so no warning about r not being used
      (setq nf (init-vector (length f) 0))
      (setf (aref nf 0) (exp (* (aref f 0) 10)))
      nf))
  )

; Determine whether a function (represented as a vector) is zero
(defun function-zero (f &optional piecewise-constant)
  (if piecewise-constant
      (e= f 0)
    (every #'(lambda (x) (e= x 0)) f))
  )

; Make constant function Boolean based on a threshold (1 if above threshold, 0 otherwise)
(defun boolean-threshold-function (f r threshold &optional piecewise-constant)
  (if piecewise-constant
      (if (<= f threshold) 0 1)
    (let ((nf (init-vector (length f) 0)))
      r ; dummy so no warning about r not being used
      (unless (<= (aref f 0) threshold)
        (setf (aref nf 0) 1)
        )
      nf))
  )

; Actually convert a constant function to its exponential
(defun actually-exponentiate-function (f &optional piecewise-constant)
  (if piecewise-constant
      (if (zerop f) 0 (exp f))
    (let ((nf (init-vector (length f) 0)))
      (when (not (= (aref f 0) 0)) ; Leave value 0 if it is so no chance of selecting a 0 option
        (setf (aref nf 0) (exp (aref f 0)))
        )
      nf))
  )

; Convert function to Boltzmann
(defun boltzmann-function (f r &optional piecewise-constant)
  r ; dummy so no warning about r not being used
  (actually-exponentiate-function (scale-function f nil (if arousal (* arousal one-over-temperature) one-over-temperature) piecewise-constant) piecewise-constant)
  )

; Make function Boolean (1 if non-zero, 0 otherwise)
(defun boolean-function (f r &optional piecewise-constant)
  (if piecewise-constant
      (if (e= f 0) 0 1)
    (let ((nf (init-vector (length f) 0)))
      r ; dummy so no warning about r not being used
      (unless (function-zero f)
        (setf (aref nf 0) 1)
        )
      nf))
  )

; Transform region by function
(defun transform-region (function r &optional nr parameter use-nr piecewise-constant)
  (assign-function (if (and parameter use-nr)
                       (funcall function (extract-function r piecewise-constant) nr parameter piecewise-constant)
                     (if parameter
                         (funcall function (extract-function r piecewise-constant) r parameter piecewise-constant)
                       (if use-nr
                           (funcall function (extract-function r piecewise-constant) nr piecewise-constant)
                         (funcall function (extract-function r piecewise-constant) r piecewise-constant))))
                   (if nr nr r) piecewise-constant)
  )

; Given a PLM, transform the values of its regions via the specified function
; Parameter is a parameter of the function
; Use-nr determines if pass the new region to the function for modification
(defun transform-plm (function p &optional parameter use-nr destructive)
  (cond (destructive
         (let ((rarray (plm-array p))
               (piecewise-constant (plm-piecewise-constant p)))
           (when trace-transform
             (format trace-stream "~&~%Destructively apply ~S to function: " function) (print-plm p t trace-stream)
             )
           (dotimes (i (array-total-size rarray))
             (transform-region function (row-major-aref rarray i) nil parameter nil piecewise-constant)
             )
           (when trace-transform
             (format trace-stream "~&~%Result of transform: " function) (print-plm p t trace-stream)
             )
           p)
         )
        (t
         (let* ((rarray (plm-array p))
                (ip (init-plm-from-plm p))
                (irarray (plm-array ip))
                (piecewise-constant (plm-piecewise-constant p)))
           (when trace-transform
             (format trace-stream "~&~%Apply ~S to function: " function) (print-plm p t trace-stream)
             )
           (dotimes (i (array-total-size rarray))
             (transform-region function (row-major-aref rarray i) (row-major-aref irarray i) parameter use-nr piecewise-constant)
             )
           (when trace-transform
             (format trace-stream "~&~%Result of transform: " function) (print-plm ip t trace-stream)
             )
           (setf (plm-piecewise-constant ip) piecewise-constant) ; This assumes that the transform function doesn't convert a constant function to a linear one
           ip)
         )
        )
  )


; -----------------------------------------------------------
; Create a non-destructive copy of a PLM

; Copy a list of slices
(defun copy-slice-list (sl)
  (mapcar #'copy-slice sl)
  )

; Copy a slice vector
(defun copy-slice-vector (sv)
  (let* ((rank (length sv))
         (nsv (init-vector rank)) ; New slice vector
        )
  (dotimes (i rank)
    (setf (aref nsv i) (copy-slice-list (aref sv i)))
    )
  nsv)
  )

; Create a vector of slice vectors from a vector of slice lists
(defun slice-vector-vector (slice-vector-list)
  (let* ((rank (length slice-vector-list))
         (svv (init-vector rank))
         size sl sv)
    (dotimes (i rank)
      (setq sl (aref slice-vector-list i))
      (setq size (length sl))
      (setq sv (init-vector size))
      (setf (aref svv i) sv)
      (dolist (s sl)
        (setf (aref sv (slice-index s)) s)
        )
      )
    svv)
  )

; Create a vector of dimensions with size of each dimension
(defun dimension-sizes-from-slices (slices)
  (let* ((rank (length slices))
         (v (init-vector rank))
         )
    (dotimes (i rank)
      (setf (aref v i) (if (aref slices i)
                           (1- (length (aref slices i)))
                         0))
      )
    v)
  )

; Vector of array dimensions
(defun array-dimensions-v (a)
  (coerce (array-dimensions a) 'vector)
  )

; Create a vector of dimensions with size of each dimension
(defun dimension-sizes-v (p)
  (array-dimensions-v (plm-array p))
  )

; Create a list of dimensions with size of each dimension
(defun dimension-sizes (p)
  (array-dimensions (plm-array p))
  )

; Create a vector of dimension multipliers for row-major indexing
(defun dimension-multipliers (maxes)
  (let* ((rank (length maxes))
         (rank-1 (1- rank))
         (v (init-vector rank))
         j j1
         )
    (dotimes (i rank)
      (setq j (- rank-1 i))
      (setq j1 (1+ j))
      (setf (aref v j) (if (zerop i) 1 (* (aref v j1) (aref maxes j1))))
      )
    v)
  )

; Compute row-major address given index vector
(defun row-major-index (index mults)
  (let* ((rmi 0)
         (rank (length index))
         )
    (dotimes (i rank)
      (setq rmi (+ (* (aref index i) (aref mults i)) rmi))
      )
    rmi)
  )

; Compute index vector from row-major index
(defun index-from-row-major (rmi mults rank)
  (let (i rem (v (init-vector rank)))
    (dotimes (d rank)
      (multiple-value-setq (i rem) (floor (/ rmi (aref mults d))))
      (setf (aref v d) i)
      (setq rmi (* rem (aref mults d)))
      )
    v)
  )

; Create a vector relating row-major indices to index vectors
; sizev is a vector with the length of each of the array's dimensions
; size is the total number of elements in the array
; Rank is the number of dimensions of the array
; (Why can't the second and third argument be derived from the first?)
(defun row-major-vector (sizev size rank)
  (if (= rank 0)
      nil
    (let* ((v (init-vector size)) ; Result
          (ci (init-vector rank 0)) ; Current index vector
          (rank-1 (1- rank))
          r-1-j ; rank - (j + 1)
          )
      (dotimes (i size) ; Cycle through all row-major indices
        (setf (aref v i) (copy-seq ci))
        (dotimes (j rank) ; Cycle through all dimensions of vector index
          (setq r-1-j (- rank-1 j)) ; Proceed from right rather than left
          (if (< (aref ci r-1-j) (1- (aref sizev r-1-j))) ; If index at position is in bounds
              (return) ; Done generating vector index for this row-major index
            (setf (aref ci r-1-j) 0)) ; Else, zero index at position and move on to next index position
          )
        (setf (aref ci r-1-j) (1+ (aref ci r-1-j))) ; Increment index at position
        )
      v))
  )

; Copy the contents of one region to another
(defun copy-region-contents (from to)
  (setf (region-constant to) (region-constant from))
  (setf (region-maximals to) (copy-seq (region-maximals from)))
  (setf (region-evidence to) (region-evidence from))
  (setf (region-exponential to) (region-exponential from))
  (let ((rds (region-dimensions from)) (nrds (region-dimensions to)))
    (dotimes (d (length rds))
      (setf (dimension-weight (aref nrds d)) (dimension-weight (aref rds d)))
      )
    )
  to)

; Copy a PLM
(defun copy-a-plm (p)
  (let* ((rank (plm-rank p))
         (np (init-plm-from-plm p))
         (nprarray (plm-array np))
         (prarray (plm-array p))
         )
    (if (zerop rank)
        (let ((or (plm-origin p)))
          (setf (plm-array np) (make-array nil))
          (setf (aref (plm-array np)) (make-region :constant (region-constant or)
                                                   :exponential (region-exponential or)
                                                   :evidence (region-evidence or)
                                                   :dimensions (init-vector rank)))
          (setf (plm-slices np) (init-vector 0))
          )
      (progn
        ; Copy content of regions
        (dotimes (i (array-total-size nprarray))
          (copy-region-contents (row-major-aref prarray i) (row-major-aref nprarray i))
          )
        ))
    (when (plm-piecewise-constant p)
      (setf (plm-piecewise-constant np) t)
      )
    (when (plm-removed-unneeded-slices p)
      (setf (plm-removed-unneeded-slices np) t)
      )
    np)
  )

; -----------------------------------------------------------
; Reverse one dimension of a PLM (currently destructive)

; Reverse a single region
(defun reverse-region (r d)
  (let ((dd (aref (region-dimensions r) d))
        temp)
    ; Swap the dimension's min-slice and max-slice
    (setq temp (dimension-min-slice dd))
    (setf (dimension-min-slice dd) (dimension-max-slice dd))
    (setf (dimension-max-slice dd) temp)
    r)
  )

; Reverse one dimension of a PLM, yielding a new PLM
(defun reverse-plm (p d)
  (let* ((rarray (plm-array p))
         (rank (plm-rank p))
         (sizev (dimension-sizes-v p))
         (sizev-1 (vector-1 sizev))
         (nrarray (make-array (coerce sizev 'list)))
         ii iir ; Index list
         r ; Original region
         (d-1 (1- (aref sizev d)))
         (slices (plm-slices p))
         )
    (setq ii (make-list rank :initial-element 0))
    (dotimes (i (array-total-size rarray))
      (setq r (apply #'aref rarray ii))
      (reverse-region r d)
      (setq iir (copy-seq ii))
      (setf (nth d iir) (- d-1 (nth d ii))) ; Invert index along dimension
      (setf (apply #'aref nrarray iir) r)
      (setq ii (next-index-list ii sizev-1 rank))
      )
    (setf (plm-array p) nrarray) ; Switch to new region array
    ; Reverse slice list
    (setf (aref slices d) (reverse (aref slices d)))
    (index-slice-list-array (plm-slices p))
    p)
  )

; -----------------------------------------------------------
; Strip vestigial dimensions from a PLM and reorder it according to a map
; Destructively modifies PLM

; Create a new vector stripping off and reordering all dimensions but ones in map
(defun strip-and-reorder-vector (map nrank ovector)
  (let ((nvector (init-vector nrank)))
    (dotimes (d nrank)
      (setf (aref nvector d) (aref ovector (if map (aref (smap-vfactor map) d) d))))
    nvector)
  )

; Strip and reorder dimensions from a region of a PLM
(defun strip-and-reorder-region (map nrank r)
  (setf (region-dimensions r) (strip-and-reorder-vector map nrank (region-dimensions r)))
  r)

; Create an index for the original region array from one for the new one
(defun srr-original-index (ni map orank)
  (let ((oi (init-vector orank 0)))
    (dotimes (d (length ni))
      (setf (aref oi (if map (aref (smap-vfactor map) d) d)) (aref ni d))
      )
    oi)
  )

; Strip and reorder dimensions from regions of a PLM
; This operation is destructive in that it doesn't create a new PLM, but changes regions in place
(defun strip-and-reorder-regions (map nrank nsizev rarray)
  (if (zerop (array-rank rarray))
      rarray
    (let* ((nrarray (make-array (coerce nsizev 'list))) ; New region array
           (nsize (array-total-size nrarray)) ; Total size of new region array
;           (nmults (dimension-multipliers nsizev)) ; Dimension multipliers for row major index
           (rank (array-rank rarray)) ; Rank of old region array
           (nsizev-1 (vector-1 (array-dimensions-v nrarray)))
           niv
           )
      (setq niv (init-vector nrank 0))
      (dotimes (i nsize)
        (setf (row-major-aref nrarray i)
              (strip-and-reorder-region map nrank (if (zerop nrank)
                                                      (row-major-aref rarray 0)
                                                    (apply #'aref rarray (coerce (srr-original-index niv map rank) 'list)))))
        (setq niv (next-index-vector niv nsizev-1 nrank))
        )
      nrarray))
  )

; Strip dimensions from factor summary-product (PLM) not used in variable PLM
; The stripped dimensions should already be vestigial
; Also reorder the PLM according to the map and variables
(defun strip-and-reorder-plm (map vs p)
  (let* ((vrank (length vs))
         (nplm (make-plm :rank vrank :variables vs)))
    (setf (plm-active nplm) (strip-and-reorder-vector map vrank (plm-active p)))
    (setf (plm-slices nplm) (strip-and-reorder-vector map vrank (plm-slices p)))
    (setf (plm-array nplm) (strip-and-reorder-regions map vrank (map 'vector #'(lambda (sl) (1- (length sl))) (plm-slices nplm)) (plm-array p)))
    (when (or trace-combine trace-summarize trace-affine)
      (format trace-stream "~&~%PLM after stripping and reordering dimensions: ") (print-plm nplm symbolic-trace)
      )
    (when (plm-piecewise-constant p)
      (setf (plm-piecewise-constant nplm) t)
      )
    nplm)
  )

; -----------------------------------------------------------
; Combine dimensions for reused variables in a pattern, extracting the diagonal along dimensions

; Combine dimensions while testing for equality across them
; This is for handling variable reuse within a single pattern in a conditional
(defun factor-combined-dimensions (p np dns)
  (let ((new-slices (init-vector (plm-rank np)))
        (old-slices (plm-slices p)))
    ; Apply slices from each incoming variable in list to other incoming variables in list
    (dolist (i dns)
      (dolist (j dns)
;        (unless (= i j)
          (setf (aref new-slices j) (append (aref old-slices i) (aref new-slices j)))
;          )
        )
      )
    (setf new-slices (sort-slice-list-array new-slices))
    (delete-duplicates-slice-list-array new-slices)
    (setf np (apply-slices new-slices nil p))
    )
  )

; Determine if region is a diagonal along listed (by number) dimensions
(defun diagonal-region (r dns)
  (let ((diagonal t)
        bounds dims first-dim other-dim)
    (setq dims (region-dimensions r))
    (setq first-dim (aref dims (car dns)))
    (setq bounds (list (slice-location (dimension-min-slice first-dim)) (slice-location (dimension-max-slice first-dim))))
    (dolist (other-dn (cdr dns))
      (setq other-dim (aref dims other-dn))
      (when (or (not (e= (car bounds) (slice-location (dimension-min-slice other-dim)) t))
                (not (e= (cadr bounds) (slice-location (dimension-max-slice other-dim)) t))
                )
        (setq diagonal nil)
        )
      )
    diagonal)
  )

; Slice vector for result of diagonalize-plm
; All but first dimension of dns should just have extreme slices
(defun diagonal-slices (slices vars dns)
  (let* ((rank (length slices))
         (nsv (init-vector rank))
         (rdns (cdr dns))
         )
    (dotimes (d rank)
      (setf (aref nsv d)
            (if (member d rdns)
                (let ((vt (svariable-type (aref vars d))))
                  (list (make-slice :location (stype-min vt) :index 0) (make-slice :location (stype-max vt) :index 1))
                  )
              (copy-slice-list (aref slices d))))
      )
    nsv)
  )

; Create content of new diagonal region from old region
(defun diagonalize-region (r nr dns &optional piecewise-constant)
  (let ((ds (region-dimensions r))
        (nds (region-dimensions nr))
        )
    (setf (region-constant nr) (region-constant r))
    ; Copy dimension information
    (dotimes (d (length ds))
      (setf (dimension-discrete (aref nds d)) (dimension-discrete (aref ds d)))
      (setf (dimension-weight (aref nds d)) (dimension-weight (aref ds d)))
      )
    ; Add weight from all of the diagonalized dimensions to the first one
    (unless piecewise-constant
      (setf (dimension-weight (aref nds (car dns))) (reduce #'+ ds :key #'dimension-weight))
      )
    nr)
  )

; Create an index for the original array from an index for the diagonalized array
; This is destructive
(defun diag-original-index (ni dns)
  (let ((dv (aref ni (car dns)))) ; Domain element to be replicated along other diagonal dimensions
    (dolist (d (cdr dns))
      (setf (aref ni d) dv) ; Change new value, which should be 0, to the same old value for first dimension of diagonal
      )
    ni)
  )

; Extract diagonal from PLM along dimensions listed (by number)
; The regions along the diagonal should already be square along these dimensions
(defun diagonalize-plm (p dns)
  (let* ((rank (plm-rank p))
         (rarray (plm-array p))
         (nslices (diagonal-slices (plm-slices p) (plm-variables p) dns))
         (np (init-plm-with-slices (plm-variables p) 0 0 (init-vector rank t) nslices))
         (nrarray (plm-array np))
         (nsizev (dimension-sizes-v np))
         (nsizev-1 (vector-1 nsizev))
         niv
         (piecewise-constant (plm-piecewise-constant p))
         )
    (setq niv (init-vector rank 0))
    (dotimes (i (array-total-size nrarray))
      (diagonalize-region (apply #'aref rarray (coerce (diag-original-index niv dns) 'list)) (row-major-aref nrarray i) dns piecewise-constant)
      (setq niv (next-index-vector niv nsizev-1 rank))
      )
    (setf (plm-piecewise-constant np) piecewise-constant)
    np)
  )

; Destructively compute next index along diagonal
(defun next-diagonal-index (index dns)
  (let ((nv (1+ (aref index (car dns)))))
    (dolist (d dns)
      (setf (aref index d) nv)
      )
    index)
  )

; Destructively zero all of the diagonal regions of a shattered PLM (so square regions along diagonal)
(defun zero-diagonal (p dns)
  (let ((rarray (plm-array p))
        (index (init-vector (plm-rank p) 0))
        (piecewise-constant (plm-piecewise-constant p))
        r)
    (dotimes (i (array-dimension rarray (car dns)))
      (setq r (apply #'aref rarray (coerce index 'list)))
      (unless (region-e-empty r piecewise-constant)
        (empty-region r)
        )
      (setq index (next-diagonal-index index dns))
      )
    p)
  )

; Combine dimensions for reused variables in a pattern, extracting everything but the diagonal along dimensions
; This is to implement a not-equal (<>) test across variables in a single pattern
(defun inverse-diagonalize-plm (p dns)
  (dolist (d dns) ; Shatter PLM along all diagonalization dimensions so yield square regions along diagonal
    (setq p (shatter-plm p d))
    )
  (zero-diagonal p dns)
  )

;---------------------------------------end PLM operations

; Create an initial message from a variable node to a factor node
(defun init-var-fact-link-content (vn fn in out)
  (let ((vn-vs (node-variables vn))
        (evidence (node-evidence fn))
        )
    (cond ((and (eq (node-subsubtype vn) 'van) ; When chaining on actions, need the input from VANs to FAN to initialize to 0
                (eq (node-subtype fn) 'combine)
                (or (eq (node-subsubtype fn) 'sum)
                    (eq (node-subsubtype fn) 'max)
                    (eq (node-subsubtype fn) 'por)
                    )
                )
           (empty-plm vn-vs)
           )
          (t
           (if (eq evidence t) ; WM factor node
               (if in ; The variable node is from an action or condact
                   (full-plm vn-vs) ; Generic message
                 nil)
             (if (member vn evidence) ; The variable node is evidence for this factor
                 (if out ; Messages flow from evidence variable nodes
                     (full-plm vn-vs) ; Generic message
                   nil) ; No message
               (if (and in ; Messages flow from non-evidence variable nodes
                        (not (eq (node-subtype fn) 'pass-through))) ; A hack to avoid the message from prediction predicate's outgoing WMVN to prediction factor node
                   (full-plm vn-vs) ; Generic message
                 nil) ; No message
               )
             )
           )
          )
    )
  )

; Create an initial message from a factor node to a variable node
(defun init-fact-var-link-content (l fn vn unary map in out)
  (let ((vn-vs (node-variables vn))
        (evidence (node-evidence fn))
        (function (node-function fn))
        )
    (if (eq evidence t) ; WM factor node
        (if out ; Messages flow to non-evidence variable nodes
            (if (link-variables-same l)
                function
              (strip-and-reorder-plm map vn-vs function)) ; Send factor function
          nil) ; No message
      (if (member vn evidence) ; The variable node is evidence for this factor
          (if in ; Messages flow to evidence variable nodes
              (if unary ; Only one variable node
                  (if (predicate-attention-predicate (node-predicate fn)) ; There is attention for this node
                      (apply-attention (node-predicate fn) ; Apply attention to initial value 
                                       (if (link-variables-same l)
                                           function
                                         (strip-and-reorder-plm map vn-vs function))) ; Send factor function
                    (if (link-variables-same l)
                                           function
                                         (strip-and-reorder-plm map vn-vs function)))
                (full-plm vn-vs)) ; Generic message
            nil) ; No message
        (if out ; Messages flow to non-evidence variable nodes
            (if unary ; Only one variable node
                function ; Send factor function
              (full-plm vn-vs)) ; Generic message
          nil)
        )
      )
    )
  )

; Add message to prequeue
(defun prequeue (m)
  (unless (aref (link-prequeue (message-link m)) (message-index m))
    (setf (aref (link-prequeue (message-link m)) (message-index m)) m)
    )
  )

; Create initial outgoing messages from a node to all of its links.
(defun init-outgoing-messages-links (node &optional ignore-decision-count)
  (let* ((ls (node-links node))
         (unary (eq (length ls) 1))
         (is-var-node (variable-nodep node))
         (index (if is-var-node var-index fact-index))
         m lnode lcontent)  
    (dolist (l ls)
      (setq lnode (if is-var-node (link-fact-node l) (link-var-node l)))
      (setq lcontent (if is-var-node
                         (init-var-fact-link-content node lnode (link-in l) (link-out l))
                       (init-fact-var-link-content l node lnode unary (link-map l) (link-in l) (link-out l))
                       )
            )
      (when (and lcontent ; Link is active in direction
                 (or ignore-decision-count ; Allow a forced reintialization even if already done on decision (for use with new evidence)
                     (< (aref (link-inits l) index) decision-count) ; Not already initialized on this decision
                     )
                 )      
        ; Create message for link and add to prequeue
        (setf (aref (link-contents l) index) lcontent)
        (setq m (make-message :link l :index index :wm-driven (wm-fnp node)))
        (prequeue m)
        (setf (aref (link-inits l) index) decision-count) ; Mark link direction with decision on which initialized
        )
      )
    )
  )

; Create initial outgoing messages for all of the descendant-links of a node
(defun update-outgoing-messages-links (node &optional ignore-decision-count)
  (let* (
         (ls (node-descendant-links node))        
         n index unary is-factor-node l m lnode lcontent
         )
    (dolist (ld ls)
      (setf l (descendant-link-link ld)) 
      (setf lnode (link-node l (descendant-link-direction ld)))
      (setf is-factor-node (factor-nodep lnode))
      (setf index (if (eq (descendant-link-direction ld) 1) 0 1))
      (setf n (link-node l index))
      (setf unary (eq (length (node-links n)) 1))
      (setq lcontent (if is-factor-node
                         (init-var-fact-link-content n lnode (link-in l) (link-out l))
                       (init-fact-var-link-content l n lnode unary (link-map l) (link-in l) (link-out l))
                       )
            )
      (when (and lcontent ; Link is active in direction
                 (or ignore-decision-count ; Allow a forced reintialization even if already done on decision (for use with new evidence)
                     (< (aref (link-inits l) index) decision-count) ; Not already initialized on this decision
                     )
                 )
        ; Create message for link and add to prequeue
        (setf (aref (link-contents l) index) lcontent)
        (setq m (make-message :link l :index index :wm-driven (and (wm-nodep n) (not is-factor-node))))
        (setf (aref (link-stale l) index) t)
        (prequeue m)
        (setf (aref (link-inits l) index) decision-count) ; Mark link direction with decision on which initialized
        )
      )
    )
  )

; Initialize message counts on links
(defun init-link-counts nil
  (dolist (l (graph-links cg))
    (setf (aref (link-counts l) var-index) 0)
    (setf (aref (link-counts l) fact-index) 0)
    )
  )


; Total number of messages sent (summing link-counts)
(defun total-link-counts ()
  (let ((total 0))
    (dolist (l (graph-links cg))
      (when (numberp (aref (link-counts l) var-index))
        (setq total (+ total (aref (link-counts l) var-index)))
        )
      (when (numberp (aref (link-counts l) fact-index))
        (setq total (+ total (aref (link-counts l) fact-index)))
        )
      )
    total)
  )

; Set messages from assumption nodes as fresh, and all others as stale
(defun init-messages-fresh-stale ()
  (dolist (l (graph-links cg))
    (setf (aref (link-stale l) var-index) t)
    (if (node-assumption (link-node l fact-index))
        (setf (aref (link-stale l) fact-index) nil)
      (setf (aref (link-stale l) fact-index) t))
    )
  )

; Initialize graph-changes vector to nil
(defun initialize-graph-changes ()
  (let ((cgs (graph-changes cg)))
    (if cgs
        (dotimes (i (graph-node-count cg))
          (setf (aref cgs i) nil)
          )
      (setf (graph-changes cg) cgs))
    )
  (setf (graph-changes cg) (init-vector (graph-node-count cg)))
  )

; Create initial messages for all links in a graph
(defun init-graph-messages (&optional ignore-decision-count)
;  (mapc #'(lambda (n) (init-outgoing-messages-links n ignore-decision-count)) (graph-nodes cg))
  (let (nf)
    (dolist (n (graph-nodes cg))
      (setq nf (node-function n))
      (init-outgoing-messages-links n ignore-decision-count)
      (when (and nf (typep nf 'plm) (plm-is-constant nf))
        (setf (plm-piecewise-constant nf) t)
        )
      )
    (initialize-graph-changes) ; In case there was any evidence that set graph-changes
    (init-messages-fresh-stale)
    )
  )

; Update messages between decisions
(defun update-graph-messages (&optional ignore-decision-count)
  (let ((cgs (graph-changes cg))
        )
    (if (and save-message-state ; We're saving message state across decisions
             (> decision-count 1) ; This is not the first decision
             )
        ; Update messages from all nodes that have changed
        (dolist (n (graph-nodes cg))
          (when (aref cgs (node-number n))
            (update-outgoing-messages-links n ignore-decision-count)
            (when (plm-is-constant (node-function n))
              (setf (plm-piecewise-constant (node-function n)) t)
              )
            )
          )
      (init-graph-messages ignore-decision-count)
      )
    (initialize-graph-changes)
    )
  )

; Return a list of copies of outgoing messages for a node
(defun node-outgoing-messages (n)
  (let ((index (if (variable-nodep n) 0 1))
        ml
        contents)
    (dolist (l (node-links n))
      (setq contents (aref (link-contents l) index))
      (when contents
        (setq ml (cons (cons (node-number (aref (link-nodes l) (- 1 index))) (copy-a-plm contents)) ml))
        )
      )
    ml)
  )

; Reorder variables by volumes
(defun reorder-factor-variables (vs)
  (sort (copy-seq vs) #'< :key #'(lambda (v) (v-type (if (listp v) (svariable-type (car v)) (svariable-type v)))))
  )

; Create a list of depth message queues of one per depth plus one for everything more (and null)
(defun full-depth-message-queues ()
  (let* ((depth (+ (graph-depth cg) 1))
         (dqs (init-vector depth))
         )
    (dotimes (i depth)
      (setf (aref dqs i) (init-queue))
      )
    dqs)
  )

; Create message queues
(defun init-message-queues ()
  (let ((qs (init-vector 2)))
    (setf (aref qs depth-queues-index) (full-depth-message-queues))
    (setf (aref qs other-queue-index) (init-queue))
    qs)
  )

; Find message coming in to WM FN node
(defun wm-change-message (pred)
  (let (message
        (predicate-wm (predicate-wm pred))
        (pos-var (predicate-incoming-vn pred))
        )
    (dolist (l (node-links predicate-wm))
      (when (equal pos-var (link-var-node l))
        (return (setq message (link-var-content l)))
        )
      )
    message)
  )

; Initialize a run by initializing link contents and messages
(defun init-messages (&optional compute-depths)
  (restrict-to-bottom-state) ; State dimensions should all be limited to base-state
  (init-graph-messages t)
  (when compute-depths
    (compute-all-depths)
    )
  (setf (graph-queues cg) (init-message-queues))
  (load-queues)
  )
(defun im nil (init-messages))

; Break when message is non-empty from one node number to another
(defun break-on-positive-messages ()
  (let (break mf)
    (dolist (m break-on-positive-messages)
      (setq mf (message-function-from-numbers (car m) (cadr m)))
      (when (not (plm-empty mf))
        (format trace-stream "~&~%Breaking on positive message from ~S to ~S: " (node-name (node-from-number (car m))) (node-name (node-from-number (cadr m))))
        (pplm mf)
        (setq break t)
        )
      )
    break)
  )  

; Get message from first node (name) to second
(defun message (n1-name n2-name)
  (let* ((n1-node (node-from-name n1-name))
         (n2-node (node-from-name n2-name))
         v-node ; Variable node
         f-node ; Factor node
         index ; Index for accessing content array
         )
    (cond ((variable-nodep n1-node)
           (setq v-node n1-node)
           (setq f-node n2-node)
           (setq index var-index)
           )
          (t
           (setq v-node n2-node)
           (setq f-node n1-node)
           (setq index fact-index)
           )
          )
    (dolist (link (node-links v-node))
      (when (eq (aref (link-nodes link) fact-index) f-node)
        (return (aref (link-contents link) index))
        )
      )
    )
  )


; Get message PLM from node numbers
(defun mp (nn1 nn2)
  (let* ((n1 (node-from-number nn1))
         (n2 (node-from-number nn2))
         (l (link-from-nodes n1 n2))
         )
    (if (variable-nodep n1)
        (link-var-content l)
      (link-fact-content l))
    )
  )      

; Messages with maximum regions, from graph-statistics
; Vector with total regions and non-empty regions
(defvar max-message-regions)

; Compute statistics on messages
(defun message-statistics (&optional stream)
  (unless stream (setq stream trace-stream))
  (let ((nums 0)
        (sums (init-vector 2 0))
        (maxs (init-vector 2 0))
        (maxms (init-vector 2 nil)) ; Lists of messages with maximum regions
        p ; message PLM
        regs) ; Data on regions in a message PLM
    (dolist (n (graph-nodes cg))
      (when (factor-nodep n)
        (dolist (l (node-links n))
          (dotimes (i 2) ; Iterate over message direction
            (setq p (aref (link-contents l) i))
            (when p
              (setf nums (+ nums 1))
              (setq regs (count-plm-regions p))
              (dotimes (j 2) ; Iterate over whether region is empty
                (setf (aref sums j) (+ (aref sums j) (aref regs j)))
                (when (= (aref regs j) (aref maxs j))
                  (setf (aref maxms j)
                        (cons (list (node-name (aref (link-nodes l) i))
                                    (node-name (aref (link-nodes l) (invert-value i)))
                                    )
                              (aref maxms j)))
                  )
                (when (> (aref regs j) (aref maxs j))
                  (setf (aref maxs j) (aref regs j))
                  (setf (aref maxms j)
                        (list (node-name (aref (link-nodes l) i))
                              (node-name (aref (link-nodes l) (invert-value i)))
                              ))
                  )
                )
              )
            )
          )
        )
      )
    (dotimes (i 2)
      (setf (aref sums i)
            (coerce (/ (aref sums i) nums) 'short-float)) ; Convert sums to averages
      )
    (format stream "~&Average regions per message: ~5,1F; non-empty regions: ~5,1F"
            (aref sums 0) (aref sums 1))
    (format stream "~&Maximum regions per message: ~s; non-empty regions: ~S"
            (aref maxs 0) (aref maxs 1))
    (setq max-message-regions maxms) ; Store maxes globally in case want to look at them
    t)
  )
; Shortcut for message-statistics
(defun ms (&optional stream)
  (unless stream (setq stream trace-stream))
  (message-statistics stream)
  )

; -----------------------------------------------------------
; Compute the depth of each link direction in a graph
; To be used in stratifying messages with depth in queues

; Compute the depth of a link from a node
; Adding new node to queue of nodes
(defun compute-link-depth (l n q)
  (let* ((direction (if (variable-nodep n) 0 1))
         (opposite (- 1 direction))
         (links (node-links n))
         (depth -1) ; Depth when no incoming links should be 0
         il-depth ; Depth of incoming link
         depth+1
         )
    (unless (or (node-assumption n) ; Don't need to check for these
                (not (incoming-links n)) ; Handle open-world WMVN nodes when no actions or condacts
                )
      (dolist (il links)
        (when (and (not (= (node-number (aref (link-nodes l) opposite)) ; Don't consider outgoing link as incoming
                           (node-number (aref (link-nodes il) opposite))))
                   (aref (link-contents il) opposite) ; Only consider active incoming links
                   )
          (setq il-depth (aref (link-depths il) opposite))
          (if il-depth
              (when (> il-depth depth) (setq depth il-depth))
            (progn
              (setq depth nil)
              (return)
              )
            )
          )
        )
      )
    (when depth
      (setq depth+1 (+ depth 1))
      (setf (aref (link-depths l) direction) depth+1)
      (when (> depth+1 (graph-depth cg)) (setf (graph-depth cg) depth+1))
      (add-to-queue (aref (link-nodes l) opposite) q)
      )
    q)
  )

; Compute the link depths outgoing from a node
; Adding new nodes to queue as found
(defun compute-node-link-depths (n q)
  (let ((direction (if (variable-nodep n) 0 1)))
    (dolist (l (node-links n))
      (when (and (aref (link-contents l) direction) ; Outgoing link is active
                 (not (aref (link-depths l) direction)) ; Not already a depth
                 )
        (setq q (compute-link-depth l n q))
        )
      )
    q)
  )

; Are there incoming links for node?
(defun incoming-links (node)
  (let (incoming
        (node-incoming-index (if (factor-nodep node) 0 1)))
    (dolist (l (node-links node))
      (when (aref (link-contents l) node-incoming-index)
        (setq incoming t)
        (return)
        )
      )
    incoming)
  )

; Determine depths of all links in graph
(defun compute-link-depths ()
  (setf (graph-depth cg) 0)
  (let ((q (init-queue)) ; Queue of nodes, not messages, for determining depth
        n)
    ; Initialize queue to assumption nodes
    (dolist (node (graph-nodes cg))
      (when (or (node-assumption node)
                (not (incoming-links node)) ; Handle open-world WMVN nodes when no actions or condacts
                )
        (add-to-queue node q)
        )
      )
    (loop
     (when (queue-empty q) (return))
     (setq n (pop-queue q))
     (setq q (compute-node-link-depths n q))
     )
    )
  )

; Compute the loop-depth of a link from a node
; Adding new node to queue of nodes
(defun compute-link-loop-depth (l n d q)
  (let ((other (- 1 d))
        (loop-depth 0)
        )
      (dolist (il (node-links n))
        (when (and (not (equal l il)) ; Don't consider outgoing link as incoming
                   (aref (link-contents il) other) ; Only consider active incoming links
                   (aref (link-loop-depths il) other) ; There is an incoming loop-depth
                   )
          (setq loop-depth (max loop-depth (+ (aref (link-loop-depths il) other) 1)))
          )
        )
    (when (> loop-depth 0)
      (setf (aref (link-loop-depths l) d) loop-depth)
      (add-to-queue (aref (link-nodes l) other) q)
      (when (> loop-depth (graph-depth cg))
        (setf (graph-depth cg) loop-depth)
        )
      )
    q)
  )


; Compute the link loop-depths outgoing from a node
; Adding new nodes to queue as found
(defun compute-node-link-loop-depths (n q)
  (let ((d (if (variable-nodep n) 0 1)))
    (dolist (l (node-links n))
      (when (and (aref (link-contents l) d) ; Outgoing link is active
                 (not (aref (link-depths l) d)) ; Not already a depth
                 (not (aref (link-loop-depths l) d)) ; Not already a loop-depth
                 )
        (setq q (compute-link-loop-depth l n d q))
        )
      )
    q)
  )

; Process link to see if it is a loop edge, given it is in loop
(defun link-loop-edge (n ol d q)
  (let ((loop-depth 0)
        (other (- 1 d))
        )
    (dolist (il (node-links n)) ; Iterate through incoming links
      (when (and (not (equal ol il)) ; Incoming and outgoing are different links
                 (aref (link-contents il) other) ; Incoming link is active
                 (aref (link-depths il) other) ; There is an incoming depth
                 )
        (setq loop-depth (max loop-depth (+ (aref (link-depths il) other) 1)))
        )
      )
    (when (> loop-depth 0) ; There are some incoming depths
      (setf (aref (link-loop-depths ol) d) loop-depth)
      (add-to-queue (aref (link-nodes ol) other) q)
      (when (> loop-depth (graph-depth cg))
        (setf (graph-depth cg) loop-depth)
        )
      )
    )
  )
        

; Process links from node to see if loop edge
(defun node-loop-edge (n q)
  (let ((direction (if (variable-nodep n) 0 1)))
    (dolist (l (node-links n))
      (when (and (aref (link-contents l) direction) ; Outgoing link is active
                 (not (aref (link-depths l) direction)) ; Child is depthless
                 ) 
        (link-loop-edge n l direction q)
        )
      )
    )
  )
          

; Determine depths of all links in graph
(defun compute-link-loop-depths ()
  (let ((q (init-queue)) ; Queue of nodes, not messages, for determining depth
        n)
    ; First process all loop edges (nodes with depthless outgoing links but with some incoming depths)
    (dolist (node (graph-nodes cg))
      (unless (node-assumption node) ; Don't need to check for these
        (node-loop-edge node q)
        )
      )
    (loop
     (when (queue-empty q) (return))
     (setq n (pop-queue q))
     (unless (node-assumption n) ; Don't need to check for these
       (setq q (compute-node-link-loop-depths n q))
       )
     )
    )
  )

; Clear all link depths
(defun clear-link-depths nil
  (setf (graph-depth cg) nil)
  (dolist (l (graph-links cg))
    (dotimes (i 2)
      (setf (aref (link-depths l) i) nil)
      (setf (aref (link-loop-depths l) i) nil)
      )
    )
  )

; Compute link-depths and then link-loop-depths
(defun compute-all-depths ()
  (when (graph-depth cg) ; Link depths have already been computed
    (clear-link-depths) ; So clear all before recomputing
    )
  (compute-link-depths)
  (compute-link-loop-depths)
  )

; -----------------------------------------------------------
; Define summary-product algorithm

; Are two messages on same arc (same link and index)
(defun same-arc (l1 i1 l2 i2)
  (and (eq i1 i2)
       (eq (link-node l1 0) (link-node l2 0))
       (eq (link-node l1 1) (link-node l2 1))
       )
  )

; Is a message on arc in the queue?
(defun arc-in-queue (l i q)
  (dolist (mi (cdr (queue-head q)) nil)
    (when (same-arc l i (message-link mi) (message-index mi))
      (return (list q mi))
      )
    )
  )

; Is a message on arc in the depth queues?
(defun arc-in-depth-queues (l i dqs)
  (let (returned)
    (dotimes (j (length dqs))
      (setq returned (arc-in-queue l i (aref dqs j)))
      (when returned
        (return returned)
        )
      )
    returned)
  )

; Is a message on arc in the queues?
(defun arc-in-queues (l i qs)
  (let (returned)
    (setq returned (arc-in-depth-queues l i (aref qs depth-queues-index)))
    (unless returned
      (setq returned (arc-in-queue l i (aref qs other-queue-index)))
      )
    returned)
  )

; Remove message on arc from queue
(defun remove-arc-in-queue (l i q)
  (do ((mi-l (queue-head q) (cdr mi-l)))
      ((null (cdr mi-l)))
    (when (same-arc l i (message-link (cadr mi-l)) (message-index (cadr mi-l)))
      (when (eq (queue-tail q) (cdr mi-l)) ; If removing last element, move tail pointer forward
        (setf (queue-tail q) mi-l))
      (rplacd mi-l (cddr mi-l)) ; Remove item from queue
      (return t)
      )
    )
  )

; Check list of depth queues if there is at least one that isn't empty
(defun depth-message-queues-not-empty (dqs)
  (let (ne)
    (dotimes (i (length dqs))
      (when (queue-not-empty (aref dqs i))
        (setq ne t)
        (return)
        )
      )
    ne)
  )

; Are the queues not empty?
(defun message-queues-not-empty (qs)
  (or (depth-message-queues-not-empty (aref qs depth-queues-index))
      (queue-not-empty (aref qs other-queue-index))
      )
  )

; Add message m to the graph's queue
(defun add-to-message-queue (m)
  (let* ((ml (message-link m))
         (mi (message-index m))
         (qs (graph-queues cg))
         q
         depth
	 msgidx
         (front nil) ; Whether to add message at the front of the queue
         )
    (setq msgidx (list (concatenate 'string (write-to-string (message-index m)) "_" (write-to-string depth) "_" (write-to-string (node-name (link-node ml 0))) "_" (write-to-string (node-name (link-node ml 1))))))
    (setf (message-didx m) msgidx)

;    (when (and (not (arc-in-queues ml mi qs)) ; Message not already in queue
    (when (and (not (gethash msgidx *qhash*)) ; Message not already in queue
               (link-content ml mi) ; Link is active in direction
               )
      (setq depth (if (aref (link-depths ml) mi)
                      (aref (link-depths ml) mi)
                    (aref (link-loop-depths ml) mi)))
      (cond (depth ; The message direction has a depth
             (setq q (aref (aref qs depth-queues-index) depth))
             (when trace-queue
               (format trace-stream "~&Adding to ~A of depth queue ~S: " (if front '"front" '"back") (aref (link-depths ml) mi))
               (print-message nil ml mi t)
               )
             )
            (t
             (setq q (aref qs other-queue-index))
             (when trace-queue
               (format trace-stream "~&Adding to ~A of other queue: " (if front '"front" '"back"))
               (print-message nil ml mi t)
               )
             )
            )
      (setf (gethash msgidx *qhash*) 1)
      (add-to-queue m q front)
      )
    )
  )

; Is variable located at current site
; If the site is a list, see if a member, otherwise just check for same
(defun variable-exists (v vs-site)
  (let ((vname (if (typep v 'svariable) (svariable-name v) v)))
    (if (listp vs-site)
        (member vname vs-site :key #'svariable-name)
      (eq vname (svariable-name vs-site))))
  )

; Find the variable number from a variable in a vector of variables
; Some of the slots may have more than one variable, so need to be sure to check all
(defun variable-number (v vs)
  (dotimes (i (length vs))
    (when (variable-exists v (aref vs i))
      (return i)
      )
    )
  )

; Factor variable(s) for dimension are part of a vector of variables from variable node
(defun variables-members (fn-vars vn-vars)
  (if (listp fn-vars)
      (dolist (fnv fn-vars nil)
        (when (find (svariable-name fnv) vn-vars :key #'svariable-name) (return t)))
    (find (svariable-name fn-vars) vn-vars :key #'svariable-name)
    )
  )

; Given a binary node and one of its links return the other one
(defun other-link (n link)
  (if (equal link (car (node-links n)))
      (cadr (node-links n))
    (car (node-links n)))
  )

; Given a transform node and one of its incoming links return the other one
(defun other-incoming-link (n link)
  (let (other)
    (dolist (l (node-links n))
      (when (and (not (equal l link)) ; Different link
                 (aref (link-contents l) var-index) ; There is an incoming message
                 )
        (return (setq other l))
        )
      )
    other)
  )

; Given a transform node and one of its incoming links return the other one
(defun other-unidirectional-incoming-link (n link)
  (let (other)
    (dolist (l (node-links n))
      (when (and (not (equal l link)) ; Different link
                 (aref (link-contents l) var-index) ; There is an incoming message
                 (not (aref (link-contents l) fact-index)) ; There is no outgoing message
                 )
        (return (setq other l))
        )
      )
    other)
  )

; Create an outgoing message from a factor node
; Need to multiply all incoming messages (except for from outgoing node)
; times the node's function and integrate/maximize out all but the outgoing variables.
; Integrates/maximizes out a variable right after the function has been multiplied by it
(defun outgoing-fact-var-message (m wm-driven)
  (let* ((ml (message-link m)) ; The link for the message
         (lc (link-fact-content ml)) ; Existing message to send to variable node
         (fn (link-fact-node ml)) ; Factor node from which message is sent
         (vn (link-var-node ml)) ; Variable node to which message is sent
         (vn-vars (node-variables vn)) ; The variables used at the variable node
         (sum-prod (node-function fn)) ; Cumulatively computed message content
         (steps (node-factor-steps fn)) ; The steps to be taken to generate message
         (skip-product (and (beta-factor fn) (plm-full (node-function fn)))) ; Skip inital product for beta factors with a function of 1 under right circumstances
         (first-product t) ; Whether product is first one performed
         new-message ; Whether generate a new message
         s
         )
    (cond ((eq (node-subtype fn) 'transform) ; Factor function is a function to be computed on input message rather than PLM
           (setq sum-prod (transform-message sum-prod (other-incoming-link fn ml) ml (node-normalize fn)))
           )
          ((eq (node-subtype fn) 'combine) ; Factor function does a one-way functional combination of inputs
           (setq sum-prod (combine-messages fn (node-subsubtype fn)))
           )
          ((eq (node-subtype fn) 'affine) ; Factor function computes a tranform on the PLM slices
            (setq sum-prod (affine-message sum-prod (other-link fn ml) ml (node-variables fn) (other-unidirectional-incoming-link fn (other-link fn ml)))) ;updated by VOLKAN
           )
          ((eq (node-subtype fn) 'explicit) ; Factor converts implicit functional value into an explicit domain value along new action variable
           (setq sum-prod (explicit-plm (aref (link-contents (other-incoming-link fn ml)) var-index) (cadr (node-function fn))))
           )
          ((eq (node-subtype fn) 'function) ; Function factor node that maintains a constant message across the decision
           (setq sum-prod lc)
           )
          (t
           ; Summarize across the product of the messages
           (do ((ss steps (cdr ss)))
               ((null ss))
             (setq s (car ss))
             (let ((sl (factor-step-argument s))) ; The link (product) or variable number (integrate/maximize) for this step
               (if (eq (factor-step-type s) 'product)
                   (when (and
                          (not (eq sl ml)) ; Not from target variable node
                          (link-var-content sl) ; Incoming message from variable node is active
                          )
                     (setq sum-prod
;                           (remove-unneeded-slices
                            (combine-plms (link-var-content sl) (link-map sl) sum-prod 'product)
;                            )
                           )
                     (setq first-product nil)
                     )
                 (unless (variables-members (aref (node-variables fn) sl) vn-vars) ; Variable is not part of target VN
                   (setq sum-prod
;                         (remove-unneeded-slices
                                   (if (eq (factor-step-type s) 'integral)
                                       (integral-plm sum-prod sl)
                                     (maximize-plm sum-prod sl))
;                                   )
                         )
                   )
                 )
               )
             )
           ; Determine when to use an inverse filter (padding with 1s)
           (when (and (eq (node-subtype fn) 'filter)
                      (or (eq (node-subsubtype fn) 'inverse-both) ; Condacts that are are using the inverse filter in both directions
                          (and (eq (node-subsubtype fn) 'inverse-in) (member vn (node-evidence fn))) ; Incoming condact or action (with open-actions-like-condacts T)
                          (and (eq (node-subsubtype fn) 'inverse-out) (not (member vn (node-evidence fn)))) ; Outgoing condition (with open-conditions-like-condacts T)
                          )
                      )
             (setq sum-prod (combine-plms sum-prod nil (node-inverse-function fn) 'por))
             )
; If there are any linear filters that are problematic, should be flagged when they are defined, rather than requiring this here
           ; For linear filters set a floor of 0 on all regions
;           (when (and (eq (node-subtype fn) 'filter)
;                      (node-linear-filter fn)
;                      )
;             (setq sum-prod (smooth-plm sum-prod 0))
;             )
           ; Copy node function if a beta factor and there are no incoming messages
           (when (and skip-product first-product)
             (setq sum-prod (copy-a-plm sum-prod))
             )
           ; Strip PLM of dimensions not part of PLM for variable node and reorder variables/dimensions as needed
           (unless (link-variables-same ml)
             (setq sum-prod (strip-and-reorder-plm (link-map ml) vn-vars sum-prod))
             )
           (setq sum-prod (remove-unneeded-slices sum-prod))
           )
          )
    ; If new PLM is epsilon-different from old, set it and add message to queue
    (cond ((plm-e= lc sum-prod arousal) ; Handle cases where initial message not changed, but it should be piecewise constant
           (unless (eq (plm-piecewise-constant lc) (plm-piecewise-constant sum-prod))
             (setf (plm-piecewise-constant lc) (plm-piecewise-constant sum-prod))
             )
           )
          (t
           (setf (aref (link-contents ml) fact-index) sum-prod)
           (setf (message-wm-driven m) wm-driven) ; If incoming messages is wm-driven, then so is outgoing
           (setq new-message t)
           )
          )
    new-message)
  )

; Test if the link has a message that should be used in the product
(defun use-link-in-var-product (l fn)
  (and (not (eq (link-fact-node l) fn)) ; Message is not from target factor node
       (link-fact-content l) ; There is a message (not being blocked by variable node being evidence)
       )
  )

; Strip off any initial links at a variable node that are not to be used in product
(defun strip-front-useless-links (ls fn)
  (do ((link-list ls (cdr link-list)))
      ((null link-list) nil)
    (when (use-link-in-var-product (car link-list) fn) (return link-list))
    )
  )

; Dynamically reorder messages coming into variable node for efficiency
(defun reorder-variable-product (ls)
  (sort (copy-seq ls) #'< :key #'(lambda (l) (if (link-fact-content l) (plm-size (link-fact-content l)) infinity)))
  )

(defvar destructive-product t)
(defvar dynamic-variable-product-ordering nil)

; Create an outgoing message from a variable node
; Just compute product of incoming messages (except for from outgoing node)
; can exponential predicates be vector predicates? Exponential predicates are not checked for vector normalization 
(defun outgoing-var-fact-message (m wm-driven)
  (let* ((ml (message-link m))
         (lc (link-var-content ml)) ; Message to send to factor node
         (vn (link-var-node ml)) ; Variable node from which message is sent
         (fn (link-fact-node ml)) ; Factor node to which message is sent
         (ls (node-links vn)) ; Links from variable node to factor nodes
         prod
         new-message ; Whether generate new message
         product-computed
         )
    ; Dynamically reorder links for product optimization
    (when dynamic-variable-product-ordering
      (setq ls (reorder-variable-product ls))
      )
    ; Instead of initializing prod to the 1 array and doing an extra product,
    ; initialize prod to first PLM not from the "to" node and not nil (i.e., from an evidence factor)
    (setq ls (strip-front-useless-links ls fn))
    (when ls ; When there are inputs to include in the product (node has more than one neighbor)
      (setq prod (link-fact-content (car ls))) ; Initialize product to message from first input
      (setq ls (cdr ls))
      ; Compute the product of the messages in prod (always of same rank so no offset)
      (dolist (l ls)
        (when (use-link-in-var-product l fn)
          (setq prod
;                (remove-unneeded-slices
                 (combine-plms (link-fact-content l) nil prod 'product (wm-nodep vn)))
          (unless product-computed (setq product-computed t))
        )
;          )

        )
      ; Exponentiate messages out of WM VN nodes for exponential predicates
      (when (node-exponential vn)
        (when (node-normalize vn)
          (setq prod (normalize-plm prod nil product-computed))
          )
        (setq prod (transform-plm #'exponentiate-constant-times10-function prod nil nil product-computed))
        )
      ; This normalizes most outgoing messages from WM VN nodes
      (when (and (node-normalize vn)
                 (not (and
                       (graph-selected-predicate cg) ; There is not a selected predicate
                       (equal (predicate-wm (graph-selected-predicate cg)) fn) ; or not going to the SELECTED WMFN (for decisions and impasse detection)
                       ))
                 (not (eq (node-subtype fn) 'pass-through)) ; This is not a predictive pass through node
                 (not (eq (node-subtype fn) 'function)) ; Don't normalize messages going to predicate function nodes
                 )
        (setq prod
              (if (node-vector vn) 
                  (normalize-plm prod nil product-computed t) 
                (normalize-plm prod nil product-computed))
              ) 
        ) 
      (setq prod (remove-unneeded-slices prod))
      ; If new PLM is epsilon-different from old
      (cond ((plm-e= lc prod arousal) ; Handle cases where initial message not changed, but it should be piecewise constant
             (unless (eq (plm-piecewise-constant lc) (plm-piecewise-constant prod))
               (setf (plm-piecewise-constant lc) (plm-piecewise-constant prod))
               )
             )
            (t
             (setf (aref (link-contents ml) var-index) prod)
             (unless (wm-nodep fn)
               (setf (message-wm-driven m) wm-driven) ; If incoming messages is wm-driven, then so is outgoing
               )
             (setq new-message t)
             )
            )
      )
    new-message)
  )

; Check if a node is a variable node
(defun variable-nodep (n) (eq (node-type n) 'variable))

; Check if a node is a factor node
(defun factor-nodep (n) (eq (node-type n) 'factor))

; Compute average message time
(defun average-message-time ()
  (when (zerop number-of-messages)
    (error "Attempt to compute average message time over zero counted messages")
    )
  (coerce (/ sum-of-message-times number-of-messages) 'short-float)
  )

; Initialize message time variables
(defun init-message-times ()
  (setq trace-message-times t)
  (setq number-of-messages 0)
  (setq sum-of-message-times 0)
  (setq maximum-message-time 0)
  )

; Shortcut for print-message-times
(defun pmt ()
  (print-message-times)
  )

; Create and process an outgoing message from a node
(defun outgoing-message (m wm-driven trace)
  (let ((ml (message-link m))
        message-time new-message)
    (when trace-message-times
        (setq number-of-messages (+ number-of-messages 1))
        (setq message-time (get-internal-run-time)) ; Time before create message
        )
    (setq new-message (if (variable-nodep (link-node ml (message-index m)))
                          (outgoing-var-fact-message m wm-driven )
                        (outgoing-fact-var-message m wm-driven)))
    (when trace-message-times
      (setq message-time (- (get-internal-run-time) message-time)) ; Time to create message
      (setq sum-of-message-times (+ sum-of-message-times message-time))
      (when (> message-time maximum-message-time)
        (setq maximum-message-time message-time)
        )
      )
    (when (or (eq trace t) ; If t, trace all messages
              (and trace ; If non-nil
                   (listp trace) ; and a list
                   ; And either node is in the list of nodes to trace
                   (or (member (node-name (link-node ml 0)) trace)
                       (member (node-number (link-node ml 0)) trace)
                       (member (node-name (link-node ml 1)) trace)
                       (member (node-number (link-node ml 1)) trace)
                       )
                   )
              )
      ; Then print the message
      (print-message (concatenate 'string
                                  (when trace-wm-driven (format nil "{~S}" (message-wm-driven m)))
                                  (format nil ">>~S>> " (if (eq message-protocol 'serial) message-count '-))
                                  )
                     ml (message-index m) symbolic-trace nil t trace-stream)
      )
    new-message)
  )

; Get next message index
(defun next-message-index (mi)
  (mod (+ mi 1) 2))

; Process a message coming into a node
(defun incoming-message (m trace)
  (let* ((ml (message-link m)) ; Incoming messaage link
         (mi (message-index m)) ; Incoming message index
         (nmi (next-message-index mi)) ; Index for outgoing messages
         (in-from (link-node ml mi)) ; Source node for incoming message
         (in-to (link-node ml nmi)) ; Target node for incoming message
         out-to ; Target node for outgoing message
         out-message ; Outgoing message
         (wm-driven (message-wm-driven m)) ; Whether incoming message is wm-driven
         queue-message new-message
         (stale (aref (link-stale ml) mi))
        )
    (setq message-count (1+ message-count)) ; Increment global count of messages processed
    ; Compute message if not fresh and set it to fresh
    (when stale
      (setq new-message (outgoing-message m wm-driven trace))
      (setf (aref (link-stale ml) mi) nil)
      )
    (when (or (not stale) new-message)
      (when (or (eq trace t) ; If t, trace all messages
                (and trace ; If non-nil
                     (listp trace) ; and a list
                   ; And either node is in the list of nodes to trace
                     (or (member (node-name in-from) trace)
                         (member (node-number in-from) trace)
                         (member (node-name in-to) trace)
                         (member (node-number in-to) trace)
                         )
                     )
                )
        ; Then print the message
        (print-message (concatenate 'string
                                    (when trace-wm-driven (format nil "{~S}" (message-wm-driven m)))
                                    (format nil "<<~S<< " message-count)
                                    )
                       (message-link m) (message-index m) symbolic-trace t t trace-stream)
        )
      (dolist (l (node-links in-to)) ; Process all links at target node
        (setq out-to (link-node l mi)) ; Set target node for outgoing message
        (when (and
               (not (eq out-to in-from)) ; Not incoming link
               (link-content l nmi) ; The outgoing link is active in that direction
               (not (node-assumption in-to))
               )
          (setf (aref (link-stale l) nmi) t) ; Mark outgoing link as stale
          (setq queue-message (arc-in-queues l nmi (graph-queues cg)))
          (setq out-message (if queue-message (cadr queue-message) (make-message :link l :index nmi)))
          (setf (message-wm-driven m) wm-driven) ; If incoming messages is wm-driven, then so is outgoing
          (add-to-message-queue out-message)
          )
        )
      )
    t)
  )

; Figure out which depth message queue to pop and pop it
(defun pop-depth-message-queues (dqs)
  (dotimes (i (length dqs))
    (when (queue-not-empty (aref dqs i))
      (return (pop-queue (aref dqs i)))
      )
    )
  )

; Get message off of first queue with content
(defun pop-message-queues (qs)
  (cond ((depth-message-queues-not-empty (aref qs depth-queues-index))
         (pop-depth-message-queues (aref qs depth-queues-index)))    
        (t (pop-queue (aref qs other-queue-index)))
        )
  )

; Mark a link in a direction to signify that a message is incoming
(defun mark-incoming-message (message trace)
  (setf (aref (link-incoming (message-link message)) (message-index message)) t)
  (setq message-count (1+ message-count)) ; Increment global count of messages processed
  (when trace
    (print-message (concatenate 'string
                                (when trace-wm-driven (format nil "{~S}" (message-wm-driven message)))
                                (format nil "<<~S<< " message-count)
                                )
                   (message-link message) (message-index message) symbolic-trace t t trace-stream)
    )
  t)


; Process one message from queue
(defun process-one-message (trace)
  (interrupt-run)
  (let ((message (pop-message-queues (graph-queues cg))))
    (when message
      (remhash  (message-didx message) *qhash*)
      (setf (aref (link-counts (message-link message)) (message-index message))
            (1+ (aref (link-counts (message-link message)) (message-index message))))
      (if (eq message-protocol 'serial)
          (incoming-message message trace)
        (mark-incoming-message message trace)
        )
      )
    )
  )

; Process message queue until empty or reach limit
(defun process-message-queue (messages trace)
  (let ((max-steps (if messages messages max-messages))
        quiescent
        steps)
    (dotimes (i max-steps)
      (unless (process-one-message trace)
        (setq quiescent t)
        (setq steps i)
        (return steps))
      )
    (when (and (not quiescent) (= max-steps max-messages))
      (format trace-stream "~&Cut off at maximum messages per decision (~S)" max-steps)
      )
    steps)
  )

; Set all but the ith element of a vector to T
(defun set-all-but-i (v length i)
  (dotimes (x length)
    (unless (equal x i) (setf (aref v x) t))
    )
  v)

; Generate vector, one element for each link of a node, with T if should generate an output based on input
(defun generate-output-vector (n index)
  (let* ((num-links (length (node-links n)))
         (generate (init-vector num-links))
         (link-number 0))
    (dolist (l (node-links n))
      (when (aref (link-incoming l) (next-message-index index))
        (set-all-but-i generate num-links link-number))
      (setq link-number (+ link-number 1))
      )
    generate)
  )

; Generate all output messages for a node in parallel mode
(defun generate-node-messages (n index trace)
  (interrupt-run)
  (let ((generate (generate-output-vector n index))
        l)
    ; Generate the output messages for the node
    (do ((ls (node-links n) (cdr ls))
         (i 0 (+ i 1))
         )
        ((null ls) t)
      (setq l (car ls))
      (when (and (aref generate i) ; Should generate based on input
                 (link-content l index) ; The link is defined in this direction
                 )
        (outgoing-message (make-message :link l :index index) nil trace)
        )
      )
    )
  )

; Generate all output messages for a parallel elaboration cycle
(defun generate-outgoing-messages (trace)
  (dolist (n (graph-nodes cg)) ; Process links at all nodes in the graph
    (if (factor-nodep n)
        (generate-node-messages n fact-index trace)
      (generate-node-messages n var-index trace)
      )
    )
  )

; Set all incoming marks back to nil
(defun initialize-incoming-marks ()
  (dolist (l (graph-links cg))
    (setf (aref (link-incoming l) var-index) nil)
    (setf (aref (link-incoming l) fact-index) nil)
    )
  )

; Execute elaboration cycles (in parallel mode)
(defun elaboration-cycles (cycles trace)
  (let (starting-message-count ; Number of messages at beginning of cycle
        messages) ; Number of messages during a cycle
    (dotimes (cycle (if cycles cycles max-elaboration-cycles)) ; For each elaboration cycle
      (unless (message-queues-not-empty (graph-queues cg)) (return cycle))
      (setq starting-message-count message-count)
      (when trace-cycles
        (format trace-stream "~&Cycle ~S" cycle)
        )
      (process-message-queue nil trace) ; Process all of the messages as inputs
      (setq messages (- message-count starting-message-count))
      (generate-outgoing-messages trace) ; Genereate all output messages
      (initialize-incoming-marks) ; Set all incoming marks back nil
      (when trace-cycles
        (format trace-stream "~&~S Messages" messages)
        (setq cycle-message-counts (cons messages cycle-message-counts))
        )
      )
    )
)

; Check if anything is typed in the listener
; If it is, return t and flush the input
; Used for interrupting runs
(defun interrupt-run nil
  (when (listen)
    (clear-input)
    (throw 'interrupt-run interrupt-symbol)
    )
  )

; Load message queues from prequeue
(defun load-queues ()
  (dolist (l (graph-links cg))
    (dotimes (i 2)
      (when (aref (link-prequeue l) i)
        (add-to-message-queue (aref (link-prequeue l) i))
        (setf (aref (link-prequeue l) i) nil)
        )
      )
    )
  )

; Process message queue for default graph and trace values
(defun run (&optional cycles initial-run)
  (let (pre-time ; Time before measurement starts
        run-result) ; T or interrupt-symbol
    (when trace-performance (setq pre-time (get-internal-run-time)))
    (unless (graph-initialized cg)
      (init-graph)
      )
    (when (or initial-run (not have-run))
      (setq message-count 0)
      (when pre-run
        (pre-run) ; Execute pre-run forms
        )
      (update-graph-messages t) ; Reinitialize messages in graph given new WM/evidence
      (load-queues) ; Load messages from prequeue into queues
      )
    (setq have-run t) ; Mark that have already done a run (within the current decision)
    (when (and trace-performance (car global-decision-statistics))
      (setf (decision-statistics-init-time (car global-decision-statistics))
            (- (get-internal-run-time) pre-time))
      )      
    (when trace-performance (setq pre-time (get-internal-run-time)))
    (setq run-result (catch 'interrupt-run
                       (if (eq message-protocol 'serial)
                           (process-message-queue cycles trace-messages)
                         (elaboration-cycles cycles trace-messages)
                         )
                       )) ; Catch returns T if completes run and interrupt-symbol if interrupted
    (when (and trace-performance (car global-decision-statistics))
      (setf (decision-statistics-run-time (car global-decision-statistics))
            (- (get-internal-run-time) pre-time))
      (setf (decision-statistics-messages (car global-decision-statistics))
            message-count)
      )
    (when post-run
      (post-run) ; Execute post-run forms
      )
    run-result)
  )

(defun r (&optional steps do-pre-run) (run steps do-pre-run))
