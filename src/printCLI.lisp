(in-package :sigma)
; basic sigma print functions which do not require the CAPI library

; Print for which assumption nodes a node number is a descendant
(defun print-node-progenitors (node-number)
  (dolist (n (graph-nodes cg))
    (when (member node-number (node-descendants n) :key #'(lambda (d) (node-number (descendant-node d))))
      (format trace-stream "~S " (node-name n))
      )
    )
  )


; Print link depths
(defun print-link-depths (&optional stream)
  (unless stream (setq stream trace-stream))
  (format stream "~&")
  (dolist (l (graph-links cg))
    (dotimes (i 2)
      (when (aref (link-contents l) i) ; Link is active in direction
        (if (aref (link-depths l) i) ; There is a depth in direction
            (format stream "[~S->~S:~S]" (node-number (aref (link-nodes l) i)) (node-number (aref (link-nodes l) (- 1 i))) (aref (link-depths l) i))
          (format stream "(~S->~S:~S)" (node-number (aref (link-nodes l) i)) (node-number (aref (link-nodes l) (- 1 i))) (aref (link-loop-depths l) i))
          )
        )
      )
    )
  )
(defun plds (&optional stream) (print-link-depths stream))


; Print message times
(defun print-message-times (&optional stream)
  (unless stream (setq stream trace-stream))
  (unless trace-message-times
    (error "Attempt to print message times without having initially set trace-message-times to T")
    )
  (format stream "~&Messages: ~S; Average time: ~4,1F msec; Maximum time: ~7,1F msec"
          number-of-messages (average-message-time) maximum-message-time)
  )

; Print incoming marks for all links in graph
(defun print-incoming-marks ()
  (dolist (n (graph-nodes cg))
    (when (factor-nodep n)
      (dolist (l (node-links n))
        (format trace-stream "~&~S ~S: ~S" (node-name (aref (link-nodes l) 0)) (node-name (aref (link-nodes l) 1)) (link-incoming l))
        )
      )
    )
  )


; Compute aggregates needed for printing global-decision-statistics
(defun sum-decision-statistics (key) (reduce #'+ global-decision-statistics :key key))

; Convert a real to a rounded integer
(defun rri (n) (coerce (round n) 'integer))

; Print global-decision-statistics
(defun print-global-decision-statistics (&optional stream)
  (unless stream (setq stream trace-stream))
  (let* ((messages (sum-decision-statistics #'decision-statistics-messages))
         (run-time (sum-decision-statistics #'decision-statistics-run-time))
         (decision-time (sum-decision-statistics #'decision-statistics-decision-time))
         (learn-time (sum-decision-statistics #'decision-statistics-learn-time))
         (init-time (sum-decision-statistics #'decision-statistics-init-time))
         (total-time (+ run-time decision-time learn-time init-time))
         (cycles (length global-decision-statistics))
         (rcycles (* 1.0 cycles)) 
         )
    (format stream "~&~%Total time ~,2,,,F sec" (/ total-time 1000.0))
    (when (> trial-count 0)
      (format stream "~&Trials: ~S; Msec per trial: ~S" trial-count (/ total-time (* trial-count 1.0)))
      )
    (format stream "~&Decision cycles: ~S; Msec per decision cycle: ~S (init: ~S, messages: ~S, decision: ~S, learn: ~S)"
            cycles (rri (/ total-time rcycles)) (rri (/ init-time rcycles)) (rri (/ run-time rcycles)) (rri (/ decision-time rcycles)) (rri (/ learn-time rcycles)))
    (format stream "~&Total messages: ~S; Messages per decision: ~S"
            messages (rri (/ messages rcycles))
            )
    (when (> messages 0)
      (format stream "; Msec per message: ~,2,,,F" (/ (* 1.0 run-time) messages))
      )
    t)
  )
(defun pgds nil (print-global-decision-statistics))


; Print link counts > minimum (or just nhighest > minimum if specified)
(defun print-link-counts (&optional nhighest minimum)
  (unless nhighest (setq nhighest infinity))
  (unless minimum (setq minimum 1))
  (let (lcs (c 0))
    ; Create a list of dotted lists, one for each active link-direction
    (dolist (l (graph-links cg))
      (when (aref (link-contents l) var-index)
        (setq lcs (cons (cons (aref (link-counts l) var-index)
                              (concat-symbols (list (node-name (aref (link-nodes l) var-index))
                                                    '->
                                                    (node-name (aref (link-nodes l) fact-index)))))
                        lcs))
        )
      (when (aref (link-contents l) fact-index)
        (setq lcs (cons (cons (aref (link-counts l) fact-index)
                              (concat-symbols (list (node-name (aref (link-nodes l) fact-index))
                                                    '->
                                                    (node-name (aref (link-nodes l) var-index)))))
                        lcs))
        )
      )
    ; Sort list by highest to lowest by count
    (setq lcs (sort lcs #'> :key #'car))
    ; Print nhighest
    (dolist (lc lcs)
      (when (or (>= c nhighest) ; Stop when have printed nhighest link directions
                (< (car lc) minimum) ; Stop once reach link directions along which no messages sent
                )
        (return)
        )
      (format trace-stream "~&~S: ~S~&" (car lc) (cdr lc))
      (setq c (1+ c))
      )
    t)
  )
(defun plc (&optional nhighest minimum)
  (print-link-counts nhighest minimum)
  )

; Print link message sizes > minimum (or just nhighest > minimum if specified)
(defun print-link-message-sizes (&optional nhighest minimum)
  (unless nhighest (setq nhighest infinity))
  (unless minimum (setq minimum 1))
  (let (lms (c 0))
    ; Create a list of dotted lists, one for each active link-direction
    (dolist (l (graph-links cg))
      (when (aref (link-contents l) var-index)
        (setq lms (cons (cons (plm-size (aref (link-contents l) var-index))
                              (concat-symbols (list (node-name (aref (link-nodes l) var-index))
                                                    '->
                                                    (node-name (aref (link-nodes l) fact-index)))))
                        lms))
        )
      (when (aref (link-contents l) fact-index)
        (setq lms (cons (cons (plm-size (aref (link-contents l) fact-index))
                              (concat-symbols (list (node-name (aref (link-nodes l) fact-index))
                                                    '->
                                                    (node-name (aref (link-nodes l) var-index)))))
                        lms))
        )
      )
    ; Sort list by highest to lowest by count
    (setq lms (sort lms #'> :key #'car))
    ; Print nhighest
    (dolist (lm lms)
      (when (or (>= c nhighest) ; Stop when have printed nhighest link directions
                (< (car lm) minimum) ; Stop once reach link directions along which no messages sent
                )
        (return)
        )
      (format trace-stream "~&~S: ~S~&" (car lm) (cdr lm))
      (setq c (1+ c))
      )
    t)
  )
(defun plms (&optional nhighest minimum)
  (print-link-message-sizes nhighest minimum)
  )



; Print an array of node outgoing messages
(defun print-gma (a)
  (dotimes (i (length a))
    (format trace-stream "~&~S: " i)
    (dolist (np (aref a i))
      (format trace-stream "<~S> " (car np))
      (print-plm (cdr np) t trace-stream)
      )
    )
  nil)

; Print gd-test results (to be used within post-t for test runs)
(defun print-gd-test-results (atts)
  (let (tr total)
    (dolist (att atts)
      (setq tr (find-gd-test-results att))
      (setq total (total-gd-test-results tr))
      (when (> total 0)
        (format trace-stream "~&~%RESULTS FOR ATTRIBUTE ~S~&TP: ~S, TN: ~S~&FP: ~S, FN: ~S"
                att
                (* 1.0 (/ (gd-test-results-true-positives tr) total))
                (* 1.0 (/ (gd-test-results-true-negatives tr) total))
                (* 1.0 (/ (gd-test-results-false-positives tr) total))
                (* 1.0 (/ (gd-test-results-false-negatives tr) total)))
        )
      )
    )
  t)

; Print a variable name and number
(defun print-svariable (v &optional stream)
  (unless stream (setq stream trace-stream))
  (format stream "~S(~S)" (svariable-name v) (stype-name (svariable-type v)))
  )

; Print a value in a function (either constant or weight)
(defun print-function-value (v &optional stream)
  (unless stream (setq stream trace-stream))
  (format stream (if (integerp v)
                         "~D"
                       (if (and (>= (abs v) .001) (< (abs v) 1000)) "~F"
                         "~G")) v)
  t)

; Print dimension d (variable v)
; Weight determines if print weight
(defun print-dimension (d-min d-max &optional v symbolic stream printvs weight)
  (unless stream (setq stream trace-stream))
  (let (type)
    (when printvs (format stream " "))
    ; Deal with either a single variable or a list of them
    (cond ((null v))
          ((listp v)
           (setq type (svariable-type (car v)))
           (when printvs (print-svariables v stream)))
          (t
           (setq type (svariable-type v))
           (when printvs (print-svariable v stream))
           )
          )
    (cond ((and symbolic type
                (not (< d-min (stype-min type))) (not (> d-max (stype-max type))) ; This can happen during traces of affine transformations
                )
           (cond ((dimension-full-scope d-min d-max type)
                  (format stream "[*>")
                  )
                 ((and (stype-numeric type)
                       (stype-discrete type)
                       )
                  (if (e= d-max (+ d-min 1) t)
                      (format stream "[~S]" (if center-discrete-numeric-on-integer (+ d-min 1/2) d-min))
                    (if center-discrete-numeric-on-integer
                        (format stream "[~S ~S>" (+ d-min 1/2) (+ d-max 1/2))
                      (format stream "[~S ~S>" d-min d-max))
                    )
                  )
                 ((not (stype-numeric type))
                  (format stream "[")
                  (cond ((e= d-max (+ d-min 1) t) ; Dimension only includes one constant
                         (format stream "~S" (get-symbol-name d-min type))
                         )
                        (t ; Dimension includes two or more constants
                         (do ((cd d-min (+ cd 1)))
                             ((e= cd d-max t))
                           (format stream "~S" (get-symbol-name cd type))
                           ; Print a space if there are more to come
                           (unless (e= (+ cd 1) d-max t) (format stream " "))
                           )
                         )
                        )
                  (format stream "]")
                  )
                 (t
                  (format stream "[~S ~S>" d-min d-max)
                  )
                 ))
          (t
           (format stream "[~S ~S>" d-min d-max)
           )
          )
    (when (and weight
               (or (not symbolic)
                   (and (numberp weight) (not (e= weight 0)))))
      (format stream ":")
      (print-function-value weight stream)
      )
    nil)
  )

; Print maximals
(defun print-maximals (ms v &optional symbolic stream)
  (unless stream (setq stream trace-stream))
    (format stream "<")
    (unless ms
      (format stream "NIL")
      )
    (dolist (m ms)
      (if symbolic
          (print-dimension (car m) (cadr m) v symbolic stream nil)
        (format stream "~S" ms)
        )
      )
    (format stream ">")
    )

; Print a region
(defun print-region (r p &optional symbolic stream piecewise-constant)
  (unless stream (setq stream trace-stream))
  (let ((vs (plm-variables p))
        d v
        (msv (region-maximals r))
        ms
        (rc (region-constant r))
        )
    (when (or ;(not symbolic)
              (and (or trace-empty
                       (not (region-e-empty r piecewise-constant)))
                   (or trace-full
                       (not (region-e-full r piecewise-constant)))
                   (or (not piecewise-constant)
                       (not trace-threshold)
                       (>= (region-constant r) trace-threshold))
                   ))
      (when print-regions-on-separate-lines
        (format stream "~%")
        )
      (format stream "(")
      (when (region-exponential r)
        (format stream "E ")
        )
      (print-function-value rc stream)
      (when (region-evidence r)
        (format stream "@"))
      (format stream ":")
      (dotimes (i (region-rank r))
        (setq d (dimension r i))
        (setq v (aref vs i))
        (when (or (not symbolic)
                  (dimension-active p i)
                  trace-maximals)
          (print-dimension (dimension-min d) (dimension-max d) v symbolic stream t (dimension-weight d))
          (setq ms (aref msv i))
          (when (and trace-maximals ms)
            (print-maximals ms v symbolic stream)
            )
          )
        )
      (format stream ") ")
      )
    nil)
  )

; Print the slices in a PLM
(defun print-plm-slices (p &optional symbolic stream)
  (unless stream (setq stream trace-stream))
  (let (vars rank rank-1)
    (setq rank (plm-rank p))
    (setq rank-1 (- rank 1))
    (dotimes (i rank)
      (when (aref (plm-slices p) i)
        (setq vars (aref (plm-variables p) i))
        (if (listp vars)
            (do ((vl vars (cdr vl)))
                ((null vl))
              (format stream "~S"
                      (svariable-name (car vl))
                      )
              (unless (null (cdr vl)) (format stream "*"))
              )
          (format stream "~S"
                      (svariable-name vars)
                      )
          )
        (unless symbolic
          (format stream ": ~S" (sort-slice-list (aref (plm-slices p) i)))
          )
        (when (< i rank-1) (format stream ", "))
        )
      )
    )
  )

; Print one PLM
(defun print-one-plm (p &optional symbolic stream limits)
  (unless stream (setq stream trace-stream))
  (let (limits-v r
        (rarray (plm-array p))
        (piecewise-constant (plm-piecewise-constant p)))
    (setq limits-v (compute-limits limits (plm-variables p)))
    (unless (eq (type-of p) 'plm) (error "Attempt to print an object of type ~S as a PLM." (type-of p)))
    (cond ((not (eq (type-of p) 'plm)) (format stream "~S" p)) ; Handles deltas and other things
          (p
           (unless symbolic ; Print slices when not symbolic
             (print-plm-slices p symbolic stream)
             (format stream "~&")
             )
           (dotimes (i (array-total-size rarray))
             (setq r (row-major-aref rarray i))
;	     (format t "~%")
             (when (region-within-limits r limits-v)
               (print-region r p symbolic stream piecewise-constant)
               )
             )
           )
          )
    nil)
  )

; Print PLM
(defun print-plm (p &optional symbolic stream limits)
  (unless stream (setq stream trace-stream))
  (let (newp)
    (setq newp (find-plm p))
    (unless newp
      (error "Could not identify PLM to print in PRINT-PLM.")
      )
    (if (listp newp)
        (dolist (np newp)
          (case (car np)
            ((wm)
             (format stream "WM function for predicate ~S:~&" p)
             (print-one-plm (cdr np) symbolic stream limits)
             (format stream "~& ~%")
             )
            ((pfun)
             (format stream "Predicate function for predicate ~S:~&" p)
             (print-one-plm (cdr np) symbolic stream limits)
             (format stream "~& ~%")
             )
            ((cfun)
             (format stream "Conditional function for conditional ~S:~&" p)
             (print-one-plm (cdr np) symbolic stream limits)
             (format stream "~& ~%")
             )
            )
          )
      (print-one-plm newp symbolic stream limits))
    nil)
  )
; Short cut for symbolic printing of a plm
(defun pplm (p) (print-plm p t))

; String of spaces
(defun space-string (n)
  (make-string n :initial-element '#\Space)
  )

; Print centered in field
; no longer needed???
(defun print-centered (x max-field &optional stream)
  (unless stream (setq stream trace-stream))
  (let* ((field (length (princ-to-string x)))
        (pad (/ (- max-field field) 2))
        )
    (if (< max-field field)
        (error "Attempt to print ~S in field of width ~S" x max-field)
      (format stream "~A~S~A" (space-string (floor pad)) x (space-string (ceiling pad)))
      )
    t)
  )

; Print right justified in field
(defun print-right-justified (x max-field &optional stream)
  (unless stream (setq stream trace-stream))
  (let* ((field (length (if (stringp x) x (princ-to-string x))))
        (pad (- max-field field))
        )
    (if (< max-field field)
;        (error "Attempt to print ~S in field of width ~S" x max-field)
	(setf max-field field)
      (progn
        (format stream "~A" (space-string pad))
        (if (stringp x)
            (format stream x)
          (format stream "~S" x))
        )
      )
    t)
  )

; Find length of label with longest name in type
(defun longest-label (vt)
  (let ((pm (stype-min vt))
        (field-max 0))
    (if (stype-numeric vt)
        (setq field-max (max (length (princ-to-string (if center-discrete-numeric-on-integer (+ (stype-min vt) 1/2) (stype-min vt))))
                             (length (princ-to-string (if center-discrete-numeric-on-integer (+ (stype-max vt) 1/2) (stype-max vt))))
                             ))
      (dotimes (i (- (stype-max vt) pm))
        (setq field-max (max field-max (length (princ-to-string (nth (+ i pm) (stype-constants vt))))))
        )
      )
    field-max)
  )

; Compute a label
(defun compute-label (name vt)
  (if (stype-numeric vt)
      (when center-discrete-numeric-on-integer
        (setq name (+ name 1/2))
        )
    (setq name (nth name (stype-constants vt)))
    )
  name)

; Compute a label string
(defun compute-label-string (name vt)
  (princ-to-string (compute-label name vt))
  )

; Print a label
(defun print-label (name vt field-max &optional dash stream)
  (unless stream (setq stream trace-stream))
  (print-right-justified (concatenate 'string "[" (compute-label-string name vt) (if dash ":>" "]")) field-max stream)
  )


; Define a range label over two values
(defun range-label (a b vt)
  (concatenate 'string '"[" (compute-label-string a vt) (if (stype-discrete vt) ":" "-") (compute-label-string b vt) ">")
  )

; Print labels along X axis
(defun print-x-headers (rarray ar vt index &optional pad ranges stream)
  (unless stream (setq stream trace-stream))
  (let* (field-max rs)
    (setq field-max (longest-label vt))
    (setq field-max (if ranges
                        (+ (* 2 field-max) range-field-beyond-label) ; 2 for brackets, 1 for separator
                      (+ field-max field-beyond-label))) ; 2 for brackets, 1 for space and 1 for possibility of dash
    (setq field-max (max minimal-parray-field field-max))
    (when pad
      (format stream (space-string pad))
      )
    (do ((r ar (region-after-old r index rarray)))
        ((null r))
      (setq rs (region-span r index))
      (cond (ranges
             (print-right-justified (range-label (region-min r index) (region-max r index) vt) field-max stream)
             )
            ((and (null (region-after-old r index rarray))
                  (stype-numeric vt)
                  (> rs max-final-discrete)
                  )
             (print-label (region-min r index) vt field-max t stream)
             )
            (t
             (let ((rm (region-min r index)))
               (dotimes (i rs)
                 (print-label (+ i rm) vt field-max nil stream)
                 )
               )
             )
          )
        )
    (format stream "~&")
    field-max)
  )

; Print a 0D function with an argmax
(defun parray-print-0d (p argmax argmax-type &optional stream)
  (unless stream (setq stream trace-stream))
  (let ((r (plm-origin p)))
    (unless (region-e-empty r)
      (format stream "~S" (compute-label (caar (aref (region-maximals r) argmax)) argmax-type))
      )
    )
  )

; Print across one dimension of a PLM as an array (possibly of regions), starting from axial region
; Ignores dimension weights if they exist
(defun parray-print-1d (rarray ar index vt field-size &optional regions argmax argmax-type stream)
  (unless stream (setq stream trace-stream))
  (let (format-string rc rs)
    (do ((r ar (region-after-old r index rarray)))
        ((null r))
      (setq rc (if argmax
                   (if (region-e-empty r)
                       ""
                     (compute-label (caar (aref (region-maximals r) argmax)) argmax-type))
                 (if (region-is-variable r)
                     '*linear*
                   (if (= (region-constant r) 0) ; Deal with bad formatting for 0.0
                       0
                     (region-constant r)))))
      (if (numberp rc)
        (setq format-string (concatenate 'string "~" (princ-to-string field-size)
                                         (if (integerp rc)
                                             "D"
                                           (if (and (< rc 100) (> rc .01))
                                               "F"
                                             "G"))
                                         )
              )
        (setq format-string nil)                                      
        )
      (setq rs (region-span r index))
      (if (and (stype-numeric vt) ; Never use regions for symbolic variables
               (or regions
                   (not (stype-discrete vt)) ; Always use regions with continuous variables
                   (and (null (region-after-old r index rarray)) ; Use region for terminal region if large
                        (> rs max-final-discrete))
                   )
               )
          (if format-string
              (format stream format-string rc)
            (print-right-justified rc field-size stream))
        (dotimes (i rs)
          (if format-string
              (format stream format-string rc)
            (print-right-justified rc field-size stream))
          )
        )
      )
    t)
  )

; Print a 1D function as an array
; Ignores dimension weights if they exist
(defun parray-1d (p &optional regions argmax argmax-type stream)
  (unless stream (setq stream trace-stream))
  (let ((as (plm-active p))
         index field-size v vt)
    (unless (= (active-variables as) 1)
      (error "Attempt to print as an array a 1D function that doesn't have exactly one active dimension: ~S" p)
      )
    (setq index (position t (plm-active p))) ; Index of first active dimension
    (setq v (aref (plm-variables p) index))
    (setq vt (svariable-type v)) 
    (format stream "~&      ~S:~&" (svariable-name v))
    (setq field-size (print-x-headers (plm-array p) (plm-origin p) vt index nil (and (or regions (not (stype-discrete vt))) (stype-numeric vt)) stream))
    (parray-print-1d (plm-array p) (plm-origin p) index (svariable-type v) field-size regions argmax argmax-type stream)
    t)
  )

; Print across two dimensions of a PLM as an array, starting from axial region
; Ignores dimension weights if they exist
(defun parray-print-2d (rarray ar index1 index2 v1t v2t &optional regions argmax argmax-type stream)
  (unless stream (setq stream trace-stream))
  (let (y-size rm2 rs2 field-size)
    (setq y-size (longest-label v2t))
    (setq field-size  (print-x-headers rarray ar v1t index1
                                       (if (or regions (not (stype-discrete v2t)))
                                           (+ (* 2 y-size) range-field-beyond-label)
                                         (+ y-size field-beyond-label))
                                       (and (or regions (not (stype-discrete v1t))) (stype-numeric v1t)) stream))
    (do ((r ar (region-after-old r index2 rarray)))
        ((null r))
      (setq rs2 (region-span r index2))
      (setq rm2 (region-min r index2))
      (if (and (stype-numeric v2t) ; Never use nregions for symbolic variables
               (or regions
                   (not (stype-discrete v2t)) ; Always use regions with continuous variables
                   (and (null (region-after-old r index2 rarray)) ; Use region for terminal region if large
                        (> rs2 max-final-discrete))
                   )
               )
          (progn
            (if (or regions (not (stype-discrete v2t)))
		(print-right-justified (range-label rm2 (region-max r index2) v2t) (+ (* 2 y-size) range-field-beyond-label) stream)
              (print-label rm2 v2t (+ y-size field-beyond-label) t stream))
            (parray-print-1d rarray r index1 v1t field-size regions argmax argmax-type stream)
            (format stream "~&")
            )
	  (dotimes (i (floor rs2))
	    (print-label (+ rm2 i) v2t (+ y-size field-beyond-label) nil stream)
	    (parray-print-1d rarray r index1 v1t field-size regions argmax argmax-type stream)
	    (format stream "~&") 
	    ) 
	  )
      )
    t)
  )

; Print a 2D function as an array
; Ignores dimension weights if they exist
(defun parray-2d (p &optional regions argmax argmax-type stream)
  (unless stream (setq stream trace-stream))
  (let ((as (plm-active p))
         index1 index2 v1 v2 v1t v2t)
    (setq index1 (position t (plm-active p))) ; Index of first active dimension
    (setq index2 (position t (plm-active p) :start (+ index1 1))) ; Index of second active dimension
    (unless (= (active-variables as) 2)
      (error "Attempt to print a 2D function as an array that doesn't have exactly two active dimensions: ~S" p)
      )
    (setq v1 (aref (plm-variables p) index1))
    (setq v1t (svariable-type v1))
    (setq v2 (aref (plm-variables p) index2))
    (setq v2t (svariable-type v2))
    (format stream "~&      ~S x ~S:~&" (svariable-name v1) (svariable-name v2))
    (parray-print-2d (plm-array p) (plm-origin p) index1 index2 v1t v2t regions argmax argmax-type stream)
    t)
  )

; Print across three dimensions of a PLM as an array, starting from axial region
; Iterates over first dimension
; Ignores dimension weights if they exist
(defun parray-print-3d-1 (p index1 index2 index3 v1t v2t v3t &optional regions argmax argmax-type stream)
  (unless stream (setq stream trace-stream))
  (let (y-size rm1 rs1 ar)
    (setf ar (plm-origin p))
    (setq y-size (longest-label v1t))
    (do ((r ar (region-after-old r index1 (plm-array p))))
        ((null r))
      (setq rs1 (region-span r index1))
      (setq rm1 (region-min r index1))

      (if (and (stype-numeric v1t) ; Never use regions for symbolic variables

               (or regions
                   (not (stype-discrete v1t)) ; Always use regions with continuous variables
                   (and (null (region-after-old r index1 (plm-array p))) ; Use region for terminal region if large
                       (> rs1 max-final-discrete))
                   )
               )
          (progn
            (format stream "~%")
            (print-right-justified (range-label rm1 (region-max r index1) v1t) (+ (* 2 y-size) range-field-beyond-label) stream)
            (format stream "~&")
            (parray-print-2d (plm-array p) r index2 index3 v2t v3t regions argmax argmax-type stream)
            (format stream "~&")
            )

        (dotimes (i rs1)
          (format stream "~%")
          (print-label (+ rm1 i) v1t (+ y-size field-beyond-label) nil stream)
          (format stream "~&")
          (parray-print-2d (plm-array p) r index2 index3 v2t v3t regions argmax argmax-type stream)
          (format stream "~&")
          )
        )
      )
    t)
  )
(defun parray-print-4d-1 (p index1 index2 index3 index4 v1t v2t v3t v4t &optional regions argmax argmax-type stream)
  (unless stream (setq stream trace-stream))
  (let (y-size rm1 rs1 ar)
    (setf ar (plm-origin p))
    (setq y-size (longest-label v1t))
    (do ((r ar (region-after-old r index1 (plm-array p))))
        ((null r))
      (setq rs1 (region-span r index1))
      (setq rm1 (region-min r index1))

      (if (and (stype-numeric v1t) ; Never use regions for symbolic variables

               (or regions
                   (not (stype-discrete v1t)) ; Always use regions with continuous variables
                   (and (null (region-after-old r index1 (plm-array p))) ; Use region for terminal region if large
                       (> rs1 max-final-discrete))
                   )
               )
          (progn
            (format stream "~%")
            (print-right-justified (range-label rm1 (region-max r index1) v1t) (+ (* 2 y-size) range-field-beyond-label) stream)
            (format stream "~&")
            (parray-print-3d-1 p index2 index3 index4 v2t v3t v4t regions argmax argmax-type stream)
            (format stream "~&")
            )

        (dotimes (i rs1)
          (format stream "~%")
          (print-label (+ rm1 i) v1t (+ y-size field-beyond-label) nil stream)
          (format stream "~&")
          (parray-print-3d-1 p index2 index3 index4 v2t v3t v4t regions argmax argmax-type stream)
          (format stream "~&")
          )
        )
      )
    t)
  )

; Print a 3D function as an array
; Ignores dimension weights if they exist
(defun parray-3d (p &optional regions argmax argmax-type stream)
  (unless stream (setq stream trace-stream))
  (let ((as (plm-active p))
         index1 index2 index3 v1 v2 v3 v1t v2t v3t uniquevars universalvars pvs allvars thisvar)
    (unless (= (active-variables as) 3)
      (error "Attempt to print a 3D function as an array that doesn't have exactly three active dimensions: ~S" p)
      )
    (setq pvs (plm-variables p))
    (dotimes (i (length pvs))
      (setf thisvar (cons (aref pvs i) i))
      (if (aref (plm-active p) i) ;if it's active
	  (if (multiple-variable (aref pvs i)) ;if it's universal
	      (progn 
	      (setf universalvars (cons thisvar universalvars)))
	     (setf uniquevars (cons thisvar uniquevars)))
	      )
	  )
    (setf allvars (append universalvars uniquevars))

    (setq index1 (cdr (nth 0 allvars))) ; Index of first active dimension
    (setq index2 (cdr (nth 1 allvars))) ; Index of 2nd active dimension
    (setq index3 (cdr (nth 2 allvars))) ; Index of 3rd active dimension

    (setq v1 (car (nth 0 allvars)))
    (setq v2 (car (nth 1 allvars)))
    (setq v3 (car (nth 2 allvars)))

    (setq v1t (svariable-type v1))
    (setq v2t (svariable-type v2))
    (setq v3t (svariable-type v3))
    (format stream "~&      ~S x [~S x ~S]:~&" (svariable-name v1) (svariable-name v2) (svariable-name v3))
    (parray-print-3d-1 p index1 index2 index3 v1t v2t v3t regions argmax argmax-type stream)
    t)
)

(defun parray-4d (p &optional regions argmax argmax-type stream)
  (unless stream (setq stream trace-stream))
  (let ((as (plm-active p))
         index1 index2 index3 index4 v1 v2 v3 v4 v1t v2t v3t v4t)
    (unless (= (active-variables as) 4)
      (error "Attempt to print a 4D function as an array that doesn't have exactly 4 active dimensions: ~S" p)
      )
    (setq index1 (position t (plm-active p))) ; Index of first active dimension
    (setq index2 (position t (plm-active p) :start (+ index1 1))) ; Index of second active dimension
    (setq index3 (position t (plm-active p) :start (+ index2 1))) ; Index of second active dimension
    (setq index4 (position t (plm-active p) :start (+ index3 1))) ; Index of second active dimension
    (setq v1 (aref (plm-variables p) index1))
    (setq v1t (svariable-type v1))
    (setq v2 (aref (plm-variables p) index2))
    (setq v2t (svariable-type v2))
    (setq v3 (aref (plm-variables p) index3))
    (setq v3t (svariable-type v3))
    (setq v4 (aref (plm-variables p) index4))
    (setq v4t (svariable-type v4))
    (format stream "~&  ~S x    ~S x [~S x ~S]:~&" (svariable-name v1) (svariable-name v2) (svariable-name v3) (svariable-name v4))
    (parray-print-4d-1 p index1 index2 index3 index4 v1t v2t v3t v4t regions argmax argmax-type stream)
    t)
  )

; print plm of any dimension as an array
(defun parray-all (p &optional regions argmax argmax-type stream)
  (unless stream (setq stream trace-stream))
  (let (index1 index2 v1 v2 uniquevars universalvars pvs allvars thisvar nvars currvar1 currvar2)
    (setq pvs (plm-variables p))
    (dotimes (i (length pvs))
      (setf thisvar (cons (aref pvs i) i))
      (if (string= (svariable-name (car thisvar)) "WM-STATE") ; it's sate save for later
	  (setf universalvars (cons thisvar universalvars))
	 (progn
	   (if (aref (plm-active p) i) ;if it's active
	       (if (multiple-variable (aref pvs i)) ;if it's universal
		   (setf universalvars (cons thisvar universalvars))
		  (setf uniquevars (cons thisvar uniquevars)))
	       ))
	   ))
    (setf allvars (append universalvars uniquevars))
    (setf nvars (length allvars))
    (dotimes (i (- nvars 3))
      (if (> (length allvars) 2)
	 (progn  
	   (setq currvar1 (pop allvars))
	   (setq v1 (car currvar1))
	   (setq index1 (cdr currvar1))
	   (setq currvar2 (pop allvars))
	   (setq v2 (car currvar2))
	   (setq index2 (cdr currvar2))
	   (format t "~&~%[ ~S x ~S ] ~&" (svariable-name v1) (svariable-name v2))
	   (parray-print-all p (plm-array p) (plm-origin p) index1 index2 v1 v2 allvars regions argmax argmax-type stream)
	   (format t "~%"))))))

(defun parray-print-all (p rarray ar index1 index2 v1 v2 restvars &optional regions argmax argmax-type stream)
  (unless stream (setq stream trace-stream))
  (let (y-size rm rs v vt index vname v3cons v3 index3 currvar)
  (if (= (length restvars) 1)
      (progn
	(setf v3cons (pop restvars))
	(setf v3 (car v3cons))
	(setf index3 (cdr v3cons))
	(parray-print-3d-1 p index1 index2 index3 (svariable-type v1) (svariable-type v2) (svariable-type v3) regions argmax argmax-type stream)
	(format stream "~%"))
      (progn 
	  (setq currvar (pop restvars))
	  (setq v (car currvar))
	  (setq vt (svariable-type v))
	  (setq index (cdr currvar))
	  (setq vname (svariable-name v))
	  (setq y-size (longest-label vt))
	  (do ((r ar (region-after-old r index rarray)))
	      ((null r))
	    (setq rs (region-span r index))
	    (setq rm (region-min r index))
	    (if (and (stype-numeric vt) ; Never use regions for symbolic variables
		     (or regions
			 (not (stype-discrete vt)) ; Always use regions with continuous variables
			 (and (null (region-after-old r index rarray)) ; Use region for terminal region if large
			      (> rs max-final-discrete))
		   )
		     )
		(progn
		  (format stream "~%~S" vname)
		  (print-right-justified (range-label rm (region-max r index) vt) (+ (* 2 y-size) range-field-beyond-label) stream)
		  (format stream "~%") ; printing at end compression
		  (parray-print-all p rarray r index1 index2 v1 v2 restvars regions argmax argmax-type stream) ;; SHOULD ORDER BE REVERSED HERE??
		  (format stream "~&~%")
		  )
		
	  (dotimes (i rs)
	    (format stream "~S" vname)
	    (print-label (+ rm i) vt (+ y-size field-beyond-label) nil stream)
	    (format stream "~&")
	    (parray-print-all p rarray r index1 index2 v1 v2 restvars regions argmax argmax-type stream)
	    (format stream "~&"))))))))

; Print one 1,2 or 3D PLM as an array
(defun parray-one (p &optional regions argmax argmax-type stream)
   (unless stream (setq stream trace-stream))
  (setf (plm-slices p) (index-slice-list-array (plm-slices p))) ; Added for region arrays at present
  (case (active-variables (plm-active p))
    ((0) (if argmax
             (parray-print-0d p argmax argmax-type stream)
           (format stream "~S" (region-constant (plm-origin p))))
;           (error "Attempt to print as an array a function that isn't 1-3 dimensions: ~S" p))
     )
    ((1) (parray-1d p regions argmax argmax-type stream))
    ((2) (parray-2d p regions argmax argmax-type stream))
    ((3) (parray-3d p regions argmax argmax-type stream))
    (t (parray-all p))
    )
  )


; Print a 1,2 or 3D PLM as an array
(defun parray (p &optional regions summarize stream)
  (unless stream (setq stream trace-stream))
  ; Handle fact that if summarize is given as 'array should just ignore it
  (when (eq summarize 'array)
    (setq summarize nil)
    )
  (let (newp argmax argmax-type cls)
    (setq newp (find-plm p summarize))
    (unless newp
      (error "Could not identify PLM to print in PRINT-PLM.")
      )
    (if (listp newp)
        (dolist (np newp)
          ; Determine argmax dimension if there is one
          (when (and summarize (listp summarize))
            (setq cls (car (last summarize)))
            (when (and (= (length cls) 2)
                       (eq (car cls) 'argmax))
              (setq argmax (position (cadr cls) (plm-variables (cdr np)) :key #'svariable-name))
              (setq argmax-type (svariable-type (aref (plm-variables (cdr np)) argmax)))
              )
            )
          (case (car np)
            ((wm)
             (format stream "WM function for predicate ~S:~&" p)
             (parray-one (cdr np) (if (plm-uniform (cdr np)) t regions) argmax argmax-type stream)
             (format stream "~& ~%")
             )
            ((pfun)
             (format stream "Predicate function for predicate ~S:~&" p)
             (parray-one (cdr np) (if (plm-uniform (cdr np)) t regions) argmax argmax-type stream)
             (format stream "~& ~%")
             )
            ((cfun)
             (format stream "Conditional function for conditional ~S:~&" p)
             (parray-one (cdr np) (if (plm-uniform (cdr np)) t regions) argmax argmax-type stream)
             (format stream "~& ~%")
             )
            )
          )
      (progn
        ; Determine argmax dimension if there is one
        (when (and summarize (listp summarize))
          (setq cls (car (last summarize)))
          (when (and (= (length cls) 2)
                     (eq (car cls) 'argmax))
            (setq argmax (position (cadr cls) (plm-variables newp) :key #'svariable-name))
            (setq argmax-type (svariable-type (aref (plm-variables newp) argmax)))
            )
          )
        (parray-one newp regions argmax argmax-type stream)
        ))
      nil)
  )
(defun pa (p &optional regions summarize stream)
  (parray p regions summarize (if stream stream trace-stream))
  )

; Print a vector of variables
(defun print-svariables (vs &optional stream)
  (unless stream (setq stream trace-stream))
  (let ((rank (length vs)))
    (dotimes (i rank)
      (print-svariable (elt vs i) stream)
      (when (< i (- rank 1))
        (format stream "*"))
      )
    )
  )


; Print a single factor step
(defun print-factor-step (fs n &optional stream)
  (unless stream (setq stream trace-stream))
  (format stream "~S" (factor-step-type fs))
  (if (eq (factor-step-type fs) 'product)
      (format stream "(~A)" (node-name (link-var-node (factor-step-argument fs))))
    (format stream "(~S)" (elt (variable-names (node-variables n)) (factor-step-argument fs)))
    )
  )

; Print the factor steps for a node
(defun print-factor-steps (n &optional stream)
  (unless stream (setq stream trace-stream))
;  (format stream "~&~%")
  (format stream "~&  FACTOR STEPS: ")
  (do ((fs-l (node-factor-steps n) (cdr fs-l)))
      ((null fs-l))
    (print-factor-step (car fs-l) n stream)
    (when (cdr fs-l) (format stream ", "))
    )
  nil)

; Print the neighbors of a node
(defun print-neighbors (n &optional stream)
  (unless stream (setq stream trace-stream))
  (let ((varp (variable-nodep n)))
    (format stream "  NEIGHBORS: ")
    (do ((link-l (node-links n) (cdr link-l)))
        ((null link-l))
      (if varp
          (format stream "~S" (node-name (link-fact-node (car link-l))))
        (format stream "~S" (node-name (link-var-node (car link-l))))
        )
      (when (cdr link-l) (format stream ", "))
      )
    nil)
  )

; Print node type information
(defun print-node-type (n &optional stream)
  (unless stream (setq stream trace-stream))
  (format stream "~&~S = " (node-name n))
  (when (node-type n)
    (format stream "Type: ~S; " (node-type n)))
  (when (node-subtype n)
    (format stream "Subtype: ~S; " (node-subtype n)))
  (when (node-subsubtype n)
    (format stream "Subsubtype: ~S; " (node-subsubtype n)))
  (format stream "Action: ~S" (node-action n))
  t)
(defun pnt (node-number)
  (print-node-type (node-from-number node-number))
  )

(defun save-node-functions (fname)
  (with-open-file (str fname
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
  (dolist (n (graph-nodes cg))
    (when (node-function n)
      (format str "~&~%~A::::" (node-name n))
      (format str " ~S ~% " (node-function n))))))

; Print node based on name or number
(defun pn (id &optional symbolic stream)
  (if (numberp id)
      (print-node (node-from-number id) symbolic stream)
    (print-node (node-from-name id) symbolic stream)))

; Print the messages coming into a node
(defun print-in-messages (n &optional symbolic stream)
  (let* ((ni (if (variable-nodep n) fact-index var-index)))
    (dolist (l (node-links n))
      (print-message "  " l ni symbolic t nil stream)
      )
    nil)
  )

; Short cut for printing incoming messages from node name symbolically to listener
(defun pim (n-id)
  (print-in-messages (if (numberp n-id) (node-from-number n-id) (node-from-name n-id)) t t))

; Print the messages going out of a node
(defun print-out-messages (n &optional symbolic stream)
  (let* ((ni (if (variable-nodep n) var-index fact-index)))
    (dolist (l (node-links n))
      (print-message "  " l ni symbolic nil nil stream)
      )
    nil)
  )

; Short cut for printing outgoing messages from node name symbolically to listener
(defun pom (n-id)
  (print-out-messages (if (numberp n-id) (node-from-number n-id) (node-from-name n-id)) t t))

; Print a link
(defun print-link (l &optional symbolic stream direction)
  (unless stream (setq stream trace-stream))
  (unless (graph-initialized cg)
    (init-graph)
    )
  (unless direction
    (format stream "~&~%<LINK>")
    )
  (when (and (or (not direction) ; If no direction, print in both directions
                 (equal direction 'from-variable))
             (link-var-content l)) ; Only print if link active in direction
    (print-message nil l var-index symbolic t stream)
    )
  (when (and (or (not direction) ; If no direction, print in both directions
                 (equal direction 'from-factor))
             (link-fact-content l)) ; Only print if link active in direction
    (print-message nil l fact-index symbolic t stream)
    )
  nil)

; Print preferences (messages out of and into selected FAN and FNAN nodes)
(defun print-preferences nil
  (unless (graph-initialized cg)
    (init-graph)
    )
  (unless (predicate-from-name 'selected t)
    (error "No selection predicate defined, so can't print preferences.")
    )
  (dolist (n (graph-positive-preferences cg)) ; Print positive preferences
    (format trace-stream "~%Preferences based on ~S:" (node-name n))
    (dolist (l (node-links n)) ; Outgoing messages
      (when (eq (node-subsubtype (aref (link-nodes l) var-index)) 'positive)
        (print-message "  " l fact-index t nil nil trace-stream)
        )
      )
    (dolist (l (node-links n)) ; Incoming messages
      (print-message "      " l var-index t t nil trace-stream)
      )
    )
  (when (graph-negative-preferences cg)
        (format trace-stream "~&~%Negative preferences:")
        )
  (dolist (n (graph-negative-preferences cg)) ; Print negative preferences
    (dolist (l (node-links n)) ; Outgoing messages
      (print-message "      " l fact-index t t nil trace-stream)
      )
    )
  )
(defun pprefs nil (print-preferences))

; Print a message based on node numbers
(defun pm (nn1 nn2 &optional symbolic stream)
  (unless stream (setq stream trace-stream))
  (let* ((n1 (node-from-number nn1))
         (n2 (node-from-number nn2))
         (l (link-from-nodes n1 n2))
        )
    (print-link l symbolic stream
                (if (variable-nodep n1)
                    'from-variable
                  'from-factor))
    )
  )

; Print factor function
(defun print-factor-function (n-name &optional symbolic stream)
  (let (n)
    (setq n (node-from-name n-name 'factor))
    (print-plm (node-function n) symbolic stream)
    )
  )

; Print queue contents
(defun print-queue-contents (q symbolic stream &optional head)
  (let ((count (queue-length q)))
    (when head
      (if (= (first head) (second head))
          (format stream "~&[~S] " (first head))
        (format stream "~&[~S,~S] " (first head) (second head))
        )
      )
    (dolist (m (cdr (queue-head q)))
      (format stream "~&")
      (print-message (if trace-wm-driven (format nil "{~S} " (message-wm-driven m)) "")
                     (message-link m) (message-index m) symbolic t t stream)
      )
    (if (eq count 1)
        (format stream "~&**** ~S message in queue." count)
      (format stream "~&**** ~S messages in queue." count)
      )
    )
  nil)

; Print a queue
(defun print-queue (q symbolic stream)
  (format stream "~&~%<QUEUE (~S: ~S)>~&~%" q
          (cond ((= q depth-queues-index) '"DEPTH MESSAGES")
                (t '"OTHER MESSAGES")
                )
          )
  (if (= q depth-queues-index)
      (let ((dqs (aref (graph-queues cg) depth-queues-index)))
        (dotimes (i (length dqs))
          (format stream "~&~%DEPTH: ~S" i)
          (print-queue-contents (aref dqs i) symbolic stream (car (queue-head (aref dqs i))))
          )
        )
    (print-queue-contents (aref (graph-queues cg) q) symbolic stream)
    )
  nil)

; Short hand for printing a queue
(defun pq (q) (print-queue q t trace-stream))

; Print all queues
(defun print-queues (&optional symbolic stream)
  (unless stream (setq stream trace-stream))
  (dotimes (i (length (graph-queues cg)))
    (print-queue i symbolic stream)
    )
  )

; Short hand for printing queues
(defun pqs nil (print-queues t trace-stream))

; Print variable nodes
(defun print-variable-nodes (&optional symbolic stream)
  (dolist (n (reverse (graph-nodes cg)))
    (when (variable-nodep n)
      (print-node n symbolic stream))))
(defun pvns nil
  (print-variable-nodes t trace-stream))

; Print factor nodes
(defun print-factor-nodes (&optional symbolic stream)
  (dolist (n (reverse (graph-nodes cg)))
    (when (factor-nodep n)
      (print-node n symbolic stream))))
(defun pfns nil
  (print-factor-nodes t trace-stream))

; Print all nodes
(defun print-nodes (&optional symbolic stream)
  (dolist (n (reverse (graph-nodes cg)))
    (print-node n symbolic stream)))
(defun pns nil
  (print-nodes t trace-stream))

; Print alpha memories
(defun print-alpha-memories (&optional conditional-name symbolic stream)
  (unless (graph-initialized cg)
    (init-graph)
    )
  (let ((c (conditional-from-name conditional-name)))
    (dolist (n (if conditional-name
                   (if c
                       (reverse (conditional-alpha-memories (conditional-from-name conditional-name)))
                     (error "No conditional named ~S." conditional-name))
                 (graph-nodes cg))
               )
      (when (and (variable-nodep n)
                 (eq (node-subtype n) 'alpha))
        (case (node-subsubtype n)
          ((condition)
           (format stream "~&<-~A: " (node-name n))
           (print-plm (variable-posterior n) symbolic stream) ; Only one message in, so posterior is this
           )
          ((action)
               (format stream "~&->~A: " (node-name n))
               (print-plm (variable-posterior n) symbolic stream) ; Only one message out, so posterior is this
               )
          ((condact)
           (dolist (l (node-links n))
             (format stream "~&~A->~S: " (node-name n) (node-name (aref (link-nodes l) fact-index)))
             (print-plm (aref (link-contents l) var-index) symbolic stream) ; Only one message out, so posterior is this
             )
           )
          )
        )
      )
    )
  )
(defun pam (&optional conditional-name)
  (print-alpha-memories conditional-name t trace-stream))

; Print a type
(defun print-type (type &optional stream)
  (unless stream (setq stream trace-stream))
  (let (variety)
    (setq variety (if (stype-numeric type)
                      (if (stype-discrete type) 'DISCRETE 'CONTINUOUS)
                    'SYMBOLIC))
    (format stream "~&~S: " (stype-name type))
    (if (eq variety 'SYMBOLIC)
        (format stream "~S" (stype-constants type))
      (if (eq variety 'DISCRETE)
          (if center-discrete-numeric-on-integer
              (format stream "[~S:~S>" (+ (stype-min type) 1/2) (+ (stype-max type) 1/2))
            (format stream "[~S:~S>" (stype-min type) (stype-max type))
            )
        (format stream "[~S,~S>" (stype-min type) (stype-max type))
        )
      )
    )
  t)
(defun pt (type) (print-type type))

; Print type from name
(defun print-type-from-name (tn)
  (print-type (type-from-name tn))
  t)
(defun ptn (tn) (print-type-from-name tn))

; Print types
(defun print-types (&optional stream)
  (unless stream (setq stream trace-stream))
  (dolist (type (reverse (graph-types cg)))
    (print-type type stream)
    )
  t)
(defun pts () (print-types))

; Print a predicate
(defun print-predicate (p &optional current-function stream)
  (unless stream (setq stream trace-stream))
  (format stream "~&(PREDICATE '~S :WORLD '~S" (predicate-name p) (predicate-world p))
  (when (predicate-persistent p)
    (format stream " :PERSISTENT ~S" (predicate-persistent p))
    )
  (when (predicate-unique p)
    (format stream " :UNIQUE '~S" (predicate-unique p))
    (when (predicate-select p)
      (format stream " :SELECT '~S" (predicate-select p))
      )
    )
  (when (predicate-perception p)
    (format stream " :PERCEPTION T")
    )
  (when (predicate-exponential p)
    (format stream " :EXPONENTIAL ~S" (predicate-exponential p))
    )
  (when (predicate-replace p)
    (format stream " :REPLACE ~S" (predicate-replace p))
    )
  (when (predicate-no-normalize p)
    (format stream " :NO-NORMALIZE ~S" (predicate-no-normalize p))
    )
  (when (predicate-arguments p)
    (format stream " :ARGUMENTS '~S" (predicate-arguments p))
    )
  (when (or (predicate-learning-rate p) (predicate-smoothing-parameter p))
    (format stream "~&   ")
    )
  (when (predicate-learning-rate p)
    (format stream " :LEARNING-RATE ~S" (predicate-learning-rate p))
    )
  (when (predicate-smoothing-parameter p)
    (format stream " :SMOOTHING PARAMETER ~S" (predicate-smoothing-parameter p))
    )
  (when (predicate-function p)
    (format stream "~&    :FUNCTION ")
    (let ((fun (if current-function (plm-cpt (node-function (predicate-function-node p))) (predicate-function p))))
      (unless (numberp fun)
        (format stream "'")
        )
      (format stream "~S" fun)
      )
    )
  (format stream ")")
  t)

; Print predicates
; If state is true print only the predicates that mention the state
(defun print-predicates (&optional current-function state stream)
  (unless stream (setq stream trace-stream))
  (dolist (pred (reverse (if state
                             (graph-state-predicates cg)
                           (graph-predicates cg)
                           )
                         ))
    (print-predicate pred current-function stream)
    )
  t)
(defun pps (&optional current-function state stream)
  (unless stream (setq stream trace-stream))
  (print-predicates current-function state stream))

; Print a predicate from its name
(defun print-predicate-from-name (p-name &optional stream)
  (print-predicate (predicate-from-name p-name) stream)
  t)
(defun pp (p-name) (print-predicate-from-name p-name))

; Print conditions, actions or condacts, one per line
(defun print-patterns (patterns type &optional stream)
  (unless stream (setq stream trace-stream))
  (let ((typel (length (symbol-name type))))
        (format stream "~&    :~S '(" type)
        (when patterns
          (format stream "~S" (car patterns))
          (setq patterns (cdr patterns))
          )
        (dolist (p patterns)
          (format stream "~&        ")
          (dotimes (i typel)
            (format stream " ")
            )
          (format stream "~S" p)
          )
        (format stream ")")
        )
  )

; Print a conditional
(defun print-conditional (c &optional current-function reordered-conditions stream)
  (unless stream (setq stream trace-stream))
  (let (fun)
    (format stream "~&(CONDITIONAL '~S" (conditional-name c))
    (if reordered-conditions
        (when (conditional-reordered-conditions c)
          (print-patterns (conditional-reordered-conditions c) 'conditions))
      (when (conditional-conditions c)
        (print-patterns (conditional-conditions c) 'conditions)))
    (when (conditional-condacts c)
      (print-patterns (conditional-condacts c) 'condacts))
    (when (conditional-actions c)
      (print-patterns (conditional-actions c) 'actions))
    (when (conditional-map c)
      (format stream "~&    :MAP T"))
    (when (conditional-function-variable-names c)
      (format stream "~&    :FUNCTION-VARIABLE-NAMES '~S" (conditional-function-variable-names c)))
    (when (conditional-normal c)
      (format stream "~&    :NORMAL '~S" (conditional-normal c)))
    (when (conditional-learning-rate c)
      (format stream "~&    :LEARNING-RATE ~S" (conditional-learning-rate c))
      )
    (when (conditional-smoothing-parameter c)
      (format stream "~&    :SMOOTHING PARAMETER ~S" (conditional-smoothing-parameter c))
      )
    (when (conditional-function c)
      (format stream "~&    :FUNCTION ")
      (setq fun (if current-function (reusable-conditional-function-c c) (conditional-function c)))
      (unless (numberp fun)
        (format stream "'")
        )
      (format stream "~S" fun)
      )
    (format stream "~&    )~%~%")
    )
  )
; Print a conditional based on its name
(defun pc (name &optional current-function reordered-conditions stream)
  (print-conditional (conditional-from-name name) current-function reordered-conditions stream))
; Print all conditionals to terminal without current function
(defun pcs (&optional current-function reordered-conditions stream)
  (dolist (c (reverse (graph-conditionals cg)))
    (print-conditional c current-function reordered-conditions stream)
    )
  )
; Print the function currently stored with a named conditional (not necessarily original function if learning)
(defun pcf (name &optional array)
  (let ((c (conditional-from-name name))
        cfn)
    (unless c
      (error "No conditional found for name ~S in call to PCF" name)
      )
    (setq cfn (conditional-function-node c))
    (when cfn ; When there is a function node for the conditional
      (if (and array (> (length (node-variables cfn)) 0) (< (length (node-variables cfn)) 4))
          (parray (node-function cfn))
        (pplm (node-function cfn))) ; Print its function
      )
    t)
  )
; Print the function currently stored with all conditionals
(defun pcfs (&optional array)
  (dolist (c (graph-conditionals cg))
    (when (conditional-function c)
      (format trace-stream "~S: " (conditional-name c))
      (pcf (conditional-name c) array)
      (format trace-stream "~&~%")
      )
    )
  )

; Print the function currently stored with a predicate (not necessarily original function if learning)
(defun ppf (name &optional array)
  (let ((p (predicate-from-name name))
        pfn)
    (unless p
      (error "No predicate found for name ~S in call to PPF" name)
      )
    (setq pfn (predicate-function-node p))
    (when pfn ; When there is a function node for the predicate
      (if (and array (> (length (node-variables pfn)) 0) (< (length (node-variables pfn)) 4))
          (parray (node-function pfn))
        (pplm (node-function pfn))) ; Print its function
      )
    t)
  )

; Print the function currently stored with all predicates
(defun ppfs (&optional array)
  (dolist (p (graph-predicates cg))
    (when (predicate-function-node p)
      (format trace-stream "~S: " (predicate-name p))
      (ppf (predicate-name p) array)
      (format trace-stream "~&~%")
      )
    )
  )

; Print the function currently stored with all predicates or conditionals
(defun pafs (&optional array)
  (format trace-stream "~&Conditional functions:~&~%")
  (pcfs array)
  (format trace-stream "~&~%Predicate functions:~&~%")
  (ppfs array)
  )

; Print one or all of the functions in episodic memory
(defun pem (&optional pred-name)
  (cond (pred-name
         (let ((pred (predicate-from-name pred-name))
               pem)
           (unless pred
             (error "No predicate named ~S in PEM" pred-name)
             )
           (setq pem (predicate-em pred))
           (unless pem
             (error "No episodic memory for predicate ~S in PEM" pred-name)
             )
           (pa (node-function pem))
           )
         )
        (t
         (mapc #'(lambda (c) (when (conditional-episodic c)
                               (format trace-stream "~&~%~S Memory: " (conditional-name c))
                               (pa (conditional-name c))
                               )
                   )
               (reverse (graph-conditionals cg))
               )
         (mapc #'(lambda (p) (when (predicate-episodic p)
                               (format trace-stream "~&~%~S Memory: " (predicate-name p))
                               (pa (predicate-name p))
                               )
                   )
               (reverse (graph-predicates cg))
               )
         )
        )
  t)

; Print results of episodic learning
(defun print-episodic-memory nil
  (ppfn temporal-predicate-name-episodic)
  (pem)
  )

; Print the predictions in episodic memory
(defun pep ()
  (format trace-stream "~&~%Episodic retrieval of time: ")
  (print-wm-vn (predicate-from-name temporal-predicate-name-episodic) t)
  (dolist (p (graph-predicates cg))
    (when (predicate-em p)
      (format trace-stream "~&~%Episodic prediction for predicate ~S: " (predicate-name p))
      (print-wm-vn (predicate-em-predicate p) t)
      )
    t)
  )

; Print graph statistics
(defun graph-statistics (&optional stream)
  (unless stream (setq stream trace-stream))
  (let* ((var-stats (count-node-variables))
         (nums (car var-stats))
         (vavgs (cadr var-stats))
         (vmaxs (caddr var-stats))
         (link-stats (count-node-links))
         (linavgs (nth 1 link-stats))
         (linmaxs (nth 2 link-stats))
         (loutavgs (nth 3 link-stats))
         (loutmaxs (nth 4 link-stats))
         )
    (format stream "~&Nodes: ~S (~S Variable, ~S Factor), Links: ~S (~S Active Directions), Depth: ~S"
            (length (graph-nodes cg))
            (aref nums 0) (aref nums 1)
            (length (graph-links cg)) (count-active-link-directions)
            (+ (graph-depth cg) 1) ; Add one because depth starts at 0
            )
    (format stream "~&Average variables per variable node: ~4,1F; factor node: ~4,1F"
            (aref vavgs 0) (aref vavgs 1))
    (format stream "~&Maximum variables per variable node: ~S; factor node: ~S"
            (aref vmaxs 0) (aref vmaxs 1))
    (format stream "~&Average incoming links per variable node: ~4,1F; factor node: ~4,1F"
            (aref linavgs 0) (aref linavgs 1))
    (format stream "~&Maximum incoming links per variable node: ~S; factor node: ~S"
            (aref linmaxs 0) (aref linmaxs 1))
    (format stream "~&Average outgoing links per variable node: ~4,1F; factor node: ~4,1F"
            (aref loutavgs 0) (aref loutavgs 1))
    (format stream "~&Maximum outgoing links per variable node: ~S; factor node: ~S"
            (aref loutmaxs 0) (aref loutmaxs 1))
    t)
  )
; Shortcut for graph-statistics
(defun gs (&optional stream)
  (unless stream (setq stream trace-stream))
  (graph-statistics stream)
  )

; Print names of nodes in graph
(defun print-node-names (&optional stream)
  (unless stream (setq stream trace-stream))
  (format stream "~&")
  (dolist (n (graph-nodes cg))
    (format stream "~S " (node-name n))
    )
  )
(defun pnn nil (print-node-names trace-stream))

; Print a graph
(defun print-graph (&optional symbolic stream)
  (unless stream (setq stream trace-stream))
  (unless (graph-initialized cg)
    (init-graph)
    )
  (format stream "~&<GRAPH:> ")
  (graph-statistics stream)
  (print-variable-nodes symbolic)
  (print-factor-nodes symbolic)
  (format stream "~&~%")
  (print-queues t trace-stream)
  t
  )

; Short hand for print-graph
(defun pg nil (print-graph symbolic-trace trace-stream))

; Print the shared WM VN for a predicate name
(defun print-wm-vn (pred &optional symbolic stream limits)
  (unless stream (setq stream trace-stream))
  (unless (graph-initialized cg)
    (init-graph)
    )
  (print-plm (vn-posterior pred) symbolic stream limits)
  )
(defun ppvn (pred-name &optional stream)
  (print-wm-vn (predicate-from-name pred-name) t stream)
  )

(defun ppwmfn (pname &optional limits)
  (print-pred-wm-function (predicate-from-name pname) t trace-stream limits)
  )  

(defun ppfn (pred-name &optional limits as-array stream)
  (unless stream (setq stream trace-stream))
  (format stream "~2&WM for ~S" pred-name)
  (print-pred-function (predicate-from-name pred-name) (if as-array as-array t) stream limits)
  )

(defun pppfn (pname &optional limits)
  (print-predicate-perception-function (predicate-from-name pname) t trace-stream limits)
  ) 
(defun pppfns nil
  (dolist (pred (graph-predicates cg))
    (when (predicate-perception pred)
      (format trace-stream "~%~&~S: " (predicate-name pred))
      (pppfn (predicate-name pred))
      (format trace-stream "~&")
      )
    )
  )

(defun ppwm (&optional pred-name as-array stream)
  (if pred-name
      (print-pred-wm (predicate-from-name pred-name) (if as-array as-array t) stream)
    (print-wm t stream))
  )

; Print the WMs associated with a graph
(defun print-wm (&optional symbolic stream)
  (dolist (pred (graph-predicates cg))
    (print-pred-wm pred symbolic stream nil)
    (format stream "~%")
    )
  nil)

; for closed world prints: function 
; for open world just want posterior on wm var nodes & message into wv nodes
; showing contents of working memory on both kinds of nodes w/o extra info that might confuse. 
; pred to use if you wanna print single predicate
; preds use if you want to print specific list of predicates
; default if no predicates given will print all in graph
(defun print-wm-function (&optional pred-name preds stream)
  (unless stream (setq stream trace-stream))
  (if pred-name
      (progn
	(setf one-pred (predicate-from-name pred-name))
	  (format stream "~%~S~&" (predicate-name one-pred))
	 (if (closed-world one-pred)
	     (progn
	       (print-pred-wm-function one-pred)
	       (if (predicate-function-node one-pred)
		   (progn 
		     (print-factor-function (predicate-function-node one-pred))
		     (format stream "~%")))
	       )
	     )
	 (if (open-world one-pred)
	     (progn 
	       (dolist (vn (predicate-wm-vns one-pred))
		 (format stream "~&")
		 (print-plm (variable-posterior vn) t) 
		 (format stream "~%"))))
	 ))
  (if preds
      (progn 
	(dolist (pred preds)
	  (format stream "~%~S~&" (predicate-name pred))
	  (if (closed-world pred)
	      (progn
		(print-pred-wm-function pred)
		(if (predicate-function-node pred)
		    (progn 
		    (print-factor-function (predicate-function-node pred))
		    (format stream "~%")))
	      )
	    )
	  (if (open-world pred)
	      (progn 
		(dolist (vn (predicate-wm-vns pred))
		  (format stream "~&")
		  (print-plm (variable-posterior vn) t) 
		  (format stream "~%"))))
	  )))
  (if (and (not pred-name) (not preds))   ; if no pred or preds supplied print all in graph
      (progn 
	(dolist (gpred (graph-predicates cg))
	  (format stream "~%~S~&" (predicate-name gpred))
	  (if (closed-world gpred)
	      (progn
		(print-pred-wm-function gpred)
		(if (predicate-function-node gpred)
		    (progn 
		      (print-factor-function (predicate-function-node gpred))
		      (format stream "~%")))
		)
	      )
	  (if (open-world gpred)
	      (progn 
		(dolist (vn (predicate-wm-vns gpred))
		  (format stream "~&")
		  (print-plm (variable-posterior vn) t) 
		  (format stream "~%"))))
	  )))
  nil)

(defun pwmf (&optional one-pred preds stream)
  (print-wm-function one-pred preds stream))

(defun pwm (&optional as-array stream)
  (print-wm (if as-array as-array t) stream))

; Print functions in function factors
(defun print-functions (&optional symbolic stream)
  (dolist (n (graph-nodes cg))
    (when (function-nodep n)
      (print-node n symbolic stream)
      )
    )
  )
(defun pfs (&optional as-array)
  (print-functions (if as-array as-array t) trace-stream)
  )

; Print the expected value of the distribution/unique variable of a conditional function
; vn is a variable name
(defun print-expected-value-cond-function (cond-name &optional vn array)
  (let* ((c (conditional-from-name cond-name))
         p) ; PLM for conditional function
    (if c
        (setq p (node-function (conditional-function-node c)))
      (error "No conditional named ~S in PRINT-EXPECTED-VALUE-COND-FUNCTION." cond-name)
      )
    ; When variable not specified, use :normal variable if there is one
    (unless vn (setq vn (conditional-normal c)))
    ; Expected value of PLM
    (if (symbolp vn)
        (setq p (expected-value-plm p (when vn (position vn (plm-variables p) :key #'svariable-name))))
      (dolist (vnx vn)
        (setq p (expected-value-plm p (position vnx (plm-variables p) :key #'svariable-name)))
        ))
    (if (and array (> (length (plm-variables p)) 1) (< (length (plm-variables p)) 5))
          (parray p)
        (pplm p))
    t)
  )
(defun pevcf (cond-name &optional vn array)
  (print-expected-value-cond-function cond-name vn array)
  )

; Print the expected value of the distribution/unique variable of a predicate function
; vn is a variable name
(defun print-expected-value-pred-function (pred-name &optional vn array)
  (let* ((pred (predicate-from-name pred-name))
         p) ; PLM for predicate function
    (if pred
        (setq p (node-function (predicate-function-node pred)))
      (error "No predicate named ~S in PRINT-EXPECTED-VALUE-COND-FUNCTION." pred-name)
      )
    ; When variable not specified, use :unique variable if there is one
    (unless vn (setq vn (predicate-unique pred)))
    ; Expected value of PLM
    (if (symbolp vn)
        (setq p (expected-value-plm p (when vn (position vn (plm-variables p) :key #'svariable-name))))
      (dolist (vnx vn)
        (setq p (expected-value-plm p (position vnx (plm-variables p) :key #'svariable-name)))
        ))
    (if (and array (> (length (plm-variables p)) 1) (< (length (plm-variables p)) 5))
          (parray p)
        (pplm p))
    t)
  )
(defun pevpf (pred-name &optional vn array)
  (print-expected-value-pred-function pred-name vn array)
  )

;----------------SMART PRINTING---------------------------------------

; Code for a smart form of PLM printing

; Determine maximum number of regions PLM may have based on variable spans
(defun max-plm-size (p)
  (let ((size 1)
        (vs (plm-variables p))
        (as (plm-active p))
        )
    (dotimes (i (plm-rank p))
      (when (aref as i)
        (setq size (* size (stype-span (svariable-type (aref vs i)))))
        )
      )
    size)
  )

; Test if should use regions in parray of PLM
(defun parray-regions (p)
  (< (/ (plm-size p) (max-plm-size p)) max-fraction-pa-regions)
  )

; Check if a PLM has at least one large symbolic variable
(defun has-large-symbolic-variable (p)
  (find-if #'(lambda (v) (let ((st (svariable-type v))) (and (not (stype-numeric st)) (> (stype-span st) max-span-pa)))) (plm-variables p))
  )

; Use parray or pplm smartly in printing a PLM
(defun print-smart (p &optional symbolic stream summarize limits)
  (if (or (and (parrayp p) (not limits)) summarize)
      (parray p (parray-regions p) summarize stream)
    (print-plm p symbolic stream limits))
  )
; Abbreviation for print-smart with symbolic and no limits
(defun ps (p &optional stream summarize limits)
  (print-smart p t stream summarize limits)
  )

; Print perceptual function
(defun print-predicate-perception-function (pred &optional symbolic stream limits)
  (print-smart (node-function (predicate-perception pred)) symbolic stream nil limits)
  )

; Print a node
(defun print-node (n &optional symbolic stream)
  (unless stream (setq stream trace-stream))
  (unless (graph-initialized cg)
    (init-graph)
    )
  (let ((varp (variable-nodep n))
        post
        )
    (format stream "~&~%~A(" (node-name n))
    (when (node-evidence n)
      (format stream "EVIDENCE "))
    (if (factor-nodep n)
        (format stream "FACTOR): ")
      (format stream "VARIABLE): "))
    (if varp
        (progn
          (print-svariables (node-variables n) stream)
          (format stream " = ")
          (setq post (variable-posterior n))
          (if (and (node-links n) post)
              (print-smart post symbolic stream)
            (format stream "UNDEFINED")
            )
          )
      (cond ((plm-p (node-function n))
             (when (eq (node-subtype n) 'combine)
               (format stream "[*~S*] " (node-subsubtype n))
               )
             (print-smart (node-function n) symbolic stream)
             )
            (t
             (format stream "~S" (node-function n)) ; When function is not a PLM
             )
            )
      )
    (format stream "~&")
    (print-in-messages n symbolic stream)
    (print-out-messages n symbolic stream)
    (when (and (not varp) (node-factor-steps n))
      (print-factor-steps n stream))
    nil)
  )

; Print a message (given its link and index)
(defun print-message (trace-header ml mi &optional symbolic reverse print-first stream)
  (unless (graph-initialized cg)
    (init-graph)
    )
  (when (link-content ml mi)
    (let (n1 n2)
      (unless trace-header (setq trace-header '"  MESSAGE: "))
      (setq n1 (node-name (aref (link-nodes ml) mi)))
      (setq n2 (node-name (aref (link-nodes ml) (next-message-index mi))))
      (format stream "~&")
      (when trace-link-depths
        (format stream "<~S>" (aref (link-depths ml) mi))
        )
      (if reverse
          (if print-first
              (format stream "~A~A<-~A: " trace-header n2 n1)
            (format stream "~A<-~A: " trace-header n1))
        (if print-first
            (format stream "~A~A->~A: " trace-header n1 n2)
          (format stream "~A->~A: " trace-header n2)))
;      (print-smart (link-content ml mi) symbolic stream) workaround for bug in print-smart
      (print-plm (link-content ml mi) symbolic stream)
      nil)
    )
  )

; Print just WM function (not intro text)
(defun print-pred-wm-function (pred &optional symbolic-or-summarize stream limits)
  (unless (predicate-wm pred)
    (error "No WMFN node for predicate ~S in PRINT-PRED-WM-FUNCTION" (predicate-name pred))
    )
  (print-smart (node-function (predicate-wm pred)) t stream (if (consp symbolic-or-summarize) symbolic-or-summarize nil) limits)
  )

; Print a WM function
(defun print-pred-function (pred &optional symbolic-or-summarize stream limits)
  (unless stream (setq stream trace-stream))
  (unless (predicate-wm pred)
    (error "No WMFN node for predicate ~S in PRINT-PRED-FUNCTION" (predicate-name pred))
    )
  (let ((wm (predicate-wm pred)))
    (format stream "~&  Factor ~A Function: " (node-name wm))
    (print-pred-wm-function pred symbolic-or-summarize stream limits)
    t)
  )

; Print a WM
(defun print-pred-wm (pred &optional symbolic stream limits)
  (unless stream (setq stream trace-stream))
  (unless (graph-initialized cg)
    (init-graph)
    )
  (format stream "~2&WM for ~S" (predicate-name pred))
  (let* ((wm (predicate-wm pred)) ; WM factor node
         posterior
         argmax
         link
         )
    (when wm
      (print-pred-function pred symbolic stream limits)
      )
    ; Check if there is an argmax to be done
    (when (and (consp symbolic)
               (setq argmax (find 'argmax symbolic :key #'car))
               )
      (setq symbolic (remove argmax symbolic))
      ; If removing the argmax yields an empty list, make it 'array that variables still use parray
      (when (null symbolic)
        (setq symbolic 'array)
        )
      )
    ; Print each of the WM variable nodes associated with the factor
    (dolist (wmvn (predicate-wm-vns pred))
      ; Print normalized variable posterior if there is one
      (cond ((node-exponential wmvn) ; Print exponentially transformed posterior
             (setq posterior (transform-plm #'exponentiate-constant-times10-function (variable-posterior wmvn) nil nil t))
             (when (node-normalize wmvn)
               (setq posterior (normalize-plm posterior nil))
               )
             (when posterior
               (if (eq (node-subsubtype wmvn) 'negative)
                   (format stream "~& -") ; prefix with a - if negative condition or action
                 (format stream "~&  ")
                 )
               (format stream "Variable ~A Normalized Posterior (e^10v): " (node-name wmvn))
               (print-smart posterior symbolic stream nil limits)
               )
             )
            (t ; Print normal posterior
             (setq posterior (variable-posterior wmvn))
             (when posterior
               (if (eq (node-subsubtype wmvn) 'negative)
                   (format stream "~& -") ; prefix with a - if negative condition or action
                 (format stream "~&  ")
                 )
               (format stream "Variable ~A Normalized Posterior: " (node-name wmvn))
               (print-smart posterior symbolic stream nil limits)
               )
             )
            )
      (when wm
        (setq link (link-from-numbers (node-number wmvn) (node-number wm)))
        (when link
          (print-message '"    " link 0 symbolic t nil stream) ; Print message from variable to factor
          )
        ))
    nil)
  )

;-----------------------------------------------------
; Print messages corresponding to conditional patterns

; Find first input link to node
(defun first-input-link (n)
  (let ((direction (if (variable-nodep n) 1 0))
        )
    (dolist (l (node-links n))
      (when (aref (link-contents l) direction)
        (return l)
        )
      )
    )
  )

; Find first input message to node
(defun first-input-message (n)
  (let ((direction (if (variable-nodep n) 1 0))
        message)
    (dolist (l (node-links n))
      (setq message (aref (link-contents l) direction))
      (when message
        (return message)
        )
      )
    )
  )

; Find link into first WM variabble node (VN or VAN) towards WM from alpha variable node
; Link is the one into the node form the graph (i.e., away from WM)
(defun first-wm-variable-node-link (n link)
  (loop
   (when (and (variable-nodep n) (eq (node-subtype n) 'wm))
     (return link)
     )
   (setq link (other-link n link)) ; Get other link, assuming only two links per node in alpha network
   (setq n (aref (link-nodes link) (if (variable-nodep n) 1 0)))
   )
  )

(defun print-pattern-matches (conditional-name &optional stream)
  (unless (graph-initialized cg)
    (init-graph)
    )
  (let ((c (conditional-from-name conditional-name))
        l ams)
    (unless c
      (error "No conditional named ~S." conditional-name)
      )
    ; Find alpha memories for conditional
    (setq ams (reverse (conditional-alpha-memories (conditional-from-name conditional-name))))
    ; Handle conditions
    (when (conditional-reordered-conditions c)
      (format stream "~&~%CONDITIONS:~&~%")
      (dolist (p (conditional-reordered-conditions c))
        (format stream "~&~S:~&" p)
        (ps (first-input-message (car ams)) stream)
        (setq ams (cdr ams))
        (format stream "~&~%")
        )
      )
    ; Handle condacts
    (when (conditional-condacts c)
      (format stream "~&~%CONDACTS:~&~%")
      (dolist (p (conditional-condacts c))
        (format stream "~&~S:~&" p)
        (dolist (cl (node-links (car ams)))
          (cond ((eq (node-subtype (aref (link-nodes cl) fact-index)) 'beta)
                 (format stream "~&<-: ")
                 (print-plm (aref (link-contents cl) var-index) stream)
                 )
                (t
                 (setq cl (first-wm-variable-node-link (car ams) (other-link (car ams) cl)))
                 (format stream "~&->: ")
                 (ps (aref (link-contents cl) fact-index) stream)
                 )
                )
          )
        (setq ams (cdr ams))
        (format stream "~&~%")
        )
      )
    ; Handle actions
    (when (conditional-actions c)
      (format stream "~&~%ACTIONS:~&~%")
      (dolist (p (conditional-actions c))
        (format stream "~&~S:~&" p)
        (setq l (first-wm-variable-node-link (car ams) (first-input-link (car ams)))) ; Use message into WM variable node rather than alpha memory node
        (ps (aref (link-contents l) fact-index) stream) ; Only one message out, so posterior is this
        (setq ams (cdr ams))
        (format stream "~&~%")
        )
      )
    )
  )
(defun ppm (conditional-name)
  (print-pattern-matches conditional-name trace-stream))

;-----------------------------------------------------------
; Fix print-smart to be smarter about when not to use parray

; Determine fraction of regions that are non-empty
(defun plm-density (p)
  (let ((ne 0)
        (pa (plm-array p))
        (nregions (plm-size p)))
    (dotimes (i nregions)
      (unless (region-e-empty (row-major-aref pa i))
        (setq ne (1+ ne))
        )
      )
    (/ ne nregions)
    )
  )

; Is PLM appropriate for parray
(defun parrayp (p &optional summarize)
  summarize ; dummy (eventually get rid of argument)
  (and (plm-piecewise-constant p) (not (has-large-symbolic-variable p)) (> (plm-density p) .1))
  )
