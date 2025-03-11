(in-package :sigma)
;--------------------------------------------------------------
; frontend-specific paths
;----------------------------------------------------------------


; Reset all parameters to their default (*ed) values
; Reset all parameters to their default (*ed) values
(defun reset-parameters-gui nil
  (dolist (gp global-parameters)
    (unless (member (car gp) parameter-override-reset)
      (eval `(setq ,(car gp) ,(cdr gp)))
      )
    )
  (if (boundp 'from-regression)
      (progn
  (setq trace-stream (if from-regression
                         (capi:interactive-pane-stream (slot-value regression-interface 'listener-tab)) ; Initialize to regresssion test listener
                       t)) ; Reinitializes to current window
  )))

(reset-parameters-gui)



; Generate a graphical representation of a factor graph
; Roots is a list of nodes at which to start the traversal
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun graph (size roots)
  (bp-get-nodes (graph-nodes cg))
   
  (let* (;(bp-factor (bp-get-nodes 'factor (graph-nodes cg)))
         (screen (capi:convert-to-screen))
         (width (capi:screen-width screen))
         (height (capi:screen-height screen))
         (graph-graph (make-instance 'capi:graph-pane
                                     :roots roots
                                     :children-function #'neighbor-name-list
                                     :layout-function :left-right
                                     :visible-min-width (* width (case size ((small) .5) ((medium) .6) (large .7)))
                                     :visible-min-height (* height (case size ((small) .3) ((medium) .42) (large .525)))
                                     :horizontal-scroll t
                                     :vertical-scroll t
;                                     :callback-type :data
                                     :selection-callback #'node-selection-callback
                                     :action-callback #'bp-node-action-callback
                                     :edge-pane-function #'link-graph-function
                                     :node-pane-function #'bp-node-function 
                                     )
                      )
         
         
         (node-collector (make-instance 'capi:collector-pane
                                        :visible-min-width (* width (case size ((small) .5) ((medium) .15) (large .2)))
                                        :visible-min-height (* height (case size ((small) .2) ((medium) .20) (large .225)))
                                        )
                         )

         ;radio panel to select the depth of the popup 
         (node-depth-radio (make-instance 'capi:radio-button-panel
                                          :title "Depth for subgraph popup"
                                          :title-position :frame
                                          :items '(2 3 4 5 6)
                                          :selected-item 3
                                          :selection-callback 
                                          #'(lambda (x)
                                              (setq *nodes-depth* x)
                                              )
                                          :callback-type :data
                                          )
                           )

;         (display-subgraph (make-instance 'capi:radio-button-panel
;                                          :title "Auto popup of subgrapgh display when go to node?"
;                                          :title-position :frame
;                                          :items '(:yes :no)
;                                          :selected-item :no
;                                          :callback-type :data
;                                          :print-function 'string-capitalize
;                                          :selection-callback
;                                          #'(lambda(x)(setf display-subgraph
;                                                            (case x
;                                                              (:yes t)
;                                                              (:no nil)
;                                                              ))
;                                              )
;                                          :visible-min-width 300
;                                          ))

         ;Search option to find the node         
         (search-nodes (make-instance 'capi:text-input-pane
                                     :visible-max-width 250
                                     :callback 'search-nodes-function
                                     :callback-type :data
                                     :buttons (list :ok t)
                                     :title "Go to Node:"
                                     :title-position :frame
                                     ))
       
         
         ;Options
         (option-factor (make-instance 'capi:option-pane
                                       :items (getf bp-nodes :factor)
                                       :selection-callback #'bp-dropdown-selection
                                       :visible-max-width 200
                                       ))
         (option-variable (make-instance 'capi:option-pane
                                         :items (getf bp-nodes :variable)
                                         :selection-callback #'bp-dropdown-selection
                                         :visible-max-width 200
                                         ))
         
         (option-wm (make-instance 'capi:option-pane
                                   :items (getf bp-nodes :wm)
                                   :visible-max-width 200
                                   :selection-callback #'bp-dropdown-selection
                                   ))

         (option-alpha (make-instance 'capi:option-pane
                                      :items (getf bp-nodes :alpha)
                                      :visible-max-width 200
                                      :selection-callback #'bp-dropdown-selection
                                      ))

         (option-beta (make-instance 'capi:option-pane
                                     :items (getf bp-nodes :beta)
                                     :visible-max-width 200
                                     :selection-callback #'bp-dropdown-selection
                                     ))

         (option-affine (make-instance 'capi:option-pane
                                       :items (getf bp-nodes :affine)
                                       :visible-max-width 200
                                       :selection-callback #'bp-dropdown-selection
                                       ))
         (option-combine (make-instance 'capi:option-pane
                                        :items (getf bp-nodes :combine)
                                        :visible-max-width 200
                                        :selection-callback #'bp-dropdown-selection
                                        ))
         (option-filter (make-instance 'capi:option-pane
                                       :items (getf bp-nodes :filter)
                                       :visible-max-width 200
                                       :selection-callback #'bp-dropdown-selection
                                       ))
         (option-inversion (make-instance 'capi:option-pane
                                          :items (getf bp-nodes :inversion)
                                          :visible-max-width 200
                                          :selection-callback #'bp-dropdown-selection
                                          ))
         (option-transform (make-instance 'capi:option-pane
                                          :items (getf bp-nodes :transform)
                                          :visible-max-width 200           
                                          :selection-callback #'bp-dropdown-selection
                                          ))
         (option-perception (make-instance 'capi:option-pane
                                          :items (getf bp-nodes :perception)
                                          :visible-max-width 200           
                                          :selection-callback #'bp-dropdown-selection
                                          ))
         (option-function (make-instance 'capi:option-pane
                                          :items (getf bp-nodes :function)
                                          :visible-max-width 200           
                                          :selection-callback #'bp-dropdown-selection
                                          ))
         
         (option-grid-1 (make-instance 'capi:grid-layout
                                       :description (list "Variable" option-variable "Factor" option-factor)
                                       :y-adjust :center
                                       :title "Node Types"
                                       :title-position :frame
                                       :visible-min-width 340
                                       :y-gap 2
                                       ))
         (option-grid-2 (make-instance 'capi:grid-layout
                                       :description (append 
                                                     (if (getf bp-nodes :wm) (list "Working Memory" option-wm) nil)
                                                     (if (getf bp-nodes :alpha) (list "Alpha" option-alpha) nil)
                                                     (if (getf bp-nodes :beta) (list "Beta"  option-beta) nil)
                                                     (if (getf bp-nodes :affine) (list "Affine" option-affine) nil)
                                                     (if (getf bp-nodes :combine) (list "Combine" option-combine) nil)
                                                     (if (getf bp-nodes :filter) (list "Filter" option-filter) nil)
                                                     (if (getf bp-nodes :inversion) (list "inversion" option-inversion) nil)
                                                     (if (getf bp-nodes :transform) (list "Transform" option-transform) nil)
                                                     (if (getf bp-nodes :perception) (list "Perception" option-perception) nil)
                                                     (if (getf bp-nodes :function) (list "Function" option-function) nil)
                                                     )
                                       :y-adjust :center
                                       :visible-min-width 340
                                       :title "Node Subtypes"
                                       :title-position :frame
                                       :y-gap 2
                                       ))
         
         (node-info (make-instance 'capi:pinboard-layout
                                   :description (list 
                                                 (make-instance 'capi:item-pinboard-object
                                                                :text "Double click a node to pop up a subgraph"
                                                                :x 2
                                                                :y 2
                                                                )
                                                 (make-instance 'capi:item-pinboard-object
                                                                :text "Select a node and then shift drag to move it"
                                                                :x 2
                                                                :y 18
                                                                )
                                                 (make-instance 'capi:item-pinboard-object
                                                                :text "Key to node colors:"
                                                                :x 2
                                                                :y 45
                                                                )  
                                                   ;Factor node
                                                 (make-instance 'capi:rectangle
                                                                :filled t
                                                                :x 2
                                                                :y 63
                                                                :width 80
                                                                :height 20
                                                                :graphics-args '(:foreground ::dodgerblue4))
                                                 (make-instance 'capi:item-pinboard-object
                                                               :text "Factor Node"
                                                                :graphics-args '(:foreground :white)
                                                                :x 7
                                                                :y 65)
;                                                 (make-instance 'capi:item-pinboard-object
;                                                                :text "- Factor Node"
;                                                                :x 52
;                                                                :y 20)
                                                 
                                                   ;Variable node
                                                 (make-instance 'capi:rectangle
                                                                :filled t
                                                                :x 87
                                                                :y 63
                                                                :width 90
                                                                :height 20
                                                                :graphics-args '(:foreground :red4))
                                                 (make-instance 'capi:item-pinboard-object
                                                                :text "Variable Node"
                                                                :graphics-args '(:foreground :white)
                                                                :x 92
                                                                :y 65)
;                                                 (make-instance 'capi:item-pinboard-object
;                                                                :text "- Variable Node"
;                                                                :x 52
;                                                                :y 45)
                                                 
                                                   ;Function node
                                                 (make-instance 'capi:rectangle
                                                                :filled t
                                                                :x 2
                                                                :y 88
                                                                :width 205
                                                                :height 20
                                                                :graphics-args '(:foreground :green4))
                                                 (make-instance 'capi:item-pinboard-object
                                                                :text "Conditional Function Factor Node"
                                                                :graphics-args '(:foreground :white)
                                                                :x 7
                                                                :y 90)
;                                                 (make-instance 'capi:item-pinboard-object
;                                                                :text "- Conditional Function Factor Node"
;                                                                :x 52
;                                                                :y 70)
                                                 
                                                   ;Working Memory node
                                                 (make-instance 'capi:rectangle
                                                                :filled t
                                                                :x 2
                                                                :y 113
                                                                :width 170
                                                                :height 20
                                                                :graphics-args '(:foreground :dodgerblue))
                                                 (make-instance 'capi:item-pinboard-object
                                                                :text "Working Memory Factor Node"
                                                                :graphics-args '(:foreground :white)
                                                                :x 7
                                                                :y 115)
;                                                 (make-instance 'capi:item-pinboard-object
;                                                                :text "- Working Memory Factor Node"
;                                                                :x 52
;                                                                :y 95)
                                                 
                                                   ;Working Memory node
                                                 (make-instance 'capi:rectangle
                                                                :filled t
                                                                :x 2
                                                                :y 138
                                                                :width 182
                                                                :height 20
                                                                :graphics-args '(:foreground :red))
                                                 (make-instance 'capi:item-pinboard-object
                                                                :text "Working Memory Variable Node"
                                                                :graphics-args '(:foreground :white)
                                                                :x 7
                                                                :y 140)
;                                                 (make-instance 'capi:item-pinboard-object
;                                                                :text "- Working Memory Variable Node"
;                                                                :x 52
;                                                                :y 120)
 
                                                 ;Perception node
                                                 (make-instance 'capi:rectangle
                                                                :filled t
                                                                :x 2
                                                                :y 163
                                                                :width 145
                                                                :height 20
                                                                :graphics-args '(:foreground :purple))                                                          
                                                 (make-instance 'capi:item-pinboard-object
                                                                :text "Perception Factor Node"
                                                                :graphics-args '(:foreground :white)
                                                                :x 7
                                                                :y 165)
;                                                 (make-instance 'capi:item-pinboard-object
;                                                                :text "- Perception Factor Node"
;                                                                :x 52
;                                                                :y 145) 
                                                 ;Predicate node
                                                 (make-instance 'capi:rectangle
                                                                :filled t
                                                                :x 152
                                                                :y 163
                                                                :width 145
                                                                :height 20
                                                                :graphics-args '(:foreground :purple4))
                                                 (make-instance 'capi:item-pinboard-object
                                                                :text "Prediction Factor Node"
                                                                :graphics-args '(:foreground :white)
                                                                :x 157
                                                                :y 165)
;                                                 (make-instance 'capi:item-pinboard-object
;                                                                :text "- Prediction Factor Node"
;                                                                :x 52
;                                                                :y 170) 
                                               
                                                 )
                                   :background :white
                                   :title "Node Operations and Representation"
                                   :title-position :frame
                                   :visible-max-height 300
                                   :visible-max-width 320
                                   )
                    )         
         
         (graph-listener (make-instance 'capi:listener-pane
                                        :title "Listener"
                                        :visible-min-height (* height (case size ((small) .2) ((medium) .11) (large .110)))))

         (option-column (make-instance 'capi:column-layout
                                       :description (list ; display-subgraph
                                                          node-depth-radio search-nodes  option-grid-1 option-grid-2 node-info) 
                                       :visible-min-width 345
                                       :y-gap 2
                                       :title ""
                                       :title-position :frame
                                       ))
         
         (graph-column (make-instance 'capi:column-layout
                                       :description (list graph-graph node-collector graph-listener)
                                       :y-ratios '(2 1)
                                       ))
         (graph-row (make-instance 'capi:row-layout
                                   :description (list option-column graph-column )
                                   
                                   )
                    )
                             
         )
    
    (setq *node-stream* (capi:collector-pane-stream node-collector))
    (capi:contain graph-row :title "Graphical Representation")
    (setf (graph-graph cg) graph-graph))
  )


; Generate an edge for a link
(defun link-graph-function (self from to)
  (declare (ignore self))
  (let* ((fromn (node-from-name from))
         (ton (node-from-name to))
         (direction (link-direction fromn ton))
;         (link (link-from-nodes fromn ton))
        )
    (apply #'make-instance
           (cond ((eq direction ':both) 'capi:double-headed-arrow-pinboard-object)
                 ((eq direction ':neither) 'capi:line-pinboard-object)
                 (t 'capi:arrow-pinboard-object)
                 )
           (if (or (eq direction ':both) (eq direction ':neither))
               nil
             (list :head-direction direction
;                   :graphics-args (list :foreground (if (loopy-link link) :indianred :black))
                   )
             )
           )
    )
  )

(defun bp-dropdown-selection (data interface)
  (declare (ignore interface))
  (let* ((node-num (node-number (node-from-name data))))
    (setq *selected-node* (node-object (node-from-name data)))
    (sh node-num)
    (capi:redraw-pinboard-layout (graph-graph cg) 0 0 10000 10000)
    (when display-subgraph
      (create-bipartite data)
      )
    )
  )


(defun bp-node-selection-callback (node interface)
  (capi:execute-with-interface interface 
       (lambda () (setq *bp-node-stream* (capi:collector-pane-stream (slot-value interface 'bp-node-collector)))))
    (print-node (node-from-name node)
                t
                *bp-node-stream*
                )
  )

(defun search-nodes-function (data)
  (let (pred c)
    (if (typep (read-from-string data) 'integer) 
        (setf data (parse-integer data))
      (progn
        (setf data (intern (string-upcase data) "SIGMA"))
        (setq pred (find data (graph-predicates cg) :key #'predicate-name))
        (when (and pred (predicate-wm pred))
          (setq data (node-number (predicate-wm pred)))
          )
        (setq c (conditional-from-name data))
        (if c
            (if pred
               ; (format trace-stream "Found predicate WMFN (~S) and conditional function node (~S) by that name." nid (node-number (conditional-function-node c)))
                (setq data (node-number (conditional-function-node c))))
          nil)
        )
      )
    (if (and (typep data 'integer) (node-from-number data))
        (sh data)
      (capi:display-message "The node you are trying to find does not exist")
      )  
    )   
  )


; Highlight a node
(defun highlight-node (node-number &optional (color :green3))
  (let* ((node (node-from-number node-number))
         (object (node-object node))
        )
    (setf (capi:pinboard-object-graphics-args object)
          (list :foreground color)
          )
    )
  )
(defun h (node-number &optional color)
  (unless color (setq color :green3))
  (highlight-node node-number color)
  )

; Scroll to a node
(defun scroll-to-node (node-number)
  (let* ((node (node-from-number node-number))
         (object (node-object node))
         (position (multiple-value-list (capi:pinboard-pane-position object)))
         (size (multiple-value-list (capi:pinboard-pane-size object)))
         (graph (graph-graph cg))
         (graph-size (multiple-value-list (capi:pinboard-pane-size graph)))
        )
    (capi:ensure-area-visible (graph-graph cg)
                              (- (car position) (/ (- (car graph-size) (car size)) 2))
                              (- (cadr position) (/ (- (cadr graph-size) (cadr size)) 2))
                              (car graph-size)
                              (cadr graph-size)
                              )
    (capi:redraw-pinboard-layout graph 0 0 10000 10000)
    )
  )
(defun s (node-number)
  (scroll-to-node node-number)
  )
(defun sh (node-description &optional color)
  (let* ((node-number (find-node-number node-description))
         (node (node-from-number node-number)))
    (when node-number
      (h node-number color)
      (s node-number)
      (print-node node t *node-stream*)
      (when bipartite-graph-display
        (setq *selected-node* (node-object node))
        (capi:redraw-pinboard-layout (graph-graph cg) 0 0 10000 10000)
        (when display-subgraph
          (create-bipartite (node-name node))
          )
        )
      )
    )
  )

; -----------------------------------------------------------
;; Bipartite Graph Display


;;; Reset all the global values;;;

(defun reset-gvalues()
  (setq bp-nodes nil)
  (setq bp-graph nil)
  (setq *bp-node-stream* nil)
  (setq *nodes-depth* 3)
  (setq *main-pinboard* nil)
  (setq *selected-node* nil)
;  (setq display-subgraph nil)  
)


; Print a node symbolically when single click on it
; This is a selection callback
(defun node-selection-callback (data interface)
  (declare (ignore interface))
  (print-node (node-from-name data)
              t
              *node-stream*
              )
  )

; Print a node non-symbolically when double click on it
; This is an action callback
(defun node-action-callback (data)
  (print-node (node-from-name data)
              nil
              *node-stream*
              )
  )

; Given a node and a link, find the other node in the link
(defun next-node (node link)
  (aref (link-nodes link)
        (if (variable-nodep node)
            1
          0)
        )
  )

; Return a list of the names of all of the neighbors of a node
; If graph-alpha is nil, doesn't include match delta nodes (and thus
; none of the alpha network since we start in the beta network)
(defun neighbor-name-list (nodename)
  (let ((node (node-from-name nodename))
        next
        namelist)
    (dolist (link (node-links node))
      (setq next (next-node node link))
      (when (or graph-alpha
                (not (eq (node-subtype next) 'delta))
                (not (eq (node-subsubtype next) 'match)))
        (setq namelist (cons (node-name next) namelist))
        )
      )
    (reverse namelist)
    )
  )
;add isolated nodes to graph
(defun add-isolated-nodes (roots)
  (dolist (n (graph-nodes cg))
    (unless (node-neighbors n)
      (setq roots (push (node-name n) roots))
      )
    )
  roots
  )

(defun add-remaining-nodes (roots)
  (dolist (n (graph-nodes cg))
      (setq roots (push (node-name n) roots))
    )
  roots
  )

; Generate a graphical representation of a graph starting at last node generated
(defun g (&optional size conditional-name)
  (when conditional-name (not (conditional-from-name conditional-name))
    (error "No conditional named ~S in (g)" conditional-name)
    )
  (reset-gvalues)
  (unless (graph-initialized cg)
    (init-graph)
    )
  (unless size (setq size 'medium))
  (if conditional-name
      (graph size (list (node-name (conditional-last-memory (conditional-from-name conditional-name)))))
    (graph size (add-remaining-nodes (mapcar #'(lambda (c) (node-name (conditional-last-memory c)))
                   (graph-conditionals cg)))))
  )

; Print a node non-symbolically when double click on it
; This is an action callback
(defun bp-node-action-callback (data interface)
  (declare (ignore interface))
  (print-node (node-from-name data)
              nil
              *node-stream*
              )
  (create-bipartite data)
  )

(defun bp-get-nodes (nodes)
  (setq bp-nodes nil)
  (let* ((factor nil) (variable nil) (wm nil) (alpha nil) (beta nil) (affine nil) (combine nil) (filter nil) (inversion nil) (transform nil) (perception nil) (function nil)
         )
    (dolist (n nodes)
      (when (factor-nodep n)  (push (node-name n) factor))
      (when (variable-nodep n) (push (node-name n) variable))
      (when (wm-nodep n) (push (node-name n) wm))
      (when (eq (node-subtype n) 'alpha) (push (node-name n) alpha))
      (when (eq (node-subtype n) 'beta) (push (node-name n) beta))
      (when (eq (node-subtype n) 'affine) (push (node-name n) affine))
      (when (eq (node-subtype n) 'combine) (push (node-name n) combine))
      (when (eq (node-subtype n) 'filter) (push (node-name n) filter))
      (when (eq (node-subtype n) 'inversion) (push (node-name n) inversion))
      (when (eq (node-subtype n) 'transform) (push (node-name n) transform))
      (when (eq (node-subtype n) 'perception) (push (node-name n) perception))
      (when (and (factor-nodep n) (eq (node-subtype n) 'function)) (push (node-name n) function))
      )
    (setq bp-nodes (list :factor factor :variable variable :wm wm :alpha alpha :beta beta
                         :affine affine :combine combine :filter filter :inversion inversion :transform transform :perception perception :function function)) 
    )
  )

; Find CF or WMFN node number from conditional or predicate name
(defun find-node-number (nid)
  (let (pred c)
    (when (symbolp nid)
      (setq pred (find nid (graph-predicates cg) :key #'predicate-name))
      (when (and pred (predicate-wm pred))
          (setq nid (node-number (predicate-wm pred)))
          )
      (setq c (conditional-from-name nid))
      (if c
        (if pred
            (format trace-stream "Found predicate WMFN (~S) and conditional function node (~S) by that name." nid (node-number (conditional-function-node c)))
          (setq nid (node-number (conditional-function-node c))))
        nil)
      )
    nid)
  )



(defun node-information (n)
  (let* ((node (node-from-name n)))    
    (format nil "Node name: ~S~%Node type: ~S~%Node Subtype: ~S~%" 
            (node-name node) (node-type node) (node-subtype node) ); (node-evidence node))
    )
  )

(defun get-bp-nodes (node)
  (setq bp-nodes (list node))
  
  ;append the neighbouring nodes
  (let* ((node-list (node-neighbors (node-from-name node))))
    (setq bp-nodes (append bp-nodes node-list))
    )

  ;iterate through all the nodes and get the respective neighbour nodes (3 iteration)
  (dotimes (i *nodes-depth*)
        (dolist (x bp-nodes)
          (let* ((y (node-neighbors (node-from-name x))))
            (setq bp-nodes (append y bp-nodes))
            )
          )
        )
  
  (setq bp-nodes (delete-duplicates bp-nodes))  
)

(defun bp-collector-stream (nodes)
  (dolist (node nodes)
    (print-node (node-from-name node)
                t
                *bp-node-stream*
                )
    )
  )

; Determine whether an edge should be a forward, backward, bidirectional or unpointed arrow
(defun link-direction (from-node to-node)
  (let (from-num to-num)
    (if (variable-nodep from-node)
        (progn
          (setq from-num 0)
          (setq to-num 1))
      (progn
        (setq from-num 1)
        (setq to-num 0)))
    (dolist (link (node-links from-node))
      (when (eq (aref (link-nodes link) to-num) to-node)
        (return (if (aref (link-contents link) from-num)
                    (if (aref (link-contents link) to-num)
                        ':both
                      ':forwards)
                  (if (aref (link-contents link) to-num)
                      ':backwards
                    ':neither)
                  )
                )
        )
      )
    )
  )


; Return node based on its number
(defun number-from-node (node)
  (dolist (n (graph-nodes cg))
    (when (eql node n)
      (return (node-number n)))))

(defun node-children-function (nodename)
  (let ((n nodename) name-list)
    (dolist (n2 bp-nodes)
       (when (eq (link-from-nodes-tf (node-from-name n) (node-from-name n2)) t)
         (setq name-list (cons n2 name-list))
         )       
       )
    (reverse name-list)
    )
  )

(defun link-from-nodes-tf (n1 n2)
  (let ((n2-name (node-name n2))
        (index (if (variable-nodep n2) 0 1))
        )
    (dolist (l (node-links n1))
      (when (eq (node-name (aref (link-nodes l) index)) n2-name)
        (return t)
        )
      )
    )
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; section adapted from circular graph pane example
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass bipartite-graph-class (capi:item-pinboard-object)
  ((indent  :initform 12 :accessor text-in-ellipse-indent  :initarg :indent)
  ; (number :initarg :number :initform 0)
   )
  (:default-initargs 
   ;; Override the defaults from capi:item-pinboard-object
   ;; so capi:calculate-constraints can set the min size.
   :visible-min-width :no-hint
   :visible-min-height :no-hint))

(defmethod capi:calculate-constraints ((self bipartite-graph-class))
  (let ((double-indent (* (text-in-ellipse-indent self) 2))
        )
    (multiple-value-bind
        (left top right bottom)
        (gp:get-string-extent (capi:pinboard-object-pinboard self)
                              (capi:item-text self)
                              (capi:simple-pane-font 
                               (capi:pinboard-object-pinboard self)))
      
      (let* ((x-diameter (+ double-indent (- right left)))
             (y-diameter (+ double-indent (- bottom top)))
             )
        (capi:with-geometry self
          (setf capi:%min-width% x-diameter 
                capi:%min-height% y-diameter
                )
          )
        )
      )
    )
  )

(defun draw-bipartite-graph (pinboard self); &key foreground background)
  (capi:with-geometry self
    (let* ((half-width  (floor (1- capi:%width%)  2))
           (half-height (floor (1- capi:%height%) 2))
           (circle-x (+ capi:%x% half-width))
           (circle-y (+ capi:%y% half-height))
           (rect-x capi:%x%)
           (rect-y (+ capi:%y% 8))
           (font (capi:simple-pane-font
                  (capi:pinboard-object-pinboard self)))
           (ascent (gp:get-font-ascent pinboard font))
           (text (capi:item-text self))
           (node (node-from-name (intern text "SIGMA")))
           (node-type-variable (variable-nodep node))
           (foreground-color (if (variable-nodep node)
                                 (if (wm-nodep node) :red :red4)
                               (if (wm-nodep node) ;:dodgerblue
                                 (if (predicate-prediction (node-predicate node)) :purple4 :dodgerblue)
                                 (if (eq (node-subtype node)'function) :green4
                                   (if (eq (node-subtype node) 'perception) :purple :dodgerblue4)))))
           (filled (if (eq self *selected-node*) nil t))
           (text-color (if filled :white foreground-color))
           )
     
      (multiple-value-bind
          (left top right bottom)
          (gp:get-string-extent (capi:pinboard-object-pinboard self)
                                text
                                font)
        
        (if (eq node-type-variable t)
            (gp:draw-ellipse pinboard
                             circle-x circle-y
                             half-width half-height
                             :filled filled
                             :foreground foreground-color
                             )
          (gp:draw-rectangle pinboard
                             rect-x rect-y
                             (* half-width 2) (* half-height 1.5)
                             :filled filled
                             :foreground foreground-color
                             )
          )
        
        (gp:draw-string pinboard
                        text 
                        (+ capi:%x% (floor (- capi:%width% (- right left)) 2))
                        (+ capi:%y% (floor (- capi:%height% (- bottom top)) 2)
                           ascent)
                        :foreground text-color
                        :font font)
        )
      )    
    )    
  )

(defmethod capi:draw-pinboard-object (pinboard (self bipartite-graph-class)
                                               &key &allow-other-keys)
  (draw-bipartite-graph pinboard self)
  )

(defmethod capi:draw-pinboard-object-highlighted ((pinboard capi:pinboard-layout)
                                                  (self bipartite-graph-class)
                                                  &key &allow-other-keys)
  (draw-bipartite-graph pinboard self)
  )

;(defmethod capi:draw-pinboard-object-unhighlighted ((pinboard capi:pinboard-layout)
;                                                    (self text-in-ellipse)
;                                                    &key &allow-other-keys)
;  (draw-text-in-ellipse pinboard self
;                        :background (capi:simple-pane-background pinboard)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; end of adaptation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; -----------------------------------------------------------
; Parameters for menu & loading of global variables

(defparameter sigma-menubar-functions '((regression-testing . "Regression Testing") (global-variables . "View/Set Global variables")
                                       (debug-messages . "Debug Messages")))

(defparameter track-gv nil)
(defparameter track-regression nil)



(capi:define-interface bipartite()
  ()
  (:panes
   (bp-graph-1 capi:graph-pane
             :roots bp-nodes
             :visible-min-width 1000
             :children-function #'node-children-function
             :layout-function :left-right
             :highlight-style :default
             :visible-min-height 400
             :edge-pane-function #'bp-edge-function
             :node-pane-function  #'bp-node-function
             :selection-callback #'bp-node-selection-callback
             :action-callback #'bp-node-action-callback
             )
   (bp-node-collector capi:collector-pane
                      :visible-min-width 900
                      :visible-min-height 300
                      )
   )
  (:layouts
   (bp-row capi:column-layout
           '(bp-graph-1 bp-node-collector)
           :y-ratios '(2 1)))
  (:default-initargs :title "Subgraph Pane"))

(defun create-bipartite (node)
  (get-bp-nodes node)
  (let (graph)
    (setq graph (make-instance 'bipartite))
    (setq *bp-node-stream* (capi:collector-pane-stream (slot-value graph 'bp-node-collector)))
    (capi:display graph)
    )
)

(defun display-tooltip (pane x y)
  (let* ((object (capi:pinboard-object-at-position pane x y)))
    (if (capi:itemp object)        
        (let ((help-text (node-information (intern (capi:item-text object) "SIGMA"))))
          (capi:with-geometry object
            ;; Display a tooltip for this item, positioned where the item starts.
            (capi:display-tooltip pane
                                  :x capi:%x%
                                  :y (+ capi:%y% 30)
                                  :text help-text)))
      )             
    )
  )

;similar to the edge graph function - included possibility of creating link to itself
(defun bp-edge-function (self from to)
  (declare (ignore self))
  (let* ((direction 
          (if (eq (link-direction (node-from-name from) (node-from-name to)) nil)
              ':neither
            (link-direction (node-from-name from) (node-from-name to))
            )
          ))
    (apply #'make-instance
           (cond ((eq direction ':both) 'capi:double-headed-arrow-pinboard-object)
                 ((eq direction ':neither) 'capi:line-pinboard-object)
                 (t 'capi:arrow-pinboard-object)
                 )
           (if (or (eq direction ':both) (eq direction ':neither))
               nil
             (list :head-direction direction )
             )           
           )
    )
  )

(defun bp-node-function (pane item)
  ;new node look
  (setf (node-object (node-from-name item))
          (apply 
           #'make-instance 'bipartite-graph-class
           :text (capi:print-collection-item item pane)
           (list :data item )
           )
          )    
  )

(defvar menu-added 0)
(defvar listeners nil)
(defvar editors nil)
(defun add-sigma-menu ()
 (setf *package* (find-package "SIGMA"))
  (setf editors (capi:collect-interfaces 'lispworks-tools:editor))
  (setf listeners (capi:collect-interfaces 'lispworks-tools:listener))
  (if (and (> (length listeners) 0) (= menu-added 0))
      (progn
	(sigma-menu)
	(incf menu-added))))

(defun sigma-menu ()
    (setq info-stream t)
    (dolist (editor editors)
      (create-sigma-menus editor 'editor)
      )
    (dolist (listener listeners)
      (create-sigma-menus listener 'listener)
      )
    )

(defun create-sigma-menus (interface type)
  (let* ((interf interface)
         (menu-bar-items (capi:interface-menu-bar-items interf))
         (sigma-menu (make-instance 'capi:menu
                                    :items (mapcar #'car sigma-menubar-functions)                                
                                    :print-function #'(lambda (function)
                                                        (string-capitalize (cdr (assoc function sigma-menubar-functions)))
                                                        )
                                    :title "Sigma"
                                    :callback-type :data
                                    :callback 'sigma-menubar-functions-callback
                                    )
                     )
         )
    (setf sigma-menu (add-sigma-submenu sigma-menu 'sigma-toolbar '(all run-programs print-definitional-groups print-other-structures)  "Sigma Toolbar" ))
    (setf sigma-menu (add-sigma-submenu sigma-menu 'g '(small medium large)  "Graphical Interface (g)" ))
    (case type
      (editor
       (when (> (list-length menu-bar-items) 9)
         (remove-sigma-menu interf)
         ))
      (listener 
       (when (> (list-length menu-bar-items) 8)
         (remove-sigma-menu interf)
         )
       )
      (regression 
       (when (> (list-length menu-bar-items) 2)
         (remove-sigma-menu interf)
         )
       )
      ) 
    (setf (capi:interface-menu-bar-items interf)
          (append 
           (capi:interface-menu-bar-items interf)
           (list sigma-menu)
           ))
    )
  )

(defun remove-sigma-menu (interface)
  (let* ((interf interface))
    (setf (capi:interface-menu-bar-items interf)
          (append 
           (butlast (capi:interface-menu-bar-items interf))
           '()
           ))
    )
  )

(defun add-sigma-submenu (menu function items title)
  (let ((submenu menu))
    (setf (capi:menu-items submenu)
          (append 
           (capi:menu-items submenu)
           (list 
            (make-instance 'capi:menu
                           :items items
                           :title title 
                           :print-function 'string-downcase
                           :callback-type :data
                           :callback #'(lambda (data)
                                         (if (eq data 'default)
                                             (execute-on-listener function nil)
                                           (execute-on-listener function (list data))
                                           ))
                           ))
           )
          )
    submenu
    )
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Menubar callback functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun sigma-menubar-functions-callback (data)
  (case data
    (regression-testing (load-regression))
    (global-variables (load-global-variables))
    (debug-messages (load-debug-messages))
    )
  )

(defun load-global-variables ()
  (let ((gv (set-global-variables)))
    (when track-gv
      (capi:destroy track-gv)
      )
    (setq track-gv gv)
    (create-sigma-menus gv nil)
    )
  )


(defun load-regression ()
  (let ((reg (regression-testing)))
    (create-sigma-menus reg 'regression)
    )
  )

(defun load-debug-messages ()
  (let ((prompt  (prompt-for-arguments "Debug Message (use node number)" "From Node Number:" "To Node Number" t)))
    (when prompt (execute-on-listener 'debug-message prompt))
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Peompting the dialog for arguments and executing on listener pane
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun prompt-for-arguments (title arg-1 &optional arg-2 imp)
  (let* ((inp-1-val)
         (input-1 (make-instance 'capi:text-input-pane
                                 :callback-type :data
                                 :change-callback #'(lambda (data pane interface te)
                                                      (declare (ignore pane interface te))
                                                      (setq inp-1-val data)
                                                      )
                                 :callback #'(lambda (data)  
                                               (if arg-2
                                                   (progn
                                                     (if imp
                                                         (capi:popup-confirmer nil (format nil "Please make sure you have entered value for ~S" arg-2)
                                                                               :cancel-button nil)
                                                       (text-return-callback data nil)
                                                       )
                                                     )
                                                 (text-return-callback data)
                                                 )
                                               )
                                 ))
         (title-1 (make-instance 'capi:row-layout
                                 :description (list input-1)
                                 :title arg-1
                                 :title-position :frame))
         (input-2 (if arg-2 (make-instance 'capi:text-input-pane
                                           :callback-type :data
                                           :callback #'(lambda (data)                                                         
                                                         (text-return-callback inp-1-val data)
                                                         )
                                           ) nil))
         (title-2 (if arg-2 (make-instance 'capi:row-layout
                                           :description (list input-2)
                                           :title arg-2
                                           :title-position :frame) nil))
         (buttons (make-instance 'capi:push-button-panel
                                 :items '(:ok :cancel)
                                 :callback-type :data
                                 :default-button :ok
                                 :print-function #'(lambda (x)
                                                     (ecase x (:ok "OK") (:cancel "Cancel")))
                                 :selection-callback #'(lambda(data)
                                                         (if (equal data :ok)
                                                             (argument-callback input-1 input-2) 
                                                           (capi:abort-dialog)
                                                           ))
                                 ))
         
         (main (make-instance 'capi:column-layout
                              :description (list title-1 title-2 buttons)
                              :title title
                              :title-position :frame
                              :visible-min-width 200
                              :automatic-resize nil
                              ))
         
         )
    (capi:display-dialog (capi:make-container main))
    )
  )


(defun text-return-callback(data-1 &optional data-2)
  (if (or (equal data-1 "") (equal data-2 ""))
      (capi:abort-dialog)
    (progn
      (if data-2
          (capi:exit-dialog (list data-1 data-2))
        (capi:exit-dialog (list data-1))
        )
      )
    )
  )

(defun argument-callback (input-1 input-2)
  (let* ((arg-1 (capi:text-input-pane-text input-1))
         (arg-2 (if input-2 (capi:text-input-pane-text input-2) nil))
         )
    (if (not (equal arg-1 ""))
        (progn
          (if arg-2 (capi:exit-dialog (list arg-1 arg-2))
            (capi:exit-dialog (list arg-1)))
          )
      (progn
        (capi:abort-dialog)
        ;(capi:display-dialog "Please provide an argument")
        )
      )
    )
  )

(defun execute-on-listener (function arguments)
  (capi:map-pane-descendant-children
   (first (capi:collect-interfaces 'lispworks-tools:listener))
   #'(lambda (l)
       (when (eq (type-of l) 'capi:listener-pane)
         (if (eq arguments nil)
             (capi:interactive-pane-execute-command l (format nil "(~S)" function))
           (capi:interactive-pane-execute-command l (format nil "(~S ~{'~a ~})" function arguments))
           )
         )
       )
   )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Toolbar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconstant rdt-toolbar-functions '((r . "Number of messages:") (d . "Number of decisions:") (t . "Number of trials:")))
(defconstant ds-toolbar-functions '((PTS . nil) (PPS . nil) (PCS . "Current functions (T)")))
(defconstant others-toolbar-functions '((PAM . "Conditional Name:") (PC . "Conditional Name:") (PCF . "Conditional Name:") (PCFS . nil) (PEM . nil) (PID . "ID Number:") (PIDS . nil) (PPPFN . "Predicate Name:") (PPREFS . nil) (PQS . nil) (PWM . nil) (PPWM . "Predicate Name:") (PPFN . "Predicate Name:")))
(defconstant sigma-toolbar-pop '(PID PC PCF PPPFN PPFN ))
(defconstant sigma-toolbar-both '(r d t PCS PAM PPWM))
(defparameter function-list nil)  
(defparameter button-functions nil)

(capi:define-interface others-toolbar ()
  ()
  (:panes
   (sig-toolbar
    capi:toolbar
    :items function-list
    )     
   )  
  )

(capi:define-interface rdt-toolbar ()
  ()
  (:panes
   (rdt capi:toolbar
        :items function-list
        )
   )
  )
(capi:define-interface def-struct-toolbar ()
  ()
  (:panes
   (def-struc capi:toolbar
              :items function-list)
   )
  )

(defun create-toolbar-buttons (t-functions)
  (dolist (f (mapcar #'car t-functions))
    (setq function-list (append function-list
                                (list 
                                 (cond
                                  ((member f sigma-toolbar-pop)
                                   (make-instance 'capi:toolbar-button
                                                  :text (string-downcase (symbol-name f))
                                                  :callback-type :none
                                                  :selection-callback #'(lambda ()
                                                                          (let ((prompt (prompt-for-arguments "Enter argument" 
                                                                                                              (string-capitalize (cdr (assoc f t-functions)))
                                                                                                              )))
                                                                            (when prompt (execute-on-listener f  prompt))
                                                                            )
                                                                          )
                                                  )
                                   )
                                  ((member f sigma-toolbar-both)
                                   (make-instance 'capi:toolbar-button
                                                  :text (string-downcase (symbol-name f));(string-capitalize (symbol-name f))
                                                  :callback-type :none
                                                  :selection-callback #'(lambda ()
                                                                          (execute-on-listener f nil)
                                                                          )
                                                  :dropdown-menu-kind :delayed
                                                  :dropdown-menu (if (eq f 'ppwm) (select-dropdown f (cdr (assoc f t-functions)) "as-array") 
                                                                   (select-dropdown f (cdr (assoc f t-functions))))
                                                  )
                                   )
                                  (t 
                                   (make-instance 'capi:toolbar-button
                                                  :text (string-downcase (symbol-name f))
                                                  :callback-type :none
                                                  :selection-callback #'(lambda ()
                                                                          ;(function-callback f interface)
                                                                          (execute-on-listener f nil)
                                                                          )
                                                  )
                                   ))
                                 )))
    
    )
  )

(defparameter drop-down '((r . ((1 . "(r 1)") (optional . "# messages")))
                          (d . ((1 . "(d 1)") (optional . "# decisions")))
                          (t . ((1 . "(t 1)") (optional . "# trials")))
                          (others . (optional "enter arguments"))))

(defun select-dropdown (f title &optional title-2)
  (make-instance 'capi:menu
                 :items (if (eq (find f (mapcar #'car drop-down)) nil)
                            (list (nth 1 (assoc 'others drop-down)))
                          (mapcar #'car (cdr (assoc f drop-down))))                       
                 :print-function #'(lambda (item)
                                     (if (eq (find f (mapcar #'car drop-down)) nil)
                                         (nth 1 (member 'optional (cdr (assoc 'others drop-down))))
                                       (cdr (assoc item (cdr (assoc f drop-down)))))                                     
                                     )
                 :callback-type :data
                 :callback #'(lambda (data)
                               (if (eq data 'optional)
                                 (progn
                                   (let ((prompt (prompt-for-arguments 
                                                  "Enter optional argument" title (when title-2 title-2) (when title-2 nil))))
                                     (when prompt (execute-on-listener f prompt))
                                     )
                                   )
                                 (execute-on-listener f (list data))
                                 )
                               )
                 )
  )



(defun create-toolbar(toolbar pos-x)
  (setq function-list (list (make-instance 'capi:toolbar-button
                                           :image 5
                                           :callback-type :interface
                                           :callback #'(lambda(interface)
                                                         (capi:destroy interface)
                                                         (setq function-list '())
                                                         )
                                             ;:help-key :text
                                           )))
  (case toolbar
    ('rdt-toolbar (create-toolbar-buttons rdt-toolbar-functions))
    ('def-struct-toolbar (create-toolbar-buttons ds-toolbar-functions))
    ('others-toolbar (create-toolbar-buttons others-toolbar-functions))
    )
  (let ((tbar-dimension)
        (tbar  (make-instance toolbar
                              :window-styles '(:hides-on-deactivate
                                               :borderless
                                               :movable-by-window-background
                                               )
                              :best-x pos-x
                              :best-y 10
                 ;:help-callback 'toolbar-help
                              )))
    (capi:display tbar)
    (setq function-list nil)
    (setq tbar-dimension (capi:interface-geometry tbar))
    (+ (car tbar-dimension) (nth 2 tbar-dimension))
    )
  )

(defun sigma-toolbar (tool-bar)
  (let* ((width)
         (rdt (capi:locate-interface 'rdt-toolbar))
         (ds (capi:locate-interface 'def-struct-toolbar))
         (others (capi:locate-interface 'others-toolbar))
         )
    (case tool-bar
      ('all 
       (when rdt (capi:destroy rdt))
       (when ds (capi:destroy ds))
       (when others (capi:destroy others))
       (setq width (create-toolbar 'rdt-toolbar 10))
       (setq width (create-toolbar 'def-struct-toolbar (+ width 1)))
       (create-toolbar 'others-toolbar (+ width 1))
       )     
      ('print-definitional-groups 
       (if ds (capi:find-interface 'def-struct-toobar) (create-toolbar 'def-struct-toolbar 190))
       )
      ('run-programs
       (if rdt (capi:find-interface 'rdt-toolbar) (create-toolbar 'rdt-toolbar 10))
       )
      ('print-other-structures
       (if others (capi:find-interface 'others-toolbar) (create-toolbar 'others-toolbar 373)))
      )
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; function to add SIGMA menu bar to the Lispworks Menu bar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Checking if the file exists READD FOR PKG
#|
(defun check-regression-exists()
(format t (concatenate 'string (namestring (merge-pathnames regression-file (user-homedir-pathname)))))
  (let ((filename-string (concatenate 'string (namestring (merge-pathnames regression-file (user-homedir-pathname)))))
         )
    (if (probe-file filename-string)
	(loop for f in (directory regression-path)
	     collect (compile-file-if-needed f :load t :in-memory t)))))


        (compile-file-if-needed (merge-pathnames regression-file (user-homedir-pathname)) :load t :in-memory t)
      (setq sigma-menubar-functions (remove 'regression-testing sigma-menubar-functions :key 'car))
      )
    )
  )

(check-regression-exists)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; adding multicore interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun check-multicore-exists()
(format t (concatenate 'string (namestring (merge-pathnames multicore-file (user-homedir-pathname)))))
  (let ((filename-string (concatenate 'string (namestring (merge-pathnames multicore-file (user-homedir-pathname)))))
         )
    (if (probe-file filename-string)
        (compile-file-if-needed (merge-pathnames multicore-file (user-homedir-pathname)) :load t :in-memory t)
      )
    )
)

(check-multicore-exists)
|#
;list with variable name as key and new value as value
(defparameter new-value-and-pane '())
(defparameter old-value-and-pane '())

(defun set-global-value (variable)
  (let* ((old-val-pane (cdr (assoc variable old-value-and-pane)))
         (new-val-pane (cdr (assoc variable new-value-and-pane)))
         )
    (set variable (read-from-string (capi:text-input-pane-text new-val-pane)))
    (capi:apply-in-pane-process old-val-pane 
                                #'(setf capi:title-pane-text) (capi:text-input-pane-text new-val-pane) old-val-pane)                          
    (setf (capi:text-input-pane-text new-val-pane) nil)
    )
  )

(defun set-global-variables ()
  (let* ((gv-interface)
         (refresh-button (make-instance 'capi:push-button
                                        :text "Reload"
                                        :callback-type :interface
                                        :selection-callback #'(lambda (interface)
                                                                (capi:destroy interface)
                                                                (if (fboundp 'create-sigma-menus)
                                                                    (create-sigma-menus (set-global-variables) nil)
                                                                  (set-global-variables))
                                                                )))
         (name (make-instance 'capi:title-pane
                              :text "Name"
                              :font (graphics-ports:make-font-description
                                     :family "times"
                                     :size 16
                                     :weight :bold
                                     :slant :roman)))
         (value (make-instance 'capi:title-pane
                               :text "Current Value"
                               :font (graphics-ports:make-font-description
                                      :family "times"
                                      :size 16
                                      :weight :bold
                                      :slant :roman)))
         (new-value (make-instance 'capi:title-pane
                                   :text "New Value"
                                   :font (graphics-ports:make-font-description
                                          :family "times"
                                          :size 16
                                          :weight :bold
                                          :slant :roman)))
         (update (make-instance 'capi:title-pane
                                :text "Update"
                                :font (graphics-ports:make-font-description
                                       :family "times"
                                       :size 16
                                       :weight :bold
                                       :slant :roman)))
         (hint (make-instance 'capi:title-pane
                              :text "Possible/Default Values"
                              :font (graphics-ports:make-font-description
                                     :family "times"
                                     :size 16
                                     :weight :bold
                                     :slant :roman)))
         (variable-grid nil)
         (new-vals-panes '())
         (old-value-panes '())  
         (row-list '())
         (global-vars (mapcar 'car *global-variables*))
         )
    (setq row-list (list name value new-value update hint))
    (dolist (gv global-vars)
      (let* ((gv-value (eval gv))
             (name-field (make-instance 'capi:title-pane
                                        :text (symbol-name gv)))
             (name-description (make-instance 'capi:display-pane
                                              :text (caddr (assoc gv *global-variables*))
                                              :font (graphics-ports:make-font-description
                                                     :family "times"
                                                     :size 12                                       
                                                     :slant :italic)
                                              :foreground :blue4
                                              ))
             (name-desc (make-instance 'capi:column-layout
                                       :description (list name-field name-description)))
             (value-field (make-instance 'capi:title-pane
                                         :text (format nil "~S" gv-value)))
             (new-value-field (make-instance 'capi:text-input-pane))
             (update-field (make-instance 'capi:push-button
                                          :text "Update"
                                          :callback-type :none
                                          :selection-callback #'(lambda ()
                                                                  (set-global-value gv))
                                          ))
             (hint-field (make-instance 'capi:title-pane
                                        :text (cadr (assoc gv *global-variables*))
                                        :font (graphics-ports:make-font-description
                                               :family "times"
                                               :size 12
                                               :slant :roman)
                                        ))
             )
        (setq new-vals-panes (append new-vals-panes (list new-value-field)))
        (setq old-value-panes (append old-value-panes (list value-field)))
        (setq row-list (append row-list (list name-desc value-field new-value-field update-field hint-field)))       
        )
      )
    (setq new-value-and-pane (pairlis global-vars new-vals-panes))
    (setq old-value-and-pane (pairlis global-vars old-value-panes))
    (setq variable-grid  (make-instance 'capi:grid-layout
                                        :description row-list
                                        :columns 5
                                        :x-gap 15
                                        :y-gap 15
                                        :has-title-column-p t
                                        :vertical-scroll t
                                        ))
    (setq gv-interface (make-instance 'capi:column-layout
                                      :description (list refresh-button variable-grid)
                                      :visible-min-height (* (capi:screen-height (capi:convert-to-screen)) 0.80)
                               ;:visible-min-height 400
                                      :height (* (capi:screen-height (capi:convert-to-screen)) 0.90)))
    (capi:contain gv-interface
                  :title "Set Global Variables"
                  :best-y 10)
    (capi:element-interface gv-interface)
    )
  )

; Bring up menu if not a listener within parallel regression testing
;(unless (or sigma::from-regression sigma::from-parallel)
;  (sigma-menu)
;  )

(unless (or (boundp 'from-regression) from-parallel)
  (sigma-menu)
  )

