(in-package :sigma)
; defining graph components &
; compileing knowledge structures in cognitive architecture
; into nodes and links in graphical architecture

; LOW LEVEL
; -----------------------------------------------------------
; Define graph structure

; Add a node to a graph
(defun add-node (node)
  (setf (node-number node) (graph-node-count cg))
  ; Tack node number onto front of name
  (setf (node-name node) (make-name-number-symbol (node-name node) (node-number node)))
  (setf (graph-last-node cg) node)
  (setf (graph-nodes cg) (cons node (graph-nodes cg)))
  (setf (graph-node-count cg) (+ (graph-node-count cg) 1))
  node)

; Determine if a node is a leaf (only linked to one other)
(defun leaf-node (n)
  (= (length (node-links n)) 1)
  )

; Add a link to a graph and to relevant nodes
(defun add-link (link)
  (setf (graph-links cg) (cons link (graph-links cg)))
  (dotimes (i 2)
    ; add- link to node in link
    (setf (node-links (link-node link i))
          (cons link (node-links (link-node link i))))
    )
  link)

; Make a new variable node for variables 'variables'
(defun init-variable-node (name subtype subsubtype action variables evidence &optional pattern-type)
  (let (vn)
    (setq vn (make-node :name name
                        :type 'variable
                        :subtype subtype
                        :action action
                        :subsubtype subsubtype
                        :evidence evidence
                        :variables variables
                        :pattern-type pattern-type))
    (add-node vn))
  )

; Is variable name in list of vectors?
(defun var-in-lov (v lov)
  (dolist (vs lov nil)
    (when (find v vs) (return t))
    )
  )

; Check one or more variables to see if they are in a list of vectors
; If only one, it will be by itself, otherwise a list
(defun vars-in-lov (vs lov)
  (if (listp vs)
      (dolist (v vs nil)
        (when (var-in-lov v lov) (return t))
        )
    (var-in-lov vs lov))
  )

; Create a map-vfactor
(defun build-smap-vfactor (vn-vars fn-vars)
  (let* ((rank (length vn-vars)) ; Number of variables in variable node
         (m-vfactor (init-vector rank)) ; Initial mapping from variable to factor node
         )
    ; Determine mapping from variable node variables to factor node variables
    (dotimes (i rank)
      (setf (aref m-vfactor i) (variable-number (aref vn-vars i) fn-vars))
      )
    m-vfactor)
  ) 

; Create a map-fvar.  For the vfactor, it provides an index that tells you how the fn variables that exist in the vn
; are ordered in the vn.  By using it, can cycle through the vn variables in the same order they are used in the fn.
(defun build-smap-fvar (v)
  (let* ((s (sort (copy-seq v) #'<))
         (d (length s))
         (o (init-vector d)))
    (dotimes (i d)
      (setf (aref o i) (position (aref s i) v))
      )
    o)
  )

; Create a vector of length corresponding to fn-vars, with a NIL when in vn-vars
; and how many vn-vars have already been seen otherwise
(defun build-smap-omitted (m-vfactor fn-vars)
  (let* ((rank (length fn-vars))
         (omitted (init-vector rank))
         (count 0)
         )
    (dotimes (i rank)
      (if (find i m-vfactor)
          (progn
            (setq count (+ count 1))
            (setf (aref omitted i) nil)
            )
        (setf (aref omitted i) count)
        )
      )
    omitted)
  )

; Build a map from a variable node to a factor node
(defun build-smap (vn-vars fn-vars)
  (if (= (length vn-vars) 0)
      nil
    (let ((m-vfactor (build-smap-vfactor vn-vars fn-vars)))
      (make-smap :vfactor m-vfactor
                 :fvar (build-smap-fvar m-vfactor)
                 :omitted (build-smap-omitted m-vfactor fn-vars))
      )
    )
  )

; Determine if variables in first vector exist in same order in second vector
; Used for determining if variables in variable nodes and factor nodes are order compatible
; If not, and can't reorder one or the other, need to add a dynamic reordering during execution
(defun variable-subset-order-compatible (vars1 vars2)
  (let ((vars2-rank (length vars2)) ; Length of second vector
        (vars2-i 0) ; Start search second vector from front
        vn1) ; Name of variable from first vector
    (dotimes (vars1-i (length vars1)) ; Iterate through first vector
      (setq vn1 (svariable-name (aref vars1 vars1-i))) ; Name of current variable in first vector
      (loop until (or (>= vars2-i vars2-rank) ; Quit when have finished search second vector or found name
                      (if (listp (aref vars2 vars2-i)) ; If this is a delta or transform factor with lists of variables
                          (member vn1 (aref vars2 vars2-i) :key #'svariable-name) ; See if name in list
                        (eq vn1 (svariable-name (aref vars2 vars2-i))))) ; See if same name
            do (setq vars2-i (+ vars2-i 1))) ; Increment pointer to second list
      )
    (if (>= vars2-i vars2-rank) nil t)) ; If have finished search of second, then not found
  )


(defvar reorder-factor-steps nil)

; Create a factor node
; Evidence is the variable node that provides evidence for this factor, or t if this is a WM factor or nil if there is none
; If in is true then messages flow from non-evidence variables and towards evidence variables (for actions and condacts)
; If out is true then messages flow from evidence variables and towards non-evidence variables (for conditions and condacts)
; If both in and out are true, messages flow to and from all variables
; If it is a beta node, then beta is true and in/out just reflect what happens for alpha-memory
; Doesn't initialize function.  Do this after so that the regions in the function can be grounded in this node.
(defun init-factor-node (name subtype variables var-nodes evidence in out beta action c &optional subsubtype)
  (let ((fn (make-node :name name :type 'factor :subtype subtype :variables variables
                       :evidence evidence :subsubtype subsubtype :action action)) ; Factor node
        step ; Maximization step
        steps ; Generic list of steps to be taken for this node, but modified by which output computing
        link ; Link between factor and variable node
        map ; Map from variable node variables to factor node variables
;        evnode ; Whether the current variable node is the evidence (alpha memory)
;        condactp ; Whether beta factor (if it is one) is for a condact
        alpha-mem new-beta-mem old-beta-mem ; Memory nodes tied to a beta factor
        vs-links ; Vector of lists of links whose variable nodes use particular factor variables
        vsl-i ; Index into vs-links
        link-product ; List of variable node names that have been included as product factor steps
        link-node-name ; Link node name for use in link-product
        fv ; Factor node variable
        reordered-variables ; Variables by increasing volume
        unique-variables ; There is at least one unique variable
        )
    ; Reorder variables so as to (hopefully) reduce cost of products
    (when (and reorder-factor-steps (not (find-if #'listp variables)))
      (setq reordered-variables (reorder-factor-variables variables))
      )
    (when c
      (setf (node-integral fn) (not (conditional-map c)))
      )
    (setq vs-links (init-vector (length variables)))
    (add-node fn)
    (when beta
;      (setq condactp (and in out))
      (setq alpha-mem (beta-memories-alpha beta))
      (setq old-beta-mem (beta-memories-old-beta beta))
      (setq new-beta-mem (beta-memories-new-beta beta))
      )
    ; Cycle through variable nodes, creating maps and links
    (dolist (vnode var-nodes)
      (setq map (build-smap (node-variables vnode) variables)) ; Build map from variable node variables to factor node variables
      (setq link (make-link :map map
                            :nodes (vector vnode fn) :contents (vector nil nil)
                            :depths (vector nil nil)
                            :loop-depths (vector nil nil)
                            :inits (vector -1 -1)
                            :incoming (vector nil nil)
                            :in (if beta
                                    (if one-way-c-a-betas
                                        (if (equal vnode alpha-mem) 
                                            in ; Connect alpha in its direction
                                           (if (equal vnode old-beta-mem) ; Always get input from old beta 
                                               t
                                             (if (equal vnode new-beta-mem)
                                                 (eq (node-pattern-type vnode) 'condact) ; Only get input from new beta if condact
                                               nil
                                               ))) 
                                      (or in (not (equal vnode alpha-mem)))) ; Bidirectional except for wrong way on alpha
                                  in )
                            :out (if beta
                                    (if one-way-c-a-betas
                                        (if (equal vnode alpha-mem) 
                                      out ; Connect alpha in its direction
                                           (if (equal vnode new-beta-mem) ; Always send output to new beta
                                               t
                                            (if (equal vnode old-beta-mem)
                                                 (eq (node-pattern-type vnode) 'condact)
                                              out
                                                 ))) ; Only send output to old beta if condact
                                      (or out (not (equal vnode alpha-mem)))) ; Bidirectional except for wrong way on alpha
                                  (and out (not (eq (node-subtype vnode) 'offset))))
                            :counts (vector 0 0)
                            :stale (vector nil nil)
                            :prequeue (vector nil nil)
                            )
            )
      ; Add link to graph
      (add-link link)
      ; Add link to lists for mapped factor variables when used in variable node
      (dotimes (i (length (node-variables vnode)))
        (setq vsl-i (aref (smap-vfactor map) i))
        (setf (aref vs-links vsl-i) (cons link (aref vs-links vsl-i)))
        )
      )
    (setf (node-links fn) (reverse (node-links fn))) ; Make link ordering the same as variable node ordering
    ; Cycle through unique variables in factor node, setting up products and summarizations
    (dotimes (i (length variables))
      (setq fv (aref (if reordered-variables reordered-variables variables) i))
      (unless (multiple-variable fv)
        (unless unique-variables (setq unique-variables t))
        (dolist (link (aref vs-links (if reordered-variables (position (svariable-name (aref reordered-variables i)) variables :key #'svariable-name) i)))
          (setq link-node-name (node-name (link-var-node link)))
          (unless (member link-node-name link-product) ; Not already added link product to factor steps
            (setq steps (append steps (list (make-factor-step :type 'product :argument link))))
            (setq link-product (cons link-node-name link-product))
            )
          )
        ; Set max by default and then switch to integral under the right circumstances
        (setq step (make-factor-step :type 'max :argument i))
        (when (and (node-integral fn) ; The summarization operator for this node is integral
                   default-integral ; Don't switch all integrals to maxes
                   c ; This use is part of a conditional rather than initializing WM
                   (or (not always-max-operators) ; Not always using max for operators/actions
                       (not (eq (stype-name (svariable-type fv))
                                (graph-operator-type-name cg))); Not operator variable
                       )
                   )
          (setf (factor-step-type step) 'integral))
        (setq steps (append steps (list step)))
        )
      )
    ; Cycle through universal variables in factor node, setting up products and summarizations
    (dotimes (i (length variables))
      (setq fv (aref (if reordered-variables reordered-variables variables) i))
      (when (multiple-variable fv)
        (dolist (link (aref vs-links (if reordered-variables (position (svariable-name (aref reordered-variables i)) variables :key #'svariable-name) i)))
          (setq link-node-name (node-name (link-var-node link)))
          (unless (member link-node-name link-product) ; Not already added link product to factor steps
            (setq steps (append steps (list (make-factor-step :type 'product :argument link))))
            (setq link-product (cons link-node-name link-product))
            )
          )
        ; Use max unless integrating universal in unique and there are unique variables
        (setq step (make-factor-step :type (if (and integrate-universal-in-unique unique-variables) 'integral 'max) :argument i))
        (setq steps (append steps (list step)))
        )
      )
    ; Cycle through variable nodes, setting up products for any that don't already have them
    ; This should only be variable nodes with no variables
    (dolist (vlink (node-links fn))
      (setq link-node-name (node-name (link-var-node vlink)))
      (unless (member link-node-name link-product)
        (setq steps (append steps (list (make-factor-step :type 'product :argument vlink))))
        )
      )
    (setf (node-factor-steps fn) steps)
    fn)
  )

; Create a graph
(defun create-graph ()
  (make-graph :node-count 0
              )
  )

; Get Node and Link info
; -----

; Return node based on its name
(defun node-from-name (name &optional type)
  (dolist (n (graph-nodes cg))
    (when (and (eq name (node-name n)) ; Node is of the right name
               (or (not type) ; Don't care what kind of node it is
                   (eq (node-type n) type))) ; Node is of right type
      (return n))))

; Return node based on its number
(defun node-from-number (number)
  (if (graph-node-vector cg)
      (aref (graph-node-vector cg) number)
    (dolist (n (graph-nodes cg))
      (when (= number (node-number n))
        (return n))))
  )
(defun nfn (number) (node-from-number number))

; Node function from number
(defun node-function-from-number (number)
  (node-function (node-from-number number))
  )
(defun nffn (number)
  (node-function-from-number number)
  )

; Retrieve a link based on the two nodes that participate in it
(defun link-from-nodes (n1 n2)
  (let ((n2-name (node-name n2))
        (index (if (variable-nodep n2) 0 1))
        )
    (dolist (l (node-links n1))
      (when (eq (node-name (aref (link-nodes l) index)) n2-name)
        (return l)
        )
      )
    )
  )

; Dimensions
; ----------

; Access dimension min and max from slices
(defun dimension-min (type) (slice-location (dimension-min-slice type)))
(defun dimension-max (type) (slice-location (dimension-max-slice type)))

; Access a dimension of a region
(defun dimension (r d)
  (aref (region-dimensions r) d))

; Span of a region's dimension
(defun dimension-span (d)
  (- (slice-location (dimension-max-slice d)) (slice-location (dimension-min-slice d)))
  )

; Retrieve a link boad on the numbers of the two nodes that participate in it
(defun link-from-numbers (num1 num2)
  (link-from-nodes (node-from-number num1) (node-from-number num2))
  )

; Return list of names of node's neighbors (in order of links)
(defun node-neighbors (n)
  (mapcar #'(lambda (l) (node-name (aref (link-nodes l) (if (variable-nodep n) 1 0)))) (node-links n))
  )

; Get node from a link based on index
(defun link-node (l i)
  (aref (link-nodes l) i))   

; Retrieve the variable and factor nodes from a link
(defun link-var-node (l)
  (link-node l var-index))
(defun link-fact-node (l)
  (link-node l fact-index))

; Get content from a link based on index
(defun link-content (l i)
  (aref (link-contents l) i))

; Retrieve the outgoing variable and factor contents from a link
(defun link-var-content (l)
  (link-content l var-index))
(defun link-fact-content (l)
  (link-content l fact-index))

; -----------------------------------------------------------
; Compute node dependencies

; Is node a WM node?
(defun wm-nodep (node)
  (eq (node-subtype node) 'wm)
  )

; Is node a WM FN node?
(defun wm-fnp (node)
  (and (factor-nodep node) (wm-nodep node))
  )

; Is node an ever, selection, state WM FN node?
(defun ess-wm-fnp (node)
  (let ((pred (node-predicate node)))
    (and (wm-fnp node)
         pred
         (predicate-persistent pred)
         (predicate-select pred)
         (state-predicate pred)
         )
    )
  )

; Is node a conditional function factor (CFF)?
(defun function-nodep (node)
  (and (factor-nodep node) (eq (node-subtype node) 'function))
  )

; Is node a perceptual function factor?
(defun perception-functionp (node)
  (and (factor-nodep node) (eq (node-subtype node) 'perception))
  )

; Recur on node-dependencies
(defun node-dependencies-recur (node dependencies)
  (let ((index (if (variable-nodep node) 1 0)) ; Index of incoming messages
        )
    (setq dependencies (adjoin node dependencies))
    (unless (wm-fnp node) ; Stop recursion at WMFN nodes
      (dolist (link (node-links node))
        (when (and
               (not (member (link-node link index) dependencies)) ; Haven't already processed linked node
               (link-content link index) ; There is a message from the linked node
               )
          (setq dependencies (node-dependencies-recur (link-node link index) dependencies))
          )
        )
      )
    dependencies)
  )

; Compute a list of nodes upon which the current node depends
; Follow links backwards that point to this node (uni or bidirectional)
(defun node-dependencies (node)
  (let ((index (if (variable-nodep node) 1 0)) ; Index of incoming messages
        dependencies
        )
  (dolist (link (node-links node))
    (when (link-content link index) ; There is a message from the linked node
      (setq dependencies (node-dependencies-recur (link-node link index) dependencies))
      )
    )
  dependencies)
  )

; List of assumption factor nodes upon which node depends
(defun function-dependencies (node)
  (let (fds)
    (dolist (dn (node-dependencies node))
      (when (node-assumption dn)
        (setq fds (cons dn fds))
        )
      )
    fds)
  )

; List of WM factor nodes upon which node depends
(defun wm-dependencies (node)
  (let (fds)
    (dolist (dn (node-dependencies node))
      (when (wm-fnp dn)
        (setq fds (cons dn fds))
        )
      )
    fds)
  )

; List of ever, selection, state WM factor nodes upon which node depends
(defun ess-wm-dependencies (node)
  (let (fds)
    (dolist (dn (node-dependencies node))
      (when (ess-wm-fnp dn)
        (setq fds (cons dn fds))
        )
      )
    fds)
  )

; Add node (with number of parent) to descendants list if not there
; If there already, augment its list of parents
(defun add-node-descendant (node old-descendant parent-number desc)
  (if old-descendant
      (if (member parent-number (descendant-parents old-descendant)) ; This parent is already included
          desc
        (progn
          (setf (descendant-parents old-descendant)
                (cons parent-number (descendant-parents old-descendant))
                )
          desc)
        )
    (cons (make-descendant :node node :parents (list parent-number)) desc)
    )
  )

; Copy a list of descendants
(defun copy-descendants (dl)
  (mapcar #'(lambda (d) (make-descendant :node (descendant-node d) :parents (descendant-parents d))) dl)
  )

; Union two lists of descendants
(defun union-descendants (l1 l2)
  (let ((ul (copy-descendants l1))
        ud
        d2node)
    (dolist (d2 l2)
      (setq d2node (descendant-node d2))
      (setq ud (find (node-name d2node) ul :key #'(lambda (x) (node-name (descendant-node x)))))
      (if ud ; There is already a node in the list of the same name
          (setf (descendant-parents ud)
                (union (descendant-parents ud) (descendant-parents d2)))
        (setq ul (cons d2 ul)) ; Add new node to list of nodes
        )
      )
    ul)
  )

; Recur on descendants
; desc is current list of descendants
(defun descendants-recur (grandparent-number node desc)
  (let ((index (if (variable-nodep node) 1 0)) ; Index of outgoing messages
        (parent-number (node-number node))
        child-node
        old-descendant
        )
    (dolist (link (node-links node))
      (setq child-node (link-node link index))
      (when (and (link-content link (- 1 index)) ; There is a message to the linked node
                 (not (= grandparent-number (node-number child-node))) ; The child is not the grandparent
                 )
        (setq old-descendant
              (find (node-name child-node) desc
                    :key #'(lambda (nl) (node-name (descendant-node nl))))) ; Previous entry for node if there is one
        (setq desc (add-node-descendant child-node old-descendant parent-number desc))
        (when (and (not (wm-fnp child-node)) ; Don't recur at WMFN nodes
                   (not old-descendant) ; Don't recur if already processed node
                   )
          (setq desc (descendants-recur parent-number child-node desc))
          )
        )
      )
    desc)
  )

; Recur on descendants
; desc is current list of descendants
; aht is the hash table where info on nodes with known descendants is stored
; common-descendants is a list that keeps the node-numbers of the nodes that have the same descendants with the original assumption node 
; This test function combines recursion with dynamic programming to speed up the process
; NO PARENTS CASE (Full parents case is in sigma1_vu_overlay10.lisp)
; (grid2) if you don't sort the parents you don't get the same number of messages for (d 2) even if the parents are the same.

(defun descendants-recur-no-parents (grandparent-number node desc aht common-descendants)
  (let ((index (if (variable-nodep node) 1 0)) ; Index of outgoing messages
        (parent-number (node-number node))
        child-node
        old-descendant 
        known-descendants
        )
    (dolist (link (node-links node))  
      (setq child-node (link-node link index))
      (when (link-content link (- 1 index)) ; There is a message to the linked node
        ; If the child is a grandparent, set common-descendants. If the link is bidirectional (being grandparent one direction, being child is another direction and hence the link is bidirectional) the parent has the same descendants as the grandparent. 
        ;This propagates back to the original node of consideration in init-node-descendants-no-parents
        (if  (= grandparent-number (node-number child-node))          
            (setf (gethash grandparent-number common-descendants) (cons (node-number node) (gethash grandparent-number common-descendants))) 
          (progn; The child is not the grandparent so find descendants
            (setq old-descendant
                  (find (node-name child-node) desc
                        :key #'(lambda (nl) (node-name nl)))) ; Previous entry for node if there is one
            (if (not (wm-fnp child-node)) (setf known-descendants (gethash (node-number child-node) aht)) (setf known-descendants nil)) ;don't check the descendants of the child-node, if the child-node is a WMFN node
            (if known-descendants 
                (setf desc (union desc known-descendants :key #'(lambda (x) (node-number x)))) ;union current list of descendants with already known descendants
              (progn  
                (setf desc (if old-descendant desc (cons child-node desc)))
                (if (and (not (wm-fnp child-node)) ; Don't recur at WMFN nodes
                         (not old-descendant) ; Don't recur if already processed node
                         )
                    (setf desc (multiple-value-setq (desc aht common-descendants) (descendants-recur-no-parents parent-number child-node desc aht common-descendants)))
                  )
                )
              )
            )   
          )
        )
      )
    (values desc aht common-descendants)
    )  
  )

; Compute a list of nodes dependent upon (descended from) the current node 
; Follow links forwards from point this node (uni or bidirectional)
(defun descendants (node)
  (descendants-recur -1 node (list (make-descendant :node node :parents nil)))
  )

; Determine node descendants for assumption FNs
(defun init-node-descendants ()
  (let (temp-desc
        )
    (dolist (node (graph-nodes cg))
      (when (node-assumption node)
        (setf temp-desc (descendants node))
        (setf (node-descendants node) temp-desc)
        ;(setf (node-descendants node) (sort temp-desc #'< :key #'(lambda (x) (node-number (descendant-node x))))); sorting mostly for debugging purposes 
        (when (eq debug-init-descendants 'all)
          (format trace-stream "~&Node ~S: ~S descendants" (node-number node) (length (node-descendants node)))
          )
        )
      )
    (dolist (d debug-descendants)
      (format trace-stream "~&~S: " (node-name (node-from-number d)))
      (print-node-progenitors d)
      )
    )
  )

; Init-node-descendants-no-parents uses this function as part of the dynamic programming approach
; This function stores descendant lists in a hash table for future use.
(defun common-descendants-recur (parent-number aht common-descendants descen) 
  (let (tempo
        ) 
    (setf tempo (gethash parent-number common-descendants))
    (if (not tempo) ; if a leaf node for the current wm-cff node, store the descendants (with the correct parents) in the hash table. 
        (setf (gethash parent-number  aht) descen) )
    (dolist (te tempo)
      (if (not (= parent-number te)) 
          (common-descendants-recur te aht common-descendants descen) )
      )
    aht)
  )

; Version of init-node-descendants wihtout parents
(defun init-node-descendants-no-parents ()
  (let ((common-descendants (make-hash-table))
        (aht (make-hash-table))
        temp-desc
        temp-node
        )
    (dolist (node (graph-nodes cg))
      (when (node-assumption node)
        (setf temp-node (gethash (node-number node) aht))
        (if temp-node  (setf (node-descendants node) temp-node) 
          (progn
            (multiple-value-setq (temp-desc aht common-descendants) (descendants-recur-no-parents -1 node (list node) aht common-descendants))
            ;(setf (node-descendants node) (sort temp-desc #'< :key #'(lambda (x) (node-number x)))); sorting is mostly for debugging purposes
            (setf (node-descendants node) temp-desc)
            (setf (gethash (node-number node) aht) (node-descendants node))
            (setf aht (common-descendants-recur (node-number node) aht common-descendants (node-descendants node)))
            (setf common-descendants (make-hash-table))
            ))
        (when (eq debug-init-descendants 'all)
          (format trace-stream "~&Node ~S: ~S descendants" (node-number node) (length (node-descendants node)))
          )
        )
      )
    (dolist (d debug-descendants)
      (format trace-stream "~&~S: " (node-name (node-from-number d)))
      (print-node-progenitors d)
      )
    )
  )

; List of node names from list of nodes
; Used in debugging
(defun node-name-list (nodes)
  (mapcar #'node-name nodes)
  )

(defun link-recur (in-link parent-links lht)
  (let ( 
        node index des-link temp old-desc-link
             (current-links (list in-link))          
        )
    (setf node (link-node (descendant-link-link in-link) (descendant-link-direction in-link))) 
    (setf index (if (variable-nodep node) 1 0))  
    (dolist (link (node-links node))
      (setf temp '())
      (when (not (eq (descendant-link-link in-link) link)) ; don't recur on the incoming link     
        (when (link-content link (- 1 index)) ;if the link is active
          ;check if this link has an entry in link hash table, if not go with regular recur
          (unless (gethash (intern (concatenate 'string (princ-to-string (node-number node))"-" 
                                                   (princ-to-string (node-number (link-node link index))))) lht)
            (setq old-desc-link
                  (find-if  #'(lambda (nl) (and (eq (descendant-link-from nl) (node-number node)) 
                                                (eq (descendant-link-to nl) (node-number (link-node link index)))
                                                )
                                ) 
                            parent-links
                            ))
            (unless old-desc-link   ; don't recur if the link is visited before (prevent cycles)           
              (setf des-link (make-descendant-link :link link :direction index :from (node-number node) :to (node-number (link-node link index))))              
              (setf parent-links (cons des-link parent-links))              
              (unless (wm-fnp (link-node link index))
                (multiple-value-setq (temp lht) (link-recur des-link parent-links lht))
                ;(setf current-links (append temp current-links))
                (setf current-links (union current-links temp :test #'(lambda (x y) 
                                                                       (and (eq (descendant-link-from x) (descendant-link-from y)) 
                                                                            (eq (descendant-link-to x) (descendant-link-to y))
                                                                            (eq (descendant-link-direction x) (descendant-link-direction y))
                                                                            ))
                                       ))
                ;(setf parent-links (append temp parent-links)) 
                (setf parent-links (union parent-links temp :test #'(lambda (x y) 
                                                                      (and (eq (descendant-link-from x) (descendant-link-from y)) 
                                                                           (eq (descendant-link-to x) (descendant-link-to y))
                                                                           (eq (descendant-link-direction x) (descendant-link-direction y))                                                                                                                  ))
                                          ))
                )
              ;; add the link to the wm-fn node to the descendant-links list
              (when (wm-fnp (link-node link index)) 
                (setf current-links (append current-links (list des-link)))
                ) 
              )
            )
          ;check if this link has an entry in link hash table, if so append the links found there to the current list of links
          (when (gethash (intern (concatenate 'string (princ-to-string (node-number node))"-"
                                               (princ-to-string (node-number (link-node link index))))) lht)
            (setf temp (gethash (intern (concatenate 'string (princ-to-string (node-number node))"-"
                                                     (princ-to-string (node-number (link-node link index))))) lht))           
            ;(setf current-links (append temp current-links))
            (setf current-links (union current-links temp :test #'(lambda (x y) 
                                                                       (and (eq (descendant-link-from x) (descendant-link-from y)) 
                                                                            (eq (descendant-link-to x) (descendant-link-to y))
                                                                            (eq (descendant-link-direction x) (descendant-link-direction y))
                                                                            ))
                                       ))
            )
          )
        ;create an entry in the links hash table for the recently calculated link
        (setf (gethash (intern (concatenate 'string (princ-to-string (node-number node))"-"
                                             (princ-to-string (node-number (link-node link index))))) lht) temp)    
        )
      )
    (values current-links lht)
    )
  )

; Assign link descendants to assumption fn nodes
(defun init-link-descendants ()
  (let (
        lht index parent-links des-link current-links temp
        ) 
    ;keep a links hashtable for storage in dynamic programming. Key is the string "10-5", where 10 is the 'from' node-number and 5 is the 'to' node-number
    (setf lht (make-hash-table))    
    (dolist (node (graph-nodes cg))  
      (when (node-assumption node)        
        (setf index (if (variable-nodep node) 1 0))
        ;parent-links is used to detect cycles (not to recur from previously process link)
        (setf parent-links '()) 
        ;temp stores the combined descendant-link list starting from each outgoing node
        (setf current-links '())
        (dolist (link (node-links node))  
          (when (link-content link (- 1 index))            
            (setf des-link (make-descendant-link :link link :direction index :from (node-number node) :to (node-number (link-node link index)))) 
            (setf parent-links (append (list des-link) parent-links))
            ;get the descendant-link list for this starting link and the updated link hash-table.
            (multiple-value-setq (temp lht) (link-recur des-link parent-links lht))
            ;(setf current-links (append current-links temp))    
            (setf current-links (union current-links temp :test #'(lambda (x y) 
                                                                       (and (eq (descendant-link-from x) (descendant-link-from y)) 
                                                                            (eq (descendant-link-to x) (descendant-link-to y))
                                                                            (eq (descendant-link-direction x) (descendant-link-direction y))
                                                                            ))
                                       ))

            )                                      
          )   
        (setf (node-descendant-links node) current-links)
        )
      )
    )
  )


;-----------------------------------------------------------
; Densely connected subgraphs 

; A binary tree is created as a subgraph and placed between the densely connected variable node and the factor nodes connected to this variable node
; Each layer of the binary tree is either beta-factor nodes or variable nodes
; Each beta factor node connects exactly two variable nodes in the lower layer to a variable node in the upper layer
; All the variable nodes and beta-factor nodes used in the subgraph are created in this function.
(defun create-subgraph-for-densely-connected-variable-nodes (node)
  (let (temp
        links-to-factor-nodes
        unmodified-links
        subgraph-variable-nodes
        subgraph-variable-nodes-temp
        beta-node
        beta-function
        l
        temp-vn
        )
    (dolist (temp (node-links node))
      ; affine delta factor nodes were creating problems but it is solved by updating the evidence list of the adf nodes
      ; there might be other types of factor nodes problematic, haven't tested for everything
      ; constant and filter nodes are working fine
      (if (and (link-in temp) (link-out temp))
        (setf links-to-factor-nodes (cons temp links-to-factor-nodes)) (setf unmodified-links (cons temp unmodified-links))
        )
      )
    (setq l (floor (/ (length links-to-factor-nodes) 2)))
    (when (< l (/ threshold-for-dense-connection 2))
      (return-from create-subgraph-for-densely-connected-variable-nodes) ; return if there are not enough factor nodes to make a difference
      )
    ; Create a variable node for every two factor nodes connected to the original variable node
    (dotimes (i l)
      (setq temp (nth (* i 2) links-to-factor-nodes))
      (setf temp-vn (init-variable-node 'Subgraph-VN (node-subtype node) 'subgraph (node-action node) (node-variables node) nil 'condact)) ; assigning the subsubtype as subgraph-need to check on this
      (setf (node-normalize temp-vn) (node-normalize node)) ;Subgraph variable nodes are extension of wmwn so they need to be normalized if wmwn is normalized
      (setq subgraph-variable-nodes 
            (cons temp-vn
                  subgraph-variable-nodes))
      (setf (aref (link-nodes temp) var-index) ; Update the variable node end of the link; replace the original variable node (wm-vn) with the recently created variable node
            (car subgraph-variable-nodes))
      (when (listp (node-evidence (aref (link-nodes temp) 1)))
        (when (member node (node-evidence (aref (link-nodes temp) 1)))
          (setf (node-evidence (aref (link-nodes temp) 1)) ; add the recently created variable to the list of evidence nodes for the factor node
                (cons (car subgraph-variable-nodes)
                      (node-evidence (aref (link-nodes temp) 1))))
          )
        )
      (setf (node-links (car subgraph-variable-nodes))
            (cons temp (node-links (car subgraph-variable-nodes))))
      (setq temp (nth (+ (* i 2) 1) links-to-factor-nodes))
      (setf (aref (link-nodes temp) var-index) ; Update the variable node end of the link; replace the original variable node (wm-vn) with the recently created variable node
            (car subgraph-variable-nodes))
      (when (listp (node-evidence (aref (link-nodes temp) 1)))
        (when (member node (node-evidence (aref (link-nodes temp) 1)))
          (setf (node-evidence (aref (link-nodes temp) 1)) ; add the recently created variable to the list of evidence nodes for the factor node
                (cons (car subgraph-variable-nodes)
                      (node-evidence (aref (link-nodes temp) 1))))
          )
        )
      (setf (node-links (car subgraph-variable-nodes))
            (cons temp (node-links (car subgraph-variable-nodes))))
      )
    (setq temp (nth  (* l 2) links-to-factor-nodes)) ; the last factor node if there are odd number of factor-nodes
    (when temp ; if there exists an unprocessed factor node, create a variable node and connect the factor node to the variable node created
      (setf temp-vn (init-variable-node 'Subgraph-VN (node-subtype node) 'subgraph (node-action node) (node-variables node) nil 'condact)); assigning the subsubtype as subgraph-need to check on this
      (setf (node-normalize temp-vn) (node-normalize node)) ;Subgraph variable nodes are extension of wmwn so they need to be normalized if wmwn is normalized
      (setf subgraph-variable-nodes 
            (cons temp-vn
                  subgraph-variable-nodes))
      (setf (aref (link-nodes temp) var-index) ;Update the variable node end of the link; replace the original variable node (wm-vn) with the recently created variable node
            (car subgraph-variable-nodes))
      (when (member node (node-evidence (aref (link-nodes temp) 1)))
        (setf (node-evidence (aref (link-nodes temp) 1)) ; add the recently created variable to the list of evidence nodes for the factor node
              (cons (car subgraph-variable-nodes)
                    (node-evidence (aref (link-nodes temp) 1)))))
      (setf (node-links (car subgraph-variable-nodes)) (cons temp (node-links (car subgraph-variable-nodes))))
      )                                                
    (setf (node-links node) unmodified-links)
    (loop
     (setq subgraph-variable-nodes-temp nil)
     (when (eql (length subgraph-variable-nodes) 2)
       (return) ; connect the last two to the original variable node via the final beta factor node after this loop
       )
     (loop for i from 0 to (- (length subgraph-variable-nodes) 1) by 2 do ; create new variable node and connect it to two previous variable nodes via a beta factor node
           (if (nth (+ i 1) subgraph-variable-nodes) 
               (progn
                 (setf temp-vn (init-variable-node 'Subgraph-VN (node-subtype node) 'subgraph (node-action node) (node-variables node) nil 'condact)) ; assigning the subsubtype as subgraph-need to check on this
                 (setf (node-normalize temp-vn) (node-normalize node)) ;Subgraph variable nodes are extension of wmwn so they need to be normalized if wmwn is normalized
                 (setf subgraph-variable-nodes-temp 
                       (cons temp-vn
                             subgraph-variable-nodes-temp))        
                 (setq beta-node
                       (init-factor-node 'Subgraph-BF 'beta (node-variables node)
                                         (list (car subgraph-variable-nodes-temp) (nth i subgraph-variable-nodes) (nth (+ i 1) subgraph-variable-nodes))
                                         nil t t 
                                         (make-beta-memories :alpha (nth i subgraph-variable-nodes) :old-beta (nth (+ i 1) subgraph-variable-nodes) :new-beta (car subgraph-variable-nodes-temp)) 
                                         nil nil))
                 (setq beta-function (full-plm (node-variables node)))
                 (setf (node-function beta-node) beta-function)
                 )
             (setq subgraph-variable-nodes-temp ; variable node without a pair should be processed in the next sweep
                   (cons (nth i subgraph-variable-nodes)subgraph-variable-nodes-temp))
             )
           )
     (setq subgraph-variable-nodes subgraph-variable-nodes-temp) ; re-initialization of the variable-node list
     )
    ; Connect final two variable nodes to the original variable node via the beta factor node-the root of the subgraph created
    (setq beta-node
          (init-factor-node 'Subgraph-Root-BF 'beta (node-variables node)
                            (list  (nth 0 subgraph-variable-nodes) (nth 1 subgraph-variable-nodes) node)
                            (node-evidence node)  t t 
                            (make-beta-memories :alpha (nth 0 subgraph-variable-nodes) :old-beta (nth 1 subgraph-variable-nodes) :new-beta node) 
                            nil nil))
    (setq beta-function (full-plm (node-variables node)))
    (setf (node-function beta-node) beta-function)
    )
  )

; Expand densely connected variable nodes to binary trees of nodes
(defun expand-densely-connected-variable-nodes nil
  (dolist (node (graph-nodes cg))
    (when (and (variable-nodep node) (> (length (node-links node)) threshold-for-dense-connection))
      (create-subgraph-for-densely-connected-variable-nodes node))   
    )
  )

;---------------------------------------------------
; Create a predicate function
(defun compile-predicate-function (pred)
  (let* ((wmfn (predicate-wm pred)) ; WMFN node
         (wmvs-v (predicate-wm-variables pred)) ; Vector of variables in predicate
         (wmvs-l (coerce wmvs-v 'list)) ; List of variables in predicate
         (wmv-names (mapcar #'svariable-name wmvs-l)) ; Variable names in WMFN node
         (fun (predicate-function pred)) ; Specified CPT for function
         row-major ; Whether should use special row-major initialization (an all discrete function with a full set of values)
         (pname (predicate-name pred))
         fun-node ; Predicate function factor node
         other-p ; Predicate to which are tied
         other-fn ; Factor node to which are tied
         p-norm ; Normal/unique variables
         p-fun ; Function stored in predicate function factor node
         (normalize-function (not (predicate-no-normalize pred))) ; Whether to normalize function
         (wmvn (predicate-outgoing-vn pred)) ; Shared WMVN node
         (share (or (not (predicate-persistent pred)) ; Conditions for sharing VN from conditions/condacts with actions
                    (and (not (predicate-select pred))
                         (or (predicate-universal pred)
                             (predicate-cumulative pred)
                             )
                         )
                    ))
         (connect-directly-to-shared-vn (or (not (predicate-persistent pred)) ; Connect directly from FN to VN
                                            (and (predicate-persistent pred)
                                                 (predicate-unique pred)
                                                 (or (predicate-select pred)
                                                     (predicate-replace pred)
                                                     )
                                                 (not (predicate-perception pred))
                                                 )
                                            ))
         )
    ; Check for special case of a row-major function
    (when (and (consp fun) (eq (car fun) 'row-major))
      (setq fun (cdr fun))
      (setq row-major t)
      )
    (setf (predicate-function-variable-names pred) wmv-names)
    ; Shared WMVN that connects the function to the rest of the graph
    ; If there is not an outgoing WM VN, use incoming if shared with it and it exists, or create it
    (unless wmvn
      (if (and share (predicate-incoming-vn pred))
          (progn
            (setq wmvn (predicate-incoming-vn pred))
            (setf (node-pattern-type wmvn) 'condact) ; Mark WMVN as bidirectional
            )
        (setq wmvn (create-wm-variable-node pred wmfn ; Create a new variable node
                                            (concat-symbols `(,pname shared wm-vn) t)
                                            nil wmvs-v nil t nil connect-directly-to-shared-vn nil))
        )
      (setf (predicate-outgoing-vn pred) wmvn)
      )
    ; Create predicate function factor node
    (setq fun-node
          (init-factor-node (concat-symbols (cons pname (cons 'PFF wmv-names)) t)
                            'function
                            wmvs-v
                            (list wmvn)
                            t feedback-to-functions t nil nil nil))
    (setf (node-assumption fun-node) t)
    (setf (node-function-name fun-node) pname)
    (cond ((numberp fun)
           (setq p-fun (init-plm wmvs-v fun 0 (init-vector (length wmvs-v) t)))
           (setf (plm-piecewise-constant p-fun) t)
           )
          ((symbolp fun) ; Copy function from another predicate
           (setq normalize-function nil) ; Don't need to normalize function, as already normalized
           (setq other-p (predicate-from-name fun))
           (unless other-p
             (error "No predicate named ~S, as specified in the :FUNCTION for predicate ~S" fun pname)
             )
           (unless (equal wmv-names (predicate-function-variable-names other-p))
             (error "Function variable names ~S in predicate ~S not the same as ~S in predicate to which function is to be tied ~S"
                    wmv-names pname (predicate-function-variable-names other-p) (predicate-name other-p))
             )
           (unless (equal (predicate-unique pred) (predicate-unique other-p)) ; This may need to be changed as work out different forms of uniqueness for predicate functions
             (error "Function normal ~S in conditional ~S not the same as ~S in conditional to which function is to be tied ~S"
                    (predicate-unique pred) pname (predicate-unique other-p) (predicate-name other-p))
             )
           (setq other-fn (predicate-function-node other-p))
           (unless other-fn
             (error "No function node for predicate ~S as needed given specification in predicate ~S" (predicate-name other-p) pname)
             )
                ; Add shared function node, and all nodes it shares with to list of shared function nodes
           (setf (node-shared-functions fun-node) (cons other-fn (node-shared-functions other-fn)))
                ; Add current node to list of shared function nodes for node shared and everyone it shares with
           (setf (node-shared-functions other-fn) (cons fun-node (node-shared-functions other-fn)))
           (dolist (cousin (node-shared-functions other-fn))
             (setf (node-shared-functions cousin) (cons fun-node (node-shared-functions cousin)))
             )
           (setq p-fun (copy-conditional-function (node-function other-fn) wmvs-v))
           )
          (row-major
           (setq p-fun (rml-plm fun wmvs-v t))
           (setf (plm-piecewise-constant p-fun) t)
           )
          (t
           (setq p-fun (cpt-function-array-plm wmv-names fun wmvs-v
                                               (if (predicate-function-default pred) (predicate-function-default pred) function-default) t))
           (setf (plm-piecewise-constant p-fun) (notany #'(lambda (x) (consp (car x))) fun)) ; No linear regions in specification
           )
          )
    (setf (predicate-function-node pred) fun-node)
    (setf (node-predicate fun-node) pred)
    (setf (node-normal fun-node) nil)
    (setq p-norm (mapcar #'convert-to-wm-variable-name (predicate-unique pred)))
    (when p-norm ; This may need to be changed as work out different forms of uniqueness for predicate functions
      (setf (node-normal fun-node) (if (symbolp p-norm)
                                       (position p-norm wmv-names)
                                     (mapcar #'(lambda (px) (position px wmv-names)) p-norm)))
      (when normalize-function ; Avoid renormalizing when it is a function copied from another conditional
        (setq p-fun (normalize-plm p-fun (node-normal fun-node) t))
        )
      )
    (setf (node-function fun-node) p-fun)
    (setf (node-restriction fun-node) (transform-plm #'boolean-function p-fun))
    (setf (node-changes fun-node) 1) ; Initialize number of changes made to fucntion via learning to 0
    ; Set the node's learning rate
    (if (predicate-learning-rate pred) ; Use value specified in predicate if specified
        (progn
          (unless (numberp (predicate-learning-rate pred))
            (error "Learning rate specified for predicate ~S is not a number" pname)
            )
          (if (not (or (node-normal fun-node) learn-no-normal (e= (predicate-learning-rate pred) 0)))
              (error "Learning rate specified for a predicate with no normal variable when learn-no-normal not T: ~S" pname)
            (setf (node-learning-rate fun-node) (predicate-learning-rate pred)))
          )
      (when adaptive-learning-rate ; Otherwise use the adaptive rate if active
        (setf (node-learning-rate fun-node) (/ 0.3 (function-span fun-node)))
        ))
    ; Set the node's smoothing parameter
    (when (predicate-smoothing-parameter pred) ; Use value specified in predicate if specified
      (unless (numberp (predicate-smoothing-parameter pred))
        (error "Learning rate specified for predicate ~S is not a number" pname)
        )
      (setf (node-smoothing-parameter fun-node) (predicate-smoothing-parameter pred))
      )
    fun-node)
  )

; -----------------------------------------------------------
; Compile conditional

; Make a new unique symbol based on a string
(defun make-name-string (string)
  (intern (symbol-name (gensym string))))

; Make a new unique symbol based on a symbol
(defun make-name-symbol (symbol)
  (intern (symbol-name (gensym (symbol-name symbol)))))

; Make a new symbol based on a symbol and a number
(defun make-name-number-symbol (symbol number)
  (intern (format nil "[~S_~S]" number symbol))
  )

; An element is a list whose car is the argument name
; and the cadr is either * (for all constants),
; a list with a beginning and ending of a numeric range
; or a list of a variable (which is itself a list with the variable's name)
; The caddr, should it exist, is is a list of the value for the constant
; and a value for other elements of the domain (defaults to (1 0))
(defun element-argument-name (e) (car e))
(defun element-content (e) (cadr e))
(defun element-rest (e) (cddr e)) ; Used to determining if there is extra stuff (syntax error)
(defun element-value (e) (if (caddr e) (caddr e) '(1 0)))

; Determine if something is a constant region (a list with two numbers)
(defun constant-element-regionp (x)
  (and (consp x) (numberp (car x)) (equal (length x) 2) (numberp (cadr x)))
  )

; Determine if an element's contents is a constant or variable
; Currently differentiated by variables being in sublists
(defun constant-element (ec) (or (symbolp ec)
                                 (numberp ec)
                                 (constant-element-regionp ec)
                                 (filter-listp ec)
                                 )
  )
(defun variable-element (ec) (and (listp ec) (symbolp (car ec)) (not (equal (car ec) filter-symbol))))

; When the element content is an asterisk (*) it is all constants
(defun star-element (ec) (eq ec '*))

; Get the variable index corresponding to an element
(defun element-wm-index (e pred)
  (position (element-argument-name e) (predicate-arguments pred) :key #'argument-name))

; Get index for a constant from a type
(defun constant-index (constant type)
  (position constant (stype-constants type)))

; Compute the element span for a constant test or a wme
; Half open interval from (and including) min to (and not including) max
(defun compute-span (content type &optional shift-discrete-lists)
  (let ((min (stype-min type))
        (max (stype-max type))
        span-min ; Minimum of numeric span
        span-max ; Maximum of numeric span
        )
    (cond ((and (listp content) (numberp (car content))) ; Case where given begin and end as numbers
           (setq span-min (car content))
           (setq span-max (cadr content))
           (when (or (not (numberp span-min)) (not (numberp span-max)))
             (error "Specified span [~S,~S> not numeric for type ~S in COMPUTE-SPAN" span-min span-max (stype-name type))
             )
           (when (and (stype-discrete type) (stype-numeric type) center-discrete-numeric-on-integer shift-discrete-lists)
             (setq span-min (- span-min 1/2))
             (setq span-max (- span-max 1/2))
             )
           (unless (and (>= span-min min) (<= span-min max) (>= span-max min) (<= span-max max))
             (error "Span [~S,~S> is not within domain of type ~S: [~S,~S>!" (car content) (cadr content) (stype-name type) min max)
             )
           (list span-min span-max))
          ((listp content) ; Case where have a list of symbols
           (let ((ci-min (constant-index (car content) type))
                 (ci-max (constant-index (car (last content)) type)))
             (unless ci-min
               (error "Constant ~S not defined as part of type ~S: ~S" (car content) (stype-name type) (stype-constants type))
               )
             (unless ci-max
               (error "Constant ~S not defined as part of type ~S: ~S" (car (last content)) (stype-name type) (stype-constants type))
               )
             (list ci-min (+ ci-max 1))
             )
           )
          ((star-element content) ; Cover whole type for a star
           (list (stype-min type) (stype-max type)))
          ((stype-numeric type) ; If number but content isn't a list
           (unless (numberp content)
             (error "Specified constant ~S not numeric for type ~S in COMPUTE-SPAN" content (stype-name type))
             )
           (cond ((and (stype-discrete type) center-discrete-numeric-on-integer shift-discrete-lists) ; Discrete type that is to be centered around integers
                  (setq span-min (- content 1/2))
                  (setq span-max (+ content 1/2))
                  )
                 (t
                  (setq span-min content)
                  (setq span-max (+ content 1))
                  )
                 )
           (unless (and (>= span-min min) (<= span-max max) (>= span-max min) (<= span-max max))
             (error "Specified value of ~S [~S,~S> is not within domain of type ~S: [~S,~S>!" content span-min span-max (stype-name type) min max)
             )
           (list span-min span-max))
          (t ; For constants, find index and include from there to +1     
           (let ((ci (constant-index content type)))
             (unless ci
               (error "Constant ~S not defined as part of type ~S: ~S" content (stype-name type) (stype-constants type))
               )
             (list ci (+ ci 1))
             )
           )
          ) 
    )
  )

; Convert one entry in a cpt table to a region
; Added the optional input ordered-variable. If the variables are aligned with variable names, no need to find the variable indices
(defun cpt-region (cpt-var-names entry variables &optional shift-discrete-lists ordered-variables)
  (let* ((fun (car entry)) ; Entry function (either a constant or a list of a constant and dimension weights)
         (dns (cdr entry)) ; List of dimension spans
         (r (make-spanning-region variables nil 0 0)) ; New region
         (dvector (region-dimensions r)) ; Vector of region's dimensions (including min, max, before, after, and weight)
         dim ; One dimensional structure in iteration
         dvar ; Variable in dimension in iteration
         dspan) ; List of min and max for dimension in iteration
    (cond ((numberp fun) ; Function is a single constant weight
           (setf (region-constant r) (car entry))
           (setq fun nil) ; Signal for rest of processing that there are no weights
           )
          ((listp fun) ; Function is a list of constant and weights
           (setf (region-constant r) (car fun))
           (setq fun (cdr fun)) ; Weights on dimensions
           )
          (t (error "Function specification ~S is neither a number or a list of numbers." fun))
          )
    (do ((dinfo-l dns (cdr dinfo-l)) ; Cdr through list of dimensions
         (cvn-l cpt-var-names (cdr cvn-l)) ; Cdr through list of variable names
         (f-l fun (if fun (cdr f-l) nil)) ; Cdr though list of dimension weights if there are any
         (i 0 (1+ i))
         )
        ((null dinfo-l)) ; Done with iteration when no more dimensions
      (if ordered-variables
          (setq dvar (aref variables i))
        (setq dvar (variable-from-name (car cvn-l) variables)) ; Variable for dimension
        )
      (unless dvar (error "Variable not found for name ~S in list ~S in cpt-region." (car cvn-l) variables))
      (if ordered-variables
          (setf dim (aref dvector i))
          (setf dim (aref dvector (variable-number dvar variables))) ; Dimensional structure for variable
          )
      (setq dspan (compute-span (car dinfo-l) (svariable-type dvar) shift-discrete-lists)) ; Span (min and max) of dimension
      (setf (dimension-min-slice dim) (make-slice :location (car dspan))) ; Set dimension min based on CPT
      (setf (dimension-max-slice dim) (make-slice :location (cadr dspan))) ; Set dimension max based on CPT
      (if f-l ; If there are dimension weights
          (setf (dimension-weight dim) (car f-l)) ; Set weight of current dimension to first element
        (setf (dimension-weight dim) 0)) ; Else set  the weight to 0
      )
    r) ; Return r
  )

; Convert a cpt table into a PLM
; A table is a list of entries, each of which is a list for a function (a constant or a list of a constant and dimension weights)
; followed by dimension spans, each of which is a constant, a region or *
(defun cpt-plm (cpt-var-names table variables default &optional shift-discrete-lists)
  (let ((plm (init-plm variables default 0 (init-vector (length variables) t)))
        )
    (dolist (entry table)
      (setq plm (update-region (cpt-region cpt-var-names entry variables shift-discrete-lists) plm t))
      )
    (remove-unneeded-slices plm))
  )

; Create a random CPT for a list of discrete types
; Incomplete.  Incorporate in stuff froom unsupervised learning initialization, and then use this there
(defun random-cpt (var-names type-names)
  (let ((types (mapcar #'type-from-name type-names)))
    (dolist (type types)
      (unless (stype-discrete type)
        (error "Cannot create a random CPT with a continuous dimension")
        )
      )
    (do ((vns var-names (cdr vns))
         (tns type-names (cdr tns))
         )
        ((or (null vns) (null tns)) t)
      )
   )
  )

; Is cpt constant (no dimension weights?)
(defun cpt-constantp (table)
  (not (find-if #'(lambda (entry) (consp (car entry))) table))
  )

; Are variables all discrete
(defun variables-discretep (variables)
  (not (find-if #'(lambda (v) (not (stype-discrete (svariable-type v)))) variables))
  )

; Create a list of dimensions for a vector of variables
(defun variable-dimensions (variables)
  (let (dims)
    (dotimes (i (length variables))
      (setq dims (cons (stype-span (svariable-type (aref variables i))) dims))
      )
    (reverse dims))
  )

; Create a discrete array
(defun init-farray (variables default)
  (make-array (variable-dimensions variables) :initial-element default)
  )

; Increment an index to an array of regions, reinitializing later columns according to minsi
; Destructively update setf for update-array-region
(defun inc-index (minsi index maxsi)
  (cond ((null index) nil) ; Beyond end of list
        ((inc-index (cdr minsi) (cdr index) (cdr maxsi)) t) ; Could increment a later index
        ((= (car index) (car maxsi)) (rplaca index (car minsi)) nil)
        ((< (car index) (car maxsi)) (rplaca index (1+ (car index)))) ; Can increment current index
        (t nil)
        )
  )

; Lists of region mins and maxs
(defun region-mins-l (region)
  (let (m
        (rds (region-dimensions region)))
    (dotimes (i (length rds))
      (setq m (cons (region-min region i) m))
      )
    (reverse m))
  )
(defun region-maxs-l (region)
  (let (m
        (rds (region-dimensions region)))
    (dotimes (i (length rds))
      (setq m (cons (region-max region i) m))
      )
    (reverse m))
  )

; Delete duplicates (based on location) from a list of slices
(defun delete-duplicates-slice-list (slices)
  (delete-duplicates slices :key #'slice-location)
  )

; Delete duplicates (based on location) from an array of slice lists
(defun delete-duplicates-slice-list-array (sla)
  (dotimes (i (length sla))
    (setf (aref sla i) (delete-duplicates-slice-list (aref sla i)))
    )
  sla)

; Sort a list of slices
(defun sort-slice-list (slices)
  (sort slices #'< :key #'slice-location)
  )

; Sort an array of slice lists
(defun sort-slice-list-array (sla)
  (dotimes (i (length sla))
    (sort-slice-list (aref sla i))
    )
  sla)

; Index a list of slices
(defun index-slice-list (slices)
  (let ((count 0))
    (dolist (sl slices)
      (setf (slice-index sl) count)
      (setq count (1+ count))
      )
    slices)
  )

; Index an array of slice lists
(defun index-slice-list-array (sla)
  (dotimes (i (length sla))
    (index-slice-list (aref sla i))
    )
  sla)

; List of minimum slice locations for a region
(defun region-min-slice-indexes-l (r)
  (let (ms
        (rds (region-dimensions r))
        )
    (dotimes (i (length rds))
      (setq ms (cons (slice-index (dimension-min-slice (aref rds i))) ms))
      )
    (reverse ms))
  )

; List of maximum slice locations for a region
(defun region-max-slice-indexes-l (r)
  (let (ms
        (rds (region-dimensions r))
        )
    (dotimes (i (length rds))
      (setq ms (cons (slice-index (dimension-max-slice (aref rds i))) ms)) 
      )
    (reverse ms))
  )

; Update a region array based on a region from a cpt entry
(defun update-function-array-region (region farray)
  (let* ((minsi (region-min-slice-indexes-l region))
         (minsi-iterate (copy-list minsi))
         (maxsi (mapcar #'1- (region-max-slice-indexes-l region)))
         )
    (do ((i minsi-iterate (progn (inc-index minsi minsi-iterate maxsi) minsi-iterate))) ; Inc-index destructively modifies its argument and doesn't return it
        ((equal i maxsi) (setf (apply #'aref farray i) (if (region-function-constantp region) (region-constant region) (extract-function region))))
      (setf (apply #'aref farray i) (if (region-function-constantp region) (region-constant region) (extract-function region)))
      )
    )
  )

; Create an array of functions based on entries
(defun cpt-function-array (rlist default slices)
  (let ((farray (make-array (mapcar #'1- (mapcar #'length (coerce slices 'list))) :initial-element default))
        )
    (dolist (r rlist)
       (update-function-array-region r farray)
      )
    farray)
  )



; Convert a cpt to a PLM
(defun cpt-function-array-plm (cpt-var-names table variables default &optional shift-discrete-lists ordered-variables)
  (let* ((rank (length variables))
         farray ; Array of functions
         rarray ; Array of regions
         region ; A region to be added to the PLM
         rlist ; List of regions
         dimensions ; Dimensions of region
         dim ; Current dimension
         vt ; Current variable type
         sl ; slice
         plm
         (slices (init-vector rank)) ; Slices to be applied to the PLM
        )
    ; Determine slices for entries
    (dolist (entry table)
      (setq region (cpt-region cpt-var-names entry variables shift-discrete-lists ordered-variables))
      (setq rlist (cons region rlist))
      (setq dimensions (region-dimensions region))
      ; Determine slices for region
      (dotimes (d rank)
        (setq dim (aref dimensions d))
        (setq sl (find (dimension-min dim) (aref slices d) :key #'slice-location :test #'ae=))
        (if sl
            (setf (dimension-min-slice dim) sl)
          (setf (aref slices d) (cons (dimension-min-slice dim) (aref slices d))))
        (setq sl (find (dimension-max dim) (aref slices d) :key #'slice-location :test #'ae=))
        (if sl
            (setf (dimension-max-slice dim) sl)
          (setf (aref slices d) (cons (dimension-max-slice dim) (aref slices d))))
        )
      )
    ; Determine slices for ends of dimensions
    (dotimes (d rank)
      (setq vt (svariable-type (aref variables d)))
      (setq sl (find (stype-min vt) (aref slices d) :key #'slice-location :test #'ae=))
      (unless sl
        (setf (aref slices d) (cons (make-slice :location (stype-min vt)) (aref slices d)))
        )
      (setq sl (find (stype-max vt) (aref slices d) :key #'slice-location :test #'ae=))
      (unless sl
        (setf (aref slices d) (cons (make-slice :location (stype-max vt)) (aref slices d)))
        )
      )
    (sort-slice-list-array slices) ; Sort slices lists
    (index-slice-list-array slices) ; Add indices to slices
    (setq plm (init-plm-with-slices variables nil 0 (init-vector rank t) slices))
    (setq rarray (plm-array plm))
    (setq farray (cpt-function-array (reverse rlist) default slices)) ; Create function array from list of update regions (from cpts)
    ; Copy functions from function array into PLM
    (dotimes (i (array-total-size rarray))
      (let ((fun (row-major-aref farray i)))
        (if (numberp fun)
            (setf (region-constant (row-major-aref rarray i)) fun)
          (assign-function fun (row-major-aref rarray i)))
        )
      )
    plm)
  )

; Convert a row-major list of constant numbers to a PLM
; Only works for fully discrete PLMs where values are specified for all of the unit-sized regions
(defun rml-plm (rml variables &optional shift-discrete-lists)
  (let* ((rank (length variables))
         rarray ; Array of regions
         rarray-size ; Total size of array rarray
         plm ; New PLM
         dim-type ; Type of the dimension
         dim-size ; Size of a dimension
         dim-size+1 ; Number of slices in dimension
         dim-sizes ; List of sizes of dimensions
         dim-slice-vector ; A vector of slices for a dimension
         (dim-slice-vectors (init-vector rank)) ; A vector of dimension slice vectors
         (slices (init-vector rank)) ; Slices to be applied to the PLM
        )
    ; Determine sizes and slices of each of the dimensions
    (dotimes (i rank)
      (setq dim-type (svariable-type (aref variables i)))
      (unless (stype-discrete dim-type)
        (error "Attempt to compile a row-major function with a non-discrete variable type ~S(~S)" (svariable-name (aref variables i)) (stype-name dim-type))
        )
      (setq dim-size (stype-span dim-type))
      (setq dim-size+1 (1+ dim-size))
      (setq dim-sizes (cons dim-size dim-sizes))
      (setq dim-slice-vector (init-vector dim-size+1))
      (dotimes (j dim-size+1)
        (setf (aref dim-slice-vector j) (make-slice :location (if (and shift-discrete-lists center-discrete-numeric-on-integer) (- j 1/2) j) :index j))
        )
      (setf (aref dim-slice-vectors i) dim-slice-vector)
      (setf (aref slices i) (coerce dim-slice-vector 'list))
      )
    (setq plm (init-plm-with-slices variables nil 0 (init-vector rank t) slices))
    (setq rarray (plm-array plm))
    (setq rarray-size (array-total-size rarray))
    (unless (= rarray-size (length rml))
      (error "Number of constants provided (~S) not the same as the size of the array (~S) to be filled in a row-major fashion" (length rml) rarray-size)
      )
    ; Copy row-major list of numbers into the region array
    (dotimes (i rarray-size)
      (setf (region-constant (row-major-aref rarray i)) (pop rml))
      )
  plm)
  )

; Find first region in list that covers span of specified region
(defun find-region (r regions)
  (let (fr)
    (dolist (ir regions)
      (when (within-region r nil ir)
        (return (setq fr ir))
        )
      )
    fr)
  )

; Recur on create-assign-id-function, going down one variable at a time
(defun create-assign-id-function-recur (predicate idp id-num d vs vns entry ev-op)
  (let* ((rank (length vs))
         (rank-1 (1- rank))
         (v (aref vs d))
         (vt (svariable-type v))
         (constants (stype-constants vt))
         vtmin ; Minimum of type (up by 1/2 if centering)
         new-entry
         rne
         )
    (setq vtmin (stype-min vt))
    (when (and center-discrete-numeric-on-integer (not constants))
      (setq vtmin (+ vtmin 1/2))
      )
    (if (and (eq (stype-name (svariable-type v)) 'operator)
             (not (and ev-op (eq (svariable-name v) 'wm-evaluate))) ; Not the evaluate argument of evaluate-operator
             )
        (progn
          (setq new-entry (cons id-num entry))
          (if (= d rank-1) ; At leaf
              (progn
                (setq rne (reverse new-entry))
                (setq idp (update-region (cpt-region vns rne vs) idp))
                (setf (aref (predicate-id-contents predicate) (- id-num (predicate-first-operator predicate))) (cdr rne))
                (setq id-num (1+ id-num))
                )
            (multiple-value-setq (id-num idp) (create-assign-id-function-recur predicate idp id-num (+ d 1) vs vns new-entry ev-op))
            )
          )
      (dotimes (i (if (and ev-op (eq (svariable-name v) 'wm-evaluate))
                      (/ (stype-span vt) 2)
                    (stype-span vt)))
        (setq new-entry (cons (if constants
                                  (nth (+ i vtmin) constants)
                                (+ i vtmin))
                              entry))
        (if (= d rank-1) ; At leaf
            (progn
              (setq rne (reverse new-entry))
              (setq idp (update-region (cpt-region vns rne vs) idp))
              (setf (aref (predicate-id-contents predicate) (- id-num (predicate-first-operator predicate))) (cdr rne))
              (setq id-num (1+ id-num))
              )
          (multiple-value-setq (id-num idp) (create-assign-id-function-recur predicate idp id-num (+ d 1) vs vns new-entry ev-op))
          )
        ))
    (values id-num idp))
  )

; Create a function that assigns operator id's to all combinations of values of the other argument types
(defun create-assign-id-function (predicate)
  (let* ((vs (predicate-wm-variables predicate))
         (idp (init-plm vs 0 0 (init-vector (length vs) t))) ; Delta function for assigning ids
         id-num
         )
    (multiple-value-setq (id-num idp) (create-assign-id-function-recur predicate idp (predicate-first-operator predicate) 0 vs (variable-names vs) '(1)
                                                                       (eq (predicate-name predicate) 'evaluate-operator)))
    id-num ; Just included so that no warning is generated for not using
    idp)
  )

; Create factor node that assigns ids to a predicate and attach to the WMVN
(defun assign-ids-to-predicate (predicate wmvn)
  (let (idn)
    (setq idn (init-factor-node (concat-symbols (list (predicate-name predicate) 'wm 'ids) t)
                    'ids (predicate-wm-variables predicate) (list wmvn) nil nil t nil
                     nil nil))
    (setf (node-function idn) (predicate-assign-ids predicate))
    t)
  )

; Eliminate any variable from the second argument (a vector) if not named in the first
; argument (a list)
(defun eliminate-vars-if-not-named (name-l var-v)
  (let* ((rank (length var-v))
         new-var-l)
    (dotimes (i rank)
      (when (member (svariable-name (aref var-v i)) name-l)
        (setq new-var-l (cons (aref var-v i) new-var-l))))
    (coerce (reverse new-var-l) 'vector)
    )
  )

; Put symbol between each pair of list entries
(defun insert-symbol-list (l sym)
  (let (nl)
    (do ((l-l l (cdr l-l)))
        ((null l-l))
      (setq nl (cons (car l-l) nl))
      (when (cdr l-l)
        (setq nl (cons sym nl))))
    (reverse nl))
  )

; Return variable name if variable otherwise combination if a list
(defun variable-list-name (vs)
  (if (listp vs)
      (concat-symbols (insert-symbol-list (variable-names vs) '*))
    (svariable-name vs))
  )

; Return list of variable names from a vector of variables
(defun variable-names (variables)
  (map 'list #'variable-list-name variables)
  )

; Is vector not empty?
(defun not-empty-vector (vector)
  (> (length vector) 0))

; Recur on merge-variable-vectors
(defun merge-variable-vectors-recur (vs1 vs1i vs2 vs2i)
  (cond ((>= vs1i (length vs1)) ; Nothing left in first vector
         (if (>= vs2i (length vs2)) ; Nothing left in second vector
             nil
           (coerce (subseq vs2 vs2i) 'list))) ; Create list for ending subsequence of second vector
        ((>= vs2i (length vs2)) ; Nothing left in second vector
         (coerce (subseq vs1 vs1i) 'list)) ; Create list for ending subsequence of first vector
        ((eq (svariable-name (aref vs1 vs1i)) ; First element of first vector also first of second
             (svariable-name (aref vs2 vs2i)))
         (cons (aref vs1 vs1i) (merge-variable-vectors-recur vs1 (+ vs1i 1) vs2 (+ vs2i 1))))
        ((not (find (svariable-name (aref vs1 vs1i)) vs2
                    :start vs2i :key #'svariable-name)) ; First element of first vector not in rest of second
         (cons (aref vs1 vs1i) (merge-variable-vectors-recur vs1 (+ vs1i 1) vs2 vs2i)))
        ((not (find (svariable-name (aref vs2 vs2i)) vs1
                    :start vs2i :key #'svariable-name)) ; First element of second vector not in rest of first
         (cons (aref vs2 vs2i) (merge-variable-vectors-recur vs1 vs1i vs2 (+ vs2i 1))))
        (t ; Cannot order merged variables so as to satisfy both existing orders
         (error "No compatible ordering for variable vectors ~S and ~S." vs1 vs2))
        )
  )

; Merge two vectors of variables from variable nodes into a new vector,
; preserving order of variables across two vectors
; Genererates an error if it isn't possible (and will require dynamic dimension reording)
(defun merge-variable-vectors (vs1 vs2)
;  (coerce (merge-variable-vectors-recur vs1 0 vs2 0) 'vector)
  (union-two-variables vs1 vs2)
  )

; Create the variables for a delta node
; numvs is the number of variables in each of the variable nodes connected to the delta
; pvs is a Boolean vector with T where a predicate/WM variable is used in delta
; cvsi points from positions in wmvs to corresponding conditional variables
; wmvs is a full vector of working memory variables for the predicate
; Resulting vector should have WM variables in each position, doubled up with
; conditional variables when correspond
(defun delta-variables (numvs pvs cvsi wmvs)
  (let ((dvs (init-vector numvs))
        (k 0) ; Index into dvs
        )
  (dotimes (i (length wmvs))
    (when (aref pvs i)
      (if (aref cvsi i)
          (setf (aref dvs k) (list (aref wmvs i) (aref cvsi i)))
        (setf (aref dvs k) (aref wmvs i)))
      (setq k (+ k 1))
      )
    )
  dvs)
  )

; Create a list of 1 through N
(defun number-list (n)
  (let (l)
    (dotimes (i n)
      (setq l (cons i l))
      )
    (reverse l))
  )

; Create a delta factor
(defun create-delta-factor (c delta-vs c-varns alpha-memory previous-vn evidence-vn in out subsubtype pred not-equals)
  (let (split-delta-vs ; Rather than pairs of variables in each slot, a single vector with all variables
        vars ; List for split-delta-vs
        delta-node ; Delta node
        affines ; List of affine transforms from variable pairs
        v ; variable
        (v-count 0) ; variable count
        affine ; Affine used at position (may be a reused one from an earlier position)
        ai ; Index of reused from/pattern variable
        )
    (dotimes (i (length delta-vs))
      (setq v (aref delta-vs i))
      (when v
        (cond ((listp v)
               (cond ((symbolp (cadr v)) ; Reuse of a variable in a pattern
                      (when in
                        (error "Reuse of variables within a single condact or action, as in conditional ~S, is not currently allowed." (conditional-name c))
                        )
                      (setq vars (cons (car v) vars))
                      (setq ai (- v-count (position (cadr v) vars :key #'svariable-name)))
                      (dolist (a affines)
                        (when (= (affine-from a) ai)
                          (setq affine a)
                          )
                        )
                      (setf (affine-to affine) ; Add new WM/predicate variable to the "to" list (which is the predicate/WM variable)
                            (if (numberp (affine-to affine))
                                (list v-count (affine-to affine))
                              (cons v-count (affine-to affine)))
                            )
                      (setq v-count (+ v-count 1))
                      )
                     (t ; Map variable
                      (setq vars (cons (car v) (cons (cadr v) vars)))
                      (setq affines (cons (setq affine (make-affine :from v-count :to (+ v-count 1))) affines))
                      (setq v-count (+ v-count 2))
                      )
                     )
               )
              (t ; Variable stays the same
               (setq vars (cons v vars))
               (setq affines (cons (make-affine :to v-count) affines))
               (setq v-count (+ v-count 1))
               )
              )
        (when (aref not-equals i)
          (setf (affine-not-equal affine) (position (aref not-equals i) c-varns))
          )
        )
      )
    ; Add WM variables that aren't in patterns to list of variables (will be in variable node before delta)
;    (dotimes (i (length wmvs))
;      (unless (aref pattern-vs i)
;        (setq vars (cons (aref wmvs i) vars))
;        (setq affines (cons 
;        )
;      )
    (setq split-delta-vs (coerce (reverse vars) 'vector))
    ; Create delta factor for predicate pattern
    (when (or in out)
      (setq delta-node
            (init-factor-node (concat-symbols (cons (conditional-name c) (cons 'ADF c-varns)) t)
                              'affine
                              split-delta-vs
                              (list previous-vn alpha-memory)
                              (list evidence-vn) in out nil nil c subsubtype))
      (setf (node-region-pad delta-node) (if (open-world pred) 1 0))
      (setf (node-function delta-node) affines)
      )
    delta-node)
  )

; From a vector of WM variables create a vector of transformed WM variables
(defun transform-variables (vs)
  (let* ((vsl (length vs))
         (nvs (init-vector vsl))
         v)
    (dotimes (i vsl)
      (setq v (aref vs i))
      (setf (aref nvs i) (make-svariable :name (concat-symbols (list (svariable-name v) '-i))
                                         :type (svariable-type v)
                                         :unique (svariable-unique v)
                                         :select (svariable-select v)
                                         ))
      )
    nvs)
  )

; Create double up variable vectors for transform factor
(defun transform-factor-variables (vs1 vs2)
    (let* ((vsl (length vs1))
         (nvs (init-vector vsl)))
    (dotimes (i vsl)
      (setf (aref nvs i) (list (aref vs1 i) (aref vs2 i)))
      )
    nvs)
  )

; Create a transform factor (must be an inverse at this point)
(defun create-transform-factor (c type t-vars previous-vars next-vn previous-vn evidence-vn in out subsubtype)
  (unless (member type '(invert negate exponentiate))
    (error "Unknown transform type ~S in conditional ~S." type (conditional-name c))
    )
  (let (transform-node)
    (when (or in out)
      (setq transform-node
            (init-factor-node (concat-symbols (list (conditional-name c) (case type ((invert) 'IF) ((negate) 'NF) ((exponentiate) 'EF))) t)
                              'transform
                              (transform-factor-variables previous-vars t-vars)
                              (if (consp previous-vn)
                                  (append previous-vn (list next-vn))
                                (list previous-vn next-vn))
                              (if (consp evidence-vn)
                                  evidence-vn
                                (list evidence-vn))
                              in out nil nil c subsubtype))
      (setf (node-function transform-node) (case type
                                             ((invert) #'invert-function)
                                             ((negate) #'negate-function)
                                             ((exponentiate) #'exponentiate-constant-times10-function)))
      )
    ; For an exponential transform, normalize the result
; To work, needs some way of saying which variable to normalize over if there are only multiple variables.
;    (setf (node-normalize transform-node) (case type ((invert) nil) ((exponentiate) t)) )
    transform-node)
  )

; A weight list is a list with two numbers
(defun weight-list (candidate)
  (and (listp candidate)
       (numberp (car candidate))
       (cdr candidate)
       (numberp (cadr candidate))))

; Determine if two spans are e=
(defun span-e= (s1 s2)
  (and
   (e= (car s1) (car s2) t)
   (e= (cadr s1) (cadr s2) t)
   )
  )

; Check if two lists of predicate arguments are compatible
; Constants should be the same, and should be variables in the same place
(defun compatible-constants (elements1 elements2 pred)
  (let ((compatible t)
        ec1 element2 ec2 e-type)
    (dolist (element1 elements1)
      (setq e-type (type-from-predicate-argument (element-argument-name element1) pred))
      (setq ec1 (element-content element1))
      (setq element2 (assoc (element-argument-name element1) elements2))
      (if element2
          (setq ec2 (element-content element2))
        (error "In compatible-constants, argument name ~S not found in argument list ~S." (element-argument-name element1) elements2)
        )
      (when (or (and (listp ec1) (symbolp ec2))
                (and (symbolp ec1) (listp ec2))
                (and (symbolp ec1) (symbolp ec2) (not (span-e= (compute-span ec1 e-type) (compute-span ec2 e-type))))
                )
        (setq compatible nil)
        (return nil))
      )
    compatible)
  )

(defun create-wm-variable-node (pred wm node-name action wmvs vn-in vn-out l-in l-out exponential)
  (let (wmvn link)
    (setq wmvn ; Create new WM variable node for predicate
          (init-variable-node node-name 'wm 'positive action wmvs t))
    (setf (node-pattern-type wmvn) (pattern-type vn-in vn-out)) ; For use when connected directly to a beta factor
    (setf (node-exponential wmvn) exponential)
    (setf (node-normalize wmvn) (and (not (predicate-no-normalize pred))
                                     (predicate-unique pred)
                                     ))
    (if (predicate-vector pred)  (setf (node-vector wmvn) t)) 
    (setf (node-predicate wmvn) pred)
    (setf (node-wmvn wmvn) t)
    ; Link WM variable node to WM factor node when there is one and an active direction
    (when (and wm
               (or l-in l-out)
               )
      (setq link (make-link :map (build-smap wmvs wmvs)
                            :nodes (vector wmvn wm)
                            :contents (vector nil nil)
                            :depths (vector nil nil)
                            :loop-depths (vector nil nil)
                            :inits (vector -1 -1)
                            :incoming (vector nil nil)
                            :in l-in
                            ; (if condacts-change-wm l-in (and l-in (not l-out)))
                            :out l-out
                            :counts (vector 0 0)
                            :stale (vector nil nil)
                            :prequeue (vector nil nil)))
      (add-link link)
      )
    ; Link incoming WM variable node to perception factor node when there is one
    (when (and (predicate-perception pred)
               (or vn-in
                   (and (open-world pred) (not (predicate-select pred)))
                   )
               )
      (setq link (make-link :map (build-smap wmvs wmvs)
                            :nodes (vector wmvn (predicate-perception pred))
                            :contents (vector nil nil)
                            :depths (vector nil nil)
                            :loop-depths (vector nil nil)
                            :inits (vector -1 -1)
                            :incoming (vector nil nil)
                            :in nil
                            :out t
                            :counts (vector 0 0)
                            :stale (vector nil nil)
                            :prequeue (vector nil nil)))
      (add-link link)
      )
    (when vn-out
      (setf (predicate-outgoing-vn pred) wmvn)
      )
    (when (and vn-in (not (predicate-prediction pred))) ; Avoid overwriting this information for prediction predicates
      (setf (predicate-incoming-vn pred) wmvn)
      )
    ; If the predicate defines operators, set up its ids
    (when (predicate-assign-ids pred)
      (assign-ids-to-predicate pred wmvn)
      )
    wmvn)
  )

; Return constant names from element list for use in WM variable node name
(defun elements-constants (elements)
  (let (constants)
    (dolist (e elements)
      (when (symbolp (element-content e))
        (setq constants (cons (element-content e) constants)))
      )
    (reverse constants))
  )

; Return condition variable node (WM VAN) for including contents of WM FN in WM FAN
(defun condition-variable-node (wm)
  (let (vn)
    (dolist (l (node-links wm))
      (setq vn (link-var-node l))
      (if (and (eq (node-subsubtype vn) 'van) (not (link-in l)) (link-out l))
          (return vn)
        (setq vn nil)
        )
      )
    vn)
  )

; Add an action to an alpha network
; Returns variable node to be hooked to pattern's delta node
(defun add-action (pred wm vn-type name constants negated exponential wmvs c shared persistent open)
  (let (outgoing-vn
        incoming-vn
        wmvn ; WM VN for combined actions
        wmvan ; VN for uncombined action
        wmfnvan ; VN for connecting WM FN to WM FAN
        wmfan ; Combining factor node
        selection-pred ; Selection predicate
        (pred-name (predicate-name pred))
        direct-vn-fn-link
        (perfn (predicate-perception pred)) ; Perception factor node
        direct-vn-per-link
        pervan
        wm-discount-node ; Function to use in discounting contents of the WMFN at the VAN
        )
    ; Find the link to the WM VN (action combination variable node)
    (setq incoming-vn (predicate-incoming-vn pred))
    ; Find the shared outgoing WM VN if sharing this action
    (when shared
      (setq outgoing-vn (predicate-outgoing-vn pred))
      )
    ; Retrieve the action/incoming WM VN if it exists, otherwise create
    (if incoming-vn ; There is already an action/incoming VN
        (setq wmvn incoming-vn)
      (if (and shared outgoing-vn) ; Use the outgoing VN
          (progn
            (setf (node-action outgoing-vn) t)
            (setf (node-pattern-type outgoing-vn) 'condact) ; Mark node as bidirectional
            (when wm
              (setq direct-vn-fn-link (link-from-numbers (node-number outgoing-vn) (node-number wm)))
              )
            (if direct-vn-fn-link ; There is already a direct link
                (when persistent
                  (setf (link-in direct-vn-fn-link) t) ; Ensure that it is marked as coming in
                  )
              (progn
                (when wm
                  (add-link (make-link :map nil ; Create link from VN to FN
                                       :nodes (vector outgoing-vn wm) :contents (vector nil nil)
                                       :depths (vector nil nil)
                                       :loop-depths (vector nil nil)
                                       :inits (vector -1 -1) :incoming (vector nil nil)
                                       :in persistent :out nil :counts (vector 0 0)
                                       :stale (vector nil nil)
                                       :prequeue (vector nil nil)))
                  )
                ; Add link between perception FN and outgoing/shared VN when perception predicate and link doesn't already exist
                (when (and perfn (not (link-from-numbers (node-number outgoing-vn) (node-number perfn))))
                  (add-link (make-link :map nil ; Create link from VN to FN
                                     :nodes (vector outgoing-vn perfn) :contents (vector nil nil)
                                     :depths (vector nil nil)
                                     :loop-depths (vector nil nil)
                                     :inits (vector -1 -1) :incoming (vector nil nil)
                                     :in nil :out t :counts (vector 0 0)
                                     :stale (vector nil nil)
                                     :prequeue (vector nil nil)))
                  )
                ))
            (setf (predicate-incoming-vn pred) outgoing-vn)
            (setq wmvn outgoing-vn)
            )
        (progn
          (setq wmvn (create-wm-variable-node pred wm ; Create a new action/incoming combination variable node
                                              (concat-symbols (list pred-name vn-type 'wm-vn) t)
                                              t wmvs t shared persistent nil exponential))
          )))
    (cond ((or (and negated (not (or (predicate-vector pred) (predicate-no-normalize pred)))) ; Negated normalized action 
               (and open open-actions-like-condacts) ; An open world action that is treated like half of a condact
               )
           wmvn) ; Just connect to wmvn
          (t ; Positive action to be disjunctively combined
            ; Create a new WM VAN (uncombined action variable node)
            ; Will link via WM FAN to WM VN (combined action variable node)
            (setq wmvan (init-variable-node (concat-symbols (if (eq constants '||) (list name 'wm-van) (list name constants 'wm-van)) t)
                                          'wm 'van nil wmvs t 'action))
            ; Get action combination factor node if exists
            (setq wmfan (predicate-fan pred))
            (cond (wmfan ; There is already an action/incoming FAN, so link to it
                   (add-link (make-link :map nil ; Link new variable node to combination factor node
                                        :nodes (vector wmvan wmfan) :contents (vector nil nil)
                                        :depths (vector nil nil)
                                        :loop-depths (vector nil nil)
                                        :inits (vector -1 -1) :incoming (vector nil nil)
                                        :in t :out nil :counts (vector 0 0)
                                        :stale (vector nil nil)
                                        :prequeue (vector nil nil)))
                   )
                  (t ; There is not an action/incoming WM FAN, so create and link to WM VAN and WM VN
                   (setq wmfan                  
                         (init-factor-node (concat-symbols (list pred-name 'wm-fan) t)
                                           'combine wmvs
                                           (list wmvan wmvn) (list wmvn) t nil nil t c
                                           (if (predicate-universal pred)
                                               'max
                                             (if (or (predicate-no-normalize pred) (predicate-vector pred)) 
                                                   'sum
                                                 'por)))) ; Probabalistic or (assuming independence)                          
                   (setf (node-predicate wmfan) pred)
                   (setf (predicate-fan pred) wmfan)
                   (setf (node-function wmfan) (init-plm wmvs (if (and (not open-world-wmfns) (open-world pred)) fan-constant 0) 0 (init-vector (length wmvs) t)))
                   ; Conditionally create WM VAN path from WM FN to incoming WM FAN
                   (when (or (and (not open)
                                  (or (predicate-select pred)
                                      (predicate-universal pred)
                                      (predicate-cumulative pred)
                                      )
                                  )
                             (and open open-world-wmfns (not open-actions-like-condacts)
                                  (or (predicate-cumulative pred) (predicate-universal pred))
                                  )
                             )
                     (setq wmfnvan (init-variable-node (concat-symbols (if (eq constants '||) (list name 'cond-wm-van) (list name constants 'cond-wm-van)) t)
                                      'wm 'van nil wmvs t 'condition))
                     ; Conditionally add discount node for WM through VAN to FAN
                     (when (and discount-wm (predicate-select pred))
                       (setq wm-discount-node (init-factor-node (concat-symbols (list pred-name 'wm-discount) t)
                                                                'discount wmvs
                                                                (list wmfnvan) nil nil t nil t c))
                       (setf (node-function wm-discount-node) (init-plm wmvs wm-discount-factor 0 (init-vector (length wmvs) t)))
                       (setf (node-assumption wm-discount-node) t)
                       )
                     (add-link (make-link :map nil ; Link new variable node to FAN
                                          :nodes (vector wmfnvan wmfan) :contents (vector nil nil)
                                          :depths (vector nil nil)
                                          :loop-depths (vector nil nil)
                                          :inits (vector -1 -1) :incoming (vector nil nil)
                                          :in t :out nil :counts (vector 0 0)
                                          :stale (vector nil nil)
                                          :prequeue (vector nil nil)))
                     (add-link (make-link :map nil ; Link new variable node to predicate WM FN
                                          :nodes (vector wmfnvan wm) :contents (vector nil nil)
                                          :depths (vector nil nil)
                                          :loop-depths (vector nil nil)
                                          :inits (vector -1 -1) :incoming (vector nil nil)
                                          :in nil :out t :counts (vector 0 0)
                                          :stale (vector nil nil)
                                          :prequeue (vector nil nil)))
                     ; Delete direct link from FN to outgoing/shared VN if sharing and they exist
                     (when (and shared (predicate-outgoing-vn pred) wm)
                       (setq direct-vn-fn-link (link-from-numbers (node-number (predicate-outgoing-vn pred)) (node-number wm)))
                       (when direct-vn-fn-link
                         (setf (link-out direct-vn-fn-link) nil)
                         )
                       )
                     ; Conditionally create WM VAN path from PER to WM FAN
                     (when (and perfn (closed-world pred))
                       (setq pervan (init-variable-node (concat-symbols (if (eq constants '||) (list name 'per-van) (list name constants 'per-van)) t)
                                                         'wm 'van nil wmvs t 'condition))
                       (add-link (make-link :map nil ; Link new variable node to FAN
                                            :nodes (vector pervan wmfan) :contents (vector nil nil)
                                            :depths (vector nil nil)
                                            :loop-depths (vector nil nil)
                                            :inits (vector -1 -1) :incoming (vector nil nil)
                                            :in t :out nil :counts (vector 0 0)
                                            :stale (vector nil nil)
                                            :prequeue (vector nil nil)))
                       (add-link (make-link :map nil ; Link new variable node to predicate PER FN
                                            :nodes (vector pervan perfn) :contents (vector nil nil)
                                            :depths (vector nil nil)
                                            :loop-depths (vector nil nil)
                                            :inits (vector -1 -1) :incoming (vector nil nil)
                                            :in nil :out t :counts (vector 0 0)
                                            :stale (vector nil nil)
                                            :prequeue (vector nil nil)))
                       (when (predicate-incoming-vn pred)
                         (setq direct-vn-per-link (link-from-numbers (node-number (predicate-incoming-vn pred)) (node-number perfn)))
                         (when direct-vn-per-link
                           (setf (link-out direct-vn-per-link) nil)
                           )
                         )
                       )
                     )
                   (setq selection-pred (predicate-from-name 'selected t))
                   (when (and selection-pred wm (eq (node-name wm) (node-name (predicate-wm selection-pred)))) ; Action for selected
                     (setf (graph-positive-preferences cg) (cons wmfan (graph-positive-preferences cg)))
                     )
                   )
                  )
            wmvan)
          )
    )
  )

; Excise a link from the graph
(defun excise-link (l)
  (setf (node-links (aref (link-nodes l) 0)) (remove l (node-links (aref (link-nodes l) 0))))
  (setf (node-links (aref (link-nodes l) 1)) (remove l (node-links (aref (link-nodes l) 1))))
  (setf (graph-links cg) (remove l (graph-links cg)))
  t)

; Add a condition to an alpha network
(defun add-condition (pred wm vn-name name constants wmvs c exponential shared-with-actions)
  (let ((wmvn (predicate-outgoing-vn pred)) ; Outgoing WM VN
        (wmvan (when wm (condition-variable-node wm))) ; Variable node for including contents of WM FN in WM FAN
        pervan ; Variable node for including contents of PER in WM FAN
        (perfn (predicate-perception pred)) ; Perception factor node
        wmfan ; Factor node for combining actions and WM
        wm-discount-node ; Function to use in discounting contents of the WMFN at the VAN
        incoming-vn ; Incoming variable node, if there is one
        (connect-directly-to-shared-vn (or (not (predicate-persistent pred)) ; Connect directly from FN to VN
                                           (and (predicate-persistent pred)
                                                (predicate-unique pred)
                                                (or (predicate-select pred)
                                                    (predicate-replace pred)
                                                    )
                                                (not (predicate-perception pred))
                                                )
                                           ))
        )
    ; If there is not an outgoing WM VN, use incoming if shared with it and it exists, or create it
    (unless wmvn
      (setq incoming-vn (predicate-incoming-vn pred))
      (if (and shared-with-actions incoming-vn)
          (progn
            (setq wmvn incoming-vn)
            (setf (node-pattern-type incoming-vn) 'condact) ; Mark node as bidirectional
            )
        (setq wmvn (create-wm-variable-node pred wm ; Create a new variable node
                                            vn-name
                                            nil wmvs nil t nil connect-directly-to-shared-vn exponential))
        )
      (setf (predicate-outgoing-vn pred) wmvn)
      )
    ; If there is a WM and not a WM VAN for including the contents of WM FN in the WM FAN, create it when doesn't connect directly
    ; Do the same for PER as well
    (when (and wm ; There is a WM FN node
               (not wmvan) ; There is not already a WM VAN node
               (not connect-directly-to-shared-vn) ; Connect via VAN/FAN rather than directly
               )
      (setq wmvan (init-variable-node (concat-symbols (if (eq constants '||) (list name 'cond-wm-van) (list name constants 'cond-wm-van)) t)
                                      'wm 'van nil wmvs t 'condition))
      ; Conditionally add discount node for WM through VAN to FAN
      (when (and discount-wm (predicate-select pred))
              (setq wm-discount-node (init-factor-node (concat-symbols (list (predicate-name pred) 'wm-discount) t)
                                                       'discount wmvs
                                                       (list wmvan) nil nil t nil t c))
              (setf (node-assumption wm-discount-node) t)
              (setf (node-function wm-discount-node) (init-plm wmvs wm-discount-factor 0 (init-vector (length wmvs) t)))
              )
      (add-link (make-link :map nil ; Link WM FN to new WM VAN
                           :nodes (vector wmvan wm) :contents (vector nil nil)
                           :depths (vector nil nil)
                           :loop-depths (vector nil nil)
                           :inits (vector -1 -1) :incoming (vector nil nil)
                           :in nil :out t :counts (vector 0 0)
                           :stale (vector nil nil)
                           :prequeue (vector nil nil)))
      (when (and perfn (closed-world pred)) ; Handle perception FN when there is one
        (setq pervan (init-variable-node (concat-symbols (if (eq constants '||) (list name 'per-van) (list name constants 'per-van)) t)
                                         'wm 'van nil wmvs t 'condition))
        (add-link (make-link :map nil ; Link PER to new PER VAN
                             :nodes (vector pervan perfn) :contents (vector nil nil)
                             :depths (vector nil nil)
                             :loop-depths (vector nil nil)
                             :inits (vector -1 -1) :incoming (vector nil nil)
                             :in nil :out t :counts (vector 0 0)
                             :stale (vector nil nil)
                             :prequeue (vector nil nil)))
        )
      (setq wmfan (predicate-fan pred))
      ; If there is not a WM FAN for this predicate, create one; otherwise hook WM VAN (and PER VAN) to it
      (cond (wmfan
             (unless (link-from-numbers (node-number wmfan) (node-number wmvan)) ; Only add link if doesn't already exist
               (add-link (make-link :map nil ; Link new variable node to FAN
                                    :nodes (vector wmvan wmfan) :contents (vector nil nil)
                                    :depths (vector nil nil)
                                    :loop-depths (vector nil nil)
                                    :inits (vector -1 -1) :incoming (vector nil nil)
                                    :in nil :out t :counts (vector 0 0)
                                    :stale (vector nil nil)
                                    :prequeue (vector nil nil)))
               (setf (node-evidence wmfan) (cons wmvan (node-evidence wmfan)))
               )
             (when (and pervan (not (link-from-numbers (node-number wmfan) (node-number pervan)))) ; Only add link if doesn't already exist
               (add-link (make-link :map nil ; Link new variable node to FAN
                                    :nodes (vector pervan wmfan) :contents (vector nil nil)
                                    :depths (vector nil nil)
                                    :loop-depths (vector nil nil)
                                    :inits (vector -1 -1) :incoming (vector nil nil)
                                    :in nil :out t :counts (vector 0 0)
                                    :stale (vector nil nil)
                                    :prequeue (vector nil nil)))
               )
             )
            (t
             (setq wmfan
                   (init-factor-node (concat-symbols (list name 'wm-fan) t)
                                     'combine wmvs
                                     (if pervan (list wmvan wmvn pervan) (list wmvan wmvn))
                                     (if pervan (list wmvan pervan) (list wmvan))
                                     nil t nil t c
                                     (if (predicate-universal pred)
                                               'max
                                               (if (or (predicate-vector pred) (predicate-no-normalize pred)) 
                                                   'sum
                                                 'por)) ; Probabalistic OR (assuming independence)
                                     ))
             (setf (node-predicate wmfan) pred)
             (setf (predicate-fan pred) wmfan)
             (setf (node-function wmfan) (init-plm wmvs (if (and (not open-world-wmfns) (open-world pred)) fan-constant 0) 0 (init-vector (length wmvs) t)))
             )
            )
      )
    wmvn)
  )

;compute the gradient for vector learning 
(defun compute-vector-gradient (node smooth)
  (let* ((link (car (node-links node)))
         (update (link-var-content link)) ; Feedback to use in updating
         (map (link-map link))
         ;(normal (node-normal node)) ; Index of the variable to normalize over, if any
         (nf (node-function node)) ; Previous factor function
         (vs (plm-variables nf))
         (tgdl (trace-gdl node))
         (lr (if (node-learning-rate node)
                 (node-learning-rate node)
               (if learning-rate-fraction-of-smoothing-parameter
                   (* learning-rate-fraction-of-smoothing-parameter smooth)
                 learning-rate)))
         (alr (if arousal (* arousal lr) lr))
         (wg (initialize-weighted-gradient vs alr))  ; Initialize plm with learning rate       
         ;(predicate-argument-unique-positions (init-vector (length vs))) ; Which variables (by position) are unique arguments for GDL in predicate
         ;(pred (node-predicate node)) ; Predicate for node
         )

     ; Multiply gradient by learning rate
    (setq update (combine-plms update map wg 'product)) 
  
    (when tgdl
      (format trace-stream "~&Vectors-Update function discounted by learning rate of ~S: " alr)
      (pplm update)
      )
    update
    )
)

; Determine if a PLM has any selection variables
; Variable must be declared unique/expected and be active in the PLM
(defun selection-variables (plm)
  (let ((pvs (plm-variables plm))
        (as (plm-active plm))
        selection)
    (dotimes (i (plm-rank plm))
      (when (and (selection-variable (aref pvs i))
                 (aref as i))
        (setq selection (cons i selection))
        )
      )
    selection)
  )

; A negated constant (as part of a variable list) is a list (- <constant>)
(defun negated-constant (item)
  (and (listp item)
       (eq (car item) '-)
       (equal (length item) 2)
       (constant-element (cadr item))
       )
  )

; Create a PLM for the constant/filter aspect of a pattern element
(defun create-element-plm (variable data &optional default-value)
  (let ((p (init-plm (vector variable) (if default-value default-value 0) 0 (vector t)))
        (type (svariable-type variable))
        r span constant weight dimension
        )
    (setf (plm-piecewise-constant p) t)
    (dolist (datum data)
      (setq span (compute-span (car datum) type t))
      (setq constant (cadr datum))
      (setq weight (caddr datum))
      (unless (zerop weight)
        (setf (plm-piecewise-constant p) nil)
        )
      (setq dimension (make-dimension :min-slice (make-slice :location (car span)) :max-slice (make-slice :location (cadr span))
                                      :weight weight :discrete (stype-discrete type)))
      (setq r (make-region :constant constant :dimensions (vector dimension) :maximals (init-vector 1)))
      (setq p (update-region r p))
      )
    p)
  )

; Extend vector with specified value at end
(defun extend-vector (vector value)
  (let* ((vr (length vector))
         (nvector (init-vector (+ vr 1)))
         )
    (dotimes (i vr)
      (setf (aref nvector i) (aref vector i))
      )
    (setf (aref nvector vr) value)
    nvector)
  )

; Determine if a list specifies a filter
(defun filter-listp (l)
  (and (consp l) (equal (car l) filter-symbol))
  )
; Access parts of a filter
(defun filter-span (l) (cadr l))
(defun filter-constant (l) (caddr l))
(defun filter-coef (l) (cadddr l))

; Create a filter function from individual filters
(defun create-filter-function (vs filters)
  (let ((rank (length vs))
        filter-fn)
    ; Add full (all 1) filters for variables not explicitly mentioned
    (dotimes (i rank)
      (unless (aref filters i)
        (setf (aref filters i) (make-constant-discrete-plm '(0) (vector (aref vs i)) 1 0))
        )
      )
      ; Create a PLM by outer product of all of the filters
    (setq filter-fn (aref filters 0))
    (dotimes (i (- rank 1))
      (setq filter-fn (product-outer-plms filter-fn (aref filters (+ i 1))))
      )
    filter-fn)
  )

; Determine if a list after a variable name in a pattern specifies an affine-transform
(defun affine-listp (l)
  (and (consp l) (symbolp (car l)) (not (equal (car l) filter-symbol)) (not (equal (car l) not-equal-symbol)))
  )

; Determine if a list after a variable name is a not-equal test on another variable
(defun not-equal-listp (l)
  (and (consp l) (equal (car l) not-equal-symbol))
  )

; Determine pattern type
(defun pattern-type (in out)
  (if in (if out 'condact 'action) (if out 'condition nil))
  )

; Is variable used uniquely in pattern?
(defun variable-unique-in-pattern (vn pp)
  (let* ((pred (predicate-from-name (car pp)))
         (uan (predicate-unique pred))
         unique ec)
    (when uan
      (setq pp (cdr pp)) ; Skip over predicate name
      (unless (listp (car pp)) ; Skip over negation/exponentiation
        (setq pp (cdr pp))
        )
      (dolist (e pp)
        (when (and (or (and (symbolp uan) (eq (element-argument-name e) uan)) ; Same argument name
                       (member (element-argument-name e) uan) ; Argument name is part of unique list
                       )
                   (variable-element (setq ec (element-content e))) ; It is a variable
                   (eq (car ec) vn) ; Same variable name
                   )
          (return (setq unique t))
          )
        )
      )
    unique)
  )

; Is variable used uniquely in a list of patterns?
(defun variable-unique-in-patterns (vn pps)
  (let (unique)
    (dolist (pp pps)
      (when (variable-unique-in-pattern vn pp)
        (return (setq unique t))
        )
      )
    unique)
  )

; Is variable used uniquely in conditional?
(defun variable-unique-in-conditional (vn c)
  (let (unique)
    (setq unique (variable-unique-in-patterns vn (conditional-conditions c)))
    (unless unique
      (setq unique (variable-unique-in-patterns vn (conditional-condacts c)))
      )
    unique)
  )

; Two vectors of variables (at most one from a factor node) are equal
(defun variables-equal (vs1 vs2)
  (let ((same (= (length vs1) (length vs2))))
    (when same
      (dotimes (i (length vs1))
        (when (or (listp (aref vs1 i))
                  (listp (aref vs2 i))
                  (not (eq (svariable-name (aref vs1 i)) (svariable-name (aref vs2 i))))
                  )
          (return (setq same nil))
          )
        )
      )
    same)
  )

; Return variable node after filter for a filter that can be shared
; Only for conditions
(defun shared-filter-wmvn (previous-wmvn filter-fn reduced-vs)
  (let (fnode old-wmvn new-wmvn)
    (dolist (l (node-links previous-wmvn))
      (setq fnode (link-fact-node l))
      (when (and (not (link-in l)) ; Not an action or a condact
                 (eq (node-subtype fnode) 'filter) ; The factor node is a filter
                 (plm-e= filter-fn (node-function fnode)) ; The new filter function is the same as the existing ones
                 (variables-equal reduced-vs (node-variables (setq old-wmvn (link-var-node (other-link fnode l))))) ; Variables are the same in resulting vn
                 )
        (return (setq new-wmvn old-wmvn))
        )
      )
    new-wmvn)
  )

; Return variable node after negation for a negation that can be shared
; Only for conditions
(defun shared-negation-wmvn (previous-wmvn)
  (let (fnode new-wmvn)
    (dolist (l (node-links previous-wmvn))
      (setq fnode (link-fact-node l))
      (when (and (not (link-in l)) ; Not an action or a condact
                 (eq (node-subtype fnode) 'transform) ; The factor node is a filter
                 (eq #'invert-function (node-function fnode)) ; The transform node is an inversion
                 )
        (return (setq new-wmvn (link-var-node (other-link fnode l))))
        )
      )
    new-wmvn)
  )

; A pair of delta variables are equal
(defun delta-variables-equal (v1 v2)
  (or (and (not (consp v1)) (not (consp v2))
           (or (and (not v1) (not v2))
               (and v1 v2 (eq (svariable-name v1) (svariable-name v2)))
               )
           ) ; Single elements (nil or variable) identical
      (and (consp v1) (consp v2)
           (eq (svariable-name (car v1)) (svariable-name (car v2))) 
           (eq (svariable-name (cadr v1)) (svariable-name (cadr v2)))
           )
      )
  )

; Two vectors of delta variables are equal
(defun delta-variable-vectors-equal (vv1 vv2)
  (let ((same (eq (length vv1) (length vv2))))
    (dotimes (i (length vv1))
      (unless (delta-variables-equal (aref vv1 i) (aref vv2 i))
        (return (setq same nil))
        )
      )
    same)
  )

; Return variable node after delta for a delta that can be shared
; Only for conditions
(defun shared-delta-wmvn (previous-wmvn new-delta-vs)
  (let (fnode new-wmvn)
    (dolist (l (node-links previous-wmvn))
      (setq fnode (link-fact-node l))
      (when (and (not (link-in l)) ; Not an action or a condact
                 (eq (node-subtype fnode) 'affine) ; The factor node is a filter
                 (delta-variable-vectors-equal (node-variables (link-var-node (other-link fnode l))) new-delta-vs) ; The transform node is an inversion
                 )
        (return (setq new-wmvn (link-var-node (other-link fnode l))))
        )
      )
    new-wmvn)
  )

(defvar share-condition-tests t)

; Get argument-name corresponding to variable use in predicate pattern
(defun argument-from-variable (v pp)
  (car (find v pp :key #'caadr))
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
         offset-vn ;Volkan
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
         o-vars-v ;Volkan Vector of variables for the offset variable node
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
         action-from-variable;VOLKAN
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
      (setq transform-node (create-transform-factor c (if (or (predicate-vector pred) (predicate-no-normalize pred)) 'negate 'invert) iwmvs wmvs reduced-wmvn wmvn wmvn in out nil))
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
                          ;(when (and in (not out)) ; An action
                         (when in ;not condition
                            (error "Use the condact/condition variable ~S directly in condact/action ~S of conditional ~S instead of using a new condact/action variable (~S) in conjunction with :FROM ~S."
                                   (affine-from aff) pp (conditional-name c) (affine-to aff) (affine-from aff))      ;  Volkan: Allowing different :from and :to variables create problems while applying coefficients                              
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
                        (if (and (not in) out) ; This is a condition ;VOLKAN
                            (error "Variables not currently allowed as offsets in conditions: Pattern ~A in conditional ~S." original-pp (conditional-name c))
                          (progn  
                            (setq action-offset-variable (variable-from-name (affine-offset aff) (conditional-variables c)))
                            (setq action-from-variable (variable-from-name (affine-from aff) (conditional-variables c)))
                            )                                              
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
      
      ; If there is a separate from variable in an action, add it to the variables
       ; (when action-from-variable
      (when (not (eql (affine-from aff) (affine-to aff))) ; when there is a separate to affine-from variable, add it to the variables
        (setq c-vars-v (extend-vector c-vars-v action-from-variable))
        ;(setf (aref c-vars-v (position (affine-to aff) c-vars-v :key #'svariable-name)) action-from-variable)
        )

       ; If there is an offset variable in an action, add it to the variables
      (when action-offset-variable
        (setq c-vars-v (extend-vector c-vars-v action-offset-variable))
        )
    
      ; Convert :from and :to fields from pattern variable name to pattern variable numbers
      (dolist (aff affines)
        (setf (affine-from aff) (position (affine-from aff) c-vars-v :key #'svariable-name))
        (setf (affine-to aff) (position (affine-to aff) c-vars-v :key #'svariable-name))
        )
     
      ; Create variable node after transform factor node
      (setq reduced-wmvn
            (init-variable-node
             (concat-symbols (list cond-name p-name 'av) t)
             'affine nil nil 
             (if (eq (affine-to aff) (affine-from aff)) c-vars-v (reduce-vector c-vars-v (aref c-vars-v (affine-to aff))))
             t (pattern-type in out)))

      ;Volkan offset-variable node
      (when action-offset-variable
        (setq o-vars-v (reduce-vector c-vars-v (aref c-vars-v (affine-to aff))))
        (setq o-vars-v (reduce-vector o-vars-v (aref c-vars-v (affine-from aff)))) ;VOLKAN need to check whether this works all the time.
        ;(setq o-vars-v  (coerce (list action-offset-variable) 'vector))
        (setq offset-vn 
            (init-variable-node
             (concat-symbols (list cond-name p-name 'ov) t)
             'offset nil nil o-vars-v t (pattern-type t nil)))
        (setf (node-normalize offset-vn) t)
      )
      
             
      ; Create transform factor node
      (setq affine-node
            (init-factor-node (concat-symbols (list cond-name p-name 'af) t)
                              'affine
                              c-vars-v
                              (if action-offset-variable (list wmvn reduced-wmvn offset-vn) (list wmvn reduced-wmvn)) ;Volkan
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
    (values (cons wmvn beta-not-equal-vars) offset-vn)) ;Volkan
  )


; Create portion of the beta network for a pattern
(defun create-beta-network (c later-vars alpha-memory previous-beta-memory create-new-beta next-action out in &optional beta-not-equal-vars offset-vn)
  (let (beta-vars ; Variables used in the beta memory
        beta-var-names ; Names of beta variables
        new-beta-memory ; Memory after beta factor
        beta-node-vs ; Variables used in the beta factor
        beta-function ; Function in beta factor
        beta-neighbors ; Neighbors of beta factor
        beta-node ; Beta factor node
        (alpha-vars (node-variables alpha-memory)) ; Variables in alpha memory
        beta-memories
        same-neighbor ; There is a neighbor with same variables (in same order)
        same-length-neighbor ; Neighbor with same variables but different order
        beta-ne-vns ; List of pairs of not-equal variable numbers (from the names in beta-not-equal-vars)
        )
    ; The variables used in the beta-memory are from the alpha and the previous beta,
    ; omitting those not used later
    (setq beta-vars
          (if previous-beta-memory
              (merge-variable-vectors (node-variables previous-beta-memory) alpha-vars)
            alpha-vars))
    ; Eliminate any variables from the list that aren't used later
    (setq beta-vars (eliminate-vars-if-not-named later-vars beta-vars))
    ; Create a new beta memory for linear chain of patterns
    (when create-new-beta
      (setq beta-var-names (variable-names beta-vars))
      (setq new-beta-memory
            (init-variable-node (concat-symbols (cons (conditional-name c)
                                                      (cons 'BM beta-var-names)) t)
                                'beta nil nil beta-vars nil (node-pattern-type alpha-memory)))
      ; The following is used to optimize out link directions coming out of BFs
      (setf (node-subsubtype new-beta-memory) (if next-action 'action (if in 'condact 'condition)))
      (when next-action
        (setf (conditional-shared-action-beta c) new-beta-memory)
        )
      )
    ; Set last memory in conditional
    (setf (conditional-last-memory c)
          (if new-beta-memory
              new-beta-memory
            (if previous-beta-memory
                previous-beta-memory
              alpha-memory)
            ))
    ; The variables used in the beta factor are from the alpha, the previous beta
    ; and the next beta if there should be one
    ; Start merging with next beta if it exists so that hopefully get variables in the right order
    (if (not-empty-vector beta-vars)
        (setq beta-node-vs (merge-variable-vectors beta-vars (node-variables alpha-memory)))
      (setq beta-node-vs (node-variables alpha-memory)))
    (when previous-beta-memory
      (setq beta-node-vs (merge-variable-vectors beta-node-vs
                                                 (node-variables previous-beta-memory))))
    ; Specify nodes that are neighbors of the beta factor node
    (setf beta-memories (make-beta-memories :alpha alpha-memory :old-beta previous-beta-memory :new-beta new-beta-memory))
    (if offset-vn (setq beta-neighbors (list alpha-memory offset-vn)) (setq beta-neighbors (list alpha-memory)))
    (when previous-beta-memory (setq beta-neighbors (cons previous-beta-memory beta-neighbors)))
    (when new-beta-memory (setq beta-neighbors (cons new-beta-memory beta-neighbors)))
    ; Reorder variables in beta factor according to first neighbor with same number of variables
    ; This is an attempt to encourage the optimization that skips the first product when the variables align
    ; It doesn't check that the neighbor actually sends messages in direction of factor.  Should it?
    (dolist (bn beta-neighbors)
      (when (= (length beta-node-vs) (length (node-variables bn)))
        (if (variables-equal beta-node-vs (node-variables bn))
            (setq same-neighbor t)
          (setq same-length-neighbor bn))
        )
      )
    (when (and same-length-neighbor (not same-neighbor))
      (setf beta-node-vs (copy-seq (node-variables same-length-neighbor)))
      )
    ; Create a beta node
    (setq beta-node (init-factor-node (concat-symbols (cons (conditional-name c)
                                                                (cons 'BF (variable-names beta-node-vs))) t)
                                        'beta
                                        beta-node-vs
                                        beta-neighbors
                                        (list alpha-memory) in out beta-memories nil c))
    ; A beta function just has a constant function of 1 unless there are not-equal (<>) tests
    (setq beta-function (full-plm beta-node-vs))
    (setq beta-ne-vns (beta-not-equal-vars-nums beta-not-equal-vars beta-node-vs))
    (dolist (bnv beta-ne-vns) ; Check that variable types match up
      (unless (eq (svariable-type (aref beta-node-vs (car bnv)))
                  (svariable-type (aref beta-node-vs (cadr bnv))))
        (error "Variables ~S (~S) and ~S (~S) are not of the same type in a not-equal (<>) test in conditional ~S"
               (svariable-name (aref beta-node-vs (car bnv))) (stype-name (aref beta-node-vs (car bnv)))
               (svariable-name (aref beta-node-vs (cadr bnv))) (stype-name (aref beta-node-vs (cadr bnv)))
               (conditional-name c))
        )
      )
    (when beta-not-equal-vars ; There are across-pattern not-equal (<>) tests
      (setq beta-function (create-beta-not-equal-function beta-function beta-ne-vns))
      (setf (node-beta-non-full beta-node) t)
      )
    (setf (node-function beta-node) beta-function)
    ; Deactivate link direction from condact beta factor to previous condition beta (or alpha) memory
    (when (and (eq (node-subsubtype (if previous-beta-memory previous-beta-memory alpha-memory)) 'condition)
               new-beta-memory
               (eq (node-subsubtype new-beta-memory) 'condact)
               )
      (setf (link-out (link-from-nodes beta-node (if previous-beta-memory previous-beta-memory alpha-memory))) nil)
      )
    beta-node)
  )


; Create a list of lists of variable numbers from names for beta not-equal (<>) tests
(defun beta-not-equal-vars-nums (ne-vars vars)
  (let (nums)
    (dolist (vpair ne-vars)
      (setq nums (cons (list (variable-number (car vpair) vars) (variable-number (cadr vpair) vars)) nums))
      )
    nums)
  )

; Create a function with 0 diagonals for a beta node based on not-equal (<>) tests
(defun create-beta-not-equal-function (p ne-vnums)
  (dolist (d (if (> (length ne-vnums) 1) (apply #'union ne-vnums) (if ne-vnums (car ne-vnums) ne-vnums))) ; Shatter each dimension involved in diagonalization
    (setq p (shatter-plm p d))
    )
  (dolist (ne-vp ne-vnums)
    (setq p (zero-diagonal p ne-vp))
    )
  p
  )

; Is node a beta factor?
(defun beta-factor (n)
  (and (factor-nodep n) (eq (node-subtype n) 'beta))
  )

; Retrieve pattern variable data given name of variable
(defun pattern-variable-data (pv)
  (find pv (graph-pattern-vars cg) :key #'pattern-variable-name))

; Retrieve alpha memory for existing pattern variable if there is one (otherwise nil)
(defun get-pattern-variable-wm-memory (pv)
  (let ((pvd (pattern-variable-data pv)))
    (if pvd (pattern-variable-wm-memory pvd) nil)
    )
  )

; Find link on graph based on variable and factor nodes
(defun find-link (vn fn)
  (dolist (link (graph-links cg))
    (when (and (eq vn (link-var-node link))
               (eq fn (link-fact-node link)))
      (return link)
      )
    )
  )

; Generate a message from first node to second node (specified as numbers)
; Used in testing message generation from nodes
(defun create-message (n1 n2 wm-driven)
  (let ((node1 (node-from-number n1))
        (node2 (node-from-number n2))
        )
    (unless node1 (error "No node corresponding to number ~S." n1))
    (unless node2 (error "No node corresponding to number ~S." n2))
    (make-message :index (if (variable-nodep node1) 0 1)
                  :link (if (variable-nodep node1)
                            (find-link node1 node2)
                          (find-link node2 node1))
                  :wm-driven wm-driven
                  )
    )
  )

; Remove links based on out and in
(defun remove-links (alpha-memory beta-factor out in)
  (let ((link (find-link alpha-memory beta-factor)))
    (unless out (setf (aref (link-contents link) var-index) nil))
    (unless in (setf (aref (link-contents link) fact-index) nil))
    )
  )

; Get pattern/alpha variable name, if it exists, from pattern; otherwise nil
(defun get-pattern-variable-name (pp)
  (setq pp (cdr pp)) ; Strip off predicate name
  (if (eq (car pp) negation-symbol) ; Strip off negation symbol if it is there
      (setq pp (cdr pp)))
  (if (listp (car pp)) ; If symbol, return, otherwise nil
      nil
    (car pp))
  )

; Return first predicate argument missing from pattern, or nil if none are missing
(defun argument-missing (pattern predicate)
  (let (missing
        (pattern-arguments (cdr pattern))
        )
    ; Strip off negation and alpha variable if they are there
    (unless (listp (car pattern-arguments))
      (setq pattern-arguments (cdr pattern-arguments)))
    (unless (listp (car pattern-arguments))
      (setq pattern-arguments (cdr pattern-arguments)))
    (dolist (arg (predicate-arguments predicate))
      (unless (member (argument-name arg) pattern-arguments :key #'car)
        (return (setq missing (argument-name arg)))
        )
      )
    missing)
  )

; Compile a pattern of a conditional.
; g is a graph, pp is the predicate pattern,
; later-vars are variables used later in the conditional, c is the conditional,
; and out and in specify whether the links out from the alpha to the beta or in
; from the beta to the alpha should be active
(defun compile-predicate-pattern (pattern-type pp later-vars create-beta-memory next-action c out in)
  (let ((previous-beta-memory (if (conditional-shared-action-beta c)
                                  (conditional-shared-action-beta c) ; Shared beta memory to which to connect all action beta factors
                                (conditional-last-memory c))) ; Last beta memory (or alpha if not beta)
        alpha-results
        alpha-memory
        predicate
        offset-vn;Volkan
;        missing-argument
        )
    ; Check if predicate is defined
    (setq predicate (predicate-from-name (car pp) t))
    (unless predicate
      (error "Undefined predicate ~S in pattern ~S of conditional ~S." (car pp) pp (conditional-name c))
      )
    ; Check if there are any predicate arguments not included in pattern (return first missing one or nil)
;    (setq missing-argument (argument-missing pp predicate))
;    (when missing-argument
;      (error "Predicate argument ~S missing from pattern ~S in conditional ~S."
;             missing-argument pp (conditional-name c))
;      )
    ; List conditional in predicate when learn for predicate and pattern is a condact
    (when (and (eq pattern-type 'condacts)
               (predicate-function predicate)
               (not (equal (predicate-learning-rate predicate) 0))
               )
      (setf (predicate-condact-conditionals predicate) (cons c (predicate-condact-conditionals predicate)))
      )         
    ; Create an alpha network (and memory)
    (multiple-value-setq (alpha-results offset-vn) (create-alpha-network pp c in out))
    (setq alpha-memory (car alpha-results))
    (when (and out (not in))
      (setf (conditional-last-condition-memory c) alpha-memory)
      )
    ; Add a beta factor and memory
    (if previous-beta-memory
        (create-beta-network c later-vars alpha-memory previous-beta-memory create-beta-memory next-action out in (cdr alpha-results) offset-vn)
      (setf (conditional-last-memory c) alpha-memory)
      )
    )
  )


; Compile a set of patterns from conditional c into a graph
; The set will be the conditions, the condacts or the actions
(defun compile-patterns (c pattern-type patterns later-vars later-linear-patterns out in)
  (do ((patterns-l patterns (cdr patterns-l))
       (lvs-l (cdr later-vars) (cdr lvs-l)))
      ((null patterns-l) nil)
    (compile-predicate-pattern pattern-type
                               (car patterns-l)
                               (car lvs-l)
                               (or later-linear-patterns ; Create a new beta memory if there are condacts or functions still yet to come or
                                   (and (not (eq pattern-type 'actions)) ; Not an action and
                                        (or (cdr patterns-l) ; Later linear patterns remaining in this class or
                                            (conditional-actions c) ; There are actions (so last linear before actions gets a beta memory)
                                            )
                                        )
                                   )
                               (and (not (eq pattern-type 'actions)) ; Next pattern is first action if these aren't actions and
                                    (not later-linear-patterns) ; There are no further classes of linear patterns and
                                    (null (cdr patterns-l)) ; And there is nothing left in the current class and
                                    (conditional-actions c) ; There are actions in the conditional
                                    )
                               c out in)      
    )
  )

; Verify predicate name and argument names in a pattern
(defun verify-predicate-pattern (pattern ptype c-name)
  (unless (consp pattern)
    (error "Non-list pattern ~S found as a ~A in conditional ~S" pattern ptype c-name)
    )
  (let ((pred (find (car pattern) (graph-predicates cg) :key #'predicate-name)))
    (unless pred
      (error "Unknown predicate ~S in ~A ~S in conditional ~S" (car pattern) ptype pattern c-name)
      )
    (dolist (element (cdr pattern))
      (when (consp element)
        (let ((argument (find (car element) (predicate-arguments pred) :key #'argument-name)))
          (unless argument
            (error "Unknown argument ~S for predicate ~S in ~A ~S in conditional ~S" (car element) (car pattern) ptype pattern c-name)
            )
          )
        )
      )
    )
  )

; Verify predicate pattern names and arguments
(defun verify-patterns (patterns ptype c-name)
  (dolist (pattern patterns)
    (verify-predicate-pattern pattern ptype c-name)
    )
  )

; Create a list of variable names in a predicate pattern
; If types is true, provide the types of the variables as well
(defun pp-variable-names (pp &optional types)
  (let (vs rest aff pos val type nev)
    (dolist (element pp)
      (when (and (listp element) ; Ignores predicate name and negation at front of pattern
                 (variable-element (element-content element)))
        (when types
          (setq type (type-from-predicate-argument (element-argument-name element) (predicate-from-name (car pp))))
          )
        (setq vs (adjoin (if types (list (car (element-content element)) type) (car (element-content element))) vs))
        (setq rest (cdr (element-content element)))
        (when (and rest (consp (setq aff (car rest))) (symbolp (car aff))) ; An affine transform
          (setq pos (position ':from aff))
          (when (and pos (setq val (nth (+ pos 1) aff)) (symbolp val)) ; There is a variable name after :from
            (setq vs (adjoin (if types (list val type) val) vs))
            )
          (setq pos (position ':offset aff))
          (when (and pos (setq val (nth (+ pos 1) aff)) (symbolp val)) ; There is a variable name after :offset
            (setq vs (adjoin (if types (list val type) val) vs))
            )
          )
        (when (and rest (not-equal-listp (car rest)))
          (setq nev (cadar rest))
          (setq vs (adjoin (if types (list nev type) nev) vs))
            )
        )
      )
      (reverse vs))
  )

; Create a list of the variable names in a list of patterns
(defun all-variable-names (patterns)
  (let (pvs)
    (dolist (pattern patterns)
      (setq pvs (union (pp-variable-names pattern) pvs))
      )
    pvs)
  )

; Determine list of lists of which variable names come after conditions, condacts and actions
(defun later-variable-names (patterns later &optional action)
  (if action
      (mapcar #'pp-variable-names patterns)
    (let ((cs-later (list later)))
      (dolist (rc (reverse patterns))
        (setq cs-later (cons (union (pp-variable-names rc) (car cs-later)) cs-later))
        )
      cs-later)
    )
  )

; If not explicitly specifying function-variable-names
; determine the variable names, and their proper order, for use in a function
; This is all of the variables in the condacts that aren't in the conditions
; Logically this could be done by set-difference if it would guarantee order preservation
(defun function-variable-names (c)
  (unless specify-function-variable-names
    (let (condition-var-names ; List of names of variables in the conditions
          function-var-names ; List of names of variables to be included in the function
          e-content ; Content of the current element
          )
      (setq condition-var-names (all-variable-names (conditional-conditions c)))
      (dolist (condact (conditional-condacts c)) ; Cycle through condacts in order
        (dolist (element condact) ; Cycle through elements of condact (including non-element stuff at front)
          (when (listp element) ; Check if actually an element or prefix information
            (setq e-content (element-content element))
            (when (variable-element e-content) ; Check if element is a variable
              (when (and ; Variable not already seen in conditions or function variables
                     (not (member (car e-content) condition-var-names))
                     (not (member (car e-content) function-var-names)))
                (setq function-var-names (cons (car e-content) function-var-names))))))
        )
      (setf (conditional-function-variable-names c) (reverse function-var-names))
      )
    )
  )

; Maximum volume of a PLM (assuming discrete/unitary variables) given a set of variables
(defun max-discrete-volume (variables)
  (let ((volume 1))
    (dotimes (i (length variables))
      (setq volume (* (stype-span (svariable-type (aref variables i))) volume))
      )
    volume)
  )

; Return conditional function node give name of conditional
(defun conditional-function-node-name-from-conditional-name (cname)
  (node-name (conditional-function-node (conditional-from-name cname)))
  )
(defun cfn (cname) (conditional-function-node-name-from-conditional-name cname))

; Find the type of a variable from a conditional function as used in an action
; Needed when variable isn't used in conditions or condacts
(defun find-action-variable-type (vname c)
  (let (element argname pred arg type)
    (dolist (action (conditional-actions c))
      (when (member vname (pp-variable-names action))
        (setq element (find vname action
                            :key #'(lambda (x) (if (and (listp x) (cdr x) (listp (cadr x)))
                                                   (caadr x)
                                                 nil))))
        (when element
          (setq argname (car element))
          (setq pred (predicate-from-name (car action)))
          (when pred
            (setq arg (assoc argname (predicate-arguments pred)))
            (when arg
              (setq type (type-from-name (cadr arg)))
              (return type)
              )
            )
          )
        )
      )
    type)
  )

; Copy a function from another conditional, updating it with the current variables
; Assumes variable types and ordering corresponds
; Reuses regions rather than copying them
(defun copy-conditional-function (f vs)
  (make-plm :rank (plm-rank f)
            :slices (plm-slices f)
            :active (plm-active f)
            :variables vs
            :array (plm-array f)
            :piecewise-constant (plm-piecewise-constant f)
            )
  )

; Union a (possibly empty) list of symbols
(defun union-lists (ls)
  (case (length ls)
    (0 nil)
    (1 (copy-seq (car ls)))
    (t (reduce #'union ls))
    )
  )

; Compile a conditional function
(defun compile-conditional-function (c)
  (let (p-memory ; Previous memory
        p-vars ; Variables in previous memory
        p-var-names ; Names of variables in previous memory
        x-var-names ; Names of function variables
        x-vars ; Function variables
        xn ; Variable index
        x-function ; Function for function factor
        x-neighbors ; Variable nodes adjacent to function factor
        x-node ; Function factor node
        v-node ; Variable node attached to function factor
        (c-fun (conditional-function c)) ; The specified conditional function
        row-major ; Whether should use special row-major initialization (an all discrete function with a full set of values)
        (c-norm (conditional-normal c)) ; Variable name for the dimension over which to normalize, if any
        (normalize-function t) ; Set the default to normalize the function if there is a normalization variables
        p-index type
        other-c ; Conditional from which function is to be copied
        other-fn ; CFF with function to copy
        message-to-function ; Whether to activate links towards function
        )
    (when (null c-fun) (setq c-fun 1)) ; Default to a constant one if no function specified
    ; Check for special case of a row-major function
    (when (and (consp c-fun) (eq (car c-fun) 'row-major))
      (setq c-fun (cdr c-fun))
      (setq row-major t)
      )
    ; List of variable names to be used in function, in order they appear (or specified variable names)
    (setq x-var-names (conditional-function-variable-names c))
    ; List of variables to be used in function
    (setq x-vars (init-vector (length x-var-names)))
    (setq xn 0)
    ; Beta memory that connects the function to the rest of the graph
    (setq v-node (init-variable-node (concat-symbols (cons (conditional-name c)
                                                  (cons 'cfv x-var-names)) t)
                                     'function nil nil x-vars nil (if (conditional-condacts c) 'condact 'action)))
    (setq p-memory (conditional-last-memory c)) ; Memory to which function subgraph is to be attached
    (if p-memory ; There is a preceding condition or condact
        (progn
          (setq p-vars (node-variables p-memory)) ; Variables in previous memory
          (setq p-var-names (variable-names p-vars)) ; Names of variables in previous memory
          )
      (setf (conditional-last-memory c) v-node))
    (dolist (xvn x-var-names)
      (when p-memory
        (setq p-index (position xvn p-var-names)) ; Index of variable in previous, if it is there
        )
      (if p-index
          (setf (aref x-vars xn) (aref p-vars p-index)) ; Copy variable from previous
        (progn
          (setq type (find-action-variable-type xvn c))
          (if type
              (setf (aref x-vars xn) (make-svariable :name xvn :type type))
            (error "Variable ~S in function for conditional ~S does not appear in any pattern." xvn (conditional-name c)))
          ))
      (setq xn (1+ xn))
      )
    ; Determine which nodes are neighbors of the function factor node
    (setq x-neighbors (list v-node))
    (setq message-to-function (and feedback-to-functions
                                   learn-via-gradient-descent
                                   (not (equal (conditional-learning-rate c) 0))
                                   (not (member (conditional-name c) do-not-learn))
                                   ))
    ; Create a function factor node
    (setq x-node
          (init-factor-node (concat-symbols (cons (conditional-name c)
                                                  (cons 'CFF x-var-names)) t)
                            'function
                            x-vars
                            x-neighbors
                            t message-to-function t nil nil c))
    (setf (node-assumption x-node) t)
    (setf (node-function-name x-node) (conditional-name c))
    (setf (conditional-function-node c) x-node)
    ; If the conditional function is a number, use it as a constant PLM
    ; If the conditional function is a symbol, assume it is the name of another conditional whose function is to be shared
    ; Otherwise assume it specifies a full PLM
    (cond ((numberp c-fun)
           (setq x-function (init-plm x-vars c-fun 0 (init-vector (length x-vars) t)))
           (setf (plm-piecewise-constant x-function) t)
           )
          ((symbolp c-fun) ; Copy function from another conditional
           (setq normalize-function nil) ; Don't need to normalize function, as already normalized
           (setq other-c (conditional-from-name c-fun))
           (unless other-c
             (error "No conditional named ~S, as specified in the :FUNCTION for conditional ~S" c-fun (conditional-name c))
             )
           (unless (equal (conditional-function-variable-names c) (conditional-function-variable-names other-c))
             (error "Function variable names ~S in conditional ~S not the same as ~S in conditional to which function is to be tied ~S"
                    (conditional-function-variable-names c) (conditional-name c) (conditional-function-variable-names other-c) (conditional-name other-c))
             )
           (unless (equal (conditional-normal c) (conditional-normal other-c))
             (error "Function normal ~S in conditional ~S not the same as ~S in conditional to which function is to be tied ~S"
                    (conditional-normal c) (conditional-name c) (conditional-normal other-c) (conditional-name other-c))
             )
           (setq other-fn (conditional-function-node other-c))
           (unless other-fn
             (error "No CFF for conditional ~S as needed given specification in conditional ~S" c-norm (conditional-name c))
             )
                             ; Add shared function node, and all nodes it shares with to list of shared function nodes
           (setf (node-shared-functions x-node) (cons other-fn (node-shared-functions other-fn)))
                             ; Add current node to list of shared function nodes for node shared and everyone it shares with
           (setf (node-shared-functions other-fn) (cons x-node (node-shared-functions other-fn)))
           (dolist (cousin (node-shared-functions other-fn))
             (setf (node-shared-functions cousin) (cons x-node (node-shared-functions cousin)))
             )
           (setq x-function (copy-conditional-function (node-function other-fn) x-vars))
           )
          (row-major
           (setq x-function (rml-plm c-fun x-vars t))
           (setf (plm-piecewise-constant x-function) t)
           )
          (t
           (setq x-function (cpt-function-array-plm x-var-names c-fun x-vars
                                                    (if (conditional-function-default c) (conditional-function-default c) function-default) t))
           (setf (plm-piecewise-constant x-function) (notany #'(lambda (x) (consp (car x))) c-fun)) ; No linear regions in specification
           )
          )
    (setf (node-restriction x-node) (transform-plm #'boolean-function x-function)) ; Function that constrains learning to 0s where initial function is 0
    (setf (node-normal x-node) nil)
    (when c-norm
      (setf (node-normal x-node) (if (symbolp c-norm)
                                     (position c-norm x-var-names)
                                   (mapcar #'(lambda (cx) (position cx x-var-names)) c-norm)))
      (when normalize-function ; Avoid renormalizing when it is a function copied from another conditional
        (setq x-function (normalize-plm x-function (node-normal x-node) t))
        )
      )
    (setf (node-function x-node) x-function)
    (setf (node-conditional-name x-node) (conditional-name c))
    (setf (node-changes x-node) 1) ; Initialize number of changes made to fucntion via learning to 0
    ; Set the node's learning rate
    (if (conditional-learning-rate c) ; Use value specified in conditional if specified
        (progn
          (unless (numberp (conditional-learning-rate c))
            (error "Learning rate specified for conditional ~S is not a number" (conditional-name c))
            )
          (if (not (or (conditional-normal c) learn-no-normal (e= (conditional-learning-rate c) 0)))
              (error "Learning rate specified for a conditional with no normal variable when learn-no-normal not T: ~S" (conditional-name c))
            (setf (node-learning-rate x-node) (conditional-learning-rate c)))
          )
      (when adaptive-learning-rate ; Otherwise use the adaptive rate if active
        (setf (node-learning-rate x-node) (/ 0.3 (function-span x-node)))
        ))
    ; Set the node's smoothing parameter
    (when (conditional-smoothing-parameter c) ; Use value specified in conditional if specified
      (unless (numberp (conditional-smoothing-parameter c))
        (error "Learning rate specified for conditional ~S is not a number" (conditional-name c))
        )
      (setf (node-smoothing-parameter x-node) (conditional-smoothing-parameter c))
      )
    ; If there is a previous memory, connect function subgraph to it via a beta factor
    (when p-memory
      (create-beta-network c
                           (union-lists (conditional-action-later-variable-names c))
                           v-node p-memory (conditional-actions c) (conditional-actions c) t message-to-function)
      )
    x-node)
  )

; Statically estimate volume for a type
(defun v-type (type)
  (if (stype-constants type) (stype-span type) ; Number of constants for symbolic
    (if (stype-discrete type) 10 ; 10 for discrete numeric
      100)) ; 100 for continuous numeric
  )

; Determine (approximate) volume of message given variables
; Each v is a variable name followed by its type
(defun vs-volume (vs)
  (let ((vol 1))
    (dolist (v vs)
      (setq vol (* vol (v-type (cadr v))))
      )
    vol)
  )

; Reorder conditions to reduce computation
(defun reorder-conditions (conditions later-vars)
  (let* ((cv (coerce conditions 'vector))
         (cvl (length cv)) ; Number of conditions
         rcl ; Reordered conditions
         bestc ; Best condition so far
         bestv ; Volume for best condition so far
         bestvs-sofar ; Variables after beta for best condition so far
         c-vars ; Variables in conditions
         count
         vs-in-cs ; alist for number of remaining conditions variables are in
         vs-sofar ; Variables included so far
         vs-after-beta ; Variables after beta
         cvol ; Volume of message after beta
         (vs (init-vector cvl)) ; Variables in each condition (with their types)
         )
    ; Determine which variables are in each condition (and overall list of variables)
    (dotimes (i cvl)
      (setf (aref vs i) (pp-variable-names (aref cv i) t))
      (setq c-vars (union (aref vs i) c-vars :key #'car))
      )
    ; Determine how many conditions each variable is in
    (dolist (cv c-vars)
      (setq count 0)
      (dotimes (i cvl)
        (when (member (car cv) (aref vs i) :key #'car)
          (setq count (1+ count))
          )
        )
      (setq vs-in-cs (acons (car cv) count vs-in-cs))
      )
    ; Generate the sequence of reordered conditions
    (dotimes (i cvl)
      (setq bestc nil)
      (setq bestv 1E30)
      ; Check each remaining condition to see if it is best at this point
      (dotimes (j cvl)
        (when (aref cv j) ; Condition not yet added in
          (setq vs-after-beta (union (aref vs j) vs-sofar :key #'car))
          ; Eliminate any variables that can be summarized out
          (dolist (v (aref vs j))
            (when (and (not (member (car v) later-vars))
                       (= (cdr (assoc (car v) vs-in-cs)) 1)
                       )
              (setq vs-after-beta (remove (car v) vs-after-beta :key #'car))
              )
            )
          ; Check if current condition better than best so far
          (setq cvol (vs-volume vs-after-beta))
          (when (< cvol bestv)
            (setq bestc j)
            (setq bestv cvol)
            (setq bestvs-sofar vs-after-beta)
            )
          )
        )
      (when (not bestc) ; No condition has a volume less than 1E30
        (setq bestc (position-if #'(lambda (x) x) cv))
        )
      (setq rcl (cons (aref cv bestc) rcl))
      (setf (aref cv bestc) nil)
      (setq vs-sofar bestvs-sofar)
      ; Reduce the number of remaining conditions for variables in best condition
      (dolist (v (aref vs bestc))
        (setq vs-in-cs (acons (car v) (1- (cdr (assoc (car v) vs-in-cs))) vs-in-cs))
        )
      )
    (reverse rcl))
  )

; Compile conditional c into a graph
(defun compile-conditional (c)
  ; Signal an error if try to redefine a conditional (reuse a conditional name)
  ; Can't presently delete a conditional from graph, so can't redefine
  ; Instead need to reload entire graph at this point
  (when (conditional-from-name (conditional-name c))
    (error "Conditional ~S already exists in graph." (conditional-name c))
    )
  (let ((conditions (conditional-conditions c)))
    ; Verify predicate and argument names in patterns
    (verify-patterns conditions "condition" (conditional-name c))
    (verify-patterns (conditional-condacts c) "condact" (conditional-name c))
    (verify-patterns (conditional-actions c) "action" (conditional-name c))
    ; Determine list of conditional's function variable names if not specified explicitly
    (function-variable-names c)
    ; List of conditional's action variables
    (setf (conditional-action-later-variable-names c)
          (later-variable-names (conditional-actions c) nil t)
          )
    ; Variables in actions and function which must be kept around during condacts
    (setf (conditional-condact-later-variable-names c)
          (later-variable-names (conditional-condacts c)
                                (if (conditional-action-later-variable-names c)
                                    (union (conditional-function-variable-names c) ; Include variables from function
                                           (reduce #'union (conditional-action-later-variable-names c))) ; Include variables from all actions
                                  (conditional-function-variable-names c) ; Just variables from function
                                  )
                                )
          )
    (when automatically-reorder-conditions
      (when (> (length conditions) 1)
        (setq conditions (reorder-conditions conditions (car (conditional-condact-later-variable-names c))))
        )
      (setf (conditional-reordered-conditions c) conditions)
      )
    ; Variables that must be kept around through conditions
    (setf (conditional-condition-later-variable-names c)
          (later-variable-names conditions (car (conditional-condact-later-variable-names c))))
    ; Record the conditional on the graph
    (setf (graph-conditionals cg) (cons c (graph-conditionals cg)))
    ; Compile the conditions and condacts with their own later-variables
    (compile-patterns c 'conditions conditions (conditional-condition-later-variable-names c)
                      (or (conditional-condacts c) (conditional-function c))
                      t nil)
    (compile-patterns c 'condacts (conditional-condacts c) (conditional-condact-later-variable-names c)
                      (conditional-function c)
                      t t)
    ; Create function factor if a conditional function is specified
    (when (conditional-function c)
      (compile-conditional-function c)
      )
    ; Compile actions
    (compile-patterns c 'actions (conditional-actions c) (conditional-action-later-variable-names c) nil nil t)
    )
  )

;---------------------
; HIGH LEVEL
;---------------------
; Vector operations
; -----------------

; Compute the pointwise sum of two vectors
(defun vector-sum (v1 v2 &optional piecewise-constant)
  (if piecewise-constant
      (+ v1 v2)
    (map 'vector #'+ v1 v2)))

; Compute the pointwise difference of two vectors
(defun vector-difference (v1 v2 &optional piecewise-constant)
  (if piecewise-constant
      (- v1 v2)
    (map 'vector #'- v1 v2)))

; Divide vector by number
(defun vector-divide (vin n &optional piecewise-constant)
  (if piecewise-constant
      (/ vin n)
    (let ((vout (init-vector (length vin))))
      (dotimes (i (length vin))
        (setf (aref vout i) (/ (aref vin i) n))
        )
      vout))
  )

; Divide vector by number (but if n is 0, yield 0)
(defun vector-divide-0 (vin n &optional piecewise-constant)
  (if piecewise-constant
      (if (zerop n)
          0
        (/ vin n))
    (let ((vout (init-vector (length vin) 0)))
      (when (not (zerop n))
        (dotimes (i (length vin))
          (setf (aref vout i) (/ (aref vin i) n))
          )
        )
      vout))
  )

; Compute whether two vectors are e=
(defun vector-e= (v1 v2 &optional piecewise-constant)
  (if piecewise-constant
      (e= v1 v2)
    (and (= (length v1) (length v2))
         (every #'e= v1 v2)
         ))
  )

; Compute pointwise OR of two vectors
(defun vector-or (v1 v2 &optional piecewise-constant)
  (if piecewise-constant
      (or v1 v2)
    (map 'vector #'(lambda (a b) (or a b)) v1 v2))
  )

; Compute inverse of a vector
(defun vector-not (v &optional piecewise-constant)
  (if piecewise-constant
      (not v)
    (map 'vector #'(lambda (x) (not x)) v))
  )

; Determine if a vector is all T
(defun vector-all-t (v &optional piecewise-constant)
  (if piecewise-constant
      v
    (every #'(lambda (x) x) v))
  )

; Create a vector of length 'dimension' of uniform 'value'
; Value defaults to nil if not given
(defun init-vector (dimension &optional value)
  (make-array (list dimension) :initial-element value))

; Reduce vector with specified value
(defun reduce-vector (vector value)
  (let* ((vr (length vector))
         nvector
         flag-pos
         )
    (dotimes (i vr)
      (when (eq (aref vector i) value) (setf flag-pos i) (return))
      )
    (when flag-pos
      (setf nvector (init-vector (- vr 1)))
      (dotimes (i flag-pos)
        (setf (aref nvector i) (aref vector i))
        )
      (dotimes (i (- vr (+ flag-pos 1)))
        (setf (aref nvector (+ i flag-pos)) (aref vector (+ 1 (+ i flag-pos))))
        )
      )
    (if flag-pos nvector vector)    
    )
  )

; Init PLM
; --------

; Create a dimension for the origin element
(defun make-spanning-dimension (type is weights)
  (make-dimension :min-slice (if is
                                 (find-slice-at-location (stype-min type) is)
                               (make-slice :location (stype-min type))
                               )
                  :max-slice (if is
                                 (find-slice-at-location (stype-max type) is)
                               (make-slice :location (stype-max type))
                               )
                  :weight weights :discrete (stype-discrete type))
  )

; Create a dimension array for an origin node of a PLM
(defun make-spanning-dimensions (variables isv weights)
  (let* ((rank (length variables))
         (ds (init-vector rank))
         var-s)
    (dotimes (d rank)
      (setq var-s (aref variables d))
      ; When more than one variable mapped to a dimension, just take first (types are all the same)
      (when (listp var-s) (setq var-s (car var-s)))
      (setf (aref ds d) (make-spanning-dimension (svariable-type var-s)
                                                 (if isv (aref isv d) nil)
                                                 weights)))
    ds)
  )

; Create a region covering entire space
; isv is an initial slice vector
(defun make-spanning-region (variables isv constant weights)
  (make-region :constant constant
               :dimensions (make-spanning-dimensions variables isv weights)
               :maximals (init-vector (length variables))
               )
  )

; Create a region, as specified by a list of dimensions, that has a discrete value
; isv is the initial slice vector
; If constant is a number, it becomes the constant value for the region.
; If constant is a list, the first element is the constant and the others are the weights.
; dps is a list of sublists.  The first element of each sublist is a dimension number.
; The rest of the sublist is a min-slice and a max-slice for the dimension.
; Weights is the weight to use for all dimensions not included in dps
(defun make-discrete-region (dps variables isv constant weights)
  (let ((cdrds (make-spanning-dimensions variables isv weights))
        (c constant) ; region constant
        ws ; dimension weights
        )
    (when (listp constant)
      (setq c (car constant))
      (setq ws (cdr constant))
      )
    (dolist (dp dps)
      (setf (dimension-min-slice (aref cdrds (car dp))) (cadr dp))
      (setf (dimension-max-slice (aref cdrds (car dp))) (caddr dp))
      (setf (dimension-weight (aref cdrds (car dp))) (if ws (nth (car dp) ws) 0))
      )
    (make-region :constant c :dimensions cdrds :maximals (init-vector (length variables)))
    )
  )

; Make a dps list (a list of sublists with dimension number, min-slice and max-slice) for dimensions/variables
(defun make-dps-list (ds isv)
  (let (dps)
    (dolist (d ds)
      (setq dps (cons (list d (car (aref isv d)) (cadr (aref isv d))) dps))
      )
    )
  )

; Create a vector of initial slice lists from a vector of variables
(defun initial-slice-vector (variables)
  (let* ((rank (length variables))
         (sv (init-vector rank))
         v-type)
    (dotimes (i rank)
      (setq v-type (svariable-type (if (listp (aref variables i))
                                      (car (aref variables i)) ; Handle case where multiple variables mapped
                                    (aref variables i))))
      (setf (aref sv i) (list (make-slice :location (stype-min v-type) :index 0)
                              (make-slice :location (stype-max v-type) :index 1)))
      )
    sv)
  )

; Create an empty PLM array
(defun make-region-array (ds)
  (make-array ds :element-type 'region)
  )

; Create a list of 1s
(defun list-of-constant (n constant)
  (make-sequence 'list n :initial-element constant)
  )

; Make a PLM with a single region that is a constant-discrete-region
; ds is just a list of dimension numbers that should be defined
(defun make-constant-discrete-plm (ds variables constant weights)
  (let ((rank (length variables))
        origin isv p)
    (setq isv (initial-slice-vector variables))
    (setq origin (make-discrete-region (make-dps-list ds isv) variables isv constant weights))
    (setq p (make-plm :rank rank :active (init-vector rank t) :variables variables
                      :array (make-region-array (list-of-constant rank 1)) :slices isv
                      :piecewise-constant (= weights 0)
                      ))
    (setf (row-major-aref (plm-array p) 0) origin)
    p)
  )

; Origin region of a region array
(defun region-array-origin (rarray)
  (row-major-aref rarray 0)
  )

; Origin region of a PLM
(defun plm-origin (p)
  (region-array-origin (plm-array p))
  )

; Create a new PLM
(defun init-plm (variables constant weights active)
  (let* ((rank (length variables))
        (isv (initial-slice-vector variables))
        (origin (make-spanning-region variables isv constant weights))
        plm
        )
    (setq plm
          (make-plm :rank rank :variables variables
                    :active active
                    :array (make-region-array (list-of-constant rank 1))
                    :slices isv
                    :piecewise-constant (= weights 0)
                    )
          )
    (setf (row-major-aref (plm-array plm) 0) origin)
    plm)
  )

; Determine number of regions in PLM
(defun plm-size (p)
  (array-total-size (plm-array p))
  )

; Reduce all elements of a vector by 1
(defun vector-1 (v)
  (map 'vector #'1- v)
  )

; Create a new PLM with slices
; Assume slices are already copied if necessary
(defun init-plm-with-slices (variables constant weights active slices)
  (let* ((rank (length variables))
         sizev ; Vector of dimension sizes
         sizev-1 ; Sizev reduced by 1 in each location
         rarray ; New region array
         size ; Number of regions in array
         ds ; Dimension vector
         svv ; Vector of slice vectors
         sv ; A slice vector
         iv ; Index vector
         )
    (cond ((zerop rank)
           (setq rarray (make-array nil))
           (setf (aref rarray) (make-region :constant constant
                                            :maximals (vector)
                                            :dimensions (vector)))
           )
          (t
           (sort-slice-list-array slices) ; Sort the slices
           (setq slices (delete-duplicates-slice-list-array slices))
           (index-slice-list-array slices) ; Index the slices
           (setq sizev (dimension-sizes-from-slices slices))
           (setq sizev-1 (vector-1 sizev))
           (setq rarray (make-region-array (coerce sizev 'list)))
           (setq size (array-total-size rarray))
           (setq svv (slice-vector-vector slices)) ; Create a vector of slice vectors
           (setq iv (init-vector rank 0))
           ; Create initial regions in all locations
           (dotimes (i size)
             (setq ds (init-vector rank))
             (setf (row-major-aref rarray i) (make-region :constant constant
                                                          :maximals (init-vector rank)
                                                          :dimensions ds))
             (dotimes (d rank)
               (setq sv (aref svv d))
               (setf (aref ds d) (make-dimension :weight weights
                                                 :discrete (stype-discrete (svariable-type (aref variables d)))
                                                 :min-slice (aref sv (aref iv d))
                                                 :max-slice (aref sv (1+ (aref iv d)))))
               )
             (setq iv (next-index-vector iv sizev-1 rank))
             )
           )
          )
    (make-plm :rank rank :variables variables
              :active active
              :array rarray
              :slices slices
              )
    )
  )

; Init one plm from another (but 0 constants and weights)
(defun init-plm-from-plm (p)
  (init-plm-with-slices (copy-seq (plm-variables p)) 0 0 (copy-seq (plm-active p)) (copy-slice-vector (plm-slices p)))
  )

; Regions
; -------

; Access minimum of a region's dimension
(defun region-min-slice (r d)
  (dimension-min-slice (dimension r d)))
(defun region-min (r d)
  (slice-location (region-min-slice r d)))

; Access maximum of a region's dimension
(defun region-max-slice (r d)
  (dimension-max-slice (dimension r d)))
(defun region-max (r d)
  (slice-location (region-max-slice r d)))

; Compute span of region along dimension
(defun region-span (r d)
  (- (region-max r d) (region-min r d))
  )

; Compute entire volume of region
(defun region-volume (r)
  (reduce #'* (region-dimensions r) :key #'dimension-span)
  )

; Access weight of a region's dimension
(defun region-weight (r d)
  (dimension-weight (dimension r d)))

; Access discrete field of a region's dimension
(defun region-discrete (r d)
  (dimension-discrete (dimension r d)))

; Rank of a region
(defun region-rank (r)
  (length (region-dimensions r)))

; Rank of a node
(defun node-rank (n)
  (length (node-variables n)))

; Determine if one region is before another along some dimension
(defun beforep (r1 r2 d)
  (e<= (dimension-max (dimension r1 d)) (dimension-min (dimension r2 d)) t))

; Determine if two regions overlap along a dimension
(defun overlapp (r1 r2 d)
  (not (or (beforep r1 r2 d) (beforep r2 r1 d))))

; Determine if first dimension is within second (based on min & max)
(defun within-dimension (d1 d2)
  (and (e>= (dimension-min d1) (dimension-min d2) t)
       (e<= (dimension-max d1) (dimension-max d2) t)))

; Determine if first region is contained within the second
(defun within-region (r1 map r2)
  (dotimes (d (region-rank r2) t)
    (unless (within-dimension (dimension r1 (if map (aref (smap-vfactor map) d) d))
                                 (dimension r2 d))
      (return nil)
      )
    )
  )

; Determine if first region is contained within the second
(defun regions-overlap (r1 r2)
  (dotimes (d (region-rank r1) t)
    (unless (overlapp r1 r2 d)
      (return nil)
      )
    )
  )

; Determine extent of a dimension (number) of a region
(defun dimension-extent (d r)
  (- (region-max r d) (region-min r d)))

; Determine if two regions have equal extent along a dimension
(defun equal-extentp (r1 r2 d)
  (and (e= (region-min r1 d) (region-min r2 d) t)
       (e= (region-max r1 d) (region-max r2 d) t)))

; Determine if one region starts before another along some dimension
(defun start-beforep (r1 r2 d)
  (e< (dimension-min (dimension r1 d)) (dimension-min (dimension r2 d))))

; Determine if one region ends after another along some dimension
(defun end-afterp (r1 r2 d)
  (e> (dimension-max (dimension r1 d)) (dimension-max (dimension r2 d))))

; Maxima of the minima along a dimension of two regions
(defun max-region-mins (r1 r2 d)
  (max (region-min r1 d) (region-min r2 d)))

; Minima of the maxima along a dimension of two regions
(defun min-region-maxs (r1 r2 d)
  (min (region-max r1 d) (region-max r2 d)))

; Copy a function from one region to another
(defun copy-function (r1 r2)
  (setf (region-constant r2) (region-constant r1))
  (dotimes (d (region-rank r1))
    (setf (dimension-weight (dimension r2 d)) (dimension-weight (dimension r1 d)))
    )
  )

; Is a region's function constant?
(defun region-function-constantp (r)
  (not (find-if #'(lambda (x) (not (zerop x))) (region-dimensions r) :key #'dimension-weight))
  )

; Is a function constant?
(defun function-constantp (f)
  (not (find-if #'(lambda (x) (not (e= x 0))) f :start 1))
  )

; Copy a region structure, copying the dimensions in the process
(defun copy-region-full (r)
  (make-region :constant (region-constant r)
               :dimensions (copy-dimensions (region-dimensions r))
               :maximals (copy-seq (region-maximals r))
               :evidence (region-evidence r)
               :exponential (region-exponential r)
               )
  )

; Slices
; ------

; Determine if two slices are equal
(defun slice= (x y)
  (e= (slice-location x) (slice-location y) t)
  )

; Determine if a slice is a member of a list (up to epsilon equality)
(defun slice-member (x l)
  (let ((xn (slice-location x))
        existing)
    (dolist (i l nil)
      (when (e= xn (slice-location i) t) ; Do an absolute comparison
        (return (setq existing i))
        )
      )
    existing)
  )

; Remove a slice from a list if it is a member (up to epsilon equality)
(defun slice-remove (x l)
  (let ((xn (slice-location x))
        newl)
    (dolist (i l (reverse newl))
      (if (e= xn (slice-location i) t)
          newl
        (setq newl (cons i newl))))
    )
  )

; Epsilon arithemetic
; -------------------

; Are two numbers equal within epsilon
(defun e= (n1 n2 &optional absolute arousal)
  (if (and (numberp n1) (numberp n2))
      (or (= n1 n2)
          (if (and use-relative-epsilon (not absolute)) ; Relative comparison
              (let ((max (max n1 n2))
                    (min (min n1 n2))
                    )
                (<= (- max min) (* (if arousal (* arousal relative-epsilon)
                                     relative-epsilon)
                                   (if (< max 0) (- min) max)))
                )
            (< (abs (- n1 n2)) (if arousal (* arousal epsilon)
                                 epsilon))) ; Absolute comparison
          )
    (eq n1 n2)) ; When either isn't a number
  )

; Are two numbers equal within absolute epsilon
(defun ae= (n1 n2)
  (e= n1 n2 t)
  )

; Is the first number greater than or equal to the second within epsilon
(defun e>= (n1 n2 &optional absolute)
  (or (e= n1 n2 absolute) (> n1 n2))
;  (>= n1 (- n2 epsilon))
  )

; Is the first number less than or equal to the second within epsilon
(defun e<= (n1 n2 &optional absolute)
  (or (e= n1 n2 absolute) (< n1 n2))
;  (<= n1 (+ n2 epsilon))
  )

; Is the first number greater than the second within epsilon
(defun e> (n1 n2)
  (> n1 n2)
;  (> n1 (+ n2 epsilon))
  )

; Is the first number less than the second within epsilon
(defun e< (n1 n2)
  (< n1 n2)
;  (< n1 (- n2 epsilon))
  )

; Is a region variable (any non-zero weight)
(defun region-is-variable (r)
  (notevery #'(lambda (d) (e= (dimension-weight d) 0)) (region-dimensions r))
  )

; Is a region constant (no non-zero weight)
(defun region-is-constant (r)
  (every #'(lambda (d) (e= (dimension-weight d) 0)) (region-dimensions r))
  )

; Are all the regions in a PLM constant
(defun plm-is-constant (p)
  (let ((rarray (plm-array p))
        (constant t))
    (dotimes (i (array-total-size rarray))
      (unless (region-is-constant (row-major-aref rarray i))
        (setq constant nil)
        (return constant)
        )
      )
    constant)
  )

; Is a region empty
(defun region-e-empty (r &optional piecewise-constant)
  (and (e= (region-constant r) 0) (not (region-exponential r))
       (or piecewise-constant
           (region-is-constant r)
           )
       )
  )

; Make region-empty
(defun empty-region (r)
  (let ((ds (region-dimensions r)))
    (setf (region-constant r) 0)
    (dotimes (d (length ds))
      (setf (dimension-weight (aref ds d)) 0)
      )
    r)
  )

; Is a region full
(defun region-e-full (r &optional piecewise-constant)
  (and (e= (region-constant r) 1)
       (or piecewise-constant
           (region-is-constant r)
           )
       )
  )

; Is a region uniform (constant >0)
(defun region-e-uniform (r &optional piecewise-constant)
  (and (e> (region-constant r) 0)
       (or piecewise-constant
           (region-is-constant r)
           )
       )
  )

; Is a dimension inactive?
(defun dimension-active (p d)
  (aref (plm-active p) d)
  )
(defun dimension-inactive (p d)
  (not (dimension-active p d)))

; Get constant weight from function
(defun function-constant (f)
  (aref f 0)
  )

; Is a region exhaustive (covering extent of PLM)?
(defun region-exhaustive (r p)
  (let ((exhaustive t)
        (ds (region-dimensions r))
        type
        )
    (dotimes (d (region-rank r))
      (setq type (svariable-type (aref (plm-variables p) d)))
      (unless (and (e= (dimension-min (aref ds d)) (stype-min type) t)
                   (e= (dimension-max (aref ds d)) (stype-max type) t))
        (setq exhaustive nil)
        (return nil)))
    exhaustive)
  )

; Is a PLM empty (one region that is a constant 0)
(defun plm-empty (p)
  (let ((oregion (plm-origin p)))
    (and (region-e-empty oregion (plm-piecewise-constant p)) ; The origin region is empty
         (= (plm-size p) 1) ; Only one region
        )
    )
  )

; Is a PLM full (one region that is a constant 1)
(defun plm-full (p)
  (let ((oregion (plm-origin p)))
    (and (region-e-full oregion (plm-piecewise-constant p)) ; The origin region is a constant 1
         (= (plm-size p) 1) ; Only one region
        )
    )
  )

; Is a PLM uniform (one region that is a constant >0)
(defun plm-uniform (p)
  (and (region-e-uniform (plm-origin p) (plm-piecewise-constant p)) ; The origin region is a constant 1
       (= (plm-size p) 1) ; Only one region
       )
  )


; Check if two slices are equal
(defun slices-e= (s1 s2 &optional arousal)
  (e= (slice-location s1) (slice-location s2) t arousal)
  )

; Check if two slice lists are equal
(defun slice-lists-e= (sl1 sl2 &optional arousal)
  (let ((sl-equal (= (length sl1) (length sl2))))
    (do ((sc1 sl1 (cdr sc1))
         (sc2 sl2 (cdr sc2)))
        ((null sc1))
      (unless (slices-e= (car sc1) (car sc2))
        (return (setq sl-equal nil))
        )
      )
    sl-equal)
  )

; Check if the slice vectors for two PLMs are the same lengths along all dimensions
; Assumes PLMs are of same rank
(defun slice-vectors-equal-length (sv1 sv2)  
(let ((lengths-equal t))
    (dotimes (d (length sv1))
      (unless (= (length (aref sv1 d)) (length (aref sv2 d)))
        (return (setq lengths-equal nil))
        )
      )
    lengths-equal)
  )

; Check if the slice vectors for two PLMs are the same lengths along all dimensions
; Assumes PLMs are of the same rank
(defun slice-vectors-e= (sv1 sv2 &optional arousal)
  (let ((sv-equal (slice-vectors-equal-length sv1 sv2)))
    (when sv-equal
      (dotimes (d (length sv1))
        (unless (slice-lists-e= (aref sv1 d) (aref sv2 d))
          (return (setq sv-equal nil))
          )
        )
      )
    sv-equal)
  )

; Are two regions e=
; Assumes regions' slices are in the same places
(defun region-e= (r1 r2 &optional arousal piecewise-constant)
  (and (e= (region-constant r1) (region-constant r2) nil arousal)
       (or piecewise-constant
           (let ((we t))
             (dotimes (d (region-rank r1) t)
               (unless (e= (region-weight r1 d) (region-weight r2 d) nil arousal)
                 (return (setq we nil))
                 )
               )
             we)
           )
       )
  )

; Are two PLMs e=
(defun plm-e= (p1 p2 &optional arousal)
    (if (= (plm-rank p1) (plm-rank p2))
        (let ((rarray1 (plm-array p1))
              (rarray2 (plm-array p2))
              (pc (and (plm-piecewise-constant p1) (plm-piecewise-constant p2)))
              (sv1 (plm-slices p1))
              (sv2 (plm-slices p2))
              )
          (and
           (slice-vectors-e= sv1 sv2) ; Check that all slices are in the same places
           (let ((equality t))
             (dotimes (i (array-total-size rarray1))
               (when (not (region-e= (row-major-aref rarray1 i) (row-major-aref rarray2 i) arousal pc))
                 (setq equality nil)
                 (return nil)
                 )
               )
             equality)
           )
          )
      nil)
    )

; -----------------------------------------------------------
; Region function functions

; Create a weight vector for a region function from the dimensions
; Put the constant in the 0 position and shift everything else over one.
; rank determines how big the weight vector should be
; Mapv is a map vector (or nil) that determines how the dimensions need to be realigned
(defun region-weights (r rank mapv &optional piecewise-constant)
  (if piecewise-constant
      (region-constant r)
    (let ((vf (init-vector (+ rank 1) 0))
          index
          (dims (region-dimensions r))
          )
      (setf (aref vf 0) (region-constant r))
      (dotimes (d (region-rank r))
        (setq index (if mapv
                        (aref mapv d)
                      d))
        (setf (aref vf (+ index 1)) (dimension-weight (aref dims d)))
        )
      vf))
  )

; Assign weights to a region function based on a vector
; The constant is in the 0th position and everything else is shifted from there
(defun assign-function (v r &optional piecewise-constant)
  (if piecewise-constant
      (setf (region-constant r) v)
    (let ((ds (region-dimensions r)))
      (setf (region-constant r) (aref v 0))
      (dotimes (d (region-rank r))
        (setf (dimension-weight (aref ds d)) (aref v (+ d 1)))
        )
      r)
    )
  )

; Extract a function from a region
(defun extract-function (r &optional piecewise-constant)
  (if piecewise-constant
      (region-constant r)
    (let* ((ds (region-dimensions r))
           (rank (region-rank r))
           (fn (init-vector (+ rank 1) 0))
           )
      (setf (aref fn 0) (region-constant r))
      (dotimes (d rank)
        (setf (aref fn (+ d 1)) (dimension-weight (aref ds d)))
        )
      fn)
    )
  )

; -----------------------------------------------------------
; Remove slices/hyperplanes from a PLM when they are no longer needed

; Determine if a hyperplane slices (intersects with) a region
(defun region-sliced (r d x)
  (and (e>= (slice-location x) (region-min r d) t) (< (slice-location x) (region-max r d))))

; Determine if a slice follows min of region
; Changed to check for the actual slice rather than location
(defun slice-at-region-min (r d x)
  (eq x (region-min-slice r d)))

; Determine if a slice follows max of region
(defun slice-at-region-max (r d x)
  (if (eq x (region-max-slice r d))
      t
      (if (e= (slice-location x) (slice-location (region-max-slice r d)) t)
          (error "Region boundary has same location as slice (~S), but is not the same slice." (slice-location x)) ; Regions should use PLM slices directly
        nil)
      )
  )

; Determine if a hyperplane borders a region
(defun slice-borders-region (r d x)
  (or (slice-at-region-min r d x) (slice-at-region-max r d x)))

; Determine if two sets (unordered lists) are equal, with specifiable equality test
(defun sets-equal (l1 l2 test)
  (and (= (length l1) (length l2))
       (dolist (i1 l1 t)
         (unless (member i1 l2 :test test)
           (return nil)
           )
         )
       )
  )

; Determine if two regions are equal in terms of not requiring a slice between them
(defun regions-equal (r1 r2)
  (function-equal r1 r2) ; Functions equal
  )

; Compute weighted average of two functions
(defun average-function (f1 s1 f2 s2)
  (let* ((s (+ s1 s2))
         (w1 (/ s1 s))
         (w2 (/ s2 s))
         (rank (length f1))
         (nf (init-vector rank))
         )
    (unless (= rank (length f2))
      (error "Functions ~S and ~S not of same length in average-function!" f1 f2)
      )
    (dotimes (i (length f1))
      (setf (aref nf i) (+ (* w1 (aref f1 i)) (* w2 (aref f2 i))))
      )
    nf)
  )

; Define exclusive or
(defun xor (a b)
  (not (eq a b))
  )

; Compute average of two region functions along dimension d
(defun average-region-functions (r1 r2 d)
  (average-function (extract-function r1) (region-span r1 d)
                    (extract-function r2) (region-span r2 d))
  )

; Determine if a slice is the extreme along a dimension in direction
(defun slice-is-extreme (loc sls compare)
  (dolist (sl sls t)
    (when (apply compare (list (slice-location sl) loc))
      (return nil)
      )
    )
  )

; Determine extreme slice
(defun extreme-slice (sls compare)
  (let ((extreme (car sls)))
    (dolist (sl (cdr sls))
      (when (apply compare (list (slice-location sl) (slice-location extreme)))
        (setq extreme sl)
        )
      )
    extreme)
  )

; Remove unneeded slices from slice lists
; Copies slice lists but not slices
(defun remove-unneeded-slices-from-list (slices needed)
  (let* ((rank (length slices))
         (nslices (init-vector rank))
         ns i nd
         )
    (dotimes (d rank)
      (setq ns nil)
      (setq i 0)
      (setq nd (aref needed d))
      (dolist (s (aref slices d))
        (when (aref nd i)
          (push s ns)
          )
        (setq i (1+ i))
        )
      (setf (aref nslices d) (reverse ns))
      )
    nslices)
  )

; Return index vector for original region array from index vector from reduced (unneeded slices removed) region array
; Assumes new slice list array is already sorted
(defun original-from-new-index (nv nslices)
  (let* ((rank (length nv))
         (ov (init-vector rank))
         nsl ; Slice list
         )
    (dotimes (d rank)
      (setq nsl (aref nslices d))
      (setf (aref ov d) (slice-index (nth (aref nv d) nsl)))
      )
    ov)
  )

; Copy a vector of dimensions, but without links to neighboring regions
(defun copy-dimensions (ds)
  (let* ((rank (length ds))
         (nds (init-vector rank))
         )
    (dotimes (i rank)
      (setf (aref nds i) (copy-dimension (aref ds i)))
      )
    nds)
  )

; Copy a region without the links, but without links to neighboring regions
(defun copy-a-region (r)
  (make-region :constant (region-constant r)
               :dimensions (copy-dimensions (region-dimensions r))
               :maximals (copy-seq (region-maximals r))
               :evidence (region-evidence r)
               :exponential (region-exponential r)
               )
  )

; Row major index for region before (based on region's index vector) along dimension number d
(defun rmi-before-ri (iv d mults)
  (if (zerop (aref iv d))
      nil
    (- (row-major-index iv mults) (aref mults d)))
  )

; Row major index for region after (based on region's index vector) along dimension number d
(defun rmi-after-ri (iv d mults sizev)
  (if (= (aref iv d) (1- (aref sizev d)))
      nil
    (+ (row-major-index iv mults) (aref mults d)))
  )

; Update of an index vector for region after, along a dimension
(defun region-after-index-vector (iv d size-1)
  (let ((i (aref iv d)))
    (if (= i size-1)
        nil
      (progn
        (setf (aref iv d) (1+ i))
        iv))
    )
  )

; Update of an index list for region after, along a dimension
(defun region-after-index-list (il d size-1)
  (let ((i (nth d il)))
    (if (= i size-1)
        nil
      (progn
        (setf (nth d il) (1+ i))
        il))
    )
  )

; Update of an index list for region before, along a dimension
; Assumes that the region isn't at beginning of dimension d
(defun region-before-index-list-x (il d)
  (setf (nth d il) (1- (nth d il)))
  il
  )

; Update of an index list for region after, along a dimension
; Assumes that the region isn't at end of dimension d
(defun region-after-index-list-x (il d)
  (setf (nth d il) (1+ (nth d il)))
  il
  )

; Update of an index list for next region in row-major order
(defun next-index-list (il sizev-1 rank)
  (if (= rank 0)
      nil
    (let* ((rank-1 (1- rank))
           r-1-j ; rank - (j + 1)
           locl ; List starting at location
           )
      (dotimes (j rank) ; Cycle through all dimensions of index list
        (setq r-1-j (- rank-1 j)) ; Proceed from right rather than left
        (setq locl (nthcdr r-1-j il))
        (if (< (car locl) (aref sizev-1 r-1-j)) ; If index at position is in bounds
            (return) ; Done generating vector index for this row-major index
          (rplaca locl 0)) ; Else, zero index at position and move on to next index position
        )
      (rplaca locl (1+ (car locl))) ; Increment index at position
      il))
  )

; Update of an index list for next region in row-major order, skipping dimension d
(defun next-index-list-no-d (il sizev-1 rank d)
  (if (< rank 2) ; Only a next index if there are at least two dimensions of the original array
      nil
    (let* ((rank-1 (1- rank))
           r-1-j ; rank - (j + 1)
           locl ; Index list starting at location
           )
      (dotimes (j rank) ; Cycle through all dimensions of index list
          (setq r-1-j (- rank-1 j)) ; Proceed from right rather than left
          (unless (= r-1-j d)
            (setq locl (nthcdr r-1-j il))
            (if (< (car locl) (aref sizev-1 r-1-j)) ; If index at position is in bounds
                (return) ; Done generating vector index for this row-major index
              (rplaca locl 0)) ; Else, zero index at position and move on to next index position
            )
          )
      (rplaca locl (1+ (car locl))) ; Increment index at position
      il))
  )

; Update of an index vector for next region in row-major order
(defun next-index-vector (iv sizev-1 rank)
  (if (= rank 0)
      nil
    (let* ((rank-1 (1- rank))
           r-1-j ; rank - (j + 1)
           )
      (dotimes (j rank) ; Cycle through all dimensions of index list
        (setq r-1-j (- rank-1 j)) ; Proceed from right rather than left
        (if (< (aref iv r-1-j) (aref sizev-1 r-1-j)) ; If index at position is in bounds
            (return) ; Done generating vector index for this row-major index
          (setf (aref iv r-1-j) 0)) ; Else, zero index at position and move on to next index position
        )
      (setf (aref iv r-1-j) (1+ (aref iv r-1-j))) ; Increment index at position
      iv))
  )

; Find indices of a regions min-slices
(defun region-index-mins (r)
  (let* ((rank (region-rank r))
         (ims (init-vector rank)))
    (dotimes (i rank)
      (setf (aref ims i) (slice-index (region-min-slice r i)))
      )
    ims)
  )

; Similar to old form of region after
(defun region-after-old (r d rarray)
  (let* ((s (region-max-slice r d))
         (rmins (region-index-mins r)))
    (if s
        (progn
          (unless (slice-index s)
            (error "Attempt to find region after region via unidexed slice: ~S" s)
            )
          (setf (aref rmins d) (slice-index s))
          (element-from-index-vector rarray rmins)
          )
      nil)
    )
  )

; Create an initial vector of vectors for needed slices
(defun init-needed-vector (rank sizev value)
  (let ((needed (init-vector rank)) ; Vector of vectors for the needed slices
        nv ; One subvector of needed
        )
    ; Initialize needed vector of vectors to track which slices are needed
    (dotimes (d rank)
      (setq nv (init-vector (1+ (aref sizev d)) value)) ; There is one more slice per dimension than region
      ; Mark that extreme slices along each dimension are needed
      (setf (aref nv 0) t)
      (setf (aref nv (aref sizev d)) t)
      (setf (aref needed d) nv)
      )
    needed)
  )

; Determine if one slice is needed
(defun slice-needed (rarray si d rank size-d sizev-1)
  (let (ii
        needed
        r ; Current region
        )
    (setq ii (make-list rank :initial-element 0))
    (setf (nth d ii) si)
    (dotimes (i size-d)
      (setq r (apply #'aref rarray ii))
      (unless (function-equal r (apply #'aref rarray (region-before-index-list-x ii d))) ; Destructively modifies ii
        (return (setq needed t))
        )
      (setq ii (next-index-list-no-d (region-after-index-list-x ii d) sizev-1 rank d))
      )
    needed)
  )

; Determine which slices are needed (one at a time)
(defun slices-needed-by-slice (p rank sizev)
  (let* ((slices (plm-slices p))
         (needed (init-needed-vector rank sizev nil)) ; Vector of vectors for the needed slices
         needed-d ; Vector for the needed slices in dimension d
         (rarray (plm-array p))
         (size (array-total-size rarray))
         sizev-d ; Size of dimension d
         (sizev-1 (vector-1 sizev))
         si ; Slice index
         )
    (dotimes (d rank)
      (setf needed-d (aref needed d))
      (setq sizev-d (aref sizev d))
      (dolist (sl (cdr (aref slices d))) ; Check all but extreme slices (start after zeroth slice)
        (setq si (slice-index sl))
        (unless (aref needed-d si) ; Ignore maximum slice, which is already needed
          (setf (aref needed-d si) (slice-needed rarray si d rank (/ size sizev-d) sizev-1))
          )
        )
      )
    needed)
  )

; Determine which slices are needed in a PLM
(defun slices-needed (p rank sizev mults &optional absolute)
  (let* ((rarray (plm-array p))
         (size (array-total-size rarray))
         (needed (init-needed-vector rank sizev nil)) ; Vector of vectors for the needed slices
         (sizev-1 (vector-1 sizev))
         r ; region
         pr ; prior region
         ri ; index vector
         si ; slice index
         bi ; index of previous region along d
         dneed ; Vector for slice needs along dimension d
         )
    ; Determine which slices are needed
    (setq ri (init-vector rank 0))
    (dotimes (i size)
      (setq r (row-major-aref rarray i))
      (dotimes (d rank)
        (setq dneed (aref needed d))
        (setq si (slice-index (region-min-slice r d)))
        (unless (aref dneed si) ; Skip comparison if know slice is already needed
          (setq bi (rmi-before-ri ri d mults))
          (setq pr (if bi (row-major-aref rarray bi) nil))
          (unless (and pr ; There is a prior region along dimension
                       (function-equal r pr absolute) ; Regions have same function
                       )
            (setf (aref dneed si) t)
            )
          )
        )
      (setq ri (next-index-vector ri sizev-1 rank))
      )
    needed)
  )

; Determine if all of the slices are needed
(defun all-needed (needed)
  (reduce #'(lambda (a b) (and a b)) (map 'vector #'(lambda (x) (reduce #'(lambda (a b) (and a b)) x)) needed))
  )

; Non-destructively eliminate hyperplane slices through a PLM that are no longer needed
; If needed is provided it is a vector of Boolean vectors true when slice is needed
; If combine is non-null, new region is that type of combination of the old pair along dimension d
; Absolute uses absolute comparisons rather than relative comparisons of region functions
(defun remove-unneeded-slices (p &optional needed combine d absolute)
  (if (zerop (plm-rank p))
      p
    (let* ((rank (plm-rank p))
           (rarray (plm-array p))
           r ; region
           (slices (plm-slices p))
           (sizev (dimension-sizes-v p))
           (mults (dimension-multipliers sizev)) ; Dimension multipliers for row major index
           np
           nslices nsizev nsizev-1 nrarray ; Equivalent variables for result array
           niv ; One index for new array
           nr ; New region
           nd ; New dimension
           nsl ; One slice list for new array
           original-fni ; Original index from new index
           original-fni-list
           (needed-d (when d (aref needed d))) ; Needed slice vector along d when it exists 
           )
      (sort-slice-list-array slices)
      (index-slice-list-array slices)
      (unless needed
        (setq needed (slices-needed p rank sizev mults absolute))
        )
      (if (plm-removed-unneeded-slices p)
          p
        (if (all-needed needed)
          (progn
            (setf (plm-removed-unneeded-slices p) t)
            p
            )
          (progn
            (setq np (make-plm :rank rank
                               :active (init-vector rank t)
                               :variables (copy-seq (plm-variables p))))
            (setq nslices (remove-unneeded-slices-from-list slices needed))
            (setq nsizev (dimension-sizes-from-slices nslices))
            (setq nsizev-1 (vector-1 nsizev))
            (setq nrarray (make-region-array (coerce nsizev 'list)))
            (setq niv (init-vector rank 0))
            (dotimes (i (array-total-size nrarray))
              (setq original-fni (original-from-new-index niv nslices))
              (setq original-fni-list (coerce original-fni 'list))
              (setq r (apply #'aref rarray original-fni-list))
              (setq nr (copy-a-region r))
              (setf (row-major-aref nrarray i) nr)
              ; Update min and max slices along each dimension
              (dotimes (j rank)
                (setq nd (aref (region-dimensions nr) j))
                (setq nsl (aref nslices j))
                (setf (dimension-min-slice nd) (nth (aref niv j) nsl))
                (setf (dimension-max-slice nd) (nth (1+ (aref niv j)) nsl)); If function already normalized, get exponentials of 0-10
                )
              (when (eq combine 'average)
                (if (and d (not (aref needed-d (+ (aref original-fni d) 1))))
                    (assign-function (average-region-functions r (row-major-aref rarray (rmi-after-ri original-fni d mults sizev)) d) nr)
                  (assign-function (average-regions-in-span nr p (dimension-sizes-v p) original-fni-list) nr))
                )
              (setq niv (next-index-vector niv nsizev-1 rank))
              )
            (setf (plm-array np) nrarray)
            (index-slice-list-array nslices)
            (setf (plm-slices np) nslices)
            (setf (plm-removed-unneeded-slices np) t)
            (when (plm-piecewise-constant p)
              (setf (plm-piecewise-constant np) t)
              )
            np)))
      ))
  )

; -----------------------------------------------------------
; Revise regions along dimension so that the function is again discrete
; Must add and remove slices as appropriate.

; Determine whether number is within epsilon of an integer
(defun eintegerp (n)
  (or (e= (if center-discrete-numeric-on-integer (+ n 1/2) n) (floor n) t)
      (e= (if center-discrete-numeric-on-integer (+ n 1/2) n) (ceiling n) t)
      )
  )

(defun discretize-dimension (p d)
  (let* ((slices (aref (plm-slices p) d))
         sizev
         sl fsl csl
         (rank (plm-rank p))
         needed ; Slices needed
         needed-d ; Slices needed along dimension d
         (new-slices (init-vector rank))
         ns
        )
    ; Add slices at the integers before and after slice location if not an integer
    (dolist (s slices)
      (setq sl (slice-location s))
      (when center-discrete-numeric-on-integer
        (setq sl (+ sl 1/2))
        )
      (setq fsl (floor sl))
      (setq csl (ceiling sl))
      ; Add slice at higher integer if not an integer and already there
      (unless (e= sl csl t)
        (setq ns (cons (make-slice :location (if center-discrete-numeric-on-integer (- csl 1/2) csl)) ns))
        )
      ; Add slice at lower integer if not an integer and not already there
      (unless (e= sl fsl t)
        (setq ns (cons (make-slice :location (if center-discrete-numeric-on-integer (- fsl 1/2) fsl)) ns))
        )
      )
    (setq ns (sort-slice-list ns))
    (setq ns (delete-duplicates-slice-list ns))
    (index-slice-list ns) ; Index the slices
    (setf (aref new-slices d) ns)
    (setq p (apply-slices new-slices nil p))
    ; Remove non-integral slices, averaging the functions on either side
    (setq sizev (dimension-sizes-v p))
    (setq needed (init-needed-vector rank sizev t))
    (setq needed-d (aref needed d))
    (setq slices (aref (plm-slices p) d))
    (dolist (s slices)
      (unless (eintegerp (slice-location s))
        (setf (aref needed-d (slice-index s)) nil)
        )
      )
    (remove-unneeded-slices p needed 'average d))
  )

; -----------------------------------------------------------
; Update a PLM based on a new region function
; Use the region to factor a PLM along all boundaries of the region
; Then update appropriate regions of PLM based on the function in the region

; Factor PLM by the min and max of each dimension of the update region
(defun factor-plm-by-dimensions (p ds)
  (let (type
        (new-slices (init-vector (plm-rank p))))
    (dotimes (d (length ds))
      (setq type (svariable-type (aref (plm-variables p) d)))
      (when (e> (dimension-min (aref ds d)) (stype-min type))
        (setf (aref new-slices d) (cons (dimension-min-slice (aref ds d)) (aref new-slices d)))
        )
      (when (e< (dimension-max (aref ds d)) (stype-max type))
        (setf (aref new-slices d) (cons (dimension-max-slice (aref ds d)) (aref new-slices d)))
        )
      )
    (sort-slice-list-array new-slices)
    (setq p (apply-slices new-slices nil p))
    p) ; Returned just to help when tracing
  )

; Return region from array given a vector of indices
(defun element-from-index-vector (array indices)
  (let ((il (coerce indices 'list)))
    (if (apply #'array-in-bounds-p array il)
        (apply #'aref array il)
      nil)
    )
  )

; recur on update-regions-in-span
(defun update-regions-in-span-recur (ur rarray i sizev starting-indices rank)
  (cond ((< i rank)
         (let ((sis (copy-seq starting-indices)))
           (do ((si sis (region-after-index-list si i (1- (aref sizev i))))) ; Destructive
               ((or (null si) (e> (region-max (apply #'aref rarray si) i) (region-max ur i))) nil)
             (update-regions-in-span-recur ur rarray (1+ i) sizev si rank)
             )
           )
         )
        ((= i rank)
         (let ((r (apply #'aref rarray starting-indices)))
           (when (within-region r nil ur)
             (copy-function ur r)
             (setf (region-evidence r) (region-evidence ur))
             )
           )
         )
        )         
  )

; Update PLM regions within span of update region
(defun update-regions-in-span (ur p sizev starting-indices)
  (update-regions-in-span-recur ur (plm-array p) 0 sizev starting-indices (plm-rank p))
  p)


; Find a slice in a vector at a location, if there is one
(defun find-slice-at-location-v (loc si sv)
  (dotimes (s (- (length sv) si))
    (when (e= loc (slice-location (aref sv (+ si s))) t)
      (return (+ si s))
      )
    )
  )

; Find the index for the slice based on its value and a starting point
; If provided, start at slice index in searching
(defun find-slice-index-v (loc si sv)
  (let ((s (find-slice-at-location-v loc si sv)))
    (if s s nil)
    )
  )

; Find a slice in a list at a location, if there is one
(defun find-slice-at-location (loc sl)
  (dolist (s sl)
    (when (e= loc (slice-location s) t)
      (return s)
      )
    )
  )

; Find the index for the slice based on its value
(defun find-slice-index (loc sl)
  (let ((s (find-slice-at-location loc sl)))
    (if s
        (slice-index s)
      nil)
    )
  )

; Find index of a region of a PLM according to its minimum slices
; If provided, use slices provided rather than those in PLM (to be able to start search part way through)
(defun find-region-index (p mins &optional slices)
  (let* ((slices (if slices slices (plm-slices p)))
        (region-index (make-list (length mins))) ; List of slice indices for region
        (ric region-index))
    (dotimes (d (plm-rank p))
      (rplaca ric (find-slice-index (aref mins d) (aref slices d)))
      (setq ric (cdr ric))
      )
    region-index)
  )

; Find a slice in a list that is largest that is <= than specified slice location, if there is one
(defun find-region-slice-at-location (loc sl)
  (let ((last-s (car sl)))
    (setq sl (cdr sl))
    (if (e< loc (slice-location last-s)) ; Loc is prior to whole dimension
        nil
      (dolist (s sl)
        (when (e< loc (slice-location s))
          (return last-s)
          )
        (setq last-s s)
        )
      )
    )
  )

; Find the index of a slice in a list that is largest that is <= than specified slice location
(defun find-region-slice-index (loc sl)
  (let ((s (find-region-slice-at-location loc sl)))
    (if s
        (slice-index s)
      nil)
    )
  )

; Error if region has a universal dimension with a weight
(defun check-universal-dimension-weights (r p)
  (dotimes (d (region-rank r))
    (when (and (multiple-variable (aref (plm-variables p) d))
               (not (e= (dimension-weight (aref (region-dimensions r) d)) 0))
               )
      (format trace-stream "~&Attempt to assign a non-zero weight to a universal dimension via region ")
      (print-region r p)
      (format trace-stream "for PLM ")
      (pplm p)
      (error "")
      )
    )
  )

; Update the function for a region of a PLM
; First factor the PLM along every boundary of region
; Then update those regions of the PLM that are within the update region
; Every PLM region should be completely within or without the update region
(defun update-region (r p &optional pred-name leave-unneeded-slices)
  (let ((pc (plm-piecewise-constant p)))
    (when (and trace-wm-changes
               (or (atom trace-wm-changes) (member pred-name trace-wm-changes))
               )
      (let (te)
        (setq te trace-empty)
        (setq trace-empty t)
        (format trace-stream "~&Updating region ")
        (print-region r p symbolic-trace trace-stream)
        (if (plm-empty p)
            (format trace-stream "of empty PLM")
          (format trace-stream "of PLM "))
        (setq trace-empty te)
        (print-plm p symbolic-trace trace-stream)
        )
      )
    (when unique-weights-only
      (check-universal-dimension-weights r p)
      )
    (setq p (factor-plm-by-dimensions p (region-dimensions r)))
    (setq p (update-regions-in-span r p (dimension-sizes-v p) (find-region-index p (region-mins-0 r))))
    (when (and trace-wm-changes
               (or (atom trace-wm-changes) (member pred-name trace-wm-changes))
               )
      (format trace-stream "~&Result region update: ")
      (print-plm p symbolic-trace trace-stream)
      )
    (when (and pc (region-is-constant r))
      (setf (plm-piecewise-constant p) t)
      )
    (unless leave-unneeded-slices
      (setq p (remove-unneeded-slices p))
      )
    p)
  )

; -----------------------------------------------------------
; Various operations on slices, including applying them

; Merge two lists of sorted slices, keeping sorted and indexing
(defun merge-slice-lists (sl1 sl2)
  (let (nsl s1 s2
        (ni 0) ; Index for position in the nsl list
        )
    (loop
     (cond ((and sl1 sl2) ; Slices left in both sl1 and sl2
            (setq s1 (slice-location (car sl1)))
            (setq s2 (slice-location (car sl2)))
            (cond ((e= s1 s2 t)
                   (setq nsl (cons (make-slice :location s1 :index ni) nsl))
                   (setq sl1 (cdr sl1))
                   (setq sl2 (cdr sl2))
                   )
                  ((e< s1 s2)
                   (setq nsl (cons (make-slice :location s1 :index ni) nsl))
                   (setq sl1 (cdr sl1))
                   )
                  (t
                   (setq nsl (cons (make-slice :location s2 :index ni) nsl))
                   (setq sl2 (cdr sl2))
                   )
                  )
            )
           (sl1 ; No slices left in sl2
             (setq nsl (cons (make-slice :location (slice-location (car sl1)) :index ni) nsl))
             (setq sl1 (cdr sl1))
             )
           (sl2 ; No slices left in sl1
             (setq nsl (cons (make-slice :location (slice-location (car sl2)) :index ni) nsl))
             (setq sl2 (cdr sl2))
             )
           (t (return)) ; No slices left in either list
           )
     (setq ni (1+ ni))
     )
    (nreverse nsl))
  )

; Merge two (sorted) vectors of slice lists yielding a sorted and indexed result
; The two inputs can be of different lengths, but the dimensions of the first must all
; be included in the second, with the map specifying the relationship
(defun merge-slice-vectors (sv1 map sv2)
  (let* ((size1 (length sv1))
         (size2 (length sv2))
         (nsv (init-vector size2))
        mapv
        d
        )
    (when map
      (setq mapv (smap-vfactor map))
      )
    ; Process dimensions that are in both vectors
    (dotimes (i size1)
      (setq d (if mapv (aref mapv i) i))
      (setf (aref nsv d) (merge-slice-lists (aref sv1 i) (aref sv2 d)))
      )
    ; Process dimensions only in second vector
    (dotimes (d size2)
      (unless (aref nsv d)
        (setf (aref nsv d) (copy-slice-list (aref sv2 d)))
        )
      )
    nsv)
  )

; Given two lists of (sorted indexed) slices, create a vector that gives the slice index in the
; first list for every index in the second list
(defun cross-index-slice-lists (sl1 sl2)
  (let* ((size2 (length sl2))
         (xi (init-vector size2)) ; Indices
         (previous-index -1)
         )
    (unless (reduce #'(lambda (a b) (and a b)) sl1 :key #'slice-index)
      (error "Can't cross-index a slice list with NIL indices: ~S" sl1)
      )
    (loop
     (when (not sl2) (return))
     (cond ((not sl1)
            (setf (aref xi (slice-index (car sl2))) -1)
            (setq sl2 (cdr sl2))
            )
           ((e= (slice-location (car sl2)) (slice-location (car sl1)) t)
            (setf (aref xi (slice-index (car sl2))) (slice-index (car sl1)))
            (setq previous-index (slice-index (car sl1)))
            (setq sl1 (cdr sl1))
            (setq sl2 (cdr sl2))
            )
           ((e< (slice-location (car sl2)) (slice-location (car sl1)))
            (setf (aref xi (slice-index (car sl2))) previous-index)
            (setq sl2 (cdr sl2))
            )
           (t ; e>
            (setq sl1 (cdr sl1))
            (setq previous-index (1+ previous-index))
            )
           )
     )
    xi)
  )

; Given two vectors of lists of (sorted indexed) slices, create a vector of vectors that for each pair of lists gives the
; slice index in the first list for every index in the second list
; Only creates entries for dimensions in both vectors
(defun cross-index-slice-vector (sv1 map sv2)
  (let* ((size1 (length sv1))
         (size2 (length sv2))
         (xiv (init-vector size2))
         (mapv (if map (smap-vfactor map) nil))
         mapd
         )
    (dotimes (d size1)
      (setq mapd (if mapv (aref mapv d) d))
      (setf (aref xiv mapd) (cross-index-slice-lists (aref sv1 d) (aref sv2 mapd)))
      )
    xiv)
  )

; Index list from cross-index
; iv is the index vector and ci is the cross-index
(defun index-list-from-cross-index (iv ci map)
  (if map ; Generate an index list appropriate to size and mapping
      (let* ((mapv (smap-vfactor map))
             (rank (length mapv))
             (civ (init-vector rank))
             mapd)
        (dotimes (d rank)
          (setq mapd (if mapv (aref mapv d) d))
          (setf (aref civ d) (aref (aref ci mapd) (aref iv mapd)))
          )
        (coerce civ 'list)
        )
    (let (cil)
      (dotimes (d (length iv))
        (setq cil (cons (aref (aref ci d) (aref iv d)) cil))
        )
      (nreverse cil)))
  )

; Index list from cross-index
; il is the index list and ci is the cross-index
(defun index-list-from-cross-index-list (il ci map)
  (if map ; Generate an index list appropriate to size and mapping
      (let* ((mapv (smap-vfactor map))
             (rank (length mapv))
             (civ (init-vector rank))
             mapd)
        (dotimes (d rank)
          (setq mapd (if mapv (aref mapv d) d))
          (setf (aref civ d) (aref (aref ci mapd) (nth mapd il)))
          )
        (coerce civ 'list)
        )
    (let (cil)
      (dotimes (d (length il))
        (setq cil (cons (aref (aref ci d) (car il)) cil))
        (setq il (cdr il))
        )
      (nreverse cil)))
  )

; Determine if there is a region in an array (given by list of dimension sizes) at specified index list
; There only shouldn't be when region index is from an affine transform that requires padding
(defun no-corresponding-region (ci ds)
  (reduce #'(lambda (a b) (or a b)) (map 'list #'>= ci ds))
  )

; Apply a slice array to a PLM
; s is a vector (one element for each dimension) of lists (one element for each slice)
; map determines how s dimensions map onto p dimensions
; p is a plm
; If svars is provided, it is the variables that go with the dimensions of s
(defun apply-slices (s map p &optional region-pad)
  (let* ((ps (plm-slices p))
         (rank (length ps))
         np ; New PLM
         nps ; Slices for np
         civ ; Slice cross-index vector
         (rarray (plm-array p))
         nrarray
         iv ; Index vector
         sizev-1
         cross-index ; Index vector for region in p corresponding to region in np
         )
    (setq nps (merge-slice-vectors s map ps))
    (cond ((slice-vectors-equal-length ps nps) ; Just use original function as no new slices were added
           p)
          (t
           (setq np (init-plm-with-slices (copy-seq (plm-variables p)) 0 0 (copy-seq (plm-active p)) nps))
           (setq nrarray (plm-array np))
           (setq civ (cross-index-slice-vector ps nil nps))
           (setq sizev-1 (vector-1 (array-dimensions-v nrarray)))
           (setq iv (init-vector rank 0))
           (dotimes (i (array-total-size nrarray))
             (setq cross-index (index-list-from-cross-index iv civ nil))
             (if (or (member -1 cross-index) (no-corresponding-region cross-index (array-dimensions rarray))) ; I'm not sure I understand this at this point
                 (setf (region-constant (row-major-aref nrarray i)) region-pad) ; Pad
               (copy-region-contents (apply #'aref rarray cross-index) (row-major-aref nrarray i))) ; Copy
             (setq iv (next-index-vector iv sizev-1 rank))
             )
           (when (plm-piecewise-constant p)
             (setf (plm-piecewise-constant np) t)
             )
           np)
          ))
  )

; Just factor plm along one slice
(defun factor-plm (p d x &optional region-pad)
  (let ((slices (init-vector (plm-rank p))))
    (setf (aref slices d) (list x))
    (apply-slices slices nil p region-pad)
    )
  )

; -----------------------------------------------------------
; Compute the pointwise combination of two PLMs
; Normally this would be a pointwise product for summary product,
; but sum is used in combining the effects of actions in deciding what changes to make to WM
; The product computation for regions was adapted from mhetree4.lisp
; Possibly could be optimized better for PLMs.

; Compute value of a linear function (as a vector of weights) at a point (as a vector)
(defun linear-value (function point)
  (let ((value 0))
    (dotimes (d (length function) value)
      (setq value (+ value (* (aref function d) (aref point d))))
      )
    value)
  )

; Compute value of an exponential function (as a vector of weights) at a point (as a vector)
(defun exponential-value (function point)
  (let ((value 0))
    (dotimes (d (length function) value)
      (setq value (+ value (* (aref function d) (aref point d))))
      )
    (exp value))
  )


; Compute outer product of two vectors
(defun outer-product (v1 v2)
  (let* ((r1 (length v1))
         (r2 (length v2))
         (op (make-array (list r1 r2)))
         )
    (dotimes (i1 r1)
      (dotimes (i2 r2)
        (setf (aref op i1 i2) (* (aref v1 i1) (aref v2 i2)))))
    op)
  )

; Given a product of two linear functions as an outer product,
; compute the value of the polynomial at a point
(defun quadratic-value (function point)
  (let ((value 0)
        (dimension (array-dimension function 0))
        )
    (dotimes (i dimension)
      (dotimes (j dimension)
        (setq value (+ value
                       (* (aref function i j)
                          (aref point i)
                          (aref point j))))
        )
      )
    value)
  )

; Compute linear slope approximation for a quadratic function along one dimension at a point.
; Essentialy computes the slope function by differentiation of the function
; with respect to variable d and then evaluates that function at point.
; The first element of the point vector should always be 1 (for product with the constant),
; and the others are point within a region for the corresponding variables.
; wps is a 2D weight array for the products of the weights of two linear functions.
; Every cell for d is counted horizontally and vertically
(defun linear-slope (wps d point)
  (let ((slope 0))
    (dotimes (i (array-dimension wps 0))
      (setq slope (+ slope (* (aref wps d i) (aref point i)))))
    (dotimes (i (array-dimension wps 1))
      (setq slope (+ slope (* (aref wps i d) (aref point i)))))
    slope)
  )

; Compute the mean of the arguments
(defun mean (&rest args)
  (/ (apply #'+ args) (length args)))

; Determine the minimum point in an n dimensional region
; Values start at 0 in the vector
(defun region-mins-0 (r)
  (let* ((rank (region-rank r))
         (ds (region-dimensions r))
         (vmin (init-vector rank))
        )
    (dotimes (d rank)
      (setf (aref vmin d) (dimension-min (aref ds d)))
      )
    vmin)
  )

; Determine the minimum point in an n dimensional region
; Values start at 1 in the vector, with the 0 element being 1 (for constant)
(defun region-mins (r)
  (let* ((rank (region-rank r))
         (ds (region-dimensions r))
         (vmin (init-vector (+ rank 1)))
        )
    (setf (aref vmin 0) 1)
    (dotimes (d rank)
      (setf (aref vmin (+ d 1)) (dimension-min (aref ds d)))
      )
    vmin)
  )

; Determine the maximum point in an n dimensional region
; Values start at 1 in the vector, with the 0 element being 1 (for constant)
(defun region-maxs (r)
  (let* ((rank (region-rank r))
         (ds (region-dimensions r))
         (vmax (init-vector (+ rank 1)))
        )
    (setf (aref vmax 0) 1)
    (dotimes (d rank)
      (setf (aref vmax (+ d 1)) (dimension-max (aref ds d)))
      )
    vmax)
  )

; Determine the midpoint in an n dimensional region
; Values start at 1 in the vector, with the 0 element being 1 (for constant)
(defun region-mids (r)
  (let* ((rmins (region-mins r))
         (rmaxs (region-maxs r))
         (rank (+ (region-rank r) 1))
         (vmid (init-vector rank)))
    (dotimes (d rank)
      (setf (aref vmid d) (/ (+ (aref rmaxs d) (aref rmins d)) 2)))
    vmid)
  )

; Compute a slope approximation by averaging the slopes derived at min, mid and amx
(defun slope-approximation-points (wps mins mids maxs d)
  (mean (linear-slope wps d mins)
        (linear-slope wps d mids)
        (linear-slope wps d maxs)))

; Compute the error/difference between quadratic function and linear approximation at a point
(defun approximation-error (wps wv point)
  (- (quadratic-value wps point)
     (linear-value wv point)))

; Compute a constant approximation by averaging over constants at min, mid and max
(defun constant-approximation-points (wps wv mins mids maxs)
  (mean (approximation-error wps wv mins)
        (approximation-error wps wv mids)
        (approximation-error wps wv maxs)))

; Take (approximate) max of two functions over a region
; Approximate by choosing function which is max at center point of region
; Return T if first is greater (rather than returning actual function)
(defun first-max-region-functions (f1 f2 r)
  (let ((mp (region-mids r)))
    (> (linear-value f1 mp) (linear-value f2 mp))
    )
  )

; Take max of two functions at a point (usually midpoint)
; Return T if first is greater (rather than returning actual function)
(defun first-max-functions (f1 f2 pt)
  (> (linear-value f1 pt) (linear-value f2 pt))
  )

; Multiply two linear functions, generating a new linear approximation of the result
; pc1 and pc2 are whether respective functions are constant
(defun product-functions (f1 f2 rank rnew &optional pc1 pc2)
  (let ((wv) ;0th element is the constant
        wps)
    (if pc1
        (if pc2 ; Both functions are constant
            (if (or (zerop f1) (zerop f2)) ; One of the functions is 0
                (setq wv 0)
              (if (= f1 1)
                  (setq wv f2)
                (if (= f2 1)
                    (setq wv f1)
                  (setq wv (* f1 f2)))))
          (if (zerop f1)
              (setq wv (init-vector (+ rank 1) 0)) ; 0 vector
            (if (= f1 1)
                (setq wv (copy-seq f2)) ; Copy second function
              (setq wv (scale-function f2 nil f1))))) ; First function a constant not 0 or 1, and second is variable
      (if pc2
          (if (zerop f2)
              (setq wv (init-vector (+ rank 1) 0)) ; 0 vector
            (if (= f2 1)
                (setq wv (copy-seq f1)) ; Copy second function
              (setq wv (scale-function f1 nil f2)))) ; Second function a constant not 0 or 1, and first is variable
        (let ((mins (region-mins rnew)) ; Neither function is constant
              (mids (region-mids rnew))
              (maxs (region-maxs rnew)))
          (setf wv (init-vector (+ rank 1) 0)) ; Initialize 0 vector
          ; Multiply two linear functions to achieve a quadratic function via outer product
          (setq wps (outer-product f1 f2))
          ; Compute slopes for the variables
          (dotimes (d rank)
            (setf (aref wv (+ d 1)) (slope-approximation-points wps mins mids maxs (+ d 1)))
            )
          ; Compute constant
          (setf (aref wv 0) 0) ; Baseline for computing constant
          (setf (aref wv 0) (constant-approximation-points wps wv mins mids maxs))
          )))
    wv)
  )

; Max of two linear functions

(defun max-functions (f1 f2 rnew &optional piecewise-constant)
  (if piecewise-constant
      (if (> f1 f2) f1 f2)
    (if (first-max-functions f1 f2 (region-mids rnew)) f1 f2))
  )

; Probabilistic OR (assuming independence) of two regions with linear functions
; pc1 and pc2 are whether respective functions are constant
(defun por-functions (f1 f2 rank rnew &optional piecewise-constant)
  (let ((sum-f (vector-sum f1 f2 piecewise-constant))
        (product-f (product-functions f1 f2 rank rnew piecewise-constant piecewise-constant))
        )
    (vector-difference sum-f product-f piecewise-constant))
  )

; Divide first function by constant in second
; Doesn't currently handle full functions in second
(defun divide-functions (f1 f2 &optional piecewise-constant)
  (vector-divide f1 (if piecewise-constant f2 (aref f2 0)) piecewise-constant)
  )

; Divide first function by constant in second
; Doesn't currently handle full functions in second
; If second function is 0, returns 0
(defun divide-0-functions (f1 f2 &optional piecewise-constant)
  (vector-divide-0 f1 (if piecewise-constant f2 (aref f2 0)) piecewise-constant)
  )

; Scale GDL increment
; If value of first function is greater than max-gdl-increment, divide by max and multiply by max-gdl increment
; Assumes both functions are constant
(defun gdl-scale (f1 f2)
  (let ((max f1))
    (if (or (zerop max) (<= max max-gdl-increment))
        f2 ; Use original function
      (let (nv)
        (setq nv (/ (* f2 max-gdl-increment) max))
        nv))
    )
  )        

; Determine if number n is within dimensions d's bounds
(defun within-bounds (n dim)
  (and (e>= n (dimension-min dim) t) (< n (dimension-max dim)))
  )

; Determine if point is in a PLM (not outside of the minimum and maximum slices for any dimension)
(defun point-in-plm (x p)
  (let ((result t)
        (vs (plm-variables p))
        xi)
    (dotimes (i (length vs))
      (setq xi (aref x i)) ; start at position 1 for initial dimension in point
      (when (or (e< xi (stype-min (svariable-type (aref vs i))))
                (e>= xi (stype-max (svariable-type (aref vs i))) t))
        (setq result nil)
        (return nil)
        )
      )
    result)
  )

; Find the index vector for the region containing the point
(defun region-point-index (slice-array x rank)
  (let ((rindex (init-vector rank))
        )
    (dotimes (d rank)
      (dolist (s (aref slice-array d))
        (when (e< (aref x d) (slice-location s))
          (setf (aref rindex d) (1- (slice-index s)))
          (return)
          )
        )
      )
    rindex)
  )

; Determine region within a PLM p containing point x (in p2 space)
; Returns nil if the point isn't in the PLM
(defun region-point (x p)
  (if (point-in-plm x p)
      (apply #'aref (plm-array p) (coerce (region-point-index (plm-slices p) x (plm-rank p)) 'list))
    nil)
  )

; Check if two PLMs to be multiplied are compatible type-wise
(defun plm-type-check (plm1 map plm2)
  (let* ((vars1 (plm-variables plm1))
         (vars2 (plm-variables plm2))
         v2
         (check t)
         mapd)
    (dotimes (d (length vars1))
      (setq mapd (if map (aref (smap-vfactor map) d) d))
      ; Deal with dimensions in factor with multiple variables (delta and transform functions)
      (setq v2 (aref vars2 mapd))
      (when (listp v2) (setq v2 (car v2)))
      ; Check that types are the same
      (unless (eq (svariable-type (aref vars1 d))
                  (svariable-type v2))
        (error "Type incompatibiltiy in plm-type-check ~D:~S, ~D:~S"
               d (stype-name (svariable-type (aref vars1 d)))
               mapd (stype-name (svariable-type v2))
               )
        (return (setq check nil))))
    check)
  )

; Beginning of code for sparse product---------------------

; Total volume of PLM
(defun plm-volume (p)
  (reduce #'* (plm-variables p) :key #'(lambda (v) (stype-span (svariable-type v))))
  )

; Just determine which slices begin non-empty regions
; Alternative to plm-dense function when don't need to figure out the density
(defun plm-non-empty (p)
  (let* ((piecewise-constant (plm-piecewise-constant p))
         (dense-volume 0)
         (rank (plm-rank p))
         (rarray (plm-array p))
         (non-empty (init-vector rank)) ; Will store vectors marking which slices start non-empty regions
         (slice-vector (plm-slices p))
         r ds)
    ; Initialize non-empty vector for each dimension's slices
    (dotimes (d rank)
      (setf (aref non-empty d) (init-vector (length (aref slice-vector d))))
      )
    (dotimes (i (array-total-size rarray))
      (setq r (row-major-aref rarray i))
      (when (not (region-e-empty r piecewise-constant))
        (setq ds (region-dimensions r))
        ; Add volume of region to dense total
        (setq dense-volume (+ (region-volume r) dense-volume)) 
        ; Mark min slices as starting non-empty regions
        (dotimes (d rank)
          (setf (aref (aref non-empty d) (slice-index (dimension-min-slice (aref ds d)))) t)
          )
        )
      )
    (values non-empty (/ dense-volume (plm-volume p)))) 
  )

; Merge a sparse list of slices with another one
; Return merged list plus cross-indexes to input lists
(defun sparse-merge-slices (sl1 sl-non-empty sl2)
  (let ((slice-mappings (init-vector 2))
        (slice-number 0)
        (last2 0)
        nsl car-sl cadr-sl min1 max1 loc2 sml1 sml2 index1 non-empty
        )
    ; Iterate through slices of first PLM
    (do ((sl1c sl1 (cdr sl1c)))
        ((null (cdr sl1c)) (setq car-sl (car sl1c)))
      (setq car-sl (car sl1c))
      (setq min1 (slice-location car-sl))
      (setq cadr-sl (cadr sl1c))
      (setq max1 (slice-location cadr-sl))
      ; Add slice from first PLM to new list
      (push (make-slice :location (slice-location car-sl) :index slice-number) nsl)
      ; Add cross-indexes
      (setq index1 (slice-index car-sl))
      (setq non-empty (aref sl-non-empty index1))
      (push index1 sml1)
      (when (e= min1 (slice-location (car sl2)) t)
        (setq last2 (slice-index (car sl2)))
        (setq sl2 (cdr sl2))
        )
      (push last2 sml2)
      (setq slice-number (1+ slice-number))
      (do ((sl2c sl2 (cdr sl2c))) ; Add the slices from the second PLM from here to the next slice of the first PLM
          ((null sl2c) nil)
        (setq loc2 (slice-location (car sl2c)))
        (cond ((e>= loc2 max1 t) ; Beyond the current region from first PLM
               (setq sl2 sl2c)
               (return)
               )
              ((and (> loc2 min1) (< loc2 max1)) ; Slice from second PLM is within region of first PLM
               (setq last2 (slice-index (car sl2c)))
               (when non-empty ; The slice in the first PLM begins a non-empty region
                 (push (make-slice :location loc2 :index slice-number) nsl)
                 (push index1 sml1)
                 (push last2 sml2)
                 (setq slice-number (1+ slice-number))
                 )
               )
              )
        )
      )
    ; Add last slice
    (push (make-slice :location (slice-location car-sl) :index slice-number) nsl)
    (push (slice-index car-sl) sml1)
    (push (slice-index (car sl2)) sml2)
    ; Create slice mappings by reversing lists and coercing to vectors
    (setf (aref slice-mappings 0) (coerce (reverse sml1) 'vector))
    (setf (aref slice-mappings 1) (coerce (reverse sml2) 'vector))
    (values (nreverse nsl) slice-mappings))
  )

; A vector that counts up from 0 until its end
(defun count-up-vector (n)
  (let ((v (init-vector n)))
    (dotimes (i n)
      (setf (aref v i) i)
      )
    v)
  )

; Find the slices and mappings for a sparse product
(defun find-sparse-slices (p1 p1-non-empty map p2)
  (let* ((rank1 (plm-rank p1))
         (rank2 (plm-rank p2))
         (nsv (init-vector rank2)) ; Slices
         (mappings (init-vector rank2)) ; Mappings
         (sv1 (plm-slices p1))
         (sv2 (plm-slices p2))
         (mapv (if map (smap-vfactor map) nil))
         md
         one-sv one-mapping
         )
    ; Handle dimensions in common between the two PLMS
    (dotimes (d rank1)
      (setq md (if mapv (aref mapv d) d))
      (multiple-value-setq (one-sv one-mapping) (sparse-merge-slices (aref sv1 d) (aref p1-non-empty d) (aref sv2 md)))
      (setf (aref nsv md) one-sv)
      (setf (aref mappings md) one-mapping)
      )
    ; Handle dimensions only in second PLM
    (dotimes (d rank2)
      (when (null (aref nsv d))
        (setf (aref nsv d) (copy-seq (aref sv2 d)))
        (setf (aref mappings d) (vector nil (count-up-vector (length (aref sv2 d)))))
        )
      )
    (values nsv mappings))
  )

; Find the slices and mappings for a sparse product
; This function is used when the second plm is the sparse one, non-empty should be created by using p2
(defun find-sparse-slices-reverse (p1 p2-non-empty map p2) 
  (let* ((rank1 (plm-rank p1))
         (rank2 (plm-rank p2))
         (nsv (init-vector rank2)) ; Slices
         (mappings (init-vector rank2)) ; Mappings
         (sv1 (plm-slices p1))
         (sv2 (plm-slices p2))
         (mapv (if map (smap-vfactor map) nil))
         md
         one-sv one-mapping
         )
    ; Handle dimensions in common between the two PLMS
    (dotimes (d rank1)
      (setq md (if mapv (aref mapv d) d))
      (multiple-value-setq (one-sv one-mapping) (sparse-merge-slices (aref sv2 md) (aref p2-non-empty md) (aref sv1 d)))
      (setf (aref nsv md) one-sv)
      (setf (aref mappings md) one-mapping)
      )
    ; Handle dimensions only in second PLM
    (dotimes (d rank2)
      (when (null (aref nsv d))
        (setf (aref nsv d) (copy-seq (aref sv2 d)))
        (setf (aref mappings d) (vector (count-up-vector (length (aref sv2 d))) nil))
        )
      )
    (values nsv mappings))
  )

; Make index for accessing an input region in sparse product
(defun sparse-region-index (nindex which-index mapv size cross-indexes)
  (let (index mapd)
    (dotimes (d size)
      (setq mapd (if mapv (aref mapv d) d))
      (push (aref (aref (aref cross-indexes mapd) which-index) (aref nindex mapd)) index)
      )
    (nreverse index))
  )

; Compute product of two PLMs where the first one is sparse
(defun sparse-product-plms (p1 non-empty map p2 &optional evidence argument)
  (let* ((pc1 (plm-piecewise-constant p1))
         (pc2 (plm-piecewise-constant p2))
         (rarray1 (plm-array p1))
         (rarray2 (plm-array p2))
         (rank1 (plm-rank p1))
         (rank2 (plm-rank p2))
         (piecewise-constant (and pc1 pc2))
         (nindex (init-vector (plm-rank p2) 0))
         (mapv (if map (smap-vfactor map) nil))
        slice-vector cross-indexes np nrarray sizev-1)
    (multiple-value-setq (slice-vector cross-indexes) (find-sparse-slices p1 non-empty map p2))
    (setq np (init-plm-with-slices (copy-seq (plm-variables p2)) 0 0 (init-vector (plm-rank p2) t) slice-vector))
    (setq nrarray (plm-array np))
    (setq sizev-1 (vector-1 (dimension-sizes-v np)))
    (dotimes (i (array-total-size nrarray))
      (combine-regions p1 (apply #'aref rarray1 (sparse-region-index nindex 0 mapv rank1 cross-indexes)) map
                       p2 (apply #'aref rarray2 (sparse-region-index nindex 1 nil rank2 cross-indexes))
                       (row-major-aref nrarray i) np 'product evidence argument pc1 pc2 piecewise-constant)
      (setq nindex (next-index-vector nindex sizev-1 rank2))
      )
    np)
  )

; Compute product of two PLMs where the first one is sparse
; This function is used when the second plm is the sparse plm,  non-empty should be created by using p2
(defun sparse-product-plms-reverse (p1 p2-non-empty map p2 &optional evidence argument) 
  (let* ((pc1 (plm-piecewise-constant p1))
         (pc2 (plm-piecewise-constant p2))
         (rarray1 (plm-array p1))
         (rarray2 (plm-array p2))
         (rank1 (plm-rank p1))
         (rank2 (plm-rank p2))
         (piecewise-constant (and pc1 pc2))
         (nindex (init-vector (plm-rank p2) 0))
         (mapv (if map (smap-vfactor map) nil))
        slice-vector cross-indexes np nrarray sizev-1
        )
    (multiple-value-setq (slice-vector cross-indexes) (find-sparse-slices-reverse p1 p2-non-empty map p2))
    (setq np (init-plm-with-slices (copy-seq (plm-variables p2)) 0 0 (init-vector (plm-rank p2) t) slice-vector))
    (setq nrarray (plm-array np))
    (setq sizev-1 (vector-1 (dimension-sizes-v np)))
    (dotimes (i (array-total-size nrarray))
      (combine-regions p1 (apply #'aref rarray1 (sparse-region-index nindex 1 mapv rank1 cross-indexes)) map
                       p2 (apply #'aref rarray2 (sparse-region-index nindex 0 nil rank2 cross-indexes))
                       (row-major-aref nrarray i) np 'product evidence argument pc1 pc2 piecewise-constant)
      (setq nindex (next-index-vector nindex sizev-1 rank2))
      ) 
    np)
  )

; For testing sparse product
(defun tsp ()
  (let ((map (make-smap :vfactor #(1)))
        preds predd sparse dense non-empty)
    (init)
    (new-type 'ten :discrete t :numeric t :min 0 :max 10)
    (setq preds (predicate 'sparse :arguments '((x ten))))
    (setq predd (predicate 'dense :arguments '((y ten) (x ten))))
    (setq sparse (cpt-function-array-plm '(wm-x) '((1 (0 3)) (1 5)) (predicate-wm-variables preds) 0))
    (setq dense (cpt-function-array-plm '(wm-y wm-x) '((1 0 0) (1 0 (2 4)) (1 0 (6 8)) (1 1 (6 8))) (predicate-wm-variables predd) 0))
    (pa sparse t)
    (pa dense t)
    (setq non-empty (plm-non-empty sparse))
;    (find-sparse-slices sparse non-empty map dense)
;    (sparse-merge-slices (aref (plm-slices sparse) 0) (aref non-empty 0) (aref (plm-slices dense) 0))
    (pa (sparse-product-plms sparse non-empty map dense))
    t)
  )

; End of code for sparse product------------------------------------

; Compute the posterior distribution for a variable as the product
; of the incoming messages for the node
(defun variable-posterior (n)
  (when (variable-nodep n)
    (let ((links (node-links n))
          post uds)
      (dolist (link links)
        (when (link-fact-content link) ; Only when link is active in this direction
          (if post
              (setq post (combine-plms (link-fact-content link) nil post 'product))
            (setq post (link-fact-content link)))
          )
        )
      (when post
        (setq post (remove-unneeded-slices post))
        )
      (when (and post (node-normalize n))
        (setq uds (unique-dimensions post t))       
        (when uds   
          (setq post
                (if (node-vector n)  
                    (normalize-plm post nil t) 
                  (normalize-plm post nil))
                )
          )
        )
      post)
    )
  )

