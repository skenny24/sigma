(in-package :sigma)
; Copyright 2009-2016 University of Southern California. All rights reserved.
;
; Redistribution and use in source and binary forms, with or without modification, are
; permitted provided that the following conditions are met:
; 
;    1. Redistributions of source code must retain the above copyright notice, this list of
;       conditions and the following disclaimer.
; 
;    2. Redistributions in binary form must reproduce the above copyright notice, this list
;       of conditions and the following disclaimer in the documentation and/or other materials
;       provided with the distribution.
; 
; THIS SOFTWARE IS PROVIDED BY UNIVERSITY OF SOUTHERN CALIFORNIA ``AS IS'' AND ANY EXPRESS OR IMPLIED
; WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
; FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL UNIVERSITY OF SOUTHERN CALIFORNIA OR
; CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
; ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING7
; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
; ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
; 
; The views and conclusions contained in the software and documentation are those of the
; authors and should not be interpreted as representing official policies, either expressed
; or implied, of University of Southern California.
; -----------------------------------------------------------
; Initialization stuff

;(declaim (optimize (speed 3)
;                   (compilation-speed 0)
;                   (safety 0)
;                   (debug 0)
;                   )
;         )

; Attempt to detect printing of circular lists and do the right thing
(setq *print-circle* nil)
(setq *print-level* 10) 
#+CLISP (unuse-package "EXT")
; -----------------------------------------------------------
; Path and version information

; Path to source files and data sets for learning
; sigma home used for compilation of temp regression-testing file
; Version of Sigma
(defconstant sigma-version "Sigma38")
(defconstant *sigmahome* (car asdf:*central-registry*))
(defconstant dataset-path (merge-pathnames "../data/" *sigmahome*))

; -----------------------------------------------------------
; Constants

; Symbol to be used at beginning of a list defining a filter
(defconstant filter-symbol ':filter)

; Symbol to be used at beginning fo a list defining a not-equal test
(defconstant not-equal-symbol '<>)

; Functional infinity for use in episodic memory and other things
; Needs to be no more than seven digits because when add epsilon it
; becomes a floating point number with only seven significant digits
; Slop probably could be just 1
(defconstant infinity 8888888)

; Maximum time allowed
(defconstant max-time 999999)

; Maximum state allowed
(defconstant max-state 100)

; Maximum continuous value allowed in types
(defconstant continuous-max 1000.0)

; Arguments for positive and negative elements
(defconstant boolean-true 'true)
(defconstant boolean-false 'false)
(defconstant sense-positive 1)
(defconstant sense-negative 0)

(defconstant negation-symbol '-) ; Define negation symbol
(defconstant best-symbol '!) ; Unique variable for that actions put best choice into WM
(defconstant distribution-symbol '%) ; Unique variable for that actions put distribution into WM
(defconstant expected-symbol '$) ; Unique variable for actions that put expected value into WM
(defconstant prob-match-symbol '=) ; Unique variable for actions that choose by probability matching
(defconstant exponential-symbol '^) ; Unique variable for actions that exponentially transform their distribution into WM

; Symbol of interruption
(defconstant interrupt-symbol '<***interrupted***>)

; Symbol of halt
(defconstant halt-symbol '<***halted***>)

; Suffix used for predicates in diachronic prediction
(defconstant prediction-suffix '*next)

; Suffice used for predicates in episodic learning
(defconstant episodic-suffix '*episodic)

; Specify temporal predicate name
(defconstant temporal-predicate-name 'time)

; Concat a list of symbols, strings and numbers into a new symbol
; When dashes is true, places a - between them
(defun concat-symbols (symbols &optional dashes)
  (let (string-l)
    (do ((symbol-l symbols (cdr symbol-l)))
        ((null symbol-l))
      (setq string-l (cons (if (symbolp (car symbol-l))
                               (symbol-name (car symbol-l))
                             (if (numberp (car symbol-l))
                                 (format nil "~S" (car symbol-l))
                               (car symbol-l)))
                           string-l))
      (when (and dashes (not (null (cdr symbol-l))))
        (setq string-l (cons '"-" string-l)))
      )
    (intern (apply #'concatenate (cons 'string (reverse string-l))))
    )
  )

; Specify (open-world) temporal predicate name for episodic memory
(defvar temporal-predicate-name-episodic (concat-symbols (list temporal-predicate-name episodic-suffix)))
;(defconstant temporal-predicate-name-episodic 'tpne) DEFCONST not working in PKG


; Specify halt predicate name
(defconstant halt-predicate-name 'halt)

; Indices into the nodes and contents links of arrays
; The variable node is always first and the factor node second
(defconstant var-index 0)
(defconstant fact-index 1)

; Indices of the various queues
(defconstant depth-queues-index 0)
(defconstant other-queue-index 1)

; Symbol to use in marking which attribute is the category
(defconstant category-symbol '*)

; System predicate names that shouldn't be created by users
(defconstant system-predicates '(detect-impasses evaluate-operator halt impasse selected state time))

; -----------------------------------------------------------
; Bipartite parameters and variables
;;; Variables
(defvar bp-nodes)
(defvar bp-graph)
(defvar *bp-node-stream*)
(defvar *nodes-depth* 3)
(defvar *main-pinboard*)
(defvar *selected-node* nil)
(defvar display-subgraph nil)
(defvar *node-stream*) ; Stream for printing information about nodes when they are clicked on
(defvar graph-alpha t) ; Whether or not to include the alpha network (before the alpha memory) in the graph

;-----------------------------------------------------------
; message queue

;(defparameter *pqueue* (make-instance 'cl-heap:priority-queue))
;(defparameter *parallelqueue* (make-instance 'cl-heap:priority-queue))
(defparameter *qhash* (make-hash-table :test 'equal))

; -----------------------------------------------------------
; Parameters

; Convert a defvar name into a defparameter name
(defun parameter-name (variable-name)
  (concat-symbols `(* ,variable-name *))
  )

; List of all global parameters (to be created dynamically)
; A list of dotted pairs, with variable name followed by parameter name
(defparameter global-parameters nil)

; List of parameters not to reset automatically (overrides normal resetting)
(defparameter parameter-override-reset nil)

; Define a global parameter
(defun parameter (variable-name initial-value)
  (let ((parameter-name (parameter-name variable-name)))
    (eval (list 'defparameter parameter-name (list 'quote initial-value)))
    (eval `(defvar ,variable-name))
    (push (cons variable-name parameter-name) global-parameters)
    initial-value)
  )

; Whether to use the new bipartite graph display
(parameter 'bipartite-graph-display t)

; Set to true if new constants will be introduced in the evidence
(parameter 'extend-type-constants-by-evidence nil)

; Determine whether to detect impasses
(parameter 'detect-impasses nil)

; Maximum messages to process at a call to process-message-queue
(parameter 'max-messages 10000)
; When non-null, set max-messages to this times number of links in graph
(parameter 'max-messages-links 20)

; Trace messages
(parameter 'trace-messages nil)

; Trace trials
(parameter 'trace-trials t)

; Track distribution of messages per parallel cycle
(parameter 'cycle-message-counts nil)

; Whether a trace of messages should be symbolic
(parameter 'symbolic-trace t)

; Whether to trace number of decisions
(parameter 'trace-decisions t)

; Whether to trace number of parallel elaboration cycles
(parameter 'trace-cycles t)
 
; Whether to print out elements that are (e= to) zero in a symbolic trace
(parameter 'trace-empty nil)
; Whether to print out elements that are (e= to) one in a symbolic trace
; Just modified for printing negated changes, which only matter when 0
(parameter 'trace-full t)

; Temperature variables for Boltzmann selection
;(parameter 'default-temperature 1/5)
(defvar default-temperature 1/5) ; should fix parameter function to add properly or extract from global-params
(parameter 'temperature-minimum 1/60)

; Need this global variable and function here to define parameter
; Number of trials (each of multiple decisions) that have been run
(defvar trial-count 0)
; Standard temperature-schedule
(defun log-trials nil
  (/ default-temperature (log (+ trial-count 1)))
  )
; Function to call to update temperature each trial
(parameter 'temperature-schedule #'log-trials)

; If true, learn for open-world predicates as well as closed-world ones
(parameter 'learn-open nil)

; If true, learn for conditional functions that have no normal variable
(parameter 'learn-no-normal nil)

; Whether to print regions on separate lines
(parameter 'print-regions-on-separate-lines nil)

; When true, use a relative rather than an absolute epsilon in equality tests
(parameter 'use-relative-epsilon t)

; Center discrete numeric (unit) regions on the integer (rather than starting them on the integer)
(parameter 'center-discrete-numeric-on-integer nil)

; Whether to run with multiple agents
(parameter 'multiagent nil)

; Whether to use automatic condition reordering
(parameter 'automatically-reorder-conditions t)

; Learn function factors via gradient descent
(parameter 'learn-via-gradient-descent nil)

; Whether to use new form of subtractive normalization in gradient-descent learning
(parameter 'gdl-subtractive-normalization t)

; Divide gradient by function-message product
; Seems to be required for correctness, but is causing problems
(parameter 'gdl-divide 'newer)

; Trace gradient-descent learning (T, NIL, or a list of node numbers, node names, or conditional names)
(parameter 'trace-gdl nil)

; Subtract average from gradient before using
(parameter 'gradient-subtract-average t)

; Use exponential-product form of gradient
; When do this, should set gradient-subtract-average to NIL and ensure that gdl-divide is NEWER
(parameter 'exponential-product-gradient nil)

; Send feedback messages to function factor nodes rather than optimizing them out
(parameter 'feedback-to-functions t)

; Learning rate for gradient ascent
(parameter 'learning-rate .05)

; Whether to use an adaptive learning rate, rather than fixed parameter
(parameter 'adaptive-learning-rate nil)

; Whether to use an adaptive learning rate, rather than fixed parameter
(parameter 'learning-rate-fraction-of-smoothing-parameter nil)

; Minimum value created in a learned function
(parameter 'smoothing-parameter .000001)

; Whether to do adaptive smoothing, rather than using the fixed parameter
(parameter 'adaptive-smoothing t)

; Maximum increment used in gradient descent learning (if specified)
(parameter 'max-gdl-increment 1)

; Turn chunking on
(parameter 'chunking t)

; Trace chunking
(parameter 'trace-chunking t)

; Predict results of diachronic activity (results of operators/actions)
(parameter 'diachronic-prediction nil)

; Determine if should do episodic learning
; Can be t, open, closed or nil
(parameter 'episodic-learning nil)

; Determine if should automatically create conditionals for action modeling (aka transition function learning)
(parameter 'automatic-action-models nil)

; Determine if should automatically create conditionals for perception modeling (including maps)
(parameter 'automatic-perception-models nil)

; Determine if should automatically create predicates and conditionals for reinforcement learning
(parameter 'automatic-reinforcement-learning nil)

; What kind of selection to perform among best operators: 'first, 'random
(parameter 'operator-best-select 'random)

; What kind of selection to perform among best values for all other unique variables: 'first, 'random
(parameter 'non-operator-best-select 'random)

; Determine if trace episodic learning
(parameter 'trace-el nil)

; When true, ensure that universal variables don't get non-zero weights in region functions
(parameter 'unique-weights-only t)

; Determine whether to make condition beta networks one direction
(parameter 'one-way-c-a-betas t)

; Determine whether to only reinitialize messages at a node when no WM or CFF factor node
; upon which it depends has changed
(parameter 'save-message-state t)

; Track messages across decisions
(parameter 'track-graph-messages nil)

; Copy state automatically on impasses
; Presently only works in single agent mode
(parameter 'impasse-copy-state nil)

; Trace performance
(parameter 'trace-performance t)

; Debug init-node-descendants-no-parents
; Other values are 'summary (actually anything other than nil or 'all) and 'all
(parameter 'debug-init-descendants nil)

; Debug descendant/message-saving code
(parameter 'debug-descendants nil)

; Trace preferences existing prior to decisions
(parameter 'trace-preferences nil)

; Trace perception
(parameter 'trace-perception nil)

; Trace depths of links on messages
(parameter 'trace-link-depths nil)

; Trace queues into which new messages are being placed
(parameter 'trace-queue nil)

; Whether to always use max for summarizing out action/operator variables
(parameter 'always-max-operators nil)

; Default value for use in creating function factors
(parameter 'function-default 0)

; If true, treat open-world actions like latter half of condacts, otherwise like closed-world actions
(parameter 'open-actions-like-condacts nil)

; If true, connect open-world WMFN directly to WMVN for open-world conditions
(parameter 'open-conditions-like-condacts nil)

; If true, pad filter outputs with 1s for messages going through condacts away from WM
; This is already automatically done for messages going to WM
(parameter 'all-condact-filters-pad-1 nil)

; Threshold that defines the number of connections required to be considered as a dense connection. 
; If the number of connections is bigger than this threshold, a subgraph constructed for efficient processing (that decreases the number of multiplications required) 
(parameter 'threshold-for-dense-connection 5)

; Minimum size of a field for a PLM array print
(parameter 'minimal-parray-field 14)

; Default to printing trace information to listener
(defvar trace-stream)

; Initialization variables for regression testing
(defparameter from-regression nil)
(defvar regression-interface nil)


; Initialization variables for parallel implementation
(defparameter from-parallel nil)
;(defvar *parallel-interface* nil)

;Stream where the information about the output will be displayed
(defparameter *info-stream* t)
(defvar info-stream *info-stream*)

; Base-level state at which processing starts in reflective hierarchy
(parameter 'base-level-state 0)

; Whether to latch evidence in WM VN nodes, implying that products with evidence 1s yields 1
(parameter 'latch-evidence t)

; If serial, use message queue to process one message at a time
; If parallel, perform elaboration cycles in which messages are processed for all links before moving on
(parameter 'message-protocol 'serial)

; Whether to trace wm-driven
(parameter 'trace-wm-driven nil)

; Default to integral in summary-product, rather than maximize
(parameter 'default-integral t)

; Default to nil for tracing product, summarize and normalize
(parameter 'trace-combine nil)
(parameter 'trace-summarize nil)
(parameter 'trace-transform nil)
(parameter 'trace-average nil)

; Whether to trace affine computations
(parameter 'trace-affine nil)

; Whether to trace impasses
(parameter 'trace-impasses t)

; Default to nil tracing for maximals
(parameter 'trace-maximals nil)

; Constant for testing epsilon equality of real numbers
(parameter 'epsilon .0000001)
; Constant used for delimiting a maximal region at the end of an interval
(parameter 'epsilon2 .0001)

; Constant added in at the FAN node for open-world predicates
; Replaces the constant evidence that used to be put explicitly into open-world WMs when there were actions
(parameter 'fan-constant 1e-10)

; Trace the process of deciding what changes to make in WM
(parameter 'trace-wm-changes nil)
; Trace addition and deletion of states
(parameter 'trace-states t)

; Variables for tracing message generation times
(parameter 'trace-message-times nil)

; Default maximum number of (parallel) elaboration cycles per decision
(parameter 'max-elaboration-cycles 50)

; Maximum number of decisions to run if no number is specified
(parameter 'max-decisions 500)

; Determine whether to explicitly specify function variable names
(parameter 'specify-function-variable-names t)

; Number of categories to use in unsupervised learning
(parameter 'unsup-categories 5)

; Maximum number of items in the final region along a discrete dimension to list explicitly
(parameter 'max-final-discrete 10)

; Extent of field beyond length of largest label
(parameter 'field-beyond-label 3)

; Extent of range field beyond twice length of largest label
(parameter 'range-field-beyond-label 3)

; Break when a message specified by a pair of node numbers is non-empty 
; List of lists of pairs of node numbers
(parameter 'break-on-positive-messages nil)

; Discount messages from WM through VAN to FAN
(parameter 'discount-wm t)

; Discount factor to use for WM through VAN to FAN
(parameter 'wm-discount-factor .01)

; Whether to create WMFN nodes for open-world predicates
(parameter 'open-world-wmfns nil)

; Arousal level
(parameter 'arousal nil)

; Integrate over universal variables if there are any unique variables in function
(parameter 'integrate-universal-in-unique nil)

; Whether to automatically calculate surprise over learned functions
(parameter 'compute-surprise nil)

; Whether to automatically calculate progress over goals
(parameter 'compute-progress nil)

; Whether to automatically calculate attention
(parameter 'compute-attention nil)

; Whether to trace attentional processing
(parameter 'trace-attention nil)

; Maximum span of a symbolic region before use print-plm rather than parray in print-smart
(parameter 'max-span-pa 10)

; Max density fraction of PLM for which use regions in arrays within print-smart
(parameter 'max-fraction-pa-regions .5)

; When doing non-array printing of PLMs, don’t print regions whose constants (in piecewise-constant PLMs) are less than this value (if it is non-nil).
(parameter 'trace-threshold nil)

;(parameter 'sparse-product t)

;(parameter 'sparse-product-threshold 2)

; The following are special, as they don't get reset by reset-parameters

; Whether to reset-parameters during init
(defparameter reset-parameters-in-init t)

; Whether to track time
(defparameter track-time t)

(defun reset-parameters nil
  (dolist (gp global-parameters)
    (unless (member (car gp) parameter-override-reset)
      (eval `(setq ,(car gp) ,(cdr gp)))
      )
)
  (setq trace-stream t))

; -----------------------------------------------------------
; Global variables

; Graph
(defvar cg)

; Bottom state
(defvar bottom-state)

; Track number of messages that have been processed
(defvar message-count 0)

; Track number of decisions that have been made
(defvar decision-count 0)

; Temperature variables for Boltzmann selection
(defvar temperature default-temperature)
(defvar one-over-temperature (/ 1 temperature))
  
(defvar epsilon21) ; Used whether multiple epsilon2 regions

; Relative epsilon (gets refined as types are defined)
(defvar relative-epsilon)

; Global variable for smooth-plm subregions to be zeroed out
(defvar smooth-plm-subthreshold-regions)

; Messages tracked across across decisions
(defvar global-graph-messages nil)

; Value is NIL after init until first run
(defvar have-run nil)

; Forms to execute before run and before and after a decision and a trial (and before all trials and for perception and action)

(defvar perceive-list nil)
; Execute perceive-list forms
(defun perceive-list nil
  (mapc #'eval perceive-list)
  t)

(defvar action-list nil)
; Execute action-list forms
(defun action-list nil
  (mapc #'eval action-list)
  t)

(defvar pre-run nil)
; Execute pre-run forms
(defun pre-run nil
  (mapc #'eval pre-run)
  t)

(defvar post-run nil)
; Execute post-run forms
(defun post-run nil
  (mapc #'eval post-run)
  t)

(defvar pre-d nil)
; Execute pre-d forms
(defun pre-d nil
  (mapc #'eval pre-d)
  t)

(defvar post-d nil)
; Execute post-d forms
(defun post-d nil
  (mapc #'eval post-d)
  t)

(defvar pre-t nil)
; Execute pre-t forms
(defun pre-t nil
  (mapc #'eval pre-t)
  t)

(defvar post-t nil)
; Execute post-t forms
(defun post-t nil
  (mapc #'eval post-t)
  t)

(defvar post-automatic nil)
; Execute post-automatic forms
(defun post-automatic nil
  (mapc #'eval post-automatic)
  t)

(defvar pre-ts nil)
; Execute pre-ts forms
(defun pre-ts nil
  (mapc #'eval pre-ts)
  t)

; Trace of performance
(defvar global-decision-statistics nil)

; Global variable for tracking which parts of the selected predicate need to be wiped after
; a change to a unique predicate that invovles the state
(defvar selected-operator-wipe)

; Variables for tracing message generation times
(defvar number-of-messages 0)
(defvar sum-of-message-times 0)
(defvar maximum-message-time 0)

; Whether should add a new state/subgoal during the current decision
(defvar add-new-state)

; Which state to flush everything below, if any
(defvar delete-lower-states)

; Global variable for impasse regions
(defvar impasse-regions)

; List of attributes (and category name) in data set
(defvar data-set-attributes nil)

; Category name in data set
(defvar data-set-category nil)

; List of conditional names for which shouldn't learn
(defvar do-not-learn nil)

; Temporarily turn off error detection for defining system predicates
(defvar define-system-predicate nil)

; Constant for use in computing attention-multiplier (will get set in init)
(defvar attention-exponential-numerator 0)

; -----------------------------------------------------------
; Structures

; A graph consists of a name, lists of predicates nodes and links,
; a list of conditional patterns, an association lists of avars and memories,
; the queues of messages to send
; agents is number of agents
(defstruct graph types predicates state-predicates nodes links conditionals pattern-vars queues node-count last-node graph changes positive-preferences negative-preferences depth agents operator-type-name initialized operator-predicates selected-predicate node-vector detect-impasses-predicate goals-set goal-list action-predicate perception-predicate agent-type surprise-predicates surprise-distribution-predicate surprise-predicate attention-predicates attention-distribution-predicate attention-predicate progress-predicates difference-predicates difference-distribution-predicate difference-predicate progress-distribution-predicate progress-predicate predicate-type)

; A queue is a list with pointers to a head element and the sublist containing last element (tail)
; to facilitate adding (to the back) and removing (from the front) elements 
; The head element remains, and the first content is really the next element
(defstruct queue head tail)

; A node (either variable or factor) consists of a name,
; a list of links, a type (variable or factor),
; a subtype (alpha, beta, delta, etc.) that isn't used during processing, only during printout
; arguments is only used for WM variable nodes of condacts, to track the argument list for automated sharing
; a function (a PLM or a special symbol if factor node)
; an inverse-function (the name of the inverse function, if any, for a filter node)
; a vector of variables that are part of the node,
; If the node is a factor, factor-steps defines the sequence of operations to be performed
; This is one place at least where a hierarchical class system would help in specializing two types of nodes
; Evidence t means that a factor node is constant and that a variable node only is to receive input from
; its constant companion
; Action determines if this is a special action node for combining shared actions
; Descendants is a list of descendants
; Descendant-links is a list of descendant links
; Region-pad is the pad to use in an affine node
; Normal is the dimension number for the variable to normalize over during learning, if any
; Conditional-name gives the conditional name for conditional function nodes
; Linear-filter is T if the node is a filter and at least one dimension has a coefficient
; If integral is T then unique varibles in this node are integrated out, otherwise they are maxed
; Changes is number of itmes a conditional factor function has been changed at node (if any)
; Shared-functions is a list of other (CFF) nodes that share the same function
; Variables-same is list of adjacent variable node names for a beta factor with the same variables (in the same order)
; vector if the node performs vector normalizations 
;;Deprecated normalize t,v, or nil: if t, l1 normalization. if v, l2 (or vector) normalization
(defstruct node name type subtype subsubtype object evidence variables factor-steps function inverse-function links number action descendants descendant-links region-pad normal conditional-name linear-filter pattern-type exponential normalize integral predicate learning-rate smoothing-parameter restriction changes shared-functions function-name unique-for-gdl wmvn beta-non-full assumption vector surprise-predicate)

; A factor-step specifies one step of the processing within a factor node
; The type of step can be product or integral
; If type is product, the argument is the link to be multiplied times the current running value
; If type is integral, the argument is a variable to integrate out
(defstruct factor-step type argument)

; Structure used to define connections for a beta factor
(defstruct beta-memories alpha old-beta new-beta)

; A descendant is a node and a set of parents by which it is descended from some other node
(defstruct descendant node parents)

; A descendant-link is a link and a set of parents by which it is descended from some other node
(defstruct descendant-link link direction from to)

; Elements of a backtrace for chunking
(defstruct backtrace link direction from-vars to-vars)

; Elements of dependency structure for chunking
(defstruct dependency message predecessors listed)

; A link consists of a point to the graph it is part of,
; an array of two nodes, an array of two messages (contents),
; an array of two numbers corresponding to the depths of the link in those directions
; an array of two numbers corresponding to the loop-depths of the link in those directions
; an array of two numbers corresponding to when message last initialized, and an array of two incoming
; The first message goes from the first node, and the second from the second
; The first node is always the variable node
; If the contents is nil, no message is sent in that direction
; Incoming specifies whether there is an incoming message in parallel mode
; If there is a T in the 0th/Variable cell that means there is an incoming message from (NOT to) the variable node
; map specifies how variable node variables map onto variables in factor function
; counts is an array of two numbers for messages sent in each direction
; Stale keeps track, for the messages in each direction, of whether the message needs to be recomputed when sent (because an incoming message has changed)
(defstruct link map nodes contents depths loop-depths inits prequeue incoming in out counts stale variables-same dependency)

; A message is a link plus an index into an element of the contents array
; and node array (node message is from)
(defstruct message index link wm-driven key didx)

; A map is a vector of size equal to the number of variables in the corresponding variable node
; vfactor gives the factor node variable index from a variable node variable index
; This earlier also had vpoint and fpoint, as defined below, but they have never been used
; vpoint gives the same mapping but for use in indexing a point (with constant in 0 position) from variable indices
; fpoint sorts the pvf mapping for use in indexing a point from factor indices
; Have added fvar, which maps from summarized product in factor node back to variable node
(defstruct smap vfactor fvar omitted)

; A conditional consists of a name, lists of conditions, condacts and actions, and a function
; Function-variable-names specifies the variable names (and ordering) used in the function
; Normal is the variable name to normalize over during gradient-descent learning, if any
; If map is true, then use max rather than integral for summarization at factor nodes within this conditional
(defstruct conditional name conditions condacts actions variables function-variable-names function condition-later-variable-names condact-later-variable-names action-later-variable-names last-memory last-condition-memory alpha-memories function-node episodic normal function-default shared-action-beta map learning-rate smoothing-parameter reordered-conditions)

; Each wme must now be an instance of a predicate, taking the place of attributes
; arguments is a list of sublists.  The car of a sublist is the argument name, while
; the cadr is the name of a type, and the caddr (if exists) is specifies form of unique variable
; wm is the working memory factor for this predicate
; em is the function factor for this predicate (if it has one)
; em-predicate is the predicate used for this predicates episodic memory, if there is one
; persistent is whether the contents of WM are persistent
; predict is the predicate for diachronic prediction of current predicate
; exponential is if WMFN should transform (the constant part of) its output messages exponentially
; assign-ids is the function to be used in assigning ids to alternatives in predicate if it is an operator
; no-normalization, if T, overides default normalization coming out of WM VN nodes
; unique is the name of the unique argument name, and nil if a multiple predicate
; select is nil, best, prob-match, boltzmann or expected
; cumulative determines whether persistent
; perception is the factor node where perceptions go
; prediction is the predicate being predicted if this is a prediction predicate
; predicted is the original predicate that this predicate is used to predict, if there is one
; perception-temp-mem is temporary memory for aggregating the perceptions before putting them into the factor node
; vector is whether this predicate represents a vector 
(defstruct predicate name world arguments wm em em-predicate persistent predict exponential assign-ids operators first-operator id-contents outgoing-vn incoming-vn fan no-normalize unique select replace agent-arg-index perception prediction perception-temp-mem prediction-link wm-variables function learning-rate smoothing-parameter function-variable-names function-node function-default condact-conditionals unique-for-gdl episodic no-models goal-predicate progress-predicate action-function perception-function vector surprise-predicate automated-perception no-surprise attention-predicate goal difference-predicate)

(defconstant vector-symbol '[]) ; Unique variable for that actions put vector into WM 

; Is symbol vector
(defun vector-argument (a) 
  (eq (argument-symbol a) vector-symbol)
  )
; Determine what form of selection argument specifies
(defun argument-vector (a) 
  (vector-argument a)
  )

; Take the sqrt of the function (only for piecewise-constants
(defun sqrt-function (f r &optional piecewise-constant) 
  r ; dummy so no warning about r not being used
  (if piecewise-constant
      (sqrt f)
    f)
  )


; Working memory, goal memory, and all messages should be PLMs
; A PLM is an N dimensional array of regions
; Active is a Boolean vector saying which variables are active
; Variables is a vector of variables in the PLM
; Array should point to the array of regions
; Slices is a vector of length rank, each with an ordered list of slices for the dimension
(defstruct plm rank active variables array slices removed-unneeded-slices piecewise-constant)

; A region tells you everything you need to know about an N dimensional rectangle of a PLM
; This includes an N dimensional linear function and pointers to adjacent regions
; Dimensions should be a vector of dimension structures
; As opposed to earlier versions, the constant is separated from the dimensions
; left and right are used as temporary storage when the region is bisected until
; the new regions are linked directly into the PLM (in which case old region goes away)
; Evidence specifies whether evidence was provided for the region
; Exponential is true if the actual value is e to the function
(defstruct region constant dimensions maximals evidence exponential bad)

; A dimension provides the information about one direction along a region
; This includes which regions come before and after it along a direction
; Min-slice and max-slice are slices that bound the region along this direction
; Weight gives the coefficient for this dimension/variable in the linear function
(defstruct dimension min-slice max-slice weight discrete)

; A slice is a simple structure just including the location of the slice
(defstruct slice location index)

; Each variable will now be of a type
; Types can be symbolic (numeric false and discrete true), integral (numeric true and discrete true)
; or continuous (numeric true and discrete false).  Max is always an open end for an interval
; For non-numeric variables, constants is a list of values, min is at 0 and max is at #constants+1
; Unit-width is width of the smallest region
(defstruct stype name numeric discrete constants min max span)

; These are the variables used in PLMs and nodes
; unique is T if a unique variable (not multiple)
; select is nil, best or expected
(defstruct svariable name type unique select)

; Structure for affine transformation of PLM variable
; From is the predicate argument being transformed
; To is the predicate argument where the transforms end up
; Coefficent is multiplied times "from" slices
; Offset is added to "from" slices
; Pad, if provided, is how to pad PLM when new regions are introduced
; When apply-coefficient-to-offset is true, apply coefficient to offset as well as to base variable
(defstruct affine from to coefficient offset pad apply-coefficient-to-offset not-equal)

; Trace performance
(defstruct decision-statistics number messages run-time decision-time learn-time init-time)

; A pattern/alpha variable data structure
; Includes name of the pattern/alpha variable, the wm-wmemory variable node,
; A list of the variable names in the node
(defstruct pattern-variable name wm-memory variable-names)

; -----------------------------------------------------------
; Initialization functions

; Conditionals
; ------------

; Initialize a conditional in a graph
(defun conditional (name &rest args)
  (let (nc)
    (setq nc (apply #'make-conditional (append (list ':name name) args)))
    (compile-conditional nc)
    (when (graph-initialized cg) ; This is a conditional added after the graph has been initialized
      (post-process-conditionals)
      )
    nc)
  )

; Find a conditional given its name
(defun conditional-from-name (name)
  (find-if #'(lambda (c) (eq name (conditional-name c))) (graph-conditionals cg))
  )

; Types (stypes)
; --------------

; Initialize a type in the current graph (cg)
(defun new-type (name &rest args)
  (when (member name (graph-types cg) :key #'stype-name)
    (error "Type ~S cannot be redefined!" name)
    )
  (let (nt)
    (setq nt (apply #'make-stype (append (list ':name name) args)))
    (cond ((not (stype-numeric nt)) ; A constant/symbolic type
           (setf (stype-discrete nt) t)
           (setf (stype-min nt) 0)
           (setf (stype-max nt) (length (stype-constants nt)))
           )
          ((and center-discrete-numeric-on-integer (stype-numeric nt) (stype-discrete nt)) ; A discrete numeric type where regions are to be centered around integers
           (setf (stype-min nt) (- (stype-min nt) 1/2))
           (setf (stype-max nt) (- (stype-max nt) 1/2))
           )
          )
    (when (< (stype-max nt) (stype-min nt))
      (error "Maximum (~S) for type ~S is less than minimum (~S)" (stype-max nt) name (stype-min nt))
      )
    (cond ((stype-discrete nt)
           (when (> (stype-max nt) infinity)
             (error "Maximum (~S) for discrete type ~S is greater than (logical) infinity (~S)" (stype-max nt) name infinity)
             )
           (when (< (stype-min nt) (- infinity))
             (error "Minimum (~S) for discrete type ~S is less than (logical) negative infinity (~S)" (stype-min nt) name (- infinity))
             )
           )
          (t
           (when (> (stype-max nt) continuous-max)
             (error "Maximum (~S) for continuous type ~S is greater than continuous max (~S)" (stype-max nt) name continuous-max)
             )
           (when (< (stype-min nt) (- continuous-max))
             (error "Minimum (~S) for continuous type ~S is less than negative continuous-max (~S)" (stype-min nt) name (- continuous-max))
             )
           )
          )
    (setf (stype-span nt) (abs (- (stype-max nt) (stype-min nt))))
    (setf (graph-types cg) (cons nt (graph-types cg)))
    ; For continuous types, determine if need to adjust relative-epsilon
    (unless (stype-discrete nt)
      (setq relative-epsilon (min relative-epsilon (* epsilon2 .1 (/ 1 (max 1 (abs (stype-min nt)) (abs (stype-max nt)))))))
      )
    nt)
  )

; Find a type from a name
(defun type-from-name (name)
  (find name (graph-types cg) :key #'stype-name))

; Find a type from a predicate and argument
(defun type-from-predicate-argument (argument predicate)
  (type-from-name (cadr (assoc argument (predicate-arguments predicate))))
  )

; Variables (svariables)
; ----------------------

; Determine if a variable is unique
(defun best-variable (v)
  (and (svariable-unique v)
       (eq (svariable-select v) 'best)
       )
  )
; Determine if a variable is expected value
(defun expected-variable (v)
  (and (svariable-unique v)
       (eq (svariable-select v) 'expected)
       )
  )
; Determine if a variable is distribution value
(defun distribution-variable (v)
  (and (svariable-unique v)
       (not (svariable-select v))
       )
  )
; Determine if a variable needs a selection (either unique or expected)
(defun selection-variable (v)
  (and (svariable-unique v)
       (svariable-select v)
       )
  )

; Determine if a variable is universal
(defun multiple-variable (v)
  (not (svariable-unique (if (listp v) (car v) v)))
)
; Determine if a variable is non-universal
(defun non-multiple-variable (v)
  (not (multiple-variable v))
  )

; Find a variable from a name
(defun variable-from-name (name variables)
  (find name variables :key #'svariable-name))

; Union of two vectors of variables
(defun union-two-variables (vs1 vs2)
  (let* ((r1 (length vs1))
         (r2 (length vs2))
         (uvs (make-array (list (+ r1 r2)) :adjustable t :fill-pointer 0)))
    (dotimes (i r1)
      (vector-push (elt vs1 i) uvs))
    (dotimes (i r2)
      (unless (find (svariable-name (elt vs2 i)) uvs :key #'svariable-name)
        (vector-push (elt vs2 i) uvs)))
    (adjust-array uvs (list (fill-pointer uvs)))
    uvs)
  )

(defun union-variables (var-vectors)
  (let ((union (car var-vectors)))
    (dolist (var-vector (cdr var-vectors))
      (setq union (union-two-variables var-vector union)))
    union)
  )

; Arguments
; ---------

; Find whether an argument is multiple within a predicate
(defun multiple-from-predicate-argument (argument predicate)
  (multiple-argument (assoc argument (predicate-arguments predicate)))
  )

; Find the position in the argument list from an argument name
(defun position-of-predicate-argument (argument predicate)
  (position argument (predicate-arguments predicate) :key #'car)
  )

; Does predicate have an argument of type operator?
(defun has-operator-argument (p)
  (dolist (arg (predicate-arguments p) nil)
    (when (eq (cadr arg) (graph-operator-type-name cg))
      (return t)
      )
    )
  )

; Define access functions for arguments
(defun argument-name (a) (car a))
(defun argument-type-name (a) (cadr a))
(defun argument-tail (a) (cddr a))
(defun argument-symbol (arg) (caddr arg))

; Is symbol unique
(defun best-argument (a)
  (eq (argument-symbol a) best-symbol)
  )
; Is symbol distribution
(defun distribution-argument (a)
  (eq (argument-symbol a) distribution-symbol)
  )
; Is symbol probability matching
(defun prob-match-argument (a)
  (eq (argument-symbol a) prob-match-symbol)
  )
; Is symbol exponential
(defun exponential-argument (a)
  (eq (argument-symbol a) exponential-symbol)
  )
; Is symbol expected
(defun expected-argument (a)
  (eq (argument-symbol a) expected-symbol)
  )
; Is symbol unique or expected
(defun selection-argument (a)
  (or (best-argument a) (expected-argument a))
  )
; Is argument multiple
(defun multiple-argument (a)
  (not (argument-tail a))
  )
; Is argument unique (not multiple)
(defun unique-argument (a)
  (argument-tail a)
  )

; Determine what form of selection argument specifies
(defun argument-select (a)
  (cond ((or (multiple-argument a)
             (distribution-argument a)
             (exponential-argument a)
             )
         nil)
        ((best-argument a) 'best)
        ((expected-argument a) 'expected)
        ((prob-match-argument a) 'prob-match)
        )
  )

; Determine if argument is state
(defun state-argument (a)
  (eq (argument-type-name a) 'state)
  )

; Predicates
; ----------

; Check whether all of the argument types in the predicate are defined
; Check that there is at most one unique argument
; Set unique and select attributes of predicate
; Check other things...
(defun check-predicate-arguments (pred)
  (let (type as agent-arg-names last-agent-arg-name state-arg-name)
    (when (eq (predicate-select pred) 'boltzman)
      (format trace-stream "Boltzman has been changed to Boltzmann in predicate ~S." (predicate-name pred))
      (setf (predicate-select pred) 'boltzmann)
          )
    (when (not (member (predicate-select pred) '(nil best prob-match boltzmann expected)))
      (error "Attempt to set selection type for predciate ~S to ~S, which is not NIL, BEST, PROB-MATCH, BOLTZMANN or EXPECTED."
             (predicate-name pred) (predicate-select pred))
      )
    (dolist (arg (predicate-arguments pred))
      ; Check whether argument type is defined
      (setq type (argument-type-name arg))
      (unless (type-from-name type)
        (error "Unknown argument type ~S in definition of predicate ~S." type (predicate-name pred))
        )
      ; Mark predicate as computing an exponential
      (when (exponential-argument arg)
        (setf (predicate-exponential pred) t)
        )
      ; Gather agent argument names in a multigent situation
      (when (and multiagent (eq (argument-type-name arg) 'agent))
        (setq agent-arg-names (cons (argument-name arg) agent-arg-names))
        )
      ; Error if predicate has more than one state argument
      (when (eq (argument-type-name arg) 'state)
        (if state-arg-name ; There already is an agent argument
            (error "Multiple state arguments in predicate ~S." (predicate-name pred))
          (setq state-arg-name (argument-name arg)))
        )
      (when (unique-argument arg)
        (setq as (argument-select arg))
        (when (and as (predicate-unique pred) (not (predicate-select pred))) ; Current argument is selection, but a previous one is unique without being selection
          (error "Mixture of selection and unique non-selection argruments found in predicate ~S." (predicate-name pred))
          )
        (when (and (predicate-select pred) ; There is already a form of selection specified
                   (unique-argument arg) ; A symbol has been specified
                   (not (eq (predicate-select pred) as)) ; The new one isn't the same as the existing one
                   )
          (error "Multiple incompatible forms of predicate selection specified in predicate ~S." (predicate-name pred))
          )
        (setf (predicate-unique pred) (adjoin (argument-name arg) (predicate-unique pred)))
        (setf (predicate-select pred) as)
        (setf (predicate-vector pred) (argument-vector arg)) 
        )
      )
    ; Print warning if multiple agent arguments and none is named AGENT
    (when agent-arg-names ; There already is an agent argument
      (if (= (length agent-arg-names) 1) ;If only one, use it
          (setf (predicate-agent-arg-index pred) (position (car agent-arg-names) (predicate-arguments pred) :key #'argument-name))
        (if (member 'agent agent-arg-names) ; If multiple, and there is one name agent, use it
            (setf (predicate-agent-arg-index pred) (position 'agent (predicate-arguments pred) :key #'argument-name))
          (progn ; Otherwise use first
            (setq last-agent-arg-name (car (last agent-arg-names)))
            (setf (predicate-agent-arg-index pred) (position last-agent-arg-name (predicate-arguments pred) :key #'argument-name))
            (format trace-stream "~&Warning: Multiple arguments of type AGENT in predicate ~S, with none being named AGENT.  The first one (~S) is therefore assumed to be primary."
                    (predicate-name pred) last-agent-arg-name)
            )))
      )
    ; Error if state predicate has no agent argument in multiagent situation
    (when (and multiagent state-arg-name (not agent-arg-names))
      (error "State predicate (~S) in multiagent domain lacks an agent argument." (predicate-name pred))
      )
    (when (and (not (predicate-persistent pred)) (predicate-select pred))
      (format trace-stream "~&Non-persistent predicates don't do selection so this has been disabled for predicate ~S." (predicate-name pred))
      (setf (predicate-select pred) nil)
      )
    (when (and (not (predicate-persistent pred)) (predicate-replace pred) (not (predicate-prediction pred)))
      (format trace-stream "~&Non-persistent predicates don't do replacement so predicate ~S has been returned to the default of cumulative." (predicate-name pred))
      (setf (predicate-replace pred) nil)
      )
    (when (and (predicate-replace pred) (predicate-select pred))
      (format trace-stream "~&Replacement doesn't make sense for selection predicates, so it has been ignored in predicate ~S." (predicate-name pred))
      (setf (predicate-replace pred) nil)
      )
    (when (and (predicate-replace pred) (predicate-perception pred) (not diachronic-prediction))
      (format trace-stream "~&Perception not (currently) allowed for replacement predicates, so it has been ignored in predicate ~S." (predicate-name pred))
      (setf (predicate-perception pred) nil)
      )
    )
  )

; Whether a predicate is universal (no unique variables)
(defun predicate-universal (pred)
  (not (predicate-unique pred))
  )

; Whether a predicate accumulates values from the WMFN and actions (not replace)
(defun predicate-cumulative (pred)
  (not (predicate-replace pred))
  )

; Convert a predicate argument into a conditional element with a variable
(defun argument-to-element (arg)
  (list (car arg) (list (car arg)))
  )

; Tests on predicate's world
(defun open-world (pred) (eq (predicate-world pred) 'open))
(defun closed-world (pred) (eq (predicate-world pred) 'closed))

; Get named predicate definition from graph
(defun predicate-from-name (name &optional dont-error)
  (let (pred)
    (setq pred (find name (graph-predicates cg) :key #'predicate-name))
    (when (and (not dont-error) (not pred))
      (error "No predicate found for name ~S in predicate-from-name." name)
      )
    pred)
  )

; Get predicate's perception node from name
(defun perception-node-from-name (name)
  (let ((pred (predicate-from-name name)))
    (unless pred
      (error "No predicate named ~S in PERCEPTION-NODE-FROM-NAME" name)
      )
    (predicate-perception pred)
    )
  )

; Replace selection argument with distribution for prediction
(defun prediction-argument (arg)
  (if (selection-argument arg)
      (list (argument-name arg) (argument-type-name arg) distribution-symbol)
    arg)
  )

(defun convert-to-wm-variable-name (name)
  (concat-symbols (list 'wm name) t)
  )

; Set up a goal (and a progress, difference and attention predicates) for a predicate
(defun goal (pred-name goal)
  (let ((pred (predicate-from-name pred-name))
        (goal-predicate-name (concat-symbols (list pred-name '*goal)))
        )
    (setf (predicate-goal-predicate pred) (predicate goal-predicate-name
                                                     :world 'closed :unique (predicate-unique pred)
                                                     :select (if (and (open-world pred) (predicate-unique pred)) 'best (predicate-select pred)) 
                                                     :replace (predicate-replace pred) :episodic (predicate-episodic pred)
                                                     :arguments (if (and (open-world pred) (predicate-unique pred))
                                                                    (make-unique-arguments-select (predicate-arguments pred))
                                                                  (predicate-arguments pred))))
    (setf (graph-goal-list cg) (cons goal (graph-goal-list cg)))
    (when compute-progress
      (setf (predicate-progress-predicate pred)
            (if (closed-world pred)
                (predicate (concat-symbols (list pred-name '*progress)) :perception t :arguments (meta-arguments pred)
                           :no-surprise t :no-normalize t :function 1
                           )
              (predicate (concat-symbols (list pred-name '*progress)) :perception t :arguments (meta-arguments pred)
                         :no-surprise t :function 1
                         )))
      (setf (predicate-difference-predicate pred)
            (if (closed-world pred)
                (predicate (concat-symbols (list pred-name '*difference)) :perception t :arguments (meta-arguments pred)
                           :no-surprise t :no-normalize t :function 1
                           )
              (predicate (concat-symbols (list pred-name '*difference)) :perception t :arguments (meta-arguments pred)
                         :no-surprise t :function 1
                         )))
      ; Set up global progress
      (setf (graph-progress-predicates cg) (cons pred (graph-progress-predicates cg)))
      (unless (graph-progress-distribution-predicate cg)
        (setf (graph-progress-distribution-predicate cg) (predicate 'progress*distribution :perception t :no-normalize t
                                                       :arguments (if multiagent '((agent agent) (value predicate %)) '((value predicate %)))))
        (setf (graph-progress-predicate cg) (predicate 'progress :perception t :no-normalize t :arguments (if multiagent '((agent agent)) '())))
        )
      ; Set up global difference
      (setf (graph-difference-predicates cg) (cons pred (graph-difference-predicates cg)))
      (unless (graph-difference-distribution-predicate cg)
        (setf (graph-difference-distribution-predicate cg) (predicate 'difference*distribution :perception t :no-normalize t
                                                         :arguments (if multiagent '((agent agent) (value predicate %)) '((value predicate %)))))
        (setf (graph-difference-predicate cg) (predicate 'difference :perception t :no-normalize t :arguments (if multiagent '((agent agent)) '())))
        )
      ; Create an attention predicate if not already one
      (when (and compute-attention (not (predicate-attention-predicate pred)))
        (setf (predicate-attention-predicate pred)
              (predicate (concat-symbols (list pred-name '*attention)) :no-surprise t :perception t :function 1
                         :arguments (meta-arguments pred)))
                 ; Set up global attention
        (setf (graph-attention-predicates cg) (cons pred (graph-attention-predicates cg)))
        (unless (graph-attention-distribution-predicate cg)
          (setf (graph-attention-distribution-predicate cg) (predicate 'attention*distribution :perception t :no-normalize t
                                                          :arguments (if multiagent '((agent agent) (value predicate %)) '((value predicate %)))))
          (setf (graph-attention-predicate cg) (predicate 'attention :perception t :no-normalize t :arguments (if multiagent '((agent agent)) '())))
          )
        )
      )
    )
  )

; Initialize a predicate definition in a graph
(defun predicate (name &rest arguments)
  (when (member name (graph-predicates cg) :key #'predicate-name)
    (error "Predicate ~S cannot be redefined!" name)
    )
  (when (and (not define-system-predicate) (member name system-predicates))
    (error "Attempt to define a predicate (~S) that is a reserved system predicate name!" name)
    )
  (let (np sp sp-perception function-node gpt)
    (setq np (apply #'make-predicate (append (list ':name name) arguments)))
    (unless (predicate-world np) ; Default to open world if not specified
      (setf (predicate-world np) 'open)
      )
    (unless (member ':persistent arguments) ; Default to persistent for closed world and not for open world
      (setf (predicate-persistent np) (if (closed-world np) t nil))
      )
    (check-predicate-arguments np)
    ; If this is a replacement perception predicate, make the predicate itself non-perception but the prediction predicate (if there is one) perceptual
    (when (and (predicate-replace np) (predicate-perception np) diachronic-prediction)
      (setf (predicate-perception np) nil)
      (setq sp-perception t)
      )
    ; It is important that the selected predicate be last in the list for impasse processing
    (setf (graph-predicates cg)
          (if (eq name 'selected)
              (append (graph-predicates cg) (list np))
            (cons np (graph-predicates cg))))
    (when (state-predicate np)
      (setf (graph-state-predicates cg) (cons np (graph-state-predicates cg)))
      )
    ; Create the WMFN node
    (init-wm np)
    ; Update the predicate type
    (setq gpt (graph-predicate-type cg))
    (setf (stype-constants gpt) (cons name (stype-constants gpt)))
    (setf (stype-max gpt) (1+ (stype-max gpt)))
    (setf (stype-span gpt) (1+ (stype-span gpt)))
    ; When there is a function, create a function node and a shared WMVN and attach to the WMFN
    (when (predicate-function np)
      (setq function-node (compile-predicate-function np))
      ; Set up the predicate for the surprise in learning
      (when (and compute-surprise ; We are computing surprise
                 learn-via-gradient-descent ; We are learning in general
                 (not (predicate-no-surprise np)) ; This not a predicate, such as a surprise predicate, for which surprise shouldn't be computed
                 (not (and (predicate-learning-rate np) ; Not a zero learning rate (and thus also not a surprise predicate)
                           (zerop (predicate-learning-rate np))
                           ))
                 )
        (setf (predicate-surprise-predicate np) (predicate (concat-symbols (list name '*surprise)) :perception t :no-surprise t :function 1
                                                           :arguments (meta-arguments np)))
        (setf (node-surprise-predicate function-node) (predicate-surprise-predicate np))
        ; Set up global surprise
        (setf (graph-surprise-predicates cg) (cons np (graph-surprise-predicates cg)))
        (unless (graph-surprise-distribution-predicate cg)
          (setf (graph-surprise-distribution-predicate cg) (predicate 'surprise*distribution :perception t :no-normalize t
                                                         :arguments (if multiagent '((agent agent) (value predicate %)) '((value predicate %)))))
          (setf (graph-surprise-predicate cg) (predicate 'surprise :perception t :no-normalize t :arguments (if multiagent '((agent agent)) '())))
          )
        ; Create an attention predicate
        (when compute-attention
          (setf (predicate-attention-predicate np)
                (predicate (concat-symbols (list (predicate-name np) '*attention)) :no-surprise t :perception t :function 1
                           :arguments (meta-arguments np)))
          ; Set up global attention
          (setf (graph-attention-predicates cg) (cons np (graph-attention-predicates cg)))
          (unless (graph-attention-distribution-predicate cg)
            (setf (graph-attention-distribution-predicate cg) (predicate 'attention*distribution :no-normalize t :perception t
                                                            :arguments (if multiagent '((agent agent) (value predicate %)) '((value predicate %)))))
            (setf (graph-attention-predicate cg) (predicate 'attention :perception t :no-normalize t :arguments (if multiagent '((agent agent)) '())))
            )
          )
        )
      )
    ; Create prediction version of predicate if doing diachronic prediction
    (when (and diachronic-prediction
               (or learn-open ; and either we learn for all predicates
                   (closed-world np) ; or the predicate is closed world
                   )
               (state-predicate np) ; and the predicate tests the state
;               (predicate-select np) ; and the predicate is selection (unique or expected)
               (not (member (predicate-name np) '(impasse state))) ; Don't do for impasse predicate
               )
      (setq sp (make-predicate :name (concat-symbols (list name prediction-suffix))
                               :world 'open
                               :persistent nil
                               ))
      (when (or (predicate-perception np) sp-perception)
        (setf (predicate-perception sp) t)
        )
      (setf (predicate-arguments sp) (mapcar #'prediction-argument (predicate-arguments np)))
      (check-predicate-arguments sp)
      (setf (predicate-prediction sp) np)
      (init-wm sp 1)
      ; It is important that the selected predicate be last in the list for impasse processing
      (setf (graph-predicates cg) (cons sp (graph-predicates cg)))
      (setf (predicate-predict np) sp)
      )
    (when (and episodic-learning ; Create episodic predicates and conditionals when episodic learning is on
               (or learn-open ; and either we learn for all predicates
                   (closed-world np) ; or the predicate is closed world
                   )
               (state-predicate np) ; and the predicate tests the state
               (predicate-select np) ; and the predicate makes a selection
               (not (member name '(impasse state))) ; don't save impasses or states in episodic memory
               )
      (init-predicate-em np))
    ; Replace separate call to goal function with attribute on predicate
    (when (predicate-goal np)
      (goal (predicate-name np) (predicate-goal np))
      )
    np)
  )

; Define a system predicate
; Needed so that no error message for defining one
(defun system-predicate (name &rest arguments)
  (let (np)
    (setq define-system-predicate t)
    (setq np (apply #'predicate (cons name arguments)))
    (setq define-system-predicate nil)
    np)
  )

; Does a predicate test the state?
; Returns state argument
(defun state-predicate (p)
  (find 'state (predicate-arguments p) :key #'argument-type-name)
  )

; Miscellaneous
; -------------

; Set the form of operator selection among best, prob-match and boltzmann
(defun operator-selection (type)
  (when (not (member type '(best prob-match boltzmann)))
    (error "Attempt to set operator selection type to ~S, which is not BEST, PROB-MATCH or BOLTZMANN." type)
    )
  (setf (predicate-select (graph-selected-predicate cg)) type)
  )

; Return a list with n stars (*) in it
(defun nstars (n)
  (let (starlist)
    (dotimes (i n)
      (setq starlist (cons '* starlist))
      )
    starlist)
  )

; A list with n stars
(defun stars (n)
  (make-list n :initial-element '*)
  )

; Compute number of regions in a PLM
(defun regions-in-plm (p)
  (let ((rs 1)
        (sv (plm-slices p))
        )
    (dotimes (i (length sv))
      (setq rs (* rs (length (aref sv i))))
      )
    rs)
  )


; Maps
; ----

; Create a vpoint mapping from a vfactor mapping
; Need to insert a 0 at the front and add 1 to all of the other elements
(defun init-vpoint-map (vfactor)
  (let* ((vf-rank (length vfactor))
         (vpoint (init-vector (+ vf-rank 1) 0)))
    (dotimes (i vf-rank)
      (setf (aref vpoint (+ i 1)) (+ (aref vfactor i) 1)))
    vpoint)
  )



; Empty node function
(defun empty-node-function (node default)
  (let ((fun (node-function node)))
    (setf (node-function node)
          (make-constant-discrete-plm (number-list (plm-rank fun))
                                      (plm-variables fun)
                                      default
                                      0)
          )
    (when save-message-state
      (setf (aref (graph-changes cg) (node-number node)) t)
      )
    )
  )



; Find index of a variable (by its type) in variable sequence
(defun var-type-index (tn vs)
  (position tn vs :key #'(lambda (v) (stype-name (svariable-type v))))
  )


; -----------------------------------------------------------
; Chunking

; Return a pattern for the predicate and the list of variables in the pattern
(defun chunk-pattern (pred)
  (let* ((pname (predicate-name pred))
         (pstr (symbol-name pname))
         v vs aname
         (pattern (list (concat-symbols (list pname '*NEXT))))
        )
    (dolist (arg (predicate-arguments pred))
      (setq aname (argument-name arg))
      (setq v (gentemp (concatenate 'string pstr "-" (symbol-name aname))))
      (setq vs (cons v vs))
      (setq pattern (cons (list aname (list v)) pattern))
      )
    (list (reverse vs) (reverse pattern))
    )
  )

; Create a chunk for a predicate
(defun create-chunk (pred)
  (let (vs conditions cp ap)
    (dolist (wmfn (ess-wm-dependencies (predicate-incoming-vn pred)))
      (setq cp (chunk-pattern (node-predicate wmfn)))
      (setq vs (append (car cp) vs))
      (setq conditions (cons (cadr cp) conditions))
      )
    (setq ap (chunk-pattern pred))
    (setq vs (append vs (car ap)))
    (conditional (concat-symbols (list (predicate-name pred) 'chunk) t)
                 :conditions conditions
                 :condacts (list (cadr ap))
                 :normal (if (symbolp (predicate-unique pred))
                             (caadr (assoc (predicate-unique pred) (cdadr ap)))
                           (mapcar #'(lambda (x) (caadr (assoc x (cdadr ap)))) (predicate-unique pred)))
                 :function-variable-names vs
                 :function (list (cons 1 (make-list (length vs) :initial-element '*)))
                 )
    )
  )

; Create a chunk for each closed-world selection state predicate
(defun create-chunks nil
  (dolist (pred (graph-predicates cg))
    )
  )

; -----------------------------------------------------------
; Initialize graph, test if should halt, and run

; Count number of operators provided by predicate
(defun count-operators-predicate (predicate)
  (setf (predicate-operators predicate)
        (reduce #'* (predicate-wm-variables predicate) :key #'(lambda (v)
                                                                         (stype-span (svariable-type v))
                                                                       )))
  )

; Count number of operators provided across operator predicates
(defun count-operators (predicate-names)
  (let ((count 0)
        countp pred)
    (dolist (pred-name predicate-names)
      (setq pred (predicate-from-name pred-name))
      (setf (predicate-first-operator pred) count)
      (setq countp (count-operators-predicate pred))
      (setf (predicate-id-contents pred) (init-vector countp))
      (setq count (+ count countp))
      )
    count)
  )
    
; Make a predicate into an operator
(defun make-predicate-operator (predicate operator-type)
  (when (state-predicate predicate)
    (error "Found STATE argument in predicate ~S, but state predicates cannot be operator predicates." (predicate-name predicate))
    )
  (when (position-of-predicate-argument 'operator predicate) ; The predicate already has an operator argument
    (error "Predicate ~S already has an OPERATOR argument that conflicts with system generated argument when creating operators from predicate." (predicate-name predicate))
    )
  (let* ((wmfn (predicate-wm predicate))
         (old-vs (predicate-wm-variables predicate))
         old-v
         (old-rank (length old-vs))
         (new-vs (init-vector (1+ old-rank)))
         )
    ; Update list of arguments in predicate
    (setf (predicate-arguments predicate)
          (append (predicate-arguments predicate) (list (list 'operator 'operator))))
    ; Copy and extend variable vector by 1 (and perform necessary checks)
    (dotimes (d old-rank)
      (setq old-v (aref old-vs d))
      (unless (stype-discrete (svariable-type old-v))
        (error "Can't include predicate ~S among operators because variable ~S is not discrete."
               (predicate-name predicate) (svariable-name old-v))
        )
      (unless (multiple-variable old-v)
        (error "Can't include predicate ~S among operators because variable ~S is not universal."
               (predicate-name predicate) (svariable-name old-v))
        )
      (when (eq (svariable-type old-v) 'operator)
        (error "Can't include predicate ~S among operators because variable ~S is of type operator."
               (predicate-name predicate) (svariable-name old-v))
        )
      (setf (aref new-vs d) old-v)
      )
    (setf (aref new-vs old-rank) (make-svariable :name 'wm-operator :type operator-type))
    (setf (predicate-wm-variables predicate) new-vs)
    (when wmfn
      (setf (node-variables wmfn) new-vs)
      ; Regenerate new WMFN function given new variables and limit on operators
      (setf (node-function wmfn)
            (init-plm new-vs
                      (if (closed-world predicate) 0 1)
                      0 (init-vector (length new-vs) t)))
      )
    ; Create assign-ids function for predicate
    (setf (predicate-assign-ids predicate) (create-assign-id-function predicate))
    t)
  )

; Define operators for selection
(defun init-operators (type operator-specification &optional impasses)
  (let (args implicit-operator-type operator-type operator-type-name number-of-operators evaluate-pred evaluation-operators implicit-operator-name)
    (when (predicate-from-name 'selected t)
      (error "Operators were already defined via INIT and can't be redefined via INIT-OPERATORS.")
      )
    (unless (member type '(symbols type predicates))
      (error "In INIT-OPERATORS, the first (TYPE) argument - ~S - must be one of SYMBOLS, TYPE or PREDICATES." type)
      )
    (setq implicit-operator-name (if impasses 'base-operator 'operator))
    (when (and (member type '(symbols predicates))
               (member implicit-operator-name (graph-types cg) :key #'stype-name)
               )
      (error "Operators cannot be specified via ~S if the ~S type is already defined." type implicit-operator-name)
      )
    (setq detect-impasses impasses) ; Set global variable
    ; Create STATE predicate if doesn't already exist
    (unless (predicate-from-name 'state t)
      (system-predicate 'state :world 'closed :arguments (if multiagent '((agent agent) (state state)) '((state state))))
      )
    ; Define type for user specified operators (doesn't include evaluation operators if any)
    (setq implicit-operator-type
          (case type
            ((symbols) (new-type implicit-operator-name :discrete t :min 0 :max (length operator-specification) :constants operator-specification))
            ((type) (if (atom operator-specification) (type-from-name operator-specification) (eval operator-specification)))
            ((predicates) (new-type implicit-operator-name :discrete t :numeric t :min 0 :max (count-operators operator-specification)))
            ))
    (setq number-of-operators (stype-span implicit-operator-type))
    (when impasses
      (setq evaluation-operators number-of-operators)
      (setq number-of-operators (* 2 number-of-operators))
      (setq operator-type (new-type 'operator :discrete t :numeric t :min 0 :max number-of-operators))
      (unless (eq type 'predicates)
        (system-predicate 'operator :arguments (list (list (stype-name implicit-operator-type) (stype-name implicit-operator-type))))
        (setq operator-specification (list 'operator))
        (count-operators operator-specification)
        )
      )
    ; For each operator predicate, extend arguments/variables for operator and generate assign-ids function
    (when (or (eq type 'predicates) impasses)
      (dolist (pred-name operator-specification)
        (make-predicate-operator (predicate-from-name pred-name) (if impasses operator-type implicit-operator-type))
        )
      (setf (graph-operator-predicates cg) (mapcar #'predicate-from-name operator-specification))
      )
    ; Create SELECTED predicate
    (setq args '((state state)))
    (if multiagent (setq args (cons '(agent agent) args)))
    (setq operator-type-name (stype-name (if impasses operator-type implicit-operator-type)))
    (setf (graph-operator-type-name cg) operator-type-name)
    (setf (graph-selected-predicate cg)
          (system-predicate 'selected :world 'closed :persistent t :select 'best
                     :arguments (reverse (cons `(operator ,operator-type-name !) (reverse args)))))
    ; Initiate the possibility of impassing
    (when impasses
      (new-type 'impasse :constants '(tie none no-change))
      (system-predicate 'impasse :world 'closed :arguments (append args `((operator ,operator-type-name) (type impasse !))))
      (setf (graph-detect-impasses-predicate cg) (system-predicate 'detect-impasses :arguments (append args '((value flag %)))))
      (setq evaluate-pred (system-predicate 'evaluate-operator :world 'open :arguments `((evaluate ,operator-type-name))))
      (setf (predicate-first-operator evaluate-pred) evaluation-operators)
      (setf (predicate-operators evaluate-pred) (predicate-first-operator evaluate-pred))
      (setf (predicate-id-contents evaluate-pred) (init-vector evaluation-operators))
      (make-predicate-operator evaluate-pred operator-type)
      (setf (graph-operator-predicates cg) (append (graph-operator-predicates cg) (list evaluate-pred)))
      )
    )
  )

; Checks to be done at init time
(defun init-checks nil
  (unless (member operator-best-select '(random first))
    (error "Unknown value ~S of OPERATOR-BEST-SELECT.  Must be FIRST or RANDOM." operator-best-select)
    )
  (unless (member non-operator-best-select '(random first))
    (error "Unknown value ~S of NON-OPERATOR-BEST-SELECT.  Must be FIRST or RANDOM." non-operator-best-select)
    )
  )

; Initialize
(defun init (&optional operators agents center-discrete)
  (setf *qhash* (make-hash-table :test 'equal))
  (when reset-parameters-in-init
    (reset-parameters))
  (setq trial-count 0)
  (setq center-discrete-numeric-on-integer center-discrete)
  (setq cg (create-graph))
  (init-checks)
  (setq epsilon21 (* epsilon2 1.1))
  (setq relative-epsilon (* .1 epsilon2)) ; Initial value before defining types
  (new-type 'boolean :constants (list boolean-false boolean-true))
  (new-type 'flag :constants '(true))
  (new-type 'state :numeric t :discrete t :min 0 :max max-state)
  (setf (graph-predicate-type cg) (new-type 'predicate :constants nil))
  (when track-time
    (new-type temporal-predicate-name :numeric t :discrete t :min 0 :max max-time)
    (system-predicate temporal-predicate-name :world 'closed :arguments '((value time !)))
    )
  (cond (agents
         (let (agent-type)
           (setq multiagent t)
           (setq agent-type (if (consp agents) ; Take a list of agent names
                                (new-type 'agent :constants agents)
                              (if (and (integerp agents) (> agents 0)) ; Take a number of agents
                                  (new-type 'agent :numeric t :discrete t :min 0 :max agents)
                                (error "Agents specification of ~S is neither a list or a positive integer." agents))))
           (setf (graph-agent-type cg) agent-type)
           (setf (graph-agents cg) (stype-span agent-type))
           (setq bottom-state (init-vector (graph-agents cg) base-level-state))
           (setq delete-lower-states (init-vector (graph-agents cg)))
           )
         )
        (t
         (setq multiagent nil)
         (setq bottom-state base-level-state)
         (setq delete-lower-states nil)
         )
        )
  (when operators
    (init-operators 'symbols operators) ; Defaults to detect-impasses nil
    )
  (system-predicate halt-predicate-name :world 'closed)
  (setf (graph-node-vector cg) nil)
  (setf (graph-goals-set cg) nil)
  (setq message-count 0)
  (setq decision-count 0)
  (setq cycle-message-counts nil)
  (setq impasse-regions nil)
  (setq perceive-list nil)
  (setq action-list nil)
  (setq pre-run nil)
  (setq post-run nil)
  (setq pre-d nil)
  (setq post-d nil)
  (setq pre-t nil)
  (setq post-t nil)
  (setq pre-ts nil)
  (setq post-automatic nil)
  (setq global-decision-statistics nil)
  (setq global-graph-messages nil)
  (setq have-run nil)
  (setq define-system-predicate nil)
  (setq attention-exponential-numerator (log (/ 10 epsilon)))
  t)

; Set WM for Halt to 1 so that halt
(defun halt nil
  (let ((pwm (predicate-wm (predicate-from-name halt-predicate-name))))
    (setf (node-function pwm) (make-constant-discrete-plm nil nil 1 0))
    (when save-message-state
      (setf (aref (graph-changes cg) (node-number pwm)) t)
      )
    )
  )

; Check if halt action executed
(defun haltp ()
  (not (plm-empty (node-function (predicate-wm (predicate-from-name halt-predicate-name)))))
  )


; Is link inactive?
(defun inactive-link (l)
  (and (not (link-out l))
       (not (link-in l))
       ) 
  )

; Excise all inactive links from graph
(defun excise-inactive-links nil
  (dolist (l (graph-links cg))
    (when (inactive-link l)
      (excise-link l)
      )
    )
  nil)

; Excise unneeded factor steps from a node
(defun excise-unneeded-factor-steps-node (n)
  ; Eliminate unnecessary product steps
  (dolist (l (node-links n))
    (unless (aref (link-contents l) var-index)
      (setf (node-factor-steps n)
            (remove (node-name (aref (link-nodes l) var-index)) (node-factor-steps n)
                    :key (lambda (fs) (if (eq (factor-step-type fs) 'product)
                                          (node-name (aref (link-nodes (factor-step-argument fs)) var-index))
                                        nil))))
      )
    )
  ; Eliminate unnecessary summary steps
  (let (v-used)
    (dotimes (i (length (node-variables n)))
      (setq v-used nil)
      (dolist (l (node-links n))
        (when (and (link-map l) ; There is a map on the link (otherwise no variables in variable node
                   (not (aref (smap-omitted (link-map l)) i)) ; Factor node variable is used in variable node
                   (aref (link-contents l) var-index)) ; The link is active from the variable to the factor node
          (return (setq v-used t))
          )
        )
      (unless v-used
        (setf (node-factor-steps n)
              (remove i (node-factor-steps n)
                      :key (lambda (fs) (if (not (eq (factor-step-type fs) 'product))
                                            (factor-step-argument fs)
                                          nil))))
        )
      )
    )
  )

; Excise unneeded factor steps from all nodes
(defun excise-unneeded-factor-steps nil
  (dolist (n (graph-nodes cg))
    (when (factor-nodep n)
      (excise-unneeded-factor-steps-node n)
      )
    )
  )

; Generate prediction links for diachronic processing
(defun include-prediction-links nil
  (let (ptn vars map link
        opnode ; Outgoing/shared WMVN for prediction predicate, if there is one
        pwm ; Predicate WM
        old-incoming-link
        plink
        )
    (dolist (pred (graph-predicates cg))
      ; Deal with prediction for persistent prediction predicates (when not already done)
      (when (and (predicate-predict pred) (not (predicate-prediction-link pred)))
        (setq opnode (predicate-outgoing-vn (predicate-predict pred)))
        (setq pwm (predicate-wm pred))
        (when (and (predicate-persistent pred) opnode)
          (when (predicate-incoming-vn pred)
            ; Link incoming WM variable node to shared prediction WMVN
            (setq vars (node-variables pwm))
            (setf ptn (init-factor-node (concat-symbols (list (predicate-name pred) 'predict) t)
                                        'pass-through vars
                                        (list (predicate-incoming-vn pred) opnode)
                                        nil t nil nil nil nil))
            (setf (node-function ptn) (full-plm vars))
            ; Deactive link direction from shared WMVN
            (setq plink (link-from-nodes ptn opnode))
            (setf (link-out plink) nil)
            (setf (aref (link-contents plink) var-index) nil)
            ; Activate links from original to prediction
            (dolist (l (node-links ptn))
              (if (= (node-number (aref (link-nodes l) var-index)) (node-number opnode))
                  (setf (aref (link-contents l) fact-index) (full-plm vars))
                (setf (aref (link-contents l) var-index) (full-plm vars)))
              )
            ; Remove the old link from the original predicate's incoming WMVN to its WMFN
            (setq old-incoming-link (link-from-numbers (node-number (predicate-incoming-vn pred)) (node-number pwm)))
            (setf (link-in old-incoming-link) nil)
            (setf (link-out old-incoming-link) nil)
            )
          ; Make shared prediction WMVN the incoming WMVN for the original predicate
          (setq map (build-smap (node-variables opnode) (node-variables pwm))) ; Build map from variable node variables to factor node variables
          (setq link (make-link :map map
                                :nodes (vector opnode pwm) :contents (vector nil nil)
                                :depths (vector nil nil)
                                :loop-depths (vector nil nil)
                                :inits (vector -1 -1)
                                :incoming (vector nil nil)
                                :in t
                                :out nil
                                :counts (vector 0 0)
                                :stale (vector nil nil)
                                :prequeue (vector nil nil)
                                )
                )
          (add-link link)
          (setf (predicate-prediction-link pred) link)
          (setf (predicate-incoming-vn pred) opnode)
          )
        )
      )
    )
  )

; Identify descendants of assumption nodes
(defun init-descendants ()
  ; These link decendants are needed if this form of state saving is specified or if automatically creating prediction conditionals
  (when (or save-message-state automatic-action-models automatic-perception-models)
    (init-link-descendants)
    )
  )

; Determine which arguments in a predicate are unique for gradient-descent learning
(defun predicate-function-unique-arguments nil
  (dolist (pred (graph-predicates cg))
    (let (unique condact cvars sv pfnode
                 (pname (predicate-name pred))
                 )
      (when (and (predicate-function pred)
                 (predicate-condact-conditionals pred)
                 )
        (dolist (c (predicate-condact-conditionals pred))
          (setq condact (find pname (conditional-condacts c) :key #'car))
          (when condact
            (setq cvars (conditional-variables c))
            (dolist (e (cdr condact))
              (setq sv (find (caadr e) cvars :key #'svariable-name))
              (when (and sv (svariable-unique sv))
                (setq unique (adjoin (car e) unique))
                )
              )
            )
          )
        (setf (predicate-unique-for-gdl pred) unique)
        (setq pfnode (predicate-function-node pred))
        (when pfnode
          (setf (node-unique-for-gdl pfnode) unique)
          )
        )
      )
    )
  )


; Determine when factor and variable node variables are same along links and store in list on 
(defun assign-variables-same nil
  (dolist (l (graph-links cg))
    (let ((vn (link-var-node l))
          (fn (link-fact-node l))
          )
      (when (variables-equal (node-variables vn) (node-variables fn))
        (setf (link-variables-same l) t)
        )
      )
    )
  )

; Processing to be done after all conditionals are defined
; Ideally this would be done incrementally, at least as much as possible, rather than redoing everything, but that is left to the future
(defun post-process-conditionals nil
  (let ((pre-time-init-node (get-internal-run-time))
        message-time
        )
    (include-prediction-links)
    (excise-inactive-links)
    (when max-messages-links
      (setq max-messages (* max-messages-links (length (graph-links cg))))
      )
    (expand-densely-connected-variable-nodes) ; Replace densely connected nodes with binary trees of nodes
    (init-messages t) ; Initialize messages and compute depths
    (setq message-time (/ (- (get-internal-run-time) pre-time-init-node) 1000))
    (when debug-init-descendants
      (format trace-stream "~&Message and Queue Initialization took ~f seconds" message-time)
      )
    (init-descendants)
    (when debug-init-descendants
      (format trace-stream "~&Descendant Initialization took ~f seconds"
              (- (/ (- (get-internal-run-time) pre-time-init-node) 1000) message-time))
      )
    (excise-unneeded-factor-steps)
    (init-node-vector)
    (predicate-function-unique-arguments)
    (assign-variables-same)
    )
  )

; Create vector of nodes from numbers
(defun init-node-vector nil
  (let (nv)
    (setq nv (init-vector (length (graph-nodes cg))))
    (dolist (n (graph-nodes cg))
      (setf (aref nv (node-number n)) n)
      )
    (setf (graph-node-vector cg) nv)
    )
  )

; Initialize a graph after it is defined
(defun init-graph nil
  (init-descendants)
  (when (or automatic-action-models automatic-perception-models automatic-reinforcement-learning)
    (create-models)
    (post-automatic)
    )
  (post-process-conditionals)
  (setf (graph-initialized cg) t)
  (init-link-counts)
  (when (get-state-predicate)
    (evidence '((state (state 0))))
    )
  )

(defun ig nil (init-graph))

; Limit state dimension in all predicate WMFNs to bottom state
(defun restrict-to-bottom-state ()
  (dolist (sp (graph-predicates cg))
    (when (state-predicate sp)
      (if multiagent
          (dotimes (ai (graph-agents cg))
            (delete-lower-states (1+ (aref bottom-state ai)) ai)
            )
        (delete-lower-states (1+ bottom-state))
        )
      )
    )
  )

;-----------------------------------------------------------
; action and perception models

; Determine which arguments are unique for the newly created model function predicate
(defun unique-model-arguments (pattern old-unique)
  (let (new-unique)
    (dolist (e (cdr pattern))
      (when (member (element-argument-name e) old-unique)
        (setq new-unique (cons (caadr e) new-unique))
        )
      )
    (reverse new-unique))
  )

; Determine which arguments of a predicate pattern are universal
(defun universal-arguments (pred pattern)
  (let ((unique (predicate-unique pred))
        uas)
    (dolist (arg pattern)
      (unless (member (argument-name arg) unique)
        (setq uas (cons arg uas))
        )
      )
    uas)
  )
; -----------------------------------------------------------
; Interface and test stuff

; Extract (first) best domain element from a 1D PLM (or a 2D state PLM)
(defun best-in-plm (plm &optional state)
    (let ((pvs (plm-variables plm)) ; PLM variables
          vtype ; Type of variable
          vi ; Index of variable
          si ; State index
          ar ; Axial region of maximized PLM (for state)
          mp ; Maximized plm
          mprarray
          ir
          ril ; Index list
          mpsizev ; Sizes of dimensions of mprarray
          best) ; Best value
      (cond (state
             (unless (= (length pvs) 2)
               (pplm plm)
               (error ": BEST-IN-PLM cannot be applied to plm because does not have exactly one non-state dimension!")
               )
             (setq si (stype-index 'state pvs))
             (setq vi (if (= si 0) 1 0))
             )
            (t
             (unless (= (length pvs) 1)
               (pplm plm)
               (error ": BEST-IN-PLM cannot be applied to plm because does not have exactly one dimension!")
               )
             (setq vi 0)
             ))
      (setq vtype (svariable-type (aref pvs vi)))
      (setq mp (maximize-plm plm vi))
      (setq mprarray (plm-array mp))
      (setq mpsizev (array-dimensions-v mprarray))
      (setq ril (make-list (plm-rank plm) :initial-element 0))
      (setq ar (row-major-aref mprarray 0)) ; Origin region
      ; If state, find axial region for it
      (when state
        (do ((il ril (region-after-index-list il si (1- (aref mpsizev si)))))
            ((and (setq ir (apply #'aref mprarray il))
                  (<= (region-min ir si) state)
                  (> (region-max ir si) state)
                  )
             (setq ar ir)
             (return)
             )
          )
        )
      (setq best (car (aref (region-maximals ar) vi))) ; Best (sub)region
      (cond ((not (stype-discrete vtype)) ; Continuous
             (car best))
            ((stype-constants vtype) ; Symbolic
             (get-symbol-name (car best) vtype))
            (t ; Discrete
             (if center-discrete-numeric-on-integer
                 (+ (car best) 1/2)
               (car best)))
            )
      )
    )

; Extract unique discrete value from a 1D WM
(defun nonstate-value (pname)
  (let* ((pred (predicate-from-name pname))
         fn
         (args (predicate-arguments pred))
         (arg (car args))
         (aname (argument-name arg))
         (dim (position-of-predicate-argument aname pred))
         smp ; Smallest maximal point
         at ; argument type
         )
    (unless (predicate-wm pred)
      (error "Predicate ~S has no WMFN in NONSTATE-VALUE" pname)
      )
    (setq fn (node-function (predicate-wm pred)))
    (unless (= (length args) 1)
      (error "Predicate ~S is not 1D in NONSTATE-VALUE" pname)
      )
    (unless (and (= (length arg) 3) (unique-argument arg))
      (error "Argument ~S in Predicate ~S is not unique in NONSTATE-VALUE" aname pname)
      )
    (setq at (type-from-name (argument-type-name arg)))
    (unless (and at (stype-discrete at))
      (error "Argument ~S in Predicate ~S is not discrete in NONSTATE-VALUE" aname pname)
      )
    (setq smp (smallest-maximal-point fn dim))
    (if center-discrete-numeric-on-integer (+ smp 1/2) smp)
    )
  )

; Extract value of argument in state
; Assumes the argument is unique and that there are no other arguments in the predicate
(defun value-in-function-state (pred fn s aname &optional agent)
  (let* ((pname (predicate-name pred))
         (slices (plm-slices fn))
         (sd (position-of-predicate-argument 'state pred)) ; Position of state argument
         (argd (position-of-predicate-argument aname pred)) ; Position of argument of interest
         agentd
         (rarray (plm-array fn))
         (size-1 (1- (aref (array-dimensions-v rarray) argd)))
         (args (predicate-arguments pred))
         (arg (nth argd args))
         argt ; argument type
         il ; Index list for region
         ir ; Region
         )
    (when (and multiagent (not agent))
      (error "No agent argument provided to VALUE-IN-FUNCTION-STATE for multiagent state predicate ~S" pname)
      )
    (unless (= (length args) (if agent 3 2))
      (error "Predicate ~S is not ~SD in VALUE-IN-FUNCTION-STATE" pname (if agent 3 2))
      )
    (unless (and (= (length arg) 3) (unique-argument arg))
      (error "Argument ~S in Predicate ~S is not unique in VALUE-IN-FUNCTION-STATE" aname pname)
      )
    (unless sd
      (error "Predicate ~S has no state argument in VALUE-IN-FUNCTION-STATE" pname)
      )
    (unless argd
      (error "Predicate ~S has no ~S argument in VALUE-IN-FUNCTION-STATE" pname aname)
      )
    (setq argt (type-from-name (argument-type-name arg)))
    (unless (and argt (stype-discrete argt))
      (error "Argument ~S in Predicate ~S is not discrete in VALUE-IN-FUNCTION-STATE" aname pname)
      )
    (setq il (make-list (plm-rank fn) :initial-element 0)) ; Set index list to origin
    ; If there is an agent, find the index of the axial region for the agent of interest
    (when agent
      (when (symbolp agent)
        (setq agent (position agent (stype-constants (type-from-name 'agent)))) ; Get number from symbol
        )
      (setq agentd (position-of-predicate-argument 'agent pred))
      ; Set region index along agent dimension to agent's slice index
      (setf (nth agentd il) (find-region-slice-index (if (and center-discrete-numeric-on-integer (stype-numeric (graph-agent-type cg))) (- agent 1/2) agent) (aref slices agentd)))
      )
    ; Find the index of the axial region for the state of interest
    (setf (nth sd il) (find-region-slice-index (if center-discrete-numeric-on-integer (- s 1/2) s) (aref slices sd)))
    ; Find (first) region with a functional value of 1 along the argument dimension
    (do ((dil il (region-after-index-list dil argd size-1)))
        ((null dil) (return nil))
      (setq ir (apply #'aref rarray dil))
      (when (e= (region-constant ir) 1)
        (return (if (stype-numeric argt)
                    (region-min ir argd)
                  (get-symbol-name (region-min ir argd) argt))
                )
        )
      )
    )
  )

; Extract value of argument in state from predicate WMFN
; Assumes the argument is unique and that there are no other arguments in the predicate
(defun value-in-state (pname s aname &optional agent)
  (let ((pred (predicate-from-name pname)))
    (unless (predicate-wm pred)
      (error "Predicate ~S has no WMFN in VALUE-IN-STATE" pname)
      )
    (value-in-function-state pred (node-function (predicate-wm pred)) s aname agent)
    )
  )

; Return the operator in state s
(defun operator-in-state (s &optional agent)
  (unless (predicate-from-name 'selected t)
    (error "No selection predicate defined, so can't get the operator from the state.")
    )
  (value-in-state 'selected s 'operator agent)
  )

; Get symbol name from type
(defun get-symbol-name (num type)
  (elt (stype-constants type) num))

; Does a dimension cover the full scope of a type?
(defun dimension-full-scope (d-min d-max type)
  (and (e= d-min (stype-min type) t)
       (e= d-max (stype-max type) t)))

; Get the type of a variable when variable may be a single variable or a list
; If a list (from a factor vector of variables), just get type of first (both should be same)
(defun variable-types (v)
  (if (listp v)
      (svariable-type (car v))
    (svariable-type v)))


; Determine if region is within limits
; It is if it overlaps with limits, even if region extends outside of limits
(defun region-within-limits (r limits-v)
  (let ((within t)
        d)
    (dotimes (i (length limits-v))
      (setq d (aref (region-dimensions r) i))
      (when (or (e<= (slice-location (dimension-max-slice d)) (car (aref limits-v i)) t)
                (e>= (slice-location (dimension-min-slice d)) (cadr (aref limits-v i)) t)
                )
        (setq within nil)
        (return)
        )
      )
    within)
  )

; Compute a limit vector on the region of the PLM to print from a list with argument names and constant elements
(defun compute-limits (l vars)
  (let* ((rank (length vars))
         (v (init-vector rank))
         var type pos)
    ; Initialize limit vector to full span of each variable/dimension
    (dotimes (i rank)
      (setq var (aref vars i))
      (when (listp var) (setq var (car var))) ; For delta nodes
      (setq type (svariable-type var))
      (setf (aref v i) (list (stype-min type) (stype-max type)))
      )
    ; Restrict limit vector based on arguments specified
    (dolist (a l)
      (setq pos (position (car a) vars
                          :key #'(lambda (v) (if (listp v)
                                                 (svariable-name (car v))
                                               (svariable-name v))
                                   )
                          )
            )
      (when pos
        (setq var (aref vars pos))
        (when (listp var) (setq var (car var))) ; For delta nodes
        (setf (aref v pos) (compute-span (cadr a) (svariable-type var) t))
        )
      )
    v)
  )

; Check with a summarization list of find-plm has at most one arg-max, and it is last
(defun validate-summarize-list (summarize)
  (and (listp summarize)
       (or (= (length summarize) 0)
           (and 
            (not (find-if #'(lambda (x) (not (and (listp x) (or (= (length x) 2) (and (= (length x) 3) (eq (car x) 'constant)))))) summarize)) ; All elements are 2 element lists
            (not (find 'argmax summarize :key #'car :end (1- (length summarize)))) ; No use of summarize before last element
            )
           )
       )
  )

; Arguments for summarization
(defun apply-summarization-arguments (p d &optional location)
  (if location
      (list p d (car (compute-span location (svariable-type (aref (plm-variables p) d)) t)))
    (list p d))
  )

; Apply a summarization operation to a PLM (or a list of two PLMs)
(defun apply-summarization (pn s)
  (let (d op location ps)
    (setq op
          (case (car s)
            ((constant) #'extract-value-plm)
            ((expected) #'expected-value-plm)
            ((max argmax) #'maximize-plm)
            (t (error "Summarization operator ~S not one of CONSTANT, EXPECTED, MAX or ARGMAX in find-plm." (car s)))
            ))
    (when (eq (car s) 'constant)
      (if (cddr s)
          (setq location (caddr s))
        (error "No location provided for CONSTANT summarization in APPLY-SUMMARIZATION."))
;      (format trace-stream "~&~S: ~S~&" (cadr s) (caddr s))
      )
    (if (listp pn)
        (dolist (p pn)
          (setq d (position (cadr s) (plm-variables (cdr p)) :key #'svariable-name))
          (when d
            (setq ps (cons (apply op (apply-summarization-arguments (cdr p) d location)) ps))
            )
          )
      (progn
        (setq d (position (cadr s) (plm-variables pn) :key #'svariable-name))
        (unless d ; No position was found
          (error "Could not find dimension ~S to summarize over via ~A in ~S." (cadr s) op pn)
          )
        (setq pn (apply op (apply-summarization-arguments pn d location)))
        )
      )
    pn)
  )

; Apply a list of summarizations to a PLM
(defun apply-summarizations (p ss)
  (let ((np p))
    (dolist (s ss)
      (setq np (apply-summarization np s))
      )
    np)
  )

; Find plm to print if given a node number, a predicate name or a conditional name (or a PLM)
; Summarize is a list of lists, each one with a summarization operation ('constant 'expected or 'max [or 'argmax, which also yields a max here])
; and dimension along which to summarize
; There can be at most one argmax, and it must be last
; If pid is specified as a name, and there is both a predicate and a conditional by that name, return a list of both
(defun find-plm (pid &optional summarize)
  (unless (validate-summarize-list summarize)
    (error "Invalid summarization list (not list of 2-element lists with at most one argmax) in find-plm: ~S." summarize)
    )
  (let (pwmn pfn p c n cn pn)
    (cond ((typep pid 'plm)
           (setq pn pid)
           )
          ((numberp pid)
           (setq n (node-from-number pid))
           (if n
               (setq pn (if (variable-nodep n)
                            (variable-posterior n)
                          (node-function n)))
             (error "No node numbered ~S found in FIND-PLM for printing PLM" pid))
           )
          ((symbolp pid)
           (setq p (predicate-from-name pid t))
           (when p
             (setq n (predicate-wm p))
             (when n
               (setq pwmn (node-function n))
               (setq pn (cons (cons 'wm pwmn) pn))
               )
             (setq n (predicate-function-node p))
             (when n 
               (setq pfn (node-function n))
               (setq pn (cons (cons 'pfun pfn) pn))
               )
             )
           (setq c (conditional-from-name pid))
           (when c
             (setq n (conditional-function-node c))
             (when n
               (setq cn (node-function n))
               (setq pn (cons (cons 'cfun cn) pn))
               )
             )
           (unless (or pwmn pfn cn)
             (error "No predicate or conditional function named ~S found in FIND-PLM for printing PLM" pid)
             )
           )
          (t ; A list
           )
          )
    (setq pn (if (listp pn)
                 (mapcar #'(lambda (x) (cons (car x) (apply-summarizations (cdr x) summarize))) pn)
               (apply-summarizations pn summarize)))
    pn)
  )


; Return a region descriptor for a conditional function
(defun function-region (r p)
  (let (fun specs dims dim dt vars)
    (if (region-function-constantp r)
        (setq fun (region-constant r))
      (setq fun (coerce (extract-function r) 'list)))
    (setq dims (region-dimensions r))
    (setq vars (plm-variables p))
    (dotimes (d (region-rank r))
      (setq dim (aref dims d))
      (setq dt (svariable-type (aref vars d)))
      (if (dimension-full-scope (dimension-min dim) (dimension-max dim) dt)
          (push '* specs)
        (if (stype-constants dt)
            (if (= (region-span r d) 1)
                (push (get-symbol-name (dimension-min dim) dt) specs)
              (push (list (get-symbol-name (dimension-min dim) dt) (get-symbol-name (1- (dimension-max dim)) dt)) specs))
          (if (stype-discrete dt)
              (if (= (region-span r d) 1)
                  (push (if center-discrete-numeric-on-integer (+ (dimension-min dim) 1/2) (dimension-min dim)) specs)
                (push (list (if center-discrete-numeric-on-integer (+ (dimension-min dim) 1/2) (dimension-min dim))
                            (if center-discrete-numeric-on-integer (+ (dimension-max dim) 1/2) (dimension-max dim))) specs))
            (push (list (dimension-min dim) (dimension-max dim)) specs))))
      )
    (if (and (numberp fun) (= fun 0))
        nil
      (cons fun (reverse specs))))
  )

; Convert a PLM into a form that works for a function in a conditional
(defun plm-cpt (p)
  (unless (eq (type-of p) 'plm) (error "Attempt to print an object of type ~S as a PLM." (type-of p)))
  (let ((rarray (plm-array p))
        r rfun pfun)
    (dotimes (i (array-total-size rarray))
      (setq r (row-major-aref rarray i))
      (if (region-bad r)
          (setq rfun nil)
        (setq rfun (function-region r p)))
      (when rfun
        (push rfun pfun)
        )
      )
    (reverse pfun))
  )

; Provide conditional function in a form that can be reused on a conditional
(defun reusable-conditional-function-c (c)
  (let (cf)
    (unless c
      (error "No conditional named ~S in REUSABLE-CONDITIONAL-FUNCTION-C" (conditional-name c))
      )
    (setq cf (node-function (conditional-function-node c)))
    (unless cf
      (error "No function for conditional ~S in REUSABLE-CONDITIONAL-FUNCTION-C" (conditional-name c))
      )
    (if (numberp cf)
        cf
      (plm-cpt cf))
    )
  )

; Provide conditional function from name in a form that can be reused on a conditional
(defun reusable-conditional-function (cname)
  (reusable-conditional-function-c (conditional-from-name cname))
  )

; Find operator type from id of operator
(defun operator-predicate (id)
  (dolist (pred (graph-operator-predicates cg))
    (when (< id (+ (predicate-first-operator pred) (predicate-operators pred)))
      (return pred)
      )
    )
  )

; Print an operator based on its id
; Doesn't yet do the right thing, as it prints whole region, not just relevant values
(defun pid (id)
  (unless (type-from-name 'operator)
    (error "Can't print operator from id because there is no operator type.")
    )
  (let* ((op (operator-predicate id))
         (vs (aref (predicate-id-contents op) (- id (predicate-first-operator op)))))
    (format trace-stream "~S: (~S" (car (last vs)) (predicate-name op))
    (do ((vl vs (cdr vl))
         (al (predicate-arguments op) (cdr al)))
        ((or (null (cdr vl)) (null (cdr al))))
      (format trace-stream " (~S ~S)" (car (argument-name al)) (car vl))
      )
    (format trace-stream ")")
    ; If an evaluate-operator operator, print the operator being evaluated as well
    (when (eq (predicate-name op) 'evaluate-operator)
      (format trace-stream " [")
      (pid (car vs))
      (format trace-stream "]")
      )
    t)
  )

; Print all operators by their ids
(defun pids nil
  (let ((operator-type (type-from-name 'operator)))
    (when operator-type
      (dotimes (i (stype-max operator-type))
        (pid i)
        (format trace-stream "~&")
        )
      )
    t)
  )


; Count active variables in PLM
(defun active-variables (as)
  (let ((count 0))
    (dotimes (i (length as))
      (when (aref as i)
        (setq count (1+ count))
        )
      )
    count)
  )


; Determine active number of link directions in graph
(defun count-active-link-directions ()
  (let ((count 0))
    (dolist (l (graph-links cg))
      (when (link-fact-content l) (setq count (+ count 1)))
      (when (link-var-content l) (setq count (+ count 1)))
      )
    count)
  )

; Nodes with maximum variables, from graph-statistics
; Vector with variable and then factor nodes
(defvar max-node-variables)

; Determine numbers of nodes, average and maximum variables for variable and factor nodes
(defun count-node-variables ()
  (let ((nums (init-vector 2 0))
        (sums (init-vector 2 0))
        (maxs (init-vector 2 0))
        (maxns (init-vector 2 nil)) ; Lists of nodes with maximum variables
        vorf ; 0 for variable 1 for factor
        vars) ; Number of variables at node
    (dolist (n (graph-nodes cg))
      (if (factor-nodep n)
          (setq vorf 1)
        (setq vorf 0))
      (setf (aref nums vorf) (+ (aref nums vorf) 1))
      (setq vars (length (node-variables n)))
      (setf (aref sums vorf) (+ (aref sums vorf) vars))
      (when (= vars (aref maxs vorf))
        (setf (aref maxns vorf) (cons (node-name n) (aref maxns vorf)))
        )
      (when (> vars (aref maxs vorf))
        (setf (aref maxs vorf) vars)
        (setf (aref maxns vorf) (list (node-name n)))
        )
      )
    (dotimes (i 2)
      (setf (aref sums i)
            (coerce (/ (aref sums i) (aref nums i)) 'short-float)) ; Convert sums to averages
      )

    (setq max-node-variables maxns) ; Store maxes globally in case want to look at them
    (list nums sums maxs))
  )

; Determine links per nodes, average and maximum incoming and outgoing for variable and factor nodes
(defun count-node-links ()
  (let ((nums (init-vector 2 0)) ; Number of variable/factor nodes
        (insums (init-vector 2 0))
        (inmaxs (init-vector 2 0))
        (outsums (init-vector 2 0))
        (outmaxs (init-vector 2 0))
        vorf ; 0 for variable 1 for factor
        ins ; Number of incoming links at node
        outs) ; Number of outgoing links at node
    (dolist (n (graph-nodes cg))
      (setq ins 0)
      (setq outs 0)
      (if (factor-nodep n)
          (setq vorf 1)
        (setq vorf 0))
      (setf (aref nums vorf) (+ (aref nums vorf) 1))
      (dolist (l (node-links n))
        (when (aref (link-contents l) (- 1 vorf)) (setq ins (+ ins 1))); Incoming message
        (when (aref (link-contents l) vorf) (setq outs (+ outs 1))) ; Outgoing message
        )
      (setf (aref insums vorf) (+ (aref insums vorf) ins))
      (setf (aref outsums vorf) (+ (aref outsums vorf) outs))
      (when (> ins (aref inmaxs vorf)) (setf (aref inmaxs vorf) ins))
      (when (> outs (aref outmaxs vorf)) (setf (aref outmaxs vorf) outs))
      )
    ; Convert sums to averages
    (dotimes (i 2)
      (setf (aref insums i)
            (coerce (/ (aref insums i) (aref nums i)) 'short-float))
      (setf (aref outsums i)
            (coerce (/ (aref outsums i) (aref nums i)) 'short-float))
      )
    (list nums insums inmaxs outsums outmaxs))
  )


; Count the number of regions in a PLM and the number that are not empty
(defun count-plm-regions (p)
  (let ((total 0)
        (nonempty 0)
        (counts (init-vector 2))
        (rarray (plm-array p))
        (piecewise-constant (plm-piecewise-constant p)))
    (dotimes (i (plm-size p))
      (setq total (1+ total))
      (unless (region-e-empty (row-major-aref rarray i) piecewise-constant)
        (setq nonempty (1+ nonempty))
        )
      )
    (setf (aref counts 0) total)
    (setf (aref counts 1) nonempty)
    counts)
  )

; Short cut for counting regions of a plm
(defun cplmr (p) (count-plm-regions p ))

; Normalized variable posterior for shared WM VN node given predicate
(defun vn-posterior (pred)
  (variable-posterior (predicate-outgoing-vn pred))
  )
(defun vnp (pred-name)
  (vn-posterior (predicate-from-name pred-name))
  )


; Determine if a factor node has any outgoing links
(defun factor-outgoing-link (n)
  (let (out)
    (dolist (l (node-links n))
      (when (aref (link-contents l) fact-index)
        (return (setq out t))
        )
      )
    out)
  )


; Return list of WMVN nodes for a predicate
(defun predicate-wm-vns (pred)
  (let (wm-vns)
    (when (predicate-outgoing-vn pred)
      (setq wm-vns (cons (predicate-outgoing-vn pred) wm-vns))
      )
    (when (and (predicate-incoming-vn pred)
               (or (not wm-vns)
                   (not (= (node-number (car wm-vns)) (node-number (predicate-incoming-vn pred))))
                   )
               )
      (setq wm-vns (cons (predicate-incoming-vn pred) wm-vns))
      )
    (reverse wm-vns))
  )


; Function used in determining exponential prior for time
(defun exp-weights (n)
  (let ((weights (init-vector n 0))
        sum
        )
    (dotimes (i n)
      (setf (aref weights i) (exp (+ i 1))))
    (setq sum (reduce '+ weights))
    (dotimes (i n)
      (setf (aref weights i) (/ (aref weights i) sum)))
    weights)
  )

; Evaluate an expression while generating no result
(defun q (exp)
  exp
  nil
  )

; Evalaute a form n times and return after total time and msecs per message
(defun average-run (n form)
  (let ((total 0)
        (mpm 0)
        run-time decision-time messages
        )
    (dotimes (i n)
      (apply form nil)
      (setq run-time (sum-decision-statistics #'decision-statistics-run-time))
      (setq decision-time (sum-decision-statistics #'decision-statistics-decision-time))
      (setq messages (sum-decision-statistics #'decision-statistics-messages))
      (setq total (+ total run-time decision-time))
      (setq mpm (+ mpm (/ (* 1.0 run-time) messages)))
      )
    (list (/ (* .001 total) n) (/ mpm n))
    )
  )

; compile virtual robot functions NEED TO LOAD IN PKG
;(load (merge-pathnames virtualrobot-file (user-homedir-pathname)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GET / SET Global variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *global-variables* '((ADAPTIVE-LEARNING-RATE . ("T or NIL (default NIL)" ("Dynamically determine learning rate at each node (when not" "overridden by learning rate on conditional) [Only one of this and" "LEARNING-RATE-FRACTION-OF-SMOOTHING-PARAMETER should set]" "[*** Not recommended at present ***]"))) 
                                   (ADAPTIVE-SMOOTHING . ("T or NIL (default T)" ("Dynamically determine smoothing (for learning) at each node")))
                                   (ALL-CONDACT-FILTERS-PAD-1 . ("T or NIL (default NIL)" ("Invert filter messages for condact messages moving away from WM")))
                                   (ALWAYS-MAX-OPERATORS . ("T or NIL (default NIL)"  ("If true, only use max when summarizing out operators")))
                                   (AROUSAL . ("Positive number or NIL (default NIL)"  "Level of arousal (set manually)"))
                                   (automatic-action-models . ("T or NIL (default NIL)"  ("Automatically create conditions for action modeling" "(aka learning of transition functions)")))
                                   (automatic-perception-models . ("T or NIL (default NIL)"  ("Automatically create conditions for perception modeling" "(including map learning)")))
                                   (AUTOMATICALLY-REORDER-CONDITIONS . ("T OR NIL (default T)"  ("Automatically reorder conditions in conditionals for efficiency")))
                                   (BIPARTITE-GRAPH-DISPLAY . ("T or NIL (default T)"  ("Use new form of factor graph display in graph/g")))
                                   (BREAK-ON-POSITIVE-MESSAGES . ("T or NIL (default NIL)"  ("A list of node-number pairs (each a list). Breaks before any decision" "with a non-empty message between those nodes at quiescence")))
                                   (CENTER-DISCRETE-NUMERIC-ON-INTEGER . ("T or NIL (default NIL)"  ("If true, center unit regions for discrete numbers around the integers" "rather than beginning the regions at the integers (needs to be set via" "optional argument to init rather than directly)")))
				   (CHUNKING . ("T or NIL (default NIL)"  ("If true, turn chunking on")))
                                   (COMPUTE-ATTENTION . ("T or NIL (default NIL)" ("Compute attention")))
                                   (COMPUTE-PROGRESS . ("T or NIL (default NIL)" ("Compute progress for goals")))
                                   (COMPUTE-SURPRISE . ("T or NIL (default NIL)" ("Compute surprise for learned functions")))

                                   (DEBUG-DESCENDANTS . ("List of node numbers (default NIL)"  ("Trace descendant and message initialization computations for node" "numbers in list")))
                                   (DEBUG-INIT-DESCENDANTS . ("SUMMARY, ALL or NIL (default NIL)" ("Trace descendant initialization, either just summary statistics (\'summary)" "or also how many descendants each has (\'all)")))
                                   (DEFAULT-INTEGRAL . ("T or NIL (default T)" ("Use integral by default (rather than max) for unique variables")))
                                   (DETECT-IMPASSES . ("T or NIL (default NIL)" ("Whether to impasse on operator selection if tie (needs to be set via" "optional argument to init-operators rather than directly)")))
                                   (DIACHRONIC-PREDICTION . ("T or NIL (default NIL)" ("Predict results of actions/operators")))
                                   (DISCOUNT-WM . ("T or NIL (default T)" ("Discount (by wm-discount-factor) messages from WMFNs to" "FANs for selection predicates")))
                                   (EPISODIC-LEARNING . ("T, OPEN, CLOSED or NIL (default NIL)" ("Perform episodic learning Should use the function learn to set and" "unset this variable")))
                                   (EPSILON . ("positive float (default .0000001)" ("Accuracy within which to compute absolute value comparisons" "(when USE-RELATIVE-EPSILON is nil)")))
                                   (EPSILON2 . ("positive float (deafult .0001)" ("Span of continuous maximal regions")))
                                   (EXPONENTIAL-PRODUCT-GRADIENT . ("T or NIL (default T)" ("Use product of exponential gradient")))
                                   (EXTEND-TYPE-CONSTANTS-BY-EVIDENCE . ("T or NIL (default NIL)" ("Instead of signaling an error if a constant is used in evidence or" "perceive that isn\'t in the type, extend the type to include it")))
                                   (FAN-CONSTANT . ("positive float (default .001)" ("Constant at open-world FAN nodes so that lack of actions" "doesn't nullify WMVN")))
                                   (FEEDBACK-TO-FUNCTIONS . ("T or NIL (default T)" ("Send feedback messages to factor function nodes")))
                                   (FIELD-BEYOND-LABEL . ("positive integer (default 3)" ("Extent of field beyond length of largest label")))
                                   (FUNCTION-DEFAULT . ("non-negative number (default 0)" ("Value to use by default for unspecified regions of conditional" "functions")))
                                   (GDL-DIVIDE . ("NEW, NEWER, T or NIL (default NEWER)" ("Divide gradient by function-message product (new version" "uses a different divisor within each region of universal variable values)")))
                                   (GDL-SUBTRACTIVE-NORMALIZATION . ("T or NIL (default T)" ("Use subtractive rather than divisive normalization in" "gradient-descent learning")))
                                   (GRADIENT-SUBTRACT-AVERAGE . ("T or NIL (default T)" ("Subtract average of gradient before updating")))
                                   (INTEGRATE-UNIVERSAL-IN-UNIQUE . ("T or NIL (default NIL)" ("Integrate rather than max over universal variables in unique functions")))
                                   (LEARN-OPEN . ("T or NIL (default NIL" ("Learn for open-world predicates as well as closed-world ones")))
                                   (LEARN-VIA-GRADIENT-DESCENT . ("T or NIL (default NIL)" ("Use gradient descent on function factor functions Should use" "the function learn to set and unset this variable")))
                                   (LEARNING-RATE . ("positive float (default 0.5)" ("Learning rate for gradient descent")))
                                   (LEARNING-RATE-FRACTION-OF-SMOOTHING-PARAMETER . ("T or NIL (default NIL)" ("If a number, the learning rate is set to this times the smoothing parameter" "[Only one of this and adaptive-learning-rate should be set] [Intended to be" "used in conjunction with adaptive smoothing, and to be <1]")))
                                   (MAX-DECISIONS . ("positive integer (default 500)"  ("Maximum number of decisions to execute when no value specified")))
                                   (MAX-ELABORATION-CYCLES . ("positive integer (default 50)" ("Maximum number of (parallel) cycles per decision")))
                                   (MAX-FINAL-DISCRETE . ("positive integer (default 10)" ("Maximum number of items in the final region along a discrete numeric" "dimension to list explicitly")))
                                   (MAX-FRACTION-PA-REGIONS . ("in [0,1] (default .5)" ("Threshold for shifting from regions in parray within print-smart")))
                                   (MAX-GDL-INCREMENT . ("positive number or NIL (default NIL)" ("Maximum learning increment in gradient descent (if specified)")))
                                   (MAX-MESSAGES . ("positive integer (default 10000)" ("Default maximum number of messages (per decision)")))
                                   (MAX-MESSAGES-LINKS . ("positive integer (default 20)" ("When non-NIL, set max-messages to this times number of links")))
                                   (MAX-SPAN-PA . ("positive integer (default 10)" ("Threshold on size of symbolic dimension in shifting from parray in print-smart")))
                                   (MESSAGE-PROTOCOL . ("SERIAL or PARALLEL (default SERIAL)" ("Pass messages serially versus simulated parallel")))
                                   (MINIMAL-PARRAY-FIELD . ("positive integer (default 14)"  ("Minimum size of a field for a PLM array print")))
                                   (MULTIAGENT . ("T or NIL (default NIL)" ("Default to multiple agents (mostly set automatically within init)")))
                                   (NON-OPERATOR-BEST-SELECT . ("RANDOM or FIRST (default RANDOM)"  ("Choose randomly among maximals for regions in predicates other" "than selected (or first maximal)")))
                                   (ONE-WAY-C-A-BETAS . ("T or NIL (default T)" ("Use unidirectional beta networks within conditions and actions")))
                                   (OPEN-ACTIONS-LIKE-CONDACTS . ("T or NIL (default NIL)" ("Invert filter messages for open-world actions, and combine via product," "as normally do action part of condacts")))
                                   (OPEN-CONDITIONS-LIKE-CONDACTS . ("T or NIL (default NIL)" ("Connect outgoing message from WMFN directly to WMVN (via product)" "instead of going through a VAN/FAN (probabilistic or) path (and invert" "filter messages for open-world conditions if all-condact-filters-pad-1 is T)")))
                                   (OPEN-WORLD-WMFNS . ("T or NIL (default NIL)" ("Create WMFN nodes for open-world predicates")))
                                   (OPERATOR-BEST-SELECT . ("RANDOM or FIRST (default RANDOM)" ("Choose randomly among maximals for operator in selected predicate" "(or first maximal)")))
                                   (parameter-override-reset . ("List of global variables (default NIL)" ("Keep current values for these variables (i.e., don't reset)")))
                                   (PRINT-REGIONS-ON-SEPARATE-LINES . ("T or NIL (default NIL)" ("Print each region in a PLM on a separate line")))
                                   (RANGE-FIELD-BEYOND-LABEL . ("positive integer (default 3)" ("This is the description")))
                                   (SAVE-MESSAGE-STATE . ("T or NIL (default T)" ("Only reinitialize messages across decisions that depend on changed" "functions")))
                                   (SMOOTHING-PARAMETER . ("positive float (default .000001)" ("Minimum value in a learned function")))
;                                   (SPARSE-PRODUCT . ("T OR NIL (Default T)" ("Use optimization for multiplying a sparse function times another function")))
;                                   (SPARSE-PRODUCT-THRESHOLD . ("Number in [0,1]" ("When SPARSE-PRODUCT is T, use optimization when function density" "below threshold")))
                                   (SPECIFY-FUNCTION-VARIABLE-NAMES . ("T or NIL (default T)" ("Explicitly specify variables used in function")))
                                   (SYMBOLIC-TRACE . ("T or NIL (default T)" ("Traces should be symbolic")))
                                   (TEMPERATURE-MINIMUM . ("positive float (default 1/60)" ("Temperature remains above this to avoid overflow")))
                                   (THRESHOLD-FOR-DENSE-CONNECTION . ("positive integer (default 5)" ("If the number of bidirectional links at a variable node is more" "than this threshold, break up into a binary tree of nodes and links")))
                                   (TRACE-AFFINE . ("T or NIL (default NIL)" ("Trace computation at affine transform factor node")))
                                   (TRACE-ATTENTION . ("T or NIL (default NIL)" ("Trace computation in determining attention")))
                                   (TRACE-AVERAGE . ("T or NIL (default NIL)" ("Trace the processing that creates an averaged version of a PLM")))
                                   (TRACE-COMBINE . ("T or NIL (default NIL)" ("Trace combination (product/sum). Print individual region" "combinations if \'region")))
				   (TRACE-CHUNKING . ("T or NIL (default NIL)" ("Trace the processing that creates a chunk")))
                                   (TRACE-CYCLES . ("T or NIL (default T)" ("Trace elaboration cycles")))
                                   (TRACE-DECISIONS . ("T or NIL (default T)" ("Trace decision cycles")))
                                   (TRACE-EL . ("T or NIL (default NIL)" ("Trace episodic learning")))
                                   (TRACE-EMPTY . ("T or NIL (default NIL)" ("Print regions in PLMs with 0 values"))) 
                                   (TRACE-FULL . ("T or NIL (default T)"  ("Print regions in PLMs with 1 values")))
                                   (TRACE-GDL . ("T, NIL or list of node num, node name or conditional names (default NIL)" ("Trace gradient-descent learning (for all nodes if T or just the" "specified function factor nodes)")))
                                   (TRACE-LINK-DEPTHS . ("T or NIL (default NIL)" ("Trace depths of links for messages")))
                                   (TRACE-MAXIMALS . ("T or NIL (default NIL)" ("Trace lists of maximal regions")))
                                   (TRACE-MESSAGE-TIMES . ("T or NIL (default NIL)" ("Trace times taken for messages")))
                                   (TRACE-MESSAGES . ("T or NIL (default NIL)" ("Trace messages (to and from nodes named/numbered)")))
                                   (TRACE-PERCEPTION . ("T or NIL (default NIL)" ("Trace perception")))
                                   (TRACE-PERFORMANCE . ("T or NIL (default T)" ("Trace performance data about decisions")))
                                   (TRACE-PREFERENCES . ("T or NIL (default NIL)" ("Trace preference messages for selected predicate"))) 
                                   (TRACE-QUEUE . ("T or NIL (default NIL)" ("Trace the queue to which each new message is added")))
                                   (TRACE-STATES . ("T or NIL (default T)" ("Trace addition and deletion of states from hierarchy")))
                                   (TRACE-SUMMARIZE . ("T or NIL (default NIL)" ("Trace summarization (integral/max)")))
                                   (TRACE-THRESHOLD . ("number or NIL (default NIL)" ("Print regions in piecewise-constant PLMs with values greater than or equal to threshold")))
                                   (TRACE-TRANSFORM . ("T or NIL (default NIL)" ("Trace transformations (including normalizations)")))
                                   (TRACE-TRIALS . ("T or NIL (default T)" ("Trace trials")))
                                   (TRACE-WM-CHANGES . ("T or NIL (default NIL)" ("Trace the process of deciding which WM changes to make (just" "for predicates listed if there is a list)")))
                                   (TRACE-WM-DRIVEN . ("T or NIL (default NIL)" ("Trace whether messages are wm-driven")))
                                   (TRACK-GRAPH-MESSAGES . ("T or NIL (default NIL)" ("Store messages at the end of each decision into a list")))
                                   (UNIQUE-WEIGHTS-ONLY . ("T or NIL (default T)" ("Generate error if region has a weight for a universal variable")))
                                   (UNSUP-CATEGORIES . ("positive integer (default 5)" ("Number of categories for unsupervised learning")))
                                   (USE-RELATIVE-EPSILON . ("T or NIL (default T)"  ("Use relative rather than absolute numeric comparisons")))
                                   (WM-DISCOUNT-FACTOR . ("positive float (default .01)" ("Factor to use when DISCOUNT-WM is T")))))

; Measure a memory program
(defun mm (program)
  (let ((tm trace-messages)
        (tc trace-cycles)
        )
    (setq trace-messages nil)
    (setq trace-cycles nil)
    (format trace-stream "~&Program ~S:~&" program)
    (eval program)
    (time (run 5000))
    (format trace-stream "~&Messages sent: ~S" message-count)
    (setq trace-messages tm)
    (setq trace-cycles tc)
    )
  )


; IGNORING FOR NOW, WILL NEED TO ADD LATER
;(defun vh-wrap (param)
;  (load (merge-pathnames "loadproject.lisp" vh-path))
;  (run-rl-vh param))
