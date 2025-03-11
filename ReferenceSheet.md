# Table of Contents
- [Initialize a Sigma Program](#initialize-a-sigma-program)
  - [Initialization before defining graph](#initialization-before-defining-graph)
  - [Specify the form of selection to use for operators](#specify-the-form-of-selection-to-use-for-operators)
  - [Define types](#define-types)
  - [Define predicates](#define-predicates)
  - [Define conditionals](#define-conditionals)
  - [Define evidence](#define-evidence)
  - [Define goal for a predicate](#define-goal-for-a-predicate)
  - [Initialize learning](#initialize-learning)
- [Format for a Function](#format-for-a-function)
- [Format for an Item of Evidence in Evidence List](#format-for-an-item-of-evidence-in-evidence-list)
- [Run a Sigma Program](#run-a-sigma-program)
- [To Print](#to-print)
- [Interactive Graphical Display](#interactive-graphical-display)
- [Access Functions](#access-functions)
- [Structures](#structures)
- [System Types](#system-types)
- [Parameters and Constants](#parameters-and-constants)
- [Saving Sessions & Graphs](#saving-sessions--graphs)
- [Parallel Execution](#parallel-execution)
- [Regression Testing](#regression-testing)

### Initialize a Sigma program

#### Initialization before defining graph:  
```lisp
(init &optional <operator-names> <multiagent> <center-discrete-numeric-on-integer>)
```  
- operator-names: List of operator names used in task or a type definition to use for operator  
multiagent: either the number of agents (integer) or a list of agent names  
```lisp
(init-operator <type-of-operator-specification> <operator-specification> &optional
<detect-impasses>); If operators not specified in init, can be specified later via init-operators 
```
 
- type-of-operator-specification: ‘symbols , ‘type or ‘predicates  
- operator-specification: If type is ‘symbols then a list of operator names, if type is ‘type then
either a type definition or the name of an existing type, if type is *‘predicates* then a list of
existing predicate names (predicates must have only universal discrete variables and no
user provided variables of type operator ) to use in generating operator instances for
combinations of discrete values across variables (each with its own operator id)    

#### Specify the form of selection to use for operators in the selected predicate: 
```lisp
(operator-selection <best or match-prob or Boltzmann>) ; best is the default  
```
#### Define types:  
```lisp
(new-type type-name &optional :numeric T :discrete T  
:min min :max max
:constants constant-list)
```
#### Define predicates:
```lisp
(predicate predicate-name &optional :world <world>
:arguments <arguments> :persistent <persistent>
:unique <unique> :select <select>
:exponential <exponential> :no-normalize <no-normalize>
:perception <perception> :function <function>)
```  
- world: *open* or *closed* (defaults to *open*)  
- arguments: list of arguments  
- argument: (<argument-name> <type-name> &optional <unique-symbol>)  
- unique-symbol: ! (select best) or % (maintain distribution) or $ (select expected value) or ^  
(maintain exponential transform of distribution [\** Experimental \**]) or = (select by
probability matching)  
- persistent: T or nil (overrides defaults, which are T for closed world defaults and nil for
open world)  
- unique: name of argument that is non-multiple [set automatically if !, %, $ or = are used], or
list of argument names if they are jointly unique    
- select: (nil, best, prob-match, boltzmann or expected ): What kind of selection, if any
to do over the unique variable, if there is one [set automatically if !, $ or = are used]  
- exponential: T or nil (whether to exponentiate outgoing messages from WMVN [set
automatically if ^ is used])  
- no-normalize: T or nil (whether to normalize outgoing messages from WMVN; may be used
to override default of T, and for unique predicates leads to combining positive actions via
addition and negative actions via subtraction)
- perception: T or nil (whether predicate can receive perceptual information)
- function: Can be a number, a list of region functions, a list starting with row-major followed
by a list of numbers (requires that all of the arguments be discrete, and that enough numbers
are provided to exactly fill all of the regions in row-major order) or the name of another
predicate (in which case it shares the same function

#### Define conditionals:
```lisp
(conditional conditional-name
&optional :conditions ; Quoted list of predicate patterns
:condacts ; Quoted list of predicate patterns
:actions ; Quoted list of predicate patterns
:function-default ; Default value for unspecified regions of function
:function-variable-names ; Variables in function (in order)
:normal ; Which variable(s) to normalize over in gradient descent learning
:function ; Function defined over function-variable-names
)
```
- function can be a number, a list of region functions, a list starting with row-major
followed by a list of numbers (requires that all of the variables be discrete, and that
enough numbers are provided to exactly fill all of the regions in row-major order) or
the name of another conditional (in which case it shares the same function, which
must use the same variables in the same order, and the same :normal variable if
there is to be learning)  

#### Define evidence: 
Define evidence by adding it to individual working memories
```lisp
(evidence evidence-list &optional empty-wms)
(perceive evidence-list) ; More appropriate version for use in perception. It enables
triggering of episodic and diachronic learning
```
#### Define goal for a predicate:  
```lisp
(goal predicate-name evidence-list) ; First argument is name of original predicate,
but the evidence list should use the name of its goal predicate (i.e., the original predicate
name followed by *goal
```
#### Initialize learning:
```lisp
(learn &optional <quoted list of learning specifiers>)
```
- :gd or :gradient-descent sets learn-via-gradient-descent to T
- :e or :episodic sets episodic-learning to T
- :am or :action-model sets automatic-action-models and diachronic-
prediction to T
- :pm or :perception-model sets automatic-perception-models and
diachronic-prediction to T
- :rl or :reinforcement-learning sets automatic-reinforcement-learning
and diachronic-prediction to T

Any form not mentioned is turned off  

### Format for predicate patterns (conditions, condacts and actions)
```lisp
(<predicate-name> <optional negation-symbol> <elements>) ; element-list must have an element for each argument in predicate definition
```
- negation-symbol: -
- element: (<argument-name> <value>)
- argument-name: a constant
- value: a or constant-region: 
 > (1) a constant, yielding an interval of length 1 starting there,  
 > (2) a *, yielding the entire scope of the dimension, or  
 > (3) a list (<min> <max>), where min and max are numbers specifying the  
beginning and ending of interval
; interval is always closed at min and open at max
- constant: a symbol or a number
- filters: (:filter <filter> ...)
- filter: (<constant-region> <constant> <coefficient>) ; Defines a linear value function over the
constant region. The outer product of the filters for all of the pattern’s arguments becomes
the message filter, which is multiplied times incoming messages.
- variable: (<variable-name> optional offset or <affine transform> or <filters> or <not-equal test>
or :explicit )
- variable name: A symbol (not a number)
- offset: a number specifying how far to offset from variable value (converted to affine)
- affine transform: (optional :from <variable-name> :offset <number or
variable> :coefficient <nonnegative number> :pad <nonnegative number>) ; :from
defaults to the current variable, :offset to 0, :coefficient to 1 and :pad to 0 for
closed-world predicates and 1 for open-world predicates. At present it shouldn’t be
necessary to use :from in an action, just use the corresponding condition variable, and
things may not work right in some cases if you do. :from can be used in conditions to map
from another variable, but it should only be used for variables in the same pattern. The
offset can only be a variable in an action, and then the smallest maximal domain element for
that variable is used as the offset.
- not-equal test: (<> <variable-name>); if the variable is discrete, and the variable after <> is of the
same type and has either been bound in the same pattern – if this is a condition – or in an
earlier condition (if this is a condition, action or condact), then limit matches of variable to
elements that are zero in <> variable (using an inverse of a delta function).
- :explicit : In a sense does the inverse of a summarization. Should only be used with a
variable in an action that is bound nowhere else, and where functional values are in [0, 1]. It
takes the functional value it receives and puts a 1 in the position along the variable’s type
corresponding to the fraction of the type before the functional value. <*** Highly
experimental ***>

### Format for a function
‘(<region-value> ...) or number ; If no function is specified, defaults to 1. Processes region-values
left to right, so can provide a default value first, and then alter subregions of it later
  
- region-value: (< function> <constant-region> ...) ; One constant-region per variable in
function  
- function: either a single number (for a constant function) or a list of numbers for the constant
and the coefficients associated with the variables

### Format for an item of evidence in evidence-list
(<predicate-name> <optional negation-symbol or function> <items>) ; Negation not allowed in
perception

- item: (<attribute> <constant-region>)

### Run a Sigma program
Pass up to n (default max-messages) messages or until quiescence:
```lisp
(run &optional n do-pre-run)
[(r &optional n do-pre-run)]
```
Make up to n (default max-decisions) decisions (each with up to max-messages messages if specified):
```lisp
(decide &optional n max-messages)
[(d &optional n max-messages)]
```
If n =0, break before decision; and if n=’d, continue through decision after break

Run n (default 1) trials (each of which makes decisions until halted):
```lisp
(trials &optional n continue)
[(ts &optional n)] ; continue is nil
```
Process a specific message from the queue:
```lisp
(m message-number)
```
To interrupt a run:
Type any character

### Debug
Recompute a message from one node to another with lots of tracing:
```lisp
(debug-message node-number1 node-number2 &optional regions)
```
Undo the tracing turned on by debug-message (for when left on by an error)
```lisp
(undo-debug-traces)
```
Compute product of two PLMs with a null variable mapping:
```lisp
(product-plms plm1 plm2)
```
Compare two arrays of messages from ends of decisions:
```lisp
(compare-gmas gma1 gma2)
```
Compare two lists of message arrays (from first to last decision):
```lisp
(compare-gma-lists gmal1 gmal2)
```
[set track-graph-messages to T to have such a list saved in global-graph-messages ]

Break before a decision where there is a positive message between any listed pair of node numbers
```lisp
break-on-positive-messages
```
### Interactive Graphical Display
Create interactive graphical representation of graph:
```lisp
(g &optional size conditional-name) ; Size is small, medium or large
```
Highlight a particular node in the interactive graphical representation
```lisp
(highlight-node node-number &optional color)
[(h node-number &optional color)]
```
Scroll so that a particular node is in middle of the interactive graphical representation
```lisp
(scroll-to-node node-number)
[(s node-number &optional color)]
```
Scroll to and highlight a particular node in the interactive graphical representation
```lisp
(sh node-identifier &optional color)
```
node-identifier can be a node number or a predicate name, in which case it goes to the
predicate’s WMFN node, or a conditional name, in which case it goes to the conditional’s CF

### To print
Alpha memories:
```lisp
(print-alpha-memories &optional conditional-name symbolic stream)
[(pam) &optional conditional-name {t trace-stream}]
```
Conditional(s):
```lisp
(print-conditional conditional &optional current-function reordered-
conditions stream)
[(pc conditional-name &optional current-function reordered-
conditions stream) ]
[(pcs &optional current-function reordered-conditions stream) ] ; All
```
Conditional function (current):
```lisp
(pcf conditional-name &optional array)
(pcfs &optional array) ; Print all defined conditional functions
(print-expected-value-cond-function conditional-name &optional variable array) 
; Print expected value of conditional’s function along (:normal or only) unique/distribution variable (or variable specified)
[(pevcf conditional-name &optional variable array)]
```
Conditional pattern matches (i.e., what messages match to each pattern in a conditional):
```lisp
(print-pattern-matches conditional-name &optional stream)
[(ppm conditional-name)]
```
Dimension:
```lisp
(print-dimension dimension-min dimension-max &optional variable(s) symbolic stream weight)
```
Factor function:
```lisp
(print-factor-function node-name &optional symbolic stream)
```
Factor step(s):
```lisp
(print-factor-step factor-step node &optional stream)
(print-factor-steps node &optional stream)
```
Function(s):
```lisp
(pafs &optional array) ; Print all defined functions
(pcfs &optional array) ; Print all defined conditional functions
(ppfs &optional array) ; Print all defined predicate functions
```
Graph:
```lisp
(print-graph &optional symbolic stream)
```
Graph statistics (nodes, variables, links):
```lisp
(graph-statistics &optional stream)
[(gs &optional stream) ]
```
Episodic memory:
```lisp
(pem &optional predicate-name) ; Print function(s) in episodic memory
(pep) ; Print variable nodes with predictions for episodic predicates
```
Link:
```lisp
(print-link link &optional symbolic stream)
```
Link depths:
```lisp
(print-link-depths &optional stream)
[(plds &optional stream)]
```
Link counts (number of messages along links during current decision):
```lisp
(print-link-counts &optional nhighest minimal-count)
[(plc &optional nhighest minimal-count) ] ; nhighest is how many (from high to low) to print
; Don’t print link direction if its count is less than minimal-count
```
Link message sizes (sizes of messages along links at present):
```lisp
(print-link-message-sizes &optional nhighest minimal-size)
[(plms &optional nhighest minimal-count) ]
; nhighest is how many (from high to low) to print
; Don’t print link direction if its message size is less than minimal-size
```
Message(s):
```lisp
(print-message trace-header link index &optional symbolic reverse print-first stream)
(print-message-times &optional stream) ; trace-message-times must have been set to T before running for this to work
[(pmt)]
(print-in-messages node &optional symbolic stream)
[(pim node-name) {T T}]
(print-out-messages node &optional symbolic stream)
[(pom node-name) {T T}]
(pm from-node-number to-node-number &optional symbolic stream)
```
Message statistics (for messages currently in graph):
```lisp
(message-statistics &optional stream)
[(ms &optional stream) ]
```
Node:
```lisp
(print-node node &optional symbolic stream)
[(pn node-id &optional symbolic stream) ] ; node-id is either number or name
```
Factor nodes:
```lisp
(print-factor-nodes &optional symbolic stream)
[(pfns) {t trace-stream }]
Function factor nodes:
(print-functions &optional symbolic stream)
[(pfs &optional as-array) {t trace-stream }]
```
Variable nodes:
```lisp
(print-variable-nodes &optional symbolic stream)
[(pvns) {t trace-stream }]
```
List of node names:
```lisp
(print-node-names &optional stream)
[(pnn) {trace-stream )]
```
Neighbors of node:
```lisp
(print-node-neighbors node &optional stream)
```
Type information about a node:
```lisp
(print-node-type node &optional stream)
[(pnt node-number) ]
```
Operators (by id, when defined by predicates)
```lisp
(pid id-number) ; Print an operator description from its id number
(pids) ; Print descriptions of all operators from ids
```
PLM:
```lisp
(print-plm {plm or node-number or predicate-name or conditional-name} &optional symbolic stream limits)
[(pplm {plm or node-number or predicate-name or conditional-name }) {t trace-stream nil}]
```
- For variable-node number, prints posterior (normalized if a WMVN node)
- For factor-node number, prints function
- For predicate-name, prints WMFN function
- For conditional-name, prints conditional function
```lisp
(parray {plm or node-number or predicate-name or conditional-name} &optional regions summarize stream)
[(pa {plm or node-number or predicate-name or conditional-name} &optional regions summarize stream)]
; Prints functions of any dimension as arrays
```
- For variable-node number, prints posterior (normalized if a WMVN node)
- For factor-node number, prints function
- For predicate-name, prints WMFN function
- For conditional-name, prints conditional function
```lisp
(print-smart {plm or node-number or predicate-name or conditional-name } &optional symbolic stream summarize limits)
[(ps {plm or node-number or predicate-name or conditional-name} &optional stream summarize limits)]
; Intelligently decide whether to use pplm or pa based on max-span-pa (and whether or not to print an array as regions based on max-fraction-pa-regions)
```
Predicate:
```lisp
(print-predicate predicate &optional current-function stream)
```
All predicates (or just those that include the state)
```lisp
(print-predicates &optional current-function state stream)
[(pps &optional current-function state)]
```
Predicate from its name
```lisp
(print-predicate-from-name predicate-name &optional stream)
[(pp predicate-name)]
```
Predicate perception function
```lisp
(print-predicate-perception-function predicate &optional symbolic stream limits)
[(pppfn predicate-name &optional limits) ]
[(pppfns)] ; Print contents of all perception functions
```
Predicate function (current)
```lisp
(ppf predicate-name &optional array)
(ppfs &optional array) ; Print all defined predicate functions
(print-expected-value-pred-function predicate-name &optional variable array) 
; Print expected value of predicate’s function along (:unique or only) unique/distribution variable (or variable specified)
```lisp
[(pevpf conditional-name &optional variable array)]
```
Preference messages (for selected predicate)
```lisp
(print-preferences)
[(pprefs)]
```
Queue(s):
```lisp
(print-queues &optional symbolic stream)
[(pqs) {t trace-stream}]
(print-queue queue-number symbolic stream)
[(pq queue-number) {t trace-stream }]
```
Region:
```lisp
(print-region region plm &optional symbolic stream)
```
Slices:
```lisp
(print-plm-slices plm &optional symbolic stream)
[(ps plm) {nil T} ]
```
Types:
```lisp
(print-type type &optional stream)
[(pt type) {nil}]
```
Type form its name:
```lisp
(print-type-from-name type-name)
[(ptn type-name) ]
```
All types:
```lisp
(print-types &optional stream)
[(pts) {nil}]
```
Variable(s):
```lisp
(print-svariable svariable &optional stream)
(print-svariables svariables &optional stream)
```
Working memory:
```lisp
(print-wm &optional symbolic stream)
[(pwm &optional as-array stream) {t }]
```
Alternate printout of working memory, allows for specific list of predicates, defaults to all, if array is true will print in array format:
```lisp
(print-wm-basic (&optional pred-list array stream)
[(pwmb (&optional pred-list array stream)]
```
A single predicate working memory:
```lisp
(print-pred-wm predicate &optional symbolic stream limits)
[(ppwm &optional predicate-name as-array stream) {t nil}]
```
####A single predicate working memory function:
```lisp
(print-pred-function predicate &optional symbolic stream limits)
[(ppfn predicate-name &optional limits as-array stream) {t}]
```
A single predicate working memory function (without preceding text):
```lisp
(print-pred-wm-function predicate &optional symbolic stream limits)
[(ppwmfn predicate-name &optional limits) {t trace-stream }]
```
A single predicate working memory variable node:
```lisp
(print-wm-vn predicate &optional symbolic stream limits)
[(ppvn predicate-name stream) {t nil}]
```
### Access functions
Access best value in a 1D PLM (or in the specified state of a 2D state PLM)
```lisp
(best-in-plm plm &optional state)
```
Access a conditional:
```lisp
(conditional-from-name conditional-name)
```
Access a conditional function node name:
```lisp
(conditional-function-node-name-from-conditional-name conditional-name)
[(cfn conditional-name)]
```
Access a link:
```lisp
(link-from-nodes node1 node2)
(link-from-numbers node-numer1 node-number2)
```
Access a message (actually, create a new message) or just function from message:
```lisp
(message-from-numbers node-number1 node-number2)
(message-function-from-numbers node-number1 node-number2)
[(mffn node-number1 node-number2)]
```
Access a message PLM:
```lisp
(mp node-number1 node-number2)
```
Access a node:
```lisp
(node-from-name node-name &optional node-type)
(node-from-number node-number)
[(nfn node-number) ]
```
Access a node’s function:
```lisp
(node-function-from-number node-number)
[(nffn node-number)]
```
Access list of names of node’s neighbors
```lisp
(node-neighbors node)
```
Access the operator selected for a state
```lisp
(operator-in-state state-number &optional agent)
```
Access a predicate:
```lisp
(predicate-from-name predicate-name)
```
Access a predicate’s perception node:
```lisp
(perception-node-from-name predicate-name)
```
Access a symbol name from its type and ordinal:
```lisp
(get-symbol-name ordinal type)
```
Access a type:
```lisp
(type-from-name type-name)
(type-from-predicate-argument argument predicate)
```
Access value in working memory of 2D predicate with state and one unique argument
```lisp
(value-in-state predicate-name state-number argument-name &optional agent)
```
Access a variable:
```lisp
(variable-from-name name svariables)
```
Access value of a discrete 1D working memory (no state argument) with a unique argument
```lisp
(nonstate-value predicate-name)
```
Access the posterior of a shared WM VN node:
```lisp
(vn-posterior predicate)
[(vnp predicate-name)]
```
### Structures  
```lisp
beta-memories
```
```alpha``` (variable node or nil): End of alpha network for new pattern  
```old-beta``` (variable node or nil): Beta memory or end alpha network for previous  
```new-beta``` (variable node or nil): New beta memory after beta factor  
```lisp
conditional
```
```name``` (symbol): Name of conditional  
```conditions``` (list of condition patterns)  
```condacts``` (list of condact patterns)  
```actions``` (list of action patterns)  
```variables``` (list of svariable structures)  
```function``` (constant or list of linear region functions): Function of function-variable-names (default 1)  
```function-default``` (0 or 1): Value to be used for unspecified regions in conditional’s  
```function``` (default 0)  
```function-node``` (a node): Factor node in which conditional’s function is stored  
```function-variable-names``` (list of variable names used in function, in order used)  
```condition-later-variable-names``` (list of lists of variable names): Variables in conditions used later in conditional  
```condact-later-variable-names``` (list of lists of variable names): Variables in condacts  used later in conditional  
```action-later-variable-names``` (list of lists of variable names): Variables in actions used later in conditional  
```last-memory``` (node ): Last beta memory in conditional’s graph  
```last-condition-memory``` (node): Last condition alpha memory in conditional’s graph  
```normal``` (nil or variable name): Variable over which to normalize during gradient descent  
```shared-action-beta``` (beta memory): Variable node connecting conditional’s actions  
```map``` (T or nil): Whether to summarize out unique variables at node by max (versus integration)  
```learning-rate``` (number in (0,1]): Learning rate for conditional’s function (if specified)  
```smoothing-parameter``` (number): Smoothing parameter for CF node (if specified)  
```episodic``` (T or nil): Whether conditional is part of episodic memory (default nil )  
```reordered-conditions``` (list of condition patterns): In order as automatically reordered  
```lisp
descendant ;(within node )
```
```node``` (node): Descendent node  
```parents``` (list of node numbers): Parents of node by which it descends  
```lisp
descendant-link ;(within node)
```
```link``` (link): Descendent link  
```direction``` (0 or 1): From variable (0) or factor (1) node  
```from``` (node): Number of source node of link direction  
```to``` (node): Number of target node of link direction  
```lisp
dimension ;(within region )  
```
```min-slice``` (slice ): Lower bound on dimension  
```max-slice``` (slice ): Upper bound on dimension  
```weight``` (number): Weight of dimension’s variable in linear function  
```discrete``` (T or nil): Whether dimension is discrete 
```lisp 
factor-step ;(within node ): Define a summarization or combination (product) step  
```
```type``` (INTEGRAL , MAX or PRODUCT ): Operation to perform  
```argument``` (dimension number or link): Argument of operation
```lisp
graph
```
```types``` (list of type): List of all of the types in the graph  
```predicates``` (list of predicate ): List of all of the predicates in the graph  
```nodes``` (list of node): List of all of the nodes in the graph  
```node-vector``` (vector of nodes): Vector of all of the nodes in the graph  
```links``` (list of link)  
```conditionals``` (list of conditional ): List of all of the conditionals in the graph  
```pattern-vars``` (list of pattern-variable ): Vestigial for explicit sharing of nodes  across conditionals  
```prequeue``` (list of message ): List of messages to go into queues  
```queues``` (queues of message )  
```node-count``` (integer): Number of nodes in graph  
```last-node``` (link ): Last node created in graph  
```positive-preferences``` (list of nodes): Positive action factor nodes for selected predicate    
```negative-preferences``` (list of nodes): Negative action factor nodes for selected predicate    
```depth``` (integer): Depth of graph  
```agents``` (integer): Number of agents  
```operator-type-name``` (name of type): Name of type to use for operators in selection  
```initialized``` (T or nil): Whether graph has been initialized  
```operator-predicates``` (list): Predicates that determine operators, if any  
```selected-predicate``` (predicate): The selected predicate if there is one  
```changes``` (list of node): Assumption nodes whose functions have changed    
```goals-set``` (T or nil): Whether predicate goals have been set (by initializing WMFNs)    
```goal-list``` (list of evidence lists): Lists from calls to goal for initializing predicate goals  
```agent-type``` (type): Definition of agent type  
```predicate-type``` (nil): A symbolic type that includes all of the predicate names  
```surprise-predicate``` (predicate): Predicate with a global measure of surprise (in PBFN)  
```surprise-distribution-predicate``` (predicate): Predicate with surprise distribution over predicates (in PBFN)  
```surprise-predicates``` (list of predicate): List of predicates for which surprise will be computed  
```progress-predicate``` (predicate): Predicate with a global measure of progress (in PBFN)  
```progress-distribution-predicate``` (predicate): Predicate with progress distribution over predicates (in PBFN)  
```progress-predicates``` (list of predicate): List of predicates for which progress will be computed  
```difference-predicate``` (predicate): Predicate with a global measure of difference (in PBFN)  
```difference-distribution-predicate``` (predicate): Predicate with difference distribution over predicates (in PBFN)  
```difference-predicates``` (list of predicate): List of predicates for which attention will be computed  
```attention-predicate``` (predicate): Predicate with a global measure of attention (in PBFN)  
```attention-distribution-predicate``` (predicate): Predicate with attention distribution over predicates (in PBFN)  
```attention-predicates``` (list of predicate): List of predicates for which attention will be computed
```lisp
link ;Link between a pair of nodes
```
```map``` (smap)  
```nodes``` (vector of two nodes ): Variable and factor nodes on link  
```contents``` (vector of two plms): Messages from variable and factor nodes on link  
```depths``` (vector of two integers): Depth of link in each direction from WM and FF factors  
```loop-depths``` (vector of two integers): Pseudo-depth of link within a loop in each direction from WM and FF factors  
```inits``` (vector of two integers): Decision on which message from node was last initialized  
```incoming``` (vector of two T or nil): Whether incoming messages in parallel mode  
```in``` (T or nil): T for links in actions and condacts, nil for conditions  
```out``` (T or nil): T for links in conditions and condacts, nil for actions  
```counts``` (vector of two integers): Number of messages sent in link directions on current decision cycle  
```stale``` (T or nil ): T for links that may not have the correct value currently stored on them given the input messages at the source node  
```lisp
smap ;(within link) How variable node variables map onto variables in factor function (short for Sigma map)
```
```vfactor``` (vector of integers): Factor node variable index from variable node variable index  
```fvar``` (vector of integers): Maps from summarized product in factor node back to variable node  
```lisp
message ;(within queue )
```
```index``` (0 or 1): Index into a link’s arrays (0 for variable and 1 for factor) for direction  
```link``` (link): Link along which message is to be sent  
```wm-driven``` (T or nil): Whether message is driven by change in WM  
```lisp
node ;Factor and variable nodes in graph
```
```name``` (symbol)  
```type``` (factor or variable )  
```subtype``` (alpha , beta, delta, combine , constant , function , inversion , offset, split or wm)  
```subsubtype``` (match , negative , nil, positive or sum)  
```object``` (capi:expandable-item-pinboard-object ): Object in graphical window for node   
```evidence``` (list of evidence nodes)  
```variables``` (list of svariables or list of pairs of svariables for delta factors)  
```factor-steps``` (list of factor-steps)  
```function``` (a plm ): Function for factor nodes  
```links``` (list of links): Links attached to node  
```number``` (integer): Node’s number  
```action``` (T or nil ): Whether is a special action node for combining shared actions  
```descendants``` (list of descendant ): Nodes dependent on values of assumption nodes  
```descendant-links``` (list of descendant-link): Links dependent on values of assumption nodes  
```region-pad``` (nil or 0 or 1): What padding to use for new regions in offsets and affines  
```normal``` (nil or integer): Dimension over which to normalize during gradient descent  
```linear-filter``` (T or nil ): Whether is a filter factor node with a linear filter  
```pattern-type``` (condition , action , condact or nil): What kind of pattern a variable node is associated with  
```exponential``` (T or nil): Whether to exponentially transform messages out if WMVN  
```normalize``` (T or nil): Whether to normalize outgoing messages from node  
```integral``` (T or nil): Whether to summarize out unique variables at node by integration (versus max) [Is overridden if default-integral is nil]  
```predicate``` (predicate ): When node is a WM FN or FAN or PFF points to corresponding
predicate  
```learning-rate``` (number in (0,1]): Learning rate for CF node (if specified on conditional)    
```smoothing-parameter``` (number): Smoothing parameter for CF node (if specified on conditional)  
```changes``` (non-negative integer): Number of times node function has been changed by gradient-descent learning  
```shared-functions``` (list of CFF nodes): Conditional factor function nodes that share the same function  
```variables-same``` (list of variable-node names): Adjacent variable-node names for a beta factor with the same variables (in the same order)  
```restriction``` (plm): Boolean function multiplied times results of learning to insure that regions that were originally 0 remain 0  
```function-name``` (symbol): For a function node, name of conditional or predicate that generated it  
```wmvn``` (T or nil): Whether node is a working memory variable node  
```beta-non-full``` (T or nil): If node is a beta factor, whether its function is not a constant 1; that is, whether there are not-equal (<>) tests to be performed   
```assumption``` (T or nil): Whether is an assumption node (messages out don’t change during message passing)  
```vector``` (T or nil): if the node performs vector normalizations  
```surprise-predicate``` (predicate): Surprise predicate pointer from predicate's function node  
```lisp
pattern-variable ;Variable for explicit pattern sharing (vestigial)
```
```name``` (symbol)  
```wm-memory``` (node): WM variable node for sharing  
```variable-names``` (list of symbols): Names of variables in WM variable node  
```lisp
plm ;A piecewise linear function
```
```rank``` (integer): Number of dimensions  
```active``` (vector of T or nil ): T for variables active (vestigial?)  
```variables``` (vector of svariables ): Variables corresponding to dimensions  
```array``` (PLM array): Array of regions  
```slices``` (vector of lists of slices ): Slices through the dimensions  
```removed-unneeded-slices``` (T or nil): Whether have removed unneeded slices  
```piecewise-constant``` (T or nil): Whether regions in function are all constant in value  
```lisp
predicate ;Relational structure for use in conditionals
```
```name``` (symbol)  
```world``` (closed or open): Defaults to open  
```arguments``` (list of <arguments>)  
```wm``` (node): Working memory factor node  
```wm-variables``` (list of variables): Variables in predicate’s working memory factor node  
```em``` (node): Episodic memory factor node  
```em-predicate``` (predicate or nil): Episodic memory predicate for predicate  
```function``` (constant or list of linear region functions): Function of function-variable-names (default 1)  
```function-default``` (0 or 1): Value to be used for unspecified regions in predicate’s function (default 0)  
```function-node``` (a node): Node in which predicate’s function is stored (if defined)  
```function-variable-names``` (list of variable names used in function, in order used)  
```perception``` (T or node or nil): If T or node, there is perception for the predicate (user specifies T, system replaces with pointer to perception node) (default is nil)  
```perception-temp-mem``` (a plm): Temporary memory for aggregating perceptions during a cycle before storing the result into the factor node  
```persistent``` (T or nil): Whether values of predicate persist in its WMFN  
```predict``` (predicate): Predicate for diachronic prediction of current predicate  
```prediction``` (node or nil): If predicate is used for diachronic prediction of another predicate then WMFN for other predicate; else nil  
```prediction-link``` (link): Prediction link for diachronic processing  
```exponential``` (T or nil): Whether to exponentially transform messages out of WMVN  
```assign-ids``` (a plm): Function used to assign ids via a factor attached to WMVNs  
```operators``` (integer): Number of operator (ids) corresponding to predicate, if any  
```first-operator``` (integer): First id for operators corresponding to predicate, if any  
```id-contents``` (vector of lists): Operator descriptions associated with ids  
```outgoing-vn``` (node): WMVN for conditions, condacts and chained actions  
```incoming-vn``` (node): WMVN for actions  
```no-normalize``` (T or nil): When true, overrides the default for non-multiple predicates and doesn’t normalize the messages coming out of the WMVN node (and for unique predicates, combines positive actions via addition and negative actions via subtraction  
```unique``` (list of symbols): List of names of non-multiple argument(s)  
```unique-for-gdl``` (list of symbols): List of names of non-multiple argument(s) for use in divisor within gradient-descent learning  
```select``` (nil, best , prob-match, boltzmann or expected ): What kind of selection, if any to do over the unique variable, if there is one  
```agent-arg-index``` (positive integer): Index in arguments of agent argument if there is one  
```fan``` (node): Combination factor node for actions  
```replace``` (T or nil ): Whether to replace current values in WMFN for the predicate with values in actions (versus updating current values)  
```learning-rate``` (number in (0,1]): Learning rate for predicate’s function (if specified)  
```smoothing-parameter``` (number): Smoothing parameter for predicate’s function (if specified)  
```condact-conditionals``` (list of conditionals): Conditionals that use the predicate in a condact  
```episodic``` (T or nil): Whether predicate is part of episodic memory (default nil)  
```no-models``` (T or nil): Whether to ignore predicate in generating perception and action models (default nil)  
```goal-predicate``` (predicate): Predicate in whose WM the predicate’s goal is stored (its name is the original predicate name with *goal added at end)  
```progress-predicate``` (predicate): Predicate whose function tracks amount of progress towards predicate’s goal (its name is the original predicate name with *progress added at end)  
```action-function``` (<function>): When doing automatic action modeling, the action model for the predicate is initialized with this function. *At the moment this is a hack you shouldn’t use unless you really know what you are doing, as the variables used in this function are not the predicate’s variables.*    
```perception-function``` (<function>): When doing automatic perception modeling, the perception model for the predicate is initialized with this function. *At the moment this is a hack you shouldn’t use unless you really know what you are doing, as the variables used in this function are not the predicate’s variables.*  
```vector``` (T or nil): whether this predicate represents a vector  
```surprise-predicate``` (predicate): Predicate for surprise map  
```progress-predicate``` (predicate): Predicate for progress map  
```difference-predicate``` (predicate): Predicate for difference map  
```surprise-predicate``` (predicate): Predicate for attention map  
```automated-perception``` (T or nil): Whether this predicate is automatically created by the template for perception learning to hold the learned perception function  
```no-surprise``` (T or nil): Whether a predicate should not have surprise computed for it  
```goal``` (<function>): Goal function for predicate  
```lisp
queue ;List of messages in queue
```  
```head```: Pointer to pseudo message that remains fixed at front of queue  
```tail``` (message): Pointer to last message in queue  
```lisp
region ;(within plm)
```  
```constant``` (number): Constant in region’s linear function  
```dimensions``` (vector of dimensions )  
```maximals``` (vector of (min max) lists): Segments of dimension yielding maximal values  
```bad``` (T or nil): Whether translated region is completely outside of type’s scope  
```lisp
slice ;(within plm): Location of a slice across a dimension
``` 
```location``` (integer)
```lisp
stype ;Variable/dimension type (short for Sigma type)
```
```name``` (symbol)  
```numeric``` (T or nil): Defaults to nil  
```discrete``` (T or nil): Defaults to nil  
```constants``` (list of symbols): Defaults to nil (when specified, discrete is set to T)  
```min``` (number): Beginning of range of type (defaults to 0 when constants specified)  
```max``` (number): End of range of type (defaults to number of constants when constants specified)  
```span``` (positive integer): abs(max-min)  
```lisp
svariable ; (short for Sigma variable)
```
```name``` (symbol)  
```type``` (type)  
```unique``` (T or nil ): Whether variable is unique or universal/multiple  
```select``` (nil, best , prob-match, boltzmann or expected ): What kind of selection, if any to do over this variable.  

### System Types

Users should never define types with these names even when the system doesn’t define them
It is also safest to avoid predicates ending in: *goal or *progress if using architectural goal
specification; and *episodic , -learn, -select and –retrieve if using episodic learning.  
```lisp
Detect-impasses [open]: (state state) (value flag %) ; Whether to detect impasses in the state  
Evaluate-operator[open]: (evaluate operator) (operator operator) ; If detect-impasses is true  
Halt[closed]:  
Impasse[closed]: (state state) (operator operator) (type impasse !) ; If
detect-impasses is true (and will have (agent agent) argument if multiagent)
Selected[closed]: (state state) (operator operator !) ; If operators have been
defined (and will have (agent agent) argument if multiagent)
State[open]: (state state) ; Will have (agent agent) argument if multiagent
Time[closed]: (value time) ; Number of decisions that have passed
```
### Parameters and constants

```action-list```: nil (list of forms to execute as external actions)  
```adaptive-learning-rate```: nil (t) ; Dynamically determine learning rate at each node (when not overridden by learning rate on conditional) [Only one of this and learning-rate-fraction-of-smoothing-parameter should set] [*** Not recommended at present ***]  
```adaptive-smoothing``` : t (nil) ; Dynamically determine smoothing (for learning) at each node  
```all-condact-filters-pad-1```: nil (t) ; Invert filter messages for condact messages moving away from WM (this is already done for condact messages heading towards WM)  
```always-max-operators``` : nil (t); If true, only use max when summarizing out operators  
```arousal``` : nil (positive number ) ; Level of arousal [default is off] (set manually)  
```automatically-reorder-conditions``` : t (nil ) ; Automatically reorder conditions in conditionals for efficiency  
```automatic-action-models``` : nil (t) ; Automatically create conditionals for action modeling (aka learning of transition functions)  
```automatic-perception-models``` : nil (t) ; Automatically create conditionals for perception modeling (including map learning)  
```automatic-reinforcement-learning``` : nil (t ) ; Automatically create types, predicates and conditionals for reinforcement learning  
```bipartite-graph-display``` : t (nil) ; Use new form of factor graph display in graph/g  
```center-discrete-numeric-on-integer``` : nil (t); If true, center unit regions for discrete numbers around the integers rather than beginning the regions at the integers (needs to be set via optional argument to init rather than directly) [Warning : May not work correct at this point with either impasses or multiple agents.]  
```compute-attention```: nil (t) ; Compute attention for predicates with progress/difference and/or surprise  
```compute-progress```: nil (t) ; Compute progress (and difference) for predicates with goals  
```compute-surprise```: nil (t) ; Compute surprise for predicates with functions  
```condacts-change-wm```: nil (t) ; Condacts send messages back to WM factor node (but don’t actually lead to changes in WM)  
```break-on-positive-messages``` : nil ; A list of node-number pairs (each a list). Breaks before any decision with a non-empty message between those nodes at quiescence  
```cycle-message-counts``` : nil ; If tracing cycles, history of messages per cycle  
```debug-descendants``` : nil (list of node numbers) ; Trace descendant and message initialization computations for node numbers in list  
```debug-init-descendants``` : nil (‘summary or ‘all ) ; Trace descendant initialization, either just summary statistics (‘summary ) or also how many descendants each has (‘all)  
```decision-count```: 0 ; Number of decisions since init  
```default-integral``` : t (nil) ; Use integral by default (rather than max) for unique variables  
```detect-impasses``` : nil (t) ; Whether to impasse on operator selection if tie (needs to be set via optional argument to init-operators rather than directly)  
```diachronic-prediction``` : nil (t) ; Predict results of actions/operators  
```discount-wm```: t (nil) ; Discount (by wm-discount-factor ) messages from WMFNs to FANs for selection predicates  
```display-subgraph``` : nil (t): Automatically pop up a restricted window showing the region around a node when go to it in the graphical display via sh (or via the entry box)  
```do-not-learn```: nil (list of conditional and predicate names): Conditionals and predicates for which not to do gradient-descent learning  
```episodic-learning``` : nil (t, ‘open, ‘closed ) ; Perform episodic learning. Should use the function ```learn``` to set and unset this variable  
```epsilon``` : .0000001 (real number) ; Accuracy within which to compute absolute value comparisons (when use-relative-epsilon is nil)  
```epsilon2``` : .0001 (real number) ; Span of continuous maximal regions  
```exponential-product-gradient``` : nil (t) ; Use an exponential product gradient rather than a traditional additive gradient. This is still experimental and not yet ready for general use  
```extend-type-constants-by-evidence``` : nil (t) ; Instead of signaling an error if a constant is used in evidence or perceive that isn’t in the type, extend the type to include it  
```fan-constant``` : .001 ; Constant at open-world FAN nodes to ensure that actions for non-matching conditionals don’t clamp the WMVN at 0  
```feedback-to-functions``` : t (nil) ; Send feedback messages to factor function nodes
```field-beyond-label``` : 3 ; Extent of field beyond length of largest label  
```function-default``` : 0 ; Value to use by default for unspecified regions of conditional/predicate functions  
```gdl-divide``` ‘new (t, nil) ; Divide gradient by function-message product (new version uses a different divisor within each region of universal variable values)  
```gdl-subtractive-normalization``` t (nil) ; Use subtractive rather than divisive normalization in gradient-descent learning  
```gradient-subtract-average``` t (nil) ; Subtract average of gradient before updating  
```integrate-universal-in-unique``` nil (t) ; Integrate rather than max over universal variables in unique functions (**very experimental**)  
```infinity``` : (most-positive-fixnum-10) ; For scope of temporal variable  
```base-level-state```: 0 ; State at which processing starts in reflective hierarchy  
```learning-rate```: .05 (real number) ; Learning rate for gradient descent  
```learning-rate-fraction-of-smoothing-parameter``` : nil (positive number) ; If a number, the learning rate is set to this times the smoothing parameter [Only one of this and ```adaptive-learning-rate``` should be set] [Intended to be used in conjunction with adaptive smoothing , and to be <1]  
```learn-no-normal``` : nil (t) ; Learn for conditionals without a normal variable  
```learn-open```: nil (t) ; Learn for open-world predicates as well as closed-world ones  
```learn-via-gradient-descent``` : nil (t) ; Use gradient descent on function factor functions. Should use the function learn to set and unset this variable  
```max-decisions``` : 500 ; Maximum number of decisions to execute when no value specified  
```max-elaboration-cycles``` : 50 ; Maximum number of (parallel) cycles per decision  
```max-final-discrete``` : 10 ; Maximum number of items in the final region along a discrete numeric dimension to list explicitly  
```max-fraction-pa-regions``` : .5 (in [0,1]); Threshold for shifting from regions in parray within print-smart  
```max-gdl-increment``` : 1 (positive number or nil); Maximum increment added during gradient-descent learning (rescales increment after learning rate applied if the largest value is greater than this)  
```max-messages``` : 10000 ; Default maximum number of messages (per decision)  
```max-messages-links``` : 20 ; When non-nil, set max-messages to this times number of links  
```max-span-pa``` : 10 ; Threshold on size of symbolic dimension in shifting from parray to pplm in print-smart  
```max-states``` : 100 ; When number of states (height of goal hierarchy)  
```max-time```: 999999 ; Size of temporal dimension (and max number of cycles, if time is being tracked via track-time, due to incrementing time each cycle)  
```message-count```: 0 ; Number of messages since init  
```message-protocol``` : ‘serial (parallel ) ; Pass messages serially versus simulated parallel  
```minimal-parray-field``` : 13 ; Minimum size of a field for a PLM array print  
```multiagent``` nil (t) ; Default to multiple agents (mostly set automatically within init )  
```non-operator-best-select```: ‘random (‘first ) ; Choose randomly among maximals for regions in predicates other than selected (or first maximal)  
```one-way-c-a-betas``` : t (nil) ; Use unidirectional beta networks within conditions and actions  
```open-actions-like-condacts``` : nil (t) ; Invert filter messages for open-world actions, and combine via product, as normally do action part of condacts  
```open-conditions-like-condacts``` : nil (t) ; Connect outgoing message from WMFN directly to WMVN (via product) instead of going through a VAN/FAN (probabilistic or) path (and invert filter messages for open-world conditions if all-condact-filters-pad-1 is T)  
```open-world-wmfns```: nil (t) ; WMFN nodes are generated for open-world predicates  
```operator-best-select``` : ‘random (‘first ) ; Choose randomly among maximals for operator in selected predicate (or first maximal)  
```parameter-override-reset```: nil (list of parameter names [without bracketing *s]) ; These parameter are not reset by reset-parameters (this parameter is itself not automatically reset)  
```perceive-list```: nil (list of forms to execute for perception)  
```pre-d```: nil ; Forms to evaluate after messages are sent but before decision and WM changes (occurs after both runs for diachronic decisions)  
```pre-run``` nil ; Forms to evaluate before messages are sent within a decision (only for the first run within a diachronic decision)  
```pre-t```: nil ; Forms to evaluate before each trial begins  
```pre-ts```: nil ; Forms to evaluate before sequence of trials begins  
```print-regions-on-separate-lines```: nil (t); Print each region in a PLM on a separate line  
```post-d```: nil ; Forms to evaluate after decision is made and WM is changed  
```post-run``` nil ; Forms to evaluate before messages are sent within a decision (only for the first run within a diachronic decision)  
```post-t```: nil ; Forms to evaluate after trial is over (on a explicit halt, via haltp)  
```range-field-beyond-label```: 3 ; Extent of range field beyond twice length of largest label  
```reset-parameters-in-init```: t (nil) ; Whether to automatically reset all parameter in init (this parameter is not itself reset in init)  
```save-message-state``` : t (nil) ; Only reinitialize messages across decisions that depend on changed functions  
```smoothing-parameter``` : .000001 (number) ; Minimum value in a learned function  
```sparse-product``` : t (nil) ; Use optimization for multiplying a sparse function times another function  
```sparse-product-threshold``` : number in [0,1]; When sparse-product is T, use optimization when function density below threshold  
```specify-function-variable-names```: t (nil) ; Explicitly specify variables used in function  
```symbolic-trace```: t (nil) ; Traces should be symbolic  
```temperature```: (number) ; Current temperature for Boltzmann selection (not for user setting)  
```temperature-minimum``` : 1/60 (number) ; Temperature remains above this to avoid overflow  
```temperature-schedule```: #’inverse-log (function) ; Function that updates temperature on each trial (default sets it as 1/log(1+trial-count)  
```threshold-for-dense-connection``` : 5 ; If the number of bidirectional links at a variable node is more than this threshold, break up into a binary tree of nodes and links  
```trace-affine``` : nil (t) ; Trace computation at affine transform factor node  
```trace-attention``` : nil (t) ; Trace attentional processing (including appraisals)  
```trace-average``` : nil (t) ; Trace the processing that creates an averaged version of a PLM  
```trace-combine``` : nil (t ‘region) ; Trace combination (product/sum). Print individual region combinations if ‘region  
```trace-cycles``` : t (nil) ; Trace elaboration cycles  
```trace-decisions``` : t (nil) ; Trace decision cycles  
```trace-el```: nil (t) ; Trace episodic learning  
```trace-empty```: nil (t) ; Print regions in PLMs with 0 values  
```trace-full``` t (nil) ; Print regions in PLMs with 1 values  
```trace-gdl```: nil (t or list of node numbers, node names, conditional names and predicate names) ; Trace gradient-descent learning (for all nodes if t or just the specified function factor nodes)  
 ```trace-impasses``` : t (nil) ; Print impasses at decision time  
```trace-link-depths``` : nil (t) ; Trace depths of links for messages  
```trace-maximals``` : nil (t) ; Trace lists of maximal regions  
```trace-messages``` : nil (t or a list of node names or numbers) ; Trace messages (to and from nodes named/numbered)  
```trace-message-times``` : nil (t) ; Trace times taken for messages  
```trace-perception``` : nil (t) ; Trace perception  
```trace-performance``` : t (nil) ; Trace performance data about decisions  
```trace-preferences``` : nil (t) ; Trace preference messages for selected predicate  
```trace-queue```: nil (t) ; Trace the queue to which each new message is added  
```trace-states```: t (nil ) ; Trace addition and deletion of states from hierarchy  
```trace-stream``` : t (nil ) ; Default stream to use for traces  
```trace-summarize``` : nil (t) ; Trace summarization (integral/max)  
```trace-transform``` : nil (t) ; Trace transformations (including normalizations)  
```trace-trials``` : t (nil ) ; Trace trials  
```trace-wm-changes``` : nil (t or a list of predicate names) ; Trace the process of deciding which WM changes to make (just for predicates listed if there is a list)  
(trace-wm-changes [boolean or list]) ; Set trace-wm-changes and trace-states  
```trace-wm-driven``` : nil (t) ; Trace whether messages are wm-driven  
```track-graph-messages``` : nil (t) ; Store messages at the end of each decision into a list  
```track-time```: t (nil) ; Create time predicate and increment each decision (this parameter is not automatically reset during init)  
```trial-count```: 0 ; Number of trials since init  
```unique-weights-only``` : t (nil) ; Generate error if region has a weight for a universal variable  
```unsup-categories``` : 5 (integer) ; Number of categories for unsupervised learning  
```use-relative-epsilon``` : t (nil) ; Use relative rather than absolute numeric comparisons  
```wm-discount-factor``` : .01 (positive number) ; Factor to use when discount-wm is T  

### Saving Sessions & Graphs

save a graph to a file 
```lisp
(save-graph "mygraph")
```
load graph from file 
```lisp
(load-graph "mygraph")
```
save session (lispworks)
```lisp
(save-sigma-session "mysession")
```
save session (CLISP): automatically saves file as ```lispinit.mem```
```lisp
save-sigma-session ()
```
both ```save-session``` functions save the file as a binary so you execute that file to restore your previous session, lispworks may require you to enter your license & key on the commandline

###Learn a naïve Bayes classifier from a data set
Each instance in the data set is one line in a file, with values for its attributes separated by commas

Learn from training data set and test on test data set
```lisp
(gdl train test &optional unsupervised training-cycles)
```
unsupervised : t (expectation maximization) or nil (supervised).

Define a data set:
```lisp
(define-data-set ‘(attribute-domain-list ...) &optional unsupervised) 
```
Define attributes and their domains (and if learning should be unsupervised)
attribute-domain-list: (<optional *> attribute-name domain-list )
If the first element is * (i.e., the category-symbol ), the attribute is the category
If no category is specified, the last attribute mentioned is the category by default
domain-list: (symbolic ...symbols...) or (<discrete or continuous > min max)

Specify an instance to be loaded into WM (all of WM is emptied first):
```lisp
(instance inst-string &optional omit not-in-data)
```
- inst-string : A comma-delimited string of values for attributes (including category)
- omit: Attribute names in the list are not read in (this is used to omit the concept name when predicting it during a test).
- not-in-data: Attribute names in the list don’t exist in the data (this is used for the category in unsupervised learning).

Set up training and test data sets:
```lisp
(setup-gd filename &optional test omit not-in-data)
```
Set up file to process one instance per decision.  

- test: when T, learning is turned off (and it is turned on when it is nil).
- omit: Attribute names in the list are not read in (this is used to omit the concept name when predicting it during a test).
- not-in-data: Attribute names in the list don’t exist in the data (this is used for the category in unsupervised learning).

### Parallel Execution
```lisp
(run-parallel function parameter-list &optional filename)
```
execute one instance of function for each parameter in the list, function must take exactly one argument.   

- filename : string representing a lisp file containing a definition for the function to be run  
over the list of parameters (not needed if you are running a function that’s already in sigma)  
- function : defined in filename, takes a single argument  
- parameter-list: a list of input parameters to be given to function  
Example: 
```lisp
SIGMA> (run-parallel 'runmodel '(1 0 2.5 3) "~/sigma/model.lisp")
```
### Regression Testing
- run all tests in parallel listeners
```lisp
SIGMA> (regression-testing)
```
- run all tests serially in current window
```lisp
SIGMA> (run-all-regression-tests)
```