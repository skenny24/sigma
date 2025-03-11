# Release Notes for Sigma #

# Sigma 38 #

## improvements ##

* new functionality for storing graphs & sessions in a file for later use
     * requires loading of cl-store library (see setup for adding to lispworks configuration)
* some speed up of message processing has been added using a hash table for tracking the message queue
     * useful primarily for models requiring a large number of messages to be processed
*  update to ```decide``` function, allows a second optional argument that specifies the maximum number of messages to pass during decisions

## bug fixes ##

* fixed bug in outgoing-var-fact-message
* bugfix in parray-print-2d requires integer for print loop

# Sigma 37 #

## improvements ##

* sigma can now be loaded as an asdf package
* ```sigma.lisp``` now split into separate modules
     * ```sigma.lisp```: ability to define types, predicates, conditions
     * ```graph-compilation.lisp```: adding/removing nodes and links, compute node dependencies, densely connected subgraphs, breaking up of graphs/nodes
     * ```graph-runtime```: sum product algorithm, queues, message initialization and processing
     * ```learning.lisp```: all learning (episodic, gradient descent, naive bayes), extending types 
     * ```deciding.lisp```: perception and making choices, creating and modifying WM values, detecting impasses, adding/removing states

* deprecating pplm: parray should now be used instead of pplm/print-plm. for backwards compatibility, keeping pplm as a wrapper that calls parray. 

## bug fixes ##

* fixed indexing issue in parray
* graphical interface now shows all nodes in display instead of just conditionals and isolated nodes

## Sigma 36 ##

### improvements ###

* Added code for a 2D virtual robot that combines there earlier SLAM and RL code.  Here you can selectively turn on perception modeling (:pm), reinforcement learning (:rl), and action modeling (:am) individually or in combination.  Please note though that nothing useful will be learned here if both perception and action modeling are turned on, as it can never understand sufficiently where it is without either an initial perception or action model.
* Added a normalization step at the end of affine transforms for unique predicates.  This counterbalances the loss of mass that can result from translations and scalings.  Affine transforms were originally thought of for universal predicates, like occupancy grids in mental imagery, where this isn’t an issue.  But it can be an issue with unique predicates.
* Chaged things so that RL all by itself now implies diachronic processing.
* parray can now handle PLM's of any dimension
* sparse product now initializes with the sparser plm. Each product has two plm inputs and now the sparser plm of the two is selected to initialize the result plm.
* Predicate arguments can now be vectors (denoted by unique-symbol []). If an argument is a vector argument, normalizations (both for the variable and the predicate function) are L2 normalizations. Gradient descent algorithm is modified such that no normalization happens after gradient addition.

### bug fixes ###

* RL template problem defining the Q predicate when the operator is specified by a type.
* in maximize-plm where maximals from earlier processed dimensions were not be carried over in all cases.
* in compute-span that arose when trying to run ma-grid2, concerning how you find the span for a symbolic operator when there are impasses.
* at ADF nodes when one conditional variable maps onto multiple WM variables.
* fixed code that makes distributions explicit (for when discrete regions center on integers)
* Bug fixed in compute-span (for when there is a region covering more than one constant element)

## Sigma 34 ##

### improvements ###

* The function run-parallel can now be used to run multiple variations on a single function in parallel.  You will get true parallelism to the extent that your machine has sufficient cores, and simulated paralellism beyond that.  For this to work, your function must take one argument, and part of the call to run-parallel is a list of argument values for the different calls. 
* A form of not-equal (<>) test on variables has been implemented for discrete dimensions.  Such tests can be within pattern for conditions; or across patterns, as long as the first pattern is a condition.  This does use an explicit form of inverse delta functions, and so can be expensive for very large variable domains.  But it can enable more general specification of some kinds of conditionals.  Examples can be found in (twvne).
* ability to specify architecturally understood goals.  You call the function goal with the name of a state predicate plus the evidence for the goal.  The corresponding goal predicate ends in *goal, and there is also a progress predicate created that ends in *progress and only has a state argument (if the original predicate is a state predicate).  The goal is put into the working memory of the *goal predicate.  The functional value of the *progress predicate is computed at the beginning of each decision, and corresponds to integrating out all dimensions other than the state from the product of the WM functions for the original and goal predicates, and then dividing it by the same integral over the goal WM function to normalize it. This should yield a value of 1 for a perfect match of a Boolean goal.  However, there is no requirement that the goal be Boolean, and thus that the highest match value necessarily be 1.  Ultimately the aim would be to do as much of this processing as possible in the graph itself, but right now it all happens outside of it.
* There are three new versions of the Eight Puzzle program.  ep82e demonstrates using episodic memory to suggest operator choices.  ep92 does hill climbing based on an explicit comparison of the goal and state.  ep102 also does hill climbing, but using an evaluation based on the architectural comparison of the goal and state.
* there is now automatic reinforcement learning (:rl).  Given a unique closed-world state predicate, it generates the necessary types (utility), predicates (reward, q, projected, projected*next), and conditionals to learn the reward, q and projected functions, including backing up the projected function for learning both it and q.  It also creates conditionals to use the q function in selecting operators. You can see this working in the program (test-rl-rl), which does automatic action model and RL, which has replaced (test-rl-am) in the regression suite. This implementation doesn't yet work for the multiagent case though. [As part of this implementation have restructured significantly the original create-model-conditionals code.]
* To efficiently initialize large distributed vectors, if you specify a function with all discrete dimensions by a list containing the constant row-major followed by exactly the right number of constants to exactly fill the function, it simply fills up a region-array with these values and then converts it into a PLM.  For an array with 1M elements, it takes about 3 seconds to do this.  You can look at the test program (trmi) to see how this works.
* Variable offsets for affine translation are no longer assumed to be constants.  It is now possible, for example, to specify a different offset for each agent in a multi-agent situation.  You can see how this works in the sample program (tcoff).
* Have provided :action-function and :observation-function attributes on predicates, to set initial values for automatically generated action and perception functions.  I don't suggest actually using these though unless you know what they are doing, as it may not be obvious what arguments these functions have, or what order they are to appear in.  These definitely may not be the same as those in the original predicate.
* Added a new system type "flag" that only has one domain element: "true".
* Added a new system predicate "detect-impasses", which can be used to selectively turn impasse detection on and off at particular states in the hierarchy.  This does not yet work with multiple agents.
* Some extensions and tuning of the automatic acquisition of action and predicate models has been done in service of the speech work.  They no longer, for example, assume that state and selected predicates are defined.  The variable bindings for universal variables between the current and next predicates in action models have been made so that the same variables are used in both.  Also, if a predicate is annotated with ":no-models t" then neither action nor perception models are learned for it.
* The function for printing conditionals can now optionally show the reordered order.

### bug fixes ###

* Fixed a bug in episodic memory introduced in Sigma 33 concerning printing of predicate rather than conditional functions.
* Fixed a couple of bugs in automated condition ordering that kept it from fully using the message volumes after the possible next conditions are joined to what came previously in deciding on which to include next.  This does make a significant difference in some tasks, such as some variants of the shift-reduce parser.
* A conceptual bug was fixed in subtractive-normalization in the presence of multiple normalization dimensions.
