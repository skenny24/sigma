# The Sigma Cognitive Architecture: Tutorial #
Sigma (Σ) is a nascent cognitive system intended to support the real-time needs of intelligent agents, robots and virtual humans.  Generally speaking, much of the field of AI can be classified into the branches of "low level pattern recognition" or "high level symbolic reasoning", with very few systems which address both sides.  Sigma is an attempt to model both cognitive and graphical architectures.  These cognitive and graphical architectures form the two halves of the cognitive architecture hypothesis, the dynamic tension, interplay and constraint between these two architectures, in both their development and execution, is the source of much of Sigma’s unique contributions to cognitive architectures, artificial intelligence, and cognitive science.

In this tutorial, we are going to use the structure of Sigma to accomplish some basic tasks. Many basic operations for a virtual agent may be defined on a grid structure.  Here, we are going to slowly build up a Sigma model that controls the movements of a virtual agent on a grid. We will start with a single dimensional empty grid and will introduce new Sigma operations and concepts as we build up the model cumulatively. The goal of this tutorial is to enable you to accomplish progressively harder/greater tasks.  This tutorial involves:

* Initializing and creating a Sigma agent
* Initializing and defining a world, including the actions which can be taken
* Initializing perceptual information
* Learning in Sigma
* Templates in Sigma

At the end of this tutorial, you will have an agent which can, albeit simply, perceive the world, model the world in memory, learn about the world, and act.  This isn’t a lightweight task, as what is going on behind the scenes (probabilistic models, SLAM, etc.) is complex, but for this tutorial much of this complexity is abstracted from you.

As a final note before we get started, this tutorial will define, redefine, and re-re-re-redefine a function called random-walk.  The goal is for each random-walk to be self-contained and for each iteration to teach you a little more about Sigma and how it operates.  Generally speaking, you can copy-and-paste the “(defun random-walk …” text wherever you see it or download the lisp file, which contains all of the code.

# Table of Contents

- [Setup](#setup)
  - [Lispworks](#lispworks)
  - [CLISP/SLIME](#clispslime)
- [Operators (+conditionals & functions)](#operators-conditionals--functions)
- [Operator selection](#operator-selection)
- [Internal action execution (+ types & predicates)](#internal-action-execution--types--predicates)
- [Trials](#trials)
- [External action execution (+ perception & action)](#external-action-execution--perception--action)
- [Value selection](#value-selection)
- [External objects](#external-objects)
- [Learning (of Maps)](#learning-of-maps)
- [Simultaneous Localization and Mapping (SLAM)](#simultaneous-localization-and-mapping-slam)
- [Semantic Memory (& Learning)](#semantic-memory--learning)

## Setup ##

### Lispworks ###
* This tutorial requires that you have a Common Lisp implementation installed. The preferred system is Lispworks. There is a free trial version available for download [here](http://www.lispworks.com/downloads/index.html) which is sufficient for all the exercises in this tutorial. 
* The source code needed for this tutorial is available [here](https://github.com/skenny24/sigma/blob/master/src/sigma38-tutorial.lisp)
* Next, start up Lispworks, select 'open' & navigate to the location of sigma-tutorial.lisp on your filesystem and double click to open.
* From the top menu select buffers -> compile
* You will now have all of the sigma functionality & the tutorial code loaded into your system so you can run any of the commands listed below.  
* For help or to report issues: sigma@ict.usc.edu

### CLISP/SLIME ###

* An alternative, non-proprietary Common Lisp implementation can be installed using the instructions [here](http://www.jonathanfischer.net/modern-common-lisp-on-windows/)
* After installation, fire up the SLIME REPL. You'll need to tell quicklisp where to find the sigma source code you just downloaded, this will then allow you to load the sigma package:

```lisp 
CL-USER> (pushnew #p"/PATH/TO/sigma/src/" asdf:*central-registry*)
CL-USER> (ql:quickload :sigma)
```
* For ease of use switch to the sigma package

```lisp
CL-USER> (in-package :sigma)
#<PACKAGE SIGMA>
SIGMA> 
```
* Next load the tutorial material for open source platforms:
```lisp
SIGMA> (load "/PATH/TO/sigma/src/rwtutorial.lisp")
```
* You should now be able to run any of the functions shown in the tutorial within the clisp/emacs/sigma environment

## Operators (+conditionals & functions) ##
First, let's introduce the [```(init)```](https://github.com/skenny24/sigma/blob/master/ReferenceSheet.md#initialize-a-sigma-program) Sigma function. [```(init)```](https://github.com/skenny24/sigma/blob/master/ReferenceSheet.md#initialize-a-sigma-program) is required to initialize a Sigma model and it needs to be called each time a new Sigma model is being initiated. Check the [reference sheet](https://github.com/skenny24/sigma/blob/master/ReferenceSheet.md) for the full definition of [```(init)```](https://github.com/skenny24/sigma/blob/master/ReferenceSheet.md#initialize-a-sigma-program). In general, clicking on any name or phrase that is highlighted will take you to the reference sheet entry for it.  At this point, we are only interested in one of the optional arguments for [```(init)```](https://github.com/skenny24/sigma/blob/master/ReferenceSheet.md#initialize-a-sigma-program), which is the ```<operator-names>```. This argument allows us to define the operators that are available to our virtual agent. Let's assume there are only three operators available to the virtual agent to move on the 1D grid. These operators are **left**, **right**, and **none** (stand still). Consequently, the call ```(init '(left right none))``` initializes a Sigma model with these operators.

Let's assume that the virtual agent is going to randomly navigate this 1D grid. Then the virtual agent needs to randomly select and apply these operators. [```Actions```](https://github.com/skenny24/sigma/blob/master/ReferenceSheet.md#format-for-predicate-patterns-conditions-condacts-and-actions) are used here to provide input for operator selection. Actions are one part of the Sigma construct [```conditional```](https://github.com/skenny24/sigma/blob/master/ReferenceSheet.md#define-conditionals), which structures the long-term memory. For example, the conditional ```acceptable``` below makes all three operators equally likely to be selected:
```lisp
  (conditional 'acceptable
               :actions '((selected (operator left))
                          (selected (operator right))
                          (selected (operator none))
                          )
               )
``` 

```selected``` is a system generated [```predicate```](https://github.com/skenny24/sigma/blob/master/ReferenceSheet.md#define-predicates) that frames the decision process among the defined operators.  [```Predicates```](https://github.com/skenny24/sigma/blob/master/ReferenceSheet.md#define-predicates) specify relations among sets of arguments. They will be introduced in more depth shortly. 

A single cognitive – or decision – cycle is run by calling ```(decide 1)``` or ```(d 1)```. So putting these together, the simple Sigma model is initialized, created and run by the function:

```lisp
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
``` 

The Sigma function [```(print-pred-function)```](https://github.com/skenny24/sigma/blob/master/ReferenceSheet.md#to-print) or ```(ppfn)``` prints the contents of a single predicate working memory function. If we call the function ```(ppfn 'selected)``` after running the ```(random-walk-1)``` function above, we will see an output similar to:

```lisp
WM for SELECTED
  Factor [2_SELECTED-WM-FN] Function: 
      WM-STATE x WM-OPERATOR:
                    [0:100>
  [LEFT]             0
 [RIGHT]             1
  [NONE]             0
```

In this case, the operator **right** had been selected. As all of the operators are equally likely, different calls to the function ```(random-walk-1)``` may yield different selected operators. The ```state``` argument exists to support possible reflective processing. The *base-level* is state 0, with each higher metalevel (or lower subgoal) assigned a number one more than its predecessor. The ```state``` argument will be discussed in detail when we are providing examples for reflective processing.

## Operator selection ##
The Sigma parameter ```post-d``` can be used to set the forms to evaluate after the end of a cognitive cycle. In `random-walk-2`, defined below, ```(ppfn 'selected)``` is automatically called after each decision.

In the Sigma cognitive language ```*``` denotes the entire domain of an domain argument. Therefore, the modified ```acceptable``` conditional here behaves exactly as in the previous case, where separate actions were provided for each element of the domain. 

By default, Sigma selects randomly among all operators with the highest rating – here all three operators share a default rating of 1 – and maintains the one selected until either it is rejected or a more highly rated operator becomes available.  This is the ```best``` selection rule. If we want the virtual agent to truly random walk, we must change this selection rule to either  ```prob-match``` (probability-match) or ```boltzmann``` using Sigma's [```(operator-selection)```](https://github.com/skenny24/sigma/blob/master/ReferenceSheet.md#specify-the-form-of-selection-to-use-for-operators-in-the-selected-predicate) function. The former chooses randomly each cycle with a probability proportional to the relative ratings, while the latter does something similar after first exponentially transforming the ratings.  With either of these, the modified Sigma model selects a random operator at each decision cycle:

```lisp
(defun random-walk-2()
  (init '(left right none))
  (operator-selection 'boltzmann)
  (setq post-d '((ppfn 'selected)))
  (conditional 'acceptable
               :actions '((selected (operator *))                          
                          )
               )
  (d 1)
)
``` 

## Internal action execution (+ types & predicates) ##
None of the models seen so far actually perform any actions, all they do is select operators to be executed. Now, let's start with the case where all the movements are mentally simulated on 1-D grid model. For this mental simulation, we need a representation for the 1-D grid and a data structure that points to our current location on the grid.  [```Types```](https://github.com/skenny24/sigma/blob/master/ReferenceSheet.md#define-types)  The representation of the 1-D grid in this example is captured by defining a [```type```](https://github.com/skenny24/sigma/blob/master/ReferenceSheet.md#define-types) named ```1D-grid``` :```(new-type '1D-grid :numeric t :discrete t :min 1 :max 8)```. Type **location** is discrete numeric and its scope is from 1 to 8 (8 not included). Each of these 7 digits correspond to a cell in our hypothetical 1-D grid. 

The agent's current location is captured via a [```predicate```](https://github.com/skenny24/sigma/blob/master/ReferenceSheet.md#define-predicates). [```Predicates```](https://github.com/skenny24/sigma/blob/master/ReferenceSheet.md#define-predicates) specify relations among sets of typed arguments. In this case, we only need a single argument ```x``` of type ```1D-grid``` for the [```predicate```](https://github.com/skenny24/sigma/blob/master/ReferenceSheet.md#define-predicates) ```location```: 
```lisp  
(predicate 'location :world 'closed :arguments '((x 1D-grid !))) 
```

One key distinction among types of predicates is whether they are unique or universal, which is itself grounded in whether the predicate’s arguments are unique or universal.  Universal arguments are like variables in rule systems, where any or all of the elements in the variable’s domain may be valid.  Unique arguments are like random variables in probabilistic systems, where a distribution is provided over all of the elements of variable’s domain but only a single value is actually correct.  The ```location``` [```predicate```](https://github.com/skenny24/sigma/blob/master/ReferenceSheet.md#define-predicates) is a unique predicate as its only argument ```x``` is annotated with ```!```. Use of ```!``` implies that a best alternative (i.e., the most likely grid location) is to be selected. 

Another distinction among types of predicates concerns whether they are *open world* versus *closed world*, and thus whether unspecified values are assumed to be unknown (as in probabilistic networks and many logics) or false (as in rules).  The ```location``` [```predicate```](https://github.com/skenny24/sigma/blob/master/ReferenceSheet.md#define-predicates) is defined as a *closed world* [```predicate```](https://github.com/skenny24/sigma/blob/master/ReferenceSheet.md#define-predicates). **Non-persistent predicates don't do selection **. 

Now, we do need to define how the selected operators are applied onto the mental representation of the grid. [```Conditionals```](https://github.com/skenny24/sigma/blob/master/ReferenceSheet.md#define-conditionals) are the appropriate constructs to do the necessary modification on the ```location``` [```predicate```](https://github.com/skenny24/sigma/blob/master/ReferenceSheet.md#define-predicates). For example, the [```conditional```](https://github.com/skenny24/sigma/blob/master/ReferenceSheet.md#define-conditionals) ```move-left``` performs the action ```left``` by decreasing the value of location by 1. 
```lisp
  (conditional 'move-left
               :conditions '(
                             (selected (operator left))
                             (location (x (value)))
                             )
               :actions '(
                          (location (x (value -1)))
                          )             
               )
```

Similarly, 
```lisp
 (conditional 'move-right
               :conditions '(
                             (selected  (operator right))
                             (location (x (value)))
                             )
               :actions '(
                          (location (x (value 1)))
                          )             
               )
```

Initial location of the virtual agent can be specified by defining an [```evidence```](https://github.com/skenny24/sigma/blob/master/ReferenceSheet.md#define-evidence). Let's assume that the agent is initially located at the grid location 4:

```lisp
(evidence '((location (x 4)) ))
```
If we want to run the current Sigma model for 5 decision cycles, then the model looks like:
```lisp
(defun random-walk-3()
  (init '(left right none))
  (operator-selection 'boltzmann)
  (new-type '1D-grid :numeric t :discrete t :min 1 :max 8)
  (predicate 'location :world 'closed :arguments '((x 1D-grid !)))
 
  (setq post-d '(
                   (ppfn 'selected)
                   (ppfn 'location)
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
```

## Trials ##
Next, let's introduce the concept of running [```trials```](https://github.com/skenny24/sigma/blob/master/ReferenceSheet.md#run-a-sigma-program). A [```trial```](https://github.com/skenny24/sigma/blob/master/ReferenceSheet.md#run-a-sigma-program) continues to run until the Sigma model is terminated by a ```halt``` call. We can modify our model such that it terminates when the agent reaches one of the end locations, 1 or 7, of the grid. For example:

```lisp
 (conditional 'halt-at-location-1
               :conditions '(                          
                             (location (x 1))
                             )
               :actions '(
                          (halt)
                          )             
               )
```

Here, we introduce a new predicate pattern: ```conditions```. This is conceptually similar to the *conditions* and *actions* found in rule based systems. In the example above, the ```conditional``` is trying to match the condition where the agent's location is the 1st grid location. If that is the case, ```halt``` is invoked as an action.

The Sigma parameter ```pre-t``` can be used to set the forms to evaluate before a trial starts. We can use the ```pre-t``` parameter to set the initial location of the agent. Another setting that is applied here is that the Sigma variable ```max-fraction-pa-regions``` is set to 0. This variable affects the print functions and it changes the region based representation to a more explicit representation. After these modifications, our Sigma model looks like:

```lisp 
(defun random-walk-4()
  (init '(left right none))
  (operator-selection 'boltzmann)
  (setf max-fraction-pa-regions 0)
  (new-type '1D-grid :numeric t :discrete t :min 1 :max 8)
  (predicate 'location :world 'closed :arguments '((x 1D-grid !))) 
  (setq pre-t '((evidence '((location (x 4)) ))))
  (setq post-d '(
                   (ppfn 'selected)
                   (ppfn 'location)
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
```

A sample output for the above model is provided below. The first decision cycle selects the ```left``` operator and the agent is at location 4. At the beginning of the 2nd decision, the agent is at location 4 and the ```left``` operator is applied so at the end of the 2nd decision cycle, the agent is now at location 3. The new operator selected is ```left```. In the 3rd decision cycle, ```left``` operator is applied so the agent moves to location 2 and now the ```none``` operator is selected. In the fourth decision cycle, agent stays at location 2 and another ```left``` operator is selected. In the 5th decision cycle, agent moves to location 1 and halt is called. Halt call overrides any other operator so the model terminates at the end of the 6th decision cycle.

```lisp
>>> Trial 1 <<<

<<< Decision 1 >>>

WM for SELECTED
  Factor [2_SELECTED-WM-FN] Function: 
      WM-STATE x WM-OPERATOR:
                  [0:>
  [LEFT]             1
 [RIGHT]             0
  [NONE]             0

WM for LOCATION
  Factor [4_LOCATION-WM-FN] Function: 
      WM-X:
           [1]           [2]           [3]           [4]           [5]           [6]           [7]
             0             0             0             1             0             0             0

<<< Decision 2 >>>

WM for SELECTED
  Factor [2_SELECTED-WM-FN] Function: 
      WM-STATE x WM-OPERATOR:
                  [0:>
  [LEFT]             1
 [RIGHT]             0
  [NONE]             0

WM for LOCATION
  Factor [4_LOCATION-WM-FN] Function: 
      WM-X:
           [1]           [2]           [3]           [4]           [5]           [6]           [7]
             0             0             1             0             0             0             0

<<< Decision 3 >>>

WM for SELECTED
  Factor [2_SELECTED-WM-FN] Function: 
      WM-STATE x WM-OPERATOR:
                  [0:>
  [LEFT]             0
 [RIGHT]             0
  [NONE]             1

WM for LOCATION
  Factor [4_LOCATION-WM-FN] Function: 
      WM-X:
           [1]           [2]           [3]           [4]           [5]           [6]           [7]
             0             1             0             0             0             0             0

<<< Decision 4 >>>

WM for SELECTED
  Factor [2_SELECTED-WM-FN] Function: 
      WM-STATE x WM-OPERATOR:
                  [0:>
  [LEFT]             1
 [RIGHT]             0
  [NONE]             0

WM for LOCATION
  Factor [4_LOCATION-WM-FN] Function: 
      WM-X:
           [1]           [2]           [3]           [4]           [5]           [6]           [7]
             0             1             0             0             0             0             0

<<< Decision 5 >>>

WM for SELECTED
  Factor [2_SELECTED-WM-FN] Function: 
      WM-STATE x WM-OPERATOR:
                  [0:>
  [LEFT]             1
 [RIGHT]             0
  [NONE]             0

WM for LOCATION
  Factor [4_LOCATION-WM-FN] Function: 
      WM-X:
           [1]           [2]           [3]           [4]           [5]           [6]           [7]
             1             0             0             0             0             0             0

<<< Decision 6 >>>

WM for SELECTED
  Factor [2_SELECTED-WM-FN] Function: 
      WM-STATE x WM-OPERATOR:
                  [0:>
  [LEFT]             1
 [RIGHT]             0
  [NONE]             0

WM for LOCATION
  Factor [4_LOCATION-WM-FN] Function: 
      WM-X:
           [1]           [2]           [3]           [4]           [5]           [6]           [7]
             1             0             0             0             0             0             0
```

## External action execution (+ perception & action) ##
Until now, all the operators were applied on a mental representation. Let's define a world external to the model and let's make our Sigma model interact with this world through perceptions and actions. The location of the agent in this external world is captured by the Lisp variable ```1d-grid-location```. 
```lisp
; True location in world
(defvar 1d-grid-location)
```
Two Lisp functions external to the Sigma model are defined to manage the interactions between the Sigma model and the external world. The function ```perceive-location``` provides information about the agent's location to Sigma's perception function and the function ```execute-operator``` executes Sigma's selected operators and modifies the location of the agent on the external world.

The ```perceive-location``` function utilizes Sigma's [```perceive```](https://github.com/skenny24/sigma/blob/master/ReferenceSheet.md#define-evidence) function. In general, the ```perceive``` function is used to provide probabilistic evidence to *open-world* ```predicates```. . The ```perceive-location``` function two parameters: (1) *correct-prob* is the probability of perceiving the correct location of the agent and (2) *correct-mass* is the probabilistic mass for the location perceived. Sigma's ```perceive``` function is called from ```perceive-location```. For example, 
```lisp (perceive ((location 0.9 (x 4))))``` 
is the perception of agent's location being position 4 with a probability of 0.9. The ```perceive-location``` function is:

```lisp
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
```

The ```execute-operator``` function reads the selected Sigma operator and applies it to the agent's external world. The *correc-prob* parameter determines the probability of whether the operators *left* or *right* are successfully applied and the agent has moved either left or right. If not, the agent stays at its current location.
```lisp
; Execute operator with some noise
(defun execute-operator (&optional correct-prob)
  (unless correct-prob (setq correct-prob 1.0))
  (let ((operator (operator-in-state base-level-state))
        (rand (random 1.0))
        )
    (when (and operator (not (haltp)))
      ; Make correct move action-noise percent of time
      (case operator
        (left (when (< rand correct-prob) (setq 1d-grid-location (max (- 1d-grid-location 1) 1))))
        (right (when (< rand correct-prob) (setq 1d-grid-location (min (+ 1d-grid-location 1) 7))))
        )
      )
     (format trace-stream "~&~&Operator:~S Next location: ~S~%" operator 1d-grid-location)
    )
  )
```

There are also changes required in the Sigma model. First, the ```perceive-location``` and ```execute-operator``` functions need to be introduced to the Sigma model. This is achieved by two predefined Sigma lists: (1) *perceive-list* and (2) *action-list*. Adding the ```perceive-location``` and ```execute-operator``` functions to the appropriate lists will result in initializing these functions within Sigma's decision cycle. ```Perceive``` functions are called within the elaboration phase and the ```action``` functions are called within the adaptation phase. 

The other change that needs to be done is to change the ```location``` predicate from *closed-world* to *open-world*. Since ```location``` refers to an external representation, it is captured probabilistically. *open-world* predicates do not have working memory function nodes so ```ppfn``` can not be used. ```(ppvn)``` prints out the posterior for variable nodes and the call ```(ppvn 'location)``` will provide us the desired functionality. Finally, the conditionals ```move-right``` and ```move-left``` are dropped from the Sigma model as the operators are applied to the external world.

```lisp
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
                 (ppvn 'location)
                 (ppfn 'selected)                
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
```

## Value selection ##
One addition that we can do the Sigma model is a ```location-selected``` predicate, which selects the most probable location as the current location. The ```location-selected``` predicate can be defined as *closed-world* with **!** (select best) as the unique symbol:
```lisp
(predicate 'location-selected :world 'closed :arguments '((x 1D-grid !)))
```

The ```conditional``` ```select-location``` manages the interaction between the ```location``` and ```location-selected``` predicates.

```lisp
 (conditional 'select-location
               :conditions '((location (x (location))))
               :actions '((location-selected (x (location))))
               )

```
The ```halt``` conditions can be defined in the forms to be evaluated in the ```pre-run``` so that *halt* conditions are checked in the external world. ```pre-run``` is before messages are sent within a decision.
```lisp
(setq pre-run '((when (or (= 1d-grid-location 1) (= 1d-grid-location 7)) (halt))))
```

So our model looks like:

```lisp
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
  (setq pre-run '((when (or (= 1d-grid-location 1) (= 1d-grid-location 7)) (halt))))
  (setq post-d '(
                 (ppvn 'location)
                 (ppfn 'location-selected)
                 (ppfn 'selected)                
                   ))

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

```

## External objects ##
Next, let's assume there is an object at each grid location and the agent perceives these objects. First, let's define a [```type```](https://github.com/skenny24/sigma/blob/master/ReferenceSheet.md#define-types) named ```obj-type``` :```(new-type 'obj-type :constants '(walker table dog human))```. Type **obj-type** is symbolic and its scope includes objects: *walker, table, dog, and human*. 

The ```object``` [```predicate```](https://github.com/skenny24/sigma/blob/master/ReferenceSheet.md#define-predicates) and the ```object-perceived``` ```predicate``` specifies the relation between the grid locations and object types:  

```lisp 
 (predicate 'object :perception t :arguments '((object obj-type %)))
 (predicate 'object-perceived :world 'closed :arguments '((location 1D-grid) (object obj-type !)))
```

This relation is established by the following conditional:

```lisp
 (conditional 'perceived-objects
               :conditions '(
                             (object (object (obj)))
                             (location (x (loc)))
                             )
               :actions '((object-perceived (object (obj)) (location (loc))))
               )

```


As the objects are situated in the external world, we need a perceptual function that interfaces with the external world.  The `perceive-object` function below perceives objects depending on the actual location of the agent on the grid. The spread of the objects on the grid is 1-dog, 2-table, 3-walker, 4-human, 5-dog, 6-table, and 7-walker. 
```lisp
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

```

The `perceive-list` then needs to be updated for perceptions about objects. The updated model is:
```lisp
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
                 (ppvn 'location)
                 (ppvn 'object)
                 (ppfn 'location-selected)  
                 (ppfn 'object-perceived)       
                 (ppfn 'selected)                
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
    
  (trials 1)

)
```
When we run this model, the agent moves on the single dimensional grid as before. Additionally, the agent perceives the object at each location on the grid. The working memory factor node (wmfn) of the predicate `object-perceived captures the object-location relation at each decision cycle as depicted in the output below.
```lisp
SIGMA 44 > (random-walk-7)

>>> Trial 1 <<<

<<< Decision 1 >>>
(1.0: WM-X(1D-GRID)[4]) (1: WM-OBJECT(OBJ-TYPE)[TABLE]) 

WM for LOCATION-SELECTED
  Factor [5_LOCATION-SELECTED-WM-FN] Function: 
      WM-X:
           [1]           [2]           [3]           [4]           [5]           [6]           [7]
             0             0             0             1             0             0             0

WM for OBJECT-PERCEIVED
  Factor [7_OBJECT-PERCEIVED-WM-FN] Function: 
      WM-LOCATION x WM-OBJECT:
                    [1]           [2]           [3]           [4]           [5]           [6]           [7]
 [WALKER]             0             0             0             0             0             0             0
  [TABLE]             0             0             0             1             0             0             0
    [DOG]             0             0             0             0             0             0             0
  [HUMAN]             0             0             0             0             0             0             0

WM for SELECTED
  Factor [2_SELECTED-WM-FN] Function: 
      WM-STATE x WM-OPERATOR:
                  [0:>
  [LEFT]             0
 [RIGHT]             0
  [NONE]             1

<<< Decision 2 >>>
(1.0: WM-X(1D-GRID)[4]) (1: WM-OBJECT(OBJ-TYPE)[TABLE]) 

WM for LOCATION-SELECTED
  Factor [5_LOCATION-SELECTED-WM-FN] Function: 
      WM-X:
           [1]           [2]           [3]           [4]           [5]           [6]           [7]
             0             0             0             1             0             0             0

WM for OBJECT-PERCEIVED
  Factor [7_OBJECT-PERCEIVED-WM-FN] Function: 
      WM-LOCATION x WM-OBJECT:
                    [1]           [2]           [3]           [4]           [5]           [6]           [7]
 [WALKER]             0             0             0             0             0             0             0
  [TABLE]             0             0             0             1             0             0             0
    [DOG]             0             0             0             0             0             0             0
  [HUMAN]             0             0             0             0             0             0             0

WM for SELECTED
  Factor [2_SELECTED-WM-FN] Function: 
      WM-STATE x WM-OPERATOR:
                  [0:>
  [LEFT]             0
 [RIGHT]             0
  [NONE]             1

<<< Decision 3 >>>
(1.0: WM-X(1D-GRID)[4]) (1: WM-OBJECT(OBJ-TYPE)[TABLE]) 

WM for LOCATION-SELECTED
  Factor [5_LOCATION-SELECTED-WM-FN] Function: 
      WM-X:
           [1]           [2]           [3]           [4]           [5]           [6]           [7]
             0             0             0             1             0             0             0

WM for OBJECT-PERCEIVED
  Factor [7_OBJECT-PERCEIVED-WM-FN] Function: 
      WM-LOCATION x WM-OBJECT:
                    [1]           [2]           [3]           [4]           [5]           [6]           [7]
 [WALKER]             0             0             0             0             0             0             0
  [TABLE]             0             0             0             1             0             0             0
    [DOG]             0             0             0             0             0             0             0
  [HUMAN]             0             0             0             0             0             0             0

WM for SELECTED
  Factor [2_SELECTED-WM-FN] Function: 
      WM-STATE x WM-OPERATOR:
                  [0:>
  [LEFT]             1
 [RIGHT]             0
  [NONE]             0

<<< Decision 4 >>>
(1.0: WM-X(1D-GRID)[3]) (1: WM-OBJECT(OBJ-TYPE)[WALKER]) 

WM for LOCATION-SELECTED
  Factor [5_LOCATION-SELECTED-WM-FN] Function: 
      WM-X:
           [1]           [2]           [3]           [4]           [5]           [6]           [7]
             0             0             1             0             0             0             0

WM for OBJECT-PERCEIVED
  Factor [7_OBJECT-PERCEIVED-WM-FN] Function: 
      WM-LOCATION x WM-OBJECT:
                    [1]           [2]           [3]           [4]           [5]           [6]           [7]
 [WALKER]             0             0             1             0             0             0             0
  [TABLE]             0             0             0             1             0             0             0
    [DOG]             0             0             0             0             0             0             0
  [HUMAN]             0             0             0             0             0             0             0

WM for SELECTED
  Factor [2_SELECTED-WM-FN] Function: 
      WM-STATE x WM-OPERATOR:
                  [0:>
  [LEFT]             1
 [RIGHT]             0
  [NONE]             0

<<< Decision 5 >>>
(1.0: WM-X(1D-GRID)[2]) (1: WM-OBJECT(OBJ-TYPE)[HUMAN]) 

WM for LOCATION-SELECTED
  Factor [5_LOCATION-SELECTED-WM-FN] Function: 
      WM-X:
           [1]           [2]           [3]           [4]           [5]           [6]           [7]
             0             1             0             0             0             0             0

WM for OBJECT-PERCEIVED
  Factor [7_OBJECT-PERCEIVED-WM-FN] Function: 
      WM-LOCATION x WM-OBJECT:
                    [1]           [2]           [3]           [4]           [5]           [6]           [7]
 [WALKER]             0             0             1             0             0             0             0
  [TABLE]             0             0             0             1             0             0             0
    [DOG]             0             0             0             0             0             0             0
  [HUMAN]             0             1             0             0             0             0             0

WM for SELECTED
  Factor [2_SELECTED-WM-FN] Function: 
      WM-STATE x WM-OPERATOR:
                  [0:>
  [LEFT]             1
 [RIGHT]             0
  [NONE]             0

<<< Decision 6 >>>
(1.0: WM-X(1D-GRID)[1]) (1: WM-OBJECT(OBJ-TYPE)[DOG]) 

WM for LOCATION-SELECTED
  Factor [5_LOCATION-SELECTED-WM-FN] Function: 
      WM-X:
           [1]           [2]           [3]           [4]           [5]           [6]           [7]
             1             0             0             0             0             0             0

WM for OBJECT-PERCEIVED
  Factor [7_OBJECT-PERCEIVED-WM-FN] Function: 
      WM-LOCATION x WM-OBJECT:
                    [1]           [2]           [3]           [4]           [5]           [6]           [7]
 [WALKER]             0             0             1             0             0             0             0
  [TABLE]             0             0             0             1             0             0             0
    [DOG]             1             0             0             0             0             0             0
  [HUMAN]             0             1             0             0             0             0             0

WM for SELECTED
  Factor [2_SELECTED-WM-FN] Function: 
      WM-STATE x WM-OPERATOR:
                  [0:>
  [LEFT]             0
 [RIGHT]             1
  [NONE]             0

Total time 0.01 sec
Trials: 1; Msec per trial: 7.0
Decision cycles: 6; Msec per decision cycle: 1 (init: 0, messages: 0, decision: 0, learn: 0)
Total messages: 162; Messages per decision: 27; Msec per message: 0.02
```

## Learning (of Maps) ##
In the above output, the working memory factor node of the predicate `object-perceived` appears to learn the object-location relation but this is not a stable representation. Learning occurs in Sigma via a process of gradient descent over functions defined in predicates or conditionals. For instance, learning a map of objects in the single dimensional grid would require defining a function representative of the concept being learned. Let's modify the `object-perceived predicate by changing its name to map and adding a function to it:
```lisp 
(predicate 'map :arguments '( (location 1D-grid) (object obj-type %)) :function 1)
```

This `map` predicate is now open-world predicate and the `object parameter is a *distribution* parameter rather than a *select-best* parameter as denoted by the **%**. The function of this predicate is defined over the parameters of the predicate: location and object. Here, the object is the parameter that we are keeping a distribution so in this case, we are defining a distribution for each location on the grid. The **1** used in function definition simply indicates that we are starting with a uniform prior, that is, every object is equally likely at the beginning for each location.

Turning on the learning in Sigma is simple: 
```lisp  
(learn '(:gd))
```
This basically tells Sigma to learn for every function in the model unless the learning is explicitly shut off for that function. There are two parameters that we are going to explicitly set for this model. First one is the `learning-rate`, which, as the name implies, sets the learning rate. The default learning rate is 0.05 and in the model below, it is set to 0.01. The second parameter that we are going to modify is the `max-gdl-increment`, which determines the maximum change that can be applied to a parameter. The default value is 1 but we are going to set it to 0.2 to further lower the cap on the maximum gradient. (This prevents a certain type of potential problem in learning that is beyond the scope of this tutorial)

One other change that we have made to the model is that we are going to run the model for a fixed number of decisions rather than waiting for the agent to arrive at a particular location. Main reason for this change is to have better coverage on the map that is learned by the agent.

So the updated model is:
```lisp
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
                 (ppvn 'location)
                 (ppvn 'object)
                 (ppfn 'location-selected) 
                 (ppfs 'map)       
                 (ppfn 'selected)                
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
```

If we want to run the model for 100 decision cycles, the required function call is:
```lisp
(random-walk-8 100)
```

The function to print `map` predicate's function is `(ppf 'map 'array)`

After 100 decision cycles, this function looks like:
```lisp
SIGMA 91 > (ppf 'map 'array)
      WM-LOCATION x WM-OBJECT:
                    [1]           [2]           [3]           [4]           [5]           [6]           [7]
 [WALKER]     0.1724305       0.16033    0.49504992    0.14520198    0.15644807    0.20907025    0.30710384
  [TABLE]     0.1724305       0.16033     0.1683167      0.564394    0.15644807    0.37278926    0.23096539
    [DOG]    0.48270845       0.16033     0.1683167    0.14520198    0.53065587    0.20907025    0.23096539
  [HUMAN]     0.1724305    0.51901007     0.1683167    0.14520198    0.15644807    0.20907025    0.23096539

```
So it looks like we are learning the correct function. With more experience, this function will converge to a deterministic function as there is no stochasticity in the model run. For instance, if we run the model for 500 decision cycles:

```lisp
SIGMA 98 > (ppf 'map 'array)
      WM-LOCATION x WM-OBJECT:
                    [1]           [2]           [3]           [4]           [5]           [6]           [7]
 [WALKER]  2.2884786E-4   2.283254E-4      0.942032    0.04620318   0.048757505   0.019322614     0.8224507
  [TABLE]  2.2884786E-4   2.283254E-4   0.019322677     0.8613905   0.048757505    0.94203216   0.059183095
    [DOG]     0.9993135   2.283254E-4   0.019322677    0.04620318     0.8537275   0.019322614   0.059183095
  [HUMAN]  2.2884786E-4      0.999315   0.019322677    0.04620318   0.048757505   0.019322614   0.059183095
```
or we can increase the learning rate to 0.1 (`(setf learning-rate 0.01)`) and run the model for 100 decision cycles:
```lisp
SIGMA 100 > (ppf 'map 'array)
      WM-LOCATION x WM-OBJECT:
                    [1]           [2]           [3]           [4]           [5]           [6]           [7]
 [WALKER]   9.708738E-4  9.6153847E-4     0.9969999  1.0100713E-3  1.0752688E-3  1.1494253E-3     0.6166667
  [TABLE]   9.708738E-4  9.6153847E-4  1.0000448E-3     0.9969698  1.0752688E-3    0.99655176    0.12777779
    [DOG]    0.99708736  9.6153847E-4  1.0000448E-3  1.0100713E-3     0.9967742  1.1494253E-3    0.12777779
  [HUMAN]   9.708738E-4     0.9971154  1.0000448E-3  1.0100713E-3  1.0752688E-3  1.1494253E-3    0.12777779
```

We are not going to further discuss parameter tuning in this tutorial but there are a number of Sigma variables that can be used to control learning.

## Simultaneous Localization and Mapping (SLAM) ##

Now, let's consider a simple change to the conditional `perceived-object` by moving the predicate pattern `(location (x (loc)))` from conditions to condacts. As you may recall, messages propagate away from conditions, whereas message flow for condacts is bidirectional. In other words, moving the predicate pattern `(location (x (loc)))` from conditions to condacts allows the predicate 'map' to influence the posterior on the `location` predicate. The `perceived-objects` conditional with the proposed change is:
```lisp
(conditional 'perceived-objects
              :conditions '(
                            (object (object (obj)))                           
                            )
              :condacts '(   
                          (location (x (loc)))
                          (map (object (obj)) (location (loc)))
                          )
              )
```

To make things more concrete, let's assume we run the original model, where the `location` predicate pattern was a condition in a probabilistic setting. In this setting, there is a 60% chance that the agent correctly perceives the location. Furthermore, 60% of the probability mass is on the perceived location and the remaining 40% is distributed over the neighboring locations. Such a scenario can be activated by:
```lisp
(random-walk-8 1000 0.6 0.6)
```
In this scenario, 1000 decision cycles are run. At some point during the run, there was a case as depicted below:
```lisp
Correct location: 4
(0.19999999: WM-X(1D-GRID)[4]) (0.6: WM-X(1D-GRID)[5]) (0.19999999: WM-X(1D-GRID)[6]) 
(1: WM-OBJECT(OBJ-TYPE)[TABLE]) 

WM for LOCATION-SELECTED
  Factor [5_LOCATION-SELECTED-WM-FN] Function: 
      WM-X:
           [1]           [2]           [3]           [4]           [5]           [6]           [7]
             0             0             0             0             1             0             0
MAP: 
      WM-LOCATION x WM-OBJECT:
                    [1]           [2]           [3]           [4]           [5]           [6]           [7]
 [WALKER]    0.05167313    0.34530607     0.7832272    0.11975537   0.050506998    0.11340284    0.79965735
  [TABLE]    1.55521E-4    1.55521E-4    0.21646221    0.87993443     0.2564895     0.8417874    0.20002768
    [DOG]    0.40140295    0.19412153   1.552944E-4  1.5503877E-4    0.69284845    0.04465457  1.5746542E-4
  [HUMAN]     0.5467684    0.46041688   1.552944E-4  1.5503877E-4  1.5503877E-4  1.5503877E-4  1.5746542E-4
```
Agent is at location 4 but the perception tells the agent it is at location 5 with 60% probability. In this case, it is not possible for the agent to recover from this error and hence, it believes that it is at location 5. However, the perception of the object being `Table` has no influence on the location in this case. When we look at the map function learned, we see that it is more likely to perceive a `dog` in location 5 rather than a `table`, whereas it is more likely to perceive a 'table' at location 4. So if the information in the learned map function can help the agent to recover from errors in perceptions of the location. So consider the case below, where the model is run with `location` predicate pattern being a condact in the `perceived-objects` conditional:

```lisp

Current location: 5

(1.0623143E-4: WM-X(1D-GRID)[3]) (0.2195618: WM-X(1D-GRID)[4]) (0.78033197: WM-X(1D-GRID)[5]) 
(1: WM-OBJECT(OBJ-TYPE)[DOG]) 

WM for LOCATION-SELECTED
  Factor [5_LOCATION-SELECTED-WM-FN] Function: 
      WM-X:
           [1]           [2]           [3]           [4]           [5]           [6]           [7]
             0             0             0             0             1             0             0
MAP: 
      WM-LOCATION x WM-OBJECT:
                    [1]           [2]           [3]           [4]           [5]           [6]           [7]
 [WALKER] 1.00908175E-4     0.1106981     0.8140824    0.02834832    0.06153891     0.3477502     0.9556432
  [TABLE] 1.00908175E-4   0.039249763   0.114756346    0.72501874    0.13430768     0.6173503   0.044159383
    [DOG]     0.9138201    0.34931234   0.071062826     0.2465345      0.804055   0.034800887  9.8716686E-5
  [HUMAN]   0.085978076     0.5007398   9.852217E-5   9.852217E-5   9.852217E-5   9.861933E-5  9.8716686E-5
```
The agent is actually at location 5 but it perceives location 4 as the current location. Additionally, the agent perceives the presence of `dog` at the current location. The learned map tells that it is more likely to have a dog at location 5 than location 4 and this information is received by the `location` as its pattern used in a condact rather than a condition in the `perceived-objects` conditional. As a result, the posterior on the `location` shows that the agent believes that it is in location 5 even if the perception on location is centered on location 4.

The phenomenon described above is a simple illustration of simultaneous localization and mapping (SLAM), commonly used in the robotics literature. 

The updated model for SLAM is provided below: 
```lisp
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
                 (ppvn 'location)
                 (ppvn 'object)
                 (ppfn 'location-selected) 
                 (ppfs 'map)       
                 (ppfn 'selected)                
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

```

## Semantic Memory (& Learning) ##
Now, we are going to step back from the agent on a grid example to explain the basic semantic memory concept in Sigma. Later, we are going to reformulate this concept within the agent on a virtual grid model. 

Now, let's assume that there are four different types of objects: `dog human table walker`. There are also three distinct features that are associated with these objects: (1) Whether they are alive, (2) Number of legs they have , and (3) What color they are. The underlying assumption in this example is that it isn't always possible to directly perceive the objects but we can observe their features to make predictions about the objects.

The perception function is defined below. It takes one input `perceive-object` to determine whether the object is perceived or not. We are going to use this to switch between training and testing. 

In this example, all objects are equally likely to be perceived. Dog and human are alive, whereas table and walker are not. Walker has definitively 1 leg; there is 80% chance that a dog has 4 legs and 20% chance it has 3 legs; there is 90% chance that a table has 4 legs and 10% chance that it has 3 legs; and there is 97% chance a human has 2 legs, 2% chance that s/he has 1 leg, and 1% chance that s/he has 0 legs. Humans are either brown or white with equal probabilities; dog is brown with 70% chance, white with 25% chance, and silver with 5% chance; walker is silver with 95% chance, and brown with 5% chance; and table is brown with 40% chance and silver with 60% chance. `perceive-object-features` function generate perceptions based on these probability distributions by sampling from them. 

```lisp
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
```
Object, color, alive, and legs are designated as perceptual predicates in this model along with their corresponding types. In the most basic form, Sigma utilizes a Naive Bayes classifier to model semantic memory. Consequently, this basic semantic memory model needs to learn four different probability distributions: (1) prior on the object, (2) conditional distribution over alive, (3) conditional distribution over color, and (4) conditional distribution over legs. For these functions, we define four function predicates. For example, the `object-prior` is a functional predicate and its function learns the prior distribution on objects.

```lisp
 (predicate 'object-prior
             :arguments '((object obj-type %))
             :function 1)

```

The `object-color` predicate's function is an example of capturing conditional probabilities. The `color` argument  of the `object-color` predicate is distributional, whereas the object argument is universal. This means that this function will learn a probability distribution over color for each object using the gradient descent learning algorithm of Sigma. 


```lisp
(predicate 'object-color
             :arguments '((object obj-type) (color color %))
             :function 1)
``` 

We need to define the necessary conditionals that defines the interaction between these function predicates and the rest of the model. For example, `perceived-objects` conditional below establishes a bidirectional flow between the `object` perceptual predicate and the `object-prior` functional predicate. In practice, when there is a perception for the `object` predicate, this data will be used to modify the `object-prior` function via gradient-descent. On the reverse direction, the functional value coming out of the `object-prior` will set the prior on the `object` predicate.

```lisp
  (conditional 'perceived-objects
               :condacts '(  
                           (object (object (obj)))   
                           (object-prior (object (obj)))
                           )
               )
```
   
The `object-color*join` predicate establishes the relation between the `object` and `color`perceptual predicates and `object-color` functional predicate. Similar to `perceived-objects` conditional, the function of the `object-color` predicate will be updated when there are perceptions for either (or both) of `object` and `color` predicates. The functional values of the `object-color` predicates will be used when generating the posteriors on `object` and/or `color` predicates.

```lisp
(conditional 'object-color*join
           :condacts '((object (object (obj))) 
                       (color (value (color)))
                       (object-color (object (obj)) (color (color))))
            )
```



The basic semantic memory model is provided below. This model defines the corresponding conditionals for `legs` and `alive` features similar to the `object-color*join` conditional above.

```lisp
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
  
  (setf post-t '(
                   (ppf 'object-prior 'array)
                   (ppf 'object-color 'array)
                   (ppf 'object-legs 'array)
                   (ppf 'object-alive 'array)
                 )
        )
 
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
  (setf post-t '())
                                
  (setq perceive-list `(
                        (perceive-object-features nil)                                           
                        ))
  (setq post-d '(               
                 (format trace-stream "~&~&Guess object: ~S~%" (best-in-plm (vnp 'object))) 
                 ))
  (format trace-stream "~%~&~&******************Testing Phase ~%")
  (trials 1)
  )
```

The above model has two parts: training and testing. In a cognitive architecture setting, differentiation between training and testing is not meaningful. Nonetheless, we have created such a split in this model for demonstrative purposes. So in the above model, the first part is used for training and then learning is turned off by `(learn)` - calling `learn` with no arguments turns off the gradient descent learning. The major distinction between the training and testing phases is that in the testing phase, objects are not perceived, whereas they are perceived in the training phase. This is achieved via setting the parameter passed to the `perceive-object-features` function to nil. So in the testing phase, `perceive-list` is updated via:
```lisp
 (setq perceive-list `(
                        (perceive-object-features nil)                                           
                        ))
```
The model can be run by:
```lisp
(random-walk-10 200)
```
This command will run 200 decision cycles for training and 10 more decision cycles for testing. For this particular run, the learned function  on the `object-prior` predicate is (`(ppwm)`):
```lisp
      WM-OBJECT:
       [WALKER]       [TABLE]         [DOG]       [HUMAN]
    0.38091165    0.18750984    0.13409288    0.29748565
```

This is different than the true population, which is uniform distribution but 200 is relatively a small sample size.

The learned conditional probabilities for the features look like:

```lisp
 WM-OBJECT x WM-COLOR:
                [WALKER]        [TABLE]         [DOG]        [HUMAN]
  [SILVER]     0.9226708      0.55758697    5.102041E-4   4.950495E-4
  [BROWN]      0.038664583    0.4419131     0.7008815     0.53610927
  [WHITE]      0.038664583    5.0E-4        0.29860833    0.46339563

 WM-OBJECT x WM-LEGS:
           [WALKER]       [TABLE]         [DOG]       [HUMAN]
 [0]  8.5179005E-3   4.950495E-4   5.050505E-4  4.9020804E-4
 [1]     0.9659284   4.950495E-4   5.050505E-4    0.15040179
 [2]  8.5179005E-3   4.950495E-4   5.050505E-4     0.8481276
 [3]  8.5179005E-3     0.2946807     0.3414362  4.9020804E-4
 [4]  8.5179005E-3     0.7038342     0.6570487  4.9020804E-4

 WM-OBJECT x WM-ALIVE:
               [WALKER]       [TABLE]         [DOG]       [HUMAN]
  [FALSE]    0.89606596    0.83220387    0.13817707    0.12088767
  [TRUE]    0.10393404     0.1677961    0.86182297     0.8791123
```

These are again different than the true probability distributions but they capture what is seen in the perceived samples. 

In the testing phase, the object is not perceived. So the task here is to predict the object using the perceptions on the features.
 
The model’s best prediction on the object can be extracted via: 
```lisp
 (best-in-plm (vnp 'object))
```

The Sigma function `vnp` accesses the posterior of a predicate in plm form. Another Sigma function `best-in-plm` works only if there is a single variable (other than state variable) defined in the predicate. It finds the value with the highest probability in the alternatives and extracts it. So in this case, the model’s guess on the object is basically the alternative that has the highest probability in the posterior distribution.
When we look at the testing phase, we see that in all 10 cases the model was able to generate the correct answer:

```lisp
******************Testing Phase 

>>> Trial 1 <<<

<<< Decision 201 >>>
Correct object: WALKER
Guess object: WALKER

<<< Decision 202 >>>
Correct object: DOG
Guess object: DOG

<<< Decision 203 >>>
Correct object: WALKER
Guess object: WALKER

<<< Decision 204 >>>
Correct object: WALKER
Guess object: WALKER

<<< Decision 205 >>>
Correct object: HUMAN
Guess object: HUMAN

<<< Decision 206 >>>
Correct object: TABLE
Guess object: TABLE

<<< Decision 207 >>>
Correct object: WALKER
Guess object: WALKER

<<< Decision 208 >>>
Correct object: HUMAN
Guess object: HUMAN

<<< Decision 209 >>>
Correct object: TABLE
Guess object: TABLE

<<< Decision 210 >>>
Correct object: WALKER
Guess object: WALKER
```
## SLAM + semantic memory ##
Next, we are going to integrate the semantic memory example and the agent on the grid model. The basic assumption that we are going to make here is that the objects at each location is not fixed. Thera can be one of the two possible objects at each grid location and it is equally likely to have either of them. So for different grid locations we can have: (Grid Location 1) dog or human, (Grid Location 2) human or table, (Grid Location 3) walker or human, (Grid Location 4) table or walker, (Grid Location 5) dog or walker, (Grid Location 6) table or dog, and (Grid Location 7) walker or table. The updated perceive object features function, namely `perceive-object-features-grid` is provided below:

```lisp
(defun perceive-object-features-grid (perceive-object)
  (let ( (rand0 (random 1.0))
         (rand (random 1.0))
         (rand2 (random 1.0))
         object
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
```

Integrating `random-walk-9` and `random-walk-10` is relatively straightforward. The main conceptual argument that needs to be made is that the combination of `map` and `location` predicates in `random-walk-9` replace the `object-prior` predicate in `random-walk-10`. So the priors on the objects originate from the belief that the agent has on its current location and the map that the agent is building up. The `perceived-objects` conditional needs to be updated to have all the predicate patterns appearing under condacts:
```lisp
  (conditional 'perceived-objects
               :condacts '(  
                           (object (object (obj)))   
                           (location (x (loc)))
                           (map (object (obj)) (location (loc)))
                           )
               )
```

This update achieves bidirectional information flow between the map, object and location predicates and hence, the combination of map and location can be used as a prior on the object.

```lisp
(defun random-walk-11(number-of-decisions &optional perception-prob perception-mass action-prob)
  (init '(left right none))
  (setf MAX-DECISIONS number-of-decisions)
  (learn '(:gd))
  ;(setf trace-decisions nil)
  (setf max-gdl-increment 0.2)
  (setf learning-rate 0.01)
  (operator-selection 'boltzmann)
  (setf max-fraction-pa-regions 0)
  (new-type '1d-grid :numeric t :discrete t :min 1 :max 8)
  (new-type 'obj-type :constants '(s table dog human))
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
                (format trace-stream "~&~&Current location pre-d: ~S~%" 1d-grid-location)
                ))

  (setf post-t '(               
                 (ppf 'object-color 'array)
                 (ppf 'object-legs 'array)
                 (ppf 'object-alive 'array)
                 (ppf 'map 'array)
                 )
        )
 
  (setq perceive-list `(
                        (perceive-location ,perception-prob ,perception-mass)
                        (perceive-object-features-grid t)                                           
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
  (trials 1)
  (learn)
  (setq post-run `((when (= decision-count (+ ,number-of-decisions 10)) (halt))))
  (setq perceive-list `(
                        (perceive-location ,perception-prob ,perception-mass)
                        (perceive-object-features-grid nil)                                           
                        ))
  (setq post-d '(
                 ;(format trace-stream "~&~&Current location: ~S~%" 1d-grid-location)
                 (format trace-stream "~&~&Guess object: ~S~%" (best-in-plm (vnp 'object))) 
                 (format trace-stream "~&~&Guess location: ~S~%" (best-in-plm (vnp 'location)))
                 ))
  (trials 1)
  )
```

We run this model with
```lisp
(random-walk-11 200)
```
Sample learned after 200 decision cycles is:
```lisp
  WM-LOCATION x WM-OBJECT:
                   [1]           [2]           [3]           [4]           [5]           [6]           [7]
 [WALKER]    0.07021035   0.045923394     0.4813388    0.38056886    0.41011244   0.047849633     0.6011289
 [TABLE]    0.07021035      0.498842   0.028358559    0.48085094    0.06240859     0.5249279    0.39765155
   [DOG]    0.36692062   0.045923394   0.028358559    0.06929011     0.4650704    0.37937284   6.097859E-4
 [HUMAN]     0.4926587    0.40931124    0.46194407    0.06929011    0.06240859   0.047849633   6.097859E-4

```

In the testing phase, the model correctly predicts the objects at different locations when we don’t have noise in location perceptions:

```lisp
******************Testing Phase 

>>> Trial 1 <<<

<<< Decision 201 >>>
Correct object: TABLE
Current location pre-d: 4
Guess object: TABLE
Guess location: 4

<<< Decision 202 >>>
Correct object: WALKER
Current location pre-d: 5
Guess object: WALKER
Guess location: 5

<<< Decision 203 >>>
Correct object: DOG
Current location pre-d: 6
Guess object: DOG
Guess location: 6

<<< Decision 204 >>>
Correct object: DOG
Current location pre-d: 5
Guess object: DOG
Guess location: 5

<<< Decision 205 >>>
Correct object: TABLE
Current location pre-d: 6
Guess object: TABLE
Guess location: 6

<<< Decision 206 >>>
Correct object: DOG
Current location pre-d: 5
Guess object: DOG
Guess location: 5

<<< Decision 207 >>>
Correct object: WALKER
Current location pre-d: 5
Guess object: WALKER
Guess location: 5

<<< Decision 208 >>>
Correct object: WALKER
Current location pre-d: 4
Guess object: WALKER
Guess location: 4

<<< Decision 209 >>>
Correct object: WALKER
Current location pre-d: 3
Guess object: WALKER
Guess location: 3

<<< Decision 210 >>>
Correct object: TABLE
Current location pre-d: 2
Guess object: TABLE
Guess location: 2

```

## Action modeling (& templates)
In this part of the tutorial (for random walk models 12, 13, and 14), we are going to discuss the templates in Sigma and how these templates can be leveraged to achieve complex functionality. Before getting into details with templates, let's first discuss diachronic processing in Sigma. For our random walk model, the actual states (the current location) are latent as we have discussed in the [```SLAM```](https://github.com/skenny24/sigma/src/tutorial.md#simultaneous-localization-and-mapping-slam) example. In such models, system state evolves through time (diachronic processing) and Sigma defines a ```prediction mode``` to architecturally distinguish the current state from the previous. Such architectural distinction enables both the current state and the previous state to be accessed simultaneously.  Setting the Sigma parameter ```diachronic-processing``` to true will enable the prediction mode in Sigma. In this mode, for any closed world state predicate, an open world \*next is created automatically by the architecture. For example, let's assume there is a closed world state predicate named ```location``` is defined when the prediction mode is on (diachronic processing is set to true):
```lisp
 (predicate 'location :world 'closed :perception t :arguments '((state state) (x 1D-grid !)))
```
An open world location*next predicate created automatically when the model is initialized:
```lisp
(PREDICATE 'SELECTED :WORLD 'CLOSED :PERSISTENT T :UNIQUE '(OPERATOR) :SELECT 'BOLTZMANN :ARGUMENTS '((STATE STATE) (OPERATOR OPERATOR !)))
(PREDICATE 'TIME :WORLD 'CLOSED :PERSISTENT T :UNIQUE '(VALUE) :SELECT 'BEST :ARGUMENTS '((VALUE TIME !)))
(PREDICATE 'STATE :WORLD 'CLOSED :PERSISTENT T :ARGUMENTS '((STATE STATE)))
(PREDICATE 'HALT :WORLD 'CLOSED :PERSISTENT T)
(PREDICATE 'LOCATION :WORLD 'CLOSED :PERSISTENT T :UNIQUE '(X) :SELECT 'BEST :PERCEPTION T :ARGUMENTS '((STATE STATE) (X 1D-GRID !)))
(PREDICATE 'LOCATION*NEXT :WORLD 'OPEN :UNIQUE '(X) :PERCEPTION T :ARGUMENTS '((STATE STATE) (X 1D-GRID %)))
T
```

The underlying assumption is the previous state is captured by the closed world predicate and the open world predicate captures the current state. For our current example, the ``location`` predicate now captures the previous state and the ```location*next``` predicate captures the current state. As time elapses (or new decisions are made), the architecture automatically shifts the contents of the ```location*next``` predicate to the location predicate.
So now we are going to adjust our random walk model to work with diachronic processing in Sigma to better illustrate the use of templates in Sigma.  First, perception function is updated. We changed the function name to ```perceive-location*next```. Only change in the function is that perceptions are now read into ```location*next``` predicate (as it captures the current state):

```lisp
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

```

Templates automatically create any additional predicates, conditionals and other structures that are needed from the predicates that have been explicitly specified. Prediction mode supports the learning of transition functions, where access to successive pairs of states is essential. A template has been defined in Sigma for probabilistic transition functions that automatically turns on prediction mode if it isn’t already on, to yield **\*next** predicates, and creates a transition conditional for each unique closed-world state predicate that is defined (where a state predicate is a predicate that includes an argument for the state). If the Selected predicate is defined, a condition for it is also included in the conditional, to convert the transition function into an action model. So let's put this in action within our random walk example. Turning on action modeling is done via  ```(learn '(:am))``` instruction. The ```:am``` does multiple things: (1) Turns on gradient descent learning (2) Turns on the prediction mode (diachronic processing) and creates the **\*next** predicates, and (3) Creates the required predicate and conditional for the action learning. So the stripped down random-walk model with only action modeling looks like:

```lisp
(defun random-walk-12(number-of-decisions &optional perception-prob perception-mass action-prob)
  (init '(left right none))
  (learn '(:am))
  (setf learning-rate 0.05)
  (operator-selection 'boltzmann)
  (setf max-fraction-pa-regions 0)
  (setf max-decisions 10000)
  (new-type '1D-grid :numeric t :discrete t :min 1 :max 8)

  (predicate 'location :world 'closed :perception t  :arguments '((state state) (x 1D-grid !)))
  
  (setq pre-t '((setf 1d-grid-location 4) )) 
  (setq post-run `( (when (= decision-count ,number-of-decisions) (halt))))
                  
  (setq perceive-list `((perceive-location*next ,perception-prob ,perception-mass)                                           
                        ))
  (setq action-list `((execute-operator ,action-prob)))
  
  (conditional 'acceptable
              :actions '((selected (operator *))                         
                          )
               )
    
  (trials 1)

)
```
As you see, the only predicate that's explicitly defined is the closed world location predicate. However, when we list the predicates after initialization, we see that two additional predicates are created:
```lisp
(PREDICATE 'LOCATION*NEXT :WORLD 'OPEN :UNIQUE '(X) :PERCEPTION T :ARGUMENTS '((STATE STATE) (X 1D-GRID %)))
(PREDICATE 'ACTION-1212 :WORLD 'OPEN :UNIQUE '(X-2) :ARGUMENTS '((X-0 1D-GRID) (OPERATOR-1 OPERATOR) (X-2 1D-GRID))
    :FUNCTION 1)
```

The predicate ```ACTION-1212``` is the predicate that captures the state transitions and stores them in its function. The name is automatically generated so we need to check the name of this predicate if we need to access this predicate. The state transition probabilities are learned with the help of the architecture generated conditional:

```lisp
(CONDITIONAL 'LOCATION-PREDICTION
    :CONDITIONS '((STATE (STATE (S)))
                  (LOCATION (STATE (S)) (X (X-0)))
                  (SELECTED (STATE (S)) (OPERATOR (OPERATOR-1))))
    :CONDACTS '((LOCATION*NEXT (STATE (S)) (X (X-2)))
                (ACTION-1212 (X-0 (X-0)) (OPERATOR-1 (OPERATOR-1)) (X-2 (X-2))))
    )

```

After running the model for 500 decisions via ```(random-walk-12 500)```, we can check the learned transition ```(ppf 'action-1212 'array)```. Predicate name can be different as they are named by Sigma):

```lisp
WM-OPERATOR-1 x [WM-X-0 x WM-X-2]:

  [LEFT]
               [1]           [2]           [3]           [4]           [5]           [6]           [7]
 [1]     0.9105056    0.99538446   3.076923E-4  3.1248297E-4  3.2258066E-4   3.236246E-4  3.2469237E-4
 [2]   0.014915756  7.6924777E-4     0.9981539  3.1248297E-4  3.2258066E-4   3.236246E-4  3.2469237E-4
 [3]   0.014915756  7.6924777E-4   3.076923E-4    0.99812514  3.2258066E-4   3.236246E-4  3.2469237E-4
 [4]   0.014915756  7.6924777E-4   3.076923E-4  3.1248297E-4     0.9980645   3.236246E-4  3.2469237E-4
 [5]   0.014915756  7.6924777E-4   3.076923E-4  3.1248297E-4  3.2258066E-4     0.9980582  3.2469237E-4
 [6]   0.014915756  7.6924777E-4   3.076923E-4  3.1248297E-4  3.2258066E-4   3.236246E-4     0.9980519
 [7]   0.014915756  7.6924777E-4   3.076923E-4  3.1248297E-4  3.2258066E-4   3.236246E-4  3.2469237E-4

 [RIGHT]
               [1]           [2]           [3]           [4]           [5]           [6]           [7]
 [1]  7.3529414E-4  3.0674847E-4   3.144654E-4   3.311514E-4  3.3005004E-4  3.2894738E-4  3.2574142E-4
 [2]     0.9955883  3.0674847E-4   3.144654E-4   3.311514E-4  3.3005004E-4  3.2894738E-4  3.2574142E-4
 [3]  7.3529414E-4    0.99815965   3.144654E-4   3.311514E-4  3.3005004E-4  3.2894738E-4  3.2574142E-4
 [4]  7.3529414E-4  3.0674847E-4    0.99811316   3.311514E-4  3.3005004E-4  3.2894738E-4  3.2574142E-4
 [5]  7.3529414E-4  3.0674847E-4   3.144654E-4    0.99801314  3.3005004E-4  3.2894738E-4  3.2574142E-4
 [6]  7.3529414E-4  3.0674847E-4   3.144654E-4   3.311514E-4     0.9980197  3.2894738E-4  3.2574142E-4
 [7]  7.3529414E-4  3.0674847E-4   3.144654E-4   3.311514E-4  3.3005004E-4     0.9980264    0.99804557

  [NONE]
               [1]           [2]           [3]           [4]           [5]           [6]           [7]
 [1]     0.9955554  7.2992704E-4  3.1054198E-4   0.014915708  7.0707677E-3  3.3783785E-4  3.4129692E-4
 [2]  7.4075774E-4     0.9956205  3.1054198E-4   0.014915708  7.0707677E-3  3.3783785E-4  3.4129692E-4
 [3]  7.4075774E-4  7.2992704E-4    0.99813676   0.014915708  7.0707677E-3  3.3783785E-4  3.4129692E-4
 [4]  7.4075774E-4  7.2992704E-4  3.1054198E-4     0.9105058  7.0707677E-3  3.3783785E-4  3.4129692E-4
 [5]  7.4075774E-4  7.2992704E-4  3.1054198E-4   0.014915708     0.9575754  3.3783785E-4  3.4129692E-4
 [6]  7.4075774E-4  7.2992704E-4  3.1054198E-4   0.014915708  7.0707677E-3    0.99797297  3.4129692E-4
 [7]  7.4075774E-4  7.2992704E-4  3.1054198E-4   0.014915708  7.0707677E-3  3.3783785E-4     0.9979522

```

So in a no noise model, the transition probabilities are learned perfectly.

## Perception modeling ##
As with transition functions, a template has been defined for perceptual memories. It enables prediction mode if it isn’t already enabled, and then creates for each perceptual predicate a conditional that has a condition for the state plus condacts for: (1) the perceptual predicate; (2) all of the *next predicates; and (3) a new memorial predicate whose function stores the conditional probability of the unique variables in the perceptual predicate given the variables in the *next predicates.

The instruction ```(learn '(:am :pm))``` turns on both **action modeling** and **perception modeling**. This also turns on prediction mode (diachronic processing) and gradient descent learning similar to the action modeling only case. In addition to the ```random-walk-12``` model, this model also has the object predicate, which is a perceptual predicate, similar to the [```external object```](https://github.com/skenny24/sigma/src/ReferenceSheet.md#external-objects) concept introduced earlier.


```lisp
(defun random-walk-13(number-of-decisions &optional perception-prob perception-mass action-prob)
  (init '(left right none))
  (learn '(:pm :am))
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

  (setq perceive-list `((perceive-location*next ,perception-prob ,perception-mass)
                        (perceive-object)                
                        ))

  (setq action-list `((execute-operator ,action-prob)))
  
  (conditional 'acceptable
              :actions '((selected (operator *))                         
                          )
               )
    
  (trials 1)

)
```

This model introduces two architecture generated predicates:
```lisp
(PREDICATE 'ACTION-1344 :WORLD 'OPEN :UNIQUE '(X-2) :ARGUMENTS '((X-0 1D-GRID) (OPERATOR-1 OPERATOR) (X-2 1D-GRID))
    :FUNCTION 1)
(PREDICATE 'PERCEPTION-1345 :WORLD 'OPEN :UNIQUE '(OBJECT-1) :ARGUMENTS '((OBJECT-1 OBJ-TYPE) (X-0 1D-GRID))
    :FUNCTION 1)
```

and two architecture generated conditionals:

```lisp
(CONDITIONAL 'LOCATION-PREDICTION
    :CONDITIONS '((STATE (STATE (S)))
                  (LOCATION (STATE (S)) (X (X-0)))
                  (SELECTED (STATE (S)) (OPERATOR (OPERATOR-1))))
    :CONDACTS '((LOCATION*NEXT (STATE (S)) (X (X-2)))
                (ACTION-1344 (X-0 (X-0)) (OPERATOR-1 (OPERATOR-1)) (X-2 (X-2))))
    )

(CONDITIONAL 'OBJECT-PERCEPTION-PREDICTION
    :CONDITIONS '((STATE (STATE (S))))
    :CONDACTS '((OBJECT (STATE (S)) (OBJECT (OBJECT-1)))
                (LOCATION*NEXT (STATE (S)) (X (X-0)))
                (PERCEPTION-1345 (OBJECT-1 (OBJECT-1)) (X-0 (X-0))))
    )

```

When we run the model, we can see that perception model learned the right function for this no noise model by calling ```(ppf 'perception-1345 'array)```:

```lisp
   WM-OBJECT-1 x WM-X-0:
          [WALKER]       [TABLE]         [DOG]       [HUMAN]
 [1]   4.608295E-4   4.608295E-4    0.99861753   4.608295E-4
 [2]      0.103523      0.103523      0.103523     0.6894311
 [3]    0.71101916    0.09632693    0.09632693    0.09632693
 [4]    0.09632695    0.71101916    0.09632695    0.09632695
 [5]    0.06952423    0.06952423     0.7914274    0.06952423
 [6]   0.012252189     0.9632434   0.012252189   0.012252189
 [7]    0.99906266   3.124702E-4   3.124702E-4   3.124702E-4
```



## Reinforcement learning ##

Now, let's assume that the agent is looking for the human, which is located at location 2 in the 1D-grid. What we want  is to learn how to get to the human location as quickly as possible. This could be done by introducing a reward structure and leveraging reinforcement learning in Sigma. Let's define a reward function, which has a reward of 9 at location 2 but no reward anywhere else.

```lisp
; Fixed vector of rewards for use in assign-reward
(defparameter rewards-rw (vector 0 9 0 0 0 0 0 ))
```

Let's also assume we have a reward predicate that utilizes the rewards (This predicate will be automatically defined by the template): 
```lisp 
(PREDICATE 'REWARD :WORLD 'OPEN :UNIQUE '(VALUE) :PERCEPTION T :ARGUMENTS '((LOCATION-X 1D-GRID) (VALUE UTILITY %)) :FUNCTION '((0 * (0 20)) (0.1 * (0 10)))) 
```
The type utility is a  discrete numeric type in the range [0,20). We can now introduce a perception function that would perceive the reward based on the location of the agent. 

```lisp
; Assign a fixed reward based on location
(defun assign-reward-rw (rewards-rw)
  (let ((cl 1d-grid-location))
    (eval `(perceive (quote ((reward .1 (location-x *) (value *)) ; Empty WM of any previous rewards
                             (reward (location-x ,cl) (value ,(aref rewards-rw  (- cl 1)))))))) ; Add reward for current state
    )
  )

```

The two functions defined above provide the basic external mechanism to run a reinforcement learning model in Sigma. The reinforcement learning model is initialized exactly the same as perception-action modeling, the only difference is that the instruction used is now ```(learn '(:pm :am :rl))```. The basic model is:

```lisp
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

```

So the new predicates defined in the model in addition to the ones defined in action and perception modeling are:
```lisp
(PREDICATE 'PROJECTED :WORLD 'OPEN :UNIQUE '(VALUE) :ARGUMENTS '((LOCATION-X 1D-GRID) (VALUE UTILITY %))
    :FUNCTION 1)
(PREDICATE 'PROJECTED*NEXT :WORLD 'OPEN :UNIQUE '(VALUE) :ARGUMENTS '((LOCATION-X 1D-GRID) (VALUE UTILITY %))
    :FUNCTION 'PROJECTED)
(PREDICATE 'REWARD :WORLD 'OPEN :UNIQUE '(VALUE) :PERCEPTION T :ARGUMENTS '((LOCATION-X 1D-GRID) (VALUE UTILITY %))
    :FUNCTION '((0 * (0 20)) (0.1 * (0 10))))
(PREDICATE 'Q :WORLD 'OPEN :UNIQUE '(VALUE) :ARGUMENTS '((LOCATION-X 1D-GRID) (OPERATOR OPERATOR) (VALUE UTILITY %))
    :FUNCTION 1)
```

Template-driven reinforcement learning in Sigma starts with perceptual learning of a reward function. This is then used as the basis for conjointly learning the projected future utility of states in the function for the projected predicate, and the policy in the function of the Q predicate. The projected future value for the next state is handled via the ```projected*next``` predicate. The important point here is that ```projected``` and ```projected*next``` predicates' functions are tied. In other words, what is being learned in this function is shared by these two predicates. The ```BACKUP-PROJECTED```conditional shows the conditional that drives Projected learning, based on backing up the sum of the reward and the discounted projected value for the next state (from projected*next). This conditional uses an affine transform to compute the projected distribution from which it will learn. It furthermore weights this by the filtered Q distribution – which converts the explicit distribution over utilities into an implicit distribution of functional values. The conditional for backing up the policy is much like this one, but has an action for the Q predicate instead. These conditionals are:

```lisp
(CONDITIONAL 'BACKUP-PROJECTED
    :CONDITIONS '((STATE (STATE (S)))
                  (LOCATION (STATE (S)) (X (X-0)))
                  (LOCATION*NEXT (STATE (S)) (X (X-1)))
                  (SELECTED (STATE (S)) (OPERATOR (OPERATOR-2)))
                  (PROJECTED*NEXT (LOCATION-X (X-1)) (VALUE (VALUE-4)))
                  (REWARD (LOCATION-X (X-1)) (VALUE (VALUE-5)))
                  (Q (OPERATOR (OPERATOR-2)) (LOCATION-X (X-0)) (VALUE (Q (:FILTER #)))))
    :ACTIONS '((PROJECTED (LOCATION-X (X-0)) (VALUE (VALUE-4 (:COEFFICIENT 0.95 :OFFSET VALUE-5 :PAD 0 :APPLY-COEFFICIENT-TO-OFFSET T)))))
    )

(CONDITIONAL 'BACKUP-Q
    :CONDITIONS '((STATE (STATE (S)))
                  (LOCATION (STATE (S)) (X (X-0)))
                  (LOCATION*NEXT (STATE (S)) (X (X-1)))
                  (SELECTED (STATE (S)) (OPERATOR (OPERATOR-2)))
                  (PROJECTED*NEXT (LOCATION-X (X-1)) (VALUE (VALUE-4)))
                  (REWARD (LOCATION-X (X-1)) (VALUE (VALUE-5)))
                  (Q (OPERATOR (OPERATOR-2)) (LOCATION-X (X-0)) (VALUE (Q (:FILTER #)))))
    :ACTIONS '((Q (LOCATION-X (X-0)) (OPERATOR (OPERATOR-2)) (VALUE (VALUE-4 (:COEFFICIENT 0.95 :OFFSET VALUE-5 :PAD 0 :APPLY-COEFFICIENT-TO-OFFSET T)))))
    )

```


The model can be run by the command: ```(random-walk-14 1000)```

In order to get expected values (EV) for the functions learned, a separate print function is defined that is called post trial in ```random-walk-14```model. This function is:

```lisp
(defun test-rl-print-tutorial (stream)
  (format stream "~&~%PROJECTED FF (EV):~&")
  (pa 'projected nil '((expected wm-value)) stream)
  (format stream "~&~%Q FF (EV):~&")
  (pa 'q nil '((expected wm-value)) stream)
  (format stream "~&~%REWARD FF (EV):~&")
  (pa 'reward nil '((expected wm-value)) stream)
  (format stream "~&~%")
  )
```

After running the model for 1000 decision cycles, these are the numbers that we get:

```lisp
PROJECTED FF (EV):
Predicate function for predicate PROJECTED:
      WM-LOCATION-X:
           [1]           [2]           [3]           [4]           [5]           [6]           [7]
      14.06555     14.565967     10.645658      7.827994     7.6288076      7.505014      7.505039
 

Q FF (EV):
Predicate function for predicate Q:
      WM-LOCATION-X x WM-OPERATOR:
                   [1]           [2]           [3]           [4]           [5]           [6]           [7]
  [LEFT]     14.065821     14.416634     16.098381      9.194201      8.088579     7.9278145     7.7437873
 [RIGHT]     15.949139     13.478781      8.110692      8.046615      7.718419     7.6289535      7.489125
  [NONE]      14.38163     16.170727     11.662604      8.114301      8.000571      7.572637     7.6661134
 

REWARD FF (EV):
Predicate function for predicate REWARD:
      WM-LOCATION-X:
           [1]           [2]           [3]           [4]           [5]           [6]           [7]
     0.5039216      9.486249    0.50406987    0.50408686     0.5041055     0.5041229    0.50416816
```

The maximum reward is at location 2 as expected. The q values for left, right and none operators are accurate as they all try to take the agent to location 2. Projected reward gets higher as the agent gets closer to location 2.