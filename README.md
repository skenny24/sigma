**THE SIGMA COGNITIVE ARCHITECTURE**  
[Developed at the University of Southern California](http://cogarch.ict.usc.edu)  
Current release: Sigma38  

## Setup ##

* If you're new to lisp, there is a lisp tutorial [here](https://gigamonkeys.com/book/)  
* A Sigma tutorial is available [here](tutorial.md)  
* Sigma reference sheet can be found [here](ReferenceSheet.md)  
* Release Notes are available [here](ReleaseNotes.md)
* For help contact: sigma@ict.usc.edu


### Where to get Sigma ###

#### Source can be downloaded [here](https://github.com/skenny24/sigma/archive/refs/heads/main.zip) or using git: ####

* You will need to have [git](http://git-scm.com/) installed on your machine
* Then you can run the following command

```
$ git clone https://github.com/skenny24/sigma.git
```

* This will copy a directory *sigma* onto your local machine
* Under ```sigma/src``` you will see the Sigma source files
* Sigma is written in Common Lisp. This version requires [LispWorks](http://www.lispworks.com/) for use of its GUI, but can be run in other implementations for use of the core functionality (tested with CLISP)
* if you're looking for an open source common lisp implementation options are available [here](http://www.jonathanfischer.net/modern-common-lisp-on-windows/) and [here](http://common-lisp.net/project/lispbox/)

### Install Quicklisp ###

* Sigma is loaded using the [Quicklisp library manager](https://www.quicklisp.org/beta/). If you don't have quicklisp installed please follow the directions on their site for installation, you will need to load the ```quicklisp.lisp``` file and then run the following command to install:
```lisp
CL-USER> (quicklisp-quickstart:install)
```
* This will create a directory called ```quicklisp``` in your home directory.
* To ensure quicklisp is loaded each time you start your lisp system you should run the following command:
```lisp
CL-USER> (ql:add-to-init-file)
```
* This will append code to your initialization file which checks and loads quicklisp each time you start your system. In LispWorks for example, commands will be added to your ```.lispworks``` file

### Load Sigma ###

* Using either LispWorks or one of the open source options you can fire up a REPL (read/eval/print loop)
* Add the sigma source directory to the ASDF source registry system (using your own path):
```lisp
CL-USER> (pushnew #p"/home/USER/sigma/src/" asdf:*central-registry*)
```
load the required cl-store library

```lisp
CL-USER> (ql:quickload :cl-store)
```
you can then load the sigma package

```lisp
CL-USER> (ql:quickload :sigma)
```
use ```in-package``` to make sigma the current package and call functions without the ```sigma:``` qualifier 
```lisp
CL-USER> (in-package :sigma)
```
### Permanently Add Sigma to your Initialization file ###
some of the commands above can be added to your init file so that you need not run them every time you start your Lisp system. For example in *Lispworks* you can add these lines to the end of your *.lispworks* file:
```lisp
(pushnew #p"/home/USER/sigma/src/" asdf:*central-registry*)
(ql:quickload :sigma)
```
Then every time you start up lispworks Sigma will be loaded and you can simply run ```(in-package :sigma)```

Additional commands can be added to set your default package to sigma every time you launch Lispworks. A sample Lispworks initalization file can be found [here](src/sigma-init.lisp)

IMPORTANT NOTE: to compile new files within the sigma package the package directive should be added at the beginning of the source file before compiling
```lisp
(in-package :sigma)
```
# Older Releases available [here](https://bitbucket.org/sigma-development/sigma-archive/src) 

Download the archive, releases < Sigma37 require the following steps

* Under *SigmaXX/src* you will see the ```sigma.lisp```
* [LispWorks](http://www.lispworks.com/) is required for use of graphical features, but can be run in other implementations for use of the core functionality (tested with CLISP)
* load/compile the file *sigma.lisp*. This will also compile the additional files that sigma requires & if Lispworks' CAPI library is detected the graphical features will be loaded as well. 
* The variable *sigma-home* by default assumes that you have downloaded sigma into your $HOME directory. If you have downloaded Sigma to another location you will need to adjust that variable (line 52 of *sigma.lisp*)
* other GUI-based git tools available: [SourceTree](https://www.atlassian.com/software/sourcetree/overview) 

*This material is based on research supported by the Army, the Office of Naval Research, and the Air Force Office of Scientific Research.
*
