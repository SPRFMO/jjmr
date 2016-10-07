# jjmr
R package for assessment model 
Jack Mackerel model
=============

Repository for the SPRFMO joint jack mackerel model
Readme file   
# File structure

## "src" directory
This is the base area where the code (main tpl file) lives.
all of the modeling work has been undertaken. 

## "jjmR" directory
This directory jjmR contains the R package code for evaluating model results 

## Docs
Contains the main documents used for the meeting. The subdirectories of this are self-explanatory.

## Assessment
Directory where model runs conducted. Contains subdirectory "config" containing control files and "input" containing datafiles
pdf files into “results” subdirectory).  
Subdirectory “arc” is where the model stores results 

In this directory, “run.bat” is a script that can be used to run models and place results in the assessment/arc directory.  E.g.:
"make s1"
will run the jjm model with input/mods1.ctl file and store results in arc directory with "s1" as the root name in the 
assessment/arc directory.

## Data directory
Contains information from intersessional work and data submitted during the meeting. 

## HCR
Contains software for doing the harvest control rule analysis (note .rdata files need to be generated locally)









