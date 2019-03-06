************************************************************
* master code to compute auxiliary processes from data     *
************************************************************

clear all
capture log close
set more off
set processors 4

* --- home directory of project
cd ~/dropbox/data/projects/fin

* log output
log using logs/auxiliary.txt, text replace

* --- work income processes
do do/auxiliary/income-work.do

* --- retirement income process
do do/auxiliary/income-ret.do

* --- out-of-pocket process
do do/auxiliary/oop.do

* --- equivalence scales 
do do/auxiliary/equivalence.do

* --- mortality rates 
do do/auxiliary/mortality.do

capture log close
exit
