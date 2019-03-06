* table asked by editor for effect of policy scenarios with learning-by-doing (phi = 10)
capture log close
clear all
set more off

cd ~/perso/emetrics/fin

* table 3 with learning by doing
infile id lesshs hs college ratio using data/simulations/final/table-3-learn.txt , clear
set obs 18
preserve
	keep in 1/5
	mkmat lesshs hs college ratio, matrix(tab3a)
	matrix tab3a = (J(1,4,.)\tab3a)
	matrix rownames tab3a = Reduced-ss medwlth welthtoinc poor tech lowfk
	matrix colnames tab3a = lesshs hs college ratio
restore
preserve
	keep in 6/10
	mkmat lesshs hs college ratio, matrix(tab3b)
	matrix tab3b = (J(1,4,.)\tab3b)
	matrix rownames tab3b = Reduced-cmin medwlth welthtoinc poor tech lowfk
	matrix colnames tab3b = lesshs hs college ratio
restore
preserve
	keep in 11/18
	mkmat lesshs hs college ratio, matrix(tab3c)
	matrix tab3c = (J(1,4,.)\tab3c\J(1,4,.))
	matrix rownames tab3c = Perfect-fk medwlth welthtoinc poor tech lowfk welfare cons_base cons_perfect pctchange
	matrix colnames tab3c = lesshs hs college ratio
	
restore

matrix tab3 = (tab3a\tab3b\tab3c)
global fmt f(%9.4g)
outtable using figures/final/table3-learn, mat(tab3) replace center nobox $fmt cap("Simulation Results from Policy Scenarios - with learning")
