* table 5
infile id lesshs hs college ratio using tables/table-3.txt, clear
set obs 18
preserve
	keep in 1/5
	mkmat lesshs hs college ratio, matrix(tab5a)
	matrix tab5a = (J(1,4,.)\tab5a)
	matrix rownames tab5a = Reduced-ss medwlth welthtoinc poor tech lowfk
	matrix colnames tab5a = lesshs hs college ratio
restore
preserve
	keep in 6/10
	mkmat lesshs hs college ratio, matrix(tab5b)
	matrix tab5b = (J(1,4,.)\tab5b)
	matrix rownames tab5b = Reduced-cmin medwlth welthtoinc poor tech lowfk
	matrix colnames tab5b = lesshs hs college ratio
restore
di "lower retirement benefits by 20%"
matrix list tab5a
di "lower means-tested benefits to $5k"
matrix list tab5b
* will append baseline from Table 2 manually in tex files
matrix tab5 = (tab5a\tab5b)
global fmt f(%9.4g)
outtable using tex/table5, mat(tab5) replace center nobox $fmt cap("Simulation Results from Policy Scenarios")

