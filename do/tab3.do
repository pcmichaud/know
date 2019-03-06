
* table 3
infile id lesshs hs college ratio using tables/table-3.txt, clear
set obs 18
preserve
	keep in 11/18
	mkmat lesshs hs college ratio, matrix(tab3)
	matrix tab3 = (J(1,4,.)\tab3\J(1,4,.))
	matrix rownames tab3 = Perfect-fk medwlth welthtoinc poor tech lowfk welfare cons_base cons_perfect pctchange
	matrix colnames tab3 = lesshs hs college ratio
restore

di "perfect financial knowledge"
matrix list tab3
* for Table 3, will append manually from Table 2 baseline
outtable using tex/table3, mat(tab3) replace center nobox $fmt 
