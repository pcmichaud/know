
* table 2
infile id lesshs hs college ratio using tables/table-2.txt, clear
mkmat lesshs hs college ratio, matrix(tab2a)
matrix tab2a = (J(1,4,.)\tab2a)
matrix rownames tab2a = Simulated medwlth inc welthtoinc poor tech share lowfk
matrix colnames tab2a = lesshs hs college ratio
matrix list tab2a
infile lesshs hs college using tables/psid-tab-2.txt, clear
gen ratio = college/lesshs
mkmat lesshs hs college ratio, matrix(tab2b)
matrix tab2b = (J(1,4,.)\tab2b)
matrix rownames tab2b = PSID medwlth poor tech share
matrix colnames tab2b = lesshs hs college ratio
matrix list tab2b
matrix tab2 = (tab2a\tab2b)
global fmt f(%9.4g)
outtable using tex/table2, mat(tab2) replace center nobox $fmt cap("Simulated and Observed Outcomes in PSID at Retirement")
