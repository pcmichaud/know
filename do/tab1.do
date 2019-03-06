* Table 1: PSID Wealth and Product Choice

infile lesshs hs college tot using tables/table-1.txt, clear
mkmat lesshs hs college tot, matrix(tab1a)
matrix tab1a = (J(1,4,.)\tab1a)
matrix rownames tab1a = "Participate" "25-35" "35-45" "45-55" "55-65" "65-75" total
matrix colnames tab1a = "less-hs" hs college total
matrix list tab1a
infile lesshs hs college tot using tables/table-1a.txt, clear
mkmat lesshs hs college tot, matrix(tab1b)
matrix tab1b = (J(1,4,.)\tab1b)
matrix rownames tab1b = "Share" "25-35" "35-45" "45-55" "55-65" "65-75" total
matrix colnames tab1b = "less-hs" hs college total
matrix list tab1b
matrix tab1 = (tab1a\tab1b)
outtable using tex/table1, mat(tab1) replace center nobox $fmt cap("Observed Take-up of Sophisticated Technology in PSID")

