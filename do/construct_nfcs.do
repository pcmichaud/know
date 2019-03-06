capture log close
clear all

* set path
cd ~/perso/emetrics/know

log using "logs/construct2012.txt", text replace
* load data
use "data/nfcs/restricted_2012FINRA.dta", clear
tab interest_question
gen interest_right=0
replace interest_right=1 if interest_question==1
tab inflation_question
gen inflation_right=0
replace inflation_right=1 if inflation_question==3
tab risk_question
gen risk_right=0
replace risk_right=1 if risk_question==2
tab mortgage_question
gen mortgage_right=0
replace mortgage_right=1 if mortgage_question==1
tab bond_question
gen bond_right=0
replace bond_right=1 if bond_question==2
gen index5 = interest_right + inflation_right + risk_right + mortgage_right + bond_right
tab index5
gen index3 = interest_right + inflation_right + risk_right 
tab index3
gen all3right =0
replace all3right=1 if interest_right==1&inflation_right==1&risk_right==1
tab all3right
gen all5right =0
replace all5right=1 if interest_right==1&inflation_right==1&risk_right==1&mortgage_right==1&bond_right==1
tab all5right
* generate demographics
tab ethnicity
gen white =0
replace white =1 if ethnicity==1
tab sex
gen male=0
replace male=1 if sex==1
sum age_2012
gen age= age_2012
gen age2= age*age
gen age3= age2*age
tab education
gen lths = 0
replace lths=1 if education==1
tab lths
gen hs = 0
replace hs=1 if education==2
tab hs
gen somecollege = 0
replace somecollege=1 if education==3
tab somecollege
gen college = 0
replace college=1 if education==4
tab college
gen morecollege = 0
replace morecollege=1 if education==5
tab morecollege
* generate age dummies
egen age5 = cut(age), at(18 25(5)65 110)
gen year = 2012
save data/nfcs/nfcs2012.dta, replace

capture log close
exit
