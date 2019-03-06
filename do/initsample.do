***************************************
* do-file for PSID data on initial
* conditions for income, educ and wealth
* Lusardi, Michaud and Mitchell (JPE)
****************************************

cd ~/perso/emetrics/know

*****************************************
* this is a panel from 1980-2005 with key
* data from Cornell distribution
* see construct_psid.do for construction
*****************************************

use data/psid/psid_panel.dta, clear
                           
******************************
* select age group           *
******************************
keep if rbyr<1970
* keep years where wealth data
keep if year>=1984
* drop if net wealth missing
keep if hatota!=.
* keep males
keep if rgender==1
rename rage age
* keep in age range trying to match
keep if age>=23&age<=27
* keep whites
drop if hrace!=1

******************************
* create wealth var          *
* drop if missing, negative  *
* or greater than 1m         *
* also drop if business assets
* do not want entrepreneurs  *
******************************
keep if hatota!=.
drop if hatota<0
drop if hatota>1000000
drop if habsns_p!=0

******************************
* create income var
******************************
replace hinet = hinet - hicap - hipub
drop if hinet<=5000|missing(hinet)
drop if hinet>=250000

******************************
* strata variable  (educ)    *
******************************
gen strata = reduc
drop if missing(strata)
replace strata = strata
keep hinet strata hatota
* replicate observations
expand 20
gen u = uniform()
* randomize order in case want to simulate small sample
sort u
drop u
order hinet strata hatota
outsheet using params/initsample.csv, replace comma





   
	   
	   



