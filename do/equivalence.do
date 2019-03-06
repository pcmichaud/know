***************************************
* do-file for to compute equivalence fact
* to take account of household size
* Lusardi, Michaud and Mitchell (JPE)
****************************************

cd ~/perso/emetrics/know

*****************************************
* this is a panel from 1980-2005 with key
* data from Cornell distribution
* see construct_psid.do for construction
*****************************************

use data/psid/psid_panel.dta, clear

*********************************
* select sample 
*********************************
* single years
keep if year>=1980&year<=1997
* keep males
keep if rgender==1
* keep whites
drop if hrace!=1
* age range
keep if rage>=25&rage<=100
******************************
* sort data for panel analysis
******************************
sort hhidpn
tsset hhidpn time

******************************
* create dependent variable  *
* drop if missing, put to 1 if
* negative
******************************
gen adult = hhsize - hhchild
gen n = (adult + 0.7*hhchild)^0.7

gen strata = 1 if reduc==1
replace strata = 2 if reduc==2
replace strata = 3 if reduc==3

drop if missing(strata)
collapse (mean) n [aw=wgid_cs], by(strata rage)
lowess n rage if strata==1, gen(n_1) nograph
lowess n rage if strata==2, gen(n_2) nograph
lowess n rage if strata==3, gen(n_3) nograph
replace n = n_1 if strata==1
replace n = n_2 if strata==2
replace n = n_3 if strata==3
drop n_*
table rage strata, content(mean n)
reshape wide n, i(rage) j(strata)
global N1 = _N+20
set obs $N1
replace n1 = n1[_n-1] if n1==.
replace n2 = n2[_n-1] if n2==.
replace n3 = n3[_n-1] if n3==. 
drop rage
outsheet using params/equivalence-par.csv, replace comma

exit



