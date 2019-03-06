***************************************
* do-file for PSID data calibration of 
* replacement rates
* Lusardi Michaud and Mitchell (JPE)
****************************************
cd ~/perso/emetrics/know

*****************************************
* this is a panel from 1980-2005 with key
* data from Cornell distribution
* see construct_psid.do for construction
*****************************************

use data/psid/psid_panel.dta, clear

*********************************
* select sample (same as previous) 
*********************************
* year (data with single year)
keep if year>=1980&year<=1997
* age, first select older than 55 and younger than 70
keep if rage>=55&rage<=100
rename rage age
* keep males
keep if rgender==1
* keep whites
drop if hrace!=1

******************************
* sort data for panel analysis
******************************
sort hhidpn
tsset hhidpn time

*******************************
* create dependent variable   *
* drop if missing or negative *
* also drop if higher than 250k
*******************************
replace hinet = hinet - hicap - hipub
drop if hinet<=0|missing(hinet)
drop if hinet>=250000

******************************
* create strata variable     *
******************************
gen strata = reduc
label def strata 1 "lhs" 2 "hs" 3 "college"
label val strata strata3
label var strata "estimation group"
tab strata
drop if missing(strata)

******************************
* define retirement          *
******************************
gen retired = status==4&rwork==0
replace retired = 1 if l.retired==1

* collapse the data

collapse (mean) hinet [aw=wgid_cs], by(strata age retired)

local retage = 65

* get income at time of retirement
gen rep = .
gen srep = .
foreach s of numlist 1/3 {
	sum hinet if age==(`retage'-1)&retired==0&strata==`s'
	local inc`s' = r(mean)
	replace rep = hinet/`inc`s'' if strata==`s'&retired==1&age>=`retage'
	reg rep age if strata==`s'&retired==1&age>=`retage' 
	predict srep`s', xb
	replace srep = srep`s' if strata==`s'&retired==1&age>=`retage'
}

table age strata, content(mean srep)

keep if age>=`retage'

keep if retired==1
keep strata age srep  	   
reshape wide srep, i(age) j(strata)

set obs 36
replace age = age[_n-1]+1 if age==.

foreach s of numlist 1/3 {
	replace srep`s' = srep`s'[_n-1] if srep`s'==. 
}


#d ;
twoway 
	(line srep1 age if age>`retage')
	(line srep2 age if age>`retage')
	(line srep3 age if age>`retage'),
	xtitle("age")
	ytitle("replacement rate (fraction of last work earnings)")
	ylabel(0(0.1)1) xlabel(65(5)100) 
	legend(label(1 "less HS") label(2 "HS") label(3 "College") rows(1));
	graph export figures/fig-reprate.eps, as(eps) replace;
#d cr

list
drop age  	 

outsheet using params/income-ret-par.csv, replace comma

exit

