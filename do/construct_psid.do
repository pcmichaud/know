*************************************************
* Constructing PSID Panel for Analysis          *
* use Cross-National Equivalence Files          *
* Lusardi, Michaud, Mitchell (2015)				*
* files can be downloaded at 
* cnef.ehe.osu.edu                              *
* we use files downloaded in 2010 at Cornell    *
* additional raw data from PSID used            *
*************************************************

clear all
capture log close
set more off
set processors 4
set mem 600m

* please change to the root of the project folder
cd ~/perso/emetrics/know

**************************
* append equivalence files
**************************

use data/psid/pequiv_1980.dta, clear
rensfix _1980
gen year = 1980
save data/psid/psid_panel.dta, replace
quietly {
foreach s of numlist 1981/1997 1999 2001 2003 2005{
	noisily di `s'
	use data/psid/pequiv_`s'.dta, clear
	rensfix _`s'
	gen year = `s'
	save temp/psid_panel_`s', replace
	use data/psid/psid_panel.dta, clear
	append using temp/psid_panel_`s'.dta
	save data/psid/psid_panel.dta, replace
	erase temp/psid_panel_`s'.dta
}
}

save temp/pequiv_merge.dta, replace


**************************
*  select sample
**************************

use temp/pequiv_merge.dta, clear

* generate death indicator
tsset x11101ll year
gen death = x11103==2 if x11103!=0

* Selecting sample
* men
keep if d11102ll==1
* only household heads
keep if d11105==1
* age range
keep if d11101>25&d11101<90
* drop oversample
keep if x11104ll==11

save temp/pequiv_merge.dta, replace


*****************************
* merge wealth information 
*****************************
/* only do this first time to create the stata datasets
quietly {
foreach s of numlist 1984 1989 1994 1999 2001 2003 2005 {
do $PATH/WLTH`s'.do
isid x11102 year
# delimit cr
}
}
*/

quietly {
use temp/pequiv_merge.dta, clear
sort x11102 year
foreach s of numlist 1984 1989 1994 1999 2001 2003 2005 {
	capture drop _merge
	merge x11102 year using data/psid/wlth_`s'.dta,  uniqusing nokeep update
	drop _merge
	sort x11102 year
}
}

save temp/pequiv_merge.dta, replace

****************************
* merge CPI information (BLS)
****************************

use temp/pequiv_merge.dta, clear
sort year
merge year using data/cpi.dta, nokeep

drop c19* c20*

save temp/pequiv_merge.dta, replace

*****************************
* rename and create variables
*****************************

use temp/pequiv_merge.dta, clear

rename x11101ll hhidpn
rename x11102 hhid
rename d11103 hrace
rename d11106 hhsize
rename d11107 hhchild
rename l11101 hgstate
rename d11102ll rgender

global ids "hhidpn hhid year hgstate hrace rgender hhsize hhchild"

rename d11101 rage
rename d11112ll rrace
rename d11104  rmstat
rename d11108 reduc
rename d11109 redyrs
global demo "rage rrace rmstat reduc redyrs"

rename w11102 wghh
rename w11101 wgid_cs
rename w11103 wgid_long
global weights "wghh wgid_*"


rename i11103 hiearn
rename i11104 hicap
rename i11106 hipriv
rename i11107 hipub
rename i11108 hissben
rename i11117 hipen
rename i11118 hiothr
rename i11110 riearn
rename i11113 hinet
rename i11101 higross
global inc "hi* riearn"

rename h11103 hnkid1
rename h11104 hnkid4
rename h11105 hnkid7
rename h11106 hnkid12
rename h11110 hnothr
rename h11112 hspouse
global hhcomp "hnkid* hnothr hspouse"

rename e11101 rhours
rename e11102 rwork
rename e11103 rpart
rename e11105 rjcocc
rename e11106 rjcind
global work "rhours rwork rpart rjcocc rjcind"

rename S16 hatotn
rename S17 hatota
rename S02 habsns_p
rename S03 habsns
rename S10 hastck_p
rename S11 hastck
rename S18 haira_p
rename S19 haira
global wlth "hatotn hatota habsns* hastck* haira*"

***************************
* Keep variables of interest
***************************

keep $ids $demo $weights $inc $hhcomp $work $medc $risk $cond $adls $iadls $shlt $wlth cpi
order $ids $demo $weights $inc $hhcomp $work $medc $risk $cond $adls $iadls $shlt $wlth cpi
sort hhidpn year
by hhidpn: gen time = _n
by hhidpn: gen period = _N
by hhidpn: gen fyear = year[1]
by hhidpn: gen lyear = year[_N]
tsset hhidpn time

****************************
* adjust for inflation
****************************

foreach var of varlist $inc $wlth {
	replace `var' = `var'/cpi*189.33
} 

gen rbyr = year - rage

save data/psid/psid_panel.dta, replace


*********************************
* update status for 1994 to 2005 
* using files intermediate files from other project
*********************************

use data/psid/psid_panel.dta, clear
sort hhidpn year
merge hhidpn year using data/psid/status.dta, nokeep
drop _merge
save data/psid/psid_panel.dta, replace


exit





