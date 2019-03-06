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


******************************
* getting labor force status
* from raw individual file
******************************
/*
use indfile.dta, clear

replace ER33101 = 100000+_n if ER33101==0
replace ER33102 = 100000+_n if ER33102==0

merge ER33101 ER33102 using $PATH/PSID_Labor_Status/psid_ind_fam_1994.dta, nokeep sort _merge(m1994) replace update

replace ER33201 = 100000+_n if ER33201==0
replace ER33202 = 100000+_n if ER33202==0

merge ER33201 ER33202 using $PATH/PSID_Labor_Status/psid_ind_fam_1995.dta, nokeep sort _merge(m1995) replace update

replace ER33301 = 100000+_n if ER33301==0
replace ER33302 = 100000+_n if ER33302==0

merge ER33301 ER33302 using $PATH/PSID_Labor_Status/psid_ind_fam_1996.dta, nokeep sort _merge(m1996) replace update

replace ER33401 = 100000+_n if ER33401==0
replace ER33402 = 100000+_n if ER33402==0

merge ER33401 ER33402 using $PATH/PSID_Labor_Status/psid_ind_fam_1997.dta, nokeep sort _merge(m1997) replace update

replace ER33501 = 100000+_n if ER33501==0
replace ER33502 = 100000+_n if ER33502==0

merge ER33501 ER33502 using $PATH/PSID_Labor_Status/psid_ind_fam_1999.dta, nokeep sort _merge(m1999) replace update

replace ER33601 = 100000+_n if ER33601==0
replace ER33602 = 100000+_n if ER33602==0

merge ER33601 ER33602 using $PATH/PSID_Labor_Status/psid_ind_fam_2001.dta, nokeep sort _merge(m2001) replace update

replace ER33701 = 100000+_n if ER33701==0
replace ER33702 = 100000+_n if ER33702==0

merge ER33701 ER33702 using $PATH/PSID_Labor_Status/psid_ind_fam_2003.dta, nokeep sort _merge(m2003) replace update

replace ER33801 = 100000+_n if ER33801==0
replace ER33802 = 100000+_n if ER33802==0

merge ER33801 ER33802 using $PATH/PSID_Labor_Status/psid_ind_fam_2005.dta, nokeep sort _merge(m2005) replace update

save indfile_fam.dta, replace


global empq "ER30353 ER30382 ER30411 ER30441 ER30474 ER30509 ER30545 ER30580 ER30616 ER30653 ER30699 ER30744 ER30816 ER33111 ER33211 ER33311 ER33411 ER33512 ER33612 ER33712 ER33813"

use ER30001 ER30002  $empq using indfile_fam.dta, clear
gen hhidpn = ER30001*1000 + ER30002
sort hhidpn

rename ER30353 status1981
rename ER30382 status1982
rename ER30411 status1983
rename ER30441 status1984
rename ER30474 status1985
rename ER30509 status1986
rename ER30545 status1987
rename ER30580 status1988
rename ER30616 status1989
rename ER30653 status1990
rename ER30699 status1991
rename ER30744 status1992
rename ER30816 status1993
rename ER33111 status1994
rename ER33211 status1995
rename ER33311 status1996
rename ER33411 status1997
rename ER33512 status1999
rename ER33612 status2001
rename ER33712 status2003
rename ER33813 status2005

reshape long status, i(hhidpn) 
rename _j year
gen unemp = status==3 if status!=9&status!=0
gen disab = status==5 if status!=9&status!=0
sort hhidpn year
save status.dta, replace
*/
*********************************
* update status for 1994 to 2005 
* using files intermediate files from other project
*********************************

use data/psid/psid_panel.dta, clear
sort hhidpn year
merge hhidpn year using data/psid/status.dta, nokeep
drop _merge
save data/psid/psid_panel.dta, replace

************************************************
* get medical expenditure data from family files
************************************************
***** 1999 ***
use data/psid/PSID_fam_1999.dta, clear
rename ER13002 hhid
gen year = 1999


global med "ER15781  ER15787  ER15793  ER15799 ER13043 ER13064 ER15780" 

keep hhid year $med

*****************HEALTH EXPENDITURES and OOP
*"H64 TOTAL HOSPITAL/NURSNG HOME EXPENSES"  
*"H70 TOT DR/OUTPT SURGRY/DENTAL EXPENSES"  
*"H76 TOTAL PRESCRIPTN/OTR SVCS EXPENSES"   
*"H82 TOTAL COST ALL MEDICAL CARE"  
rename ER15781  tothop_nur
rename ER15787  totdoc
rename ER15793  totpres
rename ER15799  totmed
***********PREMIUMS 
*	A22 ANNUAL OWNR INSURANC 	
*	A30 MTG INCL INS PREM 	
*	H63 TOTAL PAID FOR HLTH INS LAST 2 YRS	
rename ER13043 yownin
rename ER13064 insprem
rename ER15780 tothealthins2y
sort hhid year



save temp/med_1999.dta, replace

***2001

use data/psid/PSID_fam_2001.dta, clear
rename ER17002 hhid
gen year = 2001
global med "ER19842  ER19848 ER19854  ER19860  ER17048 ER17073  ER19841"
keep hhid year $med
rename ER19842  tothop_nur
rename ER19848  totdoc
rename ER19854  totpres
rename ER19860  totmed
rename ER17048 yownin
rename ER17073  insprem
rename ER19841   tothealthins2y
sort hhid year
save temp/med_2001.dta, replace



***2003

use data/psid/PSID_fam_2003.dta, clear
rename ER21002 hhid
gen year = 2003


global med "ER23279  ER23285  ER23291 ER23297 ER21047 ER21071 ER23278 " 

keep hhid year $med


rename ER23285  totdoc
rename ER23291  totpres
rename ER23297  totmed
rename ER21047 yownin
rename ER21071  insprem
rename ER23278  tothealthins2y

sort hhid year

save temp/med_2003.dta, replace



***2005

use data/psid/PSID_fam_2005.dta, clear
rename ER25002 hhid
gen year = 2005

global med "ER27239  ER27245 ER27251  ER27257  ER25038 ER25062 ER27238"

keep hhid year $med


rename ER27245  totdoc
rename ER27251  totpres
rename ER27257  totmed
rename ER25038 yownin
rename ER25062 insprem
rename ER27238 tothealthins2y


sort hhid year

save temp/med_2005.dta, replace

* now merge to PSID_panel
use data/psid/psid_panel, clear
foreach s of numlist 1999 2001 2003 2005 {
	merge hhid year using temp/med_`s'.dta,  sort uniqusing
	drop _merge
	sort hhid year
}
foreach var of varlist totdoc totpres tothop_nur totmed {
	recode `var' (30000/max=.)
	replace `var' = `var'/cpi*189.33
}
gen oop = totdoc + totpres 
replace oop = oop + tothop_nur if year==1999
replace oop = oop/2
label var oop "annual out-of-pocket medical expenditures (family)"

***********************************************************
* save final dataset that will be used for various purposes
***********************************************************

save data/psid/psid_panel.dta, replace

exit





