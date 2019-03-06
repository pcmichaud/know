***************************************
* do-file for PSID data calibration of 
* income process 
* Lusardi, Michaud and Mitchell (JPE)
****************************************

cd ~/perso/emetrics/know

*****************************************
* use built file (see construct_psid.do)
*****************************************

use data/psid/psid_panel.dta, clear

*********************************
* select sample 
*********************************
* years with annual surveys
keep if year>=1980&year<=1997
* age range to estimate income process
keep if rage>=25&rage<=62&hissben==0
* keep males
keep if rgender==1
* keep whites
drop if hrace!=1

******************************
* sort data for panel analysis
******************************
sort hhidpn
tsset hhidpn time

*************************************
* create dependent variable         *
* drop if missing, or zero          *
* household income 
* minus capital and public assistance
*************************************
replace hinet = hinet - hicap - hipub
drop if hinet<=0|missing(hinet)
drop if hinet>=250000
gen y = hinet
gen logy = log(y)

******************************
* create strata variable     *
******************************

gen strata = reduc
label def strata 1 "lhs" 2 "hs" 3 "college"
label val strata strata
label var strata "estimation group"
tab strata
drop if missing(strata)

******************************
* create age function        *
******************************
gen rage2 = rage*rage*0.01

global age "rage rage2"

******************************
* compute unrestricted prof  *
******************************
table rage reduc [aw=wgid_cs], content(mean y)

******************************
* estimate process           *
* for each strata            *
******************************
gen res = .
gen py = .

forvalues s=1/3 {
	reg logy $age [aw=wgid_cs] if strata==`s'
	matrix b_`s' = e(b)
	scalar se = e(rmse)
	predict res_`s' if strata==`s', res
	predict pred_`s' if strata==`s', xb
	replace res = res_`s' if strata==`s'
	replace py = exp(pred_`s') if strata==`s'
	sum y [aw=wgid_cs] if strata==`s'
	local my = r(mean)
	sum py [aw=wgid_cs] if strata==`s' 
	local mpy = r(mean)
	local smear = log(`my'/`mpy')
	matrix b_`s'[1,3] = b_`s'[1,3] + `smear'
	replace py = exp(`smear')*py if strata==`s' 
	di `s'
	matrix list b_`s'
}

   	   
matrix bpar = (b_1',b_2',b_3')
matrix colnames bpar = lhs hs col		   	   	   	   
matrix list bpar

* use HSZ (1995) for covariance structure
* rho
matrix bcov = J(3,3,0)
matrix bcov[1,1] = 0.955
matrix bcov[1,2] = 0.946
matrix bcov[1,3] = 0.955
* sige 
matrix bcov[2,1] = 0.033
matrix bcov[2,2] = 0.025
matrix bcov[2,3] = 0.016
* sigv (set to zero in HSZ)
matrix bcov[3,1] = 0
matrix bcov[3,2] = 0
matrix bcov[3,3] = 0

* save parameters
matrix par = bpar\bcov

svmat par, name(par)
keep par1 par2 par3
drop if par1==.
outsheet using params/income-work-par.csv, replace comma


exit




