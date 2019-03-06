***********************************************************
* do-file for calculating oop profiles
* using data from HRS
***********************************************************

cd ~/perso/emetrics/know

*****************************************
* Using RAND HRS version J
*****************************************

use data/hrs/rndhrsj_all.dta, clear

******************************
* sort data for panel analysis
******************************
keep if iwwave==1
sort hhidpn
tsset hhidpn time
keep if ragey_b>=62&ragender==1
drop if ragey_b>=90
keep if raracem==1
rename ragey_b rage

******************************
* create dependent variable  *
* drop if missing, put to 1 if
* negative
******************************
gen oop = (roopmd+soopmd)/2
drop if oop>=30000
gen logm = log(oop)


******************************
* create strata variable     *
******************************
recode raeduc (1=1) (2/3=2) (4/5=3), gen(strata)
label def strata 1 "lhs" 2 "hs" 3 "college" 
label val strata strata
label var strata "estimation group"
tab strata
drop if missing(strata)
******************************
* create age function        *
******************************
gen rage2 = rage*rage*0.01
gen rage3 = rage2*rage
gen rage4 = rage3*rage
global age "rage rage2"

******************************
* estimate process           *
* for each strata            *
******************************
	xi: reg logm $age i.strata [aw=wghh]
	matrix b = e(b)
	scalar se = e(rmse)
	predict res , res
	predict pred , xb
	gen pm = exp(pred)
	sum oop [aw=wgid]
	local mm = r(mean)
	sum pm [aw=wgid] 
	local mpm = r(mean)
	local smear = log(`mm'/`mpm')
	matrix b[1,5] = b[1,5] + `smear'
	replace pm = exp(`smear')*pm
	matrix list b
	 
matrix mPar = J(6,3,0) 
matrix mPar[1,1] = b[1,1]
matrix mPar[1,2] = b[1,1]
matrix mPar[1,3] = b[1,1]

matrix mPar[2,1] = b[1,2]
matrix mPar[2,2] = b[1,2]
matrix mPar[2,3] = b[1,2]

matrix mPar[3,1] =  b[1,5]
matrix mPar[3,2] = b[1,3] + b[1,5]
matrix mPar[3,3] = b[1,4] + b[1,5] 

// from HSZ for covariance structure
matrix mPar[4,1] = 0.901
matrix mPar[4,2] = 0.901
matrix mPar[4,3] = 0.901

matrix mPar[5,1] = 0.175
matrix mPar[5,2] = 0.156
matrix mPar[5,3] = 0.153

matrix mPar[6,1] = 0
matrix mPar[6,2] = 0
matrix mPar[6,3] = 0

matrix rownames mPar = rage rage2 const rho sige sigv
matrix colnames mPar = lhs hs col
matrix list mPar
svmat mPar, name(par)
keep par1-par3
drop if par1==.
outsheet using params/oop-par.csv, replace comma

exit







