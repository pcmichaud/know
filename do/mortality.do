***************************************
* do-file for HRS data calibration of 
* mortality rates 
* Lusardi, Michaud and Mitchell (JPE)
****************************************

cd ~/perso/emetrics/know

*****************************************
* HRS all waves 1992-2006 + tracker file
*****************************************
use data/hrs/hrs-mortality.dta, clear
* set data for survival analysis
stset dur [iw=wgt], fail(fail==1) entry(entry) origin(time 50) id(hhidpn)
* sample cut
keep if male==0
keep if black==0
* estimate gompertz models by education levels
matrix param = J(2,3,.)
xi: streg if hschool==0&college==0, dist(gompertz) nohr
matrix par1 = e(b)'
xi: streg if hschool==1&college==0, dist(gompertz) nohr
matrix par2 = e(b)'
xi: streg if hschool==0&college==1, dist(gompertz) nohr
matrix par3 = e(b)'
matrix param = par1,par2,par3
clear
set obs 76
gen age = 24 + _n
gen mx1 = exp(param[2,1]*(age-50) + param[1,1])
gen mx2 = exp(param[2,2]*(age-50) + param[1,2])
gen mx3 = exp(param[2,3]*(age-50) + param[1,3])
keep mx1 mx2 mx3
outsheet using params/mortality-par.csv, replace comma

exit
