*************************************************************
* master code to prepare inputs and create tables and figures
* Lusardi, Michaud and Mitchell (JPE)
*************************************************************

clear all
capture log close
set more off
set processors 4

* --- home directory of project
cd ~/know

* global options for graphs
global bgd "plotregion(fcolor(white))   graphregion(fcolor(white) color(white))" 
global fmt f(%9.3g)
set scheme s2mono, permanently
global lcolor "lcolor(black)"
global bcolor "lcolor(black) fcolor(black)"
global scolor "mlcolor(black)"
global irun = 0

* +++ preparing data and inputs

* --- prepare PSID data
do do/construct_psid.do

* --- prepare PSID data
do do/construct_nfcs.do

* --- work income processes (PSID)
do do/income-work.do

* --- retirement income process (PSID)
do do/income-ret.do

* --- out-of-pocket process (HRS)
do do/oop.do

* --- equivalence scales  (PSID)
do do/equivalence.do

* --- mortality rates (HRS)
do do/mortality.do

* --- initial sample for simulations (PSID)
do do/initsample.do


* +++ Running simulations (need to compile on own system and run)
if ($irun==1) {
	! ./build/resultsJPE.sh
}

* +++ producing tables

* --- wealth statistics for Table 1 and 2
do do/wealth.do

* --- Table 1
do do/tab1.do

* --- Table 2
do do/tab2.do

* --- Table 3
do do/tab3.do

* --- Table 4
do do/tab4.do

* --- Table 5
do do/tab5.do

* +++ producing figures
graph drop _all

* --- Figure 1
do do/fig1.do

* --- Figure 2
do do/fig2.do

* --- Figure 3
do do/fig3.do

* --- Figure 4
do do/fig4.do

* --- Figure 5
do do/fig5.do

* --- Figure 5
do do/fig5.do

* --- Figure 6
do do/fig6.do

* --- Figure 7
do do/fig7.do

* --- Figure 8
do do/fig8.do

* --- Figure 9
do do/fig9.do

capture log close
exit
