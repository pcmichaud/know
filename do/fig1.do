* Figure 1: Income paths by age
cd ~/perso/emetrics/know

* Figure 1 Income Profiles
preserve
	use data/psid/psid_panel.dta, clear

	*********************************
	* select sample 
	*********************************
	keep if year>=1984&year<=2005
	keep if rage>=25&rage<=90
	* keep males
	keep if rgender==1
	* keep whites
	drop if hrace!=1
	
	******************************
	* sort data for panel analysis
	******************************
	sort hhidpn
	tsset hhidpn time
	
	******************************
	* create dependent variable  *
	* drop if missing, or neg
	******************************
	*replace hinet = hinet - hicap - hipub
	drop if hinet<=0|missing(hinet)
	drop if hinet>=250000
	gen y = hinet - hicap - hipub
	
	******************************
	* create strata variable     *
	******************************
	gen strata = reduc
	label def strata 1 "lhs" 2 "hs" 3 "college"
	label val strata strata
	label var strata "estimation group"
	tab strata
	drop if missing(strata)
	recode rbyr (min/1925=1) (1925/1935=2) (1935/1945=3) (1945/1955=4) (1955/1965=5) (1965/max=6)
	gen crbyr = rbyr
	gen py = .
	foreach x of numlist 1/3 {
		replace crbyr = rbyr
		qui xi: reg y i.rage i.crbyr if strata==`x'
		replace crbyr = 3
		predict py_`x'
		replace py = py_`x' if strata==`x'
	}
	
	collapse (mean) y py [aw=wgid_cs], by(rage strata)
	
	forvalues e=1/3 {
		lowess py rage if strata==`e', gen(medy_`e') nodraw bw(0.5)
		replace medy_`e' = medy_`e'/1000
	}
	replace py = py/1000
	#d ;
	twoway (line medy_1 rage if rage<=75, $lcolor) 
		   (line medy_2 rage if rage<=75, $lcolor)
		   (line medy_3 rage if rage<=75, $lcolor)
		   (scatter py rage if rage<=75&strata==1,msymbol( oh ) $scolor) 
		   (scatter py rage if rage<=75&strata==2,msymbol( oh ) $scolor)
		   (scatter py rage if rage<=75&strata==3,msymbol( oh ) $scolor), 
		   xlabel(25(5)75) xtitle("age") $bgd 
		   ylabel(20(10)80) ytitle("average net household income (thousands)")
		   legend(label(1 "<HS") label(2 "HS") label(3 "College+") order(1 2 3) rows(1));
	#d cr
	graph export figures/fig1.eps, as(eps) replace
		  
restore

exit

