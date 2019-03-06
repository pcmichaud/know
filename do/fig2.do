* Figure 2 Wealth Profiles
clear all
capture log close
set mem 500m

cd ~/perso/emetrics/know
preserve
	use data/psid/psid_panel.dta, clear


	*********************************
	* select sample 
	*********************************
	* keep years where wealth data
	keep if year>=1984
	* drop if net wealth missing
	keep if hatota!=.
	* keep males
	keep if rgender==1
	rename rage age
	* keep in age range trying to match
	keep if age>=25&age<=75
	* keep whites
	drop if hrace!=1

	******************************
	* sort data for panel analysis
	******************************
	sort hhidpn
	tsset hhidpn time

	******************************
	* create wealth measure      *
	* drop if missing, 
	******************************
	drop if hatota>2000000&hatota!=.
	replace hatota = 0 if hatota<0
	drop if habsns>0&habsns!=.	
	******************************
	* create educ variable     *
	******************************
	rename reduc educ
	label def educ 1 "lhs" 2 "hs" 3 "college" 
	label val educ educ
	label var educ "estimation group"
	tab educ
	drop if missing(educ)
	recode rbyr (min/1925=1) (1925/1935=2) (1935/1945=3) (1945/1955=4) (1955/1965=5) (1965/max=6)
	gen crbyr = rbyr
	
	******************************
	* moments of wealth, sophis  *
	* median regression, predict *
	* for 1935-1945 cohort       *
	******************************

	gen pmeda = .
	foreach x of numlist 1/3 {
		replace crbyr = rbyr
		qui xi: qreg hatota i.age i.crbyr if educ==`x'
		replace crbyr = 3
		predict pmeda_`x'
		replace pmeda = pmeda_`x' if educ==`x'
	}
	collapse (median) pmeda  [aw=wgid_cs], by(age educ)
	
	
	forvalues e=1/3 {
		lowess pmeda age if educ==`e', gen(meda_`e') nodraw
		replace meda_`e' = meda_`e'/1000
	}
	replace pmeda = pmeda/1000
	
		
	#d ;
	twoway (line meda_1 age if age<=75, $lcolor) 
		   (line meda_2 age if age<=75,$lcolor)
		   (line meda_3 age if age<=75,$lcolor)
		   
		   (scatter pmeda age if age<=75&educ==1,msymbol( oh ) $scolor) 
		   (scatter pmeda age if age<=75&educ==2,msymbol( oh ) $scolor)
		   (scatter pmeda age if age<=75&educ==3,msymbol( oh ) $scolor)   
		   , 
		   xlabel(25(5)75) xtitle("age")
		   ylabel(25(50)500) ytitle("median net assets (thousands)") $bgd
		   legend(label(1 "<HS") label(2 "HS") label(3 "College+") order(1 2 3) rows(1));
	#d cr
	graph export figures/fig2.eps, as(eps) replace
restore

exit

