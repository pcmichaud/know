* Figure 3 NFCS data on financial knowledge and advice

capture log close

* set path
cd ~/perso/emetrics/know

* load NFCS data
use data/nfcs/nfcs2012.dta, clear

* sample selection
drop if age5==18
gen sample = 1
replace sample = 0 if white!=1
replace sample = 0 if male!=1

recode education (1=1) (2/3=2)  (4/5=3)
label define educ 1 "<HS" 2 "HS" 3 "College+"
label values education educ

replace index3 = index3==3 if index3!=.
replace index5 = index5==5 if index5!=.
label var index5 "fraction all right"

	recode savings_counseling (1=1) (2=0) (98=0) (99=0) (else=.), gen(advice)

	capture gen one = 1
	preserve
		collapse index5 [aw=weight] if sample==1, by(education age5)
		
		forvalues e = 1/3 {
			lowess index5 age5 if education==`e', gen(sindex5_`e') nodraw bw(0.6)
		}
		#d ;
		twoway (line sindex5_1 age5, $lcolor)
			   (line sindex5_2 age5,$lcolor)
			   (line sindex5_3 age5,$lcolor),
			   legend(label(1 "<HS") label(2 "HS") label(3 "College+") 
				size(small) rows(1))
				ylabel(,labsize(vsmall)) 
				xtitle("age", size(small)) ytitle("fraction ", size(small)) $bgd 
				xlabel(25(5)65,labsize(vsmall)) name(gindex5);		
		#d cr		
	restore
	
	preserve
		collapse advice [aw=weight] if sample==1, by(education)	
		#d ;
		graph bar advice, 
		over(education,relabel(1 "<HS" 2 "HS" 3 "College+")) name(gadvice) 
			bar(1,$bcolor) bar(2,$bcolor) bar(3,$bcolor) 
			blabel(bar, format(%6.3f)) ytitle("Fraction using financial advisor") $bgd;
		#d cr

	restore	
		
	di "test of interaction terms"
	reg index5 age ib1.education ib1.education#c.age [aw=weight] if sample==1
	testparm ib1.education#c.age	

	
	graph combine gindex5 gadvice, rows(1) $bgd
	graph export "figures/fig3.eps", as(eps) replace

capture log close
exit	
