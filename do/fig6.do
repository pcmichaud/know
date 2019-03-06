* Figure 6 Preferences
cd ~/perso/emetrics/know

preserve
	infile age part1 part2 part3 wealth1 wealth2 wealth3 share1 share2 share3 rate1 rate2 rate3  using "figures/fig-6b.txt", clear
	drop if age==25
	#d ;
	twoway (line share1 age if age<=90, sort $lcolor) 
		   (line share2 age if age<=90, sort $lcolor)
		   (line share3 age if age<=90, sort $lcolor), 
	 xlabel(25(5)90, labsize(vsmall)) xtitle("age", size(small)) name(ez_share) $bgd
		   ylabel(0(0.25)1,  labsize(vsmall) angle(horizontal)  ) title("share soph. tech - epstein-zin", size(small)) nodraw
		   legend(label(1 "<HS") label(2 "HS") label(3 "College+") size(vsmall) rows(1));
	#d cr

	replace wealth1 = wealth1/1000
	replace wealth2 = wealth2/1000
	replace wealth3 = wealth3/1000
	
	#d ;
	twoway (line wealth1 age if age<=90, sort $lcolor) 
		   (line wealth2 age if age<=90, sort $lcolor)
		   (line wealth3 age if age<=90, sort $lcolor), 
	 xlabel(25(5)90, labsize(vsmall)) xtitle("age", size(small)) name(ez_wealth) $bgd
		   ylabel(0(100)500,  angle(horizontal) labsize(vsmall)) title("median wealth (thousands) - epstein-zin", size(small)) nodraw
		   legend(label(1 "<HS") label(2 "HS") label(3 "College+") size(vsmall) rows(1));
	#d cr

restore

preserve
	infile age share1 share2 share3 wealth1 wealth2 wealth3 _share1 _share2 _share3 _wealth1 _wealth2 _wealth3  using "figures/fig-6a.txt", clear
	drop if age==25
	#d ;
	twoway (line share1 age if age<=90, sort $lcolor) 
		   (line share2 age if age<=90, sort $lcolor)
		   (line share3 age if age<=90, sort $lcolor), 
		   xlabel(25(5)90, labsize(vsmall)) xtitle("age", size(small)) name(baseline_share) $bgd
		   ylabel(0(0.25)1,  angle(horizontal) labsize(vsmall)) title("share soph. tech - baseline", size(small)) nodraw
		   legend(label(1 "<HS") label(2 "HS") label(3 "College+") size(vsmall) rows(1));
	#d cr

	replace wealth1 = wealth1/1000
	replace wealth2 = wealth2/1000
	replace wealth3 = wealth3/1000
	
	#d ;
	twoway (line wealth1 age if age<=90, sort $lcolor) 
		   (line wealth2 age if age<=90, sort $lcolor)
		   (line wealth3 age if age<=90, sort $lcolor), 
		   xlabel(25(5)90, labsize(vsmall)) xtitle("age", size(small)) name(baseline_wealth) $bgd
		   ylabel(0(100)500,  angle(horizontal) labsize(vsmall)) title("median wealth (thousands) - baseline", size(small)) nodraw
		   legend(label(1 "<HS") label(2 "HS") label(3 "College+") size(vsmall) rows(1));
	#d cr

	#d ;
	twoway (line _share1 age if age<=90, sort $lcolor) 
		   (line _share2 age if age<=90, sort $lcolor)
		   (line _share3 age if age<=90, sort $lcolor), 
		    xlabel(25(5)90, labsize(vsmall)) xtitle("age", size(small)) name(hetpref_share)
		   ylabel(0(0.25)1,  angle(horizontal) labsize(vsmall)) title("share soph. tech - heterogeneity", size(small)) nodraw $bgd
		   legend(label(1 "<HS") label(2 "HS") label(3 "College+") size(vsmall) rows(1));
	#d cr

	replace _wealth1 = _wealth1/1000
	replace _wealth2 = _wealth2/1000
	replace _wealth3 = _wealth3/1000
	
	#d ;
	twoway (line _wealth1 age if age<=90, sort $lcolor) 
		   (line _wealth2 age if age<=90, sort $lcolor)
		   (line _wealth3 age if age<=90, sort $lcolor), 
		   title("wealth - het. preferences")
		    xlabel(25(5)90, labsize(vsmall)) xtitle("age", size(small)) name(hetpref_wealth) $bgd
		   ylabel(0(100)500,  angle(horizontal) labsize(vsmall)) title("median wealth (thousands) -  heterogeneity", size(small)) nodraw
		   legend(label(1 "<HS") label(2 "HS") label(3 "College+") size(vsmall) rows(1));
	#d cr

restore

	grc1leg baseline_share baseline_wealth hetpref_share hetpref_wealth ez_share ez_wealth, rows(3) xsize(4) legendfrom(baseline_share) $bgd
	graph export "figures/fig6.eps", as(eps) replace
	
	graph combine baseline_share baseline_wealth ez_share ez_wealth, rows(3) xsize(4) $bgd
	graph export "figures/fig6-slides.png", as(png) replace
	
	
