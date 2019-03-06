* Figure 9 Diversification
cd ~/perso/emetrics/know

preserve
	#d ;
	infile age part1 part2 part3 wealth1 wealth2 wealth3 share1 share2 share3 rate1 rate2 rate3
		_part1 _part2 _part3 _wealth1 _wealth2 _wealth3 _share1 _share2 _share3 _rate1 _rate2 _rate3 
		using "figures/fig-9.txt", clear;
	#d cr	
	drop if age==25
	#d ;
	twoway (line part1 age if age<=90, sort $lcolor) 
		   (line part2 age if age<=90, sort $lcolor)
		   (line part3 age if age<=90, sort $lcolor), 
		   xlabel(25(5)90, labsize(small)) xtitle("age", size(small)) name(share) $bgd
		   ylabel(0(0.1)1, labsize(small)) title("participation soph. technology - diversification", size(small)) nodraw
		   legend(label(1 "<HS") label(2 "HS") label(3 "College+") size(small) rows(1));
	#d cr
	#d ;
	twoway (line _part1 age if age<=90, sort $lcolor) 
		   (line _part2 age if age<=90, sort $lcolor)
		   (line _part3 age if age<=90, sort $lcolor), 
		   xlabel(25(5)90, labsize(small)) xtitle("age", size(small)) name(share_base) $bgd
		   ylabel(0(0.1)1, labsize(small)) title("participation soph. technology - baseline", size(small)) nodraw
		   legend(label(1 "<HS") label(2 "HS") label(3 "College+") size(small) rows(1));
	#d cr
	
	replace wealth1 = wealth1/1000
	replace wealth2 = wealth2/1000
	replace wealth3 = wealth3/1000
	replace _wealth1 = _wealth1/1000
	replace _wealth2 = _wealth2/1000
	replace _wealth3 = _wealth3/1000	
	#d ;
	twoway (line wealth1 age if age<=90, sort $lcolor) 
		   (line wealth2 age if age<=90, sort $lcolor)
		   (line wealth3 age if age<=90, sort $lcolor), 
		   xlabel(25(5)90, labsize(small)) xtitle("age", size(small)) name(wealth) $bgd
		   ylabel(0(100)500, labsize(small)) title("median wealth (thousands) - diversification", size(small)) nodraw
		   legend(label(1 "<HS") label(2 "HS") label(3 "College+") size(small) rows(1));
	#d cr
	#d ;
	twoway (line _wealth1 age if age<=90, sort $lcolor) 
		   (line _wealth2 age if age<=90, sort $lcolor)
		   (line _wealth3 age if age<=90, sort $lcolor), 
		   xlabel(25(5)90, labsize(small)) xtitle("age", size(small)) name(wealth_base) $bgd
		   ylabel(0(100)500, labsize(small)) title("median wealth (thousands) - baseline", size(small)) nodraw
		   legend(label(1 "<HS") label(2 "HS") label(3 "College+") size(small) rows(1));
	#d cr	
	sum wealth* if age==65
	grc1leg share_base wealth_base  share wealth , rows(2) $bgd legendfrom(share_base)
	graph export "figures/fig9.eps", as(eps) replace
restore
