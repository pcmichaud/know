
* Figure 8 Learning by Doing
cd ~/perso/emetrics/know

preserve
	#d ;
	infile age part1 part2 part3 wealth1 wealth2 wealth3 _part1 _part2 _part3 _wealth1 
		_wealth2 _wealth3 base_part1 base_part2 base_part3 base_wealth1 base_wealth2 
			base_wealth3 using "figures/fig-8.txt", clear;
	#d cr		
	drop if age==25
	#d ;
	twoway (line part1 age if age<=90, sort $lcolor) 
		   (line part2 age if age<=90, sort $lcolor)
		   (line part3 age if age<=90, sort $lcolor), 
		   xlabel(25(5)90) xtitle("age") name(part1) $bgd
		   ylabel(0(0.2)1, labsize(vsmall)) title("part. soph. tech - {&phi} = 5", size(small)) nodraw
		   legend(label(1 "<HS") label(2 "HS") label(3 "College+") rows(1));
	#d cr

		#d ;
	twoway (line _part1 age if age<=90, sort $lcolor) 
		   (line _part2 age if age<=90, sort $lcolor)
		   (line _part3 age if age<=90, sort $lcolor), 
		   xlabel(25(5)90) xtitle("age") name(part2) $bgd
		   ylabel(0(0.2)1,labsize(vsmall)) title("part. soph. tech - {&phi} = 10", size(small)) nodraw
		   legend(label(1 "<HS") label(2 "HS") label(3 "College+") rows(1));
	#d cr

	#d ;
	twoway (line base_part1 age if age<=90, sort $lcolor) 
		   (line base_part2 age if age<=90, sort $lcolor)
		   (line base_part3 age if age<=90, sort $lcolor), 
		   xlabel(25(5)90) xtitle("age") name(part3) $bgd
		   ylabel(0(0.2)1,labsize(vsmall)) title("part. soph. tech - baseline", size(small)) nodraw
		   legend(label(1 "<HS") label(2 "HS") label(3 "College+") rows(1));
	#d cr
	
	replace wealth1 = wealth1/1000
	replace wealth2 = wealth2/1000
	replace wealth3 = wealth3/1000
	
	replace _wealth1 = _wealth1/1000
	replace _wealth2 = _wealth2/1000
	replace _wealth3 = _wealth3/1000
	
	replace base_wealth1 = base_wealth1/1000
	replace base_wealth2 = base_wealth2/1000
	replace base_wealth3 = base_wealth3/1000
	
	#d ;
	twoway (line wealth1 age if age<=90, sort $lcolor) 
		   (line wealth2 age if age<=90, sort $lcolor)
		   (line wealth3 age if age<=90, sort $lcolor), 
		   xlabel(25(5)90) xtitle("age") name(wealth1) $bgd
		   ylabel(0(150)500,labsize(vsmall)) title("median wealth (thousands) - {&phi} = 5", size(small)) nodraw
		   legend(label(1 "<HS") label(2 "HS") label(3 "College+") rows(1));
	#d cr

		#d ;
	twoway (line _wealth1 age if age<=90, sort $lcolor) 
		   (line _wealth2 age if age<=90, sort $lcolor)
		   (line _wealth3 age if age<=90, sort $lcolor), 
		   xlabel(25(5)90) xtitle("age") name(wealth2) $bgd
		   ylabel(0(150)500,labsize(vsmall)) title("median wealth (thousands) - {&phi} = 10", size(small)) nodraw
		   legend(label(1 "<HS") label(2 "HS") label(3 "College+") rows(1));
	#d cr

			#d ;
	twoway (line base_wealth1 age if age<=90, sort $lcolor) 
		   (line base_wealth2 age if age<=90, sort $lcolor)
		   (line base_wealth3 age if age<=90, sort $lcolor), 
		   xlabel(25(5)90) xtitle("age") name(wealth3) $bgd
		   ylabel(0(150)500,labsize(vsmall)) title("median wealth (thousands) - baseline", size(small)) nodraw
		   legend(label(1 "<HS") label(2 "HS") label(3 "College+") rows(1));
	#d cr
	
	grc1leg part3 wealth3 part1 wealth1 part2 wealth2, rows(3) $bgd legendfrom(wealth3)
	graph export "figures/fig8.eps", as(eps) replace
restore
