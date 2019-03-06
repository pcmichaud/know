
* Figure 4 Baseline Scenario Average FK
clear all
capture log close
set mem 500m
* Figure 4

cd ~/perso/emetrics/know

preserve
	graph drop _all
	infile age fin1 fin2 fin3 cost1 cost2 cost3  using "figures/fig-4.txt", clear
	drop if age==25
	list cost1 cost2 cost3 if age>=110
	#d ;
	twoway (line fin1 age if age<=90, sort $lcolor) 
		   (line fin2 age if age<=90, sort $lcolor)
		   (line fin3 age if age<=90, sort $lcolor), 
		   xlabel(25(5)90) xtitle("age") nodraw $bgd
		   ylabel(0(25)100, labsize(vsmall)) ytitle("average FK") name(stock)
		   legend(label(1 "<HS") label(2 "HS") label(3 "College+") rows(1));
	#d cr
	#d ;
	twoway (line cost1 age if age<=90, sort $lcolor) 
		   (line cost2 age if age<=90, sort $lcolor)
		   (line cost3 age if age<=90, sort $lcolor), 
		   xlabel(25(5)90) xtitle("age") nodraw $bgd
		   ylabel(0(500)2000, labsize(vsmall)) ytitle("average expenditures FK") name(cost)
		   legend(label(1 "<HS") label(2 "HS") label(3 "College+") rows(1));
	#d cr
	graph combine stock cost, rows(2) ysize(6) xsize(5) $bgd
	graph export "figures/fig4.eps", as(eps) replace
restore
