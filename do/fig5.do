
* Figure 5 Decomposition of wealth inequality (5 and 5a)
cd ~/perso/emetrics/know

preserve
	infile id wratio1 wratio2 wratio3 ratio  using "figures/fig-5.txt", clear
	
	#d ;
	label def id 1 "uncertainty" 2 "cmin=10k" 3 "rep. rate" 4 "demographics" 5 "mortality" 6 "baseline";
		
	graph hbar ratio if inlist(id,1,2,3,4,5,6), 
		over(id,relabel(1 "uncertainty" 2 "cons. floor" 3 "rep. rate" 
			4 "demographics" 5 "mortality" 6 "knowledge")) 
			ylabel(0(0.5)3) $bgd
			bar(1,$bcolor) bar(2,$bcolor) bar(3,$bcolor) bar(4,$bcolor) bar(5,$bcolor) bar(6,$bcolor)
			blabel(bar, format(%6.3f)) ytitle("wealth ratio: college+/<HS");
		;
	graph export "figures/fig5.eps", as(eps) replace;
	
	di "share explained by FK = " (ratio[6]-ratio[5])/(ratio[6]-ratio[1]);
restore

