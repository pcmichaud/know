
* Figure 7

cd ~/perso/emetrics/know

	
infile id age value cons wealth income oop rate inv fin educ share  using "data/simulations/simknow_baseline.dat", clear
foreach var of varlist value cons wealth income oop rate inv fin share {
rename `var' `var'_ref 
}
sort id age
save data/simulations/fig7-ref.dta, replace


infile id age value cons wealth income oop rate inv fin educ share  using "data/simulations/simknow_mortality.dat", clear
foreach var of varlist value cons wealth income oop rate inv fin share {
rename `var' `var'_ssk 
}
save data/simulations/fig7-ssk.dta, replace

merge 1:1 id age using data/simulations/fig7-ref.dta
replace wealth_ssk = wealth_ssk/1000
replace wealth_ref = wealth_ref/1000
sort id age
gen unprep = wealth_ref < wealth_ssk if age==65
egen unprep65 = total(unprep), by(id)
gen unopt = (wealth_ssk - wealth_ref )/wealth_ssk if wealth_ref<wealth_ssk&age==65

preserve
keep if age==65 
#d ; 
	twoway (scatter wealth_ssk wealth_ref , msymbol(p) $scolor) (line wealth_ref wealth_ref, $lcolor) (lowess wealth_ssk wealth_ref, $lcolor), 
		xtitle("wealth target with FK ('000)") ytitle("wealth target without FK ('000)") $bgd
		legend(off);
	graph export "figures/fig7.eps", as(eps) replace;	
#d cr
restore

* additional analysis

tabstat unprep, by(educ)

tabstat cons_ref wealth_ref income_ref oop_ref rate_ref fin_ref share_ref, by(unprep)

gen income65 = income_ref if age<=64
egen avg_income65 = mean(income65), by(id)

gen rate45m = log(rate_ref) if age<=45
egen avg_rate45m = total(rate45m), by(id)
replace avg_rate45m = exp(avg_rate45m)

gen rate45p = log(rate_ref) if age>=45&age<=64
egen avg_rate45p = total(rate45p), by(id)
replace avg_rate45p = exp(avg_rate45p)


gen fin45m = fin_ref if age<=45
egen avg_fin45m = total(fin45m), by(id)
gen fin45p = fin_ref if age<=45
egen avg_fin45p = total(fin45p), by(id)


reg unopt ib1.educ avg_income65 avg_rate45m avg_rate45p
reg unprep ib1.educ avg_income65 fin_ref avg_rate45m avg_rate45p


exit
