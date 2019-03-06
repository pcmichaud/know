*********************************************
* do-file for PSID data for wealth statistics
* Lusardi, Michaud and Mitchell (JPE)
*********************************************

cd ~/perso/emetrics/know

*****************************************
* this is a panel from 1980-2005 with key
* data from Cornell distribution
* see construct.do for construction
*****************************************

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
******************************
drop if hatota>2000000&hatota!=.
replace hatota = 0 if hatota<0
drop if habsns>0&habsns!=.

***********************************
* create technology part measure  *
***********************************
recode hastck_p (0=0) (.=.) (else=1)
recode haira_p (0=0) (.=.) (else=1)
egen hasoph_p = anymatch(hastck_p haira_p), values(1)
replace hasoph_p = . if hastck_p==.|haira_p==. 
* share
gen hatots = (hastck + haira)/hatotn
* only for those who participate
replace hatots = . if hasoph_p==0|hatotn<=0
replace hatots = . if hatots>1&hatots!=.

* wealth less than income
gen lowealth = hatota < 2*hinet if hatota!=.&hinet!=.
******************************
* create educ variable     *
******************************

rename reduc educ

label def educ 1 "lhs" 2 "hs" 3 "college" 
label val educ educ
label var educ "estimation group"
tab educ
drop if missing(educ)


******************************
* years for statistics		 *
******************************

keep if inlist(year,2001,2003,2005)

******************************
* moments for table 1		 *
******************************
egen cage = cut(age), at(25(10)75)
preserve
table cage educ [aw=wghh], content(mean hasoph_p) row col replace
matrix result = J(6,4,.)

matrix result[6,4] = table1[1]
matrix result[1,4] = table1[2]
matrix result[2,4] = table1[3]
matrix result[3,4] = table1[4]
matrix result[4,4] = table1[5]
matrix result[5,4] = table1[6]

matrix result[6,1] = table1[7]
matrix result[6,2] = table1[8]
matrix result[6,3] = table1[9]

forvalues a = 1/5 {
	forvalues e = 1/3 {
		local j = (`a'-1)*3 + `e'
		matrix result[`a',`e'] = table1[9+`j']
	}
}
matrix colnames result = lesshs hs college tot
svmat result, name(col)
keep lesshs hs college tot
drop if lesshs==.
outfile lesshs hs college tot using tables/table-1.txt, replace wide	
restore

preserve
table cage educ [aw=wghh], content(count hatots)

table cage educ [aw=wghh], content(mean hatots) row col replace

matrix result = J(6,4,.)

matrix result[6,4] = table1[1]
matrix result[1,4] = table1[2]
matrix result[2,4] = table1[3]
matrix result[3,4] = table1[4]
matrix result[4,4] = table1[5]
matrix result[5,4] = table1[6]

matrix result[6,1] = table1[7]
matrix result[6,2] = table1[8]
matrix result[6,3] = table1[9]

forvalues a = 1/5 {
	forvalues e = 1/3 {
		local j = (`a'-1)*3 + `e'
		matrix result[`a',`e'] = table1[9+`j']
	}
}
matrix colnames result = lesshs hs college tot
matrix list result
svmat result, name(col)
keep lesshs hs college tot
drop if lesshs==.
outfile lesshs hs college tot using tables/table-1a.txt, replace wide	
restore

******************************
* moments table 2			 *
******************************
keep if inlist(age,63,64,65,66,67)

#d ;
collapse (median) medwealth = hatota 
		(mean) lowealth
		(mean) isoph = hatots
		(mean) esoph = hasoph_p
		(count) n = hatota
		[aw=wghh], by(educ);
#d cr
tabstat n, by(educ)
mkmat medwealth lowealth esoph isoph, matrix(result)
matrix result = result'
matrix rownames result = medwealth lowealth esoph isoph
matrix colnames result = lesshs hs college

matrix list result		

set obs 4
svmat result, name(col)
keep lesshs hs college
drop if lesshs==.
outfile lesshs hs college using tables/psid-tab-2.txt, replace wide
		
exit


