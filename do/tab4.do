* table 4
infile id wealthlow wlthratio techratio knowratio using tables/table-4.txt, clear
mkmat wealthlow wlthratio techratio knowratio, matrix(tab4)
#d ;
matrix rownames tab4 = sig_down sig_up delta_down delta_up pi0_down 
	pi0_up pi1_down pi1_up cd_up cd_down beta_low beta_up prod9 prod75 prod5;
#d cr
matrix colnames tab4 = lesshs wlth tech know
matrix list tab4 
global fmt f(%9.4g)
outtable using tex/table4, mat(tab4) replace center nobox $fmt cap("Sensitivity Analysis")
