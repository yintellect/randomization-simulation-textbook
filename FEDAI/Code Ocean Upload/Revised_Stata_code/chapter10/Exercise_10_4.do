log using ../results/chapter10/exercise_10_4, replace

import delim ../data/chapter10/Bhavnani_APSR_2009,clear
/*----------------------------------------------
 part a
----------------------------------------------*/
	rename controltreat z
	rename turnout y
	

	ritest z _b[z], reps(1000) nodots: regress y z
	 //ate
	 di %8.7f el(r(b), 1, 1) 
	 // p-value two-sided
	 di %8.4f el(r(p), 1, 1) 

	
/*----------------------------------------------
 part b
----------------------------------------------*/


// p-value for one-tailed comparison
ritest z  testvar=((r(sd_2)^2)-(r(sd_1)^2)), ///
reps(10000) sav(10_4_var.dta, replace) nodots: ///
sdtest y, by(z)
global testvar = el(r(b), 1, 1) 

preserve
use "10_4_var.dta", clear

qui count if testvar>=$testvar
// one-tailed p-value
di %8.4f r(N)/_N

qui count if abs(testvar)>=abs($testvar)
// one-tailed p-value
di %8.4f r(N)/_N
restore


log close
translate ../results/chapter10/exercise_10_4.smcl ../results/chapter10/exercise_10_4.pdf, translator(smcl2pdf)


