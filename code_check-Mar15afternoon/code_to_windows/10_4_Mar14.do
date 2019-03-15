
// download data from: http://hdl.handle.net/10079/4f4qrrv
// copy and paste the url to your web browser


clear
use "Bhavnani_APSR_2009.dta"

/*----------------------------------------------
 part a
----------------------------------------------*/
	rename controltreat z
	rename turnout y
	

	ritest z _b[z], reps(1000): regress y z
	 //ate
	 di %8.7f el(r(b), 1, 1) 
	 // p-value two-sided
	 di %8.4f el(r(p), 1, 1) 

	
/*----------------------------------------------
 part b
----------------------------------------------*/


// p-value for one-tailed comparison
ritest z  testvar=((r(sd_2)^2)-(r(sd_1)^2)), ///
reps(10000) sav(10_4_var.dta, replace): ///
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




