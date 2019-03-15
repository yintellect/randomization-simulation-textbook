clear
use "Bhavnani_APSR_2009.dta"

*part a
	rename controltreat z
	rename turnout y
	
*part 
	ritest z _b[z], reps(1000): regress y z
	// getting variance from ritest
	
*part b


// p-value for one-tailed comparison
ritest z  testvar=((r(sd_2)^2)-(r(sd_1)^2)) , reps(1000) right: sdtest y, by(z)

// p-value for two-tailed comparison
ritest z  testvar=((r(sd_2)^2)-(r(sd_1)^2)) , reps(10000):  ttest y, by(z)

