clear
use "RushHour data for exercise 4-2.dta"

set seed 1234567
/*----------------------------------------------
 part a
----------------------------------------------*/
rename treatment D
rename posttest Y
rename pretest X

capture program drop Fstat
program define Fstat, rclass
	regress D X
    return scalar Fs=e(F)
end

tsrtest D r(Fs) using Fstat.dta, overwrite: Fstat

//ate
di r(obsvStat)

// p.value is different from R result due to rounding
// the permutation test is exactly the same
di r(uppertail)

// to exam the F statistics of every permutation
// you can use the file save the statistics

/*
use "Fstat.dta", clear
global Fstat=theta[1]
drop if _n==1 
*/

	
/*----------------------------------------------
 part b
----------------------------------------------*/

// calculate ate
qui reg Y D
global tau = _b[D]
di "ATE = " $tau



// RI under the null ate=ate

gen Y0_sim = Y
gen Y1_sim = Y
gen Y_sim = .
replace Y0_sim = Y - $tau if D==1
replace Y1_sim = Y + $tau if D==0
	
capture program drop ate_ci
program define ate_ci, rclass
	replace Y_sim = Y0_sim*(1-D) + Y1_sim*(D) 
	regress Y_sim D 
    return scalar Ys=_b[D]	
end

tsrtest D r(Ys) using ate_ci.dta, overwrite: ate_ci


use "ate_ci.dta", clear
drop if _n==1

sort theta

// 95% confidence interval (CI)

// 95% CI is different from R result due to rounding
// the permutation test is exactly the same

di round(theta[floor(_N*0.025)], 0.001) ,round(theta[floor(_N*0.975)], 0.001)


	
/*----------------------------------------------
 part c
----------------------------------------------*/
clear
use "RushHour data for exercise 4-2.dta"

rename treatment D
rename posttest Y
rename pretest X
rename improvement Y_improve 

// calculate ate.improve
qui reg Y_improve D
global tau_im = _b[D]
di "ATE.improve = " $tau_im


// RI under the null ate=ate.improve

gen Y0_sim = Y_improve
gen Y1_sim = Y_improve
gen Y_sim = .
replace Y0_sim = Y_improve - $tau_im if D==1
replace Y1_sim = Y_improve + $tau_im if D==0

capture program drop ate_im_ci
program define ate_im_ci, rclass
	replace Y_sim = Y0_sim*(1-D) + Y1_sim*(D) 
	regress Y_sim D 
    return scalar Ys_im=_b[D]	
end

tsrtest D r(Ys_im) using ate_im_ci.dta, overwrite: ate_im_ci

use "ate_im_ci.dta", clear
drop if _n==1

sort theta

// 95% confidence interval (CI)

// 95% CI is different from R result due to rounding
// the permutation test is exactly the same

di round(theta[floor(_N*0.025)], 0.001) ,round(theta[floor(_N*0.975)], 0.001)




