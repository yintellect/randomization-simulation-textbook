log using ../results/chapter04/exercise_4_2, replace


qui import delim ../data/chapter04/Gendelman_2004, clear
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

// calculate 48620 (18 choose 9) permutations
tsrtest D r(Fs) using 4_2_Fstat.dta, overwrite: Fstat

//ate
di r(obsvStat)

// p.value
di r(uppertail)



	
/*----------------------------------------------
 part b
----------------------------------------------*/

// calculate ate
qui reg Y D
global tau = _b[D]
di "ATE = " $tau



// RI under the null ate=ate
qui gen Y0_sim = Y
qui gen Y1_sim = Y
qui gen Y_sim = .
qui replace Y0_sim = Y - $tau if D==1
qui replace Y1_sim = Y + $tau if D==0
	
capture program drop ate_ci
program define ate_ci, rclass
	replace Y_sim = Y0_sim*(1-D) + Y1_sim*(D) 
	regress Y_sim D 
    return scalar Ys=_b[D]	
end

tsrtest D r(Ys) using ate_ci.dta, overwrite: ate_ci

preserve
use "ate_ci.dta", clear
qui drop if _n==1

sort theta

// 95% confidence interval (CI)
di "(" round(theta[floor(_N*0.025)], 0.001) ", "round(theta[floor(_N*0.975)], 0.001) ")"

restore
	
/*----------------------------------------------
 part c
----------------------------------------------*/

rename improvement Y_improve

// calculate ate.improve
qui reg Y_improve D
global tau_im = _b[D]
di "ATE.improve = " $tau_im


// RI under the null ate=ate.improve
qui replace Y0_sim = Y_improve
qui replace Y1_sim = Y_improve
qui replace Y_sim = .

qui replace Y0_sim = Y_improve - $tau_im if D==1
qui replace Y1_sim = Y_improve + $tau_im if D==0


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

di "95%CI = " "(" round(theta[floor(_N*0.025)], 0.001) " , " round(theta[floor(_N*0.975)], 0.001) ")"

log close
translate ../results/chapter04/exercise_4_2.smcl ../results/chapter04/exercise_4_2.pdf, translator(smcl2pdf)


