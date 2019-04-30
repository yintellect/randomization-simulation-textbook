log using ../results/chapter03/exercise_3_6, replace

import delim ../data/chapter03/Clingingsmith_et_al_QJE_2009,clear
set seed 1234567

rename success D
rename views Y


cap program drop ate
program define ate, rclass
	args Y D
    sum `Y' if `D'==1, meanonly
    local Y_treat=r(mean)
    sum `Y' if `D'==0, meanonly
    local Y_con=r(mean)
    return scalar ate_avg = `Y_treat'-`Y_con'
end


// uncomment to install tsrtest package
// net install st0158.pkg, from(http://www.stata-journal.com/software/sj9-1/) replace
tsrtest D r(ate_avg) using 3_6_resam.dta, overwrite: ate Y D
  

preserve
use "3_6_resam.dta", clear
global ate = theta[1]
di "ate = "$ate
qui drop if _n==1
qui count if theta >= $ate
scalar p_onesided = r(N)/_N
qui count if abs(theta) >= $ate
scalar p_twosided = r(N)/_N
di "p.value.onesided = "p_onesided
di "p.value.twosided = "p_twosided
restore

log close
translate ../results/chapter03/exercise_3_6.smcl ../results/chapter03/exercise_3_6.pdf, translator(smcl2pdf)


