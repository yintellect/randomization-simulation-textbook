
// download data from: http://hdl.handle.net/10079/6hdr852
// copy and paste the url to your web browser


import delim "Clingingsmith_et_al_QJE_2009dta.csv",clear
set seed 1234567

rename success D
rename views Y


//findit tsrtest
//package name:  st0158.pkg install

cap program drop ate
program define ate, rclass
	args Y D
    sum `Y' if `D'==1, meanonly
    local Y_treat=r(mean)
    sum `Y' if `D'==0, meanonly
    local Y_con=r(mean)
    return scalar ate_avg = `Y_treat'-`Y_con'
end

// ssc install tsrtest
tsrtest D r(ate_avg) using 3_6_resam.dta, overwrite: ate Y D
  

preserve 
use "3_6_resam.dta", clear
global ate = theta[1]
di $ate
drop if _n==1
count if theta >= $ate
scalar p_onesided = r(N)/_N
count if abs(theta) >= $ate
scalar p_twosided = r(N)/_N
di "p.value.onesided = "p_onesided
di "p.value.twosided = "p_twosided 
restore


