
// download data from: http://hdl.handle.net/10079/6hdr852
// copy and paste the url to your web browser

import delim "Clingingsmith_et_al_QJE_2009.csv",clear
set seed 1234567

rename success D
rename views Y



// ssc install tsrtest
// fptest -- Two-sample Fisher-Pitman permutation test for equality of means
fptest Y, by(D)

//One-side p-value under the null ATE = 0
di  r(lowertail)     

//two-side p-value under the null ATE = 0
di  r(twotail)    
