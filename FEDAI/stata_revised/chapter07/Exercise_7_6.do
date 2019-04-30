log using ../results/chapter07/exercise_7_6, replace

import delim ../data/chapter07/Angrist_et_al_AER_2006, clear


qui replace read = 0 if missing(read)
rename sex_name sex
gen observed = 1
qui replace observed = 0 if read == 0 

qui logit observed vouch0 sex phone vouch0#sex vouch0#phone
qui predict probobs
gen weights = 1/probobs

//Verify that all probabilities are 
// less than one and greater than zero

tabstat probobs, by(vouch0) stat(min p25 med mean p75 max) save


// Coefficients for unweighted regression 
// (restricting analysis to observed subjects)
qui regress read vouch0 if observed==1
mat li e(b)

qui regress read vouch0 [pw=weights]if observed==1
mat li e(b)


log close
translate ../results/chapter07/exercise_7_6.smcl ../results/chapter07/exercise_7_6.pdf, translator(smcl2pdf)
