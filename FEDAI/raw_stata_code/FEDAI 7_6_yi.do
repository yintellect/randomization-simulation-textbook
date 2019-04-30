clear
use "Angrist_et_al_AER_2006.dta"

replace read = 0 if missing(read)
rename sex_name sex
gen observed = 1
replace observed = 0 if read == 0 

logit observed vouch0 sex phone vouch0#sex vouch0#phone
predict probobs
gen weights = 1/probobs

sum probobs if vouch0==0
sum probobs if vouch0==1

regress read vouch0 if observed==1
disp _b[_cons], _b[vouch0]

regress read vouch0 [pw=weights]if observed==1
disp _b[_cons], _b[vouch0]
