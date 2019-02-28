clear
use "Angrist_et_al_AER_2006.dta"

replace read = 0 if missing(read)
rename sex_name sex
gen observed = 1
replace observed = 0 if read == 0 
gen vouch1=vouch0*sex
gen vouch2=vouch0*phone
gen id = _n

//bysort or rangerun?
