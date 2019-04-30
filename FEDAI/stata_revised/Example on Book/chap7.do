
// data download from http://hdl.handle.net/10079/76hdrk5

/* -----------------
	Box 7.1: ITT 
------------------*/

import delim Angrist_et_al_AER_2006, clear

// Subset data, keeping if age >= 9 & age <= 25 & checkid == 1
keep if age >= 9 & age <= 25 & checkid == 1

// Fix NA
//install dm67_4.pkg
nmissing

global mis_var=r(varlist) 

foreach var of varlist $mis_var {
replace `var' = 0 if missing(`var')
}

rename sex_name sex

// Generate a variable ("observed") indicating 
// whether or not the unit is observed (r_i=1)
gen observed = 1 - (read == 0)

recast int vouch0 sex age phone

gen vouch0sex = vouch0*sex
gen vouch0phone = vouch0*phone
gen vouch0age = vouch0*age

// Use logistic regression to predict probabilities of being observed
qui glm observed vouch0 sex phone age vouch0sex vouch0phone vouch0age, family(binomial)

predict probobs


// Compare distributions of predicted probabilities across experimental conditions
// Check to make sure that there are no zero predicted probabilities in either condition
tabstat probobs, by(vouch0) stat(min p25 med mean p75 max) nototal


// Generate weights: inverse of predicted probability of being observed
gen wt=1/probobs



// Restrict analysis to observed subjects.
gen sel_valid = observed == 1
tab sel_valid

//Coefficients for unweighted regression (restricting analysis to observed subjects)

qui reg read vouch0 if sel_valid == 1
mat unweight = e(b)
mat li unweight

// Coefficients for IPW regression (restricting analysis to observed subjects)
qui reg read vouch0 [iw=wt] if sel_valid == 1
mat weighted = e(b)
mat li weighted



