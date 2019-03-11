clear
//log using 10_3, name(solution10_3j)


/*----------------------------------------------
 part j
----------------------------------------------*/	
	input z Y0M0 Y1M0 Y0M1 Y1M1 M0 M1
	0 0 0 0 0 0 0 
	0 0 0 0 0 0 1
	0 0 0 0 0 1 1
	0 0 1 0 1 0 0
	0 0 1 0 1 0 1 
	0 0 1 0 1 1 1
	1 1 0 1 1 0 0
	1 1 0 1 1 0 1
	1 1 0 1 1 1 1
	1 0 1 1 1 0 0
	1 0 1 1 1 0 1
	1 0 1 1 1 1 1
	end

/****verify column averages***/	

tabstat Y0M0 Y1M0 Y0M1 Y1M1, stat(mean)	

gen M = .
gen Y = .

/**** Exact Permutation***/	

capture program drop coef
program define coef, rclass
	replace M = M0*(1-z) + M1*z
	replace Y = Y0M0*(1-z)*(1-M) + Y1M0*(z)*(1-M) + Y0M1*(1-z)*(M) + Y1M1*(z)*(M)	
	qui reg Y M z
	return scalar coy = _b[_cons]
	return scalar com = _b[M]
	return scalar coz = _b[z]
end

//ssc install tsrtest
//OR findit tsrtest to install

tsrtest z r(coy) using co_y.dta, overwrite: coef 
tsrtest z r(com) using co_m.dta, overwrite: coef
tsrtest z r(coz) using co_z.dta, overwrite: coef 


capture program drop tcoef
program define tcoef, rclass
	replace M = M0*(1-z) + M1*z
	replace Y = Y0M0*(1-z)*(1-M) + Y1M0*(z)*(1-M) + Y0M1*(1-z)*(M) + Y1M1*(z)*(M)	
	qui reg Y z
	return scalar tcoy = _b[_cons]
	return scalar tcoz = _b[z]
end

tsrtest z r(tcoy) using tco_y.dta, overwrite: tcoef

tsrtest z r(tcoz) using tco_z.dta, overwrite: tcoef 


capture program drop mcoef
program define mcoef, rclass
	replace M = M0*(1-z) + M1*z
	qui reg M z
	return scalar mcom = _b[_cons]
	return scalar mcoz = _b[z]
end

tsrtest z r(mcom) using mco_m.dta, overwrite: mcoef 
tsrtest z r(mcoz) using mco_z.dta, overwrite: mcoef





clear
/**** colMeans(na.omit(coefmat))***/	
use "co_y.dta", clear
rename theta co_y


merge 1:1 _n using "co_m.dta"
rename theta co_m 

drop _merge
merge 1:1 _n using "co_z.dta"
rename theta co_z 
drop _merge

// drop the observation statistics
drop if _n == 1 
// different result for perfect colinearity between M and Z
drop if _n==530| _n==566|_n==572|_n==797|_n==803|_n==832

tabstat co_y co_m co_z,stat(mean)


clear
/**** colMeans(na.omit(tcoefmat)))***/	

use "tco_y.dta", clear
rename theta tco_y

merge 1:1 _n using "tco_z.dta"
rename theta tco_z 
drop _merge

// drop the observation statistics
drop if _n == 1 
tabstat tco_y tco_z,stat(mean)

       
clear
/**** colMeans(na.omit(mcoefmat)))***/	
use "mco_m.dta", clear
rename theta mco_m

merge 1:1 _n using "mco_z.dta"
rename theta mco_z 
drop _merge

// drop the observation statistics
drop if _n == 1 
tabstat mco_m mco_z,stat(mean)


/*----------------------------------------------
 part k
----------------------------------------------*/	
clear	 

use "mco_z.dta"
rename theta mco_z 

merge 1:1 _n using "co_z.dta"
rename theta co_z 
drop _merge 

gen asbs = mco_z*co_z
drop if _n == 1 
// different result for perfect colinearity between M and Z
drop if _n==530| _n==566|_n==572|_n==797|_n==803|_n==832

tabstat asbs,stat(mean)
