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

/***coefmat***/
capture program drop coef
program define coef, rclass
	replace M = M0*(1-z) + M1*z
	replace Y = Y0M0*(1-z)*(1-M) + Y1M0*(z)*(1-M) + Y0M1*(1-z)*(M) + Y1M1*(z)*(M)	
	qui reg Y M z
	return scalar coy = _b[_cons]
	return scalar com = _b[M]
	return scalar coz = _b[z]
	return scalar nocoli = _se[z]
end

//ssc install tsrtest
//OR findit tsrtest to install

tsrtest z r(coy) using co_y.dta, overwrite: coef 
tsrtest z r(com) using co_m.dta, overwrite: coef
tsrtest z r(coz) using co_z.dta, overwrite: coef 
tsrtest z r(nocoli) using nocoli.dta, overwrite: coef 


 
/***tcoefmat***/
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


/***mcoefmat***/
capture program drop mcoef
program define mcoef, rclass
	replace M = M0*(1-z) + M1*z
	qui reg M z
	return scalar mcom = _b[_cons]
	return scalar mcoz = _b[z]
end

tsrtest z r(mcom) using mco_m.dta, overwrite: mcoef 
tsrtest z r(mcoz) using mco_z.dta, overwrite: mcoef



/**** colMeans(na.omit(coefmat))***/
preserve	
use "co_y.dta", clear
rename theta co_y


merge 1:1 _n using "co_m.dta"
rename theta co_m 

drop _merge
merge 1:1 _n using "co_z.dta"
rename theta co_z 
drop _merge

/*check coli*/
merge 1:1 _n using "nocoli.dta"
rename theta nocoli
drop _merge


drop if _n == 1 
// omit instances of perfect colinearity between M and Z
drop if nocoli==0

tabstat co_y co_m co_z,stat(mean)

restore


/**** colMeans(na.omit(tcoefmat)))***/	
preserve
use "tco_y.dta", clear
rename theta tco_y

merge 1:1 _n using "tco_z.dta"
rename theta tco_z 
drop _merge


/*check coli*/
merge 1:1 _n using "nocoli.dta"
rename theta nocoli
drop _merge

// drop the observation statistics
drop if _n == 1 

tabstat tco_y tco_z,stat(mean)
restore
       

/**** colMeans(na.omit(mcoefmat)))***/
preserve	
use "mco_m.dta", clear
rename theta mco_m

merge 1:1 _n using "mco_z.dta"
rename theta mco_z 
drop _merge

// drop the observation statistics
drop if _n == 1 
tabstat mco_m mco_z,stat(mean)
restore

/*----------------------------------------------
 part k
----------------------------------------------*/	
preserve	 

use "mco_z.dta", clear
rename theta mco_z 

merge 1:1 _n using "co_z.dta"
rename theta co_z 
drop _merge 

/*check coli*/
merge 1:1 _n using "nocoli.dta"
rename theta nocoli
drop _merge


gen asbs = mco_z*co_z
drop if _n == 1 
// omit instances of perfect colinearity between M and Z
drop if nocoli==0


tabstat asbs,stat(mean)
restore


/* -------------------
colinearity test
---------------------*/
// a perfect collinearity case
input z530
0 
1  
1  
0  
1  
1  
0  
0  
1  
0  
0  
1 
end

replace M = M0*(1-z530) + M1*z530
replace Y = Y0M0*(1-z530)*(1-M) + Y1M0*(z530)*(1-M) + Y0M1*(1-z530)*(M) + Y1M1*(z530)*(M)	
reg Y M z530

ereturn list
matrix B530 = e(b)
matrix V530=e(V)


// if there is colinearity, _se[z]==0


// not perfect collinearity case
replace M = .
replace Y = .
input znoco150
1  
1  
0  
0  
1  
1  
0  
0  
1  
1  
0  
0 


replace M = M0*(1-znoco150) + M1*znoco150
replace Y = Y0M0*(1-znoco150)*(1-M) + Y1M0*(znoco150)*(1-M) + Y0M1*(1-znoco150)*(M) + Y1M1*(znoco150)*(M)	
reg Y M znoco150

ereturn list
matrix B150 = e(b)
matrix V150=e(V)
