clear
clear matrix
clear mata
set matsize 11000 
set maxvar 32767
set seed 1234567


cd "/Users/yy2633/MA/2019Spring/RAship/FEDAI/FEDAI code STATA"

/*----------------------------------------------
 part a
----------------------------------------------*/
  
/* loop to simulte random assignment and save to a matrix */

cap matrix drop z
matrix z=J(40, 10000, .)

qui forvalues i = 1/10000 { //create and save 50 permutations of treatment
	use "GerberGreenBook_Chapter4_Exercises_4-5.dta"
	tempvar teststat Z
	gen `Z' = .
	gen `teststat' = -1
	while `teststat' < 0.05{
		tempvar rannum Zri t
	    gen `rannum'=uniform()
		egen `Zri' = cut(`rannum'), group(2)
		qui reg `Zri' x
		gen `t' = _b[x]/_se[x]		
		replace `teststat' = 2*ttail(e(df_r),abs(`t'))	
	}
	replace `Z' = `Zri'
	forvalues j = 1/40 {
	matrix z[`j', `i'] = `Z'[`j']
	}
	drop _all
}


use "GerberGreenBook_Chapter4_Exercises_4-5.dta",clear

matrix rowm = z * J(colsof(z), 1, 1/colsof(z))
matrix colnames rowm=probs
svmat double rowm, names(col)

gen weights = (1/probs)*D +(1/(1-probs))*(1-D)

tabstat weights, by(D) stat(v)


/*----------------------------------------------
 part b
----------------------------------------------*/

qui reg Y D [pw=weights]
global ate_restricted_RA = _b[D]

di "ATE (Restricted Assignment)= " $ate_restricted_RA

svmat z

cap matrix drop y_dis
matrix y_dis=J(10000, 1, .)


forvalues i = 1/10000 {

		qui reg Y z`i' [pw=weights]
		
		matrix y_dis[`i', 1] = _b[z`i']

}


preserve
svmat y_dis
sort y_dis1
count if abs(y_dis1) > abs($ate_restricted_RA)
di r(N)/_N
restore 

/*----------------------------------------------
 part c
----------------------------------------------*/

qui reg Y x D [pw=weights]
global ate_cov_restricted_RA = _b[D]
di "ATE Controlling Covariance (Restricted Assignment)= " $ate_cov_restricted_RA 



cap matrix drop cov_dis
matrix cov_dis=J(10000, 1, .)

forvalues i = 1/10000 {
		qui reg Y x z`i' [iw=weights]		
		matrix cov_dis[`i', 1] = _b[z`i']

}


preserve
svmat cov_dis
sort cov_dis1
count if abs(cov_dis1) > abs($ate_cov_restricted_RA )
di r(N)/_N
restore 

/*----------------------------------------------
 check the restricted randomization
----------------------------------------------*/

/*
cap matrix drop p_dis
matrix p_dis=J(1000, 1, .)

forvalues i = 1/1000 {
		tempvar t p
		qui reg z`i' x
		scalar `t' = _b[x]/_se[x]
		scalar `p'= 2*ttail(e(df_r),abs(`t'))	
		matrix p_dis[`i', 1] = `p'

}


preserve
svmat p_dis
sort p_dis1
hist p_dis1
restore 
*/

/*----------------------------------------------
 part d
----------------------------------------------*/


/*----------------------se_complete_RA------------------------*/

// calculate ate_complete_RA
qui reg Y D
global ate_complete_RA = _b[D]
di "ATE under Complete Assignment = " $ate_complete_RA


// RI under the null ate=ate_complete_RA

cap drop Y0_sim Y1_sim Y_sim
gen Y0_sim = Y
gen Y1_sim = Y
gen Y_sim = .
replace Y0_sim = Y - $ate_complete_RA if D==1
replace Y1_sim = Y + $ate_complete_RA if D==0

capture program drop ate_complete_RA_ri
program define ate_complete_RA_ri, rclass
	replace Y_sim = Y0_sim*(1-D) + Y1_sim*(D) 
	regress Y_sim D 
    return scalar Ys_complete_RA=_b[D]	
end

tsrtest D r(Ys_complete_RA) using distout_complete_RA.dta, overwrite: ///
 ate_complete_RA_ri

// calculate se_complete_RA
preserve
use "distout_complete_RA.dta", clear
drop if _n==1
tabstat theta, stat(sd N)
restore 

/*--------------------se_cov_complete_RA--------------------------*/

// calculate ate_cov_complete_RA
qui reg Y D x
global ate_cov_complete_RA = _b[D]
di "ATE Controlling Covariance under Complete Assignment= " $ate_cov_complete_RA


// RI under the null ate= ate_cov_complete_RA

cap drop Y0_sim Y1_sim Y_sim
gen Y0_sim = Y
gen Y1_sim = Y
gen Y_sim = .
replace Y0_sim = Y - $ate_cov_complete_RA if D==1
replace Y1_sim = Y + $ate_cov_complete_RA if D==0

capture program drop ate_cov_complete_RA_ri
program define ate_cov_complete_RA_ri, rclass
	replace Y_sim = Y0_sim*(1-D) + Y1_sim*(D) 
	regress Y_sim D x
    return scalar Ys_cov_complete_RA=_b[D]	
end

tsrtest D r(Ys_cov_complete_RA) using distout_cov_complete_RA.dta, overwrite: ///
 ate_cov_complete_RA_ri

// calculate se_cov_complete_RA
preserve
use "distout_cov_complete_RA.dta", clear
drop if _n==1
tabstat theta, stat(sd N)
restore 


/*--------------------se_restricted_RA--------------------------*/


// calculate ate_restricted_RA
di "ATE under Restricted Assignment= " $ate_restricted_RA


// RI under the null ate= ate_restricted_RA

cap drop Y0_sim Y1_sim Y_sim
gen Y0_sim = Y
gen Y1_sim = Y
gen Y_sim = .
replace Y0_sim = Y - $ate_restricted_RA if D==1
replace Y1_sim = Y + $ate_restricted_RA if D==0


cap matrix drop distout_restricted_RA
matrix distout_restricted_RA=J(10000, 1, .)


qui forvalues i = 1/10000 {
		replace Y_sim = Y0_sim*(1-z`i') + Y1_sim*(z`i')
		qui reg Y_sim z`i' [pw=weights]
		
		matrix distout_restricted_RA[`i', 1] = _b[z`i']

}


/*se_restricted_RA*/
preserve
svmat distout_restricted_RA
tabstat distout_restricted_RA, stat(sd N)
restore 


/*---------------------se_cov_restricted_RA-------------------------*/


// calculate ate_restricted_RA
di "ATE Controlling Covariance(Restricted Assignment)= " $ate_cov_restricted_RA

// RI under the null ate= ate_cov_restricted_RA

cap drop Y0_sim Y1_sim Y_sim
gen Y0_sim = Y
gen Y1_sim = Y
gen Y_sim = .
replace Y0_sim = Y - $ate_cov_restricted_RA if D==1
replace Y1_sim = Y + $ate_cov_restricted_RA if D==0


cap matrix drop distout_cov_restricted_RA 
matrix distout_cov_restricted_RA=J(10000, 1, .)


qui forvalues i = 1/10000 {
		replace Y_sim = Y0_sim*(1-z`i') + Y1_sim*(z`i')
		qui reg Y_sim x z`i' [pw=weights]
		
		matrix distout_cov_restricted_RA[`i', 1] = _b[z`i']

}


/*se_restricted_RA*/
preserve
svmat distout_cov_restricted_RA 
tabstat distout_cov_restricted_RA, stat(sd N)
restore 
