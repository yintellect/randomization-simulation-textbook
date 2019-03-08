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
matrix z=J(40, 1000, .)

qui forvalues i = 1/1000 { //create and save 50 permutations of treatment
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
scalar ate = _b[D]

di "ATE= " ate

svmat z

cap matrix drop y_dis
matrix y_dis=J(1000, 1, .)



forvalues i = 1/1000 {

		qui reg Y z`i' [pw=weights]
		
		matrix y_dis[`i', 1] = _b[z`i']

}


preserve
svmat y_dis
sort y_dis1
count if abs(y_dis1) > abs(ate)
di r(N)/_N
restore 

/*----------------------------------------------
 part c
----------------------------------------------*/




qui reg Y x D [pw=weights]
scalar ate_cov = _b[D]
di ate_cov

cap matrix drop cov_dis
matrix cov_dis=J(1000, 1, .)

forvalues i = 1/1000 {
		qui reg Y x z`i' [iw=weights]		
		matrix cov_dis[`i', 1] = _b[z`i']

}


preserve
svmat cov_dis
sort cov_dis1
count if abs(cov_dis1) > abs(ate_cov)
di r(N)/_N
restore 

/*----------------------------------------------
 check the restricted randomization
----------------------------------------------*/

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

