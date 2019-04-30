log using ../results/chapter08/exercise_8_10, replace

clear
macro drop _all

import delim ../data/chapter08/Hough_WorkingPaper_2010, clear
set seed 1234567

/*----------------------------------------------
 part b
----------------------------------------------*/
rename tetris Y
rename run Z

// set time variable
qui tsset day
qui gen Zlag = l.Z
qui gen Ylag = l.Y


/*----fit1----*/
qui reg Y Z
global fit1 = _b[Z]

/*----fit2----*/
qui reg Y Z Zlag
global fit2=e(F)

/*----fit3----*/
qui reg Ylag Z
global fit3=_b[Z]

/*----fit4----*/
qui reg energy Z
global fit4=_b[Z]

/*----fit5----*/
qui reg gre Z
global fit5=_b[Z]



cap program drop coeff_Z
program define coeff_Z, rclass
	args Y
	tempvar rannum Zri
	gen `rannum' = uniform()
	egen `Zri' = cut(`rannum'), group(2)
	qui reg `Y' `Zri'
	return scalar coef_Z = _b[`Zri']
end

cap program drop F_stat
program define F_stat, rclass
	args Y day
	tsset `day'
	tempvar rannum Zri
	gen `rannum' = uniform()
	egen `Zri' = cut(`rannum'), group(2)
	qui reg `Y' `Zri' l.`Zri'
	return scalar fstat = e(F)
end



/*---- dist1 ----*/
preserve	
simulate  coef_Z=r(coef_Z), reps(1000) nodots: coeff_Z Y	
qui count if coef_Z >= $fit1

// one-tailed p-value: does running increase Tetris scores
// mean(dist1 >= fit1) 
disp r(N)/_N
restore



/*---- dist2 ----*/

preserve	
simulate  fstat=r(fstat), reps(1000) nodots: F_stat Y day
qui count if fstat >= $fit2

// one-tailed p-value: does running increase Tetris scores
/* mean(dist2 >= fit2) */
disp r(N)/_N
restore



/*---- dist3 ----*/
preserve	
simulate  coef_Z=r(coef_Z), reps(1000) nodots: coeff_Z Ylag
qui count if abs(coef_Z) >= abs($fit3)

// two-tailed p-value: placebo fit
// mean(dist3 >= fit3) 
disp r(N)/_N
restore



/*---- dist4 ----*/

preserve	
simulate  coef_Z=r(coef_Z), reps(1000) nodots: coeff_Z energy 
qui count if coef_Z >= $fit4

// one-tailed p-value: does running improve energy
// mean(dist4 >= fit4)
disp r(N)/_N
restore


/*---- dist5 ----*/

preserve	
simulate  coef_Z=r(coef_Z), reps(1000) nodots: coeff_Z gre 
qui count if coef_Z >= $fit5

// one-tailed p-value: does running improve GRE
// mean(dist5 >= fit5) 
disp r(N)/_N
restore


log close
translate ../results/chapter08/exercise_8_10.smcl ../results/chapter08/exercise_8_10.pdf, translator(smcl2pdf)



