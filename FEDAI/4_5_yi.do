clear
clear matrix
clear mata
set maxvar 32000
cd "/Users/yy2633/MA/2019Spring/RAship/FEDAI/FEDAI code STATA"
set seed 1234567
/*----------------------------------------------
 part a
----------------------------------------------*/

use "GerberGreenBook_Chapter4_Exercises_4-5.dta"

matrix t=J(5000, 40, .)

forvalues i = 1/5000 { //create and save 50 permutations of treatment
	gen Z`i' = .
	tempvar teststat
	gen `teststat' = -1
	while `teststat' < 0.05{
		tempvar rannum Zri
	    gen `rannum'=uniform()
		egen `Zri' = cut(`rannum'), group(2)
		qui reg `Zri' x
		replace `teststat' = e(F)	
	}
	replace Z`i' = `Zri'
}

