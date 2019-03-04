clear
cd "/Users/yy2633/MA/2019Spring/RAship/FEDAI/FEDAI code STATA"
set seed 1234567
/*----------------------------------------------
 part a
----------------------------------------------*/

use "GerberGreenBook_Chapter4_Exercises_4-5.dta"
forvalues i = 1/50 { //create and save 50 permutations of treatment
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

tempfile permutations
save `permutations'

use "GerberGreenBook_Chapter4_Exercises_4-5.dta"
//using this file
ritest treatment _b[treatment], r(500) ///
samplingsourcefile("`permutations'") samplingmatchvar(schoolid classid): ///
reg testscore treatment age, cluster(classid)



forvalues i = 1/50 { //check F stats
		qui reg Z`i' x
		di "F`i'=  " e(F)	
}



forvalues i = 1/100 { //create and save 100 permutations of treatment
        tempvar rannum teststat
		gen `teststat' = -1
        gen `rannum'=uniform()
		egen m`i' = cut(`rannum'), group(2)
		reg 
        }


quietly forvalues i = 1/100 {
replace rannum = uniform()
egen m`i' = cut(rannum), group(2)
}




capture program drop rand
program rand
	version 13
	drop _all
	set obs 40
	tempvar rannum
	gen `rannum'=uniform()
	egen m = cut(`rannum'), group(2)
	return m
end

simulate m=m, reps(100): rand
