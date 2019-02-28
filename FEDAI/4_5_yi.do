clear
use "GerberGreenBook_Chapter4_Exercises_4-5.dta"

capture program drop randfun

program define randfun
	use "GerberGreenBook_Chapter4_Exercises_4-5.dta", clear
	capture scalar drop teststat
	local teststat = -1
	randtreat, generate(treatment) replace 
    regress treatment x
	replace `teststat' = el(r(table), 4, 1)
    while `teststat' < 0.05{ drop treatment}	
	return treatment
    exit	
end

simulate  t=treatment, reps(100) saving(treat.dta, replace): randfun
randomize, groups(2) balance(x) jointp(0.05)
