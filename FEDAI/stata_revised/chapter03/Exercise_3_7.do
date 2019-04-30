log using ../results/chapter03/exercise_3_7, replace

clear
set seed 1234567

qui input D Y
	0 1
	0 0
	0 0
	0 4
	0 3
	1 2
	1 11
	1 14
	1 0
	1 3
end

qui gen Y_star= Y+D*(-7)

cap program drop ate
program define ate, rclass
	args Y D
    sum `Y' if `D'==1, meanonly
    local Y_treat=r(mean)
    sum `Y' if `D'==0, meanonly
    local Y_con=r(mean)
    return scalar ate_avg = `Y_treat'-`Y_con'
end

tsrtest D r(ate_avg): ate Y_star D

// ate
di r(obsvStat)       

// p.value.onesided
di r(lowertail)   


log close
translate ../results/chapter03/exercise_3_7.smcl ../results/chapter03/exercise_3_7.pdf, translator(smcl2pdf)

