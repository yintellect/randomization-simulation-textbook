
// download data from: http://hdl.handle.net/10079/zcrjds4
// copy and paste the url to your web browser


clear
use "Hough_WorkingPaper_2010.dta"

/*----------------------------------------------
 part b
----------------------------------------------*/

/*----fit1----*/
quietly: reg tetris run
global fit1 = _b[run]

capture program drop zri1
program define zri1, rclass
	use "Hough_WorkingPaper_2010.dta", clear
	tsset day
	randtreat, generate(treatment) replace unequal(12/26 14/26)	
    regress tetris l.treatment
    return scalar coef = _b[l.treatment]
    exit	
end

simulate  coef=r(coef), reps(10000): zri1

count if coef >= $fit1
// one-tailed p-value:
disp r(N)/10000


/*----fit2----*/

clear
use "Hough_WorkingPaper_2010.dta"
tsset day
quietly: reg tetris run l.run
global fit2=e(F)


capture program drop zri2
program define zri2, rclass
	use "Hough_WorkingPaper_2010.dta", clear
	tsset day
	randtreat, generate(treatment) replace unequal(12/26 14/26)	
    regress tetris treatment l.treatment
    return scalar coef = e(F)
    exit	
end

simulate  coef=r(coef), reps(10000): zri2

count if coef >= $fit2 
// one-tailed p-value:
disp r(N)/10000


/*----fit3----*/

clear
use "Hough_WorkingPaper_2010.dta"
tsset day
quietly: reg l.tetris run
global fit3 = _b[run]


capture program drop zri3
program define zri3, rclass
	use "Hough_WorkingPaper_2010.dta", clear
	tsset day
	randtreat, generate(treatment) replace unequal(12/26 14/26)	
    regress l.tetris  l.treatment
    return scalar coef = _b[l.treatment]
    exit	
end

simulate  coef=r(coef), reps(10000): zri3

count if abs(coef) >=abs($fit3)
// two-tailed p-value: placebo fit
disp r(N)/10000


/*----fit4----*/

clear
use "Hough_WorkingPaper_2010.dta"
tsset day
quietly: reg energy run
global fit4 =_b[run]


capture program drop zri4
program define zri4, rclass
	use "Hough_WorkingPaper_2010.dta", clear
	tsset day
	randtreat, generate(treatment) replace unequal(12/26 14/26)	
    regress energy l.treatment
    return scalar coef = _b[l.treatment]
    exit	
end

simulate  coef=r(coef), reps(10000): zri4

count if coef >=$fit4
// one-tailed p-value:
disp r(N)/10000



/*----fit5----*/

clear
use "Hough_WorkingPaper_2010.dta"
tsset day
quietly: reg gre run
global fit5 = _b[run]


capture program drop zri5
program define zri5, rclass
	use "Hough_WorkingPaper_2010.dta", clear
	tsset day
	randtreat, generate(treatment) replace unequal(12/26 14/26)	
    regress gre l.treatment
    return scalar coef = _b[l.treatment]
    exit	
end

simulate  coef=r(coef), reps(10000): zri5

count if coef >= $fit5
// one-tailed p-value:
disp r(N)/10000







