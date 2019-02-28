clear
use "GerberGreenBook_Chapter8_Table_8_4_8_5.dta"

*part a
	mean y01 if prox500=="0"
	scalar mean_y01 = _b[y01]
	mean y00 if prox500=="0"
	scalar mean_y00 = _b[y00]
	scalar true_ate = mean_y01-mean_y00
	disp true_ate
	
	mean y if prox500=="0" & assignment =="1"
	scalar mean_y = _b[y]
	mean y00 if prox500=="0" & assignment=="0"
	scalar mean_y00_0 = _b[y00]
	scalar ate_hat = mean_y - mean_y00_0
	disp ate_hat
	
*part b
	mean y01 if prox500=="1"
	scalar mean_y01 = _b[y01]
	mean y00 if prox500=="1"
	scalar mean_y00 = _b[y00]
	mean y10 if prox500=="1"
	scalar mean_y10 = _b[y10]
	mean y11 if prox500=="1"
	scalar mean_y11 = _b[y11]
	
	scalar true_ate_01 = mean_y01 - mean_y00
	scalar true_ate_10 = mean_y10 - mean_y00
	scalar true_ate_11 = mean_y11 - mean_y00

	disp  true_ate_01
	disp  true_ate_10
	disp  true_ate_11
	
	destring(prob11), replace
	destring(prob10), replace
	
	gen q =.
	replace q=prob10 if exposure=="10"
	replace q=prob11 if exposure=="11"
	replace q=prob01 if exposure=="1"
	replace q=prob00 if exposure=="0"
	
	gen weights = 1/q
	
	destring(prox500), replace
	gen exposure_n = real(exposure)
	
	regress y exposure_n if prox500>0 & (exposure=="0" | exposure=="1") [aweight=weights]
	
	regress y exposure_n if prox500>0 & (exposure=="0" | exposure=="10") [weight=weights]
	
	regress y exposure_n if prox500>0 & (exposure=="0" | exposure=="11") [weight=weights]
	
	//coef on final regressio is wrong
	
*part c
	clear
	
	gen q=.
	replace q=prob10 if exposure==10
	replace q=prob00 if exposure==0
	gen weights = 1/q
	
	regress y exposure if (prob10>0) & (prob10<1) [weight=weights]
	
	//exposure coeff is 1/10th of correct value
