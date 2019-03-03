clear
use "Rind_Bordia_JASP_1996.dta"

/*----------------------------------------------
 part a
----------------------------------------------*/

program define var_difference, rclass
	sum tip if happyface==1, detail
	local var_treat = r(Var)
	sum tip if happyface==0, detail
	local var_control = r(Var)
	return scalar vardiff= `var_treat'-`var_control'
end

tsrtest happyface r(vardiff): var_difference	

//  p-value for var(Y1)>Var(Y0)
di r(uppertail) 

//  p-value for var(Y1)<>Var(Y0)
di r(twotail) 


/*----------------------------------------------
 part c
----------------------------------------------*/

	gen zfemale = happyface*female
	
	
	regress tip happyface female zfemale
	regress tip happyface female
	
		
	scalar coeff_z = _b[happyface]
	
	gen y0 = tip - coeff_z * happyface
	gen y1 = tip + coeff_z*(1-happyface)
	
	save "9_6_simdata.dta", replace
	

	
// 
	use "9_6_simdata.dta", clear
	quietly: regress tip happyface female zfemale
	test zfemale	
	disp r(F)

	capture program drop waldtest_sim
	program define waldtest_sim, rclass
	use "9_6_simdata.dta", clear
	randtreat, generate(z_sim) replace unequal(44/89 45/89)
	capture drop y_sim
	gen y_sim = y1 * z_sim + y0 * (1 - z_sim)
    quietly: regress y_sim z_sim female zfemale
	test zfemale
	return scalar f_sim = r(F)  
    exit	
	end

simulate  f_sims=r(f_sim), reps(100000): waldtest_sim

count if f_sims > = 3.9944127
// one-tailed p-value:
disp r(N)/100000

	
	use "9_6_simdata.dta", clear
	randtreat, generate(z_sim) replace unequal(44/89 45/89)
	capture drop y_sim
	gen y_sim = y1 * z_sim + y0 * (1 - z_sim)
    quietly: regress y_sim z_sim female zfemale
	test zfemale
	
	
	
	
	
	
	
	
	
	
	
capture program drop waldtest
program define waldtest, rclass 
	version 14.1
	args y0 y1 zsim female zfemale
	capture drop z ysim
	tempvar ysim
	generate `ysim' = `y0'*(1-`zsim')+`y1'*`zsim'
	regress `ysim' `zsim' `female' `zfemale'
	return scalar Fmodel = e(F)
	test `zfemale'	
	return scalar F = r(F)
	exit
end

	
	bootstrap F=r(F), reps(100) saving(boot): waldtest y0 y1 z fe zfe
	permute z  F=r(F), reps(100) seed(1789) right : waldtest y0 y1 z fe zfe
	
	ritest z F=r(F), reps(100) seed(345) saving(f) right: waldtest y0 y1 z fe zfe
	
	
