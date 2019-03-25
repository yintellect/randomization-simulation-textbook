
// download data from: http://hdl.handle.net/10079/vmcvdzs
// copy and paste the url to your web browser


clear
use "Rind_Bordia_JASP_1996.dta"

/*----------------------------------------------
 part a
----------------------------------------------*/
gen Z =.
replace Z = 1 if happyface==1
replace Z = 0 if happyface==0

rename tip Y

capture program drop var_difference
program define var_difference, rclass
	sum Y if Z==1, detail
	local var_treat = r(Var)
	sum Y if Z==0, detail
	local var_control = r(Var)
	return scalar vardiff= `var_treat'-`var_control'
end

//(uncomment to search package and install)
// findit tsrtest

tsrtest Z r(vardiff): var_difference	

//  p-value for var(Y1)>Var(Y0)
di %8.4f r(uppertail) 

//  p-value for var(Y1)<>Var(Y0)
di %8.3f r(twotail) 


/*----------------------------------------------
 part c
----------------------------------------------*/

rename female female_factor
gen female = .
replace female = 1 if female_factor==1
replace female = 0 if female_factor==0

gen zfemale = Z*female


//lmmodelint: regression with interaction between happyface and waitstaff sex
regress Y Z female zfemale

//lmmodel: regression model without interaction
regress Y Z female


/* confirm p-value with RI */

// use estimated coefficients from base model to impute potential outcomes	
scalar coeff_z = _b[Z]
cap drop Y0 Y1
gen Y0 = Y - coeff_z * Z
gen Y1 = Y + coeff_z*(1- Z)


qui regress Y Z female zfemale
qui test zfemale		
global f_obs = r(F)


capture program drop wald_f
program define wald_f, rclass
	tempvar Y_sim zsimfemale
	gen `Y_sim' = Y1 * Z + Y0 * (1 - Z)
	gen  `zsimfemale' = female*Z
	qui reg `Y_sim' Z female `zsimfemale'
	test `zsimfemale'
	return scalar f_sims = r(F)
end



tsrtest Z r(f_sims) using 9_6_fsims.dta, overwrite: wald_f	

/* calculate the p-value by comparing the observed F-statistic
 to the F-statistic under the null of constant & additive effects */
di %8.4f r(uppertail)

