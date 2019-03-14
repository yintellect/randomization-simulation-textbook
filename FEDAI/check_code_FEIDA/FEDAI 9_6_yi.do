
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
qui regress Y Z female


/* confirm p-value with RI */

// use estimated coefficients from base model to impute potential outcomes	
scalar coeff_z = _b[Z]
gen Y0 = Y - coeff_z * Z
gen Y1 = Y + coeff_z*(1- Z)


qui regress Y Z female zfemale
qui test zfemale		
global f_obs = r(F)


gen Y_sim=.

capture program drop wald_f
program define wald_f, rclass
	replace Y_sim = Y1 * happyface + Y0 * (1 - happyface)
	quietly: regress Y_sim Z female zfemale
	test zfemale
	return scalar f_sims = r(F)
end

tsrtest happyface r(f_sims) using f_sims.dta, overwrite: wald_f	

/* calculate the p-value by comparing the observed F-statistic
 to the F-statistic under the null of constant & additive effects */
di %8.4f r(uppertail)	
	
