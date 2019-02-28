clear
use "Rind_Bordia_JASP_1996.dta"

*part a
	rename happyface z
	rename tip y

	sum y if z==1, detail
	scalar var_treat = r(Var)
	sum y if z==0, detail
	scalar var_control = r(Var)
	scalar testvar = var_treat - var_control
	
	ritest z _b[z], reps(800) saveresampling(outs): regress y z
	use outs.dta, clear
	
	set matsize 800
	//max matsize in STATA SE is 800
	matrix vardist = J(1,800,.)
	
	forvalues i = 1/800 {
	sum z`i', detail
	matrix vardist[1,`i'] = r(Var)
	}
	
	gen n = 0
	forvalues i = 1/800 {
	replace n=n+1 if vardist[1,`i']>testvar
	}
	
	disp n/_N
	
	gen n = 0
	forvalues i = 1/800 {
	replace n=n+1 if abs(vardist[1,`i'])>abs(testvar)
	}
	
	disp n/_N
	
	//not sure issue with randomization not working 


 *part c
	gen zfemale = z*female
	regress y z female zfemale
	regress y z female
	scalar coeff_z = _b[z]
	gen y0 = y - coeff_z * z
	gen y1 = y + coeff_z*(1-z)
	
	//not sure on waldtest details
