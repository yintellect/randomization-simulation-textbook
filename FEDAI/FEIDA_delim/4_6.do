// download data from: http://hdl.handle.net/10079/fttdz9m
// copy and paste the url to your web browser

clear
//use "OBrien_Patsiorkovski_ICPSR_1999.dta.dta"
import delim OBrien_Patsiorkovski_ICPSR_1999,clear

/*----------------------------------------------
 part a
----------------------------------------------*/
	recode sexresp6 ( 2 = 1) (1 = 0), gen(female)
	recode memberc6 ( 1 = 1) (else = 0), gen(church_member)

	// fit1
	qui reg index96 index95 i.female i.church_member i.group6
	disp %8.4f e(r2)
	
	// fit2
	qui reg index96 i.female i.church_member i.group6
	disp %8.5f e(r2)

/*----------------------------------------------
 part b
----------------------------------------------*/
	
	//ssc install randtreat (install the package for random assigment)
	
	randtreat, generate(treat) strata(female church_member index96 group6) misfits(wglobal)
	
	regress treat female church_member i.group6 index96
	
	// coefficient is different due to the nature of random assignment
	// but the R-squred is similar 0.00, meaning the balance hold on 
	// the covariates
	
/*----------------------------------------------
 part c
----------------------------------------------*/

set seed 1234567

* complete assignment
clear*

capture program drop com_ra
program define com_ra, rclass
    
	import delim OBrien_Patsiorkovski_ICPSR_1999,clear
	capture drop complete
	randtreat, generate(complete)
	
    regress index97 complete
    return scalar coef = _b[complete]
    exit	
end

simulate  coef=r(coef), reps(1000): com_ra
sum
scalar complete_est = r(mean)
scalar complete_sd = r(sd)

di "Average Estimate (complete)" %8.2f complete_est 
di "Standard Error (complete)"%8.2f complete_sd 


* adjusted 
capture program drop adj_ra
program define adj_ra, rclass
    
	import delim OBrien_Patsiorkovski_ICPSR_1999,clear
	capture drop complete
	randtreat, generate(complete)
	
    regress index97 complete sexresp6 memberc6 i.group6 index96
    return scalar coef = _b[complete]
    exit	
end


simulate  coef=r(coef), reps(1000): adj_ra
sum
scalar adjusted_est = r(mean)
scalar adjusted_sd = r(sd)

di "Average Estimate (adjusted)"%8.2f adjusted_est
di "Standard Error (adjusted)"%8.2f adjusted_sd


* blocked 
capture program drop block_ra
program define block_ra, rclass
    
	import delim OBrien_Patsiorkovski_ICPSR_1999,clear
	capture drop block
	randtreat, generate(block) strata(sexresp6 memberc6 group6 index96) misfits(wglobal)
	
    regress index97 block
    return scalar coef = _b[block]
    exit	
end


simulate  coef=r(coef), reps(1000): block_ra
sum
scalar block_est = r(mean)
scalar block_sd = r(sd)

di "Average Estimate (blocked)"%8.2f block_est
di "Standard Error (blocked)"%8.2f block_sd


	
** pool results together
disp %8.2f complete_est %8.2f adjusted_est %8.2f block_est
disp %8.2f complete_sd %8.2f adjusted_sd %8.2f block_sd
	
	
	

