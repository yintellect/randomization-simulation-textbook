clear
use "russia_subset.dta"

*part a
	recode sexresp6 ( 2 = 1) (1 = 0), gen(female)
	recode memberc6 ( 1 = 1) (else = 0), gen(church_member)

	
	reg index96 index95 i.female i.church_member i.group6
	disp e(r2)
	
	reg index96 i.female i.church_member i.group6
	disp e(r2)

*part b
	
	//ssc install randtreat
	
	randtreat, generate(treat) strata(female church_member index96 group6) misfits(wglobal)
	
	regress treat female church_member i.group6 index96
	
*part c

set seed 1234

* complete assignment
clear*

capture program drop com_ra
program define com_ra, rclass
    
	use "russia_subset.dta", clear
	capture drop complete
	randtreat, generate(complete)
	
    regress index97 complete
    return scalar coef = _b[complete]
    exit	
end

simulate  coef=r(coef), reps(10000): com_ra
sum
scalar complete_est = r(mean)
scalar complete_sd = r(sd)
disp complete_est complete_sd


* adjusted 
capture program drop adj_ra
program define adj_ra, rclass
    
	use "russia_subset.dta", clear
	capture drop complete
	randtreat, generate(complete)
	
    regress index97 complete sexresp6 memberc6 i.group6 index96
    return scalar coef = _b[complete]
    exit	
end


simulate  coef=r(coef), reps(10000): adj_ra
sum
scalar adjusted_est = r(mean)
scalar adjusted_sd = r(sd)
disp adjusted_est adjusted_sd



* blocked 
capture program drop block_ra
program define block_ra, rclass
    
	use "russia_subset.dta", clear
	capture drop block
	randtreat, generate(block) strata(sexresp6 memberc6 group6 index96) misfits(wglobal)
	
    regress index97 block
    return scalar coef = _b[block]
    exit	
end


simulate  coef=r(coef), reps(10000): block_ra
sum
scalar block_est = r(mean)
scalar block_sd = r(sd)
	
** pool results together
disp complete_est, adjusted_est, block_est
disp complete_sd, adjusted_sd, block_sd
	
	
	

