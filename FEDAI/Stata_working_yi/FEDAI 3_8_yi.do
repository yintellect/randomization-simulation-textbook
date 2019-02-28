clear all
use "Titiunik data for Exercises to Chapter 3.dta"

*part a
	regress bills_introduced i.term2year##i.texas0_arkansas1
	margins i.term2year#i.texas0_arkansas1
	scalar ate_texas = el(r(b),1,3) - el(r(b),1,1)
	scalar ate_ark = el(r(b),1,4) - el(r(b),1,2)
	disp ate_texas ate_ark
	
	
*part b 
	summarize  bills_introduced if term2year==0 & texas0_arkansas1 == 0, detail
	scalar var_texas0 = r(Var)
	scalar n_texas0 = r(N)
	summarize  bills_introduced if term2year==1 & texas0_arkansas1 == 0, detail
	scalar var_texas1 = r(Var)
	scalar n_texas1 = r(N)
	scalar se_texas = sqrt(var_texas0/n_texas0 + var_texas1/n_texas1)
	
	
	
	summarize  bills_introduced if term2year==0 & texas0_arkansas1 == 1, detail
	scalar var_arkansas0 = r(Var)
	scalar n_arkansas0 = r(N)
	summarize  bills_introduced if term2year==1 & texas0_arkansas1 == 1, detail
	scalar var_arkansas1 = r(Var)
	scalar n_arkansas1 = r(N)
	scalar se_arkansas = sqrt(var_arkansas0/n_arkansas0 + var_arkansas1/n_arkansas1)
	
	
	disp se_texas
	disp se_arkansas
	

*part c
	scalar total_length = _N
	count if (texas0_arkansas1==0)
	scalar texas_length = r(N)
	count if (texas0_arkansas1==1)
	scalar ark_length = r(N)
	disp ate_texas*(texas_length/total_length)+ate_ark*(ark_length/total_length)
	
	// same as
	teffects nnmatch (bills_introduced) (term2year), ematch(texas0_arkansas1)
	
	
*part e

	disp sqrt((texas_length/total_length)^2 * se_texas^2 + ///
	(ark_length/total_length)^2 * se_arkansas^2)
	
*part f

	ritest term2year el(r(table),1,1),strata(texas0_arkansas) reps(1000): ///
	teffects nnmatch (bills_introduced) (term2year), ematch(texas0_arkansas1)
	
	
	
