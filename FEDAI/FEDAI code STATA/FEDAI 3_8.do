clear
use "Titiunik_WorkingPaper_2010.dta"

*part a
	regress bills_introduced i.term2year##i.texas0_arkansas1
	margins i.term2year#i.texas0_arkansas1
	scalar ate_texas = el(r(b),1,3) - el(r(b),1,129
	scalar ate_ark = el(r(b),1,4) - el(r(b),1,2)
	disp ate_texas ate_ark
	
*part b
	regress bills_introduced i.term2year if texas0_arkansas1==0
	scalar texas_se =_se[1.term2year]
	regress bills_introduced i.term2year if texas0_arkansas1==1
	scalar ark_se =_se[1.term2year]

*part c
	scalar total_length = r(N)
	count if (texas0_arkansas1==0)
	scalar texas_length = r(N)
	count if (texas0_arkansas1==1)
	scalar ark_length = r(N)
	disp ate_texas*(texas_length/total_length) + ate_ark * (ark_length/total_length)
	
*part e
	disp sqrt((texas_length/total_length)^2 * texas_se^2 + (ark_length/total_length)^2 * ark_se^2)
	
*part f
	ritest term2year _b[term2year], strata(texas0_arkansas) reps(10000): regress bills_introduced term2year
