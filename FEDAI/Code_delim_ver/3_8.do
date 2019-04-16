// download data from : http://hdl.handle.net/10079/s1rn910
// copy and paste the url to your web browser

// use "Titiunik_WorkingPaper_2010.csv.dta",clear 
import delim Titiunik_WorkingPaper_2010, clear
set seed 1234567
/*----------------------------------------------
 Part a
----------------------------------------------*/
	rename term2year D
	rename bills_introduced Y
	rename texas0_arkansas1 block
	
	qui tabstat Y if block ==0, by(D) stat(mean) save	
	scalar ate_texas = el(r(Stat2),1,1) - el(r(Stat1),1,1)

	
	qui tabstat Y if block ==1, by(D) stat(mean) save	
	scalar ate_ark = el(r(Stat2),1,1) - el(r(Stat1),1,1)
	
	di "ate_texas="%18.5f ate_texas 
	di "ate_arkansas="%18.5f ate_ark 	
	
/*----------------------------------------------
 Part b
----------------------------------------------*/

	qui tabstat Y if block ==0, by(D) stat(v n) save	
	
	scalar se_texas = sqrt(el(r(Stat2),1,1)/el(r(Stat2),2,1) + /// 
						el(r(Stat1),1,1)/el(r(Stat1),2,1))
						
	
	qui tabstat Y if block ==1, by(D) stat(v n) save	
	
	scalar se_arkansas = sqrt(el(r(Stat2),1,1)/el(r(Stat2),2,1) + /// 
						el(r(Stat1),1,1)/el(r(Stat1),2,1))
	
	
	di "se_texas="%18.6f se_texas
	di "se_arkansas="%18.6f se_arkansas

	
	

/*----------------------------------------------
 Part c
----------------------------------------------*/
	qui tabstat Y, by(block) stat(n) save	
	
	scalar ate_overall = el(r(Stat1),1,1)/_N*ate_texas + /// 
						 el(r(Stat2),1,1)/_N*ate_ark
	
	
	di %18.4f ate_overall
	

	// same as
	// teffects nnmatch (bills_introduced) (term2year), ematch(texas0_arkansas1)
	
	
/*----------------------------------------------
 Part e
----------------------------------------------*/

	scalar se_overall = sqrt((el(r(Stat1),1,1)/_N)^2*se_texas^2 + /// 
						 (el(r(Stat2),1,1)/_N)^2*se_arkansas^2)
						 
	di %18.5f se_overall

	
/*----------------------------------------------
 Part f
----------------------------------------------*/
	// calculate probs under block assignment
	bysort block: egen probs=mean(D)
	
	
	
	cap program drop ate_block
	
	program define ate_block, rclass
	args Y D probs
	tempvar ipw
	gen `ipw' = .
	// calculate inverse probability weight under block assignment
	replace `ipw' = `D'/`probs' + (1-`D')/(1-`probs')
	qui reg `Y' `D' [iw=`ipw']
	return scalar ate=_b[`D']
	end 
	
	// ssc install ritest (to install ritest package)
	
	//
	ritest D r(ate), strata(block) reps(10000): ///
	ate_block Y D probs
	
	
	// ate
	di el(r(b),1,1)

	// p.value.twosided
	di el(r(p),1,1)
	
	
	
