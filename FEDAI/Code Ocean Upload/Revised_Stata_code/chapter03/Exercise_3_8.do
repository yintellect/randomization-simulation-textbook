log using ../results/chapter03/exercise_3_8, replace

qui import delim ../data/chapter03/Titiunik_WorkingPaper_2010, clear
set seed 1234567

/*----------------------------------------------
 Part a
----------------------------------------------*/
	qui rename term2year D
	qui rename bills_introduced Y
	qui rename texas0_arkansas1 block
	
	qui tabstat Y if block ==0, by(D) stat(mean) save	
	scalar ate_texas = el(r(Stat2),1,1) - el(r(Stat1),1,1)

	
	qui tabstat Y if block ==1, by(D) stat(mean) save	
	scalar ate_ark = el(r(Stat2),1,1) - el(r(Stat1),1,1)
	
	di "ate_texas ="%18.5f ate_texas
	di "ate_arkansas ="%18.5f ate_ark
	
/*----------------------------------------------
 Part b
----------------------------------------------*/

	qui tabstat Y if block ==0, by(D) stat(v n) save	
	
	scalar se_texas = sqrt(el(r(Stat2),1,1)/el(r(Stat2),2,1) + /// 
						el(r(Stat1),1,1)/el(r(Stat1),2,1))
						
	
	qui tabstat Y if block ==1, by(D) stat(v n) save	
	
	scalar se_arkansas = sqrt(el(r(Stat2),1,1)/el(r(Stat2),2,1) + /// 
						el(r(Stat1),1,1)/el(r(Stat1),2,1))
	
	
	di "se_texas ="%18.6f se_texas
	di "se_arkansas ="%18.6f se_arkansas

	
	

/*----------------------------------------------
 Part c
----------------------------------------------*/
	qui tabstat Y, by(block) stat(n) save	
	
	scalar ate_overall = el(r(Stat1),1,1)/_N*ate_texas + /// 
						 el(r(Stat2),1,1)/_N*ate_ark
	
	
	di "overall ate ="%18.4f ate_overall

/*----------------------------------------------
 Part e
----------------------------------------------*/

	scalar se_overall = sqrt((el(r(Stat1),1,1)/_N)^2*se_texas^2 + /// 
						 (el(r(Stat2),1,1)/_N)^2*se_arkansas^2)
						 
    di "overall se ="%18.5f se_overall

	
/*----------------------------------------------
 Part f
----------------------------------------------*/
    // calculate probs under block assignment
    qui bysort block: egen probs=mean(D)

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
	ritest D r(ate), strata(block) reps(10000) nodots: ///
	ate_block Y D probs
		
	// ate
	di el(r(b),1,1)

	// p.value.twosided
	di el(r(p),1,1)
	
	
log close
translate ../results/chapter03/exercise_3_8.smcl ../results/chapter03/exercise_3_8.pdf, translator(smcl2pdf)

	
