log using ../results/chapter05/exercise_5_10, replace


import delim ../data/chapter05/Guan_Green_CPS_2006, clear
/*----------------------------------------------
 part a
----------------------------------------------*/
	// na.omit
	qui drop if turnout == .
	qui drop if contact == .
	qui drop if dormid == .
	qui drop if treat2 == . 
	
	rename treat2 Z
	rename turnout Y
	rename contact D
	rename dormid clust
	
	
	tabstat Y, by(Z) stat(mean) save
	// ITT
	di %8.7f el(r(Stat2),1 ,1) - el(r(Stat1),1 ,1) 

	
/*----------------------------------------------
 part b
----------------------------------------------*/
	preserve
	collapse Z, by(clust)   
	qui sum Z
	global p = r(mean)
	restore

	// probability of being assigned to treatment
	gen probs = $p


	ritest Z _b[Z], cluster(clust) reps(1000) nodots: ///
	regress Y Z
		
	// itt
	di %8.4f el(r(b),1,1)

	// p.value.twosided
	di %8.0f el(r(p),1,1)
	
/*----------------------------------------------
 part c
----------------------------------------------*/
	qui regress Y Z
	scalar itt = _b[Z]
	di %8.4f itt
	
	qui regress D Z
	scalar ittd = _b[Z]
	di %8.4f ittd
	
	scalar cace = itt/ittd
	di %8.4f cace

log close
translate ../results/chapter05/exercise_5_10.smcl ../results/chapter05/exercise_5_10.pdf, translator(smcl2pdf)

