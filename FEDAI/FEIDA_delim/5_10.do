// download data from: http://hdl.handle.net/10079/zkh18mj
// copy and paste the url to your web browser


clear
//use "Guan_Green_CPS_2006.dta.dta"
import delim Guan_Green_CPS_2006, clear
/*----------------------------------------------
 part a
----------------------------------------------*/
	// na.omit
	drop if turnout == .
	drop if contact == .
	drop if dormid == .
	drop if treat2 == . 
	
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


	ritest Z _b[Z], cluster(clust) reps(1000): ///
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
