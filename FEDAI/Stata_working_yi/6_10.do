
// download data from: http://hdl.handle.net/10079/p8cz96p
// copy and paste the url to your web browser


clear
use "Hyde_POP_2012.dta.dta"

/*----------------------------------------------
 part d
----------------------------------------------*/
	
	rename Sample Z
	rename invalidballots Y
	rename observed D
	
	qui regress Y Z
	scalar ITT = _b[Z]		
	qui regress D Z
	scalar ITTD = _b[Z]		
	scalar CACE = ITT/ITTD
	disp %8.3f ITT		
	disp %18.2f CACE

/*----------------------------------------------
 part e
----------------------------------------------*/
	ritest Z _b[Z], reps(10000): ///
	regress Y Z
	
	di %8.4f el(r(p), 1,1)
