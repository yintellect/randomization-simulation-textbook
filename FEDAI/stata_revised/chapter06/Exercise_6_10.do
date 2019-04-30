log using ../results/chapter06/exercise_6_10, replace


import delim ../data/chapter06/Hyde_POP_2012, clear
/*----------------------------------------------
 part d
----------------------------------------------*/
	
	rename sample Z
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
	ritest Z _b[Z], reps(10000) nodots: ///
	regress Y Z
	
	di %8.4f el(r(p), 1,1)


log close
translate ../results/chapter06/exercise_6_10.smcl ../results/chapter06/exercise_6_10.pdf, translator(smcl2pdf)
