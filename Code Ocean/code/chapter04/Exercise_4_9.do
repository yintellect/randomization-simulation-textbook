log using ../results/chapter04/exercise_4_9, replace

clear

/*----------------------------------------------
 part c
----------------------------------------------*/
	input ests shareoftotalN
	.00964 .049487
	-.007829 .1520981
	-.01362 .626616
	.008271 .171799
	end
	
	qui gen overall_ate = ests*shareoftotalN
	total(overall_ate)

/*----------------------------------------------
 part e
----------------------------------------------*/
	clear
	
	qui import delim ../data/chapter04/Gerber_Green_AAAPSS_2005, clear
	qui bysort strata: egen blockpr = mean(treat2)
	qui gen q = blockpr*treat2 + (1-blockpr)*(1-treat2)
	regress vote02 treat2 [aw=1/q]

log close
translate ../results/chapter04/exercise_4_9.smcl ../results/chapter04/exercise_4_9.pdf, translator(smcl2pdf)
