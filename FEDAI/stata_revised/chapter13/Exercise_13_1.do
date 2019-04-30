
log using ../results/chapter13/exercise_13_1, replace

import delim ../data/chapter13/Middleton_Rogers_AI_2010, clear

/*----------------------------------------------
 part a
----------------------------------------------*/
	rename relevant_measures_net Y
	gen int Z=.
	replace Z = 1 if treatment ==1
	replace Z = 0 if treatment ==0

	qui mean Y if Z==1
	scalar avg_treat = _b[Y]
	qui mean Y if Z==0
	scalar avg_control = _b[Y]
	global tau = avg_treat - avg_control
	
	disp "average treatment effect = " $tau

	
	twoway (scatter Y Z), ///
	xlabel(0"no" 1 "yes") xmtick(-0.5(1)1.5,grid) xtitle("Treatment")
	
graph export ../results/chapter13/exercise_13_1_a_graph.pdf	

/*----------------------------------------------
 part c
----------------------------------------------*/
ritest Z ate_sim = _b[Z], ///
	reps(1000) sav(13_1_distout.dta, replace) right nodots: ///
	regress Y Z

// one-tail p-value
di %8.4f el(r(p), 1, 1)
	
preserve
use "13_1_distout", clear
//historgam

graph twoway (histogram ate_sim,frequency bin(100)) ///
(scatteri 0 $tau 30 $tau, c(l) lc(red) lw(thick) lp(dash) m(i)), legend(off) ///
b1title("Estimated ATE") title("Distribution of the Estimated ATE") ///
xtitle("") 

graph export ../results/chapter13/exercise_13_1_c_graph.pdf	

restore

/*----------------------------------------------
 part d
----------------------------------------------*/


clear
clear matrix
clear mata
set matsize 11000 
set maxvar 32767
set seed 67887975


cap matrix drop z
matrix z=J(65, 1000, .)


// restircted RA loop
qui forvalues i = 1/1000 {
	import delim ../data/chapter13/Middleton_Rogers_AI_2010, clear	
	tempvar teststat Z
	gen `Z' = .
	gen `teststat' = 5
	while (abs(`teststat')>=0.5){
		tempvar rannum ordering Zri 
	    gen `rannum'=uniform()
		egen `ordering' = rank(`rannum')
		gen `Zri' = 1 if `ordering' <= 48
		replace `Zri' = 0 if `ordering' > 48
				
		qui reg dem_perf_06 `Zri'		
		replace `teststat' = _b[`Zri']
	}
	replace `Z' = `Zri'
	forvalues j = 1/65 {
	matrix z[`j', `i'] = `Z'[`j']
	}
	drop _all
}




import delim ../data/chapter13/Middleton_Rogers_AI_2010, clear
rename relevant_measures_net Y
gen int Z=.
replace Z = 1 if treatment ==1
replace Z = 0 if treatment ==0



matrix rowm = z * J(colsof(z), 1, 1/colsof(z))
matrix colnames rowm=probs
svmat double rowm, names(col)

// distribution of probabilities
tabstat probs, stat(min p25 med mean p75 max)


svmat z

// save assignment permutation for later inspection
//save "13_1_ra.dta",replace



cap matrix drop tau_dis
matrix tau_dis=J(1000, 1, .)


// calculate estimate distribution
forvalues i = 1/1000{
	tempvar weight`i'
	gen `weight`i'' = z`i'/probs + (1 - z`i')/(1 - probs)
	qui reg Y z`i' [pw=`weight`i'']
	matrix tau_dis[`i', 1] = _b[z`i']	

}


preserve
svmat tau_dis
qui count if tau_dis1 > $tau
// one tailed p-value
di r(N)/_N

graph twoway (histogram tau_dis1,frequency bin(100)) ///
(scatteri 0 $tau 30 $tau, c(l) lc(red) lw(thick) lp(dash) m(i)), legend(off) ///
b1title("Estimated ATE") title("Distribution of the Estimated ATE") ///
xtitle("") 

graph export ../results/chapter13/exercise_13_1_d_graph.pdf	

restore 

log close
translate ../results/chapter13/exercise_13_1.smcl ../results/chapter13/exercise_13_1.pdf, translator(smcl2pdf)




