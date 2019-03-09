clear
use "Middleton_Rogers_AI_2010"

*part a
	decode treatment, gen(treatment_string)
	mean relevant_measures_net if treatment_string=="yes"
	scalar avg_treat = _b[relevant_measures_net]
	mean relevant_measures_net if treatment_string=="no"
	scalar avg_control = _b[relevant_measures_net]
	global tau = avg_treat - avg_control
	disp "average treatment effect = " $tau

	
	twoway (scatter relevant_measures_net treatment), ///
	xlabel(0"no" 1 "yes") xmtick(-0.5(1)1.5,grid) 
	
	

*part c
	ritest treatment ate_sim = _b[treatment], ///
	reps(10000) sav(distout.dta, replace) right: ///
	regress relevant_measures_net treatment
	
preserve
//historgam
use "distout.dta", clear
graph twoway (histogram ate_sim,frequency bin(1000)) ///
(scatteri 0 $tau 40 $tau, c(l) lc(red) lw(thick) lp(dash) m(i)), legend(off) ///
b1title("Estimated ATE") title("Distribution of the Estimated ATE") ///
xtitle("") 
restore

*part d
