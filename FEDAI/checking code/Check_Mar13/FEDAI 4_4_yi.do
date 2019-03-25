// download data from: http://hdl.handle.net/10079/kkwh7b6
// copy and paste the url to your web browser

clear
use "GerberGreenBook_Chapter4_Exercises_4-5.dta"
set seed 1234567

/*----------------------------------------------
 part a
----------------------------------------------*/
	gen y = y0*(1-D) + y1*D
		
	regress y D	
	summ y if D==1, meanonly
	scalar treat_mean = r(mean)
	summ y if D==0, meanonly
	scalar control_mean = r(mean)
	disp treat_mean - control_mean
	
	global tau = treat_mean - control_mean
	
	
/*----------------------------------------------
 part b
----------------------------------------------*/

	regress y x if D==1
	local co_treat = _b[x]
	regress y x if D==0
	local co_control= _b[x]
	disp `co_treat' + `co_control'
	
	gen ydiff = y - x
	regress ydiff D

/*----------------------------------------------
 part c
----------------------------------------------*/

	regress y D x
	global tau_c = _b[D]
	
/*----------------------------------------------
 part d
----------------------------------------------*/

capture program drop ate
program define ate, rclass
	regress y D
    return scalar Ys=_b[D]
end

tsrtest D r(Ys) using ate.dta, overwrite: ate

//ate
di r(obsvStat)

// p.value
di r(twotail) 

preserve
//historgam
use "ate.dta", clear
global tau =  theta[1]
drop if _n==1

graph twoway (histogram theta,frequency bin(1000)) ///
(scatteri 0 $tau 100 $tau, c(l) lc(red) lw(thick) lp(dash) m(i)), legend(off) ///
b1title("Estimated ATE") title("Q4d: Distribution of the Estimated ATE") ///
xtitle("")
restore

/*----------------------------------------------
 part e
----------------------------------------------*/



capture program drop ate_cov
program define ate_cov, rclass
	regress y D x
    return scalar Ys=_b[D]
end

tsrtest D r(Ys) using ate_cov.dta, overwrite: ate_cov

//ate_cov
di r(obsvStat)

// p.value_cov
di r(twotail) 


/*----------------------------------------------
 part f
----------------------------------------------*/

cap program drop Y0_sim Y1_sim Y_sim
gen Y0_sim = y
gen Y1_sim = y
gen Y_sim = .
replace Y0_sim = y - $tau if D==1
replace Y1_sim = y + $tau if D==0
	
capture program drop ate_null
program define ate_null, rclass
	replace Y_sim = Y0_sim*(1-D) + Y1_sim*(D) 
	regress Y_sim D 
    return scalar Ys=_b[D]	
end

ritest D Ys = r(Ys), reps(100000) sav(ate_null.dta, replace): ate_null


preserve
// CI and Hist
use "ate_null.dta", clear

sort Ys
global ylower= round(Ys[2501], .01)

global yupper= round(Ys[97500], .01)
di "95%CI: ($ylower, $yupper)" 

graph twoway (histogram Ys,frequency bin(1000)) ///
(scatteri 0 $tau 500 $tau, c(l) lc(red) lw(thick) lp(dash) m(i)) ///
(scatteri 0 $ylower 500 $ylower, c(l) lc(red) lw(thick) lp(dash) m(i)) ///
(scatteri 0 $yupper 500 $yupper, c(l) lc(red) lw(thick) lp(dash) m(i)), ///
legend(off) ///
b1title("Estimated ATE") title("Distribution under the null ATE = 10.7") ///
xtitle("")
restore
	
/*----------------------------------------------
 part g
----------------------------------------------*/

cap program drop Y0_sim Y1_sim Y_sim
gen Y0_sim = y
gen Y1_sim = y
gen Y_sim = .
replace Y0_sim = y - $tau_c if D==1
replace Y1_sim = y + $tau_c if D==0
	
capture program drop ate_null_cov
program define ate_null_cov, rclass
	replace Y_sim = Y0_sim*(1-D) + Y1_sim*(D) 
	regress Y_sim D x
    return scalar Ys_cov=_b[D]	
end


ritest D Ys_cov = r(Ys_cov), reps(100000) sav(ate_null_cov.dta, replace): ate_null_cov

preserve
// CI and Hist
use "ate_null_cov.dta", clear

sort Ys_cov
global yclower= round(Ys_cov[2501], .001)
global ycupper= round(Ys_cov[97500], .001)

di "95%CI: ($yclower, $ycupper)" 

graph twoway (histogram Ys_cov,frequency bin(1000)) ///
(scatteri 0 $tau_c 400 $tau_c, c(l) lc(red) lw(thick) lp(dash) m(i)) ///
(scatteri 0 $yclower 400 $yclower, c(l) lc(red) lw(thick) lp(dash) m(i)) ///
(scatteri 0 $ycupper 400 $ycupper, c(l) lc(red) lw(thick) lp(dash) m(i)), ///
legend(off) ///
b1title("Estimated ATE controlling covariance") title("Distribution under the null ATE = 5.3") ///
xtitle("")
restore


