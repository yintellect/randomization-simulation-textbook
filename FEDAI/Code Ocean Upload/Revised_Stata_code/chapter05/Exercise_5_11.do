log using ../results/chapter05/exercise_5_11, replace


/*----------------------------------------------
 part a
----------------------------------------------*/
	qui set obs 7723
	// ssc install egenmore (install the package)
	qui egen z = repeat(), values("baseline")
	qui replace z = "treatment" in 2573/5144
	qui replace z = "placebo" in 5145/7723
	qui egen d = fill(0,0)
	qui replace d = 1 in 2573/3058
	qui replace d = 1 in 5145/5614
	qui egen y = fill(1,1)
	qui replace y=0 in 804/2572
	qui replace y=0 in 2763/3058
	qui replace y=0 in 3742/5144
	qui replace y=0 in 5285/5614
	qui replace y=0 in 6293/7723

	qui sum d if z=="treatment"
	scalar pr_c_treatment =  r(mean)
	di %8.3f pr_c_treatment

	qui sum d if z=="placebo"
	scalar pr_c_placebo =  r(mean)
	di %8.4f pr_c_placebo
	
/*----------------------------------------------
 part b
----------------------------------------------*/
	// rate.nt.treatment
	qui sum y if z=="treatment" & d==0 
	di %8.4f r(mean)
	
	// rate.nt.placebo
	qui sum y if z=="placebo" & d==0
	di %8.4f r(mean)

/*----------------------------------------------
 part c
----------------------------------------------*/
	qui sum y if z=="placebo"
	scalar y_placebo = r(mean)
	qui sum y if z=="baseline"
	scalar y_baseline = r(mean)
	
	scalar itt_placebo = y_placebo - y_baseline
	scalar cace_placebo = itt_placebo/pr_c_placebo
	
	// Estimate the CACE of receiving the placebo
	disp %8.3f cace_placebo
	
/*----------------------------------------------
 part d
----------------------------------------------*/
	qui sum y if z=="treatment"
	scalar y_treat = r(mean)
	qui sum y if z=="baseline"
	scalar y_base= r(mean)
	scalar itt_treatment = y_treat - y_base
	scalar cace_treatment1 = itt_treatment/pr_c_treatment
	disp %8.7f cace_treatment1
	
	qui sum y if z=="treatment" & d==1
	scalar yd_treat = r(mean)
	qui sum y if z=="placebo" & d==1
	scalar yd_placebo = r(mean)
	scalar cace_treatment2 = yd_treat - yd_placebo
	disp cace_treatment2

	
log close
translate ../results/chapter05/exercise_5_11.smcl ../results/chapter05/exercise_5_11.pdf, translator(smcl2pdf)

