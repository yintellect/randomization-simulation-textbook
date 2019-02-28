*part a
	set obs 7723
	ssc install egenmore //user-installed program
	egen z = repeat(), values("baseline")
	replace z = "treatment" in 2573/5144
	replace z = "placebo" in 5145/7723
	egen d = fill(0,0)
	replace d = 1 in 2573/3058
	replace d = 1 in 5145/5614
	egen y = fill(1,1)
	replace y=0 in 804/2572
	replace y=0 in 2763/3058
	replace y=0 in 3742/5144
	replace y=0 in 5285/5614
	replace y=0 in 6293/7723
	mean d if z=="treatment"
	mean d if z=="placebo"
	
*part b
	sum y if z=="treatment" & d==0 
	sum y if z=="placebo" & d==0

*part c
	sum y if z=="placebo"
	scalar y_placebo = r(mean)
	sum y if z=="baseline"
	scalar y_baseline = r(mean)
	scalar itt_placebo = y_placebo - y_baseline
	scalar cace_placebo = itt_placebo/pr_c_placebo
	disp cace_placebo
	
*part d
	sum y if z=="treatment"
	scalar y_treat = r(mean)
	sum y if z=="baseline"
	scalar y_base= r(mean)
	scalar itt_treatment = y_treat - y_base
	scalar cace_treatment1 = itt_treatment/pr_c_treatment
	disp cace_treatment1
	
	sum y if z=="treatment" & d==1
	scalar yd_treat = r(mean)
	sum y if z=="placebo" & d==1
	scalar yd_placebo = r(mean)
	scalar cace_treatment2 = yd_treat - yd_placebo
	disp cace_treatment2
