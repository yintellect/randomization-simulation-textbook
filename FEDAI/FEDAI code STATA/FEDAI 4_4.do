clear
use "GerberGreenBook_Chapter4_Table_4_1.dta"

*part a
	
	gen y = y0*(1-D) + y1*D
	regress y D
	twoway (scatter y d) (lfit y D)
	summ y if D==1, meanonly
	scalar treat_mean = r(mean)
	summ y if D==0, meanonly
	scalar control_mean = r(mean)
	disp treat_mean - control_mean
	
*part b

	regress y x if D==1
	scalar co_treat = _b[x]
	regress y x if D==0
	scalar co_control= _b[x]
	disp co_treat + co_control
	
	gen ydiff = y - x
	regress ydiff D

*part c
	regress y D x
	
*part d
	ritest D _b[D], reps(10000) kdensityplot: regress y D

*part e
	ritest d _b[D], reps(100000) : regress y D x

*part f
	ritest d _b[D], reps(10000) level(95): regress y D
	//calculate CI of treatment effect?
	
*part g
	ritest d _b[D], reps(10000) level(95): regress y D x
	//calculate CI of treatment effect?
