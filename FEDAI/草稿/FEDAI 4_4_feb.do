clear
import delimited GerberGreenBook_Chapter4_Table_4_1

*part a
	rename d D
	gen y = y0*(1-D) + y1*D
	regress y D
	
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
	ritest D _b[D], reps(100000) kdensityplot: regress y D

*part e
	ritest d _b[D], reps(100000) : regress y D x

*part f
	teffects ra (y) (D),  vce(bootstrap, rep(100000))
	
*part g
	teffects ra (y x) (D),  vce(bootstrap, rep(100000))

