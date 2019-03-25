clear

/*----------------------------------------------
 part c
----------------------------------------------*/
	set obs 4870
	egen y = fill(1,1)
	replace y = 0 in 39/542
	replace y = 0 in 598/1084
	replace y = 0 in 1131/1625
	replace y = 0 in 1697/2166
	replace y=0 in 2204/2836
	replace y=0 in 2885/3506
	replace y=0 in 3543/4188
	replace y=0 in 4250/4870
	
	egen boston = fill(1,1)
	replace boston = 0 in 2167/4870
	gen chicago = 1-boston
	egen lowquality = fill(1,1)
	replace lowquality = 0 in 1085/2166
	replace lowquality = 0 in 3507/4870
	gen highquality = 1-lowquality
	egen black = fill(1,1)
	replace black = 0 in 543/1084
	replace black = 0 in 1626/2166
	replace black = 0 in 2837/3506
	replace black = 0 in 4188/4870
	replace black = 0 in 4189/4870
	gen white = 1-black
	
	gen whitehighquality = white*highquality
	gen whitechicago = white*chicago
	gen highqualitychicago = highquality*chicago
	gen whitehighqualitychicago = white * highquality * chicago
	
	// fit_1
	qui regress y white highquality chicago whitehighquality whitechicago highqualitychicago whitehighqualitychicago
	estimates store m1, title(Model 1)
	
	gen blackhighquality = black*highquality
	gen blackchicago = black*chicago
	gen blackhighqualitychicago = black * highquality * chicago
	
	// fit_2
	qui regress y black highquality chicago blackhighquality blackchicago highqualitychicago blackhighqualitychicago	
	estimates store m2, title(Model 2)
	
	// fit_3
	gen whiteboston = white*boston
	gen highqualityboston = highquality*boston
	gen whitehighqualityboston = white * highquality * boston
	qui regress y white highquality boston whitehighquality whiteboston highqualityboston whitehighqualityboston
	
	estimates store m3, title(Model 3)
	
	// fit_4
	gen blacklowquality = black*lowquality
	gen lowqualitychicago = lowquality*chicago
	gen blacklowqualitychicago = black * lowquality * chicago
	
	qui regress y black lowquality chicago blacklowquality blackchicago lowqualitychicago blacklowqualitychicago		
	estimates store m4, title(Model 4)
	
	// (uncomment to install package
	//ssc install estout 
	
	// pool 4 regression models to produce result table
	estout m1 m2 m3 m4, cells(b(star fmt(3)) se(par fmt(3))) ///
	starlevels( * 0.10 ** 0.05 *** 0.010) ///
	legend label varlabels(_cons constant) ///
	stats(N r2 r2_a rmse F, fmt(0 3 3 3 3) label(N R-squared Ajusted-R2 Residual_Std_Error F-Statistic))

