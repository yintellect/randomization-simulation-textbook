clear

*part c
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

	regress y white highquality chicago whitehighquality whitechicago highqualitychicago whitehighqualitychicago
