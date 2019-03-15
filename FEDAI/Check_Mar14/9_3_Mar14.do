clear

	set obs 8
	egen block = repeat(), values("A")
	replace block ="B" in 5/8
	input int y0 int y1
	0 2
	1 5
	1 3
	2 1	
	2 3
	3 3
	4 9
	4 7
	
	
	// function to calculate population covariance
	cap program drop cov_pop
	program define cov_pop, rclass
	args x y	
	tempvar xy_dev 
	qui sum `x'
	local avg_x = r(mean)
	local length = r(N)	
	
	qui sum `y'
	local avg_y = r(mean)
		
	gen `xy_dev' = (`x'-`avg_x')*(`y'-`avg_y')
	qui tabstat `xy_dev', stat(sum) save
	return scalar cor_pop = el(r(StatTotal),1,1)/`length'
	end


	
	egen rank_y1=rank(y1), unique
	gen id=_n
	
		
	//ssc install vlookup 
	// search vlookup, all
	vlookup id, generate(y1_lowtohigh) key(rank_y1) value(y1)
	replace id = 9-id
	vlookup id, generate(y1_hightolow) key(rank_y1) value(y1)
	
	
	cov_pop y0 y1_hightolow
	di "cov.min ="%8.3f r(cor_pop)
	
		
	cov_pop y0 y1_lowtohigh
	di "cov.min ="%8.3f r(cor_pop)
	
	
	
	/*-----including blocks-----*/
	

	replace id=_n
	replace id=. if block=="B"
	egen rank_y1_A = rank(y1), unique by(block)
	replace rank_y1_A =. if block=="B"
	vlookup id, generate(y1_hightolow_block_A) key(rank_y1_A) value(y1)
	replace id = _n-4
	replace id =. if block=="A"
	egen rank_y1_B = rank(y1), unique by(block)
	replace rank_y1_B =. if block=="A"
	vlookup id, generate(y1_hightolow_block_B) key(rank_y1_B) value(y1)
	gen y1_hightolow_block = y1_hightolow_block_A
	replace y1_hightolow_block = y1_hightolow_block_B if block=="B"
	
	replace id=5-_n
	replace id=. if block=="B"
	vlookup id, generate(y1_lowtohigh_block_A) key(rank_y1_A) value(y1)
	replace id = 9-_n
	replace id =. if block=="A"
	vlookup id, generate(y1_lowtohigh_block_B) key(rank_y1_B) value(y1)
	gen y1_lowtohigh_block = y1_lowtohigh_block_A
	replace y1_lowtohigh_block = y1_lowtohigh_block_B if block=="B"
	

	
	cov_pop y0 y1_lowtohigh_block
	di "cov.min ="%8.5f r(cor_pop)
	
	cov_pop y0 y1_hightolow_block
	di "cov.min ="%8.3f r(cor_pop)
	

