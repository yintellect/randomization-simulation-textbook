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
	
	mean y0
	scalar mean_y0 = _b[y0]
	mean y1
	scalar mean_y1 = _b[y1]
	count
	scalar length = r(N)
	sort y0
	
	egen rank_y1=rank(y1), unique
	gen id=_n

	search vlookup, all
	//install vlookup 
	vlookup id, generate(y1_lowtohigh) key(rank_y1) value(y1)
	replace id = 9-id
	vlookup id, generate(y1_hightolow) key(rank_y1) value(y1)
	
	egen cov_min = sum((y0-mean_y0)*(y1_hightolow-mean_y1))
	replace cov_min = cov_min/length
	egen cov_max = sum((y0-mean_y0)*(y1_lowtohigh-mean_y1))
	replace cov_max = cov_max/length
	
	disp cov_min
	disp cov_max
	
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
	
	mean y1_hightolow_block
	scalar mean_y1_block = _b[y1_hightolow_block]
	
	egen cov_min_block = sum((y0-mean_y0)*(y1_hightolow_block-mean_y1_block))
	replace cov_min_block = cov_min_block/length
	egen cov_max_block = sum((y0-mean_y0)*(y1_lowtohigh_block-mean_y1_block))
	replace cov_max_block = cov_max_block/length
	
	disp cov_min_block
	disp cov_max_block

