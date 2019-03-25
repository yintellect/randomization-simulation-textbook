clear
use "Chapter 8_Leslie Hough self-experiment data.dta"

*part b
	rename tetris y
	rename run z
	scalar n = r(N)
	gen zlag = .
	replace zlag= z[_n-1] in 2/l
	gen ylag = .
	replace ylag = y[_n-1] in 2/l
	
	ritest z _b[z], reps(100) right: regress y z
	
	ritest z e(F), reps(100): regress y z zlag
	//zlag needs to be reinitialized in every loop; may need manual randomization 'while' loop
	ritest z _b[z], reps(10000): regress ylag z
	ritest z _b[z], reps(10000) right: regress energy z
	ritest z _b[z], reps(10000) right: regress gre z

	//p-values not quite exact
