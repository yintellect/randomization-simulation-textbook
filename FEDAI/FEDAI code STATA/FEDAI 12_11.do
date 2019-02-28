clear
use "Howell_Peterson_BIP_2002.dta"

*part a
	tab missing_y1math treat

*part b
	mean y0_1math_change if treat==0
	mean y0_1math_change if treat==1

*part c
	histogram y0_1math_change if treat==1, bin(50)
	sum y0_1math_change if treat==1
	centile y0_1math_change if treat==1, centile(5 10 15 25 50 75 85 90 95)

*part d
	centile y0_1math_change if treat==1, centile(93.6)

*part e
	mean y0_1math_change if treat==1 & missing_y1math==0 & y0_1math_change < 40
	scalar l_b = _b[y0_1math_change]
	count if treat==1 & missing_y1math==0 & y0_1math_change < 40
	scalar l_b_count = r(N)
	count if treat==1
	disp 1-l_b_count/r(N)

*part f
	mean y0_1math_change if treat==0 
	disp l_b - _b[y0_1math_change]

*part g
	centile y0_1math_change if treat==1, centile(6.4)

*part h
	mean y0_1math_change if treat==1 & missing_y1math==0 & y0_1math_change > -18
	scalar u_b = _b[y0_1math_change]
	count if treat==1
	disp 1-u_b_count/r(N)

*part i
	mean y0_1math_change if treat==0 
	disp u_b - _b[y0_1math_change]
