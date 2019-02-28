clear
use "Guan_Green_CPS_2006.dta"

*part a
	drop if turnout == .
	drop if contact == .
	drop if dormid == .
	drop if treat2 == . 
	
	sum turnout if treat==1
	scalar treat_mean = r(mean)
	sum turnout if treat==0
	scalar control_mean = r(mean)
	
	disp treat_mean - control_mean
	
*part b
	ritest treat2 _b[treat2], strata(dormid): regress turnout treat2

*part c
	regress turnout treat2
	scalar itt = _b[treat2]
	regress contact treat2
	scalar ittd = _b[treat2]
	scalar cace = itt/ittd
	disp cace
