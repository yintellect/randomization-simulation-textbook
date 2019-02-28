clear
use "GerberGreenBook_Chapter8_Table_8_4_8_5.dta"

*part d
	set obs 18859
	egen z_ind = fill(0,0)
	replace z_ind = 1 in 9534/18859
	
	//ssc install egenmore
	egen z_zip = repeat(), values("none")
	replace z_zip = "half" in 6218/12482
	replace z_zip = "all" in 12483/18859
	
	egen Y = fill(1,1)
	replace Y=0 in 1022/6217
	replace Y = 0 in 6744/9533
	replace Y=0 in 10154/12482
	replace Y=0 in 13799/18859
	
	mean Y if z_ind==1 & z_zip=="half"
	scalar ate_treat_half = _b[Y]
	mean Y if z_ind==0 & z_zip=="half"
	scalar ate_untreat_half = _b[Y]
	mean Y if z_ind==0 & z_zip=="none"
	scalar ate_untreat_none = _b[Y]
	mean Y if z_ind==1 & z_zip=="all"
	scalar ate_treat_all = _b[Y]
	
	disp ate_treat_half - ate_untreat_half
	disp ate_untreat_half - ate_untreat_none
	disp ate_treat_all - ate_treat_half
