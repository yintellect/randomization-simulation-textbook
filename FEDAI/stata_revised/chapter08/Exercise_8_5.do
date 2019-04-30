log using ../results/chapter08/exercise_8_5, replace

clear


/*----------------------------------------------
 part d
----------------------------------------------*/
	qui set obs 18859
	qui egen z_ind = fill(0,0)
	qui replace z_ind = 1 in 9534/18859
	
	// ssc install egenmore (uncomment to install the package)
	qui egen z_zip = repeat(), values("none")
	qui replace z_zip = "half" in 6218/12482
	qui replace z_zip = "all" in 12483/18859
	
	qui egen Y = fill(1,1)
	qui replace Y=0 in 1022/6217
	qui replace Y = 0 in 6744/9533
	qui replace Y=0 in 10154/12482
	qui replace Y=0 in 13799/18859
	
	qui mean Y if z_ind==1 & z_zip=="half"
	scalar ate_treat_half = _b[Y]
	qui mean Y if z_ind==0 & z_zip=="half"
	scalar ate_untreat_half = _b[Y]
	qui mean Y if z_ind==0 & z_zip=="none"
	scalar ate_untreat_none = _b[Y]
	qui mean Y if z_ind==1 & z_zip=="all"
	scalar ate_treat_all = _b[Y]
	
	// ate.fristhand.half
	disp ate_treat_half - ate_untreat_half
	// ate.secondhanf.untreated
	disp ate_untreat_half - ate_untreat_none
	// ate.secondhand.treated
	disp ate_treat_all - ate_treat_half
	
log close
translate ../results/chapter08/exercise_8_5.smcl ../results/chapter08/exercise_8_5.pdf, translator(smcl2pdf)

