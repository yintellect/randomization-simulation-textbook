

* Analysis of Teacher Incentives experiment: 
* Muralidharan, Karthik, and Venkatesh Sundararaman. 2011. 
* ÒTeacher Performance Pay: Experimental Evidence from India.Ó 
* Journal of Political Economy 119: 39-77.


* create dummy codes for missing data
gen pretest=y0_nts
replace pretest=99 if pretest==.

gen pretest_miss=0
replace pretest_miss=1 if y0_nts==.

gen parent_literacy=parent_literacy_index
replace parent_literacy=99 if parent_literacy_index==.

gen parent_miss=0
replace parent_miss=1 if parent_literacy_index==.

reg y1_nts incentive ,cl(apfschoolcode)

reg y1_nts incentive pretest pretest_miss parent_literacy parent_miss infra_index_mean,cl(apfschoolcode)

drop if y1_nts==.

keep y1_nts incentive pretest pretest_miss parent_literacy parent_miss infra_index_mean apfschoolcode
