* Program to run a t-test on a de-clustered version of the Hajj dataset
* for chapter 3

* first destring the 6 "like of peoples" variables
destring  x_s15cq5 x_s15cq6 x_s15cq7 x_s15cq8 x_s15cq9 x_s15cq10, replace

* next create a simple additive index akin to column (1) of Table V in Clingingsmith et al. 2009
gen views =  x_s15cq5+ x_s15cq6 +x_s15cq7 +x_s15cq8 +x_s15cq9 +x_s15cq10

* describe distributions
tab views success,col
tabulate views success, column nofreq

* confidence intervals
ttest views, by(success) unequal

* assess reliability of outcome measure

alpha x_s15cq5 x_s15cq6 x_s15cq7 x_s15cq8 x_s15cq9 x_s15cq10
alpha x_s15cq5 x_s15cq6 x_s15cq7 x_s15cq8 x_s15cq9 x_s15cq10 if success==0

* drop missing observations so that bootstrap works with the proper N
drop if views==.

* bootstrap t statistic using 1000 replications and saving results in bsauto file
* bootstrap t=r(t), rep(1000)  saving(bs_hajj, replace): ttest views, by(success) unequal

* bootstrap estimated ATE
bootstrap, rep(1000) saving(bs_hajj,replace): regress views success
