* Program to run a t-test on a de-clustered version of the Hajj dataset
* for chapter 3


* first destring the 6 "like of peoples" variables
destring  views_saudi views_indonesian views_turkish views_african views_chinese views_european, replace

* next create a simple additive index akin to column (1) of Table V in Clingingsmith et al. 2009
gen views =  views_saudi+ views_indonesian+ views_turkish+ views_african+ views_chinese+ views_european

* describe distributions for table 3.2
tab views success,col
tabulate views success, column nofreq

* Calculating ATE for page 17
sum views if success==0
sum views if success==1

* confidence intervals
ttest views, by(success) unequal

* assess reliability of outcome measure

alpha views_saudi views_indonesian views_turkish views_african views_chinese views_european
alpha views_saudi views_indonesian views_turkish views_african views_chinese views_european if success==0

* drop missing observations so that bootstrap works with the proper N
drop if views==.

* bootstrap t statistic using 1000 replications and saving results in bsauto file
* bootstrap t=r(t), rep(1000)  saving(bs_hajj, replace): ttest views, by(success) unequal

* bootstrap estimated ATE
bootstrap, rep(1000) saving(bs_hajj,replace): regress views success
