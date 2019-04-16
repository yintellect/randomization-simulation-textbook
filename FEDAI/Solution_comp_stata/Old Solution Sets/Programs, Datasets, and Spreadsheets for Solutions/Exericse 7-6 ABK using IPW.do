* program to analyze ABK data for Problem 7-6
use "/Users/donaldgreen/Dropbox/Field Experimentation Book/Datasets for Website/Chapter 7_Angrist, Bettinger, and Kremer (2006) Subset.dta",clear
set more off

*gen missing=0
*replace missing=1 if read==0 | read==.

*keep if age>=9 & age<=25 & checkid==1

* naive comparison of means assuming MCAR (MIPO)
ttest read if read>0, by(vouch0)

* now set up the R variable
gen nonmissing=1-missing

* per exercise 7-6
reg nonmissing  vouch0 sex_name phone 
predict yhat

* verify that predicted probabilities fall between 0 and 1
tab yhat
gen ipw=1/yhat
regress read vouch0 if read > 0  [aweight = ipw]
