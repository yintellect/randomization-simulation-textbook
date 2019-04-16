*** Create datasets for POLS 4368 Experiments midterm exam, Spring 2013
*** TA: Al Fang

clear all

*-------------- ACORN data set for midterm Q1

use "/Users/al/Dropbox/Teaching 2012-2013/Experiments/Data and R Programs/Extras/Midterm datasets/phoenix_teaching_subset_1_and_2 person households only.dta", clear

keep vote03 vote02 vote00 treatmen treat2 persons contact hhid precinct age 

numlabel, add
rename treatmen treatment

label var vote03 "Voted in 2003: 1=Voted, 0=Abstained"
label var vote02 "Voted in 2002: 1=Voted, 0=Abstained"
label var vote00 "Voted in 2000: 1=Voted, 0=Abstained"
label var treatment "Original treatment assignment: 1=Treatment, 0=Control"
label var treat2 "Either member of the household was assigned to treatment: 1=Treatment, 0=Control"
label var persons "Number of persons in household"
label var contact "Contacted? 1=Yes, 0=No"
label var hhid "Household ID"
label var precinct "Precinct ID"
label var age "Age"

tab2 treatment treat2, m

describe, f

saveold "/Users/al/Dropbox/FEDAI Homework Solutions/Midterm/q2acorn.dta", replace


*-------------- hajj data for midterm Q2

use "/Users/al/Dropbox/Teaching 2012-2013/Experiments/Data and R Programs/Extras/Midterm datasets/hajj_public.dta", clear

* first destring the 6 "like of peoples" variables
destring  x_s15cq5 x_s15cq6 x_s15cq7 x_s15cq8 x_s15cq9 x_s15cq10, replace

summ x_s15cq5 x_s15cq6 x_s15cq7 x_s15cq8 x_s15cq9 x_s15cq10, sep(6)

* next create a simple additive index akin to column (1) of Table V in Clingingsmith et al. 2009
gen views =  x_s15cq5+ x_s15cq6 +x_s15cq7 +x_s15cq8 +x_s15cq9 +x_s15cq10
* egen views = rowtotal(x_s15cq5 x_s15cq6 x_s15cq7 x_s15cq8 x_s15cq9 x_s15cq10) 

keep persid hhid age female literate hajj2006 success views

rename hhid personid
rename persid hhid

label var hhid "Household identifier"
label var personid "Within-household person identifier"
label var age "Age"
label var literate "Literate: 1=Yes, 0=No"
label var female "Female: 1=Female, 0=Male"
label var views "Index measure: Attitudes toward people from other countries"

tab1 views , m

gen flag_miss_views = 0
replace flag_miss_views = 1 if views == .
label var flag_miss_views "FLAG: views contains missing value: 1=Yes, 0=No"

describe, f

saveold "/Users/al/Dropbox/FEDAI Homework Solutions/Midterm/q1hajj.dta", replace

drop if flag_miss_views == 1
drop flag_miss_views

tab1 views, m

describe, f

saveold "/Users/al/Dropbox/FEDAI Homework Solutions/Midterm/q1hajj_sub.dta", replace

