* Program to generate generic data with two-sided noncompliance (Table 6.5)

*Alter the code below to include your working directory where you have stored the file
use "GerberGreenBook_Chapter6_Simulated_Downstream_Data.dta", clear

***Simulation 1

* show distribution of subject type
tab subject_type [fw=wt]

* assess ATE by subject type
bys  subject_type: summ t [fw=wt]

tab d z [fw=wt],col
tab y z [fw=wt],col

*itt_D
reg d z [fw=wt]

*itt
reg y z [fw=wt]

*CACE
ivregress 2sls y (d = z) [fweight = wt]

***Simulation 2
* experiment with weighting AT or NT in order to change power
replace wt=wt*2 if  subject_type=="always-takers" |  subject_type=="never-takers"

*Then Rerun Simulation 1 above

***Simulation 3
*First, reload data (first line of code)
*Then, use different numbers of Defiers (with ATE=0)
replace wt=25 if  subject_type=="defiers"

*Then Rerun Simulation 1 above

***Simulation 4
*First, reload data (first line of code)
* Then, illustrate a violation of the exclusion restriction
replace  y=y_0*(1-d)+y_1*d + 0.02*z

*Then Rerun Simulation 1 above



