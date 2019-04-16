* program to replicate each of the estimates in problem 9-9

use "/Users/donaldgreen/Dropbox/Field Experimentation Book/homework solutions/Fieldhouse data (simulated) for problem 9-9.dta",clear

* ATE of mail 
reg y m [fw=n] if p==0

* CATE of mail among compliers
reg y m [fw=n] if c==1

* CATE of mail among noncompliers
reg y m [fw=n] if p==1 & c==0

* CACE of phone
ivregress 2sls y  (c  = p ) [fweight = n] if m==0

* CACE of phone among those who receive mail
ivregress 2sls y  (c  = p ) [fweight = n] if m==1

* full interactive specification
* note that c is the effect of phone call for those without mail
*           c_m is the added effect of phone call for those who get mail
*           m is the effect of mail for those who are not called (NOT compliers/noncompliers)

*gen p_m=p*m
*gen c_m=c*m
ivregress 2sls y m (c c_m = p p_m) [fweight = n]

* rescale results in order to estimate the effects of mail for those not contacted
gen np=1-p
gen nc=1-c
gen np_m=(1-p)*m
gen nc_m=(1-c)*m

ivregress 2sls y m (nc nc_m = np np_m) [fweight = n]
