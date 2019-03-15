clear

set obs 217
egen z = fill (1 1)
replace z = 0 in 1/106

egen y = fill (0 0)
replace y =. in 1/28
replace y = 1 in 29/78
replace y =0 in 79/106
replace y =. in 107/123
replace y = 1 in 124/191


qui count if z==1 & y==.
scalar isna_treated = r(N)
qui count if z==0 & y==.
scalar isna_control= r(N)
qui count if z==1
scalar count_treated = r(N)
qui count if z==0
scalar count_control = r(N)

scalar prob_na_treated = isna_treated/count_treated
scalar prob_na_control= isna_control/count_control

scalar Q = ((1-prob_na_treated)-(1-prob_na_control))/(1-prob_na_treated)


qui gen y_z1 = y
qui replace y_z1 =. if z!=1
sort y_z1

qui count if z==1 & y!=.
scalar low_ceiling = ceil(r(N)*(1-Q))
scalar high_ceiling = ceil(r(N)*Q)

qui gen id = _n
qui gen y_z1_low = y_z1
qui replace y_z1_low=. if id>low_ceiling
qui gen y_z1_high = y_z1
qui replace y_z1_high=. if id<high_ceiling

qui mean y_z1_low
scalar y_z1_low_mean = _b[y_z1_low]
qui mean y_z1_high
scalar y_z1_high_mean = _b[y_z1_high]

qui mean y if z==0
scalar y_mean = _b[y]
 
disp y_z1_low_mean - y_mean  
disp y_z1_high_mean - y_mean
