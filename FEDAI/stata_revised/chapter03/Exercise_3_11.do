log using ../results/chapter03/exercise_3_11, replace

clear
set seed 1234567

set obs 14

/*----------------
 Part a
-----------------*/
qui input Y0 Y1
	0 0
	1 0
	2 1
	4 2
	4 0
	6 0
	6 2
	9 3
	14 12
	15 9
	16 8
	16 15
	17 5 
	18 17 end



qui gen int cluster = (_n+1)/2

//ssc install tabstatmat  (install the package)
// save tabstat summary result to matrix
qui tabstat Y0, by(cluster) stat(mean) save
qui tabstatmat Ybar0, nototal
mat colnames Ybar0=Ybar0

qui tabstat Y1, by(cluster) stat(mean) save
qui tabstatmat Ybar1, nototal
mat colnames Ybar1=Ybar1



// function to calculate population variance
cap program drop var_pop
program define var_pop, rclass
	args varname	
	tempvar x_dev 
	qui sum `varname'
	local avg = r(mean)
	local length = r(N)	
	gen `x_dev' = (`varname'-`avg')^2/`length'
	qui tabstat `x_dev', stat(sum) save
	return scalar variance_pop = el(r(StatTotal),1,1)
end


// function to calculate population covariance
cap program drop cor_pop
program define cor_pop, rclass
	args x y	
	tempvar xy_dev 
	qui sum `x'
	local avg_x = r(mean)
	local length = r(N)	
	
	qui sum `y'
	local avg_y = r(mean)
		
	gen `xy_dev' = (`x'-`avg_x')*(`y'-`avg_y')
	qui tabstat `xy_dev', stat(sum) save
	return scalar cor_pop = el(r(StatTotal),1,1)/`length'
end


preserve 

clear
qui set obs 7
svmat Ybar0, names(col)
svmat Ybar1, names(col)

// var_Ybar0	
var_pop Ybar0
scalar var_Ybar0=r(variance_pop)

// var_Ybar1 
var_pop Ybar1
scalar var_Ybar1=r(variance_pop)

// cov_Ybar0 
cor_pop Ybar0 Ybar1

scalar cov_Ybar0=r(cor_pop)

scalar se_ate = sqrt((1/6)*((4/3)*var_Ybar0+(3/4)*var_Ybar1+2*cov_Ybar0))

di "se_ate ="%8.6f se_ate

restore


/*----------------
 Part b
-----------------*/

qui replace cluster = _n
qui replace cluster = 15-cluster if (cluster>7)

clear matrix
// Ybar0	
qui tabstat Y0, by(cluster) stat(mean) save
qui tabstatmat Ybar0, nototal
mat colnames Ybar0=Ybar0

// Ybar1
qui tabstat Y1, by(cluster) stat(mean) save
qui tabstatmat Ybar1, nototal
mat colnames Ybar1=Ybar1
	
	
	
preserve 
clear
qui set obs 7
svmat Ybar0, names(col)
svmat Ybar1, names(col)	
	
// var_Ybar0 <- var.pop(Ybar0)	
var_pop Ybar0
scalar var_Ybar0=r(variance_pop)

// var_Ybar1 <- var.pop(Ybar1)
var_pop Ybar1
scalar var_Ybar1=r(variance_pop)

// cov_Ybar0 <- cov.pop(Ybar0,Ybar1)
cor_pop Ybar0 Ybar1
scalar cov_Ybar0=r(cor_pop)

// se_ate
scalar se_ate = sqrt((1/6)*((4/3)*var_Ybar0+(3/4)*var_Ybar1+2*cov_Ybar0))
di "se_ate ="%8.7f se_ate

restore	
	
	
log close
translate ../results/chapter03/exercise_3_11.smcl ../results/chapter03/exercise_3_11.pdf, translator(smcl2pdf)	
