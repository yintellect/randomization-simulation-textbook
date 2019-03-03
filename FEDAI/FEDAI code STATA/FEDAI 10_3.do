clear
//log using 10_3, name(solution10_3j)

*part j
	input z Y0M0 Y1M0 Y0M1 Y1M1 M0 M1
	0 0 0 0 0 0 0 
	0 0 0 0 0 0 1
	0 0 0 0 0 1 1
	0 0 1 0 1 0 0
	0 0 1 0 1 0 1 
	0 0 1 0 1 1 1
	1 1 0 1 1 0 0
	1 1 0 1 1 0 1
	1 1 0 1 1 1 1
	1 0 1 1 1 0 0
	1 0 1 1 1 0 1
	1 0 1 1 1 1 1
	end
	
tabstat Y0M0 Y1M0 Y0M1 Y1M1, stat(mean)





set seed 12345
generate rannum = .



quietly forvalues i = 1/1400 {
replace rannum = uniform()
egen z`i' = cut(rannum), group(2)
}





set matsize 1400

matrix coefmat=J(1400, 3, .)


matrix tcoefmat=J(1400, 2, .)

matrix mcoefmat=J(1400, 2, .)






forvalues i = 1/1400 {		
		gen M_`i' = M0*(1-z`i') + M1*z`i'
		gen Y_`i' = Y0M0*(1-z`i')*(1-M_`i') + Y1M0*(z`i')*(1-M_`i') + Y0M1*(1-z`i')*(M_`i') + Y1M1*(z`i')*(M_`i')
		
		qui reg Y_`i' M_`i' z`i'
		matrix coefmat[`i', 1] = _b[_cons]
		matrix coefmat[`i', 2] = _b[M_`i']
		matrix coefmat[`i', 3] = _b[z`i']
		
		
		qui: reg Y_`i' z`i'
		
		matrix tcoefmat[`i', 1] = _b[_cons]
		matrix tcoefmat[`i', 2] = _b[z`i']
		
		qui: reg Y_`i' M_`i' 
		
		matrix mcoefmat[`i', 1] = _b[_cons]
		matrix mcoefmat[`i', 2] = _b[M_`i']
				
		
}




mat U = J(rowsof(coefmat),1,1)
mat sum = U'*coefmat
mat meanvec = sum/rowsof(coefmat)
mat list meanvec ,format(%8.2f)

mat V = J(rowsof(tcoefmat),1,1)
mat sum = V'*tcoefmat
mat lis sum
mat meanvec = sum/rowsof(tcoefmat)
mat list meanvec,format(%8.2f)

mat W = J(rowsof(mcoefmat),1,1)
mat sum = W'*mcoefmat
mat lis sum
mat meanvec = sum/rowsof(mcoefmat)
mat list meanvec,format(%8.2f)

		
log close solution10_3j
