// FEDAI website dataset mis-specify
// correct data not available on FEIDA 



clear 
import delimited using "dupas.csv"

/*----------------------------------------------
 part b
----------------------------------------------*/
	
	gen purchased =.
	replace purchased = 1 if purchasednet=="yes"
	replace purchased = 0 if purchasednet=="no"

	rename cfw_id region		
	

matrix t=J(100, 2, .)
matrix colnames t=gammas lls


forvalues i = 1/100 {
		
		gen log_price_star_`i' = log(price + `i')
		qui glm purchased log_price_star_`i' i.region, family(binomial) link(logit)
		
		matrix t[`i', 1] = `i'
		matrix t[`i', 2] = `e(ll)'

}


svmat double t, names(col)

qui sum lls
list gammas if lls==r(max)


scatter lls gammas, xline(19)




