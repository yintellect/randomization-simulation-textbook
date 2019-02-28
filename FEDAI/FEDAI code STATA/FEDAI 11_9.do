clear 
import delimited using "/Users/yy2633/MA/2019Spring/RA/FEDAI code STATA/dupas.csv"

*part b
	
	gen purchased =.
	replace purchased = 1 if purchasednet=="yes"
	replace purchased = 0 if purchasednet=="no"

	rename cfw_id region		
	

matrix t=J(100, 2, .)
matrix colnames t=gammas lls

local row_num=1


foreach i of numlist 1(1)100 {
		
		gen log_price_star_`i' = log(price + `i')
		qui glm purchased log_price_star_`i' i.region, family(binomial) link(logit)
		
		matrix t[`row_num', 1] = `row_num'
		matrix t[`row_num', 2] = `e(ll)'
		
		local row_num = `row_num' + 1
			
}

qui sum lls
list gammas if lls==r(max)


svmat double t, names(col)
scatter lls gammas, xline(19)



