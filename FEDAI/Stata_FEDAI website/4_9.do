clear

/*----------------------------------------------
 part c
----------------------------------------------*/
	input ests shareoftotalN
	.00964 .049487
	-.007829 .1520981
	-.01362 .626616
	.008271 .171799
	end
	
	gen overall_ate = ests*shareoftotalN
	total(overall_ate)

/*----------------------------------------------
 part e
----------------------------------------------*/
	clear
	
	// download data from: http://hdl.handle.net/10079/95x6b0k
	// copy and paste the url to your web browser
	
	use "Gerber_Green_AAAPSS_2005.dta.dta"
	bysort strata: egen blockpr = mean(treat2)
	gen q = blockpr*treat2 + (1-blockpr)*(1-treat2)
	regress vote02 treat2 [aw=1/q]
