clear

/*----------------------------------------------
 part a
----------------------------------------------*/
	set obs 16
	//(uncomment to install the package)
	//ssc install egenmore 
	egen week = repeat(), values("2")
	replace week = "3" in 9/l
	egen prob00 = fill(0.5,0.5)
	replace prob00=0.25 in 9/l
	egen prob01 = fill(0.25,0.25)
	egen prob11 = fill(0.25,0.25)
	replace prob11 = 0.5 in 9/l
	
	input int y str2 z
	9 "11"
	5 "00"
	2 "01"
	3 "00"	
	3 "00"
	8 "11"
	3 "00"
	1 "01"
	4 "11"
	7 "01"
	10 "11"
	10 "01"
	3 "00"
	10 "11"
	4 "00"
	3 "11"
	
	// (uncomment to install the package)
	// ssc inst _gwtmean 
	
	gen prob01z = prob01 if z=="01"
	replace prob01z = prob01 if prob01z==.
	//difference in the way that R and STATA weighted mean functions
	gen y01z = y if z=="01"
	egen mean01 = wtmean(y01z), weight(1/prob01z)
	gen prob00z = prob00 if z=="00"
	replace prob00z = prob00 if prob01z==.
	gen y00z= y if z=="00"
	egen mean00 = wtmean(y00z), weight(1/prob00z)
	gen ate01_00 = mean01 - mean00
	disp ate01_00
	
/*----------------------------------------------
 part b
----------------------------------------------*/
	gen prob01z2 = prob01 if z=="01" & week=="2"
	gen y11z2 = y if z=="11" & week=="2"
	replace prob01z2 = .25 if prob01z2==.
	egen mean11 = wtmean(y11z2), weight(1/prob01z2)
	gen prob00z2 = prob01 if z=="00" & week=="2"
	replace prob00z2 = prob01 if prob00z2==.
	gen y00z2 = y if z=="00" & week=="2"
	drop mean00
	egen mean00 = wtmean(y00z2), weight(1/prob00z2)
	
	gen ate11_00 = mean11 - mean00
	disp ate11_00
