// download data from: http://hdl.handle.net/10079/dv41p3d
// copy and paste the url to your web browser

clear

use "Arceneaux_AAAPSSsubset_2005.dta"

*part a
	rename treatmen z
	rename vote03 y
	rename unit clust
	ritest z e(F), cluster(clust) reps(1000) seed(1234567) right: ///
	reg z v_p2003 v_m2003 v_g2002 v_p2002 v_m2002 v_s2001 v_g2000 ///
	v_p2000 v_m2000 v_s1999 v_m1999 v_g1998 v_m1998 v_s1998 v_m1997 ///
	v_s1997 v_g1996 v_p1996 v_m1996 v_s1996
	
	// one-tailed p-value
	di %8.3f el(r(p),1,1)
		
	 
	
*part b
				
	ritest z _b[z], cluster(clust) reps(2000) seed(1234567) right: ///
	reg y z v_p2003 v_m2003 v_g2002 v_p2002 v_m2002 v_s2001 v_g2000 ///
	v_p2000 v_m2000 v_s1999 v_m1999 v_g1998 v_m1998 v_s1998 v_m1997 ///
	v_s1997 v_g1996 v_p1996 v_m1996 v_s1996
	
	// ate
	di %8.5f el(r(b),1,1)

	// one-tailed p-value
	di %8.3f el(r(p),1,1)
	
		

*part c
	bysort unit: egen clustpr = mean(z)
	gen q = clustpr*z + (1-clustpr)*(1-z)
	regress y z v_p2003 v_m2003 v_g2002 v_p2002 v_m2002 v_s2001 v_g2000 v_p2000 v_m2000 v_s1999 v_m1999 v_g1998 v_m1998 v_s1998 v_m1997 v_s1997 v_g1996 v_p1996 v_m1996 v_s1996[pw = 1/q]

	regress y z [pw = 1/q]

	
	teffects ra (y) (z ) [pw = 1/q]
	
	teffects ipw (y) (z v_p2003 v_m2003 v_g2002 v_p2002 v_m2002 v_s2001 v_g2000 v_p2000 v_m2000 v_s1999 v_m1999 v_g1998 v_m1998 v_s1998 v_m1997 v_s1997 v_g1996 v_p1996 v_m1996 v_s1996), ate
	
	gen ipw = 1/q
	
	
	ritest z b[1,1], cluster(unit) reps(10) seed(1234567) right:///
	teffects ipw (y) (z v_p2003 v_m2003 v_g2002 v_p2002 v_m2002 v_s2001 v_g2000 v_p2000 v_m2000 v_s1999 v_m1999 v_g1998 v_m1998 v_s1998 v_m1997 v_s1997 v_g1996 v_p1996 v_m1996 v_s1996), ate
	




