clear
use "Arceneaux_AAAPSSsubset_2005.dta"

*part a
	rename treatmen z
	rename vote03 y
	ritest z e(F), strata(unit) reps(1000): reg v_p2003 v_m2003 v_g2002 v_p2002 v_m2002 v_s2001 v_g2000 v_p2000 v_m2000 v_s1999 v_m1999 v_g1998 v_m1998 v_s1998 v_m1997 v_s1997 v_g1996 v_p1996 v_m1996 v_s1996 z
	//p-value is off…one-tailed test?
	 
	
*part b
	teffects ra (vote03  v_p2003 v_m2003 v_g2002 v_p2002 v_m2002 v_s2001 v_g2000 v_p2000 v_m2000 v_s1999 v_m1999 v_g1998 v_m1998 v_s1998 v_m1997 v_s1997 v_g1996 v_p1996 v_m1996 v_s1996) (treatmen), ate
	//way to conduct one-tailed test?

*part c
	teffects ra (vote03  v_p2003 v_m2003 v_g2002 v_p2002 v_m2002 v_s2001 v_g2000 v_p2000 v_m2000 v_s1999 v_m1999 v_g1998 v_m1998 v_s1998 v_m1997 v_s1997 v_g1996 v_p1996 v_m1996 v_s1996) (treatmen), ate

	