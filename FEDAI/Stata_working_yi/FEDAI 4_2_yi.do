clear
use "RushHour data for exercise 4-2.dta"

*part a
ritest treatment e(F), reps(10000) seed(1234): regress treat pretest
	
*part b
teffects ra (posttest) (treat), vce(bootstrap, rep(1000))
	
*part c
teffects ra (improvement) (treat),  vce(bootstrap, rep(1000))

