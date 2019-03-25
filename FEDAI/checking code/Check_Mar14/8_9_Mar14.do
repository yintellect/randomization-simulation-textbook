// download data from: http://hdl.handle.net/10079/5x69pkq
// copy and paste the url to your web browser


clear
import delimited "GerberGreenBook_Chapter8_Table_8_4_8_5.csv"


/*----------------------------------------------
 part a
----------------------------------------------*/
	qui mean y01 if prox500==0
	scalar mean_y01 = _b[y01]
	qui mean y00 if prox500==0
	scalar mean_y00 = _b[y00]
	scalar true_ate = mean_y01-mean_y00
	disp true_ate
	
	qui mean y if prox500==0 & assignment ==1
	scalar mean_y = _b[y]
	qui mean y00 if prox500==0 & assignment==0
	scalar mean_y00_0 = _b[y00]
	scalar ate_hat = mean_y - mean_y00_0
	disp ate_hat
	

/*----------------------------------------------
 part b
----------------------------------------------*/
	qui mean y01 if prox500==1
	scalar mean_y01 = _b[y01]
	qui mean y00 if prox500==1
	scalar mean_y00 = _b[y00]
	qui mean y10 if prox500==1
	scalar mean_y10 = _b[y10]
	qui mean y11 if prox500==1
	scalar mean_y11 = _b[y11]
	
	scalar true_ate_01 = mean_y01 - mean_y00
	scalar true_ate_10 = mean_y10 - mean_y00
	scalar true_ate_11 = mean_y11 - mean_y00

	disp  true_ate_01
	disp  true_ate_10
	disp  true_ate_11
	
	
	gen q =.
	replace q=prob10 if exposure==10
	replace q=prob11 if exposure==11
	replace q=prob01 if exposure==01
	replace q=prob00 if exposure==00
	
	gen weights = 1/q
	
	
	qui regress y i.exposure if /// 
	prox500>0 & (exposure==0 | exposure==1) [aweight=weights]
	estimates store m1, title(Model 1)
	
	qui regress y i.exposure if ///
	prox500>0 & (exposure==0 | exposure==10) [pweight=weights]
	estimates store m2, title(Model 2)
	
	qui regress y i.exposure if ///
	prox500>0 & (exposure==0 | exposure==11) [pweight=weights]
	estimates store m3, title(Model 3)
	
	estout m1 m2 m3, ///
	cells(b(star fmt(3)) se(par fmt(3))) ///
	legend label varlabels(_cons Constant) ///
	stats(N r2)

	

/*----------------------------------------------
 part c
----------------------------------------------*/

// download data from: http://hdl.handle.net/10079/34tmpsh
// copy and paste the url to your web browser

	clear
	import delimited "GerberGreenBook_Chapter8_Exercise_9c.csv"
	
	gen q=.
	replace q=prob10 if exposure==10
	replace q=prob00 if exposure==0
	gen weights = 1/q
	
	regress y i.exposure if (prob10>0) & (prob10<1) [pweight=weights]
	


