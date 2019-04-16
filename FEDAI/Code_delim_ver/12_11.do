
// download data from: http://hdl.handle.net/10079/cfxpp69
// copy and paste the url to your web browser
clear
//use "Howell_Peterson_BIP_2002.dta.dta"
import delim Howell_Peterson_BIP_2002, clear
/*----------------------------------------------
 part a
----------------------------------------------*/
	
    // show the proportion in percentage
	tabulate missing_y1math treat, column nof
	

/*----------------------------------------------
 part b
----------------------------------------------*/

	tabstat y0_1math_change, by(treat) stat(mean) not
	


/*----------------------------------------------
 part c
----------------------------------------------*/
	
	histogram y0_1math_change if treat==1, bin(50) freq ///
	title("Histogram of y0_1math_change[treat == 1]")
		
	
	tabstat y0_1math_change if treat==1, stat(min max)
	
	
	centile y0_1math_change if treat==1, centile(5 10 15 25 50 75 85 90 95)
/*----------------------------------------------
 part d
----------------------------------------------*/
	centile y0_1math_change if treat==1, centile(93.6)

/*----------------------------------------------
 part e
----------------------------------------------*/
	qui mean y0_1math_change if treat==1 & missing_y1math==0 & y0_1math_change < 40
	scalar l_b = _b[y0_1math_change]
	qui count if treat==1 & missing_y1math==0 & y0_1math_change < 40
	scalar l_b_count = r(N)
	qui count if treat==1
	disp %8.6f l_b
	disp %8.7f 1-l_b_count/r(N)

/*----------------------------------------------
 part f
----------------------------------------------*/
	qui mean y0_1math_change if treat==0 
	disp %18.6f l_b - _b[y0_1math_change]

/*----------------------------------------------
 part g
----------------------------------------------*/
	centile y0_1math_change if treat==1, centile(6.4)

/*----------------------------------------------
 part h
----------------------------------------------*/
	qui mean y0_1math_change if treat==1 & missing_y1math==0 & y0_1math_change > -18
	scalar u_b = _b[y0_1math_change]
	disp %8.6f u_b
	qui count if treat==1 & missing_y1math==0 & y0_1math_change > -18
	scalar u_b_count = r(N)
	qui count if treat==1
	disp %8.7f 1-u_b_count/r(N)

/*----------------------------------------------
 part i
----------------------------------------------*/
	qui mean y0_1math_change if treat==0 
	disp %8.6f u_b - _b[y0_1math_change]
