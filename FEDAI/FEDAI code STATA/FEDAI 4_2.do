clear
use "GerberGreenBook_Chapter4_Table_4_2.dta"

*part a
	ritest treat _b[treat], reps(10000): regress pretest treat
	
*part b
	regress posttest treat
	
*part c
	regress improvement treat
