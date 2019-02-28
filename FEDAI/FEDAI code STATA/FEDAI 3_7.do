clear
use "GerberGreenBook_Chapter3_Table_3_7.dta"

gen y_star= y+d*-7
ritest d _b[d], reps(10000) left: regress y_star d
