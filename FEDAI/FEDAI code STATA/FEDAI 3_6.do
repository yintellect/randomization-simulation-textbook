clear
use "GerberGreenBook_Chapter3_Table_3_6.dta"

ritest success _b[success], reps(10000): regress views success
// c = # {|T| >= |T(obs)|} 
ritest success _b[success], right reps(10000): regress views success // one-sided 

