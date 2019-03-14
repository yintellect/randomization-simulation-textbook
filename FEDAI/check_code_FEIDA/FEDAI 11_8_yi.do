clear
import delimited GerberGreenBook_Chapter11_Table_11_3


* part a
tabstat m1_x2 m2_x2, statistics(sum) by(region)



*part b

// FEDAI website dataset mis-specify

clear 
import delimited using "/Users/yy2633/MA/2019Spring/RA/FEDAI code STATA/dupas.csv"

import delimited using "Chapter_11_Dupas_(2010)_Dataset.csv", clear
	gen log_price = log(price)
	
	gen purchased =.
	replace purchased = 1 if purchasednet=="yes"
	replace purchased = 0 if purchasednet=="no"

	rename cfw_id region		


quietly: glm purchased log_price i.region c.log_price#i.region if (price !=0 & region!=4), family(binomial) link(logit)
predict double preds_1

quietly:glm purchased log_price i.region if (price !=0 & region!=4), family(binomial) link(logit)
predict double preds_2

drop if (price ==0 | region==4)

generate non_p =.
replace non_p=1 if purchased==0
replace non_p=0 if purchased!=0

generate pred_nonp_1 = 1-preds_1
generate pred_nonp_2 = 1-preds_2


collapse (sum) purchases=purchased ///
   non_purchases=non_p ///
   pred_purchases_1=preds_1 ///
   pred_nonpurchases_1=pred_nonp_1 ///
   pred_purchases_2=preds_2 ///
   pred_nonpurchases_2=pred_nonp_2, by(region price)
   
   
gen chi_square_1 = (purchases- pred_purchases_1)^2/ pred_purchases_1 +(non_purchases- pred_nonpurchases_1)^2/ pred_nonpurchases_1

gen chi_square_2 = (purchases- pred_purchases_2)^2/ pred_purchases_2 +( non_purchases- pred_nonpurchases_2)^2/ pred_nonpurchases_2

// print table
list region price purchases non_purchases pred_purchases_1 pred_nonpurchases_1 chi_square_1
list region price purchases non_purchases pred_purchases_2 pred_nonpurchases_2 chi_square_2


tabstat chi_square_1, statistics(sum) save
matrix stats=r(StatTotal)
scalar  model_1_chi_sq=stats[1,1]



scalar pvalue_1=chiprob(9, model_1_chi_sq)
disp model_1_chi_sq
disp pvalue_1


tabstat chi_square_2, statistics(sum) save
matrix stats2=r(StatTotal)
scalar  model_2_chi_sq=stats2[1,1]


scalar pvalue_2=chiprob(13, model_1_chi_sq)
disp model_2_chi_sq
disp pvalue_2