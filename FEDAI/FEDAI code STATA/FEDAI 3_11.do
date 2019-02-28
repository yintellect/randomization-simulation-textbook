clear
use "GerberGreenBook_Chapter3_Table_3_3.dta"

*part a
	gen int cluster = (village+1) / 2
	bysort cluster: egen clu_y = mean(y)
	bysort cluster: egen clu_d = mean(d)
	
	sum clu_y
	scalar avg_y = r(mean)
	scalar length_y = r(N)/2
	sum clu_d
	scalar avg_d = r(mean)
	scalar length_d = r(N)/2
	
	gen varclu_y = ((clu_y - avg_y)^2)/length_y
	gen varclu_d = ((clu_d - avg_d)^2)/length_d
	gen covclu_y = ((clu_y - avg_y)*(clu_d - avg_d))
	
	sum varclu_y
	scalar var_y = r(sum)/2
	sum varclu_d
	scalar var_d = r(sum)/2
	sum covclu_y 
	scalar cov_y = r(sum)/(2*length_y)
	
	scalar se_ate = sqrt((1/6)*((4/3)*var_y+(3/4)*var_d+2*cov_y))
	disp se_ate

* part b
	gen cluster = village
	replace cluster = 15-village if (village>7)

	bysort cluster: egen clu_y = mean(y)
	bysort cluster: egen clu_d = mean(d)
	
	sum clu_y
	scalar avg_y = r(mean)
	scalar length_y = r(N)/2
	sum clu_d
	scalar avg_d = r(mean)
	scalar length_d = r(N)/2
	
	gen varclu_y = ((clu_y - avg_y)^2)/length_y
	gen varclu_d = ((clu_d - avg_d)^2)/length_d
	gen covclu_y = ((clu_y - avg_y)*(clu_d - avg_d))
	
	sum varclu_y
	scalar var_y = r(sum)/2
	sum varclu_d
	scalar var_d = r(sum)/2
	sum covclu_y 
	scalar cov_y = r(sum)/(2*length_y)
	
	scalar se_ate = sqrt((1/6)*((4/3)*var_y+(3/4)*var_d+2*cov_y))
	disp se_ate
