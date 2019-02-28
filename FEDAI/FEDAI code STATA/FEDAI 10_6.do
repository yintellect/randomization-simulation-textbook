clear

*part b
	
	set obs 43593
	egen condition = repeat(), values("No Mail")
	replace condition = "Standard" in 2587/9444
	replace condition = "Threat" in 9445/16138
	replace condition = "Norms" in 16139/22963
	replace condition = "Threat+Norms" in 22964/29923
	replace condition = "Fairness" in 29924/36843
	replace condition = "Threat+Fairness" in 36844/43593
	
	gen no_mail = 1 if strpos(condition, "Mail") > 0
	replace no_mail = 0 if no_mail ==.
	gen standard = 1 if strpos(condition, "Standard") > 0
	replace standard = 0 if standard ==.
	gen threat = 1 if strpos(condition, "Threat") > 0
	replace threat = 0 if threat ==.
	gen norms = 1 if strpos(condition, "Norms") > 0
	replace norms = 0 if norms ==.
	gen fairness = 1 if strpos(condition, "Fairness") > 0
	replace fairness = 0 if fairness ==.
	
	egen y = fill(1,1)
	replace y = 0 in 41/2586
	replace y = 0 in 3178/9444
	replace y = 0 in 10092/16138
	replace y = 0 in 16701/22963
	replace y = 0 in 23639/29923
	replace y = 0 in 30491/36843
	replace y = 0 in 37473/43593
	
	gen threatnorms = 0
	replace threatnorms = 1 if threat==1&norms==1
	gen threatfairness = 0
	replace threatfairness = 1 if threat==1&fairness==1
	
	regress y no_mail standard threat norms fairness threatnorms threatfairness, noconstant
