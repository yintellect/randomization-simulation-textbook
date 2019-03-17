
texdoc init chapter3.tex, replace

/***
\documentclass[a4paper]{article}
\usepackage{stata}
\usepackage[titletoc]{appendix}
\usepackage[margin=1in]{geometry}

\usepackage[T1]{fontenc}
\usepackage[adobe-utopia]{mathdesign}			

\begin{document}

\title{Stata Code and Result - Chapter \#3}


\section*{Question 6}
***/

texdoc stlog

// download data from: http://hdl.handle.net/10079/6hdr852
// copy and paste the url to your web browser


import delim "Clingingsmith_et_al_QJE_2009dta.csv",clear
set seed 1234567

rename success D
rename views Y


//findit tsrtest
//package name:  st0158.pkg install

cap program drop ate
program define ate, rclass
	args Y D
    sum `Y' if `D'==1, meanonly
    local Y_treat=r(mean)
    sum `Y' if `D'==0, meanonly
    local Y_con=r(mean)
    return scalar ate_avg = `Y_treat'-`Y_con'
end

// ssc install tsrtest
tsrtest D r(ate_avg) using 3_6_resam.dta, overwrite: ate Y D
  

preserve 
use "3_6_resam.dta", clear
global ate = theta[1]
di $ate
drop if _n==1
count if theta >= $ate
scalar p_onesided = r(N)/_N
count if abs(theta) >= $ate
scalar p_twosided = r(N)/_N
di "p.value.onesided = "p_onesided
di "p.value.twosided = "p_twosided 
restore

texdoc stlog close



/***
\section*{Question 7}
***/

texdoc stlog

clear
set seed 1234567

input D 
	0 
	0 
	0 
	0 
	0 
	1 
	1 
	1 
	1 
	1

	
input Y 
	1
	0
	0
	4
	3
	2
	11
	14
	0
	3



gen Y_star= Y+D*(-7)

cap program drop ate
program define ate, rclass
	args Y D
    sum `Y' if `D'==1, meanonly
    local Y_treat=r(mean)
    sum `Y' if `D'==0, meanonly
    local Y_con=r(mean)
    return scalar ate_avg = `Y_treat'-`Y_con'
end

// findit tsrtest (to install the package)
tsrtest D r(ate_avg): ate Y_star D

// ate
di r(obsvStat)       

// p.value.onesided
di r(lowertail)  

texdoc stlog close








/***
\end{document}
***/
