texdoc init 3_7.tex, replace
/***
\documentclass[a4paper]{article}
\usepackage{stata}
\begin{document}

\section*{Question 7}
***/

texdoc stlog

clear
set seed 1234567
set obs 10

input D Y
0 1 
0 0 
0 0 
0 4 
0 3 
1 2 
1 11 
1 14 
1 0 
1 3 


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
