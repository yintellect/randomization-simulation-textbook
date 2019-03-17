
texdoc init 3_9.tex, replace

/***
\documentclass[a4paper]{article}
\usepackage{stata}
\begin{document}

\section*{Question 9}
***/

/***
\subsection*{part(b)}
***/


texdoc stlog
// download data from : http://hdl.handle.net/10079/1g1jx43
// copy and paste the url to your web browser

use "Camerer_JPEsubset_1998.dta.dta", clear 

set seed 1234567


	rename treatment D
	rename pair block
	rename preexperimentbets covs

	// calculate probs under block assignment
	bysort block: egen probs=mean(D)
	
		
	// permuation to calculate F stat and one-side P value
	ritest D e(F), strata(block) reps(10000) right nodots: ///
	regress D covs

	// p.value
	di el(r(p),1,1)
texdoc stlog close

/***
\subsection*{part(c)}
***/
texdoc stlog


	rename experimentbets change
	
	tabstat change, by(D) stat(mean) save	
	
	di "ATE ="%180.4f el(r(Stat2),1,1)-el(r(Stat1),1,1)
	
		
texdoc stlog close


/***
\subsection*{part(d)}
***/
texdoc stlog
	bysort block (D): gen pair_diff = change - change[_n+1]
	mean(pair_diff)
	
	// the same as
	// teffects nnmatch (experimentbets block) (D)
texdoc stlog close

/***
\subsection*{part(e)}
***/
texdoc stlog


	cap program drop ate_block
	
	program define ate_block, rclass
	args Y D probs
	tempvar ipw
	gen `ipw' = .
	// calculate inverse probability weight under block assignment
	replace `ipw' = `D'/`probs' + (1-`D')/(1-`probs')
	qui reg `Y' `D' [iw=`ipw']
	return scalar ate=_b[`D']
	end 
	
	
	ritest D r(ate), strata(block) reps(10000) nodots: ///
	ate_block change D probs
	
	
	// ate
	di el(r(b),1,1)

	// p.value.twosided
	di el(r(p),1,1)
texdoc stlog close	
	
	
/***
\end{document}
***/	

