clear 
use "Camerer_JPEsubset_1998.dta"

* part b
	ritest treatment _b[treatment], strata(pair) reps(10000): regress preexperimentbets treatment

* part c
	teffects nnmatch (experimentbets pair) (treatment)
	
* part d
	bysort pair (treatment): gen diff = experimentbets - experimentbets[_n+1]
	mean(diff)

* part e
	ritest treatment _b[treatment], strata(pair) reps(10000): regress experimentbets treatment
