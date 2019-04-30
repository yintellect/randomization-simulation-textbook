cd "/Users/yy2633/MA/2019Spring/RA/FEDAI code STATA"
clear 
use "Camerer data for Chapter 3 exercises.dta"

* part b
ritest treatment e(F), strata(pair) reps(10000) seed(1234): regress treatment preexperimentbets


* part c
	teffects nnmatch (experimentbets pair) (treatment)
	
* part d
	bysort pair (treatment): gen diff = experimentbets - experimentbets[_n+1]
	mean(diff)

* part e
	ritest treatment _b[treatment], strata(pair) reps(10000): regress experimentbets treatment
