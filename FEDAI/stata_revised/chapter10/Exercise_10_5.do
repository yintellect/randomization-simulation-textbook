log using ../results/chapter10/exercise_10_5, replace

clear

/*----------------------------------------------
 part c
----------------------------------------------*/
	set obs 1000
	egen y = fill(0,0)
	replace y = 1 in 411/500
	replace y =1 in 901/1000
	egen z = fill(0,0)
	replace z = 1 in 501/1000
	egen m = fill(0,0)
	replace m = 1 in 401/500
	replace m = 1 in 801/1000
	regress y z m

log close
translate ../results/chapter10/exercise_10_5.smcl ../results/chapter10/exercise_10_5.pdf, translator(smcl2pdf)
