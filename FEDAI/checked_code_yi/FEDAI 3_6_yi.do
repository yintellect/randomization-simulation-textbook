cd "/Users/yy2633/MA/2019Spring/RA/FEDAI code STATA"

clear all
use "Clingingsmith subset.dta"

* two tailed p-value
ritest success _b[success], reps(10000) seed(12345): ///
	regress views success



* one-sided, kdens to plot sampling distribution under the null
ritest success _b[success], right reps(10000) seed(12345) kdens: ///
	regress views success 
