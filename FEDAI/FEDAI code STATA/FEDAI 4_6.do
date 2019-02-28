clear
use "OBrien_Patsiorkovski_ICPSR_1999.dta"

*part a
	reg index96 i.memberc6 i.group6 i.sexresp6 index95
	reg index96 i.memberc6 i.group6 i.sexresp6
	
*part b
	decode group6, generate(class)
	gen classpoor = 1 if (class=="poor")
	replace classpoor = 0 if classpoor==.
	gen classmiddle = 1 if (class=="middle")
	replace classmiddle = 0 if classmiddle==.
	gen classverypoor = 1 if (class=="very poor")
	replace classverypoor = 0 if classverypoor ==.
	
	ssc install randomize //user module for block randomization

	randomize, block(sexresp6 memberc6 classpoor classmiddle classverypoor index96)
