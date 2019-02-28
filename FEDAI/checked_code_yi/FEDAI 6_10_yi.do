clear
use "Chapter 6_Hyde (2010) Dataset.dta"

*part a
	regress invalidballots Sample
	scalar ITT = _b[Sample]		
	regress observed Sample
	scalar ITTD = _b[Sample]		
	scalar CACE = ITT/ITTD
	disp ITT		
	disp CACE
		
	ritest Sample _b[Sample], reps(10000): regress invalidballots Sample
