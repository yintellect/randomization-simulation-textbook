
* basic tables for chapter 9
* Table 9.4
bys  Hispanic   : tab  Response Colin_Jose ,col chi

bys  Colin_Jose : tab  Response Good_Bad_Grammar ,col chi

* Table 9.5
bys  Hispanic Colin_Jose  : tab  Response Good_Bad_Grammar ,col chi


* regressions for chapter 
gen HispXJose= Hispanic* Colin_Jose
gen HispXBadGrammar= Hispanic*Good_Bad_Grammar
gen JoseXBadGrammar=Colin_Jose*Good_Bad_Grammar
gen HispXJoseXBadGrammar=Hispanic*Colin_Jose*Good_Bad_Grammar
* one interaction
reg   Response   Colin_Jose    Hispanic HispXJose 
* all 2-way interactions
reg   Response   Colin_Jose    Good_Bad_Grammar Hispanic HispXJose HispXBadGrammar JoseXBadGrammar 
* fully saturated model
reg   Response   Colin_Jose    Good_Bad_Grammar Hispanic HispXJose HispXBadGrammar JoseXBadGrammar HispXJoseXBadGrammar 
