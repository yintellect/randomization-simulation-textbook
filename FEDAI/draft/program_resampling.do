use "GerberGreenBook_Chapter4_Exercises_4-5.dta", clear
//Define a program to create random permuations:
cap program drop permme

program permme //define the program
syntax , ///
resampvar(varname) ///<- name of the permutation variable
strata(varname)
covariate(varname)
	* //<- ritest also passes other things to the permutation procedure (e.g. run(#))
tempvar teststat
gen `teststat' = -1
while `teststat' < 0.05{
	tempvar mr r z
	qui bys `strata': gen `r'=rnormal() 
	qui bys `strata': egen `mr' = median(`r')
	replace `z' = cond(`r',`r'>`mr',.,.)		
	qui reg `z' `covariate'
	replace `teststat' = e(F)	
	}
	replace `resampvar' = `z'
	end

cap program drop permme
program permme //define the program
syntax , ///
resampvar(varname) ///<- name of the permutation variable
covariate(varname)
tempvar teststat
gen `teststat' = -1
tempvar rannum Zri
gen `rannum'=uniform()
egen `Zri' = cut(`rannum'), group(2)
qui reg `Zri' x
replace `teststat' = e(F)		
replace `resampvar' = `z' if `teststat' >= 0.05
end

gen id = _n
//Call ritest using the program:
        ritest D F=e(F), reps(100) samplingprogram(permme) ///
		sav(f_restricted.dt)  saveresampling(restricted_ds.dta) ///
        samplingprogramoptions("covariate(x)") : ///
        reg D x

// This method can be used implement any kind of re-randomization.

cd "/Users/yy2633/MA/2019Spring/RAship/FEDAI/draft"
use example_students.dta, clear


		


cap program drop permme
  program permme //define the program
     syntax , ///
  resampvar(varname) ///<- name of the permutation variable
  school(varname) ///<- name of the strata variable
  tempvar mr r z
     // draw one random variable per cluster
  qui bys `school': gen `r'=rnormal() // draw one random variable per cluster
     // compute median of these random variables within cluster
  qui bys `school': egen `mr' = median(`r')
  gen `z' = cond(`r',`r'>`mr',.,.) 
     // replace the permutation var with the new randomization outcome
  replace `resampvar' = `z'
  end

	 
	                  
   // Call ritest using the program:
        ritest treatment r(N), r(50) samplingprogram(permme) ///
        samplingprogramoptions("school(classid)") saveresampling(what.dta): ///
        summarize treatment
use whatx.dta, clear

