clear
set obs 12
generate m = 0 in 1/6
replace m = 1 in 7/12


generate nruns =_n
generate random = .
set seed 280352

generate frequency = 0


quietly forvalues i = 1/1000 {
replace random = uniform()
sort random
count if m != m[ n-1]
replace freq = freq + 1 if nruns == r(N)
}


sort nruns
label var nruns "# of runs"
tabdisp nruns if freq, c(freq)

set seed 12345
generate rannum = .
replace rannum = uniform()
egen m1 = cut(rannum), group(2)

quietly forvalues i = 1/100 {
replace rannum = uniform()
egen m`i' = cut(rannum), group(2)
}



macro drop _all
forvalues i=1/100 {
    global manyvars "$manyvars m`i'"
}
di "$manyvars"


foreach var1 of varlist $vt {
  foreach var2 of varlist * {
     if ("`var1'" != "`var2'") {
       local r=0
       capture corr `var1' `var2'
       if _rc==0 {
         matrix A=r(C)
         local r=A[2,1]
         if `r'>0.99 & `r'<=1  {
          di "`var1' = `var2' (corr=`r')"
       }
     }
   }
  }
}





sysuse auto, clear

* Generate a copy of a variable
gen mpg2 = mpg
gen mpg3 = mpg
gen make2 = make

* Check if two variables are identical and capture the return code
capture assert mpg == mpg2

macro drop vt
global vt mpg
global vt $vt mpg2
global vt $vt mpg3
global vt $vt make
global vt $vt make2
di "$vt"


* If the variables are identical, the scalar _rc is 0: drop one variable
* If the variables are different, the scalar _rc is 9: do nothing
if _rc == 0 {
  drop mpg2
}

macro drop dupli
di "$dupli"

foreach var1 of varlist $vt {
  foreach var2 of varlist * {
     capture assert "`var1'" == "`var2'"
	 if _rc == 0 {
	 global dupli "$dupli '`var1' = `var2' (corr=`r')'"
	 }
  }
}


unab orig: make-make2  // whatever the original and new variable lists are
local new varnameX-varnameY
// Compressing may help avoid problems with comparisons of variables
// differently typed in the two data sets
compress *
foreach o of local orig {
   foreach n of local new {
       capture assert `o' == `n'
       if (_rc ==9)  {
            drop `n'
      }
    }
}
