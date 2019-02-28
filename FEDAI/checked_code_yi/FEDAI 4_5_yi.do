clear
use "Teachers data for Table 4-1.dta"

* part a

drop Y
gen Y = y0*(1-D) + y1*D
scalar N = _N


regress D x
di 2*ttail(e(df_r),abs(_b[x]/_se[x]))
