log using ../results/chapter04/exercise_4_10, replace


clear
clear mata
clear matrix
set matsize 11000 
set maxvar 32767
set seed 1234567


qui import delim ../data/chapter04/Arceneaux_AAAPSSsubset_2005, clear
set seed 1234567
/*----------------------------------------------
 part a
----------------------------------------------*/

rename treatment Z
rename vote03 Y
rename unit clust

global cov v_p2003 v_m2003 v_g2002 v_p2002 v_m2002 ///
 v_s2001 v_g2000 v_p2000 v_m2000 v_s1999 ///
 v_m1999 v_g1998 v_m1998 v_s1998 v_m1997 ///
 v_s1997 v_g1996 v_p1996 v_m1996 v_s1996
 


ritest Z Fstat=e(F), right clu(clust) reps(10000) nodots: reg Z $cov

// one-tail p-value
di %8.3f el(r(p),1,1)

/*----------------------------------------------
 part b
----------------------------------------------*/

// calculate the probabibility under cluster assignment

preserve
collapse Z, by(clust)   
qui sum Z
global p = r(mean)
restore

// probability of being assigned to treatment
gen probs = $p
sum probs

//ate
qui reg Y Z $cov
di "ATE= " _b[Z]

ritest Z ate_sim=_b[Z], right clu(clust) reps(10000) nodots: reg Y Z $cov

// ate
di %8.5f el(r(b),1,1)

// one-tail p-value
di %8.3f el(r(p),1,1)

/*----------------------------------------------
 part c
----------------------------------------------*/
qui gen weights = Z/probs + (1 - Z)/(1 - probs)
qui tabstat Y [aw=weights], by(Z)  stat(sum) save

scalar ateHT = (el(r(Stat2),1,1) - el(r(Stat1),1,1))/_N
di "ate_HT ="%8.5f ateHT

/*----------------------------------------------
 part d
----------------------------------------------*/

cap program drop di_in_total
program define di_in_total, rclass
tabstat Y [aw=weights], by(Z)  stat(sum) save
return scalar ateHT_sim = (el(r(Stat2),1,1) - el(r(Stat1),1,1))/_N
end

ritest Z ateHT_sim=r(ateHT_sim), right clu(clust) reps(10000) nodots: di_in_total


// one-tail p-value
di %8.3f el(r(p),1,1)



/*----------------------------------------------
 part e
----------------------------------------------*/


mkmat $cov , mat(cov_m)
matrix rowm = cov_m * J(colsof(cov_m), 1, 1/colsof(cov_m))
matrix colnames rowm=row_mean
svmat double rowm, names(col)

qui gen Y_diff = Y - row_mean


cap program drop di_in_total2
program define di_in_total2, rclass
tabstat Y_diff [aw=weights], by(Z)  stat(sum) save
return scalar ateHT_sim2 = (el(r(Stat2),1,1) - el(r(Stat1),1,1))/_N
end

ritest Z ateHT_sim=r(ateHT_sim2), right clu(clust) reps(10000) nodots: di_in_total2

// ateHT2
di %8.5f el(r(b),1,1)

// one-tail p-value
di %8.3f el(r(p),1,1)


log close
translate ../results/chapter04/exercise_4_10.smcl ../results/chapter04/exercise_4_10.pdf, translator(smcl2pdf)

