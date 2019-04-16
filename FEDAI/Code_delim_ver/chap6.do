

// data download from http://hdl.handle.net/10079/kh189dd


import  delimit Mullainathan_et_al_BookChapterSubset_2010,clear

rename watch ASSIGNED
rename watchdps TREATED
rename ochange Y


/* -----------------
	Box 6.3: ITT_D 
------------------*/
reg TREATED ASSIGNED


/* -----------------
	Box 6.4: ITT 
------------------*/
reg Y ASSIGNED


/* -----------------
	Box 6.5: CACE 
------------------*/
ivregress 2sls Y (TREATED =ASSIGNED)


/* -----------------
	Box 6.6: 
------------------*/

// data download from http://hdl.handle.net/10079/q573nh7
import delim GerberGreenBook_Chapter6_Simulated_Downstream_Data, clear

keep z d y wt
expand wt
drop if wt ==0

reg y d z

