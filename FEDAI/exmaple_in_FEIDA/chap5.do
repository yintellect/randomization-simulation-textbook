// data download from http://hdl.handle.net/10079/q2bvqkj

clear
import delimit Gerber_Green_APSRsubset_2005

// select one-person households that were either pure controls or canvass only
keep if onetreat==1 & mailings==0 & phongotv==0 & persons==1


/* -----------------
	Box 5.4: ITT 
------------------*/
reg v98 persngrp, vce(hc3)




/* -----------------
	Box 5.5: ITT_D 
------------------*/
reg cntany persngrp, vce(hc3)


/* -----------------
	Box 5.6: CACE 
------------------*/


ivregress 2sls v98 (cntany =persngrp), vce(robust)



