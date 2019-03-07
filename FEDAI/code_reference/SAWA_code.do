********************************************************************************
*					Code to produce tables, appendix tables and figure for:					   *
*			TARGETING HEALTH SUBSIDIES THROUGH A NON-PRICE MECHANISM:		   *
*						A RANDOMIZED CONTROLLED TRIAL						   *
*																			   *
*	  PASCALINE DUPAS, VIVIAN HOFFMAN, MICHAEL KREMER, ALIX PETERSON ZWANE	   *
*																			   *
*						 Last updated: June 25, 2016						   *
********************************************************************************

clear all
capture log close
scalar drop _all
mat drop _all
macro drop _all
set mem 100m
set more off


/* Contents: 

** Table 1 (line 114)
** Table 2 (line 176)
** Table 3 (line 208)
** Table S1 (line 272)
** Table S2 (line 315)
** Table S3 (line 336)
** Table S4 (line 423)
** Table S5 (line 443)
** Figure S1 (line 634)

*/

*SET DIRECTORY

global indir "C:\Users\echuang\Dropbox\Erica-Pascaline Shared Folder\SAWA\SAWA Dataverse Files"
global indir "C:\Users\Pascaline Dupas\Dropbox\Erica-Pascaline Shared Folder\SAWA\SAWA Dataverse Files"
global outdir "${indir}\Outputs\"
global ssc "${indir}\ado"

*Import Data

use "${indir}\SAWA_data.dta", clear

*Call user-written commands folder from Dataverse

sysdir set PLUS "${ssc}"


/*----------------------------------------------
 DATA CLEANING AND PREP
----------------------------------------------*/

		***** TREATMENT VARIABLES
			*A = COST SHARING
			*B = VOUCHERS (12-MO supply)
			*C= FREE DELIVERY (12-MO supply)
			*D= BABY BOTTLE (EXCLUDED FROM ANALYSIS IN THIS PAPER)
			*E= FREE SAMPLE (EXCLUDED FROM ANALYSIS IN THIS PAPER)


			/*Radius of the earth in kilometers*/
			local R=6371
			
			gen shoptoclinicft=3280.84*shoptoclinic
			tab shoptoclinicft
			tab shoptoclinic
			* within a few hundred feet is accurate (max is 986 feet)
			* can also say w/in a third of a km

		** calculate number of kids in HH (for calibration) **

			forval i=1/13 {
				replace fol1_4e_age_yrs`i'=0 if fol1_4e_age_yrs1==. & fol1_4e_age_mths1<12
				g under5hh_`i'=1 if fol1_c3a`i'_child_lived_hh==1 & fol1_4e_age_yrs`i'<5
				g under5comp`i'=1 if fol1_c3a`i'_child_lived_hh==2 & fol1_4e_age_yrs`i'<5
				replace under5comp`i'=1 if fol1_c3a`i'_child_lived_hh==1 & fol1_4e_age_yrs`i'<5
			}

			forval i=20/23 {
				replace fol1_4e_age_yrs`i'=0 if fol1_4e_age_yrs1==. & fol1_4e_age_mths1<12
				g under5hh_`i'=1 if fol1_c3b`i'_child_live_hh==1 & fol1_4e_age_yrs`i'<5
				g under5comp`i'=1 if fol1_c3b`i'_child_live_hh==2 & fol1_4e_age_yrs`i'<5
				replace under5comp`i'=1 if fol1_c3b`i'_child_live_hh==1 & fol1_4e_age_yrs`i'<5
			}

			egen totalu5sHH=rsum(under5hh_*)
			egen totalu5scomp=rsum(under5comp*)
			sum totalu5sHH totalu5scomp if wave==2, detail
			g flag_no_u5_table=1 if totalu5sHH==0 & bl_a10_un==1 & wave==2  
			replace totalu5sHH=1 if totalu5sHH==0 & bl_a10_un==1 & wave==2  

		**** use this number for calibration:
			sum totalu5sHH if wave==2 & treatment!="D" & treatment!="E", detail
			sum totalu5sHH if wave==2, detail
			* note these are all in wave 2. no data on HH vs compound for wave 1.

			recode bl_c4_chi (99=.)
			g children=bl_c4_chi
			sum children if fol1_attrited!=1 & treatment!="D" & treatment!="E", detail
			g miss_children=1 if children==.
			recode miss_children (.=0)

			rename bl_f1_diarrhea_past_7days bl_f1_diar
			recode bl_f1_diar (2=0)
			gen miss_bl_f1_diar=(bl_f1_diar==.)


global indepvar "age years_educ sickvisit distfromclinic walkedtoclinic lastbirth_hosp bl_c14_child_net bl_c16_boil_water bl_a1_use_waterguard bl_a9_buy_waterguard"
global controls "miss_age miss_years_educ miss_sickvisit miss_distfromclinic miss_walkedtoclinic miss_lastbirth_hosp miss_bl_c14_child_net miss_bl_c16_boil_water miss_bl_a1_use_waterguard miss_bl_a9_buy_waterguard"


/*----------------------------------------------
 TABLE 1: TAKE-UP OF INTERVENTION;
----------------------------------------------*/

mat T1_col=J(2,7,0)


* Col 1-3: cost-sharing (treatment A)

	replace wgbought=0 if wgbought==. & treatment=="A"
	gen wgbought_any=(wgbought>0) if wgbought!=. & treatment=="A"
	gen wgbought_2=(wgbought>=2) if wgbought!=. & treatment=="A"
	gen wgbought_3=(wgbought>=3) if wgbought!=. & treatment=="A"
	gen share_wgbought=wgbought/5
	*Col 1
	sum wgbought_any  if treatment=="A"  
	mat T1_col[1,1]=r(mean)
	mat T1_col[2,1]=r(N)

	*Col 2
	sum wgbought_2 if treatment=="A"
	mat T1_col[1,2]=r(mean)
	mat T1_col[2,2]=r(N)

	*Col3
	sum share_wgbought  if treatment=="A"
	mat T1_col[1,3]=r(mean)
	mat T1_col[2,3]=r(N)

* Col 4-6: vouchers (treatment B)
	gen coupon_2=(num_coupons>=2) if num_coupons!=. & treatment=="B"
	gen redeemed_proportion=(num_coupons)/12
	
	*Col 4
	sum coupon_any if treatment=="B"
	mat T1_col[1,4]=r(mean)
	mat T1_col[2,4]=r(N)

	*Col 5
	sum redeemed_fol1_prev if treatment=="B"
	mat T1_col[1,5]=r(mean)
	mat T1_col[2,5]=r(N)
	
	*Col 6
	sum redeemed_proportion if treatment=="B"
	mat T1_col[1,6]=r(mean)
	mat T1_col[2,6]=r(N)

	
* Col 7: free delivery
* Everyone took free big bottle at clinic when offered
	mat T1_col[1,7]=1
	tab clinic if treatment=="C"
	mat T1_col[2,7]=r(N)

preserve
	svmat T1_col
	keep T1_col1-T1_col7
	outsheet using "${outdir}/sawa_table1.out", replace
restore
		

/*----------------------------------------------
 TABLE 2: CHLORINE USE BY TREATMENT, SHORT-TERM FOLLOW-UP
----------------------------------------------*/
	sum fol1_chlorine_positive ${indepvar} ${controls} C B clinic wave fol1_gap

	* Calculating adjusted means and SEs
		loc treat A B C
		foreach x in `treat' {
		tabstat fol1_chlorine_positive if treatment=="`x'", by(treatment) stat(mean semean N) save 
			mat `x'=r(Stat1)
			scalar t2raw`x'mean=`x'[1,1]
			scalar t2raw`x'SE=`x'[2,1]
			scalar t2raw`x'N=`x'[3,1]
		}
		
	eststo: xi: reg fol1_chlorine_positive ${indepvar} ${controls} C B i.clinic i.wave fol1_gap if treatment!="D" & treatment!="E"
		*Storing coeffs and SEs
		loc treat2 B C
		foreach y in `treat2' {
			loc t2adj`y'mean=_b[`y']
			loc t2adj`y'SE=_se[`y']
		}
		
		*Exporting adjusted results
		putexcel B1=("(1)") C1=("(2)") D1=("(3)") using "${outdir}\sawa_table2", sheet("table2") replace
		putexcel B2=("COST SHARING") C2=("VOUCHER") D2=("FREE DELIVERY") using "${outdir}\sawa_table2", sheet("table2") modify
		putexcel A3=("Raw Means") B3=(t2rawAmean) C3=(t2rawBmean) D3=(t2rawCmean) using "${outdir}\sawa_table2", sheet("table2") modify
		putexcel B4=(t2rawASE) C4=(t2rawBSE) D4=(t2rawCSE) using "${outdir}\sawa_table2", sheet("table2") modify
		putexcel A5=("Adjusting for baseline controls") B5=(t2rawAmean) C5=(`t2adjBmean'+t2rawAmean) D5=(`t2adjCmean'+t2rawAmean) using "${outdir}\sawa_table2", sheet("table2") modify
		putexcel B6=(t2rawASE) C6=(sqrt((`t2adjBSE')^2+(t2rawASE^2))) D6=(sqrt((`t2adjCSE')^2+(t2rawASE^2))) using "${outdir}\sawa_table2", sheet("table2") modify

		
/*----------------------------------------------
TABLE 3: REDEMPTION AND CHLORINE USE BY MAGNITUDE OF ORDEAL
----------------------------------------------*/

tabstat redeemed_fol1_prev fol1_chlorine_positive if treatment=="B", by(shopcloser) stat(mean semean N) save 
	foreach x of numlist 1/2{
		mat redeem`x'=r(Stat`x')
		scalar redeem`x'mean=redeem`x'[1,1]
		scalar redeem`x'SE=redeem`x'[2,1]
		scalar redeem`x'N=redeem`x'[3,1]
		
		mat clpos`x'=r(Stat`x')
		scalar clpos`x'mean=clpos`x'[1,2]
		scalar clpos`x'SE=clpos`x'[2,2]
		scalar clpos`x'N=clpos`x'[3,2]
		
		*Sorry this loop made it easier to define scalars but for the matrices above r(Stat1)="0" & r(Stat2)="1"
	}

* do ttests and input results manually

ttest redeemed_fol1_prev if treatment=="B", by(shopcloser)
ttest fol1_chlorine_positive if treatment=="B", by(shopcloser)

eststo: xi: reg redeemed_fol1_prev shopcloser if treatment=="B"
		loc redeem_diffmean=_b[shopcloser]
		loc redeem_diffSE=_se[shopcloser]
		test shopcloser
		loc redeem_diffP=r(p)
eststo: xi: reg redeemed_fol1_prev shopcloser ${indepvar} ${controls} i.clinic i.wave fol1_gap if treatment=="B"
		loc redeem_diffadjmean=_b[shopcloser]
		loc redeem_diffadjSE=_se[shopcloser]
		test shopcloser
		loc redeem_diffadjP=r(p)		
eststo: xi: reg fol1_chlorine_positive shopcloser if treatment=="B"
		loc clpos_diffmean=_b[shopcloser]
		loc clpos_diffSE=_se[shopcloser]
		test shopcloser
		loc clpos_diffP=r(p)
eststo: xi: reg fol1_chlorine_positive shopcloser ${indepvar} ${controls} i.clinic i.wave fol1_gap if treatment=="B"
		loc clpos_diffadjmean=_b[shopcloser]
		loc clpos_diffadjSE=_se[shopcloser]
		test shopcloser
		loc clpos_diffadjP=r(p)

* Export all results to Excel
	putexcel B1=("(1)") C1=("(2)") using "${outdir}\sawa_table3", replace
	putexcel B2=("Proportion redeemed voucher month of survey") C2=("Proportion with positive chlorine test") using "${outdir}\sawa_table3",  modify
	putexcel A3=("Vouchers redeemable at nearest market") B3=(redeem2mean) C3=(clpos2mean) using "${outdir}\sawa_table3", modify
	putexcel B4=(redeem2SE) C4=(clpos2SE) using "${outdir}\sawa_table3",  modify
	putexcel A5=("Observations") B5=(redeem2N) C5=(clpos2N) using "${outdir}\sawa_table3", modify
	putexcel A6=("Not redeemable at nearest market") B6=(redeem1mean) C6=(clpos1mean) using "${outdir}\sawa_table3", modify
	putexcel B7=(redeem1SE) C7=(clpos1SE) using "${outdir}\sawa_table3", modify
	putexcel A8=("Observations") B8=(redeem1N) C8=(clpos1N) using "${outdir}\sawa_table3", modify
	putexcel A9=("Difference of means") B9=(`redeem_diffmean') C9=(`clpos_diffmean') using "${outdir}\sawa_table3", modify
	putexcel A10=("P-value, unadjusted difference of means") B10=(`redeem_diffP') C10=(`clpos_diffP') using "${outdir}\sawa_table3", modify
	putexcel A11=("Difference of adjusted means") B11=(`redeem_diffadjmean') C11=(`clpos_diffadjmean') using "${outdir}\sawa_table3", modify
	putexcel A12=("P-value, difference of means") B12=(`redeem_diffadjP') C12=(`clpos_diffadjP') using "${outdir}\sawa_table3", modify
	
tab clinic, gen(clinic)		
global stratvars clinic1 clinic2 clinic3 clinic4 wave fol1_gap



/*----------------------------------------------
 TABLE S1: BASELINE SUMMARY STATISTICS
----------------------------------------------*/

#delimit ;

cap gen var="";
for any A_mean A_sd C_mean C_sd B_mean B_sd 
		BA_pval CA_pval CB_pval N: cap gen X=.;
local vars=13;
for any sickvisit distfromclinic walkedtoclinic 
femaleresp age years_educ 
 lastbirth_hosp	bl_c14_child_net bl_c16_boil_water 	bl_a1_use_waterguard 
bl_a9_buy_waterguard  bl_f1_diar obs 
\ num 1/`vars':
 cap gen miss_X=0 \
 replace var="X" if _n==Y \
 xi: reg X A C B D i.wave i.clinic if miss_X==0 \
	replace N=e(N) if _n==Y \
 	mat beta=e(b) \
	 test B=A \
		 replace BA_pval=r(p) if _n==Y \
	 test C=A \
		 replace CA_pval=r(p) if _n==Y \
	 test C=B \
		 replace CB_pval=r(p) if _n==Y \
 sum X if A==1 & miss_X==0\
	 replace A_mean=r(mean) if _n==Y \
	 replace A_sd=r(sd) if _n==Y \
 sum X if B==1 & miss_X==0\
	 replace B_mean=r(mean) if _n==Y \
	 replace B_sd=r(sd) if _n==Y \
 sum X if C==1 & miss_X==0\
	 replace C_mean=r(mean) if _n==Y \
	 replace C_sd=r(sd) if _n==Y \;
for var A_mean-C_sd: replace X=round(X, 0.01);
for var BA_pval-CB_pval: replace X=round(X, 0.001);
outsheet var A_mean-CB_pval N  using "${outdir}\sawa_tableS1.csv" if _n<=`vars', comma replace;
//outsheet var A_mean-BE_pval N  using "${outdir}\sawa_tableS1.csv" if _n<=`vars', comma replace;
drop A_mean-CB_pval N;
#delimit cr


/*----------------------------------------------;
 TABLE S2: WEALTH AND TAKE-UP;
----------------------------------------------*/

* omitting walking to clinic from controls for these regressions
global indepvar_wealth "age years_educ sickvisit distfromclinic lastbirth_hosp bl_c14_child_net bl_c16_boil_water bl_a1_use_waterguard bl_a9_buy_waterguard"
global controls_wealth "miss_age miss_years_educ miss_sickvisit miss_distfromclinic miss_lastbirth_hosp miss_bl_c14_child_net miss_bl_c16_boil_water miss_bl_a1_use_waterguard miss_bl_a9_buy_waterguard"

est clear 
xi: reg wgbought_any assetindex_simple ${indepvar_wealth} ${controls_wealth} i.clinic i.wave 
outreg2 using "${outdir}/sawa_tableS2", keep(assetindex_simple) se nonote symbol(***,**,*)  bdec(3) excel  replace

foreach indvar in wgbought_2 wgbought {
	eststo: xi: reg `indvar' assetindex_simple i.clinic i.wave ${indepvar_wealth} ${controls_wealth} if treatment=="A"
	outreg2 using "${outdir}/sawa_tableS2", keep(assetindex_simple) se nonote symbol(***,**,*)  bdec(3) excel  append
}
foreach indvar in coupon_any coupon_2 num_coupons {
	eststo: xi: reg `indvar' assetindex_simple i.clinic i.wave ${indepvar_wealth} ${controls_wealth} if treatment=="B"
	outreg2 using "${outdir}/sawa_tableS2", keep(assetindex_simple) se nonote symbol(***,**,*)  bdec(3) excel append 
}

/*----------------------------------------------
 TABLE S3: CHLORINE USE BY TREATMENT AND TIME SINCE ENROLLMENT
----------------------------------------------*/
global indepvar2 "sickvisit distfromclinic walkedtoclinic age years_educ assetindex_simple lastbirth_hosp bl_c14_child_net bl_c16_boil_water bl_a1_use_waterguard bl_a9_buy_waterguard  bl_f1_diar"
global controls2 "miss_sickvisit miss_distfromclinic miss_walkedtoclinic miss_age miss_years_educ  miss_lastbirth_hosp miss_bl_c14_child_net miss_bl_c16_boil_water miss_bl_a1_use_waterguard miss_bl_a9_buy_waterguard miss_bl_f1_diar"

**Building month gap variable (# of months between BL and FO)
	g month_gap=fol1_gap/30.5
	g month_gap_d=round(month_gap)

forval i=3/5 {
	ttest fol1_chlorine_positive if (treatment=="A" | treatment=="B") & month_gap_d==`i', by(treatment)
	local AB_pval_`i'mo=r(p)
	ttest fol1_chlorine_positive if (treatment=="A" | treatment=="C") & month_gap_d==`i', by(treatment)
	local AC_pval_`i'mo=r(p)
	ttest fol1_chlorine_positive if (treatment=="B" | treatment=="C") & month_gap_d==`i', by(treatment)
	local BC_pval_`i'mo=r(p)
	matrix define pval_`i'mo = (`AB_pval_`i'mo',`AC_pval_`i'mo',`BC_pval_`i'mo')
}

loc treat A B C
foreach y of numlist 3/5{
	foreach x in `treat' {
	tabstat fol1_chlorine_positive if treatment=="`x'" & month_gap_d==`y', by(treatment) stat(mean semean N) save 
		mat `x'`y'=r(Stat1)
		mat list `x'`y'
		scalar ts3raw`x'mean`y'=`x'`y'[1,1]
		di ts3raw`x'mean`y'
		scalar ts3raw`x'SE`y'=`x'`y'[2,1]
		di ts3raw`x'SE`y'
		scalar ts3raw`x'N`y'=`x'`y'[3,1]
		di ts3raw`x'N`y'
	}
}

**Results by month
	forval i=3/5 {
		eststo: xi: reg fol1_chlorine_positive C B i.clinic i.wave fol1_gap ${indepvar2} ${controls2} if month_gap_d==`i' & treatment!="D"  & treatment!="E"
		*running tests and saving with command local
		local diff=_b[C]-_b[B]
		test C=B
		local ptest3=r(p)
		loc treat2 B C
		foreach x in `treat2' {
			loc ts3adj`x'mean`i'=_b[`x']
			loc ts3adj`x'SE`i'=_se[`x']
		}
		sum fol1_chlorine_positive if treatment=="A" & month_gap_d==`i'
		estadd scalar Mean=r(mean)
	}

	* now get p-values
	forval i=3/5 {
		eststo: xi: reg fol1_chlorine_positive C B i.clinic i.wave fol1_gap ${indepvar2} ${controls2} if month_gap_d==`i' & treatment!="D"  & treatment!="E"
		mat p=r(table)
		scalar pC`i'=p[4,1]
		di pC`i'
		scalar pB`i'=p[4,2]
		di pB`i'
		*running tests and saving with command local
		local diff=_b[C]-_b[B]
		test C=B
		local ptest`i'=r(p)
		sum fol1_chlorine_positive if treatment=="A" & month_gap_d==`i'
		estadd scalar Mean`i'=r(mean)
		scalar Mean`i'=e(Mean`i')
	}
	
	*Exporting all results
		putexcel B1=("(1)") C1=("(2)") D1=("(3)") E1=("(4)") F1=("(5)") G1=("(6)") H1=("(7)") using "${outdir}\sawa_tableS3", replace
		putexcel B2=("COST SHARING") C2=("VOUCHER") D2=("FREE DELIVERY") E2=("Observations") F2=("P-value,PRICE vs VOUCHERS") G2=("P-value,PRICE vs FREE DELIVERY") H2=("P-value, VOUCHERS vs. FREE DELIVERY") using "${outdir}\sawa_tableS3", modify
		putexcel A3=("Surveyed 3 months after enrollment")  E3=(ts3rawAN3+ts3rawBN3+ts3rawCN3) using "${outdir}\sawa_tableS3", modify
		putexcel A4=("Raw means") B4=(ts3rawAmean3) C4=(ts3rawBmean3) D4=(ts3rawCmean3) F4=(`AB_pval_3mo') G4=(`AC_pval_3mo') H4=(`BC_pval_3mo') using "${outdir}\sawa_tableS3", modify
		putexcel B5=(ts3rawASE3) C5=(ts3rawBSE3) D5=(ts3rawCSE3) using "${outdir}\sawa_tableS3", modify
		putexcel A6=("Adjusting for baseline controls") B6=(Mean3) C6=(ts3rawAmean3+`ts3adjBmean3') D6=(ts3rawAmean3+`ts3adjCmean3') F6=(pB3) G6=(pC3) H6=(`ptest3') using "${outdir}\sawa_tableS3", modify
		putexcel B7=(ts3rawASE3) C7=(sqrt((ts3rawASE3)^2+(`ts3adjBSE3')^2)) D7=(sqrt((ts3rawASE3)^2+(`ts3adjCSE3')^2)) using "${outdir}\sawa_tableS3", modify
		putexcel A8=("Surveyed 4 months after enrollment")  E8=(ts3rawAN4+ts3rawBN4+ts3rawCN4) using "${outdir}\sawa_tableS3", modify
		putexcel A9=("Raw means") B9=(ts3rawAmean4) C9=(ts3rawBmean4) D9=(ts3rawCmean4) F9=(`AB_pval_4mo') G9=(`AC_pval_4mo') H9=(`BC_pval_4mo') using "${outdir}\sawa_tableS3", modify
		putexcel B10=(ts3rawASE4) C10=(ts3rawBSE4) D10=(ts3rawCSE4) using "${outdir}\sawa_tableS3", modify
		putexcel A11=("Adjusting for baseline controls") B11=(Mean4) C11=(ts3rawAmean4+`ts3adjBmean4') D11=(ts3rawAmean4+`ts3adjCmean4') F11=(pB4) G11=(pC4) H11=(`ptest4') using "${outdir}\sawa_tableS3", modify
		putexcel B12=(ts3rawASE4) C12=(sqrt((ts3rawASE4)^2+(`ts3adjBSE4')^2)) D12=(sqrt((ts3rawASE4)^2+(`ts3adjCSE4')^2)) using "${outdir}\sawa_tableS3", modify
		putexcel A13=("Surveyed 5 months after enrollment")  E13=(ts3rawAN5+ts3rawBN5+ts3rawCN5) using "${outdir}\sawa_tableS3", modify 
		putexcel A14=("Raw means") B14=(ts3rawAmean5) C14=(ts3rawBmean5) D14=(ts3rawCmean5) F14=(`AB_pval_5mo') G14=(`AC_pval_5mo') H14=(`BC_pval_5mo') using "${outdir}\sawa_tableS3", modify
		putexcel B15=(ts3rawASE5) C15=(ts3rawBSE5) D15=(ts3rawBSE5) using "${outdir}\sawa_tableS3", modify
		putexcel A16=("Adjusting for baseline controls") B16=(Mean5) C16=(ts3rawAmean5+`ts3adjBmean4') D16=(ts3rawAmean5+`ts3adjCmean5') F16=(pB5) G16=(pC5) H16=(`ptest4') using "${outdir}\sawa_tableS3", modify
		putexcel B17=(ts3rawASE5) C17=(sqrt((ts3rawASE5)^2+(`ts3adjCSE5')^2)) D17=(sqrt((ts3rawASE5)^2+(`ts3adjCSE4')^2)) using "${outdir}\sawa_tableS3", modify

/*----------------------------------------------
 TABLE S4: TEST FOR DIFFERENCES IN DETERMINANTS OF CHLORINE USAGE BY TREATMENT
----------------------------------------------*/

est clear
global indep " "
global indep_B " "
foreach x in $indepvar2 {
gen `x'_B=`x'*B
global indep_B " ${indep_B} `x'_B"
global indep " ${indep} `x' `x'_B"
}

xi: reg fol1_chlorine_positive ${indep} ${controls2} i.clinic i.wave fol1_gap B  if treatment=="C"|treatment=="B"
test ${indep_B}
local joint_p=r(p)
 outreg2 using "${outdir}/sawa_tableS4", keep(${indep}) se nonote symbol(***,**,*)  bdec(3) excel replace addstat("Joint P", `joint_p')
drop *_B


/*----------------------------------------------
 TABLE S5: WHAT RESPONDENTS DID WITH THE BOTTLES
----------------------------------------------*/

*Currently has non-empty Aquaguard bottle on compound
	replace fol1_e23a_shown_aquargard_bottle=0 if (treatment=="C" & fol1_4_treatment=="B") | (treatment=="C" & fol1_4_treatment=="E")
	replace fol1_e23a_shown_aquargard_bottle=0 if fol1_e23a_shown_aquargard_bottle==. & treatment=="C"
	g nonempty=0 if fol1_attrited==0 
	la var nonempty "=1 if resp showed aquaguard bottle & there is still some cl in it"
	replace nonempty=1 if fol1_e23a_shown_aquargard_bottle==1 & fol1_e23c_fullempty<5 & treatment=="C" 
	
	**Storing results in scalars
	**By month
	loc treat A B C
	foreach y of numlist 3/5{
		foreach x in `treat' {
			tabstat nonempty if treatment=="`x'" & month_gap_d==`y', by(treatment) stat(mean semean N) save 
			mat `x'`y'=r(Stat1)
			mat list `x'`y'
			scalar ne`x'mean`y'=`x'`y'[1,1]
			di ne`x'mean`y'
			scalar ne`x'SE`y'=`x'`y'[2,1]
			di ne`x'SE`y'
			scalar ne`x'N`y'=`x'`y'[3,1]
			di ne`x'N`y'
		}
	}
	
	**Overall	
	loc treat A C
	foreach x in `treat' {
		ttest nonempty if `x'==1|B==1, by(B)
		scalar ne`x'allmean=r(mu_1)
			di ne`x'allmean
		scalar ne`x'allSE=r(se)
			di ne`x'allSE
		scalar ne`x'allN=r(N_1)
			di ne`x'allN
	}
		ttest nonempty if B==1|C==1, by(C)
		scalar neBallmean=r(mu_1)
			di neBallmean
		scalar neBallSE=r(se)
			di neBallSE
		scalar neBallN=r(N_1)
			di neBallN
	
*Reports giving bottle/coupons away
	gen gave_away=0 if fol1_attrited==0
	replace gave_away=1 if fol1_e19b_gave_within_cmpd==1|fol1_e19c_gave_outside_cmpd==1|fol1_e21b_gav==1|fol1_e21c_gav==1
	la var gave_away "=1 if resp reported giving the wg bottle/coupon away"
	recode gave_away (.=0) if fol1_e19b_gave_within_cmpd==2|fol1_e19c_gave_outside_cmpd==2|fol1_e21b_gav==2|fol1_e21c_gav==2
	replace gave_away=1 if fol1_e23b_why_not=="GAVE AQUAGUARD TO HER PARENTS-IN-LAW"
	replace gave_away=1 if fol1_e23b_why_not=="GAVE IT AWAY WITH AQUAGUARD IN IT TO OTHERS IN ANOTHER COMPOUND"
	replace gave_away=1 if fol1_e23b_why_not=="GAVE SOMEONE ELSE IN ANOTHER COMPOUND WITH AQUAGUARD IN IT"
	replace gave_away=1 if fol1_e21c_gave_cpns_outside_cmpd==1 |fol1_e21b_gave_cpns_within_cmpd==1
	replace gave_away=1 if fol1_e22c_gave_cpns_outside_cmpd==1 |fol1_e22b_gave_cpns_within_cmpd==1
	
	**Storing results in scalars
	**By Month
	loc treat A B C
	foreach y of numlist 3/5{
		foreach x in `treat' {
			tabstat gave_away if treatment=="`x'" & month_gap_d==`y', by(treatment) stat(mean semean N) save 
			mat `x'`y'=r(Stat1)
			mat list `x'`y'
			scalar ga`x'mean`y'=`x'`y'[1,1]
			di ga`x'mean`y'
			scalar ga`x'SE`y'=`x'`y'[2,1]
			di ga`x'SE`y'
			scalar ga`x'N`y'=`x'`y'[3,1]
			di ga`x'N`y'
		}
	}
					
	**Overall	
	loc treat A C
	foreach x in `treat' {
		ttest gave_away if `x'==1|B==1, by(B)
		scalar ga`x'allmean=r(mu_1)
		scalar ga`x'allSE=r(se)
		scalar ga`x'allN=r(N_1)
	}
		ttest gave_away if B==1|C==1, by(C)
		scalar gaBallmean=r(mu_1)
		scalar gaBallSE=r(se)
		scalar gaBallN=r(N_1)
	
	
	
*Declares running out of chlorine
	g finishedbottle=0
	la var finishedbottle "=1 if resp finished the wg by follow-up 1 (incl why bottle not shown to FO)"
	replace finishedbottle=1 if fol1_e23a_shown_aquargard_bottle==1 & fol1_e23c_fullempty==5
	replace finishedbottle=1 if fol1_e19f==1
			
	**Adding obs from strings that weren't recorded in either e23a or e19f
			replace finishedbottle=1 if fol1_e23b_why_not=="BOTTLE TRHOWN AWAY AFTER FINISHING THE PRODUCT"
			replace finishedbottle=1 if fol1_e23b_why_not=="BOTTLE WAS THROWN AWAY AFTER FINISHING THE AQUARGUARD"
			replace finishedbottle=1 if fol1_e23b_why_not=="BOTTLE WAS THROWN AWAY WHEN AQUARGUARD GOT FINISHED"
			replace finishedbottle=1 if fol1_e23b_why_not=="DIDN'T KEEP BOTTLE AFTER AQUAGUARD RAN OUT"
			replace finishedbottle=1 if fol1_e23b_why_not=="DIDN'T KEEP BOTTLE WHEN AQUAGUARD GOT FINISHED"
			replace finishedbottle=1 if fol1_e23b_why_not=="EMPTY BOTTLE GOT LOST BY CHILDREN WHILE PLAYING"
			replace finishedbottle=1 if fol1_e23b_why_not=="FINISHED IN AUGUST(MID) 2008 AND BOTTLE THROWN."
			replace finishedbottle=1 if fol1_e23b_why_not=="GOT FINISHED AND AQUAGUARD BOTTLE LOST."
			replace finishedbottle=1 if fol1_e23b_why_not=="GOT FINISHED THEN KIDS TOOK BOTTLE TO PLAY WITH"
			replace finishedbottle=1 if fol1_e23b_why_not=="LOST THE BOTTLE AFTER AQUARGUARD RUN OUT"
			replace finishedbottle=1 if fol1_e23b_why_not=="RESPONDENT IN BASELINE THREW THE BOTTLE AWAY AFTER USING THE AQUAGUARD"
			replace finishedbottle=1 if fol1_e23b_why_not=="RESPONDENT SAID THAT THE BOTTLE WAS BURNT. THEY BURN ALL PLASTIC CONTAINERS THAT ARE EMPTY"
			replace finishedbottle=1 if fol1_e23b_why_not=="THAT AQUAGUARD GOT FINISHED AND CHILDREN LOST THE BOTTLE"
			replace finishedbottle=1 if fol1_e23b_why_not=="THE BOTTLE WAS THROWN AWAY AS SOON AS THE AQUAGUARD GOT FINISHED"
			replace finishedbottle=1 if fol1_e23b_why_not=="THE RESPONDENT DISPOSED THE CONTAINER BECAUSE IT WAS EMPTY"
			replace finishedbottle=1 if fol1_e23b_why_not=="THREW AWAY AFTER USING"
			replace finishedbottle=1 if fol1_e23b_why_not=="THREW AWAY BOTTLE WHEN AQUAGUARD GOT FINISHED."
			replace finishedbottle=1 if fol1_e23b_why_not=="THREW AWAY BOTTLE WHEN IT GOT FINISHED"
			replace finishedbottle=1 if fol1_e23b_why_not=="THREW AWAY BOTTLE WHEN IT GOT FINISHED."
			replace finishedbottle=1 if fol1_e23b_why_not=="THREW AWAY THE EMPTY BOTTLED."
			replace finishedbottle=1 if fol1_e23b_why_not=="THREW THE BOTTLE AWAY WHEN THE AQUAGUARD GOT FINISHED"
			replace finishedbottle=1 if fol1_e23b_why_not=="WAS DISPOSED OFF AND BURNT WHEN THE AQUAGUARD WAS FINISHED"
			replace finishedbottle=1 if fol1_e23b_why_not=="WAS GIVEN AWAY WHEN THE AQUAGUARD GOT FINISHED"
			replace finishedbottle=1 if fol1_e23b_why_not=="WHEN AQUAGUARD GOT FINISHED, CHILDREN PLAYED WITH THE BOTTLE AND LOST IT"
			
	**Storing results in scalars
	**By Month
	loc treat A B C
	foreach y of numlist 3/5{
		foreach x in `treat' {
			tabstat finishedbottle if treatment=="`x'" & month_gap_d==`y', by(treatment) stat(mean semean N) save 
			mat `x'`y'=r(Stat1)
			mat list `x'`y'
			scalar fb`x'mean`y'=`x'`y'[1,1]
			di fb`x'mean`y'
			scalar fb`x'SE`y'=`x'`y'[2,1]
			di fb`x'SE`y'
			scalar fb`x'N`y'=`x'`y'[3,1]
			di fb`x'N`y'
		}
	}
				
	**Overall	
	loc treat A C
	foreach x in `treat' {
		ttest finishedbottle if `x'==1|B==1, by(B)
		scalar fb`x'allmean=r(mu_1)
		scalar fb`x'allSE=r(se)
		scalar fb`x'allN=r(N_1)
	}
		ttest finishedbottle if B==1|C==1, by(C)
		scalar fbBallmean=r(mu_1)
		scalar fbBallSE=r(se)
		scalar fbBallN=r(N_1)

 
**Exporting all results

putexcel A1=("Table S5. What happened to the study-provided bottles?") using "${outdir}\sawa_tableS5", replace
putexcel A2=("RAW MEANS") B2=("COST SHARING") C2=("VOUCHERS") D2=("FREE DELIVERY") E2=("Observations") using "${outdir}\sawa_tableS5", modify
putexcel A3=("All") using "${outdir}\sawa_tableS5", modify
putexcel A4=("Currently has non-empty Aquaguard bottle on compound") D4=(neCallmean) E4=(neCallN) using "${outdir}\sawa_tableS5", modify
putexcel D5=(neCallSE) using "${outdir}\sawa_tableS5", modify
putexcel A6=("Reports giving bottle/coupons away") B6=(gaAallmean) C6=(gaBallmean) D6=(gaCallmean) E6=(gaAallN+gaBallN+gaCallN) using "${outdir}\sawa_tableS5", modify
putexcel B7=(gaAallSE) C7=(gaBallSE) D7=(gaCallSE) using "${outdir}\sawa_tableS5", modify

putexcel A10=("Declares running out of chlorine") B10=(fbAallmean) C10=(fbBallmean) D10=(fbCallmean) E10=(fbAallN+fbBallN+fbCallN) using "${outdir}\sawa_tableS5", modify
putexcel B11=(fbAallSE) C11=(fbBallSE) D11=(fbCallSE) using "${outdir}\sawa_tableS5", modify
putexcel A12=("Surveyed 3 months after enrollment") using "${outdir}\sawa_tableS5", modify
putexcel A13=("Currently has non-empty Aquaguard bottle on compound") D13=(neCmean3) E13=(neCN3) using "${outdir}\sawa_tableS5", modify
putexcel D14=(neCSE3) using "${outdir}\sawa_tableS5", modify
putexcel A15=("Reports giving bottle/coupons away") B15=(gaAmean3) C15=(gaBmean3) D15=(gaCmean3) E15=(gaAN3+gaBN3+gaCN3) using "${outdir}\sawa_tableS5", modify
putexcel B16=(gaASE3) C16=(gaBSE3) D16=(gaCSE3) using "${outdir}\sawa_tableS5", modify

putexcel A19=("Declares running out of chlorine") B19=(fbAmean3) C19=(fbBmean3) D19=(fbCmean3) E19=(fbAN3+fbBN3+fbCN3) using "${outdir}\sawa_tableS5", modify
putexcel B20=(fbASE3) C20=(fbBSE3) D20=(fbCSE3) using "${outdir}\sawa_tableS5", modify
putexcel A21=("Surveyed 4 months after enrollment") using "${outdir}\sawa_tableS5", modify
putexcel A22=("Currently has non-empty Aquaguard bottle on compound") D22=(neCmean4) E22=(neCN4) using "${outdir}\sawa_tableS5", modify
putexcel D23=(neCSE4) using "${outdir}\sawa_tableS5", modify
putexcel A24=("Reports giving bottle/coupons away") B24=(gaAmean4) C24=(gaBmean4) D24=(gaCmean4) E24=(gaAN4+gaBN4+gaCN4) using "${outdir}\sawa_tableS5", modify
putexcel B25=(gaASE4) C25=(gaBSE4) D25=(gaCSE4) using "${outdir}\sawa_tableS5", modify

putexcel A28=("Declares running out of chlorine") B28=(fbAmean4) C28=(fbBmean4) D28=(fbCmean4) E28=(fbAN4+fbBN4+fbCN4) using "${outdir}\sawa_tableS5", modify
putexcel B29=(fbASE4) C29=(fbBSE4) D29=(fbCSE4) using "${outdir}\sawa_tableS5", modify
putexcel A30=("Surveyed 5 months after enrollment") using "${outdir}\sawa_tableS5", modify
putexcel A31=("Currently has non-empty Aquaguard bottle on compound") D31=(neCmean5) E31=(neCN5) using "${outdir}\sawa_tableS5", modify
putexcel D32=(neCSE5) using "${outdir}\sawa_tableS5", modify
putexcel A33=("Reports giving bottle/coupons away") B33=(gaAmean5) C33=(gaBmean5) D33=(gaCmean5) E33=(gaAN5+gaBN5+gaCN5) using "${outdir}\sawa_tableS5", modify
putexcel B34=(gaASE5) C34=(gaBSE5) D34=(gaCSE5) using "${outdir}\sawa_tableS5", modify

putexcel A37=("Declares running out of chlorine") B37=(fbAmean5) C37=(fbBmean5) D37=(fbCmean5) E37=(fbAN5+fbBN5+fbCN5) using "${outdir}\sawa_tableS5", modify
putexcel B38=(fbASE5) C38=(fbBSE5) D38=(fbCSE5) using "${outdir}\sawa_tableS5", modify


/*----------------------------------------------
 FIGURE S1: COUPON REDEMPTION OVER TIME
----------------------------------------------*/

preserve
foreach x of numlist 1/12 {
	egen mmean`x'=mean(m`x'_new)
}

keep hhid mmean1 mmean2 mmean3 mmean4 mmean5 mmean6 mmean7 mmean8 mmean9 mmean10 mmean11 mmean12


reshape long mmean, i(hhid) j(month)

twoway (connected mmean month, sort msymbol(circle)), ytitle(Fraction of coupons redeemed) yscale(range(0 1)) ///
ylabel(#11) xtitle(Coupon number (=months since distribution)) xscale(range(1 12)) xlabel(#12) ///
title(Coupon redemption over time) graphregion(fcolor(white)) saving("${outdir}\sawa_figureS1.gph", replace)

restore

/****by wave***/

preserve
foreach x of numlist 1/12 {
	bys wave: egen mmean`x'=mean(m`x'_new)
}

keep hhid mmean1 mmean2 mmean3 mmean4 mmean5 mmean6 mmean7 mmean8 mmean9 mmean10 mmean11 mmean12 wave


reshape long mmean, i(hhid) j(month)

twoway (connected mmean month if wave==1, sort msymbol(circle)) (connected mmean month if wave==2, sort msymbol(circle)), ytitle(Fraction of coupons redeemed) yscale(range(0 1)) ///
ylabel(#11) legend(order(1 "wave 1" 2 "wave 2")) xtitle(Coupon number (=months since distribution)) xscale(range(1 12)) xlabel(#12) ///
title(Coupon redemption over time) graphregion(fcolor(white)) saving("${outdir}\sawa_figureS1_waves.gph", replace)

restore
