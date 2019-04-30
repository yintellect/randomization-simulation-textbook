/**THIS DO FILE PROVIDES THE ANALYSIS FOR
THE MANUSCRIPT:  
Baird, McIntosh, Ozler (2011), "Cash or Condition? Evidence from a cash transfer experiment", 
Quarterly Journal of Economics, 126(4), pp. 1709-1753**/

capture log close
#delimit;
clear;
clear matrix;
set more off;
set memory 250m;
set matsize 300;

/***SET UP FOLDERS***/

global dir=""; /*PUT DIRECTORY HERE*/ 
global ta="$dir/";
use "$dir/CorC_Public_Data_FINAL_DEIDENTIFIED.dta", replace;

******SET CONTROLS*********
#delimit;
global RHS_controls="highest_grade_baseline asset_index_baseline never_had_sex_baseline _Iage_R1_14- _Iage_R1_20
	    stratum1 stratum2";


/**************************************************/
/* Table I: ATTRITION */
/**************************************************/


#delimit;
reg  partII_round3 T2a T2b if round==3 & sample_SG==1 [pw=wgt], robust cl(eaid);
	estimates store reg1;
	test T2a=T2b;
reg  panel T2a T2b if round==3 & sample_SG==1 [pw=wgt], robust cl(eaid);
	estimates store reg2;
	test T2a=T2b;
reg  education_score_exists T2a T2b if round==3 & sample_SG==1 [pw=wgt], robust cl(eaid);
	estimates store reg3;
	test T2a=T2b;
reg  SS_sample_R2 T2a T2b if round==2 & sample_SG==1 [pw=wgt], robust cl(eaid);
	estimates store reg4;
	test T2a=T2b;
reg  SS_sample_R3 T2a T2b if round==3 & sample_SG==1 [pw=wgt_SSR3], robust cl(eaid);
	estimates store reg5;
	test T2a=T2b;
reg  legible_ledger T2a T2b if round==3 & sample_SG==1 [pw=wgt_SSR3], robust cl(eaid);
	estimates store reg6;
	test T2a=T2b;
xml_tab reg1 reg2 reg3 reg4 reg5 reg6 
using "$ta/CorC_Main Tables", below replace cons("constant (control mean)") keep(T2a T2b _cons)
cnames("Round 3" "Panel"  "Education Test" "School Survey R2" "School Survey R3" "Has Legible Ledger")
title("Table I: Three Round Panel, Education Exists") stats(N) sheet("TableI_Attrition");


/**************************************************/
/* Table II: BASELINE BALANCE*/
/**************************************************/

#delimit;
local i=1;
foreach x in hhsize asset_index female_headed mobile  
age_R1 highest_grade_baseline mother_alive  father_alive never_had_sex ever_preg_r1 
	{;
	sum `x' [aw=wgt] if C2==1 & round==1 & sample_SG==1 & panel==1;
	sum `x' [aw=wgt] if T2a==1 & round==1 & sample_SG==1 & panel==1;
	sum `x' [aw=wgt] if T2b==1 & round==1 & sample_SG==1 & panel==1;
	reg `x'  T2a T2b if sample_SG==1 & panel==1 & round==1 [pw=wgt], cl(eaid);
	test T2a=T2b;
	est store reg`i';
	local i=`i'+1;
	pause;
	};

xml_tab reg1 reg2 reg3 reg4 reg5 reg6 reg7 reg8 reg9 reg10
using "$ta/CorC_Main Tables", below append cons("constant (control mean)") keep(T2a T2b _cons )
cnames( "HH Size" "asset_index" "female_headed" "mobile" 
"age_R1" "highest_grade" "mother_alive"  "father_alive" "never_had_sex" "Ever pregnant")
title("Table II: Baseline Balance") stats(N) sheet("TableII_Baseline Balance");

reg hh_amount 	T2a if (T2a==1 | T2b==1) & panel==1 & round==1 [pw=wgt], cl(eaid);
reg individual_amount 	T2a if (T2a==1 | T2b==1) & panel==1 & round==1 [pw=wgt], cl(eaid);


/*********************************************************************/
/* TABLE III PANEL A: SCHOOL ENROLLMENT & ATTENDANCE (SELF-REPORTED) */
/*********************************************************************/

#delimit;
reg term1_r2 T2a T2b $RHS_controls [pw=wgt] if sample_SG==1 & panel==1 & round==2, cl(eaid);
estimates store reg1;
test T2a=T2b;
reg term2_r2 T2a T2b $RHS_controls [pw=wgt] if sample_SG==1 & panel==1 & round==2, cl(eaid);
estimates store reg2;
test T2a=T2b;
reg term3_r2 T2a T2b $RHS_controls [pw=wgt] if sample_SG==1 & panel==1 & round==2, cl(eaid);
estimates store reg3;
test T2a=T2b;
reg inschool_term1_2009 T2a T2b $RHS_controls [pw=wgt] if sample_SG==1 & panel==1 & round==3, cl(eaid);
estimates store reg4;
test T2a=T2b;
reg inschool_term2_2009 T2a T2b $RHS_controls [pw=wgt] if sample_SG==1 & panel==1 & round==3, cl(eaid);
estimates store reg5;
test T2a=T2b;
reg inschool_term3_2009 T2a T2b $RHS_controls [pw=wgt] if sample_SG==1 & panel==1 & round==3, cl(eaid);
estimates store reg6;
test T2a=T2b;
reg num_terms_enrolled T2a T2b $RHS_controls [pw=wgt] if sample_SG==1 & panel==1 & round==3, cl(eaid);
estimates store reg7;
test T2a=T2b;
reg inschool_term1_2010 T2a T2b $RHS_controls [pw=wgt] if sample_SG==1 & panel==1 & round==3, cl(eaid);
estimates store reg8;
test T2a=T2b;

xml_tab reg1 reg2 reg3 reg4 reg5 reg6 reg7 reg8
using "$ta/CorC_Main Tables", below append keep(T2a T2b)
cnames("2008 Term 1" "2008 Term 2" "2008 Term 3" "2009 Term 1" "2009 Term 2" "2009 Term 3" "TOTAL number of terms" "2010 Term 1")
title("Table III Panel A: Enrollment-SR") stats(N) sheet("TableIIIa_Enrollment-SR");

foreach x in term1_r2 term2_r2 term3_r2{;
    mean `x' [aw=wgt] if C2==1 & sample_SG==1 & panel==1 & round==2;
	};
foreach x in inschool_term1_2009 inschool_term2_2009 inschool_term3_2009{;
    mean `x' [aw=wgt] if C2==1 & sample_SG==1 & panel==1 & round==3;
	};
foreach x in num_terms_enrolled inschool_term1_2010{;
    mean `x' [aw=wgt] if C2==1 & sample_SG==1 & panel==1 & round==3;
	};
	
/*********************************************************************/
/* TABLE III PANEL B: SCHOOL ENROLLMENT & ATTENDANCE (SCHOOL SURVEY) */
/*********************************************************************/

reg inschool_term1_2008_SS T2a T2b $RHS_controls [pw=wgt] if sample_SG==1 & panel==1 & round==2, cl(eaid);
estimates store reg1;
test T2a=T2b;
reg inschool_term2_2008_SS T2a T2b $RHS_controls [pw=wgt] if sample_SG==1 & panel==1 & round==2, cl(eaid);
estimates store reg2;
test T2a=T2b;
reg inschool_term3_2008_SS T2a T2b $RHS_controls [pw=wgt] if sample_SG==1 & panel==1 & round==2, cl(eaid);
estimates store reg3;
test T2a=T2b;
reg inschool_term1_2009_SS T2a T2b $RHS_controls [pw=wgt_SSR3] if sample_SG==1 & panel==1 & round==3 & sample_R3SS<., cl(eaid);
estimates store reg4;
test T2a=T2b;
reg inschool_term2_2009_SS T2a T2b $RHS_controls [pw=wgt_SSR3] if sample_SG==1 & panel==1 & round==3 & sample_R3SS<., cl(eaid);
estimates store reg5;
test T2a=T2b;
reg inschool_term3_2009_SS T2a T2b $RHS_controls [pw=wgt_SSR3] if sample_SG==1 & panel==1 & round==3 & sample_R3SS<., cl(eaid);
estimates store reg6;
test T2a=T2b;
reg num_terms_enrolled_SS T2a T2b $RHS_controls [pw=wgt_SSR3] if sample_SG==1 & panel==1 & round==3 & sample_R3SS<., cl(eaid);
estimates store reg7;
test T2a=T2b;
reg inschool_term1_2010_SS T2a T2b $RHS_controls [pw=wgt_SSR3] if sample_SG==1 & panel==1 & round==3 & sample_R3SS<., cl(eaid);
estimates store reg8;
test T2a=T2b;

xml_tab reg1 reg2 reg3 reg4 reg5 reg6 reg7 reg8
using "$ta/CorC_Main Tables", below append keep(T2a T2b)
cnames("2008 Term 1" "2008 Term 2" "2008 Term 3" "2009 Term 1" "2009 Term 2" "2009 Term 3" "TOTAL number of terms: 2008-2009" "2010 Term 1")
title("Table III Panel B: Enrollment-SS") stats(N) sheet("TableIIIb_Enrollment-SS");

foreach x in inschool_term1_2008_SS inschool_term2_2008_SS inschool_term3_2008_SS{;
    mean `x' [aw=wgt] if C2==1 & sample_SG==1 & panel==1 & round==2;
	};
foreach x in inschool_term1_2009_SS inschool_term2_2009_SS inschool_term3_2009_SS num_terms_enrolled_SS inschool_term1_2010_SS{;
    mean `x' [aw=wgt] if C2==1 & sample_SG==1 & panel==1 & round==3 & sample_R3SS<.;
	};


/***********************************************************/
/* TABLE V: SCHOOL ENROLLMENT & ATTENDANCE (SCHOOL SURVEY) */
/***********************************************************/


reg frac_attend_term1_2009 T2a T2b $RHS_controls [pw=wgt_SSR3] if sample_SG==1 & panel==1 & round==3, cl(eaid);
estimates store reg1;
test T2a=T2b;
reg frac_attend_term2_2009 T2a T2b $RHS_controls  [pw=wgt_SSR3] if sample_SG==1 & panel==1 & round==3, cl(eaid);
estimates store reg2;
test T2a=T2b;
reg frac_attend_term3_2009 T2a T2b $RHS_controls  [pw=wgt_SSR3] if sample_SG==1 & panel==1 & round==3, cl(eaid);
estimates store reg3;
test T2a=T2b;
reg frac_attend_2009_v1 T2a T2b $RHS_controls  [pw=wgt_SSR3] if sample_SG==1 & panel==1 & round==3, cl(eaid);
estimates store reg4;
test T2a=T2b;
reg frac_attend_term1_2010 T2a T2b $RHS_controls  [pw=wgt_SSR3] if sample_SG==1 & panel==1 & round==3, cl(eaid);
estimates store reg5;
test T2a=T2b;
xml_tab  reg1 reg2 reg3  reg4 reg5
using "$ta/CorC_Main Tables", below append keep(T2a T2b _cons)
cnames("Ledger 2009-T1" "Ledger 2009-T2" "Ledger 2009-T3"  "Overall 2009" "Ledger 2010-T1" ) 
title("Table V: Attendance-SS") stats(N) sheet("TableV_Attendance-SS");

foreach x in frac_attend_term1_2009 frac_attend_term2_2009 frac_attend_term3_2009 frac_attend_2009_v1 frac_attend_term1_2010{;
    mean `x' [aw=wgt] if C2==1 & sample_SG==1 & panel==1 & round==3;
	};
	

/**************************************************/
/* TABLE VI: EDUCATION TESTS */
/**************************************************/

#delimit;
reg eng_std T2a T2b eng_pilot $RHS_controls if round==3 & sample_SG==1 & panel==1 [pw=wgt], robust cl(eaid);
test T2a=T2b;
est store reg1;

reg TIMMS_std T2a T2b math_pilot $RHS_controls if round==3 & sample_SG==1 & panel==1 [pw=wgt], robust cl(eaid);
test T2a=T2b;
est store reg3;

reg math_malawi_std T2a T2b math_pilot $RHS_controls if round==3 & sample_SG==1 & panel==1 [pw=wgt], robust cl(eaid);
test T2a=T2b;
est store reg2;

reg cog_std T2a T2b  $RHS_controls if round==3 & sample_SG==1 & panel==1 [pw=wgt], robust cl(eaid);
test T2a=T2b;
est store reg5;

xml_tab reg1 reg3 reg2 reg5
using "$ta/CorC_Main Tables", below append keep(T2a T2b)
cnames("English Score (standardized)" "TIMMS Score Standardized"  "Math Malawi Score (standardized)" 
 "Cognitive Score (standardized)")
title("Table VI: Educational Tests") stats(N) sheet("TableVI_Educational Tests");



/**************************************************/
/*TABLE VII: MARRIAGE AND FERTILITY*/
/**************************************************/

reg ever_married  T2a T2b $RHS_controls [pw=wgt] if sample_SG==1 & panel==1 & round==2, cl(eaid);
test T2a=T2b;
est store reg1;
reg ever_married  T2a T2b $RHS_controls [pw=wgt] if sample_SG==1 & panel==1 & round==3, cl(eaid);
test T2a=T2b;
est store reg2;
reg ever_pregnant T2a T2b $RHS_controls [pw=wgt] if sample_SG==1 & panel==1 & round==2, cl(eaid);
test T2a=T2b;
est store reg3;
reg ever_pregnant T2a T2b $RHS_controls [pw=wgt] if sample_SG==1 & panel==1 & round==3, cl(eaid);
test T2a=T2b;
est store reg4;

xml_tab  reg1 reg2 reg3 reg4
using "$ta/CorC_Main Tables", below append keep(T2a T2b)
cnames("Ever Married R2" "Ever Married R3"  "Ever Pregnant R2" "Ever Pregnant R3")
title("Table VII: Marriage and Pregnancy") stats(N) sheet("TableVII_Marriage&Pregnancy");

foreach x in ever_married ever_pregnant{;
    mean `x' [aw=wgt] if C2==1 & sample_SG==1 & panel==1 & round==2;
	};
	
foreach x in ever_married ever_pregnant{;
    mean `x' [aw=wgt] if C2==1 & sample_SG==1 & panel==1 & round==3;
	};
	

/******************************************************/
/* TABLE VIII: MARITAL STATUS BY ENROLLMENT AND GROUP */
/******************************************************/


tab group inschool_term1_2010_SS [aw=wgt] if sample_SG==1 & panel==1 & round==3, sum(ever_married) nost nofreq; /* cell means */
tab group inschool_term1_2010_SS [aw=wgt] if sample_SG==1 & panel==1 & round==3, row nofreq; /* row percentages */

/**************************************************/
/* TABLE IX: SCHOOL ENROLLMENT AND MARITAL STATUS */
/**************************************************/

reg inschool_term1_2010_SS T2a T2b $RHS_controls [pw=wgt_SSR3] if sample_SG==1 & panel==1 & round==3 & sample_R3SS<., cl(eaid);
test T2a=T2b;
estimates store reg1;
reg ever_married  T2a T2b $RHS_controls [pw=wgt_SSR3] if sample_SG==1 & panel==1 & round==3 & sample_R3SS<., cl(eaid);
test T2a=T2b;
estimates store reg2;
reg ever_married T2a T2b $RHS_controls [pw=wgt] if sample_SG==1 & panel==1 & round==3 & sample_R3SS<. & inschool_term1_2010_SS==1, cl(eaid);
test T2a=T2b;
estimates store reg3;
reg ever_married T2a T2b $RHS_controls [pw=wgt] if sample_SG==1 & panel==1 & round==3 & sample_R3SS<. & inschool_term1_2010_SS==0, cl(eaid);
test T2a=T2b;
estimates store reg4;
xml_tab  reg1 reg2 reg3 reg4
using "$ta/CorC_Main Tables", below append keep(T2a T2b)
cnames("In School" "Ever Married"  "Ever Married" "Ever Married")
title("Table IX: Teacher Reported School Enrollment and Marital Status") stats(N) sheet("TableIX_Schooling and Marriage");

mean inschool_term1_2010_SS [aw=wgt] if C2==1 & sample_SG==1 & panel==1 & round==3 & sample_R3SS<.;
foreach x in ever_married {;
    mean `x' [aw=wgt] if C2==1 & sample_SG==1 & panel==1 & round==3 & sample_R3SS<.;
	mean `x' [aw=wgt] if C2==1 & sample_SG==1 & panel==1 & round==3 & sample_R3SS<. & inschool_term1_2010_SS==1;
	mean `x' [aw=wgt] if C2==1 & sample_SG==1 & panel==1 & round==3 & sample_R3SS<. & inschool_term1_2010_SS==0;
	};
	

/**********************************************/
/* TABLE X: HETEROGENEITY BY AGE */
/**********************************************/


global RHS_controls="highest_grade_baseline asset_index_baseline never_had_sex_baseline above_15
	    stratum1 stratum2";

reg num_terms_enrolled_SS T2a T2b T2a_above T2b_above $RHS_controls if round==3 & sample_SG==1 & panel==1 [pw=wgt_SSR3], robust cl(eaid);
	test T2a=T2b;
	test T2a_above=T2b_above;
	est store reg1;

reg eng_std T2a T2b T2a_above T2b_above eng_pilot  $RHS_controls if round==3 & sample_SG==1 & panel==1 [pw=wgt], robust cl(eaid);
	test T2a=T2b;
	test T2a_above=T2b_above;
	est store reg2;

reg ever_married T2a T2b T2a_above T2b_above $RHS_controls if round==3 & sample_SG==1 & panel==1 [pw=wgt], robust cl(eaid);
	test T2a=T2b;
	test T2a_above=T2b_above;
	est store reg3;

reg ever_pregnant T2a T2b T2a_above T2b_above $RHS_controls if round==3 & sample_SG==1 & panel==1 [pw=wgt], robust cl(eaid);
	test T2a=T2b;
	test T2a_above=T2b_above;
	est store reg4;

xml_tab  reg1 reg2 reg3 reg4 
using "$ta/CorC_Main Tables", below append 
keep(T2a T2b T2a_above_15 T2b_above_15 above_15)
cnames("Enrollment" "English" "Ever Married" "Ever Pregnant")
title("Table X: Age Heterogeneity") stats(N) sheet("TableX_Age Heterogeneity");


/*********************THE CODE BELOW MANIPULATES AND CREATES VARIABLES FOR TABLE XI***************************/


#delimit;
replace hh_amount=hh_amount*140;
replace individual_amount=individual_amount*140;

foreach x in individual_amount hh_amount total_amount{;
	replace `x'=0 if C1==1 | C2==1 | S2==1;
	};
	
/*Subtracting the minimum amount off of all transfer amounts so that when we use them in regression with a treatment dummy,
	the coefficient on the treatment dummy gives the impact at the smallest transfer size.*/
	
replace hh_amount=hh_amount-560;
replace individual_amount=individual_amount-140;
/*replace total_amount=total_amount-700;*/

/*Dollar amounts for transfers, ER=140 Mkw/$*/
gen individual_amount_USD=individual_amount/140;
label var individual_amount_USD "Dollar amounts for individual transfers";

gen hh_amount_USD=hh_amount/140;
label var hh_amount_USD "Dollar amounts for household transfers";

/*To permamently switch all transfer amounts to dollars:*/
replace hh_amount=hh_amount_USD;
replace individual_amount=individual_amount_USD;

gen treat_ind_amount3=treatment_year3;	
replace treat_ind_amount3=individual_amount if (ea_stat==1 & round==3);

gen treat_hh_amount3=treatment_year3;	
replace treat_hh_amount3=hh_amount if (ea_stat==1 & round==3);

gen cond_treat_ind3 = treat_ind_amount3 * cond_treat_year3;
gen uncond_treat_ind3 = treat_ind_amount3 * uncond_treat_year3;
gen cond_treat_hh3 = treat_hh_amount3 * cond_treat_year3;
gen uncond_treat_hh3 = treat_hh_amount3 * uncond_treat_year3;

label var treat_ind_amount3 "treatment group, individual amount round 3";
label var treat_hh_amount3 "treatment group, hh amount round 3";


label var cond_treat_ind3 "conditional treatment, individual amount, round 3";
label var uncond_treat_ind3 "unconditional treatment, individual amount, round 3";
label var cond_treat_hh3 "conditional treatment, household amount, round 3";
label var uncond_treat_hh3 "unconditional treatment, household amount, round 3";


*****************************************************************************************************
/* TABLE XI MARRIAGE AND PREGNANCY AND ENROLLMENT AND ENGLISH TEST SCORES (with transfer amounts) */
*****************************************************************************************************


#delimit;

reg num_terms_enrolled_SS cond_treat_year3 cond_treat_ind3 cond_treat_hh3 
						  uncond_treat_year3 uncond_treat_ind3 uncond_treat_hh3 $RHS_controls 
						  [pw=wgt_SSR3] if sample_SG==1 & panel==1 & round==3, cluster(eaid);

test cond_treat_year3=uncond_treat_year3;
test cond_treat_hh3 = uncond_treat_hh3;
test cond_treat_ind3 = uncond_treat_ind3;
est store reg1;


reg eng_std cond_treat_year3 cond_treat_ind3 cond_treat_hh3 
						  uncond_treat_year3 uncond_treat_ind3 uncond_treat_hh3 eng_pilot $RHS_controls 
						  [pw=wgt_SSR3] if sample_SG==1 & panel==1 & round==3, cluster(eaid);

test cond_treat_year3=uncond_treat_year3;
test cond_treat_hh3 = uncond_treat_hh3;
test cond_treat_ind3 = uncond_treat_ind3;
est store reg2;

reg ever_married cond_treat_year3 cond_treat_ind3 cond_treat_hh3
			 uncond_treat_year3 uncond_treat_ind3 uncond_treat_hh3 $RHS_controls
			 [pw=wgt] if sample_SG==1 & panel==1 & round==3, cluster(eaid);

test cond_treat_year3=uncond_treat_year3;
test cond_treat_hh3 = uncond_treat_hh3;
test cond_treat_ind3 = uncond_treat_ind3;
est store reg3;

reg ever_pregnant cond_treat_year3 cond_treat_ind3 cond_treat_hh3
			 uncond_treat_year3 uncond_treat_ind3 uncond_treat_hh3 $RHS_controls
			 [pw=wgt] if sample_SG==1 & panel==1 & round==3, cluster(eaid);

test cond_treat_year3=uncond_treat_year3;
test cond_treat_hh3 = uncond_treat_hh3;
test cond_treat_ind3 = uncond_treat_ind3;
est store reg4;

xml_tab reg1 reg2 reg3 reg4 
using "$ta/CorC_Main Tables", below append keep(cond_treat_year3 cond_treat_ind3 cond_treat_hh3
										uncond_treat_year3 uncond_treat_ind3 uncond_treat_hh3)
cnames( "Total terms enrolled" "English test score"  "Ever Married" "Ever Pregnant")
title("Table XI: Transfer Amounts") stats(N) sheet("TableXI_Transfer Amounts");


		
