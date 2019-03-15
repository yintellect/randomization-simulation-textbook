* ---------------------------------------------------------------------------------------------------------------- *
* This file generates the tables in Bruhn, Miriam, Dean Karlan, and Antoinette Schoar.                             *
* The Impact of Consulting Services on Small and Medium Enterprises: Evidence from a Randomized Trial in Mexico	   *
* Journal of Political Economy, forthcoming.                                                                       *
* ---------------------------------------------------------------------------------------------------------------- *

# delimit ;
cap log close;
clear all;
set mem 100m;
set more off;
set matsize 1000;

global path "YOUR PATH"; /*ENTER your local directory before running the dofile*/
global data "${path}\data";
global results "${path}\results";
global log "${path}\logfiles";
global do "${path}\do";
global ssc "${path}\adofiles";

**Create logfile;
log using "${log}\bruhn_karlan_schoar_replication_final.smcl", replace;

**Call user-written commands;
sysdir set PLUS "${ssc}";

**Set directory (Do not change);
cd "${path}";

use "${data}\bruhn_karlan_schoar_survey_data.dta";


* --------------------------------------------------- *;
* Generating variables for summary stats and analysis *;
* --------------------------------------------------- *;

**Month of interview;

egen month=ends(date) if status==1, head;
tab month if treatment==1;
tab month if treatment==0;

gen march=0 if month~="";
replace march=1 if month=="March";

**Employment;

gen empl_paid=full_time_employees-full_time_employees_nopay;
replace empl_paid=P191a-P192a if followup==2;
label var empl_paid "Paid full-time employees";
rename total_employees empl;
replace empl=P18 if followup==2;
gen lempl=ln(empl);
label var lempl "Log(Total employees)";

**Sales;
gen flag_bl=0 if followup==0;
replace flag_bl=1 if total_sales_jul==0 & total_sales_aug==0 & total_sales_sep==0 & followup==0;
reg flag_bl treatment if followup==0, r;
gen flag=0 if status==1 & followup==1;
replace flag=1 if total_sales_feb==0 & total_sales_jan==0 & total_sales_dec==0 & followup==1;
reg flag treatment if followup==1, r;
gen flag_fu2=0 if status_fu2=="Interviewed" & followup==2;
replace flag_fu2=1 if P29a==0 & P29b==0 & P29c==0;
replace flag_fu2=1 if P29b==0 & P29c==0;
reg flag_fu2 treatment if followup==2, r;
*3.7% of firms report zero for all three months in both the treatment and control group;
*browse if total_sales_feb==0 & total_sales_jan==0 & total_sales_dec==0;
*browse if total_sales_sep==0 & total_sales_aug==0 & total_sales_jul==0;
*Firms that report zero sales for all three months have non-zero employees, assets, costs or hours worked, suggesting they are operating;
*We assume these firms did not want to report sales and set sales to missing; 
replace total_sales_feb=. if total_sales_feb==0 & total_sales_jan==0 & total_sales_dec==0;
replace total_sales_jan=. if total_sales_feb==. & total_sales_jan==0 & total_sales_dec==0;
replace total_sales_dec=. if total_sales_feb==. & total_sales_jan==. & total_sales_dec==0;
replace total_sales_sep=. if total_sales_sep==0 & total_sales_aug==0 & total_sales_jul==0;
replace total_sales_aug=. if total_sales_sep==. & total_sales_aug==0 & total_sales_jul==0;
replace total_sales_jul=. if total_sales_sep==. & total_sales_aug==. & total_sales_jul==0;
replace P29a=. if P29a==0 & P29b==0 & P29c==0;
replace P29b=. if P29a==0 & P29b==0 & P29c==0;
replace P29c=. if P29a==0 & P29b==0 & P29c==0;
replace P29b=. if P29a==. & P29b==0 & P29c==0;
replace P29c=. if P29a==. & P29b==. & P29c==0;

egen sales=rmean(total_sales_feb total_sales_jan total_sales_dec);
egen sales_bl=rmean(total_sales_sep total_sales_aug total_sales_jul) if followup==0;
egen sales_fu2=rmean(P29a P29b P29c) if followup==2;
replace sales=sales_bl if followup==0;
replace sales=sales_fu2 if followup==2;
drop sales_bl sales_fu2;
replace sales=. if sales<0;
replace sales=sales/12.3/1000; /* Throughout the analysis we assume an exchange rate of 12.3 Mexican pesos to the US dollar */
label var sales "Avg. sales for past 3 months (1000s)";
gen lsales=ln(sales);
label var lsales "Log[Avg. sales for past 3 month (1000s)]";

**Costs;

gen costs=total_costs_feb;
replace costs=total_costs_sep if followup==0;
replace costs=P23a if followup==2;
replace costs=costs/12.3/1000;
label var costs "Costs in past month (1000s)";
replace cost_materials=(cost_materials+cost_phones+cost_utilities+cost_maintenance)/12.3/1000;
label var cost_materials "Total costs/expenses in 02/2009 of raw materials, merchandise, spare parts and other materials + phones, utilities, maintenance";
note cost_materials: Total costs/expenses in 02/2009 of raw materials, merchandise, spare parts and other materials + phones, utilities, maintenance;
replace cost_materials=1 if followup==2; /* This variable was not collected in 2014 follow-up so it's set to 1 */
gen lcost_materials=ln(cost_materials);
label var lcost_materials "log(Cost of materials)";
**Profits;

gen profits_sc=(total_sales_feb-total_costs_feb);
replace profits_sc=(total_sales_sep-total_costs_sep) if followup==0;
replace profits_sc=(P29a-P23a) if followup==2;
replace profits_sc=profits_sc/12.3/1000;
label var profits_sc "Profits calculated as sales minus costs (1000s)";

**Productivity;

foreach var in machinery tools equipment vehicles property other_assets{;
replace `var'_value=0 if `var'==0;
};
egen assets=rsum(machinery_value tools_value equipment_value vehicles_value property_value other_assets_value);
foreach var in machinery tools equipment vehicles property other_assets{;
replace assets=. if `var'_value==.;
};
foreach var in a b c d e f{;
replace P222`var'=0 if P221`var'==0;
};
egen assets_fu2=rsum(P222a P222b P222c P222d P222e P222f);
foreach var in a b c d e f{;
replace assets_fu2=. if P222`var'==.;
};
replace assets=assets_fu2 if followup==2;
drop assets_fu2;
label var assets "Value of all assets/capital stock (1000s)";
replace assets=assets/12.3/1000;
gen lassets=ln(assets);
label var lassets "Log(Value of business assets)";
gen ltotal_sales_feb=ln(total_sales_feb);
gen ltotal_sales_sep=ln(total_sales_sep);
gen lP29a=ln(P29a);
reg ltotal_sales_feb lempl lassets if followup==1,r;
predict prod if e(sample), resid;
reg ltotal_sales_sep lempl lassets if followup==0,r;
predict bl_prod if e(sample), resid;
reg lP29a lempl lassets if followup==2,r;
predict fu2_prod if e(sample), resid;
replace prod=bl_prod if followup==0;
replace prod=fu2_prod if followup==2;
drop bl_prod fu2_prod;
label var prod "Productivity: residual from regressing log of one month's sales on log empl. and log assets"; /* In each case, the month is the one before questionnaire was administered */

reg ltotal_sales_feb lempl lassets lcost_materials if followup==1,r;
predict prod_materials if e(sample), resid;
reg ltotal_sales_sep lempl lassets lcost_materials if followup==0,r;
predict bl_prod_materials if e(sample), resid;
replace prod_materials=bl_prod_materials if followup==0;
replace prod_materials=1 if followup==2; /* This variable was not collected in 2014 follow-up so it's set to 1 */
drop bl_prod_materials;
label var prod_material "Productivity: residual from regressing log February sales on log empl., log assets and log materials";

gen sales_prod=total_sales_feb if followup==1;
replace sales_prod=total_sales_sep if followup==0;
replace sales_prod=P29a if followup==2;
gen lsales_prod=ln(sales_prod);
label var sales_prod "Sales used to calculate productivity";
label var lsales_prod "Log(Sales used to calculate productivity)";

gen solow=ln(total_sales_feb)-0.24*lempl-0.16*lassets-0.6*lcost_materials if followup==1;
replace solow=ln(total_sales_sep)-0.24*lempl-0.16*lassets-0.6*lcost_materials  if followup==0;
replace solow=1 if followup==2; /* This variable was not calculated in 2014 follow-up so it's set to 1 */
label var solow "Solow residual: log sales - 0.24 log empl. - 0.16 log assets - 0.6 log materials";

gen roa=profits_sc/(assets);
label var roa "Return on assets: profits over total asset value";

**Generating trimmed and winsorized outcome variables;

replace sector=P05 if followup==2;

gen size1=.;
replace size1=1 if empl_paid<=10 & empl_paid~=.;
replace size1=2 if sector==1 & empl_paid>=11 & empl_paid<=50 | sector==2 & empl_paid>=11 & empl_paid<=30 | sector==3 & empl_paid>=11 & empl_paid<=50;
replace size1=3 if sector==1 & empl_paid>=51 & empl_paid~=. | sector==2 & empl_paid>=31 & empl_paid~=. | sector==3 & empl_paid>=51 & empl_paid~=.;
label var size1 "Firm size classification based on reported paid full-time emloyees";
label define firm_size 1 "firm is in any sector and has 10 or less employees" 2 "firm is in industry or service sector with 11-50 employees or in commerce sector with 11-30 employees" 3 "firm is in industry or service sector with 51+ employees or in commere sector with 31+ employees";
label values size1 firm_size;

foreach var in empl empl_paid sales sales_prod costs cost_materials assets profits_sc prod prod_materials roa solow{;

	foreach x in 1{;

	gen `var'_trim`x'=`var';
	gen `var'_w`x'=`var';
	
	local y=100-`x';
		/* y=99 for 99 pctile (top) and x=1 for 1 pctile (bottom). */	
		foreach f in 0 1 2{; /*f is equivalent to survey round (=followup variable). */
		
			xi: reg `var' i.size1 if followup==`f';
			predict resid if e(sample) & followup==`f', resid;
			predict xb if e(sample) & followup==`f', xb;
		
			egen cent_bottom=pctile(resid) if followup==`f', p(`x');
			egen cent_top=pctile(resid) if followup==`f', p(`y');
			/* Replacing extreme values with trimmed/windorized values. */
			replace `var'_trim`x'=. if resid<=cent_bottom & followup==`f' | resid>=cent_top & followup==`f'; 
			replace `var'_w`x'=xb+cent_bottom if resid<cent_bottom & followup==`f' & `var'~=.;
			replace `var'_w`x'=xb+cent_top if resid>cent_top & followup==`f' & `var'~=.;

			drop cent* resid xb;

		};
	
	};
	
};

foreach var in empl sales sales_prod cost_materials assets{;
foreach x in 1{;
gen l`var'_trim`x'=ln(`var'_trim`x');
gen l`var'_w`x'=ln(`var'_w`x');
};
};

foreach var in empl_paid sales sales_prod costs cost_materials assets profits_sc prod prod_materials roa solow{;
label var `var'_trim1 "`var' without top and bottom 1% outliers";
label var `var'_w1 "`var' winsorized top and bottom 1%";
};

**Balancing variables to be used as controls;

foreach var in profit_margin buap indigenous entrepr_spirit total_assets_value age_business age{; /*Control variables */
gen ran_`var'_d=0; 
replace ran_`var'_d=1 if ran_`var'==.; /* All variables with the "_d" suffix are dummys for if the observation is missing this value (=1 if missing) */
replace ran_`var'=0 if ran_`var'==.;
};

local varlist "ran_puebla ran_age_business ran_total_assets_value ran_profit_margin ran_general_risk 
ran_loan ran_entrepr_spirit ran_buap ran_hours_worked ran_talents ran_age ran_male ran_schooling_years ran_indigenous
ran_profit_margin_d ran_buap_d ran_indigenous_d ran_entrepr_spirit_d ran_total_assets_value_d ran_age_business_d ran_age_d"; 

**Other variables;

gen age_business=2007-year_start_business;
label var age_business "Business age in 2007";

foreach var in primary secondary high_school technical bachelors masters doctorate{;
recode `var'_years 99=0;
};
forvalues x=1/2{;
recode other_school_years_`x' 99=0;
};
egen schooling_years=rsum(primary_years secondary_years high_school_years technical_years bachelors_years masters_years doctorate_years other_school_years_1 other_school_years_2);
label var schooling_year "Years of schooling completed by principal decision maker";

foreach var in industry commerce services{;
gen `var'=0 if sector~=.;
label var `var' "Dummy for firm being in `var'";
};
replace industry=1 if sector==1;
replace commerce=1 if sector==2;
replace services=1 if sector==3;

* ------------------------------------ *;
* Table 2: Baseline summary statistics *;
* ------------------------------------ *;

**Baseline statistics for complete sample;

preserve;

keep if followup==0;

matrix t=J(68, 6, .);
matrix colnames t=mean_treatment treat_N mean_control control_N diff diff_N;

local row_num=1;

foreach var in industry commerce services empl_paid age male schooling_years age_business  
sales sales_w1 costs costs_w1 profits_sc profits_sc_w1 assets assets_w1 prod prod_w1 roa roa_w1{;
		
		qui sum `var' if treatment == 1;
		matrix t[`row_num', 1] = r(mean);
		matrix t[`row_num' + 1, 1] = r(sd);
		matrix t[`row_num', 2] = r(N);

		qui sum `var' if treatment == 0;
		matrix t[`row_num', 3] = r(mean);
		matrix t[`row_num' + 1, 3] = r(sd);
		matrix t[`row_num', 4] = r(N);

		reg `var' treatment,r;
		matrix t[`row_num', 5] = _b[treatment];
		matrix t[`row_num' + 1, 5] = 2*ttail(e(df_r),abs(_b[treatment]/_se[treatment]));
		matrix t[`row_num', 6] = e(N);
		
		local row_num = `row_num' + 2; 
			
};

matrix list t;
clear;

svmat t, names(col);
label variable mean_treatment "Treatment";
label variable mean_control "Control";
label variable diff "Diff";

generate variable="";
order variable;
local i = 1;
foreach item in industry sd commerce sd services sd empl_paid sd age sd male sd schooling_years sd age_business sd  
sales sd sales_w1 sd costs sd costs_w1 sd profits_sc sd profits_sc_w1 sd assets sd assets_w1 sd prod sd prod_w1 sd roa sd roa_w1 sd 
{;
		
		replace variable = "`item'" in `i';
		local i = `i' + 1;
};

save "${results}\table2_all", replace;
restore;

**Baseline statistics for treatment group;

preserve;

keep if followup==0 & treatment==1;

matrix t=J(68, 6, .);
matrix colnames t=mean_in_program in_prog_N mean_not_in_prog not_in_prog_N diff diff_N;

local row_num=1;

foreach var in industry commerce services empl_paid age male schooling_years age_business  
sales sales_w1 costs costs_w1 profits_sc profits_sc_w1 assets assets_w1 prod prod_w1 roa roa_w1 {;
		
		qui sum `var' if in_program == 1;
		matrix t[`row_num', 1] = r(mean);
		matrix t[`row_num' + 1, 1] = r(sd);
		matrix t[`row_num', 2] = r(N);

		qui sum `var' if in_program == 0;
		matrix t[`row_num', 3] = r(mean);
		matrix t[`row_num' + 1, 3] = r(sd);
		matrix t[`row_num', 4] = r(N);
		
		reg `var' in_program,r;
		matrix t[`row_num', 5] = _b[in_program];
		matrix t[`row_num' + 1, 5] = 2*ttail(e(df_r),abs(_b[in_program]/_se[in_program]));
		matrix t[`row_num', 6] = e(N);
		
		local row_num = `row_num' + 2; 
			
};

matrix list t;
clear;

svmat t, names(col);
label variable mean_in_program "In program";
label variable mean_not_in_prog "Not in program";
label variable diff "Diff";

generate variable="";
order variable;
local i = 1;
foreach item in industry sd commerce sd services sd empl_paid sd age sd male sd schooling_years sd age_business sd  
sales sd sales_w1 sd costs sd costs_w1 sd profits_sc sd profits_sc_w1 sd assets sd assets_w1 sd prod sd prod_w1 sd roa sd roa_w1 sd 
{;
		
		replace variable = "`item'" in `i';
		local i = `i' + 1;
};

save "${results}\table2_treatment", replace;
restore;

**Joint orthogonality test - baseline;

reg treat sales costs profits_sc assets prod roa if followup==0,r;
test sales costs profits_sc assets prod roa;

reg treat sales_w1 costs_w1 profits_sc_w1 assets_w1 prod_w1 roa_w1 if followup==0,r;
test sales_w1 costs_w1 profits_sc_w1 assets_w1 prod_w1 roa_w1;

reg in_program sales costs profits_sc assets prod roa if followup==0 & treatment==1,r;
test sales costs profits_sc assets prod roa;

reg in_program sales_w1 costs_w1 profits_sc_w1 assets_w1 prod_w1 roa_w1 if followup==0 & treatment==1,r;
test sales_w1 costs_w1 profits_sc_w1 assets_w1 prod_w1 roa_w1;

* ------------------------------------ *;
* Table 3: Short-run business outcomes *;
* ------------------------------------ *;

**Employment;

foreach var in empl_paid{;

	gen bl_`var'=`var' if followup==0;
	sort folio followup;
	by folio: replace bl_`var'=bl_`var'[_n-1] if bl_`var'==.; 

	xi: reg `var' treatment march i.ran_size_sector `varlist' if followup==1,r;
	estimates store `var'1;
	xi: reg `var' treatment bl_`var' march i.ran_size_sector `varlist' if followup==1,r;
	estimates store `var'2;

	foreach name in w trim{;
	
		foreach x in 1{;

			xi: reg `var'_`name'`x' treatment march i.ran_size_sector `varlist' if followup==1,r;
			estimates store `var'1_`name'`x';
			xi: reg `var'_`name'`x' treatment bl_`var' march i.ran_size_sector `varlist' if followup==1,r;	
			estimates store `var'2_`name'`x';
		
		};

	};

xml_tab *
, nolabel keep(treatment bl_`var' _cons) font("Times New Roman" 11) below stats(r2 N) tblanks(1)
line(LAST_ROW 2 COL_NAMES 2)
format(sclr0 nccr3 nccr3)
sheet("`var'") 
save("${results}\table3.xml") 
replace;
estimates clear;

};

foreach var in lempl{;

	gen bl_`var'=`var' if followup==0;
	sort folio followup;
	by folio: replace bl_`var'=bl_`var'[_n-1] if bl_`var'==.;
	
	xi: reg `var' treatment march i.ran_size_sector `varlist' if followup==1,r;
	estimates store `var'1;
	xi: reg `var' treatment bl_`var' march i.ran_size_sector `varlist' if followup==1,r;
	estimates store `var'2;

	foreach name in w trim{;
	
		foreach x in 1{;

			xi: reg `var'_`name'`x' treatment march i.ran_size_sector `varlist' if followup==1,r;
			estimates store `var'1_`name'`x';
			xi: reg `var'_`name'`x' treatment bl_`var' march i.ran_size_sector `varlist' if followup==1,r;	
			estimates store `var'2_`name'`x';
		
		};

	};

xml_tab *
, nolabel keep(treatment bl_`var' _cons) font("Times New Roman" 11) below stats(r2 N) tblanks(1)
line(LAST_ROW 2 COL_NAMES 2)
format(sclr0 nccr3 nccr3)
sheet("`var'") 
save("${results}\table3.xml") 
append;
estimates clear;

};

**Sales, profits, assets, and productivity;

foreach var in sales lsales costs profits_sc lassets prod roa{;

	gen bl_`var'=`var' if followup==0;
	sort folio followup;
	by folio: replace bl_`var'=bl_`var'[_n-1] if bl_`var'==.;
	gen bl_`var'_d=1 if bl_`var'==.;
	replace bl_`var'_d=0 if bl_`var'_d==.;
	replace bl_`var'=0 if bl_`var'==.;

	xi: reg `var' treatment march i.ran_size_sector `varlist' if followup==1,r;
	estimates store `var'1;
	xi: reg `var' treatment bl_`var' bl_`var'_d march i.ran_size_sector `varlist' if followup==1,r;
	estimates store `var'2;
	
	foreach name in w trim{;
	
		foreach x in 1{;

		xi: reg `var'_`name'`x' treatment march i.ran_size_sector `varlist' if followup==1,r;
		estimates store `var'1_`name'`x';
		xi: reg `var'_`name'`x' treatment bl_`var' bl_`var'_d march i.ran_size_sector `varlist' if followup==1,r;	
		estimates store `var'2_`name'`x';
		
		};

	};

xml_tab *
, nolabel keep(treatment bl_`var' bl_`var'_d _cons) font("Times New Roman" 11) below stats(r2 N) tblanks(1)
line(LAST_ROW 2 COL_NAMES 2)
format(sclr0 nccr3 nccr3)
sheet("`var'") 
save("${results}\table3.xml") 
append;
estimates clear;

};

sum empl_paid lempl sales lsales costs profits_sc lassets prod roa if treatment==0 & followup==1; /*The values on this table correspond to control group means (column 7 on Table 3) */ 

* ----------------------------------- *;
* Table 4: Long-run business outcomes *;
* ----------------------------------- *;
preserve;

*Panel A;
use "${data}\bruhn_karlan_schoar_imss_data.dta", replace;

reg employment treatment_post treatment post, r;
reg wage_bill_usd treatment_post treatment post, r;

gen wage_bill_usd_sd = wage_bill_sd / 12.3 ; //make into USD using 12.3 exchange rate

*Panel B (two-sample t-tests);
foreach var in employment wage_bill_usd { ;
	foreach year in 2005 2006 2010 2011 2012 2013 2014 { ;
		*make locals of relevant values;
		levelsof `var' if treatment==1 & year==`year', loc(treat_m) ;
		levelsof `var'_sd if treatment==1 & year==`year', loc(treat_sd) ;
		levelsof `var' if treatment==0 & year==`year', loc(control_m) ;
		levelsof `var'_sd if treatment==0 & year==`year', loc(control_sd) ;
		
		*t-tests;
		di in r "testing `var' in year `year' " ;
		ttesti 89 `treat_m' `treat_sd' 164 `control_m' `control_sd' ;
	} ;
} ;

restore;

* --------------------------- *;
* Table 5: Business processes *;
* --------------------------- *;

**Reported changes in the business;

foreach var in new_products new_clients new_process new_investors{;

	gen bl_`var'=`var' if followup==0;
	sort folio followup;
	by folio: replace bl_`var'=bl_`var'[_n-1] if bl_`var'==.; 
	gen bl_`var'_d=1 if bl_`var'==.;
	replace bl_`var'_d=0 if bl_`var'_d==.;
	replace bl_`var'=0 if bl_`var'==.;

	xi: reg `var' treatment march i.ran_size_sector `varlist' if followup==1,r;
	estimates store `var'1;
	xi: reg `var' treatment bl_`var' bl_`var'_d march i.ran_size_sector `varlist' if followup==1,r;
	estimates store `var'2;

};

xml_tab *  
, nolabel keep(treatment bl_new_products bl_new_products_d bl_new_clients bl_new_clients_d 
bl_new_process bl_new_process_d bl_new_investors bl_new_investors_d _cons) font("Times New Roman" 11) below stats(r2 N) tblanks(1)
line(LAST_ROW 2 COL_NAMES 2)
format(sclr0 nccr3 nccr3)
sheet("Reported changes I") 
title("Self-Reported Changes Made in the Business During the Past Year") 
save("${results}\table5.xml") 
replace;
estimates clear;

foreach var in patent_process certification_process new_marketing expansion remodel{;

	xi: reg `var' treatment march i.ran_size_sector `varlist' if followup==1,r;
	estimates store `var'1;

};

xml_tab *  
, nolabel keep(treatment _cons) font("Times New Roman" 11) below stats(r2 N) tblanks(1)
line(LAST_ROW 2 COL_NAMES 2)
format(sclr0 nccr3 nccr3)
sheet("Reported changes II") 
title("Self-Reported Changes Made in the Business During the Past Year - not asked on baseline") 
save("${results}\table5.xml") 
append;
estimates clear;

**Human resources management index;

rename management_objectives management_ob;
rename management_responsibilities management_resp;
rename management_criticism management_criti;
rename management_centralized management_centr;
rename management_recognition management_reco;
rename management_low_turnover management_low;

pca management_ob management_resp management_criti management_centr management_reco management_low if followup==1;
predict management if followup==1, score;

pca management_ob management_resp management_criti management_centr management_reco management_low if followup==0;
predict bl_management if followup==0, score;
replace management=bl_management if followup==0;

foreach var in management{;

	sort folio followup;
	by folio: replace bl_`var'=bl_`var'[_n-1] if bl_`var'==.; 
	gen bl_`var'_d=1 if bl_`var'==.;
	replace bl_`var'_d=0 if bl_`var'_d==.;
	replace bl_`var'=0 if bl_`var'==.;
	
	xi: reg `var' treatment march i.ran_size_sector `varlist' if followup==1,r;
	estimates store `var'1;
	xi: reg `var' treatment bl_`var' bl_`var'_d march i.ran_size_sector `varlist' if followup==1,r;
	estimates store `var'2;

};

xml_tab *  
, nolabel keep(treatment bl_management bl_management_d _cons) 
font("Times New Roman" 11) below stats(r2 N) tblanks(1)
line(LAST_ROW 2 COL_NAMES 2)
format(sclr0 nccr3 nccr3)
sheet("Management") 
title("Management style index and components") 
save("${results}\table5.xml") 
append;
estimates clear;

**Keeping formal accounts;

gen formal_accounts=1 if accounts==1 | accounts==3;
replace formal_accounts=0 if accounts==2 | accounts==4 | accounts==5;

foreach var in formal_accounts{;

	gen bl_`var'=`var' if followup==0;
	sort folio followup;
	by folio: replace bl_`var'=bl_`var'[_n-1] if bl_`var'==.; 
	gen bl_`var'_d=1 if bl_`var'==.;
	replace bl_`var'_d=0 if bl_`var'_d==.;
	replace bl_`var'=0 if bl_`var'==.;

	xi: reg `var' treatment march i.ran_size_sector `varlist' if followup==1,r;
	estimates store `var'1;
	xi: reg `var' treatment bl_`var' bl_`var'_d march i.ran_size_sector `varlist' if followup==1,r;
	estimates store `var'2;

};

xml_tab *  
, nolabel keep(treatment bl_formal_accounts bl_formal_accounts_d _cons) font("Times New Roman" 11) below stats(r2 N) tblanks(1)
line(LAST_ROW 2 COL_NAMES 2)
format(sclr0 nccr3 nccr3)
sheet("Accounts") 
title("Keeps Formal Accounts Dummy") 
save("${results}\table5.xml") 
append;
estimates clear;

**Kling, Leibman, and Katz index;
foreach var in management new_products new_clients new_process new_investors patent_process certification_process new_marketing expansion remodel formal_accounts{;
egen `var'm=mean(`var') if followup==1 & treatment==0; 
egen `var'sd=sd(`var') if followup==1 & treatment==0; 
sort followup `var'm;
replace `var'm=`var'm[_n-1] if followup==1 & `var'm==.;
replace `var'sd=`var'sd[_n-1] if followup==1 & `var'sd==.; 
gen `var'z=(`var'-`var'm)/`var'sd;
drop `var'm `var'sd;
};

egen index=rmean(managementz new_productsz new_clientsz new_processz new_investorsz patent_processz certification_processz new_marketingz expansionz remodelz formal_accountsz);
egen indexm=mean(index) if followup==1; 
egen indexsd=sd(index) if followup==1; 
replace index=(index-indexm)/indexsd; /*standardize value */
drop indexm indexsd;

foreach var in management new_products new_clients new_process new_investors formal_accounts{;
egen `var'm0=mean(`var') if followup==0 & treatment==0; 
egen `var'sd0=sd(`var') if followup==0 & treatment==0; 
sort followup `var'm0;
replace `var'm0=`var'm0[_n-1] if followup==0 & `var'm0==.;
replace `var'sd0=`var'sd0[_n-1] if followup==0 & `var'sd0==.;
gen `var'z0=(`var'-`var'm0)/`var'sd0;
drop `var'm0 `var'sd0;
};

egen bl_index=rmean(managementz0 new_productsz0 new_clientsz0 new_processz0 new_investorsz0 formal_accountsz0);
egen bl_indexm=mean(bl_index) if followup==0; 
egen bl_indexsd=sd(bl_index) if followup==0; 
replace bl_index=(bl_index-bl_indexm)/bl_indexsd; /*standardize value */
drop bl_indexm bl_indexsd;

foreach var in index{;

	sort folio followup;
	by folio: replace bl_`var'=bl_`var'[_n-1] if bl_`var'==.; 
	gen bl_`var'_d=1 if bl_`var'==.;
	replace bl_`var'_d=0 if bl_`var'_d==.;
	replace bl_`var'=0 if bl_`var'==.;

	xi: reg `var' treatment march i.ran_size_sector `varlist' if followup==1,r;
	estimates store `var'1;
	xi: reg `var' treatment bl_`var' bl_`var'_d march i.ran_size_sector `varlist' if followup==1,r;
	estimates store `var'2;

};

xml_tab * 
, nolabel keep(treatment bl_index bl_index_d _cons) font("Times New Roman" 11) below stats(r2 N) tblanks(1)
line(LAST_ROW 2 COL_NAMES 2)
format(sclr0 nccr3 nccr3)
sheet("Index") 
title("Kling, Leibman and Katz index of business processes") 
save("${results}\table5.xml") 
append;
estimates clear;

sum index new_products new_clients new_process new_investors patent_process certification_process new_marketing expansion remodel management formal_accounts if followup==1 & treatment==0;  /*The values on this table correspond to control group means (column 4 on Table 5) */

rename certification_process cert_process;

foreach y in sales profits_sc prod{;
foreach x in index new_products new_clients new_process new_investors patent_process cert_process new_marketing expansion remodel management formal_accounts{;
reg `y'_w1 `x' if followup==1,r;
estimates store `y'_`x';
};
};

xml_tab * 
, nolabel font("Times New Roman" 11) below stats(r2 N) tblanks(1)
line(LAST_ROW 2 COL_NAMES 2)
format(sclr0 nccr3 nccr3)
sheet("Corr") 
title("Correlations with Short-Run Business Outcomes") 
save("${results}\table5.xml") 
append;
estimates clear;

* ------------------------------- *;
* Table 6: Entrepreneurial spirit *;
* ------------------------------- *;

foreach var in motivate routine gov{;
replace entrepreneur_`var'=6-entrepreneur_`var'_reverse if followup==1;
rename entrepreneur_`var'_reverse entrepreneur_`var'r;
replace entrepreneur_`var'r=6-entrepreneur_`var' if followup==0; /*These Agree/Disagree questions were recorded in reverse order at 2009 followup; now changed to same scale */
};

pca entrepreneur_prof_goals entrepreneur_revise_goals entrepreneur_try_again
entrepreneur_successful entrepreneur_daily_goals entrepreneur_motivate
entrepreneur_routine entrepreneur_gov if followup==1;
predict entr_spirit if followup==1, score;

pca entrepreneur_prof_goals entrepreneur_revise_goals entrepreneur_try_again entrepreneur_daily_goals 
entrepreneur_routine entrepreneur_gov if followup==1 & entrepreneur_successful~=. & entrepreneur_motivate~=.;
predict entr_spirit2 if followup==1 & entrepreneur_successful~=. & entrepreneur_motivate~=., score;

pca entrepreneur_prof_goals entrepreneur_revise_goals entrepreneur_try_again
entrepreneur_successful entrepreneur_daily_goals entrepreneur_motivate
entrepreneur_routine entrepreneur_gov if followup==0;
predict bl_entr_spirit if followup==0, score;
replace entr_spirit=bl_entr_spirit if followup==0;

pca entrepreneur_prof_goals entrepreneur_revise_goals entrepreneur_try_again entrepreneur_daily_goals 
entrepreneur_routine entrepreneur_gov if followup==0 & entrepreneur_successful~=. & entrepreneur_motivate~=.;
predict bl_entr_spirit2 if followup==0 & entrepreneur_successful~=. & entrepreneur_motivate~=., score;
replace entr_spirit2=bl_entr_spirit2 if followup==0;

label var entr_spirit "PCA Entrepreneurial Spirit Index";
label var entr_spirit2 "PCA Entrepreneurial Spirit Index w/o questions d) and e)";

foreach var in entrepreneur_prof_goals entrepreneur_revise_goals entrepreneur_try_again entrepreneur_successful entrepreneur_daily_goals entrepreneur_motivate
entrepreneur_routine entrepreneur_gov{;
egen `var'm=mean(`var') if followup==1 & treatment==0; 
egen `var'sd=sd(`var') if followup==1 & treatment==0; 
sort followup `var'm;
replace `var'm=`var'm[_n-1] if followup==1 & `var'm==.;
replace `var'sd=`var'sd[_n-1] if followup==1 & `var'sd==.;
gen `var'z=(`var'-`var'm)/`var'sd;
drop `var'm `var'sd;
};

egen entr_spirit_klk=rmean(entrepreneur_prof_goalsz entrepreneur_revise_goalsz entrepreneur_try_againz entrepreneur_successfulz entrepreneur_daily_goalsz entrepreneur_motivatez
entrepreneur_routinez entrepreneur_govz);
egen entr_spirit_klkm=mean(entr_spirit_klk) if followup==1; 
egen entr_spirit_klksd=sd(entr_spirit_klk) if followup==1; 
replace entr_spirit_klk=(entr_spirit_klk-entr_spirit_klkm)/entr_spirit_klksd;
drop entr_spirit_klkm entr_spirit_klksd;

egen entr_spirit_kl2=rmean(entrepreneur_prof_goalsz entrepreneur_revise_goalsz entrepreneur_try_againz entrepreneur_daily_goalsz entrepreneur_routinez entrepreneur_govz);
egen entr_spirit_kl2m=mean(entr_spirit_kl2) if followup==1; 
egen entr_spirit_kl2sd=sd(entr_spirit_kl2) if followup==1; 
replace entr_spirit_kl2=(entr_spirit_kl2-entr_spirit_kl2m)/entr_spirit_kl2sd;
drop entr_spirit_kl2m entr_spirit_kl2sd;

foreach var in entrepreneur_prof_goals entrepreneur_revise_goals entrepreneur_try_again entrepreneur_successful entrepreneur_daily_goals entrepreneur_motivate
entrepreneur_routine entrepreneur_gov{;
egen `var'm0=mean(`var') if followup==0 & treatment==0; 
egen `var'sd0=sd(`var') if followup==0 & treatment==0; 
sort followup `var'm0;
replace `var'm0=`var'm0[_n-1] if followup==0 & `var'm0==.;
replace `var'sd0=`var'sd0[_n-1] if followup==0 & `var'sd0==.;
gen `var'z0=(`var'-`var'm0)/`var'sd0;
drop `var'm0 `var'sd0;
};

egen bl_entr_spirit_klk=rmean(entrepreneur_prof_goalsz0 entrepreneur_revise_goalsz0 entrepreneur_try_againz0 entrepreneur_successfulz0 entrepreneur_daily_goalsz0 entrepreneur_motivatez0
entrepreneur_routinez0 entrepreneur_govz0);
egen bl_entr_spirit_klkm=mean(bl_entr_spirit_klk) if followup==0; 
egen bl_entr_spirit_klksd=sd(bl_entr_spirit_klk) if followup==0; 
replace bl_entr_spirit_klk=(bl_entr_spirit_klk-bl_entr_spirit_klkm)/bl_entr_spirit_klksd;
drop bl_entr_spirit_klkm bl_entr_spirit_klksd;

egen bl_entr_spirit_kl2=rmean(entrepreneur_prof_goalsz0 entrepreneur_revise_goalsz0 entrepreneur_try_againz0 entrepreneur_daily_goalsz0 entrepreneur_routinez0 entrepreneur_govz0);
egen bl_entr_spirit_kl2m=mean(bl_entr_spirit_kl2) if followup==0;
egen bl_entr_spirit_kl2sd=sd(bl_entr_spirit_kl2) if followup==0; 
replace bl_entr_spirit_kl2=(bl_entr_spirit_kl2-bl_entr_spirit_kl2m)/bl_entr_spirit_kl2sd;
drop bl_entr_spirit_kl2m bl_entr_spirit_kl2sd;

label var entr_spirit_klk "KLK Entrepreneurial Spirit Index";
label var entr_spirit_kl2 "KLK Entrepreneurial Spirit Index w/o questions d) and e)";

rename entrepreneur_revise_goals entrepreneur_rev_goals;
rename entrepreneur_daily_goals entrepreneur_day_goals;
foreach var in prof_goals rev_goals try_again successful day_goals motivater routiner govr{;
rename entrepreneur_`var' entr_`var';
gen bl_entr_`var'=entr_`var' if followup==0;
};

*Note: If a different person was interviewed at follow-up, we don't have their baseline entrepreneurial spirit index;
foreach var in entr_spirit entr_spirit2 entr_spirit_klk entr_spirit_kl2 entr_prof_goals entr_rev_goals entr_try_again entr_motivater entr_successful entr_routiner entr_govr entr_day_goals{;

	sort folio followup;
	by folio: replace bl_`var'=bl_`var'[_n-1] if bl_`var'==.; 
	gen bl_`var'_d=1 if different_person==1 | bl_`var'==.;
	replace bl_`var'_d=0 if different_person==0 & bl_`var'~=.;
	replace bl_`var'=0 if different_person==1 | bl_`var'==.;

	xi: reg `var' treatment march i.ran_size_sector `varlist' if followup==1,r;
	estimates store `var'1;
	xi: reg `var' treatment bl_`var' bl_`var'_d march i.ran_size_sector `varlist' if followup==1,r;
	estimates store `var'2;

};

xml_tab *  
, nolabel keep(treatment bl_entr_spirit bl_entr_spirit_d bl_entr_spirit2 bl_entr_spirit2_d bl_entr_spirit_klk bl_entr_spirit_klk_d bl_entr_spirit_kl2 bl_entr_spirit_kl2_d 
bl_entr_prof_goals bl_entr_prof_goals_d 
bl_entr_rev_goals bl_entr_rev_goals_d bl_entr_try_again bl_entr_try_again_d bl_entr_successful bl_entr_successful_d bl_entr_day_goals bl_entr_day_goals_d bl_entr_motivater bl_entr_motivater_d 
bl_entr_routiner bl_entr_routiner_d bl_entr_govr bl_entr_govr_d _cons) 
font("Times New Roman" 11) below stats(r2 N) tblanks(1)
line(LAST_ROW 2 COL_NAMES 2)
format(sclr0 nccr3 nccr3) 
sheet("Entr Spirit") 
title("Entrepreneurial spirit index and components") 
save("${results}\table6.xml") 
replace;
estimates clear;

/*The values on this table correspond to control group means (column 4 on Table 6) */
sum entr_spirit entr_spirit2 entr_spirit_klk entr_spirit_kl2 entr_prof_goals entr_rev_goals entr_try_again entr_motivater entr_successful entr_routiner entr_govr entr_day_goals if followup==1 & treatment==0;

* ------------------------------ *;
* Table 7: Changes due to crisis *;
* ------------------------------ *;

rename crisis_changes_3 other_changes_a;
egen crisis_changes_3=ends(other_changes_a), punct(,) trim head;
egen other_changes_b=ends(other_changes_a), punct(,) trim tail;
egen crisis_changes_4=ends(other_changes_b), punct(,) trim head;
egen crisis_changes_5=ends(other_changes_b), punct(,) trim tail;
destring crisis_changes_3, replace;
destring crisis_changes_4, replace;
destring crisis_changes_5, replace;

tab crisis_impact if treatment==1;
tab crisis_impact if treatment==0;
/* The lines below create a dummy variable =1 if the firm mentioned experiencing a crisis in any of the following categories */
foreach var in empl wages prod div gov none other{;
gen crisis_`var'=0 if crisis_changes_1~=.;
};

replace crisis_empl=1 if crisis_changes_1==1 | crisis_changes_1==2 | crisis_changes_2==2;

forvalues x=1/5{;
replace crisis_wages=1 if crisis_changes_`x'==3;
};

forvalues x=1/5{;
replace crisis_prod=1 if crisis_changes_`x'==4;
};

forvalues x=1/5{;
replace crisis_div=1 if crisis_changes_`x'==5;
};

forvalues x=1/5{;
replace crisis_gov=1 if crisis_changes_`x'==6;
};
replace crisis_gov=0 if crisis_changes_program=="mipymes campeones" | crisis_changes_program=="150 mipymes campeonas ippc";

forvalues x=1/5{;
replace crisis_none=1 if crisis_changes_`x'==7;
};

forvalues x=1/5{;
replace crisis_other=1 if crisis_changes_`x'==8;
};

egen crisis_count=rsum(crisis_empl crisis_wages crisis_prod crisis_div crisis_gov crisis_other) if crisis_empl~=.;

foreach var in empl wages prod div gov none other{;

xi: reg crisis_`var' treatment march i.ran_size_sector `varlist' if followup==1,r;
estimates store crisis_`var';
};

xi: reg crisis_count treatment march i.ran_size_sector `varlist' if followup==1,r;
estimates store crisis_count;

xml_tab * 
, nolabel keep(treatment _cons) font("Times New Roman" 11) below stats(r2 N) tblanks(1)
line(LAST_ROW 2 COL_NAMES 2)
format(sclr0 nccr3 nccr3)
sheet("Crisis") 
title("Changes made to address crisis") 
save("${results}\table7.xml") 
replace;
estimates clear;

sum crisis_empl - crisis_other if treatment==1;
sum crisis_empl - crisis_other if treatment==0; /* These numbers correspond to control group means in Table 7 column 3 */
sum crisis_count if treatment==0; /* This corresponds to the control group mean for number of changes made in response to a crisis (last number in Table 7 column 3) */
 
* ----------------------------------------- *;
* Table 8: Reasons for not using consulting *;
* ----------------------------------------- *;

foreach var in money cost benefits need know consider other{;
gen ment_r_`var'=0 if mentor_not_use_1~=.;
};
/* The lines below create dummys =1 if factor was mentioned as reason for not using consulting services */
replace ment_r_money=1 if mentor_not_use_1==1;
replace ment_r_cost=1 if mentor_not_use_1==2 | mentor_not_use_2==2;
replace ment_r_benefits=1 if mentor_not_use_1==3 | mentor_not_use_2==3 | mentor_not_use_3==3;
replace ment_r_need=1 if mentor_not_use_1==4 | mentor_not_use_2==4 | mentor_not_use_3==4;
replace ment_r_know=1 if mentor_not_use_1==5 | mentor_not_use_2==5 | mentor_not_use_3==5;
replace ment_r_consider=1 if mentor_not_use_1==6 | mentor_not_use_2==6 | mentor_not_use_3==6;
replace ment_r_other=1 if mentor_not_use_1==7 | mentor_not_use_2==7 | mentor_not_use_3==7;

sum ment_r_* if treatment==0 & mentor_current==0 & mentor_past==0;

* -------------------------------------------------------- *;
* Appendix Table A1: Predicting follow-up survey attrition *;
* -------------------------------------------------------- *;

gen attrited=1 if status==0 | status==3 | status==4;
replace attrited=0 if status==1 | status==2;
sort folio followup;
by folio: replace attrited=attrited[_n+1] if attrited==.; 

tab attrited if followup==0;

sum attrited; /* This provides the mean of the dependent variable */

foreach var in age age_business{;
	gen `var'_d=1 if `var'==.;
	replace `var'_d=0 if `var'_d==.;
	replace `var'=0 if `var'==.;
};

foreach var in commerce services industry bl_empl_paid age age_d male age_business age_business_d bl_lsales bl_lsales_d bl_profits_sc bl_profits_sc_d bl_prod bl_prod_d bl_roa bl_roa_d{;
gen `var'_t=`var'*treatment;
};

reg attrited treatment if followup==0, r;
estimates store attrition1;
reg attrited treatment commerce services bl_empl_paid age age_d male age_business age_business_d bl_lsales bl_lsales_d bl_profits_sc bl_profits_sc_d bl_roa bl_roa_d 
if followup==0, r;
estimates store attrition2;
reg attrited treatment commerce services bl_empl_paid age age_d male age_business age_business_d bl_lsales bl_lsales_d bl_profits_sc bl_profits_sc_d bl_roa bl_roa_d 
commerce_t services_t bl_empl_paid_t age_t age_d_t male_t age_business_t age_business_d_t bl_lsales_t bl_lsales_d_t bl_profits_sc_t bl_profits_sc_d_t bl_roa_t bl_roa_d_t  
if followup==0, r;
estimates store attrition3;
test commerce_t services_t bl_empl_paid_t age_t age_d_t male_t age_business_t age_business_d_t bl_lsales_t bl_lsales_d_t bl_profits_sc_t bl_profits_sc_d_t bl_roa_t bl_roa_d_t;
test commerce_t services_t bl_empl_paid_t age_t male_t age_business_t bl_lsales_t bl_profits_sc_t bl_roa_t;

xml_tab *  
, nolabel keep(treatment commerce services bl_empl_paid age male age_business bl_lsales bl_profits_sc bl_roa 
commerce_t services_t bl_empl_paid_t age_t male_t age_business_t bl_lsales_t bl_profits_sc_t bl_roa_t _cons) 
font("Times New Roman" 11) below stats(r2 N) tblanks(1)
line(LAST_ROW 2 COL_NAMES 2)
format(sclr0 nccr3 nccr3)
sheet("Attrition") 
title("Predicting follow-up survey attrition") 
save("${results}\tableA1.xml") 
replace;
estimates clear;

* -------------------------------------------------------- *;
* Appendix Table A2: Predicting who was found in IMSS data *;
* -------------------------------------------------------- *;

tab imss if followup==0;
sum imss; /* This provides the average of the dependent variable */

reg imss treatment if followup==0, r;
estimates store imss1;
reg imss treatment commerce services bl_empl_paid age age_d male age_business age_business_d bl_lsales bl_lsales_d bl_profits_sc bl_profits_sc_d bl_roa bl_roa_d  
if followup==0, r;
estimates store imss2;
reg imss treatment commerce services bl_empl_paid age age_d male age_business age_business_d bl_lsales bl_lsales_d bl_profits_sc bl_profits_sc_d bl_roa bl_roa_d 
commerce_t services_t bl_empl_paid_t age_t age_d_t male_t age_business_t age_business_d_t bl_lsales_t bl_lsales_d_t bl_profits_sc_t bl_profits_sc_d_t bl_roa_t bl_roa_d_t  
if followup==0, r;
estimates store imss3;
test commerce_t services_t bl_empl_paid_t age_t age_d_t male_t age_business_t age_business_d_t bl_lsales_t bl_lsales_d_t bl_profits_sc_t bl_profits_sc_d_t bl_roa_t bl_roa_d_t;
test commerce_t services_t bl_empl_paid_t age_t male_t age_business_t bl_lsales_t bl_profits_sc_t bl_roa_t;

xml_tab * 
, nolabel keep(treatment commerce services bl_empl_paid age male age_business bl_lsales bl_profits_sc bl_roa 
commerce_t services_t bl_empl_paid_t age_t male_t age_business_t bl_lsales_t bl_profits_sc_t bl_roa_t _cons) 
font("Times New Roman" 11) below stats(r2 N) tblanks(1)
line(LAST_ROW 2 COL_NAMES 2)
format(sclr0 nccr3 nccr3)
sheet("IMSS Attrit") 
title("Predicting which firms were matched with IMSS data") 
save("${results}\tableA2.xml") 
replace;
estimates clear;

* ------------------------------------------------------------------------------- *;
* Appendix Table A3: Number of Enterprises Surveyed Each Month (Follow-Up Survey) *; 
* ------------------------------------------------------------------------------- *;

tab month if treatment==1;
tab month if treatment==0;

* ------------------------------------------------------------------ *;
* Appendix Table A4: Short-Run business outcomes for balanced sample *;
* ------------------------------------------------------------------ *;

preserve;

keep if empl_paid~=. & sales~=. & costs~=. & lassets~=. & prod~=.;

**Employment;

foreach var in empl_paid lempl{;

	xi: reg `var' treatment march i.ran_size_sector `varlist' if followup==1,r;
	estimates store `var'1;
	xi: reg `var' treatment bl_`var' march i.ran_size_sector `varlist' if followup==1,r;
	estimates store `var'2;

};

**Sales, profits, assets, and productivity;

foreach var in sales lsales costs profits_sc lassets prod roa{;

	xi: reg `var' treatment march i.ran_size_sector `varlist' if followup==1,r;
	estimates store `var'1;
	xi: reg `var' treatment bl_`var' bl_`var'_d march i.ran_size_sector `varlist' if followup==1,r;
	estimates store `var'2;

};
	
xml_tab *
, nolabel keep(treatment _cons) font("Times New Roman" 11) below stats(r2 N) tblanks(1)
line(LAST_ROW 2 COL_NAMES 2)
format(sclr0 nccr3 nccr3)
sheet("Balanced sample") 
save("${results}\tableA4.xml") 
replace;
estimates clear;

sum empl_paid lempl sales lsales costs profits_sc lassets prod roa if treatment==0 & followup==1; /* These numbers correspond to those for the control group mean in Appendix Table A4 column 3 */

restore;

* ------------------------------------------- *;
* Appendix Table A5: Difference-in-difference *;
* ------------------------------------------- *;

gen treat_followup=treatment*followup;
replace march=0 if followup==0; 

foreach var in empl_paid lempl sales lsales costs profits_sc lassets prod roa{;

	xi: reg `var' treat_followup treatment followup march i.ran_size_sector `varlist',r;
	estimates store `var';
	
	foreach name in w trim{;

		xi: reg `var'_`name'1 treat_followup treatment followup march i.ran_size_sector `varlist',r;
		estimates store `var'_`name'1;
		
	};

};

xml_tab *
, nolabel keep(treat_followup treatment followup _cons) font("Times New Roman" 11) below stats(r2 N) tblanks(1)
line(LAST_ROW 2 COL_NAMES 2)
format(sclr0 nccr3 nccr3)
notes(All regressions include strata dummies and re-randomization controls) 
sheet("DD") 
save("${results}\tableA5.xml")  
replace;

sum empl_paid lempl sales lsales costs profits_sc lassets prod roa if treatment==0 & followup==1; /*The values on this table correspond to control group means (column 4 on Table A5) */ 

estimates clear;

* ----------------------------------------------- *;
* Appendix Table A6: Follow-up summary statistics *;
* ----------------------------------------------- *;

preserve;

keep if followup==1;

matrix t=J(68, 3, .);
matrix colnames t=mean_treatment mean_control diff;

local row_num=1;

foreach var in 
sales sales_w1 costs costs_w1 profits_sc profits_sc_w1 assets assets_w1 prod prod_w1 roa roa_w1{;
		
		qui sum `var' if treatment == 1;
		matrix t[`row_num', 1] = r(mean);
		matrix t[`row_num' + 1, 1] = r(sd);

		qui sum `var' if treatment == 0;
		matrix t[`row_num', 2] = r(mean);
		matrix t[`row_num' + 1, 2] = r(sd);

		reg `var' treatment,r;
		matrix t[`row_num', 3] = _b[treatment];
		matrix t[`row_num' + 1, 3] = 2*ttail(e(df_r),abs(_b[treatment]/_se[treatment]));

		local row_num = `row_num' + 2; 
			
};

matrix list t;
clear;

svmat t, names(col);
label variable mean_treatment "Treatment";
label variable mean_control "Control";
label variable diff "Diff";

generate variable="";
order variable;
local i = 1;
foreach item in   
sales sd sales_w1 sd costs sd costs_w1 sd profits_sc sd profits_sc_w1 sd assets sd assets_w1 sd prod sd prod_w1 sd roa sd roa_w1 sd  
{;
		
		replace variable = "`item'" in `i';
		local i = `i' + 1;
};

save "${results}\tableA6_all", replace;
restore;

**Follow-up statistics without firms that did not take-up the program;

preserve;

keep if followup==1;
drop if treatment==1 & in_program==0;

matrix t=J(68, 3, .);
matrix colnames t=mean_treatment mean_control diff;

local row_num=1;

foreach var in    
sales sales_w1 costs costs_w1 profits_sc profits_sc_w1 assets assets_w1 prod prod_w1 roa roa_w1{;
		
		qui sum `var' if treatment == 1;
		matrix t[`row_num', 1] = r(mean);
		matrix t[`row_num' + 1, 1] = r(sd);

		qui sum `var' if treatment == 0;
		matrix t[`row_num', 2] = r(mean);
		matrix t[`row_num' + 1, 2] = r(sd);

		reg `var' treatment,r;
		matrix t[`row_num', 3] = _b[treatment];
		matrix t[`row_num' + 1, 3] = 2*ttail(e(df_r),abs(_b[treatment]/_se[treatment]));

		local row_num = `row_num' + 2; 
			
};

matrix list t;
clear;

svmat t, names(col);
label variable mean_treatment "Treatment";
label variable mean_control "Control";
label variable diff "Diff";

generate variable="";
order variable;
local i = 1;
foreach item in 
sales sd sales_w1 sd costs sd costs_w1 sd profits_sc sd profits_sc_w1 sd assets sd assets_w1 sd prod sd prod_w1 sd roa sd roa_w1 sd 
{;
		
		replace variable = "`item'" in `i';
		local i = `i' + 1;
};

save "${results}\tableA6_treatment", replace;
restore;

* -----------------------------------------------------*;
* Appendix Table A7: Robustness Checks for TFP Measure *;
* -----------------------------------------------------*;

**Employment;

foreach var in lempl{;

	xi: reg `var' treatment march i.ran_size_sector `varlist' if followup==1,r;
	estimates store `var'1;
	xi: reg `var' treatment bl_`var' march i.ran_size_sector `varlist' if followup==1,r;
	estimates store `var'2;

};

foreach var in lsales_prod lcost_materials prod_materials solow{;

	gen bl_`var'=`var' if followup==0;
	sort folio followup;
	by folio: replace bl_`var'=bl_`var'[_n-1] if bl_`var'==.;
	gen bl_`var'_d=1 if bl_`var'==.;
	replace bl_`var'_d=0 if bl_`var'_d==.;
	replace bl_`var'=0 if bl_`var'==.;
	
};

**Sales, profits, assets, and productivity;

foreach var in prod lsales_prod lassets lcost_materials prod_materials solow{;

	xi: reg `var' treatment march i.ran_size_sector `varlist' if followup==1,r;
	estimates store `var'1;
	xi: reg `var' treatment bl_`var' bl_`var'_d march i.ran_size_sector `varlist' if followup==1,r;
	estimates store `var'2;

};
	
xml_tab *
, nolabel keep(treatment _cons) font("Times New Roman" 11) below stats(r2 N) tblanks(1)
line(LAST_ROW 2 COL_NAMES 2)
format(sclr0 nccr3 nccr3)
sheet("TFP Robustness") 
save("${results}\tableA7.xml") 
replace;
estimates clear;

sum prod lsales_prod lempl lassets lcost_materials prod_materials solow if treatment==0 & followup==1;

* --------------------------------------- *;
* Matching treated firms to control group *;
* --------------------------------------- *;

sort folio followup;
by folio: replace status=status[_n+1] if status==.;

gen treated=1 if in_program==1 & treatment==1;
replace treated=0 if in_program==0 & treatment==0;

pscore treated industry male empl_paid age_business bl_prod bl_prod_d if followup==0 & treated~=., pscore(score);

set seed 12345;
gen rand=uniform();
sort rand;
gen logitscore=logit(score);
sum logitscore;
disp .4016847*.2;
psmatch2 treated, pscore(logitscore) cal(0.08033694) noreplacement;

sort folio followup;
by folio: replace _w=_w[_n-1] if _w==.;

* --------------------------------------------------------- *;
* Appendix Table A8: Baseline statistics for matched sample *;
* --------------------------------------------------------- *;

preserve;
keep if followup==0 & _w==1;
tab treatment; /* These numbers correspond to the sample sizes listed in Table A7 */

matrix t=J(68, 3, .);
matrix colnames t=mean_treated mean_matched diff;

local row_num=1;

foreach var in industry commerce services empl_paid age male schooling_years age_business  
sales sales_w1 costs costs_w1 profits_sc profits_sc_w1 assets assets_w1 prod prod_w1 roa roa_w1{;
		
		qui sum `var' if treated == 1;
		matrix t[`row_num', 1] = r(mean);
		matrix t[`row_num' + 1, 1] = r(sd);

		qui sum `var' if treated == 0;
		matrix t[`row_num', 2] = r(mean);
		matrix t[`row_num' + 1, 2] = r(sd);

		reg `var' treated,r;
		matrix t[`row_num', 3] = _b[treated];
		matrix t[`row_num' + 1, 3] = 2*ttail(e(df_r),abs(_b[treated]/_se[treated]));

		local row_num = `row_num' + 2; 
			
};

matrix list t;
clear;

svmat t, names(col);
label variable mean_treated "Treated";
label variable mean_matched "Matched";
label variable diff "Diff";

generate variable="";
order variable;
local i = 1;
foreach item in industry sd commerce sd services sd empl_paid sd age sd male sd schooling_years sd age_business sd  
sales sd sales_w1 sd costs sd costs_w1 sd profits_sc sd profits_sc_w1 sd assets sd assets_w1 sd prod sd prod_w1 sd roa sd roa_w1 sd 
{;
		
		replace variable = "`item'" in `i';
		local i = `i' + 1;
};

save "${results}\tableA8", replace;
restore;

* ----------------------------------------------------------------- *;
* Appendix Table A9: Short-run business outcomes for matched sample *;
* ----------------------------------------------------------------- *;

foreach var in empl_paid{;

	xi: reg `var' treated march if followup==1 & _w==1,r;
	estimates store `var'1;
	xi: reg `var' treated bl_`var' march if followup==1 & _w==1,r;
	estimates store `var'2;

	foreach name in w trim{;
	
		foreach x in 1{;

			xi: reg `var'_`name'`x' treated march if followup==1 & _w==1,r;
			estimates store `var'1_`name'`x';
			xi: reg `var'_`name'`x' treated bl_`var' march if followup==1 & _w==1,r;	
			estimates store `var'2_`name'`x';
		
		};

	};

xml_tab *
, nolabel keep(treated bl_`var' _cons) font("Times New Roman" 11) below stats(r2 N) tblanks(1)
line(LAST_ROW 2 COL_NAMES 2)
format(sclr0 nccr3 nccr3)
sheet("`var'") 
save("${results}\tableA9.xml") 
replace;
estimates clear;

};

foreach var in lempl{;

	xi: reg `var' treated march if followup==1 & _w==1,r;
	estimates store `var'1;
	xi: reg `var' treated bl_`var' march if followup==1 & _w==1,r;
	estimates store `var'2;

	foreach name in w trim{;
	
		foreach x in 1{;

			xi: reg `var'_`name'`x' treated march if followup==1 & _w==1,r;
			estimates store `var'1_`name'`x';
			xi: reg `var'_`name'`x' treated bl_`var' march if followup==1 & _w==1,r;	
			estimates store `var'2_`name'`x';
		
		};

	};

xml_tab *
, nolabel keep(treated bl_`var' _cons) font("Times New Roman" 11) below stats(r2 N) tblanks(1)
line(LAST_ROW 2 COL_NAMES 2)
format(sclr0 nccr3 nccr3)
sheet("`var'") 
save("${results}\tableA9.xml") 
append;
estimates clear;

};

**Sales, profits, assets, and productivity;

foreach var in sales lsales costs profits_sc lassets prod roa{;

	xi: reg `var' treated march if followup==1 & _w==1,r;
	estimates store `var'1;
	xi: reg `var' treated bl_`var' bl_`var'_d march if followup==1 & _w==1,r;
	estimates store `var'2;
	
	foreach name in w trim{;
	
		foreach x in 1{;

		xi: reg `var'_`name'`x' treated march if followup==1 & _w==1,r;
		estimates store `var'1_`name'`x';
		xi: reg `var'_`name'`x' treated bl_`var' bl_`var'_d march if followup==1 & _w==1,r;	
		estimates store `var'2_`name'`x';
		
		};

	};

xml_tab *
, nolabel keep(treated bl_`var' bl_`var'_d _cons) font("Times New Roman" 11) below stats(r2 N) tblanks(1)
line(LAST_ROW 2 COL_NAMES 2)
format(sclr0 nccr3 nccr3)
sheet("`var'") 
save("${results}\tableA9.xml") 
append;
estimates clear;

};

sum empl_paid lempl sales lsales costs profits_sc lassets prod roa if treated==0 & followup==1 & _w==1; /* These values correspond to those for the matched control group listed in Appendix Table A8 column 7 */

* ------------------------------------------------------ *;
* Appendix Table A10: Heterogeneous effects by firm size *;
* ------------------------------------------------------ *;

gen sme=0 if ran_size_sector==11 | ran_size_sector==12 | ran_size_sector==13;
replace sme=1 if ran_size_sector==21 | ran_size_sector==22 | ran_size_sector==23 
| ran_size_sector==31 | ran_size_sector==32 | ran_size_sector==33;
gen treat_sme=treatment*sme;

**Employment;

foreach var in empl_paid lempl{;

	xi: reg `var' treatment treat_sme bl_`var' march i.ran_size_sector `varlist' if followup==1,r;
	estimates store `var';
	test treatment+treat_sme==0;
	
};

**Sales, profits, assets, and productivity;

foreach var in sales lsales costs profits_sc lassets prod roa{;

	xi: reg `var' treatment treat_sme bl_`var' bl_`var'_d march i.ran_size_sector `varlist' if followup==1,r;
	estimates store `var';
	test treatment+treat_sme==0;
	
};

xml_tab *
, nolabel keep(treatment treat_sme _cons) font("Times New Roman" 11) below stats(r2 N) tblanks(1)
line(LAST_ROW 2 COL_NAMES 2)
format(sclr0 nccr3 nccr3) 
sheet("Hetero_size") 
title("Short-run business outcomes") 
save("${results}\tableA10.xml") 
replace;
estimates clear;

* ----------------------------------------------------- *;
* Appendix Table A11: Heterogeneous effects by industry *;
* ----------------------------------------------------- *;

gen manuf=1 if ran_size_sector==11 | ran_size_sector==21 | ran_size_sector==31;
replace manuf=0 if ran_size_sector==12 | ran_size_sector==13 | ran_size_sector==22 
| ran_size_sector==23 | ran_size_sector==32 | ran_size_sector==33;
gen treat_manuf=treatment*manuf;

gen commer=1 if ran_size_sector==12 | ran_size_sector==22 | ran_size_sector==32;
replace commer=0 if ran_size_sector==11 | ran_size_sector==13 | ran_size_sector==21 
| ran_size_sector==23 | ran_size_sector==31 | ran_size_sector==33;
gen treat_commer=treatment*commer;

**Employment;

foreach var in empl_paid lempl{;

	xi: reg `var' treatment treat_manuf treat_commer bl_`var' march i.ran_size_sector `varlist' if followup==1,r;
	estimates store `var';
	test treatment+treat_manuf==0;
	test treatment+treat_commer==0;

};

**Sales, profits, assets, and productivity;

foreach var in sales lsales costs profits_sc lassets prod roa{;

	xi: reg `var' treatment treat_manuf treat_commer bl_`var' bl_`var'_d march i.ran_size_sector `varlist' if followup==1,r;
	estimates store `var';
	test treatment+treat_manuf==0;
	test treatment+treat_commer==0;
	
};

xml_tab * 
, nolabel keep(treatment treat_manuf treat_commer _cons) font("Times New Roman" 11) below stats(r2 N) tblanks(1)
line(LAST_ROW 2 COL_NAMES 2)
format(sclr0 nccr3 nccr3)
sheet("Hetero_sector") 
title("Short-run business outcomes") 
save("${results}\tableA11.xml") 
replace;
estimates clear;

* ---------------------------------------------------- *;
* Appendix Table A12: Non-response on follow-up survey *;
* ---------------------------------------------------- *;

gen nosales=100 if status==1 & followup==1 & lsales==.;
replace nosales=0 if status==1 & followup==1 & lsales~=.;

sum no_contact nosales if treatment==1;
sum no_contact nosales if treatment==0;

reg no_contact treatment, r;
reg nosales treatment, r;

* ---------------------------------------------------------------------- *;
* Appendix Table A13: Short-run business outcomes for firms in IMSS data *;
* ---------------------------------------------------------------------- *;

preserve;
keep if imss==1;

**Employment;

foreach var in empl_paid lempl{;

	xi: reg `var' treatment march i.ran_size_sector `varlist' if followup==1,r;
	estimates store `var'1;
	xi: reg `var' treatment bl_`var' march i.ran_size_sector `varlist' if followup==1,r;
	estimates store `var'2;

};

**Sales, profits, assets, and productivity;

foreach var in sales lsales costs profits_sc lassets prod roa{;

	xi: reg `var' treatment march i.ran_size_sector `varlist' if followup==1,r;
	estimates store `var'1;
	xi: reg `var' treatment bl_`var' bl_`var'_d march i.ran_size_sector `varlist' if followup==1,r;
	estimates store `var'2;

};

xml_tab *
, nolabel keep(treatment _cons) font("Times New Roman" 11) below stats(r2 N) tblanks(1)
line(LAST_ROW 2 COL_NAMES 2)
format(sclr0 nccr3 nccr3) 
sheet("IMSS") 
title("Short-run business outcomes") 
save("${results}\tableA13.xml") 
replace;
estimates clear;

sum empl_paid lempl sales lsales costs profits_sc lassets prod roa if treatment==0 & followup==1; /*The values on this table correspond to control group means (column 3 on Table A12) */

restore;

* -------------------------------------------------------------------- *;
* Online Appendix Table A3.1: Short-run business outcomes - Followup 2 *;
* -------------------------------------------------------------------- *;

**Employment;

gen subdate=mofd(dofc(submissiondate));
format subdate %tm;
gen dmar2015=(subdate==tm(2015m3));

foreach var in empl_paid{;

	xi: reg `var' treatment dmar2015 if followup==2,r;
	estimates store `var'1;
	xi: reg `var' treatment dmar2015 bl_`var' if followup==2,r;
	estimates store `var'2;
	xi: reg `var' treatment dmar2015 i.ran_size_sector `varlist' if followup==2,r;
	estimates store `var'3;
	xi: reg `var' treatment dmar2015 bl_`var' i.ran_size_sector `varlist' if followup==2,r;
	estimates store `var'4;

xml_tab *
, nolabel keep(treatment bl_`var' _cons) font("Times New Roman" 11) below stats(r2 N) tblanks(1)
line(LAST_ROW 2 COL_NAMES 2)
format(sclr0 nccr3 nccr3)
sheet("`var'") 
save("${results}\online_tableA3.1.xml") 
replace;
estimates clear;

};

foreach var in lempl{;

	xi: reg `var' treatment dmar2015 if followup==2,r;
	estimates store `var'1;
	xi: reg `var' treatment dmar2015 bl_`var' if followup==2,r;
	estimates store `var'2;
	xi: reg `var' treatment dmar2015 i.ran_size_sector `varlist' if followup==2,r;
	estimates store `var'3;
	xi: reg `var' treatment dmar2015 bl_`var' i.ran_size_sector `varlist' if followup==2,r;
	estimates store `var'4;

xml_tab *
, nolabel keep(treatment bl_`var' _cons) font("Times New Roman" 11) below stats(r2 N) tblanks(1)
line(LAST_ROW 2 COL_NAMES 2)
format(sclr0 nccr3 nccr3)
sheet("`var'") 
save("${results}\online_tableA3.1.xml") 
append;
estimates clear;

};

**Sales, profits, assets, and productivity;

foreach var in sales lsales costs profits_sc lassets prod roa{;

	xi: reg `var' treatment dmar2015 if followup==2,r;
	estimates store `var'1;
	xi: reg `var' treatment dmar2015 bl_`var' bl_`var'_d if followup==2,r;
	estimates store `var'2;
	xi: reg `var' treatment dmar2015 i.ran_size_sector `varlist' if followup==2,r;
	estimates store `var'3;
	xi: reg `var' treatment dmar2015 bl_`var' bl_`var'_d i.ran_size_sector `varlist' if followup==2,r;
	estimates store `var'4;

xml_tab *
, nolabel keep(treatment bl_`var' bl_`var'_d _cons) font("Times New Roman" 11) below stats(r2 N) tblanks(1)
line(LAST_ROW 2 COL_NAMES 2)
format(sclr0 nccr3 nccr3) 
sheet("`var'") 
save("${results}\online_tableA3.1.xml") 
append;
estimates clear;

};

sum empl_paid lempl sales lsales costs profits_sc lassets prod roa if treatment==0 & followup==2;

* ----------------------------------------------------------------------------- *;
* Online Appendix Table A3.2: Comparison of 2014 follow-up survey and IMSS data *;
* ----------------------------------------------------------------------------- *;

sum P21a P21b P21c P191a if imss==1 & treatment==1 & P21a>=1 & P21a<=40;
sum P21a P21b P21c P191a if imss==1 & treatment==0 & P21b>=1 & P21b<=100;
*Note: IMSS data means and std dev from Table 4;

log close;
