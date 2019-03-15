// data on FEIDA website is mismatched

clear
// use "Fieldhouse_et_al_unpublished_2010.dta" (mismatched)

use "Fieldhouse_et_al_unpublished_2010_expanded.dta"

/*----------------------------------------------
 part c
----------------------------------------------*/

rename m mail
rename p phone_assign
rename c phone_contact
rename y vote
rename c_m phone_contact_mail
rename p_m phone_assign_mail


gen mail_phone_contact = mail*phone_contact
gen mail_phone_assign = mail*phone_assign
	
ivregress 2sls vote mail (mail_phone_contact phone_contact = mail phone_assign mail_phone_assign)


