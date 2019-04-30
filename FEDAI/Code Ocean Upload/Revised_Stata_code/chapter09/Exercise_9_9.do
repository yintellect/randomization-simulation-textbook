log using ../results/chapter09/exercise_9_9, replace

clear

import delim ../data/chapter09/Fieldhouse_et_al_unpublished_2010_expanded,clear
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


log close
translate ../results/chapter09/exercise_9_9.smcl ../results/chapter09/exercise_9_9.pdf, translator(smcl2pdf)
