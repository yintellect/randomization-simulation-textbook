clear
use "Fieldhouse_et_al_unpublished_2010.dta"

*part c
	rename m mail
	rename p phone
	rename c phone_contact
	rename y vote
	gen mailphone = mail*phone
	gen mailphone_contact = mail*phone_contact
	ivregress 2sls vote (phone_contact mailphone_contact = phone mailphone) mail 
	//IV reg is off
