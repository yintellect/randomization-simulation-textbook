*---------------------------------------------------------------------
* analysis, Campbell's replication of Room with a Viewpoint.
*---------------------------------------------------------------------

tab  reuse_dummy_recode Treat if  days_stay_max > 1 & daterank==1 ,col chi
bys Hotel: tab  reuse_dummy_recode Treat if  days_stay_max > 1 & daterank==1,col chi

tab  avg_reuse_rate Treat if  days_stay_max > 1 & daterank==1 ,col chi
bys Hotel: tab avg_reuse_rate Treat if  days_stay_max > 1 & daterank==1,col chi

* here is the replication regression

reg  reuse_dummy_recode Treatment if  days_stay_max > 1 & daterank==1
bys Hotel: reg  reuse_dummy_recode Treatment if  days_stay_max > 1 & daterank==1

reg  avg_reuse_rate Treatment if  days_stay_max > 1 & daterank==1

reg   Num_Towels_Reused Treatment if  days_stay_max > 1 & daterank==1

* ...and various other versions
reg  avg_num_towels_reused Treatment if  days_stay_max > 1 & daterank==1


reg  reuse_dummy_recode Treatment if  days_stay_max > 1,cl(unique_spell)
reg   Num_Towels_Reused Treatment if  days_stay_max > 1,cl(unique_spell)
