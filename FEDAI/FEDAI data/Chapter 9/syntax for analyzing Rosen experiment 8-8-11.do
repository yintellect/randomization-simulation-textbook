* program to estimate F-statistics for Rosen's letter-writing experiment

gen J= Colin_Jose
gen G= Good_Bad_Grammar
gen JG=J*G
gen H=Hispanic
gen JH=J*H
gen GH=G*H
gen JGH=J*G*H

* compare null and alternative models for the sample as a whole
reg  Response J G
reg  Response J G JG
test JG

* introduce interactions with Hispanic legislator
reg  Response J G JG H
reg  Response J G JG H JH GH JGH
test  JH GH JGH

* add party just for fun
replace Dem_GOP=0 if Dem_GOP==2

gen P=0
replace P=1 if Dem_GOP==1

gen JP=J*P
gen GP=G*P
gen HP=H*P
gen JGP=J*G*P
gen JHP=J*H*P
gen GHP=G*H*P
gen JGHP=J*G*H*P

reg  Response J G JG H JH GH JGH P JP GP HP JGP JHP GHP JGHP
test JP GP JGP JHP GHP JGHP
