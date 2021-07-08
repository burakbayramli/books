
*This program creates time varying measures of paramilitary and guerrilla presence

clear 

set mem 500m
set matsize 1500
set logtype text 
set more off


cd "E:\the monopoly of violence 2012\data\presence and controls"




******************displaced data. Database with number of displaced per year.


*displaced by the paramilitary. 
use dispar, clear

gen par_1997d=par1997*2
*since data starts in 1997 and need to use average between 1996 and 1997 
egen par_2001d= rsum (par2000 par2001)
egen par_2005d=rsum (par2004 par2005)
drop par1997-par2007
sort cod_dane

tempfile disparm
save "`disparm'", replace



*displaced by the guerrilla. 

use dispguer, clear

gen guer_1997d=guer1997*2
egen guer_2001d=rsum (guer2000 guer2001)
egen guer_2005d=rsum (guer2004  guer2005)

drop  guer1997-guer2007 
sort cod_dane

tempfile disp
save "`disp'", replace


merge cod_dane using  "`disparm'"

drop _merge
sort cod_dane

save "`disp'", replace



* Organizing in Panel form.

foreach  x in 1997 2001 2005    {

use "`disp'", clear

keep par_`x'd guer_`x'd cod_dane
rename par_`x'd parvd
rename guer_`x'd  guervd
gen year=`x'+1

tempfile dis`x'


save  "`dis`x''"

}

append using "`dis2001'"

append using "`dis1997'"

sort year cod_dane

tempfile dis
save "`dis'"




******************attacks data. Database with sum of paramilitary events ("attacks") per year. You can find 
*"****************raw data and aggregation in excel book "attacks" 


use allguer_allauc, clear

keep if year==1997
replace year=year+1
replace allauc=allauc*2
replace allguer=allguer*2
keep year cod_dane allauc allguer
rename allauc parv
rename allguer guerv

tempfile a1998
save "`a1998''"

use allguer_allauc, clear

keep if (year == 2000 | year == 2001)
collapse (sum) allauc allguer, by(cod_dane) 

rename allauc parv
rename allguer guerv

gen year=2002

tempfile a2002
save "`a2002''"


use allguer_allauc, clear

keep if (year == 2004 | year ==2005)
collapse (sum) allauc allguer, by(cod_dane) 

rename allauc parv
rename allguer guerv

gen year=2006

append using "`a2002''" 

append using "`a1998''"


sort year cod_dane 

merge year cod_dane using "`dis'"

drop _merge 



sort cod_dane year  

tempfile tempo
save `tempo'

******following lines of program are just to have the samecrossection in all three years. We 
use controls
keep pobav cod_dane
gen year=1998
tempfile a
save `a'


use controls
keep pobav cod_dane
gen year=2002
tempfile b
save `b'


use controls
keep pobav cod_dane
gen year=2006

append using `a'
append using `b'

sort cod_dane year

tempfile c
save `c'

use `tempo'

merge cod_dane year using `c'

************

drop _merge


*Municipalities with no presence in the two years windows
replace parv=0 if parv==. 
replace guerv=0 if guerv==. 
replace parvd=0 if parvd==. 
replace guervd=0 if guervd==. 


replace parv=1000*parv/pobav if parv>0
replace guerv=1000*guerv/pobav if guerv >0
replace parvd=1000*parvd/pobav if parvd>0
replace guervd=1000*guervd/pobav if guervd>0

* This is done like that since for some municipalities we do not have the population data
* However if for these attacks or displaced were zero, attacks or displaced for any size of the 
* population is zero. 


****************Principal components ******************************************** 

pca guerv guervd 

predict g_presencev


pca parv parvd 

predict p_presencev

*********************************************************************************


*time varying dummies of High Presence

foreach y in parv parvd guerv guervd  {

gen dum`y'=0

_pctile `y' , p(75) 
return list 
*just prints the cutoff value
replace dum`y'=1 if (`y' > r(r1))
replace dum`y'=. if (`y' ==.)
}


*dummy, presence=1 if high displaced and high attacks

gen p_bothv=.
replace p_bothv=0 if dumparv!=. |  dumparvd!=.
replace p_bothv=1 if dumparv==1 & dumparvd==1



gen g_bothv=.
replace g_bothv=0 if dumguerv!=. |  dumguervd!=.
replace g_bothv= 1 if dumguerv==1 & dumguervd==1


sort year cod_dane

drop pobav

save presence_timev, replace 

sort cod_dane


* and more descriptive statistics for table 2
merge cod_dane using presence


su parv if year ==1998 
su parv if year ==1998  & dumpar_9701==1
su parv if year ==1998  & dumpar_9701==0


su parv if year ==2002
su parv if year ==2002  & dumpar_9701==1
su parv if year ==2002  & dumpar_9701==0


su parv if year ==2006
su parv if year ==2006  & dumpar_9701==1
su parv if year ==2006  & dumpar_9701==0


********

su guerv if year ==1998 
su guerv if year ==1998  & dumpar_9701==1
su guerv if year ==1998  & dumpar_9701==0


su guerv if year ==2002
su guerv if year ==2002  & dumpar_9701==1
su guerv if year ==2002  & dumpar_9701==0


su guerv if year ==2006
su guerv if year ==2006  & dumpar_9701==1
su guerv if year ==2006  & dumpar_9701==0







