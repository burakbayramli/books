
*This program creates time invariant measures of paramilitary and guerrilla presence



set logtype text 
set more off

cd "E:\the monopoly of violence 2012\data\presence and controls"


*Displaced data

use dispar, clear
egen par_9707d=rowtotal(par1997-par2007)
egen par_9701d=rowtotal(par1997-par2001)
drop par1997-par2007 
sort cod_dane

tempfile dis_par
save "`dis_par'", replace


use dispguer, clear
egen guer_9707d=rowtotal(guer1997-guer2007)
egen guer_9701d=rowtotal(guer1997-guer2001)
drop  guer1997-guer2007 
sort cod_dane


merge cod_dane using  "`dis_par'"

drop _merge
sort cod_dane

tempfile disp
save "`disp'", replace




* attacks data


use allguer_allauc, clear

collapse (sum) allauc allguer, by(cod_dane) 

rename allauc  par_9705
rename allguer guer_9705

sort cod_dane 

tempfile at
save "`at'", replace



use allguer_allauc, clear

keep if year < 2002
collapse (sum) allauc allguer, by(cod_dane) 
rename allauc  par_9701
rename allguer  guer_9701

sort cod_dane
merge cod_dane using "`at'"
drop _merge
sort cod_dane



merge cod_dane using "`disp'"

drop _merge
sort cod_dane


tempfile pres
save "`pres'", replace

sort cod_dane

merge cod_dane using controls

drop _merge 
*only values with merge=2 or 3.
*controls database is the database that identifies the crossection. 

*interpreting missings as no-events

replace par_9707d=0 if  par_9707d==.
replace guer_9707d=0 if  guer_9707d==.

replace par_9705=0 if  par_9705==.
replace guer_9705=0 if  guer_9705==.

replace par_9701d=0 if  par_9701d==.
replace guer_9701d=0 if  guer_9701d==.

replace par_9701=0 if  par_9701==.
replace guer_9701=0 if  guer_9701==.


*scaling by population

foreach  x in 9707d 9705 9701d 9701 {

replace par_`x'=1000*par_`x'/pobav if par_`x'>0
replace guer_`x'=1000*guer_`x'/pobav if guer_`x'>0

}
* This is done like that since for some municipalities we do not have the population data
* However if for these attacks or displaced were zero, attacks or displaced for any size of the 
* population is zero. 



*******************************Principal components

pca guer_9701 guer_9701d 

predict g_presence


pca par_9701 par_9701d 

predict p_presence


******************************************************


* High presence dummies (attacks and displaced)


foreach  x in 9701 9701d 9705 9707d {

_pctile par_`x', p(75)
return list

gen  dumpar_`x'=0 
replace dumpar_`x'=1 if  par_`x' >r(r1) 
replace dumpar_`x'=. if par_`x' ==.

_pctile guer_`x', p(75)
return list

gen  dumguer_`x'=0 
replace dumguer_`x'=1 if  guer_`x' >r(r1) 
replace dumguer_`x'=. if guer_`x' ==.

}




*dummy, presence=1 if high displaced and high attacks

gen p_both=.
replace p_both=0 if dumpar_9701!=. | dumpar_9701d!=.
replace p_both= 1 if dumpar_9701==1 & dumpar_9701d==1


gen g_both=.
replace g_both=0 if dumguer_9701!=. | dumguer_9701d!=.
replace g_both= 1 if dumguer_9701==1 & dumguer_9701d==1

sort cod_dane 

save  presence, replace 


* generate means for table 2 

su par_9701
su par_9701 if dumpar_9701 ==1
su par_9701  if dumpar_9701==0

su guer_9701
su guer_9701 if dumpar_9701==1
su guer_9701  if dumpar_9701==0

su ags
su ags if dumpar_9701==1
su ags if dumpar_9701==0

su up86
su up86 if dumpar_9701==1
su up86 if dumpar_9701==0

su ginix
su ginix if dumpar_9701==1
su ginix if dumpar_9701==0


su nbi
su nbi if dumpar_9701==1
su nbi if dumpar_9701==0

su pobav
su pobav if dumpar_9701==1
su pobav if dumpar_9701==0

su rur
su rur if dumpar_9701==1
su rur if dumpar_9701==0


su discap
su discap if dumpar_9701==1
su discap if dumpar_9701==0


su altura
su altura if dumpar_9701==1
su altura if dumpar_9701==0

su precipitacion
su precipitacion if dumpar_9701==1
su precipitacion if dumpar_9701==0

su damapola
su damapola if dumpar_9701==1
su damapola if dumpar_9701==0


su dcoca
su dcoca if dumpar_9701==1
su dcoca if dumpar_9701==0









