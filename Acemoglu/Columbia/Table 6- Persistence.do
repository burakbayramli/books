




*this program runs persistence regressions (robustness, three years windows)


clear 

set mem 500m
set matsize 1500
set logtype text 
set more off




*FIRST IMPORT DATA on Presence and Create windows. 


cd "E:\the monopoly of violence 2012\data\presence and controls"


***************** attacks data(3  years)

use allguer_allauc, clear
*attacks database 

*attacks before 2002
keep if (year == 2001 | year == 2000 | year == 1999)
collapse (sum) allauc allguer, by(cod_dane) 

rename allauc  par_9901
rename allguer guer_9901

tempfile at_9901
save "`at_9901'", replace



use allguer_allauc, clear

*attacks after 2002
keep if (year == 2003 | year == 2004 | year == 2005)
collapse (sum) allauc allguer, by(cod_dane) 

rename allauc  par_0305
rename allguer  guer_0305


sort cod_dane 

tempfile attacks_disp
save "`attacks_disp'"


merge cod_dane using "`at_9901'"

sort cod_dane 
drop _merge

save "`attacks_disp'", replace



***************** displaced data(two years windowas)

use dispar, clear

*displaced after 2002 
egen par_0305d= rsum (par2003 par2004 par2005)
*displaced before  2002 
egen par_9901d= rsum (par1999 par2000 par2001)

drop  par1997-par2007 

sort cod_dane

tempfile dispar_m
save "`dispar_m'", replace


use dispguer, clear

egen guer_0305d=rsum(guer2003 guer2004 guer2005)
egen guer_9901d=rsum(guer1999 guer2000 guer2001)

drop  guer1997-guer2007
sort cod_dane


merge cod_dane using "`dispar_m'"


drop _merge
sort cod_dane

tempfile disp
save "`disp'", replace


use "`attacks_disp'", clear
merge cod_dane using "`disp'"

sort cod_dane 
drop _merge

merge cod_dane using controls
*controls database is the database that identifies the crossection.  

drop _merge

save "`attacks_disp'", replace



******************************************************************************************************************************************************\


*THEN DEFINE VARIABLES


foreach  x in 9901d 0305d  9901 0305     {

*interpreting missings as no-events

replace par_`x'=0 if par_`x'==.
replace guer_`x'=0 if guer_`x'==.


replace par_`x'=1000*par_`x'/pobav if par_`x'>0
replace guer_`x'=1000*guer_`x'/pobav if  guer_`x'>0
* This is done like that since for some municipalities we do not have the population data
* However if for these attacks or displaced were zero, attacks or displaced for any size of the 
* population is zero. 


}

foreach  x in  9901   0305     {


pca guer_`x' guer_`x'd 

predict g_presence_`x'


pca par_`x' par_`x'd

predict p_presence_`x'

}


sort cod_dane


*vote share of uribe in 2002 
merge cod_dane  using "E:\the monopoly of violence 2012\data\national level voting data\uribe2002.dta"
rename share uribe
drop _merge year

sort cod_dane

*vote share of pastrana in 1998
merge cod_dane  using "E:\the monopoly of violence 2012\data\national level voting data\pastrana1998.dta"
rename share pastrana
drop _merge year

save "`attacks_disp'", replace




* redefine variables to report less decimals
replace uribe=uribe/100
replace pastrana=pastrana/100
replace ags=ags/100
replace up86=up86/100


gen luribe=log(100*uribe)
gen lpastrana=log(100*pastrana)

gen lpar_0305=log(par_0305)
gen lpar_0305d=log(par_0305d)
gen lguer_0305=log(guer_0305)
gen lguer_0305d=log(guer_0305d)
gen lpar_9901=log(par_9901)
gen lpar_9901d=log(par_9901d)
gen lguer_9901=log(guer_9901)
gen lguer_9901d=log(guer_9901d)

*Principal component of the log

foreach  x in  9901   0305  {


pca lguer_`x' lguer_`x'd 

predict lg_`x'


pca lpar_`x' lpar_`x'd

predict lp_`x'

}



*controls in logs

gen laltura = log(altura)
gen ldiscap = log(discap)
gen lprecipitacion = log(precipitacion)

gen lpobav=log(pobav)
gen lrur = log(rur)
gen lginix = log(ginix)
gen lnbi=log(nbi)
gen leficiencia=log(eficiencia)
gen lags=log(ags)
gen lup86 = log(up86)




***************************************************************
*****Regressions



*demeaning variables
sum uribe if par_9901>0
gen uribe_hat=uribe-r(mean)

sum pastrana if par_9901>0
gen pastrana_hat=pastrana-r(mean)

gen pastrana_hat2=pastrana_hat*pastrana_hat
gen pastrana_hat3=pastrana_hat*pastrana_hat*pastrana_hat
gen pastrana_hat4=pastrana_hat*pastrana_hat*pastrana_hat*pastrana_hat


gen uribepastrana_hat=uribe_hat*pastrana_hat 


*some labels
label variable uribe_hat "Uribe Vote Share"
label variable pastrana_hat  "Patrana Vote Share"
label variable pastrana_hat2  "Patrana Vote Share 2"
label variable pastrana_hat3  "Patrana Vote Share 3"
label variable pastrana_hat4  "Patrana Vote Share 4"
label variable uribepastrana_hat "Uribe Vote Share X Pastrana Vote Share"

label variable par_9901 "Paramilitary Presence Before 2002"
label variable guer_9901 "Guerrilla Presence Before 2002"
label variable lpar_9901 " Paramilitary Presence Before 2002"
label variable lguer_9901 "Guerrilla Presence Before 2002"

label variable par_9901d " Paramilitary Presence Before 2002"
label variable guer_9901d "Guerrilla Presence Before 2002"
label variable lpar_9901d " Paramilitary Presence Before 2002"
label variable lguer_9901d "Guerrilla Presence Before 2002"

label variable p_presence_9901 " Paramilitary Presence Before 2002"
label variable g_presence_9901 "Guerrilla Presence Before 2002"
label variable lp_9901 " Paramilitary Presence Before 2002"
label variable lg_9901 "Guerrilla Presence Before 2002"


local controls "altura discap precipitacion pobav rur ginix nbi dcoca damapola ags up86 "
local lcontrols "laltura ldiscap lprecipitacion lpobav lrur lginix lnbi dcoca damapola lags lup86 "

* relationship between Uribe and Pastrana


gen pastrana2=pastrana*pastrana
gen pastrana3=pastrana*pastrana*pastrana
gen pastrana4=pastrana*pastrana*pastrana*pastrana

reg uribe pastrana pastrana2  pastrana3  pastrana4, r






*1 Attacks . No controls. No guerrilla. 

xi: reg par_0305 uribe_hat pastrana_hat uribepastrana_hat par_9901 if par_9901 >0, cluster (cod_dane) 
outreg2  uribe_hat pastrana_hat uribepastrana_hat par_9901   ///
using "E:\the monopoly of violence 2012\outreg\Table_6_persistence_main", replace  se noaster  bdec(2) excel label   ///
title (Table 9: Persistence of Paramilitaries and Vote Share for Alvaro Uribe) 

*2 Attacks. Controls. No guerrilla. 

reg par_0305 uribe_hat pastrana_hat uribepastrana_hat par_9901 `controls'   if par_9901 >0, cluster (cod_dane)

outreg2  uribe_hat pastrana_hat uribepastrana_hat par_9901   ///
using "E:\the monopoly of violence 2012\outreg\Table_6_persistence_main",   se noaster  bdec(2) excel label  append


*3 Attacks. Controls & Guerrilla. 

reg par_0305 uribe_hat pastrana_hat uribepastrana_hat  par_9901 guer_9901  `controls'  if par_9901 > 0, cluster(cod_dane)

outreg2  uribe_hat pastrana_hat uribepastrana_hat par_9901  guer_9901 ///
using "E:\the monopoly of violence 2012\outreg\Table_6_persistence_main",  se noaster  bdec(2) excel label  append


*4 Attacks . No controls. No guerrilla. Quartic

xi: reg par_0305 uribe_hat uribepastrana_hat  pastrana_hat pastrana_hat2 pastrana_hat3 pastrana_hat4 par_9901 if par_9901 >0, cluster (cod_dane) 
outreg2  uribe_hat uribepastrana_hat pastrana_hat pastrana_hat2 pastrana_hat3 pastrana_hat4 par_9901   ///
using "E:\the monopoly of violence 2012\outreg\Table_6_persistence_main",  se noaster  bdec(2)  label  excel  append

*5 Attacks. Controls. No guerrilla. Quartic

reg par_0305 uribe_hat uribepastrana_hat pastrana_hat pastrana_hat2 pastrana_hat3 pastrana_hat4  par_9901 `controls'   if par_9901 >0, cluster (cod_dane)

outreg2  uribe_hat uribepastrana_hat pastrana_hat pastrana_hat2 pastrana_hat3 pastrana_hat4 par_9901   ///
using "E:\the monopoly of violence 2012\outreg\Table_6_persistence_main",  se noaster  bdec(2)  label  excel  append


*6 Attacks. Controls & Guerrilla. Quartic. 

reg par_0305 uribe_hat uribepastrana_hat pastrana_hat pastrana_hat2 pastrana_hat3 pastrana_hat4   par_9901 guer_9901  `controls'  if par_9901 > 0, cluster(cod_dane)

outreg2  uribe_hat uribepastrana_hat pastrana_hat pastrana_hat2 pastrana_hat3 pastrana_hat4  par_9901  guer_9901 ///
using "E:\the monopoly of violence 2012\outreg\Table_6_persistence_main" ,  se noaster  bdec(2)  label  excel  append








