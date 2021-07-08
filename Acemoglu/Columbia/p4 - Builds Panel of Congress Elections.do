


**This program builds the panel of legislative elections for the congress.

clear 
set mem 500m
set matsize 1500
set logtype text 
set more off


cd "E:\the monopoly of violence 2012\data\national level voting data"


*database with candidates, lists and parties for 1991
use parties_1991c, clear 
gen cand_numeric=_n
sort cand
tempfile parties
save `parties' 



*elections of 1991 (congress) 
*we have the database by state! In what follows we organize the data in a panel whith pairs munipality-lists as identifiers 

use amazonas91, clear

rename cod_dane mun 
*temporary rename this variable (Just to run the program without using cod_dane as a variable in c* (see below))

keep mun municipio departamento c1

rename c1 votes 

replace votes=0 if votes==.

gen cand = "c1"

tempfile c
save `c' 



use amazonas91, clear

rename cod_dane mun 

foreach i of varlist c2-c9 {

use amazonas91, clear

rename cod_dane mun

keep mun municipio departamento `i' 

rename `i'  votes 

replace votes=0 if votes==.

gen cand = "`i'"

append using `c' 
save `c' ,replace


}

*now do the same for all states. 

foreach k in antioquia91 arauca91 atlantico91 bogota91 bolivar91 boyaca91 caldas91 caqueta91 casanare91  cauca91 cesar91 choco91 cordoba91 ///
cundinamarca91 guainia91 guajira91 guaviare91 huila91 magdalena91 meta91 narino91 nsantander91 putumayo91 quindio91 risaralda91 sanandres91 ///
santander91 sucre91 tolima91 valle91 vaupes91 vichada91 {

use `k', clear
rename cod_dane  mun

     foreach i of varlist c* {
     use `k', clear
     rename cod_dane mun
     keep mun municipio departamento `i' 
     rename `i'  votes 
     replace votes=0 if votes==.
     gen cand = "`i'"
     append using `c' 
     save `c' ,replace 
     }

}


rename mun cod_dane


keep if cod_dane !=.
*since these are totals 

sort cand 

save `c' ,replace 

merge cand using `parties'



gen no_info=0 

replace no_info=1 if del!=1 & dl !=1 & dc!=1 & third !=1

* for the 1991 congress elections data some candidates (head of lists) do not have the name of the list and we cannot assign a party 
*to these. These candidates are the candidates that did not win and did not obtain less than 10% of the quotient!
*here no_info = 1 if we do not have the candidate party. 

*completing dummies for parties since databases have missings as zero.
replace dl=0 if dl==. & no_info==0

replace dc=0 if dc==. & no_info==0

replace third=0 if third==. & no_info==0

replace left=0 if left==. & no_info==0

drop _merge
gen year=1991

keep if del !=1
*del is equal to one for null votes and totals

drop del 

drop cand

rename cand_numeric cand

save `c', replace



*************************************************************************************************************************************************


*Congress elections of 1994


*database with candidates, lists and parties for 1994

use parties_1994c, clear 
gen cand_numeric=_n
sort cand
tempfile parties
save `parties' , replace

*elections of 1994 (congress) 
*we have a database for each state. In what follows we organize the data in a panel whith pairs munipality-lists as identifiers 

use amazonas94, clear

rename cod_dane mun 
* just to run the program without using cod_dane as a variable in c*

keep mun municipio departamento c1

rename c1 votes 

replace votes=0 if votes==.

gen cand = "c1"


tempfile d
save `d' 




use amazonas94, clear

rename cod_dane mun 

foreach i of varlist c2-c24 {

use amazonas94, clear

rename cod_dane mun

keep mun municipio departamento `i' 

rename `i'  votes 

replace votes=0 if votes==.

gen cand = "`i'"

append using `d' 
save `d' ,replace


}

* do the same for all states

foreach k in antioquia94 arauca94 atlantico94 bogota94 bolivar94 boyaca94 caldas94 caqueta94 casanare94  cauca94 cesar94 choco94 cordoba94 ///
cundinamarca94 guainia94 guajira94 guaviare94 huila94 magdalena94 meta94 narino94 nsantander94 putumayo94 quindio94 risaralda94 sanandres94 ///
santander94 sucre94 tolima94 valle94 vaupes94 vichada94 {

use `k', clear
rename cod_dane  mun

     foreach i of varlist c* {
     use `k', clear
     rename cod_dane mun
     keep mun municipio departamento `i' 
     rename `i'  votes 
     replace votes=0 if votes==.
     gen cand = "`i'"
     append using `d' 
     save `d' ,replace 
     }

}


rename mun cod_dane

keep if cod_dane !=.
*these are  totals

sort cand 

save `d' ,replace 

merge cand using `parties'

keep if dnegras!=1
*dnegras refers to afro colombian candidates that have a different election process. 

keep if del !=1
*refers to totals and null votes. 

drop dnegras del

drop _merge

gen year=1994

drop cand

rename cand_numeric cand

save `d', replace




**********************************************************************************************************************************************

*Legislative elections for 1998 (congress). From Now on the structure of the data changes. We do not have anymore the databases by state but just one big
*database from registraduria.  


*the following database comes from registraduria and it has the code of the party, the name of the party and wether 
*the party belongs to a third party or to the left (the last two variables were coded by me).
use parties_1998, clear

replace left=1 if nombpart=="ALIANZA NACIONAL POPULAR ANAPO"
replace third=0 if nombpart=="ALIANZA NACIONAL POPULAR ANAPO"

replace left=1 if nombpart=="EDUCACION,TRABAJO Y CAMBIO SOCIAL"
replace third=0 if nombpart=="EDUCACION,TRABAJO Y CAMBIO SOCIAL"


tempfile key1
sort codipart
save "`key1'", replace


*this database comes from the registraduria and it assigns a party to each list. 
use  list&parties_1998c, clear
sort codipart

merge codipart using "`key1'"

drop if _merge==2
drop _merge
*some parties that exist in the senate but not in the congress


keep if codidepa !=.
*eliminates from here unmarked ballots, blank votes and unmarked ballots (reintroduced later)

sort codidepa codilist

tempfile parties98
save "`parties98'", replace



*database of votes in the congress in 1998

use congress_98, clear
sort codidepa codilist 
merge codidepa codilist using "`parties98'"

*blank votes
replace nombcama="Votos en Blanco" if codilist==297
*null votes
replace nombcama="Votos nulos" if codilist==298
*unmarked ballots
replace nombcama="No marcadas " if codilist==299
*totals
drop if codilist==300

drop _merge


replace codimuni= codimuni-1000*codidepa

gen cod_dpto=codidepa
gen cod_mpio=codimuni

*liberals
gen dl=0
replace dl=1 if codipart==1

*conservatives
gen dc=0
replace dc=1 if codipart==2

*since if our definition of total votes we will not use null votes and unmarked ballots
drop if codilist==298 | codilist==299

rename votocama votes

sort cod_dpto cod_mpio

tempfile e
save `e', replace


*changing the registraduria municipality codes by DANE municipality codes.

use key 

sort cod_dpto cod_mpio

tempfile codigos
save "`codigos'"

use "`e'", clear

merge cod_dpto cod_mpio using "`codigos'"

keep if _merge == 3

rename depto departamento
rename mun municipio
rename nombcama candidate
rename codilist cand
gen year=1998
keep cod_dane municipio departamento votes cand candidate dl dc third left year


save `e', replace


********************************************************************************************************************************************************
**2002



*Legislative elections for 2002 (congress).

*database with dane codes and registraduria codes
use key, clear
sort cod_dpto cod_mpio
tempfile codigos
save "`codigos'"

*database with 2002 parties classified as third parties or  left parties. 
use parties_2002, clear
sort cod_partido
tempfile parties02
save "`parties02'"

use congress_02, clear
*dropping votes in consulates
drop if cod_dpto==88
*totals, unmarked ballots and null votes
drop if cod_candidato==999 | cod_candidato==998 | cod_candidato ==997

sort cod_partido

merge cod_partido using "`parties02'"

drop if _merge ==2

drop _merge 


*liberals
gen dl=0
replace dl=1 if cod_partido==1

*conservatives
gen dc=0
replace dc=1 if cod_partido==2


*registraduria municipality codes into dane municipality codes

sort cod_dpto cod_mpio

merge cod_dpto cod_mpio using "`codigos'"

keep if _merge==3

drop _merge

rename votos votes 
rename nom_dpto departamento
rename nom_mpio municipio
rename nom_candidato candidate
rename cod_candidato cand

gen year=2002

keep cod_dane departamento municipio votes cand candidate dl dc third left year

tempfile f
save `f'

********************************************************************************************************************************************************
**2006


use key, clear
sort cod_dpto cod_mpio
tempfile codigos
save "`codigos'"

*change these for congress lists


use parties_2006, replace
replace left=0 if movi_pol=="MOVIMIENTO VOLUNTAD POPULAR"
replace left=1 if movi_pol=="PARTIDO SOCAL DEMOCRATA COLOMBIANO"
sort codi_par
tempfile parties06
save "`parties06'"


use senate&congress_06, replace
keep if codi_cor=="02"
*selecting congress only 

destring codi_par, replace

*votes for lists in each municipality / agrregating over lists (to deal with "preferential lists"). 
collapse (sum) vota_can, by(codi_dep codi_mun codi_par) 
rename vota_can votos

sort codi_par

merge codi_par using "`parties06'"

drop _merge

*conservatives
gen dc=0 
replace dc=1 if codi_par==65

*liberals
gen dl=0 
replace dl=1 if codi_par==68

replace movi_pol = "TOTAL" if codi_par ==999
replace movi_pol = "NO MARCADAS" if codi_par ==998
replace movi_pol = "NULOS" if codi_par ==997
replace movi_pol = "BLANCOS" if codi_par ==996


drop if movi_pol == ""
*there are indigenous and black districts


drop if movi_pol == "TOTAL" 
drop if movi_pol == "NO MARCADAS" 
drop if movi_pol == "NULOS"

destring codi_dep, replace
destring codi_mun, replace

gen cod_dpto=codi_dep 
gen cod_mpio=codi_mun

drop codi_dep codi_mun

sort cod_dpto cod_mpio

merge cod_dpto cod_mpio using "`codigos'"

keep if _merge ==3

rename codi_par cand 
rename movi_pol candidate
rename depto departamento 
rename mun municipio
rename votos votes 
gen year=2006

keep cod_dane municipio departamento votes cand candidate dl dc third left year


tempfile g
save `g'

* creating the big (pseudo)panel

append using `f'

append using `e'

append using `d'

append using `c'


replace no_info =0 if year >1991

save elections_panel_congress, replace

use elections_panel_congress, clear


gen tercero=0
replace tercero=1 if third ==1 & left !=1
*since third includes third parties from the left

gen correction=1
replace correction=0 if no_info==1

gen votes_c=votes*correction
*this is done like that use only the information from the candidates that have a reported party.

gen votesthird=votes_c*tercero

collapse (sum) sum_votesc=votes_c sum_votes_third=votesthird sum_votes=votes (max) max_votes=votes  , by(cod_dane year ) 


gen share_third =100*sum_votes_third/sum_votesc

gen maxshare =100* max_votes/sum_votes


sort year cod_dane 
save legislative_congress, replace

use legislative_congress, clear



