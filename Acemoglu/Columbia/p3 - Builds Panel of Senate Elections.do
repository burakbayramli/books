


**This program builds the panel for senate elections. At the end will have a database with candidate-municipality-year identifier

clear 
set mem 500m
set matsize 1500
set logtype text 
set more off


cd "E:\the monopoly of violence 2012\data\national level voting data"



*senate elections of 1991.

use senate_91, clear

rename var157 cod_dane
rename var1 municipio

*cod_dane=. are some totals in this dataset.
drop if cod_dane == .


*Organizing data into a candidate-municipality dataset. 

tempfile base
save `base', replace 

keep c1 cod_dane
rename c1 votes
gen id_can="c1"

tempfile d
save `d' 



foreach num of numlist 2/146{ 
*variables go up to c154 but after c146 have aggregate variables

use `base' , clear 

keep c`num' cod_dane
gen id_can="c`num'"
rename c`num'  votes

append using `d' 
save `d' , replace

}

rename id_can cod_can
sort cod_can





*Now merge with the parties database

tempfile ele91
save `ele91' , replace

use "E:\the monopoly of violence 2012\data\national level voting data\parties_1991s", clear
gen id=_n
sort cod_can

tempfile parties_1991
save `parties_1991' 

use `ele91' , clear
merge cod_can using `parties_1991' 

drop cod_can
rename id cod_can
*since I want a numerical format for cod_can

* merge is equal to 3 always
drop _merge 

rename candidato partidos 
*for 1991-1994 variable candidato has candidate name and , in parentheses the party

gen year=1991
save `ele91' , replace


*******************************************************************************************************



*senate elections of 1994.


use senate_94, clear


rename var257 cod_dane
rename var1 municipio

keep if cod_dane != .
*again some totals

tempfile base
save `base', replace 


keep c1 cod_dane
rename c1 votes
gen id_can="c1"

tempfile d
save `d' 


foreach num of numlist 2/254 { 

use `base' , clear 

keep c`num' cod_dane
gen id_can="c`num'"
rename c`num'  votes

append using `d' 
save `d' , replace

}

rename id_can cod_can
sort cod_can

tempfile ele94
save `ele94' , replace

use "E:\the monopoly of violence 2012\data\national level voting data\parties_1994s", clear
gen id = _n
sort cod_can

replace dl=0 if candidato=="Samuel Rodriguez Olaya"
replace dc=0 if candidato=="Samuel Rodriguez Olaya"
replace third=1 if candidato=="Samuel Rodriguez Olaya"
replace dc=0 if candidato=="Armando Holguin"
replace dc=0 if candidato=="Antonio Jose Restrepo"
replace dc=1 if candidato=="Guillermo Alfonso Niño"


tempfile parties_1994
save `parties_1994' 

use `ele94' , clear
merge cod_can using `parties_1994'

drop cod_can

gen cod_can= id 
*a numerical format for the candidate code

drop id

*merge = 3 always
drop _merge


rename candidato partidos 
* for 1991-1994 variable candidato has candidate name and , in parentheses the party

 
gen year=1994
save `ele94' , replace


************************************************************************************************************************************************************
* Now we take data after 1998. These raw  databases have a different structure (easier to work with) that those for 1991 and 1994

*elections 1998 

cd "E:\the monopoly of violence 2012\data\national level voting data"

*database with parties in 1998 and party codes
use parties_1998, clear
sort codipart

replace left=1 if nombpart=="ALIANZA NACIONAL POPULAR ANAPO"
replace third=0 if nombpart=="ALIANZA NACIONAL POPULAR ANAPO"

replace left=1 if nombpart=="EDUCACION,TRABAJO Y CAMBIO SOCIAL"
replace third=0 if nombpart=="EDUCACION,TRABAJO Y CAMBIO SOCIAL"



tempfile temp1
save "`temp1'", replace


* database with party code corresponding to each list code. 
use  list&parties_1998s, clear
sort codipart


merge codipart using "`temp1'"


* this merge has values equal to 1. These are null votes, blank votes unmarked ballots and total. We fill in for this below.
* it also has values with merge equal to 2. These are parties in the congress but not in the senate.
* we can drop these.

keep if _merge==3
drop _merge

sort codilist

tempfile parties98
save "`parties98'", replace



use senate98, clear


sort codilist 

merge codilist using "`parties98'"

drop _merge

*_merge had values equal to 1 (blank votes, null votes, unmarked ballots, total) and 3. 


replace nombpart="VOTOS EN BLANCO" if codilist==897
replace nombpart="VOTOS NULOS" if codilist==898
replace nombpart="TARJETAS NO MARCADAS" if codilist==899
replace nombpart="TOTAL ALL BUT BLANK" if codilist==900


*two candidates that are not in the 
drop if codilist ==427 | codilist ==548

*two codes (candidates) that are in the database with the votes but that are not in the database with 
*the names of the candidates. We drop these two.


rename codilist codi_can

rename votosena votes

rename nombpart partidos

*create party dummies.

gen dl=0
replace dl=1 if codipart==1

gen dc=0
replace dc=1 if codipart==2

keep codi_can votes partidos dl dc third left codidep codimun


*raw databases have a registraduria municipality code that is different from the dane municipality code. Now we turn the former into the later.

replace codimuni= codimuni-1000*codidepa

gen cod_dpto=codidepa
gen cod_mpio=codimuni

sort cod_dpto cod_mpio


drop codidepa codimuni

tempfile ele98

save `ele98'

*database with municipality codes from registraduria and from Dane. 
use key 
sort cod_dpto cod_mpio
tempfile codigos
save "`codigos'"

use `ele98', clear

merge cod_dpto cod_mpio using "`codigos'"


* merge takes de value of 1 if consulates. We do not need these. 
keep if _merge==3

drop _merge cod_dpto cod_mpio depto mun

rename codi_can cod_can

gen year =1998 


save "`ele98'", replace




****Elections 2002*****************************************************************************************************************************************************



use key, clear
sort cod_dpto cod_mpio
tempfile codigos
save "`codigos'"

use parties_2002, clear
sort cod_partido
tempfile parties02
save "`parties02'"

use senate_02, clear
keep if circunscripcion==0
*in this dataset circunscripcion=0 if senate

*dropping consulates
drop if cod_dpto==88
sort cod_partido


merge cod_partido using "`parties02'"

drop if  _merge==2
*dropping parties that are in the congress but not in the senate.

*merge has values equal to 1 that we fill in now. 

rename nombre partidos 
replace partidos = "TOTAL" if cod_candidato ==999
replace partidos = "NO MARCADAS" if cod_candidato ==998
replace partidos = "NULOS" if cod_candidato ==997
replace partidos = "BLANCOS" if cod_candidato ==996

*[Cannot use registraduria total (the 999)  since these include indigenous districts]
* in part for this reason (that totals are not comparable over time in the registraduria datasets
* we create our own totals definitons. 

*Parties
gen dl=0
replace dl=1 if cod_partido==1

gen dc=0
replace dc=1 if cod_partido==2


rename votos votes
rename cod_candidato codi_can

keep codi_can votes partidos dl dc third left cod_dpto cod_mpio

sort cod_dpto cod_mpio

tempfile ele02
save "`ele02'",  replace

merge cod_dpto cod_mpio using "`codigos'"

keep if _merge==3
* some values of _merge = 2 but we drop that. 

drop depto mun cod_dpto cod_mpio

drop _merge

rename codi_can cod_can

gen year=2002

save "`ele02'",  replace



******************elections 2006*************************************************************************************
*Starting in this year a party cannot issue more than one list

use key, clear
sort cod_dpto cod_mpio
tempfile codigos
save "`codigos'"

use parties_2006, clear
sort codi_par
replace left=0 if movi_pol=="MOVIMIENTO VOLUNTAD POPULAR"
replace left=1 if movi_pol=="PARTIDO SOCAL DEMOCRATA COLOMBIANO"

tempfile parties06



save "`parties06'"

use senate&congress_06, replace
keep if codi_cor=="01"
*codi_cor = 01 for senate
drop if codi_dep=="88"
*dropping consulates

destring codi_par, replace

*agrregating over lists (to deal with "preferential lists"). 

collapse (sum) vota_can, by(codi_dep codi_mun codi_par) 
sort codi_par

merge codi_par using "`parties06'"

gen dc=0 
replace dc=1 if codi_par==65

gen dl=0 
replace dl=1 if codi_par==68

replace movi_pol = "TOTAL" if codi_par ==999
replace movi_pol = "NO MARCADAS" if codi_par ==998
replace movi_pol = "NULOS" if codi_par ==997
replace movi_pol = "BLANCOS" if codi_par ==996


drop if movi_pol ==""
*these are indigenous districts and null votes for indigenous districts

destring codi_dep, replace
destring codi_mun, replace


gen cod_dpto=codi_dep 
gen cod_mpio=codi_mun

drop codi_dep codi_mun


drop _merge


sort cod_dpto cod_mpio

merge cod_dpto cod_mpio using "`codigos'"

keep if _merge==3
*have merge==2 but no problem. do not need that. 

drop _merge

rename movi_pol partidos


gen year=2006

gen cod_can= codi_par
drop codi_par



rename vota_can votes

drop cod_dpto cod_mpio depto mun

tempfile ele06
save "`ele06'", replace

append using "`ele02'"

append using "`ele98'"

append using "`ele94'"

append using "`ele91'"


* dropping unmarked ballots, null votes and totals. could have done that since the beginning but may be useful in anothe ocasion.

drop if year ==1991 & (cod_can ==145 | cod_can ==146) 
drop if year ==1994 & (cod_can == 253 | cod_can==254)
drop if year ==1998 & (cod_can >=898)
drop if year ==2002 & (cod_can >=997)
drop if year ==2006 & (cod_can >=997)


save elections_panel, replace

use elections_panel, clear



gen tercero=0
replace tercero=1 if third ==1 & left !=1
*since third includes third parties from the left


gen votesthird=votes*tercero

collapse (sum) sum_votes=votes sum_votes_third=votesthird (max) max_votes=votes  , by(cod_dane year ) 


gen share_third =100* sum_votes_third/sum_votes

gen maxshare =100* max_votes/sum_votes


sort year cod_dane 
save legislative, replace





