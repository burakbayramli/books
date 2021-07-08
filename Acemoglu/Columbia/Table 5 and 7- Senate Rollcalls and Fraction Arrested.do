


clear matrix
clear 

set mem 500m
set matsize 1500
set logtype text 
set more off



cd "G:\Dropbox_old\the monopoly of violence 2012\data\national level voting data"


*database with municipality codes from registraduria and from dane

use key, clear
keep cod_dpto cod_mpio cod_dane
sort cod_dpto cod_mpio
tempfile key 
save "`key'", replace


* Now import database of votes


*database of senate votes in 2002. 
use senate_02, clear
keep if circunscripcion == 0  
*0 for senate

*consulates
drop if cod_dpto == 88
*null votes
drop if cod_candidato ==997
*unmarked ballots
drop if cod_candidato ==998
*totals
drop if cod_candidato ==999

rename nom_mpio municipio
rename nom_dpto departamento
rename cod_candidato cod_cand
rename votos votosen

sort cod_dpto cod_mpio


merge cod_dpto cod_mpio using "`key'"
*since registraduria codes are different from DANE codes 

drop if _merge==2
*and no values of merge equal to 1

drop _merge 

drop cod_dpto cod_mpio  movimiento

sort cod_dane 

tempfile votes
save "`votes'", replace


use "G:\Dropbox_old\the monopoly of violence 2012\data\presence and controls\presence.dta"

**"high right preferences and left preferences municipalities"

_pctile ags, p(75)
return list

gen  dumags=0 
replace dumags=1 if  ags >r(r1) 
replace dumags=. if ags ==. 


_pctile up86, p(75)
return list


gen  dumup86=0 
replace dumup86=1 if  up86 >r(r1) 
replace dumup86=. if up86 ==. 


sort cod_dane

tempfile pres
save `pres'

use `votes', clear
merge cod_dane using `pres'

keep if _merge == 3


tempfile sen02vx
save "`sen02vx'", replace
*database that  will be useful when running second strategy (but we are not running it anymore

*votes obtained in zones with presence of non-state armed actor
gen votespar=votosen* dumpar_9701
gen votesguer=votosen*dumguer_9701 

gen votespard=votosen* dumpar_9701d
gen votesguerd=votosen* dumguer_9701d

*votes obtained in lef and right oriented areas

gen votesleft = votosen*dumup86
gen votesright = votosen*dumags

*now calculate total of votes and total of votes by area.
collapse (sum) votosen votesleft votesright votespar votesguer votespard votesguerd  , by(cod_cand) 
*since votes reported by head of the list

gen shareleft =votesleft/votosen
gen shareright =votesright/votosen

gen sharepara=votespar/votosen
gen shareguer=votesguer/votosen

gen shareparad=votespard/votosen
gen shareguerd=votesguerd/votosen


rename votosen listotal
*total votes obtained by each list 

keep cod_cand   shareleft   shareright  sharepara shareguer  shareparad shareguerd listotal

sort cod_cand
save "`votes'", replace


*database with roll-call votes 

use "G:\Dropbox_old\the monopoly of violence 2012\data\senate and congress level voting data\rollcall_senate.dta", clear



*Only three lists with more than one elected candidatc:
*i. Luis Alfredo Ramos, Uribista, was first in a list with Zapata Correa and Cogollos Amaya
*ii. Vargas Lleras, Uribistas, is with Sosa Pacheco
*iii. Antonio Navarro Wolf, leftist. is with Gerardo Jumi Tapias

*In 2002 there are not preferential lists. ie: different candidates in the same lists do not compete for votes. 

sort cod_cand

replace preso =0 if preso==.
* just because a missing in the arrests variables means that senator has never been arrested. 

keep cod_cand cod_partido reelection apel6164 preso  investigado absuelto condenado  nombres apellidos
*vote for reelection, for articles of the justice and peace law and arrested congressmen


***next lines generate fraction of YES votes in a list and fraction of arrested congressmen in a list. 

sort cod_cand
tempfile step1
save `step1'

gen dum= 1 

collapse (sum) dum, by(cod_cand)

sort cod_cand
tempfile step2
save `step2'

use `step1', clear 
merge cod_cand using `step2'
drop _merge
* just to identify lists with more than one candidate in the senate.

save `step1', replace

keep if dum > 1

gen tot_preso=1 if preso !=.
gen tot_reelection=1 if reelection !=.
gen tot_apel6164=1 if apel6164 !=.
*to identify people that actually voted. Remember that missings represent senator that did not vote. 

collapse (sum) reelection apel6164 preso tot_reelection tot_apel6164 tot_preso (mean) cod_partido, by(cod_cand)

replace  reelection =reelection /tot_reelection
replace  apel6164 =apel6164/ tot_apel6164
replace  preso = preso / tot_preso
*dividing by zero and obtaining a missing is ok and what we want here.

drop tot_reelection tot_apel6164 tot_preso 

tempfile step3
save `step3'

use `step1', clear
drop if dum > 1 
drop dum

append using `step3'
*and now we have a database with lists and fraction of "yes" votes in the list or arrested congressmen in the list. 

sort cod_cand

merge  cod_cand using "`votes'"

drop if _merge == 2
*candidates that won
drop _merge 

sort cod_partido

tempfile rollcall
save "`rollcall'" , replace






*database with parties and their classification as third parties. 
use parties_2002, clear 

sort cod_partido
tempfile parties
save "`parties'" , replace

use "`rollcall'", clear 

merge cod_partido using "`parties'" 


keep if _merge ==3
drop _merge
*some values of merge equal to 2 (paties that did not make it into the senate). 

gen dl=0
replace dl=1 if cod_partido==1

gen dc=0
replace dc=1 if cod_partido==2

gen dleft=0
replace dleft=1 if left==1

gen dthird=0
replace dthird=1 if third==1 & left!=1
*since a third party may be a leftist party. 

save "`rollcall'", replace

replace preso =1 if apellidos == "CLAVIJO VARGAS" 
*updating




reg preso dc dleft dthird , r 
outreg2   dc dleft dthird  ///
using "E:\the monopoly of violence 2012\outreg\Table_5_senate_regressions_congressmen_arrested", replace    se   bdec (2) noaster excel   ///
title (Table 8: ...and Congressmen Elected from High Paramilitary Presence Areas ) 


reg preso sharepara shareguer,r
outreg2  sharepara  shareguer  ///
using "E:\the monopoly of violence 2012\outreg\Table_5_senate_regressions_congressmen_arrested",       se   bdec (2) noaster excel   append


reg preso sharepara shareguer shareright shareleft,r
outreg2  sharepara  shareguer shareright shareleft  ///
using "E:\the monopoly of violence 2012\outreg\Table_5_senate_regressions_congressmen_arrested",     se   bdec (2) noaster excel   append

reg preso dc dleft dthird sharepara shareguer shareright shareleft ,r
outreg2  dc dleft dthird sharepara  shareguer shareright shareleft ///
using "E:\the monopoly of violence 2012\outreg\Table_5_senate_regressions_congressmen_arrested",   se   bdec (2) noaster excel   append


******************

reg apel6164 dc dleft dthird  , r

outreg2   dc dleft dthird   ///
using "E:\the monopoly of violence 2012\outreg\Table_5_senate_regressions_congressmen_arrested",    se   bdec (2) noaster excel   append


reg apel6164 sharepara shareguer , r
outreg2  sharepara  shareguer  ///
using "E:\the monopoly of violence 2012\outreg\Table_5_senate_regressions_congressmen_arrested",     se   bdec (2) noaster excel   append



reg apel6164 sharepara shareguer shareright shareleft,r
outreg2  sharepara  shareguer shareright shareleft  ///
using "E:\the monopoly of violence 2012\outreg\Table_5_senate_regressions_congressmen_arrested",   se   bdec (2) noaster excel   append


reg apel6164  dc dleft dthird sharepara shareguer shareright shareleft ,r
outreg2  dc dleft dthird sharepara  shareguer shareright shareleft ///
using "E:\the monopoly of violence 2012\outreg\Table_5_senate_regressions_congressmen_arrested",    se   bdec (2) noaster excel   append




******************

reg reelection dc dleft dthird  , r

outreg2   dc dleft dthird   ///
using "E:\the monopoly of violence 2012\outreg\Table_7_Reelection_and_senators_elected", replace    se   bdec (2) noaster excel   ///
title (Table 11: Vote for Reelection and Congressmen Elected from High Paramilitary Presence Areas ) 


reg reelection sharepara shareguer , r
outreg2  sharepara  shareguer  ///
using "E:\the monopoly of violence 2012\outreg\Table_7_Reelection_and_senators_elected",      se   bdec (2) noaster excel   append



reg reelection sharepara shareguer shareright shareleft,r
outreg2  sharepara  shareguer shareright shareleft  ///
using "E:\the monopoly of violence 2012\outreg\Table_7_Reelection_and_senators_elected",    se   bdec (2) noaster excel   append


reg reelection  dc dleft dthird sharepara shareguer shareright shareleft ,r
outreg2  dc dleft dthird sharepara  shareguer shareright shareleft ///
using "E:\the monopoly of violence 2012\outreg\Table_7_Reelection_and_senators_elected",    se   bdec (2) noaster excel   append
















