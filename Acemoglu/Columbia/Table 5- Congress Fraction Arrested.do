

clear 

set mem 500m
set matsize 1500
set logtype text 
set more off


cd "E:\the monopoly of violence 2012\data\national level voting data"


*import database with parties and their classification as third parties. 
use parties_2002, clear 

sort cod_partido
tempfile parties
save "`parties'" , replace


*import database with municipality codes from registraduria and from dane


use key, clear
keep cod_dpto cod_mpio cod_dane
sort cod_dpto cod_mpio
tempfile key 
save "`key'", replace


* Now import database of votes


*database of congress votes in 2002. 
use congress_02, clear

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
rename votos votoscam


gen cod_cand2 = cod_dpto*1000+cod_cand
*since in some cases candidates from different states have the same code

sort cod_dpto cod_mpio
merge cod_dpto cod_mpio using "`key'"



drop if _merge==2
*the rest of the merge values are equal to 3
drop _merge 
drop cod_dpto cod_mpio  

sort cod_dane


tempfile votes
save "`votes'", replace


use "E:\the monopoly of violence 2012\data\presence and controls\presence.dta"


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


*votes obtained in zones with presence of non-state armed actor
gen votespar=votoscam* dumpar_9701
gen votesguer=votoscam*dumguer_9701 

gen votespard=votoscam* dumpar_9701d
gen votesguerd=votoscam* dumguer_9701d

*votes obtained in lef and right oriented areas
gen votesleft = votoscam*dumup86
gen votesright = votoscam*dumags



collapse (sum) votoscam votespar votesguer votespard votesguerd votesleft votesright, by(departamento cod_cand2) 


gen shareleft =votesleft/votoscam
gen shareright =votesright/votoscam

gen sharepara=votespar/votoscam
gen shareguer=votesguer/votoscam

gen shareparad=votespard/votoscam
gen shareguerd=votesguerd/votoscam

sort cod_cand2
save "`votes'", replace


*database with roll-call votes 

use "E:\the monopoly of violence 2012\data\senate and congress level voting data\rollcall_congress", clear


*dropping candidate from black and indigenous communities. 
drop if especial==1

sort cod_cand2

merge cod_cand2 using `votes'
keep if _merge ==3
drop _merge

*we do not have list with more than one candidate. we do not have to calculate fracttion of "yes" votes in a list

sort cod_partido

merge cod_partido using "`parties'" 
drop if _merge ==2
drop _merge

gen dl=0
replace dl=1 if cod_partido==1

gen dc=0
replace dc=1 if cod_partido==2

gen dleft=0
replace dleft=1 if left==1

gen dthird=0
replace dthird=1 if third==1 & left!=1
*since a third party may be a leftist party. 

replace preso = 0 if preso ==.


reg preso dc dleft dthird , r 
outreg2   dc dleft dthird  ///
using "E:\the monopoly of violence 2012\outreg\Table_5_congress_regressions_congressmen_arrested", replace    se   bdec (2) noaster excel   ///
title (Table: ...and Congressmen Elected from High Paramilitary Presence Areas ) 


reg preso sharepara shareguer,r
outreg2  sharepara  shareguer  ///
using "E:\the monopoly of violence 2012\outreg\Table_5_congress_regressions_congressmen_arrested",       se   bdec (2) noaster excel   append


reg preso sharepara shareguer shareright shareleft,r
outreg2  sharepara  shareguer shareright shareleft  ///
using "E:\the monopoly of violence 2012\outreg\Table_5_congress_regressions_congressmen_arrested",     se   bdec (2) noaster excel   append

reg preso dc dleft dthird sharepara shareguer shareright shareleft ,r
outreg2  dc dleft dthird sharepara  shareguer shareright shareleft ///
using "E:\the monopoly of violence 2012\outreg\Table_5_congress_regressions_congressmen_arrested",   se   bdec (2) noaster excel   append




