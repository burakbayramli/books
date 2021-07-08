**This program builds the panel of presidential elections.




clear 
set mem 500m
set matsize 1500
set logtype text 
set more off

cd "E:\the monopoly of violence 2012\data\national level voting data"


*database with municipalities codes

use key
sort cod_dpto cod_mpio
tempfile codigos
save "`codigos'"


*database with presidential elections votes in 1998.

use presidentes1998, clear

gen cod_mpio=codimuni-1000*codidepa
gen cod_dpto= codidepa
gen votes=  votoprim

*first round votes for Pastrana in 1998


keep if cod_dpto != 88
*dropping consulates

drop if codicand ==97 | codicand == 98  | codicand ==99
*null and unmarked ballots that we drop to define total number of votes as total votes in te municipality (including blank votes but excludin null and
*unmarked ballots). 99 are totals. 

*total number of votes by municipality in 1998
collapse (sum) sum_votes=votes , by(cod_dpto cod_mpio)


sort cod_dpto cod_mpio

tempfile total
save `total'


*votes obtained by pastrana in the first round

use presidentes1998, clear

gen cod_mpio=codimuni-1000*codidepa
gen cod_dpto= codidepa
gen votes=  votoprim

keep if cod_dpto != 88
*dropping consulates

keep if codicand==13 
*(pastrana)

keep cod_dpto cod_mpio votes

sort cod_dpto cod_mpio

merge cod_dpto cod_mpio using `total'

drop _merge 

gen share= 100* votes/sum_votes

drop votes sum_votes


sort cod_dpto cod_mpio

save pastrana1998, replace

*to use DANE codes
merge cod_dpto cod_mpio using `codigos'

keep if _merge ==3 
drop _merge
gen year=1998


sort cod_dane 

keep share cod_dane year
save pastrana1998, replace


***************************************************************************************************************************************************************

*database with presidential elections votes in 2002.

use presidentes2002, clear

gen votes= votos


keep if cod_candidato < 997
*totals and null and unmarked ballots that we drop to define total number of votes. 

*total number of votes by municipality in 2002
collapse (sum) sum_votes=votes , by(cod_dpto cod_mpio)


sort cod_dpto cod_mpio

tempfile total
save `total', replace


*uribe votes in 2002

use presidentes2002, clear

gen votes=  votos


keep if cod_candidato==4
*(uribe)

keep cod_dpto cod_mpio votes

sort cod_dpto cod_mpio

merge cod_dpto cod_mpio using `total'

drop _merge 

gen share= 100* votes/sum_votes

drop votes sum_votes

sort cod_dpto cod_mpio

save uribe2002, replace

*to use DANE codes 

merge cod_dpto cod_mpio using `codigos'

keep if _merge ==3
drop _merge

gen year=2002

sort cod_dane 

keep share year cod_dane 

save uribe2002, replace


***************************************************************************************************************************************************************




*database with presidential elections votes in 2006.

use e2006, replace
keep if codi_cor=="03"
*codi_cor = 3 for presidential elections
drop if codi_dep=="88"
*dropping consulates
rename vota_can votes
destring codi_dep, replace
destring codi_mun, replace
destring codi_can, replace
gen cod_dpto=codi_dep 
gen cod_mpio=codi_mun
save presidentes2006, replace


keep if codi_can < 997
*totals, null and unmarked ballots that we drop to define total number of votes. 

*total number of votes by municipality in 2006
collapse (sum) sum_votes=votes , by(cod_dpto cod_mpio)

sort cod_dpto cod_mpio

tempfile total
save `total', replace


*uribe votes in 2006

use presidentes2006, clear


keep if codi_can==3
*(uribe)

keep cod_dpto cod_mpio votes

sort cod_dpto cod_mpio

merge cod_dpto cod_mpio using `total'

drop _merge 

gen share= 100* votes/sum_votes

drop votes sum_votes


sort cod_dpto cod_mpio

save uribe2006, replace

*to use DANE codes
merge cod_dpto cod_mpio using `codigos'

keep if _merge ==3
drop _merge
gen year=2006

sort cod_dane 


keep share cod_dane year

save uribe2006, replace


append using uribe2002

append using pastrana1998

sort cod_dane

save presidential_elections_panel, replace

