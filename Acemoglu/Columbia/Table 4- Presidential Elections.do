clear

set mem 500m
set matsize 1500
set logtype text 
set more off


cd "E:\the monopoly of violence 2012\data\national level voting data"

use presidential_elections_panel

keep share year cod_dane

sort cod_dane 


gen t2002=0
gen t2006=0
replace t2002=1 if year==2002
replace t2006=1 if year==2006

sort cod_dane 

* presence databases

merge cod_dane using "E:\the monopoly of violence 2012\data\presence and controls\presence.dta"
keep if _merge==3
drop _merge
sort  year cod_dane

merge year  cod_dane using "E:\the monopoly of violence 2012\data\presence and controls\presence_timev.dta"
keep if _merge!=2
drop _merge
sort  cod_dane

********************************************************************************************************************************************************

su share if year <=2001
su share  if dumpar_9701==1 &  year <= 2001
su share  if dumpar_9701==0 &  year <= 2001

su share if year > 2001
su share  if dumpar_9701==1 &  year > 2001
su share if dumpar_9701==0 &  year > 2001


local p1="par_9701"
local g1="guer_9701"

local p2="dumpar_9701"
local g2="dumguer_9701"

local p3="dumparv"
local g3="dumguerv"


local output = "Table_4_Paramilitary_Presence_and_Winning_Presidential_candidate_share_of_votes"


******************************************************************************************************************************************************

cd "E:\the monopoly of violence 2012\outreg"

gen dpara=`p1'
label variable dpara "Paramilitary Presence"
gen t2002dpara=t2002*`p1'
label variable  t2002dpara "Paramilitary Presence X 2002"
gen t2006dpara=t2006*`p1'
label variable  t2006dpara "Paramilitary Presence X 2006"


gen dguer=`g1'
label variable dguer "Guerrilla Presence"
gen t2002dguer=t2002*`g1'
label variable  t2002dguer "Guerrilla Presence X 2002"
gen t2006dguer=t2006*`g1'
label variable  t2006dguer "Guerrilla Presence X 2006"




*time interacted variables
foreach  x in altura discap precipitacion pobav rur ginix nbi eficiencia dcoca damapola  ags up86  {

gen t2002`x'=t2002*`x'
gen t2006`x'=t2006*`x'

}


* our set of controls

local controlsa = "t2002altura t2006altura  t2002discap t2006discap" 
local controlsb = "t2002precipitacion t2006precipitacion" 
local geo = "`controlsa' `controlsb'"


local controlsc = "t2002pobav t2006pobav t2002rur t2006rur t2002ginix t2006ginix " 
local controlsd = "t2002nbi t2006nbi" 
local demo = "`controlsc' `controlsd'"


local drugs = "t2002dcoca t2006dcoca t2002damapola t2006damapola" 

local politics = "t2002ags t2006ags t2002up86 t2006up86" 

xtset cod_dane year


*************************************************
****** TIME INVARIANT PARAMILITARY ATTACKS. CONTROLS.

xtreg  share  t2002 t2006 t2002dpara t2006dpara,  fe  cluster(cod_dane)  
outreg2  t2002dpara t2006dpara ///
using `output', replace  se  bdec(2)  excel label noaster    ///
title (Table 7: Paramilitary Presence and Presidential Elections: Winning Candidate Share of Votes ) 




*************************************************
****** TIME INVARIANT PARAMILITARY ATTACKS. CONTROLS.

xtreg  share  t2002 t2006 t2002dpara t2006dpara `geo'  `demo' `drugs'  `politics',  fe  cluster(cod_dane)  
outreg2  t2002dpara t2006dpara ///
using `output',   se bdec(2)  excel label noaster  append



*************************************************
****** TIME INVARIANT PARAMILITARY PRESENCE DUMMY. CONTROLS + GUERRILLA PRESENCE

xtreg  share  t2002 t2006  t2002dpara t2006dpara t2002dguer t2006dguer `geo'  `demo' `drugs'  `politics',  fe  cluster(cod_dane)  
outreg2  t2002dpara t2006dpara t2002dguer t2006dguer ///
using `output',  se  bdec(2)  excel label noaster   append 


replace dpara=`p2'
replace t2002dpara=t2002*`p2'
replace t2006dpara=t2006*`p2'


replace dguer=`g2'
replace t2002dguer=t2002*`g2'
replace t2006dguer=t2006*`g2'



*************************************************
****** TIME INVARIANT PARAMILITARY PRESENCE DUMMY.

xtreg  share   t2002 t2006  t2002dpara t2006dpara, fe   cluster(cod_dane) 
outreg2  t2002dpara t2006dpara ///
using `output',  se bdec(2)  excel label noaster   append 

*************************************************
****** TIME INVARIANT PARAMILITARY PRESENCE DUMMY. CONTROLS

xtreg  share   t2002 t2006  t2002dpara t2006dpara `geo'  `demo' `drugs'  `politics', fe cluster(cod_dane)
outreg2   t2002dpara t2006dpara ///
using `output',  se  bdec(2)  excel label noaster   append 


*************************************************
****** TIME INVARIANT PARAMILITARY PRESENCE DUMMY. CONTROLS + GUERRILLA PRESENCE


xtreg  share   t2002 t2006  t2002dpara t2006dpara t2002dguer t2006dguer `geo'  `demo' `drugs'  `politics',  fe  cluster(cod_dane)
outreg2  t2002dpara t2006dpara t2002dguer t2006dguer ///
using `output', se  bdec(2)  excel label noaster   append 



replace dpara=`p3' 
replace t2002dpara=t2002*`p3' 
replace t2006dpara=t2006*`p3' 

replace dguer=`g3' 
label variable  `g3'  "Guerrilla Presence"
replace t2002dguer=t2002*`g3' 
replace t2006dguer=t2006*`g3' 



*************************************************
****** TIME VARYING PARAMILITARY PRESENCE DUMMY. 

xtreg  share   t2002 t2006 dpara t2002dpara t2006dpara, fe cluster(cod_dane)
outreg2  dpara t2002dpara t2006dpara  ///
using `output',  se bdec(2)  excel label noaster   append 


*************************************************
****** TIME VARYING PARAMILITARY PRESENCE DUMMY. CONTROLS + GUERRILLA PRESENCE

xtreg  share   t2002 t2006 dpara t2002dpara t2006dpara  dguer t2002dguer t2006dguer, fe cluster(cod_dane)
outreg2  dpara t2002dpara t2006dpara dguer t2002dguer t2006dguer ///
using `output', se  bdec(2)  excel label noaster   append 


