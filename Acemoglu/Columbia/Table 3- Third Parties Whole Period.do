*    This program runs regressions on third parties vote share (Baseline with attacks). 

clear
set mem 500m
set matsize 1500
set logtype text 
set more off

cd "E:\the monopoly of violence 2012\data\national level voting data"


*Database with third parties vote share 
use legislative, clear

gen t1994=0
gen t1998=0
gen t2002=0
gen t2006=0
replace t1994=1 if year==1994
replace t1998=1 if year==1998
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



su share_third if year <=2001
su share_third  if dumpar_9701==1 &  year <= 2001
su share_third  if dumpar_9701==0 &  year <= 2001

su share_third if year > 2001
su share_third  if dumpar_9701==1 &  year > 2001
su share_third  if dumpar_9701==0 &  year > 2001




********************************************************************************************************************************************************
*Paramilitary Presence measures used in this program 

*attacks in levels 
local p1="par_9701"
local g1="guer_9701"

*attacks dummy
local p2="dumpar_9701"
local g2="dumguer_9701"

*displaced in levels
local p3="par_9701d"
local g3="guer_9701d"

*displaced dummies
local p4="dumpar_9701d"
local g4="dumguer_9701d"

*attacks time varying dummy
local p5="dumparv"
local g5="dumguerv"

local output = "table_3_Paramilitary_Presence_and_Third_Parties_Share_of_Votes_in_the_Senate"


******************************************************************************************************************************************************

cd "E:\the monopoly of violence 2012\outreg"



gen dpara=`p1'
label variable dpara "Paramilitary Presence"
gen t1994dpara=t1994*`p1'
label variable  t1994dpara "Paramilitary Presence X 1994"
gen t1998dpara=t1998*`p1'
label variable  t1998dpara "Paramilitary Presence X 1998"
gen t2002dpara=t2002*`p1'
label variable  t2002dpara "Paramilitary Presence X 2002"
gen t2006dpara=t2006*`p1'
label variable  t2006dpara "Paramilitary Presence X 2006"



gen dguer=`g1'
label variable dguer "Guerrilla Presence"
gen t1994dguer=t1994*`g1'
label variable  t1994dguer "Guerrilla Presence X 1994"
gen t1998dguer=t1998*`g1'
label variable  t1998dguer "Guerrilla Presence X 1998"
gen t2002dguer=t2002*`g1'
label variable  t2002dguer "Guerrilla Presence X 2002"
gen t2006dguer=t2006*`g1'
label variable  t2006dguer "Guerrilla Presence X 2006"





*time interacted variables
foreach  x in altura discap precipitacion pobav rur ginix nbi eficiencia dcoca damapola  ags up86  {

gen t1994`x'=t1994*`x'
gen t1998`x'=t1998*`x'
gen t2002`x'=t2002*`x'
gen t2006`x'=t2006*`x'

}




* our set of controls

local controlsa = "t1994altura t1998altura t2002altura t2006altura  t1994discap t1998discap t2002discap t2006discap" 
local controlsb = "t1994precipitacion t1998precipitacion t2002precipitacion t2006precipitacion" 
local geo = "`controlsa' `controlsb'"


local controlsc = "t1994pobav t1998pobav t2002pobav t2006pobav t1994rur t1998rur t2002rur t2006rur t1994ginix t1998ginix t2002ginix t2006ginix " 
local controlsd = "t1994nbi t1998nbi t2002nbi t2006nbi" 
local demo = "`controlsc' `controlsd'"


local drugs = "t1994dcoca t1998dcoca t2002dcoca t2006dcoca t1994damapola t1998damapola t2002damapola t2006damapola" 

local politics = "t1994ags t1998ags t2002ags t2006ags t1994discap t1994up86 t1998up86 t2002up86 t2006up86" 


xtset cod_dane year


*************************************************
****** TIME INVARIANT PARAMILITARY PRESENCE (Attacks).

xtreg  share_third t1994 t1998 t2002 t2006  t1994dpara t1998dpara t2002dpara t2006dpara , fe   cluster(cod_dane)  
outreg2  t1994dpara t1998dpara  t2002dpara t2006dpara ///
using `output',  bdec(2) excel label noaster se  replace    ///
title (Table 3: Paramilitary Presence and Third Parties Share of Votes ) 



*************************************************
****** TIME INVARIANT PARAMILITARY PRESENCE + CONTROLS (Attacks).

xtreg   share_third t1994 t1998 t2002 t2006  t1994dpara t1998dpara t2002dpara t2006dpara `geo'  `demo' `drugs'  `politics', fe   cluster(cod_dane)  
outreg2  t1994dpara t1998dpara  t2002dpara t2006dpara ///
using `output',  bdec(2) excel label noaster se  append


*************************************************
****** TIME INVARIANT PARAMILITARY PRESENCE + CONTROLS + GUERRILLA PRESENCE (Attacks).

xtreg   share_third t1994 t1998 t2002 t2006  t1994dpara t1998dpara t2002dpara t2006dpara  t1994dguer t1998dguer t2002dguer t2006dguer ///
`geo'  `demo' `drugs'  `politics' , fe   cluster(cod_dane)  
outreg2   t1994dpara t1998dpara  t2002dpara t2006dpara t1994dguer t1998dguer  t2002dguer t2006dguer ///
using `output',  bdec(2) excel label noaster se  append




* changing paramilitary presence measure

replace dpara=`p2'
replace t1994dpara=t1994*`p2'
replace t1998dpara=t1998*`p2'
replace t2002dpara=t2002*`p2'
replace t2006dpara=t2006*`p2'

replace dguer=`g2'
replace t1994dguer=t1994*`g2'
replace t1998dguer=t1998*`g2'
replace t2002dguer=t2002*`g2'
replace t2006dguer=t2006*`g2'






*************************************************
****** TIME INVARIANT PARAMILITARY PRESENCE Dummy.

xtreg   share_third t1994 t1998 t2002 t2006  t1994dpara t1998dpara t2002dpara t2006dpara   , fe   cluster(cod_dane)  
outreg2  t1994dpara t1998dpara  t2002dpara t2006dpara ///
using `output', bdec(2) excel label noaster se append

*************************************************
****** TIME INVARIANT PARAMILITARY PRESENCE Dummy + CONTROLS

xtreg   share_third t1994 t1998 t2002 t2006  t1994dpara t1998dpara t2002dpara t2006dpara `geo'  `demo' `drugs'  `politics' , fe   cluster(cod_dane)  
outreg2  t1994dpara t1998dpara  t2002dpara t2006dpara ///
using `output',  bdec(2) excel label noaster se  append



*************************************************
****** TIME INVARIANT PARAMILITARY PRESENCE Dummy + CONTROLS + GUERRILLA PRESENCE


xtreg    share_third t1994 t1998 t2002 t2006  t1994dpara t1998dpara t2002dpara t2006dpara  t1994dguer t1998dguer t2002dguer t2006dguer   ///
`geo'  `demo' `drugs'  `politics', fe   cluster(cod_dane)  
outreg2   t1994dpara t1998dpara  t2002dpara t2006dpara t1994dguer t1998dguer  t2002dguer t2006dguer ///
using `output',  bdec(2) excel label noaster se  append



replace dpara=`p5'  

label variable  `p5' "Paramilitary Presence"
replace t2002dpara=t2002*`p5' 
replace t2006dpara=t2006*`p5' 

replace dguer=`g5' 

label variable  `g5'  "Guerrilla Presence"
replace t2002dguer=t2002*`g5' 
replace t2006dguer=t2006*`g5' 



*************************************************
****** TIME VARYING PARAMILITARY PRESENCE DUMMY. 

xtreg  share_third   t2002 t2006 dpara t2002dpara t2006dpara, fe cluster(cod_dane)
outreg2  dpara t2002dpara t2006dpara  ///
using `output', bdec(2) excel label noaster se   append



*************************************************
****** TIME VARYING PARAMILITARY PRESENCE DUMMY. With GuERRILLA PRESENCE

xtreg  share_third   t2002 t2006 dpara t2002dpara t2006dpara  dguer t2002dguer t2006dguer, fe cluster(cod_dane)
outreg2  dpara t2002dpara t2006dpara dguer t2002dguer t2006dguer ///
using `output',    bdec(2) excel label noaster se   append


