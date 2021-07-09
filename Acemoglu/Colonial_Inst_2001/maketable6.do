**********************************************************************************
*Creates Table 6: Robustness Checks for IV Regressions with Log GDP Per Capita
**********************************************************************************
clear
capture log close
cd G:\daron\colonial_origins
log using maketable6, replace

/*Data Files Used
	maketable6.dta
	
*Data Files Created as Final Product
	none
	
*Data Files Created as Intermediate Product
	none*/
	
use maketable6, clear
keep if baseco==1


**********************************
*--Panels A and B, IV Regressions
**********************************

*--Columns 1 and 2 (Temperature and humidity controls)

ivreg logpgp95 (avexpr=logem4) temp* humid*, first
ivreg logpgp95 lat_abst (avexpr=logem4) temp* humid*, first

*--Columns 3 and 4 (Control for percent of European descent in 1975)

ivreg logpgp95 (avexpr=logem4) edes1975, first
ivreg logpgp95 lat_abst (avexpr=logem4) edes1975, first

*--Columns 5 and 6 (Controls for soil quality, natural resources, and landlocked)

ivreg logpgp95 (avexpr=logem4)  steplow deslow stepmid desmid drystep drywint goldm iron silv zinc oilres landlock, first
ivreg logpgp95 lat_abst (avexpr=logem4)  steplow deslow stepmid desmid drystep  drywint goldm iron silv zinc oilres landlock, first

*--Columns 7 and 8 (Control for ethnolinguistic fragmentation)

ivreg logpgp95 (avexpr=logem4) avelf, first
ivreg logpgp95 lat_abst (avexpr=logem4) avelf, first

*--Column 9 (All Controls)
ivreg logpgp95 lat_abst (avexpr=logem4) temp* humid* edes1975 avelf steplow deslow stepmid desmid drystep  drywint goldm iron silv zinc oilres landlock, first


**********************************
*--Panel C, OLS Regressions
**********************************


*--Columns 1 and 2 (Temperature and humidity controls)

reg logpgp95 avexpr temp* humid*
reg logpgp95 lat_abst avexpr temp* humid*

*--Columns 3 and 4 (Control for percent of European descent in 1975)

reg logpgp95 avexpr edes1975
reg logpgp95 lat_abst avexpr edes1975

*--Columns 5 and 6 (Controls for soil quality, natural resources, and landlocked)

reg logpgp95 avexpr  steplow deslow stepmid desmid drystep drywint goldm iron silv zinc oilres landlock
reg logpgp95 lat_abst avexpr  steplow deslow stepmid desmid drystep  drywint goldm iron silv zinc oilres landlock

*--Columns 7 and 8 (Control for ethnolinguistic fragmentation)

reg logpgp95 avexpr avelf
reg logpgp95 lat_abst avexpr avelf

*--Column 9 (All Controls)
reg logpgp95 lat_abst avexpr temp* humid* edes1975 avelf steplow deslow stepmid desmid drystep  drywint goldm iron silv zinc oilres landlock


log close
