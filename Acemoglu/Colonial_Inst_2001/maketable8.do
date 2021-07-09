**********************************************************************************
*Creates Table 8: Overidentification Tests
**********************************************************************************
clear
capture log close
cd G:\daron\colonial_origins
log using maketable8, replace

/*Data Files Used
	maketable8.dta
	
*Data Files Created as Final Product
	none
	
*Data Files Created as Intermediate Product
	none*/
	

use maketable8, clear
keep if baseco==1


**********************************
*--Panels A and B, IV Regressions
**********************************

*Columns 1 - 2 (European settlements in 1900 as instrument)

ivreg logpgp95 (avexpr=euro1900), first
ivreg logpgp95 lat_abst (avexpr=euro1900), first

*Columns 3 - 4 (Constraint on executive in 1900 as instrument)

ivreg logpgp95 (avexpr=cons00a), first
ivreg logpgp95 lat_abst (avexpr=cons00a), first

*Columns 5 - 6 (Democracy in 1900)

ivreg logpgp95 (avexpr=democ00a), first
ivreg logpgp95 lat_abst (avexpr=democ00a), first

*Columns 7 - 8 (Constraint on executive in first year of independence)

ivreg logpgp95 (avexpr=cons1) indtime, first
ivreg logpgp95 lat_abst (avexpr=cons1) indtime, first

*Columns 9 - 10 (Democracy in first year of independence)

ivreg logpgp95 (avexpr=democ1) indtime, first
ivreg logpgp95 lat_abst (avexpr=democ1) indtime, first

***********************************************************************
*--Panel C, Overidentification Tests
***********************************************************************

*Columns 1 - 2 (European settlements in 1900 as instrument)

ivreg logpgp95 (avexpr=euro1900 logem4), first
estimates store efficient
ivreg logpgp95 (avexpr=euro1900), first
estimates store consistent
hausman consistent efficient

ivreg logpgp95 lat_abst (avexpr=euro1900 logem4), first
estimates store efficient
ivreg logpgp95 lat_abst (avexpr=euro1900), first
estimates store consistent
hausman consistent efficient

*Columns 3 - 4 (Constraint on executive in 1900 as instrument)

ivreg logpgp95 (avexpr=cons00a logem4), first
estimates store efficient
ivreg logpgp95 (avexpr=cons00a), first
estimates store consistent
hausman consistent efficient

ivreg logpgp95 lat_abst (avexpr=cons00a logem4), first
estimates store efficient
ivreg logpgp95 lat_abst (avexpr=cons00a), first
estimates store consistent
hausman consistent efficient

*Columns 5 - 6 (Democracy in 1900)

ivreg logpgp95 (avexpr=democ00a logem4), first
estimates store efficient
ivreg logpgp95 (avexpr=democ00a), first
estimates store consistent
hausman consistent efficient

ivreg logpgp95 lat_abst (avexpr=democ00a logem4), first
estimates store efficient
ivreg logpgp95 lat_abst (avexpr=democ00a), first
estimates store consistent
hausman consistent efficient

*Columns 7 - 8 (Constraint on executive in first year of independence)

ivreg logpgp95 (avexpr=cons1 logem4) indtime, first
estimates store efficient
ivreg logpgp95 (avexpr=cons1) indtime, first
estimates store consistent
hausman consistent efficient

ivreg logpgp95 lat_abst (avexpr=cons1 logem4) indtime, first
estimates store efficient
ivreg logpgp95 lat_abst (avexpr=cons1) indtime, first
estimates store consistent
hausman consistent efficient

*Columns 9 - 10 (Democracy in first year of independence)

ivreg logpgp95 (avexpr=democ1 logem4) indtime, first
estimates store efficient
ivreg logpgp95 (avexpr=democ1) indtime, first
estimates store consistent
hausman consistent efficient

ivreg logpgp95 lat_abst (avexpr=democ1 logem4) indtime, first
estimates store efficient
ivreg logpgp95 lat_abst (avexpr=democ1) indtime, first
estimates store consistent
hausman consistent efficient


***********************************************************************
*--Panel D, Second Stage with Log Mortality as Exogenous Variable
***********************************************************************

*Columns 1 - 2 (European settlements in 1900 as instrument)

ivreg logpgp95 (avexpr=euro1900) logem4
ivreg logpgp95 lat_abst (avexpr=euro1900) logem4

*Columns 3 - 4 (Constraint on executive in 1900 as instrument)

ivreg logpgp95 (avexpr=cons00a) logem4
ivreg logpgp95 lat_abst (avexpr=cons00a) logem4

*Columns 5 - 6 (Democracy in 1900)

ivreg logpgp95 (avexpr=democ00a) logem4
ivreg logpgp95 lat_abst (avexpr=democ00a) logem4

*Columns 7 - 8 (Constraint on executive in first year of independence)

ivreg logpgp95 (avexpr=cons1) indtime logem4
ivreg logpgp95 lat_abst (avexpr=cons1) indtime logem4

*Columns 9 - 10 (Democracy in first year of independence)

ivreg logpgp95 (avexpr=democ1) indtime logem4
ivreg logpgp95 lat_abst (avexpr=democ1) indtime logem4

