***********************************************************
*Creates Table 4: IV Regressions of Log GDP per Capita
***********************************************************
clear
capture log close
cd G:\daron\colonial_origins
log using maketable4, replace

/*Data Files Used
	maketable4.dta
	
*Data Files Created as Final Product
	none
	
*Data Files Created as Intermediate Product
	none*/
	

use maketable4, clear
keep if baseco==1

**********************************
*--Panels A and B, IV Regressions
**********************************
*Columns 1 - 2 (Base Sample)

ivreg logpgp95 (avexpr=logem4), first
ivreg logpgp95 lat_abst (avexpr=logem4), first


*Columns 3 - 4 (Base Sample w/o Neo-Europes)

ivreg logpgp95 (avexpr=logem4) if rich4!=1, first
ivreg logpgp95 lat_abst (avexpr=logem4) if rich4!=1, first


*Columns 5 - 6 (Base Sample w/o Africa)

ivreg logpgp95 (avexpr=logem4) if africa!=1, first
ivreg logpgp95 lat_abst (avexpr=logem4) if africa!=1, first


*Columns 7 - 8 (Base Sample with continent dummies)

gen other_cont=.
replace other_cont=1 if (shortnam=="AUS" | shortnam=="MLT" | shortnam=="NZL")
recode other_cont (.=0)
tab other_cont

ivreg logpgp95 (avexpr=logem4) africa asia other_cont, first
ivreg logpgp95 lat_abst (avexpr=logem4) africa asia other_cont, first


*Column 9 (Base Sample, log GDP per worker)

ivreg loghjypl (avexpr=logem4), first




**********************************
*--Panel C, OLS Regressions
**********************************

*Columns 1 - 2 (Base Sample)

reg logpgp95 avexpr
reg logpgp95 lat_abst avexpr


*Columns 3 - 4 (Base Sample w/o Neo-Europes)

reg logpgp95 avexpr if rich4!=1
reg logpgp95 lat_abst avexpr if rich4!=1


*Columns 5 - 6 (Base Sample w/o Africa)

reg logpgp95 avexpr if africa!=1
reg logpgp95 lat_abst avexpr if africa!=1


*Columns 7 - 8 (Base Sample with continent dummies)

reg logpgp95 avexpr africa asia other_cont
reg logpgp95 lat_abst avexpr africa asia other_cont


*Column 9 (Base Sample, log GDP per worker)

reg loghjypl avexpr


