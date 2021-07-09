*****************************************************
*Creates Table 7: Geography and Health Variables
*****************************************************
clear
capture log close
cd G:\daron\colonial_origins
log using maketable7, replace

/*Data Files Used
	maketable7
	
*Data Files Created as Final Product
	none
	
*Data Files Created as Intermediate Product
	none*/
	
use maketable7, clear
keep if baseco==1


**********************************
*--Panels A and B, IV Regressions
**********************************

*Columns 1 and 2: Instrumenting only for average protection against expropriaton risk: malaria control

ivreg logpgp95 (avexpr=logem4) malfal94, first
ivreg logpgp95 lat_abst (avexpr=logem4) malfal94, first

*Columns 3 and 4: Instrumenting only for average protection against expropriaton risk: life expectancy control

ivreg logpgp95 (avexpr=logem4) leb95, first
ivreg logpgp95 lat_abst (avexpr=logem4) leb95, first

*Columns 5 and 6: Instrumenting only for average protection against expropriaton risk: infant mortality control

ivreg logpgp95 (avexpr=logem4) imr95, first
ivreg logpgp95 lat_abst (avexpr=logem4) imr95, first

*Column 7: Instrumenting for all RHS variables: malaria control

ivreg logpgp95 (avexpr malfal94=logem4 latabs lt100km meantemp), first

*Column 8: Instrumenting for all RHS variables: life expectancy control

ivreg logpgp95 (avexpr leb95=logem4 latabs lt100km meantemp), first

*Column 9: Instrumenting for all RHS variables: infant mortality control

ivreg logpgp95 (avexpr imr95=logem4 latabs lt100km meantemp), first

*Columns 10 and 11: Yellow fever instrument for average protection against expropriation risk

ivreg logpgp95 (avexpr = yellow), first

gen other_cont=.
replace other_cont=1 if (shortnam=="AUS" | shortnam=="MLT" | shortnam=="NZL")
recode other_cont (.=0)
tab other_cont

ivreg logpgp95 (avexpr = yellow) africa asia other_cont, first



**********************************
*--Panel C, OLS Regressions
**********************************

*Columns 1 and 2: Instrumenting only for average protection against expropriaton risk: malaria control

reg logpgp95 avexpr malfal94
reg logpgp95 lat_abst avexpr malfal94

*Columns 3 and 4: Instrumenting only for average protection against expropriaton risk: life expectancy control

reg logpgp95 avexpr leb95
reg logpgp95 lat_abst avexpr leb95

*Columns 5 and 6: Instrumenting only for average protection against expropriaton risk: infant mortality control

reg logpgp95 avexpr imr95
reg logpgp95 lat_abst avexpr imr95

*Column 7: Instrumenting for all RHS variables: malaria control
reg logpgp95 avexpr malfal94 if  (logem4!=.&latabs!=.& lt100km!=.& meantemp!=.)

*Column 8: Instrumenting for all RHS variables: life expectancy control
reg logpgp95 avexpr leb95 if  (logem4!=.&latabs!=.& lt100km!=.& meantemp!=.)

*Column 9: Instrumenting for all RHS variables: infant mortality control
reg logpgp95 avexpr imr95 if  (logem4!=.&latabs!=.& lt100km!=.& meantemp!=.)

*Columns 10 and 11: Yellow fever instrument for average protection against expropriation risk

reg logpgp95 avexpr yellow if yellow!=.

reg logpgp95 avexpr yellow africa asia other_cont if yellow!=.


