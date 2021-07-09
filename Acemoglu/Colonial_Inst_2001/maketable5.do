**********************************************************************************
*Creates Table 5: IV Regressions of Log GDP per Capita with Additional Controls
**********************************************************************************
clear
capture log close
cd G:\daron\colonial_origins
log using maketable5, replace

/*Data Files Used
	maketable5.dta
	
*Data Files Created as Final Product
	none
	
*Data Files Created as Intermediate Product
	none*/
	

use maketable5, clear
keep if baseco==1

**********************************
*--Panels A and B, IV Regressions
**********************************

*--Columns 1 and 2 (British and French colony dummies)

ivreg logpgp95 (avexpr=logem4) f_brit f_french, first
ivreg logpgp95 lat_abst (avexpr=logem4) f_brit f_french, first

*--Columns 3 and 4 (British colonies only)

ivreg logpgp95 (avexpr=logem4) if f_brit==1, first
ivreg logpgp95 lat_abst (avexpr=logem4) if f_brit==1, first

*--Columns 5 and 6 (Control for French legel origin)

ivreg logpgp95 (avexpr=logem4) sjlofr, first
ivreg logpgp95 lat_abst (avexpr=logem4) sjlofr, first

*--Columns 7 and 8 (Religion dummies)

ivreg logpgp95 (avexpr=logem4) catho80 muslim80 no_cpm80, first
ivreg logpgp95 lat_abst (avexpr=logem4) catho80 muslim80 no_cpm80, first

*--Columns 9 (Multiple controls)

ivreg logpgp95 (avexpr=logem4) f_french sjlofr catho80 muslim80 no_cpm80, first
ivreg logpgp95 lat_abst (avexpr=logem4) f_french sjlofr catho80 muslim80 no_cpm80, first


**********************************
*--Panel C, OLS Regressions
**********************************


*--Columns 1 and 2 (British and French colony dummies)

reg logpgp95 avexpr f_brit f_french
reg logpgp95 lat_abst avexpr f_brit f_french

*--Columns 3 and 4 (British colonies only)

reg logpgp95 avexpr if f_brit==1
reg logpgp95 lat_abst avexpr if f_brit==1

*--Columns 5 and 6 (Control for French legel origin)

reg logpgp95 avexpr sjlofr
reg logpgp95 lat_abst avexpr sjlofr

*--Columns 7 and 8 (Religion dummies)

reg logpgp95 avexpr catho80 muslim80 no_cpm80
reg logpgp95 lat_abst avexpr catho80 muslim80 no_cpm80

*--Columns 9 (Multiple controls)

reg logpgp95 lat_abst avexpr f_french sjlofr catho80 muslim80 no_cpm80

