***********************************************************
*Creates Table 5: Contracting vs. Property Rights Institutions: Private Credit and Stock Market Capitalization (2SLS)
***********************************************************
clear
capture log close
cd G:\daron\unbundling
log using maketable5, replace

/*Data Files Used
	unbundle
	
*Data Files Created as Final Product
	none
	
*Data Files Created as Intermediate Product
	none*/
	
***---------Panel A. Dependent Variable: Credit to Private Sector, Second Stage of 2SLS	
	
use unbundle, clear
keep if ex2col==1

*Column 1: Log Settler Mortality
ivreg credittops1998 (sdformalism xcon1990sj = sjlouk logem4)

*Column 2: Log Population Density
ivreg credittops1998 (sdformalism xcon1990sj = sjlouk lpd1500s)

*Column 3: Log Settler Mortality
ivreg credittops1998 (ecproccompindex xcon1990sj = sjlouk logem4)

*Column 4: Log Settler Mortality
ivreg credittops1998 (ecnumprocedures xcon1990sj = sjlouk logem4)

*Column 5: Log Settler Mortality
ivreg credittops1998 (sdformalism avexpr = sjlouk logem4)

*Column 6: Log Settler Mortality
ivreg credittops1998 (sdformalism efhrpr7 = sjlouk logem4)


***-----------Results in Equivalent OLS Specification


*Column 1: Log Settler Mortality
reg credittops1998 sdformalism xcon1990sj if (sjlouk!=. & logem4!=.)

*Column 2: Log Population Density
reg credittops1998 sdformalism xcon1990sj if (sjlouk!=. & lpd1500s!=.)

*Column 3: Log Settler Mortality
reg credittops1998 ecproccompindex xcon1990sj if (sjlouk !=. & logem4!=.)

*Column 4: Log Settler Mortality
reg credittops1998 ecnumprocedures xcon1990sj if (sjlouk!=. & logem4!=.)

*Column 5: Log Settler Mortality
reg credittops1998 sdformalism avexpr if (sjlouk!=. & logem4!=.)

*Column 6: Log Settler Mortality
reg credittops1998 sdformalism efhrpr7 if (sjlouk!=. & logem4!=.)



***------Panel B. Dependent Variable: Stock Market Capitalization, Second Stage of 2SLS



use unbundle, clear
keep if ex2col==1

*Column 1: Log Settler Mortality
ivreg mcapsj (sdformalism xcon1990sj = sjlouk logem4)

*Column 2: Log Population Density
ivreg mcapsj (sdformalism xcon1990sj = sjlouk lpd1500s)

*Column 3: Log Settler Mortality
ivreg mcapsj (ecproccompindex xcon1990sj = sjlouk logem4)

*Column 4: Log Settler Mortality
ivreg mcapsj (ecnumprocedures xcon1990sj = sjlouk logem4)

*Column 5: Log Settler Mortality
ivreg mcapsj (sdformalism avexpr = sjlouk logem4)

*Column 6: Log Settler Mortality
ivreg mcapsj (sdformalism efhrpr7 = sjlouk logem4)


***-----------Results in Equivalent OLS Specification


*Column 1: Log Settler Mortality
reg mcapsj sdformalism xcon1990sj if (sjlouk!=. & logem4!=.)

*Column 2: Log Population Density
reg mcapsj sdformalism xcon1990sj if (sjlouk!=. & lpd1500s!=.)

*Column 3: Log Settler Mortality
reg mcapsj ecproccompindex xcon1990sj if (sjlouk !=. & logem4!=.)

*Column 4: Log Settler Mortality
reg mcapsj ecnumprocedures xcon1990sj if (sjlouk!=. & logem4!=.)

*Column 5: Log Settler Mortality
reg mcapsj sdformalism avexpr if (sjlouk!=. & logem4!=.)

*Column 6: Log Settler Mortality
reg mcapsj sdformalism efhrpr7 if (sjlouk!=. & logem4!=.)

