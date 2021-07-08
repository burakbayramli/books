***********************************************************
*Creates Table 4: Contracting vs. Property Rights Institutions: GDP per Capita and Investment- GDP Ratio (2SLS)
***********************************************************
clear
capture log close
cd G:\daron\unbundling
log using maketable4, replace

/*Data Files Used
	unbundle
	
*Data Files Created as Final Product
	none
	
*Data Files Created as Intermediate Product
	none*/
	
***---------Panel A. Dependent Variable: Log GDP per Capita, Second Stage of 2SLS	
	
use unbundle, clear
keep if ex2col==1

*Column 1: Log Settler Mortality
ivreg loggdppc1995 (sdformalism xcon1990sj = sjlouk logem4)

*Column 2: Log Population Density
ivreg loggdppc1995 (sdformalism xcon1990sj = sjlouk lpd1500s)

*Column 3: Log Settler Mortality
ivreg loggdppc1995 (ecproccompindex xcon1990sj = sjlouk logem4)

*Column 4: Log Settler Mortality
ivreg loggdppc1995 (ecnumprocedures xcon1990sj = sjlouk logem4)

*Column 5: Log Settler Mortality
ivreg loggdppc1995 (sdformalism avexpr = sjlouk logem4)

*Column 6: Log Settler Mortality
ivreg loggdppc1995 (sdformalism efhrpr7 = sjlouk logem4)


***-----------Results in Equivalent OLS Specification


*Column 1: Log Settler Mortality
reg loggdppc1995 sdformalism xcon1990sj if (sjlouk!=. & logem4!=.)

*Column 2: Log Population Density
reg loggdppc1995 sdformalism xcon1990sj if (sjlouk!=. & lpd1500s!=.)

*Column 3: Log Settler Mortality
reg loggdppc1995 ecproccompindex xcon1990sj if (sjlouk !=. & logem4!=.)

*Column 4: Log Settler Mortality
reg loggdppc1995 ecnumprocedures xcon1990sj if (sjlouk!=. & logem4!=.)

*Column 5: Log Settler Mortality
reg loggdppc1995 sdformalism avexpr if (sjlouk!=. & logem4!=.)

*Column 6: Log Settler Mortality
reg loggdppc1995 sdformalism efhrpr7 if (sjlouk!=. & logem4!=.)



***------Panel B. Dependent Variable: Investment-GDP Ratio, Second Stage of 2SLS



use unbundle, clear
keep if ex2col==1

*Column 1: Log Settler Mortality
ivreg avgci1990s (sdformalism xcon1990sj = sjlouk logem4)

*Column 2: Log Population Density
ivreg avgci1990s (sdformalism xcon1990sj = sjlouk lpd1500s)

*Column 3: Log Settler Mortality
ivreg avgci1990s (ecproccompindex xcon1990sj = sjlouk logem4)

*Column 4: Log Settler Mortality
ivreg avgci1990s (ecnumprocedures xcon1990sj = sjlouk logem4)

*Column 5: Log Settler Mortality
ivreg avgci1990s (sdformalism avexpr = sjlouk logem4)

*Column 6: Log Settler Mortality
ivreg avgci1990s (sdformalism efhrpr7 = sjlouk logem4)


***-----------Results in Equivalent OLS Specification


*Column 1: Log Settler Mortality
reg avgci1990s sdformalism xcon1990sj if (sjlouk!=. & logem4!=.)

*Column 2: Log Population Density
reg avgci1990s sdformalism xcon1990sj if (sjlouk!=. & lpd1500s!=.)

*Column 3: Log Settler Mortality
reg avgci1990s ecproccompindex xcon1990sj if (sjlouk !=. & logem4!=.)

*Column 4: Log Settler Mortality
reg avgci1990s ecnumprocedures xcon1990sj if (sjlouk!=. & logem4!=.)

*Column 5: Log Settler Mortality
reg avgci1990s sdformalism avexpr if (sjlouk!=. & logem4!=.)

*Column 6: Log Settler Mortality
reg avgci1990s sdformalism efhrpr7 if (sjlouk!=. & logem4!=.)

