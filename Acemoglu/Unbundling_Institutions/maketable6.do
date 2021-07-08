***********************************************************
*Creates Table 6: Semi-Reduced Forms: Sample of Ex-Colonies
***********************************************************
clear
capture log close
cd G:\daron\unbundling
log using maketable6, replace

/*Data Files Used
	unbundle
	
*Data Files Created as Final Product
	none
	
*Data Files Created as Intermediate Product
	none*/
	
***----Panel A. With Constraint on Executive, Second Stage from 2SLS
use unbundle, clear
keep if ex2col==1

*Column 1: Dependent Variable: Log GDP per Capita, Instrument: Log Settler Mortality

ivreg loggdppc1995 sjlouk (xcon1990sj = logem4) if ecnumprocedures!=.

*Column 2: Dependent Variable: Log GDP per Capita, Instrument: Log Population Density

ivreg loggdppc1995 sjlouk (xcon1990sj = lpd1500s) if ecnumprocedures!=.

*Column 3: Dependent Variable: Investment-GDP Ratio, Instrument: Log Settler Mortality

ivreg avgci1990s sjlouk (xcon1990sj = logem4) if (ecnumprocedures!=. & loggdppc1995!=.)

*Column 4: Dependent Variable: Investment-GDP Ratio, Instrument: Log Population Density

ivreg avgci1990s sjlouk (xcon1990sj = lpd1500s) if (ecnumprocedures!=. & loggdppc1995!=.)

*Column 5: Dependent Variable: Credit to the Private Sector, Instrument: Log Settler Mortality

ivreg credittops1998  sjlouk (xcon1990sj = logem4) if (ecnumprocedures!=. )

*Column 6: Dependent Variable: Credit to the Private Sector, Instrument: Log Population Density

ivreg credittops1998 sjlouk (xcon1990sj = lpd1500s) if (ecnumprocedures!=. )

*Column 7: Dependent Variable: Stock Market Capitalization, Instrument: Log Settler Mortality

ivreg mcapsj  sjlouk (xcon1990sj = logem4) if (ecnumprocedures!=. )

*Column 8: Dependent Variable: Stock Market Capitalization, Instrument: Log Population Density

ivreg mcapsj sjlouk (xcon1990sj = lpd1500s) if (ecnumprocedures!=. )


***----Panel B. With Protection against Expropriation, Second Stage from 2SLS


*Column 1: Dependent Variable: Log GDP per Capita, Instrument: Log Settler Mortality

ivreg loggdppc1995 sjlouk (avexpr = logem4) if (ecnumprocedures!=. ) 

*Column 2: Dependent Variable: Log GDP per Capita, Instrument: Log Population Density

ivreg loggdppc1995 sjlouk (avexpr = lpd1500s) if (ecnumprocedures!=. ) 

*Column 3: Dependent Variable: Investment-GDP Ratio, Instrument: Log Settler Mortality

ivreg avgci1990s sjlouk (avexpr = logem4) if (ecnumprocedures!=. /**/)

*Column 4: Dependent Variable: Investment-GDP Ratio, Instrument: Log Population Density

ivreg avgci1990s sjlouk (avexpr = lpd1500s) if (ecnumprocedures!=. /**/)

*Column 5: Dependent Variable: Credit to the Private Sector, Instrument: Log Settler Mortality

ivreg credittops1998  sjlouk (avexpr = logem4) if (ecnumprocedures!=. )

*Column 6: Dependent Variable: Credit to the Private Sector, Instrument: Log Population Density

ivreg credittops1998 sjlouk (avexpr = lpd1500s) if (ecnumprocedures!=. )

*Column 7: Dependent Variable: Stock Market Capitalization, Instrument: Log Settler Mortality

ivreg mcapsj  sjlouk (avexpr = logem4) if (ecnumprocedures!=. )

*Column 8: Dependent Variable: Stock Market Capitalization, Instrument: Log Population Density

ivreg mcapsj sjlouk (avexpr = lpd1500s) if (ecnumprocedures!=. )




***----Panel C. With Private Property, Second Stage from 2SLS


*Column 1: Dependent Variable: Log GDP per Capita, Instrument: Log Settler Mortality

ivreg loggdppc1995 sjlouk (efhrpr7 = logem4) if (ecnumprocedures!=. ) 

*Column 2: Dependent Variable: Log GDP per Capita, Instrument: Log Population Density

ivreg loggdppc1995 sjlouk (efhrpr7 = lpd1500s) if (ecnumprocedures!=. ) 

*Column 3: Dependent Variable: Investment-GDP Ratio, Instrument: Log Settler Mortality

ivreg avgci1990s sjlouk (efhrpr7 = logem4) if (ecnumprocedures!=. )

*Column 4: Dependent Variable: Investment-GDP Ratio, Instrument: Log Population Density

ivreg avgci1990s sjlouk (efhrpr7 = lpd1500s) if (ecnumprocedures!=. )

*Column 5: Dependent Variable: Credit to the Private Sector, Instrument: Log Settler Mortality

ivreg credittops1998  sjlouk (efhrpr7 = logem4) if (ecnumprocedures!=. )

*Column 6: Dependent Variable: Credit to the Private Sector, Instrument: Log Population Density

ivreg credittops1998 sjlouk (efhrpr7 = lpd1500s) if (ecnumprocedures!=. )

*Column 7: Dependent Variable: Stock Market Capitalization, Instrument: Log Settler Mortality

ivreg mcapsj  sjlouk (efhrpr7 = logem4) if (ecnumprocedures!=. )

*Column 8: Dependent Variable: Stock Market Capitalization, Instrument: Log Population Density

ivreg mcapsj sjlouk (efhrpr7 = lpd1500s) if (ecnumprocedures!=. )


