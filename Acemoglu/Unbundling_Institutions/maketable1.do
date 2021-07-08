***********************************************************
*Creates Table 1: Descriptive Statistics
***********************************************************
clear
capture log close
cd G:\daron\unbundling
log using maketable1, replace

/*Data Files Used
	unbundle
	
*Data Files Created as Final Product
	none
	
*Data Files Created as Intermediate Product
	none*/

***----Column 1: World Sample

use unbundle, clear

*Row 1: Legal Formalism
summ sdformalism

*Row 2: Procedural Complexity
summ ecproccompindex  

*Row 3: Number of Procedures
summ ecnumprocedures 

*Row 4: Constraint on Executive
summ  xcon1990sj

*Row 5: Average Protection Against Expropriation
summ avexpr

*Row 6: Private Property
summ efhrpr7

*Row 7: Log GDP per capita in 1995 (PPP measure)
summ loggdppc1995

*Row 8: Average investment-GDP ratio
summ avgci1990s

*Row 9: Credit to the private sector
summ credittops1998

*Row 10: Stock market capitalization
summ mcapsj

*Row 11: Log settler mortality
summ logem4

*Row 12: Log population density in 1500
summ lpd1500s 


***----Column 2: Ex-Colonies Sample

use unbundle, clear
keep if ex2col==1

*Row 1: Legal Formalism
summ sdformalism

*Row 2: Procedural Complexity
summ ecproccompindex  

*Row 3: Number of Procedures
summ ecnumprocedures 

*Row 4: Constraint on Executive
summ  xcon1990sj

*Row 5: Average Protection Against Expropriation
summ avexpr

*Row 6: Private Property
summ efhrpr7

*Row 7: Log GDP per capita in 1995 (PPP measure)
summ loggdppc1995

*Row 8: Average investment-GDP ratio
summ avgci1990s

*Row 9: Credit to the private sector
summ credittops1998

*Row 10: Stock market capitalization
summ mcapsj

*Row 11: Log settler mortality
summ logem4

*Row 12: Log population density in 1500
summ lpd1500s 


***----Column 3: English Ex-Colonies

use unbundle, clear
keep if (ex2col==1 & sjlouk==1)

*Row 1: Legal Formalism
summ sdformalism

*Row 2: Procedural Complexity
summ ecproccompindex  

*Row 3: Number of Procedures
summ ecnumprocedures 

*Row 4: Constraint on Executive
summ  xcon1990sj

*Row 5: Average Protection Against Expropriation
summ avexpr

*Row 6: Private Property
summ efhrpr7

*Row 7: Log GDP per capita in 1995 (PPP measure)
summ loggdppc1995

*Row 8: Average investment-GDP ratio
summ avgci1990s

*Row 9: Credit to the private sector
summ credittops1998

*Row 10: Stock market capitalization
summ mcapsj

*Row 11: Log settler mortality
summ logem4

*Row 12: Log population density in 1500
summ lpd1500s 


***----Column 4: English Ex-Colonies (Low Settler Mortality)

use unbundle, clear
egen median = median(logem4)
keep if (ex2col==1 & sjlouk==1)
keep if logem4<=median

*Row 1: Legal Formalism
summ sdformalism

*Row 2: Procedural Complexity
summ ecproccompindex  

*Row 3: Number of Procedures
summ ecnumprocedures 

*Row 4: Constraint on Executive
summ  xcon1990sj

*Row 5: Average Protection Against Expropriation
summ avexpr

*Row 6: Private Property
summ efhrpr7

*Row 7: Log GDP per capita in 1995 (PPP measure)
summ loggdppc1995

*Row 8: Average investment-GDP ratio
summ avgci1990s

*Row 9: Credit to the private sector
summ credittops1998

*Row 10: Stock market capitalization
summ mcapsj

*Row 11: Log settler mortality
summ logem4

*Row 12: Log population density in 1500
summ lpd1500s 


***----Column 5: English Ex-Colonies (High Settler Mortality)

use unbundle, clear
egen median = median(logem4)
keep if (ex2col==1 & sjlouk==1)
keep if logem4>median

*Row 1: Legal Formalism
summ sdformalism

*Row 2: Procedural Complexity
summ ecproccompindex  

*Row 3: Number of Procedures
summ ecnumprocedures 

*Row 4: Constraint on Executive
summ  xcon1990sj

*Row 5: Average Protection Against Expropriation
summ avexpr

*Row 6: Private Property
summ efhrpr7

*Row 7: Log GDP per capita in 1995 (PPP measure)
summ loggdppc1995

*Row 8: Average investment-GDP ratio
summ avgci1990s

*Row 9: Credit to the private sector
summ credittops1998

*Row 10: Stock market capitalization
summ mcapsj

*Row 11: Log settler mortality
summ logem4

*Row 12: Log population density in 1500
summ lpd1500s 


***----Column 6: French Ex-Colonies

use unbundle, clear
keep if (ex2col==1 & sjlofr==1)


*Row 1: Legal Formalism
summ sdformalism

*Row 2: Procedural Complexity
summ ecproccompindex  

*Row 3: Number of Procedures
summ ecnumprocedures 

*Row 4: Constraint on Executive
summ  xcon1990sj

*Row 5: Average Protection Against Expropriation
summ avexpr

*Row 6: Private Property
summ efhrpr7

*Row 7: Log GDP per capita in 1995 (PPP measure)
summ loggdppc1995

*Row 8: Average investment-GDP ratio
summ avgci1990s

*Row 9: Credit to the private sector
summ credittops1998

*Row 10: Stock market capitalization
summ mcapsj

*Row 11: Log settler mortality
summ logem4

*Row 12: Log population density in 1500
summ lpd1500s 


***----Column 7: French Ex-Colonies (Low Settler) Mortality

use unbundle, clear

egen median = median(logem4)
keep if (ex2col==1 & sjlofr==1)
keep if logem4<=median


*Row 1: Legal Formalism
summ sdformalism

*Row 2: Procedural Complexity
summ ecproccompindex  

*Row 3: Number of Procedures
summ ecnumprocedures 

*Row 4: Constraint on Executive
summ  xcon1990sj

*Row 5: Average Protection Against Expropriation
summ avexpr

*Row 6: Private Property
summ efhrpr7

*Row 7: Log GDP per capita in 1995 (PPP measure)
summ loggdppc1995

*Row 8: Average investment-GDP ratio
summ avgci1990s

*Row 9: Credit to the private sector
summ credittops1998

*Row 10: Stock market capitalization
summ mcapsj

*Row 11: Log settler mortality
summ logem4

*Row 12: Log population density in 1500
summ lpd1500s 


***----Column 8: French Ex-Colonies (High Settler) Mortality

use unbundle, clear
egen median = median(logem4)
keep if (ex2col==1 & sjlofr==1)
keep if logem4>median

*Row 1: Legal Formalism
summ sdformalism

*Row 2: Procedural Complexity
summ ecproccompindex  

*Row 3: Number of Procedures
summ ecnumprocedures 

*Row 4: Constraint on Executive
summ  xcon1990sj

*Row 5: Average Protection Against Expropriation
summ avexpr

*Row 6: Private Property
summ efhrpr7

*Row 7: Log GDP per capita in 1995 (PPP measure)
summ loggdppc1995

*Row 8: Average investment-GDP ratio
summ avgci1990s

*Row 9: Credit to the private sector
summ credittops1998

*Row 10: Stock market capitalization
summ mcapsj

*Row 11: Log settler mortality
summ logem4

*Row 12: Log population density in 1500
summ lpd1500s 





