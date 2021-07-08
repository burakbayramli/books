***********************************************************
*Creates Table 7: Contracting vs. Property Rights Institutions: Alternative Samples
***********************************************************
clear
capture log close
cd G:\daron\unbundling
log using maketable7, replace

/*Data Files Used
	unbundle
	
*Data Files Created as Final Product
	none
	
*Data Files Created as Intermediate Product
	none*/
	
	
use unbundle, clear
keep if ex2col==1

***-------Panel A

*Column 1: Dependent Variable: Log GDP per Capita in 1995, Common-Law Countries

ivreg loggdppc1995 (xcon1990sj = logem4) if (sjlouk==1), first

*Column 2: Dependent Variable: Log GDP per Capita in 1995, French Legal Origin Countries

ivreg loggdppc1995 (xcon1990sj = logem4) if (sjlofr==1), first

*Column 3: Dependent Variable: Log GDP per Capita in 1995, Without Neo-Europes

ivreg loggdppc1995 (sdformalism xcon1990sj = sjlouk logem4) if (rich4!=1), first

*Column 4: Dependent Variable: Log GDP per Capita in 1995, Countries Above Median World Income

ivreg loggdppc1995 (sdformalism xcon1990sj = sjlouk logem4) if loggdppc1995>7.135, first

*Column 5: Dependent Variable: Investment-GDP Ratio in 1990s, Common-Law Countries

ivreg avgci1990s (xcon1990sj = logem4) if (sjlouk==1), first 

*Column 6: Dependent Variable: Investment-GDP Ratio in 1990s, French Legal Origin Countries

ivreg avgci1990s (xcon1990sj = logem4) if (sjlofr==1), first

*Column 7: Dependent Variable: Investment-GDP Ratio in 1990s, Without Neo-Europes

ivreg avgci1990s (sdformalism xcon1990sj = sjlouk logem4) if (rich4!=1), first

*Column 8: Dependent Variable: Investment-GDP Ratio in 1990s, Countries Above Median World Income

ivreg avgci1990s (sdformalism xcon1990sj = sjlouk logem4) if loggdppc1995>7.135, first



***-------Panel B

*Column 1: Dependent Variable: Log GDP per Capita in 1995, Common-Law Countries

ivreg credittops1998 (xcon1990sj = logem4) if (sjlouk==1)

*Column 2: Dependent Variable: Log GDP per Capita in 1995, French Legal Origin Countries

ivreg credittops1998 (xcon1990sj = logem4) if (sjlofr==1)

*Column 3: Dependent Variable: Log GDP per Capita in 1995, Without Neo-Europes

ivreg credittops1998 (sdformalism xcon1990sj = sjlouk logem4) if (rich4!=1)

*Column 4: Dependent Variable: Log GDP per Capita in 1995, Dropping Outliers

ivreg credittops1998 (sdformalism xcon1990sj = sjlouk logem4) if (shortnam~="BWA" & shortnam~="USA" & shortnam~="ZAF" & shortnam~="MYS")

*Column 5: Dependent Variable: Investment-GDP Ratio in 1990s, Common-Law Countries

ivreg mcapsj (xcon1990sj = logem4) if (sjlouk==1) 

*Column 6: Dependent Variable: Investment-GDP Ratio in 1990s, French Legal Origin Countries

ivreg mcapsj (xcon1990sj = logem4) if (sjlofr==1)

*Column 7: Dependent Variable: Investment-GDP Ratio in 1990s, Without Neo-Europes

ivreg mcapsj (sdformalism xcon1990sj = sjlouk logem4) if (rich4!=1)

*Column 8: Dependent Variable: Investment-GDP Ratio in 1990s, Dropping Outliers

ivreg mcapsj (sdformalism xcon1990sj = sjlouk logem4) if (shortnam~="MYS" & shortnam~="SGP" & shortnam~="ZAF")

