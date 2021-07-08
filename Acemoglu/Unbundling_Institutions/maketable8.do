***********************************************************
*Creates Table 8: Contracting vs. Property Rights Institutions: Other Control Variables
***********************************************************
clear
capture log close
cd G:\daron\unbundling
log using maketable8, replace

/*Data Files Used
	unbundle
	
*Data Files Created as Final Product
	none
	
*Data Files Created as Intermediate Product
	none*/
	
	
use unbundle, clear
keep if ex2col==1

***----Panel A

* Column 1: Dependent Variable: Log GDP per Capita in 1995: Religion

ivreg loggdppc1995 protmg80 muslim80 no_cpm80 (sdformalism xcon1990sj = sjlouk logem4), first
testparm  protmg80 muslim80 no_cpm80 

* Column 2: Dependent Variable: Log GDP per Capita in 1995: Latitude

ivreg loggdppc1995 lat_abst (sdformalism xcon1990sj = sjlouk logem4), first


* Column 3: Dependent Variable: Log GDP per Capita in 1995: Macro

ivreg loggdppc1995 logavgcpinflat7098 averagegovsh5 avgbereale7098 (sdformalism xcon1990sj = sjlouk logem4), first
testparm logavgcpinflat7098 averagegovsh5 avgbereale7098  


* Column 4: Dependent Variable: Investment-GDP Ratio: Religion

ivreg avgci1990s protmg80 muslim80 no_cpm80 (sdformalism xcon1990sj = sjlouk logem4), first
testparm  protmg80 muslim80 no_cpm80

* Column 5: Dependent Variable: Investment-GDP Ratio: Latitude

ivreg avgci1990s lat_abst (sdformalism xcon1990sj = sjlouk logem4), first

* Column 6: Dependent Variable: Investment-GDP Ratio: Macro

ivreg avgci1990s logavgcpinflat7098 averagegovsh5 avgbereale7098 (sdformalism xcon1990sj = sjlouk logem4), first
testparm logavgcpinflat7098 averagegovsh5 avgbereale7098  


***----Panel B


use unbundle, clear
keep if ex2col==1


* Column 1: Dependent Variable: Credit to the Private Sector: Religion

ivreg credittops1998 protmg80 muslim80 no_cpm80 (sdformalism xcon1990sj = sjlouk logem4)
testparm  protmg80 muslim80 no_cpm80 

* Column 2: Dependent Variable: Credit to the Private Sector: Latitude

ivreg credittops1998 lat_abst (sdformalism xcon1990sj = sjlouk logem4)


* Column 3: Dependent Variable: Credit to the Private Sector: Macro

ivreg credittops1998 logavgcpinflat7098 averagegovsh5 avgbereale7098 (sdformalism xcon1990sj = sjlouk logem4)
testparm logavgcpinflat7098 averagegovsh5 avgbereale7098  


* Column 4: Dependent Variable: Stock Market Capitalization: Religion

ivreg mcapsj protmg80 muslim80 no_cpm80 (sdformalism xcon1990sj = sjlouk logem4)
testparm  protmg80 muslim80 no_cpm80

* Column 5: Dependent Variable: Stock Market Capitalization: Latitude

ivreg mcapsj lat_abst (sdformalism xcon1990sj = sjlouk logem4)

* Column 6: Dependent Variable: Stock Market Capitalization: Macro

ivreg mcapsj logavgcpinflat7098 averagegovsh5 avgbereale7098 (sdformalism xcon1990sj = sjlouk logem4)
testparm logavgcpinflat7098 averagegovsh5 avgbereale7098  


