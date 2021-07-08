***********************************************************
*Creates Table 2: Contracting and Property Rights Institutions: GDP per Capita, Investment, Credit, and Stock Market Capitalization
***********************************************************
clear
capture log close
cd G:\daron\unbundling
log using maketable2, replace

/*Data Files Used
	unbundle
	
*Data Files Created as Final Product
	none
	
*Data Files Created as Intermediate Product
	none*/
	

***********************************************************
*Table 2
*note that in this table (unlike our usual tables), each cell reports results from a separate regression
*Panels A and B use the three measures of legal formalism 
*Panels C and D use the three measures of property rights 

***********************************************
use unbundle, clear

*Panel A
*log GDP per capita as dependent variable
*
*instrumenting formalism measures with British legal origin

*ROW 1; using sdformalism; extended index from Simeon Djankov
*col 1: all countries
reg loggdppc1995 sdformalism
*col 2: all former colonies for which have logem4 data
reg loggdppc1995 sdformalism if ex2col==1
*col 3
ivreg loggdppc1995 (sdformalism=sjlouk) if ex2col==1, first

*ROW 2
*using ecproccompindex: procedural complexity (Doing Business 2004)
*col 1: all countries
reg loggdppc1995 ecproccompindex
*col 2: all former colonies for which have legal formalism data
reg loggdppc1995 ecproccompindex if ex2col==1
*col 3
ivreg loggdppc1995 (ecproccompindex=sjlouk) if ex2col==1, first

*ROW 3
*using  ecnumprocedures: number of procedures (Doing Business 2004)
*col 1: all countries
reg loggdppc1995  ecnumprocedures
*col 2: all former colonies for which have legal formalism data
reg loggdppc1995  ecnumprocedures if ex2col==1
*col 3
ivreg loggdppc1995 ( ecnumprocedures=sjlouk) if ex2col==1, first

************************************************
*Panel A, second set of rows (columns 4 through 6)
*Investment/GDP ratio
*avgci1990s
*using sdformalism; extended index from Simeon, just for check
*ROW 1
*col 4: all countries
reg avgci1990s sdformalism if /*yr==1998 &*/ loggdppc1995~=.
*col 5: all former colonies for which have logem4 data
reg avgci1990s sdformalism if ex2col==1 /*& yr==1998*/
*col 6
ivreg avgci1990s (sdformalism=sjlouk) if ex2col==1 /*& yr==1998*/, first
*
*ROW 2
*using ecproccompindex
*col 4: all countries
reg avgci1990s ecproccompindex /*if yr==1998*/
*col 5: all former colonies for which have legal formalism data
reg avgci1990s ecproccompindex if ex2col==1 /*& yr==1998*/
*col 6
ivreg avgci1990s (ecproccompindex=sjlouk) if ex2col==1 /*& yr==1998*/, first
*
*ROW 3
*using ecnumprocedures
*col 4: all countries
reg avgci1990s ecnumprocedures /*if yr==1998*/
*col 5: all former colonies for which have legal formalism data
reg avgci1990s ecnumprocedures if ex2col==1 /*& yr==1998*/
*col 6
ivreg avgci1990s (ecnumprocedures=sjlouk) if ex2col==1 /*& yr==1998*/, first
************************************************
*PRIVATE CREDIT as dependent variable
*instrumenting with British legal origin
*panel B

*using sdformalism; extended index from Simeon Djankov (check measure)
*ROW 1
*col 1: all countries
reg  credittops1998 sdformalism
*col 2: all former colonies for which have logem4 data
reg credittops1998  sdformalism if ex2col==1
*col 3
ivreg credittops1998 (sdformalism=sjlouk) if ex2col==1, first
*
*ROW 2
*using ecproccompindex
*col 1: all countries
reg credittops1998 ecproccompindex
*col 2: all former colonies for which have legal formalism data
reg credittops1998 ecproccompindex if ex2col==1
*col 3
ivreg credittops1998 (ecproccompindex=sjlouk) if ex2col==1, first
*
*ROW 3
*using ecnumprocedures
*col 1: all countries
reg credittops1998 ecnumprocedures
*col 2: all former colonies for which have legal formalism data
reg credittops1998 ecnumprocedures if ex2col==1
*col 3
ivreg credittops1998 (ecnumprocedures=sjlouk) if ex2col==1, first

**********************************************************
*panel B, continued: market capitalization is dependent variable, mcapsj, from Ross Levine
*second set of rows (columns 
*using sdformalism; extended index from Simeon, just for check
*col 4: all countries
reg mcapsj sdformalism
*col 5: all former colonies for which have logem4 data
reg mcapsj sdformalism if ex2col==1
*col 6
ivreg mcapsj (sdformalism=sjlouk) if ex2col==1, first
*
*using ecproccompindex,
*col 4: all countries
reg mcapsj ecproccompindex
*col 5: all former colonies for which have legal formalism data
reg mcapsj ecproccompindex if ex2col==1
*col 6
ivreg mcapsj (ecproccompindex=sjlouk) if ex2col==1, first
*
*using ecnumprocedures
*col 4: all countries
reg mcapsj ecnumprocedures
*col 5: all former colonies for which have legal formalism data
reg mcapsj ecnumprocedures if ex2col==1
*col 6
ivreg mcapsj (ecnumprocedures=sjlouk) if ex2col==1, first
******************************


*Panel C
*log GDP per capita as dependent variable

*ROW 1; using xcon1990sj; constraint on exec, av 1990s
*col 1: all countries
reg loggdppc1995 xcon1990sj
*col 2: all former colonies for which have logem4 data
reg loggdppc1995 xcon1990sj if ex2col==1
*col 3
ivreg loggdppc1995 (xcon1990sj=logem4) if ex2col==1, first

*ROW 2
*using avexp: average protection against expropriation risk
*col 1: all countries
reg loggdppc1995 avexp
*col 2: all former colonies for which have legal formalism data
reg loggdppc1995 avexp if ex2col==1
*col 3
ivreg loggdppc1995 (avexp=logem4) if ex2col==1, first

*ROW 3
*using  efhrpr7: HF: Property Rights '97
*col 1: all countries
reg loggdppc1995  efhrpr7
*col 2: all former colonies for which have legal formalism data
reg loggdppc1995  efhrpr7 if ex2col==1
*col 3
ivreg loggdppc1995 ( efhrpr7=logem4) if ex2col==1, first

************************************************
*Panel C, second set of rows (columns 4 through 6)
*Investment/GDP ratio
*avgci1990s
*using xcon1990sj; constraint on exec, av 1990s, just for check
*ROW 1
*col 4: all countries
reg avgci1990s xcon1990sj if /*yr==1998 &*/ loggdppc1995~=.
*col 5: all former colonies for which have logem4 data
reg avgci1990s xcon1990sj if ex2col==1 /*& yr==1998*/
*col 6
ivreg avgci1990s (xcon1990sj=logem4) if ex2col==1 /*& yr==1998*/, first
*
*ROW 2
*using avexp
*col 4: all countries
reg avgci1990s avexp /*if yr==1998*/
*col 5: all former colonies for which have legal formalism data
reg avgci1990s avexp if ex2col==1 /*& yr==1998*/
*col 6
ivreg avgci1990s (avexp=logem4) if ex2col==1 /*& yr==1998*/, first
*
*ROW 3
*using efhrpr7
*col 4: all countries
reg avgci1990s efhrpr7 /*if yr==1998*/
*col 5: all former colonies for which have legal formalism data
reg avgci1990s efhrpr7 if ex2col==1 /*& yr==1998*/
*col 6
ivreg avgci1990s (efhrpr7=logem4) if ex2col==1 /*& yr==1998*/, first
************************************************
*PRIVATE CREDIT as dependent variable
*Panel D

*using xcon1990sj; constraint on exec, av 1990s (check measure)
*ROW 1
*col 1: all countries
reg  credittops1998 xcon1990sj
*col 2: all former colonies for which have logem4 data
reg credittops1998  xcon1990sj if ex2col==1
*col 3
ivreg credittops1998 (xcon1990sj=logem4) if ex2col==1, first
*
*ROW 2
*using avexp
*col 1: all countries
reg credittops1998 avexp
*col 2: all former colonies for which have legal formalism data
reg credittops1998 avexp if ex2col==1
*col 3
ivreg credittops1998 (avexp=logem4) if ex2col==1, first
*
*ROW 3
*using efhrpr7
*col 1: all countries
reg credittops1998 efhrpr7
*col 2: all former colonies for which have legal formalism data
reg credittops1998 efhrpr7 if ex2col==1
*col 3
ivreg credittops1998 (efhrpr7=logem4) if ex2col==1, first

**********************************************************
*Panel D, continued: market capitalization is dependent variable, mcapsj, from Ross Levine
*second set of rows (columns 
*using xcon1990sj; constraint on exec, av 1990s, just for check
*col 4: all countries
reg mcapsj xcon1990sj
*col 5: all former colonies for which have logem4 data
reg mcapsj xcon1990sj if ex2col==1
*col 6
ivreg mcapsj (xcon1990sj=logem4) if ex2col==1, first
*
*using avexp,
*col 4: all countries
reg mcapsj avexp
*col 5: all former colonies for which have legal formalism data
reg mcapsj avexp if ex2col==1
*col 6
ivreg mcapsj (avexp=logem4) if ex2col==1, first
*
*using efhrpr7
*col 4: all countries
reg mcapsj efhrpr7
*col 5: all former colonies for which have legal formalism data
reg mcapsj efhrpr7 if ex2col==1
*col 6
ivreg mcapsj (efhrpr7=logem4) if ex2col==1, first
******************************

