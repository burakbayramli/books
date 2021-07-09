***********************************************************
*Creates Table 1: Summary Statistics 
***********************************************************
clear
capture log close
cd G:\daron\colonial_origins
log using maketable1, replace

/*Data Files Used
	maketable1.dta
	
*Data Files Created as Final Product
	none
	
*Data Files Created as Intermediate Product
	none*/
	

****************************
*---column 1 (whole world)
****************************

use maketable1, clear

*log GDP per capita, PPP, in 1995
summ logpgp95

*log output per worker in 1988
summ loghjypl

*Average protection against expropriation risk, 1985-1995
summ avexpr 

*Constraint on executive, 1900
summ cons00a

*Constraint on executive in first year of independence
summ cons1

*Democracy in 1900
summ democ00a

*European settlement in 1900
summ euro1900 


****************************
*---column 2 (base sample)
****************************

use maketable1, clear

keep if baseco==1

*log GDP per capita, PPP, in 1995
summ logpgp95

*log output per worker in 1988
summ loghjypl

*Average protection against expropriation risk, 1985-1995
summ avexpr 

*Constraint on executive, 1900
summ cons00a

*Constraint on executive in first year of independence
summ cons1

*Democracy in 1900
summ democ00a

*European settlement in 1900
summ euro1900 

*Log European settler mortality
summ logem4


****************************
*---columns 3-6 (quartiles)
****************************


*log GDP per capita, PPP, in 1995
sum logpgp95 if excolony==1 & extmort4~=. & avexpr~=. & logpgp95~=. & extmort4<65.4 & extmort4~=.
sum logpgp95 if excolony==1 & extmort4~=. & avexpr~=. & logpgp95~=. & extmort4>=65.4 & extmort4<78.1
sum logpgp95 if excolony==1 & extmort4~=. & avexpr~=. & logpgp95~=. & extmort4>=78.1 & extmort4<280
sum logpgp95 if excolony==1 & extmort4~=. & avexpr~=. & logpgp95~=. & extmort4>=280 & extmort4~=.

*log output per worker in 1988
sum loghjypl if excolony==1 & extmort4~=. & avexpr~=. & logpgp95~=. & extmort4<65.4 & extmort4~=.
sum loghjypl if excolony==1 & extmort4~=. & avexpr~=. & logpgp95~=. & extmort4>=65.4 & extmort4<78.1
sum loghjypl if excolony==1 & extmort4~=. & avexpr~=. & logpgp95~=. & extmort4>=78.1 & extmort4<280
sum loghjypl if excolony==1 & extmort4~=. & avexpr~=. & logpgp95~=. & extmort4>=280 & extmort4~=.

*Average protection against expropriation risk, 1985-1995
sum avexpr if excolony==1 & extmort4~=. & avexpr~=. & logpgp95~=. & extmort4<65.4 & extmort4~=.
sum avexpr if excolony==1 & extmort4~=. & avexpr~=. & logpgp95~=. & extmort4>=65.4 & extmort4<78.1
sum avexpr if excolony==1 & extmort4~=. & avexpr~=. & logpgp95~=. & extmort4>=78.1 & extmort4<280
sum avexpr if excolony==1 & extmort4~=. & avexpr~=. & logpgp95~=. & extmort4>=280 & extmort4~=.

*Constraint on executive, 1900
sum cons00a if excolony==1 & extmort4~=. & avexpr~=. & logpgp95~=. & extmort4<65.4 & extmort4~=.
sum cons00a if excolony==1 & extmort4~=. & avexpr~=. & logpgp95~=. & extmort4>=65.4 & extmort4<78.1
sum cons00a if excolony==1 & extmort4~=. & avexpr~=. & logpgp95~=. & extmort4>=78.1 & extmort4<280
sum cons00a if excolony==1 & extmort4~=. & avexpr~=. & logpgp95~=. & extmort4>=280 & extmort4~=.

*Constraint on executive in 1990
sum cons90 if excolony==1 & extmort4~=. & avexpr~=. & logpgp95~=. & extmort4<65.4 & extmort4~=.
sum cons90 if excolony==1 & extmort4~=. & avexpr~=. & logpgp95~=. & extmort4>=65.4 & extmort4<78.1
sum cons90 if excolony==1 & extmort4~=. & avexpr~=. & logpgp95~=. & extmort4>=78.1 & extmort4<280
sum cons90 if excolony==1 & extmort4~=. & avexpr~=. & logpgp95~=. & extmort4>=280 & extmort4~=.

*Constraint on executive in first year of independence
sum cons1 if excolony==1 & extmort4~=. & avexpr~=. & logpgp95~=. & extmort4<65.4 & extmort4~=.
sum cons1 if excolony==1 & extmort4~=. & avexpr~=. & logpgp95~=. & extmort4>=65.4 & extmort4<78.1
sum cons1 if excolony==1 & extmort4~=. & avexpr~=. & logpgp95~=. & extmort4>=78.1 & extmort4<280
sum cons1 if excolony==1 & extmort4~=. & avexpr~=. & logpgp95~=. & extmort4>=280 & extmort4~=.

*Democracy in 1900
sum democ00a if excolony==1 & extmort4~=. & avexpr~=. & logpgp95~=. & extmort4<65.4 & extmort4~=.
sum democ00a if excolony==1 & extmort4~=. & avexpr~=. & logpgp95~=. & extmort4>=65.4 & extmort4<78.1
sum democ00a if excolony==1 & extmort4~=. & avexpr~=. & logpgp95~=. & extmort4>=78.1 & extmort4<280
sum democ00a if excolony==1 & extmort4~=. & avexpr~=. & logpgp95~=. & extmort4>=280 & extmort4~=.

*European settlement in 1900
sum euro1900 if excolony==1 & extmort4~=. & avexpr~=. & logpgp95~=. & extmort4<65.4 & extmort4~=.
sum euro1900 if excolony==1 & extmort4~=. & avexpr~=. & logpgp95~=. & extmort4>=65.4 & extmort4<78.1
sum euro1900 if excolony==1 & extmort4~=. & avexpr~=. & logpgp95~=. & extmort4>=78.1 & extmort4<280
sum euro1900 if excolony==1 & extmort4~=. & avexpr~=. & logpgp95~=. & extmort4>=280 & extmort4~=.

*Log European settler mortality
sum logem4 if excolony==1 & extmort4~=. & avexpr~=. & logpgp95~=. & extmort4<65.4 & extmort4~=.
sum logem4 if excolony==1 & extmort4~=. & avexpr~=. & logpgp95~=. & extmort4>=65.4 & extmort4<78.1
sum logem4 if excolony==1 & extmort4~=. & avexpr~=. & logpgp95~=. & extmort4>=78.1 & extmort4<280
sum logem4 if excolony==1 & extmort4~=. & avexpr~=. & logpgp95~=. & extmort4>=280 & extmort4~=.

capture log close

