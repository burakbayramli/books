***********************************************************
*Creates Table 3: Determinants of Institutions
***********************************************************
clear
capture log close
cd G:\daron\colonial_origins
log using maketable3, replace

/*Data Files Used
	colonial_origins.dta
	
*Data Files Created as Final Product
	none
	
*Data Files Created as Intermediate Product
	none*/
	

use maketable3, clear
keep if excolony==1
keep if extmort4!=.
replace euro1900=euro1900 / 100

*******************
*--Panel A
*******************

*--Dependent Variable Is Average Protection Against Exproptiation Risk in 1985-1995

*col 1
reg  avexpr cons00a if excolony==1 & extmort4~=.
*col 2
reg  avexpr lat_abst cons00a if excolony==1 & extmort4~=.
*col 3
reg  avexpr democ00a if excolony==1 & extmort4~=.
*col 4
reg  avexpr democ00a lat_abst if excolony==1 & extmort4~=.
*col 5
reg  avexpr indtime cons1 if excolony==1 & extmort4~=.
*col 6
reg  avexpr indtime cons1 lat_abst if excolony==1 & extmort4~=.
*col 7
reg  avexpr euro1900 if excolony==1 & extmort4~=.
*col 8
reg  avexpr euro1900 lat_abst if excolony==1 & extmort4~=.
*col 9
reg  avexpr logem4 if excolony==1 & extmort4~=. & logpgp95~=.
*col 10
reg  avexpr logem4 lat_abst if excolony==1 & extmort4~=. & logpgp95~=.

***************
***---Panel B
***************

*col 1
reg cons00a euro1900 if excolony==1 & extmort4~=. & logpgp95~=.
* col 2
reg cons00a euro1900 lat_abst if excolony==1 & extmort4~=. & logpgp95~=.
*col 3
reg  cons00a logem4 if excolony==1 & extmort4~=.
*col 4
reg  cons00a lat_abst logem4 if excolony==1 & extmort4~=.
*col 5
reg democ00a euro1900 if excolony==1 & extmort4~=. & logpgp95~=.
*col 6
reg democ00a lat_abst euro1900 if excolony==1 & extmort4~=. & logpgp95~=.
*col 7
reg democ00a logem4 if excolony==1 & extmort4~=. & logpgp95~=.
*col 8
reg democ00a lat_abst logem4 if excolony==1 & extmort4~=. & logpgp95~=.
*col 9
reg euro1900 logem4 if excolony==1 & extmort4~=. & logpgp95~=.
*col 10
reg euro1900 lat_abst logem4 if excolony==1 & extmort4~=. & logpgp95~=.


