***********************************************************
*Creates Table 2: OLS Regressions
***********************************************************
clear
capture log close
cd G:\daron\colonial_origins
log using maketable2, replace

/*Data Files Used
	maketable2.dta
	
*Data Files Created as Final Product
	none
	
*Data Files Created as Intermediate Product
	none*/
	
***********************
*---Column 1
***********************

*Note: there are 111 countries in the world sample that have all the necessary data to run the below regressions, though the paper only reports 110 observations. I'm not sure which of the 111 observations is not used in the paper, so the regressions below will use all 111 obs. and won't quite match the results reported in the paper. 

use maketable2, clear

regress logpgp95 avexpr, robust


***********************
*---Column 2
***********************

regress logpgp95 avexpr if baseco==1, robust


***********************
*--Column 3
***********************

regress logpgp95 avexpr lat_abst, robust

***********************
*--Column 4
***********************
	
regress logpgp95 avexpr lat_abst africa asia other, robust

***********************
*--Column 5
***********************

regress logpgp95 avexpr lat_abst if baseco==1, robust

***********************
*--Column 6
***********************

regress logpgp95 avexpr lat_abst africa asia other if baseco==1, robust

***********************
*---Column 7
***********************

regress loghjypl avexpr, robust


***********************
*---Column 8
***********************

regress loghjypl avexpr if baseco==1, robust


log close


