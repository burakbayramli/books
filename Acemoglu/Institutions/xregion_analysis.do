clear
clear matrix
capture log close
set mem 100m

/******************************************************************
All the sources and descriptions of the variables included are explained in
Section 4 of the paper.

People who use the Protestant missionary data should cite:

Woodberry, Robert D. 2012. "The Missionary Roots of Liberal Democracy."
American Political Science Review 106(2): 244-274.

People who use the subnational mission data should also cite:

Woodberry, Robert D. and Juan Carlos Esparza Ochoa. 2006. Project on Religion and Economic Change Website. http://www.prec.com/ 

People using data on Protestant missionaries at the country level should notice that the variable "dummy_dennis" in the xcountry_data.dta file indicates the countries we constructed information on the number of Protestant missionaries from Dennis et al. (1911) and not from Woodberry (2012).

For those interested in additional variables about missions see:
https://nus.academia.edu/RobertWoodberry  &  http://www.prec.com/

******************************************************************/

use xregion_data, clear
	
* Table 9: OLS regressions, cross-region 

set more 1
log using regress_xr.log, replace
outreg2 using xregion, replace keep(yearsed) bdec(3) lab: xi: ivreg2 lgdp years i.bbb , r first
outreg2 using xregion, keep(yearsed) bdec(3) lab: xi: ivreg2 lgdp years i.bbb  if capital_old~=., r first
outreg2 using xregion, keep(yearsed capital_old)  bdec(3) lab: xi: ivreg2 lgdp years capital_old   i.bbb , r first
outreg2 using xregion, keep(yearsed capital_old invdistcoast  invdis2 landlocked )   bdec(3) lab: xi: ivreg2 lgdp years capital_old   invdistc  invdis2 landlocked i.bbb , r first
outreg2 using xregion, keep(yearsed capital_old invdistcoast  invdis2 landlocked temp_avg temp2) bdec(3) lab: xi: ivreg2 lgdp years capital_old   invdistc  invdis2 landlocked temp_ temp2 i.bbb , r first
outreg2 using xregion, see excel keep(yearsed capital_old invdistcoast  invdis2 landlocked temp_avg temp2 lpopd_i) bdec(3) lab: xi: ivreg2 lgdp years capital_old   invdistc  invdis2 landlocked temp_ temp2 lpopd_i i.bbb , r first


* Table 10, Panel A: 2SLS regressions, cross-region
outreg2 using xregion_2sls, replace keep(yearsed )  bdec(3) lab: xi: ivreg2 lgdp (years=miss_presence)   i.bbb , r first
outreg2 using xregion_2sls, keep(yearsed capital_old )  bdec(3) lab: xi: ivreg2 lgdp (years=miss_presence)  capital_old   i.bbb , r first
outreg2 using xregion_2sls, keep(yearsed capital_old invdistcoast  invdis2 landlocked )  bdec(3) lab: xi: ivreg2 lgdp (years=miss_presence)  capital_old    invdistc  invdis2 landlocked i.bbb , r first
outreg2 using xregion_2sls, keep(yearsed capital_old invdistcoast  invdis2 landlocked temp_avg temp2) bdec(3) lab: xi: ivreg2 lgdp (years=miss_presence)   capital_old  invdistc  invdis2 landlocked temp_ temp2  i.bbb , r first
outreg2 using xregion_2sls, see excel keep(yearsed capital_old invdistcoast  invdis2 landlocked temp_avg temp2 lpopd_i) bdec(3) lab: xi: ivreg2 lgdp (years=miss_presence)   capital_old  invdistc  invdis2 landlocked temp_ temp2  lpopd_i i.bbb , r first

* Table 10, Panel B: first stages
outreg2 using xregion_u_f, replace keep(miss_presence )  bdec(3) lab: xi: ivreg2 years miss_presence i.bbb   if lgdp~=., r first
reg years miss_presence i.bbb   if lgdp~=., r 
test miss_presence
outreg2 using xregion_u_f, keep( miss_presence capital_old   ) bdec(3) lab: xi: ivreg2 years miss_presence capital_old   i.bbb    if lgdp~=., r first
reg years miss_presence capital_old   i.bbb    if lgdp~=., r 
test miss_presence
outreg2 using xregion_u_f, keep( miss_presence capital_old invdistcoast  invdis2 landlocked ) bdec(3) lab: xi: ivreg2 years miss_presence capital_old   invdistc  invdis2 landlocked i.bbb    if lgdp~=., r first
reg years miss_presence capital_old   invdistc  invdis2 landlocked i.bbb    if lgdp~=., r 
test miss_presence
outreg2 using xregion_u_f, keep( miss_presence capital_old  invdistcoast  invdis2 landlocked temp_avg temp2) bdec(3) lab: xi: ivreg2 years miss_presence capital_old   invdistc  invdis2 landlocked temp_ temp2 i.bbb    if lgdp~=., r first
reg years miss_presence capital_old   invdistc  invdis2 landlocked temp_ temp2 i.bbb    if lgdp~=., r 
test miss_presence
outreg2 using xregion_u_f, see excel keep( miss_presence capital_old  invdistcoast  invdis2 landlocked temp_avg temp2 lpopd_i) bdec(3) lab: xi: ivreg2 years miss_presence capital_old   invdistc  invdis2 landlocked temp_ temp2 lpopd_i i.bbb    if lgdp~=., r first
reg years miss_presence capital_old   invdistc  invdis2 landlocked temp_ temp2 lpopd_i i.bbb    if lgdp~=., r 
test miss_presence

log close
