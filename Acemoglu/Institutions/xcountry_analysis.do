clear 
clear matrix
set mem 100m
set more 1
capture log close


/******************************************************************
All the sources and descriptions of the variables included are explained in Section 4 of the paper.

People who use the Protestant missionary data should cite:

Woodberry, Robert D. 2012. "The Missionary Roots of Liberal Democracy." American Political Science Review 106(2): 244-274.

People who use the subnational mission data should also cite:
Woodberry, Robert D. and Juan Carlos Esparza Ochoa. 2006. Project on Religion and Economic Change Website. http://www.prec.com/ 

People using data on Protestant missionaries at the country level should notice that the variable "dummy_dennis" in the xcountry_data.dta file indicates the countries we constructed information on the number of Protestant missionaries from Dennis et al. (1911) and not from Woodberry (2012).

For those interested in additional variables about missions see:
https://nus.academia.edu/RobertWoodberry  &  http://www.prec.com/

The data set presented below includes a correction for Hong Kong (the dummy_dennis is equal to 1, as the data for Hong Kong was taken from Dennis et al., 1914), therefore some of the results of the regressions below are not exactly the same as in the published paper. 

In addition, users can experiment with two alternative definitions of Protestant missionaries for the countries taken from Dennis et al. 1914.

Alternative Definition 1, including as Protestant missionaries to all foreign missionaries (including both ordained and un-ordained people), this is closer to the definition used by Woodberry (2012). In this case users should run the following Stata commands:

replace protmiss=0.1097143 if code=="AUS"
replace protmiss=0.4702282 if code=="CAN"
replace protmiss=0.1011323 if code=="HKG"
replace protmiss=0.209569 if code=="NZL"
replace protmiss=0.0530361 if code=="USA"

Alternative Definition 2, including as Protestant missionaries to all foreign and native missionaries (including both ordained and un-ordained people). In this case users should run the following Stata commands:

replace protmiss=0.1988751 if code=="AUS"
replace protmiss=0.8611575 if code=="CAN"
replace protmiss=0.3928906 if code=="HKG"
replace protmiss=2.354067 if code=="NZL"
replace protmiss=0.1037007 if code=="USA"

It is worth noting that the main conclusions of the paper do not change when using these alternative definitions.

******************************************************************/

use xcountry_data.dta

*Table 2: OLS regressions, cross country


log using ols.log, replace
outreg2 using  ols, replace bdec(3) lab:  ivreg2 logpgdp05 tyr05_n if tyr05_n~=., first r liml
outreg2 using  ols, bdec(3) lab:  ivreg2 logpgdp05 ruleof if tyr05_n~=., first r liml
outreg2 using  ols, bdec(3) lab:  ivreg2 logpgdp05 ruleof tyr05_n if tyr05_n~=., first r liml
outreg2 using  ols, bdec(3) lab:  ivreg2 logpgdp05 tyr05_n lat_abs if tyr05_n~=., first r liml
outreg2 using  ols, bdec(3) lab:  ivreg2 logpgdp05 ruleof lat_abs if tyr05_n~=., first r liml
outreg2 using  ols, bdec(3) lab:  ivreg2 logpgdp05 ruleof tyr05_n lat_abs if tyr05_n~=., first r liml
outreg2 using  ols, bdec(3) lab:  ivreg2 logpgdp05 tyr05_n lat_abs africa america asia if tyr05_n~=., first r liml
outreg2 using  ols, bdec(3) lab:  ivreg2 logpgdp05 ruleof lat_abs africa america asia if tyr05_n~=., first r liml
outreg2 using  ols, bdec(3) lab:  ivreg2 logpgdp05 ruleof tyr05_n lat_abs africa america asia if tyr05_n~=., first r liml
outreg2 using  ols, bdec(3) lab:  ivreg2 logpgdp05 tyr05_n lat_abs africa america asia f_bri f_fren  if tyr05_n~=., first r liml
outreg2 using  ols, bdec(3) lab:  ivreg2 logpgdp05 ruleof lat_abs africa america asia f_bri f_fren  if tyr05_n~=., first r liml
outreg2 using  ols, see excel bdec(3) lab:  ivreg2 logpgdp05 ruleof tyr05_n lat_abs africa america asia f_bri f_fren  if tyr05_n~=., first r liml
log close


*Table 3: Falsification Exercise, Protestant Missionaries, cross country
outreg2 using  falsif, replace bdec(3) lab: reg  prienr1870 protmiss dummy_dennis if tyr05_n~=.  , r  
outreg2 using  falsif, bdec(3) lab: reg  prienr1870 protmiss dummy_dennis lat_abs if tyr05_n~=.  , r  
outreg2 using  falsif, bdec(3) lab: reg  prienr1870 protmiss dummy_dennis lat_abs asia africa america     if tyr05_n~=.  , r  
outreg2 using  falsif, bdec(3) lab: reg  prienr1870 protmiss dummy_dennis asia africa america  lat_abs   f_fr f_br if tyr05_n~=.  , r  
outreg2 using  falsif, bdec(3) lab: reg  prienr1940 protmiss dummy_dennis if tyr05_n~=.   , r  
outreg2 using  falsif, bdec(3) lab: reg  prienr1940 protmiss dummy_dennis  lat_abs if tyr05_n~=.   , r  
outreg2 using  falsif, bdec(3) lab: reg  prienr1940 protmiss dummy_dennis asia africa america lat_abs  if tyr05_n~=.   , r  
outreg2 using  falsif, bdec(3) lab: reg  prienr1940 protmiss dummy_dennis asia africa america lat_abs   f_br f_fr if tyr05_n~=.   , r 
outreg2 using  falsif, bdec(3) lab: reg  tyr05_n protmiss dummy_dennis if tyr05_n~=.   & Yrsm<90 & protmiss~=0, r  
outreg2 using  falsif, bdec(3) lab: reg  tyr05_n protmiss dummy_dennis  lat_abs if tyr05_n~=.   & Yrsm<90 & protmiss~=0, r  
outreg2 using  falsif, bdec(3) lab: reg  tyr05_n protmiss dummy_dennis asia africa america lat_abs  if tyr05_n~=.   & Yrsm<90 & protmiss~=0, r  
outreg2 using  falsif, see excel bdec(3) lab: reg  tyr05_n protmiss dummy_dennis asia africa america lat_abs   f_br f_fr if tyr05_n~=.  & Yrsm<90 & protmiss~=0, r  

*Table 4, Panel A: Year of Schooling, Semi-structural regressions, 2sls, cross-country
* Semi-structural
set more 1
log using new_b.log, replace
outreg2 using  new_b, replace bdec(3) lab:  ivreg2 logpgdp05 (tyr05_n =  prienr1900 protmiss ) dummy_denn   if  tyr05_n~=.  , first r 
outreg2 using  new_b, bdec(3) lab:  ivreg2 logpgdp05 (tyr05_n =  prienr1900 protmiss ) dummy_denn   lat_abs if  tyr05_n~=.  , first r 
outreg2 using  new_b, bdec(3) lab:  ivreg2 logpgdp05 (tyr05_n =  prienr1900 protmiss ) dummy_denn   lat_abs africa america asia  if  tyr05_n~=.  , first r 
outreg2 using  new_b, bdec(3) lab:  ivreg2 logpgdp05 (tyr05_n =  prienr1900 protmiss ) dummy_denn   lat_abs africa america asia f_fr f_br if  tyr05_n~=.  , first r 
outreg2 using  new_b, bdec(3) lab:  ivreg2 logpgdp05 (tyr05_n =  prienr1900 protmiss ) dummy_denn   lcapped lpd1500 if  tyr05_n~=.  , first r 
outreg2 using  new_b, bdec(3) lab:  ivreg2 logpgdp05 (tyr05_n =  prienr1900 protmiss ) dummy_denn   lcapped lpd1500 lat_abs if  tyr05_n~=.  , first r 
outreg2 using  new_b, bdec(3) lab:  ivreg2 logpgdp05 (tyr05_n =  prienr1900 protmiss ) dummy_denn   lcapped lpd1500 lat_abs africa america asia  if  tyr05_n~=.  , first r 
outreg2 using  new_b, bdec(3) lab:  ivreg2 logpgdp05 (tyr05_n =  prienr1900 protmiss ) dummy_denn   lcapped lpd1500 lat_abs africa america asia f_fr f_br  if  tyr05_n~=.  , first r 
outreg2 using  new_b, bdec(3) lab:  ivreg2 logpgdp05 (tyr05_n =  prienr1900 protmiss ) dummy_denn   lcapped lpd1500 if  tyr05_n~=.  , first r liml
outreg2 using  new_b, bdec(3) lab:  ivreg2 logpgdp05 (tyr05_n =  prienr1900 protmiss ) dummy_denn   lcapped lpd1500 lat_abs if  tyr05_n~=.  , first r liml
outreg2 using  new_b, bdec(3) lab:  ivreg2 logpgdp05 (tyr05_n =  prienr1900 protmiss ) dummy_denn   lcapped lpd1500 lat_abs africa america asia  if  tyr05_n~=.  , first r liml
outreg2 using  new_b, see excel bdec(3) lab:  ivreg2 logpgdp05 (tyr05_n =  prienr1900 protmiss ) dummy_denn   lcapped lpd1500 lat_abs africa america asia f_fr f_br  if  tyr05_n~=.  , first r liml


log close



*Table 4: Panel B: First stage regressions
set more 1
log using new_ib, replace
outreg2 using  new_ib, replace bdec(3) lab:  reg tyr05_n prienr1900 protmiss dummy_denn   if  tyr05_n~=.  ,  r 
reg tyr05_n  prienr1900 protmiss dummy_denn   if  tyr05_n~=.  ,  r 
test prienr1900 protmiss 
outreg2 using  new_ib, bdec(3) lab:  reg tyr05_n  prienr1900 protmiss dummy_denn   lat_abs if  tyr05_n~=.  , r 
reg tyr05_n  prienr1900 protmiss dummy_denn   lat_abs if  tyr05_n~=.  ,  r 
test prienr1900 protmiss 
outreg2 using  new_ib, bdec(3) lab:  reg tyr05_n  prienr1900 protmiss dummy_denn   lat_abs africa america asia if  tyr05_n~=.  , r 
reg tyr05_n  prienr1900 protmiss dummy_denn   lat_abs africa america asia if  tyr05_n~=.  ,  r 
test prienr1900 protmiss 
outreg2 using  new_ib, bdec(3) lab:  reg tyr05_n  prienr1900 protmiss dummy_denn   lat_abs africa america asia f_br f_fr if  tyr05_n~=.  , r 
reg tyr05_n  prienr1900 protmiss dummy_denn   lat_abs africa america asia f_br f_fr if  tyr05_n~=.  ,  r 
test prienr1900 protmiss 
outreg2 using  new_ib, bdec(3) lab:  reg tyr05_n  prienr1900 protmiss dummy_denn   lcapped lpd1500 if  tyr05_n~=.  ,  r 
reg tyr05_n  prienr1900 protmiss dummy_denn   lcapped lpd1500 if  tyr05_n~=.  ,  r 
test prienr1900 protmiss 
outreg2 using  new_ib, bdec(3) lab:  reg tyr05_n  prienr1900 protmiss dummy_denn   lat_abs lcapped lpd1500 if  tyr05_n~=.  , r 
reg tyr05_n  prienr1900 protmiss dummy_denn   lat_abs lcapped lpd1500 if  tyr05_n~=.  ,  r 
test prienr1900 protmiss 
outreg2 using  new_ib, bdec(3) lab:  reg tyr05_n  prienr1900 protmiss dummy_denn   lat_abs africa america asia lcapped lpd1500 if  tyr05_n~=.  , r 
reg tyr05_n  prienr1900 protmiss dummy_denn   lat_abs africa america asia lcapped lpd1500 if  tyr05_n~=.  ,  r 
test prienr1900 protmiss 
outreg2 using  new_ib, see excel bdec(3) lab:  reg tyr05_n  prienr1900 protmiss dummy_denn   lat_abs africa america asia f_br f_fr lcapped lpd1500 if  tyr05_n~=.  , r 
reg tyr05_n  prienr1900 protmiss dummy_denn   lat_abs africa america asia f_br f_fr lcapped lpd1500 if  tyr05_n~=.  ,  r 
test prienr1900 protmiss 
**Columns 9 to 12 are the same as columns 5 to 8
log close



*Table 5, Panel A: Rule of Law, Semi-structural regressions, 2sls, cross-country
* Semi-structural
set more 1
log using new_rule.log, replace
outreg2 using  new_rule, replace bdec(3) lab:  ivreg2 logpgdp05 (ruleof=  lcapped lpd1500)   if  tyr05_n~=.  , first r 
outreg2 using  new_rule, bdec(3) lab:  ivreg2 logpgdp05 (ruleof=  lcapped lpd1500)   lat_abs if  tyr05_n~=.  , first r 
outreg2 using  new_rule, bdec(3) lab:  ivreg2 logpgdp05 (ruleof=  lcapped lpd1500)   lat_abs africa america asia  if  tyr05_n~=.  , first r 
outreg2 using  new_rule, bdec(3) lab:  ivreg2 logpgdp05 (ruleof=  lcapped lpd1500)   lat_abs africa america asia f_fr f_br if  tyr05_n~=.  , first r 
outreg2 using  new_rule, bdec(3) lab:  ivreg2 logpgdp05 (ruleof=  lcapped lpd1500) dummy_den  prienr1900 protmiss if  tyr05_n~=.  , first r 
outreg2 using  new_rule, bdec(3) lab:  ivreg2 logpgdp05 (ruleof=  lcapped lpd1500) dummy_den  prienr1900 protmiss lat_abs if  tyr05_n~=.  , first r 
outreg2 using  new_rule, bdec(3) lab:  ivreg2 logpgdp05 (ruleof=  lcapped lpd1500) dummy_den  prienr1900 protmiss lat_abs africa america asia  if  tyr05_n~=.  , first r 
outreg2 using  new_rule, bdec(3) lab:  ivreg2 logpgdp05 (ruleof=  lcapped lpd1500) dummy_den  prienr1900 protmiss lat_abs africa america asia f_fr f_br  if  tyr05_n~=.  , first r 
outreg2 using  new_rule, bdec(3) lab:  ivreg2 logpgdp05 (ruleof=  lcapped lpd1500) dummy_den  prienr1900 protmiss if  tyr05_n~=.  , first r liml
outreg2 using  new_rule, bdec(3) lab:  ivreg2 logpgdp05 (ruleof=  lcapped lpd1500) dummy_den  prienr1900 protmiss lat_abs if  tyr05_n~=.  , first r liml
outreg2 using  new_rule, bdec(3) lab:  ivreg2 logpgdp05 (ruleof=  lcapped lpd1500) dummy_den  prienr1900 protmiss lat_abs africa america asia  if  tyr05_n~=.  , first r liml
outreg2 using  new_rule, see excel bdec(3) lab:  ivreg2 logpgdp05 (ruleof=  lcapped lpd1500) dummy_den  prienr1900 protmiss lat_abs africa america asia f_fr f_br  if  tyr05_n~=.  , first r liml


log close



*Table 5: Panel B: Rule of Law, First stage regressions, 
set more 1
log using new_rule_i, replace
outreg2 using  new_rule_i, replace bdec(3) lab:  reg ruleof lcapped lpd1500 if  tyr05_n~=.  ,  r 
reg ruleof lcapped lpd1500  if  tyr05_n~=.  ,  r 
test lcapped lpd1500  
outreg2 using  new_rule_i, bdec(3) lab:  reg ruleof lcapped lpd1500  lat_abs if  tyr05_n~=.  , r 
reg ruleof lcapped lpd1500 lat_abs if  tyr05_n~=.  ,  r 
test lcapped lpd1500  
outreg2 using  new_rule_i, bdec(3) lab:  reg ruleof lcapped lpd1500  lat_abs africa america asia if  tyr05_n~=.  , r 
reg  ruleof lcapped lpd1500  lat_abs africa america asia if  tyr05_n~=.  ,  r 
test lcapped lpd1500  
outreg2 using  new_rule_i, bdec(3) lab:  reg ruleof lcapped lpd1500  lat_abs africa america asia f_br f_fr if  tyr05_n~=.  , r 
reg ruleof lcapped lpd1500  lat_abs africa america asia f_br f_fr if  tyr05_n~=.  ,  r 
test lcapped lpd1500  
outreg2 using  new_rule_i, bdec(3) lab:  reg ruleof lcapped lpd1500  prienr1900 protmiss dummy_den  if  tyr05_n~=.  ,  r 
reg ruleof lcapped lpd1500  prienr1900 protmiss dummy_den  if  tyr05_n~=.  ,  r 
test lcapped lpd1500  
outreg2 using  new_rule_i, bdec(3) lab:  reg ruleof lcapped lpd1500  prienr1900 protmiss dummy_den  lat_abs if  tyr05_n~=.  , r 
reg ruleof lcapped lpd1500  prienr1900 protmiss dummy_den  lat_abs if  tyr05_n~=.  ,  r 
test lcapped lpd1500  
outreg2 using  new_rule_i, bdec(3) lab:  reg ruleof lcapped lpd1500  prienr1900 protmiss dummy_den  lat_abs africa america asia if  tyr05_n~=.  , r 
reg ruleof lcapped lpd1500  prienr1900 protmiss dummy_den  lat_abs africa america asia if  tyr05_n~=.  ,  r 
test lcapped lpd1500  
outreg2 using  new_rule_i, see excel bdec(3) lab:  reg ruleof lcapped lpd1500  prienr1900 protmiss dummy_den  lat_abs africa america asia f_br f_fr if  tyr05_n~=.  , r 
reg ruleof lcapped lpd1500  prienr1900 protmiss dummy_den  lat_abs africa america asia f_br f_fr if  tyr05_n~=.  ,  r 
test lcapped lpd1500  
**Columns 9 to 12 are the same as columns 5 to 8
log close



*Table 6: Panel A: 2SLS and LIML Estimates
set more 1
log using 2sls_b.log, replace
outreg2 using  2sls, replace bdec(3) lab:  ivreg2 logpgdp05 (tyr05_n ruleof= prienr1900 protmiss lcapped lpd1500 ) dummy_den  if  tyr05_n~=.  , first r 
outreg2 using  2sls, bdec(3) lab:  ivreg2 logpgdp05 (tyr05_n ruleof= prienr1900 protmiss lcapped lpd1500 ) dummy_den  lat_abs if  tyr05_n~=.  , first r 
outreg2 using  2sls, bdec(3) lab:  ivreg2 logpgdp05 (tyr05_n ruleof= prienr1900 protmiss lcapped lpd1500 ) dummy_den  lat_abs africa america asia   if  tyr05_n~=.  , first r 
outreg2 using  2sls,  bdec(3) lab:  ivreg2 logpgdp05 (tyr05_n ruleof= prienr1900 protmiss lcapped lpd1500 ) dummy_den  lat_abs africa america asia  f_br f_fr   if  tyr05_n~=.  , first r 
outreg2 using  2sls, bdec(3) lab:  ivreg2 logpgdp05 (tyr05_n ruleof= prienr1900 protmiss lcapped lpd1500 ) dummy_den  if  tyr05_n~=.  , first r liml
outreg2 using  2sls, bdec(3) lab:  ivreg2 logpgdp05 (tyr05_n ruleof= prienr1900 protmiss lcapped lpd1500 ) dummy_den  lat_abs if  tyr05_n~=.  , first liml r 
outreg2 using  2sls, bdec(3) lab:  ivreg2 logpgdp05 (tyr05_n ruleof= prienr1900 protmiss lcapped lpd1500 ) dummy_den  lat_abs africa america asia   if  tyr05_n~=.  , first r liml 
outreg2 using  2sls,  see excel bdec(3) lab:  ivreg2 logpgdp05 (tyr05_n ruleof= prienr1900 protmiss lcapped lpd1500 ) dummy_den  lat_abs africa america asia  f_br f_fr   if  tyr05_n~=.  , first r liml
log close


*Table B: First stages
set more 1
log using 2sls_f.log, replace
outreg2 using  2sls_f, replace bdec(3) lab:  reg tyr05_n prienr1900 protmiss lcapped lpd1500 dummy_den   if  tyr05_n~=.  , r 

reg tyr05_n prienr1900 protmiss lcapped lpd1500 dummy_den   if  tyr05_n~=.  , r 

test prienr1900 protmiss 

test lcapped lpd1500 
outreg2 using  2sls_f, bdec(3) lab:  reg tyr05_n prienr1900 protmiss lcapped lpd1500 dummy_den   lat_abs if  tyr05_n~=.  , r 
reg tyr05_n prienr1900 protmiss lcapped lpd1500 dummy_den   lat_abs if  tyr05_n~=.  , r 
test prienr1900 protmiss 
test lcapped lpd1500 
outreg2 using  2sls_f, bdec(3) lab:  reg tyr05_n prienr1900 protmiss lcapped lpd1500 dummy_den   lat_abs america asia africa if  tyr05_n~=.  , r 
reg tyr05_n prienr1900 protmiss lcapped lpd1500 dummy_den   lat_abs america asia africa if  tyr05_n~=.  , r 
test prienr1900 protmiss 
test lcapped lpd1500 
outreg2 using  2sls_f, bdec(3) lab:  reg tyr05_n prienr1900 protmiss lcapped lpd1500 dummy_den   lat_abs america asia africa f_fr f_br if  tyr05_n~=.  , r 
reg tyr05_n prienr1900 protmiss lcapped lpd1500 dummy_den   lat_abs america asia africa f_fr f_br if  tyr05_n~=.  , r 
test prienr1900 protmiss 
test lcapped lpd1500 
outreg2 using  2sls_f, bdec(3) lab:  reg ruleof prienr1900 protmiss lcapped lpd1500 dummy_den   if  tyr05_n~=.  , r 
reg ruleof prienr1900 protmiss lcapped lpd1500 dummy_den   if  tyr05_n~=.  , r 
test prienr1900 protmiss 
test lcapped lpd1500 
outreg2 using  2sls_f,  bdec(3) lab:  reg ruleof  prienr1900 protmiss lcapped lpd1500 dummy_den   lat_abs if  tyr05_n~=.  , r 
reg ruleof  prienr1900 protmiss lcapped lpd1500 dummy_den   lat_abs if  tyr05_n~=.  , r 
test prienr1900 protmiss 
test lcapped lpd1500 
outreg2 using  2sls_f,  bdec(3) lab:  reg ruleof  prienr1900 protmiss lcapped lpd1500 dummy_den   lat_abs america asia africa if  tyr05_n~=.  , r 
reg ruleof  prienr1900 protmiss lcapped lpd1500 dummy_den   lat_abs america asia africa if  tyr05_n~=.  , r 
test prienr1900 protmiss 
test lcapped lpd1500 
outreg2 using  2sls_f, see excel  bdec(3) lab:  reg ruleof  prienr1900 protmiss lcapped lpd1500 dummy_den   lat_abs america asia africa f_fr f_br if  tyr05_n~=.  , r 
reg ruleof  prienr1900 protmiss lcapped lpd1500 dummy_den   lat_abs america asia africa f_fr f_br if  tyr05_n~=.  , r 
test prienr1900 protmiss 
test lcapped lpd1500 
log close


*Table 7: Robustness exercises
set more 1
log using 2sls_b_neo.log, replace
outreg2 using  2sls_b_neo,  replace bdec(3) lab:  ivreg2 logpgdp05 (tyr05_n ruleof=  prienr1900 protmiss lcapped lpd1500) dummy_den  lat_abs africa america asia    if  neoe==0 &  tyr05_n~=.  , first liml r
outreg2 using  2sls_b_neo,  bdec(3) lab:  ivreg2 logpgdp05 (tyr05_n ruleof=  prienr1900 protmiss lcapped lpd1500) dummy_den  lat_abs africa america asia  f_br f_fr   if  neoe==0 &  tyr05_n~=.  , first liml r
outreg2 using  2sls_b_neo,  bdec(3) lab:  ivreg2 logpgdp05 (tyr05_n ruleof=  prienr1900 protmiss lcapped lpd1500) dummy_den  lat_abs africa america asia    malfal94 if   tyr05_n~=.  , first liml r
outreg2 using  2sls_b_neo,  bdec(3) lab:  ivreg2 logpgdp05 (tyr05_n ruleof=  prienr1900 protmiss lcapped lpd1500) dummy_den  lat_abs africa america asia   f_br f_fr  malfal94 if   tyr05_n~=.  , first liml r
outreg2 using  2sls_b_neo, bdec(3) lab:  ivreg2 logpgdp05 (tyr05_n ruleof=  prienr1900 protmiss lcapped lpd1500) dummy_den  lat_abs africa america asia   temp* humid* if  tyr05_n~=.  , first r liml
outreg2 using  2sls_b_neo,  bdec(3) lab:  ivreg2 logpgdp05 (tyr05_n ruleof=  prienr1900 protmiss lcapped lpd1500) dummy_den  lat_abs africa america asia  f_br f_fr   temp* humid* if  tyr05_n~=.  , first r liml
outreg2 using  2sls_b_neo, bdec(3) lab:  ivreg2 logpgdp05 (tyr05_n ruleof=  prienr1900 protmiss lcapped lpd1500) dummy_den  lat_abs africa america asia cath1900 prot1900 musl1900 if  tyr05_n~=.  , first liml r
outreg2 using  2sls_b_neo,  see excel bdec(3) lab:  ivreg2 logpgdp05 (tyr05_n ruleof=  prienr1900 protmiss lcapped lpd1500) dummy_den  lat_abs africa america asia  f_br f_fr   cath1900 prot1900 musl1900  if  tyr05_n~=.  , first liml r
log close

*Table 8, Panel A: Year of Schooling, Semi-structural regressions, 2sls, cross-country
* Semi-structural
set more 1
log using inst_edu.log, replace
outreg2 using  inst_edu, replace bdec(3) lab:  ivreg2 ruleoflaw (tyr05_n =  prienr1900 protmiss) dummy_den if  tyr05_n~=.  , first r 
outreg2 using  inst_edu, bdec(3) lab:  ivreg2 ruleoflaw (tyr05_n =  prienr1900 protmiss) dummy_den lat_abs if  tyr05_n~=.  , first r 
outreg2 using  inst_edu, bdec(3) lab:  ivreg2 ruleoflaw (tyr05_n =  prienr1900 protmiss) dummy_den lat_abs africa america asia  if  tyr05_n~=.  , first r 
outreg2 using  inst_edu, bdec(3) lab:  ivreg2 ruleoflaw (tyr05_n =  prienr1900 protmiss) dummy_den lat_abs africa america asia f_fr f_br if  tyr05_n~=.  , first r 
outreg2 using  inst_edu, bdec(3) lab:  ivreg2 ruleoflaw (tyr05_n =  prienr1900 protmiss) dummy_den lcapped lpd1500 if  tyr05_n~=.  , first r 
outreg2 using  inst_edu, bdec(3) lab:  ivreg2 ruleoflaw (tyr05_n =  prienr1900 protmiss) dummy_den lcapped lpd1500 lat_abs if  tyr05_n~=.  , first r 
outreg2 using  inst_edu, bdec(3) lab:  ivreg2 ruleoflaw (tyr05_n =  prienr1900 protmiss) dummy_den lcapped lpd1500 lat_abs africa america asia  if  tyr05_n~=.  , first r 
outreg2 using  inst_edu, bdec(3) lab:  ivreg2 ruleoflaw (tyr05_n =  prienr1900 protmiss) dummy_den lcapped lpd1500 lat_abs africa america asia f_fr f_br  if  tyr05_n~=.  , first r 
outreg2 using  inst_edu, bdec(3) lab:  ivreg2 ruleoflaw (tyr05_n =  prienr1900 protmiss) dummy_den lcapped lpd1500 if  tyr05_n~=.  , first r liml
outreg2 using  inst_edu, bdec(3) lab:  ivreg2 ruleoflaw (tyr05_n =  prienr1900 protmiss) dummy_den lcapped lpd1500 lat_abs if  tyr05_n~=.  , first r liml
outreg2 using  inst_edu, bdec(3) lab:  ivreg2 ruleoflaw (tyr05_n =  prienr1900 protmiss) dummy_den lcapped lpd1500 lat_abs africa america asia  if  tyr05_n~=.  , first r liml
outreg2 using  inst_edu, see excel bdec(3) lab:  ivreg2 ruleoflaw (tyr05_n =  prienr1900 protmiss) dummy_den lcapped lpd1500 lat_abs africa america asia f_fr f_br  if  tyr05_n~=.  , first r liml


log close

*Table Appendix 1 in Online appendix
set more 1
log using 2sls_b.log, replace
outreg2 using  2sls_id, replace bdec(3) lab:  ivreg2 logpgdp05 (tyr05_n ruleof=  protmiss lcapped ) dummy_de if  tyr05_n~=.  , first r 
outreg2 using  2sls_id, bdec(3) lab:  ivreg2 logpgdp05 (tyr05_n ruleof=  protmiss lcapped) dummy_den  lat_abs if  tyr05_n~=.  , first r 
outreg2 using  2sls_id, bdec(3) lab:  ivreg2 logpgdp05 (tyr05_n ruleof=  protmiss lcapped) dummy_den  lat_abs africa america asia   if  tyr05_n~=.  , first r 
outreg2 using  2sls_id,  see excel bdec(3) lab:  ivreg2 logpgdp05 (tyr05_n ruleof=  protmiss lcapped) dummy_den  lat_abs africa america asia  f_br f_fr   if  tyr05_n~=.  , first r 

outreg2 using  2sls_id_f, replace bdec(3) lab:  ivreg2 tyr05_n protmiss lcapped dummy_den if  tyr05_n~=.  , r 
outreg2 using  2sls_id_f, bdec(3) lab:  ivreg2 tyr05_n protmiss lcapped dummy_den lat_abs if  tyr05_n~=.  , first r 
outreg2 using  2sls_id_f, bdec(3) lab:  ivreg2 tyr05_n protmiss lcapped dummy_den lat_abs africa america asia   if  tyr05_n~=.  , first r 
outreg2 using  2sls_id_f,  bdec(3) lab:  ivreg2 tyr05_n protmiss lcapped dummy_den lat_abs africa america asia  f_br f_fr   if  tyr05_n~=.  , first r 
outreg2 using  2sls_id_f, bdec(3) lab:  ivreg2 ruleof protmiss lcapped dummy_den if  tyr05_n~=.  , r 
outreg2 using  2sls_id_f, bdec(3) lab:  ivreg2 ruleof protmiss lcapped dummy_den lat_abs if  tyr05_n~=.  , first r 
outreg2 using  2sls_id_f, bdec(3) lab:  ivreg2 ruleof  protmiss lcapped dummy_den lat_abs africa america asia   if  tyr05_n~=.  , first r 
outreg2 using  2sls_id_f,  see excel bdec(3) lab:  ivreg2 ruleof  protmiss lcapped dummy_den lat_abs africa america asia  f_br f_fr   if  tyr05_n~=.  , first r 

reg tyr05_n protmiss lcapped dummy_den if  tyr05_n~=.  , r 
test protmiss 
test lcapped

reg tyr05_n protmiss lcapped dummy_den lat_abs if  tyr05_n~=.  , r 
test protmiss 
test lcapped

reg tyr05_n protmiss lcapped dummy_den lat_abs africa america asia   if  tyr05_n~=.  , r 
test protmiss 
test lcapped

reg tyr05_n protmiss lcapped dummy_den lat_abs africa america asia  f_br f_fr   if  tyr05_n~=.  , r 
test protmiss 
test lcapped

reg ruleof protmiss lcapped dummy_den if  tyr05_n~=.  , r 
test protmiss 
test lcapped

reg ruleof protmiss lcapped dummy_den lat_abs if  tyr05_n~=.  , r 
test protmiss 
test lcapped

reg ruleof  protmiss lcapped dummy_den lat_abs africa america asia   if  tyr05_n~=.  , r 
test protmiss 
test lcapped

reg ruleof  protmiss lcapped dummy_den lat_abs africa america asia  f_br f_fr   if  tyr05_n~=.  , r 
test protmiss 
test lcapped

log close
