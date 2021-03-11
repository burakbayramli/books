clear
set more off
use "reiterwars.dta"
rename init_ccode ccode
merge m:m year ccode using "power1.dta"
drop if version==.
rename ccode init_ccode
rename y ya
rename gdp gdpa
rename cinc cinca
drop _merge irst milex milper pec tpop upop
renam target_ccode ccode
merge m:m year ccode using "power1.dta"
drop if version==.
rename ccode target_ccode
rename y yb
rename gdp gdpb
rename cinc cincb
drop _merge irst milex milper pec tpop upop
gen yfrac=ya/(ya+yb)
gen cincfrac=cinca/(cinca+cincb)
gen gdpfrac=gdpa/(gdpa+gdpb)
gen y1=1 if yfrac>.5
replace y1=0 if yfrac<.5
gen gdp1=1 if gdpfrac>.5
replace gdp1=0 if gdpfrac<.5
gen cinc1=1 if cincfrac>.5
replace cinc1=0 if cincfrac<.5
gen win=1 if annual_outcome==1
replace win=0 if annual_outcome==2
drop if annual_outcome==0
drop if joiner==1
drop if gdpfrac==. | yfrac==. | cincfrac==. | gdp1==. | y1==. | cinc1==.

gen ywl = 1 if win==1 & y1==1 | win==0 & y1==0 
replace ywl=0 if win==1 & y1==0 | win==0 & y1==1 
gen gdpwl = 1 if win==1 & gdp1==1 | win==0 & gdp1==0 
replace gdpwl=0 if win==1 & gdp1==0 | win==0 & gdp1==1 
gen cincwl = 1 if win==1 & cinc1==1 | win==0 & cinc1==0 
replace cincwl=0 if win==1 & cinc1==0 | win==0 & cinc1==1 
sum ywl
sum gdpwl
sum cincwl

*limit to bilateral wars only
drop if joiner==1
drop if larger_war_name=="WorldWarII" 
keep if adv2ccode==-9 & ally1ccode==-9
sum ywl
sum gdpwl
sum cincwl
list init_war_name gdpfrac yfrac if win==1 & y1==1 & gdp1==0
list init_war_name gdpfrac yfrac if win==0 & y1==0 & gdp1==1
list init_war_name gdpfrac yfrac if win==1 & y1==0 & gdp1==1
list init_war_name gdpfrac yfrac if win==0 & y1==1 & gdp1==0
list init_war_name cincfrac yfrac if win==1 & y1==1 & cinc1==0
list init_war_name cincfrac yfrac if win==0 & y1==0 & cinc1==1
list init_war_name cincfrac yfrac if win==0 & y1==1 & cinc1==0
list init_war_name cincfrac yfrac if win==1 & y1==0 & cinc1==1



