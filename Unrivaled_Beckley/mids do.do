clear
set more off
use "midparticipants.dta"
rename styear year
merge m:m year ccode using "power1.dta"
drop if dispnum3==.

gen ya=y if sidea==1
gen yb=y if sidea==0
gen gdpa=gdp if sidea==1
gen gdpb=gdp if sidea==0
gen cinca=cinc if sidea==1
gen cincb=cinc if sidea==0
gen state_a=stabb if sidea==1
gen state_b=stabb if sidea==0
collapse (firstnm) state_a (lastnm) state_b (sum) ya yb gdpa gdpb cinca cincb, by (dispnum3)

merge 1:1 dispnum3 using "mids.dta"
drop if ya==0 | yb==0 | gdpa==0 | gdpb==0
drop if outcome>4 | outcome==-9

gen yfrac=ya/(ya+yb)
gen cincfrac=cinca/(cinca+cincb)
gen gdpfrac=gdpa/(gdpa+gdpb)
gen y1=1 if yfrac>.5
replace y1=0 if yfrac<.5
gen gdp1=1 if gdpfrac>.5
replace gdp1=0 if gdpfrac<.5
gen cinc1=1 if cincfrac>.5
replace cinc1=0 if cincfrac<.5

gen win=1 if outcome==1 | outcome==4
replace win=0 if outcome==2 | outcome==3

list dispnum3 styear state_a state_b gdpfrac yfrac if win==1 & y1==1 & gdp1==0
list dispnum3 styear state_a state_b gdpfrac yfrac if win==0 & y1==0 & gdp1==1
list dispnum3 styear state_a state_b gdpfrac yfrac if win==1 & y1==0 & gdp1==1
list dispnum3 styear state_a state_b gdpfrac yfrac if win==0 & y1==1 & gdp1==0
list dispnum3 styear state_a state_b cincfrac yfrac if win==1 & y1==1 & cinc1==0
list dispnum3 styear state_a state_b cincfrac yfrac if win==0 & y1==0 & cinc1==1
list dispnum3 styear state_a state_b cincfrac yfrac if win==0 & y1==1 & cinc1==0
list dispnum3 styear state_a state_b cincfrac yfrac if win==1 & y1==0 & cinc1==1

gen ywl = 1 if win==1 & y1==1 | win==0 & y1==0 
replace ywl=0 if win==1 & y1==0 | win==0 & y1==1 
gen gdpwl = 1 if win==1 & gdp1==1 | win==0 & gdp1==0 
replace gdpwl=0 if win==1 & gdp1==0 | win==0 & gdp1==1 
gen cincwl = 1 if win==1 & cinc1==1 | win==0 & cinc1==0 
replace cincwl=0 if win==1 & cinc1==0 | win==0 & cinc1==1 

sum ywl
sum gdpwl
sum cincwl

*limit to bilateral mids
clear
set more off
use "midparticipants.dta"
sort dispnum3
gen ug=1
egen count=total(ug), by(dispnum3)
keep if count==2

rename styear year
merge m:m year ccode using "power1.dta"
drop if dispnum3==.

gen ya=y if sidea==1
gen yb=y if sidea==0
gen gdpa=gdp if sidea==1
gen gdpb=gdp if sidea==0
gen cinca=cinc if sidea==1
gen cincb=cinc if sidea==0
gen state_a=stabb if sidea==1
gen state_b=stabb if sidea==0
collapse (firstnm) state_a (lastnm) state_b count (sum) ya yb gdpa gdpb cinca cincb, by (dispnum3)

merge 1:1 dispnum3 using "mids.dta"
drop if ya==0 | yb==0 | gdpa==0 | gdpb==0
drop if outcome>4 | outcome==-9
drop if count==.
gen yfrac=ya/(ya+yb)
gen cincfrac=cinca/(cinca+cincb)
gen gdpfrac=gdpa/(gdpa+gdpb)
gen y1=1 if yfrac>.5
replace y1=0 if yfrac<.5
gen gdp1=1 if gdpfrac>.5
replace gdp1=0 if gdpfrac<.5
gen cinc1=1 if cincfrac>.5
replace cinc1=0 if cincfrac<.5

gen win=1 if outcome==1 | outcome==4
replace win=0 if outcome==2 | outcome==3

list dispnum3 styear state_a state_b gdpfrac yfrac if win==1 & y1==1 & gdp1==0
list dispnum3 styear state_a state_b gdpfrac yfrac if win==0 & y1==0 & gdp1==1
list dispnum3 styear state_a state_b gdpfrac yfrac if win==1 & y1==0 & gdp1==1
list dispnum3 styear state_a state_b gdpfrac yfrac if win==0 & y1==1 & gdp1==0
list dispnum3 styear state_a state_b cincfrac yfrac if win==1 & y1==1 & cinc1==0
list dispnum3 styear state_a state_b cincfrac yfrac if win==0 & y1==0 & cinc1==1
list dispnum3 styear state_a state_b cincfrac yfrac if win==0 & y1==1 & cinc1==0
list dispnum3 styear state_a state_b cincfrac yfrac if win==1 & y1==0 & cinc1==1

gen ywl = 1 if win==1 & y1==1 | win==0 & y1==0 
replace ywl=0 if win==1 & y1==0 | win==0 & y1==1 
gen gdpwl = 1 if win==1 & gdp1==1 | win==0 & gdp1==0 
replace gdpwl=0 if win==1 & gdp1==0 | win==0 & gdp1==1 
gen cincwl = 1 if win==1 & cinc1==1 | win==0 & cinc1==0 
replace cincwl=0 if win==1 & cinc1==0 | win==0 & cinc1==1 

sum ywl
sum gdpwl
sum cincwl


