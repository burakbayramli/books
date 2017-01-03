clear 
#delimit ;

/* File to input and format leader data*/
/* Alastair Smith: May 6th 2002 */
/* Data: based originally on BdM and Siverson 1995. This data was cleaned up by Goemans. 
Alastair Smith updated it through 2001 with the research help of Matthew Gross (mainly), Melissa Muscat and Carolyn Zabrycki (Yale undergrads).
leaderdata.dta is list of leaders.
leaderyeardata.dta is leaders years.*/

set mem 100m;
cd c:\bdm2s2\data\basic_datasets\leader_data;
use leaderdata.dta;
note: Data: based originally on BdM and Siverson 1995. This data was cleaned up by Goemans. 
Alastair Smith updated it through 2001 with the research help of Matthew Gross (mainly), 
Melissa Muscat and Carolyn Zabrycki (Yale undergrads);



/* for missing indays and inmonth (mainly in C19) set to 15th day and 6th month etc */ 
gen ind=inday; replace ind=15 if ind==. & inyear~=.; gen inm=inmonth; replace inm=6 if inm==. & inyear~=.;
gen outd=outday; replace outd=15 if outd==. & outyear~=.; gen outm=outmonth; replace outm=6 if outm==. & outyear~=.;
gen indate=mdy(inm,ind,inyear); gen outdate=mdy(outm,outd,outyear);
drop ind inm outd outm; 
/* some (3) leaders are deposed on the day they enter office */ replace outdate=outdate+0.9 if outdate==indate;

sort ccode inyear indate;
/* create leader ID */
by ccode: gen leader_id=(ccode*10000)+100+_n;   /* i.e first leader in country 265 is labeled 2650101 */
qui by ccode: drop if _N==1 & inyear==. ; /* drop countries for which we do not yet have leader data! */
sort ccode leader_id; 
save bdm2s2_basic_leader_list,replace; 


/* Create leaderyears data set */ 
/* earliest inyear is 1763. Make 2001-1763 observations for each leader and keep only the appropriate ones */ 
expand 239; sort leader_id; qui by leader_id: gen year=1762+_n;  
drop if year<inyear; drop if year>outyear & outyear~=.;
sort leader_id year; gen leadgone=0; replace leadgone=1 if year==outyear; 
/* Error for  Bermudez of Peru in 1894. Out before In!! */ drop if outdate<indate; 

 

gen intime = mdy(1, 1,year); 
replace intime=indate if (year==inyear);
gen endtime=mdy(12,31,year);
replace endtime=outdate if outyear==year;
replace intime=intime-1 if  intime==endtime /* problems for (59) leaders entering on Dec 31st */;

stset endtime, id(leader_id) fail(leadgone==1) origin(time intime)  scale(365.25);


sort leader_id year; 

gen time_in_office =(endtime-indate)/365.25; label var time_in_office "total tenure of leader thus far ";
gen year_in_office=year-inyear+1; label var year_in_office "1st, 2nd, 3rd, ... year in office? ";

drop leadid; 
sort ccode year; 
save bdm2s2_leader_year_data,replace; 



