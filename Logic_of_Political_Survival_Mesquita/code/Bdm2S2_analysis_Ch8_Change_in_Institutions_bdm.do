/********************************************************************/
/* Bdm2S2: CHAPTER 8 ANALYSIS: Change in Political Institutions */
/* may 22nd 2002: Alastair's modification of Bruce's orginal file */
/* The Unit of Observation is Nation Year */
/********************************************************************/

clear
set mem 50m
#delimit ;
cd c:\bdm2s2\data\May2002; 

use bdm2s2_nation_year_data_may2002 ;

global logfile "output\Bdm2S2_ch7_inst_change.log,replace";
capture log close; 
set matsize 800; set more off;

tsset , clear ;
/* Chapter 7 tests */

log using $logfile; 

reg lgdpWB W S ; predict WSincomeresWB, residual;


gen immigrep=immig~=.; replace immigrep=. if year>1993; 
gen emigrep=emig~=.;   replace emigrep=. if year>1993; 
label var immigrep "Immigration data available (0/1)";
label var emigrep "Emmigration data available (0/1)";

gen emigcost=-1*(emigrep-1);
gen wemigcost=W*emigcost;

sort year;
qui by year: gen simmig=sum(immig);
qui by year: replace simmig=simmig[_N];
qui by year: gen semig=sum(emig);
qui by year: replace semig=semig[_N];
egen mW=mean(W), by(year);
gen emimrat=semig/simmig;
label var emimrat "ratio of worldwide emmigration/ worldwide immigration by year";


gen clcost=CL/7;
gen clcostw=clcost*W;
gen clw=(8-CL-1)*W;

gen IMM=immig/WB_pop; label var IMM "immigration/population by country year";
gen EMM=emig/WB_pop; label var EMM "emmigration/population by country year";
regr demaut W; predict demres, residual;

tsset ccode year;
gen lagGrowthWB = L1.WB_growth; 
gen l3riot=L3.Riots;
gen l3strike=L3.GenStrikes;
gen l3demons=L3.AntiGovDem;
gen dcl30=CL-L3.CL;
gen l3civwar=L3.CL_WAR;
gen L3W=L3.W;
gen F3S=F3.S-S;
gen F3W=F3.W-W;
gen collapse=(F.change==-77); 
replace collapse=. if F.change==.;
label var collapse "Central state Authourity collapses next year";
/* (!77) Authority Collapse: A "-77" code is recorded in the initial year of a collapse of central state authority (i.e., state failure) and
 "!77" for each subsequent year of collapse until a new polity is established or a transition 
(!88) to a new polity or foreign interruption (!66) is initiated. (from POLITY4 codebook)*/

gen CL_war_end = (CL_WAR==0 & L.CL_WAR>0); replace CL_war_end=. if CL_WAR==.;
gen Revol_start=(Revolutions>0 & L1.Revolutions==0); replace Revol_start=. if Revolutions==.|L.Revolutions==.;
gen Revol_end=(Revolutions==0 & L1.Revolutions>0);  replace Revol_end=. if Revolutions==.|L.Revolutions==.;
gen Guerilla_start = (Guerilla>0 &L1.Guerilla==0); replace Guerilla_start =. if Guerilla==. |L1.Guerilla==.;
gen Guerilla_end = (Guerilla==0 &L1.Guerilla>0);  replace Guerilla_end =. if Guerilla==. |L1.Guerilla==.;
tsset, clear;
regr lgdpWB W S;
predict incWB, residual;

gen coup=Coups>=1;
replace coup=0 if Coups==0;
replace coup=. if Coups==.;

des CL_WAR CL_war_start Guerilla ; 

 gen clwar=CL_WAR;
replace clwar=0 if CL_WAR==. & year>=1816 & year~=.;
replace clwar=. if year<1816;
gen revolution=Revolutions>=1;
replace revolution=. if year<1919;
gen guerilla=Guerilla>=1;
replace guerilla=. if year<1919;
replace CL_war_start=0 if clwar==0;
replace CL_war_start=. if year<1816;
replace Guerilla_start=0 if guerilla==0;
replace Guerilla_start=. if year<1919;
replace Revol_start=0 if Revolutions==0;
replace Revol_start=. if year<1919;
gen Revol_startW=Revol_start*W;
gen Revol_endW=Revol_end*W;
gen riot=Riots>=1; replace riot=. if Riots==.;
gen anti=AntiGovDem>=1; replace anti=. if AntiGovDem==.;
gen strike=GenStrikes>=1; replace strike=. if GenStrikes==.;
gen action=riots==1|anti==1|strike==1|Revol_start==1|CL_war_start==1|coup==1|guerilla==1;
gen bigaction=Revol_start==1|CL_war_start==1|guerilla==1|coup==1;

 drop action bigaction;
gen action=riots==1|anti==1|strike==1|Revol_start==1|CL_war_start==1|guerilla==1;
gen bigaction=Revol_start==1|CL_war_start==1|guerilla==1;

gen actsum=riot==1;
replace actsum=actsum+1 if anti==1;
replace actsum=actsum+1 if strike==1;
replace actsum=actsum+1 if Revol_start==1;
replace actsum=actsum+1 if CL_war_start==1;
replace actsum =actsum+1 if guerilla==1;
gen actsumbig=Revol_start==1;
replace actsumbig=actsumbig+1 if CL_war_start==1;
replace actsumbig=actsumbig+1 if guerilla==1;

/* reg lgdpWB W S ; predict WSincomeresWB, residual;*/
gen W_WSincomeresWB=W*WSincomeresWB;
tsset ccode year;
replace F3W=F3.W;
tsset, clear; 
gen DF3W=F3W-W; 
gen UPDOWN=1 if DF3W>=0 &DF3W~=.;
replace UPDOWN = 0 if DF3W<0 &DF3W~=.;
gen f3w=DF3W ;
replace f3w=-1 if DF3W<0; 
replace f3w=1 if DF3W>0;
replace f3w=. if DF3W==.;

gen updown=dw30>=0;
replace updown=. if dw30==.;
replace updown=0 if dw30<0;

gen updown012=updown;
replace updown012=2 if updown==1;
replace updown012=1 if dw30==0;
tsset ccode year;
gen l3act=L3.action;
gen l3actw=L3.action*L3.W;
gen l3actw2=l3actw^2;
gen l3bigact=L3.bigaction;
gen l3bigactw=L3.bigaction*L3.W;
gen l3bigactw2=l3bigactw^2;

gen l3bigactsum=L3.actsumbig; 
gen l3actsum=L3.actsum; 
gen l3actsumW=l3actsum*L3.W;
gen l3bigactsumW=l3bigactsum*L3.W;
gen l3actsum2=l3actsum^2;
gen l3actsumW2=l3actsumW^2;
gen l3bigactsum2=l3bigactsum^2;
gen l3bigactsumW2=l3bigactsumW^2;






gen l3coup=L3.coup;
gen l2coup=L2.coup;
gen l1coup=L1.coup;
gen f1coup=F1.coup;
gen f2coup=F2.coup;
gen fwos30=F3.WoverS-WoverS;
gen fwos20=F2.WoverS-WoverS;
gen lwos30=WoverS-L3.WoverS;
replace nexcap=((Expenditure/100)*WB_gdp95)/WB_pop;
gen fnexcap30=F3.nexcap-nexcap;
gen fnexcap30p=fnexcap30/nexcap;
gen fnexcap20p=(F2.nexcap-nexcap)/nexcap;
gen fnexcap10p=(F1.nexcap-nexcap)/nexcap;
gen fnexcap40p=(F4.nexcap-nexcap)/nexcap;
gen fnexcap50p=(F5.nexcap-nexcap)/nexcap;
tsset, clear;






logit immigrep W lpop_WB lgdpWB , or;
logit emigrep W lpop_WB lgdpWB , or;

xtreg immig W , fe i(regyr);
xtreg immig W lpop_WB lgdpWB, fe i(regyr);
xtreg immig W demres lpop_WB lgdpWB, fe i(regyr);
xtreg immig demaut lpop_WB lgdpWB, fe i(regyr);
xtreg immig lagGrowthWB  W lpop_WB lgdpWB,  fe i(regyr);
xtreg immig lagGrowthWB  W lpop_WB ,  fe i(regyr);
xtreg immig   exchprem W lpop_WB lgdpWB,  fe i(regyr);
xtreg immig lagGrow  exchprem W lpop_WB ,  fe i(regyr);
xtreg immig   Coups W lpop_WB lgdpWB,  fe i(regyr);
xtreg immig   W  s lpop_WB  , fe i(regyr);

des  Assinations GenStrikes Purges GovCrises Riots Revolutions AntiGovDem ConflictIndex Coups HeadofState Guerilla; 
des CL_WAR CL_war_start;


xtreg AntiGovDem   W emigcost wemigcost     , fe i(regyr);
test W+emigcost+wemigcost=0;
test emigcost+wemigcost=0;

xtreg GenStrikes   W emigcost wemigcost   , fe i(regyr);
test W+emigcost+wemigcost=0;
test emigcost+wemigcost=0;

xtreg Riots   W emigcost wemigcost    , fe i(regyr);
test W+emigcost+wemigcost=0;
test emigcost+wemigcost=0;

xtreg AntiGovDem   W clcost clcostw      , fe i(regyr);
test W+clcost+clcostw=0;
test clcost+clcostw=0;

xtreg GenStrikes   W clcost clcostw  , fe i(regyr);
test W+clcost+clcostw=0;
test clcost+clcostw=0;

xtreg Riots   W clcost clcostw  , fe i(regyr);
test W+clcost+clcostw=0;
test clcost+clcostw=0;

xtreg dw30 l3demons l3riot l3strike , fe i(regyr);
xtreg dcl30 l3demons l3riot l3strike , fe i(regyr);

capture program drop stuff;
program define stuff; 
xtlogit $depend dw30  , fe i(regyr);
xtlogit $depend W , fe i(regyr);
xtlogit $depend W s , fe i(regyr);
xtlogit $depend WoverS  , fe i(regyr);
xtlogit $depend W clcost clcostw, fe i(regyr);
xtlogit $depend W emigcost wemigcost, fe i(regyr);
xtlogit $depend W lpop_WB lgdpWB, fe i(regyr);
xtlogit $depend W dw30 , fe i(regyr);
xtlogit $depend dw30 , fe i(regyr); 
end;

global depend "revolution"; stuff;
global depend "guerilla"; stuff;
global depend "clwar"; stuff;
global depend "Revol_start"; stuff; 
global depend "Revol_end";   stuff; 
global depend "Guerilla_start"; stuff; 
global depend "Guerilla_end"; stuff; 
global depend "CL_war_start"; stuff; 
global depend "collapse"; stuff; 


xtlogit revolution W AntiGovDem GenStrikes RIOTS, fe i(regyr);
xtlogit revolution W dw30 AntiGovDem GenStrikes RIOTS, fe i(regyr);
xtlogit revolution W S clcost incWB WB_growth, fe i(regyr) or;
xtlogit guerilla W AntiGovDem GenStrikes RIOTS, fe i(regyr);
xtlogit guerilla W dw30 AntiGovDem GenStrikes RIOTS, fe i(regyr);
xtlogit clwar W AntiGovDem GenStrikes RIOTS, fe i(regyr);
xtlogit clwar W dw30 AntiGovDem GenStrikes RIOTS, fe i(regyr);
xtreg F3S Revol_start Revol_end, fe i(regyr);
xtreg F3S Revol_start Revol_end if F3S~=0, fe i(regyr);
xtreg F3S Revol_start Revol_end  W, fe i(regyr);
xtreg F3S Revol_start Revol_end Revol_startW W, fe i(regyr);
xtreg F3S Revol_start Revol_end Revol_startW Revol_endW W, fe i(regyr);

gen W_WB_gdppc_con=W*WB_gdppc_con;

xtreg actsum W W_WB_gdppc_con WB_gdppc_con , fe i(regyr);
test W+W_WB_gdppc_con+WB_gdppc_con=0;
xtreg actsumbig W W_WB_gdppc_con WB_gdppc_con , fe i(regyr);
test W+W_WB_gdppc_con+WB_gdppc_con=0;

xtlogit action W W_WB_gdppc_con WB_gdppc_con , fe i(regyr);
test W+W_WB_gdppc_con+WB_gdppc_con=0;
xtlogit bigaction W W_WB_gdppc_con WB_gdppc_con , fe i(regyr);
test W+W_WB_gdppc_con+WB_gdppc_con=0;


gen WlgdpWB=W*lgdpWB;
xtlogit action W WlgdpWB lgdpWB , fe i(regyr);
test W+  WlgdpWB +lgdpWB=0;
xtlogit bigaction W WlgdpWB lgdpWB , fe i(regyr);
test W+  WlgdpWB +lgdpWB=0;

xtlogit action W  WSincomeresWB W_WSincomeresWB , fe i(regyr);
test W + WSincomeresWB +W_WSincomeresWB=0;
xtlogit bigaction W  WSincomeresWB W_WSincomeresWB , fe i(regyr);
test W + WSincomeresWB +W_WSincomeresWB=0;

xtreg actsum W WSincomeresWB W_WSincomeresWB , fe i(regyr);
test W+WSincomeresWB+W_WSincomeresWB=0;
xtreg actsumbig W WSincomeresWB W_WSincomeresWB , fe i(regyr);
test W+WSincomeresWB+W_WSincomeresWB=0;


xtlogit updown l3actw l3actw2 L3W l3act, fe i(regyr); 
xtlogit updown l3bigactw l3bigactw2 L3W l3bigact, fe i(regyr);  
ologit updown012 l3actw l3actw2 L3W  l3act, cluster(regyr);
xtreg dw30  l3actw l3actw2 L3W  l3act, fe i(regyr);
xtlogit updown l3bigactw l3bigactw2 L3W l3bigact, fe i(regyr);
ologit updown012 l3bigactw l3bigactw2 L3W  l3bigact, cluster(regyr);
xtreg dw30  l3bigactw l3bigactw2 L3W  l3bigact, fe i(regyr);

gen WorldWar=0;
replace WorldWar=1 if year>=1914 & year<=1921; 
replace WorldWar=1 if year>=1939 &year<=1948;
xtlogit updown l3actw l3actw2 L3W l3act if WorldWar==0, fe i(regyr);

xtlogit updown l3actsum l3actsum2 l3actsumW l3actsumW2 L3W , fe i(regyr);
xtlogit updown l3bigactsum l3bigactsum2 l3bigactsumW l3bigactsumW2 L3W , fe i(regyr);

 

 
xtreg dw30 l3act if L3W==0, fe i(regyr);
xtreg dw30 l3act if L3W==0.25, fe i(regyr);
xtreg dw30 l3act if L3W==0.5, fe i(regyr);
xtreg dw30 l3act if L3W==0.75, fe i(regyr);
xtreg dw30 l3act if L3W==1, fe i(regyr);




sort W;
by W: tab f3w coup if IS_WAR~=1 &lgdpWB~=.  , all; 
 by W: tab f3w coup if IS_WAR~=1   , all; 
sort ccode year;
list ccode longname year IS_WAR if W==1 & coup==1; 
/* list ccode longname year IS_WAR if W==1 & f3w==-1; */
list ccode longname year IS_WAR if W==1 & f3w==-1 & lgdpWB~=.; 


xtlogit coup W , fe i(regyr);
xtlogit coup WoverS , fe i(regyr);
xtlogit coup W lpop_WB WSincomeresWB , fe i(regyr);
xtlogit coup W lpop_WB WSincomeresWB demres , fe i(regyr);

xtreg fwos30 coup, fe i(regyr);
xtreg fwos30 coup lpop_WB lgdpWB , fe i(regyr);
xtreg fwos30 coup lpop_WB lgdpWB demres, fe i(regyr);

xtreg fwos30 W s action if year>=1816 & year~=. , fe i(regyr);
xtreg fwos30 W s action if year>=1816 & year~=. & WoverS<1, fe i(regyr);
xtreg fwos30 W s if action==0 & year>=1816 & year~=. , fe i(regyr);
xtreg fwos30 W s if action==0 & year>=1816 & year~=. & WoverS<1, fe i(regyr);



xtreg fnexcap30p l1coup l2coup l3coup , fe i(regyr);
xtreg fnexcap30p l1coup l2coup l3coup WoverS, fe i(regyr);
xtreg fnexcap30p l1coup l2coup l3coup WoverS lpop_WB lgdpWB demres, fe i(regyr);
xtreg fnexcap30p f1coup f2coup coup WoverS lpop_WB lgdpWB demres, fe i(regyr);
xtreg fnexcap30p f1coup f2coup coup , fe i(regyr);
xtreg fnexcap30p f1coup  coup , fe i(regyr);
xtreg fnexcap30p f1coup  coup WoverS lpop_WB lgdpWB demres, fe i(regyr);

xtreg fnexcap30p l1coup l2coup coup WoverS lpop_WB lgdpWB demres, fe i(regyr);
gen coupfwos30=coup*fwos30;
xtreg fnexcap50p coup coupfwos30 fwos30 lpop_WB lgdpWB demres  ,  fe i(regyr); 
xtreg fnexcap50p coup coupfwos30 fwos30 lpop_WB lgdpWB demres WoverS ,  fe i(regyr); 



/****** Human rights abuses ******/
xtreg ainew W , fe i(regyr);
xtreg ainew  WoverS  lpop WSincomeresWB , fe i(regyr);
xtreg ainew W   lpop WSincomeresWB , fe i(regyr);
xtreg ainew W   lpop WSincomeresWB demres, fe i(regyr);

xtreg sdnew W , fe i(regyr);
xtreg sdnew  WoverS  lpop WSincomeresWB , fe i(regyr);
xtreg sdnew W   lpop WSincomeresWB , fe i(regyr);
xtreg sdnew W   lpop WSincomeresWB demres, fe i(regyr);

tsset ccode year;
gen lagainew=L.ainew; 
gen lagsdnew=L.sdnew;
tsset,clear; 

xtreg ainew W lagainew, fe i(regyr);
xtreg ainew  WoverS  lpop WSincomeresWB lagainew, fe i(regyr);
xtreg ainew W   lpop WSincomeresWB lagainew, fe i(regyr);
xtreg ainew W   lpop WSincomeresWB demres lagainew, fe i(regyr);

xtreg sdnew W lagsdnew, fe i(regyr);
xtreg sdnew  WoverS  lpop WSincomeresWB lagsdnew, fe i(regyr);
xtreg sdnew W   lpop WSincomeresWB lagsdnew, fe i(regyr);
xtreg sdnew W   lpop WSincomeresWB demres lagsdnew, fe i(regyr);
