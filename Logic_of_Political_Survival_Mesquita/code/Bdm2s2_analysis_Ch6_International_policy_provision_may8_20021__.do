/********************************************************************/
/* Bdm2S2: CHAPTER 5 ANALYSIS: International Outcomes *****/
/* 8/5/2002: Alastair's modification of Bruce's orginal file ********/
/* The Unit of Observation is Nation Year *************************/
/*********************************************************************/
clear
set mem 50m
#delimit ;
cd c:\bdm2s2\data\May2002; 

use bdm2s2_nation_year_data_may2002 ;

global logfile "output\Bdm2S2_ch5_International_policy.log,replace";
capture log close; 
set matsize 800; set more off;

tsset , clear ;


/* merge on MID crisis data  such that we can tell the strength of the opponent etc.*/ 
sort ccode year; 
merge ccode year using datasets\BdM2S2_MIDonly_.dta ; 
tab _m;  drop _m;
capture drop nationyear;
gen nationyear=ccode*10000+year; 
sort nationyear; 
sort nationyear hostlevA capB  fataldis ccodeB  ; 
qui by nationyear: keep if _n==_N; /* keep the highest level disputed, coded by hostility level, opponent's capabilkity and fatality*/



/*********************************************************/
/******* MILITARY EXPENDITURE  RECODES*********/
/*********************************************************/ 
des milex  defenseovertotal;
sum milex  defenseovertotal;

qui gen lmilex=log(milex); label var lmilex "log(military expenditure): COW data";
tsset ccode year;
gen laglmilex=L.lmilex; label var laglmilex "lagged log(military expenditure): COW data";
gen lagmilex=L1.milex;
qui gen dmil=100*(milex-L.milex)/L.milex;   /**** This is recoded ****/
label var dmil "% change in military expenditure";
qui gen demlag=L.demaut;
gen changeTOTspending = (NatGovExpend-L. NatGovExpend)/ L.NatGovExpend ;
sort ccode year;
des  is_war_start IS_WAR number_of_ISwars; 
replace IS_WAR=0 if IS_WAR==. & year>=1816&year<=1997; 
replace is_war_start=0 if is_war_start==. & year>=1816&year<=1997; 
replace number_of_ISwars=0 if number_of_ISwars==. & year>=1816&year<=1997; 
gen secondyearofwar=(IS_WAR==1& L.is_war_start==1);
label var secondyearofwar "An Interstate war started last year and continues this year";
gen is_war_startW=is_war_start*lagW; gen secondyearofwarW=secondyearofwar*lagW; 
replace EX_WAR=0 if EX_WAR==. & year>=1816&year<=1997; 

gen war_lagW=IS_WAR*lagW;
gen hostlevW=lagW*hostlevA; 
gen war_lagD=IS_WAR*demlag;
gen EX_WARW=EX_WAR*lagW;
gen EX_WARD=EX_WAR*demlag;
gen warcapabilityRatio=0;
replace warcapabilityRatio=capabilityRatio if IS_WAR==1;
gen warcapabilityRatioW=warcapabilityRatio*lagW;
gen warcapabilityRatioW2=warcapabilityRatioW^2;
gen warcapabilityRatio2=warcapabilityRatio^2;
gen highprob=(warcapabilityRatio>.95);
gen highprobW=highprob*lagW;
gen contest=(warcapabilityRatio>.2 & warcapabilityRatio<0.8);
gen contestW=contest*lagW;
gen contest3070=(warcapabilityRatio>.3 & warcapabilityRatio<0.7);
gen contest3070W=contest3070*lagW;
gen even=1-(2*abs(.5-warcapabilityRatio));
gen evenW=even*lagW;
gen evenD=even*demlag;
gen logenergycap=log(energy+1/tpop);


gen highcrisis=(hostlevA>=4);
gen highcrisisW=highcrisis*W;

gen lmilper=log(milper+1);
tsset ccode year;
gen laglmilper=L1.lmilper;
gen lagmilper=L1.milper;
regr demaut W S;
predict demres, residual;
gen lagdemres=L1.demres;
tsset, clear; 
gen europe=ccode>199&ccode<400;
gen usa=ccode==2;
gen post20=year>=1920 & year~=.;
tsset ccode year; 
gen milpertroop=(milex+1)/(milper+1);
gen lmilpertroop=log(milpertroop);
gen lagmilpertroop=L1.milpertroop;
gen laglmilpertroop=L1.lmilpertroop;
gen dmilpertroop=(milpertroop-L1.milpertroop)/(L1.milpertroop);
gen lagdmilpertroop=L1.dmilpertroop;
gen lpopA=log(tpop*1000);
tsset, clear;
 log using $logfile; 
/***********************************************************************************************/
/************ ANALYSIS***********************************************************************/
/***********************************************************************************************/
global fe "regyr";  

xtlogit EX_WAR demaut, fe i($fe);
xtlogit EX_WAR W , fe i($fe);
xtlogit EX_WAR W demres, fe i($fe);

global model1 "lagW IS_WAR war_lagW ";
global model2 "lagW IS_WAR war_lagW  even evenW ";
global model3 "lagW EX_WAR EX_WARW ";
global model4 "demlag IS_WAR war_lagD ";
global model5 "demlag IS_WAR war_lagD  even evenD lagdemres";
global model6 "demlag EX_WAR EX_WARD";

heckman  lmilex laglmilex lagW IS_WAR war_lagW lpopA  
logenergycap , select(year europe usa  );
test IS_WAR+war_lagW=0;
test IS_WAR war_lagW;
test IS_WAR;

heckman  lmilex laglmilex demlag IS_WAR war_lagD lpopA  
logenergycap , select(year europe usa  );
test IS_WAR+war_lagD=0;
test IS_WAR war_lagD;
test IS_WAR;

heckman  lmilex laglmilex lagW IS_WAR war_lagW even evenW lpopA, select(year europe usa  );
test IS_WAR+war_lagW=0;
test IS_WAR war_lagW;
test IS_WAR;
test IS_WAR war_lagW even evenW;
test IS_WAR +war_lagW +even +evenW = 0;
test IS_WAR +war_lagW +even +evenW =IS_WAR+even;


heckman  lmilex laglmilex lagW IS_WAR war_lagW even evenW lpopA  
logenergycap , select(year europe usa  );
test IS_WAR+war_lagW=0;
test IS_WAR war_lagW;
test IS_WAR;
test IS_WAR war_lagW even evenW;
test IS_WAR +war_lagW +even +evenW = 0;
test IS_WAR +war_lagW +even +evenW =IS_WAR+even;

heckman  lmilex laglmilex demlag IS_WAR war_lagD even evenD lpopA , select(year europe usa  );
test IS_WAR+war_lagD=0;
test IS_WAR war_lagD;
test IS_WAR;
test IS_WAR war_lagD even evenD;
test IS_WAR +war_lagD +even +evenD = 0;
test IS_WAR +war_lagD +even +evenD =IS_WAR+even;


heckman  lmilex laglmilex demlag IS_WAR war_lagD even evenD lpopA  
logenergycap , select(year europe usa  );
test IS_WAR+war_lagD=0;
test IS_WAR war_lagD;
test IS_WAR;
test IS_WAR war_lagD even evenD;
test IS_WAR +war_lagD +even +evenD = 0;
test IS_WAR +war_lagD +even +evenD =IS_WAR+even;

heckman  lmilex laglmilex lagW EX_WAR EX_WARW lpopA  
logenergycap , select(year europe usa  );
test EX_WAR+EX_WARW=0;
test EX_WAR EX_WARW;
test EX_WAR;


heckman  lmilex laglmilex lagW EX_WAR EX_WARD lpopA  
logenergycap , select(year europe usa  );
test EX_WAR+EX_WARD=0;
test EX_WAR EX_WARD;
test EX_WAR;

heckman  lmilper laglmilper lagW EX_WAR EX_WARD lpopA , select(year europe usa  );
test EX_WAR+EX_WARD=0;
test EX_WAR EX_WARD;
test EX_WAR;

heckman  lmilper laglmilper lagW EX_WAR EX_WARD lpopA  
logenergycap , select(year europe usa  );
test EX_WAR+EX_WARD=0;
test EX_WAR EX_WARD;
test EX_WAR;

heckman  lmilper laglmilper lagW EX_WAR EX_WARW lpopA , select(year europe usa  );
test EX_WAR+EX_WARW=0;
test EX_WAR EX_WARW;
test EX_WAR;

heckman  lmilper laglmilper lagW EX_WAR EX_WARW lpopA  
logenergycap , select(year europe usa  );
test EX_WAR+EX_WARW=0;
test EX_WAR EX_WARW;
test EX_WAR;

heckman  lmilper laglmilper lagW IS_WAR war_lagW lpopA , select(year europe usa  );
test IS_WAR+war_lagW=0;
test IS_WAR war_lagW;
test IS_WAR;

heckman  lmilper laglmilper lagW IS_WAR war_lagW lpopA  
logenergycap , select(year europe usa  );
test IS_WAR+war_lagW=0;
test IS_WAR war_lagW;
test IS_WAR;

heckman  lmilper laglmilper demlag IS_WAR war_lagD lpopA , select(year europe usa  );
test IS_WAR+war_lagD=0;
test IS_WAR war_lagD;
test IS_WAR;

heckman  lmilper laglmilper demlag IS_WAR war_lagD lpopA  
logenergycap , select(year europe usa  );
test IS_WAR+war_lagD=0;
test IS_WAR war_lagD;
test IS_WAR;

heckman  lmilper laglmilper lagW IS_WAR war_lagW even evenW lpopA , select(year europe usa  );
test IS_WAR+war_lagW=0;
test IS_WAR war_lagW;
test IS_WAR;
test IS_WAR war_lagW even evenW;
test IS_WAR +war_lagW +even +evenW = 0;
test IS_WAR +war_lagW +even +evenW =IS_WAR+even;

heckman  lmilper laglmilper lagW IS_WAR war_lagW even evenW lpopA  
logenergycap , select(year europe usa  );
test IS_WAR+war_lagW=0;
test IS_WAR war_lagW;
test IS_WAR;
test IS_WAR war_lagW even evenW;
test IS_WAR +war_lagW +even +evenW = 0;
test IS_WAR +war_lagW +even +evenW =IS_WAR+even;

heckman  lmilper laglmilper demlag IS_WAR war_lagD even evenD lpopA , select(year europe usa  );
test IS_WAR+war_lagD=0;
test IS_WAR war_lagD;
test IS_WAR;
test IS_WAR war_lagD even evenD;
test IS_WAR +war_lagD +even +evenD = 0;
test IS_WAR +war_lagD +even +evenD =IS_WAR+even;

heckman  lmilper laglmilper demlag IS_WAR war_lagD even evenD lpopA  
logenergycap , select(year europe usa  );
test IS_WAR+war_lagD=0;
test IS_WAR war_lagD;
test IS_WAR;
test IS_WAR war_lagD even evenD;
test IS_WAR +war_lagD +even +evenD = 0;
test IS_WAR +war_lagD +even +evenD =IS_WAR+even;

xtreg lmilex laglmilex $model1 , fe i($fe);
test IS_WAR + war_lagW = 0;
test IS_WAR war_lagW  ;
test IS_WAR;
test IS_WAR=war_lagW;

xtreg lmilper laglmilper $model1  , fe i($fe);
test IS_WAR + war_lagW = 0;
test IS_WAR war_lagW ;
test IS_WAR;
test IS_WAR=war_lagW;

xtreg lmilex laglmilex $model2  , fe i($fe);
test IS_WAR + war_lagW + even + evenW = 0;
test IS_WAR war_lagW even evenW ;
test IS_WAR even;
test IS_WAR=war_lagW;
test even=evenW;
test IS_WAR+even=war_lagW+evenW;

xtreg lmilper laglmilper $model2 , fe i($fe) ;
test IS_WAR + war_lagW + even + evenW = 0;
test IS_WAR war_lagW even evenW ;
test IS_WAR even;
test IS_WAR=war_lagW;
test even=evenW;
test IS_WAR+even=war_lagW+evenW;

xtreg lmilex laglmilex $model3 , fe i($fe);
test EX_WAR + EX_WARW = 0;
test EX_WAR EX_WARW  ;
test EX_WAR;
test EX_WAR=EX_WARW;

xtreg lmilper laglmilper $model3 , fe i($fe);
test EX_WAR + EX_WARW = 0;
test EX_WAR EX_WARW  ;
test EX_WAR;
test EX_WAR=EX_WARW;

xtreg lmilex laglmilex $model4 , fe i($fe);
test IS_WAR + war_lagD = 0;
test IS_WAR war_lagD  ;
test IS_WAR;
test IS_WAR=war_lagD;

xtreg lmilper laglmilper $model4  , fe i($fe);
test IS_WAR + war_lagD = 0;
test IS_WAR war_lagD ;
test IS_WAR;
test IS_WAR=war_lagD;

xtreg lmilex laglmilex $model5  , fe i($fe);
test IS_WAR + war_lagD + even + evenD = 0;
test IS_WAR war_lagD even evenD ;
test IS_WAR even;
test IS_WAR=war_lagD;
test even=evenD;
test IS_WAR+even=war_lagD+evenD;

xtreg lmilper laglmilper $model5 , fe i($fe) ;
test IS_WAR + war_lagD + even + evenD = 0;
test IS_WAR war_lagD even evenD ;
test IS_WAR even;
test IS_WAR=war_lagD;
test even=evenD;
test IS_WAR+even=war_lagD+evenD;

xtreg lmilex laglmilex $model6 , fe i($fe);
test EX_WAR + EX_WARD = 0;
test EX_WAR EX_WARD  ;
test EX_WAR;
test EX_WAR=EX_WARD;

xtreg lmilper laglmilper $model6 , fe i($fe);
test EX_WAR + EX_WARD = 0;
test EX_WAR EX_WARD  ;
test EX_WAR;
test EX_WAR=EX_WARD;





/* xtreg lmilpertroop laglmilpertroop lagW is_war_start  is_war_startW , fe i($fe);
test IS_WAR+war_lagW=0;

xtreg lmilpertroop laglmilpertroop lagW is_war_start  is_war_startW contest3070 contest3070W, fe i($fe);
test IS_WAR+war_lagW=0;
test contest3070 + contest3070W=0;
xtreg lmilpertroop laglmilpertroop lagW is_war_start  is_war_startW highprob highprobW, fe i($fe);
test IS_WAR+war_lagW=0;
test highprob+highprobW=0;

xtreg lmilpertroop laglmilpertroop lagW is_war_start  is_war_startW contest contestW, fe i($fe);
test IS_WAR+war_lagW=0;
test contest+contestW=0;


xtreg dmilpertroop lagdmilpertroop lagW is_war_start  is_war_startW , fe i($fe);
test IS_WAR+war_lagW=0;


xtreg dmilpertroop lagdmilpertroop lagW is_war_start  is_war_startW contest3070 contest3070W  , fe i($fe);
test IS_WAR+war_lagW=0;
test contest3070 + contest3070W=0;

xtreg dmilpertroop lagdmilpertroop lagW is_war_start  is_war_startW highprob highprobW, fe i($fe);
test IS_WAR+war_lagW=0;
test highprob+highprobW=0;

xtreg dmilpertroop lagdmilpertroop lagW is_war_start  is_war_startW contest contestW, fe i($fe);
test IS_WAR+war_lagW=0;
test contest+contestW=0;


xtreg lmilex laglmilex  lagW is_war_start  is_war_startW  lpop_WB laglgdpWB, fe i($fe);
test IS_WAR +war_lagW=0;

xtreg lmilex laglmilex  lagW is_war_start  is_war_startW , fe i($fe);
test IS_WAR +war_lagW=0;

xtreg lmilex laglmilex  lagW is_war_start  is_war_startW   warcapabilityRatio warcapabilityRatio2 , fe i($fe);
test IS_WAR +war_lagW=0;
test warcapabilityRatio  warcapabilityRatio2  ;  /* nations try hardest when war is 50:50 */

xtreg lmilex laglmilex  lagW is_war_start  is_war_startW   warcapabilityRatio warcapabilityRatioW 
							warcapabilityRatio2 warcapabilityRatioW2, fe i($fe);
test IS_WAR +war_lagW=0;
test warcapabilityRatio warcapabilityRatioW warcapabilityRatio2 warcapabilityRatioW2;

xtreg lmilex laglmilex  lagW is_war_start  is_war_startW   highprob highprobW,  fe i($fe);
test IS_WAR +war_lagW=0;
test highprob+highprobW=0;

xtreg lmilex laglmilex  lagW is_war_start  is_war_startW  contest contestW,  fe i($fe);
test IS_WAR +war_lagW=0;
test contest +contestW=0;

xtreg lmilex laglmilex  lagW is_war_start  is_war_startW  contest3070 contest3070W,  fe i($fe);
test IS_WAR +war_lagW=0;
test contest3070+ contest3070W=0;


/* Look for escalation of effort in first and second years of the war (once full effort is being made subsequent war years should see no more effort)*/

xtreg lmilex laglmilex  lagW is_war_start is_war_startW secondyearofwar secondyearofwarW, fe i($fe);
test is_war_start+ is_war_startW=0; test secondyearofwar+ secondyearofwarW=0; 
test is_war_start+ is_war_startW+ secondyearofwar+ secondyearofwarW=0;

xtreg lmilex laglmilex  lagW is_war_start is_war_startW secondyearofwar secondyearofwarW
					warcapabilityRatio warcapabilityRatio2 , fe i($fe);
test is_war_start+ is_war_startW=0; test secondyearofwar+ secondyearofwarW=0; 
test is_war_start+ is_war_startW+ secondyearofwar+ secondyearofwarW=0;
test warcapabilityRatio +warcapabilityRatio2 =0;


xtreg lmilex laglmilex  lagW is_war_start is_war_startW secondyearofwar secondyearofwarW
			warcapabilityRatio warcapabilityRatio2 warcapabilityRatioW warcapabilityRatioW2 , fe i($fe);
test is_war_start+ is_war_startW=0; test secondyearofwar+ secondyearofwarW=0; 
test is_war_start+ is_war_startW+ secondyearofwar+ secondyearofwarW=0;
test warcapabilityRatio +warcapabilityRatio2 =0;
test warcapabilityRatio+ warcapabilityRatio2 +warcapabilityRatioW +warcapabilityRatioW2=0;


/* percentage change in military expenditure */


xtreg dmil  lagW is_war_start  is_war_startW  lpop_WB laglgdpWB, fe i($fe);
test IS_WAR +war_lagW=0;

xtreg dmil lagW is_war_start  is_war_startW , fe i($fe);
test IS_WAR +war_lagW=0;

xtreg dmil lagW is_war_start  is_war_startW   warcapabilityRatio warcapabilityRatio2 , fe i($fe);
test IS_WAR +war_lagW=0;
test warcapabilityRatio  warcapabilityRatio2  ;  /* nations try hardest when war is 50:50 */

xtreg dmil  lagW is_war_start  is_war_startW   warcapabilityRatio warcapabilityRatioW 
							warcapabilityRatio2 warcapabilityRatioW2, fe i($fe);
test IS_WAR +war_lagW=0;
test warcapabilityRatio warcapabilityRatioW warcapabilityRatio2 warcapabilityRatioW2;

xtreg dmil  lagW is_war_start  is_war_startW   highprob highprobW,  fe i($fe);
test IS_WAR +war_lagW=0;
test highprob+highprobW=0;

xtreg dmil  lagW is_war_start  is_war_startW  contest contestW,  fe i($fe);
test IS_WAR +war_lagW=0;
test contest +contestW=0;

xtreg dmil lagW is_war_start  is_war_startW  contest3070 contest3070W,  fe i($fe);
test IS_WAR +war_lagW=0;
test contest3070+ contest3070W=0;


/* Look for escalation of effort in first and second years of the war (once full effort is being made subsequent war years should see no more effort)*/

xtreg dmil lagW is_war_start is_war_startW secondyearofwar secondyearofwarW, fe i($fe);
test is_war_start+ is_war_startW=0; test secondyearofwar+ secondyearofwarW=0; 
test is_war_start+ is_war_startW+ secondyearofwar+ secondyearofwarW=0;

xtreg dmil lagW is_war_start is_war_startW secondyearofwar secondyearofwarW
					warcapabilityRatio warcapabilityRatio2 , fe i($fe);
test is_war_start+ is_war_startW=0; test secondyearofwar+ secondyearofwarW=0; 
test is_war_start+ is_war_startW+ secondyearofwar+ secondyearofwarW=0;
test warcapabilityRatio +warcapabilityRatio2 =0;


xtreg dmil  lagW is_war_start is_war_startW secondyearofwar secondyearofwarW
			warcapabilityRatio warcapabilityRatio2 warcapabilityRatioW warcapabilityRatioW2 , fe i($fe);
test is_war_start+ is_war_startW=0; test secondyearofwar+ secondyearofwarW=0; 
test is_war_start+ is_war_startW+ secondyearofwar+ secondyearofwarW=0;
test warcapabilityRatio +warcapabilityRatio2 =0;
test warcapabilityRatio+ warcapabilityRatio2 +warcapabilityRatioW +warcapabilityRatioW2=0;




/**********************************************************/
/**** Percentage change in TOTAL expenditure with war******/
/**********************************************************/

xtreg changeTOTspending  lagW is_war_start  is_war_startW  lpop_WB laglgdpWB, fe i($fe);
test IS_WAR +war_lagW=0;

xtreg changeTOTspending lagW is_war_start  is_war_startW , fe i($fe);
test IS_WAR +war_lagW=0;

xtreg changeTOTspending lagW is_war_start  is_war_startW   warcapabilityRatio warcapabilityRatio2 , fe i($fe);
test IS_WAR +war_lagW=0;
test warcapabilityRatio  warcapabilityRatio2  ;  /* nations try hardest when war is 50:50 */

xtreg changeTOTspending  lagW is_war_start  is_war_startW   warcapabilityRatio warcapabilityRatioW 
							warcapabilityRatio2 warcapabilityRatioW2, fe i($fe);
test IS_WAR +war_lagW=0;
test warcapabilityRatio warcapabilityRatioW warcapabilityRatio2 warcapabilityRatioW2;

xtreg changeTOTspending  lagW is_war_start  is_war_startW   highprob highprobW,  fe i($fe);
test IS_WAR +war_lagW=0;
test highprob+highprobW=0;

xtreg changeTOTspending  lagW is_war_start  is_war_startW  contest contestW,  fe i($fe);
test IS_WAR +war_lagW=0;
test contest +contestW=0;

xtreg changeTOTspending lagW is_war_start  is_war_startW  contest3070 contest3070W,  fe i($fe);
test IS_WAR +war_lagW=0;
test contest3070+ contest3070W=0;


/* Look for escalation of effort in first and second years of the war (once full effort is being made subsequent war years should see no more effort)*/

xtreg changeTOTspending lagW is_war_start is_war_startW secondyearofwar secondyearofwarW, fe i($fe);
test is_war_start+ is_war_startW=0; test secondyearofwar+ secondyearofwarW=0; 
test is_war_start+ is_war_startW+ secondyearofwar+ secondyearofwarW=0;

xtreg changeTOTspending lagW is_war_start is_war_startW secondyearofwar secondyearofwarW
					warcapabilityRatio warcapabilityRatio2 , fe i($fe);
test is_war_start+ is_war_startW=0; test secondyearofwar+ secondyearofwarW=0; 
test is_war_start+ is_war_startW+ secondyearofwar+ secondyearofwarW=0;
test warcapabilityRatio +warcapabilityRatio2 =0;


xtreg changeTOTspending  lagW is_war_start is_war_startW secondyearofwar secondyearofwarW
			warcapabilityRatio warcapabilityRatio2 warcapabilityRatioW warcapabilityRatioW2 , fe i($fe);
test is_war_start+ is_war_startW=0; test secondyearofwar+ secondyearofwarW=0; 
test is_war_start+ is_war_startW+ secondyearofwar+ secondyearofwarW=0;
test warcapabilityRatio +warcapabilityRatio2 =0;
test warcapabilityRatio+ warcapabilityRatio2 +warcapabilityRatioW +warcapabilityRatioW2=0;

*/

/*************** Extra-State Wars (little extra effort required?) ***************/

xtreg lmilex laglmilex  lagW EX_WAR EX_WARW  lpop_WB laglgdpWB, fe i($fe);
test EX_WAR +EX_WARW=0;

xtreg lmilex laglmilex  lagW EX_WAR EX_WARW , fe i($fe);
test EX_WAR +EX_WARW=0;


xtreg lmilex laglmilex  lagW EX_WAR EX_WARW is_war_start  is_war_startW  lpop_WB laglgdpWB, fe i($fe);
test is_war_start+is_war_startW=0;
test EX_WAR +EX_WARW=0;

xtreg lmilex laglmilex  lagW EX_WAR EX_WARW IS_WAR war_lagW , fe i($fe);
test IS_WAR +war_lagW=0;
test EX_WAR +EX_WARW=0;

/*********************************************/
/***** COLONIAL AND IMPERIAL WARS ***/
/*********************************************/
/* DEMOCRACIES NOT IMMUNE FROM FIGHTING THE WEAK*/

des  EX_war_start EX_WAR;
replace EX_WAR=0 if EX_WAR==. & year >1815 &year<=1997;
xtlogit EX_WAR demaut, fe i($fe);
xtlogit EX_WAR W, fe i($fe);
xtlogit EX_WAR demaut lgdpWB lpop_WB  , fe i($fe);
xtlogit EX_WAR W w2, fe i($fe);
xtlogit EX_WAR W w2 lgdpWB lpop_WB , fe i($fe);



