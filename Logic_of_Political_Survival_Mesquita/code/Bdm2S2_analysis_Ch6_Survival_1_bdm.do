/********************************************************************/
/* Bdm2S2: CHAPTER 7 ANALYSIS: Survival of political leaders        */
/* The Unit of Observation is Leader-Year                           */
/********************************************************************/

clear
set mem 50m
#delimit ;
cd c:\bdm2s2\data\May2002; 

use bdm2s2_nation_year_data_may2002; sort ccode year; save, replace;
use datasets\bdm2s2_leader_year_data; 
sort ccode year; 
merge ccode year using bdm2s2_nation_year_data_may2002 ;
tab _m; drop _m;

global logfile "output\Bdm2S2_ch7_survival.log,replace";

set matsize 800; set more off;
/* stset endtime, id(leader_id) fail(leadgone==1) origin(time intime)  scale(365.25); */
tsset , clear ;
gen tenure = (endtime-indate)/365.25;


/**********************************************************************/
/**********************************************************************/
/********************* HAZARD ANALYSIS ********************************/
/**********************************************************************/
/**********************************************************************/

gen binw=W>=.75;
replace binw=. if W==.;

capture log close; 
log using $logfile; 

/**********************************************/
/**************** Hall of Fames ***************/
/**********************************************/
replace CL=8-CL;
replace PR=8-PR;

/* place to bracket out lists */

sort leader_id;
by leader_id: egen meanCL=mean(CL) if CL~=.; label var meanCL "average Civil liberties under leader's tenure";
by leader_id: egen meanPR=mean(PR) if PR~=.; label var meanPR "average Property Rights under leader's tenure";
by leader_id: egen meangrow=mean(grow); label var meangrow "average growth rate under leader's tenure";
by leader_id: egen meangrowWB=mean(WB_growth) if WB_growth~=.; label var meangrow "average WB growth rate under leader's tenure";
by leader_id: egen meanW=mean(W)if W~=.; label var meanW "average W under leader's tenure";
by leader_id: egen meandemaut=mean(demaut) if demaut~=.; label var meandemaut "average demaut under leader's tenure";
by leader_id: egen meanpop=mean(WB_pop) ; label var meanpop "average WorldBank population";
gen econdata=(WB_growth~=.);
by leader_id: gen least5=sum(econdata) if leader_id~=.; 
gen at_least5=(least5>=5); 
gen at_least3=(least5>=3);
 drop econdata least5;
qui egen lowgrow=min(grow), by( leader_id);
sort leader_id year;	
qui by leader_id: gen LLlast=(_N==_n);
qui by leader_id:gen wartotal=sum(CL_WAR+IS_WAR);
qui by leader_id: replace wartotal=wartotal[_N];
gen pres_gro=(meangrowWB~=.);
gen post55=(year>1955);
egen pctgrowWB=pctile(meangrowWB) if LLlast==1 & post55==1 & meangrowWB~=., p(75);
gen highgrow=(meangrowWB>=pctgrowWB);
replace highgrow=. if meangrowWB==.;
egen pctCL=pctile(meanCL) if LLlast==1 & post55==1 & meanCL~=., p(75);
gen highCL=(meanCL>=pctCL);
replace highCL=. if meanCL==.;
egen pctPR=pctile(meanPR) if LLlast==1 & post55==1 & meanPR~=., p(75);
gen highPR=(meanPR>=pctPR);
replace highPR=. if meanPR==.;
format tenure meanW meangrowWB meanCL meanPR %4.2f;


/*
/* Longest Lasting Leaders for the last 50 years */
 gsort -LLlast -tenure;   
list  leader longname meanW  tenure  meangrowWB  meanCL meanPR if LLlast==1 & post55==1 & 
tenure>=3 & tenure~=. & meanpop>=1000000 & meanpop~=. in 1/60;
sum   meanW meandemaut tenure meangrowWB meanCL meanPR if LLlast==1 & post55==1 & meanpop>=1000000 & meanpop~=. in 1/60  ; 

/* Highest average growth rate for those with at least 5 years of economic data */
gsort  -LLlast -at_least3 -meangrowWB +wartotal -meanPR -meanCL -tenure;

gsort -LLlast -meangrowWB -tenure;
list  leader  longname meanW  tenure  meangrowWB  meanCL meanPR if highgrow==1&wartotal==0 &  
(meanPR>=4.5|meanCL>=4.5) &meanCL~=. & meanPR~=. & LLlast==1 & post55==1 & 
tenure>=3 & tenure~=. & meanpop>=1000000 & meanpop~=. in 1/192 ;

sum meanW  meandemaut tenure  meangrowWB  meanCL meanPR if highgrow==1&wartotal==0 &  
(meanPR>=4.5|meanCL>=4.5) &meanCL~=. & meanPR~=. & LLlast==1 & post55==1 & 
tenure>=3 & tenure~=.  & meanpop>=1000000 & meanpop~=. in 1/192;

list  leader longname meanW tenure meangrowWB meanCL meanPR year if highgrow==1&wartotal==0 & highCL==1 & highPR==1 
& LLlast==1 & at_least3==1 & post55==1 in 1/30  ; 
sum W  meanW meandemaut tenure meangrow meangrowWB meanCL meanPR if highgrow==1&wartotal==0 & highCL==1 & highPR==1 in 1/50  ; 
*/

/**********************************************/
/**********************************************/
/**************** ANALYSES *******************/
/**********************************************/
/**********************************************/


/* Benchmarks */
/* mean survival time */
gen binW=(W>=0.75); replace binW=. if W==.;
stdes; sort W; by W: stdes; sort binW; by binW: stdes; 
stdes if tenure>1 & binW==0; stdes if tenure>1 & binW==1;

/* Surivival functions */
gen binaryW=binW; 
sts graph, by(binaryW) tmax(15)saving(output\Figure5_1a,replace) 
	l1("Proportion of Leaders Surviving") b2("Tenure in office(Years)");

sts graph, by(W) tmax(15)saving(output\Figure5_1b,replace)
	l1("Proportion of Leaders Surviving") b2("Tenure in office(Years)");

sts list, by(binW) compare at (0 1 to 15);

sts test binW ;
sts list, by(W) compare at (0 1 to 15);
sts test W;  sts test binW;  

*/



/* STREG TESTS of AFFINITY PREDICTION */
/* Theory suggests the revelation of affinity over time strongly influences ability to survive in small W, but must less so in big W. Therefore the paramter p in weibull regression should be much less that one with small W (a footnote for this result?)*/
streg  if binw==1, dist(weibull) cluster(ccode) nolog;
streg if binw==0,  dist(weibull) cluster(ccode) nolog;
streg W s , dist(weibull) cluster(ccode) anc(W) nolog;
streg W s , dist(weibull) cluster(ccode) anc(W s) nolog;
 
/* alternatively to avoid the need for the ancilliary parameter specification */
gen Yr=log(1+year-inyear); gen YrW=Yr*W;
streg W Yr YrW , dist(exp) nohr nolog;
streg W s Yr YrW , dist(exp) nohr  nolog;

/* End of term effect? */
des finittrm yrcurnt;
gen finalyear_in_term=(yrcurnt<1);
gen penultimateyr_in_term=(yrcurnt<2&yrcurnt>0);
streg W s finalyear_in_term penultimateyr_in_term, dist(weibull) cluster(ccode) anc(W s) nolog;
streg W s finalyear_in_term penultimateyr_in_term finittrm, dist(weibull) cluster(ccode) anc(W s) nolog;
gen LargeWfinalyear=W*finalyear_in_term;
streg W s finalyear_in_term LargeWfinalyear, dist(weibull) cluster(ccode) anc(W s) nolog;



/**************************************************/
/*********** PERFORMANCE, AFFECT ON TENURE ********/
/**************************************************/
des NatGovRevenue NatGovExpend bal_bud GovDebt;
 /* Expenditures */ 
sort leader_id year;
qui by leader_id: gen yearch=year-year[_n-1];
gen logexpend=log(Expenditure+1);
qui by leader_id: gen lagexpend=Expenditure[_n-1] if yearch==1;
gen laglogexpend=log(lagexpend+1);
xtreg logexpend laglogexpend Yr YrW, fe i(regyr);
gen Wfinalyear_in_term=W*finalyear_in_term;
xtreg logexpend laglogexpend Yr YrW W finalyear_in_term Wfinalyear_in_term, fe i(regyr);
xtreg logexpend laglogexpend Yr YrW finalyear_in_term Wfinalyear_in_term, fe i(regyr);



gen nexpc=NatGovExpend/WB_pop; 
gen nexpcYr=nexpc*Yr; gen nexpcW=nexpc*W; gen nexpcYrW=nexpc*Yr*W;
streg W s nexpc nexpcW Yr YrW nexpcYr nexpcYrW  , dist(exp) cluster(ccode) nohr; 
streg nexpc nexpcW W s , dist(wei) anc(W s) cluster(ccode) nohr; 
test nexpc nexpcW;

/* does a balanced budget help survival? */ 
gen bal_budYr=bal_bud*Yr; gen bal_budW=bal_bud*W; gen bal_budYrW=bal_bud*Yr*W;
streg W s bal_bud bal_budW Yr YrW bal_budYr bal_budYrW  , dist(exp) cluster(ccode) nohr; 
streg bal_bud bal_budW W s , dist(wei) anc(W s) cluster(ccode) nohr; 
test bal_bud bal_budW;

/* does Debt hurt survival? */ 
gen GovDebtYr=GovDebt*Yr; gen GovDebtW=GovDebt*W; gen GovDebtYrW=GovDebt*Yr*W;
streg W s GovDebt GovDebtW Yr YrW GovDebtYr GovDebtYrW  , dist(exp) cluster(ccode) nohr; 
streg GovDebt GovDebtW W s , dist(wei) anc(W s) cluster(ccode) nohr; 
test GovDebt GovDebtW;


/* PUBLIC GOODS : focus on core public goods of growth PR CL*/

egen wealth=std(lgdpWB); egen St_grow=std(WB_growth); egen labored=std(bhkavg);
egen free=std(open); 
gen COW_WAR=0 ; replace COW_WAR=1 if  CL_WAR==1|IS_WAR==1; label var COW_WAR "Is nation involved in a COW war (either Interstate or CIVIL but not Extra-State)";
egen wars=std(COW_WAR); egen liberties=std(CL); egen polRights=std(PR);
gen public=(wealth+St_grow+labored+free+liberties+polRights)/5;

gen publicbw=public*binw; gen pub_YrW=public*W*Yr; gen pub_W=public*W;
gen warsbw=wars*binw;

gen BW=binw; gen WB_growthW=WB_growth*W; gen WB_growthBW=WB_growth*BW; 
gen WB_growthYrW=WB_growth*Yr*W;gen WB_growthYr=WB_growth*Yr;
gen PRW=PR*W; gen PRYr=PR*Yr; gen PRWYr=PR*Yr*W;
gen CLW=CL*W; gen CLYr=CL*Yr; gen CLWYr=CL*Yr*W;


/*******************************************************/
/*********** PUBLIC GOODS ******************************/
/*******************************************************/

/* models 1 */
streg W WB_growth WB_growthW , dist(wei) anc(W) nohr nolog;
streg W s WB_growth WB_growthW , dist(wei) anc(W s) nohr nolog;
streg W s WB_growth WB_growthW , dist(wei) anc(W) nohr nolog;
streg W s Yr YrW WB_growth WB_growthW WB_growthYr WB_growthYrW , dist(exp) nohr nolog;
streg W s WB_growth   , dist(wei) anc(W s) nohr nolog;
streg W s PR   , dist(wei) anc(W s) nohr nolog;
streg W s CL   , dist(wei) anc(W s) nohr nolog;
streg W s WB_growth PR CL , dist(wei) anc(W s) nohr nolog;

streg W s public  , dist(wei) anc(W s) nohr nolog ;
streg W s public pub_YrW YrW pub_YrW, dist(exp)  nohr nolog;



/********* PRIVATE GOODS *********/

/* Black Market */  
gen BLACK=exp(exchprem)-1; 
gen BLACKBW=BLACK*BW;
streg W s BLACK   BLACKBW   , dist(wei) anc(W s) cluster(ccode) nohr; 

egen black=std(exchprem); gen blackbw=black*binw;
gen blackW=black*W; gen blackYrW=black*W*Yr; gen blackYr=black*Yr;
#delimit;
 gen buildBW=build*BW; gen buildW=build*W; gen buildYrW=build*Yr*W; gen buildYr=Yr*build; 

streg W s Yr YrW black blackYr blackW blackYrW  , dist(exp) cluster(ccode) nohr; 
streg W s black   blackW   , dist(wei) anc(W s) cluster(ccode) nohr; 
test black blackW;

streg W s build buildW    , dist(wei) anc(W s) cluster(ccode) nohr; 
test build buildW ; 

/*********************************************************/
/*********** Survival by different sized W (to examine marginal effects */
/*********************************************************/
/* The survival properties differ between difference sized W: 
	Affinity learning effect means survival differs drastically. 
	Study different W sized groups separately to estimate difference in performance */

sort W;by W : sum WB_growth; sort binw; by binw: sum WB_growth;
sum WB_growth if W==1; local WB_growth_W_equals1=r(mean);
sum WB_growth if W<1; local WB_growth_W_not_equals1=r(mean);
sum WB_growth if binw==1; local WB_growth_binW_equals1=r(mean);
sum WB_growth if binw==0; local WB_growth_binW_equals0=r(mean);

streg W s WB_growth if W==1 , dist(wei)  nohr ;


/*********************************************************/
/*********** Survival by different sized W (to examine marginal effects */
/*********************************************************/
/* The survival properties differ between difference sized W: 
	Affinity learning effect means survival differs drastically. 
	Study different W sized groups separately to estimate difference in performance */
sort W;by W : sum WB_growth; sort binw; by binw: sum WB_growth;
sum WB_growth if W==1; local WB_growth_W_equals1=r(mean);
sum WB_growth if W<1; local WB_growth_W_not_equals1=r(mean);
sum WB_growth if binw==1; local WB_growth_binW_equals1=r(mean);
sum WB_growth if binw==0; local WB_growth_binW_equals0=r(mean);



streg W s WB_growth if binw==1 , dist(wei)  nohr ;

streg W s WB_growth if binw==0 , dist(wei)  nohr ;

streg  black  blackbw  WB_growth WB_growthBW BW , dist(wei) anc(BW)  nohr;
test WB_growth+WB_growthBW=0;
test WB_growth WB_growthBW;
test black+blackbw=0;
test black blackbw;

streg  black  blackbw WB_growth WB_growthBW BW s, dist(wei) anc(BW)  nohr;

test WB_growth+WB_growthBW=0;
test WB_growth WB_growthBW;
test black+blackbw=0;
test black blackbw;
#delimit ;
streg  BW  WB_growth WB_growthBW BLACK BLACKBW , dist(wei) anc(BW)  nohr;
streg  BW S WB_growth WB_growthBW BLACK BLACKBW , dist(wei) anc(BW)  nohr;
streg  BW S WB_growth WB_growthBW BLACK BLACKBW , dist(wei) anc(BW S)  nohr;


streg  black  blackbw  WB_growth WB_growthBW BW s, dist(wei) anc(BW s)  nohr;
test WB_growth+WB_growthBW=0;
test WB_growth WB_growthBW;
test black+blackbw=0;
test black blackbw;

/*** Note given the scaling of black: a zero exchange rate premium corresponds to black = -0.629
 and a 10 fold exchange rate prem corresponds to black=3.873 ***/


