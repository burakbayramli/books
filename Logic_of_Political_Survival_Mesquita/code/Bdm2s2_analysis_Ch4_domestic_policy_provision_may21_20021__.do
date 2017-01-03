/********************************************************************/
/* Bdm2S2: CHAPTER 4 ANALYSIS: PROVISION OF PUBLIC AND PRIVATE GOODS */
/* 8/5/2002: Alastair's modification of Bruce's orginal file */
/* The Unit of Observation is Nation Year */
/********************************************************************/

clear
set mem 50m
#delimit ;
cd c:\bdm2s2\data\May2002; 

use bdm2s2_nation_year_data_may2002 ;
sort ccode year;
merge ccode year using c:\bdm2s2\data\basic_datasets\corruption\transparencyinternational_corruptiondata.dta;

global logfile "output\Bdm2S2_ch4_domestic_policyWS.log,replace";
capture log close; 
set matsize 800; set more off;

tsset , clear ;


 /**********************************************************************/
 /*************** PROGRAM to run Analyses ******************************/
 /**********************************************************************/
 capture program drop modelm; program define modelm; 
 args type id cont num  ; mac shift 4; 
 local control =lower(substr(`"`cont'"',1,1)); 
 while "`1'"~= "" {;
         local count = 1; 
display "/**********************************************/";
		des `1'; sum `1'; 

         while `count'<=`num' {;
         
         if "`type'"=="fe" {;   xtreg `1'  ${eq`count'} , fe i("`id'") ; 
         
         if "`control'"=="y" {; xtreg `1'  ${eq`count'} $c  , fe i("`id'");};
 };
         if "`type'"=="re" {;   xtreg `1'  ${eq`count'} , re i("`id'") ; 
                 if "`control'"=="y" {; xtreg `1'  ${eq`count'}  $c, re i("`id'");};
 };

         if "`type'"=="gls" {;   xtgls `1'  ${eq`count'} , i(ccode) t(year) panels(het) corr(ar1) force ; 
                 if "`control'"=="y" {; xtgls `1'  ${eq`count'}  $c, i(ccode) t(year) panels(het) corr(ar1) force ;};
 };
local count = `count'+1; }; 

 mac shift;      };
 end; 
 /*********************************************************************/
 /*********************************************************************/

/******* INDEPENDENT VARIABLE SPECIFICATIONS */
 global eq1 "W "; 
 global eq2 "W lpop_WB incWB demres"; 
 global eq3 "WoverS ";
 global eq4 "WoverS lpop_WB incWB demres";
 global eq5 "W lpop_WB incWB demres Parl_Pres";
 log using $logfile; 



/****************************************************************/
/**************** Publication details  ***************************/
/****************************************************************/

/* measurement */
gen ss=log((S+1)*10)/3; corr S ss;  drop ss;
gen ParloverS=Parl_Pres/ (log((S+1)*10))/3;

corr W demaut if demaut==1 |demaut==0; 
tab W demaut if demaut==1 |demaut==0;
corr W demaut if demaut~=1 & demaut~=0; 
corr W xconst; corr demaut xconst;
corr W S; reg demaut W S;


gen invest=GrossDomInvest; gen savings=GrossDomSaving;
sum WB_gdppc_constUS95 invest FDI savings PRIVCON; 

pwcorr  WB_gdppc_constUS95 invest FDI savings PRIVCON; 
pwcorr Deathrate Beds HealthEXP Doctors InfMort LowBirth LifeExp ImmuneMEASLES ImmuneDPT  GXPDSSEC sh_h2o_s;


sum TAXGDP;
/*table 4.1*/

reg lgdpWB W S; 
predict incWB,residual;
reg lgdpWB demaut; 
predict demincWB, residual;
regr demaut W S;
predict demres, residual;
label var demres "Residual democracy not accounted for by W";

/* table 4.2 */
tsset ccode year;
gen L3W=L3.W; gen L3S=L3.S;
gen DW30=W-L3.W; gen DS30=S-L3.S;
gen lagS=L1.S;
gen L2W=L2.W;
gen L2S=L2.S;

tsset , clear;
xtreg lgdpWB DW30 DS30 L3W L3S, fe i(regyr); 

replace CL=8-CL;
replace PR=8-PR;
replace Parl_Pres=Parl_Pres/3;




/* transparency*/
gen TaxYN=(TAXGDP~=.); replace TaxYN=. if year>1990|year<1970;
gen IncomeYN=(WB_gdppc~=.); replace IncomeYN=. if year<1960;
logit TaxYN W S; predict TaxYNres, residual;
regr TAXGDP W S; predict taxrateres, residual;
/* peace */
gen COW_WAR=0 ; replace COW_WAR=1 if  CL_WAR==1|IS_WAR==1;
label var COW_WAR "Is nation involved in a COW war (either Interstate or CIVIL but not Extra-State)";


sum TaxYN IncomeYN COW_WAR CL PR  WB_gdppc_con LongGR1 VarGrow ;
pwcorr TaxYN IncomeYN COW_WAR CL PR  WB_gdppc_con LongGR1 VarGrow;
des TaxYN IncomeYN COW_WAR CL PR  WB_gdppc_con LongGR1 VarGrow;


/*Education */
sum EducExpend FemSec bhkavg Illiteracy;
des EducExpend FemSec bhkavg Illiteracy;
xtreg Illiteracy W lpop_WB incWB demres EducExpend, fe i(regyr);
xtreg EDT W lpop_WB incWB demres EducExpend, fe i(regyr);
xtreg FemSec W lpop_WB incWB demres EducExpend, fe i(regyr);
xtreg FemSec W lpop_WB incWB demres if FemSec<50, fe i(regyr);
test W+demres=0;
test W+demres+incWB=0;
xtreg FemSec W lpop_WB incWB demres if FemSec~=. & FemSec>50, fe i(regyr);
test W+demres=0;
test W+demres+incWB=0;

/* Health */
sum Deathrate Beds HealthEXP Doctors InfMort LowBirth LifeExp ImmuneMEASLES ImmuneDPT 
	GXPDSSEC sh_staa  sh_med_b  sh_med_p  sh_h2o_s ;
des Deathrate Beds HealthEXP Doctors InfMort LowBirth LifeExp ImmuneMEASLES ImmuneDPT 
	 GXPDSSEC sh_staa  sh_med_b  sh_med_p  sh_h2o_s ;
/* Trade */
des open ;sum open;
/* private */
gen blackmarketexch=exp(exchprem)-1; 
sum build exchprem blackmarketexch;
des build exchprem blackmarketexch;
pwcorr exchprem blackmarketexch build TIcorruption, obs;

/* expenditures/ Debt */
replace Klepto=abs(TAXGDP-Expenditure);

sum NatGovRevenue NatGovExpend  Klepto GovDebt;
des NatGovRevenue NatGovExpend  Klepto GovDebt;
pwcorr NatGovRevenue NatGovExpend  Klepto GovDebt;



/********DEPENDENT VARIABLES **********/
 global PUBLIC " CL PR 	EducExpend EDT FemSec bhkavg Illiteracy 
	Deathrate Beds HealthEXP Doctors InfMort LowBirth LifeExp ImmuneMEASLES ImmuneDPT 
	GXPDSSEC sh_h2o_s open TIcorruption build exchprem blackmarketexch  "     ;

 modelm fe regyr no 5  $PUBLIC   ;

xtreg TIcorruption W if year==1980, fe i(regyr);
xtreg TIcorruption W if year==1988, fe i(regyr);
xtreg TIcorruption W if year==1995, fe i(regyr);
xtreg TIcorruption W if year==1996, fe i(regyr);
xtreg TIcorruption W if year==1997, fe i(regyr);
xtreg TIcorruption W if year==1998, fe i(regyr);
xtreg TIcorruption W if year==1999, fe i(regyr);

xtreg TIcorruption W lpop_WB incWB demres if year==1980, fe i(regyr);
xtreg TIcorruption W lpop_WB incWB demres if year==1988, fe i(regyr);
xtreg TIcorruption W lpop_WB incWB demres if year==1995, fe i(regyr);
xtreg TIcorruption W lpop_WB incWB demres if year==1996, fe i(regyr);
xtreg TIcorruption W lpop_WB incWB demres if year==1997, fe i(regyr);
xtreg TIcorruption W lpop_WB incWB demres if year==1998, fe i(regyr);
xtreg TIcorruption W lpop_WB incWB demres if year==1999, fe i(regyr);

xtreg TIcorruption WoverS if year==1980, fe i(regyr);
xtreg TIcorruption WoverS if year==1988, fe i(regyr);
xtreg TIcorruption WoverS if year==1995, fe i(regyr);
xtreg TIcorruption WoverS if year==1996, fe i(regyr);
xtreg TIcorruption WoverS if year==1997, fe i(regyr);
xtreg TIcorruption WoverS if year==1998, fe i(regyr);
xtreg TIcorruption WoverS if year==1999, fe i(regyr);

xtreg TIcorruption WoverS lpop_WB incWB demres if year==1980, fe i(regyr);
xtreg TIcorruption WoverS lpop_WB incWB demres if year==1988, fe i(regyr);
xtreg TIcorruption WoverS lpop_WB incWB demres if year==1995, fe i(regyr);
xtreg TIcorruption WoverS lpop_WB incWB demres if year==1996, fe i(regyr);
xtreg TIcorruption WoverS lpop_WB incWB demres if year==1997, fe i(regyr);
xtreg TIcorruption WoverS lpop_WB incWB demres if year==1998, fe i(regyr);
xtreg TIcorruption WoverS lpop_WB incWB demres if year==1999, fe i(regyr);


xtreg Expenditure W S , fe i(regyr);
xtreg Expenditure W S lpop_WB demres incWB, fe i(regyr);
xtreg Expenditure WoverS , fe i(regyr);
xtreg Expenditure WoverS lpop_WB demres incWB, fe i(regyr);

xtreg Klepto W S , fe i(regyr);
xtreg Klepto W S lpop_WB demres incWB, fe i(regyr);
xtreg Klepto WoverS , fe i(regyr);
xtreg Klepto WoverS lpop_WB demres incWB, fe i(regyr);
xtreg Klepto W S incWB lpop_WB aid_gdp GovDebt demres, fe i(regyr);
xtreg Klepto WoverS incWB lpop_WB aid_gdp GovDebt demres, fe i(regyr);
xtreg Klepto WoverS aid_gdp GovDebt , fe i(regyr);


/* Table 4.1 */
xtreg lgdpWB W S , fe i(regyr);
xtreg lgdpWB W S lpop_WB demres , fe i(regyr);
xtreg lgdpWB WoverS , fe i(regyr);
xtreg lgdpWB WoverS lpop_WB demres , fe i(regyr);
xtreg lgdpWB Parl_Pres  if demaut==1, fe i(regyr);	
xtreg lgdpWB Parl_Pres lpop_WB if demaut==1, fe i(regyr);

xtreg invest W S , fe i(regyr);
heckman invest W S  if year>1959, select(W);
xtreg invest W S lpop_WB demres incWB, fe i(regyr);
heckman invest W S lpop_WB demres incWB if year>1959, select(W);
xtreg invest WoverS , fe i(regyr);
heckman invest WoverS if year>1959, select(W);
xtreg invest WoverS lpop_WB demres incWB, fe i(regyr);
heckman invest WoverS lpop_WB demres incWB if year>1959, select(W);


xtreg saving W S , fe i(regyr);
heckman saving W S  if year>1959, select(W);
xtreg saving W S lpop_WB demres incWB, fe i(regyr);
heckman saving W S lpop_WB demres incWB if year>1959, select(W);
xtreg saving WoverS , fe i(regyr);
heckman saving WoverS if year>1959, select(W);
xtreg saving WoverS lpop_WB demres incWB, fe i(regyr);
heckman saving WoverS lpop_WB demres incWB if year>1959, select(W);

xtreg PRIVCON	 W S , fe i(regyr);
heckman PRIVCON W S  if year>1969& year<1991, select(W);
xtreg PRIVCON W S lpop_WB demres incWB, fe i(regyr);
heckman PRIVCON W S lpop_WB demres incWB if year>1969& year<1991, select(W);
xtreg PRIVCON WoverS , fe i(regyr);
heckman PRIVCON WoverS if year>1969 & year<1991, select(W);
xtreg PRIVCON WoverS lpop_WB demres incWB, fe i(regyr);
heckman PRIVCON WoverS lpop_WB demres incWB if year>1969& year<1991, select(W);


/* TaxGDP */
	
xtreg TAXGDP Parl_Pres  if demaut==1, fe i(regyr);
xtreg TAXGDP Parl_Pres  lpop_WB incWB if demaut==1, fe i(regyr);
		

/* Table 4.4 */
global tab44 " CL PR ";
   modelm fe regyr no 5  $tab44   ;


/***** Wealth and Growth ******/ 
sort W; by W: sum WB_gdppc_con lgdpWB;
sort Parl_Pres; by Parl_Pres: sum WB_gdppc_con lgdpWB;

/* Table 4.6 */

reg CL W   ; predict CLres, residual;
reg PR W  ; predict PRres, residual;
reg invest W  ; predict investres, residual;
reg savings W  ; predict savingsres, residual;
reg open W S; predict openres,residual;

xtreg lgdpWB laglgdpWB W S CLres PRres investres savingsres lpop_WB, fe i(regyr);
xtreg lgdpWB laglgdpWB W S CLres PRres investres savingsres lpop_WB demres, fe i(regyr);
xtreg lgdpWB laglgdpWB WoverS CLres PRres investres savingsres lpop_WB, fe i(regyr);
xtreg lgdpWB laglgdpWB WoverS CLres PRres investres savingsres lpop_WB demres, fe i(regyr);

/* KEY QUESTION: IF WE CHANGE INSTITUTIONS DOES IT IMPROVE LONG TERM GROWTH */
/* political change: argument is that as institutions change we will see a disruption */
tsset ccode year; gen polchange= (W-L2.W)^2; 
gen DW20=W-L2.W; label var DW20 "Change in W: W-L2.W: directional political change";
gen DS20=S-L2.S;
replace polchange=(S-L2.S)^2 if polchange~=. & (abs(DS20))>(abs(DW20)) & S~=. & L2.S~=. & W~=. & L2.W~=.;
label var polchange "squared difference between W today and W two years previous or, if W did not change and S did, then the change in S over 2 years"; 
gen DWoverS20=WoverS-L2.WoverS; label var DWoverS "Change in WoverS: WoverS-L2.WoverS: directional change in loyalty norm";
gen DParloverS20=ParloverS-L2.ParloverS; label var DParloverS20 "Change in Parl_Pres/S over two years"; 
tsset, clear;
gen temp=(polchange>0 & polchange~=. & WB_growth ~=.);
sort ccode; by ccode: egen grochan=sum(temp); drop temp;
label var grochan "Do institutions change during period for which we have economic data? (sum of polchange)";

#delimit ;
xtreg WB_growth  W S polchange DWoverS20  if grochan>0, fe i(regyr); 
xtreg WB_growth  W S polchange  DWoverS20 lpop_WB if grochan>0, fe i(regyr);
xtreg WB_growth  W S polchange DWoverS20 lpop_WB  investres savingsres if grochan>0, fe i(regyr);
xtreg WB_growth  W S polchange DWoverS20 lpop_WB  investres savingsres CLres PRres if grochan>0, fe i(regyr);
xtreg WB_growth  W S polchange DWoverS20 lpop_WB  FDI investres savingsres CLres PRres if grochan>0, fe i(regyr);


xtreg WB_growth  WoverS polchange DWoverS20 if grochan>0, fe i(regyr);
xtreg WB_growth  WoverS polchange  DWoverS20  lpop_WB if grochan>0, fe i(regyr);
xtreg WB_growth  WoverS polchange DWoverS20  lpop_WB  investres savingsres if grochan>0, fe i(regyr);
xtreg WB_growth  WoverS polchange DWoverS20  lpop_WB  investres savingsres CLres PRres if grochan>0, fe i(regyr);
xtreg WB_growth  WoverS polchange DWoverS20  lpop_WB  FDI investres savingsres CLres PRres if grochan>0, fe i(regyr);

xtreg WB_growth  ParloverS polchange DParloverS20 if grochan>0 & demaut==1, fe i(regyr);
xtreg WB_growth  ParloverS polchange  DParloverS20  lpop_WB if grochan>0 & demaut==1, fe i(regyr);
xtreg WB_growth  ParloverS polchange DParloverS20  lpop_WB  investres savingsres if grochan>0 & demaut==1, fe i(regyr);
xtreg WB_growth  ParloverS polchange DParloverS20  lpop_WB  investres savingsres CLres PRres if grochan>0 & demaut==1, fe i(regyr);

xtreg VarGrow_ W S , fe i(regyr);
xtreg VarGrow_ W S lpop_WB demres incWB , fe i(regyr);
xtreg VarGrow_ WoverS , fe i(regyr);
xtreg VarGrow_ WoverS lpop_WB demres incWB, fe i(regyr);

/* Reverse causality: growth causes political change */ 
tsset ccode year;
gen L4growWB=L4.WB_growth;gen L3growWB=L3.WB_growth;gen L2growWB=L2.WB_growth; gen L1growWB=L1.WB_growth; 
gen DW=W-L.W;capture drop DS; gen DS=S-L.S;
tsset, clear;

xtreg DW lagW L4growWB L3growWB L2growWB L1growWB  , fe i(regyr); 
test L4growWB+L3growWB+ L2growWB+ L1growWB=0;
xtreg DW L4growWB L3growWB L2growWB L1growWB laglgdpWB  lpop_WB , fe i(regyr); 
test L4growWB+L3growWB+ L2growWB+ L1growWB=0;
oprobit DW  L4growWB L3growWB L2growWB L1growWB laglgdpWB  lpop_WB; 
test L4growWB+ L3growWB+ L2growWB+ L1growWB=0;

xtreg polchange  lagW L4growWB L3growWB L2growWB L1growWB laglgdpWB  lpop_WB , fe i(regyr); 
test L4growWB+ L3growWB+ L2growWB+ L1growWB=0;


xtreg DS lagW L3growWB L2growWB L1growWB  , fe i(regyr); 
test L3growWB+ L2growWB+ L1growWB=0;
xtreg DS lagW L4growWB L3growWB L2growWB L1growWB  , fe i(regyr); 
test L4growWB+L3growWB+ L2growWB+ L1growWB=0;
xtreg DS  L3growWB L2growWB L1growWB laglgdpWB  lpop_WB , fe i(regyr); 
test L3growWB+ L2growWB+ L1growWB=0;
oprobit DS  L3growWB L2growWB L1growWB laglgdpWB  lpop_WB; 
test L3growWB+ L2growWB+ L1growWB=0;


/***** Foreign Aid *****/
/* controversal what effect Aid has. It depends upon institutions*/

gen Waid_gdp=W*aid_gdp;
gen WlaglgdpWB=W*laglgdpWB;

xtreg WB_growth Waid_gdp aid_gdp lpop_WB , fe i(regyr); 
xtreg WB_growth Waid_gdp aid_gdp lpop_WB , fe i(regyr); 
xtreg WB_growth Waid_gdp aid_gdp lpop_WB  W , fe i(regyr); 
xtreg WB_growth Waid_gdp aid_gdp lpop_WB W w2 , fe i(regyr); 
xtreg WB_growth Waid_gdp aid_gdp lpop_WB W w2 S, fe i(regyr); 
xtreg WB_growth Waid_gdp aid_gdp lpop_WB W w2 S laglgdpWB, fe i(regyr); 
xtreg WB_growth Waid_gdp aid_gdp lpop_WB W w2 S laglgdpWB WlaglgdpWB, fe i(regyr); 
xtreg WB_growth W S DWoverS20 polchange CLres PRres TaxYNres taxrateres openres lpop_WB  
demres incWB if grochan>0  & grochan~=.,  fe i(regyr);
xtreg WB_growth W S DWoverS20 polchange FDI CLres PRres TaxYNres taxrateres openres lpop_WB  
demres incWB if grochan>0  & grochan~=.,  fe i(regyr);

 reg3 (invest W Waid_gdp aid_gdp lpop_WB PR lgdpWB  ) (WB_grow Waid_gdp aid_gdp invest   lpop_WB   ) 
											,  endog(WB_grow invest ) ;

gen expcap=((Expenditure/100)*WB_gdp95)/WB_pop;
label variable expcap "Expenditures/Capita by Government";

xtreg expcap W w2 , fe i(regyr);
xtreg expcap W w2 incWB lpop_WB , fe i(regyr);
xtreg expcap W w2 incWB lpop_WB demres, fe i(regyr);
xtreg expcap W w2 incWB lpop_WB demres Parl_Pres, fe i(regyr);

xtlogit COW_WAR W , fe i(ccode);
xtlogit COW_WAR W lpop_WB incWB demres , fe i(ccode);
xtlogit COW_WAR WoverS , fe i(regyr);
xtlogit COW_WAR WoverS lpop_WB incWB demres, fe i(ccode) ; 
xtlogit COW_WAR W lpop_WB incWB demres Parl_Pres , fe i(ccode);

xtlogit TaxYN W , fe i(regyr);
xtlogit TaxYN W lpop_WB incWB demres , fe i(regyr);
xtlogit TaxYN WoverS , fe i(regyr);
xtlogit TaxYN WoverS lpop_WB incWB , fe i(regyr) ; 
xtlogit TaxYN W lpop_WB incWB demres Parl_Pres , fe i(regyr);

xtlogit IncomeYN W , fe i(regyr);
xtlogit IncomeYN W lpop_WB demres , fe i(regyr);
xtlogit IncomeYN WoverS , fe i(regyr);
xtlogit IncomeYN WoverS lpop_WB demres, fe i(regyr);
xtlogit IncomeYN W lpop_WB  demres Parl_Pres , fe i(regyr);


