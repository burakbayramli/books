clear
set mem 50m
#delimit ;

/********************************************************************/
/* Compile Nation-Year level data */
/* May 6th 2002: Alastair Smith: Revisions and compilations of data */

cd c:\bdm2s2\data\May2002\;
set matsize 800; set more off;

/********************************************************************/
/**************** Compile Data **************************************/
/********************************************************************/

/* data organized by cow country code */
use datasets\polity4; sort ccode year; save,replace;
use datasets\BanksCNTS_data.dta; sort ccode year; save,replace;
use datasets\Preworski_cheibub_tax_data.dta; sort ccode year; save,replace; 
use datasets\extract_from_original_bdm2s2_from_Randy; sort ccode year; save,replace;
use datasets\Freedom_house.dta; sort ccode year; save,replace;
use datasets\COWwars_nationyear.dta; sort ccode year; save,replace;
use datasets\poerights.dta; sort ccode year; save,replace;



use datasets\Preworski_cheibub_tax_data.dta; 
/* Add labels */
keep  ccode year TAXGDP  TAXGNP TREV INCOMEY INCOMET DIRT INDIRT INST AGEA AUT
 AGEI AUT BRITCOL CATH  COMEX  EDT EDTG  ELF60   ETHNIC GINI  GXPDEDUC  GXPDHLTH GXPDSSEC GXPKTOTL 
INEQ INST  LEGSELEC MOSLEM PRIVCON OIL PROT REGION REGION2 RELIGION RIOTS  STRIKES  UNSTABLE  WAR ;
label var ccode "COW country code";
label var INST "ACLP: Classification of regimes: 0=dictator; 1=parliamentary demo: 2=mixed demo:3=Presidential demo";
label var AGEA "ACLP: Age of regime by AUT";
label var AGEI "ACLP: Age of regime by INST";
label var AUT "ACLP: Classification of regimes: 0=demo: 1=bureaucracy: 2=autocracy";
label var BRITCOL "ACLP:Former British Colony ";
label var CATH "ACLP: %age Catholics in population ";
label var COMEX  "ACLP: Primiary Commodity exporters =1 ";
label var EDTG "ACLP: Growth rate in EDT ";
label var EDT "ACLP: Cummulative years of education for average member of work force (Bhalla 1994) ";
label var ELF60 "ACLP: ethnolinguistic fractionization";
label var GINI "ACLP: GINI index of inequality";
label var GXPDEDUC "ACLP: Central government expenditure on education as as share of GDP (both at factor cost, current national currency). [WB1994";
label var GXPDHLTH "ACLP: Central government expenditure on health as a share of GDP (both at factor cost, current national currency). [WB 1994";
label var GXPDSSEC "ACLP: Central government expenditure on social security and welfare as a share of GDP (both at factor cost, current national currency) [WB 1994";
label var GXPKTOTL "ACLP: Central government total capital expenditure as a share of GDP (both at factor cost, current national currency) [WB 1994";
label var ETHNIC "ACLP: Percentage of population of the largest ETHNIC group, measured in the year for which data were available (roughly 1976-1985). [The Economist 1988 and Vanhanen 1992].";
label var INEQ "ACLP: Ratio of income shares of the top to the bottom quintiles of income recipients. [Deininger and Squire 1996].";
label var LEGSELEC "ACLP: Legislative selection. As in BANKS but undated and slightly modofied ";
label var MOSLEM "ACLP: Percentage of Moslems in the population. [LPS 1993/94]."; 
label var OIL "ACLP: Dummy variable coded 1 if the average ratio of fuel exports to total exports in 1984-86 exceeded 50%, 0 otherwise. [International Monetary Fund (1994b)].";
label var PROT "ACLP: Percentage of Protestants in the populaton [LPS 1993/94].";
label var PRIVCON "ACLP: Private Consumption ";
label var REGION "ACLP: Region of the world, coded 1 if Latin America and Caribbean; 2 if Middle East; 3 if Eastern Europe; 4 if Africa; 5 if Southern Asia; 6 if Eastern Asia; 7 if OECD. [World Bank 1990].";
label var REGION2 "ACLP: Region of the world, coded 1 if Sub-Saharan Africa; 2 if South Asia; 3 if East Asia; 4 if South East Asia; 5 if Pacific Islands/Oceania; 6 if Middle East/North Africa; 7 if Latin America; 8 if Caribbean and non-Iberic America; 9 if Eastern Europe/Soviet Union; 10 if Industrial Countries.";
label var RELIGION "ACLP: Percentage of population of the largest religious group, measured in the year for which data were available (roughly 1976-1985). [The Economist 1988 and Vanhanen 1992]";
label var RIOTS "ACLP: Number of violent demonstrations or clashes of more than 100 citizens involving the use of physical force. [Banks 1996].";
label var STRIKES "ACLP: Number of strikes of 1,000 or more industrial or service workers that involves more than one employer and that is aimed at national government policies or authority. [Banks 1996].";
label var UNSTABLE "ACLP: Dummy variable coded 1 for all the years in a country that experienced at least one regime transition (as defined by REG) between 1950 and 1990.";
label var WAR "Dummy variable coded 1 when there is a war of any type (international or civil) on the territory of a country, 0 otherwise. [Singer and Small 1994";
label var TAXGDP "Cheibub: Tax revenue / GDP";
label var TAXGNP "Cheibub: Tax Revenue /GNP";
label var TREV "Cheibub: Total Revenue/GDP";
label var INCOMEY "Cheibub: Income Tax/GDP";
label var INCOMET "Cheibub: Income tax/tax revenue ";
label var DIRT "Cheibub: Direct Tax/TAX revenue";
label var INDIRT "Cheibub: Indirect TAX/ TAX revenue ";
sort ccode year; 



merge ccode year using datasets\polity4; 
tab _m; drop _m; sort ccode year;
drop cyear scode country flag fragment prior post eyear edate eprec byear interim;
merge ccode year using datasets\BanksCNTS_data; 
tab _m; drop _m; sort ccode year;
 sort ccode year;
merge ccode year using datasets\extract_from_original_bdm2s2_from_Randy;
tab _m; drop _m; sort ccode year;
merge ccode year using datasets\Freedom_house.dta;
tab _m; drop _m; sort ccode year;
merge ccode year using datasets\COWwars_nationyear.dta;
tab _m; drop _m; sort ccode year;
merge ccode year using  datasets\poerights.dta; 
tab _m; drop _m; sort ccode year;


save temp1, replace; 

 




/* World Bank Country Code organized data */ 
use datasets\country_code_correspondences; sort worldbankcode ; save, replace;
use datasets\datavine_data.dta; sort worldbankcode year; save, replace;

keep finittrm yrcurnt military auton  strikes nspolpr  agovdem govtcris 
corrupti80_89 purges coups riots constchg cabchg compolt eda
 pop rgdpch rgdpl c i g rgdptt y cgdp cc ci cg p pc pi pg xr rgdpea rgdpw kapw kdur knres
 kother kres ktranp open rgnp ipri stliv worldbankcode year ; 



replace finittrm=. if finittrm==-999;
label var finittrm "Is there a finite term in office? (1 if yes, O if no): Phil Keefer:World Bank data set on political institutions";
replace yrcurnt=. if yrcurnt==-999;
label var yrcurnt "Years left in current term: Phil Keefer:World Bank data set on political institutions";
replace military=. if military==-999;
label var military "Is Chief Executive a military officer?: Phil Keefer:World Bank data set on political institutions";
replace auton=. if auton==-999;
label var auton "Federalism? Autonomy regions: Phil Keefer:World Bank data set on political institutions";
label var strikes "strikes: Easterley";
label var nspolpr "political protests:Easterley";



label var agovdem "Anti-government demonstrations: Any peaceful public gathering of at least 100 people for the primary purpose of displaying or voicing their opposition to 
		government policies or authority, excluding demonstrations of a distinctly anti-foreign nature.:Easterley&Levine";
label var govtcris " Major Government Crises: Any rapidly developing situation that threatens to bring the downfall of the present regine - excluding situations of revolt aimed at such overthrow.:Easterley&levine";
label var corrupti80_89 "corruption:Easterley&levine";
label var purges " Purges: Any systematic elimination by jailing or execution of political opposition within the ranks of the regime or the opposition.:Easterley&levine";
label var coups " Coups dEtat: The number of extraconstitutional or forced changes in the top government elite and/or its effective control of the nations power structure in a given year. Unsuccessful coups are not counted.:Easterley&levine";
label var riots "Any violent demonstration or clash of more than 100 citizens involving the use of physical force.:Easterley&levine";
label var constchg "Major Constitutional Changes: The number of basic alternations in a states constitutional structure, the extreme case being the adoption of a new constitution that significantly alters the prerogatives of the various branches of government.:Easterley&levine";
label var cabchg "Major Cabinet Changes: The number of times in a year that a new premier is named and/or 50% of the cabinet posts are occupied by new ministers.:Easterley&levine";
rename compolt genocide; 
label var genocide "Dummy =1 for countries with genocidal incident involving political victims or mixed communal and political victims.:Easterley&levine";
label var eda "Effective Development Assistance (=EDA GQ +grants -TA-B Debt): Craig Burnside, David Dollar: Aid Dataset";
label var pop "population: PWT";
label var rgdpch "Real GDP per capita in constant dollars (Chain Index) (expressed in international prices, base 1985.) : PWT";
label var  rgdpl "Real GDP per capita (Laspeyres index) (1985 intl. prices) : PWT";
label var c  " Real Consumption share of GDP [%] (1985 intl. prices) : PWT";
label var  i " Real Investment share of GDP [%] (1985 intl. prices): PWT";
label var  g " Real Government share of GDP [%] (1985 intl. prices): PWT";
label var  rgdptt  "Real GDP per capita in constant dollars adjusted for changes in terms of trade (1985 international prices for domestic absorption and current prices for exports and imports.) : PWT";
label var  cgdp " Real GDP per capita (current intl. prices): PWT";
label var y  "CGDP relative to U.S. [%] (U.S.=100, current intl. prices) : PWT";
label var  cc " Real Consumption share of GDP [%] (current intl. prices) : PWT";
label var  ci " Real Investment share of GDP [%] (current intl. prices): PWT";
label var  cg "Real Government share of GDP [%] (current intl. prices) : PWT";
label var  p "Price level GDP [%] (PPP GDP/ U.S. dollar exchange rate) : PWT";
label var  pc " Price level Consumption [%] ([PPP of C]/XR): PWT";
label var  pi "Price level Investment [%] ([PPP of I]/XR) : PWT";
label var  pg "Price level Government [%] ([PPP of G]/XR) : PWT";
label var xr "Exchange Rate with U.S. dollar : PWT";
label var rgdpea "Real GDP per Equivalent Adult (1985 intl. prices): PWT";
label var  rgdpw "Real GDP per Worker (1985 intl. prices) : PWT";
label var kapw  "Non-residential Capital Stock per Worker (1985 intl. prices) : PWT";
label var kdur  "Producer Durables (% of KAPW) (1985 intl. prices) : PWT";
label var knres " Nonresidential construction (% of KAPW) (1985 intl. prices) : PWT";
label var  kother " Other Construction (% of KAPW) (1985 intl. prices): PWT";
label var kres "Residential construction (% of KAPW) (1985 intl. prices) : PWT";
label var ktranp "Transportation Equipment (% of KAPW) (1985 intl. prices) : PWT";
label var open "Openness (Exports+Imports)/Nominal GDP : PWT";
label var rgnp "Real Gross National Product (% of CGDP) : PWT";
label var  ipri " Gross Domestic Private Investment (% of Gross Domestic Investment in current intl.prices) : PWT";
label var  stliv " Standard of Living Index (Consumption plus government consumption minus military expenditure, % of GDP) : PWT";

sort worldbankcode year;
save tempDataVine,replace;


use datasets\WB_DevInd2001; sort worldbankcode year; save, replace;

merge worldbankcode year using tempDataVine.dta; 
tab _m ; drop _m; sort worldbankcode year; 
merge worldbankcode using datasets\country_code_correspondences;
tab _m ;  drop if _m==2; drop _m; sort ccode year; 

save WBtemp, replace; use temp1; 
merge ccode year using WBtemp; 
tab _m;  drop _m; 
sort ccode year; 
 sort ccode year;
 gen NatYr=1;
 replace NatYr=0 if (year==year[_n-1] & ccode==ccode[_n-1]); 
/* the cases of multiple observations per nation year correspond to missing ccode */ 
keep if NatYr==1;


label var esolow "Solow productivity residual=ZPGDP-0.4*ZKAP-0.6*ZLAB: Fischer J of Monetary Econ 1993";
label var bhkavg "average Barro-Lee human capital stock (average years of educational attainment of the labor force: Fischer J of Monetary Econ 1993";
label var exchprem "log (1+BLACK), where BLACK is the average black market exchange rate premium from WDR: Fischer J of Monetary Econ 1993";


/********************************************************************/
/**************** Make region year fixed effects ********************/
/********************************************************************/
capture drop cowreg;
gen cowreg=0; replace cowreg=1 if ccode>=2 &ccode<=56 /* North America and Caribbean*/; 
replace cowreg=2 if ccode>=70 &ccode<=165 /* South and Central America */;
replace cowreg=3 if ccode>=200 &ccode<=395 /* Europe */;
replace cowreg=4 if ccode>=402 &ccode<=591 /* Africa */;
replace cowreg =5 if ccode>=600 & ccode<=696 /*Middle East */;
replace cowreg =6 if ccode>=700 & ccode<=991 /* Asia */;
gen regyr=cowreg*10000+year; 
label var cowreg "COW region code";
label var regyr "region-year dummy";
sort ccode year; 


/********************************************************************/
/**************** Polity and Institution Variables ******************/
/********************************************************************/

/* When Regtype is not missing data and is not equal to codes 2 or 3 in the Polity data set, so that the regime type was not a military or 
military/civilian regime, we award one point to W. When Xrcomp--that is, the competitiveness of executive recruitment--
is larger than or equal to 2 then another point is assigned to W. An Xrcomp code of 1 means that the chief executive was 
selected by heredity or in rigged, unopposed elections. Code values of 2 and 3 refer to greater degrees of responsiveness to supporters, 
ndicating a larger winning coalition. Xropen - the openness of executive recruitment -- contributes an additional point to W if 
the executive is recruited in a more open setting than heredity (that is, the variable's value is greater than 2). 
Finally, one more point can be contributed to the index of W if Parcomp - competitiveness of participation -- is coded as a 5, 
meaning that "there are relatively stable and enduring political groups which regularly compete for political influence at the national level" (Polity II, p. 18). */ 

/* demaut */
gen demaut = (democ-autoc+10)/20  if (democ>=0 & autoc>=0);
label var demaut "Polity: (democracy score - autocracy score +10)/20";
/* Population */
 gen  lpop=log(pop); label var lpop "log of population: Penn World Tables";
 gen  lpop_WB=log(WB_pop); label var lpop_WB "log of population (World Bank)";

/* Selectorate */
gen s=(Legselec)/2 ; 
gen S=s; tab S; 
label var S "Selectorate size";
/* Coalition Size */ 
gen W=0; 
replace W=W+1 if (xrcomp>=2);
replace W=W+1 if (xropen>2);
replace W=W+1 if parcomp==5;
gen W_extend = W; 
replace W=W + 1 if (RegimeType~=. & RegimeType~=2 & RegimeType~=3);
replace W=W/4; replace W=. if year>1999; 
label var W  " Winning Coalition size";

/* W_mod */ 
gen W_mod=1; 
replace W_mod=W_mod+1 if (xrcomp>=2);
replace W_mod=W_mod+1 if (xropen>2);
replace W_mod=W_mod+1 if parcomp==5;
replace W_mod=W_mod-1 if (xrreg==1 |xrreg==2);
replace W_mod=W_mod/4; 
label var W_mod "Alternative coding for Winning Coalition Size";

/* Length of a Polity : create a code for polity: a polity is a nation until its institutions change*/
 sort ccode year; gen polityW=0;  qui by ccode: replace polityW=1 if _n==1; qui by ccode: replace polityW=1 if W~=W[_n-1];
 qui replace polityW=sum(polityW);  sort polityW; qui by polityW: gen polityWn=_N; 

/* Other institutions */ 
gen Parl_Pres=demaut==1;
replace Parl_Pres=Parl_Pres*INST;
replace Parl_Pres=. if demaut==.|INST==.;
/* Parl_Pres provides a way to test the parliamentary/presidential conjecture */


 gen w2=W^2; label var w2 "Winning Coalition (W) squared";
 gen demaut2=demaut^2; label var demaut2 "Democracy measure (demaut) squared";
 gen WoverS=W/(log((s+1)*10)/3);  label var WoverS "W/S: transformed to avoid division by zero: =W/(log((s+1)*10)/3)";

 gen dbigw=demaut>W if demaut~=. & W ~=.; label var dbigw "demaut is bigger than W";
  gen dW=W-W[_n-1] if W~=. & W[_n-1]~=.;


 tsset ccode year; 
 gen dw30 = W-L3.W;  label var dw30 "Change in W: W minus W at t-3";
 gen lagW=L.W;   label var lagW "lagged W";
 gen lagW2=lagW^2; label var lagW2 "lagged W squared";
 gen dwos30=WoverS-L3.WoverS; label var dwos30 "change in WoverS: today versus three years earlier";





/* Growth Penn World Tables */
 /* Warning: OMA (ccode 698) has a grow rate of 180% in 1968-- note gdp is only listed for a few years: -- I am prepared to drop this !!!*/
 drop if ccode ==698 & year==1968; 

qui  gen grow=(rgdpc-L.rgdpc)/L.rgdpc if year>=1950 ;
gen LongGR1= (F2.rgdpc-L2.rgdpc)/L2.rgdpc; 
gen LongGR2=(rgdpc-L5.rgdpc)/L5.rgdpc; 
gen LongGR3=(0.25*F2.grow+0.5*F1.grow+grow+0.5*L1.grow+0.25*L2.grow)/2.5;
gen LongGR4=(F4.rgdpc-rgdpc)/rgdpc;
gen lagGrow=L.grow; 

gen lrgdpc=log(rgdpc); label var lrgdpc "log GDP pc: PWT";
gen laglrgdpc=L.lrgdpc; label var laglrgdpc "lagged log of GDP pc PWT";
gen lgdpWB=log(WB_gdppc_constUS95); label var lgdpWB "log GDP pc: WBDI 2001";
gen laglgdpWB=L.lgdpWB; label var laglgdpWB "lagged log of GDP pc: WBDI 2001";


/* Variance in the growth rate: in addition to large W having a higher mean, we expect them to be consistent: leaders in small W have discretion and might luckily stumble on good policies etc.. */
 sort polityW year; egen VarGrow=sd(grow),  by(polityW);  sort polityW; gen temp=(grow~=.); qui  egen PolWobs=sum(temp), by(polityW);
 gen VarGrow_=VarGrow if PolWobs>=4;  drop temp PolWobs; label var VarGrow_ "Variance in growth rate (PWT growth)";
 sort ccode year; tsset ccode year;


/* Growth World Development Indicators 2001 (World Bank) */
sum grow WB_growth ; corr grow WB_growth;



 /* PRIVATE GOODS */
 gen build=knres+kres+kother;
 label var build "noresidential+residential and other construction: WBDI ";


 /* EXPENDITURES, REVENUES AND KLEPTOCRACY */
 gen bal_bud = abs((NatGovRevenue- NatGovExpend)/NatGovRevenue);
 label var bal_bud "unBalanced Budget: abs((NatGovRevenue- NatGovExpend)/NatGovRevenue)"; 
/* this should be heteroscedastic with greater variance for small W */
 sort polityW year; qui egen VarBud=sd(bal_bud),  by(polityW); 
   gen temp=(bal_bud~=.); qui  egen PolWobs=sum(temp), by(polityW);
 gen Var_bud=VarBud if PolWobs>=4; drop temp PolWobs;

 gen Klepto=(NatGovRevenue- NatGovExpend)/NatGovRevenue;
 label var Klepto "Kleptocracy? surplus funds not spend by leader";


 /* government revenue/gdp)*/
 gen nexcap=NatGovExpend/pop;   /* longer time series */
label var nexcap "percapita government expenditure in US$";

 tsset , clear ; compress;
 save bdm2s2_nation_year_data_may2002 , replace;
erase temp1.dta; erase WBtemp.dta; erase tempDataVine.dta;



/*******************************************************/
/* We do not have permission to distribute Banks data */
/* We replace data values with missing data 		*/
/******************************************************/
#delimit ; 
replace Radios=.; replace BANKScode=.; 
drop Newspapers; replace Assinations =.;replace GenStrikes =.; replace GovCrises =.;
replace Purges =.; replace Riots =.; replace Revolutions=.; replace AntiGovDem=.; 
replace ConflictIndex=.; replace RegimeType=.; replace Coups=.; replace Guerilla=.;  
replace HeadofState=.; replace RegisteredVoters=.; replace MajConstChg=.; replace Premier=.;  
replace Legselec=.; replace NatGovRevenue=.; replace NatGovExpend=.; replace defenseovertotal=.; 
save bdm2s2_nation_year_data_may2002_webversion , replace;

 



