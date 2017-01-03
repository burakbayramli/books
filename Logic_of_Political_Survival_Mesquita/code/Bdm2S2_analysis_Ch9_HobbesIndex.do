/********************************************************************/
/* Bdm2S2: CHAPTER 9 ANALYSIS: Hobbes Index *************************/
/* 8/5/2002: Alastair's modification of Bruce's orginal file ********/
/* The Unit of Observation is Nation Year ***************************/
/********************************************************************/

clear
set mem 50m
#delimit ;
cd c:\bdm2s2\data\May2002; 

use bdm2s2_nation_year_data_may2002 ;

global logfile "output\Bdm2S2_ch9_hobbes.log,replace";
capture log close; 
set matsize 800; set more off;


capture log close;log using $logfile;
gen cl=7-CL;
regr demaut W; predict demres, residual; label var demres "Residual democracy not accounted for by W";
gen TaxYN=(TAXGDP~=.); replace TaxYN=. if year>1990|year<1970;
gen pr=7-PR; regr pr W s ; predict prres,residual;


egen nasty=rank(cl) if year>1971 & year~=.;
des CL LifeExp Death lgdpWB CL_WAR Revolutions Guerilla IS_WAR  ; 
egen mnasty=max(nasty) if year>1971 & year~=.;
replace nasty=(nasty/mnasty)*100;
egen short=rank(Deathrate) if year>1971 & year~=.;
egen mshort=max(short) if year>1971 & year~=.;
replace short=(short/mshort)*100;
replace short=-1*(short-100);
egen poor=rank(lgdpWB) if year>1971 & year~=.;
egen mpoor=max(poor) if year>1971 & year~=.;
replace poor=((poor)/(mpoor))*100;
gen cw1=CL_WAR;
replace cw1=0 if CL_WAR==.;
egen cw2=rank(cw1) if year>1971 & year~=.;
egen mcw2=max(cw2) if year>1971 & year~=.;
egen mincw2=min(cw2) if year>1971 & year~=.;
replace cw1=((cw2-mincw2)/(mcw2-mincw2))*100;
gen rev=Revolutions;
replace rev=0 if Revolutions==.;
egen rev1=rank(rev) if year>1971 & year~=.;
egen mrev1=max(rev1) if year>1971 & year~=.;
egen minrev1=min(rev1) if year>1971 & year~=.;
replace rev=((rev1-minrev1)/(mrev1-minrev1))*100;
replace IS_WAR=0 if IS_WAR==. & year<=1997;
egen cow=rank(IS_WAR) if year>1971 & year~=.;
egen mcow=max(cow) if year>1971 & year~=.;
egen mincow=min(cow) if year>1971 & year~=.;
replace cow=((cow-mincow)/(mcow-mincow))*100;
gen brute1=cow+rev+cw1;
egen brute=rank(brute1) if year>1971 & year~=.;
egen mbrute=max(brute) if year>1971 & year~=.;
egen minbrute=min(brute) if year>1971 & year~=.;
replace brute=((brute-minbrute)/(mbrute-minbrute))*100;
replace brute=-1*(brute-100);
drop mnasty mpoor mbrute brute1 mshort;

gen immigrep=immig~=.; replace immigrep=. if year>1993; label var immigrep "Immigration data available (0/1)";
gen IMM=immig/WB_pop; label var IMM "immigration/population by country year";
gen alone=immigrep*immig if year>1971 & year~=.;
replace alone=0 if immigrep==0 & year>1971 & year~=.;
egen solitary=rank(alone) if year>1971 & year~=.;
egen msolitary=max(solitary) if year>1971 & year~=.;
egen minsolitary=min(solitary) if year>1971 & year~=.;
replace solitary=((solitary-minsolitary)/(msolitary-minsolitary))*100 if year>1971 & year~=.;
/*Alternatives for solitary ??? Radios newspapers*/

des Radios Newspapers; 
egen soliRad=rank(Radios) if year>1971 & year~=.;
egen msoliRad=max(soliRad) if year>1971 & year~=.;
egen minsoliRad=min(soliRad) if year>1971 & year~=.;
replace soliRad=((soliRad-minsoliRad)/(msoliRad-minsoliRad))*100 if year>1971 & year~=.;
corr soliRad solitary; 
gen sol1=(soliRad+solitary)/2;



gen Hobbes=(poor+nasty+short+brute+solitary)/5;
label var Hobbes "Hobbes Index: Life is Nasty, Brutish, Solitary and Short";

gen parl=1 if Parl_Pres==1;
replace parl=0 if Parl_Pres~=1;
replace parl=. if Parl_Pres==.;
gen mixed=1 if Parl_Pres==2;
replace mixed=0 if Parl_Pres~=2;
replace mixed=. if Parl_Pres==.;
gen pres=1 if Parl_Pres==3;
replace pres=0 if Parl_Pres~=3;
replace pres=. if Parl_Pres==.;
xtreg Hobbes W s , fe i(regyr);
xtreg Hobbes W s lpop_WB , fe i(regyr);
xtreg Hobbes W s lpop_WB demres parl mixed pres, fe i(regyr);
xtreg Hobbes W s lpop_WB demres parl mixed pres, fe i(ccode);
xtreg Hobbes W s lpop_WB WB_growth , fe i(regyr);
xtreg Hobbes W s demres, fe i(regyr);
xtreg Hobbes W s lpop_WB demres, fe i(regyr);
xtreg Hobbes W s lpop_WB WB_growth demres, fe i(regyr);
xtreg Hobbes W s demres parl mixed pres, fe i(regyr);
xtreg Hobbes W s lpop_WB WB_growth demres parl mixed pres , fe i(regyr);
xtreg Hobbes W s lpop_WB WB_growth demres parl mixed pres TaxYN, fe i(regyr);
xtreg Hobbes W s lpop_WB WB_growth demres parl mixed pres TaxYN prres, fe i(regyr);

tsset ccode year; gen L3W=L3.W; gen L3S=L3.S;tsset, clear;
xtreg Hobbes dw30 L3W L3S WB_growth demres parl mixed pres TaxYN prres, fe i(regyr);
reg open W S; predict openres, residual; 
reg bhkavg W S; predict laboredr, residual;
xtreg Hobbes W s lpop_WB WB_growth demres parl mixed pres TaxYN prres openres laboredr, fe i(regyr);
xtreg exchprem ELF60 , fe i(regyr);
xtreg ELF60 W demaut , fe i(regyr);
xtreg ELF60 WoverS demaut, fe i(regyr);
log close;
