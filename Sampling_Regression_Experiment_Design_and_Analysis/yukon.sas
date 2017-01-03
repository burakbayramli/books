/* Regression Trend over time
   2014-05-23 CJS First Edition 

As reported by CBC (\url{http://www.cbc.ca/news/canada/north/yukon-ice-breakup-betting-could-help-climate-researchers-1.622521}):
\begin{quote}
Climate change researchers should scour the110-year-old Dawson City Ice Pool 
contest records for evidence of global warming, an Alaskan scientist says.

Since 1896, people in the Klondike capital have placed bets on when 
the ice on the Yukon River will start to move out.

By examining the winning bets, researchers could find out valuable 
information about the territory's changing climate, said Martin Jeffries, 
who works at the Geophysical Institute in Fairbanks, Alaska.

``[In a] gambling competition for ice breakup, there is clearly something, 
there is scientifically-useful information,'' Jeffries said in a telephone interview Monday.

``I think it would be very interesting to look at the Dawson records. 
I think somebody should go ahead and do it.''

An analysis of results from an ice breakup-guessing contest on a 
tributary of the Yukon River, the Tanana, produced clear evidence of a changing climate, he said.

Although the timing of the Tanana breakup didn't change much between 1917 and 1960,
since then the records show the ice started disappearing sooner, he said.

``Breakup has become progressively earlier,'' he said.
``All I can say is you are probably safe to bet earlier rather than late.''

The Dawson City Ice Pool is organized by the Imperial Order of the Daughters of the Empire.
\end{quote}

Details about the break-up on the Yukon River are available at
\url{http://www.yukonriverbreakup.com}. As the focus of a vigorous betting tradition, 
the exact time and date of breakup has been recorded annually since 1896. 
These breakup times note the moment when the ice moves the tripod on the Yukon River at Dawson.

A tripod is set up on which is connected by cable to the Danoja Zho Cultural Centre. 
When the ice starts moving, it takes the tripod with it and stops the clock, thereby recording the official break up time.
Here is a picture of the tripod taken from the official site:
includegraphics[]{../MyPrograms/Reg/YukonBreakup/YukonRiverTripod.jpg}


/* Lines starting with *---partxxxe and *---parxxxb are used in my Latex file and 
   should be ignored */
/* The tagsets.tablesonlylatex again is used by the Latex Program course notes */

dm 'output' clear;
dm 'log'    clear;
proc datasets kill; run;

title 'Yukon River Breakup';
options orientation=landscape;
ods pdf file='yukon-SAS.pdf' style=styles.printer;

*---part010b;
proc import file='yukon.csv' dbms=csv out=yukon replace;
run;

/* Convert the date and time to julian date */
data yukon;
   set yukon;
   length temp $20.;
   temp =  scan(date,2) || '-' || substr(scan(date,1),1,3) || '-' || put(year,4.0) || ' ' || time;
   fulldate = input(temp, datetime20.);
   jdate = intck('second', dhms( mdy( 01, 01, year ), 0, 0, 0 ), fulldate )/ 3600/24;
   format fulldate datetime20.;
   drop temp;
   attrib jdate label='Breakup Date' format=7.2;
run;
*---part010e;

proc print data=yukon(obs=8);
   title2 'part of the raw data';
run;

/* plot of the raw data */
/* plot the data and fit a simple regression */
ods document name=prelimplot(write);
*---partprelimplotb;
proc sgplot data=yukon;
   title 'Breakup Date of the Yukon River';
   series x=year y=jdate;
   xaxis label='Year';
   yaxis label='Julian Date of Breakup';
run;
*---partprelimplote;
ods document close;

ods document name=regoutput(write);
*---partregfitb;
ods graphics on;
proc reg data=yukon plot=all;
   model jdate = Year / dwprob clm cli clb;
   output out=predfit pred=pred lclm=lclm uclm=uclm lcl=lcli ucl=ucli ;
   ods output ParameterEstimates=coef;
   ods output dwstatistic=dwstatistic;
   ods output fitstatistics=fitstatistics;
run;
ods graphics off;
*---partregfite;
ods document close;

/* plot of the raw data */
/* plot the data and fit a simple regression */
ods document name=regplot(write);
proc sgplot data=yukon;
   title 'Breakup Date of the Yukon River';
   series x=year y=jdate;
   reg    x=year y=jdate;
   xaxis label='Year';
   yaxis label='Julian Date of Breakup';
run;
ods document close;


ods pdf close;


/* create the files to be included in the LaTex document */

/* Create LaTeX files for inclusion by my notes */
%include "../../MyLatexTagset.sas"; run;
title;
footnote;
ods listing;

proc document name=regoutput;
   list /levels=all;
run;


ods listing;
ods graphics on / imagefmt=png imagename='yukon-SAS-prelimplot' reset=index;
proc document name=prelimplot;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;


ods tagsets.mylatex file='yukon-SAS-010.tex' (notop nobot);
proc print data=yukon(obs=10) label split=" ";
run;
ods tagsets.mylatex close;

ods tagsets.mylatex file='yukon-SAS-regfit.tex' (notop nobot);
proc print data=coef label split=" " noobs;
run;
ods tagsets.mylatex close;

ods tagsets.mylatex file='yukon-SAS-dwtest.tex' (notop nobot);
proc print data=dwstatistic label split=" ";
   var label1 nvalue1;
   label label1='Statistic';
   label nvalue1='Value';
run;
ods tagsets.mylatex close;

ods listing;
ods graphics on / imagefmt=png imagename='yukon-SAS-diagplot' reset=index;
proc document name=regoutput;
   replay  	\Reg#1\MODEL1#1\ObswiseStats#1\jdate#1\DiagnosticPlots#1\DiagnosticsPanel#1/ dest=listing;
run;
ods graphics off;
ods listing close;

ods listing;
ods graphics on / imagefmt=png imagename='yukon-SAS-predci' reset=index;
proc document name=regoutput;
   replay  	Reg#1\MODEL1#1\ObswiseStats#1\jdate#1\FitPlot#1/ dest=listing;
run;
ods graphics off;
ods listing close;

ods tagsets.mylatex file='yukon-SAS-predci-values.tex' (notop nobot);
proc print data=predfit(obs=5) label split=" " noobs;
   var year pred lclm uclm lcli ucli;
run;
ods tagsets.mylatex close;

ods tagsets.mylatex file='yukon-SAS-RMSE.tex' (notop nobot);
proc print data=fitstatistics noobs label split=' '; ;
   var label1 nvalue1 label2 nvalue2;
   label label1='Statistic';
   label label2='Statistic';
   label nvalue1='Value';
   label nvalue2='Value';
run;
ods tagsets.mylatex close;
