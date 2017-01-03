/*   Change point problem.

  The Nenana river in the Interior of Alaska usually freezes 
  over during October and November. The ice continues to grow throughout 
  the winter accumulating an average maximum thickness of about 110 cm, 
  depending upon winter weather conditions. The Nenana River Ice Classic 
  competition began in 1917 when railroad engineers bet a total of 800 dollars, 
  winner takes all, guessing the exact time (month, day, hour, minute) ice 
  on the Nenana River would break up. Each year since then, Alaska 
  residents have guessed at the timing of the river breakup. A tripod, 
  connected to an on-shore clock with a string, is planted in two feet 
  of river ice during river freeze-up in October or November. 
  The following spring, the clock automatically stops when the tripod 
  moves as the ice breaks up. The time on the clock is used as the river 
  ice breakup time. Many factors influence the river ice breakup, 
  such as air temperature, ice thickness, snow cover, wind, water 
  temperature, and depth of water below the ice. Generally, the Nenana 
  river ice breaks up in late April or early May (historically, April 20 to May 20). 
  The time series of the Nenana river ice breakup dates can be used to 
  investigate the effects of climate change in the region.

  In 2010, the jackpot was almost $300,000 and the ice went out at 
  9:06 on 2010-04-29. In 2012, the jackpot was over $350,000 
  and the ice went out at 19:39 on 2012-04-23 - 
  as reported at http://www. cbc.ca/news/offbeat/story/2012/05/02/alaska-ice-contest.html. 
  The latest winner, Tommy Lee Waters, has also won twice before, 
  but never has been a solo winner. Waters spent time drilling holes in 
  the area to measure the thickness of the ice. Altogether he spent 
  $5,000 on tickets for submitting guesses (he purchased every minute of the 
  afternoon of 23 April) and spent an estimated 1,200 hours working out 
  the math by hand. And, it was also his birthday! (What are the odds?) 
  You too can use statistical methods to gain fame and fortune!

  More details about the Ice Classic are available at 
  http://www.nenanaakiceclassic.com. */

/* Lines starting with *---partxxxe and *---parxxxb are used in my Latex file and 
   should be ignored */
/* The tagsets.tablesonlylatex again is used by the Latex Program course notes */

dm 'output' clear;
dm 'log'    clear;
proc datasets kill; run;

title 'Nenana Ice Classic';
options orientation=landscape;
ods pdf file='nenana-SAS.pdf';
goptions device=pdf color=(black) rotate=landscape;

*---part010b;
proc import file='nenana.csv' dbms=csv out=nenana replace;
run;
*---part010e;

proc print data=nenana(obs=8);
   title2 'part of the raw data';
run;

/* plot of the raw data */
proc sgplot data=nenana;
   title2 'plot of the raw counts';
   scatter x=day y=count / group=trt;
   xaxis label='Day';
   yaxis label='Count';
run;

/* plot the data and fit a simple regression */
proc sgplot data=nenana;
   title2 'Ice breakup time by year';
   series x=Year y=Julian_date;
   reg    x=Year y=Julian_date;
run;

ods graphics on;
proc reg data=nenana plot=all;
   model Julian_date = Year;
run;
ods graphics off;




/* We fit a change point model where the change point is 
   assumed to be 1970 */
 
*---part011b;
data nenana;
   set nenana;
   year1970p = max(0, year-1970);
run;
*---part011e;

*---part020b;
ods graphics on;
proc reg data=nenana plots=all;
   title2 'Fit a change point model with known change point in 1970';
   model Julian_date = year year1970p;
   ods output ParameterEstimates=est1970cp;
   output out=predfit pred=pred;
run;
ods graphics off;
*---part020e;

ods document name=cp1970(write);
proc sgplot data=predfit;
   title2 'Fitted change point (1970) model';
   scatter x=Year y=Julian_date;
   series  x=Year y=pred;
run;
ods document close;


/* Fit a model searcing for the change point */

ods document name=nlin40(write);
*---part040b;
/* Fit a changepoint model */
proc nlin data=nenana;
   title2 'Change point model with change point unknown';
   parms CP=1970 beta0=150 beta1=-.1 beta2=-.1;
   if (Year < CP) then 
        mean = beta0 + beta1*Year;
   else mean = beta0 + beta1*Year +beta2*(Year-CP);
   model Julian_Date = mean;
   output out=model_fit predicted=pred;
   ods output ParameterEstimates=Nlin_est; 
run;
*---part040e;
ods document close;

ods document name=plot40(write);
proc sgplot data=model_fit;
   title2 'Fitted model';
   scatter x=Year y=Julian_Date;
   series  x=Year y=pred;
run;
ods document close;


ods pdf close;


/* create the files to be included in the LaTex document */

/* Create LaTeX files for inclusion by my notes */
%include "../../MyLatexTagset.sas"; run;
title;
footnote;
ods listing;

proc document name=nlin40;
   list /levels=all;
run;


ods tagsets.mycolorlatex file='nenana-SAS-010.tex' (notop nobot);
proc print data=nenana(obs=10) label split=" ";
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='nenana-SAS-020.tex' (notop nobot);
proc print data=est1970cp label split=" ";
run;
ods tagsets.mycolorlatex close;

ods listing;
ods graphics on / imagefmt=png imagename='nenana-SAS-cp1970' reset=index;
proc document name=cp1970;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;


ods tagsets.mycolorlatex file='nenana-SAS-040.tex' (notop nobot);
proc print data=Nlin_est label split=" ";
run;
ods tagsets.mycolorlatex close;

ods listing;
ods graphics on / imagefmt=png imagename='nenana-SAS-040' reset=index;
proc document name=plot40;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;




