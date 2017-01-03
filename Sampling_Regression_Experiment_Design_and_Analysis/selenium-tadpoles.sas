/* Does Selenium levels affect tadpole deformity rates?


Selenium (Se) is an essential element required for the health of humans,
animals and plants, but becomes a toxicant at elevated concentrations. The
most sensitive species to selenium toxicity are oviparous (egg-laying) animals.
Ecological impacts in aquatic systems are usually associated with teratogenic
effects (deformities) in early life stages of oviparous biota as a result of maternal
sequestering of selenium in eggs. In aquatic environments, inorganic selenium,
found in water or in sediments is converted to organic selenium at the base of
the food chain (e.g., bacteria and algae) and then transferred through dietary
pathways to other aquatic organisms (invertebrates, fish). Selenium also tends
to biomagnify up the food chain, meaning that it accumulates to higher tissue
concentrations among organisms higher in the food web.
Selenium often occurs naturally in ores and can leach from mine tailings.
This leached selenium can make its way to waterways and potentially contaminate
organisms.
As a preliminary survey, samples of tadpoles were selected from a control site
and three sites identified as low, medium, and high concentrations of selenium
based on hydro-logic maps and expert opinion. These tadpoles were examined,
and the number that had deformities were counted.  */

/* 2015-07-08 CJS Update for latest version of SAS etc */
dm 'output' clear;
dm 'log'    clear;
proc datasets kill; run;
title; footnote; run;
ods graphics on;


title 'Effect of selenium levels on tadpole deformity rates';
ods pdf file='selenium-tadpoles-SAS.pdf' style=styles.printer;
 
*---part001b;
data tadpoles;
    infile 'selenium-tadpoles.csv' dlm=',' dsd missover firstobs=2;
    length selenium $8 status $12;
    input selenium $ tot_tadpoles status $ count;
run;
*---part001e;

proc print data=tadpoles;
   title2 'raw data';
run;

*---part020b;
/* In order to make a mosaic chart, we need to compute the percents outselves*/
proc sort data=tadpoles;
   by selenium;
run;
proc means data=tadpoles noprint;
   by selenium;
   var count;
   output out=totalcount sum=totalcount;
run;
data tadpoles;
   merge tadpoles totalcount;
   by selenium;
   percent = count / totalcount * 100;;
   format percent 7.0;
   drop _type_ _freq_ totalcount;
run;
proc print data=tadpoles;
   title2 'percents computed';
run;
*---part020e;

ods document name=chart1(write);
*---partmosaicb;
/* create the mosaic plot using side-by-side segmented bar chart */
/* In order to have the selenium levels ordered properly, I need to create
   a numeric variable with the proper order and a suitable format */
/* refer to http://blogs.sas.com/content/graphicallyspeaking/2014/08/03/legend-order-in-sgplot-procedure/?utm_source=feedburner&utm_medium=feed&utm_campaign=Feed%3A+tzio+%28Graphically+Speaking%29 */

data tadpoles2;
   set tadpoles;
   if      selenium = 'Control' then mytype=1;
   else if selenium = 'low'     then mytype=2;
   else if selenium = 'medium'  then mytype=3;
   else if selenium = 'high'    then mytype=4;
run;
 
proc sort data=tadpoles2 out=sorttadpoles2;
   by mytype;
run;
 
data myfmt;
  set sorttadpoles2;
  by mytype;
  if first.mytype then do;
    fmtname='typefmt';
    start=mytype;
    label=selenium;
    drop selenium;
    output;
  end;
run;
 
proc format cntlin=myfmt;
run; 

proc sgplot data=tadpoles2;
   title2 'Side-by-side segmented barcharts';
   vbar mytype / group=status 
                response=percent
                groupdisplay=stack
                stat=sum ;
   format mytype typefmt.;
run;
*---partmosaice;
ods document close;

ods document name=freq1(write);
*---partchisqb;
/* Chi-square test and computing the odds ratio */
proc freq data=tadpoles;
   title2 'Chi-square test';
   table selenium*status / nopercent nocol chisq measures cl;
   weight count;
   ods output relativerisks=chirelrisk;
   ods output chisq        =chitest;
run;
*---partchisqe;
ods document close;





ods document name=genmod1(write);
*---partgenfitb;
/********* using GENMOD ********/ 
proc genmod data=tadpoles ;
   class selenium;
   model status = selenium / dist=binomial link=logit  type3;
   lsmeans selenium / cl diff adjust=tukey lines ilink oddsratio;
   freq count;
   ods output parameterestimates=genmodest;
   ods output type3             =genmodtest;
   ods output diffs             =genmodlsmeandiff;
   ods output lsmeans           =genmodlsmeans;
run;
*---partgenfite;
ods document close;

proc print data=genmodlsmeans; run;
 
/* Make a plot of the marginal means */
/* We again want to sort these properly */
data genmodlsmeans2;
   set genmodlsmeans;
   if      selenium = 'Control' then mytype=1;
   else if selenium = 'low'     then mytype=2;
   else if selenium = 'medium'  then mytype=3;
   else if selenium = 'high'    then mytype=4;
run;
 
proc sort data=genmodlsmeans2 out=sortgenmodlsmeans2;
   by mytype;
run;
 
data myfmt;
  set sortgenmodlsmeans2;
  by mytype;
  if first.mytype then do;
    fmtname='typefmt';
    start=mytype;
    label=selenium;
    drop selenium;
    output;
  end;
run;
 
proc format cntlin=myfmt;
run; 
proc sort data=genmodlsmeans2; by mytype; run;
ods document name=chart2(write);
proc sgplot data=genmodlsmeans2;
   title2 'estimated marginal log(odds) deformity';
   scatter x=mytype y=estimate;
   series  x=mytype y=estimate;
   highlow x=mytype high=upper low=lower;
   xaxis integer;
   yaxis label='logit(deformity) with 95% ci';
   format mytype typefmt.;
run; 
ods document close;


ods pdf close;

/* now create the LaTeX files for inclusion in the course notes */
%include "../../MyLatexTagset.sas"; run;
ods listing close;
title;
footnote;
ods listing;

proc document name=genmod1;
 list /levels=all;
run;

ods tagsets.mylatex file='selenium-tadpoles-SAS-data.tex' (notop nobot);
proc print data=tadpoles;
run;
ods tagsets.mylatex close;

/* chisquare test statistics and test results */

ods tagsets.mylatex file='selenium-tadpoles-SAS-freqs.tex' (notop nobot);
proc document name=freq1;
   obstitle \Freq#1\Table1#1\CrossTabFreqs#1 " "; /* delete the title*/
   obtitle \Freq#1\Table1#1\CrossTabFreqs#1 " "; /* delete the title*/
   replay \Freq#1\Table1#1\CrossTabFreqs#1;
run;
ods tagsets.mylatex close;

ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='selenium-tadpoles-SAS-mosaic' reset=index;
proc document name=chart1;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;

ods tagsets.mylatex file='selenium-tadpoles-SAS-chitest.tex' (notop nobot);
proc print data=chitest noobs label split=" ";
   where index(statistic,'Like')>0 or statistic='Chi-Square';
run;
ods tagsets.mylatex close;

ods tagsets.mylatex file='selenium-tadpoles-SAS-chirelrisk.tex' (notop nobot);
proc print data=chirelrisk noobs label split=" ";
run;
ods tagsets.mylatex close;



/* genmod output */
ods tagsets.mylatex file='selenium-tadpoles-SAS-genmodest.tex' (notop nobot);
proc print data=genmodest noobs label split=" ";
run;
ods tagsets.mylatex close;

ods tagsets.mylatex file='selenium-tadpoles-SAS-genmodtest.tex' (notop nobot);
proc print data=genmodtest noobs label split=" ";
run;
ods tagsets.mylatex close;

ods tagsets.mylatex file='selenium-tadpoles-SAS-genmodlsmeandiff.tex' (notop nobot);
proc print data=genmodlsmeandiff noobs label split=" ";
   var effect selenium _selenium estimate stderr;
run;

proc print data=genmodlsmeandiff noobs label split=" ";
   var effect selenium _selenium oddsratio lowerOR upperOR;
run;
ods tagsets.mylatex close;

ods tagsets.mylatex file='selenium-tadpoles-SAS-genmodcld.tex' (notop nobot);
proc document name=genmod1;
   obstitle \Genmod#1\LSMLines#1 " "; /* delete the title*/
   obtitle  \Genmod#1\LSMLines#1 " "; /* delete the title*/
   replay   \Genmod#1\LSMLines#1;
run;
ods tagsets.mylatex close;

ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='selenium-tadpoles-SAS-genmodcldplot' reset=index;
proc document name=chart2;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;



