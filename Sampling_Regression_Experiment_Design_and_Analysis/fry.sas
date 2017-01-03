
/* In this survey, several locations were selected from a location.
   At each location, several sites were selected at random.
   At each site-location combination, fry density was measured for
   a number of years.
*/

dm 'output' clear;
dm 'log'    clear;
proc datasets kill; run;



title 'Examination of differences in fry density over time';
options noovp nodate orientation=landscape;
ods pdf file='fry.pdf';
goptions device=pdf colors=(black) rotate=landscape;

*---part001b;
data fry;
   infile 'fry.csv' dlm=',' dsd missover firstobs=2 expandtabs;
   input location $ site $ y2000 y2001 y2002 y2003 y2004;  /* read in the data */
   year = 2000; fry=y2000; output;  /* convert the data to a list */
   year = 2001; fry=y2001; output;
   year = 2002; fry=y2002; output;
   year = 2003; fry=y2003; output;
   year = 2004; fry=y2004; output;
   keep location site year fry;
run;
data fry;   /* compute the log(fry) */
   set fry;
   lfry = log(fry);
   attrib lfry label='log(fry)' format=7.2;
   location_site = compress(location || '.' || site);
run;
*---part001e;

proc print data=fry;
   title2 'raw data';
run;

/* Some preliminary plots */

ods document name=plot1(write);
*---part005b;
proc sgplot data=fry;
   title2 'Preliminary plot';
   series y=fry x=year / group=location_site;
   xaxis offsetmin=.05 offsetmax=0.05;
run;
*---part005e;
ods document close;

ods document name=plot2(write);
*---part005bb;
proc sgplot data=fry;
   title2 'Preliminary plot of log(fry)';
   series y=lfry x=year / group=location_site;
   xaxis offsetmin=.05 offsetmax=0.05;
run;
*---part005be;
ods document close;




/***** Analysis I. Average over subsamples and do a simple RCB. */
/* Note that Time cannot be randomized so there is always the
   possibility that things go wrong */
*---part020b;
proc sort data=fry;
   by year location;
run;
proc means data=fry noprint;
   by year location;
   var lfry;
   output out=meanlfry mean=meanlfry;
run;
*---part020e;
 
proc print data=meanlfry;
   title2 'data after averaging over sites within a location-year combination';
run;

 
ods document name=mixed1(write);
*---part030b;
ods graphics on;
proc mixed data=meanlfry plots=all;
   title2 'approximate analysis of mean(log fry) using a RCB analysis';
   class location year;
   model meanlfry = location year / ddfm=kr;
   lsmeans  year / cl adjust=tukey;
   estimate '2004 vs prev years' year -.25 -.25 -.25 -.25 1 / cl;  /* compare avg of prev 4 years to 2004 */
   ods output tests3 =Mixed1Tests;
   ods output lsmeans=Mixed1LSmeans;
   ods output diffs  =Mixed1Diffs;
   ods output estimates=Mixed1Estimates;
run;
ods graphics off;
*---part030e;
ods document close;


/* Get a joined lines plot */
*---part040b;
%include '../../pdmix800.sas';
%pdmix800(mixed1diffs,mixed1lsmeans,alpha=0.05,sort=yes);
*---part040e;
run;
   

/********* Analysis II ******** Analyze the raw values */
/* The results differ from JMP because of the satterthwaite approximation
   for the degrees of freedom done here that is not available in JMP */


ods document name=mixed2(write);
*---part130b;
ods graphics on; 
proc mixed data=fry plots=all;
   title2 'analysis on log(fry) counts using individual values';
   class location year site;
   model lfry = location year / ddfm=satterth;
   random site(location);
   lsmeans year / cl adjust=tukey;  /* estimate year changes on log scale */
   estimate '2004 vs prev years' year -.25 -.25 -.25 -.25 1 / cl;  /* compare avg of prev 4 years to 2004 */
   ods output tests3 =Mixed2Tests;
   ods output lsmeans=Mixed2LSmeans;
   ods output diffs  =Mixed2Diffs;
   ods output covparms=Mixed2CovParms;
   ods output estimates=Mixed2Estimates;
run;
ods graphics off;
*---part130e;
ods document close;


/* Get a joined lines plot */
*---part140b;
%include '../../pdmix800.sas';
%pdmix800(mixed2diffs,mixed2lsmeans,alpha=0.05,sort=yes);
*---part140e;





/******* Analysis III ****** Analyze the raw values but use an AR(1) structure to
 see if there is evidence of autocorrelation over time */

ods graphics on;
proc mixed data=fry plots=all;
   title2 'analysis on log(fry) counts using individual values';
   class location year site;
   model lfry = location year / ddfm=satterth;
   repeated year / type = ar(1) r subject=site(location);
   lsmeans year / cl adjust=tukey;  /* estimate year changes on log scale */
   estimate '2004 vs prev years' year -.25 -.25 -.25 -.25 1 / cl;  /* compare avg of prev 4 years to 2004 */
run;
ods graphics off;



/* Do a simple power analysis */
ods document name=power20(write);
*---part160b;
proc power;
  title 'Power analysis for fry example';
  /* We vary the size of the difference to see what sample size is needed */
  twosamplemeans
     test=diff     /* indicates that you wish to test for differences in the mean */
     meandiff=0.22   /* size of difference to be detected */  
     stddev=0.62    /* the standard deviation within each group */
     power=.80     /* target power of 80% */
     alpha=.05     /* alpha level for the test */
     sides=2       /* a two sided test for difference in the mean should be done */
     ntotal=.      /* solve for the total sample size assuming equal sample sizes in both groups */
  ;                /* end of the twosamplemeans statement - DON'T FORGET THIS */  
  ods output output=power20;
run;
*---part160e;
ods document close;


ods pdf close;


/* Now to create the various outputs for my LaTeX files */

/* Create LaTeX files for inclusion by my notes */
%include "../../MyLatexTagset.sas"; run;
title;
footnote;
ods listing;

proc document name=plot1;
   list /levels=all;
run;

ods tagsets.mycolorlatex file='fry-SAS-001.tex' (notop nobot) stylesheet="sas.sty";
proc print data=fry(obs=10);
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='fry-SAS-020.tex' (notop nobot);
proc print data=meanlfry(obs=10 drop=_type_ _freq_);
run;
ods tagsets.mycolorlatex close;

ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='fry-SAS-005' reset=index;
proc document name=plot1;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;

ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='fry-SAS-005b' reset=index;
proc document name=plot2;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;




ods tagsets.mycolorlatex file='fry-SAS-030.tex' (notop nobot);
proc print data=Mixed1Tests noobs label split=" ";
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='fry-SAS-040.tex' (notop nobot);
proc print data=Mixed1LSmeans noobs label split=" ";
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='fry-SAS-045a.tex' (notop nobot);
proc print data=Mixed1Diffs(drop=df tvalue probt lower upper) noobs label split=" " ;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='fry-SAS-045b.tex' (notop nobot);
%pdmix800(mixed1diffs,mixed1lsmeans,alpha=0.05,sort=yes);
ods tagsets.mycolorlatex close;

ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='fry-SAS-050' reset=index;
proc document name=mixed1;
   replay \Mixed#1\ResidualPlots#1\ResidualPanel#1 / dest=listing;
run;
ods graphics off;
ods listing close;




ods tagsets.mycolorlatex file='fry-SAS-130.tex' (notop nobot);
proc print data=Mixed2Tests noobs label split=" ";
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='fry-SAS-135.tex' (notop nobot);
proc print data=Mixed2CovParms noobs label split=" ";
run;
ods tagsets.mycolorlatex close;


ods tagsets.mycolorlatex file='fry-SAS-140.tex' (notop nobot);
proc print data=Mixed2LSmeans noobs label split=" ";
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='fry-SAS-145a.tex' (notop nobot);
proc print data=Mixed2Diffs(drop=df tvalue probt lower upper) noobs label split=" " ;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='fry-SAS-145b.tex' (notop nobot);
%pdmix800(mixed2diffs,mixed2lsmeans,alpha=0.05,sort=yes);
ods tagsets.mycolorlatex close;

ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='fry-SAS-150' reset=index;
proc document name=mixed2;
   replay \Mixed#1\ResidualPlots#1\ResidualPanel#1 / dest=listing;
run;
ods graphics off;
ods listing close;


ods tagsets.mycolorlatex file='fry-SAS-160.tex' (notop nobot);
proc print data=power20 /* noobs split=' ' label */;
  var alpha meandiff stddev sides nulldiff nominalpower power Ntotal;
run;
ods tagsets.mycolorlatex close;



