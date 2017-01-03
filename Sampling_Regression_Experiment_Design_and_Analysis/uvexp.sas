/* Example of analysis of RCB with subsampling */
dm 'output' clear;
dm 'log'    clear;
proc datasets kill; run;

/* A series of flumes were placed across a river. These were 
blocked into sets of three flumes. Each flume within a block
was assigned to Control (normal sunlight), UVA (normal sunlight
plus UVa radiation) or UVAB (normal + UVa + UVb). Five fish
were placed in each flume. After 150 days, the weight gain
of each fish was recorded.  */

title 'Effect of UV radition on weight gain - subsampling in RCB';
options nodate noovp orientation=landscape;
ods pdf file='uvexp.pdf';
goptions device=pdf colors=(black) rotate=landscape;

*---part001b;
data gain;
   infile 'uvexp.csv' dlm=',' dsd missover firstobs=2;
   input block $ trt $ flume $ fishid $ gain;
run;
*---part001e;

proc print data=gain;
   title2 'raw data';
run;
 

/******* Analysis of means over subsamples *******/
*---part020b;
proc sort data=gain; by flume; run;
proc means data=gain noprint;
   by flume;
   var gain;
   output out=mean_gain mean=mgain;
   id block trt ;    /* keep these variables with the dataset */
run;
*---part020e;
 
proc print data=mean_gain;
   title2 'Analysis of averaged values';
   title3 'Average for each flume';
run;
 
ods document name=mixed1(write);
*---part030b;
ods graphics on;
proc mixed data=mean_gain plots=all;
   title3 ' ';
   class block trt;
   model mgain = block trt / ddfm = satterth;
   lsmeans trt / pdiff cl adjust=tukey;
   ods output tests3 =Mixed1Tests;
   ods output lsmeans=Mixed1LSmeans;
   ods output diffs  =Mixed1Diffs;
run;
ods graphics off;
*---part030e;
ods document close;


/* Get a joined lines plot */
*---part040b;
%include '../../pdmix800.sas';
%pdmix800(mixed1diffs,mixed1lsmeans,alpha=0.05,sort=yes);
*---part040e;

 

 
/*********** Analysis of entire data set **********/

ods document name=mixed2(write);
*---part130b;
ods graphics on; 
proc mixed data=gain plots=all;
   title2 'Analysis of all values';
   class block trt fishid;
   model gain = block trt / ddfm=satterth;
   random block*trt;     /* random component for flumes */
   lsmeans trt / diff cl adjust=tukey;
   ods output tests3 =Mixed2Tests;
   ods output lsmeans=Mixed2LSmeans;
   ods output diffs  =Mixed2Diffs;
   ods output covparms=Mixed2CovParms;
run;
ods graphics off;
*---part130e;
ods document close;


/* Get a joined lines plot */
*---part140b;
%include '../../pdmix800.sas';
%pdmix800(mixed2diffs,mixed2lsmeans,alpha=0.05,sort=yes);
*---part140e;



proc mixed data=gain;  /* to demonstrate that you can specify the flume effect in a different way */
   title2 'Analysis of all values';
   class block trt fishid flume;
   model gain = block trt / ddfm=satterth;
   random flume(trt);     /* random component for flumes */
   lsmeans trt / pdiff cl adjust=tukey;
run;

ods pdf close;


/* Now to create the various outputs for my LaTeX files */

/* Create LaTeX files for inclusion by my notes */
%include "../../MyLatexTagset.sas"; run;
title;
footnote;
ods listing;

proc document name=mixed1;
   list /levels=all;
run;

ods tagsets.mycolorlatex file='uvexp-SAS-001.tex' (notop nobot) stylesheet="sas.sty";
proc print data=gain(obs=10);
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='uvexp-SAS-020.tex' (notop nobot);
proc print data=mean_gain(obs=10 drop=_type_ _freq_);
run;
ods tagsets.mycolorlatex close;



ods tagsets.mycolorlatex file='uvexp-SAS-030.tex' (notop nobot);
proc print data=Mixed1Tests noobs label split=" ";
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='uvexp-SAS-040.tex' (notop nobot);
proc print data=Mixed1LSmeans noobs label split=" ";
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='uvexp-SAS-045a.tex' (notop nobot);
proc print data=Mixed1Diffs(drop=df tvalue probt lower upper) noobs label split=" " ;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='uvexp-SAS-045b.tex' (notop nobot);
%pdmix800(mixed1diffs,mixed1lsmeans,alpha=0.05,sort=yes);
ods tagsets.mycolorlatex close;

ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='uvexp-SAS-050' reset=index;
proc document name=mixed1;
   replay \Mixed#1\ResidualPlots#1\ResidualPanel#1 / dest=listing;
run;
ods graphics off;
ods listing close;




ods tagsets.mycolorlatex file='uvexp-SAS-130.tex' (notop nobot);
proc print data=Mixed2Tests noobs label split=" ";
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='uvexp-SAS-135.tex' (notop nobot);
proc print data=Mixed2CovParms noobs label split=" ";
run;
ods tagsets.mycolorlatex close;


ods tagsets.mycolorlatex file='uvexp-SAS-140.tex' (notop nobot);
proc print data=Mixed2LSmeans noobs label split=" ";
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='uvexp-SAS-145a.tex' (notop nobot);
proc print data=Mixed2Diffs(drop=df tvalue probt lower upper) noobs label split=" " ;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='uvexp-SAS-145b.tex' (notop nobot);
%pdmix800(mixed2diffs,mixed2lsmeans,alpha=0.05,sort=yes);
ods tagsets.mycolorlatex close;

ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='uvexp-SAS-150' reset=index;
proc document name=mixed2;
   replay \Mixed#1\ResidualPlots#1\ResidualPanel#1 / dest=listing;
run;
ods graphics off;
ods listing close;

