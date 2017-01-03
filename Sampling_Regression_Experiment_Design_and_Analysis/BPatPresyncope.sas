/* RCB analysis  */

/* Blood Pressure at presyncope under different experimental conditions.

   15 subjects were measured while wearing three different stocking to control
   presyncope. The order of treatments was randomized within each patient.

   Data provided by Clare Protheroe of BPK at SFU in 2011 */

/* ods document statements are used to extract portions for output to LaTeX */

dm 'output' clear;
dm 'log'    clear;
proc datasets kill;
run;

options nodate noovp orientation=landscape;;
ods pdf file='BPatPresyncope.pdf';
goptions device=pdf colors=(black) rotate=landscape;

title 'Blood pressure at presyncope';

*---part001b;
proc import file="BPatPresyncope.csv" dbms=csv out=bp replace;
run;
*---part001e;

ods document name=RawData(write);
proc print data=bp(obs=5);
   title2 'Part of the raw data';
   format bp_at_presyncope 6.1;
run;
ods document close;

proc tabulate data=bp;
   title2 'Check that the blocks are complete';
   class subject treatment;
   var bp_at_presyncope;
   table subject, treatment*bp_at_presyncope*sum*f=6.1;
run;

ods document name=Tabulate1(write);
*---part005b;
proc tabulate data=bp;
   title2 'A preliminary tabulation';
   class treatment;
   var bp_at_presyncope;
   table treatment, n*f=5.0 
         bp_at_presyncope*(mean*f=6.2 std*f=6.2);
run;
*---part005e;
ods document close;
 
proc sort data=bp;
   by treatment subject;
run;
ods document name=Profile(write);
*---part010b;
proc sgplot data=bp;
   title2 'Profile plot to check for additivity and outliers';
   series y=bp_at_presyncope x=subject / group= treatment;;
run;
*---part010e;
ods document close;

*---part015b;
data bp_nooutlier;
   set time;
   if bp_at_presyncope < 40 then delete;
run;
*---part015e;

ods document name=Tabulate2(write);
proc tabulate data=bp_nooutlier;
   title2 'A preliminary tabulation after outliers removed';
   class treatment;
   var bp_at_presyncope;
   table treatment, n*f=5.0 
         bp_at_presyncope*(mean*f=6.2 std*f=6.2);
run;
ods document close;

/* now for the analysis */
ods graphics on;
ods document name=RCBanal(write);
*---part020b;
proc mixed data=bp_nooutlier plots=all;
   title2 'Analysis';
   class subject treatment;          
   model bp_at_presyncope = treatment;
   random subject;
   lsmeans treatment / diff cl adjust=tukey;
   ods output tests3 =tests;
   ods output diffs  =LSMeansDiffs;
   ods output lsmeans=LSmeans;
   ods output covparms=Covparms;
run;
*---part020e;
ods document close;
ods graphics off;


/************* Power analysis **************/
ods document name=power(write);
ods graphics on;
*---part050b;
proc power;
   title2 'Using a base difference of 5 minutes';
   onewayanova
      groupmeans = 65 | 65 | 70  /* list the group means */
      stddev = 9       /* what is the standard deviation */
      alpha = .05      /* what is the alpha level */
      power = .80      /* target power */
      ntotal = .       /* solve for power */
   ;    /* end of the onewayanova statement - don't forget it */
   plot x=power min=0.05 max=0.90;
   ods output output=poweroutput;
run;
*---part050e;
ods graphics off;
ods document close;

ods pdf close;



/* Now to create files for inclusion in my notes using LaTeX tagsets */
%include "../../MyLaTeXtagset.sas"; run;
title;
footnote;
ods listing;

proc document name=power;
 list /levels=all;
run;

ods tagsets.mycolorlatex file='BPatPresyncope-SAS-001.tex' (notop nobot);
proc document name=RawData;
   obtitle \print#1\Print#1 ;
   replay \Print#1\Print#1  / levels=all;;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='BPatPresyncope-SAS-005.tex' (notop nobot);
proc document name=Tabulate1;
   obtitle \Tabulate#1\Report#1\Table#1 ;
   replay  \Tabulate#1\Report#1\Table#1  / levels=all;;
run;
ods tagsets.mycolorlatex close;

ods graphics on / outputfmt=png imagename="BPatPresyncope-SAS-010" reset=index;
proc document name=profile;
  replay \Sgplot#1\SGPlot#1 / levels=all;
run;
ods graphics off;

ods tagsets.mycolorlatex file='BPatPresyncope-SAS-015.tex' (notop nobot);
proc document name=Tabulate2;
   obtitle \Tabulate#1\Report#1\Table#1 ;
   replay  \Tabulate#1\Report#1\Table#1  / levels=all;;
run;
ods tagsets.mycolorlatex close;


ods tagsets.mycolorlatex file='BPatPresyncope-SAS-020.tex' (notop nobot);
proc print data=tests noobs split=' ' label;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='BPatPresyncope-SAS-021.tex' (notop nobot);
proc print data=LSmeans noobs split=' ' label;
   var treatment estimate stderr;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='BPatPresyncope-SAS-022.tex' (notop nobot);
proc print data=LSmeansDiffs(drop=effect df tvalue alpha lower upper probt) noobs split=' ' label;
run;
ods tagsets.mycolorlatex close;


ods graphics on / outputfmt=png imagename="BPatPresyncope-SAS-023" reset=index;
proc document name=RCBanal;
  obtitle \Mixed#1\ResidualPlots#1\ResidualPanel#1;
  replay \Mixed#1\ResidualPlots#1\ResidualPanel#1 / levels=all;
run;
ods graphics off;


ods tagsets.mycolorlatex file='BPatPresyncope-SAS-050.tex' (notop nobot);
proc document name=Power;
   obtitle \Power#1\OneWayANOVA#1\Output#1 ;
   replay  \Power#1\OneWayANOVA#1\Output#1 / levels=all;;
run;
ods tagsets.mycolorlatex close;

ods graphics on / outputfmt=png imagename="BPatPresyncope-SAS-051" reset=index;
proc document name=power;
  obtitle \Power#1\OneWayANOVA#1\PlotStatement1#1\NTotalPower#1\PowerPlot#1;
  replay  \Power#1\OneWayANOVA#1\PlotStatement1#1\NTotalPower#1\PowerPlot#1 / levels=all;
run;
ods graphics off;

ods tagsets.mycolorlatex file='BPatPresyncope-SAS-052.tex' (notop nobot);
proc print data=covparms noobs split=' ' label;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='BPatPresyncope-SAS-053.tex' (notop nobot);
proc print data=poweroutput noobs split=' ' label;
run;
ods tagsets.mycolorlatex close;


ods listing close;
