/*  Incomplete block design
    Analysis of water quality data */

/* Impact of development on water quality

   Water quality monitoring studies often take the form of incomplete block 
   designs. For example, the following data represents TSS in water samples 
   taken upstream of a development (the {\it reference} sample), at the #development 
   (the{\it mid-stream} sample), or downstream of the development #(the {\it ds} sample). 
   Samples are taken during storm events when water 
   quality may be compromised by the development. Here is a small set of data.

   Such a small set of data likely has very poor power to detect 
   anything but very large differences in water quality among  the three 
   locations. Before conducting such a study, please perform a power analysis 
   to ensure that sufficient samples are taken.}:
*/


/* Line starting with *---partxxxb; and *---partxxxe; are for inclusion by my LaTeX code
   and can be ignored. */
dm 'output' clear;
dm 'log'    clear;
proc datasets kill; run; 

options orientation=landscape;
ods pdf file='water-quality-SAS.pdf';
goptions device=pdf colors=(black) rotate=landscape;

 
title 'Water Quality - Incomplete block design';

*---part001b;
data water_quality; 
   infile 'water-quality.csv'  firstobs=2 dlm="," dsd missover;
   length location event $10.;
   input location $ event $ tss;
   logTSS = log(tss);
   format logTSS 7.2;
run;
*---part001e;


proc print data=water_quality;
   title2 'raw data';
run;
 

ods document name=mixed1(write);
*---part010b;
ods graphics on;
proc mixed data=water_quality;
   title2 'Fixed block analysis (The intra-block analysis)';
   class location event;
   model logTSS = location event/ ddfm=kr;
   lsmeans location / diff cl adjust=tukey;
   ods output tests3 =Mixed1tests;
   ods output lsmeans=Mixed1LSmeans;
   ods output diffs  =Mixed1Diffs;
run;
ods graphics off;
*---part010e;
ods document close;


ods document name=mixed2(write);
*---part020b;
ods graphics on;
proc mixed data=water_quality;
   title2 'Random block analysis - combined inter- and intra- block analysis';
   class location event;
   model logTSS = location / ddfm=kr;
   random event;
   lsmeans location / diff cl adjust=tukey;
   ods output tests3 =Mixed2tests;
   ods output lsmeans=Mixed2LSmeans;
   ods output diffs  =Mixed2Diffs;
run;
ods graphics off;
*---part020e;
ods document close;



ods pdf close; 







/* Now to create the various outputs for my LaTeX files */

/* Create LaTeX files for inclusion by my notes */
%include "../../MyLatexTagset.sas"; run;
title;
footnote;
ods listing;

proc document name=mixed;
   list /levels=all;
run;

ods tagsets.mycolorlatex file='water-quality-SAS-001.tex' (notop nobot) stylesheet="sas.sty";
proc print data=water_quality;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='water-quality-SAS-002.tex' (notop nobot);
proc tabulate data=water_quality;
   title2 ' ';
   class location event;
   var logTSS;
   table location, event*logTSS*mean=' '*f=7.3 / rts=20;
run;
ods tagsets.mycolorlatex close;



ods tagsets.mycolorlatex file='water-quality-SAS-010a.tex' (notop nobot);
proc print data=Mixed1Tests noobs label split=" " ;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='water-quality-SAS-010b.tex' (notop nobot);
proc print data=Mixed1Lsmeans noobs label split=" " ;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='water-quality-SAS-010c.tex' (notop nobot);
proc print data=Mixed1Diffs noobs label split=" " ;
   var location _location estimate stderr adjustment adjp adjlower adjupper;
run;
ods tagsets.mycolorlatex close;


ods tagsets.mycolorlatex file='water-quality-SAS-020a.tex' (notop nobot);
proc print data=Mixed2Tests noobs label split=" " ;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='water-quality-SAS-020b.tex' (notop nobot);
proc print data=Mixed2Lsmeans noobs label split=" " ;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='water-quality-SAS-020c.tex' (notop nobot);
proc print data=Mixed2Diffs noobs label split=" " ;
   var location _location estimate stderr adjustment adjp adjlower adjupper;
run;
ods tagsets.mycolorlatex close;


