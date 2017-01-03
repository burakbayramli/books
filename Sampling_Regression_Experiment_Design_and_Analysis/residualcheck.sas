/* The importance of checking residuals.
   This dataset is extracted from:

   Leonard A. Stefanski, (2007)
   Residual Sur(Realism)'
   American Statistician,  61, 163-177.

   Also check out the website at:
   http://www4.stat.ncsu.edu/~stefanski/NSF_Supported/Hidden_Images/stat_res_plots.html

   2014-04-24 CJS updates to SAS 9.4/ sgplot etc. 
*/
dm 'output' clear;
dm 'log'    clear;
proc datasets kill; run;
footnote ' ';
title ' ';


title 'Hidden messages in residual plots';
options noovp orientation=landscape;
ods pdf file='residualcheck-SAS.pdf' style=styles.printer;
 
data satan;
   infile 'residualcheck.csv' dlm=',' dsd missover firstobs=2;
   input y x1 x2 x3 x4 x5 x6;
run;
 
proc print data=satan(obs=20);
   title2 'part of the original data';
run;
 
proc sgscatter data=satan;
   title2 'some preliminary plots';
   plot y*(x1 x2 x3 x4 x5 x6);
run;
 
proc reg data=satan plots=all;
   title2 'fit a model';
   model y = x1 x2 x3 x4 x5 x6 / clb;
   output out=pred p=pred r=resid;
run;


proc sgplot data=pred;
   title2 'Residual plot';
   scatter y=resid x=pred;
   xaxis min=-3 max=3 integer;
run;

ods pdf close;



