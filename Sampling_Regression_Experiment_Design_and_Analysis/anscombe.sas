/*  The Anscombe dataset.
    Anscombe, F. J.  (1973) American Statistician 27, 17-21.
    2014-04-24 CJS Revised edition*/

dm 'output' clear;
dm 'log'    clear;
proc datasets kill; run;

options  noovp orientation=landscape;
ods pdf file='anscombe-sas.pdf' style=styles.printer;
 
title 'Anscombe dataset';
 
data anscombe;
   infile 'anscombe.csv' dlm=',' dsd missover firstobs=2;
   input x1 y1 x2 y2 x3 y3 x4 y4;
run;
 
proc print data=anscombe;
   title2 'raw data';
run;
 
%macro fit(case);
proc sgplot data=anscombe;
   title2 "y&case vs x&case";
   scatter y=y&case x=x&case;
   reg     y=y&case x=x&case;
run;
 
ods graphics on;
proc reg data=anscombe plots=all;
   title2 "y&case vs x&case";
   model y&case = x&case / clb;
run;
ods graphics off;
%mend;

%fit(case=1);
%fit(case=2);
%fit(case=3);
%fit(case=4);


ods pdf close;

