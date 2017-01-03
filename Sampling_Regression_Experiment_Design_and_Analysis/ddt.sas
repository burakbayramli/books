/* Estimate variance components from pseudo-replicated data */
/* 2014-08-11 CJS First Editions */
dm 'log'    clear;
dm 'output' clear;
proc datasets kill; run;
footnote ' ';


options orientation=landscape;
ods pdf file='ddt-SAS.pdf' style=styles.printer;

title 'DDT - separating process and sampling error';
 
data ddt;
   infile 'ddt.csv' dlm=',' dsd missover;
   input year ddt;
run;
 
proc print data=ddt(obs=10);
   title2 'Part of the raw data';
run;
 
proc sgplot data=ddt;
   title2 'plot of the data';
   scatter x=year y=ddt;
run;
 
/* estimate the variance components */
proc mixed data=ddt;
   title2 'estimate the variance components';
   class year;
   model ddt = / ddfm=kr;
   random year;
run;

ods pdf close;
