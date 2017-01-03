/* ANCOVA

/* Fish density and stream slope.
   2015-07-21 CJS analyze using ANCOVA
 
/* A series of streams were randomly selected. 
   Two streams in each stream were selected and
   the slope of the stream bed and the areal fish 
   density (per sq meter) were measured. */

  
/* Lines starting with ##***part001b; or ##***part001e; bracket the source 
   line for inclusion by LaTex and usually are not coded. */
 

dm 'log'    clear;
dm 'output' clear;
proc datasets kill; run;
title; footnote ' '; run;
ods graphics on; run;

options orientation=landscape;
ods pdf file='paired-stream-SAS.pdf' style=styles.printer;

title 'Alternate analysis of paired stream example';

/* read in the data */
*---partdatab;
data stream;
   infile 'paired-stream.csv' dlm=',' dsd missover firstobs=2;
   length Stream slopeclass $10;
   input Stream $ slope slopeclass density;
   logdensity = log(density);
run;
*---partdatae;


proc print data=stream(obs=10);
   title2 'raw data';
run;
 
ods document name=prelimplot(write);
/* Create a preliminary plot of the data */
*---partprelimplotb;
ods graphics / reset attrpriority=none;
proc sgplot data=stream;
   title2 'Preliminary plot of log(density)';
   scatter x=slope y=logdensity / group=stream;
   xaxis label="slope" offsetmin=0.05 offsetmax=0.05;
   yaxis label="log(density)";
run;
*---partprelimplote;
ods document close;




ods document name=glmfitp(write);
/* Fit the  model with parallel slopes */
*---partglmfitpb;
ods graphics on;
proc glm data=stream plots=(all diagnostics(unpack));
   title2 'Parallel slopes model';
   class stream;
   model logdensity = stream slope / p  cli clparm;
   estimate 'common slope' slope 1;
   ods output ModelAnova     =glmfitpanova;
   ods output Estimates      =glmfitpestimates;
   ods output predictedvalues=glmfitppredictedvaluescli;
   ods output lsmeandiffcl   =glmfitplsmeandiffcl;
   ods output predictedinfo  =glmfitppredictedinfo;
run;
*---partglmfitpe;
ods document close;


ods document name=glmfitp2(write);
/* Fit the  model with parallel slopes - stream 2 excluded */
*---partglmfitp2b;
ods graphics on;
proc glm data=stream plots=(all diagnostics(unpack));
   where stream ^= '2';
   title2 'Parallel slopes model - stream 2 excluded';
   class stream;
   model logdensity = stream slope / p  cli clparm;
   estimate 'common slope' slope 1;
   ods output ModelAnova     =glmfitpanova2;
   ods output Estimates      =glmfitpestimates2;
   ods output predictedvalues=glmfitppredictedvaluescli2;
   ods output lsmeandiffcl   =glmfitplsmeandiffcl2;
   ods output predictedinfo  =glmfitppredictedinfo2;
run;
*---partglmfitp2e;
ods document close;


*---parttransposeb;
/* Alternate analysis - a ratio estimator */
proc sort data=stream; by stream slopeclass; run;
proc transpose data=stream out=stream_wide1 ;
   by stream;
   var logdensity slope;
   id slopeclass;
run;
data stream_wide1;
   set stream_wide1;
   diff = high - low;
run;
proc transpose data=stream_wide1 out=stream_wide2;
   by stream;
   var diff;
   id _name_;
run;
*---parttransposee;

proc print data=stream_wide2(obs=5);
   title2 'stream - wide format';
run;

ods document name=regfit(write);
*---partregfitb;
proc reg data=stream_wide2;
   title2 'ratio estimator';
   model logdensity = slope / noint;
   ods output ParameterEstimates = regEstimates;
run;
*---partregfite;
ods document close;


ods pdf close;


/* create the files to be included in the LaTex document */

/* Create LaTeX files for inclusion by my notes */
%include "../../MyLatexTagset.sas"; run;
title;
footnote;
ods listing;

proc document name=regfit;
   list /levels=all;
run;




ods tagsets.mylatex file='paired-stream-SAS-010.tex' (notop nobot);
proc print data=stream(obs=10) label split=" ";
run;
ods tagsets.mylatex close;

ods listing;
ods graphics on / imagefmt=png imagename='paired-stream-SAS-prelimplot' reset=index;
proc document name=prelimplot;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;

/* Parallel slope model - show the fitted model */
ods listing;
ods graphics on / imagefmt=png imagename='paired-stream-SAS-plotslope' reset=index;
proc document name=glmfitp;
   replay  	\GLM#1\ANOVA#1\logdensity#1\ANCOVAPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;

/* get the obs vs predicted plot */
ods listing;
ods graphics on / imagefmt=png imagename='paired-stream-SAS-obsvspredplot' reset=index;
proc document name=glmfitp;
   replay  	\GLM#1\ANOVA#1\logdensity#1\DiagnosticPlots#1\ObservedByPredicted#1 / dest=listing;
run;
ods graphics off;
ods listing close;

/* residual plot */
ods listing;
ods graphics on / imagefmt=png imagename='paired-stream-SAS-residplot' reset=index;
proc document name=glmfitp;
   replay  \GLM#1\ANOVA#1\logdensity#1\DiagnosticPlots#1\ResidualByPredicted#1 / dest=listing;
run;
ods graphics off;
ods listing close;

 	
 	
ods tagsets.mylatex file='paired-stream-SAS-glmfitpanova.tex' (notop nobot);
proc print data=glmfitpanova(drop=dependent) noobs label split=' ';
   where HypothesisType=3;
run;
ods tagsets.mylatex close;

ods tagsets.mylatex file='paired-stream-SAS-glmfitpestimates.tex' (notop nobot);
proc print data=glmfitpestimates(drop=dependent tvalue probt) noobs label split=' ';
run;
ods tagsets.mylatex close;

ods tagsets.mylatex file='paired-stream-SAS-glmfitplsmeandiffcl.tex' (notop nobot);
proc print data=glmfitplsmeandiffcl(drop=dependent i j) noobs label split=' ';
run;
ods tagsets.mylatex close;


/* information after stream 2 is excluded */	
ods tagsets.mylatex file='paired-stream-SAS-glmfitpanova2.tex' (notop nobot);
proc print data=glmfitpanova2(drop=dependent) noobs label split=' ';
   where HypothesisType=3;
run;
ods tagsets.mylatex close;

ods tagsets.mylatex file='paired-stream-SAS-glmfitpestimates2.tex' (notop nobot);
proc print data=glmfitpestimates2(drop=dependent tvalue probt) noobs label split=' ';
run;
ods tagsets.mylatex close;


/* alternate analysis */

ods tagsets.mylatex file='paired-stream-SAS-streamwide.tex' (notop nobot);
proc print data=stream_wide2(obs=5) noobs label split=' ';
run;
ods tagsets.mylatex close;

ods tagsets.mylatex file='paired-stream-SAS-regestimates.tex' (notop nobot);
proc print data=regestimates noobs label split=' ';
run;
ods tagsets.mylatex close;

ods listing; /* fitted plot of the ratio estimator */
ods graphics on / imagefmt=png imagename='paired-stream-SAS-regfitplot' reset=index;
proc document name=regfit;
   replay  \Reg#1\MODEL1#1\ObswiseStats#1\logdensity#1\FitPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;




