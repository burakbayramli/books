/* Taken from
    Sparks, T.H., Croxton, J.P.J., Collinson, N., and Grisenthwaite, D.A. (2005)
    The Grass is Greener (for longer).
`   Weather 60,  121-123.

    D.G. Grisenthwaite, a pensioner who has spent 20 years keeping detailed 
    records of how often he cuts his grass has been included in a climate change study.
    David Grisenthwhaite, 77, and a self-confessed "creature of habit", has 
    kept a note of cutting grass in his Kirkcaldy garden since 1984.
    The grandfather's data was so valuable it was used by the Royal Meteorological Society 
    in a paper on global warming.

    The retired paper-maker, who moved to Scotland from Cockermouth 
    in West Cumbria in 1960, said he began making a note of the time and date
    of every occasion he cut the grass simply "for the fun of it". 

    I have extracted the data on the cutting duration from the above paper. */

/* 2014-04-25 CJS Update for SAS 9.4 */
 
title 'The Grass is Greener (for longer)';
options  noovp orientation=landscape;
ods pdf file='grass-SAS.pdf' style=styles.printer; 

*---part010b;
data duration;
   infile 'grass.csv' dlm=',' dsd missover firstobs=2;
   input year duration;
run;
*---part010e;

proc print data=duration;
   title2 'raw data';
run;
 
/* plot of the raw data */
/* plot the data and fit a simple regression */
ods document name=prelimplot(write);
*---partprelimplotb;
proc sgplot data=duration;
   title2 'Preliminary data plot';
   scatter  y=duration x=year;
   series   y=duration x=year;
   yaxis label='Length of grass cutting season (days)';
   xaxis label='Year' integer offsetmax=0.05 offsetmin=0.05;
run;
*---partprelimplote;
ods document close;
 
/* Fit the regression line */
ods document name=regoutput(write);
*---partregfitb;
ods graphics on;
proc reg data=duration plot=all;
   title2 'Is there evidence that duration of season has increased over time';
   model duration = year / dw dwprob clm cli clb; 
   output out=predfit pred=pred lclm=lclm uclm=uclm lcl=lcli ucl=ucli ;
   ods output ParameterEstimates=coef;
   ods output dwstatistic=dwstatistic;
   ods output fitstatistics=fitstatistics;
run;
ods graphics off;
*---partregfite;
ods document close;

data duration;  /* merge the original data set with the predictions */
   merge duration predictions; 
   drop model dependent observation depvar;
   format _numeric_ 6.1;
   format year duration 5.0;
run;

proc print data=duration;
   title3 'Predicted values and confidence intervals';
run;

proc sort data=duration; by year;

/* plot the data and fit a simple regression */
ods document name=regplot(write);
proc sgplot data=duration noautolegend;
   title2 'Fitted line with 95% ci for mean and 95% pi for individual durations';
   scatter  y=duration x=year;
   series   y=duration x=year;
   series   y=PredictedValue x=year;
   band     lower=lowercl      upper=uppercl     x=year / transparency=0.5;
   band     lower=LowerCLmean  upper=UpperClmean x=year;
   scatter  y=duration x=year;
   series   y=duration x=year;
   yaxis label='Length of grass cutting season (days)';
   xaxis label='Year' integer offsetmax=0.05 offsetmin=0.05;
run;
ods document close;




/* Find the non-parametric measures of association */

*---partpearsonb;
proc corr data=duration pearson spearman kendall;
   title2 'Estimates of rank order correlation';
   var duration year;
   ods output PearsonCorr   = PearsonCorr;
   ods output KendallCorr   = KendallCorr;
   ods output SpearmanCorr  = SpearmanCorr;
run;
*---partpearsone;

  
/* Find the Theil-Kendall-Sen estimator of the slope and a 95% confidence interval */
/* First find all the pairwise slopes */

*---partsenb; 
proc iml;
   use duration;
   read all var{year duration} into xy where( duration ^= .);
   print "Data values read in ", xy;
   slopes = j(1,1,0);  /* create a vector to hold slopes with a dummy first value */
   do i=1 to nrow(xy); 
      do j=i+1 to nrow(xy);
         if xy[i,1] ^= xy[j,1] then do;  /* ignore pairs where the x's are the same */
            slope = (xy[j,2]-xy[i,2])/(xy[j,1]-xy[i,1]);
            slopes = slopes // slope;
         end;
      end;
   end;
   slopes = slopes[2:nrow(slopes)];  /* drop the initial dummy value */
   call sort( slopes, 1);  /* sort the slopes */
   print "Sorted slope values", slopes;

   /* figure out the appropriate percentiles to use */
   NN = nrow(slopes);
   n  = nrow(xy); 
   estslope = median(slopes); 
   w = 1.96 * sqrt(n*(n-1)*(2*n+5)/18); /*replace the 1.96 if you don't want a 95% confidence interval */
   q1 = .5*(NN-w)-1;
   q2 = NN-q1+1;
   lower=slopes[q1];
   upper=slopes[q2];
   print "Estimated value of w, q1, and q2" NN n w q1 q2;
   mattrib estslope format=7.3;
   mattrib lower    format=7.3;
   mattrib upper    format=7.3;
   print "Estimated slope and 95% confidence interval", estslope lower upper;
run;
*---partsene;
ods pdf close;



/* create the files to be included in the LaTex document */

/* Create LaTeX files for inclusion by my notes */
%include "../../MyLatexTagset.sas"; run;
title;
footnote;
ods listing;

proc document name=regoutput;
   list /levels=all;
run;


ods listing;
ods graphics on / imagefmt=png imagename='grass-SAS-prelimplot' reset=index;
proc document name=prelimplot;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;


ods tagsets.mylatex file='grass-SAS-010.tex' (notop nobot);
proc print data=grass(obs=10) label split=" ";
run;
ods tagsets.mylatex close;

ods tagsets.mylatex file='grass-SAS-regfit.tex' (notop nobot);
proc print data=coef label split=" " noobs;
run;
ods tagsets.mylatex close;

ods tagsets.mylatex file='grass-SAS-dwtest.tex' (notop nobot);
proc print data=dwstatistic label split=" ";
   var label1 nvalue1;
   label label1='Statistic';
   label nvalue1='Value';
run;
ods tagsets.mylatex close;

ods listing;
ods graphics on / imagefmt=png imagename='grass-SAS-diagplot' reset=index;
proc document name=regoutput;
   replay  	\Reg#1\MODEL1#1\ObswiseStats#1\jdate#1\DiagnosticPlots#1\DiagnosticsPanel#1/ dest=listing;
run;
ods graphics off;
ods listing close;

ods listing;
ods graphics on / imagefmt=png imagename='grass-SAS-predci' reset=index;
proc document name=regoutput;
   replay  	Reg#1\MODEL1#1\ObswiseStats#1\jdate#1\FitPlot#1/ dest=listing;
run;
ods graphics off;
ods listing close;

ods tagsets.mylatex file='grass-SAS-predci-values.tex' (notop nobot);
proc print data=predfit(obs=5) label split=" " noobs;
   var year pred lclm uclm lcli ucli;
run;
ods tagsets.mylatex close;

ods tagsets.mylatex file='grass-SAS-RMSE.tex' (notop nobot);
proc print data=fitstatistics noobs label split=' '; ;
   var label1 nvalue1 label2 nvalue2;
   label label1='Statistic';
   label label2='Statistic';
   label nvalue1='Value';
   label nvalue2='Value';
run;
ods tagsets.mylatex close;


ods tagsets.mylatex file='grass-SAS-spearman.tex' (notop nobot);
proc print data=spearmancorr label split=" " noobs;
run;
ods tagsets.mylatex close;

ods tagsets.mylatex file='grass-SAS-kendall.tex' (notop nobot);
proc print data=kendallcorr label split=" " noobs;
run;
ods tagsets.mylatex close;

ods tagsets.mylatex file='grass-SAS-pearson.tex' (notop nobot);
proc print data=pearsoncorr label split=" " noobs;
run;
ods tagsets.mylatex close;




