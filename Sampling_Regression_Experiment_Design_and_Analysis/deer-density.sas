
/* Example of power analysis for trend analysis with both process and sampling error. on the LOG scale.
   We do this on the log-scale to try and match what happens with Program MONITOR (see my notes) */
/* 2014-07-29 CJS First edition */

/* Deer density example. Initial value is 70; we are interested in power to detect 5% decline/year 
   in a 10 year study. */
/* We cannot use GLMpower because there is both process and sampling error. Need to use Proc Mixed and 
   methods of stroup */

/* Line starting with *---partxxxb; and *---partxxxe; are for inclusion by my LaTeX code
   and can be ignored. */


dm 'output' clear;
dm 'log'    clear;
proc datasets kill; run;
footnote ' ';


options orientation=landscape mprint;
*ods pdf file='deer-density-SAS.pdf' style=styles.printer;

 
title 'Trend Power analysis with process and sampling error';

*---part010b;
/* The sampling SD was 14 which corresponds to a sampling sd on the log scale of 14/70=0.20 */
/* The process SD was  20 which corresponds to a process  sd on the log scale of 20/70=0.29 */
%let samplingVAR = %sysevalf(0.20**2); /* VARIANCE on the log-scale*/
%let processVAR  = %sysevalf(0.29**2); /* VARIANCE on the log-scale */
%let totalSD     = %sysfunc(sqrt(&samplingVAR + &processVAR));
/* The slope of interest (beta1) is 5% decline/year or .05 on the log scale */
%let beta1 = -0.05;       /* 5% decline/year */;
%let beta0 = %sysfunc(log(70));  /* The initial value*/

%let alpha = 0.05;

/* Generate the mean at each X value on the log-scale */
data means;
   do x=0 to 9 by 1;  /* two measurements each year */
       mu = &beta0 + &beta1*x;
       output;
   end;
run;

/* With a single measurement in each year, you can use glm power with the sd set as the sum(process.SD**2 + sampling.SD**2)*/

proc glmpower data=means;
   title2 'Deer-Density Power';
   model mu = x;
   power
     stddev = &totalSD   /* sqrt(.20**2 + .29**2)=  0.35*/
     alpha = &alpha
     ntotal= 10 /* This needs to match the number of data points */
     power = .;
  ods output Output=glmpower_output1;
run;
*---part010e;



*------------------------ create plot of power for different slopes;
*---part020b;
/* We need to generate a set of means for each slope */
data means;
   do beta1 = -.10 to .10 by .02;;
      do x=0 to 9;
         mu = &beta0 + beta1*x;
         output;
	  end;
   end;
run;

proc glmpower data=means;
   title2 'Power for several slopes';
   by beta1;
   model mu = x;
   power
     stddev = &totalSD
     alpha = &alpha 
     ntotal= 10  /* THis needs to match the number of data points */
     power = .;
  ods output Output=glmpower_output3;
run;
*---part020e;

proc print data=glmpower_output3;
run;

ods document name=powerplot(write);
proc sgplot data=glmpower_output3;
   title2 'Power for several slopes';
   series x=beta1 y=power;
   refline 0.80 /axis=y;
   xaxis label='Slope' offsetmin=0.05 offsetmax=0.05;
   yaxis label='Power';
run;
ods document close;


/* How many years are needed to detect this trend? */
/* This is tricky to do because GLMpower requires the ntotal argument
   to match the number of data points, but there is no way to automatically
   set this based on the data -- it must be hard coded. Argh !!!!
 
   Consequently, we create a macro for each individual call, and concatenate
   all of the individual results together. 

   I used individual macro call, but a call execute() could have been used. 
   Contact me for details */

/* The same process and sampling SD are used */


%macro mypower1(nyears);
data means;
   nyears = &nyears;
   do beta1 = -.05;   /* specify the single beta here */
      do x=0 to &nyears-1;
         mu = &beta0 + beta1*x;
         output;
	  end;
   end;
run;

proc glmpower data=means;
   title2 'Power for several slopes';
   by  nyears beta1;
   model mu = x;
   power
     stddev = &totalSD
     alpha = &alpha 
     ntotal= &nyears  /* THis needs to match the number of data points */
     power = .;
  ods output Output=temp;
run;

proc append base=glmpower4 data=temp; /* append all the power values */
run;
%mend mypower1;


proc datasets;
   delete glmpower4; /* will save the power values */
run;
%mypower1(nyears=10);
%mypower1(nyears=12);
%mypower1(nyears=14);
%mypower1(nyears=16);
%mypower1(nyears=18);
%mypower1(nyears=20);

proc print data=glmpower4;
   title2 'how many years of sampling are needed to detect a -0.05 trend/year?';
run;

ods document name=yearplot(write);
proc sgplot data=glmpower4;
   title2 'Power for different number of years';
   series x=nyears y=power;
   refline 0.80 /axis=y;
   xaxis label='Number of years in the study' offsetmin=0.05 offsetmax=0.05;
   yaxis label='Power';
run;
ods document close;



/*****************************************************************************************/
/* Increase sampling effort in each year by a factor of 4. This adjusts the sampling.SD, but
   not the process.SD */
*---part310b;
%let totalSD     = %sysfunc(sqrt(&samplingVAR/4 + &processVAR));
/* The slope of interest (beta1) is 5% decline/year or .05 on the log scale */
%let beta1 = -0.05;       /* 5% decline/year */;
%let beta0 = %sysfunc(log(70));  /* The initial value*/

%let alpha = 0.05;

/* Generate the mean at each X value on the log-scale */
data means;
   do x=0 to 9 by 1;  /* two measurements each year */
       mu = &beta0 + &beta1*x;
       output;
   end;
run;

/* With a single measurement in each year, you can use glm power with the sd set as the sum(process.SD**2 + sampling.SD**2)*/

proc glmpower data=means;
   title2 'Deer-Density Power';
   model mu = x;
   power
     stddev = &totalSD   /* sqrt(.20**2 + .29**2)=  0.35*/
     alpha = &alpha
     ntotal= 10 /* This needs to match the number of data points */
     power = .;
  ods output Output=glmpower_output310;
run;
*---part310e;



/**************************************************************************************/
/* Different designs with replicated X values */
/* We need to replicate the values of X for a class variable */
data means;
   do x=0 to 9 by 1, 0 to 9 by 1, 0 to 9 by 1, 0 to 9 by 1;  /* two measurements each year */
       mu = &beta0 + &beta1*x;
	   xclass = x;
       output;
   end;
run;

proc print data=means (obs=10);
   title2 'Deer Density: Mean response at X values';
run;

/* Later versions of SAS won't compute the df with the kr options (groan), so we need to 
   compute the correct df.
   In the case of regression with repeated measurements at some X values and process error,
   the df is approximately equal to the number of unique X - 2. */
/* see http://support.sas.com/kb/36/898.html for counting unique levels */
ods select none;
proc freq data=means nlevels ;
   tables x;
   ods output nlevels=nlevels;
run; 
ods select all;

data _null_;
   set nlevels;
   ddf = nlevels-2;
   call symput("ddf", put(ddf, f5.0));
run;

proc mixed data=means noprofile;
   title2 'Estimate the non-centrality parameter ';
   class xclass;
   model mu =  x / ddf = &ddf; *ddfm=kr;  /* a linear regression in X */
   random xclass;
   parms   (&processVAR)  (&samplingVAR) / noiter;   /* this is where the estimated  variance components are specified */
   /* save the results to the ods datasets */
   ods output tests3=power_effects;
run;
ods listing; run;   /* turn the output back on */
ods select all; run;

/* now to compute approximations to the power */
data glmpower5;
   set power_effects;
   nc = numdf*Fvalue;  /* approximate non-centrality parameter */ 
   fcrit = finv(1-&alpha, numDF, denDF, 0);  /* estimate critical value */
   power = 1 - probf(fcrit, numdf, dendf, nc);  /* estimated power */
   attrib power label='Power' format=7.2;
   attrib nc    label='Non-centrality' format=7.1;
   attrib fcrit label='F-critical value' format=7.2;
   drop probF ;
run;

proc print data=glmpower5 label split=' ';
   title2 'Estimated power under various scenarios';
   footnote "Alpha=&alpha; Variance components: processVAR: &processVAR; samplingVAR; &samplingVAR";
run;
                 
   


ods pdf close;

/* Now to create the latex output for use with my course notes */
/* Create LaTeX files for inclusion by my notes */
%include "../../../MyLatexTagset.sas"; run;
title;
footnote;
ods listing;


proc document name=mixed;
   list /levels=all;
run;



ods tagsets.mylatex file='deer-density-SAS-010.tex' (notop nobot) stylesheet="sas.sty";
proc print data=glmpower_output1 label split=" " noobs;
   title2 'Deer Density: power values';
   var Alpha StdDev Ntotal Power;
   format power 9.5;
run;
ods tagsets.mylatex close;

ods tagsets.mylatex file='deer-density-SAS-020.tex' (notop nobot) stylesheet="sas.sty";
proc print data=glmpower_output3 label split=" " noobs;
   title ' ';
   var Alpha Beta1 StdDev ntotal Power;
   format power 9.3;
   label beta1='Trend/year';
run;
ods tagsets.mylatex close;


ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='deer-density-SAS-powerplot' reset=index;
proc document name=powerplot;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;



ods tagsets.mylatex file='deer-density-SAS-030.tex' (notop nobot) stylesheet="sas.sty";
proc print data=glmpower4 label split=" " noobs;
   title ' ';
   var Alpha Beta1 nyears StdDev Power;
   format power 9.3;
   label beta1='Trend/year';
run;
ods tagsets.mylatex close;


ods tagsets.mylatex file='deer-density-SAS-310.tex' (notop nobot) stylesheet="sas.sty";
proc print data=glmpower_output310 label split=" " noobs;
   title2 'Deer Density: power values with 4x sampling effort in each year';
   var Alpha StdDev Ntotal Power;
   format power 9.3;
run;
ods tagsets.mylatex close;



ods tagsets.mylatex file='deer-density-SAS-320.tex' (notop nobot) stylesheet="sas.sty";
proc print data=glmpower5 label split=" " noobs;
   title2 'Estimated power when increasing sampling in each year 4x';
   var power;
   format power 6.3;
   footnote "Alpha=&alpha; Variance components: processVAR: &processVAR; samplingVAR; &samplingVAR";
run;
ods tagsets.mylatex close;




