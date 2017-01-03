/* Compute power curves for a two factor CRD ANOVA */
/* SAS also has an interactive program to compute power - look in your sas directory
   under the START button in WinDoze */
/* This is based on the result from the PCB example */

dm 'output' clear;
dm 'log'    clear;
proc datasets kill;

/* Power computations will be done using two procedures
   - Proc Power is not suitable for two-factor or more complex designs and so is not used
   - Proc GLMPower suitable for more complex designs with a single error term
     allowing for covariates etc.
   - Methods of Stroup suitable for ALL design (including those
     with multiple error terms)

/* There are 2 sex x 3 species in which PCB concentrations are measured.
   The standard deviation is about 5. */

options nodate noovp orientation=landscape;
ods pdf file='pcb-power-SAS.pdf';
goptions device=pdf colors=(black) rotate=landscape;

title 'PCB concentration power analysis';

%let alpha=.05;
%let power=.80;
%let stddev= 5;
%let var   =%sysevalf(&stddev*&stddev);  /* be sure to specify VARIANCE in Proc Mixed */

/* Proc power does not deal with two-factor designs */

/* Using Proc GLMPower. */
/* You must first input the means of each level of the factor(s) */
*---part001b;
data means;
   input sex $ species $ pcb;
   datalines;
m s1 15
m s2 15 
m s3 20
f s1 9
f s2 13
f s3 16
;;;;
*---part001e;

proc tabulate data=means;
   title2 'Means for power computation';
   class sex species;
   var pcb;
   table sex, species*pcb*sum=' ';
run;

ods document name=glmpower(write);
*---part010b;
ods graphics on;
proc glmpower data=means;
   title2 'GLMPower';
   class sex species;
   model pcb = sex species sex*species;
   power
      stddev = 5
      alpha  = .05
      ntotal =  . 
      power  = .80 ;
   plot y=power yopts=(ref=.80 crossref=yes) min=.05 max=.95;;
   footnote 'NOTE: You require different sample sizes for each effect';
run;
ods graphics off;
*---part010e;
ods document close;



/* This will illustrate how to estimate the power based on the ideas of 
   Stroup, W. W. (1999)
   Mixed model procedures to assess power, precision, and sample size in the design of experiments.
   Pages 15-24 in Proc. Biopharmaceutical Section. Am. Stat. Assoc., Baltimore, MD.

   The nice thing about this method is that it can be used for ANY design 
   regardless of complexity.

   The idea is to 

   1. Create a data set with the structure of the design
      to be assessed. Instead of observed data, use the
      means that reflect the departure from H0 of interest.

   2. Run PROC MIXED with the variance and
     covariance components set at the anticipated
     values. Use the NOPROFILE and NOITER
     options (see below) to set the (co)variance
     components.

  3. The MODEL and CONTRAST statements in
     PROC MIXED compute F values which gives the non-centrality parameter.

  4. Pass these to a data step that estimates the power
 
  5. Plot (or print) the results.

***** CAUTION: Be sure to specify the VARIANCE in proc mixed and not the std dev!!!
*/

/* You can have several sets of mean indentified by the set number 
   and separate power computations will be 
   done for each set */
data effect_sizes;
   set means;
   set = 1;
   mu = pcb;
   output;
   keep set sex species mu;
;;;;
 
proc sort data=effect_sizes;
   by set sex species;
run;

proc tabulate data=effect_sizes;
   title2 'Effect sizes for which power computations are done';
   class sex set species;
   var mu;
   table set, sex, species*mu*sum=' '*f=5.0 / rts=10;
run;

/* Generate data sets for various sample sizes for later plotting purposes */
 
data trials;
   set effect_sizes;
   do n_per_group =  2 to 20 by 1 ;  /* number of replicates at level of the factor */
      /* generate data with sufficient replicates all with the same mean values */
      do rep=1 to n_per_group;
         output;
      end;
   end;
   attrib n_per_group label='Number of replicates at each species';
run;
 
proc sort data=trials;  by set n_per_group sex species;  /* group data appropriately */

proc print data=trials(obs=30);
   title2 'part of the generated data';
run;

/* Show the results from ONE analysis */
proc mixed data=trials noprofile;  /* the noprofile options prevents trying to fit the model */
   title2 'Estimate the non-centrality parameter for one set of data ';
   where set=1 & n_per_group=2;
   by set n_per_group;
   class sex species;
   model mu = sex species sex*species;
   parms   (&var) / noiter;   /* this is where the estimated  variance components are specified */
run; 

/* Estimate the non-centrality parameter from all the combinations */
ods select none; run; /* turn the output off as it is not needed */
proc mixed data=trials noprofile;  /* the noprofile options prevents trying to fit the model */
   title2 'Estimate the non-centrality parameter ';
   by set n_per_group;
   class sex species;
   model mu = sex species sex*species;
   parms   (&var) / noiter;   /* this is where the estimated  variance components are specified */
   /* save the results to the ods datasets */
   ods output tests3=whole_model_power_effects;  /* extract the F-statistic */
run; 
ods select all; run;   /* turn the output back on */

proc print data=whole_model_power_effects(obs=20);
   title2 'Part of the output from Proc Mixed on the fake data';
run;

/* now to compute approximations to the power */
data power;
   set whole_model_power_effects;
   nc = numdf*Fvalue;  /* approximate non-centrality parameter */
   fcrit = finv(1-&alpha, numDF, denDF, 0);  /* estimate critical value */
   power = 1 - probf(fcrit, numdf, dendf, nc);  /* estimated power */
   attrib power label='Power' format=7.2;
   attrib nc    label='Non-centrality' format=7.1;
   attrib fcrit label='F-critical value' format=7.2;
   drop probF;
run;

proc print data=power label split=' ';
   title2 'Estimated power using Stroup method';
   by set;
run;

proc sgplot data=power;
   title2 "Estimated power at alpha=&alpha";
   by set;
   yaxis label='Power' min=0 max=1;
   xaxis label='Sample size PER treatment group';
   series y=power x=n_per_group / group=effect;
   refline 0.80/ axis=y;
run;

ods pdf close;


/* Now to create the various outputs for my LaTeX files */

/* Create LaTeX files for inclusion by my notes */
%include "../../MyLatexTagset.sas"; run;
title;
footnote;
ods listing;

proc document name=glmpower;
   list /levels=all;
run;

ods tagsets.mycolorlatex file='pcb-power-SAS-001.tex' (notop nobot) stylesheet="sas.sty";
proc print data=means;
run;
ods tagsets.mycolorlatex close;


ods tagsets.mycolorlatex file='pcb-power-SAS-010-fixed.tex' (notop nobot);
proc document name=glmpower;
   obstitle \Glmpower#1\Power#1\FixedElements#1; /* kill titles */
   obtitle  \Glmpower#1\Power#1\FixedElements#1;
   replay \Glmpower#1\Power#1\FixedElements#1;
run;
ods tagsets.mycolorlatex close;


ods tagsets.mycolorlatex file='pcb-power-SAS-010-results.tex' (notop nobot);
proc document name=glmpower;
   obstitle \Glmpower#1\Power#1\Output#1; /* kill titles */
   obtitle  \Glmpower#1\Power#1\Output#1;
   replay \Glmpower#1\Power#1\Output#1;
run;
ods tagsets.mycolorlatex close;

ods listing;
ods graphics on / imagefmt=png imagename='pcb-power-SAS-010-powerplot' reset=index;
proc document name=glmpower;
   replay \Glmpower#1\Power#1\PlotStatement1#1\PowerNTotal#1\PowerPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;


