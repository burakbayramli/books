/* Two factor unbalanced CRD */
dm 'output' clear;
dm 'log'    clear;

title h=1 'Use-Dependent Inactivation in Sodium Channel Beta Subunit Mutation';

/* This dataset was provided by Csilla Egri as part of a 2011 M.Sc.
   Thesis C121W: A thermosensitive sodium channel mutation''. 
   Additional # details are available at
   http://dx.doi.org/10.1016/j.bpj.2010.12.2506. */


/* Voltage gated sodium (NaV) channels are macromolecular complexes 
   which pass sodium specific inward current and are the main determinants 
   of action potential in initiation and propagation. 
   NaV normally associate with one or more auxiliary beta subunits 
   which modify voltage dependent properties. 
   Mutations to these proteins can cause epilepsy, 
   cardiac arrhythmias, and skeletal muscle disorders. */ 

/* Lines starting with *---partxxxe and *---parxxxb are used in my Latex file and 
   should be ignored */
/* The tagsets.tablesonlylatex again is used by the Latex Program course notes */



options noovp nodate orientation=landscape;
ods document name=work.output(write);
ods pdf file='UDI.pdf';
goptions device=pdf colors=(black) rotate=landscape;

/* Read in the data */

*---part001b; 
data UDI;
   infile 'UDI.csv' dlm=',' dsd missover firstobs=2;
   length subunit $20;
   input Subunit $ Temp UDI_Yo;
run;

 
proc print data=UDI (obs=15);
   title2 'Part of the raw data';
run;
*---part001e; 

/* Compute the means and standard deviations and sample sizes for
  each treatment group. */
*---part002b; 
proc tabulate data=UDI;
   class Temp Subunit;
   var UDI_Yo;
   table temp*subunit, UDI_Yo*(n*f=4.0 mean*f=7.2 std*f=7.2);
run;
*---part002e; 
 
/* Plot the standard deviations against the mean */
*---part003b; 
proc sort data=UDI;
   by temp subunit;
run;
proc means data=UDI noprint;
   by temp subunit;
   var UDI_Yo;
   output out=sumstats mean=mean std=std n=n;
run;
proc sgplot data=sumstats;
   title2 'Std dev vs. the mean';
   yaxis label='Standard deviation' offsetmin=.05 offsetmax=.05;
   xaxis label='Mean'               offsetmin=.05 offsetmax=.05;
   scatter x=mean y=std / markerattrs=(symbol=circlefilled);
run;
*---part003e; 

*---part004b; 
/* Side-by-side dot plots to check for outliers */
/* Create the pseudo factor */
data UDI;
   set UDI;
   length trt $20.;
   trt = put(temp,2.0) || '.' || subunit;
run;
proc sgplot data=UDI;
   title2 'Side-by-side dot plots';
   yaxis label='UDI'        offsetmin=.05 offsetmax=.05;
   xaxis label='Treatmemt'  offsetmin=.05 offsetmax=.05;
   scatter x=trt  y=UDI_yo /  markerattrs=(symbol=circlefilled);
run;
*---part004e; 


/* Fit the linear model.
   We do this using both GLM (use the TYpe III SS) and Proc Mixed (using REML) */

*---part010b; 
ods graphics on; 
proc glm data=UDI plots=diagnostics;
   title2 'GLM analysis';
   class temp subunit;
   model UDI_Yo = temp subunit temp*subunit;
   lsmeans temp         / stderr pdiff cl adjust=tukey lines ;
   lsmeans subunit      / stderr pdiff cl adjust=tukey lines;
   lsmeans temp*subunit / stderr pdiff cl adjust=tukey lines;
   estimate 'CW vs reg at 34' 
       subunit 0 1 -1 
       subunit*temp 0 0 0   0 1 -1 / e;
   ods output ModelAnova=GLMModelAnova;
run;
ods graphics off;
*---part010e; 

*---part011b;  
ods graphics on;
proc mixed data=UDI plot=residualpanel;
   title2 'Mixed analysis';
   class temp subunit;
   model UDI_Yo = temp subunit temp*subunit / ddfm=kr ;
   lsmeans temp         / diff adjust=tukey;
   lsmeans subunit      / diff adjust=tukey;
   lsmeans temp*subunit / diff adjust=tukey;
   estimate 'CW vs reg at 34' 
       subunit 0 1 -1 
       subunit*temp 0 0 0   0 1 -1 / e;
   ods output tests3=MixedTests;
   ods output lsmeans=MixedLSMeans;
   ods output diffs=MixedDiffs;
run;
ods graphics off;
*---part011e; 

ods pdf close;
ods document close;


/* Now to replay selected portions of the output. This would not normally
   be done by the user, but I used this for the latex documents */
/* First see the names of the objects in the document store */

proc document name=work.output;
  list / levels=all;
run;
quit;

%include '../../MyLaTeXtagset.sas'; run;
title ;
footnote ;
ods listing;

ods tagsets.Mycolorlatex file='UDI-SAS-001.tex' (notop nobot) ;
proc print data=UDI (obs=15);
run;
ods tagsets.mycolorlatex close;

ods tagsets.Mycolorlatex file='UDI-SAS-002.tex' (notop nobot) ;
proc document name=work.output;
   obstitle \Tabulate#1\Report#1\Table#1 ;
   obtitle  \Tabulate#1\Report#1\Table#1 ;
   replay  \Tabulate#1\Report#1\Table#1 / levels=all;
run;
ods tagsets.mycolorlatex close;

ods graphics on / imagefmt=png imagename='UDI-SAS-003' reset=index;
proc document name=work.output;
   replay  \sGplot#1\sGPLOT#1 / levels=all dest=(listing);
run;
ods graphics off;

ods graphics on / imagefmt=png imagename='UDI-SAS-004' reset=index;
proc document name=work.output;
   replay  \sGplot#2\sGPLOT#1 / levels=all dest=(listing);
run;
ods graphics off;

ods tagsets.Mycolorlatex file='UDI-SAS-010a.tex' (notop nobot) ;
proc print data=MixedTests noobs label split=' ';
run;
ods tagsets.mycolorlatex close;
ods tagsets.Mycolorlatex file='UDI-SAS-010b.tex' (notop nobot) ;
proc print data=GLMModelAnova noobs label split=' ';
   where hypothesistype=3;
   label hypothesistype='Hypothesis Type';
run;
ods tagsets.mycolorlatex close;


ods graphics on / imagefmt=png imagename='UDI-SAS-011' reset=index;
proc document name=work.output;
   replay  \GLM#1\ANOVA#1\UDI_Yo#1\DiagnosticsPanel#1 / levels=all dest=(listing);
run;
ods graphics off;


ods graphics on / imagefmt=png imagename='UDI-SAS-012' reset=index;
proc document name=work.output;
   replay  \GLM#1\ANOVA#1\UDI_Yo#1\IntPlot#1 / levels=all dest=(lisitng);
run;
ods graphics off;


ods tagsets.Mycolorlatex file='UDI-SAS-013.tex' (notop nobot) ;
proc print data=MixedLSMeans noobs label split=' ';
run;
ods tagsets.mycolorlatex close;

ods tagsets.Mycolorlatex file='UDI-SAS-014.tex' (notop nobot) ;
proc print data=MixedDiffs(drop=df tvalue probt adjustment) noobs label split=' ';
   label estimate='Est';
   label stderr='SE';
run;
ods tagsets.mycolorlatex close;

ods tagsets.Mycolorlatex file='UDI-SAS-015a.tex' (notop nobot) ;
proc document name=work.output;
   obtitle \Mixed#1\Coef#1 ;
   replay  \Mixed#1\Coef#1 /levels=all;
run;
ods tagsets.mycolorlatex close;
ods tagsets.Mycolorlatex file='UDI-SAS-015b.tex' (notop nobot) ;
proc document name=work.output;
   obtitle \Mixed#1\Estimates#1 ;
   replay  \Mixed#1\Estimates#1 /levels=all;
run;
ods tagsets.mycolorlatex close;


ods listing close;
