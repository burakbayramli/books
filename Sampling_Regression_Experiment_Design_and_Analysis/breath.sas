/* Two-factor Split-plot */
 
/* Breath holding times.
   How does the time that a subject can hold its breath vary by the temperature of the water
   in which you are immersed. Does it vary between males and females?

   Several subjects of each sex were asked to hold their breath when immersed in
   water of various temperatures. The time (seconds) was recorded.

   This data provided by Matthew D. White in BPK of Kinesology */

/* Statements of the form *---part01b; and *---part01e; are for the LaTex notes file and can be removed */

dm 'output' clear;
dm 'log'    clear;
proc datasets kill;
run;

options nodate orientation=landscape;
ods pdf file='breath-SAS.pdf';
goptions device=pdf colors=(black) rotate=landscape;

title 'How long can you hold your breath?';
 
*---part001b;
data breath;
   infile 'breath.csv' dlm=',' dsd missover firstobs=2;
   length gender $10.;
   input subject height gender temp0 temp5 temp10 temp15 temp20 air;
   /* convert to standard format */
   temp = 0;  time=temp0;   output;
   temp = 5;  time=temp5;   output;
   temp =10;  time=temp10;  output;
   temp =15;  time=temp15;  output;
   temp =20;  time=temp20;  output;
   temp =25;  time=air;     output;   /* temperature in air */
   keep gender subject height temp time;
run;
*---part001e;
 
proc print data=breath;
   title2 'part of the raw data';
run;

proc export data=breath file='breath2.csv' dbms=csv replace label;
run;

/* See if a transformation is needed. For example, you might think that the differences
   in breath time are not consistent (e.g. a person which good control will be less
   affected in absolute time than a person with poor control */
proc sort data=breath; by subject temp;
ods document name=plot1(write);
*---part002b;
proc sgplot data=breath;
   title2 'profile plot';
   series x=temp y=time / group=subject lineattrs=(color=black) datalabel=subject;
run;
*---part002e;
ods document close;


/* Remove subjects 3 and 6 from the analysis as they look like an outlier */
*---part003b;
data breath_nooutlier;
   set breath;
   if subject in (3,6) then delete;
run;
*---part003e;

ods document name=plot2(write);
proc sgplot data=breath_nooutlier;
   title2 'profile plot with subjects 3 and 6 removed';
   series x=temp y=time / group=subject lineattrs=(color=black) datalabel=subject;
run;
ods document close;


/* Split-plot analysis on original measurements */
ods document name=mixed(write);
ods graphics on;
*---part010b;
proc mixed data=breath_nooutlier plots=all;
   title2 'Analysis after outliers removed';
   class gender temp subject;
   model time = gender | temp / ddfm=kr;
   random subject(gender);
   lsmeans gender*temp / diff cl;
   lsmeans gender / diff cl;
   lsmeans temp / diff cl adjust=tukey;
   ods output Tests3 = MixedTests;
   ods output LSmeans= MixedLSmeans;
   ods output diffs = MixedLSmeansDiffs;
   ods output covparms=MixedCovParms;
run;
*---part010e;
ods graphics off;
ods document close;


/* Create the joined line plot for the temperature effect */
ods document name=tempjoinedline(write);
title;
*---part020b;
%include '../../pdmix800.sas'; run;
data tempdiffs;
   set mixedlsmeansdiffs;
   if index(lowcase(effect),'gender')=0 and index(lowcase(effect),'temp')>0;
run;
data tempmeans;
   set mixedlsmeans;
   if index(lowcase(effect),'gender')=0 and index(lowcase(effect),'temp')>0;
run;
%pdmix800(tempdiffs, tempMeans, alpha=0.05, sort=yes);
*---part020e;
ods document close;


/* Create the profile plot of the mean response over time by gender */
ods document name=Profile(write);
*---part011b;
data plotdata;
   set mixedlsmeans;
   if lowcase(gender)='male' then temp = temp - .3; /* create a bit of jitering */
run;
proc sgplot data=Plotdata;
   title2 'Profile plot of estimated mean time to hold breath';
   where index(lowcase(effect),'gender')>0 and index(lowcase(effect),'temp')>0;
   series x=temp y=estimate / group=gender lineattrs=(color=black) datalabel=gender;
   highlow x=temp low=lower high=upper / group=gender;
   xaxis offsetmin=.05 offsetmax=.05 label='Water Temperature (C)';
   yaxis label='Estimated mean time to hold breath (s)';
   footnote 'Bars are 95% confidence intervals';
   footnote2 'Plotting positions jittered slightly to avoid overplotting';
   footnote3 'Temperature = 25 = Ambient air';
run;
*---part011e;
ods document close;



/*************************** Add the effect of height ***************************/

ods graphics on;
*---part030b;
proc mixed data=breath_nooutlier plots=all;
   title2 'Analysis after outliers removed - Height covariate included';
   class gender temp subject;
   model time = height gender | temp / ddfm=kr;
   random subject(gender);
   lsmeans gender*temp / diff cl;
   lsmeans gender / diff cl;
   lsmeans temp / diff cl adjust=tukey;
   ods output Tests3 = MixedTestsHeight;
   ods output LSmeans= MixedLSmeansHeight;
   ods output diffs = MixedLSmeansDiffsHeight;
run;
*---part030e;
ods graphics off;

/* Create the profile plot of the mean response over time by gender */
ods document name=ProfileHeight(write);
*---part031b;
data plotdata3;
   set mixedlsmeansheight;
   if lowcase(gender)='male' then temp = temp - .3; /* create a bit of jitering */
run;
proc sgplot data=Plotdata3;
   title2 'Profile plot of estimated mean time to hold breath - adjusted for height';
   where index(lowcase(effect),'gender')>0 and index(lowcase(effect),'temp')>0;
   series x=temp y=estimate / group=gender lineattrs=(color=black) datalabel=gender;
   highlow x=temp low=lower high=upper / group=gender;
   xaxis offsetmin=.05 offsetmax=.05 label='Water Temperature (C)';
   yaxis label='Estimated mean time to hold breath (s)';
   footnote 'Bars are 95% confidence intervals';
   footnote2 'Plotting positions jittered slightly to avoid overplotting';
   footnote3 'Temperature = 25 = Ambient air';
run;
*---part031e;
ods document close;



/*************************** Use Temperature as a continuous variable ***************************/
/* we remove the ambient air temperature */
data breath_nooutlier_noair;
   set breath_nooutlier;
   if temp = 25 then delete;
run;

ods graphics on;
*---part050b;
proc mixed data=breath_nooutlier_noair plots=all;
   title2 'Analysis after outliers removed - Temp as continuous variable';
   class gender subject;
   model time = gender | temp / ddfm=kr;
   random subject(gender);
   estimate 'female 0'  intercept 1 gender 1 0  temp 0   gender*temp  0  0/ cl;
   estimate 'female 5'  intercept 1 gender 1 0  temp 5   gender*temp  5  0/ cl;
   estimate 'female 10' intercept 1 gender 1 0  temp 10  gender*temp 10  0/ cl;
   estimate 'female 15' intercept 1 gender 1 0  temp 15  gender*temp 15  0/ cl;
   estimate 'female 20' intercept 1 gender 1 0  temp 20  gender*temp 20  0/ cl;
   estimate 'male    0' intercept 1 gender 0 1  temp 0   gender*temp  0  0/ cl;
   estimate 'male 5'    intercept 1 gender 0 1  temp 5   gender*temp  0  5/ cl;
   estimate 'male 10'   intercept 1 gender 0 1  temp 10  gender*temp  0 10/ cl;
   estimate 'male 15'   intercept 1 gender 0 1  temp 15  gender*temp  0 15/ cl;
   estimate 'male 20'   intercept 1 gender 0 1  temp 20  gender*temp  0 20/ cl;
   ods output Tests3   = MixedTestsReg;
   ods output Estimates= Mixedestimatesreg;
run;
*---part050e;
ods graphics off;

proc print data=Mixedestimatesreg;
run;

/* Create the profile plot of the mean response over time by gender */
ods document name=ProfileReg(write);
*---part051b;
data plotdata2;
   set mixedEstimatesReg;
   length gender $10.;
   if index(lowcase(label),'female')>0 then gender='Female'; else gender='Male';
   temp = scan(label,2)+0;
   estimatereg=estimate;
   estimate = .;
   lowerreg = lower;
   upperreg = upper;
   if lowcase(gender)='male' then temp=temp-.3;
   effect = 'gender*temp';
run;
data plotdata2;
   set plotdata2 plotdata;
   if temp>24 then delete;
run;
proc sgplot data=Plotdata2;
   title2 'Profile plot of estimated mean time to hold breath - regression approach';
   where index(lowcase(effect),'gender')>0 and index(lowcase(effect),'temp')>0  ;
   scatter x=temp y=estimate / group=gender datalabel=gender markerattrs=(symbol=circlefilled);
   series  x=temp y=estimatereg / group=gender lineattrs=(color=black);
   highlow x=temp low=lowerreg high=upperreg / group=gender;
   xaxis offsetmin=.05 offsetmax=.05 label='Water Temperature (C)';
   yaxis label='Estimated mean time to hold breath (s)';
   footnote 'Bars are 95% confidence intervals';
   footnote2 'Plotting positions jittered slightly to avoid overplotting';
run;
*---part051e;
ods document close;



/********************* Power analysis/Sample Size determination *************/
/* We use the method of Stroup to estimate the power/sample size to detect effect
   size of interest. Refer to the course notes for details */

data means;  /* Enter guestimates of the means at each combination of temperature and gender */
   input temp male_mean female_mean;
   length gender $10.;
   gender = 'Male';    mu = male_mean; output;
   gender = 'Female';  mu = female_mean; output;
   keep temp gender mu;
   datalines;
0       20       10   
5        25       15   
10       30       20   
15       35       25  
20       40       30 
;;;; 

proc print data=means;
   title2 'Guestimates of means for planning purposes';
run;
 
data fake_data;  /* generate fake data with the means for each combination of total number of subjects */
   set means;
   do subjects_per_gender = 4 to 30 by 2;
      do subject=1 to subjects_per_gender;  /* half subjects are male and half are female */
         output;
      end;
   end;
run;

proc sort data=fake_data;
   by subjects_per_gender;
run;
 
proc tabulate data=fake_data;
   title2 'Summary of conditions tested';
   class subjects_per_gender gender temp;
   var mu;
   table subjects_per_gender, (gender ALL)*temp*mu*n*f=5.0;
run;
 
/* Now for method of Stroup. We analyze the fake data specifying the
   variance components etc and then pass the resulting F-statistics to get the 
   non-centrality parameter and estimated power */
 
%let alpha=.05;   /* specify the alpha level */

/* Estimate the non-centrality parameter */
ods select none; run; /* turn the output off as it is not needed */
proc mixed data=fake_data noprofile;
   title2 'Estimate the non-centrality parameter ';
   by subjects_per_gender;
   class gender subject temp;
   model mu = gender | temp;
   random subject(gender);
   parms   (81) (100) / noiter;   /* this is where the estimated  variance components are specified for gender and residual */
   /* save the results to the ods datasets */
   ods output tests3=power;
run;
ods select all; run;   /* turn the output back on */

/* now to compute approximations to the power */
data power;
   set power;
   nc = numdf*Fvalue;  /* approximate non-centrality parameter */
   fcrit = finv(1-&alpha, numDF, denDF, 0);  /* estimate critical value */
   power = 1 - probf(fcrit, numdf, dendf, nc);  /* estimated power */
   attrib power label='Power' format=7.2;
   attrib nc    label='Non-centrality' format=7.1;
   attrib fcrit label='F-critical value' format=7.2;
   drop probF;
run;

ods tagsets.mycolorlatex file='breath-SAS-071.tex' (notop nobot);
proc tabulate data=power;
   title ;
   class subjects_per_gender effect;
   var power;
   table subjects_per_gender, effect*power*sum=' '*f=7.3;
   footnote;
run;
ods tagsets.mycolorlatex close;

ods document name=PowerPlot(write);
proc sgplot data=power;
   title2 "Estimated power at alpha=&alpha";
   yaxis label='Power' values=(0 to 1 by .05) ;
   series y=power x=subjects_per_gender/  group=effect;
   refline 0.8 / axis=y;
run;
ods document close;


ods pdf close;






/* Create LaTeX files for inclusion by my notes */
%include "../../MyLaTeXtagset.sas"; run;
title;
footnote;
ods listing;

proc document name=tempjoinedline;
 list /levels=all;
run;


ods tagsets.mycolorlatex file='breath-SAS-001.tex' (notop nobot);
proc print data=breath (obs=10);
run;
ods tagsets.mycolorlatex close;

ods graphics on / outputfmt=png imagename="breath-SAS-002" reset=index;
proc document name=plot1;
  replay \Sgplot#1\SGPlot#1 / levels=all;
run;
ods graphics off;

ods graphics on / outputfmt=png imagename="breath-SAS-003" reset=index;
proc document name=plot2;
  replay \Sgplot#1\SGPlot#1 / levels=all;
run;
ods graphics off;

ods tagsets.mycolorlatex file='breath-SAS-010.tex' (notop nobot);
proc print data=MixedTests noobs split=' ' label;
run;
ods tagsets.mycolorlatex close;

ods graphics on / outputfmt=png imagename="breath-SAS-011" reset=index;
proc document name=profile;
  replay \Sgplot#1\SGPlot#1 / levels=all;
run;
ods graphics off;


ods tagsets.mycolorlatex file='breath-SAS-015.tex' (notop nobot);
proc print data=MixedLSmeansDiffs noobs split=' ' label;
   where lowcase(effect)='gender';
   var effect estimate stderr lower upper;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='breath-SAS-020.tex' (notop nobot);
proc document name=tempjoinedline;
  obtitle \Print#1\ByGroup1#1\Print#1 ;
  obfootn \Print#1\ByGroup1#1\Print#1 ;
  replay \Print#1\ByGroup1#1\Print#1 / levels=all;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='breath-SAS-021.tex' (notop nobot);
proc print data=MixedLSmeansDiffs noobs split=' ' label;
   where index(lowcase(effect),'gender')=0 and index(lowcase(effect),'temp')>0;
   var effect temp _temp estimate stderr adjp adjlower adjupper;
run;
ods tagsets.mycolorlatex close;

ods graphics on / outputfmt=png imagename="breath-SAS-025" reset=index;
proc document name=mixed;
  replay \Mixed#1\ResidualPlots#1\ResidualPanel#1 / levels=all;
run;
ods graphics off;



ods tagsets.mycolorlatex file='breath-SAS-030.tex' (notop nobot);
proc print data=MixedTestsHeight noobs split=' ' label;
run;
ods tagsets.mycolorlatex close;

ods graphics on / outputfmt=png imagename="breath-SAS-031" reset=index;
proc document name=profileheight;
  replay \Sgplot#1\SGPlot#1 / levels=all;
run;
ods graphics off;


ods tagsets.mycolorlatex file='breath-SAS-050.tex' (notop nobot);
proc print data=MixedTestsReg noobs split=' ' label;
run;
ods tagsets.mycolorlatex close;

ods graphics on / outputfmt=png imagename="breath-SAS-051" reset=index;
proc document name=profilereg;
  replay \Sgplot#1\SGPlot#1 / levels=all;
run;
ods graphics off;



ods tagsets.mycolorlatex file='breath-SAS-070.tex' (notop nobot);
proc print data=MixedCovParms noobs split=' ' label;
run;
ods tagsets.mycolorlatex close;

ods graphics on / outputfmt=png imagename="breath-SAS-072" reset=index;
proc document name=PowerPlot;
  replay \Sgplot#1\SGPlot#1 / levels=all;
run;
ods graphics off;


ods listing close;
