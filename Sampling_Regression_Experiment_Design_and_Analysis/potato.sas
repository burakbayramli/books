/* A sample SAS program to analyze the potato data */

/* Line starting with *---partxxxb; and *---partxxxe; are for inclusion by my LaTeX code
   and can be ignored. */
dm 'output' clear;
dm 'log'    clear;
proc datasets kill; run; 


options nodate noovp orientation=landscape;
ods pdf file='potato-SAS.pdf';
goptions device=pdf colors=(black) rotate=landscape;

title 'Potato peeling analysis';
 
/* Virtually all SAS programs consist of a DATA step where
   the raw data is read into a SAS file, and procedure (PROC)
   step which perform various analyses */
 
data time;    /* this will read the data into an internal SAS dataset called time */
   infile 'potato.csv' dlm=',' dsd missover firstobs=2;
   input group $ time; /* the $ indicates a character variable */
run;

proc print data=time;  /* this will print out the raw data  for checking */
   title2 'raw data'; 
run;
 
proc sort data=time;
   by group;
run;

proc tabulate data=time;
   title2 'simple summary statistics';
   class group;
   var time;
   table group*time, n*f=5.0 mean*f=7.2 std*f=7.2;
run;
 
proc gplot data=time;   /* request a plot of the raw data */
   title2 'do plot of the raw data';
   axis1 offset=(1 cm, 1 cm);
   plot time*group /haxis=axis1;
   symbol1 v=plus i=stdmj2;  /* draw points, 95% ci, and join means */
   footnote 'Raw data and 95% confidence intervals';
run;

/* You can use GLM or Mixed for the analysis */
ods graphics on;
proc glm data=time plots=all;   /* GLM = General linear model = anova analysis */
   title2 'Analysis using GLM';
   class group;       /* class statement indicates that group is a factor */
   model time = group;/* builds a model. Note the grand mean is implied */
   lsmeans group / stderr pdiff adjust=tukey lines; /* multiple comparison with tukey adjustment */
   footnote ' ';
run;
ods graphics off;
 
ods graphics on;
proc mixed data=time plots=all;   /* Mixed = mixed models */
   title2 'Analysis using Mixed';
   class group;       /* class statement indicates that group is a factor */
   model time = group;/* builds a model. Note the grand mean is implied */
   lsmeans group / adjust=tukey diff; /* multiple comparison with tukey adjustment */
   ods output tests3 =MixedTest; 
   ods output lsmeans=MixedLsmeans;
   ods output diffs  =MixedDiffs;
run;
ods graphics off;

/* Get a joined lines plot */
%include 'pdmix800.sas';
%pdmix800(MixedDiffs,MixedLsmeans,alpha=0.05,sort=yes);

ods pdf close;
   
