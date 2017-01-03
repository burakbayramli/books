/* Analysis of barley yields data set */
/* This SAS program illustrates how to create side-by-side box plots 
   and to compute some summary statistics */

title 'Comparison of barley yields';
options nodate nonumber noovp;
 
data barley;
   infile 'barleyyields.txt' firstobs=15 expandtabs;
   input obs y1980 y1982 y1986;   /* input the three yields */
   /* now to structure the data into two columns. One column has the year, and the second
      column has the yield. Eliminate any missing values as these are artefacts of the
      data file */
   year = 1980; yield= y1980; if obs < 51 then  output;
   year = 1982; yield= y1982; if obs < 41 then  output;
   year = 1986; yield= y1986; if obs < 56 then  output;
   keep year yield;

proc print data=barley (obs=20);
   title2 'part of the raw data';
 
proc tabulate data=barley;  /* get some simple summary statistics */
   title2 'some simple summary statistics';
   class year;
   var yield;
   table year, yield*(n*f=5.0 mean*f=7.1 std*f=5.1) / rts=20;
run;

/* set up the graphics file */

filename gsasfile 'barleyyields.ps';
goptions device=ps300 gaccess=sasgaedt 
         gsflen=80 gsfname=gsasfile gsfmode=replace rotate=landscape
         colors=(black);

/* create side-by-side dot plots with some jittering of the data points */

data plotdata1;
   set barley;
   year = year + rannor(2343)*.05;   /* add random noise to the year variable */

proc gplot data=plotdata1;
   title2 'a side-by-side dot plot with data points jittered';
   axis1 label=(a=90 r=0 'Yield (g/400 m2)') ;
   axis2 label=('Year') order=1975 to 1990 by 1;
   plot yield*year / vaxis=axis1 haxis=axis2;
run;

/* create side-by-side box plots */
proc sort data=barley; by year;
goptions gsfmode=append;  /* allow more plots to be appended */
proc boxplot data=barley;
   title2 'side-by-side box plots';
   plot yield*year;
run;

/* create histograms of the three years */
proc gchart data=barley;
   title2 'histograms of the yields';
   vbar yield / midpoints=130 to 390 by 20 type=percent group=year space=0;
run;
    

 
x 'ps2pdf barleyyields.ps barleyyields.pdf';
x 'chmod o+r *.pdf';
x '/usr/bin/rm *.ps';
   
 

