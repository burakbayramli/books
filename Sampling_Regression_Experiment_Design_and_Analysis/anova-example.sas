/* Intuitive explanation of ANOVA tables */
 

/* Consider the two following experiments - which do you think is more indicative of an effect? 

       Experiment I      Experiment II

         Method             Method
         A   B  C          A   B  C
        ---------          ---------
        65  84 75          80 100 60
        66  85 76          65  85 75
        64  86 74          50  70 90
        ---------          ---------
Average 65  85 75          65  85 75  */

/* Line starting with *---partxxxb; and *---partxxxe; are for inclusion by my LaTeX code
   and can be ignored. */

dm 'output' clear;
dm 'log'    clear;
proc datasets kill; run; 

options nodate  noovp orientation=landscape;
ods pdf file='anova-example-SAS.pdf';
goptions device=pdf colors=(black) rotate=landscape;

title 'Intuitive explanation of ANOVA';
 
data example;
   input a1 b1 c1   a2 b2 c2;
   experiment = 1; trt='a'; Y =a1; output;  /* convert to proper structure */
   experiment = 1; trt='b'; Y =b1; output;
   experiment = 1; trt='c'; Y =c1; output;

   experiment = 2; trt='a'; Y =a2; output;
   experiment = 2; trt='b'; Y =b2; output;
   experiment = 2; trt='c'; Y =c2; output;
   keep experiment trt y;
   datalines;
        65  84 75          80 100 60
        66  85 76          65  85 75
        64  86 74          50  70 90
;;;;


proc sort data=example; 
   by experiment trt;
run;

proc print data=example;
   by experiment;
   title2 'raw data in proper format';
run;
 
proc gplot data=example;
   title2 'dot plots';
   axis1 offset=(1 cm, 1 cm);
   axis2 order=50 to 100 by 10;
   by experiment;
   plot y*trt / haxis=axis1 vaxis=axis2;;
   symbol1 v=plus i=none;
run;
 
ods graphics on;
proc glm data=example /*plots=all */  ; /* uncommenting plots=all crashes my system */
   title2 'analysis of experiments';
   by experiment;
   class trt;
   model y= trt;
   lsmeans trt / stderr cl pdiff adjust=tukey;
run;
ods graphics off;
 
ods pdf close;

