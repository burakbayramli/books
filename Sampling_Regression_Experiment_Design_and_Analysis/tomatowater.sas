options nodate;
title 'Effect of moisture on yield of tomatos';
title2 'An example of a polynomial regression in one variable';
 
data tomato;
   input moist yield;
   /* now we compute the square and cubic powers. With most modern
      computer packages it is not necessary to center the
      variables before taking powers as recommended in NWK */
   moist2 = moist*moist;
   moist3 = moist*moist*moist;
   cards;
6    49.2
6    48.1
6    48.0
6    49.6
6    47.0
8    51.5
8    51.7
8    50.4
8    51.2
8    48.4
10   51.1
10   51.5
10   50.3
10   48.9
10   48.7
12   48.6
12   47.0
12   48.0
12   46.4
12   46.2
14   43.2
14   42.6
14   42.1
14   43.9
14   40.5
;;;;
 
proc print data=tomato;
  title3 'raw data';
 
proc plot data=tomato;
  title3 'preliminary plot';
  plot yield*moist;

*-------------------------------------------------------------;
/* Fit a simple linear relationship to see what happens */ 
proc reg data=tomato;
   title3 'What if a simple linear regression to data?';
   model yield = moist;
   output out=fitreg p=pred r=resid;
proc plot data=fitreg;
   title4 'residual plot';
   plot resid*moist / vref=0;
   footnote 'notice the obvious pattern to the residuals';

*-------------------------------------------------------------;
* Fit a cubic and then test if we can drop higher order term;
proc reg data=tomato;
   title3 'Fit a cubic and test higher order terms';
   model yield = moist moist2 moist3;
   test1: test moist3=0;
   test2: test moist3=0, moist2=0;
   footnote ;

*-------------------------------------------------------------;
* Finally, fit a quadratic and check residuals; 
proc reg data=tomato;
   title3 'fit a quadratic regression to data';
   model yield= moist moist2;
   output out=fitreg p=pred r=resid;

proc plot data=fitreg;
   title4 'residual plot';
   plot resid*moist / vref=0;
 
proc plot data=fitreg;
   title4 'fitted response surface';
   plot yield*moist='*' pred*moist='x' / overlay;

proc rank data=fitreg normal=vw ties=high out=rfitreg;
   var resid;
   ranks normscr;
proc plot data=rfitreg;
   title4 'normal probability plot';
   plot resid*normscr $ moist;
   label normscr = 'Normal score';
   format moist 2.0;
 
*-------------------------------------------------------------;
* An illustration of RSREG - response surface regression;
proc sort data=tomato;  /* must sort by X to get lack-of-fit test */
   by moist;
proc rsreg data=tomato;
   title3 'Illustration of response surface regression';
   model yield = moist /lackfit;
