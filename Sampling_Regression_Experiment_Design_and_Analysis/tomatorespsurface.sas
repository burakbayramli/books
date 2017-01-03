/* Problem 9.12 of NWK */
 
/* An agronomist studied the effects of moisture (inches)
   and temperature (celcius) on the yield of a new hybrid
   tomato.   */
 
title 'Effect of moisture and temperature on yield of tomato';
options nodate noovp linesize=80 nocenter;

data yield;
   input yield moist temp;
   mm = moist**2;
   tt = temp**2;
   mt = moist*temp;
   datalines;
   49.2    6.0   20.0
   48.1    6.0   21.0
   48.0    6.0   22.0
   49.6    6.0   23.0
   47.0    6.0   24.0
   51.5    8.0   20.0
   51.7    8.0   21.0
   50.4    8.0   22.0
   51.2    8.0   23.0
   48.4    8.0   24.0
   51.1   10.0   20.0
   51.5   10.0   21.0
   50.3   10.0   22.0
   48.9   10.0   23.0
   48.7   10.0   24.0
   48.6   12.0   20.0
   47.0   12.0   21.0
   48.0   12.0   22.0
   46.4   12.0   23.0
   46.2   12.0   24.0
   43.2   14.0   20.0
   42.6   14.0   21.0
   42.1   14.0   22.0
   43.9   14.0   23.0
   40.5   14.0   24.0
     .     7     22.0   new x for predictions
;;;;
 
proc print data=yield;
   title2 'raw data';
 
proc reg data=yield;
   title2 'initial response surface';
   model yield = moist temp mm tt mt / p clm;
   output out=fitreg p=pred r=resid;
   ctest: test mt=0, tt=0;
 
proc plot data=fitreg;
   title3 'residual and other plots';
   plot yield*pred;


*---------------- fit a simpler model with no interaction ----;
proc reg data=yield;
   title2 'simpler model with no interaction';
   model yield = moist temp mm tt;
 
*---------------- fit final model with no interaction ----;
 
/* generate the grid for the contour plot  */
data grid;
   do moist = 6 to 14 by .25;
      do temp = 20 to 24 by .1;
         mm = moist **2;
         yield = .;
         output;
      end;
   end;
 
data yield;
   set yield grid;

proc reg data=yield;
   title2 'final model with no interaction';
   model yield = moist temp mm;
   output out=fitregf p=pred r=resid;
 
proc plot data=fitregf;
   title3 'residual and other plots';
   plot resid*(pred moist temp) / vref=0;
 
proc plot data=fitregf;
   title3 'contour plot of response';
   plot temp*moist=pred / contour;

/*now for the normal probability plot */
/* construct the normal probability plot */
proc rank data=fitregf normal=vw ties=high out=rfitreg;
   var resid;
   ranks normscr;
proc plot data=rfitreg;
   title3 'normal probability plot';
   plot resid*normscr;
   label normscr = 'Normal score';

*----------------- now for rsreg ----------;
proc rsreg data=yield;
   title2 'response surface regression';
   model yield = moist temp;
 
