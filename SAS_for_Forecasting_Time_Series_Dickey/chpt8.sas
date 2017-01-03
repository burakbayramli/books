*-------------------------------------------------------------------*/
 /*         SAS FOR FORECASTING TIME SERIES, SECOND EDITION           */
 /*          by John C. Brocklebank and David A. Dickey               */
 /*       Copyright(c) 2003 by SAS Institute Inc., Cary, NC, USA      */
 /*                   SAS Publications order # 57275                  */
 /*  Jointly co-published by SAS Institute and John Wiley & Sons 2003 */
 /*                   SAS ISBN 1-59047-182-2                          */
                John Wiley & Sons, Inc. ISBN 0-471-39566-8            */
 /*-------------------------------------------------------------------*/
 /*                                                                   */
 /* This material is provided "as is" by SAS Institute Inc.  There    */
 /* are no warranties, expressed or implied, as to merchantability or */
 /* fitness for a particular purpose regarding the materials or code  */
 /* contained herein. The Institute is not responsible for errors     */
 /* in this material as it now exists or will exist, nor does the     */
 /* Institute provide technical support for it.                       */
 /*                                                                   */
 /*-------------------------------------------------------------------*/
 /* Questions or problem reports concerning this material may be      */
 /* addressed to the author:                                          */
 /*                                                                   */
 /* SAS Institute Inc.                                                */
 /* Books by Users                                                    */
 /* Attn: John Brocklebank and David Dickey                           */
 /* SAS Campus Drive                                                  */
 /* Cary, NC   27513                                                  */
 /*                                                                   */
 /*                                                                   */
 /* If you prefer, you can send email to:  sasbbu@sas.com             */
 /* Use this for subject field:                                       */
 /*     Comments for John Brocklebank and David Dickey                */
 /*                                                                   */
 /*-------------------------------------------------------------------*/

Chapter 8 - Data Mining and Forecasting

proc hpf data=vc_data.daily_stats_09aug02 outfor=forecasts lead=7 print=all;
   id date interval=day;
   forecast revenue;
run;
symbol1 v = none i = join l = 1 c = black
        height=.5 width=.5;
symbol2 v = =    i = join l = 2 c = green
        height=.5 width=.5;
symbol3 v = none i = join l = 1 c = red
        height=.5 width=.5;
proc gplot data=forecasts;
   plot actual*date = 1
        predict*date = 2
        lower*date = 3
        upper*date = 3 / overlay
        hminor = 0
        vminor = 0
        caxis = black
        frame ;
   title h=2 c=black f=swissb 'The Vermont Country Store Revenue';
   format actual dollar15.2;
run;
quit;


proc autoreg data=vc_data.daily_stats_09aug02;
   model revenue=buyer dollars_per_purch_session 
         /*perc_abandon_carts*/   productsuggestion_pages
         /*new_buy_perc*/
        /nlag=8 backstep slstay=.15 method=ml;
run;



proc model data=vc_data.daily_stats_09aug02 outmodel=outmodel;
   revenue = b0 + b1*buyer + b2*dollars_per_purch_session + 
             b3*productsuggestion_pages ;
   %ar(revenue,7,,1 5 7);
   fit revenue / outest=outest maxiter=1000
       method=marquardt;
run;
data goal goal2;
   set vc_data.daily_stats_09aug02(where=(date='09aug02'd) keep=
   revenue buyer dollars_per_purch_session productsuggestion_pages
   date);
   output goal;
   revenue = round(1.05*revenue); /* 5% increase */
   output goal2;
run;
proc model model=outmodel;
   solve buyer / estdata=outest data=goal2
         out=outsolve1;
   solve dollars_per_purch_session / estdata=outest data=goal2
         out=outsolve2;
   solve productsuggestion_pages / estdata=outest data=goal2
         out=outsolve3;
run; 
data solve; 
   set goal(in=goal) outsolve1(in=outsolve1) outsolve2(in=outsolve2)
       outsolve3(in=outsolve3) ;
   length type $ 31;
   if goal then type='';
   else if outsolve1 then type='Purchasing Sessions Solve';
   else if outsolve2 then type='Average Order Value Solve';
   else if outsolve3 then type='Product Suggestion Pages Solve';
run;
proc print data=solve;
   format productsuggestion_pages 7.2;
run;
