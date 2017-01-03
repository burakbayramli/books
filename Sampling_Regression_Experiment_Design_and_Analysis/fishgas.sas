/* Effect of dissolved gas upon fish behaviour */
/* An experiment was conduced to investigate the effects of elevated 
   dissolved gas super-saturation upon the survival and behavior of fish.

  The experiment consisted of exposing the treatment fish to elevated 
  dissolved gas super-saturation (125\% TGP) and the control fish to normal,
  air-equilibrated water (102\% TGP), for a given duration of time (36 hours). 

  Two tanks are available for the experiment. In week 1, one tank
  was randomly assigned to the elevated dissolved gas treatment; 
  the other tank remained as the control; In week 2, the roles of the 
  tanks were reversed; in week 3, the roles of the tank reverted back 
  those in week 1. 

  In each week, twenty fish were randomly placed into each control 
  and each treatment tank. The fish were randomly selected and allocated 
  to the tanks, using groups of 10 fish each.

  This provided a total of 120 fish (60 Treated Fish - 20 per each of 3 
  replicates; and 60 Control fish - also 20 per each of 3 replicates).  

  At the end of the exposure, each fish was subjected to a swim test.  
  The fish were tested one at a time (alternating between treatment 
  and control fish) in a trough, and the time taken to escape (swim) 
  to "cover" was measured.  The length and weight of each fish was 
  measured after it finished the swim test.  The condition factor 
  was calculated from length and weight measurements.  */

/* Refer to the Case Studies Document in Stat403/650 for more details */

title 'Effect of dissolved gas on fish behaviour';
options nodate nonumber noovp;


data time;
   length status $4. dazed $3;
   input time trt $ block $ condfact weight length;
   status = 'live';
   if time > 89 then dazed = 'yes';
                else dazed = 'no ';
   datalines;
90.0    C     1     0.99     1.1     48
90.0    C     1     0.96     1.0     47
15.6    C     1     1.13     0.9     43
90.0    C     1     0.94     1.4     53
2.5     C     1     0.95     1.5     54
2.4     C     1     0.92     1.3     52
9.5     C     1     0.92     0.9     46
5.8     C     1     0.97     1.8     57
2.4     C     1     0.84     1.4     55
90.0    C     1     0.96     1.6     55
90.0    T     1     1.06     1.4     51
9.9     T     1     1.02     1.2     49
90.0    T     1     0.98     1.3     51
3.5     T     1     0.99     1.1     48
90.0    T     1     1.00     1.4     52
90.0    T     1     1.07     1.6     53
90.0    T     1     0.76     1.2     54
90.0    T     1     0.96     1.2     50
2.6     C     2     0.94     0.8     44
2.0     C     2     0.92     1.7     57
12.2    C     2     0.88     0.7     43
6.7     C     2     0.81     0.9     48
90.0    C     2     0.83     1.3     54
5.3     C     2     0.87     1.3     53
4.8     C     2     0.97     1.7     56
4.7     C     2     0.97     1.7     56
1.6     C     2     0.74     1.1     53
1.5     C     2     1.07     1.6     53
90.0    C     2     0.90     1.5     55
11.1    C     2     0.95     1.5     54
90.0    C     2     0.95     1.5     54
3.7     C     2     0.91     1.6     56
10.8    C     2     0.98     1.3     51
90.0    C     2     0.90     1.2     51
2.7     C     2     0.92     1.3     52
90.0    T     2     1.08     1.7     54
90.0    T     2     1.08     1.8     55
10.6    T     2     0.95     1.5     54
2.0     T     2     1.21     1.8     53
90.0    T     2     0.81     0.6     42
21.5    T     2     0.87     1.3     53
90.0    T     2     0.93     1.1     49
90.0    T     2     1.01     1.5     53
90.0    T     2     1.02     1.8     56
2.2     T     2     1.02     1.6     54
90.0    T     2     0.93     1.1     49
7.0     T     2     1.20     2.1     56
10.7    T     2     1.02     1.7     55
21.2    T     2     0.88     0.7     43
90.0    T     2     1.04     1.3     50
10.6    C     3     0.83     1.3     54
3.6     C     3     0.85     1.5     56
6.5     C     3     0.95     1.5     54
5.4     C     3     0.96     1.6     55
3.1     C     3     0.66     1.1     55
5.2     C     3     0.95     1.5     54
3.1     C     3     1.02     1.6     54
3.3     C     3     1.00     1.4     52
90.0    C     3     0.95     1.5     54
10.2    C     3     1.06     1.4     51
90.0    C     3     0.95     1.5     54
5.5     C     3     1.02     1.7     55
7.7     C     3     1.00     1.4     52
7.1     C     3     0.90     1.2     51
3.7     C     3     0.96     1.2     50
90.0    T     3     0.90     1.5     55
3.6     T     3     0.99     0.5     37
5.9     T     3     0.95     1.5     54
5.2     T     3     0.98     1.3     51
90.0    T     3     0.85     1.2     52
5.8     T     3     0.99     1.1     48
5.7     T     3     0.95     1.5     54
3.7     T     3     0.97     1.9     58
4.2     T     3     1.02     1.6     54
90.0    T     3     1.06     1.4     51
5.4     T     3     0.95     1.5     54
9.5     T     3     1.00     1.4     52
6.7     T     3     1.02     0.7     41
7.4     T     3     0.83     1.1     51
90.0    T     3     0.90     1.5     55  
;;;;


/* First add the dead fish back to the above data */

proc sort data=time; by trt block;
 
proc means data=time noprint;
   by trt block;
   var time;
   output out=countlive n=n; 

data dead;
   length status $4.;
   set countlive;
   do i=n+1 to 20;
      time = .;
      status = 'dead';
      condfact = .;
      weight = .;
      length = .;
      output;
   end;
 
data time2;
   set time dead; 
   drop i n _freq_ _type_;

proc sort data=time2;  by trt block;

proc print data=time2 (obs=40);
   title2 'expanded data set';
 
  

*-------------------------------------------------------------------;

/* Some basic statistics */
 
proc tabulate data=time2 missing;
   class status dazed trt block;
   var time;
   table trt*block, status*n*f=5.0;
   table trt*block, dazed*n*f=5.0;


      
*------------------------------------------------------------------;

/* Analysis of live/dead status */

proc genmod data=time2;
   title2 'Analysis of live/dead status';
   class trt block;
   model status = trt block / dist=binomial type3;
   estimate 'diff   '             trt -1 1 / exp;  /* estimate the difference on the logt scale and the odds ratio */



*------------------------------------------------------------------;

/* analysis of dazed/not dazed given that survived */ 
 
proc genmod data=time2;
   /* this analysis assumes that there are NO tank effects and that individual fish
      are independent among tanks */
   title2 'Analysis of dazed/not dazed';
   where status = 'live';
   class trt block;
   model dazed = trt block / dist=binomial type3;
   estimate 'diff   '             trt -1 1 / exp;  /* estimate difference on logit scale and odds ratio */


proc genmod data=time2;
   title2 'Analysis of dazed/not dazed - cluster effects - independent correlation structure - model based se';
   where status = 'live';
   class trt block;
   model dazed = trt block / dist=binomial type3;
   repeated subject=trt*block /type=indep modelse;      /* data is 'clustered' by tanks */
   estimate 'diff   '             trt -1 1 / exp;  /* estimate difference on logit scale and odds ratio */

proc genmod data=time2;
   title2 'Analysis of dazed/not dazed - cluster effects - compound symmetric covariances - model based se';
   where status = 'live';
   class trt block;
   model dazed = trt block / dist=binomial type3;
   repeated subject=trt*block / type=cs modelse ;      /* data is 'clustered' by tanks */
   estimate 'diff   '             trt -1 1 / exp;  /* estimate difference on logit scale and odds ratio */

proc genmod data=time2;
   title2 'Analysis of dazed/not dazed - cluster effects - compound symmetric - model based se - logodds';
   where status = 'live';
   class trt block;
   model dazed = trt block / dist=binomial type3;
   repeated subject=trt*block /logor=exch modelse;      /* data is 'clustered' by tanks */
   estimate 'diff   '             trt -1 1 / exp;  /* estimate difference on logit scale and odds ratio */

run;
      

/* Use nlmixed to fit model with normal error on logit of dazed variable */
/* first get dataset read for action */ 

data dazed;
   set time2;
   if status = 'live';
   if dazed='yes' then dazed2=1;
                  else dazed2=0;

proc sort data=dazed; by trt block;
 
proc means data=dazed noprint;
   by trt block;
   var dazed2;
   output out=dazed2 n=n sum=ndazed;
 
data dazed2;   /* create indicator variables for trt and block effects */
   set dazed2;
   xtrt = 0;
   if trt = 'C' then xtrt = 1;
 
   xblock1 = 0; xblock2 = 0;
   if block =1 then xblock1 = 1;
   if block =2 then xblock2 = 1;
   tank = xtrt*10 + block;  /* set up an indicator for each tank */
 
proc print data=dazed2;
   title2 'NLMIXED approach';
   title3 'raw data';

run;
   
proc nlmixed data=dazed2;
   title3 ' ';
   parms b0=0 bc=0 bb1=0 bb2=0 s2=1;
   eta = b0 + xtrt*bc + xblock1*bb1 + xblock2*bb2 + alpha;
   p = 1/(1+exp(eta));        /* convert from log-odds back to actual probability */ 
   model ndazed ~ binomial(n ,p);
   random alpha ~ normal(0,s2*s2) subject=tank;
 
   estimate 'odds for c' exp(bc);
run;

*------------------------------------------------------------------;

/* analysis of swim time give not dazed */
 
proc mixed data=time2;
   title2 'Analysis of time to swim';
   where status = 'live' and dazed='no';
   class trt block;
   model time = trt block / ddfm = satterth;
   random trt*block;   /* experimental unit effect */

   estimate 'diff   '             trt 1 -1;

run;




*-----------------------------------------------------------------;
/* try to use nlmixed to fit model with normal error on logit
   of dazed variable */
 
/* first get dataset read for action */ 

data dazed;
   set time2;
   if status = 'live';
   if dazed='yes' then dazed2=1;
                  else dazed2=0;

proc sort data=dazed; by trt block;
 
proc means data=dazed noprint;
   by trt block;
   var dazed2;
   output out=dazed2 n=n sum=ndazed;
 
data dazed2;   /* create indicator variables for trt and block effects */
   set dazed2;
   xtrt = 0;
   if trt = 'C' then xtrt = 1;
 
   xblock1 = 0; xblock2 = 0;
   if block =1 then xblock1 = 1;
   if block =2 then xblock2 = 1;
   tank = xtrt*10 + block;  /* set up an indicator for each tank */
 
proc print data=dazed2;
   title2 'NLMIXED approach';
   title3 'raw data';

run;
   
proc nlmixed data=dazed2;
   title3 ' ';
   parms b0=0 bc=0 bb1=0 bb2=0 s2=1;
   eta = b0 + xtrt*bc + xblock1*bb1 + xblock2*bb2 + alpha;
   p = 1/(1+exp(eta));        /* convert from log-odds back to actual probability */ 
   model ndazed ~ binomial(n ,p);
   random alpha ~ normal(0,s2*s2) subject=tank;
 
   estimate 'odds for c' exp(bc);
run;
