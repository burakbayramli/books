/* 2012-08-27 */
/* Power analysis for before vs. after study. */
/* Invertebrate data from a simple before/after analysis.

   Two streams were measured at a small hydro electric power project. At each stream
   multiple quadrats were taken on the steam, and the number of invertebrates
   was counted.

   After 5 years, the hydro electric plant started up, and an additional 5 years
   of data were collected.
 
   Is there evidence of a change in abundance after the plant starts?

   Note that this is NOT a BACI design, and is a VERY poor substitute for such.
   The key problem is that changes may have occured over time unrelated
   to the impact, so a "change" may simple be natural. 

   Also, in this analysis we have ignored autocorrelation which can be
   a serious problem in long-term studies. */

/* The analysis of the data was done in the invert.sas program and
   the variance components were extracted from program */

dm 'output' clear;
dm 'log'    clear;
proc datasets kill; run;

options orientation=landscape nodate noovp;
ods pdf file='invert-power-SAS.pdf';
goptions device=pdf colors=(black) rotate=landscape;

title 'Invertebrate Before/After Analysis - power analysis';



/* 1. Power analysis for a single stream before/after study 
      based on the analysis of the averages.
   Here the residual error is a combination of the year-to-year variation
   and the quadrat-to-qaudrat variation based on a "average" number of quadrats
   sampled each year. So the only thing that can change is the number of years before
   or after impact occurred.
 
   The power analysis is exactly the same as that for a two-sample t-test with 
   the number of years before or after as the sample sizes */
 
/* From the output the pooled estimate of variance is 6.7667**2 or a standard deviation of about 7 */
 
*---part100b;
ods graphics on;
proc power;
  title 'Power analysis for before/after analysis with one stream based on analysis of averages';
  footnote  'Residual std dev of 6.77 assumes an average of 5 quadrats/year';
  footnote2 'Inference limited to that single stream and cannot be generalized to other streams';
  /* We vary the total sample size to see what power is obttained */
  twosamplemeans
     test=diff     /* indicates that you wish to test for differences in the mean */
     meandiff=10   /* size of difference to be detected */  
     stddev=6.77   /* the standard deviation from analysis of stream 1 yearly averages */
     power=.       /* solve for power */
     alpha=.05     /* alpha level for the test */
     sides=2       /* a two sided test for difference in the mean should be done */
     groupns=5 | 5 to 20 by 1      /* number of years BEFORE (5) and AFter (5 to 10 by 1) to examine */
  ;                /* end of the twosamplemeans statement - DON'T FORGET THIS */ 
  ods output output=power100;
run;
ods graphics off; 
*---part100e;

/* Egads - over 20 years post impact are required to detect a 10 unit change in density !!! */
/* Perhaps we can improve things by changing the number of quadrats measured each years.
   So we now need to separte the quadarat-to-quadrat and year-to-year variation.
   We need to refer to the analysis on the individual values */



/* 2. Power analysis for a single stream before/after study 
      after changing the number of quadrats measured each year.
   
   We need estimates of the quadrat-to-quadrat variation and the year-to-year variation.
 
   The power analysis is the same as for a two-sample t-test with sub-sampling */
 
/* From the analysis we find that 
     residual variance = 59.17
     year-to-year variance = 33.79 */
/* Note that the estimate of the resudal variance in the previous analysis of   
      6.7667**2 = 45.78 is approx equal to  
          33.79 (year-to-year variance) + 59.17 (quadrat-to-quadrat variation)/(4.5 or the average number of quadrats/year) */

/* We could use the previous Proc Power with different estimates of residual variance depending on the number of quadrats
   measured per year. For example, suppose you could do 10 quadrats/year. Then the revised residual variance is approximately
      33.79 + 59.17/10 = 39.707 and the residual std deviation is 6.30 */

*---part200b;
 ods graphics on;
proc power;
  title 'Power analysis for before/after analysis with one stream based on analysis of averages';
  footnote  'Residual std dev of 6.3 assumes an average of 10 quadrats/year';
  footnote2 'Inference limited to that single stream and cannot be generalized to other streams';
  /* We vary the total sample size to see what power is obttained */
  twosamplemeans
     test=diff     /* indicates that you wish to test for differences in the mean */
     meandiff=10   /* size of difference to be detected */  
     stddev=6.3    /* 33.79 (year-to-year) + 59.17/10 (quad-to-quad/n-quad) */
     power=.       /* solve for power */
     alpha=.05     /* alpha level for the test */
     sides=2       /* a two sided test for difference in the mean should be done */
     groupns=5 | 5 to 20 by 1      /* number of years BEFORE (5) and AFter (5 to 10 by 1) to examine */
  ;                /* end of the twosamplemeans statement - DON'T FORGET THIS */  
  ods output output=power200;
run;
ods graphics off; 
*---part200e;

/* Power has improved, but notice that even if you took infinite quadrats/year you can't reduce
   the residual variance below that of the year-to-year variation. */

/* Note that the previous power analysis is to detect a difference for THAT particular stream only 
   and inference would be limited to that single stream only*/
 
* -----------------------------------------------------------;



 
/* 3. Power analysis for multiple streams before/after study 
      based on changing the number of streams and number of quadrats measured per year.
 
   Now inference is for the entire set of streams.
   
   We need estimates of the stream-to-stream, year-to-year, stream-year interaction variation, and the
   quadrat-to-quadrat variation.
  
/* From the analysis of multiple streams and individual values we find that 
     year-to-year variation = 38.63
     stream-to-stream variation = 37.90
     year-stream interaction variation = 0 (actually reported as negative)
     residual variance = 68.98 */



/* You can use the Stroup method for power analysis if you want to change the number of years for each site etc
   or you can use the previous methods if you assume that every site is measured every year and the same
   number of quadrats is measured in each year-site combination */
 
/* Stroup's method */
/* Stroup, W. W. (1999)
   Mixed model procedures to assess power, precision, and sample size in the design of experiments.
   Pages 15-24 in Proc. Biopharmaceutical Section. Am. Stat. Assoc., Baltimore, MD.

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
     PROC MIXED compute F values .

  4. Pass these to a data step that estimates the power
 
  5. Plot (or print) the results.

*/


%let alpha=.05;   /* specify the alpha level */
%let effect= 10;  /* effect size of interest */

%let year_to_year_var =36.64;
%let stream_to_stream_var =38.312;
%let stream_year_interaction_var = 00;
%let quad_to_quad_var = 66.35;

/* Generate the fake data for the effect size of interest */ 
data fake_data;
   length period $10.;
   Year_var = &year_to_year_var;  /* The variance components */
   Stream_var = &stream_to_stream_var;
   Stream_year_var = &stream_year_interaction_var;
   Quad_var = &quad_to_quad_var;

   do n_years_before = 5;
      do n_years_after = 5 to 20 by 1;
         do n_streams=4;
            do n_quadrats = 5;
               do year=1 to n_years_before + n_years_after;
                  period = 'before';
                  mu = 0;
                  if year > n_years_before then do;
                     period = 'after';
                     mu = &effect;  /* this is the effect size of interest */
                  end;
                  do stream=1 to n_streams;
                     do quadrat = 1 to n_quadrats;
                        output;   /* generate fake data */
                     end;
                  end;
               end;
            end;
         end;
      end;
   end;
run;

proc sort data=fake_data;
   by n_years_before n_years_after n_streams n_quadrats;
run;

proc tabulate data=fake_data;
   title2 'Summary of fake data';
   class n_years_before n_years_after n_streams n_quadrats year period stream;
   var mu;
   table n_years_before*n_years_after*n_streams*n_quadrats,
         year*period, stream*mu*(n*f=5.0 mean*f=5.0);
run;

/* Generate data sets for various sample sizes for later plotting purposes */
 
/* Estimate the non-centrality parameter */
ods select none; run; /* turn the output off as it is not needed */
proc mixed data=fake_data noprofile;
   title2 'Estimate the non-centrality parameter ';
   by n_years_before n_years_after n_streams n_quadrats
      year_var Stream_var Stream_year_var Quad_var;
   class period year stream;
   model mu = period;
   random year(period) stream stream*year(period);
   /* 
     year-to-year variation = 38.63
     stream-to-stream variation = 37.90
     year-stream interaction variation = 0 (actually reported as negative)
     residual variance = 68.98 */
   parms   (&year_to_year_var) 
           (&stream_to_stream_var) 
           (&stream_year_interaction_var) 
           (&quad_to_quad_var) / noiter;   /* this is where the estimated  variance components are specified */
   /* save the results to the ods datasets */
   ods output tests3=power_effects;
run;
ods select all; run;   /* turn the output back on */

/* now to compute approximations to the power */
data power_effects;
   set power_effects;
   nc = numdf*Fvalue;  /* approximate non-centrality parameter */
   fcrit = finv(1-&alpha, numDF, denDF, 0);  /* estimate critical value */
   power = 1 - probf(fcrit, numdf, dendf, nc);  /* estimated power */
   attrib power label='Power' format=7.2;
   attrib nc    label='Non-centrality' format=7.1;
   attrib fcrit label='F-critical value' format=7.2;
   drop probF;
run;

proc print data=power_effects label split=' ';
   title 'Power analysis for before/after analysis with 4 streams, 5 quadats/stream/year based on analysis of averages';
    footnote 'Scope of inference is for ALL streams';
    format power 8.5;
run;


/* Alternatively, if the design is balanced, we can use the previous results by computing
   the residual variance as
     year-to-year variation 
     year-stream interaction variation / n_stream
     quadrat variance = n_streams*n_quadrates
   Note that because the same site is measured over time, it "disappears" from the power computation
   in the same way as blocking causes block effects to disappear.
 
   
   So for the case of 4 streams and 5 quadrats/stream/year, our estimated residual variation is
     38.63 + 0/4 + 68.98/4/5 = 42.079 and the std dev is sqrt(42.079)=6.487
 */   

/* Once again notice that the limiting term for the power analysis is the year-to-year variation
   and that even if you measure thousands of streams, you can't ever reduce the impact of this term.

/* Note that for a single stream, the "year-to-year" variance really is a combination of 
   the year-to-year variance and the year-stream interaction variance components as these
   cannot be separated if you have a single stream. */
 
/* It appears that multiple-streams have no impact vs. a single stream, but this is misleading. As noted
   earlier, with a single stream, you cannot separate the year-to-year variance and the stream-year
   interaction variance components. With multiple streams, you can separate the two components and 
   reduce the impact of the stream-year interaction by measuring multiple streams.  

   As well, the scope of inference is different. 
   In the case of a single stream, you are interested ONLY in that stream. In the case of
   multiple streams, you want to generalize to all streams. In this case, the stream-year interaction
   variance was estimated to be zero, so this implies that there is no evidence that individual streams
   behave differently from each other. If this variance component was non-zero, then you have to deal
   with the additional variation caused by individual streams behaving differently over time from each other. */ 
 
*---part400b;
%let year_to_year_var =36.64;
%let stream_to_stream_var =38.312;
%let stream_year_interaction_var = 00;
%let quad_to_quad_var = 66.35;

ods graphics on;
%let stddev=%sysevalf((&year_to_year_var + 
                   &stream_year_interaction_var/4 + 
                   &quad_to_quad_var / 4 / 5)**0.5);
proc power;
  title 'Power analysis for before/after analysis with 4 streams, 5 quadats/stream/year based on analysis of averages';
  footnote 'Scope of inference is for ALL streams';
  /* We vary the total sample size to see what power is obttained */
  twosamplemeans
     test=diff     /* indicates that you wish to test for differences in the mean */
     meandiff=10   /* size of difference to be detected */  
     stddev=&stddev /* the standard deviation within each group */
     power=.       /* solve for power */
     alpha=.05     /* alpha level for the test */
     sides=2       /* a two sided test for difference in the mean should be done */
     groupns=5 | 5 to 20 by 1      /* number of years BEFORE (5) and AFter (5 to 10 by 1) to examine */
  ;                /* end of the twosamplemeans statement - DON'T FORGET THIS */  
  ods output output=power400;
run;
ods graphics off;
*---part400e; 

ods pdf close;



/* Now to create the various outputs for my LaTeX files */

/* Create LaTeX files for inclusion by my notes */
%include "../../MyLatexTagset.sas"; run;
title;
footnote;
ods listing;

proc document name=Stream1TtestAverages;
   list /levels=all;
run;


ods tagsets.mycolorlatex file='invert-power-SAS-100.tex' (notop nobot) stylesheet="sas.sty";
proc print data=power100(drop=error info);
  title 'Power analysis for before/after analysis with one stream based on analysis of averages';
  footnote 'Residual std dev of 6.77 assumes an average of 5 quadrats/year';
  footnote2 'Inference limited to that single stream and cannot be generalized to other streams';
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='invert-power-SAS-200.tex' (notop nobot) stylesheet="sas.sty";
proc print data=power200(drop=error info);
  title 'Power analysis for before/after analysis with one stream based on analysis of averages';
  footnote 'Residual std dev of 6.3 assumes an average of 10 quadrats/year';
  footnote2 'Inference limited to that single stream and cannot be generalized to other streams';
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='invert-power-SAS-300.tex' (notop nobot) stylesheet="sas.sty";
proc print data=power_effects(drop=numdf dendf Fvalue NC Fcrit)  label split='_';
   title 'Power analysis for before/after analysis with 4 streams, 5 quadats/stream/year based on analysis of averages';
   footnote 'Scope of inference is for ALL streams';
   title ;
   footnote;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='invert-power-SAS-400.tex' (notop nobot) stylesheet="sas.sty";
proc print data=power400(drop=error info);
  title 'Power analysis for before/after analysis with 4 streams, 5 quadats/stream/year based on analysis of averages';
  footnote 'Scope of inference is for ALL streams';
run;
ods tagsets.mycolorlatex close;

