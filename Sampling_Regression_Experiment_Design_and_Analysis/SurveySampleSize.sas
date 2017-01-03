/* Find the required sample size for a simple random sample.

This program can be used to determine the appropriate sample size	
when trying to estimate a population mean or a population proportion.	

In order to use this program you will need some preliminary information	
(a) What level of precision do you want?	
   A rough rule of thumb is that 	
	a preliminary survey, should have a 95% ci that is +/-50% of the estimate
	a management survey,  should have a 95% ci that is +/- 25% of the estimate
	scientific surveys    should have a 95% ci that is +/- 10% of the estimate

(b) What is the standard deviation of measurements on the survey units	
     This can be obtained from preliminary surveys, or using the (very) rough rule of thump	
     that the standard deviation is approximately the "usual" range of the data/4.	
(c) What is the approximate mean or proportion in the population	
(e) What is the approximate total number of survey units.						
     If this is enormous, use any large number.			

These must be translated into HALFwidths

These should be specified in the appropriate places in the macro variables (%let) below.

Remember though that the exercise is not to see if the sample size is 25 or 26, but rather if the
required sample size is 25, 250, or 2500. Afterall, you are just "guessing" as to the 
values in the population.  */

title 'Determine appropriate sample size for simple random sample';
options nodate nonumber noovp;
%let conflevel       = .95;
/**************** Confidence interval for a mean ******************************/

/* Change these macro variables below */
%let desired_size_ci = .25;   /* desired half-width (+/- part) of confidence interval as a fraction of the mean */
%let std_deviation   = .844;  /* standard deviation of individual units */
%let approx_mean     = .667;  /* approximate mean of the population */

/* Don't change this statement below */
%let half_width_ci   = %sysevalf(&approx_mean * &desired_size_ci);

proc power;
   title2 'sample size for estimating a mean';
   onesamplemean
          ci        = T                 /* T=solve for sample size for confidence interval */ 
          probwidth = &conflevel        /* size of the confidence interval */
          sides     = 2                 /* a two sided confidence interval */
          stddev    = &std_deviation    /* standard deviation of INDIVIDUAL observations */
                                        /* Note you can also specify a cv for non-normal data */
          dist      = normal            /* distribution of individual data values */
          halfwidth = &half_width_ci    /* HALF width of confidence interval */
          ntotal    = .                 /* solve for total sample size */
          ;
run;


			
/**************** Confidence interval for a proportion ******************************/

/* Change these macro variables below */
%let desired_margin_error = .03;  /* what is the size of the half width of the confidence interval */
                                  /* this differs from the relative formulation used above */
%let approx_prop     = .50;  /* approximate proportion in the population */

/* Don't change this statement below */
%let half_width_ci   = %sysevalf(&desired_margin_error);
%let std_deviation   = %sysevalf((&approx_prop * (1-&approx_prop))**.5);   /* p*(1-p) */

proc power;
   title2 'sample size for estimating a proportion';
   onesamplemean                        /* uses a normal approximation to a binomial confidence interval */
          ci        = T                 /* T=solve for sample size for confidence interval */ 
          probwidth = &conflevel        /* size of the confidence interval */
          sides     = 2                 /* a two sided confidence interval */
          stddev    = &std_deviation    /* standard deviation of INDIVIDUAL observations */
          dist      = normal            /* distribution of individual data values */
          halfwidth = &half_width_ci    /* HALF width of confidence interval */
          ntotal    = .                 /* solve for total sample size */
          ;
run;



