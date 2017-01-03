/* 
This case study shows the analysis of a split-plot-in-time design with pseudo-replication
at two levels.

This case study was based on a study by M. Franklin, a student at SFU working on her B.Sc. in
Environmental Science in 2002.


Can bees learn to forage over time? Do different miticides (chemicals to kill
mites that attack bees) affect this learning?


This experiment took place over a span of three weeks in a greenhouse. In each week, three
colonies were brought into the greenhouse. Each colony received, at random, one of
three miticide treatments (control - no miticide, low dose, or a high dose).

Then from each colony, between 1 and 6 bees were marked with colored dots on their
body so that individual bees could be identified.

Outside the colony, artificial flowers were constructed that dispensed food and had
a novel flower structure that made it difficult for bees to enter the flower.

A camera then recorded bees that visited the flower. Because some of the bees were
marked, it was possible to measure the time the bee spent trying to get to the food
within the flower for each of up to 40 attempts.

Hence the original data consists of up to 40 measurements for each labelled bee.

As a preliminary analysis, the average access time for the first 10 visit (early)
and visits 21-30 (late) was found for each bee.  */

title 'Bee foraging analysis';
options nodate nonumber noovp;

data bees;
   length miticide $16.;
   infile datalines expandtabs;
   input colony miticide $ bee week access_early access_late access_diff;
   format access_early access_late access_diff 7.2;
   datalines;
11     1.control     1     1     29.722     19.644375     10.077625
11     1.control     2     1     11.383     9.603125     1.779875
11     1.control     3     1     16.362     7.350625     9.011375
11     1.control     4     1     12.657     6.6     6.057
11     1.control     5     1     14.732     8.0675     6.6645
11     1.control     6     1     11.048     6.626875     4.421125
17     1.control     1     3     13.299     6.1     7.199
17     1.control     2     3     9.867     4.006875     5.860125
17     1.control     3     3     16.398     5.366875     11.031125
17     1.control     4     3     12.775     8.710625     4.064375
17     1.control     5     3     6.82     11.35875     -4.53875
17     1.control     6     3     17.888     4.58     13.308
21     1.control     1     2     11.103     5.4675     5.6355
21     1.control     2     2     8.316     7.3025     1.0135
21     1.control     3     2     6.686     4.494375     2.191625
21     1.control     4     2     10.277     5.06     5.217
21     1.control     5     2     6.58     4.7575     1.8225
21     1.control     6     2     20.074     11.54625     8.52775
14     2.low         1     2     6.62     3.938125     2.681875
14     2.low         2     2     9.001     6.3625     2.6385
14     2.low         3     2     10.934     3.94875     6.98525
14     2.low         4     2     11.503     7.92     3.583
15     2.low         1     3     15.918     7.546875     8.371125
15     2.low         2     3     5.742     5.424375     0.317625
15     2.low         3     3     6.331     8.3025     -1.9715
15     2.low         4     3     11.07     4.776875     6.293125
15     2.low         5     3     14.306     5.548125     8.757875
15     2.low         6     3     10.129     6.113125     4.015875
19     2.low         1     1     7.052     6.294375     0.757625
19     2.low         2     1     5.343     7.798125     -2.455125
19     2.low         3     1     11.132     9.999375     1.132625
19     2.low         4     1     8.201     7.13125     1.06975
19     2.low         5     1     6.846     8.994375     -2.148375
10     3.high        1     3     7.105     6.28125     0.82375
10     3.high        2     3     7.514     4.06375     3.45025
10     3.high        3     3     10.305     5.753125     4.551875
10     3.high        4     3     8.732     4.81875     3.91325
20     3.high        1     2     12.119     6.455625     5.663375
20     3.high        2     2     14.199     5.225     8.974
20     3.high        3     2     10.328     10.525625     -0.197625
20     3.high        4     2     4.895     12.329375     -7.434375
5      3.high        1     1     9.653     8.69625     0.95675
5      3.high        2     1     7.332     6.32875     1.00325
5      3.high        3     1     9.841     11.650625     -1.809625
;;;;
 
proc print data=bees;
   title2 'raw data';


/*----------------------- Analysis of differences ----------------------*/

/* 1. Approximate analysis of means over bees within a colony  */

proc sort data=bees; by colony;

proc means data=bees noprint;
   by colony;
   var access_diff;
   output out=colonymean mean=mean_access_diff;
   id miticide week;

proc print data=colonymean;
   title2 'average difference over bees within a colony';
   format mean_access_diff 7.2;
 
proc mixed data=colonymean;
   title2 'analyze average difference over bees within a colony';
   class week colony miticide;
   model mean_access_diff = week miticide;
   /* The test for miticide tests if the mean learning is equal for all miticides */
   /* The confidence interval test from lsmeans examines if there is no learning for each miticide in turn */
   lsmeans miticide / cl ;
   /* The following tests give the same information for each miticide and then an omnibus test */
   estimate 'no learning 1' intercept 1 miticide 1 0 0 / cl;
   estimate 'no learning 2' intercept 1 miticide 0 1 0 / cl;
   estimate 'no learning 3' intercept 1 miticide 0 0 1 / cl;
   contrast 'omnibus test' intercept 1 miticide 1 0 0,
                           intercept 1 miticide 0 1 0,
                           intercept 1 miticide 0 0 1;
   estimate 'avg learning' intercept 3 miticide 1 1 1 / divisor=3 cl;
   footnote 'The test for miticide examines if the mean learning is equal for all miticides';
   footnote2 'The ci from the LSMeans examin if there is no learning for each miticide in turn';
   footnote3 'The estimate statements repeat the LSmeans statements';
   footnote4 'The contrast output examines an omnibus test of no learning in all miticides';




/* 2. Analysis of individual differences */

proc mixed data=bees;
   title2 'analysis of individual differenes';
   class colony miticide week;
   model access_diff = week miticide / ddfm = satterth; 
   random week*miticide;
   /* ddfm = method of computing the denominator degrees of freedom for test statistcs = satterthwaite approximation */

   /* The test for miticide tests if the mean learning is equal for all miticides */
   /* The confidence interval test from lsmeans examines if there is no learning for each miticide in turn */
   lsmeans miticide /  cl;
   /* The following tests give the same information for each miticide and then an omnibus test */
   estimate 'no learning 1' intercept 1 miticide 1 0 0 / cl;
   estimate 'no learning 2' intercept 1 miticide 0 1 0 / cl;
   estimate 'no learning 3' intercept 1 miticide 0 0 1 / cl;
   contrast 'omnibus test' intercept 1 miticide 1 0 0,
                           intercept 1 miticide 0 1 0,
                           intercept 1 miticide 0 0 1;
   estimate 'avg learning' intercept 3 miticide 1 1 1 / divisor=3 cl;



/*----------------------- Analysis of  access times at the different phases ----------------------*/

/* First stack the data */
data bees2;
   set bees;
   length phase $10.;
   phase = 'early'; access_time = access_early; output;
   phase = 'late';  access_time = access_late;  output;
   drop access_diff access_early access_late;
   format access_time 7.2;

proc print data=bees2;
   title2 'raw data after stacking the access times';
   footnote '';


/* 3. Analyse the averages over bees */

proc sort data=bees2; by colony phase; 

proc means data=bees2 noprint;
   by colony phase;
   var access_time;
   output out=meancolony mean=mean_access_time;
   id week miticide;

proc print data=meancolony;
   title2 'average over bees';

proc mixed data=meancolony;
   title2 'analysis in a split-plot structure using average over bees';
   class week colony miticide phase;
   model mean_access_time = week miticide phase phase*miticide / ddfm=satterth;
   random week*miticide;

   /* now the test for the phase*miticide interaction tests if the learning was the same for all
      miticides, i.e. parallel responses */
   /* the test for the main effect of phase tests if there was any learning over all miticides */
   /* the lsmeans with the slice option tests if there was learning for each miticide */
   lsmeans  phase*miticide / slice=miticide cl;
   /* the following estimate statements also examine if learning occured at each  miticide level */
   estimate 'learning 1' phase 1 -1 phase*miticide 1 -1   0  0   0  0 / cl ;
   estimate 'learning 2' phase 1 -1 phase*miticide 0 0   1 -1    0  0 / cl;
   estimate 'learning 3' phase 1 -1 phase*miticide 0 0   0 0     1 -1 / cl;
   estimate 'avg learning' phase 1 -1 / cl;
  
   footnote 'The phase*miticide test examines if learning was equal for all miticides';
   footnote2 'The lsmeans with slice option examines if learning occured at each miticide level';
   footnote3 'The estimate output examines learning at each miticide level (same as lsmeans with slice';


 


/* 4. Analyse the individual values */ 
 
proc mixed data=bees2;
   title2 'split plot analysis using individual bees';
   class colony miticide week bee phase;
   model access_time = week miticide phase phase*miticide phase*week / ddfm =satterth;
   random week*miticide bee(week*miticide) phase*miticide*week;;
   
   /* now the test for the phase*miticide interaction tests if the learning was the same for all
      miticides, i.e. parallel responses */
   /* the test for the main effect of phase tests if there was any learning over all miticides */
   /* the lsmeans with the slice option tests if there was learning for each miticide */
   lsmeans  phase*miticide / slice=miticide cl;
   estimate 'learning 1' phase 1 -1 phase*miticide 1 -1   0  0   0  0 / cl;
   estimate 'learning 2' phase 1 -1 phase*miticide 0 0   1 -1    0  0 / cl;
   estimate 'learning 3' phase 1 -1 phase*miticide 0 0   0 0     1 -1 / cl;
   estimate 'avg learning' phase 1 -1 / cl;

   
