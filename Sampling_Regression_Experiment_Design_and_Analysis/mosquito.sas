/* Example of a 2 factor, fixed and random effects, CRD - Mosquito bites */

/* 

[Based upon a real study conducted at the University of Manitoba but using 
simulated data.].
Biting insects can be a real pest
and can be health hazard - e.g. malaria and equine encephilitus is a
serious disease that is transmitted by mosquitoes. What
is the best method of deterring these pesky critters from biting.

There are a number of insect repellants available on the market
place. Some use the chemical DEET which is quite effective
but many people are reluctant to use these sprays because
they are quite `strong' - e.g. many sprays containing DEET
will soften paint. As an alternative, there is a strong
`urban legend' about an Avon (a perfume and toilettry company)
product called `Skin so soft' that many people claim is also
an effective repellant.

To investigate these claims,
twenty four volunteers were recruited. These were
randomly assigned to 3 groups of 8 people which then 
went to 3 locations on the University Campus. At each location,
half of the volunteers 
spread a DEET product on their right arm; the other half used the Avon product on 
their right arm. Each subject stood at least 10 m from any other subject. 
Then the subjects let
mosquitos bite their exposed arm, and after 15 minutes, the
total number and severity of the bites was scored using a
standard scale for such studies (how some one came up this
scale I can only hazard a guess!). The higher the score,
the worse the biting experience. */

title 'Two factor, fixed and random effects, CRD - mosquito bites';
options nodate noovp linesize=100 nocenter;

data bites;
   input score product $ location $;
   datalines;
21     deet     1   
19     deet     1   
20     deet     1   
22     deet     1   
14     sss     1   
15     sss     1   
13     sss     1   
16     sss     1   
14     deet     2   
17     deet     2   
15     deet     2   
17     deet     2   
12     sss     2   
11     sss     2   
12     sss     2   
14     sss     2   
16     deet     3   
20     deet     3   
18     deet     3   
19     deet     3   
14     sss     3   
14     sss     3   
14     sss     3   
12     sss     3   
;;;;

proc print data=bites;
   title2 'raw data';
 
/* DO NOT USE GLM - it gives incorrect results */
 
proc mixed data=bites ratio;
   title2 'ANOVA using MIXED';
   class product location;
   model score = product / ddfm=satterth;  /* note specify fixed effects only here */
   random location product*location;  /* specify random effects in random statement only */
   lsmeans product / adjust=tukey cl diff;

