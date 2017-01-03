/* Two-factor - 2-factor - CRD - completely randomized design - fixed effects- random effects - 
   mixed model */

/* 
A study was conducted to investigate the effects of
irradiating fat to prevent it from 
going rancid. [This is a proposed treatment for many foods
to kill many of the bacteria which cause foods to spoil.
The promoters of this treatment claim that it
doesn't affect taste nor nutrition and is 
perfectly safe.]

The experiment was performed using two machines (of the same 
make and model) to assess the consistency of the results
across machines.

In this experiment, 12 batches of fat were obtained and
split between the 2 machines. Three of the samples
were irradiated by each machine, and three were saved
as a control for each machine.
Twelve rats (all aged 30 to 34 days) were
obtained, and the rats were assigned at random to
each of the batches of fat.

The fat were allowed to feed ad libitum and the 
total consumption of fat (grams) was noted over 73
days.  */

title 'Two factor, CRD, fixed and random effects - Fat consumption';
options nodate noovp linesize=100;

data fat;
   input amount fat $ machine $;
   datalines;
709      control      1 
679      control      1
699      control      1   
562      treated      1   
518      treated      1   
496      treated      1   
657      control      2   
594      control      2   
677      control      2   
508      treated      2   
505      treated      2   
539      treated      2   
;;;;

proc print data=fat;
   title2 'raw data';

/* DO NOT USE GLM - it gives wrong results */ 
 
proc mixed data=fat covtest ratio;
   title2 'ANOVA using MIXED';
   class fat machine;
   model amount = fat / ddfm=satterth;  /* note specify fixed effects only here */
   random machine fat*machine;  /* note than random effects are specified in random statement */
   lsmeans fat / adjust=tukey cl diff ;

run;
