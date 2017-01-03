/* Chi-square test on two groups; contingency table; Fisher's Exact Test */
 
/*  Fisher's Exact Test  - Northern Australlian Quoll

  In 1935, the highly toxic cane toad was introduced to Australia to aid with 
  pest control of scarab beetles. The beetles were wreaking havoc on 
  sugarcane crops. Unfortunately, this decision led to an unforeseen 
  and devastating effect on Australia's wildlife due to animal's consuming 
  the toxic toad. Damage has included the possible extinction of the 
  Northern Australian quoll. Although initiatives such as relocating the 
  quoll on the nearby islands have been taken in order to save the species, 
  there is no guarantee the new habitats will remain toadless. 
  Scientists have developed a new plan of attack using conditioned taste 
  aversion ({\bf CTA}) in order to save the quoll population.

  A sample of $62$ quolls was taken ($32$ males and $30$ females). 
  The quolls were then split up into two treatment groups;  
  toad smart (quolls that were given the CTA treatment, $15$ males and $16$ 
  females) and toad naive (Control group, $17$ males and $14$ females). 
  A samples of $34$ quolls ($21$ males, $13$ females) were subjected 
  to a prescreening trial (prior to release into the wild). 

  The trial proceeded as follows: Both toad naive and toad smart quolls 
  were subjected to a live cane toad in a controlled environment.  
  The quolls were monitored using hidden cameras and their response 
  was recorded. The response variable had three levels; 
     attack (attacked the toad), 
     reject(sniffed but did not pursue) and 
     ignore. 
*/

/* Lines starting with *---partxxxe and *---partxxxb are used in my Latex file and should be ignored */
/* The tagsets.tablesonlylatex again is used by the Latex Program course notes */
dm 'output' clear;
dm 'log'    clear;
proc datasets kill; run;


title 'Nortern Quolls';

options noovp nodate orientation=landscape;
ods pdf file='quolls-SAS.pdf';
goptions device=pdf colors=(black) rotate=landscape;


*---part001b;
data quolls;
   infile 'quolls.csv' dlm=',' dsd missover firstobs=2;
   length sex trt response $10.;
   input sex $ trt $ response count;
   if count = 0 then delete;
run;
*---part001e;

proc print data=quolls;
   title2 'raw data';
run;

proc sort data= quolls; by sex;

ods document name=freq(write);
*---part010b;
proc freq data=quolls;
   title2 'simple two-way table analysis';
   by sex;
   table trt*response / chisq measures cl relrisk riskdiff nocol nopercent expected;
   exact fisher chisq ;
   weight count;
   ods output FishersExact=FishersExact;
run;
*---part010e;
ods document close;

ods pdf close;





/* Now to create the various outputs for my LaTeX files */

/* Create LaTeX files for inclusion by my notes */
%include "../../MyLatexTagset.sas"; run;
title;
footnote;
ods listing;

proc document name=freq;
   list /levels=all;
run;

ods tagsets.mycolorlatex file='quolls-SAS-001.tex' (notop nobot) stylesheet="sas.sty";
proc print data=quolls;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='quolls-SAS-010f.tex' (notop nobot) ;
proc document name=freq;
   obstitle \Freq#1\ByGroup1#1\Table1#1\CrossTabFreqs#1; /* delete the title*/
   obtitle \Freq#1\ByGroup1#1\Table1#1\CrossTabFreqs#1; /* delete the title*/
   replay \Freq#1\ByGroup1#1\Table1#1\CrossTabFreqs#1;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='quolls-SAS-010m.tex' (notop nobot) ;
proc document name=freq;
   obstitle \Freq#1\ByGroup2#1\Table1#1\CrossTabFreqs#1; /* delete the title*/
   obtitle \Freq#1\ByGroup2#1\Table1#1\CrossTabFreqs#1; /* delete the title*/
   replay  \Freq#1\ByGroup2#1\Table1#1\CrossTabFreqs#1;
run;
ods tagsets.mycolorlatex close;


ods tagsets.mycolorlatex file='quolls-SAS-011f.tex' (notop nobot) ;
proc document name=freq;
   obstitle \Freq#1\ByGroup1#1\Table1#1\PearsonChiSq#1; /* delete the title*/
   obtitle \Freq#1\ByGroup1#1\Table1#1\PearsonChiSq#1; /* delete the title*/
   replay  \Freq#1\ByGroup1#1\Table1#1\PearsonChiSq#1;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='quolls-SAS-011m.tex' (notop nobot) ;
proc document name=freq;
   obstitle \Freq#1\ByGroup2#1\Table1#1\PearsonChiSq#1; /* delete the title*/
   obtitle \Freq#1\ByGroup2#1\Table1#1\PearsonChiSq#1; /* delete the title*/
   replay  \Freq#1\ByGroup2#1\Table1#1\PearsonChiSq#1;
run;
ods tagsets.mycolorlatex close;



ods tagsets.mycolorlatex file='quolls-SAS-012f.tex' (notop nobot) ;
proc document name=freq;
   obstitle \Freq#1\ByGroup1#1\Table1#1\FishersExact#1 ; /* delete the title*/
   obtitle \Freq#1\ByGroup1#1\Table1#1\FishersExact#1; /* delete the title*/
   replay  \Freq#1\ByGroup1#1\Table1#1\FishersExact#1;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='quolls-SAS-012m.tex' (notop nobot) ;
proc document name=freq;
   obstitle \Freq#1\ByGroup2#1\Table1#1\FishersExact#1 ; /* delete the title*/
   obtitle \Freq#1\ByGroup2#1\Table1#1\FishersExact#1; /* delete the title*/
   replay  \Freq#1\ByGroup2#1\Table1#1\FishersExact#1;
run;
ods tagsets.mycolorlatex close;



