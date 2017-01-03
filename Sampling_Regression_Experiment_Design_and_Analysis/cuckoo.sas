/* Is there a difference in the mean length of eggs laid by cuckoo birds? */

/* Line starting with *---partxxxb; and *---partxxxe; are for inclusion by my LaTeX code
   and can be ignored. */
dm 'output' clear;
dm 'log'    clear;
proc datasets kill; run; 


options orientation=landscape;
ods pdf file='cuckoo-SAS.pdf';
goptions device=pdf colors=(black) rotate=landscape;

title 'The Truth about the Cuckoo Bird';
options nodate noovp ;


/* L.H.C. Tippett (1902-1985) was one of the pioneers in the 
   field of statistical quality control, This data on
   the lengths of cuckoo eggs found in the nests of other birds 
   (drawn from the work of O.M. Latter in 1902) is used by
   Tippett in his fundamental text. 

   Cuckoos are knows to lay their eggs in the nests of other 
   (host) birds. The eggs are then adopted and hatched by the host birds. 

   That cuckoo eggs were peculiar to the locality where found was 
   already known in 1892. A study by E.B.  Chance in 1940 called 
   The Truth About the Cuckoo demonstrated that cuckoos return year 
   after year to the same territory and lay their eggs in the nests 
   of a particular host species. Further, cuckoos appear to mate only within their
   territory. Therefore, geographical sub-species are developed, 
   each with a dominant foster-parent species, and natural
   selection has ensured the survival of cuckoos most fitted to 
   lay eggs that would be adopted by a particular foster-parent 

   L.H.C. Tippett, The Methods of Statistics, 4th Edition, 
   John Wiley and Sons, Inc., 1952, p. 176.                    */

*---part001b;
data length;
   /* this illustrates the flexibility of SAS to read data in a 
      variety of formats and to transform it into a standard
      case by variable format */
   infile 'cuckoo.csv' dlm=',' dsd missover firstobs=2;
   input sp1 sp2 sp3 sp4 sp5 sp6;  /* read in 6 species. A '.' implies a missing value */
   length species $15.;
   species = 'Meadow Pipit '; length =sp1; output; /* convert to standard format */
   species = 'Tree Pipit   '; length =sp2; output;
   species = 'Hedge Sparrow'; length =sp3; output;
   species = 'Robin        '; length =sp4; output;
   species = 'Pied Wagtail '; length =sp5; output;
   species = 'Wren         '; length =sp6; output;
   label length = 'Egg length (mm)';
   keep species length; /* which variables to retain */
run;
*---part001e;

data length;
   set length;
   if length = . then delete;  /* remove all missing data */
run;
 
proc print data=length;
   title2 'raw data';
run;

proc sort data=length;
   by species;
run;

ods document name=plot1(write);
*---part005b;
proc sgplot data=length;
   title2 'dot plot of rawdata';
   scatter x=species y=length;
run;
*---part005e;
ods document close;

*---part010b; 
proc tabulate data=length;
   title2 'summary statistics';
   class species;
   var length;
   table species, length*(n*f=5.0  mean*f=6.2 std*f=6.2) / rts=20;
run;
*---part010e;
 

*---part011b;
proc boxplot data=length;
   title2 'side-by-side box plots';
   plot length*species ;
   footnote;
run;
*---part011e;


ods document name=GLM(write);
*---part030b;
ods graphics on;
proc glm data=length plots=all;
   title2 'ANOVA using GLM';
   class species;
   model length = species;
   lsmeans species / adjust=tukey pdiff cl stderr lines;
   estimate 'Special Contrast' species 0 .5 0 -1 .5 0;
   ods output LSmeanDiffCL = GLMdiffs;
   ods output LSmeans      = GLMLSmeans;
   ods output LSmeanCL     = GLMLSmeansCL;
   ods output LSMlines     = GLMlines;
   ods output ModelANOVA   = GLManova;
   ods output Estimates    = GLMest;
run;
ods graphics off;
*---part030e;
ods document close;


/* Alternatively, you can use Mixed */
ods document name=mixed(write);
*---part040b;
ods graphics on;
proc mixed data=length plots=all;
   title2 'ANOVA using Mixed';
   class species;
   model length=species;
   lsmeans species / adjust=tukey diff cl;
   estimate 'Special Contrast' species 0 .5 0 -1 .5 0;
   ods output tests3 =MixedTest; 
   ods output lsmeans=MixedLsmeans;
   ods output diffs  =MixedDiffs;
   ods output estimates=MixedEst;
run;
ods graphics off;
*---part040e;
ods document close;

/* Get a joined lines plot */
*---part045b;
%include 'pdmix800.sas';
%pdmix800(MixedDiffs,MixedLsmeans,alpha=0.05,sort=yes);
*---part045e;

ods pdf close;



/* Now to create the various outputs for my LaTeX files */

/* Create LaTeX files for inclusion by my notes */
%include "../../MyLatexTagset.sas"; run;
title;
footnote;
ods listing;

proc document name=mixed;
   list /levels=all;
run;

ods tagsets.mycolorlatex file='cuckoo-SAS-001.tex' (notop nobot) stylesheet="sas.sty";
proc print data=length(obs=10);
run;
ods tagsets.mycolorlatex close;

ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='cuckoo-SAS-005' reset=index;
proc document name=plot1;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;


ods tagsets.mycolorlatex file='cuckoo-SAS-010.tex' (notop nobot);
proc tabulate data=length;
   title2 ' ';
   class species;
   var length;
   table species, length*(n*f=5.0  mean*f=6.2 std*f=6.2) / rts=20;
run;
ods tagsets.mycolorlatex close;


ods tagsets.mycolorlatex file='cuckoo-SAS-030a.tex' (notop nobot);
proc print data=GLManova noobs label split=" ";
   where hypothesistype = 3;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='cuckoo-SAS-030b.tex' (notop nobot);
proc print data=GLMLSmeans noobs label split=" ";
run;
ods tagsets.mycolorlatex close;
ods tagsets.mycolorlatex file='cuckoo-SAS-030bb.tex' (notop nobot);
proc print data=GLMLSmeansCL noobs label split=" ";
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='cuckoo-SAS-030c.tex' (notop nobot);
proc print data=GLMdiffs noobs label split=" ";
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='cuckoo-SAS-030d.tex' (notop nobot);
proc print data=GLMlines noobs label split=" ";
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='cuckoo-SAS-030g.tex' (notop nobot);
proc print data=GLMest noobs label split=" ";
run;
ods tagsets.mycolorlatex close;


ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='cuckoo-SAS-030e' reset=index;
proc document name=glm;
   replay \GLM#1\LSMEANS#1\species#1\length#1\DiffPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;

ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='cuckoo-SAS-030f' reset=index;
proc document name=glm;
   replay \GLM#1\ANOVA#1\length#1\DiagnosticsPanel#1 / dest=listing;
run;
ods graphics off;
ods listing close;


ods tagsets.mycolorlatex file='cuckoo-SAS-040a.tex' (notop nobot);
proc print data=MixedTest noobs label split=" " ;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='cuckoo-SAS-040b.tex' (notop nobot);
proc print data=MixedLsmeans noobs label split=" " ;
run;
ods tagsets.mycolorlatex close;


ods tagsets.mycolorlatex file='cuckoo-SAS-040c.tex' (notop nobot);
proc print data=MixedDiffs noobs label split=" " ;
   var species _species estimate stderr adjustment adjp adjlower adjupper;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='cuckoo-SAS-040g.tex' (notop nobot);
proc print data=MixedEst noobs label split=" " ;
run;
ods tagsets.mycolorlatex close;



ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='cuckoo-SAS-040f' reset=index;
proc document name=mixed;
   replay \Mixed#1\ResidualPlots#1\ResidualPanel#1 / dest=listing;
run;
ods graphics off;
ods listing close;

ods tagsets.mycolorlatex file='cuckoo-SAS-045.tex' (notop nobot);
%pdmix800(MixedDiffs,MixedLsmeans,alpha=0.05,sort=yes);
ods tagsets.mycolorlatex close;
