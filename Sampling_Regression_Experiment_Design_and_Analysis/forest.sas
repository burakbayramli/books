/* Split-plot with main plots in an RCB */
 
/* A forestry trial was conducted to investigate the 
   relative growth rates of tree seedlings of different varieties of trees when
   the seedling was treated after planting.

   The experiment was conducted by selecting seeds 
   from 4 varieties of trees (denoted Victoria1, Victoria2, 
   Clinton, and Branch).
   Around the province, 4 forestry blocks were selected for trials.
   Within each block, 1 ha plots were planted with seedlings from
   each variety. Then within each 1 ha plot, four 100 m2  sub-plots 
   were selected, and one of five different sprays (consisting
   combinations of nutrients, fungus suppressants, and the like) 
   were sprayed on the seedlings. These five treatments were denoted
   as (Check, Ceresan, Panogen, Agrox).

   The total biomass (kg) was measured after 5 years on each plant. */


/* Lines starting with *---partxxxe and *---parxxxb are used in my Latex file and 
   should be ignored */
/* The tagsets.tablesonlylatex again is used by the Latex Program course notes */
dm 'output' clear;
dm 'log'    clear
proc datasets kill;

options nodate noovp orientation=landscape;
ods pdf file='forest-SAS.pdf';
goptions device=pdf colors=(black) rotate=landscape;
 
title 'Split-plot, 2 factor, main plot in blocks - varieties of seedlings and sprays ';
options nodate noovp ;

*---part010b;
data biomass;
   infile 'forest.csv' dlm=',' dsd missover firstobs=2;  
   length variety spray $10. trt $20;
   input variety block spray biomass;
   trt = variety || '-' || spray;
run;

/* name the experimental plots */
/* The actual labels are not that crucial, but notice how
   all sub-plots within a block*variety combination is a 
   separate plot */
proc sort data=biomass; by block variety; run;
data biomass;
   set biomass;
   by block variety;
   retain plot 0;
   if first.variety then plot=plot+1;
run;
*---part010e;

proc print data=biomass;
  title2 'raw data';
run;


/* Construct side-by-side dot plots to check for outliers and unusual points */
ods document name=dotplot(write);
*---part020b; 
proc sgplot data=biomass;
   title2 'Side-by-side dot plots';
   yaxis label='Biomass'    offsetmin=.05 offsetmax=.05;
   xaxis label='Treatmemt'  offsetmin=.05 offsetmax=.05;
   scatter x=trt  y=biomass  /  markerattrs=(symbol=circlefilled);
run;
*---part020e; 
ods document close;

/*------------------------- */
/* Find some simple summary statistics.*/ 
/* Do NOT compute a simple SE as this is NOT a CRD and the std error reported will be WRONG */
ods document name=meanstable(write);
*---part030b;
proc tabulate data=biomass; /* proc tabulate is not for the faint of heart */
   title2 'Summary table of means, std devs';
   class variety spray;
   var biomass;
   table variety*spray, biomass*(n*f=5.0  mean*f=5.2 std*f=5.2) /rts=15;
run;
*---part030e;
ods document close;


/* Create a profile plot of the means */
/* Because this is a split=plot design, you should not compute the simple std errors and ci as in a CRD designs*/
/*  Need to sort the data first */ 
 
ods document name=profileplot(write);
*---part040b;
proc sort data=biomass;
   by variety spray;
run;

proc means data=biomass noprint;  /* find simple summary statistics */
   by variety spray;
   var biomass;
   output out=means n=n mean=mean std=stddev;
run;

proc sgplot data=means;
   title2 'Profile plot - NO ci plotted because not a CRD design';
   series y=mean x=variety / group=spray;
   yaxis label='Mean biomass'  offsetmin=.05 offsetmax=.05;
   xaxis label='Variety'       offsetmin=.05 offsetmax=.05;
run;
*---part040e;
ods document close;


 
*------------------ Initial analysis of ALL the data -------;

ods document name=mixed1(write);
*---part100b;
ods graphics on;
proc mixed data=biomass plots=all cl ratio;
   title2 'Analysis of all of the data';
   title3 'Using the block*mainplot factor  for main plot error';
   class variety spray block;
   model biomass = variety spray spray*variety / ddfm=kr;
   random block block*variety;
   /* note that the block*variety term is the main plot experimental unit
      and is NOT a block*factor interaction */
   lsmeans variety / pdiff cl adjust=tukey;
   lsmeans spray   / pdiff cl adjust=tukey;
   ods output tests3 =Mixed1Test;  /* needed for the pdmix800 */
   ods output lsmeans=Mixed1Lsmeans;
   ods output diffs  =Mixed1Diffs;
run;
ods graphics off;

/* Get a joined lines plot */
%include '../../pdmix800.sas';
%pdmix800(Mixed1Diffs,Mixed1Lsmeans,alpha=0.05,sort=yes);
*---part100e;
ods document close;

ods graphics on;
proc mixed data=biomass plots=all cl ratio;
   title3 'Using the main plot unit number as the experimental unit because it is uniquely labelled';
   class variety spray block plot;
   model biomass = variety spray spray*variety / ddfm=kr;
   random block plot;
   lsmeans variety / pdiff cl adjust=tukey;
   lsmeans spray   / pdiff cl adjust=tukey;
   lsmeans variety*spray / cl adjust=tukey;
   ods output tests3 =Mixed1aTest;  /* needed for the pdmix800 */
   ods output lsmeans=Mixed1aLsmeans;
   ods output diffs  =Mixed1aDiffs;
   footnote 'Because plots are uniquely numbered, a simpler syntax for the model can be used ';
run;
ods graphics off;

/* Get a joined lines plot */
%include '../../pdmix800.sas';
%pdmix800(Mixed1aDiffs,Mixed1aLsmeans,alpha=0.05,sort=yes);




 
*------------------- Remove the victoria1 variety and reanalyze *;
ods document name=mixed2(write);
*---part200b;
ods graphics on;
proc mixed data=biomass plots=all cl ratio;
   footnote 'Analysis dropping victoria1 variety';
   where  variety ^= 'victoria1';
   class variety spray block;
   model biomass = variety spray spray*variety / ddfm = kr;
   random block  block*variety;
   lsmeans variety / pdiff cl adjust=tukey;
   lsmeans spray   / pdiff cl adjust=tukey;
   ods output tests3 =Mixed2Test;  /* needed for the pdmix800 */
   ods output lsmeans=Mixed2Lsmeans;
   ods output diffs  =Mixed2Diffs;
run;
ods graphics off;

/* Get a joined lines plot */
%include '../../pdmix800.sas';
%pdmix800(Mixed2Diffs,Mixed2Lsmeans,alpha=0.05,sort=yes);
*---part200e;
ods document close;

ods pdf close;



/* Now to create the various outputs for my LaTeX files */

/* Create LaTeX files for inclusion by my notes */
%include "../../MyLatexTagset.sas"; run;
title;
footnote;
ods listing;

proc document name=mixed2;
   list /levels=all;
run;

ods tagsets.mycolorlatex file='forest-SAS-010.tex' (notop nobot) stylesheet="sas.sty";
proc sort data=biomass; by block variety; run;
proc print data=biomass(obs=10);
run;
ods tagsets.mycolorlatex close;

/* Side by side dot plots */
ods listing;
ods graphics on / imagefmt=png imagename='forest-SAS-020' reset=index;
proc document name=dotplot;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;

/* Table of means and standard deviations */
ods tagsets.mycolorlatex file='forest-SAS-030.tex' (notop nobot);
proc document name=meanstable;
   obstitle \Tabulate#1\Report#1\Table#1; /* kill titles */
   obtitle  \Tabulate#1\Report#1\Table#1;
   obfootn  \Tabulate#1\Report#1\Table#1;
   replay \Tabulate#1\Report#1\Table#1;
run;
ods tagsets.mycolorlatex close;

/* Profile plot */
ods listing;
ods graphics on / imagefmt=png imagename='forest-SAS-040' reset=index;
proc document name=profileplot;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;


/* Type 3 tests for effects */
ods tagsets.mycolorlatex file='forest-SAS-100-type3.tex' (notop nobot);
proc document name=mixed1;
   obstitle \Mixed#1\Tests3#1; /* kill titles */
   obtitle  \Mixed#1\Tests3#1;
   obfootn  \Mixed#1\Tests3#1;
   replay \Mixed#1\Tests3#1;
run;
ods tagsets.mycolorlatex close;

/* Lsmeans as requested */
ods tagsets.mycolorlatex file='forest-SAS-100-LSMean.tex' (notop nobot);
proc document name=mixed1;
   obstitle \Mixed#1\LSMeans#1 ; /* kill titles */
   obtitle  \Mixed#1\LSMeans#1 ;
   obfootn  \Mixed#1\LSMeans#1 ;
   replay   \Mixed#1\LSMeans#1 ;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='forest-SAS-100-LSMean-b.tex' (notop nobot);
proc print data=MixedLSMeans noobs label split=" " ;
   var variety spray estimate stderr lower upper;
run;
ods tagsets.mycolorlatex close;


/* Differences in LSMeans */
/* This is often too big, so we use the tables with selected output */
ods tagsets.mycolorlatex file='forest-SAS-100-LSMeandiff.tex' (notop nobot);
proc document name=mixed1;
   obstitle \Mixed#1\Diffs#1; /* kill titles */
   obtitle  \Mixed#1\Diffs#1;
   obfootn  \Mixed#1\Diffs#1;
   replay   \Mixed#1\Diffs#1;
run;
ods tagsets.mycolorlatex close;

/* Differences in lsmeans */
ods tagsets.mycolorlatex file='forest-SAS-100-LSMeandiff-b.tex' (notop nobot);
proc print data=MixedDiffs noobs label split=" " ;
   var variety spray _variety _spray estimate stderr /*adjustment */ adjp adjlower adjupper;
run;
ods tagsets.mycolorlatex close;

/* Joined line plots */
ods tagsets.mycolorlatex file='forest-SAS-100-LSMeanjline.tex' (notop nobot);
proc document name=mixed1;
   obstitle \Print#1\ByGroup1#1\Print#1; /* kill titles */
   obtitle  \Print#1\ByGroup1#1\Print#1;
   obfootn  \Print#1\ByGroup1#1\Print#1;
   replay   \Print#1\ByGroup1#1\Print#1;

   obtitle  \Print#1\ByGroup2#1\Print#1;
   obfootn  \Print#1\ByGroup2#1\Print#1;
   replay   \Print#1\ByGroup2#1\Print#1;
run;
ods tagsets.mycolorlatex close;

/* Estimated variance components */
ods tagsets.mycolorlatex file='forest-SAS-100-varcomp.tex' (notop nobot);
proc document name=mixed1;
   obstitle \Mixed#1\CovParms#1; /* kill titles */
   obtitle  \Mixed#1\CovParms#1;
   obfootn  \Mixed#1\CovParms#1;
   replay   \Mixed#1\CovParms#1;
run;
ods tagsets.mycolorlatex close;

ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='forest-SAS-100-diagnostics' reset=index;
proc document name=mixed1;
   replay \Mixed#1\ResidualPlots#1\ResidualPanel#1 / dest=listing;
run;
ods graphics off;
ods listing close;


/*---------------- Analysis after dropping one of the varieties -------------- */

/* Type 3 tests for effects */
ods tagsets.mycolorlatex file='forest-SAS-200-type3.tex' (notop nobot);
proc document name=mixed2;
   obstitle \Mixed#1\Tests3#1; /* kill titles */
   obtitle  \Mixed#1\Tests3#1;
   obfootn  \Mixed#1\Tests3#1;
   replay \Mixed#1\Tests3#1;
run;
ods tagsets.mycolorlatex close;

/* Lsmeans as requested */
ods tagsets.mycolorlatex file='forest-SAS-200-LSMean.tex' (notop nobot);
proc document name=mixed2;
   obstitle \Mixed#1\LSMeans#1 ; /* kill titles */
   obtitle  \Mixed#1\LSMeans#1 ;
   obfootn  \Mixed#1\LSMeans#1 ;
   replay   \Mixed#1\LSMeans#1 ;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='forest-SAS-200-LSMean-b.tex' (notop nobot);
proc print data=Mixed2LSMeans noobs label split=" " ;
   var variety spray estimate stderr lower upper;
run;
ods tagsets.mycolorlatex close;


/* Differences in LSMeans */
/* This is often too big, so we use the tables with selected output */
ods tagsets.mycolorlatex file='forest-SAS-200-LSMeandiff.tex' (notop nobot);
proc document name=mixed2;
   obstitle \Mixed#1\Diffs#1; /* kill titles */
   obtitle  \Mixed#1\Diffs#1;
   obfootn  \Mixed#1\Diffs#1;
   replay   \Mixed#1\Diffs#1;
run;
ods tagsets.mycolorlatex close;

/* Differences in lsmeans */
ods tagsets.mycolorlatex file='forest-SAS-200-LSMeandiff-b.tex' (notop nobot);
proc print data=Mixed2Diffs noobs label split=" " ;
   var variety spray _variety _spray estimate stderr /*adjustment */ adjp adjlower adjupper;
run;
ods tagsets.mycolorlatex close;

/* Joined line plots */
ods tagsets.mycolorlatex file='forest-SAS-200-LSMeanjline.tex' (notop nobot);
proc document name=mixed2;
   obstitle \Print#1\ByGroup1#1\Print#1; /* kill titles */
   obtitle  \Print#1\ByGroup1#1\Print#1;
   obfootn  \Print#1\ByGroup1#1\Print#1;
   replay   \Print#1\ByGroup1#1\Print#1;

   obtitle  \Print#1\ByGroup2#1\Print#1;
   obfootn  \Print#1\ByGroup2#1\Print#1;
   replay   \Print#1\ByGroup2#1\Print#1;
run;
ods tagsets.mycolorlatex close;

/* Estimated variance components */
ods tagsets.mycolorlatex file='forest-SAS-200-varcomp.tex' (notop nobot);
proc document name=mixed2;
   obstitle \Mixed#1\CovParms#1; /* kill titles */
   obtitle  \Mixed#1\CovParms#1;
   obfootn  \Mixed#1\CovParms#1;
   replay   \Mixed#1\CovParms#1;
run;
ods tagsets.mycolorlatex close;

ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='forest-SAS-200-diagnostics' reset=index;
proc document name=mixed2;
   replay \Mixed#1\ResidualPlots#1\ResidualPanel#1 / dest=listing;
run;
ods graphics off;
ods listing close;
