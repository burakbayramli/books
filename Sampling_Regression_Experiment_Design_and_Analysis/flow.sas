/*  Annual peak discharges for the Saddle River, NJ 
    Data taken from Helsel and Hirsch  2002
    Statistical Methods in Water Research
    available at http://water.usgs.gov/pubs/twri/twri4a3/*/
title 'Annual peak discharges from Saddle River, N.J.';
options nodate nonumber noovp linesize=80;
 
data flow;
   input year flow;
   logflow = log(flow);
   attrib flow     format=7.0 label='Flow (cfs)';
   attrib logflow format=7.2 label='log(Flow - cfs)';
   datalines;
1925    980
1926    741
1927    1630
1928    829
1929    903
1930    418
1931    549
1932    686
1933    1320
1934    850
1935    614
1936    1720
1937    1060
1938    1680
1939    760
1940    1380
1941    1030
1942    820
1943    1020
1944    998
1945    3500
1946    1100
1947    1010
1948    830
1949    1030
1950    452
1951    2530
1952    1740
1953    1860
1954    1270
1955    2200
1956    1530
1957    795
1958    1760
1959    806
1960    1190
1961    952
1962    1670
1963    824
1964    702
1965    1490
1966    1600
1967    800
1968    3330
1969    1540
1970    2130
1971    3770
1972    2240
1973    3210
1974    2940
1975    2720
1976    2440
1977    3130
1978    4500
1979    2890
1980    2470
1981    1900
1982    1980
1983    2550
1984    3350
1985    2120
1986    1850
1987    2320
1988    1630
1989    2380
;;;;

proc print data=flow (obs=10);
   title2 'portion of the raw data';
 
filename gsasfile 'flow.ps';
goptions device=ps300 gaccess=sasgaedt gprolog='25210D0A'x
         gsflen=80 gsfname=gsasfile gsfmode=replace rotate=landscape
         colors=(black);
 
proc capability data=flow normaltest gout=graph;
   title2 'test for normality on original and logged scale';
   var flow logflow;
   histogram flow logflow / normal;  /* drap histogram with fitted normal curve */
   probplot flow logflow / normal(mu=est sigma=est);    /* create probability plot for the flow variable */
run;

goptions gsfmode=append;
/* now to do everything on the log-normal scale */
/* test for normality and other attributes of original and logged data */
/* The cipctlDF     requests confidence intervals for percentiles that are distribution free
       cipctlNORMAL requests confidence intervals based on assumption of normality 
                    Both of these can be requested for single sided, two sided, asymetric etc - see the SAS documentation 
       normaltest   requests test for normality   */

proc capability data=flow normaltest gout=graph cipctldf cipctlnormal;
   title2 'Use log(flow) as the response variable';
   var logflow;
   qqplot logflow / normal;  /* create qq plot */
   /* the following interval statement asks for prediction intervals (method=1) for k=1 
      future observations. method=3 is a tolerance interval */
   interval logflow / k=1 methods=(1,3) ;
run;


/* Now to generate png graphs for inclusion into output */
goptions device=png  gaccess=sasgaedt   /* set up graphics device */
         gsflen=80 gsfname=gsasfile gsfmode=replace rotate=landscape
         colors=(black) ftext=swiss nocharacters;


proc greplay nofs igout=graph;
   list igout;  /* list the entries in the catalogue */
run;

filename gsasfile 'flowSAS001.png';
proc greplay nofs igout=graph;
   replay 1;
run;

filename gsasfile 'flowSAS002.png';
proc greplay nofs igout=graph;
   replay 2;
run;

filename gsasfile 'flowSAS003.png';
proc greplay nofs igout=graph;
   replay 3;
run;

filename gsasfile 'flowSAS004.png';
proc greplay nofs igout=graph;
   replay 4;
run;

filename gsasfile 'flowSAS005.png';
proc greplay nofs igout=graph;
   replay 5;
run;

