*-------------------------------------------------------------------*/
 /*         SAS FOR FORECASTING TIME SERIES, SECOND EDITION           */
 /*          by John C. Brocklebank and David A. Dickey               */
 /*       Copyright(c) 2003 by SAS Institute Inc., Cary, NC, USA      */
 /*                   SAS Publications order # 57275                  */
 /*  Jointly co-published by SAS Institute and John Wiley & Sons 2003 */
 /*                   SAS ISBN 1-59047-182-2                          */
                John Wiley & Sons, Inc. ISBN 0-471-39566-8            */
 /*-------------------------------------------------------------------*/
 /*                                                                   */
 /* This material is provided "as is" by SAS Institute Inc.  There    */
 /* are no warranties, expressed or implied, as to merchantability or */
 /* fitness for a particular purpose regarding the materials or code  */
 /* contained herein. The Institute is not responsible for errors     */
 /* in this material as it now exists or will exist, nor does the     */
 /* Institute provide technical support for it.                       */
 /*                                                                   */
 /*-------------------------------------------------------------------*/
 /* Questions or problem reports concerning this material may be      */
 /* addressed to the author:                                          */
 /*                                                                   */
 /* SAS Institute Inc.                                                */
 /* Books by Users                                                    */
 /* Attn: John Brocklebank and David Dickey                           */
 /* SAS Campus Drive                                                  */
 /* Cary, NC   27513                                                  */
 /*                                                                   */
 /*                                                                   */
 /* If you prefer, you can send email to:  sasbbu@sas.com             */
 /* Use this for subject field:                                       */
 /*     Comments for John Brocklebank and David Dickey                */
 /*                                                                   */
 /*-------------------------------------------------------------------*/


/* Chapter 7 Output 7.1 - 7.5*/



DATA PLANTS; 
   TITLE "ENZYME ACTIVITY";
   DO T=1 TO 30;
      INPUT Y @@;
      PI=3.1415926; 
      S1=SIN(2*PI*T/3);
      C1=COS(2*PI*T/3); 
      OUTPUT;
   END; 
   CARDS; 
265.945 290.385 251.099 285.870 379.370 301.173 283.096 306.199 341.696
246.352 310.648 276.348 234.870 314.744 261.363 321.780 313.289 253.460
307.988 303.909 284.128 252.886 317.432 287.160 213.168 308.458 296.351
283.666 333.544 316.998
;
RUN; 
PROC REG DATA=PLANTS;  
   MODEL Y = S1 C1/SS1;
   OUTPUT OUT=OUT1 PREDICTED=P RESIDUAL=R;
RUN; 
DATA ANNO;
   SET OUT1;
   XSYS="2";
   YSYS="2"; 
   FUNCTION="MOVE";
   X=T;
   OUTPUT;
   Y=P;
   FUNCTION = "DRAW";
   OUTPUT; 
RUN;
PROC GPLOT DATA=OUT1 ANNOTATE=ANNO;
   PLOT (Y P)*T/OVERLAY HMINOR=0 VMINOR=0; 
   SYMBOL1 V=DIAMOND I=NONE C=BLACK W=3;  
   SYMBOL2 V=DOT I=SPLINE C=BLACK L=1 ; 
   TITLE "ENZYME ACTIVITY";
RUN; 
PROC SPECTRA DATA=PLANTS OUT=OUT2 WHITETEST COEF;
   VAR Y; 
RUN;
DATA OUT2;
   SET OUT2;
   SSE = P_01;
   TITLE "ENZYME DATA"; 
   IF PERIOD=3 OR PERIOD=. THEN SSE=0; 
   IF ROUND (FREQ, .0001) = 3.1416 THEN SSE = .5*P_01; 
RUN;
PROC PRINT DATA=OUT2;
   SUM SSE; 
RUN;
PROC GPLOT DATA=OUT2;
   PLOT P_01*FREQ /HMINOR=0 VMINOR=0;
   WHERE FREQ>0; 
   SYMBOL1 V=DOT I=NEEDLE C=BLACK;
   TITLE "ENZYME PERIODOGRAM"; 
   LABEL P_01 = "Ordinate"; 
RUN; 
QUIT;

/*Output 7.6*/

DATA A;
   PI2 = 8*ATAN(1); 
   DO T = 1 TO 36 BY .1; INT=.; 
      S1=SIN(PI2*T/12); S2=5+SIN(2*PI2*T/12); S3=10+SIN(3*PI2*T/12); 
      C1=2.5+COS(PI2*T/12); C2=7.5+COS(2*PI2*T/12);
      C3=12.5+COS(3*PI2*T/12);
      SS1=.; SS2=.; SS3=.; CC1=.; CC2=.; CC3=.; 
      IF ABS(T-ROUND(T))<.0001 THEN DO; 
      INT=-1.5; SS1=S1; SS2=S2; SS3=S3; CC1=C1; CC2=C2; CC3=C3; 
   END;  
   OUTPUT;
   END; 
RUN;
PROC GPLOT DATA=A; 
   AXIS1 ORDER=40 TO 0 BY -5; 
   PLOT T*(S1--C3 SS1--CC3 INT)/OVERLAY VAXIS=AXIS1 HMINOR=0 VMINOR=0
                                HREF = 0 2.5 5 7.5 10 12.5; 
   SYMBOL1 V=NONE I=SPLINE C=BLACK R=6; 
   SYMBOL2 V=DOT I=NONE C=BLACK R=7; 
   TITLE "X MATRIX OF WAVES"; 
RUN; 
QUIT;

/*7.7-7.10*/

DATA HARMONIC;
   TITLE "PERIOD 12 PLUS 1 HARMONIC"; 
   ARRAY S(6); ARRAY C(6); PI=3.1415926; DROP PI; 
   DO T=1 TO 36  BY .1;
      Y=.;
      IF  ABS(T-ROUND(T))<.0001 THEN DO;
      Y=10 + 5*SIN(2*PI*T/12) + 4*SIN(4*PI*T/12+.4) +
      NORMAL(1827677);
   END;  
   DO J=1 TO 6;
      S(J)=SIN(2*PI*J*T/12);
      C(J)=COS(2*PI*J*T/12); 
   END;
   OUTPUT; 
   END;  
RUN;
PROC REG DATA=HARMONIC NOPRINT;
   MODEL Y = S1 C1;
   OUTPUT OUT=OUT1 PREDICTED=P1; 
RUN;
PROC REG DATA=HARMONIC NOPRINT;
   MODEL Y = S1 C1 S2 C2;
   OUTPUT OUT=OUT2 PREDICTED=P2; 
RUN;
PROC REG DATA=HARMONIC;
   MODEL Y = S1 C1 S2 C2 S3-S5 C3-C6/SS1;
   HARMONICS: TEST S3=0, C3=0, S4=0, C4=0, S5=0, C5=0, S6=0, C6=0; 
   OUTPUT OUT=OUT3 PREDICTED=P3; 
RUN;
DATA OUT2;
   SET OUT2;
   T=T+36; 
RUN;
DATA OUT3;
   SET OUT3;
   T=T+72;
RUN;
PROC MEANS; 
RUN;
DATA ALL;
   SET OUT1 OUT2 OUT3; 
RUN;
PROC GPLOT DATA=ALL;
   PLOT (Y P1 P2 P3)*T/OVERLAY HREF=36.5 72.5 HMINOR=0 VMINOR=0;  
   SYMBOL1 V=DOT I=NONE C=BLACK; 
   SYMBOL2 V=NONE L=1 I=JOIN R=3; 
   TITLE "PERIOD 12 PLUS 1 HARMONIC"; 
RUN;
DATA COMPRESS;
   SET HARMONIC;
   IF Y NE .; 
RUN;
PROC SPECTRA DATA=COMPRESS P S ADJMEAN OUT=OUTSPECTRA;
   VAR Y;
   WEIGHTS 1 2 3 4 3 2 1; 
RUN;
PROC GPLOT DATA=OUTSPECTRA; 
   PLOT P_01*FREQ ;
   SYMBOL1 V=DOT I=NEEDLE;  
   TITLE "PERIODOGRAM, PERIOD 12 PLUS 1 HARMONIC"; 
RUN;
PROC GPLOT DATA=OUTSPECTRA; 
   PLOT S_01*FREQ /HMINOR=0 VMINOR=0; 
   SYMBOL1 V=DOT I=NEEDLE;  
   TITLE "TRIANGULAR WEIGHTS, PERIOD 12 PLUS 1 HARMONIC"; 
RUN;
QUIT;

/*Output 7.11*/

PROC GREPLAY NOFS;
   IGOUT=WORK.GSEG;
   DELETE _ALL_;
  ** clear out the existing graphics catalog ***;
RUN;
DATA THREE;
   PI = 4*ATAN(1);
   DROP PI; 
   Y=10; W=10; Z=10; Y1=10; 
   DO T = -50 TO 5000;
      E=.6*NORMAL(6920061); 
      Y = 10 + .8*(Y-10)+E;
      YPLUS = Y + .2*SIN(2*PI*T/25 + .1); 
      Z = 10 - .8*(Z-10)+E;
      D = Y-Y1; 
      W = 10 + 5*E/3;
      Y1=Y;
      DROP Y1; 
      IF T>0 THEN OUTPUT;
   END;
RUN; 
PROC GPLOT DATA=THREE;
   PLOT (Y YPLUS)*T/OVERLAY HMINOR=0 VMINOR=0;
      WHERE T<200; 
   SYMBOL1 V=NONE I=JOIN C=GRAY80; 
   SYMBOL2 V=NONE I=JOIN C=BLACK; 
   SYMBOL3 V=NONE I=JOIN C=GRAY40; 
RUN;
PROC MEANS MEAN VAR;
   VAR W Y Z D YPLUS; 
PROC SPECTRA P S ADJMEAN OUT=SMOOTH; 
   WEIGHTS 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 ; 
   VAR W Y Z D YPLUS; 
DATA SMOOTH;
   SET SMOOTH; 
   F_02 = 8*ATAN(1)*(1.64-1.6*COS(FREQ)); F_02=.36/F_02; 
   F_03 = 8*ATAN(1)*(1.64+1.6*COS(FREQ)); F_03=.36/F_03; 
   LABEL FREQ = "- pi to pi"; 
   LABEL S_01 = "Smoothed"; 
   OUTPUT;
   FREQ=-1*FREQ;
   OUTPUT;
RUN; 
PROC SORT;
   BY FREQ;   
RUN;
PROC GPLOT UNIFORM;
   TITLE "SYMMETRIZED SPECTRUM"; 
   PLOT (P_01-P_05)*FREQ /HMINOR=0 VMINOR=0; 
   SYMBOL1 V=DOT H=.2 I=NEEDLE C=BLACK; 
   LABEL P_01 = "W"; LABEL P_02 = "Y"; LABEL P_03 = "Z"; 
   LABEL P_04 = "D"; LABEL P_05 = "Y + wave"; 
RUN;
PROC GPLOT DATA=SMOOTH; 
   PLOT (S_01-S_03 F_02 F_03)*FREQ/OVERLAY VREF=.1592 CVREF=RED
                                   HMINOR=0 VMINOR=0; 
   SYMBOL1 V=NONE I=JOIN C=GRAYB0; 
   SYMBOL2 V=NONE I=JOIN C=GRAY80; 
   SYMBOL3 V=NONE I=JOIN C=GRAY40;
   SYMBOL4 V=NONE I=JOIN C=BLACK W=2 R=2; 
RUN; 
PROC GREPLAY TC=TEMPCAT NOFS IGOUT=WORK.GSEG;
   TDEF SEVEN DES='SEVEN PANELS'

1/LLX =  0 LLY = 60  
  ULX =  0 ULY = 98  
  URX = 33 URY = 98  
  LRX = 33 LRY = 60  

2/LLX = 34 LLY = 60
  ULX = 34 ULY = 98
  URX = 67 URY = 98
  LRX = 67 LRY = 60

3/LLX = 68  LLY = 60
  ULX = 68  ULY = 98
  URX = 100 URY = 98
  LRX = 100 LRY = 60

4/LLX =  2 LLY = 40
  ULX =  2 ULY = 60
  URX = 98 URY = 60
  LRX = 98 LRY = 40

5/LLX =  0 LLY =  2  
  ULX =  0 ULY = 40  
  URX = 33 URY = 40  
  LRX = 33 LRY =  2  

6/LLX = 34 LLY =  2  
  ULX = 34 ULY = 40  
  URX = 67 URY = 40 
  LRX = 67 LRY =  2  
  
7/LLX = 68  LLY =  2 
  ULX = 68  ULY = 40 
  URX = 100 URY = 40
  LRX = 100 LRY =  2
;
   TEMPLATE = SEVEN;

   TREPLAY 1:GPLOT1  2:GPLOT2 3:GPLOT3 4:GPLOT 5:GPLOT4 6:GPLOT5 7:GPLOT6;
RUN ;
QUIT; 


  /*Output 7.12*/


DATA A;
   ONE:T+1;
   IF T=1 THEN X1=0;
   IF T=1 THEN Y1=0;
   X=.5*X1+NORMAL(1234567);
   Y=.8*Y1+X;
   TEMP=Y+NORMAL(1234567);
   OUTPUT;
   X1=X;
   Y1=Y;
   IF T<512 THEN GO TO ONE;
RUN;
PROC SPECTRA DATA=A OUT=OOO P S CROSS A K PH;
   WEIGHTS 1 1 1 1 1 1 1 1 1 1 1;
   VAR TEMP X;
RUN;
DATA NEXT;
   SET OOO;
   FXX=1/(2*3.1415927*(1.25-COS(FREQ)));
   C=(1-.8*COS(FREQ))/(1.64-1.6*COS(FREQ));
   Q=-.8*SIN(FREQ)/(1.64-1.6*COS(FREQ));
   CSPEC=C*FXX;
   QSPEC=Q*FXX;
   GAIN=SQRT(C*C+Q*Q);
   AMP=GAIN*FXX;
   PHASE=ATAN(Q/C);
   FYY=AMP*AMP/FXX+1/(2*3.1415927);
   K=AMP*AMP/(FXX*FYY);
RUN;
PROC GPLOT DATA=NEXT;
   PLOT PHASE*FREQ=1 PH_01_02*FREQ=2/OVERLAY HMINOR=0 VMINOR=0; 
   TITLE 'PHASE OF Y BY X';
   SYMBOL1 C=BLACK  V=NONE I=JOIN L=1;
   SYMBOL2 C=BLACK  V=NONE I=JOIN L=2;
RUN;
PROC GPLOT DATA=NEXT;
   PLOT K*FREQ=1 K_01_02*FREQ=2/OVERLAY VMINOR=0 HMINOR=0 VAXIS=0 TO 1 BY .25;
   TITLE 'SQUARED COHERENCY';
   SYMBOL1 C=BLACK  V=NONE I=JOIN L=1;
   SYMBOL2 C=BLACK  V=NONE I=JOIN L=2;
RUN;
PROC GPLOT DATA=NEXT;
   PLOT AMP*FREQ=1 A_01_02*FREQ=2/OVERLAY HMINOR=0 VMINOR=0; 
   TITLE 'CROSS AMPLITUDE Y BY X';
   SYMBOL1 C=BLACK  V=NONE I=JOIN L=1;
   SYMBOL2 C=BLACK  V=NONE I=JOIN L=2;
RUN;
PROC GPLOT DATA=NEXT ;
   PLOT CSPEC*FREQ=1 CS_01_02*FREQ=2/OVERLAY HMINOR=0 VMINOR=0 VAXIS=0 TO 4 BY 1;
   TITLE 'COSPECTRUM';
   SYMBOL1 C=BLACK  V=NONE I=JOIN L=1;
   SYMBOL2 C=BLACK  V=NONE I=JOIN L=2;
RUN;
PROC GPLOT DATA=NEXT;
   PLOT QSPEC*FREQ=1 QS_01_02*FREQ=2/OVERLAY HMINOR=0 VMINOR=0 VAXIS=-1.5 TO 1 BY .5;
   TITLE 'QUADRATURE SPECTRUM';
   SYMBOL1 C=BLACK  V=NONE I=JOIN L=1;
   SYMBOL2 C=BLACK  V=NONE I=JOIN L=2;
RUN;
QUIT;

/*Output 7.13*/

DATA NEUSE;
   TITLE 'FLOW RATES OF NEUSE RIVER AT GOLDSBORO AND KINSTON';
   RETAIN DATE '30SEP70'D;
   INPUT GOLD KINS @@;
   LGOLD=LOG(GOLD);
   LKINS=LOG(KINS);
   X=LGOLD-LAG(LGOLD);
   Y=LKINS-LAG(LKINS);
   DATE=INTNX('DAY',DATE,1);
   FORMAT DATE DATE7.;
CARDS;
   525   548    616   615    361   716    298   555    298   433    160   415 
   153   342    182   271    212   280    262   295    160   349    184   289 
   143   286    190   265    156   280    146   280    138   286    187   271 
   361   262    271   373    316   433    382   471    416   583    539   653 
   556   643    429   709    475   646    464   594    367   622    271   562 
   528   534    493   759   1030   875   1460  1050   1330  1480   1100  1540 
   895  1390    753  1190    620  1020    592   864    606   777    620   852 
   810  1000   1520  1140   1810  1520   1880  1880   2060  1990   2250  2100 
  1770  2250   1810  2090   1350  1950   1110  1720   1010  1410    912  1260 
   808  1160    765  1060    736   997    701   952    696   919    689   887 
   605   871    591   811    702   751    619   826    563   821    613   738 
   612   752    593   768    573   751    562   738    539   723    509   704 
   523   676    574   655    512   687    488   682    640   652    962   878 
  1150  1240   1710  1360   1770  1670   1600  1880   1390  1830   1400  1690 
  1540  1710   1790  1800   2050  1910   2120  2100   1890  2230   1700  2160 
  1360  1940   1210  1700   1130  1550   1110  1450   1190  1380   1350  1410 
  1490  1520   1730  1640   2770  1830   3500  2620   4000  3080   4400  3610 
  4900  4110   5420  4560   5480  5000   5130  5380   4560  5820   3870  5990 
  3230  5800   2750  5230   2470  4490   2200  3790   1980  3210   1800  2760 
  1690  2460   1900  2310   2540  2580   3130  3040   3560  3420   3670  3780 
  3370  4040   2880  4150   2750  4220   2820  4140   3130  4050   3320  3970 
  3250  4050   3300  4160   3740  4270   4660  4430   5560  4740   6480  5310 
  7200  5950   7870  6480   8630  7000   9510  7830  10300  8670  10700  9350 
 10600 10100  10300 10700   9770 11000   8970 11100   7570 10800   5990 10200 
  4290  9380   4630  8450   5540  7460   6440  6860   7040  6670   7070  6860 
  6070  7200   4750  7460   4230  7390   5560  7370   7790  8220   8410  9170 
  9400  9300  10400  9510  11400 10000  12200 10700  12300 11500  11800 12400 
 10700 12900   8570 13000   6260 12500   4080 11300   3500  9070   3410  7010 
  3530  5530   3560  4930   3280  5620   2810  4280   2520  3940   2350  3480 
  2230  3090   2040  2850   2080  2710   2470  2760   2940  2980   3300  3240 
  3900  3560   4510  3920   4930  4320   5190  4770   5360  5200   5080  5560 
  3930  5770   4450  5980   5280  5950   6020  6110   6580  6390   6880  6670 
  6930  6940   6850  7130   6640  7250   5700  7260   3930  7140   2700  6790 
  2350  5530   2060  4160   1840  3160   1690  2660   1560  2260   1600  2060 
  1630  1990   1620  2020   1670  2010   1620  1990   1530  1970   1700  1980 
  2030  2220   2670  2390   3070  2660   3040  3000   2630  3230   2130  3190 
  1850  2830   1600  2350   1360  2000   1240  1730   1180  1540   1100  1440 
  1110  1350    999  1310   1120  1300   1220  1460   2160  1560   2760  2210 
  3020  2750   3550  3070   3910  3330   4110  3620   4280  4100   4240  4430 
  3620  4450   1900  4370   1370  3520   1180  2110   1030  1520    902  1310 
   874  1410   1070  1270   1910  1270   2580  1760   3000  2280   3170  2700 
  3350  2940   2880  3110   1490  3180   1020  2320    846  1360    782  1060 
   711   946    525   872    630   732    753   764    634   856    634   804 
   651   753    623   773    623   763    641   757    683   756    634   779 
   578   766    606   844    571   975    588   826    585   758    634   747 
   585   762    477   729    407   649    394   580    519   561    842   675 
   606   972    560   821    439   722    616   680    749   897    854  1030 
   918   999    854  1110   1030  1080   1190  1140    942  1260    627  1160 
   532   855    442   708    400   627    314   552    362   506    445   524 
   455   588    690   596    752   690    602   825    458   768    388   722 
   506   688    602   699   1350   720   1480  1250   1370  1550   1140  1580 
   914  1410    814  1170    763   987    588   921    850   784   1090   828 
   926  1080    606  1050    529   830    483   670    423   625    350   574 
   347   511    445   535    543   718    602   995    609  1090    806   954 
   874   968    763  1060    676   997    496   920    648   773    858   779 
   958  1030    822  1330    954  1360   1040  1300   1430  1300   1520  1480 
  1000  1670    641  1370    557   998    435   807    338   715    296   603 
   376   553    474   570    423   654    359   672    376   703    407   939 
   439   926   1620   728   2310  1340   2590  2030   2370  2420   1090  2480 
   651  1660    451  1060    496   883    464   697    557   648    539   634 
   529   658    416   641    376   578    362   504    567   655    790  1840 
   906  2360   2400  2680   3470  3080   4040  3580   4420  3990   4490  4350 
  4420  4560   4400  4700   4630  4890   4920  5130   5110  5470   5240  5680 
  5180  5710   5060  5710   5040  5660   5020  5600   4820  5530   3420  5440 
  2070  5240   1650  4250   1530  3410   2330  3740   3470  4440   4600  5130 
  5330  5380   6140  5630   6870  5990   7470  6420   7830  6860   7890  7330 
  7720  7780   7580  8100   7630  8320   7580  8280
;
RUN;
PROC ARIMA;
   I VAR=LGOLD(1) NOPRINT;
   E P=1 Q=1 ML NOCONSTANT;
   I VAR=LKING(1) CROSSCOR=(LGOLD(1)) NOPRINT;
   E INPUT=(1$(1)LGOLD) P=2 Q=1 ML MAXIT=100 NOCONSTANT;
RUN;
DATA NEUSE2;
   SET NEUSE;
   IF Y NE .;
RUN;
PROC SPECTRA K PH OUT=NEUSE3 S CROSS ADJMEAN;
   VAR X Y; WEIGHTS 1 2 3 4 5 6 7 6 5 4 3 2 1;
RUN;
DATA NEUSE4;
   SET NEUSE3;
   PI=3.14159265;
   SPEC_X=((1+.874**2-2*COS(FREQ)*.874)*.0399/(2*PI))/
           (1+1.241**2+.291**2+.117**2
            -2*COS(FREQ)*(1.241+1.241*.291-.291*.117)
            -2*COS(2*FREQ)*(1.241*.117-.291)+2*COS(3*FREQ)*.117);
   PHASE=ATAN((.495*SIN(FREQ)+.273*SIN(2*FREQ))/(.495*COS(FREQ)+
               .273*COS(2*FREQ)));
   SPEC_V=(1+.888**2 -2*.888*COS(FREQ))/
           (1+1.163**2+.48**2 -2*COS(FREQ)*1.163*.48 -2*COS(FREQ)*1.163
            +2*COS(2*FREQ)*.48)*.0058/(2*PI);
   MODAFSQ=(.495*COS(FREQ)+.273*COS(2*FREQ))**2+(.495*SIN(FREQ)+
            .273*SIN(2*FREQ))**2;
   SPEC_Y=SPEC_X*MODAFSQ+SPEC_V;
   COHER=MODAFSQ*SPEC_X/SPEC_Y;
   IF PH_01_02 < 0 THEN DO;
      PH_01_02=ABS(PH_01_02);
      PHASE=PHASE+PI;
   END;
   IF PHASE < 0 THEN PHASE=PHASE+PI;
   IF PH_01_02 > 1 AND PHASE < .5 THEN PHASE=PHASE+PI;
RUN;
PROC REG DATA=NEUSE4(OBS=75) NOPRINT;
   MODEL PHASE=FREQ;
RUN;
PROC GPLOT DATA=NEUSE4;
    PLOT (SPEC_X S_01)*FREQ/OVERLAY HMINOR=0 
                            VAXIS=0 TO .03 BY .005 VMINOR=0;
    LABEL SPEC_X='LGOLD';
    TITLE h=3 'MODEL AND DATA SPECTRA';
    TITLE2 'LOG GOLDSBORO % CHANGE IN FLOW';
    SYMBOL1 L=1 I=JOIN C=BLACK;
    SYMBOL2 L=1 I=JOIN C=BLACK;
RUN;
PROC GPLOT DATA=NEUSE4;
    PLOT (SPEC_Y S_02)*FREQ/OVERLAY HMINOR=0
                            VAXIS=0 TO .018 BY .003 VMINOR=0;
    LABEL SPEC_Y='LKINS';
    TITLE h=3 'MODEL AND DATA SPECTRA';
    TITLE2 'LOG KINSTON % CHANGE IN FLOW';
    SYMBOL1 L=1 I=JOIN C=BLACK;
    SYMBOL2 L=1 I=JOIN C=BLACK;
RUN;
PROC GPLOT DATA=NEUSE4;
    PLOT (PHASE PH_01_02)*FREQ/OVERLAY HMINOR=0 VMINOR=0;
    TITLE h=3 'PHASE SPECTRA';
    TITLE2 'FROM DATA AND MODEL';
    SYMBOL1 L=1 I=JOIN C=BLACK;
    SYMBOL2 L=1 I=JOIN C=BLACK;
RUN;
PROC GPLOT DATA=NEUSE4;
    PLOT (COHER K_01_02)*FREQ/OVERLAY HMINOR=0 VMINOR=0
                              VAXIS=0 TO 1 BY .25;
    TITLE h=3 'COHERENCIES';
    TITLE2 'LOG % CHANGE KINS FLOW AND GOLD FLOW';
    SYMBOL1 L=1 I=JOIN C=BLACK;
    SYMBOL2 L=1 I=JOIN C=BLACK;
RUN;
QUIT;


/*Output 7.14*/

DATA A;
   PI = 4*ATAN(1);
   X=0;
   DO T = 1 TO 64;
      Y = 3*X;
      X=NORMAL(1827655);
      IF T=64 THEN X=0;
      OUTPUT;
   END;
RUN;
PROC SPECTRA P S CROSS A K PH OUT=OUT1 COEFF;
   VAR X Y;
RUN;
PROC PRINT LABEL DATA=OUT1;
   WHERE PERIOD > 12;
   ID PERIOD FREQ;
RUN;
QUIT;