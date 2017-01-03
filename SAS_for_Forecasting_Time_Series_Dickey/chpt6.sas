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


/* Use the DATA steps below with the appropriate PROC statements in  */
/* Chapter 6, "State Space Modeling," of SAS FOR FORECASTING         */
/* TIME SERIES, SECOND EDITION, to produce Output 6.1-6.3.           */
                                                     

DATA TEST;
   Y1=0; X1=0; Y2=0; X2=0;
   DATE = '01JAN55'D;
   DO T=-100 TO 2000;
   DATE=DATE+1; FORMAT DATE DATE7.;
   EY=NORMAL(1827655);
   EX=NORMAL(1827655);
   YY = 1.3*Y1 - 0.9*X1 -0.4*Y2 +EY;
   XX = .1*Y1 + .7*X1 + EX;  Y=YY+15;
   X = XX + 8;
   IF T>O THEN OUTPUT;
   Y2=Y1; Y1=YY; X2=X1; X1=XX;
   END;
RUN;
PROC STATESPACE OUT=OUTSTATE LEAD=30; VAR Y X;
   *** Comment out the following statement for
      the first (unrestricted) output***;
   RESTRICT F(2,3)=0;
   ID DATE;
RUN;
DATA PLOTS;
   SET OUTSTATE;
   FX=.; FY=.; UX=.; LX=.; UY=.; LY=.;
   IF X=. THEN DO;
   FY=FOR1; LY=FOR1-1.96*STD1; UY=FOR1+1.96*STD1;
   FX=FOR2; LX=FOR2-1.96*STD2; UX=FOR2+1.96*STD2;
END;
RUN;
GOPTIONS RESET=ALL;
TITLE "(Y(t), X(t)) DATA AND FORECASTS";
PROC GPLOT;
   PLOT (X FX LX UX Y FY LY UY)*DATE/OVERLAY HMINOR=0 VMINOR=0 LEGEND;
   WHERE DATE > '01JUL60'D;
   SYMBOL1 V=NONE I=JOIN C=BLACK R=1;
   SYMBOL2 V=DOT  I=JOIN C=BLACK R=1;
   SYMBOL3 V=NONE I=JOIN C=BLACK R=2;
   SYMBOL4 V=NONE I=JOIN L=2 C=GRAY R=1;
   SYMBOL5 V=DOT  I=JOIN C=GRAY R=1;
   SYMBOL6 V=NONE I=JOIN L=2 C=GRAY R=2;
RUN;
QUIT;

/*6.4-6.5*/

DATA TEST;
   ONE:T+1;
   E=NORMAL(1234567);
   GROUP=3;
   IF T<152 THEN GROUP=2;
   IF T< 52 THEN GROUP=1;
   Y=E+.8*E1;
   OUTPUT;
   E1=E;
   IF T<751 THEN GO TO ONE;
RUN;

DATA TEST2;
   SET TEST;
   IF GROUP=2;
RUN;

/*6.6-6.8*/
DATA MINKMUSK;
   INPUT YEAR MINK MUSKRAT @@;
   FORMAT MINK MUSKRAT COMMA10.;
   LMINK=LOG(MINK);
   LMUSKRAT=LOG(MUSKRAT);
   T=_N_;
   TITLE 'HUDSON''S BAY FUR TRAPPING RECORDS 1842-1890';
   CARDS;
1842  17780  549577 1843  25382  543155 1844  24855  265177
1845  32031  295617 1846  53264  303172 1847  36621  248710
1848  37123  224347 1849  35712  179075 1850  24772  192261
1851  17827  291281 1852  27413  488238 1853  39686  527161
1854  49373  319444 1855  58628  260805 1856  54924  295847
1857  65522  311953 1858  73066  243862 1859  55720  206156
1860  32548  205471 1861  38306  330527 1862  45534  356789
1863  59599  429304 1864  63724  367302 1865  49349  424875
1866  52113  312543 1867  73752  610280 1868  81769  469775
1869  35081  272963 1870  28184  436509 1871  35660  590916
1872  44352  711174 1873  55496  659159 1874  63810  474942
1875  83319  626711 1876  79206  583319 1877  90080  464297
1878  63318  511993 1879  36360  519963 1880  38828  830100
1881  40834 1028187 1882  56882 1081489 1883  52258 1082999
1884 110610  817003 1885  76393  347050 1886  64215  380022
1887  82941  344818 1888  43641  223615 1889  35400  322360
1890  29363  574742
;
DATA DETREND;
   SET MINKMUSK;
   T+1;
RUN;
PROC REG DATA=MINKMUSK NOPRINT;
   MODEL LMINK LMUSKRAT=T;
   OUTPUT OUT=RESID R=RMINK RMUSKRAT;
RUN;
PROC STATESPACE NOCENTER DATA=RESID;
   VAR RMINK RMUSKRAT;
RUN;
PROC ARIMA;
   I VAR=RMINK NOPRINT;
   E P=2 ; 
   I VAR=RMUSKRAT CROSSCOR=(RMINK) NLAG=10; 
RUN; 
QUIT;
