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



/* Use the DATA step below with the appropriate PROC statements in   */
/* Chapter 2, "Simple Models: Autoregression," of SAS FOR            */
/* FORECASTING TIME SERIES, SECOND EDITION, to produce Output 2.1.   */


DATA EXAMPLE;
   ONE:T+1;
   IF T=1 THEN Y=100+NORMAL(1234567)/SQRT(.36);
   IF T=1 THEN Y=100+20*NORMAL(1234567)/SQRT(.36);
   Y=100+.8*(Y-100)+20*NORMAL(1234567);
   OUTPUT;
   RETAIN;
   IF T<200 THEN GO TO ONE;
RUN;

/*code to produce 2.2-2.3*/


** clear out the existing graphics catalog ***;
PROC GREPLAY  NOFS; IGOUT=WORK.GSEG;  DELETE _ALL_;
  
%MACRO ANNO(DSN,RHO,MU,ZMIN,ZMAX);
DATA ANNO&DSN; XSYS="2"; YSYS="2"; ZSYS="2";
   WHEN = "B";  FUNCTION="MOVE"; Z=1*&ZMIN; COLOR="GRAY";
   X = .5; Y=&MU; OUTPUT; X = 1.2; FUNCTION="DRAW"; OUTPUT;
   FUNCTION = "MOVE";
   X = &RHO; Y=9; OUTPUT; Y=16; FUNCTION="DRAW"; OUTPUT;
   FUNCTION = "MOVE"; Y=10; X=.5; Z=1*&ZMIN; COLOR="BLACK";
   WHEN="A";  OUTPUT;
   FUNCTION = "DRAW"; Z=1*&ZMAX + .02*(&ZMAX-&ZMIN); OUTPUT;
   X=1.2; OUTPUT; WHEN="B";  Z=1*&ZMIN; OUTPUT;
   TITLE "ESTIMATES: " FONT=GREEKU "m" FONT=CENT " &mu, "FONT=GREEKU "r" FONT=CENT " &rho";
   LABEL
%MEND ANNO;


DATA A;
   ARRAY Y(10) Y1-Y10;
   ARRAY X(10) X1-X10;
   INPUT Y1-Y10 @@;
   PI = 4*ATAN(1);
   DO MU = 9 TO 16 BY .1;
   DO I=1 TO 10;
      X(I) = Y(I) - MU;
   END;
   DO RHO = .5 TO 1.2 BY .01;
      CSS = X(1)**2;
   DO T=2 TO 10;
      CSS = CSS + (X(T) - RHO*X(T-1))**2;
   END;
   USS = (RHO < 1)*(1-RHO*RHO)*X1**2;
   DO T=2 TO 10;
      USS = USS + (X(T) - RHO*X(T-1))**2;
   END;
   IF RHO<1 THEN NLIKE=(1/2)*LOG(1-RHO*RHO)-(10/2)*LOG(USS)-(10/2)-(10/2)*LOG(2*PI/10);
   ELSE NLIKE =  -999999;
   NLIKE = -1*NLIKE;
   IF NLIKE > 23.25  THEN NLIKE = 23.25;
   IF CSS > 63 THEN CSS=63;
   IF USS > 54 THEN USS=54;
OUTPUT;
END;
END;
CARDS;
14 15 14 10 12 10 5 6 6 8
;
RUN;

PROC MEANS MIN MAX;
   VAR USS CSS NLIKE;
   AXIS1 LABEL = (HEIGHT=1.25 FONT=CENT );
   AXIS2 LABEL = (HEIGHT=1.25 FONT=GREEK 'r');
   AXIS3 LABEL = (HEIGHT=1.25 FONT=GREEK 'm');
RUN;

PROC GPLOT DATA=A;
   PLOT CSS*RHO/HREF = .71186 HAXIS=AXIS2;
   WHERE ABS(MU-10)<.0001;
   TITLE "RHO ESTIMATE .71186" ;
   TITLE2 "CLS - CENTERED";
   SYMBOL1 V=NONE I=JOIN;
RUN;

PROC GPLOT DATA=A;
   PLOT USS*RHO/HREF = .82349 HAXIS=AXIS2;
   WHERE ABS(MU-10)<.0001;
   TITLE "RHO ESTIMATE .82324";
   TITLE2 "ULS - CENTERED";
RUN;

PROC GPLOT DATA=A;
   PLOT NLIKE*RHO/HREF = .73829 HAXIS=AXIS2;
   WHERE ABS(MU-10)<.0001;
   TITLE "RHO ESTIMATE .73829";
   TITLE2 "ML - CENTERED";
RUN;

%ANNO(1,.94097,13.54,55.46,63);
PROC G3D DATA=A ANNOTATE=ANNO1;
   PLOT MU*RHO = CSS;
   TITLE2 "CLS";
RUN;

%ANNO(2,.82324,10.48,52.69,54);
PROC G3D DATA=A ANNOTATE=ANNO2;
  PLOT MU*RHO = USS;
  TITLE2 "ULS";
RUN;

%ANNO(3,.73899,10.36,22.962,23.25);
PROC G3D DATA=A ANNOTATE=ANNO3;
   PLOT MU*RHO = NLIKE;
   TITLE2 "ML" ;
RUN;

PROC GREPLAY TC=TEMPCAT NOFS IGOUT=WORK.GSEG;
   TDEF SIX DES='SIX PANELS'

  1/ LLX =  0   LLY = 66
     ULX =  0   ULY = 96
     URX = 48   URY = 96
     LRX = 48   LRY = 66

   2/LLX = 52   LLY = 66
     ULX = 52   ULY = 96
     URX =100   URY = 96
     LRX =100   LRY = 66


   3/LLX =  0   LLY = 33
     ULX =  0   ULY = 63
     URX = 48   URY = 63
     LRX = 48   LRY = 33

   4/LLX = 52   LLY = 33
     ULX = 52   ULY = 63
     URX =100   URY = 63
     LRX =100   LRY = 33

   5/LLX =  0   LLY =  0
     ULX =  0   ULY = 30
     URX = 48   URY = 30
     LRX = 48   LRY =  0

   6/LLX = 52   LLY =  0
     ULX = 52   ULY = 30
     URX =100   URY = 30
     LRX =100   LRY =  0
;
   TEMPLATE = SIX;

   TREPLAY 1:GPLOT  2:G3D  3:GPLOT1 4:G3D1 5:GPLOT2 6:G3D2;
   REPLAY TEMPLATE;
RUN ;

DATA ESTIMATE;
   INPUT Y @@;
   CARDS;
14 15 14 10 12 10 5 6 6 8
;
RUN;

PROC ARIMA DATA=ESTIMATE;
   I VAR=Y NOPRINT;
   E P=1 PRINTALL;
   E P=1 METHOD=ULS PRINTALL;
   E P=1 METHOD= ML PRINTALL;
   I VAR=Y CENTER;
   E P=1 NOCONSTANT PRINTALL;
   E P=1 METHOD=ULS NOCONSTANT PRINTALL;
   E P=1 METHOD=ML  NOCONSTANT PRINTALL;
RUN;
QUIT;


 /* Use the DATA step below with the appropriate PROC statements in   */
 /* Chapter 2, "Simple Models: Autoregression," of the SAS FOR        */
 /* FORECASTING TIME SERIES, SECOND EDITION, to produce Output 2.4-   */
 /* Output 2.5.                                                       */


DATA SILVER;
   TITLE 'MONTH END STOCKS OF SILVER';
   INPUT SILVER @@;
   T=_N_;
   RETAIN DATE '01DEC76'D LSILVER1-LSILVER4;
   DATE=INTNX('MONTH',DATE,1);
   FORMAT DATE MONYY.;
   OUTPUT;
   LSILVER4=LSILVER3;
   LSILVER3=LSILVER2;
   LSILVER2=LSILVER1;
   LSILVER1=SILVER;
   CARDS;
 846 827 799 768 719 652 580 546 500 493 530 548 565 572 632 645 674 
 693 706 661 648 604 647 684 700 723 741 734 708 728 737 729 678 651 
 627 582 521 519 496 501 555 541 485 476 515 606 694 788 761 794 836 
 846
;
RUN;