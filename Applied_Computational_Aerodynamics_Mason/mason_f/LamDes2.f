c
c     Lamar's Design/Optimization Code
c   
c     mods by W. H. Mason
c
c     last mod: April 5, 1994
c     bug fix from Oleg Golovidov, Dec. 1995
c
      common /inout/ iwrit,iread,iecho,ioutwr,iplfit
      character*24 filenm

      iwrit   = 6
      iecho   = 1
      iread   = 8
      ioutwr  = 0

      write(iwrit,900)
      read(5,*) filenm

      open(unit = iread,file=filenm,status='old')

      call lamard
      stop

  900 format(/2x,'enter name of input file:'/)

      end


      subroutine lamard
c
c     John Lamar's Minimum Induced Drag Program, NASA TN D 8090
c
      dimension xref(25), yref(25), sar(25), a(25),
     1          rsar(25), x(25), y(25), botsv(2), sa(2), vbord(51),
     2          spy(50,2), kfx(2), iyl(50,2), iyt(50,2)

      common /inout/ iwrit,iread,iecho,ioutwr,iplfit
      common /all/ bot,m,beta,ptest,qtest,tblscw(50),q(400),pn(400),
     1             pv(400),s(400),psi(400),phi(50),zh(50),nssw
      common /maina/ icode,total,aan(2),xs(2),ys(2),kfcts(2),
     1             xreg(25,2), yreg(25,2), areg(25,2), dih(25,2),
     2             mcd(25,2), xx(25,2), yy(25,2), as(25,2),ttwd(25,2),
     3             mmcd(25,2), an(2), zz(25,2),iflag
      common /onetre/ twist(2),cref,sref,cave,cldes,strue,ar,artrue,
     1                rtcdht(2), config, nsswsv(2), msv(2), kbot,
     2                plan,iplan,xmch,sswwa(50), xcfw,xcft,aa,bb
      common /ccrrdd/ chord(50),xte(50),kbit,tspan,tspana
      common /pdrag/ pdrg1(2),pdrg2(2),pdrg3(2),tdklue,nqcdp1(5),
     1               qclp1(50,5),qcdp1(50,5),nqcdp2(5),qclp2(50,5),
     2               qcdp2(50,5), nspolr(2)
      common /opt/    kon,cmb,ipunch,case,spnklu,xitmax,epsmax,crbmnt
      character*80 data
      character*4 prtcon,blank,first,second

      data blank /'    '/, first/'1st '/, second/'2nd '/

c
c     part one - geometry computation
c
c                section one - input of reference wing position
c
      write(iwrit,129)
  129 format(/3x,'Lamar Design Code',5x,'mods by W.H. Mason'/)

      read(iread,128)  data
      write(iwrit,131) data

      total     = 1.
      ptest     = 0.
      qtest     = 0.
      twist(1)  = 0.
      twist(2)  = 0.
      rtcdht(1) = 0.
      rtcdht(2) = 0. 
      YTOL      = 1.E-10
      AZY       = 1.E+13
      PIT       = 1.5707963
      RAD       = 57.29578

C     SET PLAN EQUAL TO 1. FOR A WING ALONE COMPUTATION 
C     SET PLAN EQUAL TO 2. FOR A WING - TAIL COMBINATION
 
  128 format(a80)
  131 format(3x,a80)
      READ (iread,98,END=93) PLAN,xmref,CREF,SREF,tdklue,case,spnklu
      write(iwrit,240) PLAN,xmref,CREF,tdklue,case,spnklu,sref
  240 format(/3x,'plan   =',f4.1,2x,'xmref = ',f8.4,3x,'cref   =',f8.4/
     1        3x,'tdklue = ',f3.1,2x,'case  = ',f5.1,
     2        6x,'spnklu = ',f3.1/
     3        3x,'sref   = ',f12.4/)

      IPLAN = PLAN
      icase = case
 
C     SET AAN(IT) EQUAL TO THE MAXIMUM NUMBER OF CURVES REQUIRED TO
C     DEFINE THE PLANFORM PERIMETER OF THE (IT) PLANFORM,
C
C     SET RTCDH(IT) EQUAL TO THE ROOT CHORD HEIGHT OF THE LIFTING
C     SURFACE (IT), WHOSE PERIMETER POINTS ARE BEING READ IN, WITH
C     RESPECT TO THE WING ROOT CHORD HEIGHT
 
      DO 6 IT=1,IPLAN
      READ (iread,98) AAN(IT),XS(IT),YS(IT),RTCDHT(IT),pdrg1(it),
     1                pdrg2(it),pdrg3(it)
      N=AAN(IT)
      N1=N+1
      MAK=0
       IF (IPLAN.EQ.1) PRTCON=blank          
       IF (IPLAN.EQ.2.AND.IT.EQ.1) PRTCON=FIRST   
       IF (IPLAN.EQ.2.AND.IT.EQ.2) PRTCON=SECOND   
      WRITE (iwrit,97) PRTCON,N,RTCDHT(IT)
      WRITE (iwrit,109)
      DO 5 I=1,N1
      READ (iread,98) XREG(I,IT),YREG(I,IT),DIH(I,IT),AMCD
      xreg(i,it) = xreg(i,it) - xmref
      MCD(I,IT)=AMCD
      IF (I.EQ.1) GO TO 5
      IF (MAK.NE.0.OR.MCD(I-1,IT).NE.2) GO TO 2
      MAK=I-1
2     IF (ABS(YREG(I-1,IT)-YREG(I,IT)).LT.YTOL) GO TO 3
      AREG(I-1,IT)=(XREG(I-1,IT)-XREG(I,IT))/(YREG(I-1,IT)-YREG(I,IT))
      ASWP=ATAN(AREG(I-1,IT))*RAD
      GO TO 4
3     YREG(I,IT)=YREG(I-1,IT)
      AREG(I-1,IT)=AZY
      ASWP=90.
4     J=I-1
C
C     WRITE PLANFORM PERIMETER POINTS AND ANGLES
C
      WRITE (iwrit,106) J,XREG(J,IT),YREG(J,IT),ASWP,DIH(J,IT)
      DIH(J,IT)=TAN(DIH(J,IT)/RAD)
5     CONTINUE
      KFCTS(IT)=MAK
      WRITE (iwrit,106) N1,XREG(N1,IT),YREG(N1,IT)
6     CONTINUE
C
C     PART 1 - SECTION 2
C     READ GROUP 2 DATA AND CONPUTE DESIRED WING POSITION
C
7     READ (iread,105,end=93) CONFIG,SCW,VIC,xmch,CLDES,xitmax,epsmax
      write(iwrit,245) scw,vic,xitmax,epsmax
  245 format(/10x,'scw    = ',f4.1,6x,'vic = ',f4.1/10x,'xitmax = ',
     1        f4.1,3x,'epsmax = ',f8.5/)
c
      write(iwrit,99) config,xmref
8     if( ptest .ne. 0. .and. qtest .ne. 0.) go to 95
      DO 9 I=1,50
9     TBLSCW(I)=SCW
11    DO 37 IT=1,IPLAN
      N=AAN(IT)
      N1=N+1
      DO 12 I=1,N
      XREF(I)=XREG(I,IT)
      YREF(I)=YREG(I,IT)
      A(I)=AREG(I,IT)
      RSAR(I)=ATAN(A(I))
      IF (A(I).EQ.AZY) RSAR(I)=PIT
12    CONTINUE
      XREF(N1)=XREG(N1,IT)
      YREF(N1)=YREG(N1,IT)
      IF (KFCTS(IT).GT.0) GO TO 13
      K=1
      SA(IT)=RSAR(1)*RAD
      GO TO 14
13    K=KFCTS(IT)
14    WRITE (iwrit,102) K,SA(IT),IT
      SB=SA(IT)/RAD
C
C       REFERENCE PLANFORM COORDINATES ARE STORED UNCHANGED FOR WINGS
C       WITHOUT CHANGE IN SWEEP.
C
      DO 16 I=1,N
      X(I)=XREF(I)
      Y(I)=YREF(I)
      IF (RSAR(I).EQ.PIT) GO TO 15
      A(I)=TAN(RSAR(I))
      GO TO 16
15    A(I)=AZY
16    SAR(I)=RSAR(I)
      X(N1)=XREF(N1)
      Y(N1)=YREF(N1)

      DO 36 I=1,N
      XX(I,IT)=X(I)
      YY(I,IT)=Y(I)
      MMCD(I,IT)=MCD(I,IT)
      TTWD(I,IT)=DIH(I,IT)
   36 AS(I,IT)=A(I)
      XX(N1,IT)=X(N1)
      YY(N1,IT)=Y(N1)
      AN(IT)=AAN(IT)
   37 CONTINUE
C
C       LINE UP BREAKPOINTS AMONG PLANFORMS
C
      BOTSV(1)=0.
      BOTSV(2)=0.
      WRITE (iwrit,108)
      DO 49 IT=1,IPLAN
      NIT=AN(IT)+1
      DO 43 ITT=1,IPLAN
      IF (ITT.EQ.IT) GO TO 43
      NITT=AN(ITT)+1
      DO 42 I=1,NITT
      JPSV=0
      DO 38 JP=1,NIT
      IF (YY(JP,IT).EQ.YY(I,ITT)) GO TO 42
   38 CONTINUE
      DO 39 JP=1,NIT
      IF (YY(JP,IT).LT.YY(I,ITT)) GO TO 40
   39 CONTINUE
      GO TO 42
   40 IF(jp .eq. 1) go to 42
      JPSV=JP
      IND=NIT-(JPSV-1)
      DO 41 JP=1,IND
      K2=NIT-JP+2
      K1=NIT-JP+1
      XX(K2,IT)=XX(K1,IT)
      YY(K2,IT)=YY(K1,IT)
      MMCD(K2,IT)=MMCD(K1,IT)
      AS(K2,IT)=AS(K1,IT)
   41 TTWD(K2,IT)=TTWD(K1,IT)
      YY(JPSV,IT)=YY(I,ITT)
      AS(JPSV,IT)=AS(JPSV-1,IT)
      TTWD(JPSV,IT)=TTWD(JPSV-1,IT)
      XX(JPSV,IT)=(YY(JPSV,IT)-YY(JPSV-1,IT))*AS(JPSV-1,IT)+XX(JPSV-1,IT
     1)
      MMCD(JPSV,IT)=MMCD(JPSV-1,IT)
      AN(IT)=AN(IT)+1.
      NIT=NIT+1
  42  CONTINUE
  43  CONTINUE
C
C     SEQUENCE WING COORDINATES FROM TIP TO ROOT
C
      N1=AN(IT)+1.
      DO 44 I=1,N1
  44  Q(I)=YY(I,IT)
      DO 48 J=1,N1
      HIGH=1.
      DO 45 I=1,N1
      IF ((Q(I)-HIGH).GE.0.) GO TO 45
      HIGH=Q(I)
      IH=I
  45  CONTINUE
      IF (J.NE.1) GO TO 46
      BOTSV(IT)=HIGH
      KFX(IT)=IH
  46  Q(IH)=1.
      SPY(J,IT)=HIGH
      IF (IH.GT.KFX(IT)) GO TO 47
      IYL(J,IT)=1
      IYT(J,IT)=0
      GO TO 48
  47  IYL(J,IT)=0
      IYT(J,IT)=1
  48  CONTINUE
  49  CONTINUE
C
C     SELECT MAXIMUM B/2 AS THE WING SEMISPAN.  IF BOTH FIRST AND
C     SECOND PLANFORMS HAVE SAME SEMISPAN THEN THE SECOND PLANFORM IS
C     TAKEN TO BE THE WING.
C
      KBOT=1
      IF (BOTSV(1).GE.BOTSV(2)) KBOT=2
      BOT=BOTSV(KBOT)
C
C     COMPUTE NOMINAL HORSESHOE VORTEX WIDTH ALONG WING SURFACE
C
      TSPAN=0
      ISAVE=KFX(KBOT)-1
      I=KFX(KBOT)-2
  50  IF (I.EQ.0) GO TO 51
      IF (TTWD(I,KBOT).EQ.TTWD(ISAVE,KBOT)) GO TO 52
  51  CTWD=COS(ATAN(TTWD(ISAVE,KBOT)))
      TLGTH=(YY(ISAVE+1,KBOT)-YY(I+1,KBOT))/CTWD
      TSPAN=TSPAN+TLGTH
      IF (I.EQ.0) GO TO 53
      ISAVE=I
  52  I=I-1
      GO TO 50

   53 TSPANA=0.
      KBIT=2
      IF (IPLAN.EQ.1) GO TO 57
      IF (KBOT.EQ.2) KBIT=1
      ISAVEA=KFX(KBIT)-1
      IA=KFX(KBIT)-2
  54  IF (IA.EQ.0) GO TO 55
      IF (TTWD(IA,KBIT).EQ.TTWD(ISAVEA,KBIT)) GO TO 56
  55  CTWDA=COS(ATAN(TTWD(ISAVEA,KBIT)))
      TLGTHA=(YY(ISAVEA+1,KBIT)-YY(IA+1,KBIT))/CTWDA
      TSPANA=TSPANA+TLGTHA
      IF (IA.EQ.0) GO TO 57
      ISAVEA=IA
  56  IA=IA-1
      GO TO 54
  57  CONTINUE
      vi = amin1(tspan,tspana)/vic
      vstol = vi/2
C
C     ELIMINATE PLANFORM BREAKPOINTS WHICH ARE WITHIN (B/2)/2000 UNITS
C     LATERALLY
C
      DO 59 IT=1,IPLAN
       N=AN(IT)
       N1=N+1
       DO 59 J=1,N
       AA=ABS(SPY(J,IT)-SPY(J+1,IT))
       IF (AA.EQ.0..OR.AA.GT.ABS(TSPAN/2000.0)) GO TO 59
       IF (AA.GT.YTOL) WRITE (iwrit,111) SPY(J+1,IT),SPY(J,IT)
       DO 58 I=1,N1
       IF (YY(I,IT).NE.SPY(J+1,IT)) GO TO 58
       YY(I,IT)=SPY(J,IT)
 58    CONTINUE
       SPY(J+1,IT)=SPY(J,IT)
 59    CONTINUE
C
C      COMPUTE Z COORDINATES
C
      DO 63 IT =1,IPLAN
       JM=AN(IT)+1.
       N1=JM
       DO 60 JZ=1,N1
 60    ZZ(JZ,IT)=RTCDHT(IT)
       JZ=1
 61    JZ=JZ+1
       IF (JZ.GT.KFX(IT)) GO TO 62
       ZZ(JZ,IT)=ZZ(JZ-1,IT)+(YY(JZ,IT)-YY(JZ-1,IT))*TTWD(JZ-1,IT)
       GO TO 61
 62    JM =JM-1
       IF (JM .EQ. KFX(IT)) GO TO 63
       ZZ(JM,IT)=ZZ(JM+1,IT)+(YY(JM,IT)-YY(JM+1,IT))*TTWD(JM,IT)
       GO TO 62
 63    CONTINUE
C
C     WRITE PLANFORM PERIMETER POINTS ACTUALLY USED IN THE COMPUTATIONS
C
      WRITE (iwrit,100)
      DO 65 IT=1,IPLAN
       N=AN(IT)
       N1=N+1
       IF (IT .EQ. 2) WRITE(iwrit,110)
       DO 64 KK = 1,N
       TOUT=ATAN(TTWD(KK,IT))*RAD
       AOUT=ATAN(AS(KK,IT))*RAD
       IF (AS(KK,IT).EQ.AZY) AOUT=90.
       WRITE(iwrit,101)KK,XX(KK,IT),YY(KK,IT),ZZ(KK,IT),AOUT,TOUT
 64    CONTINUE
       WRITE(iwrit,101) N1,XX(N1,IT),YY(N1,IT),ZZ(N1,IT)
 65    CONTINUE
C
C     PART ONE - SECTION THREE - LAY OUT YAWED HORSESHOE VORTICES
C
      STRUE=0.
      NSSWSV(1)=0
      NSSWSV(2)=0
      MSV(1)=0
      MSV(2)=0
      DO 74 IT=1,IPLAN
       N1=AN(IT)+1.
       I=0
       J=1
       YIN=BOTSV(IT)
       ILE=KFX(IT)
       ITE=KFX(IT)
C
C      DETERMINE SPANWISE BORDERS OF HORSESHOE VORTICES
C
 66    IXL=0
       IXT=0
       I=I+1
      cphi = cos(atan(ttwd(ile,it)))
      IF (YIN.GE.(SPY(J,IT)+VSTOL*cphi))GO TO 67
C
C     BORDER IS WITHIN VORTEX SPACING TOLERENCE (VSTOL) OF BREAKPOINT
C     THEREFORE USE THE NEXT BREAKPOINT INBOARD FOR THE BORDER
C
       VBORD(I)=YIN
       GO TO 70
C
C     USE NOMINAL VORTEX SPACING TO DETERMINE THE BORDER
C
 67    VBORD(I)=SPY(J,IT)
C
C     COMPUTE SUBSCRIPTS ILE AND ITE TO INDICATE WHICH
C     BREAKPOINTS ARE ADJACENT AND WHETHER THEY ARE ON THE WING
C     LEADING EDGE OR TRAILING EDGE
C
 68   IF (J.GE.N1) GO TO 69
      IF (SPY(J,IT).NE.SPY(J+1,IT))GO TO 69
      IXL=IXL+IYL(J,IT)
      IXT=IXT+IYT(J,IT)
      J=J+1
      GO TO 68
 69   YIN=SPY(J,IT)
      IXL=IXL+IYL(J,IT)
      IXT=IXT+IYT(J,IT)
      J=J+1
c----------------------------------------------
c
c 740  CPHI=COS(ATAN(TTWD(ILE,IT)))
c
c----------------------------------------------
  70  IPHI=ILE-IXL
      IF (J.GE.N1) IPHI=1
      YIN = YIN-VI*COS(ATAN(TTWD(IPHI,IT)))
      IF (I.NE.1) GO TO 72
 71   ILE=ILE-IXL
      ITE=ITE+IXT
      GO TO 66
C
C     COMPUTE COORDINATES FOR CHORDWISE ROW OF HORSESHOE VORTICES
C
 72   YQ=(VBORD(I-1)+VBORD(I))/2.0
      HW=(VBORD(I)-VBORD(I-1))/2.
      IM1=I-1+NSSWSV(1)
      ZH(IM1)=ZZ(ILE,IT)+(YQ-YY(ILE,IT))*TTWD(ILE,IT)
      PHI(IM1)=TTWD(ILE,IT)
      SSWWA(IM1)=AS(ILE,IT)
      XLE=XX(ILE,IT)+AS(ILE,IT)*(YQ-YY(ILE,IT))
      XET=XX(ITE,IT)+AS(ITE,IT)*(YQ-YY(ITE,IT))
      XLOCAL=(XLE-XET)/TBLSCW(IM1)
C
C     COMPUTE WING AREA PROJECTED TO THE X-Y PLANE
C
      STRUE=STRUE+XLOCAL*TBLSCW(IM1)*(HW*2.)*2.
C
      NSCW=TBLSCW(IM1)
      DO 73 JCW=1,NSCW
       AJCW=JCW-1
       XLEL=XLE-AJCW*XLOCAL
       NTS=JCW+MSV(1)+MSV(2)
       PN(NTS)=XLEL-.25*XLOCAL
       PV(NTS)=XLEL-.75*XLOCAL
c--------------------------------------
       PSI(NTS)=((XLE-PN(NTS))*AS(ITE,IT)+(PN(NTS)-XET)*AS(ILE,IT))/
     1 (XLE-XET)
c---------------------------------------
       S(NTS)=HW/CPHI
       Q(NTS)=YQ
 73    CONTINUE
      MSV(IT)=MSV(IT)+NSCW
C
C     TEST TO DETERMINE WHEN RING ROOT IS REACHED
C
      IF (VBORD(I) .LT. YREG(1,IT))GO TO 71
       NSSWSV(IT)=I-1
 74    CONTINUE
       M=MSV(1)+MSV(2)
C
C      COMPUTE ASPECT RATIO AND AVERAGE CHORD
C
       BOT=-BOT
       AR=4.*BOT*BOT/SREF
       ARTRUE=4.*BOT*BOT/STRUE
       CAVE=SREF/(2.*BOT)
       BETA=(1.-XMCH*XMCH)**.5
      if(icase.lt.1) write(iwrit,124) pdrg2(1),pdrg1(1),pdrg3(1)
  124 format(/12x,'a =',f6.3,5x,'clmin = ',f6.3,5x,'cd0 =',f6.4)
      if(icase.eq.1) write(iwrit,125) pdrg2(1),pdrg1(1),pdrg3(1),
     1                                pdrg2(2),pdrg1(2),pdrg3(2)
  125 format(/12x,'a =',f6.3,5x,'clmin =',f6.3,5x,'cd0 =',f6.4,3x,
     1 'on planform 1'/
     2  /12x,'a =',f6.3,5x,'clmin =',f6.3,5x,'cd0 =',f6.4,3x,
     3 'on planform 2'//) 
       WRITE(iwrit,114) M
       WRITE(iwrit,115) (IT,MSV(IT),NSSWSV(IT),IT=1,IPLAN)
       IF(SCW.NE.0.0) WRITE(iwrit,112)SCW
       IF(SCW.EQ.0.0) WRITE(iwrit,113) (TBLSCW(I),I=1,NSTA)
C
C      APPLY PRANDTL-GLAUERT CORRECTION
C
       DO 75 NV=1,M
       PSI(NV)=ATAN(PSI(NV)/BETA)
       PN(NV)=PN(NV)/BETA
 75    PV(NV)=PV(NV)/BETA
      nssw = nsswsv(1) + nsswsv(2)
      jn = 0
      do 77 jssw = 1,nssw
      chord(jssw) = 0.
      nscw = tblscw(jssw)
      do 76 jscw = 1,nscw
      jn = jn + 1
      chord(jssw) = chord(jssw)-2.*(pv(jn)-pn(jn))*beta
   76 continue
   77 xte(jssw)=(pv(jn)+(pv(jn)-pn(jn))/2.)*beta
c
      phisum = 0.0
      do 78 iky = 1,nssw
      phisum = phisum + phi(iky)
   78 continue

      iflag = 1
      if(iplan .eq. 1 .and. phisum .ne. 0.) iflag = 2
      if(iplan .eq. 2 .and. phisum .ne. 0.) go to 79
      go to 83

   79 do 81 ip = 1,iplan
      ia = 1 + (ip-1)*nsswsv(1)
      ib = nsswsv(1) + (ip-1)*nsswsv(2)
      ic = 1 - (ip-2)*nsswsv(1)
      id = nsswsv(1) - (ip-2)*nsswsv(2)
      do 80 iu = ia,ib
      do 80 iz = ic,id
      if( zh(iu) .eq. zh(iz)) go to 82
   80 continue
   81 continue

      iflag = 2
      go to 83

   82 iflag = 3
   83 continue
      read(iread,122)  xcfw,xcft,fkon,cmb,ficam,punch,crbmnt
      write(iwrit,250) xcfw,xcft,fkon,ficam,punch,crbmnt,cmb,iflag
      icam = ficam
  250 format(/10x,'xcfw  = ',f4.2,5x,'xcft   = ',f5.2,6x,'fkon   =   ',
     1  f4.2,/10x,'ficam = ',f4.2,5x,'punch  =  ',
     2  f4.2, 6x,'crbmnt =  ',f6.3/
     3        10x,'cmb   = ',f4.2,5x,'iflag  =  ',i2)
      ipunch = punch
      kon    = fkon
c
c     check tolerances
c
      if(m .gt. 400) go to 86
      nsw  = nsswsv(1) + nsswsv(2)
      if ( nsw .gt. 50) go to 85
      itsv = 0
      do 84 it = 1,iplan
      if (an(it) .le. 25.) go to 84
      write(iwrit,118) it,an(it)
      itsv = 1
   84 continue
      if(itsv .gt. 0) return
      go to 87
   85 write(iwrit,117) nsw
      return
   86 write(iwrit,116) m
      return
c
   87 aa = yreg(1,1)
      bb = yreg(1,2)
c
      call wb10(iflag)
      if (icam.ne.0) call wi20
      return
c
   93 write(iwrit,103) config
      return
   95 write(iwrit,107) ptest,qtest
      return
c
96    format (1h1//63x,'geometry data')

  97  FORMAT (/10X,A4,22HREFERENCE PLANFORM HAS,I3,7H CURVES/
     1        10X,19HROOT CHORD HEIGHT =F12.4/)
  98  FORMAT (8F10.4)
  99  FORMAT (9x,'CONFIGURATION NO.',F8.0/
     1        9x,'delta ord shift for moment = ',f10.4/)
 100  FORMAT (/8X,5HPOINT,3X,1HX,8X,1HY,8X,1HZ,7X,5HSWEEP,
     1         2X,8HDIHEDRAL/42X,5HANGLE,4X,5HANGLE)
 101  FORMAT (9X,I2,3F9.4,2F9.4,I6)
 102  FORMAT (9X,5HCURVE,I3,9H IS SWEPT,F8.4,20H DEGREES ON PLANFORM,
     1I3)
 103  FORMAT (1H1///1X,43HEND OF FILE ENCOUNTERED AFTER CONFIGURATION,
     1        F7.0)
 105  FORMAT (6F5.3,2F10.6)
 106  FORMAT (10x,I3,2F9.4,2F10.5,4X,I4)
 107  FORMAT (1H1///1X,38HERROR - PROGRAM CANNOT PROCESS PTEST =,F5.1,
     112H AND QTEST =,F5.1)
 108  FORMAT (/10X,35HBREAK POINTS FOR THIS CONFIGURATION )
 109  FORMAT (10X,5HPOINT,3X,1HX,8X,1HY,7X,5HSWEEP,4X,8HDIHEDRAL,
     1 /17X,3HREF,6X,3HREF,6X,5HANGLE,5X,5HANGLE)
 110  FORMAT (10X,'SECOND PLANFORM BREAK POINTS')
 111  FORMAT (////25X,34HTHE BREAKPOINT LOCATED SPANWISE AT,F11.5,3X,20H
     1HAS BEEN ADJUSTED TO,F9.5////)
 112  FORMAT (/10X,F5.0,41H HORSESHOE VORTICES IN EACH CHORDWISE ROW)
 113  FORMAT (/23X,98HTABLE OF HORSESHOE VORTICES IN EACH CHORDWISE ROW
     1(FROM TIP TO ROOT BEGINNING WITH FIRST PLANFORM)//25F5.0/25F5.0)
 114  FORMAT (/10x,I5,' HORSESHOE VORTICES USED'
     1/11X,'PLANFORM     TOTAL        SPANWISE')
 115  FORMAT (11x,I4,10X,I3,10X,I4)
 116  format(1h1//18x,i6,' horseshoe vortices laidout.'/10X,' This is ',
     1 'more than the 400 maximum. This configuration is aborted.')
 117  format(1h1//10x,i6,' row of horseshoe vortices laidout. This'
     1 ' is more than the 50 maximum. This configuration is aborted.') 
 118  format(1h1//10x,'PLanform ',i6,' has',i6,' breakpoints. This is '
     1 'more than the 25 maximum. This configuration is aborted.')
 122  format(8f10.4)
      end

      subroutine simeq(a,n,b,m,determ,ipivot,nmax,iscale)
c
c     solution of simultaneous linear equations
c
      dimension ipivot(n), a(nmax,n), b(nmax,m)
      equivalence (irow,jrow), (icolum,jcolum), (amax,t,swap)
c
      double precision pivot,t,pivoti,r1,r2
c     initialization
c
1     iscale = 0
      r1 = 10.0**30
      r2 = 1.0/r1
      determ = 1.0
      do 2 j = 1,n
2     ipivot(j) = 0
      do 38 i = 1,n
c
c     search for pivot element
c
      amax = 0.0
      do 7 j = 1,n
      if(ipivot(j) - 1) 3,7,3
3     do 6 k = 1,n
      if(ipivot(k) - 1) 4,6,39
4     if (abs(amax) - abs(a(j,k))) 5,6,6
5     irow = j
      icolum = k
      amax = a(j,k)
6     continue
7     continue
      if (amax) 9,8,9
8     determ = 0.0
      iscale = 0
      go to 39
9     ipivot(icolum) = ipivot(icolum) + 1
c
c     interchange rows to put pivot element on diagonal
c
      if(irow - icolum) 10,14,10
10    determ = -determ
      do 11 l = 1,n
      swap = a(irow,l)
      a(irow,l) = a(icolum,l)
11    a(icolum,l) = swap
      if (m) 14,14,12
12    do 13 l = 1,m
      swap = b(irow,l)
      b(irow,l) = b(icolum,l)
13    b(icolum,l) = swap
14    pivot = a(icolum,icolum)
      if (pivot) 15,8,15
c
c      scale the determinant
c
15    pivoti = pivot
      if (abs(determ) - r1) 18,16,16
16    determ = determ/r1
      iscale = iscale + 1
      if (abs(determ) - r1) 21,17,17
17    determ = determ/r1
      iscale = iscale + 1
      go to 21
18    if (abs(determ) - r2) 19,19,21
19    determ = determ*r1
      iscale = iscale - 1
      if (abs(determ) - r2) 20,20,21
20    determ = determ*r1
      iscale = iscale -1
21    if (dabs(pivoti) - r1) 24,22,22
22    pivoti = pivoti/r1
      iscale = iscale + 1
      if (dabs(pivoti) - r1) 27,23,23
23    pivoti = pivoti/r1
      iscale = iscale + 1
      go to 27
24    if (dabs(pivoti) - r2) 25,27,27
25    pivoti = pivoti*r1
      iscale = iscale - 1
      if (dabs(pivoti) - r2) 26,26,27
26    pivoti = pivoti*r1
      iscale = iscale - 1
27    determ = determ*pivoti
c
c     divide pivot row by pivot element
c
      do 29 l = 1,n
      if(ipivot(l) - 1) 28,29,39
28    a(icolum,l) = a(icolum,l)/pivot
29    continue
      if (m) 32,32,30
30    do 31 l = 1,m
31    b(icolum,l) = b(icolum,l)/pivot
c
c     reduce non-pivot rows
c
32    do 38 l1 = 1,n
      if (l1 - icolum) 33,38,33
33    t = a(l1,icolum)
      do 35 l = 1,n
      if (ipivot(l) - 1) 34,35,39
34    a(l1,l) = a(l1,l) - a(icolum,l)*t
35    continue
      if (m) 38,38,36
36    do 37 l = 1,m
37    b(l1,l) = b(l1,l) - b(icolum,l)*t
38    continue
39    return
      end


      subroutine dragsu (r,a,y,z,s,xis,xjs,wnk)
      zp     = z + s*sin(a)
      yp     = y + s*cos(a)
      zm     = z - s*sin(a)
      ym     = y - s*cos(a)
      rl     = sqrt(zp**2 + yp**2)
      rr     = sqrt(zm**2 + ym**2)
      zpoyp  = zp/yp
      zmoym  = zm/ym
      philtl = atan(zpoyp)
      phirtl = atan(zmoym)
      plmpi  = philtl - r
      prmpi  = phirtl - r
      cospli = cos(plmpi)
      cospri = cos(prmpi)
      wnk    = xis*cospli/rl - xjs*cospri/rr
      return
      end

      subroutine giasos(iop,md,nd,m,n,a,nos,b,iac,q,v,irank,aplus,ierr)
c
      logical withu,withv
      dimension a(md,n),v(nd,n),q(n),e(256),b(md,nos),aplus(nd,m)
c
      tol = 1.0e-16
      size = 0.0
      np1 = n + 1
c
c      compute the e-norm of matrix a as zero test for singular values
c
      sum = 0.0
      do 500 i = 1,m
      do 500 j = 1,n
  500 sum = sum + a(i,j)**2
      ztest = sqrt(sum)
      if( iac .gt. 13) go to 505
      ztest = ztest*10.**(-iac)
      go to 510
  505 ztest = ztest * 2.0**(-48)
      ztest = sqrt(sum)*2.0**(-48)
c
  510 if (iop .ne. 1) go to 515
      withu = .false.
      withv = .false.
      go to 520
  515 withu = .true.
      withv = .true.
  520 continue
      g = 0.0
      x = 0.0
      do 30 i = 1,n
c
c      householder reduction to bidiagonal form
c
      e(i) = g
      s = 0.0
      l = i + 1
c
c      annihilate the i-th column below diagonal
c
      do 3 j = i,m
    3 s = s + a(j,i)**2
      g = 0.0
      if(s .lt. tol) go to 10
      g = sqrt(s)
      f = a(i,i)
      if(f .ge. 0.0) g = -g
      h = f*g - s
      a(i,i) = f - g
      if(i .eq. n) go to 10
       do 9 j = l,n
       s = 0.0
      do 7 k = i,m
    7 s = s + a(k,i)*a(k,j)
      f = s/h
      do 8 k = i,m
    8 a(k,j) = a(k,j) + f*a(k,i)
    9 continue
   10 q(i) = g
      if ( i .eq. n) go to 20
c
c     annihilate the i-th row to right of super-diag.
c
      s = 0.0
      do 11 j = l,n
   11 s = s + a(i,j)**2
      g = 0.0
      if(s .lt. tol) go to 20
      g = sqrt(s)
      f = a(i,i+1)
      if(f .ge. 0.0) g = -g
      h = f*g - s
      a(i,i+1) = f - g
      do 15 j = l,n
   15 e(j) = a(i,j)/h
      do 19 j = l,m
      s = 0.0
      do 16 k = l,n
   16 s = s + a(j,k) * a(i,k)
      do 17 k = l,n
   17 a(j,k) = a(j,k) + s*e(k)
   19 continue
   20 y = abs(q(i)) + abs(e(i))
      if(y .gt. size) size = y
   30 continue
      if( .not. withv) go to 41
c
c      accumulation of right transformations
c
      do 40 ii = 1,n
      i = np1 - ii
      if(i .eq. n) go to 39
      if(g .eq. 0.0) go to 37
      h = a(i,i+1)*g
      do 32 j = l,n
   32 v(j,i) = a(i,j)/h
      do 36 j = l,n
      s = 0.0
      do 33 k = l,n
   33 s = s + a(i,k)*v(k,j)
      do 34 k = l,n
   34 v(k,j) = v(k,j) + s*v(k,i)
   36 continue
   37 do 38 j = l,n
      v(i,j) = 0.0
   38 v(j,i) = 0.0
   39 v(i,i) = 1.0
      g = e(i)
   40 l = i
   41 continue
      if(.not. withu) go to 53
c
c      accumulation of left transformations
c
      do 52 ii = 1,n
      i = np1 - ii
      l = i + 1
      g = q(i)
      if(i .eq. n) go to 43
      do 42 j = l,n
   42 a(i,j) = 0.0
   43 continue
      if(g .eq. 0.0) go to 49
      if(i .eq. n) go to 47
      h = a(i,i)*g
      do 46 j = l,n
      s = 0.0
      do 44 k = l,m
   44 s = s + a(k,i)*a(k,j)
      f = s/h
      do 45 k = i,m
   45 a(k,j) = a(k,j) + f*a(k,i)
   46 continue
   47 do 48 j = i,m
   48 a(j,i) = a(j,i)/g
      go to 51
   49 do 50 j = i,m
   50 a(j,i) = 0.0
   51 a(i,i) = a(i,i) + 1.0
   52 continue
   53 continue
c
c     diagonalization of bidiagonal form
c
      do 100 kk = 1,n
      k = np1 - kk
      itcnt = 0
      kp1 = k + 1
c
c      test f splitting
c
   59 continue
      do 60 ll=1,k
      l = kp1 - ll
      if((size + abs(e(l))) .eq. size) go to 64
      lm1 = l - 1
      if((size + abs(q(lm1))) .eq. size) go to 61
   60 continue
c
c     cancellation of e(l) if l .gt. 1.
c
   61 c = 0.0
      s = 1.0
      l1 = l - 1
      do 63 i = l,k
      f = s*e(i)
      e(i) = c*e(i)
      if((size + abs(f)) .eq. size) go to 64
      g = q(i)
      q(i) = sqrt(f*f + g*g)
      h = q(i)
      c = g/h
      s = -f/h
      if(.not. withu) go to 63
      do 62 j = 1,m
      y = a(j,l1)
      z = a(j,i)
      a(j,l1) = y*c + z*s
      a(j,i) = -y*s + z*c
   62 continue
c
   63 continue
c
c     test f convergence
c
   64 z = q(k)
      if(l .eq. k) go to 75
      if(itcnt .le. 30) go to 65
      ierr = kk
      return
   65 itcnt = itcnt + 1
c
c     shift from lower 2x2
c
      x = q(l)
      y = q(k-1)
      g = e(k-1)
      h = e(k)
      f = ((y-z)*(y+z)+(g-h)*(g+h))/(2.0*h*y)
      g = sqrt(f*f + 1.0)
      if(f .lt. 0.0) g = -g
      f = ((x-z)*(x+z)+h*(y/(f+g)-h))/x
c
c     next qr transformation
c
      c = 1.0
      s = 1.0
      lp1 = l + 1
      do 73 i = lp1,k
      g = e(i)
      y = q(i)
      h = s*g
      g = c*g
      z = sqrt(f*f + h*h)
      e(i-1) = z
      c = f/z
      s = h/z
      f = x*c + g*s
      h = y*s
      y = y*c
      if(.not. withv) go to 70
      do 68 j = 1,n
      x = v(j,i-1)
      z = v(j,i)
      v(j,i-1) = x*c + z*s
      v(j,i)  = -x*s + z*c
   68 continue
c
   70 z = sqrt(f*f + h*h)
      q(i-1) = z
      c = f/z
      s = h/z
      f = c*g + s*y
      x = -s*g + c*y
      if(.not. withu) go to 73
      do 72 j = 1,m
      y = a(j,i-1)
      z = a(j,i)
      a(j,i-1) = y*c + z*s
      a(j,i) = -y*s + z*c
   72 continue
c
   73 e(l) = 0.0
      e(k) = f
      q(k) = x
      go to 59
c
c      convergence
c
   75 continue
      if(z .eq. 0.0) go to 100
      q(k) = -z
      if(.not. withv) go to 100
      do 76 j = 1,n
   76 v(j,k) = -v(j,k)
  100 continue
c
      ierr = 0
      do 280 ii = 2,n
      i = ii - 1
      k = i
      p = q(i)
c
      do 250 j = ii,n
      if(q(j) .le. p) go to 250
      k = j
      p = q(j)
  250 continue
c
      if(k .eq. i) go to 280
      q(k) = q(i)
      q(i) = p
c
      if(iop .eq. 1) go to 280
c
      do 260 j = 1,n
      p = v(j,i)
      v(j,i) = v(j,k)
      v(j,k) = p
  260 continue
c
      do 270 j = 1,m
      p = a(j,i)
      a(j,i) = a(j,k)
      a(j,k) = p
  270 continue
c
  280 continue
c
      j = n
  290 if(q(j) .gt. ztest) go to 300
      q(j) = 0.0
      j = j - 1
      go to 290
  300 irank = j
      temp = ztest/q(j)
      if(temp .gt. 0.0625) ierr = -1
c
      if(iop .lt. 3) return
      if(iop .gt. 3) go to 170
      do 160 l = 1,nos
      do 130 j = 1,irank
      sum = 0.0
      do 120 i = 1,m
  120 sum = sum + a(i,j)*b(i,l)
  130 e(j) = sum/q(j)
c
      do 150 k = 1,n
      sum = 0.0
      do 140 i = 1,irank
  140 sum = sum + v(k,i)*e(i)
  150 b(k,l) = sum
  160 continue
      return
  170 do 200 j = 1,m
      do 190 i = 1,n
      sum = 0.0
      do 180 k = 1,irank
  180 sum = sum + v(i,k)*a(j,k)/q(k)
  190 aplus(i,j) = sum
  200 continue
c
      if(iop .eq. 4) return
      do 230 k = 1,nos
      do 220 i = 1,n
      sum = 0.0
      do 210 j = 1,m
  210 sum = sum + aplus(i,j)*b(j,k)
  220 e(i) = sum
      do 225 i = 1,n
  225 b(i,k) = e(i)
  230 continue
      return
      end

      subroutine wb10(iflag)
c
c      program circul2
c
      dimension a0(2),b0(2),a1(2),b1(2),c1(2),d1(2),isum(2),isump(2),
     1 isump2(2),ppp(100),wn(2),yy(2),zzh(50),zhh(100),yb(50),
     2 y(100),pphi(50),xtt(50),xta(103),chd(100),a(103,103),cdrag(103),
     3 spnsta(100),ipivot(103),nma(2),yq(100),yqq(50),yc(100)
      common /all/bot,m,beta,ptest,qtest,tblscw(50),q(400),pn(400),
     1            pv(400),s(400),psi(400),phi(50),zh(50),nssw
      common /inout/iwrit,iread,iecho,ioutwr,iplfit
      common /onetre/twist(2),cref,sref,cave,cldes,strue,ar,artrue,
     1               rtcdht(2), config, nsswsv(2), msv(2), kbot,
     2               plan,iplan,xmch,sswwa(50),xcfw,xcft,yreg(1,2)
      common /tothre/cir(400)
      common /ccrrdd/chord(50),xte(50),kbit,tspan,tspana
      common /opt/kon,cmb,ipunch,case,spnklu,xitmax,epsmax,crbmnt
      common /pdrag/pdrg1(2),pdrg2(2),pdrg3(2),tdklue,nqcdp1(5),
     1        qclp1(50,5),qcdp1(50,5),nqcdp2(5),qclp2(50,5),
     2        qcdp2(50,5),nspolr(2)
      dimension aasava(103,103),spanld(100),chordl(100),cl(100),
     1 cdpp(100),v(103,103),qcl(50),qcd(50),clold(100),clhist(50),
     2 epshst(50),cdihst(50),cdphst(50),cpc2(100),clmin(100),
     3 cloccl(100),cdrgsu(103),qy(50),qcclca(50),cdrago(100),
     4 aiipo(100),isnpts(2),yspnpt(50,2),clspnp(50,2),xlocal(50),
     5 ylocal(50),cd0x(100),ppq(100),cdragp(103),clsol(2),
     6 cmpsol(2),cmbsol(2) 
      character*80 title
c
      tolc = (bot*15.e-05)**2
c
      iplfit = 1
c
      cl1    = 0.0
      cl2    = 0.0
      crbm1  = 0.0
      crbm2  = 0.0
      cm1    = 0.0
      cm2    = 0.0
      cd1    = 0.0
      cd2    = 0.0
      nma(1) = 0
      nma(2) = 0
      pi     = 4.*atan(1.)
      rad    = 180./pi
c
c      itdkl is the klueto determine the type of optimization that
c      is performed
c
c      = 1   induced + pressure
c      = 0   induced only
c
      itdkl  = tdklue
      ispnkl = spnklu
      icase  = case
c
      read(iread,402) relax,fioutw,cd0,firbm,yrbm,zrbm
      write(iwrit,430) relax,fioutw,cd0,firbm,yrbm,zrbm
      ioutwr = fioutw
      irbm   = firbm
  430 format(/10x,'relax = ',f4.2,5x,'fioutw =  ',f4.2,6x,
     1            'cd0    =   ',f6.4/
     2        10x,'firbm = ',f4.2,5x,'yrbm   = ',f7.4,4x,
     3            'zrbm   = ',f8.4/)
c
      if(icase .lt. 2) go to 412
c
c     read in drag polar
c
  401 format(a80)
  402 format(6f10.5)
  406 format(/10x,a80//10x,'there are ',f4.1,' polars on this surface'/)
  408 format(10x,f5.1,3x,'points this polar',2x,'planform',i2//
     1 19x,'qcl',6x,'qcd')
c
      iplx = 1
      fplors = 1.0
      if(icase .ge. 3) iplx = iplan
      do 410 i = 1,iplx
      read(iread,401) title
      if(icase.gt.3) read(iread,402) fplors
      nspolr(i)=fplors
      nnnp = fplors
      write(iwrit,406) title,fplors
      do 410 j = 1,nnnp
      read(iread,402) fnclcd
      if(i.eq.1) nqcdp1(j) = fnclcd
      if(i.eq.2) nqcdp2(j) = fnclcd
      nclcd = fnclcd
      write(iwrit,408) fnclcd,i
      do 410 ii = 1,nclcd
      read(iread,402) fqcl,fqcd
      if(i .eq. 1) qclp1(ii,j) = fqcl
      if(i .eq. 1) qcdp1(ii,j) = fqcd
      if(i .eq. 2) qclp2(ii,j) = fqcl
      if(i .eq. 2) qcdp2(ii,j) = fqcd
  410 write(iwrit,420) fqcl,fqcd
c
c
c
  412 continue
  420 format(13x,3f10.4)
c
      if(ispnkl .ne. 1) go to 418
c
c     read in spanload
c
      read(iread,401) title
      do 416 i = 1,iplan
      read(iread,402) fspnpt
      ilimit = fspnpt
      isnpts(i) = ilimit
      write(iwrit,414) ilimit,i
      do 416 j = 1,ilimit
      read(iread,402)  yspnpt(j,i),clspnp(j,i)
  416 write(iwrit,420) yspnpt(j,i),clspnp(j,i)
c
  414 format(/5x,i3,' input pairs for spanload on planform ',i2/
     1 /2x,' yspnpt(j,i)',4x,'clspnp(j,i)'/)
c
c
  418 continue
c
      botl = abs(tspan)
      bol  = abs(tspana)
      snn  = amax1(botl,bol)/100.
      deltyb = 2.*snn
      nma(kbot) = botl/deltyb+0.1
      nma(kbit) = bol/deltyb+0.1
      nmax = nma(1) + nma(2)
      lm = nmax
      il = lm + 1
c     jm=lm+2
c     if( lm .eq. nma(1) .or. kon .eq. 1) jm = lm + 1
c     if( kon .ne. 1) jm = lm + 2
c     im = lm + 2
      imp = lm + 2
      imb = lm + 3
      if(kon .eq. 2) imb = lm + 2
      im = imb
      jm = imb
      if( kon .eq.1) jm = lm + 1
      if( kon .eq.0) jm = lm + 2
      if( kon .eq.2) imp = lm + 3
c
      write(iwrit,310) lm,il,jm,im,tspan,tspana,botl,bol,snn,deltyb,
     1 nma(kbot),kbot,nma(kbit),kbit
  310 format(/3x,'LM =',i3,2x,'IL =',i3,2x,'JM =',i3,2x,'IM =',i3,2x,
     1        'TSPAN =',f8.3,4x,'TSPANA =',f8.3/3x,'BOTL = ',f7.3,4x,
     2        'BOL  =',f7.3,5x,'SNN   = ',f7.4,4x,'DELTYB =',f8.4/3x,
     3        'NMA(KBOT) = ',i2,4x, 'KBOT =',i2,10x,'NMA(KBIT) = ',i2,
     4        5x,'KBIT   = ',i2/)
c
c     setup right hand side of ax=b
c
      do 111 i = 1,lm
      clold(i) = 0.0
      cpc2(i)  = pdrg2(1)
      clmin(i) = pdrg1(1)
      cd0x(i)  = pdrg3(1)
      if(icase .eq. 1 .and. i .gt. nma(kbit)) cpc2(i)  = pdrg2(2) 
      if(icase .eq. 1 .and. i .gt. nma(kbit)) clmin(i) = pdrg1(2)
      if(icase .eq. 1 .and. i .gt. nma(kbit)) cd0x(i)  = pdrg3(2)
      cdrago(i) = 0.0
  111 aiipo(i)  = 0.0
      do 1   i  = 1,im
      do 1   j  = 1,im
      cdrag(i)  = 0.0
    1 a(i,j)    = 0.0
      cdrag(il) = cldes
c
c       these are the constraints
c
      cdrag(imp) = cmb
      cdrag(imb) = crbmnt
c
      scwmin  = 20.
      do 2  i = 1,nssw
    2 scwmin  = amin1(scwmin,tblscw(i))
      nscwmi  = scwmin
      ii      = 1
      do 13 i = 1,iplan
      ib      = nsswsv(i)
      ic      = msv(1) + (i-1)*msv(2)
      id      = ic + 1
      iz      = nsswsv(1) + (i-1)*nsswsv(2)
      d       = xcfw
      if ( i .eq. 2) d = xcft
c
c      compute i of eqn. 12
c
      ai   = nscwmi*d+0.75
      imax = int(ai)
c
      if( d .eq. 1.) go to 3
      b0(i) = -1./(nscwmi*(1.-d))
      a0(i) = imax - b0(i)*(nscwmi+0.75)*(nscwmi-imax)
      go to 4
    3 b0(i) = 0.
      a0(i) = imax
    4 isum(i)   = 0
      isump(i)  = 0
      isump2(i) = 0
      if(imax .eq. 0) go to 6
      do 5 in = 1,imax
    5 isum(i) = isum(i) + in
    6 imm     = imax + 1
      if(imm .gt. nscwmi) go to 8
c
c     isump     = sum i    isump2 = sum i*i
      do 7   in = imm,nscwmi
      isump(i)  = isump(i) + in
    7 isump2(i) = isump2(i) + in**2
    8 iamm      = nma(i)
      iuz       = nsswsv(i)
      ycat      = yreg(1,i)
      do 10   j = 1,iuz
      jj        = j + (i - 1)*nsswsv(1)
      zzh(j)    = zh(jj)
      pphi(j)   = phi(jj)
      xtt(j)    = xte(jj)
      cir(j)    = chord(jj)
      yqq(j)    = q(ii)
      ii        = ii + tblscw(jj)
      ie        = ib - j + 1
      itl       = tblscw(iz)
      id        = id - itl
      ia        = id + itl
      if(ia .gt. ic) ycat = ycat - s(id)
      if(ia .gt. ic) go to 9
      ycat      = ycat - s(id) - s(ia)
    9 iz        = iz - 1
      yb(ie)    = ycat
   10 continue
      do 11   j = 1,iuz
      jj        = j + (i-1)*nsswsv(1)
      yc(jj)    = yb(j)
   11 continue
      yob       = -nma(i)*2.*snn - snn + yreg(1,i)
      do 12   k = 1,iamm
      kk        = k + (i-1)*nma(1)
      yob       = yob + deltyb
      y(kk)     = yob
      call ftlup (yob,yq(kk),+1,iuz,yb,yqq,50,50)
      call ftlup (yob,xta(kk),+1,iuz,yb,xtt,50,50)
      call ftlup (yob,chd(kk),+1,iuz,yb,cir,50,50)
      call ftlup (yob,ppp(kk),+1,iuz,yb,pphi,50,50)
      call ftlup (yob,zhh(kk),+1,iuz,yb,zzh,50,50)
      b1(i)     = -chd(kk)/nscwmi
      a1(i)     =  ((xta(kk) + chd(kk)) - 0.75*b1(i))*a0(i)
      c1(i)     =  b0(i)*(xta(kk) + 2.*chd(kk) - 1.5*b1(i))
      d1(i)     =  b1(i)*b0(i)
c
c     the factor 8 is used instead of the factor 4 to take into
c     account both sides of the wing
c
      ppq(kk)   = cos(atan(ppp(kk)))
      cnnsta    = 8.*snn*ppq(kk)/sref
c
c     eqn 7    com. with 13
c
      a(kk,il)  = cnnsta*(a0(i) + b0(i)*isump(i))
c
c     eqn 8    com. with 14
c
      a(kk,imp) = cnnsta/cref*(a1(i) + b1(i)*isum(i) + 
     1            c1(i)*isump(i) + d1(i)*isump2(i))
      if(yq(kk) .lt. yrbm .and. i .ge. irbm) a(kk,imb) =
     1 .5*a(kk,il)*((yq(kk)-yrbm)+(zhh(kk)-zrbm)*ppp(kk))/bot
      if(i .eq. 2 .and. irbm .eq. 1) a(kk,imb) = 0.0
   12 continue
   13 continue
c
c
      do 14   k = 1,lm
      a(il,k)   = a(k,il)
      a(imp,k)  = a(k,imp)
      a(imb,k)  = a(k,imb)
   14 continue
c
c
c     the A-matrix stands for the drag matrix -cdv-
c
c
      do 17   i = 1,lm
      rphi      = atan(ppp(i))
      csr       = a(i,il)*sref/(8.*snn*cos(rphi))
      do 17   j = 1,lm
      sphi      = atan(ppp(j))
      css       = a(j,il)*sref/(8.*snn*cos(sphi))
      yy(1)     = yq(i) - yq(j)
      yy(2)     = yq(i) + yq(j)
      zz        = zhh(i) - zhh(j)
      do 16   k = 1,2
      xisgn     = 1.
      xjsgn     = 1.
      if(k .eq. 2) go to 15
      if(yy(1) .lt. tolc) xjsgn = -1.
      if(yy(1) .lt. (-tolc)) xisgn = -1.
   15 yyy       = yy(k)
c
c     bracketed term in (15) when called twice
c
      call dragsu(rphi,sphi,yyy,zz,snn,xisgn,xjsgn,wn(k))
      sphi      = -sphi
   16 continue
c
c     eqn. 15
c
      a(i,j)    = snn*csr*css*(wn(1)-wn(2))/(pi*sref)
   17 continue
c
      do 100    i = 1,jm
      do 100    j = 1,jm
  100 aasava(i,j) = a(i,j)
      do 101    i = 1,im
  101 cdrgsu(i)   = cdrag(i)
c
c     commence iteration loop for pressure drag
c
      if(icase  .le. 1) iplfit =   0
      if(icase  .lt. 2) xitmax = 1.0
      if(ispnkl .eq. 1) xitmax = 1.0
      if(itdkl  .eq. 0) xitmax = 1.0
c
      relaxo       = relax
      itmax        = xitmax
      do 500 iiter = 1,itmax
      relax = relaxo
      if(iiter .eq. 1 .or. iiter .gt. 50) relax = 1.0
c
c     cd,ik+cd,ki     see eqn 24
c
      do 19   i = 1,lm
      do 18   j = 1,lm
   18 xta(j)    = aasava(i,j) + aasava(j,i)
      do 110 ik = 1,lm
  110 a(ik,i)   = xta(ik)
   19 continue
c
c     add pressure drag to scheme
c
  144 format(12x,'A=',f6.4,5x,'CLMIN=',f6.4/)
c
      do 140 i = 1,lm
      afact    = sref/deltyb/chd(i)
c
      if(iplfit .eq. 0 .or. icase .lt. 2) 
     1 aiip = itdkl*afact*aasava(i,il)**2*cpc2(i)/2.0
      if(iplfit .eq. 1 .and. icase .ge. 2) aiip = 0.0
c
      aiiad = relax*aiip + (1.0 - relax)*aiipo(i)
c
      if(iplfit .eq. 0 .or. icase .lt. 2)  
     1 cdragn = itdkl*cpc2(i)*clmin(i)*aasava(i,il)
      if(iplfit .eq. 1 .and. icase .ge. 2)  
     1 cdragn = -itdkl*cpc2(i)/2.0*aasava(i,il)
c
      cdraga = relax*cdragn + (1.0 - relax)*cdrago(i)
c
      a(i,i) = a(i,i) + aiiad
      cdragp(i) = cdrag(i)
      cdrag(i)  = cdrgsu(i) + cdraga
c
      aiipo(i) = aiiad
      cdrago(i) = cdraga
c
  140 continue
c
      cdragp(lm+1) = cdrag(lm+1)
      cdragp(lm+2) = cdrag(lm+2)
c
      cdrag(lm+1) = cdrgsu(lm+1)
      cdrag(lm+2) = cdrgsu(lm+2)
      cdrag(lm+3) = cdrgsu(lm+3) 
c
      if(ispnkl .eq. 1) go to 210
      if(iiter .gt. 100) go to 142
c
      if(iflag.le.2) call simeq(a,jm,cdrag,1,determ,ipivot,103,iscale)
      if(iflag.eq.3) call giasos(3,103,103,jm,jm,a,1,cdrag,15,xta,v,
     1                           irank,ap,ierr)
      if(iflag.eq.3) write(iwrit,220) irank,ierr
  220 format(/10x,'irank = ',i4,10x,'ierr = ',i4//)
c
      go to 143
  142 call xitimp(a,cdragp,cdrag,jm)
  143 continue
c
  210 continue
      do 120 i = 1,jm
      do 120 j = 1,jm
  120 a(i,j) = aasava(i,j)
c
c
c        if spanload is read in, redefine cdrag
c
      if(ispnkl .ne. 1) go to 230
c
      i = 0
      afact = 2.0*cave*deltyb/sref
      do 234 iplx = 1,iplan
      nlocal = isnpts(iplx)
      do 228 j = 1,nlocal
      xlocal(j) = yspnpt(j,iplx)
  228 ylocal(j) = clspnp(j,iplx)
      nqloc = nma(iplx)
      do 234 ii = 1,nqloc
      i = i + 1
      yminus = -y(i)
      call ftlup(yminus,spldx,1,nlocal,xlocal,ylocal,50,50)
  234 cdrag(i) = spldx*afact/aasava(i,il)
c
  230 continue
c
c     eqn 27   cd = seum sum 2*cdrag(i)*cdrag(j)*a(i,j)
c
      if(ioutwr .eq. 0) write(iwrit,330)
  330 format(8x,'i',5x,'cll',7x,'cdpt',5x,'cdloc',5x,'cloccl',5x,
     1 'clloc',4x,'cdrag(i)',2x,'a(i,il)',3x,'chd(i)',5x,'y(i)'/)
      cd = 0.0
      cll = 0.0
      eps = 0.0
      cdpt = 0.0
      i = 0
c
      do 22 iplx = 1,iplan
      if(icase .lt. 2) go to 772
      if(icase .gt. 2) go to 770
      nclcd = nqcdp1(1)
      do 768 iq = 1,nclcd
      qcl(iq) = qclp1(iq,1)
  768 qcd(iq) = qcdp1(iq,1)
      go to 772
  770 continue
      if(iplx .eq. 1) nclcd = nqcdp1(1)
      if(iplx .eq. 2) nclcd = nqcdp2(1)
      do 771 iq = 1,nclcd
      if(iplx .eq. 1) qcl(iq) = qclp1(iq,1)
      if(iplx .eq. 1) qcd(iq) = qcdp1(iq,1)
      if(iplx .eq. 2) qcl(iq) = qclp2(iq,1)
      if(iplx .eq. 2) qcd(iq) = qcdp2(iq,1)
  771 continue
  772 continue
c
c
      nqloc = nma(iplx)
      do 22 ii = 1,nqloc
      i = i + 1
      cloccl(i) = cdrag(i)*a(i,il)*sref/2.0/deltyb/chd(i)
      clxx = cloccl(i)/ppq(i)
      if(icase .le. 1) cdloc = cpc2(i)*(clxx-clmin(i))**2+cd0x(i)
      if(icase .ge. 2) call ftlup(clxx,cdloc,1,nclcd,qcl,qcd,50,50)
      cll = cll + cdrag(i)*a(i,il)
      clloc = cdrag(i)*a(i,il)
      cdpt = cdpt + 2.0/sref*cdloc*chd(i)*deltyb
      do 21 j = 1,lm
   21 cd = cd + cdrag(i)*a(i,j)*cdrag(j)*2.
c
      eps = eps + abs(cll - clold(i))
      clold(i) = cll
c
      if(ioutwr .eq. 1) go to 22
      write(iwrit,320) i,cll,cdpt,cdloc,cloccl(i),clloc,cdrag(i),
     1                 a(i,il),chd(i),y(i)
  320 format(5x,i4,10f10.4)
   22 continue
      write(6,130) cd,cdpt
  130 format(12x,'induced drag cd =',f8.5,5x,
     1           'pressure drag cdpt =',f8.5/)
c
      if(itmax .eq. 1) go to 530
      call qdrag(cloccl,cpc2,clmin,nclcd,qcl,qcd,lm,iplan,icase,nma,ppq)
      epshst(iiter) = eps
      clhist(iiter) = cll
      cdihst(iiter) = cd
      cdphst(iiter) = cdpt
      iacitr = iiter
      if(eps .lt. epsmax) go to 510
  500 continue
  510 if(eps .le. epsmax) write(iwrit,550)
      if(eps .gt. epsmax) write(iwrit,552)
      write(iwrit,554)
      do 520 i = 1,iacitr
      cdt = cdihst(i) + cdphst(i)
      write(iwrit,556) i,epshst(i),clhist(i),cdihst(i),cdphst(i),cdt
  520 continue
  530 continue
c
  550 format(/10x,'pressure drag iteration has converged '/)
  552 format(/10x,'pressure drag iteration did not converge',
     1       ' in allowed iterations '/)
  554 format(15x,'k',5x,'eps',7x,'cl',8x,'cdi',7x,'cdp',5x,'cdi+cdp')
  556 format(11x,i5,6f10.5)
c
c     interpolate back to the lattice grid
c
      clsol(1)  = 0.0
      clsol(2)  = 0.0
      cmpsol(1) = 0.0
      cmpsol(2) = 0.0
      cmbsol(1) = 0.0
      cmbsol(2) = 0.0
c
      do 26 i = 1,iplan
      iuz = nma(i)
      do 24 j = 1,iuz
      jj = j + (i-1)*nma(1)
      zzh(j) = y(jj)
      xtt(j) = cdrag(jj)
c
c     calculate constraint values from matrix coefficients
c
      clsol(i)  = clsol(i)  + a(jj,il)  * cdrag(jj)
      cmpsol(i) = cmpsol(i) + a(jj,imp) * cdrag(jj)
      cmbsol(i) = cmbsol(i) + a(jj,imb) * cdrag(jj)
   24 continue
      iuu = nsswsv(i)
      do 25 j = 1,iuu
      jj = j + (i - 1)*nsswsv(1)
      call ftlup(yc(jj),ppp(jj),+1,iuz,zzh,xtt,50,50)
   25 continue
   26 continue

      jk = 0
      do 28 i = 1,iplan
      ka = 1 + (i-1)*nsswsv(1)
      kb = nsswsv(1) + (i-1)*nsswsv(2)
      d  = xcfw
      if(i .eq. 2) d = xcft
      do 27 j = ka,kb
      nscw = tblscw(j)
      ai   = nscw*d + 0.75
      imax = int(ai)
      do 27 k = 1,nscw
      jk = jk + 1
      e = 1.
      if(k .gt. imax) e = (1. - (k - 0.75)/nscw)/(1.0 - d)
      cir(jk) = ppp(j)*e
   27 continue
   28 continue
      write(iwrit,46)
      if(itdkl .eq. 1 .and. ispnkl .ne. 1) write(iwrit,47)
      if(itdkl .eq. 0 .and. ispnkl .ne. 1) write(iwrit,48)
      if(ispnkl .eq. 1) write(iwrit,52)
      write(iwrit,35) cref,cave,strue,sref,bot,ar,artrue,xmch
      write(iwrit,60) clsol(1), cmpsol(1),cmbsol(1)
   60 format(/10x,'first  planform',
     1        3x,'cl =',f10.5,2x,'cm =',f10.5,2x,'cb =',f10.5)
      if(iplan .eq. 2) write(iwrit,62) clsol(2),cmpsol(2),cmbsol(2)
   62 format(10x,'second planform',
     1        3x,'cl =',f10.5,2x,'cm =',f10.5,2x,'cb =',f10.5)
c
      cltot  = 0.0
      cmtot  = 0.0
      i = 0
      crbmtt = 0.0
      strip = q(2*nscwmi+1) - q(nscwmi + 1)
c
      do 31 iplx = 1,iplan
      if(icase .lt. 2) go to 872
      if(icase .gt. 2) go to 870
      nclcd = nqcdp1(1)
      do 868 iq = 1,nclcd
      qcl(iq) = qclp1(iq,1)
  868 qcd(iq) = qcdp1(iq,1)
      go to 872
  870 continue
      if(iplx .eq. 1) nclcd = nqcdp1(1)
      if(iplx .eq. 2) nclcd = nqcdp2(1)
      do 871 iq = 1,nclcd
      if(iplx .eq. 1) qcl(iq) = qclp1(iq,1)
      if(iplx .eq. 1) qcd(iq) = qcdp1(iq,1)
      if(iplx .eq. 2) qcl(iq) = qclp2(iq,1)
      if(iplx .eq. 2) qcd(iq) = qcdp2(iq,1)
  871 continue
  872 continue
c
c
      if(iplx .eq. 1) nqloc = nsswsv(1)
      if(iplx .eq. 2) nqloc = nssw-nsswsv(1)
      do 31 ii  = 1,nqloc
      i = i + 1
      spanld(i) = 0.
      do 30 ij  = 1,nscwmi
      ik = (i - 1)*nscwmi + ij
      spanld(i) = spanld(i) + 2.*cir(ik)*cos(atan(phi(i)))
      cltot = cltot + 8.*s(ik)*cir(ik)/sref*cos(atan(phi(i)))
      cmtot = cmtot + 8.*s(ik)*cir(ik)*pn(ik)*beta*cos(atan(phi(i)))/
     1                (sref*cref)
   30 continue
      spanld(i) = spanld(i)/cave
      crbinc = 0.0
      if(q(ik) .lt. yrbm .and. iplx .ge. irbm) crbinc = spanld(i)*
     1 s(ik)*((q(ik) - yrbm) + (zh(i) - zrbm)*phi(i))/bot**2
      crbmtt = crbmtt + crbinc
      spnsta(i) = q(ik)
      chordl(i) = chord(i)/cave
      cl(i) = spanld(i)/chord(i)*cave
      clxx = cl(i)/cos(atan(phi(i)))
      if(icase .le. 1) cpc2(i)  = pdrg2(1)
      if(icase .le. 1) clmin(i) = pdrg1(1)
      if(icase .le. 1) cd0x(i)  = pdrg3(1)
      if(icase .eq. 1 .and. i .gt. nsswsv(1)) cpc2(i)   = pdrg2(2)
      if(icase .eq. 1 .and. i .gt. nsswsv(1)) clmin(i)  = pdrg1(2)
      if(icase .eq. 1 .and. i .gt. nsswsv(1)) cd0x(i)   = pdrg3(2)
      if(icase .gt. 1) call ftlup(clxx,cdpp(i),1,nclcd,qcl,qcd,50,50)
      if(icase .le. 1) cdpp(i)   = cpc2(i)*(clxx-clmin(i))**2 + cd0x(i)
      if(i .eq. nsswsv(1)) cl1   = cltot
      if(i .eq. nsswsv(1)) cm1   = cmtot
      if(i .eq. nsswsv(1)) crbm1 = crbmtt
      if(i .eq. nssw .and. iplan .eq. 2) cl2    = cltot - cl1
      if(i .eq. nssw .and. iplan .eq. 2) cm2    = cmtot - cm1
      if(i .eq. nssw .and. iplan .eq. 2) crbm22 = crbmtt - crbm1
   31 continue
      strip = spnsta(3) - spnsta(2)
      cdpt  = 0.0
      cd1   = 0.0
      cd2   = 0.0
      do 160 i = 1,nssw
      cdpt = cdpt + 2.0/sref*chord(i)*cdpp(i)*strip
      if(i .eq. nsswsv(1)) cd1 = cdpt
      if(i .eq. nssw .and. iplan .eq. 2) cd2 = cdpt - cd1
  160 continue
      ewing = cltot**2/cd/pi/ar
      cdtt = cd + cdpt + cd0
      write(iwrit,43) cl1,cd1,cm1,crbm1,cl2,cd2,cm2,crbm2
      if(ispnkl .eq. 1) go to 99
      if(jm .eq. il) write(iwrit,40)
      if (KON .eq. 2) write(iwrit,51)
      if (KON .eq. 0) write(iwrit,56)
      write(iwrit,39) cldes,cltot,cmtot,cd,ewing,cdpt,cdtt
      do 54 i = 1,nssw
      if(i .eq. 1) write(iwrit,41)
      if(i .eq. (nsswsv(1) + 1)) write(iwrit,42)
      write(iwrit,44) spnsta(i),spanld(i),chordl(i),cl(i),cdpp(i)
   54 continue
c
   99 return
c
   51 format(/12x,'no pitching moment constraint'/)
   56 format(/12x,'no root bending moment constraint'/)
   35 format(3x,'ref. chord =',f10.3,'  c average   =  ',f9.4,
     1           '  true area = ',f10.3/3x,'ref. area  =',f10.3,
     2           '  b/2         =   ',f8.4,'  ref ar    =',
     3           f8.4/3x,'true ar    =  ',f8.4,2x,
     4           'Mach number = ',f10.4)
   39 format(10x,'CL DES  =',f8.5,4x,'CL COMPUTED =',f8.4,4x,
     1 'CM =',f8.4/10x,'CD I    =',f8.5,4x,'E           =',f8.4/10x,
     2 'CDPRESS =',f8.5,4x,'CDTOTAL     = ',f8.5/)
   40 format(/3x,'no pitching moment or bending moment constraints')
   41 format(/3x,'first planform '//16x,1hY,7x,
     1 9hCL*C/CAVE,5x,'C/CAVE',8x,'CL',10x,'CD')
   42 format(/3x,'second planform'/)
   43 format(/3x,'1st planform',3x,'CL =',f7.4,3x,'CDP =',f7.4,3x,
     1 'CM =',f8.4,3x,'CB =',f8.4/3x,'2nd planform',3x,'CL =',f7.4,
     2 3x,'CDP =',f7.4,3x,'CM =',f8.4,3x,'CB =',f8.4)
   44 format(10x,f10.4,4f12.5)
   46 format(/)
   47 format(3x,'induced + pressure drag was minimized on this run'/)
   48 format(3x,'induced drag alone was minimized on this run'/)
   52 format(3x,'spanload was input on this run'/)
      end
     

      subroutine wi20
      dimension yy(2), fv(2), fw(2), dzdx(400), xxcc(20), wou(20),
     1          x3c4(22), aloc(22,1), t(41), ss(41,1), ss1(41,1),
     2          ss2(41,1), s2(22,1), s3(22,1), dely(22,1), h(22),
     3          psum(41,1), xcle(50),xcte(50),zcle(50),zcte(50),
     4          ytwst(50),ettwst(50),twistd(50)

      common /all/ bot,m,beta,ptest,qtest,tblscw(50),q(400),pn(400),
     1             pv(400),s(400),psi(400),phi(50),zh(50),nssw
      common /tothre/ cir(400)
      common /ccrrdd/ chord(50),xte(50),kbit,tspan,tspana
      common /insuba/ apsi,aphi,xx,yyy,zz,snn,tolc
      common /opt/    kon,cmb,ipunch,case,spnklu,xitmax,epsmax,crbmnt
c
c     part 3 - compute z/c versus x/c
c
c     the tolerance set at this point in the program may need to be
c     changed for computers other than the cdc 6000 series
c
      write(6,12)

      tolc = (bot*15.e-05)**2
      pi   = 3.1415926536
      izz  = 1
      nnv  = tblscw(izz)

      do 3 nv  = 1,m
      dzdx(nv) = 0.
      iz       = 1
      nnn      = tblscw(iz)
      do 2 nn  = 1,m
      aphi     = atan(phi(iz))

      apsi = psi(nn)
      xx   = pv(nv) - pn(nn)
      yy(1) = q(nv) - q(nn)
      yy(2) = q(nv) + q(nn)
      zz    = zh(izz) - zh(iz)
      snn = s(nn)
      do 1 i = 1,2
      yyy = yy(i)
      call infsub(bot,fv(i),fw(i))
      aphi = -aphi
      apsi = -apsi
1     continue
      fvn = fw(1) + fw(2) - (fv(1) + fv(2))*phi(izz)
      dzdx(nv) = dzdx(nv) + fvn*cir(nn)/12.5663704
      if (nn.lt.nnn.or.nn.eq.m) go to 2
      iz = iz + 1
      nnn = nnn + tblscw(iz)
2     continue
      if (nv .lt. nnv .or. nv .eq. m) go to 3
      izz = izz + 1
      nnv = nnv + tblscw(izz)
3     continue
c
c     integrate dzdx to obtain z/c versus x/c at the various y locations
c
      la = 1
      lb = 0
      do 9 i = 1,nssw
      in = tblscw(i)
      if (i.eq.1) go to 4
      la = la + tblscw(i-1)
4     lb = lb + tblscw(i)
      do 5 j = la,lb
      n = j - la + 1
      wou(n) = -dzdx(j)
      xxcc(n) = (n - 0.25)/in
      k = in + 1 + la - j
      x3c4(k) = pv(j)*beta
5     aloc(k,1) = -dzdx(j)
      y = q(la)/bot
      write(6,10) q(la),y,chord(i)

      ytwst(i)   = q(la)
      ettwst(i)  = y

      write(6,13)
      do 60 ij = 1,in
   60 write(6,11) xxcc(ij),wou(ij)
      if (ipunch .ne. 0) write(7,19) (wou(ij),ij = 1,in)
19    format(7f10.5)
      write(6,15)
      write(6,16)
      k1 = in + 2
      k2 = in + 1
      aloc(1,1) = aloc(2,1)
      aloc(k1,1) = aloc(k2,1)
      x3c4(1) = xte(i)
      x3c4(k1) = xte(i) + chord(i)
      d1 = 0.
      d2 = 0.
      do 6 l = 1,41
6     t(l) = xte(i) + chord(i)*(l-1)*0.025
      iw = 0
      call spline (22,1,41,k1,1,41,x3c4,aloc,t,a,ss,ss1,ss2,s2,s3,
     1             dely,h,iw,d1,d2,1,psum)
      do 7 l = 1,40
      k = 42 - l
      j = 41 - l
7     psum(k,1) = psum(j,1)
      psum(1,1) = 0.
      do 8 l = 1,41
      k = 42 - l
      xoc = 1. + (xte(i) - t(k))/chord(i)
      zoc = psum(k,1)/chord(i)
      x = xoc*chord(i)

      if (l .eq. 1) zocref = zoc

      zbarc = zoc - (1. - xoc)*zocref

      write(6,11) xoc,zoc,x,psum(k,1),zbarc

      if (l .eq. 1)  then 
                     xcle(i) = xoc
                     zcle(i) = zoc
                     endif
      if (l .eq. 41) then
                     xcte(i) = xoc
                     zcte(i) = zoc
                     endif
8     continue

      write(6,18)
9     continue

c     output twist table

      write(6,110)

      do 100 i  = 1,nssw
      ttwist    = (zcte(i) - zcle(i)) / (xcte(i) - xcle(i))
      twistd(i) = 180./pi*atan(ttwist)
      write(6,130) i,ytwst(i),ettwst(i),twistd(i)
  100 continue

  110 format(/6x,'twist table'//6x,'i',8x,'y',8x,'y/(b/2)',6x,'twist')
  130 format(3x,i4,8f12.5)

      return

10    format(10x,'y=',f10.4,6x,'y/(b/2) =',f10.4,6x,'chord=',f10.4//)
11    format(5x,f9.4,4(2x,f9.4))
12    format(//10x,'mean camber lines to obtain the spanload'//
     1         10x,'(subsonic linear theory)'//)
13    format(10x,'slopes, dz/dx, at control points, from front to rear'//
     1       10x,'x/c',8x,'dz/dx')
14    format(/10x,'corrsponding x/c locations from fron to rear'/)
15    format(//10x,'mean camber shape (interpolated to 41 points)'/)
16    format(10x,'x/c',8x,'z/c',5x,'delta x',4x,'delta z',4x,
     1       '(z-zle)/c')
17    format(5x,10f10.5)
18    format(1h1)

      end

 
      subroutine qdrag(cloccl,cpc2,clmin,nqclcd,qcl,qcd,
     1                 lm,iplan,icase,nma,ppp)
c
c      curve fit the input polar to extract the relevant
c      properties for the minimization
c
      common /inout/ iwrit,iread,iecho,ioutwr,iplfit
      common /pdrag/ pdrg1(2),pdrg2(2),pdrg3(2),tdklue,nqcdp1(5),
     1               qclp1(50,5),qcdp1(50,5),nqcdp2(5),
     2               qclp2(50,5),qcdp2(50,5),nspolr(2)
      dimension      cloccl(100),cpc2(100),clmin(100),
     1               qcl(nqclcd),qcd(nqclcd),
     2               nma(2),ppp(100)
c
      xh = 0.1
      i  = 0
      if(iplfit .eq. 1) xh = 0.02
c
      do 10 iplx = 1,iplan
      if (icase .gt. 2) go to 8
      nqclcd = nqcdp1(1)
      do 7 iq = 1,nqclcd
      qcl(iq) = qclp1(iq,1)
    7 qcd(iq) = qcdp1(iq,1)
      go to 9
    8 continue
      if (iplx .eq.1) nqclcd = nqcdp1(1)
      if (iplx .eq.2) nqclcd = nqcdp2(1)
      do 20 iq = 1,nqclcd
      if(iplx .eq. 1) qcl(iq) = qclp1(iq,1)
      if(iplx .eq. 1) qcd(iq) = qcdp1(iq,1)
      if(iplx .eq. 2) qcl(iq) = qclp2(iq,1)
      if(iplx .eq. 2) qcd(iq) = qcdp2(iq,1)
   20 continue
    9 continue
c
      nqloc = nma(iplx)
      do 10 ii = 1,nqloc
      i = i + 1
      cl = cloccl(i)
      cl = cl/ppp(i)
      clu = cl + xh
      cll = cl - xh
      clmin(i) = 0.0
c
      call ftlup(cl,cd,1,nqclcd,qcl,qcd,nqclcd,nqclcd)
      call ftlup(clu,cdu,1,nqclcd,qcl,qcd,nqclcd,nqclcd)
      call ftlup(cll,cdl,1,nqclcd,qcl,qcd,nqclcd,nqclcd)
c
      cdp = (cdu - cdl)/(2.0*xh)
      cdpp = (cdu - 2.0*cd + cdl)/(xh**2)
c
      if(iplfit .eq. 1) go to 5
      cl1 = cll
      cl2 = cl
      cl3 = clu
      cd1 = cdl
      cd2 = cd
      cd3 = cdu
      acoef = (cd3 - cd1)/(cd2 - cd1)
      write(6,100) i,cl1,cl2,cl3,cd1,cd2,cd3,acoef
      clmin(i) = 0.5*(cl3**2 - cl1*2 - acoef*(cl2**2 - cl1**2))/
     1               (cl3 - cl1 - acoef*(cl2 - cl1))
      cpc2(i) = (cd2-cd1)/((cl2-clmin(i))**2-(cl1-clmin(i))**2)
c
      b = cd - cpc2(i)*(cl - clmin(i))**2
c
      go to 18
    5 continue
c
      cpc2(i) = (cdu - cdl)/(clu - cll)
      b = cdu - cpc2(i)*clu
c
   18 if(ioutwr .eq. 1) go to 10
c     write(iwrit,100) i,cl,cd,cdu,cdl,cdp,cdpp,cpc2(i),clmin(i),b
c
   10 continue
c
  100 format(i5,10f12.5)
      return
      end


      SUBROUTINE INFSUB(BOT,FVI,FWI)
      COMMON /INSUBA/ PSII,APHII,XXX,YYY,ZZZ,SNN,TOLRNC
c
      FC = COS(PSII)
      FS = SIN(PSII)
      FT = FS/FC
      FPC= COS(APHII)
      FPS= SIN(APHII)
      FPT= FPS/FPC
      F1 = XXX+SNN*FT*FPC
      F2 = YYY+SNN*FPC
      F3 = ZZZ+SNN*FPS
      F4 = XXX-SNN*FT*FPC
      F5 = YYY-SNN*FPC
      F6 = ZZZ-SNN*FPS
      FFA=(XXX**2+(YYY*FPS)**2+FPC**2*((YYY*FT)**2+(ZZZ/FC)**2-2.*XXX*
     1     YYY*FT)-2.*ZZZ*FPC*(YYY*FPS+XXX*FT*FPS))
      FFB=(F1*F1+F2*F2+F3*F3)**.5
      FFC=(F4*F4+F5*F5+F6*F6)**.5
      FFD=F5*F5+F6*F6
      FFE=F2*F2+F3*F3
      FFF=(F1*FPC*FT+F2*FPC+F3*FPS)/FFB-(F4*FPC*FT+F5*FPC+F6*FPS)/FFC
C
C     THE TOLERANCE SET AT THIS POINT IN THE PROGRAM MAY NEED TO BE
C     CHANGED FOR COMPUTERS OTHER THAN THE CDC 6000 SERIES
C
      IF (ABS(FFA).LT.(BOT*15.E-5)**2) GO TO 10
      FVONE = (XXX*FPS-ZZZ*FT*FPC)*FFF/FFA
      FWONE = (YYY*FT-XXX)*FFF/FFA*FPC
      GO TO 20
10    FVONE=0.
      FWONE=0.
C
20    IF (ABS(FFD).LT.TOLRNC) GO TO 30
      FVTWO=F6*(1.-F4/FFC)/FFD
      FWTWO=-F5*(1.-F4/FFC)/FFD
      GO TO 40
30    FVTWO=0.
      FWTWO=0.
C
40    IF (ABS(FFE).LT.TOLRNC) GO TO 50
      FVTHRE=-F3*(1.-F1/FFB)/FFE
      FWTHRE=F2*(1.-F1/FFB)/FFE
      GO TO 60
50    FVTHRE=0.
      FWTHRE=0.
C
60    FVI=FVONE+FVTWO+FVTHRE
      FWI=FWONE+FWTWO+FWTHRE
      RETURN
      END


      subroutine spline (mnpts,mncvs,mmax,n,ncvs,m,x,y,t,proxin,
     1                   ss,ss1,ss2,s2,s3,dely,h,iw,d1,d2,kab,psum)
      dimension th(50),delh(50,1),ct(50),th2(50),delsqh(50),d(50),
     1          st2(50,1),d1(ncvs),d2(ncvs),kab(ncvs),h2(50),c(50),
     2          psum(mmax,mncvs),x(mnpts),y(mnpts,mncvs),t(mmax),
     3          dely(mnpts,mncvs),s2(mnpts,mncvs),s3(mnpts,mncvs),
     4          ss1(mmax,mncvs),ss(mmax,mncvs),h(mnpts),delsqy(50),
     5          ss2(mmax,mncvs),proxin(mncvs)

      mndim = 50

      if (iw) 9,1,9
1     n1 = n - 1
      iw = 2
      do 8 k = 1,ncvs
      do 2 i = 1,n1
      h(i) = x(i+1) - x(i)
      ii = i + 1
      dely(i,k) = (y(ii,k) - y(i,k))/h(i)
2     c(i) = h(i)
      do 3 i = 2,n1
      h2(i) = (h(i-1) + h(i))*2.
      delsqy(i) = (dely(i,k) - dely(i-1,k))*6.
3     continue
      if(kab(k) .eq. 0) go to 4
      h2(1) = 2.*h(1)
      h2(n) = 2.*h(n1)
      delsqy(1) = 6.*(dely(1,k) - d1(k))
      delsqy(n) = (d2(k) - dely(n1,k))*6.
      go to 5
4     h2(1) = 1.0
      h2(n) = 1.0
      c(1)  = 0.0
      h(n1) = 0.0
      delsqy(1) = 0.0
      delsqy(n) = 0.0
5     call trimat(h,h2,c,delsqy,d,n,mndim)
      do 6 i = 1,n
6     s2(i,k) = d(i)
      h(n1) = c(n1)
      do 7 i = 1,n1
      ii = i + 1
7     s3(i,k) = (s2(ii,k) - s2(i,k))/h(i)
8     continue
9     continue
      j = 0
10    j = j + 1
      i = 1
      if (t(j) - x(1)) 14,17,11
11    if (t(j) - x(n)) 13,15,14
12    if (t(j) - x(i)) 16,17,13
13    i = i + 1
      go to 12
14    continue
      print 25,j
      print 26, (x(i),i=1,n)
      print 26, (y(i,1),i=1,n)
      go to 19
15    i = n
16    continue
      iw = -i
      i = i - 1
17    do 18 k = 1,ncvs
      ht1 = t(j) - x(i)
      ii = i + 1
      ht2 = t(j) - x(ii)
      prod = ht1*ht2
      ss2(j,k) = s2(i,k) + ht1*s3(i,k)
      delsqs = (s2(i,k) + s2(ii,k) + ss2(j,k))/6.
      ss(j,k) = y(i,k) + ht1*dely(i,k) + prod*delsqs
      ss1(j,k) = dely(i,k) + (ht1 + ht2)*delsqs+prod*s3(i,k)/6.0
18    continue
19    continue
      if (j.lt.m) go to 10
      m1 = m - 1
      do 24 k = 1,ncvs
      do 20 i = 1,m1
      th(i) = t(i+1) - t(i)
      ii = i + 1
      delh(i,k) = (ss(ii,k) - ss(i,k))/th(i)
      ct(i) = th(i)
20    continue
      do 21 i = 2,m1
      th2(i) = (th(i-1) + th(i))*2.
      delsqh(i) = (delh(i,k) - delh(i-1,k))*6.
21    continue
      th2(1) = 1.
      th2(m) = 1.
      ct(1) = 0.
      th(m1) = 0.
      delsqh(1) = 0.
      delsqh(m) = 0.
      call trimat(th,th2,ct,delsqh,d,m,mndim)
      do 22 i = 1,m
      st2(i,k) = d(i)
22    continue
      th(m1) = ct(m1)
      proxin(k) = 0.0
      do 23 i = 1,m1
      ii = i + 1
      proxin(k) = proxin(k) + 0.5*th(i)*(ss(i,k) + ss(ii,k)) -
     1            th(i)**3*(st2(i,k) + st2(ii,k))/24.
      psum(i,k) = proxin(k)
23    continue
24    continue
      return
c
25    format(i4,' th argument out of range')
26    format(10f10.3)
      end


      subroutine xitimp(xk,y,p,jm)
c
c     iterative improvement
c
      dimension xk(102,102), y(102), p(102)
      do 10 i = 1,jm
      sum     = 0.0
      do 8 j  = 1,jm
      if(i.eq.j) go to 8
      sum     = sum + xk(i,j)*y(j)
    8 continue
      p(i)    = (p(i) - sum)/xk(i,i)
      write(6,120) i,y(i),p(i),sum,xk(i,i)
  120 format(12x,i6,4e16.5)
   10 continue
c
      return
      end

      subroutine trimat(a,b,c,d,t,n,mndim)
      dimension a(mndim),  b(mndim), c(mndim), d(mndim), t(mndim), 
     1          w(50),     sv(50),   g(50)

c     routine solves the tridiagonal (except two elements) matrix
c
      w(1)   = b(1)
      sv(1)  = c(1)/b(1)
      g(1)   = d(1)/w(1)
      nm1    = n - 1
      do 2 k = 2,n
      km1    = k-1
      w(k)   = b(k) - a(km1)*sv(km1)
      if(k.eq.n) go to 1
      sv(k)  = c(k)/w(k)
1     g(k)   = (d(k) - a(km1)*g(km1))/w(k)
2     continue
      t(n)   = g(n)
      do 3 k = 1,nm1
      kk     = n - k
      t(kk)  = g(kk) - sv(kk)*t(kk+1)
3     continue
      return
      end

      subroutine ftlup (x,y,m,n,vari,vard,idim1,idim2)
c     ***document date 09-12-69    subroutine revised 07-07-69*********
c     modification of library interpolation subroutine ftlup
      dimension vari(idim1), vard(idim2), v(3), yy(2)
      dimension ii(43)
c
c      initialize all interval pointers to -1.0  for monotonicity check
      data (ii(j),j=1,43)/43* -1/
      ma=iabs(m)
c
c     assign interval pointers for given vari table
c     the same pointer will be used on a given vari table every time
c
c      li=mod(%loc(vari(1)),43)+1
c      i=ii(li)
      li=1
      i= -1
      if (i.ge.0) go to 60
      if (n.lt.2) go to 60
c
c     monotonicity check
c
      if(vari(2)-vari(1))20,20,40
c
c     error in monotonicity
c
c  10  k= %loc(vari(1))
   10  k= 1
       print 170, j,k,(vari(j),j=1,n),(vard(j),j=1,n)
       stop
c
c     monotonic decreasing
c
20    do 30 j=2,n
      if (vari(j)-vari(j-1))30,10,10
30    continue
      go to 60
c
c     monotonic increasing
c
40    do 50 j=2,n
      if (vari(j)-vari(j-1)) 10,10,50
50    continue
c
c     interpolation
c
60    if (i.le.0) i=1
      if (i.ge.n) i=n-1
      if (n.le.1) go to 70
      if (ma.ne.0) go to 80
c
c     zero order
c
70    y=vard(1)
      go to 160
c
c    locate i interval (x(i).le.x.lt.x(i+1))
c
80    if ((vari(i)-x)*(vari(i+1)-x)) 110,110,90
c
c     gives direction for search of intervals
c
90    in= sign(1.0,(vari(i+1)-vari(i))*(x-vari(i)))
c
c     if x outside endpoints, extrapolate from end interval
c
100   if ((i+in).le.0) go to 110
      if ((i+in).ge.n) go to 110
      i=i+in
      if ((vari(i)-x)*(vari(i+1)-x)) 110,110,100
110   if (ma.eq.2) go to 120
c
c     first order
c
      y=(vard(i)*(vari(i+1)-x)-vard(i+1)*(vari(i)-x))/(vari(i+1)-vari(i)
     1)
      go to 160
c
c     second order
c
120   if (n.eq.2) go to 10
      if (i.eq.(n-1)) go to 140
      if (i.eq.1) go to 130
c
c     pick third point
c
      sk=vari(i+1)-vari(i)
      if ((sk*(x-vari(i-1))).lt.(sk*(vari(i+2)-x))) go to 140
130   l=i
      go to 150
140   l=i-1
150   v(1)=vari(l)-x
      v(2)=vari(l+1)-x
      v(3)=vari(l+2)-x
      yy(1)=(vard(l)*v(2)-vard(l+1)*v(1))/(vari(l+1)-vari(l))
      yy(2)=(vard(l+1)*v(3)-vard(l+2)*v(2))/(vari(l+2)-vari(l+1))
      y=(yy(1)*v(3)-yy(2)*v(1))/(vari(l+2)-vari(l))
160   ii(li)=i
      return
c
c
170   format(1h1,' table below out of order for ftlup  at position ',
     1i5,/' x table is stored in location  ',i6//8g15.8)
      end
