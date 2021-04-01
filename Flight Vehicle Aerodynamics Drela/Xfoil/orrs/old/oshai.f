      SUBROUTINE OSHAI(RSP,FSP,HSP, AI,
     &                 AI_R, AI_F, AI_H,
     &                 AIF_R,AIF_F,AIF_H , OK)
C---------------------------------------------------------------------
C    
C     Returns imaginary part of complex wavenumber (Ai) eigenvalue 
C     from Orr-Sommerfeld solution with mean profiles characterized
C     by shape parameter H.  Also returns the sensitivities of Ai
C     with respect to the input parameters.
C
C     The eigenvalue Ai(Rtheta,f,H) is stored as a 3-D array at 
C     discrete points, which is then interpolated to any (Rtheta,f,H)
C     via a tricubic spline.  The spline coordinates actually used are:
C
C       RL = log10(Rtheta)    
C       FL = log10(f) + 0.5 log10(Rtheta)
C       HL = H
C
C
C      Input:
C      ------
C        RSP    momentum thickness Reynolds number  Rtheta = Theta Ue / v
C        FSP    normalized disturbance frequency         f = w Theta/Ue
C        HSP    shape parameter of mean profile          H = Dstar/Theta
C
C      Output:
C      -------
C        AI     imaginary part of complex wavenumber * Theta
C        AI_R   d(AI)/dRtheta
C        AI_F   d(AI)/df
C        AI_H   d(AI)/dH
C        AIF_R  d(dAI/df)/dRtheta
C        AIF_F  d(dAI/df)/df
C        AIF_H  d(dAI/df)/dH
C        OK     T  if look up was successful; all values returned are valid
C               F  if point fell outside (RL,FL) spline domain limits; 
C                  all values (AI, AI_R, etc.) are returned as zero.
C                  Exception: If points only falls outside HL spline limits,
C                  then the HL limit is used and an AI value is calculated,
C                  but OK is still returned as F.
C                  
C---------------------------------------------------------------------
C
      REAL B(2,2), BR(2,2), BF(2,2), BH(2,2),
     &            BRF(2,2),BRH(2,2),BFH(2,2),BRFH(2,2)
      REAL C(2)  , CR(2)  , CF(2)  , CH(2)  ,
     &            CRF(2)  ,CRH(2)  ,CFH(2)  ,CRFH(2)
C
      PARAMETER (NRX=31, NFX=41, NHX=21)
      COMMON /AICOM/ NR, NF, NH,
     &               IF1(NHX), IF2(NHX), IR1(NHX),IR2(NHX),
     &               RINCR, FINCR, RL(NRX), FL(NFX), HL(NHX),
     &                A(NRX,NFX,NHX),
     &               AR(NRX,NFX,NHX),
     &               AF(NRX,NFX,NHX),
     &               AH(NRX,NFX,NHX),
     &              ARF(NRX,NFX,NHX),
     &              ARH(NRX,NFX,NHX),
     &              AFH(NRX,NFX,NHX),
     &             ARFH(NRX,NFX,NHX)
      LOGICAL LOADED, OK
      SAVE LOADED
C
      DATA LOADED /.FALSE./
C
C---- set ln(10) for derivatives of log10 function
      DATA AL10 /2.302585093/
C
C
      IF(LOADED) GO TO 9
C
C---- first time OSHAI is called ... load in 3-D spline data
      OPEN(UNIT=31,FILE='~/codes/mses/orrs/oshai.dat',
     &             STATUS='OLD',FORM='UNFORMATTED')
      WRITE(*,*) 'Loading Orr-Sommerfeld maps...'
      READ(31) NR, NF, NH
      IF(NR.GT.NRX) STOP 'OSHAI: R array limit overflow.'
      IF(NF.GT.NFX) STOP 'OSHAI: F array limit overflow.'
      IF(NH.GT.NHX) STOP 'OSHAI: H array limit overflow.'
      READ(31) (RL(IR), IR=1,NR)
      READ(31) (FL(IF), IF=1,NF)
      READ(31) (HL(IH), IH=1,NH)
      READ(31) (IR1(IH),IR2(IH),IF1(IH),IF2(IH), IH=1,NH)
      DO 3 IH=1, NH
        DO 2 IF=IF1(IH), IF2(IH)
          READ(31) (   A(IR,IF,IH), IR=IR1(IH),IR2(IH))
          READ(31) (  AR(IR,IF,IH), IR=IR1(IH),IR2(IH))
          READ(31) (  AF(IR,IF,IH), IR=IR1(IH),IR2(IH))
          READ(31) (  AH(IR,IF,IH), IR=IR1(IH),IR2(IH))
          READ(31) ( ARF(IR,IF,IH), IR=IR1(IH),IR2(IH))
          READ(31) ( ARH(IR,IF,IH), IR=IR1(IH),IR2(IH))
          READ(31) ( AFH(IR,IF,IH), IR=IR1(IH),IR2(IH))
          READ(31) (ARFH(IR,IF,IH), IR=IR1(IH),IR2(IH))
    2   CONTINUE
    3 CONTINUE
      CLOSE(31)
C
      RINCR = (RL(NR) - RL(1))/FLOAT(NR-1)
      FINCR = (FL(NF) - FL(1))/FLOAT(NF-1)
      LOADED = .TRUE.
C
    9 CONTINUE
C
C---- set returned variables in case of out-of-limits error
      AI = 0.0
      AI_R = 0.0
      AI_F = 0.0
      AI_H = 0.0
      AIF_R = 0.0
      AIF_F = 0.0
      AIF_H = 0.0
C
C---- define specified spline coordinates
      RLSP = ALOG10(RSP)
      FLSP = ALOG10(FSP) + 0.5*RLSP
      HLSP = HSP
C
      OK = .TRUE.
C
C---- find H interval
      DO 10 IH=2, NH
        IF(HL(IH) .GE. HLSP) GO TO 11
   10 CONTINUE
      IH = NH
   11 CONTINUE
C
      IF(HSP.LT.HL(1) .OR. HSP.GT.HL(NH)) THEN
       OK = .FALSE.
CCC       WRITE(6,*) 'Over H limits.  R w H:', RSP,FSP,HSP
CCC       RETURN
      ENDIF
C
C---- find R interval
      IR = INT((RLSP-RL(1))/RINCR + 2.001)
      IR1MAX = MAX0( IR1(IH) , IR1(IH-1) )
      IR2MIN = MIN0( IR2(IH) , IR2(IH-1) )
      IF(IR-1.LT.IR1MAX .OR. IR.GT.IR2MIN) THEN
       OK = .FALSE.
CCC       WRITE(6,*) 'Over R limits.  R w H:', RSP,FSP,HSP
CCC       RETURN
      ENDIF
C
C---- find F interval
      IF = INT((FLSP-FL(1))/FINCR + 2.001)
      IF1MAX = MAX0( IF1(IH) , IF1(IH-1) )
      IF2MIN = MIN0( IF2(IH) , IF2(IH-1) )
      IF(IF-1.LT.IF1MAX .OR. IF.GT.IF2MIN) THEN
       OK = .FALSE.
CCC       WRITE(6,*) 'Over w limits.  R w H:', RSP,FSP,HSP
CCC       RETURN
      ENDIF
C
C
      DRL = RL(IR) - RL(IR-1)
      DFL = FL(IF) - FL(IF-1)
      DHL = HL(IH) - HL(IH-1)
      TR = (RLSP - RL(IR-1)) / DRL
      TF = (FLSP - FL(IF-1)) / DFL
      TH = (HLSP - HL(IH-1)) / DHL
C
C---- evaluate spline in Rtheta at the corners of HL,FL cell
      DO 20 KH=1, 2
        JH = IH + KH-2
        DO 205 KF=1, 2
          JF = IF + KF-2
          A1    = A   (IR-1,JF,JH)
          AR1   = AR  (IR-1,JF,JH)
          AF1   = AF  (IR-1,JF,JH)
          AH1   = AH  (IR-1,JF,JH)
          ARF1  = ARF (IR-1,JF,JH)
          ARH1  = ARH (IR-1,JF,JH)
          AFH1  = AFH (IR-1,JF,JH)
          ARFH1 = ARFH(IR-1,JF,JH)
C
          A2    = A   (IR  ,JF,JH)
          AR2   = AR  (IR  ,JF,JH)
          AF2   = AF  (IR  ,JF,JH)
          AH2   = AH  (IR  ,JF,JH)
          ARF2  = ARF (IR  ,JF,JH)
          ARH2  = ARH (IR  ,JF,JH)
          AFH2  = AFH (IR  ,JF,JH)
          ARFH2 = ARFH(IR  ,JF,JH)
C
          DA1   = DRL*AR1   - A2   + A1
          DA2   = DRL*AR2   - A2   + A1
          DAF1  = DRL*ARF1  - AF2  + AF1
          DAF2  = DRL*ARF2  - AF2  + AF1
          DAH1  = DRL*ARH1  - AH2  + AH1
          DAH2  = DRL*ARH2  - AH2  + AH1
          DAFH1 = DRL*ARFH1 - AFH2 + AFH1
          DAFH2 = DRL*ARFH2 - AFH2 + AFH1
C
C-------- set  AI, dAI/dFL, dAI/dHL, d2AI/dHLdFL
          B(KF,KH)   =  (1.0-TR)* A1   + TR* A2
     &               + ((1.0-TR)*DA1   - TR*DA2  )*(TR-TR*TR)
          BF(KF,KH)  =  (1.0-TR)* AF1  + TR* AF2
     &               + ((1.0-TR)*DAF1  - TR*DAF2 )*(TR-TR*TR)
          BH(KF,KH)  =  (1.0-TR)* AH1  + TR* AH2
     &               + ((1.0-TR)*DAH1  - TR*DAH2 )*(TR-TR*TR)
          BFH(KF,KH) =  (1.0-TR)* AFH1 + TR* AFH2
     &               + ((1.0-TR)*DAFH1 - TR*DAFH2)*(TR-TR*TR)
C
C-------- also, the RL derivatives of the quantities above
          BR(KF,KH)   = (A2   - A1
     &     + (1.0-4.0*TR+3.0*TR*TR)*DA1   + (3.0*TR-2.0)*TR*DA2  )/DRL
          BRF(KF,KH)  = (AF2  - AF1
     &     + (1.0-4.0*TR+3.0*TR*TR)*DAF1  + (3.0*TR-2.0)*TR*DAF2 )/DRL
          BRH(KF,KH)  = (AH2  - AH1
     &     + (1.0-4.0*TR+3.0*TR*TR)*DAH1  + (3.0*TR-2.0)*TR*DAH2 )/DRL
          BRFH(KF,KH) = (AFH2 - AFH1
     &     + (1.0-4.0*TR+3.0*TR*TR)*DAFH1 + (3.0*TR-2.0)*TR*DAFH2)/DRL
C
  205   CONTINUE
   20 CONTINUE
C
C---- evaluate spline in  HL  at the two FL-interval endpoints
      DO 30 KF=1, 2
        B1    = B   (KF,1)
        BR1   = BR  (KF,1)
        BF1   = BF  (KF,1)
        BH1   = BH  (KF,1)
        BRF1  = BRF (KF,1)
        BRH1  = BRH (KF,1)
        BFH1  = BFH (KF,1)
        BRFH1 = BRFH(KF,1)
C
        B2    = B   (KF,2)
        BR2   = BR  (KF,2)
        BF2   = BF  (KF,2)
        BH2   = BH  (KF,2)
        BRF2  = BRF (KF,2)
        BRH2  = BRH (KF,2)
        BFH2  = BFH (KF,2)
        BRFH2 = BRFH(KF,2)
C
        DB1   = DHL*BH1   - B2   + B1
        DB2   = DHL*BH2   - B2   + B1
        DBR1  = DHL*BRH1  - BR2  + BR1
        DBR2  = DHL*BRH2  - BR2  + BR1
        DBF1  = DHL*BFH1  - BF2  + BF1
        DBF2  = DHL*BFH2  - BF2  + BF1
        DBRF1 = DHL*BRFH1 - BRF2 + BRF1
        DBRF2 = DHL*BRFH2 - BRF2 + BRF1
C
C------ set  AI, dAI/dRL, dAI/dFL
        C(KF)   =  (1.0-TH)* B1   + TH* B2
     &          + ((1.0-TH)*DB1   - TH*DB2  )*(TH-TH*TH)
        CR(KF)  =  (1.0-TH)* BR1  + TH* BR2
     &          + ((1.0-TH)*DBR1  - TH*DBR2 )*(TH-TH*TH)
        CF(KF)  =  (1.0-TH)* BF1  + TH* BF2
     &          + ((1.0-TH)*DBF1  - TH*DBF2 )*(TH-TH*TH)
        CRF(KF) =  (1.0-TH)* BRF1 + TH* BRF2
     &          + ((1.0-TH)*DBRF1 - TH*DBRF2)*(TH-TH*TH)
C
C------ also, the HL derivatives of the quantities above
        CH(KF)   = (B2   - B1
     &     + (1.0-4.0*TH+3.0*TH*TH)*DB1   + (3.0*TH-2.0)*TH*DB2  )/DHL
        CRH(KF)  = (BR2  - BR1
     &     + (1.0-4.0*TH+3.0*TH*TH)*DBR1  + (3.0*TH-2.0)*TH*DBR2 )/DHL
        CFH(KF)  = (BF2  - BF1
     &     + (1.0-4.0*TH+3.0*TH*TH)*DBF1  + (3.0*TH-2.0)*TH*DBF2 )/DHL
        CRFH(KF) = (BRF2 - BRF1
     &     + (1.0-4.0*TH+3.0*TH*TH)*DBRF1 + (3.0*TH-2.0)*TH*DBRF2)/DHL
C
   30 CONTINUE
C
C---- evaluate cubic in  FL
      C1    = C   (1)
      CR1   = CR  (1)
      CF1   = CF  (1)
      CH1   = CH  (1)
      CRF1  = CRF (1)
      CRH1  = CRH (1)
      CFH1  = CFH (1)
      CRFH1 = CRFH(1)
C
      C2    = C   (2)
      CR2   = CR  (2)
      CF2   = CF  (2)
      CH2   = CH  (2)
      CRF2  = CRF (2)
      CRH2  = CRH (2)
      CFH2  = CFH (2)
      CRFH2 = CRFH(2)
C
      DC1   = DFL*CF1   - C2   + C1
      DC2   = DFL*CF2   - C2   + C1
      DCH1  = DFL*CFH1  - CH2  + CH1
      DCH2  = DFL*CFH2  - CH2  + CH1
      DCR1  = DFL*CRF1  - CR2  + CR1
      DCR2  = DFL*CRF2  - CR2  + CR1
      DCRH1 = DFL*CRFH1 - CRH2 + CRH1
      DCRH2 = DFL*CRFH2 - CRH2 + CRH1
C
C---- set  AI, dAI/dRL, dAI/dHL
      AI      =  (1.0-TF)* C1   + TF* C2
     &        + ((1.0-TF)*DC1   - TF*DC2  )*(TF-TF*TF)
      AI_RL   =  (1.0-TF)* CR1  + TF* CR2
     &        + ((1.0-TF)*DCR1  - TF*DCR2 )*(TF-TF*TF)
      AI_HL   =  (1.0-TF)* CH1  + TF* CH2
     &        + ((1.0-TF)*DCH1  - TF*DCH2 )*(TF-TF*TF)
C
C---- also, the FL derivatives of the quantities above
      AI_FL   = (C2   - C1
     &   + (1.0-4.0*TF+3.0*TF*TF)*DC1   + (3.0*TF-2.0)*TF*DC2  )/DFL
      AIF_RL  = (CR2  - CR1
     &   + (1.0-4.0*TF+3.0*TF*TF)*DCR1  + (3.0*TF-2.0)*TF*DCR2 )/DFL
      AIF_HL  = (CH2  - CH1
     &   + (1.0-4.0*TF+3.0*TF*TF)*DCH1  + (3.0*TF-2.0)*TF*DCH2 )/DFL
C
      AIF_FL  = ((6.0*TF-4.0)*DC1  + (6.0*TF-2.0)*DC2 )/DFL**2
C
C
C---- convert derivatives wrt to spline coordinates (RL,FL,HL) into 
C-    derivatives wrt input variables (Rtheta,f,H)
      AI_R = (AI_RL + 0.5*AI_FL) / (AL10 * RSP)
      AI_F = (AI_FL            ) / (AL10 * FSP)
      AI_H =  AI_HL
C
      AIF_R = (AIF_RL + 0.5*AIF_FL) / (AL10**2 * FSP*RSP)
      AIF_F = (AIF_FL - AL10*AI_FL) / (AL10**2 * FSP*FSP)
      AIF_H =  AIF_HL               / (AL10    * FSP    )
C
C---- if we're within the spline data space, the derivatives are valid
      IF(OK) RETURN
C
C---- if not, the  ai  value is clamped, and its derivatives are zero
      AI_R = 0.0
      AI_F = 0.0
      AI_H = 0.0
C
      AIF_R = 0.0
      AIF_F = 0.0
      AIF_H = 0.0
C
      RETURN
      END

