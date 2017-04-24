(*     ----------------------------------------------------------------      *)

                               UNIT SGP4UNIT;
(*
*    this file contains the sgp4 procedures for analytical propagation
*    of a satellite. the code was originally released in the 1980 and 1986
*    spacetrack papers. a detailed discussion of the theory and history
*    may be found in the 2006 aiaa paper by vallado, crawford, hujsak,
*    and kelso.
*
*                            companion code for
*               fundamentals of astrodynamics and applications
*                                    2007
*                              by david vallado
*
*       (w) 719-573-2600, email dvallado@agi.com
*
*    current :
*              26 Aug 08  david vallado
*                           fix atime for faster operation in dspace
*                           add operationmode for afspc (a) or improved (i)
*                           performance mode
*    changes :
*              16 jun 08  david vallado
*                           update small eccentricity check
*              16 nov 07  david vallado
*                           misc fixes for better compliance
*               2 apr 07  david vallado
*                           misc fixes for constants
*              11 aug 06  david vallado
*                           chg lyddane choice back to strn3, constants, misc doc
*              15 dec 05  david vallado
*                           misc fixes
*              26 jul 05  david vallado
*                           fixes for paper
*                           note that each fix is preceded by a
*                           comment with "sgp4fix" and an explanation of
*                           what was changed
*              10 aug 04  david vallado
*                           2nd printing baseline working
*              14 may 01  david vallado
*                           2nd edition baseline
*                     80  norad
*                           original baseline
*      ----------------------------------------------------------------      *)

                                  INTERFACE

(*     ----------------------------------------------------------------      *)

    Uses
      sgp4ext;

    TYPE
      Str3       = STRING[3];
      Str10      = STRING[10];
      Str11      = STRING[11];
      Str12      = STRING[12];
      Str255     = STRING[255];
      Str69 = STRING[69];
      Str25 = STRING[25];
      Str8  = STRING[8];

     ElSetRec = RECORD
                  SatNum               : LONGINT;
                  EpochYr              : BYTE;
                  a,Altp,Alta,EpochDays,JDSatEpoch : EXTENDED;
                  NDDot, NDot, BStar,RCSe : EXTENDED;
                  Inclo, nodeo, Ecco, Argpo, Mo, No : EXTENDED;
                  EpochTyNumRev        : LONGINT;
                  Init : CHAR;

                  Isimp, Error  : INTEGER;
                  Method : CHAR;
                  Aycof , CON41 , Cc1   , Cc4   , Cc5   , D2    ,
                  D3    , D4    , Delmo , Eta   , ArgpDot , Omgcof,
                  Sinmao, T     , T2cof , T3cof , T4cof , T5cof ,
                  X1mth2, X7thm1, MDot  , nodeDot,Xlcof, Xmcof ,
                  nodeCF : EXTENDED;

                  IRez  : INTEGER;
                  D2201 , D2211 , D3210 , D3222 , D4410 , D4422 ,
                  D5220 , D5232 , D5421 , D5433 , Dedt  , Del1  ,
                  Del2  , Del3  , Didt  , Dmdt  , Dnodt , Domdt ,
                  E3    , Ee2   , Peo   , Pgho  , Pho   , Pinco ,
                  Plo   , Se2   , Se3   , Sgh2  , Sgh3  , Sgh4  ,
                  Sh2   , Sh3   , Si2   , Si3   , Sl2   , Sl3   ,
                  Sl4   , GSTo  , Xfact , Xgh2  , Xgh3  , Xgh4  ,
                  Xh2   , Xh3   , Xi2   , Xi3   , Xl2   , Xl3   ,
                  Xl4   , Xlamo , Zmol  , Zmos  , Atime , Xli   ,
                  Xni : EXTENDED;
                END;
    gravconsttype = ( wgs721, wgs72, wgs84 );


   VAR
     Help, Obsmode     : CHAR;
     SGP4File : Text;
common:str25;

PROCEDURE SGP4Init           ( whichconst                            : gravconsttype;
                               Satn                                  : Longint;
                               Epoch                                 : EXTENDED;
                               VAR xBStar, xEcco, xArgpo, xInclo, xMo,
                                   xNo, xnodeo                       : EXTENDED;
                               VAR SatRec                            : ElSetRec);

PROCEDURE SGP4               ( whichconst : gravconsttype;
                               VAR SatRec                            : ElSetRec;
                               TSince                                : Extended;
                               VAR r,v                               : Vector );

FUNCTION  GSTIME             ( JDUT1                                 : EXTENDED ): EXTENDED;

PROCEDURE getgravconst
                             ( whichconst                            : gravconsttype;
                               VAR tumin, mu, radiusearthkm, xke, j2,
                               j3, j4, j3oj2                         : Extended );


(*     ----------------------------------------------------------------      *)

                                IMPLEMENTATION

(*     ----------------------------------------------------------------      *)

{ -----------------------------------------------------------------------------
*
*                           procedure dpper
*
*  this procedure provides deep space long period periodic contributions
*    to the mean elements.  by design, these periodics are zero at epoch.
*    this used to be dscom which included initialization, but it's really a
*    recurring function.
*
*  author        : david vallado                  719-573-2600   28 jun 2005
*
*  inputs        :
*    e3          -
*    ee2         -
*    peo         -
*    pgho        -
*    pho         -
*    pinco       -
*    plo         -
*    se2 , se3 , Sgh2, Sgh3, Sgh4, Sh2, Sh3, Si2, Si3, Sl2, Sl3, Sl4 -
*    t           -
*    xh2, xh3, xi2, xi3, xl2, xl3, xl4 -
*    zmol        -
*    zmos        -
*    ep          - eccentricity                           0.0 - 1.0
*    inclo       - inclination - needed for lyddane modification
*    nodep       - right ascension of ascending node
*    argpp       - argument of perigee
*    mp          - mean anomaly
*
*  outputs       :
*    ep          - eccentricity                           0.0 - 1.0
*    inclp       - inclination
*    nodep       - right ascension of ascending node
*    argpp       - argument of perigee
*    mp          - mean anomaly
*
*  locals        :
*    alfdp       -
*    betdp       -
*    cosip  , sinip  , cosop  , sinop  ,
*    dalf        -
*    dbet        -
*    dls         -
*    f2, f3      -
*    pe          -
*    pgh         -
*    ph          -
*    pinc        -
*    pl          -
*    sel   , ses   , sghl  , sghs  , shl   , shs   , sil   , sinzf , sis   ,
*    sll   , sls
*    xls         -
*    xnoh        -
*    zf          -
*    zm          -
*
*  coupling      :
*    none.
*
*  references    :
*    hoots, roehrich, norad spacetrack report #3 1980
*    hoots, norad spacetrack report #6 1986
*    hoots, schumacher and glover 2004
*    vallado, crawford, hujsak, kelso  2006
  ---------------------------------------------------------------------------- }

PROCEDURE DPPER(   e3    , ee2   , peo   , pgho  , pho   , pinco ,
                   plo   , se2   , se3   , sgh2  , sgh3  , sgh4  ,
                   sh2   , sh3   , si2   , si3   , sl2   , sl3   ,
                   sl4   , T     , xgh2  , xgh3  , xgh4  , xh2   ,
                   xh3   , xi2   , xi3   , xl2   , xl3   , xl4   ,
                   zmol  , zmos  ,inclo  : EXTENDED;
                   init  : CHAR;
                   VAR   EP    , Inclp   , nodep, Argpp, Mp : EXTENDED );

     { --------------------- Local Variables ------------------------ }
   CONST
     TwoPi      : EXTENDED =     2.0 * pi;    { 6.28318530717959; }
   VAR  alfdp , betdp , cosip , cosop , dalf  , dbet  , dls   ,
        f2    , f3    , pe    , pgh   , ph    , pinc  , pl    ,
        sel   , ses   , sghl  , sghs  , shll   , shs  , sil   ,
        sinip , sinop , sinzf , sis   , sll   , sls   , xls   ,
        xnoh  , zf    , zm,
        Zel   , Zes   , Znl   , Zns   : EXTENDED;
   BEGIN
     { ---------------------- Constants ----------------------------- }
     ZNS  := 1.19459E-5;
     ZES  := 0.01675;
     ZNL  := 1.5835218E-4;
     ZEL  := 0.05490;

     { ---------------- CALCULATE TIME VARYING PERIODICS ------------ }
     ZM   := ZMOS+ZNS*T;
     IF (Init = 'y') THEN
         ZM:= ZMOS;
     ZF   := ZM+2.0*ZES*SIN(ZM);
     SINZF:= SIN(ZF);
     F2   :=  0.5*SINZF*SINZF-0.25;
     F3   := -0.5*SINZF*COS(ZF);
     SES  := SE2*F2 + SE3*F3;
     SIS  := SI2*F2 + SI3*F3;
     SLS  := SL2*F2 + SL3*F3 + SL4*SINZF;
     SGHS := SGH2*F2 + SGH3*F3 + SGH4*SINZF;
     SHS  := SH2*F2 + SH3*F3;
     ZM   := ZMOL + ZNL*T;
     IF (Init = 'y') THEN
         ZM:= ZMOL;
     ZF   := ZM + 2.0*ZEL*SIN(ZM);
     SINZF:= SIN(ZF);
     F2   :=  0.5*SINZF*SINZF-0.25;
     F3   := -0.5*SINZF*COS(ZF);
     SEL  := EE2*F2 + E3*F3;
     SIL  := XI2*F2 + XI3*F3;
     SLL  := XL2*F2 + XL3*F3 + XL4*SINZF;
     SGHL := XGH2*F2 + XGH3*F3 + XGH4*SINZF;
     Shll := XH2*F2 + XH3*F3;
     PE   := SES + SEL;
     PINC := SIS + SIL;
     PL   := SLS + SLL;
     PGH  := SGHS + SGHL;
     PH   := SHS + Shll;

     IF (INIT = 'n') THEN
       BEGIN
         PE    := PE   - PEO;
         PINC  := PINC - PINCO;
         PL    := PL   - PLO;
         PGH   := PGH  - PGHO;
         PH    := PH   - PHO;
         Inclp := Inclp  + PINC;
         EP    := EP   + PE;
         SINIP := SIN(Inclp);
         COSIP := COS(Inclp);

         { ----------------- APPLY PERIODICS DIRECTLY ------------ }
         {  sgp4fix for lyddane choice
            strn3 used original inclination - this is technically feasible
            gsfc used perturbed inclination - also technically feasible
            probably best to readjust the 0.2 limit value and limit discontinuity
            0.2 rad = 11.45916 deg
            use next line for original strn3 approach and original inclination
            if (inclo >= 0.2 ) THEN
            use next line for gsfc version and perturbed inclination } 
         if (inclp >= 0.2) THEN
           BEGIN
             PH     := PH/SINIP;
             PGH    := PGH - COSIP*PH;
             Argpp  := Argpp + PGH;
             nodep  := nodep + PH;
             Mp     := Mp + PL;
           END
           ELSE
           BEGIN
             { ---- APPLY PERIODICS WITH LYDDANE MODIFICATION ---- }
             SINOP  := SIN(nodep);
             COSOP  := COS(nodep);
             ALFDP  := SINIP*SINOP;
             BETDP  := SINIP*COSOP;
             DALF   :=  PH*COSOP + PINC*COSIP*SINOP;
             DBET   := -PH*SINOP + PINC*COSIP*COSOP;
             ALFDP  := ALFDP + DALF;
             BETDP  := BETDP + DBET;
             nodep  := MODFUNC(nodep,TwoPi);
             {  sgp4fix for afspc written intrinsic functions }
             { nodep used without a trigonometric function ahead }
             if (nodep < 0.0) and (obsmode = 'a') then
                 nodep := nodep + twopi;
             XLS    := Mp + Argpp + COSIP*nodep;
             DLS    := PL + PGH - PINC*nodep*SINIP;
             XLS    := XLS + DLS;
             XNOH   := nodep;
             nodep := ATan2(ALFDP,BETDP);
             {  sgp4fix for afspc written intrinsic functions }
             { nodep used without a trigonometric function ahead }
             if (nodep < 0.0) and (obsmode = 'a') then
                 nodep := nodep + twopi;
             IF (ABS(XNOH-nodep) > PI) THEN
                IF (nodep < XNOH) THEN
                    nodep := nodep+TWOPI
                  ELSE
                    nodep := nodep-TWOPI;
             Mp   := Mp + PL;
             Argpp:=  XLS - Mp - COSIP*nodep;
           END;
       END;

(*    {$I debug1.pas }*)

  END;  { PROCEDURE DPPer }

{ -----------------------------------------------------------------------------
*
*                           procedure dscom
*
*  this procedure provides deep space common items used by both the secular
*    and periodics subroutines.  input is provided as shown. this routine
*    used to be called dpper, but the functions inside weren't well organized.
*
*  author        : david vallado                  719-573-2600   28 jun 2005
*
*  inputs        :
*    epoch       -
*    ep          - eccentricity
*    argpp       - argument of perigee
*    tc          -
*    inclp       - inclination
*    nodep       - right ascension of ascending node
*    np          - mean motion
*
*  outputs       :
*    sinim  , cosim  , sinomm , cosomm , snodm  , cnodm
*    day         -
*    e3          -
*    ee2         -
*    em          - eccentricity
*    emsq        - eccentricity squared
*    gam         -
*    peo         -
*    pgho        -
*    pho         -
*    pinco       -
*    plo         -
*    rtemsq      -
*    se2, se3         -
*    sgh2, sgh3, sgh4        -
*    sh2, sh3, si2, si3, sl2, sl3, sl4         -
*    s1, s2, s3, s4, s5, s6, s7          -
*    ss1, ss2, ss3, ss4, ss5, ss6, ss7, sz1, sz2, sz3         -
*    sz11, sz12, sz13, sz21, sz22, sz23, sz31, sz32, sz33        -
*    xgh2, xgh3, xgh4, xh2, xh3, xi2, xi3, xl2, xl3, xl4         -
*    nm          - mean motion
*    z1, z2, z3, z11, z12, z13, z21, z22, z23, z31, z32, z33         -
*    zmol        -
*    zmos        -
*
*  locals        :
*    a1, a2, a3, a4, a5, a6, a7, a8, a9, a10         -
*    betasq      -
*    cc          -
*    ctem, stem        -
*    x1, x2, x3, x4, x5, x6, x7, x8          -
*    xnodce      -
*    xnoi        -
*    zcosg  , zsing  , zcosgl , zsingl , zcosh  , zsinh  , zcoshl , zsinhl ,
*    zcosi  , zsini  , zcosil , zsinil ,
*    zx          -
*    zy          -
*
*  coupling      :
*    none.
*
*  references    :
*    hoots, roehrich, norad spacetrack report #3 1980
*    hoots, norad spacetrack report #6 1986
*    hoots, schumacher and glover 2004
*    vallado, crawford, hujsak, kelso  2006
  ---------------------------------------------------------------------------- }

PROCEDURE DSCOM( EPOCH , Ep  , Argpp, Tc  , Inclp, nodep, Np : EXTENDED;
                 VAR SNODM , CNODM , SINIM , COSIM , SINOMM, COSOMM,
                     DAY   , E3    , Ee2   , EM    , EMSQ  , GAM   ,
                     Peo   , Pgho  , Pho   , PInco , Plo   ,
                     RTemSq, Se2   , Se3   , Sgh2  , Sgh3  , Sgh4  ,
                     Sh2   , Sh3   , Si2   , Si3   , Sl2   , Sl3   ,
                     Sl4   , S1    , S2    , S3    , S4    , S5    ,
                     S6    , S7    , SS1   , SS2   , SS3   , SS4   ,
                     SS5   , SS6   , SS7   , SZ1   , SZ2   , SZ3   ,
                     SZ11  , SZ12  , SZ13  , SZ21  , SZ22  , SZ23  ,
                     SZ31  , SZ32  , SZ33  , Xgh2  , Xgh3  , Xgh4  ,
                     Xh2   , Xh3   , Xi2   , Xi3   , Xl2   , Xl3   ,
                     Xl4   , Nm    , Z1    , Z2    , Z3    , Z11   ,
                     Z12   , Z13   , Z21   , Z22   , Z23   , Z31   ,
                     Z32   , Z33   , Zmol  , Zmos : EXTENDED );

     { --------------------- Local Variables ------------------------ }
   CONST
     TwoPi      : EXTENDED =     2.0 * pi;    { 6.28318530717959; }
   VAR
     LsFlg : INTEGER;
     c1ss  , c1L   , zcosis, zsinis, zsings, zcosgs, Zes   , zel,
     a1    , a2    , a3    , a4    , a5    , a6    , a7    ,
     a8    , a9    , a10   , betasq, cc    , ctem  , stem  ,
     x1    , x2    , x3    , x4    , x5    , x6    , x7    ,
     x8    , xnodce, xnoi  , zcosg , zcosgl, zcosh , zcoshl,
     zcosi , zcosil, zsing , zsingl, zsinh , zsinhl, zsini ,
     zsinil, zx    , zy  : EXTENDED;
   BEGIN
     { -------------------------- Constants ------------------------- }
     ZES    :=  0.01675;
     ZEL    :=  0.05490;
     C1SS   :=  2.9864797E-6;
     C1L    :=  4.7968065E-7;
     ZSINIS :=  0.39785416;
     ZCOSIS :=  0.91744867;
     ZCOSGS :=  0.1945905;
     ZSINGS := -0.98088458;

     Nm     := Np;
     EM     := EP;
     SNODM  := SIN(nodep);
     CNODM  := COS(nodep);
     SINOMM := SIN(Argpp);
     COSOMM := COS(Argpp);
     SINIM  := SIN(Inclp);
     COSIM  := COS(Inclp);
     EMSQ   := EM*EM;
     BETASQ := 1.0-EMSQ;
     RTEMSQ := SQRT(BETASQ);
                                                                        
     { ----------------- INITIALIZE LUNAR SOLAR TERMS --------------- }
     PEO    := 0.0;
     PINCO  := 0.0;
     PLO    := 0.0;
     PGHO   := 0.0;
     PHO    := 0.0;
     DAY    := EPOCH + 18261.5 + TC/1440.0;
     XNODCE := MODFUNC(4.5236020 - 9.2422029E-4*DAY,TwoPi);
     STEM   := SIN(XNODCE);
     CTEM   := COS(XNODCE);
     ZCOSIL := 0.91375164 - 0.03568096*CTEM;
     ZSINIL := SQRT(1.0 - ZCOSIL*ZCOSIL);
     ZSINHL := 0.089683511*STEM / ZSINIL;
     ZCOSHL := SQRT(1.0 - ZSINHL*ZSINHL);
     GAM    := 5.8351514 + 0.0019443680*DAY;
     ZX     := 0.39785416*STEM/ZSINIL;
     ZY     := ZCOSHL*CTEM + 0.91744867*ZSINHL*STEM;
     ZX     := ATan2(ZX,ZY);
     ZX     := GAM + ZX - XNODCE;
     ZCOSGL := COS(ZX);
     ZSINGL := SIN(ZX);

     { ------------------------- DO SOLAR TERMS --------------------- }
     ZCOSG := ZCOSGS;
     ZSING := ZSINGS;
     ZCOSI := ZCOSIS;
     ZSINI := ZSINIS;
     ZCOSH := CNODM;
     ZSINH := SNODM;
     CC    := C1SS;
     XNOI  := 1.0 / Nm;

     FOR LSFlg:= 1 to 2 DO
       BEGIN
         A1 :=   ZCOSG*ZCOSH + ZSING*ZCOSI*ZSINH;
         A3 :=  -ZSING*ZCOSH + ZCOSG*ZCOSI*ZSINH;
         A7 :=  -ZCOSG*ZSINH + ZSING*ZCOSI*ZCOSH;
         A8 :=   ZSING*ZSINI;
         A9 :=   ZSING*ZSINH + ZCOSG*ZCOSI*ZCOSH;
         A10:=   ZCOSG*ZSINI;
         A2 :=   COSIM*A7 + SINIM*A8;
         A4 :=   COSIM*A9 + SINIM*A10;
         A5 :=  -SINIM*A7 + COSIM*A8;
         A6 :=  -SINIM*A9 + COSIM*A10;

         X1 :=  A1*COSOMM + A2*SINOMM;
         X2 :=  A3*COSOMM + A4*SINOMM;
         X3 := -A1*SINOMM + A2*COSOMM;
         X4 := -A3*SINOMM + A4*COSOMM;
         X5 :=  A5*SINOMM;
         X6 :=  A6*SINOMM;
         X7 :=  A5*COSOMM;
         X8 :=  A6*COSOMM;

         Z31:= 12.0*X1*X1 - 3.0*X3*X3;
         Z32:= 24.0*X1*X2 - 6.0*X3*X4;
         Z33:= 12.0*X2*X2 - 3.0*X4*X4;
         Z1 :=  3.0* (A1*A1 + A2*A2) + Z31*EMSQ;
         Z2 :=  6.0* (A1*A3 + A2*A4) + Z32*EMSQ;
         Z3 :=  3.0* (A3*A3 + A4*A4) + Z33*EMSQ;
         Z11:= -6.0*A1*A5 + EMSQ* (-24.0*X1*X7-6.0*X3*X5);
         Z12:= -6.0* (A1*A6 + A3*A5) + EMSQ*
             ( -24.0*(X2*X7+X1*X8) - 6.0*(X3*X6+X4*X5) );
         Z13:= -6.0*A3*A6 + EMSQ*(-24.0*X2*X8 - 6.0*X4*X6);
         Z21:=  6.0*A2*A5 + EMSQ*(24.0*X1*X5-6.0*X3*X7);
         Z22:=  6.0* (A4*A5 + A2*A6) + EMSQ*
             (  24.0*(X2*X5+X1*X6) - 6.0*(X4*X7+X3*X8) );
         Z23:=  6.0*A4*A6 + EMSQ*(24.0*X2*X6 - 6.0*X4*X8);
         Z1 := Z1 + Z1 + BETASQ*Z31;
         Z2 := Z2 + Z2 + BETASQ*Z32;
         Z3 := Z3 + Z3 + BETASQ*Z33;
         S3 := CC*XNOI;
         S2 := -0.5*S3 / RTEMSQ;
         S4 := S3*RTEMSQ;
         S1 := -15.0*EM*S4;
         S5 := X1*X3 + X2*X4;
         S6 := X2*X3 + X1*X4;
         S7 := X2*X4 - X1*X3;

         { ----------------------- DO LUNAR TERMS ------------------- }
         IF (LSFLG = 1) THEN
           BEGIN
             SS1   := S1;
             SS2   := S2;
             SS3   := S3;
             SS4   := S4;
             SS5   := S5;
             SS6   := S6;
             SS7   := S7;
             SZ1   := Z1;
             SZ2   := Z2;
             SZ3   := Z3;
             SZ11  := Z11;
             SZ12  := Z12;
             SZ13  := Z13;
             SZ21  := Z21;
             SZ22  := Z22;
             SZ23  := Z23;
             SZ31  := Z31;
             SZ32  := Z32;
             SZ33  := Z33;
             ZCOSG := ZCOSGL;
             ZSING := ZSINGL;
             ZCOSI := ZCOSIL;
             ZSINI := ZSINIL;
             ZCOSH := ZCOSHL*CNODM+ZSINHL*SNODM;
             ZSINH := SNODM*ZCOSHL-CNODM*ZSINHL;
             CC    := C1L;
           END;
       END;  { For }

     ZMOL  := MODFUNC( 4.7199672 + 0.22997150*DAY-GAM,TwoPi );
     ZMOS  := MODFUNC( 6.2565837 + 0.017201977*DAY,TwoPi );
                                                                        
     { ------------------------ DO SOLAR TERMS ---------------------- }
     SE2 :=  2.0*SS1*SS6;
     SE3 :=  2.0*SS1*SS7;
     SI2 :=  2.0*SS2*SZ12;
     SI3 :=  2.0*SS2*(SZ13-SZ11);
     SL2 := -2.0*SS3*SZ2;
     SL3 := -2.0*SS3*(SZ3-SZ1);
     SL4 := -2.0*SS3*(-21.0-9.0*EMSQ)*ZES;
     SGH2:=  2.0*SS4*SZ32;
     SGH3:=  2.0*SS4*(SZ33-SZ31);
     SGH4:=-18.0*SS4*ZES;
     SH2 := -2.0*SS2*SZ22;
     SH3 := -2.0*SS2*(SZ23-SZ21);
                                                                        
     { ------------------------ DO LUNAR TERMS ---------------------- }
     EE2 :=  2.0*S1*S6;
     E3  :=  2.0*S1*S7;
     XI2 :=  2.0*S2*Z12;
     XI3 :=  2.0*S2*(Z13-Z11);
     XL2 := -2.0*S3*Z2;
     XL3 := -2.0*S3*(Z3-Z1);
     XL4 := -2.0*S3*(-21.0-9.0*EMSQ)*ZEL;
     XGH2:=  2.0*S4*Z32;
     XGH3:=  2.0*S4*(Z33-Z31);
     XGH4:=-18.0*S4*ZEL;
     XH2 := -2.0*S2*Z22;
     XH3 := -2.0*S2*(Z23-Z21);

(*    {$I debug2.pas }*)

   END; { PROCEDURE DSCom }

{ -----------------------------------------------------------------------------
*
*                           procedure dsinit
*
*  this procedure provides deep space contributions to mean motion dot due
*    to geopotential resonance with half day and one day orbits.
*
*  author        : david vallado                  719-573-2600   28 jun 2005
*
*  inputs        :
*    Cosim, Sinim-
*    Emsq        - Eccentricity squared
*    Argpo       - Argument of Perigee
*    S1, S2, S3, S4, S5      -
*    Ss1, Ss2, Ss3, Ss4, Ss5 -
*    Sz1, Sz3, Sz11, Sz13, Sz21, Sz23, Sz31, Sz33 -
*    T           - Time
*    Tc          -
*    GSTo        - Greenwich sidereal time                   rad
*    Mo          - Mean Anomaly
*    MDot        - Mean Anomaly dot (rate)
*    No          - Mean Motion
*    nodeo       - right ascension of ascending node
*    nodeDot     - right ascension of ascending node dot (rate)
*    XPIDOT      -
*    Z1, Z3, Z11, Z13, Z21, Z23, Z31, Z33 -
*    Eccm        - Eccentricity
*    Argpm       - Argument of perigee
*    Inclm       - Inclination
*    Mm          - Mean Anomaly
*    Xn          - Mean Motion
*    nodem      - right ascension of ascending node
*
*  outputs       :
*    em          - eccentricity
*    argpm       - argument of perigee
*    inclm       - inclination
*    mm          - mean anomaly
*    nm          - mean motion
*    nodem      - right ascension of ascending node
*    irez        - flag for resonance           0-none, 1-one day, 2-half day
*    atime       -
*    d2201, d2211, d3210, d3222, d4410, d4422, d5220, d5232, d5421, d5433    -
*    dedt        -
*    didt        -
*    dmdt        -
*    dndt        -
*    dnodt       -
*    domdt       -
*    del1, del2, del3        -
*    Ses  , Sghl , Sghs , Sgs  , Shl  , Shs  , Sis  , Sls
*    theta       -
*    xfact       -
*    xlamo       -
*    xli         -
*    xni
*
*  locals        :
*    ainv2       -
*    aonv        -
*    cosisq      -
*    eoc         -
*    f220, f221, f311, f321, f322, f330, f441, f442, f522, f523, f542, f543  -
*    g200, g201, g211, g300, g310, g322, g410, g422, g520, g521, g532, g533  -
*    sini2       -
*    temp        -
*    temp1       -
*    theta       -
*    xno2        -
*
*  coupling      :
*    getgravconst
*
*  references    :
*    hoots, roehrich, norad spacetrack report #3 1980
*    hoots, norad spacetrack report #6 1986
*    hoots, schumacher and glover 2004
*    vallado, crawford, hujsak, kelso  2006
  ---------------------------------------------------------------------------- }

PROCEDURE DSINIT( whichconst                                    : gravconsttype;
                  Cosim , Emsq  , Argpo , S1    , S2    , S3    ,
                  S4    , S5    , Sinim , Ss1   , Ss2   , Ss3   ,
                  Ss4   , Ss5   , Sz1   , Sz3   , Sz11  , Sz13  ,
                  Sz21  , Sz23  , Sz31  , Sz33  , T     , Tc    ,
                  GSTo  , Mo    , MDot  , No    , nodeo, nodeDot,
                  XPIDOT, Z1    , Z3    , Z11   , Z13   , Z21   ,
                  Z23   , Z31   , Z33   ,Ecco   , Eccsq : EXTENDED;
                  VAR EM    , Argpm, Inclm , Mm , Nm    , nodem : EXTENDED;
                  VAR IREZ : INTEGER;
                  VAR Atime , D2201 , D2211 , D3210 , D3222 ,
                      D4410 , D4422 , D5220 , D5232 , D5421 , D5433 ,
                      Dedt  , Didt  , DMDT  , DNDT  , DNODT , DOMDT ,
                      Del1  , Del2  , Del3  , Xfact , Xlamo , Xli   ,
                      Xni: EXTENDED );
     { --------------------- Local Variables ------------------------ }
   CONST
     TwoPi      : EXTENDED =     2.0 * pi;    { 6.28318530717959; }
   VAR
      ainv2 , aonv  , cosisq, eoc   , f220  , f221  , f311  ,
      f321  , f322  , f330  , f441  , f442  , f522  , f523  ,
      f542  , f543  , g200  , g201  , g211  , g300  , g310  ,
      g322  , g410  , g422  , g520  , g521  , g532  , g533  ,
      ses   , sgs   , sghl  , sghs  , shs   , shll   , sis   ,
      sini2 , sls   , temp  , temp1 , Theta , xno2,
      Q22   , Q31   , Q33   , ROOT22, ROOT44, ROOT54,
      RPTim , Root32, Root52, X2o3  , XKe   , Znl   ,
      emo,   Zns, tumin, mu, radiusearthkm, j2, j3, j4, j3oj2 : EXTENDED;

   BEGIN
        Q22    := 1.7891679E-6;
        Q31    := 2.1460748E-6;
        Q33    := 2.2123015E-7;
        ROOT22 := 1.7891679E-6;
        ROOT44 := 7.3636953E-9;
        ROOT54 := 2.1765803E-9;
        RPTim  := 4.37526908801129966E-3;  { this equates to 7.29211514668855e-5 rad/sec }
        Root32 := 3.7393792E-7;
        Root52 := 1.1428639E-7;
        X2o3   := 2.0 / 3.0;
        ZNL    := 1.5835218E-4;
        ZNS    := 1.19459E-5;
 
        { sgp4fix identify constants and allow alternate values }
        getgravconst( whichconst, tumin, mu, radiusearthkm, xke, j2, j3, j4, j3oj2 );

        { -------------------- Deep Space Initialization ------------ }
        IREZ := 0;
        IF ((Nm<0.0052359877) AND (Nm>0.0034906585)) THEN
            IREZ := 1;
        IF ((Nm>=8.26E-3) AND (Nm<=9.24E-3) AND (EM>=0.5)) THEN
            IREZ := 2;
                                                                        
        { ------------------------ DO SOLAR TERMS ------------------- }
        SES  :=  SS1*ZNS*SS5;
        SIS  :=  SS2*ZNS*(SZ11 + SZ13);
        SLS  := -ZNS*SS3*(SZ1 + SZ3 - 14.0 - 6.0*EMSQ);
        SGHS :=  SS4*ZNS*(SZ31 + SZ33 - 6.0);
        SHS  := -ZNS*SS2*(SZ21 + SZ23);
        { sgp4fix for 180 deg incl }
        if ((inclm < 5.2359877e-2) or (inclm > pi - 5.2359877e-2)) THEN
            SHS := 0.0;
        IF (SINIM<>0.0) THEN
            SHS := SHS/SINIM;
        SGS  := SGHS-COSIM*SHS;
                                                                        
        { ------------------------- DO LUNAR TERMS ------------------ }
        DEDT := SES + S1*ZNL*S5;
        DIDT := SIS + S2*ZNL*(Z11 + Z13);
        DMDT := SLS - ZNL*S3*(Z1 + Z3 - 14.0 - 6.0*EMSQ);
        SGHL := S4*ZNL*(Z31 + Z33 - 6.0);
        shll := -ZNL*S2*(Z21 + Z23);
        { sgp4fix for 180 deg incl }
        if ((inclm < 5.2359877e-2) or (inclm > pi - 5.2359877e-2)) THEN
            shll := 0.0;
        DOMDT:= SGS+SGHL;
        DNODT:= SHS;
        IF (SINIM <> 0.0) THEN
          BEGIN
            DOMDT := DOMDT-COSIM/SINIM*shll;
            DNODT := DNODT+shll/SINIM;
          END;
                                                                        
        { ----------- CALCULATE DEEP SPACE RESONANCE EFFECTS -------- }
        DNDT  := 0.0;
        THETA := MODFUNC(GSTo + TC*RPTIM,TwoPi);
        EM    := EM + DEDT*T;
        Inclm := Inclm + DIDT*T;
        Argpm := Argpm + DOMDT*T;
        nodem := nodem + DNODT*T;
        Mm    := Mm + DMDT*T;
        {  sgp4fix for negative inclinations
           the following if statement should be commented out }
        {   IF (Inclm < 0.0) THEN
             BEGIN
               Inclm  := -Incl;
               Argpm  := Argpm-PI;
               nodem  := nodem+PI;
             END;  }

        { -------------- Initialize the resonance terms ------------- }
        IF (IRez <> 0) THEN
          BEGIN
            AONV := POWER( Nm/XKE,X2O3 );

        { ---------- GEOPOTENTIAL RESONANCE FOR 12 HOUR ORBITS ------ }
        IF (IREZ = 2) THEN
          BEGIN
            COSISQ := COSIM*COSIM;
            emo    := em;
            em     := Ecco;
            emsq   := Eccsq;
            EOC    := EM*EMSQ;
            G201   := -0.306-(EM-0.64)*0.440;

            IF (EM<=0.65) THEN
              BEGIN
                G211 :=   3.616 -  13.2470*EM +  16.2900*EMSQ;
                G310 := -19.302 + 117.3900*EM - 228.4190*EMSQ +  156.5910*EOC;
                G322 := -18.9068+ 109.7927*EM - 214.6334*EMSQ +  146.5816*EOC;
                G410 := -41.122 + 242.6940*EM - 471.0940*EMSQ +  313.9530*EOC;
                G422 :=-146.407 + 841.8800*EM - 1629.014*EMSQ + 1083.4350*EOC;
                G520 :=-532.114 + 3017.977*EM - 5740.032*EMSQ + 3708.2760*EOC;
              END
              ELSE
              BEGIN
                G211 :=  -72.099 +   331.819*EM -   508.738*EMSQ +   266.724*EOC;
                G310 := -346.844 +  1582.851*EM -  2415.925*EMSQ +  1246.113*EOC;
                G322 := -342.585 +  1554.908*EM -  2366.899*EMSQ +  1215.972*EOC;
                G410 :=-1052.797 +  4758.686*EM -  7193.992*EMSQ +  3651.957*EOC;
                G422 :=-3581.690 + 16178.110*EM - 24462.770*EMSQ + 12422.520*EOC;
                IF (EM>0.715) THEN
                    G520 :=-5149.66 + 29936.92*EM - 54087.36*EMSQ + 31324.56*EOC
                  ELSE
                    G520 := 1464.74 -  4664.75*EM +  3763.64*EMSQ;
              END;

            IF (EM<0.7) THEN
              BEGIN
                G533 := -919.22770 + 4988.6100*EM - 9064.7700*EMSQ + 5542.21*EOC;
                G521 := -822.71072 + 4568.6173*EM - 8491.4146*EMSQ + 5337.524*EOC;
                G532 := -853.66600 + 4690.2500*EM - 8624.7700*EMSQ + 5341.4*EOC;
              END
              ELSE
              BEGIN
                G533 :=-37995.780 + 161616.52*EM - 229838.20*EMSQ + 109377.94*EOC;
                G521 :=-51752.104 + 218913.95*EM - 309468.16*EMSQ + 146349.42*EOC;
                G532 :=-40023.880 + 170470.89*EM - 242699.48*EMSQ + 115605.82*EOC;
              END;

            SINI2 :=  SINIM*SINIM;
            F220  :=  0.75* (1.0+2.0*COSIM+COSISQ);
            F221  :=  1.5*SINI2;
            F321  :=  1.875*SINIM * (1.0-2.0*COSIM-3.0*COSISQ);
            F322  := -1.875*SINIM * (1.0+2.0*COSIM-3.0*COSISQ);
            F441  := 35.0*SINI2*F220;
            F442  := 39.3750*SINI2*SINI2;
            F522  :=  9.84375*SINIM * (SINI2* (1.0-2.0*COSIM-
                    5.0*COSISQ)+0.33333333 * (-2.0+4.0*COSIM+6.0*COSISQ) );
            F523  :=  SINIM * (4.92187512*SINI2 * (-2.0-4.0*COSIM+
                    10.0*COSISQ) + 6.56250012* (1.0+2.0*COSIM-3.0*COSISQ));
            F542  :=  29.53125*SINIM * (2.0-8.0*COSIM+COSISQ*
                    (-12.0+8.0*COSIM+10.0*COSISQ) );
            F543  := 29.53125*SINIM * (-2.0-8.0*COSIM+COSISQ*
                    (12.0+8.0*COSIM-10.0*COSISQ) );
            XNO2   :=  Nm * Nm;
            AINV2  :=  AONV * AONV;
            TEMP1  :=  3.0*XNO2*AINV2;
            TEMP   :=  TEMP1*ROOT22;
            D2201  :=  TEMP*F220*G201;
            D2211  :=  TEMP*F221*G211;
            TEMP1  :=  TEMP1*AONV;
            TEMP   :=  TEMP1*ROOT32;
            D3210  :=  TEMP*F321*G310;
            D3222  :=  TEMP*F322*G322;
            TEMP1  :=  TEMP1*AONV;
            TEMP   :=  2.0*TEMP1*ROOT44;
            D4410  :=  TEMP*F441*G410;
            D4422  :=  TEMP*F442*G422;
            TEMP1  :=  TEMP1*AONV;
            TEMP   :=  TEMP1*ROOT52;
            D5220  :=  TEMP*F522*G520;
            D5232  :=  TEMP*F523*G532;
            TEMP   :=  2.0*TEMP1*ROOT54;
            D5421  :=  TEMP*F542*G521;
            D5433  :=  TEMP*F543*G533;
            XLAMO  :=  MODFUNC(Mo+nodeo+nodeo-THETA-THETA,TwoPi);
            XFACT  :=  MDot + DMDT + 2.0 * (nodeDot+DNODT-RPTIM) - No;
            em     := emo;
          END;

        { ---------------- SYNCHRONOUS RESONANCE TERMS -------------- }
        IF Irez = 1 THEN
          BEGIN
            G200  := 1.0 + EMSQ * (-2.5+0.8125*EMSQ);
            G310  := 1.0 + 2.0*EMSQ;
            G300  := 1.0 + EMSQ * (-6.0+6.60937*EMSQ);
            F220  := 0.75 * (1.0+COSIM) * (1.0+COSIM);
            F311  := 0.9375*SINIM*SINIM*
                 (1.0+3.0*COSIM) - 0.75*(1.0+COSIM);
            F330  := 1.0+COSIM;
            F330  := 1.875*F330*F330*F330;
            DEL1  := 3.0*Nm*Nm*AONV*AONV;
            DEL2  := 2.0*DEL1*F220*G200*Q22;
            DEL3  := 3.0*DEL1*F330*G300*Q33*AONV;
            DEL1  := DEL1*F311*G310*Q31*AONV;
            XLAMO := MODFUNC(Mo+nodeo+Argpo-THETA,TwoPi);
            XFACT := MDot + XPIDOT - RPTIM + DMDT + DOMDT + DNODT - No;
          END;

        { ------------ FOR SGP4, INITIALIZE THE INTEGRATOR ---------- }
            XLI   := XLAMO;
            XNI   := No;
            ATIME := 0.0;
            Nm    := No + DNDT;
        END;

(*    {$I debug3.pas }*)

   END; { PROCEDURE DSInit }

{ -----------------------------------------------------------------------------
*
*                           procedure dspace
*
*  this procedure provides deep space contributions to mean elements for
*    perturbing third body.  these effects have been averaged over one
*    revolution of the sun and moon.  for earth resonance effects, the
*    effects have been averaged over no revolutions of the satellite.
*    (mean motion)
*
*  author        : david vallado                  719-573-2600   28 jun 2005
*
*  inputs        :
*    d2201, d2211, d3210, d3222, d4410, d4422, d5220, d5232, d5421, d5433       -
*    dedt        -
*    del1, del2, del3  -
*    didt        -
*    dmdt        -
*    dnodt       -
*    domdt       -
*    irez        - flag for resonance           0-none, 1-one day, 2-half day
*    argpo       - argument of perigee
*    argpdot     - argument of perigee dot (rate)
*    t           - time
*    tc          -
*    gsto        - gst
*    xfact       -
*    xlamo       -
*    no          - mean motion
*    atime       -
*    em          - eccentricity
*    ft          -
*    argpm       - argument of perigee
*    inclm       - inclination
*    xli         -
*    mm          - mean anomaly
*    xni         - mean motion
*    nodem       - right ascension of ascending node
*
*  outputs       :
*    atime       -
*    em          - eccentricity
*    argpm       - argument of perigee
*    inclm       - inclination
*    xli         -
*    mm          - mean anomaly
*    xni         -
*    nodem       - right ascension of ascending node
*    dndt        -
*    nm          - mean motion
*
*  locals        :
*    delt        -
*    ft          -
*    theta       -
*    x2li        -
*    x2omi       -
*    xl          -
*    xldot       -
*    xnddt       -
*    xndt        -
*    xomi        -
*
*  coupling      :
*    none        -
*
*  references    :
*    hoots, roehrich, norad spacetrack report #3 1980
*    hoots, norad spacetrack report #6 1986
*    hoots, schumacher and glover 2004
*    vallado, crawford, hujsak, kelso  2006
  ---------------------------------------------------------------------------- }

PROCEDURE DSPACE ( IRez : INTEGER;
                   D2201 , D2211 , D3210 , D3222 , D4410 ,
                   D4422 , D5220 , D5232 , D5421 , D5433 , Dedt  ,
                   Del1  , Del2  , Del3  , Didt  , Dmdt  , Dnodt ,
                   Domdt , Argpo , ArgpDot , T   , TC    , GSTo  ,
                   Xfact , Xlamo , No   : EXTENDED;
                   VAR  Atime , EM    , Argpm, Inclm , Xli   , Mm  ,
                        XNi   , nodem, Dndt  , Nm        : EXTENDED  );
     { --------------------- Local Variables ------------------------ }
   CONST
     TwoPi      : EXTENDED =     2.0 * pi;    { 6.28318530717959; }
   VAR
     iretn , iret : INTEGER;
     Delt  , Ft    , theta , x2li  , x2omi , xl    , xldot ,
     xnddt , xndt  , xomi,
     G22   , G32   , G44   , G52   , G54   , Fasx2 ,
     Fasx4 , Fasx6 , RPtim , Step2 , Stepn , Stepp : EXTENDED;
   BEGIN
     { ------------------------- Constants -------------------------- }
     FASX2 := 0.13130908;
     FASX4 := 2.8843198;
     FASX6 := 0.37448087;
     G22   := 5.7686396;
     G32   := 0.95240898;
     G44   := 1.8014998;
     G52   := 1.0508330;
     G54   := 4.4108898;
     RPTIM := 4.37526908801129966E-3;
     STEPP :=    720.0;
     STEPN :=   -720.0;
     STEP2 := 259200.0;

     { ----------- CALCULATE DEEP SPACE RESONANCE EFFECTS ----------- }
     DNDT  := 0.0;
     THETA := MODFUNC(GSTo + TC*RPTIM,TwoPi);
     EM    := EM + DEDT*T;
     Inclm := Inclm + DIDT*T;
     Argpm := Argpm + DOMDT*T;
     nodem := nodem + DNODT*T;
     Mm    := Mm + DMDT*T;

     {  sgp4fix for negative inclinations
        the following if statement should be commented out }
     {IF (Inclm < 0.0) THEN
       BEGIN
         Inclm  := -Inclm;
         Argpm  := Argpm-PI;
         nodem  := nodem+PI;
       END; }


     { - UPDATE RESONANCES : NUMERICAL (EULER-MACLAURIN) INTEGRATION - }
     { ------------------------- EPOCH RESTART ---------------------- }

     {   sgp4fix for propagator problems
         the following integration works for negative time steps and periods
         the specific changes are unknown because the original code was so convoluted }

     { sgp4fix take out atime = 0.0 and fix for faster operation }
     ft   := 0.0;

     IF (IREZ <> 0) THEN
       BEGIN
         { sgp4fix streamline check }
         IF ( (ATIME = 0.0) or (T * atime <= 0.0) or (abs(T) < abs(ATIME)) ) THEN
           BEGIN
             ATIME := 0.0;
             XNI   := No;
             XLI   := XLAMO;
           END;
         IF (T > 0.0) THEN
             DELT := STEPp
           ELSE
             Delt := Stepn;

         iretn:=381; { added for do loop }
         iret :=  0; { added for loop    }
         WHILE (IRetn = 381) DO
           BEGIN
             { ------------------- DOT TERMS CALCULATED ------------- }
             { ----------- NEAR - SYNCHRONOUS RESONANCE TERMS ------- }
             IF (IREZ <> 2) THEN
               BEGIN
                 XNDT  := DEL1*SIN(XLI-FASX2) + DEL2*SIN(2.0*(XLI-FASX4)) +
                        DEL3*SIN(3.0*(XLI-FASX6));
                 XLDOT := XNI + XFACT;
                 XNDDT := DEL1*COS(XLI-FASX2) +
                        2.0*DEL2*COS(2.0*(XLI-FASX4)) +
                        3.0*DEL3*COS(3.0*(XLI-FASX6));
                 XNDDT := XNDDT*XLDOT;
               END
               ELSE
               BEGIN

                 { --------- NEAR - HALF-DAY RESONANCE TERMS -------- }
                 XOMI := Argpo + ArgpDot*ATIME;
                 X2OMI:= XOMI + XOMI;
                 X2LI := XLI + XLI;
                 XNDT := D2201*SIN(X2OMI+XLI-G22) + D2211*SIN(XLI-G22) +
                       D3210*SIN(XOMI+XLI-G32)  + D3222*SIN(-XOMI+XLI-G32)+
                       D4410*SIN(X2OMI+X2LI-G44)+ D4422*SIN(X2LI-G44) +
                       D5220*SIN(XOMI+XLI-G52)  + D5232*SIN(-XOMI+XLI-G52)+
                       D5421*SIN(XOMI+X2LI-G54) + D5433*SIN(-XOMI+X2LI-G54);
                 XLDOT := XNI+XFACT;
                 XNDDT := D2201*COS(X2OMI+XLI-G22) + D2211*COS(XLI-G22) +
                        D3210*COS(XOMI+XLI-G32) + D3222*COS(-XOMI+XLI-G32)+
                        D5220*COS(XOMI+XLI-G52) + D5232*COS(-XOMI+XLI-G52)+
                        2.0*(D4410*COS(X2OMI+X2LI-G44) +
                        D4422*COS(X2LI-G44) + D5421*COS(XOMI+X2LI-G54) +
                        D5433*COS(-XOMI+X2LI-G54));
                 XNDDT := XNDDT*XLDOT;
               END; { IF Irez }
             { ----------------------- INTEGRATOR ------------------- }
             { sgp4fix move end checks to end of routine }
             IF (ABS(T-ATIME) >= STEPP) THEN
               BEGIN
                 IRET  := 0;
                 IRETN := 381;
               END
               ELSE
               BEGIN
                 FT    := T-ATIME;
                 IRETN := 0;
               END;

             IF (IRETN = 381) THEN
               BEGIN
                 XLI   := XLI + XLDOT*DELT + XNDT*STEP2;
                 XNI   := XNI + XNDT*DELT + XNDDT*STEP2;
                 ATIME := ATIME + DELT;
               END;
           END; { While }

         Nm := XNI+XNDT*FT+XNDDT*FT*FT*0.5;
         XL := XLI+XLDOT*FT+XNDT*FT*FT*0.5;
         IF (IREZ <> 1) THEN
           BEGIN
             Mm   := XL-2.0*nodem+2.0*THETA;
             DNDT := Nm-No;
           END
           ELSE
           BEGIN
             Mm   := XL-nodem-Argpm+THETA;
             DNDT := Nm-No;
           END;

         Nm := No+DNDT;
       END;  { IF Irez <> 0 }

(*    {$I debug4.pas }*)

   END;  { PROCEDURE DSpace }

{ -----------------------------------------------------------------------------
*
*                           procedure initl
*
*  this procedure initializes the spg4 propagator. all the initialization is
*    consolidated here instead of having multiple loops inside other routines.
*
*  author        : david vallado                  719-573-2600   28 jun 2005
*
*  inputs        :
*    ecco        - eccentricity                           0.0 - 1.0
*    epoch       - epoch time in days from jan 0, 1950. 0 hr
*    inclo       - inclination of satellite
*    no          - mean motion of satellite
*    satn        - satellite number
*
*  outputs       :
*    ainv        - 1.0 / a
*    ao          - semi major axis
*    con41       -
*    con42       - 1.0 - 5.0 cos(i)
*    cosio       - cosine of inclination
*    cosio2      - cosio squared
*    eccsq       - eccentricity squared
*    method      - flag for deep space                    'd', 'n'
*    omeosq      - 1.0 - ecco * ecco
*    posq        - semi-parameter squared
*    rp          - radius of perigee
*    rteosq      - square root of (1.0 - ecco*ecco)
*    sinio       - sine of inclination
*    gsto        - gst at time of observation               rad
*    no          - mean motion of satellite
*
*  locals        :
*    ak          -
*    d1          -
*    del         -
*    adel        -
*    po          -
*
*  coupling      :
*    getgravconst
*    gstime      - find greenwich sidereal time from the julian date
*
*  references    :
*    hoots, roehrich, norad spacetrack report #3 1980
*    hoots, norad spacetrack report #6 1986
*    hoots, schumacher and glover 2004
*    vallado, crawford, hujsak, kelso  2006
  ---------------------------------------------------------------------------- }

PROCEDURE INITL              ( Satn                                  : Longint;
                               whichconst                            : gravconsttype;
                               Ecco, EPOCH, Inclo                    : EXTENDED;
                               VAR No                                : EXTENDED;
                               VAR Method                            : CHAR;
                               VAR AINV  , AO    , CON41 , CON42 , COSIO ,
                                   COSIO2, EccSQ , OMEOSQ, POSQ  ,
                                   rp    , RTEOSQ, SINIO , GSTo      : EXTENDED );
     { --------------------- Local Variables ------------------------ }
   CONST
     TwoPi      : EXTENDED =     2.0 * pi;    { 6.28318530717959; }
   VAR
     ak, d1, del, adel, po, X2o3, J2, XKE, tumin, mu, radiusearthkm,
     j3, j4, j3oj2  : EXTENDED;
     { sgp4fix use old way of finding gst }
     ds70 : extended;
     ts70, tfrac, c1, thgr70, fk5r, c1p2p, thgr, thgro : extended;
   BEGIN
      { -------------------- WGS-72 EARTH CONSTANTS ----------------- }
        X2o3  := 2.0/3.0;

        { sgp4fix identify constants and allow alternate values }
        getgravconst( whichconst, tumin, mu, radiusearthkm, xke, j2, j3, j4, j3oj2 );   

      { ------------- CALCULATE AUXILLARY EPOCH QUANTITIES ---------- }
        EccSQ := Ecco*Ecco;
        OMEOSQ:= 1.0 - EccSQ;
        RTEOSQ:= SQRT(OMEOSQ);
        COSIO := COS(Inclo);
        COSIO2:= COSIO*COSIO;

      { ------------------ UN-KOZAI THE MEAN MOTION ----------------- }
        { make sure .elm file has correct line endings! }
        AK   := POWER( XKE/No,X2O3 );
        D1   := 0.75*J2* (3.0*COSIO2-1.0) / (RTEOSQ*OMEOSQ);
        DEL  := D1/(AK*AK);
        ADEL := AK * ( 1.0 - DEL*DEL - DEL*
                     (1.0/3.0 + 134.0*DEL*DEL / 81.0) );
        DEL  := D1/(ADEL*ADEL);
        No   := No/(1.0 + DEL);

        AO   := POWER( XKE/No,X2O3 );
        SINIO:= SIN(Inclo);
        PO   := AO*OMeoSQ;
        CON42:= 1.0-5.0*COSIO2;
        CON41:= -CON42-COSIO2-COSIO2;
        AINV := 1.0/AO;
        POSQ := PO*PO;
        rp   := AO*(1.0-Ecco);
        METHOD := 'n';

        { sgp4fix modern approach to finding sidereal time }
        if obsmode <> 'a' THEN   
            GSTo:= GSTIME( Epoch + 2433281.5 )
          ELSE  
          BEGIN
            { sgp4fix use old way of finding gst           }
            { count integer number of days from 0 jan 1970 }
            ts70  := epoch - 7305.0;
            ds70 := trunc(ts70 + 1.0e-8);
            tfrac := ts70 - ds70;
            { find greenwich location at epoch }
            c1    := 1.72027916940703639e-2;
            thgr70:= 1.7321343856509374;
            fk5r  := 5.07551419432269442e-15;
            c1p2p := c1 + twopi;
            gsto  := modfunc( thgr70 + c1*ds70 + c1p2p*tfrac + ts70*ts70*fk5r, twopi);
          END;
        if ( gsto < 0.0 ) then
            gsto := gsto + twopi;


(*    {$I debug5.pas }*)

   END; { PROCEDURE Initl }

{ -----------------------------------------------------------------------------
*
*                             procedure sgp4init
*
*  this procedure initializes variables for sgp4.
*
*  author        : david vallado                  719-573-2600   28 jun 2005
*
*  inputs        :
*    satn        - satellite number
*    bstar       - sgp4 type drag coefficient              kg/m2er
*    ecco        - eccentricity
*    epoch       - epoch time in days from jan 0, 1950. 0 hr
*    argpo       - argument of perigee (output if ds)
*    inclo       - inclination
*    mo          - mean anomaly (output if ds)
*    no          - mean motion
*    nodeo       - right ascension of ascending node
*
*  outputs       :
*    satrec      - common values for subsequent calls
*    return code - non-zero on error.
*
*  locals        :
*    CNODM  , SNODM  , COSIM  , SINIM  , COSOMM , SINOMM
*    Cc1sq  , Cc2    , Cc3
*    Coef   , Coef1
*    cosio4      -
*    day         -
*    dndt        -
*    em          - eccentricity
*    emsq        - eccentricity squared
*    eeta        -
*    etasq       -
*    gam         -
*    argpm       - argument of perigee
*    ndem        -
*    inclm       - inclination
*    mm          - mean anomaly
*    nm          - mean motion
*    perige      - perigee
*    pinvsq      -
*    psisq       -
*    qzms24      -
*    rtemsq      -
*    s1, s2, s3, s4, s5, s6, s7          -
*    sfour       -
*    ss1, ss2, ss3, ss4, ss5, ss6, ss7         -
*    sz1, sz2, sz3
*    sz11, sz12, sz13, sz21, sz22, sz23, sz31, sz32, sz33        -
*    tc          -
*    temp        -
*    temp1, temp2, temp3       -
*    tsi         -
*    xpidot      -
*    xhdot1      -
*    z1, z2, z3          -
*    z11, z12, z13, z21, z22, z23, z31, z32, z33         -
*
*  coupling      :
*    getgravconst
*    initl       -
*    dscom       -
*    dpper       -
*    dsinit      -
*    sgp4        -
*
*  references    :
*    hoots, roehrich, norad spacetrack report #3 1980
*    hoots, norad spacetrack report #6 1986
*    hoots, schumacher and glover 2004
*    vallado, crawford, hujsak, kelso  2006
  ---------------------------------------------------------------------------- }

PROCEDURE SGP4Init           ( whichconst                            : gravconsttype;
                               Satn                                  : Longint;
                               Epoch                                 : EXTENDED;
                               VAR xBStar, xEcco, xArgpo, xInclo, xMo,
                                   xNo, xnodeo                       : EXTENDED;
                               VAR SatRec                            : ElSetRec );
     { --------------------- Local Variables ------------------------ }
   CONST
     TwoPi      : EXTENDED =     2.0 * pi;    { 6.28318530717959; }
   VAR
     Ao,ainv,con42,cosio,sinio,cosio2,Eccsq,omeosq,posq,rp,rteosq,
     CNODM , SNODM , COSIM , SINIM , COSOMM, SINOMM, Cc1sq ,
     Cc2   , Cc3   , Coef  , Coef1 , Cosio4, DAY   , Dndt  ,
     EM    , EMSQ  , Eeta  , Etasq , GAM   , Argpm, nodem,
     Inclm , Mm  , Nm    , Perige, Pinvsq, Psisq , Qzms24,
     RTEMSQ, S1    , S2    , S3    , S4    , S5    , S6    ,
     S7    , SFour , SS1   , SS2   , SS3   , SS4   , SS5   ,
     SS6   , SS7   , SZ1   , SZ2   , SZ3   , SZ11  , SZ12  ,
     SZ13  , SZ21  , SZ22  , SZ23  , SZ31  , SZ32  , SZ33  ,
     Tc    , Temp  , Temp1 , Temp2 , Temp3 , Tsi   , XPIDOT,
     Xhdot1, Z1    , Z2    , Z3    , Z11   , Z12   , Z13   ,
     Z21   , Z22   , Z23   , Z31   , Z32   , Z33,
     qzms2t, SS, mu, RadiusEarthKm, J2,J3oJ2,J4,X2o3,
     tumin,  xke, j3, temp4 : EXTENDED;
     r, v : Vector;
   BEGIN
     Satrec.error:= 0;
     SatRec.Method:= 'n';

   { sgp4fix - note the following variables are also passed directly via satrec. 
     it is possible to streamline the sgp4init call by deleting the "x"
     variables, but the user would need to set the satrec.* values first. we
     include the additional assignment in case twoline2rv is not used. }

     SatRec.bstar := xbstar;
     SatRec.ecco  := xecco;
     SatRec.argpo := xargpo;
     SatRec.inclo := xinclo;
     SatRec.mo    := xmo;
     SatRec.no    := xno;
     SatRec.nodeo := xnodeo;

     { ------------------------ INITIALIZATION --------------------- }
     { ----------- set all Near Earth variables to zero ------------ }
     SatRec.Isimp := 0;   SatRec.Aycof := 0.0; SatRec.CON41 := 0.0; SatRec.Cc1   := 0.0;
     SatRec.Cc4   := 0.0; SatRec.Cc5   := 0.0; SatRec.D2    := 0.0; SatRec.D3    := 0.0;   SatRec.D4    := 0.0;
     SatRec.Delmo := 0.0; SatRec.Eta   := 0.0; SatRec.ArgpDot := 0.0; SatRec.Omgcof:= 0.0; SatRec.Sinmao:= 0.0;
     SatRec.T     := 0.0; SatRec.T2cof := 0.0; SatRec.T3cof := 0.0; SatRec.T4cof := 0.0;   SatRec.T5cof := 0.0;
     SatRec.X1mth2:= 0.0; SatRec.X7thm1:= 0.0; SatRec.MDot  := 0.0; SatRec.nodeDot:= 0.0; SatRec.Xlcof := 0.0;
     SatRec.Xmcof := 0.0; SatRec.nodeCF:= 0.0;

     { ----------- set all Deep Space variables to zero ------------ }
     SatRec.IRez  := 0;   SatRec.D2201 := 0.0; SatRec.D2211 := 0.0; SatRec.D3210 := 0.0; SatRec.D3222 := 0.0;
     SatRec.D4410 := 0.0; SatRec.D4422 := 0.0; SatRec.D5220 := 0.0; SatRec.D5232 := 0.0; SatRec.D5421 := 0.0;
     SatRec.D5433 := 0.0; SatRec.Dedt  := 0.0; SatRec.Del1  := 0.0; SatRec.Del2  := 0.0; SatRec.Del3  := 0.0;
     SatRec.Didt  := 0.0; SatRec.Dmdt  := 0.0; SatRec.Dnodt := 0.0; SatRec.Domdt := 0.0; SatRec.E3    := 0.0;
     SatRec.Ee2   := 0.0; SatRec.Peo   := 0.0; SatRec.Pgho  := 0.0; SatRec.Pho   := 0.0; SatRec.Pinco := 0.0;
     SatRec.Plo   := 0.0; SatRec.Se2   := 0.0; SatRec.Se3   := 0.0; SatRec.Sgh2  := 0.0; SatRec.Sgh3  := 0.0;
     SatRec.Sgh4  := 0.0; SatRec.Sh2   := 0.0; SatRec.Sh3   := 0.0; SatRec.Si2   := 0.0; SatRec.Si3   := 0.0;
     SatRec.Sl2   := 0.0; SatRec.Sl3   := 0.0; SatRec.Sl4   := 0.0; SatRec.GSTo  := 0.0; SatRec.Xfact := 0.0;
     SatRec.Xgh2  := 0.0; SatRec.Xgh3  := 0.0; SatRec.Xgh4  := 0.0; SatRec.Xh2   := 0.0; SatRec.Xh3   := 0.0;
     SatRec.Xi2   := 0.0; SatRec.Xi3   := 0.0; SatRec.Xl2   := 0.0; SatRec.Xl3   := 0.0; SatRec.Xl4   := 0.0;
     SatRec.Xlamo := 0.0; SatRec.Zmol  := 0.0; SatRec.Zmos  := 0.0; SatRec.Atime := 0.0; SatRec.Xli   := 0.0;
     SatRec.Xni   := 0.0;

     { sgp4fix identify constants and allow alternate values }
     getgravconst( whichconst, tumin, mu, radiusearthkm, xke, j2, j3, j4, j3oj2 );   

     { sgp4fix divisor for divide by zero check on inclination
       the old check used 1.0 + cos(pi-1.0e-9), but then compared it to
       1.5e-12, so the threshold was changed to 1.5e-12 for consistency }
     temp4  :=  1.5e-12;
     SS     := 78.0/RadiusEarthKm+1.0;
     QZMS2T := POWER( ((120.0-78.0)/RadiusEarthKm),4 );
     X2o3   :=  2.0 / 3.0;

     SatRec.Init  := 'y';
     SatRec.T     := 0.0;

     INITL( Satn , whichconst, SatRec.Ecco   , EPOCH , SatRec.Inclo  , SatRec.No   ,
            SatRec.Method, AINV  , AO    , SatRec.CON41 , CON42 , COSIO , COSIO2,
            EccSQ  , OMEOSQ, POSQ , rp    , RTEOSQ, SINIO , SatRec.GSTo );

     IF (OMEOSQ >= 0.0 ) OR ( SatRec.No >= 0.0) THEN
       BEGIN
          SatRec.ISIMP := 0;
          IF (rp < (220.0/RadiusEarthKm+1.0)) THEN
              SatRec.ISIMP := 1;
          SFour := SS;
          QZMS24 := QZMS2T;
          PERIGE := (rp-1.0)*RadiusEarthKm;

          { - For perigees below 156 km, S and Qoms2t are altered - }
          IF (PERIGE < 156.0) THEN
            BEGIN
              SFour := PERIGE-78.0;
              IF (PERIGE <= 98.0) THEN
                  SFour := 20.0;
              QZMS24 := POWER( ( (120.0-SFour)/RadiusEarthKm ),4.0 );
              SFour     := SFour/RadiusEarthKm + 1.0;
            END;
          PINVSQ := 1.0/POSQ;

          TSI    := 1.0/(AO-SFour);
          SatRec.ETA    := AO*SatRec.Ecco*TSI;
          ETASQ  := SatRec.ETA*SatRec.ETA;
          EETA   := SatRec.Ecco*SatRec.ETA;
          PSISQ  := ABS(1.0-ETASQ);
          COEF   := QZMS24* POWER( TSI,4.0 );
          COEF1  := COEF / POWER( PSISQ,3.5 );
          CC2    := COEF1*SatRec.No* (AO* (1.0+1.5*ETASQ+EETA*(4.0+ETASQ) )+0.375*
                    J2*TSI/PSISQ*SatRec.CON41*(8.0+3.0*ETASQ*(8.0+ETASQ)));
          SatRec.CC1    := SatRec.BSTAR*CC2;
          CC3    := 0.0;
          IF (SatRec.Ecco > 1.0E-4) THEN
              CC3 := -2.0*COEF*TSI*J3OJ2*SatRec.No*SINIO/SatRec.Ecco;
          SatRec.X1MTH2 := 1.0-COSIO2;

          SatRec.CC4    := 2.0*SatRec.No*COEF1*AO*OMeoSQ*(SatRec.Eta*(2.0+0.5*ETASQ)
                    +SatRec.Ecco*(0.5 + 2.0*ETASQ) - J2*TSI / (AO*PSISQ)*
                    (-3.0*SatRec.Con41*(1.0-2.0*
                    EETA+ETASQ*(1.5-0.5*EETA))+0.75*SatRec.x1mth2*(2.0*ETASQ
                    -EETA*(1.0+ETASQ))*COS(2.0*SatRec.argpo)));
          SatRec.CC5    := 2.0*COEF1*AO*OMEOSQ* (1.0 + 2.75*
                    (ETASQ + EETA) + EETA*ETASQ );
          COSIO4 := COSIO2*COSIO2;
          TEMP1  := 1.5*J2*PINVSQ*SatRec.No;
          TEMP2  := 0.5*TEMP1*J2*PINVSQ;
          TEMP3  := -0.46875*J4*PINVSQ*PINVSQ*SatRec.No;
          SatRec.mdot   := SatRec.No + 0.5*TEMP1*RTEOSQ*SatRec.Con41 + 0.0625*TEMP2*
                    RTEOSQ*(13.0 - 78.0*COSIO2 + 137.0*COSIO4);
          SatRec.argpdot  := -0.5*TEMP1*CON42 + 0.0625*TEMP2*
                    (7.0 - 114.0*COSIO2 +
                    395.0*COSIO4)+TEMP3*(3.0-36.0*COSIO2+49.0*COSIO4);
          XHDOT1 := -TEMP1*COSIO;
          SatRec.nodedot := XHDOT1+(0.5*TEMP2*(4.0-19.0*COSIO2)+
                    2.0*TEMP3*(3.0 - 7.0*COSIO2))*COSIO;
          XPIDOT := SatRec.argpdot+SatRec.nodedot;
          SatRec.omgcof := SatRec.bstar*CC3*COS(SatRec.argpo);
          SatRec.xmcof  := 0.0;
          IF (SatRec.Ecco > 1.0E-4) THEN
              SatRec.xmcof := -X2O3*COEF*SatRec.bstar/EETA;
          SatRec.nodeCF := 3.5*OMEOSQ*XHDOT1*SatRec.CC1;
          SatRec.t2cof  := 1.5*SatRec.CC1;
         { sgp4fix for divide by zero with xinco = 180 deg }
         if (abs(cosio+1.0) > 1.5e-12) THEN
             satrec.xlcof := -0.25 * j3oj2 * sinio * (3.0 + 5.0 * cosio) / (1.0 + cosio)
           else
             satrec.xlcof := -0.25 * j3oj2 * sinio * (3.0 + 5.0 * cosio) / temp4;
          SatRec.aycof  := -0.5*J3OJ2*SINIO;
          SatRec.delmo  := POWER( (1.0+SatRec.Eta*COS(SatRec.Mo)),3 );
          SatRec.sinmao := SIN(SatRec.Mo);
          SatRec.x7thm1 := 7.0*COSIO2-1.0;

          { --------------- Deep Space Initialization ------------- }
          IF ((TWOPI/SatRec.No) >= 225.0) THEN
            BEGIN
              SatRec.method := 'd';
              SatRec.isimp  := 1;
              TC     := 0.0;
              Inclm  := SatRec.inclo;

              DSCOM( EPOCH , SatRec.Ecco    , SatRec.argpo, Tc    , SatRec.inclo ,
                     SatRec.nodeo, SatRec.No   ,
                     SNODM , CNODM , SINIM , COSIM , SINOMM, COSOMM,
                     DAY   , SatRec.e3    , SatRec.ee2   , EM    , EMSQ  , GAM   ,
                     SatRec.Peo   , SatRec.Pgho  , SatRec.Pho   , SatRec.PInco , SatRec.Plo   ,
                     RTemSq, SatRec.Se2   , SatRec.Se3   , SatRec.Sgh2  , SatRec.Sgh3  , SatRec.Sgh4  ,
                     SatRec.Sh2   , SatRec.Sh3   , SatRec.Si2   , SatRec.Si3   , SatRec.Sl2   , SatRec.Sl3   ,
                     SatRec.Sl4   , S1    , S2    , S3    , S4    , S5    ,
                     S6    , S7    , SS1   , SS2   , SS3   , SS4   ,
                     SS5   , SS6   , SS7   , SZ1   , SZ2   , SZ3   ,
                     SZ11  , SZ12  , SZ13  , SZ21  , SZ22  , SZ23  ,
                     SZ31  , SZ32  , SZ33  , SatRec.Xgh2  , SatRec.Xgh3  , SatRec.Xgh4  ,
                     SatRec.Xh2   , SatRec.Xh3   , SatRec.Xi2   , SatRec.Xi3   , SatRec.Xl2   , SatRec.Xl3   ,
                     SatRec.Xl4   , Nm    , Z1    , Z2    , Z3    , Z11   ,
                     Z12   , Z13   , Z21   , Z22   , Z23   , Z31   ,
                     Z32   , Z33   , SatRec.Zmol  , SatRec.Zmos );
              DPPER( SatRec.e3    , SatRec.ee2   , SatRec.peo  , SatRec.pgho , SatRec.pho , SatRec.pinco ,
                     SatRec.plo   , SatRec.se2   , SatRec.se3   , SatRec.sgh2  , SatRec.sgh3  , SatRec.sgh4  ,
                     SatRec.sh2   , SatRec.sh3   , SatRec.si2   , SatRec.si3   , SatRec.sl2   , SatRec.sl3   ,
                     SatRec.sl4   , SatRec.T     , SatRec.xgh2  , SatRec.xgh3  , SatRec.xgh4  , SatRec.xh2   ,
                     SatRec.xh3   , SatRec.xi2   , SatRec.xi3   , SatRec.xl2   , SatRec.xl3   , SatRec.xl4   ,
                     SatRec.zmol  , SatRec.zmos  , Inclm , SatRec.Init  ,
                     SatRec.Ecco    , SatRec.inclo , SatRec.nodeo, SatRec.argpo, SatRec.mo );

              Argpm := 0.0;  { Add these to make DS work on initial }
              Mm    := 0.0;
              nodem := 0.0;

              DSINIT( whichconst, Cosim , Emsq , SatRec.argpo, S1   , S2   , S3   ,
                      S4    , S5    , Sinim , Ss1   , Ss2   , Ss3   ,
                      Ss4   , Ss5   , Sz1   , Sz3   , Sz11  , Sz13  ,
                      Sz21  , Sz23  , Sz31  , Sz33  , SatRec.T     , Tc    ,
                      SatRec.gsto , SatRec.mo  , SatRec.mdot , SatRec.No   , SatRec.nodeo, SatRec.nodedot,
                      XPIDOT, Z1    , Z3    , Z11   , Z13   , Z21   ,
                      Z23   , Z31   , Z33   ,
                      SatRec.Ecco,Eccsq,
                      EM    , Argpm, Inclm , Mm  , Nm    , nodem,
                      SatRec.IREZ  , SatRec.Atime , SatRec.D2201 , SatRec.D2211 , SatRec.D3210 , SatRec.D3222 ,
                      SatRec.D4410 , SatRec.D4422 , SatRec.D5220 , SatRec.D5232 , SatRec.D5421 , SatRec.D5433 ,
                      SatRec.Dedt  , SatRec.Didt  , SatRec.DMDT  , DNDT  , SatRec.DNODT , SatRec.DOMDT ,
                      SatRec.Del1  , SatRec.Del2  , SatRec.Del3  , SatRec.Xfact , SatRec.Xlamo , SatRec.Xli   ,
                      SatRec.Xni );
          END;

          { ----------- Set variables IF not deep space ----------- }
          IF (SatRec.isimp <> 1) THEN
            BEGIN
              CC1SQ := SatRec.cc1*SatRec.cc1;
              SatRec.d2    := 4.0*AO*TSI*CC1SQ;
              TEMP  := SatRec.d2*TSI*SatRec.cc1 / 3.0;
              SatRec.d3    := (17.0*AO + SFour) * TEMP;
              SatRec.d4    := 0.5*TEMP*AO*TSI * (221.0*AO + 31.0*SFour) * SatRec.cc1;
              SatRec.t3cof := SatRec.d2 + 2.0*CC1SQ;
              SatRec.t4cof := 0.25* (3.0*SatRec.d3+SatRec.cc1*(12.0*SatRec.d2+10.0*CC1SQ) );
              SatRec.t5cof := 0.2* (3.0*SatRec.d4 + 12.0*SatRec.cc1*SatRec.d3 + 6.0*SatRec.d2*SatRec.d2 +
                      15.0*CC1SQ* (2.0*SatRec.d2 + CC1SQ) );
            END;
       END; { if }

    IF (rp < 1.0) THEN
{        WriteLn( ' *** SATN',Satn,' EPOCH ELTS SUB-ORBITAL *** ' );}
        Satrec.error:= 5;

     SGP4( whichconst, SatRec, 0.0, r, v );

     SatRec.init:= 'n';

(*    {$I debug6.pas }*)

      END; { PROCEDURE SGP4Init }

{ -----------------------------------------------------------------------------
*
*                             procedure sgp4
*
*  this procedure is the sgp4 prediction model from space command. this is an
*    updated and combined version of sgp4 and sdp4, which were originally
*    published separately in spacetrack report #3. this version follows the
*    methodology from the aiaa paper (2006) describing the history and
*    development of the code.
*
*  author        : david vallado                  719-573-2600   28 jun 2005
*
*  inputs        :
*    satrec	 - initialised structure from sgp4init() call.
*    tsince	 - time eince epoch (minutes)
*
*  outputs       :
*    r           - position vector                     km
*    v           - velocity                            km/sec
*  return code - non-zero on error.
*
*  locals        :
*    am          -
*    axnl, aynl        -
*    betal       -
*    COSIM   , SINIM   , COSOMM  , SINOMM  , Cnod    , Snod    , Cos2u   ,
*    Sin2u   , Coseo1  , Sineo1  , Cosi    , Sini    , Cosip   , Sinip   ,
*    Cosisq  , Cossu   , Sinsu   , Cosu    , Sinu
*    Delm        -
*    Delomg      -
*    Dndt        -
*    Eccm        -
*    EMSQ        -
*    Ecose       -
*    El2         -
*    Eo1         -
*    Eccp        -
*    Esine       -
*    Argpm       -
*    Argpp       -
*    Omgadf      -
*    Pl          -
*    R           -
*    RTEMSQ      -
*    Rdotl       -
*    Rl          -
*    Rvdot       -
*    Rvdotl      -
*    Su          -
*    T2  , T3   , T4    , Tc
*    Tem5, Temp , Temp1 , Temp2  , Tempa  , Tempe  , Templ
*    U   , Ux   , Uy    , Uz     , Vx     , Vy     , Vz
*    inclm       - inclination
*    mm          - mean anomaly
*    nm          - mean motion
*    nodem       - longi of ascending node
*    xinc        -
*    xincp       -
*    xl          -
*    xlm         -
*    mp          -
*    xmdf        -
*    xmx         -
*    xmy         -
*    nodedf      -
*    xnode       -
*    nodep       -
*    np          -
*
*  coupling      :
*    getgravconst
*    dpper
*    dspace
*
*  references    :
*    hoots, roehrich, norad spacetrack report #3 1980
*    hoots, norad spacetrack report #6 1986
*    hoots, schumacher and glover 2004
*    vallado, crawford, hujsak, kelso  2006
  ---------------------------------------------------------------------------- }

PROCEDURE SGP4               ( whichconst : gravconsttype;
                               VAR SatRec                            : ElSetRec;
                               TSince                                : Extended;
                               VAR r,v                               : Vector );
     { --------------------- Local Variables ------------------------ }
   CONST
     TwoPi      : EXTENDED =     2.0 * pi;    { 6.28318530717959; }
   VAR
     AM    , Axnl  , Aynl  , Betal , COSIM , Cnod  ,
     Cos2u , Coseo1, Cosi  , Cosip , Cosisq, Cossu , Cosu  ,
     Delm  , Delomg, EM    , EMSQ  , Ecose , El2   , Eo1   ,
     Ep    , Esine , Argpm , Argpp , ArgpDF, Pl    ,
     Rdotl , Rl    , Rvdot , Rvdotl, SINIM ,
     Sin2u , Sineo1, Sini  , Sinip , Sinsu , Sinu  ,
     Snod  , Su    , T2    , T3    , T4    , Tem5  , Temp  ,
     Temp1 , Temp2 , Tempa , Tempe , Templ , U     , Ux    ,
     Uy    , Uz    , Vx    , Vy    , Vz    , Inclm , Mm  ,
     Nm    , nodem, Xinc  , Xincp , Xl    , Xlm   , Mp  ,
     Xmdf  , Xmx   , Xmy   , nodeDF, Xnode , nodep, Tc    , Dndt, temp4 : EXTENDED;
     X2O3, J2,J3,XKE,J3OJ2, j4,tumin,mu, radiusearthkm, vkmpersec : EXTENDED;
     iter : INTEGER;
   BEGIN
      { -------------------- WGS-72 EARTH CONSTANTS ----------------- }
      { ------------------ SET MATHEMATICAL CONSTANTS --------------- }
      X2O3   := 2.0/3.0;

      { sgp4fix identify constants and allow alternate values }
      getgravconst( whichconst, tumin, mu, radiusearthkm, xke, j2, j3, j4, j3oj2 );   

      VKmpersec:= radiusearthkm * xke/60.0;
     { sgp4fix divisor for divide by zero check on inclination
       the old check used 1.0 + cos(pi-1.0e-9), but then compared it to
       1.5 e-12, so the threshold was changed to 1.5e-12 for consistancy }
      temp4  :=   1.5e-12;
   
      { --------------------- CLEAR SGP4 ERROR FLAG ----------------- }
      SatRec.t     := tsince;
      satrec.Error := 0;

      { ------- UPDATE FOR SECULAR GRAVITY AND ATMOSPHERIC DRAG ----- }
      XMDF   := SatRec.mo + SatRec.mdot*SatRec.t;
      ArgpDF := SatRec.argpo + SatRec.argpdot*SatRec.t;
      nodeDF := SatRec.nodeo + SatRec.nodedot*SatRec.t;
      Argpm  := ArgpDF;
      Mm     := XMDF;
      T2     := SatRec.t*SatRec.t;
      nodem  := nodeDF + SatRec.nodecf*T2;
      TEMPA  := 1.0 - SatRec.cc1*SatRec.t;
      TEMPE  := SatRec.bstar*satrec.cc4*SatRec.t;
      TEMPL  := SatRec.t2cof*T2;
      IF (SatRec.isimp <> 1) THEN
        BEGIN
          DELOMG:= SatRec.omgcof*SatRec.t;
          DELM  := SatRec.xmcof*(POWER( ( 1.0+SatRec.Eta*COS(XMDF) ),3 )-SatRec.delmo);
          TEMP  := DELOMG + DELM;
          Mm  := XMDF + TEMP;
          Argpm := ArgpDF - TEMP;
          T3    := T2*SatRec.t;
          T4    := T3*SatRec.t;
          TEMPA := TEMPA - SatRec.d2*T2 - SatRec.d3*T3 - SatRec.d4*T4;
          TEMPE := TEMPE + SatRec.bstar*SatRec.cc5*(SIN(Mm) - SatRec.sinmao);
          TEMPL := TEMPL + SatRec.t3cof*T3 + T4*(SatRec.t4cof + SatRec.t*SatRec.t5cof);
        END;

      Nm    := SatRec.No;
      EM    := SatRec.Ecco;
      Inclm := SatRec.inclo;
      IF (SatRec.method = 'd') THEN
        BEGIN
          TC:= SatRec.t;
          DSPACE( SatRec.IRez, SatRec.D2201 , SatRec.D2211 , SatRec.D3210 , SatRec.D3222 , SatRec.D4410 ,
                  SatRec.D4422 , SatRec.D5220 , SatRec.D5232 , SatRec.D5421 , SatRec.D5433 , SatRec.Dedt  ,
                  SatRec.Del1  , SatRec.Del2  , SatRec.Del3  , SatRec.Didt  , SatRec.Dmdt  , SatRec.Dnodt ,
                  SatRec.Domdt , SatRec.argpo  , SatRec.argpdot , SatRec.t     , TC , SatRec.gsto ,
                  SatRec.Xfact , SatRec.Xlamo , SatRec.No     ,
                  SatRec.Atime , EM    , Argpm , Inclm , SatRec.Xli   , Mm  ,
                  SatRec.XNi   , nodem, Dndt  , Nm  );
        END;

      IF (Nm <= 0.0) THEN
        BEGIN
{          WriteLn( 'Error Nm ',Nm );}
          satrec.Error:= 2;
        END;
      AM := POWER( (XKE/Nm),X2O3 )*TEMPA*Tempa;
      Nm := XKE / POWER( AM,1.5 );
      EM := EM-TEMPE;
      { fix tolerance for error recognition }
      IF (EM >= 1.0) or (EM < -0.001) or (AM < 0.95) THEN
        BEGIN
{          WriteLn( 'Error Em ',em );}
          satrec.Error:= 1;
        END;
{   sgp4fix change test condition for eccentricity }
      IF (EM < 1.0E-6) THEN
          EM := 1.0E-6;
      Mm     := Mm+SatRec.No*TEMPL;
      XLM    := Mm+Argpm+nodem;
      EMSQ   := EM*EM;
      TEMP   := 1.0 - EMSQ;
      nodem  := MODFUNC(nodem,TwoPi);
      Argpm  := MODFUNC(Argpm,TwoPi);
      XLM    := MODFUNC(XLM,TwoPi);
      Mm     := MODFUNC(XLM - Argpm - nodem,TwoPi);

      { ----------------- COMPUTE EXTRA MEAN QUANTITIES ------------- }
      SINIM  := SIN(Inclm);
      COSIM  := COS(Inclm);
                                                                        
      { -------------------- ADD LUNAR-SOLAR PERIODICS -------------- }
      EP     := EM;
      XINCP  := Inclm;
      Argpp  := Argpm;
      nodep  := nodem;
      Mp     := Mm;
      SINIP  := SINIM;
      COSIP  := COSIM;
      IF (SatRec.method = 'd') THEN
        BEGIN
          DPPER( SatRec.e3    , SatRec.ee2   , SatRec.peo  , SatRec.pgho , SatRec.pho , SatRec.pinco ,
                 SatRec.plo   , SatRec.se2   , SatRec.se3   , SatRec.sgh2  , SatRec.sgh3  , SatRec.sgh4  ,
                 SatRec.sh2   , SatRec.sh3   , SatRec.si2   , SatRec.si3   , SatRec.sl2   , SatRec.sl3   ,
                 SatRec.sl4   , SatRec.T     , SatRec.xgh2  , SatRec.xgh3  , SatRec.xgh4  , SatRec.xh2   ,
                 SatRec.xh3   , SatRec.xi2   , SatRec.xi3   , SatRec.xl2   , SatRec.xl3   , SatRec.xl4   ,
                 SatRec.zmol  , SatRec.zmos  , SatRec.Inclo , 'n'  ,
                 Ep    , xincp , nodep, argpp, mp );

          IF (XINCP < 0.0) THEN
            BEGIN
              XINCP  := -XINCP;
              nodep := nodep + PI;
              Argpp := Argpp - PI;
            END;
          IF (EP < 0.0 ) OR ( EP > 1.0) THEN
            BEGIN
{              WriteLn( 'Error Ep ',ep );}
              satrec.Error:= 3;
            END;
        END;

      { -------------------- LONG PERIOD PERIODICS ------------------ }
      IF (SatRec.method = 'd') THEN
        BEGIN
          SINIP :=  SIN(XINCP);
          COSIP :=  COS(XINCP);
          SatRec.aycof := -0.5*J3OJ2*SINIP;
          { sgp4fix for divide by zero with xincp = 180 deg }
          if (abs(cosip+1.0) > 1.5e-12) THEN
              satrec.xlcof := -0.25 * j3oj2 * sinip * (3.0 + 5.0 * cosip) / (1.0 + cosip)
            else
              satrec.xlcof := -0.25 * j3oj2 * sinip * (3.0 + 5.0 * cosip) / temp4;
         END;
      AXNL := EP*COS(Argpp);
      TEMP := 1.0 / (AM*(1.0-EP*EP));
      AYNL := EP*SIN(Argpp) + TEMP*SatRec.aycof;
      XL   := Mp + Argpp + nodep + TEMP*SatRec.xlcof*AXNL;
                                                                        
      { --------------------- SOLVE KEPLER'S EQUATION --------------- }
      U    := MODFUNC(XL-nodep,TwoPi);
      EO1  := U;
      iter := 0;
      Tem5 := 9999.9;
      {  sgp4fix for kepler iteration
         the following iteration needs better limits on corrections }
      WHILE ((ABS(Tem5)>=1.0E-12) and (iter < 10)) DO
        BEGIN
          iter  := iter + 1;
          SINEO1:= SIN(EO1);
          COSEO1:= COS(EO1);
          TEM5  := 1.0 - COSEO1*AXNL - SINEO1*AYNL;
          TEM5  := (U - AYNL*COSEO1 + AXNL*SINEO1 - EO1) / TEM5;
          temp:= abs(tem5);
          if temp > 1.0 THEN
              Tem5:= Tem5/temp;
          EO1   := EO1+TEM5;
        END;

      { ------------- SHORT PERIOD PRELIMINARY QUANTITIES ----------- }
      ECOSE := AXNL*COSEO1+AYNL*SINEO1;
      ESINE := AXNL*SINEO1-AYNL*COSEO1;
      EL2   := AXNL*AXNL+AYNL*AYNL;
      PL    := AM*(1.0-EL2);
      IF ( PL < 0.0 ) THEN
        BEGIN
{          WriteLn( 'Error Pl ',pl );}
          satrec.Error:= 4;
        END
        ELSE
        BEGIN
          RL    := AM*(1.0-ECOSE);
          RDOTL := SQRT(AM)*ESINE/RL;
          RVDOTL:= SQRT(PL)/RL;
          BETAL := SQRT(1.0-EL2);
          TEMP  := ESINE/(1.0+BETAL);
          SINU  := AM/RL*(SINEO1-AYNL-AXNL*TEMP);
          COSU  := AM/RL*(COSEO1-AXNL+AYNL*TEMP);
          SU    := ATan2(SINU,COSU);
          SIN2U := (COSU+COSU)*SINU;
          COS2U := 1.0-2.0*SINU*SINU;
          TEMP  := 1.0/PL;
          TEMP1 := 0.5*J2*TEMP;
          TEMP2 := TEMP1*TEMP;
                                                                        
          { -------------- UPDATE FOR SHORT PERIOD PERIODICS ------------ }
          IF (SatRec.method = 'd') THEN
            BEGIN
              COSISQ := COSIP*COSIP;
              SatRec.Con41  := 3.0*COSISQ - 1.0;
              SatRec.x1mth2 := 1.0 - COSISQ;
              SatRec.x7thm1 := 7.0*COSISQ - 1.0;
            END;
          r[4] := RL*(1.0 - 1.5*TEMP2*BETAL*SatRec.Con41) + 0.5*TEMP1*SatRec.x1mth2*COS2U;
          SU   := SU - 0.25*TEMP2*SatRec.x7thm1*SIN2U;
          XNODE:= nodep + 1.5*TEMP2*COSIP*SIN2U;
          XINC := XINCP + 1.5*TEMP2*COSIP*SINIP*COS2U;
          v[4] := RDOTL - Nm*TEMP1*SatRec.x1mth2*SIN2U / XKE;
          RVDOT:= RVDOTL + Nm*TEMP1* (SatRec.x1mth2*COS2U+1.5*SatRec.Con41) / XKE;

          { --------------------- ORIENTATION VECTORS ------------------- }
          SINSU:=  SIN(SU);
          COSSU:=  COS(SU);
          SNOD :=  SIN(XNODE);
          CNOD :=  COS(XNODE);
          SINI :=  SIN(XINC);
          COSI :=  COS(XINC);
          XMX  := -SNOD*COSI;
          XMY  :=  CNOD*COSI;
          UX   :=  XMX*SINSU + CNOD*COSSU;
          UY   :=  XMY*SINSU + SNOD*COSSU;
          UZ   :=  SINI*SINSU;
          VX   :=  XMX*COSSU - CNOD*SINSU;
          VY   :=  XMY*COSSU - SNOD*SINSU;
          VZ   :=  SINI*COSSU;


          { ------------------- POSITION AND VELOCITY ------------------- }
          r[1] := r[4]*UX * radiusearthkm;
          r[2] := r[4]*UY * radiusearthkm;
          r[3] := r[4]*UZ * radiusearthkm;
          v[1] := (v[4]*UX+RVDOT*VX) * vKmPerSec;
          v[2] := (v[4]*UY+RVDOT*VY) * vKmPerSec;
          v[3] := (v[4]*UZ+RVDOT*VZ) * vKmPerSec;
          MAG( r );
          MAG( v );
        END;  { If error vode is ok }

     {/ sgp4fix for decaying satellites }
     if (r[4] < radiusearthkm) THEN
       BEGIN
{         writeln('# decay condition ',r[4]);}
         satrec.error := 6;
       END;


(*    {$I debug7.pas }*)

   END; { PROCEDURE Sgp4 }


{ ------------------------------------------------------------------------------
|
|                           FUNCTION GSTIME
|
|  This FUNCTION finds the Greenwich SIDEREAL time.
|
|  Author        : David Vallado                  719-573-2600    1 Mar 2001
|
|  Inputs          Description                    Range / Units
|    JDUT1       - Julian Date in UT1             days from 4713 BC
|
|  OutPuts       :
|    GSTIME      - Greenwich SIDEREAL Time        0 to 2Pi rad
|
|  Locals        :
|    Temp        - Temporary variable for reals   rad
|    TUT1        - Julian Centuries from the
|                  Jan 1, 2000 12 h epoch (UT1)
|
|  Coupling      :
|    REALMOD     - MOD FUNCTION for REAL variables
|
|  References    :
|    Vallado       2007, 194, Eq 3-45
 ----------------------------------------------------------------------------- }

FUNCTION GSTIME              ( JDUT1                                 : EXTENDED ): EXTENDED;
   CONST
     TwoPi      : EXTENDED =     2.0 * pi;    { 6.28318530717959; }
     Deg2Rad    : EXTENDED =     Pi/180.0;
   VAR
     Temp, TUT1 : EXTENDED;
   BEGIN
     TUT1:= ( JDUT1 - 2451545.0 ) / 36525.0;
     Temp:= - 6.2E-6*TUT1*TUT1*TUT1
            + 0.093104*TUT1*TUT1
            + (876600.0*3600 + 8640184.812866)*TUT1
            + 67310.54841;  {sec }
     Temp:= MODFUNC( Temp*Deg2Rad/240.0,TwoPi ); { 360/86400 = 1/240, to deg, to rad }

     { ------------------------ Check quadrants --------------------- }
     IF Temp < 0.0 THEN
         Temp:= Temp + TwoPi;

     GSTIME:= Temp;

   END; { FUNCTION GSTIME }


{* -----------------------------------------------------------------------------
*
*                           function getgravconst
*
*  this function gets constants for the propagator. note that mu is identified to
*    facilitiate comparisons with newer models.
*
*  author        : david vallado                  719-573-2600   21 jul 2006
*
*  inputs        :
*    whichconst  - which set of constants to use  72, 84
*
*  outputs       :
*    tumin       - minutes in one time unit
*    radiusearthkm - radius of the earth in km
*    xke         - reciprocal of tumin
*    j2, j3, j4  - un-normalized zonal harmonic values
*    j3oj2       - j3 divided by j2
*
*  locals        :
*    mu          - earth gravitational parameter
*
*  coupling      :
*
*  references    :
*    norad spacetrack report #3
*    vallado, crawford, hujsak, kelso  2006
  --------------------------------------------------------------------------- *}

procedure getgravconst ( whichconst : gravconsttype;
                         VAR tumin, mu, radiusearthkm, xke, j2, j3, j4, j3oj2 : Extended );
   BEGIN

       CASE whichconst OF
         wgs721 :
         BEGIN
           { -- wgs-72 low precision str#3 constants -- }
           mu     := 398600.79964;        { in km3 / s2 }
           radiusearthkm := 6378.135;     { km }
           xke    := 0.0743669161;
           tumin  := 1.0 / xke;
           j2     :=   0.001082616;
           j3     :=  -0.00000253881;
           j4     :=  -0.00000165597;
           j3oj2  :=  j3 / j2;
         END;
         { ------------ wgs-72 constants ------------ }
         wgs72 :
         BEGIN
           mu     := 398600.8;            { in km3 / s2 }
           radiusearthkm := 6378.135;     { km }
           xke    := 60.0 / sqrt(radiusearthkm*radiusearthkm*radiusearthkm/mu);
           tumin  := 1.0 / xke;
           j2     :=   0.001082616;
           j3     :=  -0.00000253881;
           j4     :=  -0.00000165597;
           j3oj2  :=  j3 / j2;
         END;
         wgs84 :
         BEGIN
          { ------------ wgs-84 constants ------------}
           mu     := 398600.5;            { in km3 / s2 }
           radiusearthkm := 6378.137;     { km }
           xke    := 60.0 / sqrt(radiusearthkm*radiusearthkm*radiusearthkm/mu);
           tumin  := 1.0 / xke;
           j2     :=   0.00108262998905;
           j3     :=  -0.00000253215306;
           j4     :=  -0.00000161098761;
           j3oj2  :=  j3 / j2;
         END;
         ELSE
           writeln( 'unknown gravity option ');
         END; { Case }

     END;  { getgravconst }

BEGIN

  Help:= 'Y';
  Help:= 'N';

END.  { Unit SGP4Unit }

