* ---------------------------------------------------------------------
*
*                              TESTFOR.FOR
*
*  this program tests the sgp4 propagator.
*
*                          companion code for
*             fundamentals of astrodynamics and applications
*                                  2007
*                            by david vallado
*
*     (w) 719-573-2600, email dvallado@agi.com
*     *****************************************************************
*  current :
*             3 jul 08  david vallado
*                        add switch for afspc compatibility and improved operation
*  changes :
*             2 apr 07  david vallado
*                        misc fixes for manual operation
*            15 aug 06  david vallado
*                        update mfe for verification time steps, constants
*            20 jul 05  david vallado
*                         fixes for paper, corrections from paul crawford
*             7 jul 04  david vallado
*                         fix record file and get working
*            14 may 01  david vallado
*                         2nd edition baseline
*                   80  norad
*                         original baseline
*     *****************************************************************
*
*  Files         :
*    Unit 10     - input elm file  input file for element sets
*    Unit 11     - sgp4test.out    output file
*    Unit 14     - sgp4test.dbg    debug output file
*    Unit 15     - sgp4rec.bak     temporary file of record for 2 line element sets
*
*  Uses object and include files:
*    Astmath.cmn,
*    Sgp4.cmn,
*    Sgp4ext,
*    Sgp4io,
*    Sgp4unit

      PROGRAM Main
        IMPLICIT NONE
        Character typerun, typeinput
        Character*12 InFileName

        Character*3 MonStr,Monthtitle(12)
        Integer Code, NumSats, TotalNumSats, k, error, whichconst
        Real*8 ro(3),vo(3), startmfe, stopmfe, deltamin

        REAL*8 p, ecc, incl, node, argp, nu, m,arglat,truelon,lonper

* ----------------------------  Locals  -------------------------------
        REAL*8 J2,TwoPi,Rad,mu, RadiusEarthKm,VKmPerSec, xke,
     &         de2ra, xpdotp, T, sec, JD, pi, j3, j4, j3oj2, tumin
        INTEGER i,j, Year,yr,mon,day,hr,min


        INCLUDE 'SGP4.CMN'

        COMMON /DebugHelp/ Help
        CHARACTER Help
        Help = 'N'

* ------------------------  Implementation   --------------------------
        Opsmode = 'a' ! afspc compatible mode
        Opsmode = 'i' ! improved sgp4 operation
        write(*,*) 'Input Opsmode - a, i'
        read(*,*) Opsmode

        typerun = 'C' ! complete catlog
        typerun = 'V' ! validation only, header satnum and r and v tsince
        write(*,*) 'Input typerun - V, C, M'
        read(*,*) typerun

        if ((typerun .ne. 'V') .and. (typerun .ne. 'C')) then
            write(*,*) 'input mfe, epoch (YMDHMS), or dayofyr approach',
     &                  ', M,E,D'
            read(*,*) typeinput
          else
            typeinput = 'E'
          endif

        write(*,*) 'Input whichconst - 721, 72, 84'
        read(*,*) whichconst

        pi            =    4.0D0 * datan(1.0D0)  ! 3.14159265358979D0
        TwoPi         =    2.0D0 * pi    ! 6.28318530717959D0
        Rad           =   180.0D0 / pi   ! 57.29577951308230D0
        DE2RA         =    pi / 180.0D0  ! 0.01745329251994330D0
        xpdotp        =  1440.0 / (2.0 *pi)  ! 229.1831180523293D0

        ! sgp4fix identify constants and allow alternate values
        CALL getgravconst( whichconst, tumin, mu, radiusearthkm, xke,  
     &       j2, j3, j4, j3oj2 )
        VKmPerSec     =  RadiusEarthKm * xke/60.0D0

        MonthTitle( 1)= 'Jan'
        MonthTitle( 2)= 'Feb'
        MonthTitle( 3)= 'Mar'
        MonthTitle( 4)= 'Apr'
        MonthTitle( 5)= 'May'
        MonthTitle( 6)= 'Jun'
        MonthTitle( 7)= 'Jul'
        MonthTitle( 8)= 'Aug'
        MonthTitle( 9)= 'Sep'
        MonthTitle(10)= 'Oct'
        MonthTitle(11)= 'Nov'
        MonthTitle(12)= 'Dec'

        ! ---------------- Setup files for operation ------------------
        ! 10 input 2-line element set file
        Write(*,*) 'Input elset filename '
        Read(*,*) InFileName
        OPEN(10,FILE = InFileName ,STATUS='OLD',
     &          ACCESS = 'SEQUENTIAL' )

        ! 11 output file
        IF (typerun.eq.'C') THEN
            OPEN(11,FILE = 'tforall.out' ,STATUS='UNKNOWN',
     &              ACCESS = 'SEQUENTIAL' )
          ELSEIF (typerun.eq.'V') THEN
                OPEN(11,FILE = 'tforver.out' ,STATUS='UNKNOWN',
     &              ACCESS = 'SEQUENTIAL' )
              ELSE
                OPEN(11,FILE = 'tfor.out' ,STATUS='UNKNOWN',
     &              ACCESS = 'SEQUENTIAL' )
              ENDIF

        OPEN(14,FILE = 'sgp4test.dbg' ,STATUS='UNKNOWN',
     &          ACCESS = 'SEQUENTIAL' )

        ! ----- 15 temporary file of record for 2 line element sets ---
        OPEN(15,FILE = 'Sgp4Rec.bak', ACCESS = 'DIRECT',
     &          FORM = 'UNFORMATTED', RECL = 1100, STATUS = 'UNKNOWN' )

        ! ----------------- Test simple propagation -------------------
        NumSats = 0
        Numsats = NumSats + 1
        CALL TwoLine2RVSGP4 ( NumSats,typerun,typeinput,whichconst,
     &                        startmfe,stopmfe,deltamin,Code )
        DOWHILE (Code.ne.999)
            Write(11,*) '',SatNum,' xx'
            Write(*,*) SatNum
            ! write out epoch value 
            T = 0.0D0
            CALL SGP4 ( whichconst, T, Ro, Vo, Error )
            WRITE( 11,800 ) T, ro(1),ro(2),ro(3),vo(1),vo(2),vo(3)

            ! now initialize time variables
            T      = startmfe

            ! check so the first value isn't written twice
            IF ( DABS(T).gt.0.00000001D0 ) THEN
                T = T - DeltaMin
              ENDIF

            DOWHILE ( (T.lt.stopmfe).and.(Error.eq.0) )
                T = T + DeltaMin
                IF (T.gt.stopmfe) THEN
                    T = stopmfe
                  ENDIF
                CALL SGP4 ( whichconst, T, Ro, Vo, Error )

                IF (Error .gt. 0 ) THEN
                    Write(*,*) '# Error in SGP4 .. ',
     &                     Error
                  ENDIF

                IF ( error .eq. 0) THEN

                 IF ((typerun.ne.'V').and.(typerun.ne.'C')) THEN
                     JD = JDSatEpoch + T/1440.0D0
                     CALL INVJDAY( JD, Year,Mon,Day,Hr,Min, Sec )
                     IF (Year.ge.2000) THEN
                         Yr = Year - 2000
                       ELSE
                         Yr = Year - 1900
                       ENDIF
                     MonStr = MonthTitle( Mon )
                     WRITE( 11,'(F17.8,3F17.8,3F17.8,
     &                       1x,I4,1x,A3,I3,I3,A1,I2,A1,F9.6)' )
     &                   t,ro(1),ro(2),ro(3),vo(1),vo(2),vo(3),
     &                   Day,MonStr,Yr,Hr,':',Min,':',Sec

                   ELSE
                     WRITE( 11,800 )
     &                   T,ro(1),ro(2),ro(3),vo(1),vo(2),vo(3)
  800  FORMAT(F17.8,3F17.8,3(1X,F14.9))

c                     call rv2coe(ro, vo, mu, p, a, ecc, incl, node, 
c     &                           argp, nu, m, arglat, truelon, 
c     &                           lonper )
c
c                     write(11,801) T ,ro(1),ro(2),ro(3),vo(1),vo(2),
c     &                             vo(3),a, ecc, incl*rad, node*rad,
c     &                             argp*rad, nu*rad, m*rad

  801  FORMAT(F17.8,3F17.8,3(1X,F13.9),f15.6,f9.6,f11.5,f11.5,f11.5,
     &          f11.5,f11.5)

                  ENDIF ! typerun

               ENDIF ! if error

             ENDDO ! propagating the case

            ! get code for next satellite if it's there
            Numsats = NumSats + 1
            CALL TwoLine2RVSGP4 ( NumSats,typerun,typeinput,whichconst,
     &                            startmfe,stopmfe,deltamin,Code )

          ENDDO ! while through file

        CLOSE(11)

        write(*,*) 'hit return to continue, or ctr-c to quit'
        read(*,*)

        OPEN(11,FILE = 'tfora.out' ,STATUS='UNKNOWN',
     &          ACCESS = 'SEQUENTIAL' )

        ! ----- Now test ability to handle mulitple satellites --------
        ! ------- Read in the elsets and form file of record ----------
        REWIND(10)
        Numsats = 0
        Code    = 0 ! Flag inside TwoLine2RVSGP4 to find EOF
        DOWHILE (Code.ne.999)
            Numsats = NumSats + 1
            CALL TwoLine2RVSGP4 ( NumSats,typerun,typeinput,whichconst,
     &                            startmfe,stopmfe,deltamin,Code )
         write(*,*) code,numsats
          ENDDO
        TotalNumSats = NumSats-1

        ! Now do random reads and propagations
        DO k=1,3

            Numsats = k
            IF (k .eq. 1) Numsats = 3
            IF (k .eq. 2) Numsats = 4
            IF (k .eq. 3) Numsats = 1

            ! --- Read common block of data from file of record --
            Read(15,Rec=NumSats) SatName,
     &          SatNum, ELNO  , EPHTYP, REVI  , EpochYr,
     &          BStar , Ecco  , Inclo , nodeo, Argpo , No    , Mo    ,
     &          NDot  , NDDot ,
     &          alta  , altp  , a     ,
     &          DeltaMin, JDSatEpoch, EpochDays,
     &          Isimp , Init  , Method, Opsmode,
     &          Aycof , CON41 , Cc1   , Cc4   , Cc5   , D2    , D3    ,
     &          D4    , Delmo , Eta   , ArgpDot,Omgcof, Sinmao,
     &          T2cof , T3cof , T4cof , T5cof , X1mth2, X7thm1, MDot  ,
     &          nodeDot,Xlcof, Xmcof , Xnodcf,
     &          D2201 , D2211 , D3210 , D3222 , D4410 , D4422 , D5220 ,
     &          D5232 , D5421 , D5433 , Dedt  , Del1  , Del2  , Del3  ,
     &          Didt  , Dmdt  , Dnodt , Domdt , E3    , Ee2   , Peo   ,
     &          Pgho  , Pho   , Pinco , Plo   , Se2   , Se3   , Sgh2  ,
     &          Sgh3  , Sgh4  , Sh2   , Sh3   , Si2   , Si3   , Sl2   ,
     &          Sl3   , Sl4   , GSTo  , Xfact , Xgh2  , Xgh3  , Xgh4  ,
     &          Xh2   , Xh3   , Xi2   , Xi3   , Xl2   , Xl3   , Xl4   ,
     &          Xlamo , Zmol  , Zmos  , Atime , Xli   , Xni   , IRez

            ! now initialize time variables
            T      = startmfe

            ! check so the first value isn't written twice
            IF ( DABS(T).gt.0.00000001D0 ) THEN
                T = T - DeltaMin
              ENDIF

            Write(*,*) 'For Sat num ',SatNum
            Write(11,*) 'For Sat num ',SatNum

            DOWHILE ( (T.lt.stopmfe).and.(Error.eq.0) )
                T = T + DeltaMin

                CALL SGP4 ( whichconst, T, Ro, Vo, Error )

                WRITE( 11,'(F17.8,F14.3,3F12.4,3F11.6)' )
     &                 JDSatEpoch+T/1440.0D0,T,
     &                 ro(1),ro(2),ro(3),vo(1),vo(2),vo(3)

              ENDDO ! propagating the case

          ENDDO ! from the random tests loop of k

      STOP
      END


