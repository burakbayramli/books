*     ----------------------------------------------------------------
*
*                               sgp4io.for
*
*    this file contains a function to read two line element sets. while 
*    not formerly part of the sgp4 mathematical theory, it is 
*    required for practical implemenation.
*
*                            companion code for
*               fundamentals of astrodynamics and applications
*                                    2007
*                              by david vallado
*
*       (w) 719-573-2600, email dvallado@agi.com
*
*  current :
*             3 jul 08  david vallado
*                        add switch for afspc compatibility and improved operation
*  changes :
*              14 mar 07  david vallado
*                           misc fixes and manual operation
*              15 aug 06  david vallado
*                           original baseline
*       ----------------------------------------------------------------


* -----------------------------------------------------------------------------
*
*                           SUBROUTINE TWOLINE2RVSGP4
*
*  this function converts the two line element set character string data to
*    variables and initializes the sgp4 variables. several intermediate varaibles
*    and quantities are determined. note that the result is a "structure" so multiple
*    satellites can be processed simultaneously without having to reinitialize. the
*    verification mode is an important option that permits quick checks of any
*    changes to the underlying technical theory. this option works using a
*    modified tle file in which the start, stop, and delta time values are
*    included at the end of the second line of data. this only works with the
*    verification mode. the catalog mode simply propagates from -1440 to 1440 min
*    from epoch and is useful when performing entire catalog runs.
*
*  author        : david vallado                  719-573-2600    1 mar 2001
*
*  inputs        :
*    Numsats     - Number of satellites processed. It also becomes the record
*                  number for each satellite
*    typerun     - type of run                    verification 'V', catalog 'C', 
*                                                 manual 'M'
*    typeinput   - type of manual input           mfe 'M', epoch 'E', dayofyr 'D'
*    whichconst  - which set of constants to use  72, 84
*    opsmode   - type of manual input           afspc 'a', imporved 'i'
*
*  outputs       :
*    Code        - EOF indicator. Code = 999 when EOF reached
*    startmfe    - starttime of simulation,       min from epoch
*    stopmfe     - stoptime of simulation,        min from epoch
*    deltamin    - time step                      min
*
*  coupling      :
*    days2mdhms  - conversion of days to month, day, hour, minute, second
*    jday        - convert day month year hour minute second into julian date
*    sgp4init    - initialize the sgp4 variables
*
*  Files         :
*    Unit 10     - test.elm        input 2-line element set file
*    Unit 11     - test.bak        output file
*    Unit 15     - sgp4rec.bak     temporary file of record for 2 line element sets
*
*  references    :
*    norad spacetrack report #3
*    vallado, crawford, hujsak, kelso  2006
*------------------------------------------------------------------------------

      SUBROUTINE TwoLine2RVSGP4 ( NumSats, Typerun, typeinput, 
     &                            whichconst, 
     &                            startmfe, stopmfe, deltamin, Code )
        IMPLICIT NONE
        Character Typerun, typeinput
        Integer Code, NumSats, whichconst
        REAL*8 startmfe, stopmfe, deltamin

* ----------------------------  Locals  -------------------------------
        REAL*8 J2, mu, RadiusEarthKm,VKmPerSec, xke, tumin
        REAL*8 BC,EPDay, sec, xpdotp, j3, j4, j3oj2 
        REAL*8 startsec, stopsec, startdayofyr, stopdayofyr, jdstart, 
     &         jdstop
        INTEGER startyear, stopyear, startmon, stopmon, startday, 
     &          stopday, starthr, stophr, startmin, stopmin 
        INTEGER Yr,Mon,Day,Hr,Minute,  ICrdno,nexp,bexp, error
        CHARACTER Show
        Character*130 LongStr1,LongStr2

        COMMON /DebugHelp/ Help
        CHARACTER Help

        INCLUDE 'SGP4.CMN'
        INCLUDE 'ASTMATH.CMN'

        ! --------------------  Implementation   ----------------------
        Show = 'N'
        xpdotp        =  1440.0D0 / (2.0D0 * pi) ! 229.1831180523293

        CALL getgravconst( whichconst, tumin, mu, radiusearthkm, xke,  
     &       j2, j3, j4, j3oj2 );
        VKmPerSec     =  RadiusEarthKm * xke / 60.0D0

c        make sure the main program opens this file, otherwise do so here
c        ! store results in a temporary file of record
c        OPEN(15,FILE='Sgp4Rec.bak',ACCESS='DIRECT', FORM='UNFORMATTED',
c     &       RECL=1000,STATUS='UNKNOWN')

* ----------------- READ THE FIRST LINE OF ELEMENT SET ----------------
        Code = 0

        LongStr1 = ' '
   50   READ(10,'(a130)',END=999) LongStr1
        IF(LongStr1(1:1) .eq. '#') GOTO 50 ! Commented line of text, skip

        READ(LongStr1,500) ICRDNO,SatNum,SatName,EpochYr,EpDay,
     &                       NDot,NDDot,nexp,BStar,bexp,EPHTYP,ELNO
  500   FORMAT( I1,1X,I5,1X,A10,I2,D12.0,1X,D10.0,1X,
     &          F6.5,I2,1X,F6.5,I2,1X,I1,1X,I4 )

* ----------- READ THE SECOND LINE OF ELEMENT SET AND TIME ------------
        LongStr2 = ' '
   51   READ(10,'(a130)',END=999) LongStr2
        IF(LongStr2(1:1) .eq. '#') GOTO 51 ! Commented line of text, skip

        IF (Typerun.eq.'V') THEN
          READ(LongStr2,502) ICRDNO,Inclo,nodeo,Ecco,Argpo,Mo,No,REVI,
     &              startmfe, stopmfe, DeltaMin
         else
          READ(LongStr2,501) ICRDNO,Inclo,nodeo,Ecco,Argpo,Mo,No,REVI
         endif
  501   FORMAT( I1,7X,D8.0,1X,D8.0,1X,F7.7,1X,D8.0,1X,D8.0,1X,D11.0,I5)
  502   FORMAT( I1,7X,D8.0,1X,D8.0,1X,F7.7,1X,D8.0,1X,D8.0,1X,D11.0,I5,
     &          1X,F12.6,F12.6,F12.6 )

* ---------------------- CONVERT TO INTERNAL UNITS --------------------
* ---- RADIANS, DISTANCE IN EARTH RADII, AND VELOCITY IN ER/KEMIN) ----
        NDDot  = NDDot * 10.0D0**Nexp
        NDot   = NDot / (XPDOTP*1440)
        NDDot  = NDDot / (XPDOTP*1440*1440)
        BStar  = BStar * 10.0D0**Bexp

        No     = No / XPDOTP
        a      = (No*TUMin)**(-2.0D0/3.0D0)
        Inclo  = Inclo  * Deg2Rad
        nodeo  = nodeo * Deg2Rad
        Argpo  = Argpo * Deg2Rad
        Mo     = Mo   * Deg2Rad
                                                                        
        IF (DABS(Ecco-1.0D0) .gt. 0.000001D0) THEN
            Altp= (a*(1.0D0-Ecco))-1.0D0
            Alta= (a*(1.0D0+Ecco))-1.0D0
          ELSE
            Alta= 999999.9D0
            Altp= 2.0D0* (4.0D0/(No*No)**(1.0D0/3.0D0))
          ENDIF

        ! ---- Ballistic Coefficient ----
        IF (DABS(BStar) .gt. 0.00000001D0) THEN
            BC= 1.0D0/(12.741621D0*BStar)
          ELSE
            BC= 1.111111111111111D0
          ENDIF

        ! ----------------------------------------------------------------
        ! find sgp4epoch time of element set
        ! remember that sgp4 uses units of days from 0 jan 1950 (sgp4epoch)
        ! and minutes from the epoch (time)
        ! ----------------------------------------------------------------

        ! Temporary year fix
        IF (EpochYr.lt.57) THEN
            Yr = EpochYr + 2000
          ELSE
            Yr = EpochYr + 1900
          ENDIF

        CALL Days2MDHMS( Yr,EpDay, Mon,Day,Hr,Minute,Sec )
        CALL JDAY ( Yr,Mon,Day,Hr,Minute,Sec,  JDSatEpoch )

        ! -------------- input start stop times manually --------------- 
        if ((typerun .ne. 'V') .and. (typerun .ne. 'C')) then
            ! --------- enter start/stop ymd hms values ---------------- 
            if (typeinput .eq. 'E') then
                write(*,*) 'input start y m d h m s '
                read(*,*) startyear,startmon,startday,starthr,startmin,
     &                    startsec
                CALL jday( startyear,startmon,startday,starthr,startmin,
     &                     startsec, jdstart)

                write(*,*)'input stop  y m d h m s '
                read(*,*) stopyear,stopmon,stopday,stophr,stopmin,
     &                    stopsec
                CALL jday( stopyear,stopmon,stopday,stophr,stopmin,
     &                     stopsec, jdstop )

                startmfe = (jdstart - jdsatepoch)*1440.0D0
                stopmfe  = (jdstop - jdsatepoch)*1440.0D0

                write(*,*)'input time step in minutes '
                read(*,*) deltamin
              ENDIF
            ! ------ enter start/stop year and days of year values -----
            if (typeinput .eq. 'D') then
                write(*,*) 'input start y dayofyr '
                read(*,*) startyear,startdayofyr

                write(*,*) 'input stop  y dayofyr '
                read(*,*) stopyear,stopdayofyr

                CALL days2mdhms ( startyear,startdayofyr, mon,day,hr,
     &                            minute,sec) 
                CALL jday( startyear,mon,day,hr,minute,sec, jdstart )
                CALL days2mdhms ( stopyear,stopdayofyr, mon,day,hr,
     &                            minute,sec )
                CALL jday( stopyear,mon,day,hr,minute,sec, jdstop )

                startmfe = (jdstart - jdsatepoch)*1440.0D0
                stopmfe  = (jdstop - jdsatepoch)*1440.0D0

                write(*,*)'input time step in minutes '
                read(*,*) deltamin
              ENDIF
           ! -------------- enter start/stop mfe values --------------- 
           IF (typeinput .eq. 'M') then
               write(*,*) 'input start min from epoch'
               read(*,*) startmfe
               write(*,*) 'input stop min from epoch'
               read(*,*) stopmfe
               write(*,*) 'input time step in minutes'
               read(*,*) deltamin
             ENDIF
          ENDIF

        ! -------- perform complete catalog evaluation, -+ 1 day ------- 
        if (typerun .eq. 'C') THEN
            startmfe = -1440.0D0
            stopmfe  =  1440.0D0
            deltamin =    10.0D0
          ENDIF

* ------------------- MAKE INITIAL PREDICTION AT EPOCH ----------------
        ! 2433281.5 - 2400000.5 = 33281.0, thus time from 1950
        CALL SGP4Init( whichconst,
     &                 SatNum,BStar, Ecco, JDSatEpoch-2433281.5D0,
     &                 Argpo,Inclo,Mo,No, nodeo, Error )

        ! ---- Write common block of data into file of record ----
        WRITE(15,Rec=NumSats) SatName,
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

        IF(Error .GT. 0) THEN
            WRITE( *,*) '# *** SGP4 Model Error ***',Error
          ENDIF

c      write tle output details
c      INCLUDE 'debug8.for'

        ! ---- Fix to indicate end-of-file
        GOTO 1000
  999   Code = 999
 1000   CONTINUE

       RETURN
       END  !       SUBROUTINE TwoLine2RVSGP4




