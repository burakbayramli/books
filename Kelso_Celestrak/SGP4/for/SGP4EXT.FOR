*     ----------------------------------------------------------------
*
*                               sgp4ext.for
*
*    this file contains extra routines needed for the main test program for sgp4.
*    these routines are derived from the astro libraries.
*
*                            companion code for
*               fundamentals of astrodynamics and applications
*                                    2007
*                              by david vallado
*
*       (w) 719-573-2600, email dvallado@agi.com
*
*    current :
*               2 apr 07  david vallado
*                           misc updates for new baseline
*    changes :
*              14 aug 06  david vallado
*                           original baseline
*       ----------------------------------------------------------------



* ------------------------------------------------------------------------------
*
*                           SUBROUTINE MAG
*
*  This subroutine finds the magnitude of a vector.  The tolerance is set to
*    0.00000001D0, thus the 1.0D0E-16 for the squared test of underflows.
*
*  Author        : David Vallado                  719-573-2600    1 Mar 2001
*
*  Inputs          Description                    Range / Units
*    Vec       - Vector
*
*  OutPuts       :
*    Vec       - Answer stored in fourth component
*
*  Locals        :
*    None.
*
*  Coupling      :
*    None.
*
* ------------------------------------------------------------------------------  

      REAL*8 FUNCTION MAG    ( Vec )
        IMPLICIT NONE
        REAL*8 Vec(3)
* -----------------------------  Locals  ------------------------------
        Real*8 Temp

        ! --------------------  Implementation   ----------------------
        Temp= Vec(1)*Vec(1) + Vec(2)*Vec(2) + Vec(3)*Vec(3)

        IF ( DABS( Temp ) .ge. 1.0D-16 ) THEN
            MAG = DSQRT( Temp )
          ELSE
            MAG = 0.0D0
          ENDIF
      RETURN
      END  ! end mag


* ------------------------------------------------------------------------------
*
*                           SUBROUTINE CROSS
*
*  This subroutine crosses two vectors.
*
*  Author        : David Vallado                  719-573-2600    1 Mar 2001
*
*  Inputs          Description                    Range / Units
*    Vec1        - Vector number 1
*    Vec2        - Vector number 2
*
*  OutPuts       :
*    OutVec      - Vector result of A x B
*
*  Locals        :
*    None.
*
*  Coupling      :
*    None.
*
* ------------------------------------------------------------------------------  

      SUBROUTINE CROSS       ( Vec1,Vec2, OutVec )
        IMPLICIT NONE
        REAL*8 Vec1(3), Vec2(3), OutVec(3)

        ! --------------------  Implementation   ----------------------
        OutVec(1)= Vec1(2)*Vec2(3) - Vec1(3)*Vec2(2)
        OutVec(2)= Vec1(3)*Vec2(1) - Vec1(1)*Vec2(3)
        OutVec(3)= Vec1(1)*Vec2(2) - Vec1(2)*Vec2(1)

      RETURN
      END  ! end cross


* ------------------------------------------------------------------------------
*
*                           FUNCTION DOT
*
*  This function finds the DOT product of two vectors.
*
*  Author        : David Vallado                  719-573-2600    1 Mar 2001
*
*  Inputs          Description                    Range / Units
*    Vec1        - Vector number 1
*    Vec2        - Vector number 2
*
*  OutPuts       :
*    DOT         - Result
*
*  Locals        :
*    None.
*
*  Coupling      :
*    None.
*
* ------------------------------------------------------------------------------  

      REAL*8 FUNCTION DOT    ( Vec1,Vec2 )
        IMPLICIT NONE
        REAL*8 Vec1(3), Vec2(3)

        ! --------------------  Implementation   ----------------------
        DOT= Vec1(1)*Vec2(1) + Vec1(2)*Vec2(2) + Vec1(3)*Vec2(3)
      RETURN
      END  ! end dot


* ------------------------------------------------------------------------------
*
*                           SUBROUTINE ANGLE
*
*  This subroutine calculates the ANGLE between two vectors.  The output is
*    set to 999999.1D0 to indicate an undefined value.  Be SURE to check  
*    this at the output phase.
*
*  Author        : David Vallado                  719-573-2600    1 Mar 2001
*
*  Inputs          Description                    Range / Units
*    Vec1        - Vector number 1
*    Vec2        - Vector number 2
*
*  OutPuts       :
*    Theta       - ANGLE between the two vectors  -Pi to Pi
*
*  Locals        :
*    Temp        - Temporary REAL variable
*
*  Coupling      :
*    DOT           DOT Product of two vectors
*    DACOS         Arc Cosine FUNCTION
*
* ------------------------------------------------------------------------------  

      SUBROUTINE ANGLE       ( Vec1,Vec2, Theta )
        IMPLICIT NONE
        REAL*8 Vec1(3), Vec2(3), Theta, magvec1, magvec2
        EXTERNAL Dot, Mag
* -----------------------------  Locals  ------------------------------
        REAL*8 Temp, Dot, Mag
        INCLUDE 'astmath.cmn'

        ! --------------------  Implementation   ----------------------
        magvec1 = MAG(vec1)
        magvec2 = MAG(vec2)
        IF ( magVec1*magVec2 .gt. Small**2 ) THEN
            Temp= DOT(Vec1,Vec2) / (magVec1*magVec2)
            IF ( DABS( Temp ) .gt. 1.0D0 ) THEN
                Temp= DSIGN(1.0D0, Temp)
              ENDIF
            Theta= DACOS( Temp ) 
          ELSE
            Theta= Undefined
          ENDIF
      RETURN
      END  ! end angle


* ------------------------------------------------------------------------------
*
*                           FUNCTION ASINH
*
*  This function evaluates the inverse hyperbolic sine.
*
*  Author        : David Vallado                  719-573-2600    1 Mar 2001
*
*  Inputs          Description                    Range / Units
*    XVal        - ANGLE Value                                  any real
*
*  OutPuts       :
*    ASINH       - Result                                       any real
*
*  Locals        :
*    None.
*
*  Coupling      :
*    None.
*
* ------------------------------------------------------------------------------  

      REAL*8 FUNCTION ASINH( XVal )
        IMPLICIT NONE
        REAL*8 XVal

        ! --------------------  Implementation   ----------------------
        ASINH= DLOG( XVal + DSQRT( XVal*XVal + 1.0D0 ) )

      RETURN
      END  ! end asinh


* ------------------------------------------------------------------------------
*
*                           SUBROUTINE NEWTONNU
*
*  This subroutine solves Keplers equation when the true anomaly is known.
*    The Mean and Eccentric, parabolic, or hyperbolic anomaly is also found.
*    The parabolic limit at 168ø is arbitrary. The hyperbolic anomaly is also
*    limited. The hyperbolic sine is used because it's not double valued.
*
*  Author        : David Vallado                  719-573-2600    1 Mar 2001
*
*  Inputs          Description                    Range / Units
*    Ecc         - Eccentricity                   0.0D0 to
*    Nu          - True Anomaly                   -2Pi to 2Pi rad
*
*  Outputs       :
*    E0          - Eccentric Anomaly              0.0D0 to 2Pi rad       153.02 deg
*    M           - Mean Anomaly                   0.0D0 to 2Pi rad       151.7425 deg 
*
*  Locals        :
*    E1          - Eccentric Anomaly, next value  rad
*    SinE        - Sine of E
*    CosE        - Cosine of E
*    Ktr         - Index
*
*  Coupling      :
*    ASINH       - Arc hyperbolic sine
*    SINH        - Hyperbolic Sine
*
*  References    :
*    Vallado       2007, 85, Alg 5
*
* ------------------------------------------------------------------------------

      SUBROUTINE NEWTONNU    ( Ecc, Nu, E0, M )
        IMPLICIT NONE
        REAL*8 Ecc, Nu, E0, M
        EXTERNAL ASINH
* -----------------------------  Locals  ------------------------------
        REAL*8 SinE, CosE, ASINH

        INCLUDE 'astmath.cmn'

        ! --------------------  Implementation   ----------------------
        E0= 999999.9D0
        M = 999999.9D0
        ! --------------------------- Circular ------------------------
        IF ( DABS( Ecc ) .lt. 0.000001D0 ) THEN
            M = Nu
            E0= Nu 
          ELSE
            ! ---------------------- Elliptical -----------------------
            IF ( Ecc .lt. 0.999D0 ) THEN
                SinE= ( DSQRT( 1.0D0-Ecc*Ecc ) * DSIN(Nu) ) /
     &                ( 1.0D0+Ecc*DCOS(Nu) )
                CosE= ( Ecc + DCOS(Nu) ) / ( 1.0D0 + Ecc*DCOS(Nu) )
                E0  = DATAN2( SinE, CosE )
                M   = E0 - Ecc*DSIN(E0) 
              ELSE
                ! -------------------- Hyperbolic  --------------------
                IF ( Ecc .gt. 1.0001D0 ) THEN
                    IF ( ((Ecc .gt. 1.0D0) .and. (DABS(Nu)+0.00001D0
     &                     .lt. Pi-DACOS(1.0D0/Ecc)) ) ) THEN
                        SinE= ( DSQRT( Ecc*Ecc-1.0D0 ) * DSIN(Nu) ) /
     &                        ( 1.0D0 + Ecc*DCOS(Nu) )
                        E0  = ASINH( SinE )
                        M   = Ecc*DSINH(E0) - E0
                      ENDIF 
                  ELSE
                    ! ----------------- Parabolic ---------------------
                    IF ( DABS(Nu) .lt. 168.0D0/57.29578D0 ) THEN
                        E0= DTAN( Nu*0.5D0 )
                        M = E0 + (E0*E0*E0)/3.0D0 
                      ENDIF
                  ENDIF
              ENDIF
          ENDIF

        IF ( Ecc .lt. 1.0D0 ) THEN
            M = DMOD( M, 2.0D0*Pi )
            IF ( M .lt. 0.0D0 ) THEN
                M= M + 2.0D0*Pi 
              ENDIF
            E0 = DMOD( E0, 2.0D0*Pi )
          ENDIF 
      RETURN
      END  ! end newtonnu


* ------------------------------------------------------------------------------
*
*                           SUBROUTINE rv2coe
*
*  This subroutine finds the classical orbital elements given the Geocentric
*    Equatorial Position and Velocity vectors.
*
*  Author        : David Vallado                  719-573-2600    1 Mar 2001
*
*  Inputs          Description                    Range / Units
*    R           - IJK Position vector            km
*    V           - IJK Velocity vector            km / s
*    mu          - gravitational parameter        km3 / s2
*
*  Outputs       :
*    P           - SemiLatus rectum               km
*    A           - semimajor axis                 km
*    Ecc         - Eccentricity
*    Incl        - inclination                    0.0D0 to Pi rad
*    Omega       - Longitude of Ascending Node    0.0D0 to 2Pi rad
*    Argp        - Argument of Perigee            0.0D0 to 2Pi rad
*    Nu          - True anomaly                   0.0D0 to 2Pi rad
*    M           - Mean anomaly                   0.0D0 to 2Pi rad
*    ArgLat      - Argument of Latitude      (CI) 0.0D0 to 2Pi rad
*    LamTrue     - True Longitude            (CE) 0.0D0 to 2Pi rad
*    LonPer      - Longitude of Periapsis    (EE) 0.0D0 to 2Pi rad
*
*  Locals        :
*    HBar        - Angular Momentum H Vector      km2 / s
*    EBar        - Eccentricity     E Vector
*    NBar        - Line of Nodes    N Vector
*    c1          - V**2 - u/R
*    RDotV       - R DOT V
*    Hk          - Hk norm vector
*    SME         - Specfic Mechanical Energy      km2 / s2
*    i           - index
*    E           - Eccentric, Parabolic,
*                  Hyperbolic Anomaly             rad
*    Temp        - Temporary variable
*    TypeOrbit   - Type of orbit                  EE, EI, CE, CI
*
*  Coupling      :
*    MAG         - Magnitude of a vector
*    CROSS       - CROSS product of two vectors
*    DOT         - DOT product of two vectors
*    ANGLE       - Find the ANGLE between two vectors
*    NEWTONNU    - Find the mean anomaly
*
*  References    :
*    Vallado       2007, 121, Alg 9, Ex 2-5
*
* ------------------------------------------------------------------------------

      SUBROUTINE rv2coe      ( R, V, mu, P, A, Ecc, Incl, Omega, Argp,
     &                         Nu, M, ArgLat, TrueLon, LonPer )
        IMPLICIT NONE
        REAL*8 R(3), V(3), mu, P, A, Ecc, Incl, Omega, Argp, Nu, M, 
     &         ArgLat, TrueLon, LonPer
        EXTERNAL DOT, MAG
* -----------------------------  Locals  ------------------------------
        REAL*8 c1, RDotV, hk, SME, Hbar(3), Ebar(3), Nbar(3),
     &         Dot, E, Temp, MAG, maghbar, magnbar, magr, magv
        INTEGER i
        CHARACTER*2 TypeOrbit

        INCLUDE 'astmath.cmn'

        ! --------------------  Implementation   ----------------------
        magr = MAG( R )
        magv = MAG( V )
        ! ------------------  Find H N and E vectors   ----------------
        CALL CROSS( R, V, HBar )
        maghbar = MAG(Hbar)
        IF ( maghbar .gt. Small ) THEN
            NBar(1)= -HBar(2)
            NBar(2)=  HBar(1)
            NBar(3)=   0.0D0
            magnbar = MAG( Nbar )
            c1 = magv**2 - mu/magr
            RDotV= DOT( R, V )
            DO i= 1 , 3
                EBar(i)= (c1*R(i) - RDotV*V(i))/mu
              ENDDO

            Ecc = MAG( EBar )

            ! ------------  Find a e and semi-Latus rectum   ----------
            SME= ( magv*magv*0.5D0 ) - ( mu/magr )
            IF ( DABS( SME ) .gt. Small ) THEN
                A= -mu / (2.0D0*SME)
              ELSE
                A= Infinite
              ENDIF
            P = maghbar*maghbar/mu

            ! -----------------  Find inclination   -------------------
            Hk= HBar(3)/maghbar
c            IF ( DABS( DABS(Hk) - 1.0D0 ) .lt. Small ) THEN
c                ! -------------  Equatorial Orbits   ------------------
c                IF ( DABS(HBar(3)) .gt. 0.0D0 ) THEN
c                    Hk= DSIGN(1.0D0, HBar(3))
c                  ENDIF
c              ENDIF
            Incl= DACOS( Hk ) 

            ! --------  Determine type of orbit for Later use  --------
            ! ------ Elliptical, Parabolic, Hyperbolic Inclined -------
            TypeOrbit= 'EI' 
            IF ( Ecc .lt. Small ) THEN
                ! ----------------  Circular Equatorial ---------------
                IF ( (Incl.lt.Small).or.(DABS(Incl-Pi).lt.Small) ) THEN
                    TypeOrbit= 'CE'
                  ELSE
                    ! --------------  Circular Inclined ---------------
                    TypeOrbit= 'CI'
                  ENDIF
              ELSE
                ! - Elliptical, Parabolic, Hyperbolic Equatorial --
                IF ( (Incl.lt.Small).or.(DABS(Incl-Pi).lt.Small) ) THEN
                    TypeOrbit= 'EE'
                  ENDIF
              ENDIF

            ! ----------  Find Longitude of Ascending Node ------------
            IF ( magnbar .gt. Small ) THEN
                Temp= NBar(1) / magnbar
                IF ( DABS(Temp) .gt. 1.0D0 ) THEN
                    Temp= DSIGN(1.0D0, Temp)
                  ENDIF
                Omega= DACOS( Temp ) 
                IF ( NBar(2) .lt. 0.0D0 ) THEN
                    Omega= TwoPi - Omega
                  ENDIF
              ELSE
                Omega= Undefined 
              ENDIF

            ! ---------------- Find Argument of perigee ---------------
            IF ( TypeOrbit .eq. 'EI' ) THEN
                CALL ANGLE( NBar, EBar, Argp )
                IF ( EBar(3) .lt. 0.0D0 ) THEN
                    Argp= TwoPi - Argp 
                  ENDIF
              ELSE
                Argp= Undefined 
              ENDIF

            ! ------------  Find True Anomaly at Epoch    -------------
            IF ( TypeOrbit(1:1) .eq. 'E' ) THEN
                CALL ANGLE( EBar, r, Nu )
                IF ( RDotV .lt. 0.0D0 ) THEN
                    Nu= TwoPi - Nu 
                  ENDIF
              ELSE
                Nu= Undefined 
              ENDIF

            ! ----  Find Argument of Latitude - Circular Inclined -----
            IF ( TypeOrbit .eq. 'CI' ) THEN
                CALL ANGLE( NBar, R, ArgLat )
                IF ( R(3) .lt. 0.0D0 ) THEN
                    ArgLat= TwoPi - ArgLat
                  ENDIF
              ELSE
                ArgLat= Undefined 
              ENDIF

            ! -- Find Longitude of Perigee - Elliptical Equatorial ----
            IF ( ( Ecc.gt.Small ) .and. (TypeOrbit.eq.'EE') ) THEN
                Temp= EBar(1)/Ecc
                IF ( DABS(Temp) .gt. 1.0D0 ) THEN
                    Temp= DSIGN(1.0D0, Temp)
                  ENDIF
                LonPer= DACOS( Temp ) 
                IF ( EBar(2) .lt. 0.0D0 ) THEN
                    LonPer= TwoPi - LonPer 
                  ENDIF
                IF ( Incl .gt. HalfPi ) THEN
                    LonPer= TwoPi - LonPer
                  ENDIF
              ELSE
                LonPer= Undefined
              ENDIF

            ! -------- Find True Longitude - Circular Equatorial ------
            IF ( ( magr.gt.Small ) .and. ( TypeOrbit.eq.'CE' ) ) THEN
                Temp= R(1)/magr
                IF ( DABS(Temp) .gt. 1.0D0 ) THEN
                    Temp= DSIGN(1.0D0, Temp)
                  ENDIF
                TrueLon= DACOS( Temp )
                IF ( R(2) .lt. 0.0D0 ) THEN
                    TrueLon= TwoPi - TrueLon
                  ENDIF
                IF ( Incl .gt. HalfPi ) THEN
                    TrueLon= TwoPi - TrueLon
                  ENDIF
              ELSE
                TrueLon= Undefined
              ENDIF

            ! ------------ Find Mean Anomaly for all orbits -----------
            CALL NEWTONNU(Ecc, Nu, E, M )

         ELSE
           P    = Undefined
           A    = Undefined
           Ecc  = Undefined
           Incl = Undefined
           Omega= Undefined 
           Argp = Undefined 
           Nu   = Undefined 
           M    = Undefined 
           ArgLat  = Undefined 
           TrueLon= Undefined 
           LonPer = Undefined 
         ENDIF 

      RETURN
      END  ! end rv2coe


* -----------------------------------------------------------------------------
*
*                           SUBROUTINE JDay
*
*  This subroutine finds the Julian date given the Year, Month, Day, and Time.
*
*  Author        : David Vallado                  719-573-2600    1 Mar 2001
*
*  Inputs          Description                    Range / Units
*    Year        - Year                           1900 .. 2100
*    Mon         - Month                          1 .. 12
*    Day         - Day                            1 .. 28,29,30,31
*    Hr          - Universal Time Hour            0 .. 23
*    Min         - Universal Time Min             0 .. 59
*    Sec         - Universal Time Sec             0.0D0 .. 59.999D0
*    WhichType   - Julian .or. Gregorian calender   'J' .or. 'G'
*
*  Outputs       :
*    JD          - Julian Date                    days from 4713 BC
*
*  Locals        :
*    B           - Var to aid Gregorian dates
*
*  Coupling      :
*    None.
*
*  References    :
*    Vallado       2007, 189, Alg 14, Ex 3-14
* -----------------------------------------------------------------------------

      SUBROUTINE JDay        ( Year,Mon,Day,Hr,Min, Sec, JD )
        IMPLICIT NONE
        INTEGER Year, Mon, Day, Hr, Min
        REAL*8  Sec, JD

        ! --------------------  Implementation   ----------------------
        JD= 367.0D0 * Year
     &        - INT( (7* (Year+INT ( (Mon+9)/12.0) ) ) * 0.25D0 )
     &        + INT( 275*Mon / 9.0 )
     &        + Day + 1721013.5D0
     &        + ( (Sec/60.0D0 + Min ) / 60.0D0 + Hr ) / 24.0D0
*     &      - 0.5D0*DSIGN(1.0D0, 100.0D0*Year + Mon - 190002.5D0) + 0.5D0
      RETURN
      END  ! end jday


* -----------------------------------------------------------------------------
*
*                           SUBROUTINE DAYS2MDHMS
*
*  This subroutine converts the day of the year, days, to the equivalent month
*    day, hour, Minute and second.
*
*  Algorithm     : Set up array for the Number of days per month
*                  Find Leap Year - be sure to account for the 400 years
*                  Loop through a Temp value for WHILE the value is .lt. the days
*                  Perform INTEGER conversions to the correct day and month
*                  Convert remainder into H M S using type conversions
*
*  Author        : David Vallado                  719-573-2600    1 Mar 2001
*
*  Inputs          Description                    Range / Units
*    Year        - Year                          +1900 .. 2100+
*    Days        - Julian Day of the year         0.0D0  .. 366.0D0
*
*  OutPuts       :
*    Mon         - Month                          1 .. 12
*    Day         - Day                            1 .. 28,29,30,31
*    Hr          - Hour                           0 .. 23
*    Min         - Minute                         0 .. 59
*    Sec         - Second                         0.0D0 .. 59.999D0
*
*  Locals        :
*    DayofYr     - Day of year
*    Temp        - Temporary REAL*8 values
*    IntTemp     - Temporary INTEGER value
*    i           - Index
*    LMonth[12]  - INTEGER Array containing the Number of days per month
*
*  Coupling      :
*    None.
* -----------------------------------------------------------------------------

      SUBROUTINE DAYS2MDHMS  ( Year,Days,  Mon,Day,Hr,Min,Sec )
        IMPLICIT NONE
        REAL*8 Days,Sec
        INTEGER Year, Mon, Day, Hr, Min
* ----------------------------  Locals  -------------------------------
        INTEGER IntTemp,i,DayofYr, LMonth(12)
        REAL*8 Temp

        ! --------------------  Implementation   ----------------------
        ! -------------- Set up array of days in month  ---------------
        DO i = 1,12
            LMonth(i) = 31
          ENDDO
        LMonth( 2) = 28
        LMonth( 4) = 30
        LMonth( 6) = 30
        LMonth( 9) = 30
        LMonth(11) = 30

        DayofYr= IDINT(Days )

        ! ---------------- Find month and Day of month ----------------
        IF (MOD(Year,4).eq.0) THEN
            LMonth(2)= 29
          ENDIF
        i= 1
        IntTemp= 0
        DO WHILE ( (DayofYr.gt.IntTemp + LMonth(i) ) .and. ( i.lt.12 ))
            IntTemp= IntTemp + LMonth(i)
            i= i+1
          ENDDO
        Mon= i
        Day= DayofYr - IntTemp

        ! ---------------- Find hours Minutes and seconds -------------
        Temp= (Days - DayofYr )*24.0D0
        Hr  = IDINT( Temp )
        Temp= (Temp-Hr) * 60.0D0
        Min = IDINT( Temp )
        Sec = (Temp-Min) * 60.0D0

        ! ---- Check for roundoff errors
c        IF (Sec .ge. 59.9999D0) THEN
c            Sec = 0.0D0
c            Min = Min + 1
c            IF (Min .gt. 59) THEN
c                Min = 0
c                Hr = Hr + 1
c                IF (Hr .gt. 23) THEN
c                    Hr = 0
c                    Day = Day + 1
c                  ENDIF
c              ENDIF
c          ENDIF
      RETURN
      END  ! end days2mdhms


* -----------------------------------------------------------------------------
*
*                           SUBROUTINE INVJDay
*
*  This subroutine finds the Year, month, day, hour, Minute and second
*  given the Julian date. TU can be UT1, TDT, TDB, etc.
*
*  Author        : David Vallado                  719-573-2600    1 Mar 2001
*
*  Inputs          Description                    Range / Units
*    JD          - Julian Date                    days from 4713 BC
*
*  OutPuts       :
*    Year        - Year                           1900 .. 2100
*    Mon         - Month                          1 .. 12
*    Day         - Day                            1 .. 28,29,30,31
*    Hr          - Hour                           0 .. 23
*    Min         - Minute                         0 .. 59
*    Sec         - Second                         0.0D0 .. 59.999D0
*
*  Locals        :
*    Days        - Day of year plus fractional
*                  portion of a day               days
*    Tu          - Julian Centuries from 0 h
*                  Jan 0, 1900
*    Temp        - Temporary real values
*    LeapYrs     - Number of Leap years from 1900
*
*  Coupling      :
*    DAYS2MDHMS  - Finds MD HMS given Days and Year
*
*  References    :
*    Vallado       2007, 208, Alg 22, Ex 3-13
* -----------------------------------------------------------------------------

      SUBROUTINE INVJDay     ( JD, Year,Mon,Day,Hr,Min, Sec )
        IMPLICIT NONE
        INTEGER Year, Mon, Day, Hr, Min
        REAL*8  Sec, JD
* ----------------------------  Locals  -------------------------------
        INTEGER LeapYrs
        REAL*8  Days, Tu, Temp

        ! --------------------  Implementation   ----------------------
        ! ---------------- Find Year and Days of the year -------------
        Temp   = JD-2415019.5D0
        Tu     = Temp / 365.25D0
        Year   = 1900 + IDINT( Tu )
        LeapYrs= IDINT( ( Year-1901 )*0.25D0 )
        Days   = Temp - ((Year-1900)*365.0D0 + LeapYrs )

        ! -------------- Check for case of beginning of a year --------
        IF ( Days .lt. 1.0D0 ) THEN
            Year   = Year - 1
            LeapYrs= IDINT( ( Year-1901 )*0.25D0 )
            Days   = Temp - ((Year-1900)*365.0D0 + LeapYrs )
          ENDIF

        ! ------------------ Find remaing data  -----------------------
        CALL DAYS2MDHMS( Year,Days, Mon,Day,Hr,Min,Sec )

      RETURN
      END

