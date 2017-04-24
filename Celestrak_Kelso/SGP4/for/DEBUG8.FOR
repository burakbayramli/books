        IF (Show.eq.'Y') THEN
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


            WRITE(11,*) SatName
            WRITE(11,525)IYR,SRDAY
            WRITE(11,550)JYR,SPDAY
            WRITE(11,575)DELTAMin
  525 FORMAT(' THE REQUESTED START TIME IS YR ',I2,' AND DAY ',F12.8)
  550 FORMAT(' THE REQUESTED STOP  TIME IS YR ',I2,' AND DAY ',F12.8)    
  575 FORMAT(' THE DELTA TIME IN MINUTES IS ',F16.8,/)                   

            WRITE(11,700)
            WRITE(11,750)
            WRITE(11,*)
c            WRITE(11,800)T,ro(1),ro(2),ro(3),vo(1),vo(2),vo(3)
            WRITE(11,*)
             WRITE(11,*)
            IF((TWOPI/No) .GT. 225.0D0) THEN
                WRITE(11,775)
               ENDIF
  700 FORMAT('      POSITION AND VELOCITY at EPOCH')
  750 FORMAT(6X,'TSINCE (MIN)',7X,'X (KM)',11X,'Y (KM)',11X,'Z (KM)',   
     &       10X,'XDOT (KM/S)',6X,'YDOT (KM/S)',6X,'ZDOT (KM/S)')
  775 FORMAT('PERIOD > 225 MIN - DEEP SPACE PERTURBATIONS APPLIED',/)   
  800 FORMAT(F17.8,3F12.4,3F11.6)

                Write(11,'(A,I2,A4,I5,I3,A,I2,A,F7.4,A  )')
     &                   ' Epoch Time     ',Day,MonthTitle(Mon),Yr,Hr,
     &                   ':',minute,':',Sec,' UTC'
                Write(11,*) 'Beware: orbital elements may change durin',
     &                   'g init!!! '
                Write(11,'(A,F14.8,A,F12.5,A)') ' Semimajor axis       '
     &                   ,a,' ER = ',a*RadiusEarthkm, ' km'
                Write(11,'(A,F14.8,A)') ' Perigee altitude     '
     &                   ,Altp*RadiusEarthKm,' km'

                IF (Alta .lt. 999999.5D0) THEN
                    Write(11,'(A,F14.8,A)')  ' Apogee altitude      ',
     &                       Alta*RadiusEarthKm,' km'
                  ELSE
                    Write(11,'(A,A14,A)')
     &                       ' Apogee altitude      ','Infinity','  km'
                  ENDIF
                Write(11,'(A,F14.8,A)')  ' Period               ',
     &                   TwoPi/No,' min'

                CALL NEWTONM( Ecco,Mo,  E1,Nuo )

                Write(11,'(A,f15.9)')  ' Eccentricity         ',Ecco
                Write(11,'(A,f10.4,A)')  ' Inclination          ',
     &                   Inclo * Rad,'     ø'
                Write(11,'(A,f10.4,A)')  ' Long of Asc Node     ',
     &                   nodeo* Rad,'     ø'
                Write(11,'(A,f10.4,A)')  ' Arg of Perigee       ',
     &                   Argpo* Rad,'     ø'
                Write(11,'(A,f10.4,A)')  ' Mean Anomaly         ',
     &                   Mo  * Rad,'     ø'
                Write(11,'(A,f14.8,A)')  ' True Anomaly         ',
     &                   Nuo*Rad,' ø'
                Write(11,'(A,F14.8,A)')  ' Mean Motion          ',
     &                     No*XpDotp,' Rev/Day'
                Write(11,'(A,F14.8,A)')  ' N Dot                ',
     &                   NDot*XPDotp*1440,' .5Rev/day2'
                Write(11,'(A,F14.8,A)')  ' N Dble Dot           ',
     &                   NDDot*XPDotp*1440*1440,' 1/6 rev/day3'
                Write(11,'(A,F14.8,A)')  ' BStar                ',
     &                   BStar,' none (ERm2/kg)'
                Write(11,'(A,F12.6,A)')  ' Ballistic Coefficient',
     &                   BC,'  kg/m2'
                Write(11,*)
                Write(11,'(A,F12.6,A)')  ' Long of Asc Node rate',
     &                   nodeDot*Rad,'   ø/tu'
                Write(11,'(A,F12.6,A)')  ' Arg of perigee rate  ',
     &                   ArgpDot*Rad,'   ø/tu'
                Write(11,'(A)')  ' Eccentricity rate    '
                Write(11,*)
              ENDIF

