c     sample main program to check stdatm

c     w.h. mason, Feb. 27, 1994
c     Dept. of Aerospace and Ocean Engineering
c     Blacksburg, VA 24061
c     mason@aoe.vt.edu

c     this is a sample main showing how to
c     call stdatm
c     the loop is done twice to get output
c     suitable to include in text(80 col)

      real mu

      kd      = 1

      write(6,90)

      do 10 i = 1,21
      z       = 5000.*(i-1)

      call stdatm(z,t,p,r,a,mu,ts,rr,pp,rm,qm,kd,kk)

      if (kk .ne. 0) then
                     write(6,120)
                     stop
                     endif

      write(6,100) z,t,p,r,a,mu
   10 continue

      write(6,110)
      do 20 i = 1,21
      z       = 5000.*(i-1)

      call stdatm(z,t,p,r,a,mu,ts,rr,pp,rm,qm,kd,kk)

      if (kk .ne. 0) then
                     write(6,160)
                     stop
                     endif

      write(6,120) z,ts,rr,pp,rm,qm
   20 continue

   90 format(/3x,'1976 Standard Atmoshere'//
     1        3x,'    alt      T       P        Rho',
     2        2x,'     a         Mu',
     4       /3x,'   (ft)   (deg R)  (psf)    (s/ft^3)',
     5        2x,' (f/s)  (slugs/ft/sec)')
  100 format(3x,f9.1,f8.2,f8.2,e12.4,f8.2,e12.4)
  110 format(/3x,'1976 Standard Atmoshere'//
     1        3x,'    alt    T/Tsl  R/Rsl',
     2        2x, 'P/Psl  Re/M/ft    q/M^2',
     4       /3x,'   (ft)',34x,'(lb/ft^2)')
  120 format(3x,f9.1,3f7.4,e10.3,f10.4)
  160 format(/4x,'error in return code from stdatm - pgm stops'/)
      stop
      end

      subroutine stdatm(z,t,p,r,a,mu,ts,rr,pp,rm,qm,kd,kk)
c
c   *********** 1976 STANDARD ATMOSPHERE SUBROUTINE **********
c
c     Mason's BASIC program, converted to FORTRAN - Sept. 1, 1989
c
c     W.H. Mason
c     Department of Aerospace and Ocean Engineering
c     Virginia Tech, Blacksburg, VA 24061
c     email: mason@aoe.vt.edu
c
c     kd -  = 0 - metric units
c          <> 0 - English units
c
c     kk -    0 - good return
c             1 - error: altitude out of table,
c                 do not use output (max altitude for this
c                 routine is 84.85 km or 282,152 ft.)
c
c     z  - input altitude, in feet or meters (depending on kd)
c
c     output:
c                      units: metric        English
c     t  - temp.               deg K         deg R
c     p  - pressure            N/m^2         lb/ft^2
c     r  - density (rho)       Kg/m^3        slug/ft^3
c     a  - speed of sound      m/sec         ft/sec
c     mu - viscosity           Kg/(m sec)    slug/<ft sec)
c     
c     ts - t/t at sea level
c     rr - rho/rho at sea level
c     pp - p/p at sea level
c
c     rm - Reynolds number per Mach per unit of length
c     qm - dynamic pressure/Mach^2
c
      real k, h, mu, ml
      KK = 0
      K  = 34.163195
      C1 = 3.048E-04
      IF (KD .eq. 0) go to 1240
      TL = 518.67
      PL = 2116.22
      RL = .0023769
      AL = 1116.45
      ML = 3.7373E-07
      BT = 3.0450963E-08
      GO TO 1260

 1240 TL = 288.15
      PL = 101325
      RL = 1.225
      C1 = .001
      AL = 340.294
      ML = 1.7894E-05
      BT = 1.458E-06

 1260 H = C1 * Z / (1 + C1 * Z / 6356.766)
      IF (H .gt. 11.0) go to 1290
      T  = 288.15 - 6.5 * H
      PP = (288.15 / T) ** ( - K / 6.5)
      GO TO 1420

 1290 IF (H .gt. 20.0) go to 1310
      T  = 216.65
      PP = .22336 *  EXP ( - K * (H - 11) / 216.65)
      GO TO 1420

1310  IF (H .gt. 32.0) go to 1330
      T  = 216.65 + (H - 20)
      PP = .054032 * (216.65 / T) ** K
      GO TO 1420

1330  IF (H .gt. 47.0) go to 1350
      T  = 228.65 + 2.8 * (H - 32)
      PP = .0085666 * (228.65 / T) ** (K / 2.8)
      GO TO 1420

1350  IF( H .gt. 51.0) go to 1370
      T  = 270.65
      PP = .0010945 *  EXP ( - K * (H - 47) / 270.65)
      GO TO 1420

1370  IF (H .gt. 71.) go to 1390
      T  = 270.65 - 2.8 * (H - 51)
      PP = .00066063 * (270.65 / T) ** ( - K / 2.8)
      GO TO 1420

1390  IF (H .gt. 84.852) THEN 
                              kk = 1
                              write(6,200) H
                              return
                         END IF

      T = 214.65 - 2 * (H - 71)
      PP = 3.9046E-05 * (214.65 / T) ** ( - K / 2)

1420  RR = PP / (T / 288.15)
      MU = BT * T**1.5 / (T + 110.4)
      TS = T / 288.15
      A  = AL *  SQRT (TS)
      T  = TL * TS
      R  = RL * RR
      P  = PL * PP
      RM = R * A / MU
      QM = .7 * P

  200 format('   Out of Table in StdAtm- too high !'//
     1        4x,'H =',f12.3,'  > 84.852 km'/)

      return
      end
