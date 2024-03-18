
      program foilgen

c **********************************************************
c *      airfoil coordinates from AEROCAL PAK2:            *            
c *        GEOMETRY FOR AERODYNAMICS, BY W.H. MASON        *
c * now: Department of Aerospace and Ocean Engineering     *
c *      Virginia Tech, Blacksburg, VA, 24062              *
c *             contact: mason@aoe.vt.edu                  *
c *          see also: http://www.aoe.vt.edu/              *
c *                                                        *
c *                                                        *
c *     APPLESOFT VERSION FOR APPLE ][ SERIES COMPUTERS    *
c *     RELEASE 1.2           LAST MOD: JANUARY 4, 1987    *
c *                                                        *
c *      PORTED WITH MINIMUM MODS TO MAC COMPUTER FOR USE  *
c *       WITH QuickBASIC and no print out capability -    *
c *                                                        *
c *       FEBRUARY 1990                                    *
c *                                                        *
c *      Airfoil Ordinate Generation portion of the Code   *
c *                                                        *
c *      mod to FORTRAN - March 1992                       *
c *      mod to run on WATFOR - March 1993                 *
c *                                                        *
c *      note: the translation from AppleSoft is           *
c *            responsible for the two character           *
c *            variable names!                             *
c **********************************************************

      common /consts/ a0,a1,a2,a3,a4,d1,d2,d3
      common /contrl/ tk,xmt,xmc,cx,xmm,xk1,A,ci,
     1                g,h,tt,xmx,ntk,nck

      DIMENSION XC(131),XU(131),YU(131),XL(131),YL(131)
      character*20 filenm
      character*79 filetl
      character*1 ans
      PI = 3.14159265

      write(6,500)

c     thickness distribution options

  100 write(6,510)
      read(5,*) TK
      ntk  = tk
      IF (ntk .eq. 1 .or. ntk .eq. 2 ) go to 10
      write(6,530) 
      GO TO 100

c     define maximum thickness

   10 write(6,540)
      read(5,*) XMT

      A0 = 1.4845
      A1 = 0.6300
      A2 = 1.7580
      A3 = 1.4215
      A4 = 0.5075
      RL = 1.1019 * XMT**2
      TA = 2. * 180. / PI *  ATAN(1.16925 * XMT)

      IF (ntk .eq. 1) go to 1660

c     define position of max thickness position, xt/c

      write(6,550)
      read(5,*) xmx

c     define leading edge parameter I

      write(6,560) 
      read(5,*) RE
      nre = re
      RX  = RE / 6.
      IF (nRE .eq. 9) RX = 10.3923 / 6.
      RL  = 1.1019 * (RX * XMT)**2

      call n4modp(xmx,rx,d1,d2,d3,a0,a1,a2,a3)

      TA = 360. / PI *  ATAN (5. * XMT * D1)

1660  continue
      write(6,570) rl,ta
 
c     select camber distribution options

  200 write(6,580)
      read(5,*)ck
      nck = ck
      IF (nck .eq. 1 .or. nck .eq. 2 .or. nck .eq. 3) go to 1695
      write(6,530)
      GO TO 200

 1695 if( nCK .eq. 1) GO TO 1700
      if( nCK .eq. 2) GO TO 1720
      if( nCK .eq. 3) GO TO 1742

c     NACA 4 digit camber line

c     input maximum camber

 1700 write(6,600)
      read(5,*) xmc
      CX = .3
      IF (abs(xmc) .lt. 0.00001) go to 1760

c     input x/c position of maximum camber

      write(6,610)
      read(5,*) CX
      GO TO 1760

c     NACA 5 digit camber line

c     input max camber

 1720 write(6,620)
      read(5,*) xmc
      CL  = 1.5 * xmc
      XF  = .4
      xMM = XF

      IF (abs(xmc) .lt. 0.00001) go to 1760

c     input x/c position of max camber

      write(6,630)
      read(5,*) XF

      xMM = XF
      EM  = 1.E-06

 1732 F   = XF - xMM + xMM**1.5 /sqrt(3.)
      FP  =  Sqrt(3.)/2. *  Sqrt(xMM) - 1.
      xMN = xMM - F/FP
      EP  =  ABS (xMN - xMM)
      xMM = xMN
      IF (EP .gt. EM) go to 1732

      C1  =  (3.* xMM - 7.*xMM**2 + 8.*xMM**3 - 4.*xMM**4)
     1      /sqrt(xMM*(1. - xMM))
      Q   = C1 - 1.5*(1 - 2.*xMM) * 
     1           (PI/2. - ASIN(1. - 2.*xMM))
      xK1 = 6. * CL / Q

      GO TO 1760
c
c     6 and 6A camber line
c
c     enter the design lift coefficient

 1742 write(6,640)
      read(5,*) CI
      A = 1.
      IF (abs(CI) .lt. 0.00001) go to 1760

c     input x/c constant loading position, A

      write(6,650)
      read(5,*) A

      ans = 'N'
      IF (abs(A - .8) .gt. .0001)  go to 1750

c     establish if this is an A series camber line

      write(6,660)
      read(5,'(a)') ans
 1750 if (ans .eq. 'Y' .or. ans .eq. 'y') CI = CI / 1.0209

      if (abs(A - 1.) .lt. 0.00001) go to 1760

      G =  - .25
      IF (A .gt. 0.) G = - 1./(1. - A) * 
     1                     (A**2 * (alog(A)/2. - .25) + .25)
      H  = (1. - A) * ( alog (1. - A) / 2. - .25) + G
      AL = CI * H / 2. / PI / (1. + A)

c     REM  * CHOOSE TYPE OF OUTPUT

 1760 continue

      call outopt(nopt)

      IF(nopt .eq.2) go to 1900

  665 write(6,670)
      read(5,*) x
      i  = 1
      im = i

      CALL COMP(x,yt,dt,d2t,yc,dc,xu(i),yu(i),xl(i),yl(i),ans)

      if(dt  .gt. 100.0) dt  = 99.99999
      if(d2t .gt. 100.0) d2t = 99.99999

      ytcurv = (1 + DT**2)**1.5 /ABS(D2t)
      theta  = 180. * TT / PI
      write(6,680) x,yt,dt,d2t,ytcurv,yc,dc,theta
      write(6,690) xu(i),yu(i),xl(i),yl(i)

      write(6,700)
      read(5,'(a)') ans

      IF (ans .eq. 'Y' .or. ans .eq. 'y') then
                                          GO TO 665
                                          else
                                          go to 999
                                          end if
 
c     REM  * SELECT AND GET X DISTRIBUTION

 1900 continue

      call outdst(im,xc)

 1940 continue
 
c      REM  * SETUP and print results for DISTRIBUTION

 2000 CONTINUE

      write(6,710)
      write(6,720)

      do 750 i = 1,im
      X = XC(I)
      CALL COMP(x,yt,dt,d2t,yc,dc,xu(i),yu(i),xl(i),yl(i),ans)
      IF (DT .gt. 100.) DT = 99.9999
      xuperc = 100.*xu(i)
      yuperc = 100.*yu(i)
      xlperc = 100.*xl(i)
      ylperc = 100.*yl(i) 
      WRITE(6,730) i,x,yt,dt,yc,dc,xuperc,yuperc,xlperc,ylperc
  750 continue

      write(6,720)
c
c     output a file in standard format
c
      WRITE(6,1020)
      read(5,'(a)') ans
      IF(ans .EQ. 'N' .or. ans .EQ. 'n') GO to 999

      write(6,1040) 
      read(5,'(a)') filenm
      open(unit = 2, file = filenm, status = 'new')

      write(6,1050)
      read(5,'(a)') filetl
      write(2,1060) filetl
      xim = im
      write(2,1080) xim,xim
      write(2,1070)
      do 800 i = 1,im
  800 write(2,1080) xu(i),yu(i)
      write(2,1090)
      do 810 i = 1,im
  810 write(2,1080) xl(i),yl(i)

      close(2)

  999 continue

      write(6,1100)
      read(5,'(a)') ans
      if(ans .eq. 'Y' .or. ans .eq. 'y') go to 100

      stop

  500 format(/5x,'foilgen:',1x,
     1            'NACA Airfoil Ordinate Generation'/
     2       /5x,'W.H. Mason, March 15, 1992'/
     3 5x,'Department of Aerospace and Ocean Engineering'/
     4 5x,'Virginia Tech, Blacksburg, VA, 24062'/
     5 5x,'mason@aoe.vt.edu',1x,
     6    'see also: http://www.aoe.vt.edu/'/)
  510 format(/5x,'Thickness Distribution Options:'/
     1       /5x,'    1 - NACA 4 Digit Series'
     2       /5x,'    2 - NACA Modified 4 Digit Series'/
     3       /5x,'  Select 1 or 2 :')
  530 format(/5x,' ILLEGAL CHOICE, RE-ENTER'/)
  540 format(/5x,' Input Max Thickness,  T/C =')
  550 format(/5x,' X/C Position of Max Thickness =')
  560 format(/5x,' Input leading edge parameter:'/ 
     1       /5x,' Choose values from 0 to 9 - '
     2       /5x,'  (6 is the 4 Series value)  ')
  570 format(/5x,'Leading Edge Radius, rle/C = ',f7.5/
     1       /5x,'Trailing Edge Angle is',f6.2,' degrees'
     2       /5x,' [this is the TOTAL included angle]')
  580 format(/5x,'Camber Distribution Options:'/
     1       /5x,'  1 - NACA 4 Digit Series'
     2       /5x,'  2 - NACA 5 Digit Series'
     3       /5x,'  3 - NACA 6 & 6A  Series'/
     4       /5x,' Select 1,2 or 3: ')
  600 format(/5x,'Input Max Camber: ')
  610 format(/5x,'Input X/C Position of Max Camber: ')
  620 format(/5x,'Input Max Camber, where'
     1       /5x,'the design lift is 3/2 the camber: ')
  630 format(/5x,'Input X/C position of Max Camber: ')
  640 format(/5x,' Design Lift Coefficient = ')
  650 format(/5x,' Input X/C for constant loading, A = ')
  660 format(/5x,'6A-series camber line ? (Y/N):')
  670 format(/5x,' NACA Airfoil Ordinate Values '/
     1       /5x,'  X/C= '/)
  680 format(/5x,'  X/C          =',f10.5
     1       /5x,'  YT/C         =',f10.5
     1       /5x,'  DYT/DX       =',f10.5
     2       /5x,'  D2YT/DX2     =',f10.5
     3       /5x,'  YT-CURVATURE =',f10.5
     4       /5x,'  YC/C         =',f10.5
     5       /5x,'  DYC/DX       =',f10.5
     6       /5x,'  Theta (deg)  =',f10.5/)
  690 format(/5x,'  xu/c         =',f10.5
     1       /5x,'  yu/c         =',f10.5
     2       /5x,'  xl/c         =',f10.5
     3       /5x,'  yl/c         =',f10.5/)
  700 format(///5x,'Another X/C ? (Y/N) :')
  710 format(//)
  720 format(2x,'I     X/C    YT/C    DYT/X   YC/C    DYC/C',
     1          '   XU/C(%)  YU/C(%)  XL/C(%) YL/C(%)')
  730 format(1x,i3,5f8.4,4f9.4)

 1020 format(/2x,'send output to a file? (Y/N):'/)
 1040 format(/2x,'enter file name:'/)
 1050 format(/2x,'enter file title:'/)
 1060 format(a80)
 1070 format(2x,'Upper Surface')
 1080 format(2f10.6)
 1090 format(2x,'Lower Surface')
 1100 format(/5x,'Another case?')

      end

      subroutine comp(x,yt,dt,d2t,yc,dc,xu,yu,xl,yl,ans)

c     REM  * COMPUTATION MODULE

      common /consts/ a0,a1,a2,a3,a4,d1,d2,d3
      common /contrl/ tk,xmt,xmc,cx,xmm,xk1,A,ci,
     1                g,h,tt,xmx,ntk,nck
      character*1 ans
      pi = 3.1415926536

      if(ntk .eq. 1) GO TO 2410
      if(ntk .eq. 2) GO TO 2420

c     REM  * NACA 4 DIGIT THICKNESS DISTRIBUTION

 2410 YT  = xMT*(A0*sqrt(X) - A1*X - A2*X**2 + A3*X**3 - A4*X**4)
      DT  = 9.99E+20
      D2T = DT
      IF (X .eq. 0.) go to 2435
      DT  = xMT*(A0/2./sqrt(X) - A1 - 2.*A2*X + 3.*A3*X**2 - 4.*A4*X**3)
      D2T = xMT*(-A0/4. * X**1.5 - 2.*A2 + 6.*A3*X - 12.*A4*X**2)
      GO TO 2435

c     REM  * NACA MODIFIED 4 DIGIT THICKNESS DISTRIBUTION

 2420 IF (X .gt. xMX) go to 2430
      YT  =  5. * xMT * (A0*sqrt(X) + A1*X + A2*X**2 + A3*X**3)
      DT  = 9.99E+20
      D2T = DT
      IF (X .eq. 0.) go to 2435
      DT  =  5. * xMT * (A0/2./sqrt(X) + A1 + 2.*A2*X + 3.*A3*X**2)
      D2T =  5. * xMT * (2.*A2 + 6.* A3*X - A0/4.* X**1.5)
      GO TO 2435
 2430 YT  =  5. * xMT * (.002 + D1 * (1. - X) + 
     1       D2*(1. - X)**2 + D3 * (1. - X)**3)
      DT  =- 5. * xMT * (D1 + 2.*D2 *
     1       (1. - X) + 3.* D3 * (1.- X)**2)
      D2T =  5. * xMT * (2.*D2 + 6.*D3 * (1. - X))

c     REM  * CONSTRUCT CAMBER LINE

2435  if(nck .eq. 1) go to 2440
      if(nck .eq. 2) go to 2470
      if(nck .eq. 3) go to 2500

c     REM  * NACA 4 DIGIT

 2440 YC = 0
      DC = 0
      IF (xMC .eq. 0.) go to 2600
      IF (X .gt. CX) go to 2460
      YC = xMC / CX**2 * (2. * CX * X - X**2)
      DC = 2. * xMC / CX**2 * (CX - X)
      GO TO 2600
 2460 YC = xMC / (1 - CX)**2 * (1 - 2 * CX + 2 * CX * X - X**2)
      DC = 2. * xMC / (1 - CX)**2 * (CX - X)
      GO TO 2600

c     REM  * NACA 5 DIGIT

 2470 yc = 0.0
      dc = 0.0
      if (abs(xmc) .lt. 0.00001) go to 2600
      IF (X .gt. xMM) go to 2485
      YC = xK1/6. * (X**3 - 3.*xMM*X**2 + xMM**2*(3. - xMM)*X)
      DC = xK1/6. * (3.*X**2 - 6.*xMM*X + xMM**2 * (3. - xMM))
      GO TO 2600
 2485 YC = xK1 / 6. * xMM**3 * (1. - X)
      DC =-xK1 / 6. * xMM**3
      GO TO 2600

c     REM  * 6 & 6A SERIES CAMBER LINE

 2500 YC = 0
      DC = 0
      IF (X .eq. 0.) go to 2600
      IF (X .eq. 1.) go to 2600
      IF (A .ne. 1.) go to 2530
      YC =  - CI /4./PI * ((1. - X) * ALOG(1. - X) + X *ALOG(X))
      DC =    CI /4./PI * ( ALOG(1. - X) -  ALOG(X))
      GOTO 2600

 2530 Q1 = A  - X
      Q2 = 1. - X
      T1 = 0. 
      IF (Q1 .ne. 0.) T1 = .5*Q1**2 * ALOG(ABS(Q1))
      T2 = .5 * Q2**2 * ALOG(Q2)
      T3 = Q2**2/4. - Q1**2/4.
      T4 = G - H*X - X*ALOG(X)
      T5 = CI/2./PI/(1. + A)
      YC = T5 * ((T1 - T2 + T3)/(1. - A) + T4)
      T1 = Q2 *  ALOG(Q2)
      IF (Q1 .ne. 0.) T1 = T1 - Q1 * ALOG(ABS(Q1))

      DC = T5 * (T1/(1. - A) - ALOG(X) - 1. - H)
      IF (ans .eq. 'N' .or. ans .eq. 'n') go to 2600

      IF (X .lt. .87437) go to 2600

      YC = CI * (.0302164 - .245209 * (X - .87437))
      DC =  -.245209 * CI

c     REM  * COMBINE THICKNESS AND CAMBER

 2600 TT =  ATAN (DC)
      XU = X  - YT * SIN(TT)
      YU = YC + YT * COS(TT)
      XL = X  + YT * SIN(TT)
      YL = YC - YT * COS(TT)

      RETURN
      end
 
      subroutine n4modp(xmx,rx,d1,d2,d3,a0,a1,a2,a3)
c
c    * PRE-PROCESSING FOR MOD 4DIG THICKNESS
c
      D1 = (2.24 - 5.42*xmx + 12.3*xmx**2)/10./(1. - .878 * xmx)
      D2 = (.294 - 2. * (1. - xmx) * D1) / (1. - xmx)**2
      D3 = ( - .196 + (1 - xmx) * D1) / (1. - xmx)**3

      A0 = .296904 * RX
      R1 = .2 * (1. - xmx)**2/(.588 - 2.*D1*(1. - xmx))
      A1 = .3/xmx - 15./8.*A0/sqrt(xmx)-xmx/10./R1
      A2 =  -(.3/xmx**2) + 5./4. * A0/xmx**1.5 + 1./5./R1
      A3 = .1/xmx**3 - .375 * A0 /xmx**2.5 - .1/R1 /xmx

      return
      end

      subroutine outopt(nopt)
c
c      REM  * SELECT OUTPUT TYPE SUB
c
      write(6,100)
  100 format(/5x,' Choose output option :'/
     1       /5x,'  1 - Point by point'
     2       /5x,'  2 - Distribution'/
     3       /5x,'    Select 1 or 2:') 
      read(5,*) fnopt
      nopt = fnopt
      RETURN
      end
 
      subroutine outdst(im,xc)

c     REM  * SUB TO COMPUTE X DISTRIBUTION
      
      dimension xc(121)
      pi = 3.1415926536

      write(6,10)
   10 format(/5x,'  Select type of distribution:'/
     1       /5x,'  1 - Even Spacing'
     2       /5x,'  2 - Full Cosine'
     3       /5x,'      (Concentrated at both LE & TE)'
     4       /5x,'  3 - Half Cosine' 
     5       /5x,'      (Concentrated at LE)'/ 
     6       /6x,'    Choose 1, 2, or 3 :')
      read(5,*) fnopt
      nopt = fnopt

      write(6,30)
   30 format(/5x,'  Number of points in distribution,'
     1       /5x,'                     (131 maximum) =')

      read(5,*) FIM
      im   = fim
      xim  = im

      do 100 I = 1, IM
      xi = i
      IF (nopt .eq. 1) XC(I) = (xi - 1.)/(xim - 1.)
      IF (nopt .eq. 2) XC(I) = .5 *(1. - COS((xi - 1.)/(xim - 1.)*PI))
      IF (nopt .eq. 3) XC(I) = 1. - COS ((xi - 1.)*PI/2./(xim - 1.))
  100 continue

      RETURN

      end  