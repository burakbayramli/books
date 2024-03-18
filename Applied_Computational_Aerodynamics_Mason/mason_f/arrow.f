       program arrowx
c---------------------------------------

c     Virginia Tech Educational Aerodynamics Software

c     Brett Malone   10/07/91

c     Arrow wing aerodynamics, at supersonic speeds
c     analytic solution for Clalpha and Ct/CL^2

c     from R.T. Jones and Doris Cohen, High Speed Wing Theory
c     Princeton University Press, 1960, pp.  198-202.

c     Xbar is the distance from the apex
c     to the aerodynamic center, in root chords,
c     from Puckett and Stewart, JAS, Oct. 1947, pp. 567-578 

c     Mod for Design Class by w h mason, Nov. 14, 1994
c     Mod for Config Aero class, April 25, 2000

c     fold to UPPER case in compile
c---------------------------------------

      implicit double precision (a-h,l-z)
      double precision kcd0,kcd100
      character ans

      pi     = acos(-1.0d0)
      
      write(6,5)
    5 format(/4x,'Arrow wing drag due to lift ',
     1           'at supersonic speeds'
     2       /4x,'Virginia Tech Educational Aerodynamics ',
     3           'Software')

   10 write(6,100)
      read(5,*) lesweep
  100 format(/4x,'enter leading edge sweep (deg.): ',$)

      write(6,110)
  110 format(/4x,'enter Mach number: ',$)
      read(5,*) mach
      beta = sqrt(mach**2 - 1.0)

      write(6,120)
  120 format(/4x,'enter notch ratio: ',$)
      read(5,*)  notchratio

      lam    = lesweep*(pi/180.0)
      cotlam = 1./(tan(lam))
      m      = beta*cotlam
      nch    = notchratio
      ar     = 4./(tan(lam)*(1. - nch))
      MnLE   = mach*cos(lam)
      teswp  = atan(nch*tan(lam))
      teswpd = 180./pi*teswp
      MnTE  = mach*cos(teswp)

      write(6,900) m, beta, ar,nch, MnLE, teswpd, MnTE

      If (MnTE .lt. 1.0) then

              write(6,800)
              go to 60

              endif

      if (m.lt.1.0) then

c-- subsonic leading edge case

c-- Clalpha Components --
      em     = ellip(m)
      cla1   = 4.0*m/(beta*em)
      cla2   = nch/(1.0+nch)
      cla3   = (1-nch)/((1-nch**2)**1.5)
      cla4   = dacos(-1.0*nch)

      cla    = cla1*(cla2+cla3*cla4)

c-- Ct/Cl^2 Components --
      ctcl21 = pi*ar/4.0
      ctcl22 = dsqrt(1.0d0-m**2)/(em**2)
      ctcl23 = 1.0/cla**2
      ctcl2  = ctcl21*ctcl22*ctcl23

c-- aerodynamic center
      xbar1  = acos(nch)/sqrt(1.0 - nch**2)
      xbar2  = nch*(4. - nch**2) + (2.0 + nch**2)*xbar1
      xbar3  = 3.0*(1.0 - nch**2)*(nch + xbar1)

      xbar   = xbar2/xbar3

c-- supersonic leading edge case

      else

      cla1   = 8.0*m/(beta*pi*(1+nch))
      cla2   = dacos(-1.0*nch/m)/(dsqrt(m**2-nch**2))
      cla3   = dacos(1.0/m)*nch/(dsqrt(m**2-1.0))
      cla    = cla1*(cla2+cla3)

      ctcl2 = 0.0

c-- aerodynamic center

      if (nch .eq. 0.0) then

             xbar = 2./3.

             else

             xbar1   = acos(-nch/m)
             xbar2   = acos(1.0/m)
             xbar3   = 2.0 - (nch/m)**2 - nch**4/m**2
             xbar4   = (1.0 - nch**2)*(1.0 - (nch/m)**2)**1.5
             xbar5   = nch*(3.0 - nch**2)/(1.0 - nch**2)/
     1                 sqrt(1.0 - 1.0/m**2)
             xbar6   = nch*m/(m**2 - nch**2)

             xbarnm  = xbar3*xbar1/xbar4 + xbar5*xbar2 + xbar6

             xbar7   = xbar1/sqrt(1.0 - (nch/m)**2)
             xbar8   = nch*xbar2/sqrt(1.0 - 1.0/m**2)

             xbardn  = xbar7 + xbar8

             xbar    = xbarnm/xbardn/3.0

            endif

      endif

c -- 0% Leading edge suction polar
      kCd0  = 1.0/cla

c -- 100% Leading edge suction polar

      kCd100  = (1.0/cla)-ctcl2

      BCLA       = beta*cla
      cdbcl20    = kCd0/beta
      cdbcl2100  = kCd100/beta
      CD0p       = 100.0*kCd0
      CD100p     = 100.0*kCd100
      deltacd    = CD0p - CD100p
      write(6,1000) BCLA, cdbcl20, cdbcl2100, 
     1              cla, ctcl2, kCd0, kCd100,
     2              CD0p, CD100p, deltacd, xbar

   20 write(6,150)
  150 format(/4x,'enter angle of attack, deg.: ',$)
      read(5,*) alphad
      alpha  = alphad*(pi/180.0)

      cl    = cla * alpha
      cd0   = kcd0*cl**2
      cd100 = kcd100*cl**2

      write(*,1010) alphad, cl, cd0, cd100

      write(6,200)
  200 format(/4x,'Another alpha? (Y/N): ',$)
      read(5,*) ans
      if(ans .eq. 'Y' .or. ans .eq. 'y') go to 20

   60 write(6,210)
  210 format(/4x,'Another case? (Y/N): ',$)
      read(5,*) ans
      if(ans .eq. 'Y' .or. ans .eq. 'y') go to 10

  800 format(/4x,'Subsonic trailing edge, theory invalid')

  900 format(/4x,'m           = ',f8.5,2x,'[Beta*cot(LEsweep)]'
     1       /4x,'beta        = ',f8.5
     2       /4x,'AR          = ',f8.5
     3       /4x,'notch ratio = ',f8.5
     4       /4x,'MnLE        = ',f8.5
     5       /4x,'TE sweep    = ',f8.5,2x,'(deg)'
     6       /4x,'MnTE        = ',f8.5)

 1000 format(//4x,'betaCLalpha             = ',f10.5
     1        /4x,'CD/(beta CL**2) (0%)    = ',f10.5
     2        /4x,'CD/(beta CL**2) (100%)  = ',f10.5
     3        /4x,'CLalpha (per rad.)      = ',f10.5
     4        /4x,'CT/CL^2                 = ',f10.5
     5        /4x,'kCD(0%) (1/CLalpha)     = ',f10.5
     6        /4x,'kCD0(100%)              = ',f10.5
     7       //4x,' at CL = 0.10: CD(0%)   = ',f6.1 ' counts'
     8        /4x,'               CD(100%) = ',f6.1 ' counts'
     9        /4x,'               Delta CD = ',f6.1 ' counts'
     A        /4x,'Xbar                    = ',f10.5
     B        /4x,' (the number of root chords the aerodynamic center'
     C        /4x,'  is behind the wing apex)')

 1010 format(/4x,'alphad      = ',f9.5,4x,'CL            = ',f9.5,
     1       /4x,'CDL(0% LES) = ',f9.5,4x,'CDL(100% LES) = ',f9.5
     2      //4x,'Note: These are the drag due to lift values.'
     3       /4x,'The actual drag due to lift falls between the 0% and ' 
     4       /4x,'100% values, and the wave and skin friction drag'
     5       /4x,'must be added to get total drag')

      stop
      end

c------------------------
      function ellip(m)

c      this is  from Abromowitz and Stegun, page 592, section 17.3.36
c      the arguments are  different in notation, not uncommon when
c      working with special functions

c-------------------------
      implicit double precision (a-h,l-z)

      a1    = 0.44325141463
      a2    = 0.06260601220
      a3    = 0.04757383546
      a4    = 0.01736506451
      b1    = 0.24998368310
      b2    = 0.09200180037
      b3    = 0.04069697526
      b4    = 0.00526449639

      elp1  = 1.0+a1*m**2+a2*m**4+a3*m**6+a4*m**8
      elp2  = (b1*m**2+b2*m**4+b3*m**6+b4*m**8)*log(1./m**2)
      ellip =  elp1 + elp2

      return
      end