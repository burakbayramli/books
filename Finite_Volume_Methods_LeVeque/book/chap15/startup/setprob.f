      subroutine setprob
      implicit double precision (a-h,o-z)
      common /param/ gamma, gamma1
      common /comic/ rhol,rhoul,el,rhor,rhour,er
c
c     # Set gamma and gamma1 = gamma-1 for Euler equations
c     # Passed to the Riemann solver rp1.f in a common block

c     # set values for use in qinit: shock tube initial data
c
      open(unit=7,file='setprob.data',status='old',form='formatted')


      read(7,*) gamma
      gamma1 = gamma - 1.d0


       read(7,*) pl
       read(7,*) rhor,ur,pr
c
c      # data in right state:

       rhour = rhor*ur
       er = pr/gamma1 + 0.5d0*rhor*ur**2

c      # data in left state:
c      # set to 3-shock using Hugoniot relation:
c
      gamma2 = (gamma + 1.d0) / gamma1
      rhol = rhor * (pr/pl + gamma2) / (1.d0 + gamma2*pr/pl)
      cr = dsqrt(gamma*pr/rhor)
      rhoul = rhol*(rhour/rhor - (2.d0*cr/gamma1)
     &              * dsqrt(gamma1/(2.d0*gamma))
     &              * (1.d0-pl/pr) / dsqrt(1.d0 + gamma2*pl/pr))
      el = pl/gamma1 + 0.5d0*rhoul**2/rhol
c
      return
      end
