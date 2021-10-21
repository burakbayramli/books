      subroutine setprob
      implicit double precision (a-h,o-z)
c
c     # Euler equations on a quadrilateral grid
c
      common /param/ gamma,gamma1
      common/cdisc/ x0,y0,alf,beta,r0,idisc
      common /comic/ rhol,rhor,rhoul,rhour,el,er

      gamma = 1.4d0
      gamma1 = gamma - 1.d0

c     # initial data:

      idisc = 1
      x0 = 0.d0
      y0 = 0.d0
      alf = 1.d0
      beta = 0.d0

      pr = 1.d0
      rhor = 1.d0
      rhour = 0.d0

      pl = 5.d0
c
c     # find shock wave data using Hugoniot relation:
      gamma2 = (gamma + 1.d0) / gamma1
      rhol = rhor * (pr/pl + gamma2) / (1.d0 + gamma2*pr/pl)
      cr = dsqrt(gamma*pr/rhor)
      rhoul = rhol*(rhour/rhor - (2.d0*cr/gamma1)
     &              * dsqrt(gamma1/(2.d0*gamma))
     &              * (1.d0-pl/pr) / dsqrt(1.d0 + gamma2*pl/pr))
      el = pl/gamma1 + 0.5d0*rhoul**2/rhol
      er = pr/gamma1 + 0.5d0*rhour**2/rhor

      ul = rhoul / rhol
      cl = dsqrt(gamma*pl/rhol)
      write(6,*) 'rhol, ul, pl, cl:', rhol,ul,pl,cl

      return
      end
