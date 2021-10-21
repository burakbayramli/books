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

      pl = 3.d0
      rhol = 3.d0
      ul = 0.0d0
      rhoul = rhol*ul
      el = pl/gamma1 + 0.5d0*rhol*ul**2/rhol

      pr = 1.d0
      rhor = 1.d0
      ur = 0.0d0
      rhour = rhor*ur
      er = pr/gamma1 + 0.5d0*rhor*ur**2

      return
      end
