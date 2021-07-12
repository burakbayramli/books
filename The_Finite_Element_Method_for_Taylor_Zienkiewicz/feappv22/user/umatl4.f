c$Id:$
      subroutine umatl4(eps,theta,td,d,ud,hn,h1,nh,ii,istrt, sig,dd,isw)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c     Purpose: User Constitutive Model 1

c     Input:
c          eps(*)  -  Current strains at point      (small deformation)
c                  -  Deformation gradient at point (finite deformation)
c          theta   -  Trace of strain at point
c                  -  Determinant of deforamtion gradient
c          td      -  Temperature change
c          d(*)    -  Program material parameters (ndd)
c          ud(*)   -  User material parameters (nud)
c          hn(nh)  -  History terms at point: t_n
c          h1(nh)  -  History terms at point: t_n+1
c          nh      -  Number of history terms
c          ii      -  Current point number
c          istrt   -  Start state: 0 = elastic; 1 = last solution
c          isw     -  Solution option from element

c     Output:
c          sig(*)  -  Stresses at point.
c                     N.B. 1-d models use only sig(1)
c          dd(6,*) -  Current material tangent moduli
c                     N.B. 1-d models use only dd(1,1) and dd(2,1)
c-----[--.----+----.----+----.-----------------------------------------]
      implicit none

      integer  nh,istrt,isw, ii
      real*8   td
      real*8   eps(*),theta(*),d(*),ud(*),hn(nh),h1(nh), sig(*),dd(6,*)

c     Compute and output stress (sig) and (moduli)

      end
