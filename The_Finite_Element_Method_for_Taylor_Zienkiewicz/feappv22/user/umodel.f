c$Id:$
      subroutine umodel(umat,eps,theta,td,d,ud,hn,h1,nh,ii,istrt,
     &                  sig,dd, isw)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Regents of the University of California
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c     Purpose: User Constitutive Model

c     Input:
c          umat    -  User material type
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
c          sig(6)  -  Stresses at point.
c          dd(6,6) -  Current material tangent moduli

c-----[--.----+----.----+----.-----------------------------------------]
      implicit none

      include 'iofile.h'

      integer  umat,nh,istrt,isw, ii
      real*8   td
      real*8   eps(*),theta(*),d(*),ud(*),hn(*),h1(*), sig(*),dd(*)

      save

c     Material Model 1

      if(    umat.eq.1) then
        call umatl1(eps,theta,td,d,ud,hn,h1,nh,ii,istrt, sig,dd, isw)

c     Material Model 2

      elseif(umat.eq.2) then
        call umatl2(eps,theta,td,d,ud,hn,h1,nh,ii,istrt, sig,dd, isw)

c     Material Model 3

      elseif(umat.eq.3) then
        call umatl3(eps,theta,td,d,ud,hn,h1,nh,ii,istrt, sig,dd, isw)

c     Material Model 4

      elseif(umat.eq.4) then
        call umatl4(eps,theta,td,d,ud,hn,h1,nh,ii,istrt, sig,dd, isw)

c     Material Model 5

      elseif(umat.eq.5) then
        call umatl5(eps,theta,td,d,ud,hn,h1,nh,ii,istrt, sig,dd, isw)

c     Error no umat set

      else

        write(iow,4000)
        call plstop()

      endif

c     Format

4000  format(/' *ERROR* User model name incorrectly set.')

      end
