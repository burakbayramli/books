      subroutine setprob
      implicit double precision (a-h,o-z)
      common /param/  g    !# gravitational parameter 
      common/cdisc/ x0,y0,alf,beta,r0,idisc
      common /comic/ hin,hout
c
c
      open(unit=7,file='setprob.data',status='old',form='formatted')
c
c     # gravitational constant:
      read(7,*) g

c     # data for radial dam-break problem:
      idisc = 2
      read(7,*) x0,y0,r0
      read(7,*) hin,hout
c

      return
      end
