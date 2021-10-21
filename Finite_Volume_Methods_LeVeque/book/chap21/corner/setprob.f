      subroutine setprob
      implicit double precision (a-h,o-z)
      common /comaux/ zl,cl,zr,cr
c
c     # Set the material parameters for the acoustic equations
c
      open(unit=7,file='setprob.data',status='old',form='formatted')

c
c     # Piecewise constant medium with single interface at x=0
c     # Impedance and sound speed to left and right:

      read(7,*) zl
      read(7,*) cl
      read(7,*) zr
      read(7,*) cr

      return
      end
