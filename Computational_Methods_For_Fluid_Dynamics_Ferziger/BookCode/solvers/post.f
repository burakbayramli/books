c*********************
      subroutine post
c*********************
      include 'comdp.inc'
      include 'param.inc'
      include 'com0.inc'
      include 'com1.inc'
c
c--------------------------------------------------------
c  Print forces exerted on region 0
c--------------------------------------------------------
c
      OPEN(1,FILE='ptau.dat',STATUS='UNKNOWN')
      rewind 1
      write(1,*) '#     X            Y        Tau         Pres'
      write(1,*) '#  '
C
      ftx=0.
      fty=0.
      fpx=0.
      fpy=0.
c
c   Select region 0
c
      ir=0
c
c   loop over boundary faces of region ir
c
      do k=nsr(ir),ner(ir)
c
c   calculate x and y comp. of pressure (fpx,fpy) and shear (ftx,fty) forces
c
        ftx=ftx+btr(1,k)
        fty=fty+btr(2,k)
        fpx=fpx+pb(k)*sb(1,k)
        fpy=fpy+pb(k)*sb(2,k)
c
c   calculate wall shear stress and its direction
c
        tau=sqrt((btr(1,k)/sb(1,k))**2+(btr(2,k)/sb(2,k))**2)
        si=sign(1.,(btr(1,k))
        tau=si*tau
c
c   write coordinates of boundary element, pressure and shear stress
c
        write(1,'(1p4e14.4)') xb(1,k),xb(2,k),tau,pb(k)
      end do
c
      print *, '  Ftx = ',ftx
      print *, '  Fty = ',fty
      print *, '  Fpx = ',fpx
      print *, '  Fpy = ',fpy
c
      return
      end
