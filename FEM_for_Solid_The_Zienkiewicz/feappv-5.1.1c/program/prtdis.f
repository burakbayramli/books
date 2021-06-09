!$Id:$
      subroutine prtdis(x,b,ttim,prop,ndm,ndf,n1,n2,n3,ii,prth)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Output nodal values for real solutions

!      Inputs:
!         x(ndm,*)  - Nodal coordinates of mesh
!         b(*)      - Current value of solution
!         ttim      - Value of solution time
!         prop      - Value of total proportional load
!         ndm       - Spatial dimension of mesh
!         ndf       - Number dof/node
!         n1        - First node to output
!         n2        - Last noed to output
!         n3        - Increment to n1
!         ii        - Type of output: 1 = displacement; 2 = velocity;
!                                     3 = acceleration
!         prth      - Output title/header data if true

!      Outputs:
!         None      - Outputs to file
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'fdata.h'
      include  'iofile.h'
      include  'xtout.h'
      include  'pointer.h'
      include  'comblk.h'

      character (len=30) :: fmt1,fmt2
      character (len=6)  :: cd,di(4)
      character (len=4)  :: nd

      logical       :: prth,tot
      integer       :: ndm,ndf,n1,n2,n3,ii, i,n, n_count, nxt1
      real (kind=8) :: ttim,prop

      real (kind=8) :: x(ndm,*),b(ndf,*)

      save

      data      nd   /'Node'/,cd /' Coord'/
      data      di   /' Displ',' Veloc',' Accel',' EigV.'/
      data      fmt1 /'(3x,a4,3(i6,a6)/(7x,6(i6,a6)))'/
      data      fmt2 /'(i7,1p,3e12.4/(7x,1p,6e12.4))'/

      tot   = (ndf+ndm).le.6
      if(.not.tot) then
        write(fmt1(8:8),'(i1)') ndm
        write(fmt2(8:8),'(i1)') ndm
      endif

      n_count = 0
      nxt1  = max(1,nxt)
      do n = n1,n2,n3
        if( (mr(np(190)-1+n).ge.0) .and. ( nxt.eq.0  .or.
     &     ( abs(x(nxt1,n)-xt).le.xtol ) ) ) then
          n_count = n_count - 1
          if(n_count.le.0) then
            call prtitl(prth)
            if(ii.le.3) then
              write(iow,2000) ttim,prop
              if(ior.lt.0.and.pfr) then
                write(*,2000) ttim,prop
              endif
            else
              write(iow,2001) ttim,prop
              if(ior.lt.0.and.pfr) then
                write(*,2001) ttim,prop
              endif
            endif
            if(tot) then
              write(iow,2002) (i,cd,i=1,ndm),(i,di(ii),i=1,ndf)
              if(ior.lt.0.and.pfr) then
                write(*,2002) (i,cd,i=1,ndm),(i,di(ii),i=1,ndf)
              endif
            else
              write(iow,fmt1) nd,(i,cd,i=1,ndm),(i,di(ii),i=1,ndf)
              if(ior.lt.0.and.pfr) then
                write(*,fmt1) nd,(i,cd,i=1,ndm),(i,di(ii),i=1,ndf)
              endif
            endif
            n_count = 48
          endif
          if(tot) then
            write(iow,2003) n,(x(i,n),i=1,ndm),(b(i,n),i=1,ndf)
            if(ior.lt.0.and.pfr) then
              write(*,2003) n,(x(i,n),i=1,ndm),(b(i,n),i=1,ndf)
            endif
          else
            write(iow,fmt2) n,(x(i,n),i=1,ndm),(b(i,n),i=1,ndf)
            if(ior.lt.0.and.pfr) then
              write(*,fmt2) n,(x(i,n),i=1,ndm),(b(i,n),i=1,ndf)
            endif
          endif
        endif
      end do

!     Format

2000  format('  N o d a l   D i s p l a c e m e n t s',5x,
     &  'Time',e18.5/44x,'Prop. Ld.',1pe13.5/1x)

2001  format('  N o d a l   D i s p l a c e m e n t s',5x,
     &  'Time',e18.5/43x,'Eigenvalue',1pe13.5/1x)

2002  format('   Node',6(i6,a6):/(7x,6(i6,a6)))

2003  format(i7,1p,6e12.4/(7x,1p,6e12.4))

      end subroutine prtdis
