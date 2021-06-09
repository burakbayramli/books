!$Id:$
      subroutine sblkn(nr,ns,xl,ixl,shp,x,dr,ds,ni,n,ndm,nodinc,ntyp,
     &                 nm,ctype,prt,prth)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Generate nodes and elements for 2-d problems

!         nm < 4 : Generates a line of elements
!           ns = 1     2-node line
!           ns = 2     3-node line
!         nm > 3 : Generates a block of elements
!           ntyp = 1   4-node quadrilaterals
!           ntyp = 2   3-node triangles - diags ll to ur
!           ntyp = 3   3-node triangles - diags ul to lr
!           ntyp = 4   3-node triangles - diags
!           ntyp = 5   3-node triangles - diags
!           ntyp = 6   3-node triangles - diags union jack
!           ntyp = 7   6-node triangles - diags ll to ur
!           ntyp = 8   8-node quadrilaterals
!           ntyp = 9   9-node quadrilaterals

!           ntyp =-1   3-node triangles - crossed pattern
!           ntyp =-7   7-node triangles - diags ll to ur (bubble node)

!      Inputs:
!         nr        - Number nodes in 1-local coordinate dir.
!         ns        - Number nodes in 2-local coordinate dir.
!         xl(ndm,*) - Block nodal coordinate array
!         ixl(*)    - Block nodal connection list
!         shp(3,*)  - Shape functions for block
!         dr        - 1-local coordinate increment
!         ds        - 2-local coordinate increment
!         ni        - Initial node number for block
!         ndm       - Spatial dimension of mesh
!         nodinc    - Increment to node numbers at end of each line
!         ntyp      - Block type
!         nm        - Number of nodes on block
!         ctype     - Type of block coordinates
!         prt       - Output generated data if true
!         prth      - Output title/header data if true

!      Outputs:
!         n         - Final node number for block
!         x(ndm,*)  - Nodal coordinates for block
!-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'cdata.h'
      include  'cdat2.h'
      include  'iofile.h'
      include  'trdata.h'

      character (len=15) :: ctype
      character (len=6)  :: xh
      logical       :: prt,prth, pcomp
      integer       :: i,j,k,n, mct,me, nm, inc8
      integer       :: nr,ns,nsn,ni,ndm,nodinc,ntyp
      integer       :: ixl(*)
      real (kind=8) :: rr,sn,cn, drh,dsh,dr,ds, xsj
      real (kind=8) :: xl(3,*),x(ndm,*),shp(3,*),shp1(2,3)
      real (kind=8) :: xx(3),ss(2),tt(2)

      save

      data      xh /' coord'/

      do k = 1,3
        xx(k) = 0.0d0
      end do

      drh =  dr*0.5d0
      dsh =  ds*0.5d0
      n   =  ni
      mct =  0
      if(nm.lt.4) then
        nsn = 1
      else
        nsn = ns
      endif
      ss(2)   = -1.0
      do j = 1,nsn
        ss(1) = -1.0
        if(ntyp.eq.8 .and. mod(j,2).eq.0) then
          inc8 = 2
        else
          inc8 = 1
        endif
        do i = 1,nr,inc8
          if (nm.gt.3) then
            call shp2d(ss,xl,shp,xsj,3,nm,ixl,.true.)
            call cfunc(shp,xl,ixl,ndm,xx)
          else
            call shap1d(ss(1),nm,shp1)
            do k = 1,ndm
              xx(k) = 0.0d0
              do me = 1,nm
                xx(k) = xx(k) + shp1(2,me)*xl(k,me)
              end do
            end do
          endif
          if(ndm.ge.2 .and. pcomp(ctype,'pola',4)) then
            call pdegree(xx(2), sn,cn)
            rr    = xx(1)
            xx(1) = x0(1) + rr*cn
            xx(2) = x0(2) + rr*sn
          endif
          do k = 1,ndm
            x(k,n) = xr(k)+tr(k,1)*xx(1)+tr(k,2)*xx(2)+tr(k,3)*xx(3)
          end do
          if(prt) then
            mct = mct + 1
            if(mod(mct,50).eq.1) then
              call prtitl(prth)
              write(iow,2003) (k,xh,k=1,ndm)
              if(ior.lt.0) then
                write(*,2003) (k,xh,k=1,ndm)
              endif
            endif
            write(iow,2004) n,(x(k,n),k=1,ndm)
            if(ior.lt.0) then
              write(*,2004) n,(x(k,n),k=1,ndm)
            endif
          endif
          n = n + 1
          if(nm.gt.3.and.ntyp.eq.-1.and.i.lt.nr.and.j.lt.ns) then
            tt(1) = ss(1) + drh
            tt(2) = ss(2) + dsh
            call shp2d(tt,xl,shp,xsj,3,nm,ixl,.true.)
            call cfunc(shp,xl,ixl,ndm,x(1,n))
            if(ndm.ge.2 .and. pcomp(ctype,'pola',4)) then
              call pdegree(x(2,n), sn,cn)
              rr     = x(1,n)
              x(1,n) = x0(1) + rr*cn
              x(2,n) = x0(2) + rr*sn
            endif
            if(prt) then
              mct = mct + 1
              if(mod(mct,50).eq.1) then
                call prtitl(prth)
                write(iow,2003) (k,xh,k=1,ndm)
              endif
              write(iow,2004) n,(x(k,n),k=1,ndm)
              if(ior.lt.0) then
                if(mod(mct,50).eq.1) then
                  write(*,2003) (k,xh,k=1,ndm)
                endif
                write(*,2004) n,(x(k,n),k=1,ndm)
              endif
            endif
            n = n + 1
          endif
          ss(1) = ss(1) + dr*inc8
        end do
        n = n + nodinc
        ss(2) = ss(2) + ds
      end do

!     Formats

2003  format(/'  N o d a l   C o o r d i n a t e s'//6x,'Node',5(i7,a6))

2004  format(i10,1p,5e13.4)

      end subroutine sblkn
