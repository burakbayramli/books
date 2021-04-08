!$Id:$
      subroutine pltbou(id,x,angl,ip, ndm,ndf,numnp,nbou)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Display boundary conditions on screen for 1-3 dof

!      Inputs:
!         id(ndf,*) - Boundary condition indicator array
!         x(ndm,*)  - Nodal coordinates of mesh
!         angl(*)   - Angle for sloping boundaries
!         ip(*)     - Active node indicators
!         ndm       - Dimension of x array
!         ndf       - Number dof/node
!         numnp     - Number of nodes in mesh
!         nbou      - Component to display ( 0 = all)

!      Outputs:
!         none      - Plot outputs to screen/file
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'pdata1.h'
      include  'pdata4.h'
      include  'pointer.h'
      include  'comblk.h'

      logical       :: zoom,bc0
      integer       :: ndm,ndf,numnp,nbou, n
      real (kind=8) :: dx1, x1,x2,x3, cs,sn,ang
      integer       :: id(ndf,*),ip(*)
      real (kind=8) :: x(ndm,*),angl(*)

      save

!     Plot boundary restraints (lines = fixed)

      bc0 = nbou.eq.0
      dx1 = .006d0/scalef
      do n = 1,numnp
        if(ip(n).gt.0) then
          if(angl(n).ne.0.0d0) then
            ang = angl(n)*0.017453292d0
            cs = cos(ang)*dx1
            sn = sin(ang)*dx1
          else
            cs = dx1
            sn = 0.0d0
          endif
          if(zoom(x(1,n),ndm) .and. mr(np(190)+n-1).ge.0) then
            x1 = x(1,n)
            x2 = x(2,n)
            x3 = x(3,n)
            if (( bc0 .or. nbou.eq.1) .and. id(1,n) .le. 0) then
              call plotl(x1+cs, x2+sn, x3, 3)
              call plotl(x1-cs, x2-sn, x3, 2)
            endif
            if (ndf.ge.2 .and. ndm.ge.2) then
              if(id(2,n) .le. 0 .and. (bc0 .or. nbou.eq.2) ) then
                call plotl(x1-sn, x2+cs, x3, 3)
                call plotl(x1+sn, x2-cs, x3, 2)
              endif
            endif
            if (ndf.ge.3 .and. ndm.ge.2)then
              if(id(3,n) .le. 0 .and. (bc0 .or. nbou.eq.3) ) then
                call plotl(x1,x2, x3+dx1, 3)
                call plotl(x1,x2, x3-dx1, 2)
              endif
            endif
            if (ndf.ge.4 .and. ndm.ge.2) then
              if(id(4,n) .le. 0 .and. nbou.eq.4 ) then
                call plotl(x1+cs, x2+sn, x3, 3)
                call plotl(x1-cs, x2-sn, x3, 2)
              endif
            endif
            if (ndf.ge.5 .and. ndm.ge.2) then
              if(id(5,n) .le. 0 .and. nbou.eq.5 ) then
                call plotl(x1-sn, x2+cs, x3, 3)
                call plotl(x1+sn, x2-cs, x3, 2)
              endif
            endif
            if (ndf.ge.6 .and. ndm.ge.2)then
              if(id(6,n) .le. 0 .and. nbou.eq.6 ) then
                call plotl(x1,x2, x3+dx1, 3)
                call plotl(x1,x2, x3-dx1, 2)
              endif
            endif
          endif
        endif

      end do

      end subroutine pltbou
