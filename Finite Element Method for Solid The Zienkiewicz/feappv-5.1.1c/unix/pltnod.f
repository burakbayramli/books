!$Id:$
      subroutine pltnod(x,ip,ndm,n1,n2,n3)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--+---------+---------+---------+---------+---------+---------+-]
!      Purpose: Plot location of nodal points in mesh

!      Inputs:
!         x(ndm,*)  - Nodal coordinates of mesh
!         ip(*)     - Plot only nodes with positive values
!         ndm       - Dimension of x array
!         n1        - Place number near node if .ne. 0
!         n2        - First node to plot
!         n3        - Last node to plot

!      Outputs:
!         none      - Plot outputs to screen/file
!-----[--+---------+---------+---------+---------+---------+---------+-]
      implicit  none

      include  'pdata1.h'
      include  'pdata4.h'
      include  'plflag.h'
      include  'ppers.h'

      logical       :: zoom
      integer       :: n, ndm, n1, n2, n3, ip(*)
      real (kind=8) :: x1, x2, x3, dx1, shft1, shft2, shft3, x(ndm,*)

      save

!     Open plot and plot locations of all nodes: Add labels if n1 .ne. 0

      dx1 = .002d0/scalef
      if(kpers.ne.0) then
        shft1 = -12.d0*dx1
        shft2 =   2.d0*dx1
        shft3 = -12.d0*dx1
      else
        shft1 = -12.d0*dx1
        shft2 =   2.d0*dx1
        shft3 =   0.d0*dx1
      endif
      x3 = 0.0d0
      do n = n2,n3
        if(ip(n).gt.0 .and. zoom(x(1,n),ndm)) then
          x1 = x(1,n)
          x2 = x(2,n)
          if(ndm.ge.3) x3 = x(3,n)
          call plotl(x1-dx1 , x2+dx1 , x3, 3)
          call plotl(x1-dx1 , x2-dx1 , x3, 2)
          call plotl(x1+dx1 , x2-dx1 , x3, 2)
          call plotl(x1+dx1 , x2+dx1 , x3, 2)
          call plotl(x1-dx1 , x2+dx1 , x3, 2)
          if(n1.ne.0) then
            call plotl(x1+shft1, x2+shft2, x3+shft3, 3)
            if(clip) call plabl(n)
          endif
        endif
      end do

      end subroutine pltnod
