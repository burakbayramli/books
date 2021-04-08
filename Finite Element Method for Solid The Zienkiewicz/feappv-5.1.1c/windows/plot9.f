!$Id:$
      subroutine plot9(iel,ix,x,ndm,nel,isp)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--+---------+---------+---------+---------+---------+---------+-]
!      Purpose: Plot 2-d 3 to 9 node elements

!      Inputs:
!         iel       - Element type number
!         ix(*)     - Node list for element
!         x(ndm,*)  - Nodal coordinates for element
!         nel       - Number of nodes on element
!         isp       - Indicator on line type for outline of panels

!      Outputs:
!         none      - Plot outputs to screen/file
!-----[--+---------+---------+---------+---------+---------+---------+-]
      implicit  none

      include  'pdata2.h'

      integer       :: iel, ndm, nel, isp, i, j, is, iu, iplt(50)
      integer       :: ix(*)
      real (kind=8) :: x(ndm,*),v

      save

      call pltord(ix,iel, iu,iplt)

!     Set for fill panel or line

      if(nel.gt.2) then
        is = abs(isp)
      else
        is      = 3
        iplt(1) = 1
        iplt(2) = 2
        iu      = 2
      endif

      v = 0.0d0
      if(ndm.eq.3) v = x(3,iplt(1))
      call plotl(x(1,iplt(1)),x(2,iplt(1)),v,is)
      do i = 2,iu
        j = iplt(i)
        if((j.le.nel).and.(j.ne.0).and.(ix(j).ne.0)) then
          if(ndm.eq.3) v = x(3,j)
          call plotl(x(1,j),x(2,j),v,2)
        endif
      end do

!     Fill in corners for a clip and close panel

      if(is.eq.1) then

        call clpan

!       Outline part in black or white

        if(isp.gt.0) then
          is = icolr

          if(icolr.ne.0 .and.icolr.ne.7 ) then
            call pppcol(0,1)
          else
            call pppcol(1,1)
          endif

          v = 0.0d0
          if(ndm.eq.3) v = x(3,iplt(1))
          call plotl(x(1,iplt(1)),x(2,iplt(1)),v,3)
          do i = 2,iu
            j = iplt(i)
            if((j.le.nel).and.(j.ne.0).and.(ix(j).ne.0)) then
              if(ndm.eq.3) v = x(3,j)
              call plotl(x(1,j),x(2,j),v,2)
            endif
          end do
          icolr = is
        endif

      endif

      end subroutine plot9
