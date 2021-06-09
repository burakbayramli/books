!$Id:$
      subroutine reshis(ix,nen1,numel,n1, n2)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Initialize t_n+1 history variables from final value
!               of variables at t_n

!      Inputs:
!         ix(nen1,*)  - Element connection/history pointer array
!         nen1        - Dimension of ix array
!         numel       - Number of elements in mesh
!         n1          - Pointer in ix to t_n   data
!         n2          - Pointer in ix to t_n+1 data

!      Outputs:
!         none        - Output is retained in blank common
!-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'hdata.h'
      include  'pointer.h'
      include  'comblk.h'

      integer       :: nen1,numel,n1,n2, n,nh, ix(nen1,numel)

      save

!     Move history variables from location 'n1' to 'n2'

      do n = 1,numel
        nh1 = np(49) + ix(n1,n) - 1
        nh2 = np(49) + ix(n2,n) - 1
        if(nh2.ne.nh1) then
          do nh = 1,abs(ix(n2,n) - ix(n1,n))
            hr(nh+nh2) = hr(nh+nh1)
          end do ! nh
        endif
      end do ! n

      end subroutine reshis
