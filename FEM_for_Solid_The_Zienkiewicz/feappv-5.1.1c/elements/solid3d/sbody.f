!$Id:$
      subroutine sbodyf(d, body)

!-----[--+---------+---------+---------+---------+---------+---------+-]
!      Purpose: Compute body force values

!     Inputs:
!       d(*)    - Material parameters

!     Outputs:
!       body(*) - Body force intensities
!-----[--+---------+---------+---------+---------+---------+---------+-]
      implicit   none

      include   'eldata.h'
      include   'prld1.h'

      integer       :: ii
      real (kind=8) :: d(*), body(3)

!     Set body load levels

      do ii = 1,3
        if(int(d(73+ii)).gt.0) then
          body(ii) = d(10+ii) + prldv(int(d(73+ii)))*d(70+ii)
        else
          body(ii) = d(10+ii)*dm
        endif
      end do ! ii

      end subroutine sbodyf
