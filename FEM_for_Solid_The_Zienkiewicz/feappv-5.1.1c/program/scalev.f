!$Id:$
      subroutine scalev(v,pdf,ndm,ndf,numnp)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Scale vector to have maximum element of +1.0

!      Inputs:
!         v(ndf,*) - Vector of values
!         pdf(*)   - DOF to scale on
!         ndm      - Space dimension of mesh
!         ndf      - DOF's/node (maximum)
!         numnp    - Number of nodes

!      Outputs:
!         v(ndf,*) - Unit vector of values
!-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      integer       :: i,n,ndm,ndf,numnp
      integer       :: pdf(*)
      real (kind=8) :: v(ndf,*),vmax

      save

!     Locate maximum

      vmax = 0.0d0
      do i = 1,ndm
        if(pdf(i).ge.1.and.pdf(i).le.ndf) then
          do n = 1,numnp
            vmax = max(vmax,abs(v(pdf(i),n)))
          end do ! n
        endif
      end do ! i

!     Perform scaling

      if(vmax.gt.0.0d0) then
        vmax = 1.d0/vmax
        do n = 1,numnp
          do i = 1,ndf
            v(i,n) = v(i,n)*vmax
          end do ! i
        end do ! n
      else
        write(*,*) ' ** WARNING ** Zero length vector in SCALEV'
      endif

      end subroutine scalev
