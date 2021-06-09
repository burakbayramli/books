!$Id:$
      subroutine pload0(f,f0,u,nn,p)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Form nodal load vector for current time and partition

!      Inputs:
!         f(*,2)    - Current load/displacement vector
!         u(*)      - Current solution state
!         nn        - Number of components in vectors
!         p         - Current total proportional load level

!      Outputs:
!         f0(*,1)   - Fixed solution level for subsequent solutions
!         f0(*,2)   - Set to current solution state
!         f(*,*)    - Set to zero
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'pointer.h'
      include  'prld1.h'
      include  'sdata.h'
      include  'comblk.h'

      integer       :: n,nn, ipro
      real (kind=8) :: p, f(nn,*),f0(nn,*),u(nn)

!     Set nodal forces and displacements for NEWF command

      do n = 1,nn

!       Set force values

        ipro = mr(np(29)+n-1)
        if(ipro.eq.0) then
          f0(n,1) = f(n,1)*p + f0(n,1)
        else
          f0(n,1) = f(n,1)*prldv(ipro) + f0(n,1)
        endif

!       Set displacement values

        f0(n,2) = u(n)

!       Zero variable loading component

        f(n,1)  = 0.0d0
        f(n,2)  = 0.0d0
      end do

      end subroutine pload0
