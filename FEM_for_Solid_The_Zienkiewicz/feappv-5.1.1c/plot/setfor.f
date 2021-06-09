!$Id:$
      subroutine setfor(f,f0,prop,nn, dr)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Set current value of nodal forces for plots

!      Inputs:
!         f(*)    - Value of force controlled by prop
!         f0(*)   - Base value of force
!         prop    - Proportional load factor
!         nn      - Number of dof

!      Outputs:
!         dr(*)   - Values of nodal forces
!-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'pointer.h'
      include  'prld1.h'
      include  'comblk.h'

      integer       :: n,nn
      integer       :: nty
      real (kind=8) :: prop
      real (kind=8) :: f(*),f0(*),dr(*)

      save

!     Check for proportional loading: f = current load,
!         f0(2*nn) = base load, f0 = user supplied loads

      do n = 1,nn
        nty = mr(np(29)+n-1)
        if(nty.le.0) then
          dr(n) = dr(n) + f(n)*prop       + f0(n) + f0(2*nn+n)
        else
          dr(n) = dr(n) + f(n)*prldv(nty) + f0(n) + f0(2*nn+n)
        endif
      end do

      end subroutine setfor
