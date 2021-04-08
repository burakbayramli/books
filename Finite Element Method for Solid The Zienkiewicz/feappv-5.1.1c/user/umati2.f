!$Id:$
      subroutine umati2(vtype,vv, d, ud, n1,n3)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Dummy user material model routine

!      Inputs:
!         vtype  - Name of material model
!         vv(5)  - Command line real data
!         d(*)   - Program material parameter data

!      Outputs:
!         ud(*)  - Material parameter data for model
!         n1     - Number of history items/point (time   dependent)
!         n3     - Number of history items/point (time independent)
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      character (len=15) :: vtype
      logical       :: pcomp
      integer       :: n1,n3
      real (kind=8) :: vv(5),d(*),ud(*)

!     Set command name

      if(pcomp(vtype,'mat2',4)) then     ! Default  form DO NOT CHANGE
!       vtype = 'name'                   ! Specify 'name'

!     Input user data and save in ud(*) array

      else                              ! Perform input for user data

      endif

      end subroutine umati2
