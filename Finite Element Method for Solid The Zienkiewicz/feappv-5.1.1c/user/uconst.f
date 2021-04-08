!$Id:$
      subroutine uconst(vtype,vv, d, ud,n1,n3, umat)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: User material model interface

!      Inputs:
!         vtype   - Name of constitutive model (character variable)
!         vv(*)   - Parameters: user parameters from command line
!         d(*)    - Program material parameter data

!      Outputs:
!         n1      - Number history terms: nh1,nh2
!         n3      - Number history terms: nh3
!         ud(*)   - User material parameters
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'iofile.h'
      include  'umac1.h'

      character (len=15) :: vtype
      character (len=4)  :: matnm(10)

      logical       :: pcomp
      integer       :: n1,n3,umat
      real (kind=8) :: vv(5), d(*), ud(*)

!     Default names for materials: DO NOT CHANGE

      data      matnm /'mat1', 'mat2', 'mat3', 'mat4', 'mat5',
     &                 'mat5', 'mat7', 'mat8', 'mat9', 'mat0'/
      save

!     Initial calls to reset the name

      if(pcomp(uct,'mate',4)) then

        if(    pcomp(vtype,'mat1',4)) then
          call umati1(vtype,vv, d, ud,n1,n3)
          matnm(1) = vtype(1:4)
        elseif(pcomp(vtype,'mat2',4)) then
          call umati2(vtype,vv, d, ud,n1,n3)
          matnm(2) = vtype(1:4)
        elseif(pcomp(vtype,'mat3',4)) then
          call umati3(vtype,vv, d, ud,n1,n3)
          matnm(3) = vtype(1:4)
        elseif(pcomp(vtype,'mat4',4)) then
          call umati4(vtype,vv, d, ud,n1,n3)
          matnm(4) = vtype(1:4)
        elseif(pcomp(vtype,'mat5',4)) then
          call umati5(vtype,vv, d, ud,n1,n3)
          matnm(5) = vtype(1:4)
        elseif(pcomp(vtype,'mat0',4)) then
          call umati0(vtype,vv, d, ud,n1,n3)
          matnm(10) = vtype(1:4)
        endif

!     Calls to do the inputs

      else
        if(    pcomp(vtype,matnm(1),4)) then
          call umati1(vtype,vv, d, ud,n1,n3)
          umat     = 1
        elseif(pcomp(vtype,matnm(2),4)) then
          call umati2(vtype,vv, d, ud,n1,n3)
          umat     = 2
        elseif(pcomp(vtype,matnm(3),4)) then
          call umati3(vtype,vv, d, ud,n1,n3)
          umat     = 3
        elseif(pcomp(vtype,matnm(4),4)) then
          call umati4(vtype,vv, d, ud,n1,n3)
          umat     = 4
        elseif(pcomp(vtype,matnm(5),4)) then
          call umati5(vtype,vv, d, ud,n1,n3)
          umat     = 5
        elseif(pcomp(vtype,matnm(10),4)) then
          call umati0(vtype,vv, d, ud,n1,n3)
          umat     = 10
        else
          write(iow,3000) vtype
          if(ior.lt.0) then
            write(*,3000) vtype
          endif
          call plstop(.true.)
        endif

      endif

!     Format

3000  format(' **ERROR** User material error: Type: ',a,' not defined')

      end subroutine uconst
