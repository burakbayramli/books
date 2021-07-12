c$Id:$
      subroutine uconst(type,vv, d, ud,n1,n3, umat)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: User material model interface

c      Inputs:
c         type    - Name of constitutive model (character variable)
c         vv(*)   - Parameters: user parameters from command line
c         d(*)    - Program material parameter data

c      Outputs:
c         n1      - Number history terms: nh1,nh2
c         n3      - Number history terms: nh3
c         ud(*)   - User material parameters
c-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'iofile.h'
      include  'umac1.h'

      logical   pcomp
      character type*15, matnm(10)*4
      integer   n1,n3,umat
      real*8    vv(5), d(*), ud(*)

c     Default names for materials: DO NOT CHANGE

      data      matnm /'mat1', 'mat2', 'mat3', 'mat4', 'mat5',
     &                 'mat5', 'mat7', 'mat8', 'mat9', 'mat0'/
      save

c     Initial calls to reset the name

      if(pcomp(uct,'mate',4)) then

        if(    pcomp(type,'mat1',4)) then
          call umati1(type,vv, d, ud,n1,n3)
          matnm(1) = type(1:4)
        elseif(pcomp(type,'mat2',4)) then
          call umati2(type,vv, d, ud,n1,n3)
          matnm(2) = type(1:4)
        elseif(pcomp(type,'mat3',4)) then
          call umati3(type,vv, d, ud,n1,n3)
          matnm(3) = type(1:4)
        elseif(pcomp(type,'mat4',4)) then
          call umati4(type,vv, d, ud,n1,n3)
          matnm(4) = type(1:4)
        elseif(pcomp(type,'mat5',4)) then
          call umati5(type,vv, d, ud,n1,n3)
          matnm(5) = type(1:4)
        endif

c     Calls to do the inputs

      else
        if(    pcomp(type,matnm(1),4)) then
          call umati1(type,vv, d, ud,n1,n3)
          umat     = 1
        elseif(pcomp(type,matnm(2),4)) then
          call umati2(type,vv, d, ud,n1,n3)
          umat     = 2
        elseif(pcomp(type,matnm(3),4)) then
          call umati3(type,vv, d, ud,n1,n3)
          umat     = 3
        elseif(pcomp(type,matnm(4),4)) then
          call umati4(type,vv, d, ud,n1,n3)
          umat     = 4
        elseif(pcomp(type,matnm(5),4)) then
          call umati5(type,vv, d, ud,n1,n3)
          umat     = 5
        else
          write(iow,3000) type
          if(ior.lt.0) then
            write(*,3000) type
          endif
          call plstop()
        endif

      endif

c     Format

3000  format(' **ERROR** User material error: Type: ',a,' not defined')
      end
