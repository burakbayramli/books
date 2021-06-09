!$Id:$
      subroutine pltgmv(jsw)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose:  2-d  plots for NURBS Graphics

!      Inputs:
!         lct       - Command character parameters
!         ctl(3)    - Command numerical parameters
!         prt       - Flag, output if true

!      Outputs:
!         N.B.  Users are responsible for command actions.  See
!               programmers manual for example.
!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      include   'cdata.h'
      include   'cnurb.h'
      include   'idptr.h'
      include   'iofile.h'
      include   'pdata3.h'
      include   'pfeapb.h'
      include   'pltfac.h'
      include   'sdata.h'

      include   'pointer.h'
      include   'comblk.h'

      logical    setvar, palloc
      integer    jsw, i

      save

!     Restore analysis values

      if(jsw.eq.1) then
        numnp = onumnp
        numel = onumel
        nen1  = onen1
        nen   = onen
        nneq  = onneq
        vneq  = ovneq

        nxd   = nen1
        nxn   = nen
        nne   = numel

        npix  = np(33)
        npuu  = np(40)
        npud  = np(42)
        npxx  = np(43)
        npev  = np(77)
        nprn  = np(89)
        npty  = np(190)
        nper  = np(57)
        npnp  = np(58)
        id31  = np(31)

!       setvar = palloc(279,'N_LIN',0,1)
!       setvar = palloc(280,'T_LIN',0,1)
!       setvar = palloc(284,'ID_LN',0,1)

      elseif(jsw.eq.2) then

        if(np(279).eq.0) then
          setvar = palloc(279,'N_LIN',nd_lin*2  ,1)
          setvar = palloc(280,'T_LIN',nd_lin    ,1)
          setvar = palloc(284,'ID_LN',nd_lin*ndf,1)
        endif

        do i = 0,nd_lin-1
          mr(np(279)+i       ) = i+1
          mr(np(279)+i+nd_lin) = i+1
          mr(np(280)+i       ) = 0
        end do ! i

        do i = 0,nd_lin*ndf-1
          mr(np(284)+i) = i+1
        end do ! i

        npix = np(276)
        npxx = np(277)
        npuu = np(278)
        npud = np(296)
        nprn = np(279)
        npty = np(280)
        npnp = np(281)
        nper = np(282)
        npev = np(283)
        id31 = np(284)

        numnp = nd_lin
        numel = ne_lin
        if(ndm.eq.1) then
          nen  = 2
        elseif(ndm.eq.2) then
          nen  =  4
        else
          nen  =  8
        endif
        nen1 = 19
        nneq = ndf*numnp
        vneq = nneq

        nxd  = nen1
        nxn  = nen
        nne  = numel

      endif

      end
