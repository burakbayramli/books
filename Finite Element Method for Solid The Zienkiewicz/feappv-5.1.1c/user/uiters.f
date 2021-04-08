!$Id:$
      subroutine uiters(nnu,isw)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--+---------+---------+---------+---------+---------+---------+-]
!     Purpose:  User utility for forming sparse arrays

!     Inputs:
!       isw      Switch parameter (must be a negative number)

!     Outputs:
!       nnu      Length of allocated array: Integer parameter
!                Location arrays: Returned in pointers
!                   np(151) for # terms/column
!                   np(152) for location of terms
!-----[--+---------+---------+---------+---------+---------+---------+-]
      implicit   none

      include   'allotd.h'
      include   'cdata.h'
      include   'comblk.h'
      include   'compac.h'
      include   'pointer.h'
      include   'sdata.h'

      logical        :: setvar,palloc
      integer        :: isw,kp,nnu

!     Compute sparse storage for matrix

      if(isw.eq.-1) then

        setvar = palloc(153,'USER3', numnp*ndf, 1)
        call elcnt(numnp,numel,nen,nen1,mr(np(33)),mr(np(153)),.true. )
        call sumcnt(mr(np(153)),numnp*ndf,kp)

        setvar = palloc(154,'USER4', kp, 1)
        call pelcon(numel,nen,nen1,mr(np(33)),mr(np(153)),mr(np(154)),
     &              kp,1)

!       Assign storage type

        ubycol = .true.       ! Store by columns
        udiag  = .false.      ! Diagonal in separate part of array
        uall   = .false.
        setvar =  palloc(151,'USER1', ndf*numnp, 1)
        if(np(152).ne.0) then
          setvar = palloc(152,'USER2',0, 1)
        endif
        setvar = palloc(152,'USER2',neq, 1)
        call comproa(numnp,nen,nen1,ndf,mr(np(33)),mr(np(31)),
     &              mr(np(153)),mr(np(154)),mr(np(152)),kp,
     &              kbycol,kdiag,kall)
        setvar = palloc(152,'USER2', kp, 1)
        call comprob(numnp,nen,nen1,ndf,mr(np(33)),mr(np(31)),
     &              mr(np(153)),mr(np(154)),mr(np(152)),mr(np(151)),
     &              kbycol,kdiag,kall)
        nnu    = kp

!       Delete temporary arrays

        setvar = palloc(154,'USER4', 0, 1)
        setvar = palloc(153,'USER3', 0, 1)

      else
        write(*,*) '  *ERROR* OPTION NOT IMPLEMENTED FOR ISW =',isw
      endif

      end subroutine uiters
