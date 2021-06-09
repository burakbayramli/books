!$Id:$
      subroutine ploadc()

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Control routine for data inputs based on nodal
!               coordinates

!      Inputs:
!         none      - Data retrieved through common blocks

!      Outputs:
!         none      - Data stored in blank common locations
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'cdata.h'
      include  'corset.h'
      include  'corfil.h'
      include  'print.h'
      include  'sdata.h'

      include  'pointer.h'
      include  'comblk.h'

      logical       :: setvar,palloc
      integer       :: ityp(6), ndft(6), n,nn

      save

!     Allocate temporary arrays

      setvar = palloc(111,'TEMP1',numnp+3*numel,1)
      setvar = palloc(112,'TEMP2',numnp        ,2)

!     Set the counter

      nn = 0

!     Set coordinate angle conditions

      if(angfl) then
        nn       = nn + 1
        ndft(nn) = 1
        ityp(nn) = 3
      endif

!     Set coordinate boundary codes

      if(boufl) then
        nn       = nn + 1
        ndft(nn) = ndf
        ityp(nn) = 2
      endif

!     Set cordinate force values

      if(forfl) then
        nn       = nn + 1
        ndft(nn) = ndf
        ityp(nn) = 5
      endif

!     Set cordinate displacement values

      if(disfl) then
        nn       = nn + 1
        ndft(nn) = ndf
        ityp(nn) = 4
      endif

!     Set cordinate force proportional load values

      if(cprfl) then
        nn       = nn + 1
        ndft(nn) = ndf
        ityp(nn) = 6
      endif

!     Set coordinate surface loads

      if(surfl) then
        nn       = nn + 1
        ndft(nn) = ndf
        ityp(nn) = 1
      endif

!     Call routine to do generations

      do n = 1,nn
        call pesurf(mr(np(31)),mr(np(33)),mr(np(111)),mr(np(111)+numnp),
     &              hr(np(112)),hr(np(43)),hr(np(27)),hr(np(45)),
     &              ndft(n),ndm,nen,nen1,numnp,numel,prt,prth,ityp(n))
      end do ! n

!     Destroy temporary arrays

      setvar = palloc(112,'TEMP2',0,2)
      setvar = palloc(111,'TEMP1',0,1)

      end subroutine ploadc
