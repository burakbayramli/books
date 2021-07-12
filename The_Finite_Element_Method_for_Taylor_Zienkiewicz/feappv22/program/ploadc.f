c$Id:$
      subroutine ploadc()

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Control routine for data inputs based on nodal
c               coordinates

c      Inputs:
c         none      - Data retrieved through common blocks

c      Outputs:
c         none      - Data stored in blank common locations
c-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'cdata.h'
      include  'corset.h'
      include  'corfil.h'
      include  'print.h'
      include  'sdata.h'

      include  'pointer.h'
      include  'comblk.h'

      logical   setvar,palloc
      integer   ityp(6), ndft(6), n,nn

      save

c     Allocate temporary arrays

      setvar = palloc(81,'TEMP1',numnp+3*numel,1)
      setvar = palloc(82,'TEMP2',numnp        ,2)

c     Set the counter

      nn = 0

c     Set coordinate angle conditions

      if(angfl) then
        nn       = nn + 1
        ndft(nn) = 1
        ityp(nn) = 3
      endif

c     Set coordinate boundary codes

      if(boufl) then
        nn       = nn + 1
        ndft(nn) = ndf
        ityp(nn) = 2
      endif

c     Set cordinate force values

      if(forfl) then
        nn       = nn + 1
        ndft(nn) = ndf
        ityp(nn) = 5
      endif

c     Set cordinate displacement values

      if(disfl) then
        nn       = nn + 1
        ndft(nn) = ndf
        ityp(nn) = 4
      endif

c     Set cordinate force proportional load values

      if(cprfl) then
        nn       = nn + 1
        ndft(nn) = ndf
        ityp(nn) = 6
      endif

c     Set coordinate surface loads

      if(surfl) then
        nn       = nn + 1
        ndft(nn) = ndf
        ityp(nn) = 1
      endif

c     Call routine to do generations

      do n = 1,nn
        call pesurf(mr(np(31)),mr(np(33)),mr(np(81)),mr(np(81)+numnp),
     &              hr(np(82)),hr(np(43)),hr(np(27)),hr(np(45)),
     &              ndft(n),ndm,nen,nen1,numnp,numel,prt,prth,ityp(n))
      end do ! n

c     Destroy temporary arrays

      setvar = palloc(82,'TEMP2',0,2)
      setvar = palloc(81,'TEMP1',0,1)

      end
