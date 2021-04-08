!$Id:$
      subroutine pltnurb(gmvoff,new_per)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Set graphics data for different plot types from NURBS &
!               T-Splines.

!      Inputs:

!      Outputs:
!-----[--+---------+---------+---------+---------+---------+---------+-]
      implicit   none

      include   'cdata.h'
      include   'cdat1.h'
      include   'cnurb.h'
      include   'iofile.h'
      include   'pdata3.h'
      include   'pdata4.h'
      include   'pdatas.h'
      include   'pltfac.h'
      include   'ppers.h'
      include   'print.h'
      include   'sdata.h'

      include   'pointer.h'
      include   'comblk.h'

      logical    setvar, palloc, gmvoff, new_per

      save

!     Set for plot mode

      call pltgmv(2)
      gmvoff = .false.

      if(new_per) then
        setvar = palloc(128,'APLOT',numel,1)
        call setclp(hr(npxx),ndm,numnp)
        call plfacn(mr(npix),mr(np(128)),nen,
     &              numel,nface,mr(np(32)),nie)
        setvar = palloc( 54,'FCIX ',7*max(nface,1),1)
        call plfacx(mr(npix),mr(np(128)),mr(np(54)),
     &              nen,numel,mr(np(32)),nie)

        setvar = palloc( 61,'OUTL ',max(nface,numnp)+1,1)
        setvar = palloc( 62,'SYMM ',max(nface,numel)  ,1)
        call psetip(mr(np(62)),  max(nface,numel))
        setvar = palloc( 66,'VISN ',numnp, 1)
        new_per = .false.

!       Reset reflection table

        nfac       = nne

      endif

      end ! subroutine pltnurb
