!$Id:$
      subroutine phide(ct,nix,nxd,nxn,nne,nface,iln)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!     P l o t   C o n t r o l   R o u t i n e   F o r   F E A P
!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: do hidden line removal

!      Inputs:
!         ct        - Plot negative faces if positive
!         iln(2)    - Line type data
!         nface     - Number of faces on surfaces

!      Outputs:
!         nix       - Face connection location
!         nxd       - Face connection dimension
!         nxn       - Number of nodes/face
!         nne       - Number of faces
!-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'comblk.h'
      include  'cdata.h'
      include  'pdata3.h'
      include  'pdata4.h'
      include  'plflag.h'
      include  'pointer.h'
      include  'ppers.h'
      include  'sdata.h'

      integer       :: nix,nxd,nxn,nne,nface

      integer       :: iln(2)
      real (kind=8) :: ct

      save

!     Plot visible mesh

      call pzeroi(mr(np(66)),numnp)
      nix     = 54
      nxd     = 7
      nxn     = 4
      nne     = nface
      nfac    = nne
      call plface(mr(np(nix)),mr(np(62)),hr(np(53)),
     &            3,nxd,numnp,nne,iln,ct)

!     Set plot sequence for z-sort

      if(kpers.ne.0) then
        call perspz(hr(np(53)),mr(np(nix)),
     &              mr(np(62)),nxd,nxn,  3,numnp,nne)
      endif

      end subroutine phide
