c$Id:$
      subroutine phide(ct,nix,nxd,nxn,nne,nface,iln)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c     P l o t   C o n t r o l   R o u t i n e   F o r   F E A P
c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: do hidden line removal

c      Inputs:
c         ct        - Plot negative faces if positive
c         iln(2)    - Line type data
c         nface     - Number of faces on surfaces

c      Outputs:
c         nix       - Face connection location
c         nxd       - Face connection dimension
c         nxn       - Number of nodes/face
c         nne       - Number of faces
c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'comblk.h'
      include  'cdata.h'
      include  'pdata3.h'
      include  'pdata4.h'
      include  'plflag.h'
      include  'pointer.h'
      include  'ppers.h'
      include  'sdata.h'

      integer   nix,nxd,nxn,nne,nface

      integer   iln(2)
      real*8    ct

      save

c     Plot visible mesh

      call pzeroi(mr(np(59)),numnp)
      nix     = 55
      nxd     = 7
      nxn     = 4
      nne     = nface
      nfac    = nne
      call plface(mr(np(nix)),mr(np(62)),hr(np(54)),
     &            3,nxd,numnp,nne,iln,ct)
c     nne     = nfac

c     Set plot sequence for z-sort

      if(kpers.ne.0) then
        call perspz(hr(np(54)),mr(np(nix)), hr(np(61)),hr(np(56)),
     &              mr(np(62)),nxd,nxn,  3,numnp,nfac)
c    &              mr(np(62)),nxd,nxn,  3,numnp,nne)
      endif

      end
