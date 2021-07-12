c$Id:$
      subroutine formfe(pnu,pnb,pna,pnl,aufl,bfl,dfl,
     &                  isw,nl1,nl2,nl3)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Forms finite element arrays as required

c      Inputs:
c         pnu    - Pointer to current nodal solution vectors
c         pnb    - Pointer for FEM Vector
c         pna    - Pointer for FEM diagonal and upper part of array
c         pnl    - Pointer for FEM lower part of array
c         aufl   - If true assemble 'a' array (which includes 'au')
c         bfl    - If true assemble 'b' array
c         dfl    - If true assembel 'b' uncompressed
c         isw    - Solution switch controlling action to be taken
c         nl1    - First element to be processed
c         nl2    - Last  element to be processed
c         nl3    - Increment to 'nl1'

c      Outputs:
c         hr(pnb)- Values for FEM Vector
c         hr(pna)- Values for FEM diagonal and upper array part
c         hr(pnl)- Values for FEM lower array part
c-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'cdata.h'
      include  'cdat1.h'
      include  'hdatam.h'
      include  'ndata.h'
      include  'pointer.h'
      include  'prflag.h'
      include  'sdata.h'
      include  'comblk.h'

      logical   aufl,bfl,dfl
      integer   pnu,pnb,pna,pnl
      integer   isw, nl1, nl2, nl3

      save

c     Form appropriate finite element arrays

      call pform(hr(np(41)),hr(np(44)),hr(np(39)),mr(np(34)),hr(np(35)),
     &           hr(np(36)),mr(np(32)),hr(np(25)),mr(np(31)),hr(np(43)),
     &           mr(np(33)),hr(np(30)),hr(np(38)),
     &           mr(np(21)),hr(pnu),hr(np(42)),hr(pnb),hr(pna),
     &           hr(pnl),ndd,nie,ndf,ndm,nen1,nst,aufl,bfl,dfl,
     &           isw,nl1,nl2,nl3)

c     Reset update flag for history variables

      hflgu  = .false.
      h3flgu = .false.

      end
