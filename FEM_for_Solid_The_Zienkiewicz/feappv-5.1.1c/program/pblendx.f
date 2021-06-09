!$Id:$
      subroutine pblendx(nn,nr,ns,ni,ntyp,ndm, fxim,fetp,fxip,fetm,
     &                   nty,x,nflag,prt,prth)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]

!     Purpose:  Construct two dimensional interpolation using blending

!     Inputs:
!        nr        - Number of divisions in  xi-direction
!        ns        - Number of divisions in eta-direction
!        ni        - Number of last node generated
!        ntyp      - Element types
!        ndm       - Spatial dimension of mesh
!        fxim(2,*) - Interpolations on  xi = -1 side
!        fxip(2,*) - Interpolations on  xi =  1 side
!        fetm(2,*) - Interpolations on eta = -1 side
!        fetp(2,*) - Interpolations on eta =  1 side
!        prt       - Output list of coordinates if true
!        prth      - Output header information  if true

!     Outputs:
!        nty(*)    - Node type
!        x(ndm,*)  - Nodal coordinates for blended patch

!-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'iofile.h'
      include  'pointer.h'
      include  'comblk.h'

      logical       :: nflag,prt,prth
      integer       :: i,j,k, nr,ns,ni,ntyp, nn, ndm, ninc

      real (kind=8) :: n1i,n2i, n1j,n2j, rnr,rns

      integer       :: nty(*)
      real (kind=8) :: fxim(ndm,0:nr), fxip(ndm,0:nr)
      real (kind=8) :: fetm(ndm,0:ns), fetp(ndm,0:ns)
      real (kind=8) :: x(ndm,*)

      save

!     Blend interpolations to form nodal coordinates

      rnr = 1.d0/dble(nr)
      rns = 1.d0/dble(ns)

      nn  = ni - 1

      if(prt) then
        call prtitl(prth)
        write(iow,2000) (i,i=1,ndm)
      endif
      do j = 0,ns

        n2j = dble(j)*rns
        n1j = 1.d0 - n2j
        if(ntyp.eq.8 .and. mod(j,2).eq.1) then
          ninc = 2
        else
          ninc = 1
        endif

        do i = 0,nr,ninc

          nn = nn + 1

          if(nflag .or. nty(nn).ge.0) then
            n2i = dble(i)*rnr
            n1i = 1.d0 - n2i

            nty(nn) = 0
            do k = 1,ndm
              x(k,nn) = n1i*fetm(k,j)       + n2i*fetp(k,j)
     &                + n1j*fxim(k,i)       + n2j*fxip(k,i)
     &                - n1i*(n1j*fxim(k, 0) + n2j*fxip(k, 0))
     &                - n2i*(n1j*fxim(k,nr) + n2j*fxip(k,nr))
            end do ! k

            if(prt) then
              write(iow,2001) nn,(x(k,nn),k=1,ndm)
            endif

          endif

        end do

      end do

!     Formats

2000  format('   B l e n d e d   C o o r d i n a t e s'//
     &       '     Node',4(i5,'-Coordinate':))

2001  format(i8,1p,4e15.5)

      end subroutine pblendx
