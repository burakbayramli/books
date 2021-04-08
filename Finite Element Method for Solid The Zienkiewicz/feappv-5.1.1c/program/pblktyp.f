!$Id:$
      logical function pblktyp(layer,td, ntyp,ns,mab)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!     Modification log                                Date (dd/mm/year)
!       Original version                                    01/11/2006
!       1. For 'quad' = 0,4 set block type to 0             06/01/2007
!       2. Check that number of nodes one element < 'nen'   09/02/2007
!       3. Add 14- & 15-node tets with ntyp = 17 & 18.      30/08/2007
!       4. Add 'user' option and 'ublk1.h' data             02/09/2008
!       5. Add multiple user block options                  19/11/2008
!       6. Add 64-node brick with ntyp = 19.                06/02/2009
!       7. Add 4-node cubic line with ns = 4                26/03/2009
!       8. Add 'mate' to set of material number             30/11/2010
!       9. Add 10-node triangular element with ntyp = -10   07/01/2016
!      10. Add netyp to set ix(nen+7,.) value               17/12/2016
!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Describe element type and number of nodes/element.

!      Inputs:
!        layer       - Type of element
!        td(*)       - Number nodes on element

!      Outputs:
!        ntyp        - Element type
!        ns          - Generation type
!        mab         - Material number
!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      include   'cdata.h'
      include   'iofile.h'
      include   'setups.h'
      include   'ublk1.h'

      character (len=15) :: layer

      logical       :: pcomp
      integer       :: ntyp,ns,mab, n,j
      real (kind=8) :: td(5)

!     Line elements

      if    (pcomp(layer,'line',4)) then
        pblktyp = .false.
        n       = max(2,nint(td(1)))
        if(n.gt.nen) go to 900
        if(n.eq.2) then
          ns = 1
        elseif(n.eq.3) then
          ns = 2
        elseif(n.eq.4) then
          ns = 3
        endif
        netyp   = -1

!     Triangular elements

      elseif(pcomp(layer,'tria',4)) then
        pblktyp = .false.
        n       = max(3,nint(td(1)))
        if(n.gt.nen) go to 900
        j       = nint(td(2))
        if(n.eq.3) then              !  3-node linear triangle
          if(j.ge.0) then
            ntyp = min(6,max(1,j))
          elseif(j.eq.-1) then
            ntyp = -1
          endif
        elseif(n.eq.6) then          !  6-node quadratic triangle
          ntyp = 7
        elseif(n.eq.7) then          !  7-node quadratic triangle
          ntyp = -7
        elseif(n.eq.10) then         ! 10-node cubic     triangle
          ntyp = -10
        endif
        netyp   = -2

!     Quadrilateral elements

      elseif(pcomp(layer,'quad',4)) then
        pblktyp = .false.
        n       = max(4,nint(td(1)))
        if(n.gt.nen) go to 900
        if(n.eq.4) then       !  4-node Linear quadrilateral
          ntyp = 0
        elseif(n.eq. 8) then  !  8-node Serendipity quadratic quad
          ntyp = 8
        elseif(n.eq. 9) then  !  9-node Lagrangian  quadratic quad
          ntyp = 9
        elseif(n.eq.16) then  ! 16-node Lagrangian  cubic     quad
          ntyp = 16
        endif
        netyp   = -3

!     Tetrahedral elements

      elseif(pcomp(layer,'tetr',4)) then
        pblktyp = .false.
        n       = max(4,nint(td(1)))
        if(n.gt.nen) go to 900
        if(n.eq.4) then              !  4-node linear tetrahedron
          ntyp = 11
        elseif(n.eq.10) then         ! 10-node quadratic tetrahedron
          ntyp = 13
        elseif(n.eq.11) then         ! 11-node quadratic tetrahedron
          ntyp = 15
        elseif(n.eq.14) then         ! 14-node quadratic tetrahedron
          ntyp = 17
        elseif(n.eq.15) then         ! 15-node quadratic tetrahedron
          ntyp = 18
        endif
        netyp   = -4

!     Brick elements

      elseif(pcomp(layer,'bric',4) .or. pcomp(layer,'hexa',4)) then
        pblktyp = .false.
        n       = max(8,nint(td(1)))
        if(n.gt.nen) go to 900
        if(n.eq.8) then       !  8-node linear brick
          ntyp = 10
        elseif(n.eq.20) then  ! 20-node Serendipity quadratic brick
          ntyp = 14
        elseif(n.eq.27) then  ! 27-node Lagrangian  quadratic brick
          ntyp = 12
        elseif(n.eq.64) then  ! 64-node Lagrangian  cubic     brick
          ntyp = 19
        endif
        netyp   = -5

!     User elements

      elseif(pcomp(layer,'user',4)) then

        pblktyp = .false.
        n       = max(1,nint(td(1)))
        if(n.gt.nen) go to 900
        ntyp = 30+n
        ublknum = max(1,nint(td(2)))
        do j = 1,3
          ublkdat(j,ublknum) = nint(td(j+2))
        end do ! j

!     Material number

      elseif(pcomp(layer,'mate',4)) then

        pblktyp = .false.
        mab     = nint(td(1))

!     No match

      else
        pblktyp = .true.
        n       =  0
      endif

      return

!     Error on element type

900   write(iow,3000) layer,n,nen
      write(*,3000) layer,n,nen
      call plstop(.true.)

!     Formats

3000  format('--> ERROR in BLOCk generation: Element type ',a/
     &       '    with ',i3,' nodes greater than ',i3,' specified',
     &       ' on control record')

      end function pblktyp
