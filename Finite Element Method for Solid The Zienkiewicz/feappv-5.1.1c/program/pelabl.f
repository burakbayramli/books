!$Id:$
      function pelabl(nnty)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!     Modification log                                Date (dd/mm/year)
!       Original version                                    07/07/2017
!       1. Add output of 'U01' to 'U50' for user elements   11/08/2017
!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Return element type label

!      Inputs:
!        nnty      - ix(nen+7,n) value (defines element shape type

!      Outputs:
!        pelabl    - Character type
!-----[--.----+----.----+----.-----------------------------------------]
      implicit    none

      character (len=5) :: pelabl, txlab

      integer        :: nnty, l

      if(nnty.eq.-1) then
        txlab = ' Line'
      elseif(nnty.eq.-2) then
        txlab = 'Trian'
      elseif(nnty.eq.-3) then
        txlab = 'Quadr'
      elseif(nnty.eq.-4) then
        txlab = 'Tetra'
      elseif(nnty.eq.-5) then
        txlab = ' Hexa'
      elseif(nnty.eq.-6) then
        txlab = 'Wedge'
      elseif(nnty.eq.-7) then
        txlab = ' Pyra'
      elseif(nnty.eq.-8) then
        txlab = 'Point'
      elseif(nnty.eq.-22) then
        txlab = ' VEM2'
      elseif(nnty.eq.-23) then
        txlab = ' VEM3'
      elseif(nnty.lt.-100) then ! User elements
        l = -nnty - 100
        txlab = '  U00'
        if(l.lt.10) then
          write(txlab(5:5),'(i1)') l
        else
          write(txlab(4:5),'(i2)') l
        endif
      elseif(nnty.gt.9) then
        txlab = '  IgA'
      else
        txlab = ' Unk?'
      endif

!     Return label

      pelabl = txlab

      end function pelabl
