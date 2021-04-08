!$Id:$
      subroutine peltyp(tx,nnty)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!     Modification log                                Date (dd/mm/year)
!       Original version                                    01/11/2006
!       1. Add set of user elements as nnty -101 to -150    11/08/2017
!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Set element type number from imputs

!      Inputs:
!         tx         - Character string

!      Outputs:
!         nnty       - FEAP element type number
!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      character (len=15) :: tx,ty

      logical       :: pcomp
      integer       :: nnty, l

      if(pcomp(tx,'line',4)) then
        nnty = -1
      elseif(pcomp(tx,'tria',4)) then
        nnty = -2
      elseif(pcomp(tx,'quad',4)) then
        nnty = -3
      elseif(pcomp(tx,'tetr',4)) then
        nnty = -4
      elseif(pcomp(tx,'hexa',4)) then
        nnty = -5
      elseif(pcomp(tx,'wedg',4)) then
        nnty = -6
      elseif(pcomp(tx,'pyra',4)) then
        nnty = -7
      elseif(pcomp(tx,'poin',4)) then
        nnty = -8
      elseif(pcomp(tx,'vem2',4)) then
        nnty = -22
      elseif(pcomp(tx,'vem3',4)) then
        nnty = -23
      elseif(pcomp(tx,'vem',3)) then
        nnty = -22
      elseif(pcomp(tx(1:1),'u',1)) then ! User elements
        ty = trim(adjustl(tx))
        l  = len(trim(ty))
        if(l.ge.3) then
          nnty = 10*(ichar(tx(l-1:l-1)) - ichar('0'))
     &         +    (ichar(tx(l  :l  )) - ichar('0'))
          nnty = -(nnty + 100)
        else
          write(*,*) 'Unknown element type =:',ty(1:l),':'
          nnty =  0
        endif
      else
        ty = trim(adjustl(tx))
        l  = len(trim(ty))
        write(*,*) 'Unknown element type =:',ty(1:l),':'
        nnty =  0
      endif

      end subroutine peltyp
