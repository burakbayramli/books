!$Id:$
      subroutine pknotlen(dknot,sknot,nknot)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Compute length of knot vector

!      Data:
!        open, num, lknot, (knots(j),j=1,lknot)
!        peri, num, lknot, oknot, (knots(j),j=1,lknot)

!      Inputs:

!      Outputs:
!        dknot  - Dimension for maximum number of knot entries
!        nknot  - Maximum knot number
!-----[--+---------+---------+---------+---------+---------+---------+-]
      implicit   none

      include   'iodata.h'
      include   'iofile.h'

      logical          :: pcomp,errck,tinput,pinput, startfl
      character        :: tx*15
      integer (kind=4) :: dknot, sknot, nknot, lknot, i,j, nk
      real    (kind=8) :: td(16), last

!     Set initial parameters to read knot vectors

      startfl = .true.
      tx = 'start'
      do while(.not.pcomp(tx,'    ',4))
        errck = tinput(tx,1,td,15)
        nk    = 0

!       Open knot

        if(pcomp(tx,'knot',4) .or.
     &     pcomp(tx,'open',4) .or.
     &     pcomp(tx,'side',4)) then

!         Set dimensioning values

          nknot = max(nknot,nint(td(1)))
          lknot = nint(td(2))
          dknot = max(dknot,lknot)
          if(startfl) then
            write(ios,'(a)') 'KNOTs'
            startfl = .false.
          endif

          nk   = 0
          last = td(3)
          do j = 2,min(13,lknot)
            if(td(j+2).gt.last) then
              nk = nk + 1
            endif
            last = td(j+2)
          end do ! j
          write(ios,2001) nint(td(1)),lknot,(td(j+2),j=1,min(13,lknot))

          do i= 14,lknot,16
            errck = pinput(td,16)
            do j = 1,min(i+15,lknot-i)
              if(td(j).gt.last) then
                nk = nk + 1
              endif
              last = td(j)
            end do ! j
            write(ios,2002) (td(j-i+1),j=i,min(i+15,lknot))
          end do ! i

!       Closed or Periodic knot

        elseif(pcomp(tx,'clos',4) .or.
     &         pcomp(tx,'peri',4)) then

!         Set dimensioning values

          nknot = max(nknot,nint(td(1)))
          lknot = nint(td(2))
          dknot = max(dknot,lknot)
          if(startfl) then
            write(ios,'(a)') 'KNOTs'
            startfl = .false.
          endif
          write(ios,2003) nint(td(1)),lknot,nint(td(3)),
     &                        (td(j+3),j=1,min(12,lknot))

          nk   = 0
          last = td(4)
          do j = 2,min(12,lknot)
            if(td(j+3).gt.last) then
              nk = nk + 1
            endif
            last = td(j+3)
          end do ! j
          do i= 13,lknot,16
            errck = pinput(td,16)
            do j = 1,min(i+15,lknot-i)
              if(td(j).gt.last) then
                nk = nk + 1
              endif
              last = td(j)
            end do ! j
            write(ios,2002) (td(j-i+1),j=i,min(i+15,lknot))
          end do ! i
        endif

!       Set size of knot elements array

        sknot = max(nk,sknot)

      end do ! while
      write(ios,'(a)') ' '

!     Formats

2001  format('  open',2i4,1p,13e16.8)
2002  format(1p,16e16.8)
2003  format('  periodic',3i4,1p,13e16.8)

      end subroutine pknotlen
