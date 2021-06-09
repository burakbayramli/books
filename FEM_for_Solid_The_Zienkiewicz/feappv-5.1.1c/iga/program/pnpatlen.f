!$Id:$
      subroutine pnpatlen(dsid,nsid,nblk)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: NURBS Patch descriptions

!      Inputs:

!      Outputs:
!-----[--+---------+---------+---------+---------+---------+---------+-]
      implicit   none

      include   'iodata.h'

      logical    errck, pinput, tinput, pcomp, startfl
      character  stext*15, btext*15
      real*8     td(7),cd(16)
      integer    dsid,nsid,nblk

      integer    ma, pn1,pn2,pn3, k1,k2,k3
      integer    i,j,k

!     Initialize and count sizes

      startfl = .true.
      btext   = 'start'
      do while(.not.pcomp(btext,'    ',4))
        errck = tinput(btext,1,td,7)
        if(pcomp(btext,'line',4)) then
          if(startfl) then
            write(ios,'(a)') 'NPATch'
            startfl = .false.
          endif
          ma   = max(1,nint(td(1)))
          pn1  = nint(td(2))
          k1   = nint(td(3))
          dsid = max(dsid,pn1)
          nsid = nsid + 1
          nblk = nblk + 1
          write(ios,'(a,3i6)') '  line',ma,pn1,k1

          do k = 1,pn1,16
            errck = pinput(cd(1),min(16,pn1-k+1))
            write(ios,'(16i6)') (nint(cd(j)),j=1,min(16,pn1-k+1))
          end do ! k

        elseif(pcomp(btext,'surf',4)) then
          if(startfl) then
            write(ios,'(a)') 'NPATch'
            startfl = .false.
          endif
          ma   = max(1,nint(td(1)))
          pn1  = nint(td(2))
          pn2  = nint(td(3))
          k1   = nint(td(4))
          k2   = nint(td(5))
          dsid = max(dsid,pn1,pn2)
          nsid = nsid + pn2 + 4 ! 2
          nblk = nblk + 1
          write(ios,'(a,5i6)') '  surface',ma,pn1,pn2,k1,k2

          do i = 1,pn2
            do k = 1,pn1,16
              errck = pinput(cd(1),min(16,pn1-k+1))
              write(ios,'(16i6)') (nint(cd(j)),j=1,min(16,pn1-k+1))
            end do ! k
          end do ! i

!       Three-d blocks

        elseif(pcomp(btext,'soli',4)) then
          if(startfl) then
            write(ios,'(a)') 'NPATch'
            startfl = .false.
          endif
          ma    = max(1,nint(td(1)))
          pn1   = nint(td(2))
          pn2   = nint(td(3))
          pn3   = nint(td(4))
          k1    = nint(td(5))
          k2    = nint(td(6))
          k3    = nint(td(7))
          dsid  = max(dsid,pn1,pn2,pn3)
          nsid  = nsid + pn1*pn2
          nblk = nblk + 1
          write(ios,'(a,7i6)') '  solid',ma,pn1,pn2,pn3,k1,k2,k3

          do i = 1,pn1*pn2
            errck = tinput(stext,0,cd(1),pn3)
            write(ios,'(16i6:)') (nint(cd(j)),j=1,pn3)
          end do ! i

        endif
      end do ! while

      write(ios,'(a)') ' '

      end
