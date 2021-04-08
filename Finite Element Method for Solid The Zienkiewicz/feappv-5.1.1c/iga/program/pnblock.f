!$Id:$
      subroutine pnblock(prt,knots,nsides,lknot,lside,nblk,nblksd)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Set nurb blocks

!      Data: ib = block number (automatically counted)
!        block nblksd(1,ib) nside

!      Inputs:
!         prt   - Print flag

!      Outputs:
!-----[--+---------+---------+---------+---------+---------+---------+-]
      implicit   none

      include   'bdata.h'
      include   'cnurb.h'
      include   'igdata.h'
      include   'iofile.h'
      include   'nblend.h'
      include   'region.h'

      include   'p_ptname.h'

      real*8     knots(dknotig,*)
      integer    nsides(dsideig,*),lknot(0:4,*),lside(2,*),nblk(14,*)
      integer    nblksd(dblokig,*)
!     real*8     npre(4,*)

      logical    pcomp,errck,tinput, prt
      character  tx*15, typ*15
      integer    i,j, ii,jj,jm, ib, knotn, onubk, oside
      real*8     td(16),tc(4)

      jm    = 0
      ib    = nurbk
      onubk = ib + 1
      tx    = 'start'
      do while (.not.pcomp(tx,'    ',4))
        errck = tinput(tx,1,td,8)
        if(pcomp(tx,'surf',4) .or. pcomp(tx,'bloc',4)) then
          ib    = ib + 1              ! Current block number
          nurbk = max(nurbk,ib)       ! Number of blocks

!         Set part number

!         Set block dimension and side numbers along 1-nurb direction

          nblk(1,ib) = nint(td(1))        ! Dimension block (1,2 or 3)
          nblk(2,ib) = max(1,nint(td(2))) ! Material set for block
          nblk(3,ib) = nreg
          nblk(5,ib) = ipart              ! nurbpart(ib) = ipart
          nblk(6,ib) = nint(td(3))        ! 1 or 2 side number of blk
          nblk(7,ib) = 0

          if(nblk(1,ib).le.2) then
            oside        = nblk(6,ib)
            nblk(6,ib)   = oside
            nblksd(1,ib) = nsides(1,oside)  ! First 1-dir side (2-d blk)
            knotn        = lside(2,oside)   ! Knot number of input side

!           1-D blocks

            if(nblk(1,ib).eq.1) then

              nblksd(1,ib) = oside      ! First 1-dir side
              nblk(4,ib)   = lside(1,oside)
              nactb(1)     = .true.
              call pknotelm(knots(1,knotn),lknot(1,knotn), last_elm)
              last_elm = ptelm(1,ipart) + last_elm - 1

!           2-D blocks

            elseif(nblk(1,ib).eq.2) then

!             Input eside data for bending strips

              errck = tinput(typ,1,tc,4)
              if(pcomp(typ,'esid',4)) then
                do j = 1,4
                  nblk(j+7,ib) = nint(tc(j))  ! eside(1:4)
                end do ! j
              else
                if(ior.gt.0) backspace (ior)
              endif

              call pknotelm(knots(1,knotn),lknot(1,knotn), last_elm)
              nblk(4,ib)   = lside(1,oside)
              nactb(2)     = .true.
              do j = 1,lside(1,oside)

!               Find matching sides (Direction 1 on block)

                do i = 1,nursd
                  if((i.ne.oside) .and.
     &               (nsides(j,oside).eq.nsides(1,i))) then
                    nblksd(j,ib) = i
                    exit
                  endif
                end do ! i
              end do ! j

!             Set side for Direction 2 on block

              nblksd(lside(1,oside)+1,ib) = oside

!             Set last element number
              i = lside(2,nblksd(1,ib))
              call pknotelm(knots(1,i),lknot(1,i), j)
              last_elm = ptelm(1,ipart) + last_elm*j - 1

            endif

!         3-D block

          elseif(nblk(1,ib).eq.3) then

!           Save pressure data
!           npre(1,ib) = td(5)
!           npre(2,ib) = td(6)
!           npre(3,ib) = td(7)
!           npre(4,ib) = td(8)

!           Input 1 and 2 direction size and knot number

            nactb(3)    = .true.
            nblk(7,ib)  = nint(td(4))      ! Knot number
            jj          = lknot(3,nblk(6,ib))*lknot(3,nblk(7,ib))

            ii = 0
            do i = 1,jj,16
              errck = tinput(tx,0,td,16)
              do j = 1,min(jj-ii,16)
                nblksd(j+ii,ib) = nint(td(j))
              end do ! j
              ii = ii + 16
            end do ! i
            nblk(4,ib) = jj        ! lblksd(ib)

!         Error in input data

          else

            write(iow,4000) nblk(1,ib)
            call plstop(.true.)
          endif
        endif
        jm = max(jm,nblk(4,ib))
      end do ! while

!     Output knot results for 1- and 2-d blocks

      if(prt .and. nblk(1,ib).le.2 ) then
        if(nblk(1,ib).eq.1) then
          write(iow,2000) head,(j,j=1,jm)
        else
          write(iow,2000) head,2,(1,j=1,jm)
        endif
        do ii = onubk,nurbk
          i = nblk(4,ii) + 1
          write(iow,2001) ii,nblk(2,ii),nblksd(i,ii),
     &                    (nblksd(j,ii),j=1,nblk(4,ii))
        end do ! ii
      elseif(prt .and. nblk(1,ib).le.3 ) then
        write(iow,2002) head,(nblk(i,ib),i=6,7),
     &                  lside(2,nblksd(1,ib)),(j,j=1,jm)
        do ii = onubk,nurbk
          write(iow,2001) ii,nblk(2,ii),
     &                    (nblksd(j,ii),j=1,nblk(4,ii))
        end do ! is

      endif

!     Formats

2000  format(/1x,19a4,a3//5x,'B L O C K   S i d e   N o d e s'//
     &  '    Block   Matl.',6(i5,'-Side')/(17x,6(i5,'-Side')))

2001  format(2i8,6i10/(16x,6i10))

2002  format(/1x,19a4,a3//5x,'B L O C K   S i d e   N o d e s'//
     &  '    Knot Vectors: 1 =',i5,' 2 =',i5,' 3 =',i5/
     &  '    Block   Matl.',6(i5,'-Side')/(17x,6(i5,'-Side')))

4000  format(' **ERROR** NURB block dimension = ',i4,
     &       ' not available')

      end
