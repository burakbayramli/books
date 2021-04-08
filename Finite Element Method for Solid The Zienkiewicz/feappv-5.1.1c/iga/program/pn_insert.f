!$Id:$
      subroutine pn_insert(lct,ctl, nblk)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!     Modification log                                Date (dd/mm/year)
!       Original version                                    01/03/2017
!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Insert knots in patches of NURBS elements
!      Inputs:
!      Outputs:
!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      include   'cdata.h'
      include   'cnurb.h'
      include   'iodata.h'
      include   'iofile.h'
      include   'pconstant.h'
      include   'sdata.h'
      include   'umac1.h'

      include   'pointer.h'
      include   'comblk.h'

      logical    pcomp
      character  lct*15
      real*8     ctl(3)
      integer    nblk(14,*)

      integer    blk, bdm, rr, dir, n, bblk
      real*8     uu

      save

      nnurnp = 0
      nnside = 0

!     Save file:

      open(unit = ios, file = 'NURBS_mesh', status = 'unknown')
      rewind ios

      write(iow,2000)
      if(ior.lt.0) then
        write(*,2000)
      endif

!     Insert knot in one block

      if(pcomp(lct,'bloc',4) .or. pcomp(lct,'patc',4)) then

!       Set control data for inserting knot into block

        blk = nint(ctl(1))    ! Block number to refine
        bdm = nblk(1,blk)     ! Dimension of block
        if(bdm.eq.1) then
          dir = 1
          if(maci.gt.0) then
            uu  = ctl(3)      ! Knot location
            rr  = maci        ! Number of times to insert
        else
            uu  = ctl(2)      ! Knot location
            rr  = nint(ctl(3))! Number of times to insert
          endif
        else
          dir = nint(ctl(2))  ! Knot direction
          uu  = ctl(3)        ! Knot location
          rr  = maci          ! Number of times to insert
        endif
        rr  = max(1,rr)

!       Output command data

        write(iow,2001) blk,dir,uu,rr
        if(ior.lt.0) then
          write(*,2001) blk,dir,uu,rr
        endif

!       Output remaining blocks

        do n = 1,nurbk
          bdm = nblk(1,n)    ! Dimension of block
          if(n.ne.blk) then
            bblk = n
            if(bdm.eq.1) then
              call pelvout1d(bblk,hr(np(43)),hr(np(263)),hr(np(298)),
     &                           mr(np(299)),mr(np(308)),mr(np(309)),
     &                           nblk       ,mr(np(312)))
            elseif(bdm.eq.2) then
              call pelvout2d(bblk,hr(np(43)),hr(np(263)),hr(np(298)),
     &                           mr(np(299)),mr(np(308)),mr(np(309)),
     &                           nblk       ,mr(np(312)))
            else
              call pelvout3d(bblk,hr(np(43)),hr(np(263)),hr(np(298)),
     &                           mr(np(299)),mr(np(308)),mr(np(309)),
     &                           nblk       ,mr(np(312)))
            endif
          else
            bblk = blk
            if(bdm.eq.1) then
              call poutblk1d(bblk,uu,rr,
     &                       hr(np(298)),mr(np(299)),mr(np(308)),
     &                       mr(np(309)),nblk       ,mr(np(311)),
     &                       mr(np(312)))
            elseif(bdm.eq.2) then
              call poutblk2d(bblk,dir,uu,rr,
     &                       hr(np(298)),mr(np(299)),mr(np(308)),
     &                       mr(np(309)),nblk       ,mr(np(311)),
     &                       mr(np(312)))
            elseif(bdm.eq.3) then
              call poutblk3d(bblk,dir,uu,rr,
     &                       hr(np(298)),mr(np(299)),mr(np(308)),
     &                       mr(np(309)),nblk       ,mr(np(311)),
     &                       mr(np(312)))
            endif
          endif
        end do ! n

!     Uncoded option

      else

        write(*,*) ' OPTION;',lct(1:4),' NOT CODED'

      endif

!     Close file for new mesh

      keepfl = .false.     ! Prevents multiple output files
      close(ios,status ='keep')

!     Formats

2000  format(' Initialize for knot Insertion'/)

2001  format(' Knot Insertion'/10x,'Block= ',i3,': Direction = ',i2,
     &       ': u = ',1p,1e11.3,',',i2,' times')

      end
