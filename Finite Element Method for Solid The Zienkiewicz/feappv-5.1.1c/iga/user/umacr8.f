!$Id:$
      subroutine umacr8(lct,ctl)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose:  Curve: Elevation of order

!      Use:      ELEVate BLOCk num_blk  blk_dir inc_order
!             or ELEVate PATCh num_blk  blk_dir inc_order

!      Data structure:
!         Knots:
!            lknot(1,k) - Length of knot vector 'k'
!            lknot(2,k) - Order  of knot vector 'k'
!            lknot(3,k) - Number control points for knot 'k'
!            knots(j,i) - Knot values for vector 'k'

!         Sides:
!            lside(1,s)  - Length of side 'k'
!            lside(2,s)  - Knot vector for side 'k'
!            nsides(j,s) - Control point numbers for side 'k'

!         Blocks:
!            nblk(1,b)   - Dimension of block (1 or 2)
!            nblk(2,b)   - Number of 1-direction sides
!            nblksd(j,b) - 1-direction side numbers
!            nblksd(e,b) - 2-direction side number (e = lblksd(b) + 1)

!      Inputs:
!         lct       - Command character parameters
!         ctl(3)    - Command numerical parameters

!      Outputs:
!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      include   'umac1.h'

      include   'pointer.h'
      include   'comblk.h'

      logical    pcomp
      character  lct*15
      real*8     ctl(3)

      save

!     Set command word

      if(pcomp(uct,'mac8',4)) then      ! Usual    form
        uct = 'elev'                    ! Specify 'elevate' function
      elseif(urest.eq.1) then           ! Read  restart data

      elseif(urest.eq.2) then           ! Write restart data

      else                              ! Perform user operation

        call u8elev(lct,ctl, mr(np(310)))

      endif

      end

      subroutine u8elev(lct,ctl, nblk)

      implicit   none

      include   'cdata.h'
      include   'cnurb.h'
      include   'igdata.h'
      include   'iodata.h'
      include   'iofile.h'
      include   'sdata.h'
      include   'umac1.h'

      include   'pointer.h'
      include   'comblk.h'

      logical    pcomp
      character  lct*15
      integer    blk, bdim, n, bblk
      real*8     ctl(3)
      integer    nblk(14,*)

      save

      nnurnp = 0
      nnside = 0

!     Save file:

      open(unit = ios, file = 'NURBS_mesh', status = 'unknown')
      rewind ios

!     Output information

      write(iow,2000)
      if(ior.lt.0) then
        write(*,2000)
      endif

!     Elevate one block

      if(pcomp(lct,'bloc',4) .or. pcomp(lct,'patc',4)) then

        blk  = nint(ctl(1))  ! Block number to refine
        blk  = max(1,min(nurbk,blk))

!       Output blocks

        do n = 1,nurbk

          bdim = nblk(1,n)   ! Dimension of block

!         Unmodified blocks

          if(n.ne.blk) then
            bblk = n
            if(bdim.eq.1) then
              call pelvout1d(bblk,hr(np(43)),hr(np(263)),hr(np(298)),
     &                           mr(np(299)),mr(np(308)),mr(np(309)),
     &                           mr(np(310)),mr(np(312)))
            elseif(bdim.eq.2) then
              call pelvout2d(bblk,hr(np(43)),hr(np(263)),hr(np(298)),
     &                           mr(np(299)),mr(np(308)),mr(np(309)),
     &                           mr(np(310)),mr(np(312)))
            else
              call pelvout3d(bblk,hr(np(43)),hr(np(263)),hr(np(298)),
     &                           mr(np(299)),mr(np(308)),mr(np(309)),
     &                           mr(np(310)),mr(np(312)))
            endif

!         Elevate specified block

          else
            if(bdim.eq.1) then
              call pelvblk1d(ctl,hr(np(298)),mr(np(299)),mr(np(308)),
     &                           mr(np(309)),mr(np(310)),mr(np(311)),
     &                           mr(np(312)))
            elseif(bdim.eq.2) then
              call pelvblk2d(ctl,hr(np(298)),mr(np(299)),mr(np(308)),
     &                           mr(np(309)),mr(np(310)),mr(np(311)),
     &                           mr(np(312)))
            else
              call pelvblk3d(ctl,hr(np(298)),mr(np(299)),mr(np(308)),
     &                           mr(np(309)),mr(np(310)),mr(np(312)))
            endif
          endif
        end do ! n

!     Uncoded option

      else

        write(*,*) ' OPTION;',lct(1:4),' NOT CODED'

      endif

!     Close file for new mesh

      keepfl = .false.        ! Prevents multiple output files
      close(ios,status ='keep')

!     Formats

2000  format('-> Elevation Initialization'/)

      end
