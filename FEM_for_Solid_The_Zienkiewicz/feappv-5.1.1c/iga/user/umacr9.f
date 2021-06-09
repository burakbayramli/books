!$Id:$
      subroutine umacr9(lct,ctl)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose:  Curve: Knot insertion

!      Use:     INSErt BLOCk num_bk dir u_knot num_times
!            or INSErt PATCh num_bk dir u_knot num_times

!      Data structure:
!         Knots:
!            lknot(1,k) - Length of knot vector 'k'
!            lknot(2,k) - Order  of knot vector 'k'
!            lknot(3,k) - Number control points for knot 'k'
!            knots(j,i) - Knot values for vector 'k'

!         Sides:
!            lside(s)   - Length of side 'k'
!            kside(s)   - Knot vector for side 'k'
!            nsides(j,s) - Control point numbers for side 'k'

!         Blocks:
!            nblk(1,b)   - Dimension of block (1 or 2)
!            nblk(4,b)   - Number of 1-direction sides
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

      if(pcomp(uct,'mac9',4)) then      ! Usual    form
        uct = 'inse'                    ! Specify 'insert' function
      elseif(urest.eq.1) then           ! Read  restart data

      elseif(urest.eq.2) then           ! Write restart data

      else                              ! Perform user operation

        call u9insert(lct,ctl,mr(np(310)))

      endif

      end

      subroutine u9insert(lct,ctl, nblk)

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
