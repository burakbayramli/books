!$Id:$
      subroutine pn_elev(lct,ctl, nblk)

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
