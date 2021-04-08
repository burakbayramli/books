!$Id:$
      subroutine vem_proj2d(xl, isw)

      implicit   none

      include   'debugs.h'
      include   'iofile.h'
      include   'cdata.h'
      include   'eldata.h'
      include   'pconstant.h'
      include   'pmod2d.h'
      include   'qudshp.h'
      include   'sdata.h'
      include   'vem_data.h'

      integer          :: isw
      real    (kind=8) :: xl(ndm,*)

!     Local variable

      integer          :: ll

      save

!     Linear order solution

      if(k_order.eq.1) then

!       Set quadrature on triangles

        if(isw.eq.5) then
          ll = -3     ! Interior
        else
          ll = 1     ! Interior
        endif
        call tint2d(ll,lint2,el2)

        lint1 = 1    ! Edge
        call int1d(lint1,sg1)

        npm = 1
        nvn = nel

!     Quadratic order solution

      elseif(k_order.eq.2) then

!       Set quadrature on triangles

        if(isw.eq.5) then
          ll =  7    ! Interior
        else
          call tint2d(1,lint2,elq) ! Straight edge order
          ll =  7    ! Interior    ! Curved edge order
        endif
        call tint2d(ll,lint2,el2)

!       Set edge quadrature points
        lint1 = 3    ! Edge
        call int1d(lint1,sg1)

        npm = 3
        nvn = nel/2

      else

        write(*,'(a,i3,a)') ' PROJ: Order =',k_order,' not coded'
        call plstop(.true.)

      endif

!     Set number of functions
!                                  k_order  !  1  !  2  !  3
!     -------------------------------------------------------
      nk   = (k_order + 1)*(k_order + 2)/2  !  3  !  6  ! 10
      nkm1 = (k_order    )*(k_order + 1)/2  !  1  !  3  !  6
      nkm2 = (k_order - 1)*(k_order    )/2  !  0  !  1  !  3
!     -------------------------------------------------------

!     Compute area and centroid for 2-d element

      call vem_cent2d(xl,ndm,nel)
      hVm1  = sqrt(acos(-1.0d0)/vol)
      hVm1  = 1.0d0
      volhm = vol*hVm1

!     This is common to all derivative forms

      call vem_pmat2d(xl, nel)

      if(k_order.eq.1 .and. isw.eq.3 .and. stype.ne.3) then
        ltot = 1
        if(debug) write(*,*) ' LTOT_stif =',ltot
      endif

      end subroutine vem_proj2d
