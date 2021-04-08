!$Id:$
      subroutine vem_cent2d(xl,ndm,nel)

!-----[--+---------+---------+---------+---------+---------+---------+-]
!     Purpose: Compute volume and centroid for VEM

!     Inputs:
!       xl(ndm,*) - Edge nodal coordinates
!       ndm       - Dimension of nodes on mesh
!       nel       - Number of element nodes
!       k_order   - Order of VEM

!     N.B. k_order = 2 include one internal moment node

!     Outputs: through include 'vem_data.h'
!       vol       - Volume of VEM
!       xc(2)     - Coordinates of VEM centroid
!-----[--+---------+---------+---------+---------+---------+---------+-]
      implicit   none

      include   'debugs.h'     ! debug
      include   'iofile.h'     ! iow
      include   'qudshp.h'     ! el2(4,*), lint2
      include   'vem_data.h'   ! m0, m1,m2

      integer          :: ndm,nel
      real    (kind=8) :: dvl
      real    (kind=8) :: xl(ndm,nel), xb(ndm)

!     Local variables

      integer          :: n,m, c, ne, l
      real    (kind=8) :: xx(2), dx1(2),dx2(2)
      real    (kind=8) :: dxn(2),dxm(2),dx4(2)

      integer          :: ll
      real    (kind=8) :: tol

      save

      data    elq  / 0.0d0, 0.5d0, 0.5d0, 1.0d0,
     &               0.5d0, 0.0d0, 0.5d0, 1.0d0,
     &               0.5d0, 0.5d0, 0.0d0, 1.0d0 /

      data    tol / 1.d-12 /

!     Compute an anchor point

      elq(4,1:3) = 1.d0/3.0d0

      vol   = 0.0d0
      xc(:) = 0.0d0

!     Compute the various cases

      select case (k_order)

        case (1)        ! Linear edges

          xb(:) = sum(xl,2)/dble(nel)

          do n = 1,nel
            m     = mod(n,nel) + 1
            dvl   = (xl(1,n) - xb(1))*(xl(2,m) - xb(2))
     &            - (xl(2,n) - xb(2))*(xl(1,m) - xb(1))
            vol   = vol + dvl  ! Note dvl is 2 x triangular area
!           Measure centroid from location xb(:)
            xc(:) = xc(:) + (xl(:,n) + xl(:,m) - 2.0d0*xb(:))*dvl
          end do ! n
          xc(:) = xb(:) + xc(:)/(vol*3.0d0)
          vol   = vol*0.5d0

        case (2)        ! Quadratic edges

          xb(:) = 0.0d0
          do n = 1,nel-1
            xb(:) = xb(:) + xl(1:2,n)
          end do ! n
          xb(:) = xb(:)/dble(nel-1)

          ll = 0
          ne = nel/2 ! Number of edges
          do n = 1,ne
            m      = mod(n,ne) + 1
            c      = ne + n        ! Mid-edge node number
            dxn(:) = xl(:,n) - xb(:)
            dxm(:) = xl(:,m) - xb(:)
            dx4(:) = xl(:,c)*2.0d0 - xl(:,n) - xl(:,m)

            if((dx4(1)**2 + dx4(2)**2) .lt.
     &         tol*((dxn(1)-dxm(1))**2 + (dxn(2)-dxm(2))**2)) then
              elv(:,ll+1:ll+3) = elq(:,:)
              lintv(n) = 3
            else
              elv(:,ll+1:ll+lint2) = el2(:,1:lint2)
              lintv(n) = lint2
            endif

            do l = 1,lintv(n)
              ll     = ll + 1
              dx1(:) = dxn(:) + 2.d0*elv(2,ll)*dx4(:)
              dx2(:) = dxm(:) + 2.d0*elv(1,ll)*dx4(:)
              xx(:)  = elv(1,ll)*dxn(:) + elv(2,ll)*dxm(:)
     &               + 2.0d0*elv(1,ll)*elv(2,ll)*dx4(:)
              dvl    = (dx1(1)*dx2(2) - dx1(2)*dx2(1))*elv(4,ll)
              vol    = vol + dvl  ! Note dvl is 2 x triangular area
              xc(:)  = xc(:) + xx(:)*dvl
            end do ! l
          end do ! n
          xc(:) = xb(:) + xc(:)/vol
          vol   = vol*0.5d0

        case default

          write(*,'(a,i3,a)') ' --> Order',k_order,' not coded'

      end select

      if(debug) then
        write(*,*) ' VOL:XC',vol,xc
      endif

      end subroutine vem_cent2d
