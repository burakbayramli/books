!$Id:$
      subroutine vem_new(ix, intel, x, xl, maxfac, k, ntype)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!     Purpose: Elevate order of VEM mesh and output new input file

!     Inputs:
!       ix(nen1,numel)  - Element connection array for k=1 mesh
!       intel(5,maxfac) - Edge connection list
!                         1 = Element 1 number
!                         2 = Element 2 number
!                         3 = Edge facet node 1
!                         4 = Edge facet node 2
!                         5 = Edge facet node 3  (used for 3-d not here)
!       x(ndm,numnp)    - Node coordinates for k=1 mesh
!       maxfac          - Number of edge facets
!       k               - Mesh order to generate (boundary nodes only)
!       ntype           - .true.  = equal spacing
!       ntype           - .false. = Lobatto spacing

!     Work:
!       xl(ndm,nen)     - Node coordinates element

!     Outputs:
!       Coordinates and connection for k-order VEM mesh
!----[--+---------+---------+---------+---------+---------+---------+-]
      implicit   none

      include   'cdata.h'
      include   'sdata.h'
      include   'iodata.h'
      include   'iofile.h'
      include   'vem_data.h'

      integer          :: ix(nen1,*)
      integer          :: intel(5,*)
      real    (kind=8) :: x(ndm,*)
      real    (kind=8) :: xl(ndm,*)

      character        :: vem_out*10
      logical          :: ntype, dir1, dir2
      integer          :: n,nd, nelm,nel, maxfac
      integer          :: i
      integer          :: k, km1,km2
      integer          :: el1,nd1,nel1, i1,j1
      integer          :: el2,nd2,nel2, i2,j2
      real    (kind=8) :: xn(2), dx(2), xx(2)
      real    (kind=8) :: e0(2), e1(2), e2(2)
      real    (kind=8) :: sw(2,4)

      integer          :: nextra
      integer         , allocatable :: ixk(:,:,:)
      integer         , allocatable :: ixi(:,:)

      save

      data      e0 / 0.1d0, 0.1d0 /
      data      e1 / 0.2d0, 0.0d0 /
      data      e2 / 0.0d0, 0.2d0 /

!     Set internal points & output order of mesh to screen

      if(ntype) then
        call int1dn(k+1,sw)
        call mprint(sw,1,k+1,2,'SW_EQUAL')
        vem_out = 'VEM_MESH3E'
        write(*,'(a,i3)') ' --> Generate VEM_E mesh at order = ',k
      else
        call int1dl(k+1,sw)
        call mprint(sw,1,k+1,2,'SW_LOBATTO')
        vem_out = 'VEM_MESH3L'
        write(*,'(a,i3)') ' --> Generate VEM_L mesh at order = ',k
      endif

!     Set order sub-sizes

      km1 = k - 1
      km2 = km1*k/2

!     Open output file

      if(k.eq.2) then
        vem_out(9:9)='2'
      endif
      open( unit=ios, file=vem_out )

      nelm = 0
      do n = 1,numel
        nel = 0
        do i = 1,nen
          if(ix(i,n).gt.0) nel = i
        end do ! i
        if(ix(nen+7,n).eq.-22) then
          nelm = max(nelm,nel)
        endif
      end do ! n

!     Allocate arrays for extra nodes

      allocate( ixk(km1,nelm,numel) ) ! For boundary node storage
      allocate( ixi(km2,numel) )      ! For internal node storage

      ixk(:,:,:) = 0

      nd = numnp
      write(ios,'(a)')  'COORdinates added nodes'

      do n = 1,numnp
        write(ios,'(i8,i4,1p,3e16.7)') n,0,x(1:2,n)
      end do ! n

!     Loop over edge facets

      do n = 1,maxfac

!       First element

        el1  = intel(1,n)
        el2  = intel(2,n)
        nd1  = intel(3,n)
        nd2  = intel(4,n)
        nel1 = 0
        nel2 = 0
        do i = 1,nen
          if(ix(i,el1).gt.0) nel1 = i
          if(ix(i,el2).gt.0) nel2 = i
        end do ! i

        do i1 = 1,nel1
          j1 = mod(i1,nel1) + 1
          if    (ix(i1,el1).eq.nd1 .and. ix(j1,el1).eq.nd2) then
            dir1 = .true.
            go to 100
          elseif(ix(i1,el1).eq.nd2 .and. ix(j1,el1).eq.nd1) then
            dir1 = .false.
            go to 100
          endif
        end do ! i1
        write(  *,*) ' ERROR NEWVEM: EXIT LOOP I1',i1
        write(iow,*) ' ERROR NEWVEM: EXIT LOOP I1',i1
100     continue

!       Second element

        do i2 = 1,nel2
          j2 = mod(i2,nel2) + 1
          if    (ix(i2,el2).eq.nd1 .and. ix(j2,el2).eq.nd2) then
            dir2 = .true.
            go to 200
          elseif(ix(i2,el2).eq.nd2 .and. ix(j2,el2).eq.nd1) then
            dir2 = .false.
            go to 200
          endif
        end do ! i2
        write(  *,*) ' ERROR NEWVEM: EXIT LOOP I2',i2
        write(iow,*) ' ERROR NEWVEM: EXIT LOOP I2',i2
200     continue

!       Add the new nodes and fill element edges

        if(dir1) then
          dx(:) = (x(1:2,nd2) - x(1:2,nd1))*0.5d0
          xn(:) = x(1:2,nd1)
          do i = 1,km1
            xx(:) = xn(:) + dx(:)*(1.d0 + sw(1,i+1))
            write(ios,'(i8,i4,1p,3e16.7)') nd+i,0,xx(:)
            ixk(i,i1,el1) = nd+i
          end do ! i
        elseif(.not.dir1) then
          dx(:) = (x(1:2,nd1) - x(1:2,nd2))*0.5d0
          xn(:) = x(1:2,nd2)
          do i = 1,km1
            xx(:) = xn(:) + dx(:)*(1.d0 + sw(1,i+1))
            write(ios,'(i8,i4,1p,3e16.7)') nd+i,0,xx(:)
            ixk(i,i1,el1) = nd+i
          end do ! i
        endif

!       Fill element 2 edges

        if(dir2) then
          do i = 1,km1
            ixk(i,i2,el2) = nd+k-i
          end do ! i
        elseif(.not.dir2) then
          do i = 1,km1
            ixk(i,i2,el2) = nd+k-i
          end do ! i
        endif

        nd = nd + km1

      end do ! n

!     Fill boundary nodes with equations

      do n = 1,numel

        nel1 = 0
        do i = 1,nen
!         if(ix(i,el1).gt.0) nel1 = i
          if(ix(i,n).gt.0) nel1 = i
        end do ! i
        write(iow,*) ' ELEM=',n,' NEL1 =',nel
        do i1 = 1,nel1
          if(ixk(1,i1,n).eq.0) then
            j1    =  mod(i1,nel1) + 1
            dx(:) = (x(1:2,ix(j1,n)) - x(1:2,ix(i1,n)))*0.5d0
            xn(:) =  x(1:2,ix(i1,n))
            do i = 1,km1
              nd    = nd + 1
              xx(:) = xn(:) + dx(:)*(1.d0 + sw(1,i+1))
              write(ios,'(i8,i4,1p,3e16.7)') nd,0,xx(:)
              ixk(i,i1,n) = nd
            end do ! i
          endif
        end do ! i1

!       Add internal nodes
        do i = 1,nel1
          xl(1:2,i) = x(1:2,ix(i,n))
        end do ! i
        k_order = 1
        call vem_cent2d(xl,2,nel1)
        if(k.eq.2) then
          nd       = nd + 1
          ixi(1,n) = nd
          write(ios,'(i8,i4,1p,3e16.7)') nd,0,xc(:)
        elseif(k.eq.3) then
          xc(:)    = xc(:) - e0(:)*sqrt(vol)

          nd       = nd + 1
          ixi(1,n) = nd
          xn(:)    = xc(:)
          write(ios,'(i8,i4,1p,3e16.7)') nd,0,xn(:)

          nd       = nd + 1
          ixi(2,n) = nd
          xn(:)    = xc(:) + e1(:)*sqrt(vol)
          write(ios,'(i8,i4,1p,3e16.7)') nd,0,xn(:)

          nd       = nd + 1
          ixi(3,n) = nd
          xn(:) = xc(:) + e2(:)*sqrt(vol)
          write(ios,'(i8,i4,1p,3e16.7)') nd,0,xn(:)
        endif
      end do ! n

      write(ios,'(a)') ' '

!     call iprint(ix,nen,numel,nen1,'IX_array')
!     do n = 1,numel
!       call iprint(ixk(1,1,n),km1,nelm,km1,'IXK_elemt')
!     end do ! n

!     Output expanded element connection array

      write(ios,'(a,i3,a,i2)') 'ELEMent NODES=',nelm*k+km2,
     &                         ' TYPE=VEM ORDER=',k

      do n = 1,numel
        nel = 0
        do i = 1,nen
          if(ix(i,n).gt.0) nel = i
        end do ! i
        nextra = (nelm-nel)*k
        write(ios,'(16i8:)') n,0,ix(nen1,n),ix(1:nel,n),
     &                          (ixk(1:km1,i,n),i=1,nel),
     &                           ixi(1:km2,n),(0,i=1,nextra)
      end do ! n
      write(ios,'(a)') ' '

      close(ios, status = 'keep')

!     Deallocate arrays

      deallocate ( ixk )
      deallocate ( ixi )

      end subroutine vem_new
