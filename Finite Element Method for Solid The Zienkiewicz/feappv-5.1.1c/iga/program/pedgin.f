!$Id:$
      subroutine pedgin()

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Control routine for data inputs based on edge coordinate

!      Inputs:
!         none      - Data retrieved through common blocks

!      Outputs:
!         none      - Data stored in blank common locations
!-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'cdata.h'
      include  'cnurb.h'
      include  'comfil.h'
      include  'conval.h'
      include  'edgdat.h'
      include  'iodata.h'
      include  'iofile.h'
      include  'ioincl.h'
      include  'pointer.h'
      include  'print.h'
      include  'sdata.h'
      include  'comblk.h'

      character fnam*128, fext*4, vtype*4
      logical   lsave, oprt,oprth
      integer   i,n,l1, iosave

      save

!     Set edge angle values

      oprt  = prt
      oprth = prth
      if(eanfl) then
        do l1 = 0,neang-1
          fext = 'ww0'
          if(l1.le.9) then
            write(fext(3:3),'(i1)') l1
          elseif(l1.le.99) then
            write(fext(2:3),'(i2)') l1
          endif
          fnam = fsav
          call addext(fnam,fext,18,4)
          call opnfil(fext,fnam,-2,ios,lsave)
          if(lsave) then
            iosave = ior
            ior    = ios

            do i = 0,36
              do n = 1,26
                vvsave(n,i) = vvv(n,i)
              end do
            end do

            read(ior,1000) vtype,fincld(isf),irecrd(isf),prt,prth
            read(ior,1001) vvv
          else
            write(iow,3000)
            call plstop(.true.)
          endif

          call peforc(hr(np(43)),hr(np(45)),ndm,1,numnp,
     &                vtype,prt,prth,'Angle')

          close(ior,status = 'delete')
          ior = iosave

          do i = 0,36
            do n = 1,26
              vvv(n,i) = vvsave(n,i)
            end do
          end do
        end do
      endif

!     Set edge boundary conditions

      if(ebcfl) then
        do l1 = 0,nebcs-1
          fext = 'co0'
          if(l1.le.9) then
            write(fext(3:3),'(i1)') l1
          elseif(l1.le.99) then
            write(fext(2:3),'(i2)') l1
          endif
          fnam = fsav
          call addext(fnam,fext,18,4)
          call opnfil(fext,fnam,-2,ios,lsave)
          if(lsave) then
            iosave = ior
            ior    = ios

            do i = 0,36
              do n = 1,26
                vvsave(n,i) = vvv(n,i)
              end do
            end do

            read(ios,1000) vtype,fincld(isf),irecrd(isf),prt,prth
            read(ios,1001) vvv
          else
            write(iow,3000)
            call plstop(.true.)
          endif

          call pedges(hr(np(43)),mr(np(31)+ndf*numnp),ndm,ndf,numnp,
     &                vtype,prt,prth,'B.C.')

          close(ios,status = 'delete')
          ior = iosave

          do i = 0,36
            do n = 1,26
              vvv(n,i) = vvsave(n,i)
            end do
          end do
        end do
      endif

!     Set edge displacement values

      if(edifl) then
        do l1 = 0,nedis-1
          fext = 'ud0'
          if(l1.le.9) then
            write(fext(3:3),'(i1)') l1
          elseif(l1.le.99) then
            write(fext(2:3),'(i2)') l1
          endif
          fnam = fsav
          call addext(fnam,fext,18,4)
          call opnfil(fext,fnam,-2,ios,lsave)
          if(lsave) then
            iosave = ior
            ior    = ios

            do i = 0,36
              do n = 1,26
                vvsave(n,i) = vvv(n,i)
              end do
            end do

            read(ios,1000) vtype,fincld(isf),irecrd(isf),prt,prth
            read(ios,1001) vvv
          else
            write(iow,3000)
            call plstop(.true.)
          endif

          call peforc(hr(np(43)),hr(np(27)+ndf*numnp),ndm,ndf,numnp,
     &                vtype,prt,prth,'Displ')

          close(ios,status = 'delete')
          ior = iosave

          do i = 0,36
            do n = 1,26
              vvv(n,i) = vvsave(n,i)
            end do
          end do

        end do
      endif

!     Set edge force values

      if(efcfl) then
        do l1 = 0,nefrc-1
          fext = 'ld0'
          if(l1.le.9) then
            write(fext(3:3),'(i1)') l1
          elseif(l1.le.99) then
            write(fext(2:3),'(i2)') l1
          endif
          fnam = fsav
          call addext(fnam,fext,18,4)
          call opnfil(fext,fnam,-2,ios,lsave)
          if(lsave) then
            iosave = ior
            ior    = ios

            do i = 0,36
              do n = 1,26
                vvsave(n,i) = vvv(n,i)
              end do
            end do

            read(ios,1000) vtype,fincld(isf),irecrd(isf),prt,prth
            read(ios,1001) vvv
          else
            write(iow,3000)
            call plstop(.true.)
          endif

          call peforc(hr(np(43)),hr(np(27)),ndm,ndf,numnp,
     &                vtype,prt,prth,'Force')

          close(ios,status = 'delete')
          ior = iosave

          do i = 0,36
            do n = 1,26
              vvv(n,i) = vvsave(n,i)
            end do
          end do

        end do
      endif

!     Set edge proportional load numberes

      if(eprfl) then
        do l1 = 0,nepro-1
          fext = 'ep0'
          if(l1.le.9) then
            write(fext(3:3),'(i1)') l1
          elseif(l1.le.99) then
            write(fext(2:3),'(i2)') l1
          endif
          fnam = fsav
          call addext(fnam,fext,18,4)
          call opnfil(fext,fnam,-2,ios,lsave)
          if(lsave) then
            iosave = ior
            ior    = ios

            do i = 0,36
              do n = 1,26
                vvsave(n,i) = vvv(n,i)
              end do
            end do

            read(ios,1000) vtype,fincld(isf),irecrd(isf),prt,prth
            read(ios,1001) vvv
          else
            write(iow,3000)
            call plstop(.true.)
          endif

          vtype = 'set'
          call pedges(hr(np(43)),mr(np(29)),ndm,ndf,numnp,
     &                vtype,prt,prth,'Prop')

          close(ios,status = 'delete')
          ior = iosave

          do i = 0,36
            do n = 1,26
              vvv(n,i) = vvsave(n,i)
            end do
          end do
        end do
      endif

!     Set NURBS loads: account for load tables or normal inputs

      if(nlodfl) then
        call plblock(2)
      endif

!     Set NURBS displacements: account for load tables or normal inputs

      if(ndisfl) then
        call pdblock(2)
      endif

      prt  = oprt
      prth = oprth

1000  format(a4,2x,a12,i8,2l5)
1001  format(1p,4e20.12)

3000  format(' *ERROR* Edge data file',a,' does not exist')

      end
