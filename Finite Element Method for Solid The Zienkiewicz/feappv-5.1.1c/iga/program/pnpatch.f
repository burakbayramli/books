!$Id:$
      subroutine pnpatch(knots,nsides,lknot,lside,nblk,nblksd,pflag)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: NURBS Patch descriptions

!      Inputs:
!        pflag           : True for printed outputs

!      Outputs:
!        nsides(*,*)     : Side vector control point list
!        lside(2,*)      : Side length and knot vector
!        nblk(14,*)      : Block data storage
!        nblksd(*,*)     : Store side numbers for blocks
!-----[--+---------+---------+---------+---------+---------+---------+-]
      implicit   none

      include   'bdata.h'
      include   'cnurb.h'
      include   'dstars.h'
      include   'igdata.h'
      include   'iofile.h'
      include   'nblend.h'
      include   'print.h'
      include   'p_ptname.h'
      include   'region.h'

      logical    pflag, errck, pinput, tinput, pcomp
      character  btext*15
      integer    nblk(14,*)
      integer    nsides(dsideig,*),lknot(0:4,*),lside(2,*)
      integer    nblksd(dblokig,*)
      real*8     knots(dknotig,*)
      real*8     td(7),cd(16)

      integer    ib,is,is0, ma, pn1,pn2,pn3, k1,k2,k3, onubk,onusd
      integer    i,j,k, jj,jm, ilaste,jlaste,klaste

!     Initialize and count sizes

      jm    = 0
      ib    = nurbk
      is    = nursd
      onubk = ib + 1
      onusd = is + 1
      btext = 'start'
      do while(.not.pcomp(btext,'    ',4))
        errck = tinput(btext,1,td,7)
        if(pcomp(btext,'line',4)) then
          ib    = ib + 1
          nurbk = ib
          nblk(5,ib) = ipart  ! Set part number nurbpart(ib)
          ma  = max(1,nint(td(1)))
          pn1 = nint(td(2))
          k1  = nint(td(3))

!         Check for error

          if(min(ma,pn1,k1).le.0) then
            write(iow,3001) ma,pn1,k1
            write(  *,3001) ma,pn1,k1
            call plstop(.true.)
          endif

!         Count number of elements in patch

          if(pflag) then
            call pknotelm(knots(1,k1),lknot(1,k1),ilaste)
            last_elm = ptelm(1,ipart) + ilaste - 1
          endif

!         Set block parameters

          nactb(1)    = .true.

!         nblkdm(ib) = 1; nurmat(ib) = ma ; nuregn(ib)  = nreg
!         lblksd(ib) = pn1

          nblk(1,ib)  = 1
          nblk(2,ib)  = ma
          nblk(3,ib)  = nreg
          nblk(4,ib)  = pn1
          jm          = max(jm,pn1)

          is0 = is
          is  = is + 1
          do k = 1,pn1,16
            errck = pinput(cd(1),min(16,pn1-k+1))
            jj = 0
            do j = k,min(k+15,pn1)
              jj           = jj + 1
              nsides(j,is) = nint(cd(jj)) + starnd
            end do ! j
          end do ! k
          nblk(6,ib) = is        ! nblk3d(1,ib)
          nblk(7,ib) = 0         ! nblk3d(2,ib)
          lside(1,is)  = pn1
          lside(2,is)  = k1      ! kside(is)
          nblksd(1,ib) = is
!         nblk(4,is)   = pn1     ! lblksd(is)
          nursd        = is
          nurbk        = ib

        elseif(pcomp(btext,'surf',4)) then
          ib    = ib + 1
          nurbk = ib
          nblk(5,ib) = ipart      ! nurbpart(ib) Set part number
          ma    = max(1,nint(td(1)))
          pn1   = nint(td(2))
          pn2   = nint(td(3))
          k1    = nint(td(4))
          k2    = nint(td(5))

!         Check for error

          if(min(ma,pn1,pn2,k1,k2).le.0) then
            write(iow,3002) ma,pn1,pn2,k1,k2
            write(  *,3002) ma,pn1,pn2,k1,k2
            call plstop(.true.)
          endif

!         Count number of elements in patch

          if(pflag) then
            call pknotelm(knots(1,k1),lknot(1,k1),ilaste)
            call pknotelm(knots(1,k2),lknot(1,k2),jlaste)
            last_elm = ptelm(1,ipart) + ilaste*jlaste - 1
          endif

!         Set block parameters

          nactb(2)    = .true.
!         nblkdm(ib)  = 2    ; nurmat(ib)  = ma; nuregn(ib) = nreg
!         nbk3d(1,ib) = pn2+1; nbk3d(2,ib) = 0 ; lblksd(ib) = pn2
          nblk(1,ib)  = 2
          nblk(2,ib)  = ma
          nblk(3,ib)  = nreg
          nblk(4,ib)  = pn2
          nblk(6,ib)  = pn2+1
          nblk(7,ib)  = 0
          jm          = max(jm,pn2)

          nblk(8,ib)  = is+1         ! eside(1,ib)
          is0 = is
          do i = 1,pn2
            is    = is + 1
            do k = 1,pn1,16
              errck = pinput(cd(1),min(16,pn1-k+1))
              jj = 0
              do j = k,min(k+15,pn1)
                jj           = jj + 1
                nsides(j,is) = nint(cd(jj)) + starnd
              end do ! j
            end do ! k
            lside(1,is)  = pn1
            lside(2,is)  = k1
            nblksd(i,ib) = is
          end do ! i
          nblk( 9,ib) = is            ! eside(2,ib) = is
          nblk(10,ib) = is+1          ! eside(3,ib) = is+1
          nblk(11,ib) = is+2          ! eside(4,ib) = is+2

!         Increase for bottom side storage

          is               = is + 1
          nblksd(pn2+1,ib) = is
          do j = 1,pn2
            nsides(j,is) = nsides(1,is0+j)
          end do ! j
          lside(1,is) = pn2
          lside(2,is) = k2

!         Increase for top side storage

          is               = is + 1
          do j = 1,pn2
            nsides(j,is) = nsides(pn1,is0+j)
          end do ! j
          lside(1,is) = pn2
          lside(2,is) = k2
          nursd     = is

!       Three-d blocks

        elseif(pcomp(btext,'soli',4)) then
          ib    = ib + 1
          nurbk = ib
          nblk(5,ib) = ipart          ! nurbpart(ib) Set part number
          ma    = max(1,nint(td(1)))
          pn1   = nint(td(2))
          pn2   = nint(td(3))
          pn3   = nint(td(4))
          k1    = nint(td(5))
          k2    = nint(td(6))
          k3    = nint(td(7))

!         Check for error

          if(min(ma,pn1,pn2,pn3,k1,k2,k3).le.0) then
            write(iow,3003) ma,pn1,pn2,pn3,k1,k2,k3
            write(  *,3003) ma,pn1,pn2,pn3,k1,k2,k3
            call plstop(.true.)
          endif

!         Count number of elements in patch

          if(pflag) then
            call pknotelm(knots(1,k1),lknot(1,k1),ilaste)
            call pknotelm(knots(1,k2),lknot(1,k2),jlaste)
            call pknotelm(knots(1,k3),lknot(1,k3),klaste)
            last_elm = ptelm(1,ipart) + ilaste*jlaste*klaste - 1
          endif

!         Set block parameters

          nactb(3)    = .true.

!         nblkdm(ib)  = 3 ; nurmat(ib)  = ma; nuregn(ib) = nreg
!         nbk3d(1,ib) = k1; nbk3d(2,ib) = k2; lblksd(ib) = pn1*pn2
          nblk(1,ib)  = 3
          nblk(2,ib)  = ma
          nblk(3,ib)  = nreg
          nblk(4,ib)  = pn1*pn2
          nblk(6,ib)  = k1
          nblk(7,ib)  = k2
          jm          = max(jm,pn2,1)

          is0 = is
          do i = 1,pn1*pn2
            is    = is + 1
            do k = 1,pn3,16
              errck = pinput(cd(1),min(16,pn3-k+1))
              jj = 0
              do j = k,min(k+15,pn3)
                jj           = jj + 1
                nsides(j,is) = nint(cd(jj)) + starnd
              end do ! j
            end do ! k
            lside(1,is) = pn3
            lside(2,is) = k3
            nblksd(i,ib) = is
          end do ! i
          nursd     = is

        endif
      end do ! while

!     Output knot results for 1-d and 2-d blocks

      if(pflag) then
        if(prt .and. nblk(1,ib).le.2 ) then
          do is = onubk,nurbk
            if(nblk(1,is).eq.1) then
              write(iow,2000) head,(j,j=1,jm)
            else
              write(iow,2000) head,2,(1,j=1,jm)
            endif
            i = nblk(4,is) + 1
            write(iow,2001) is,nblk(2,is),nblksd(i,is),
     &                      (nblksd(j,is),j=1,nblk(4,is))
          end do ! is
!         Output sides generated

          write(iow,2003)
          do is = onusd,nursd
           write(iow,2001) is,lside(2,is),(nsides(j,is),j=1,lside(1,is))
          end do ! i

!       Output for 3-d blocks

        elseif(prt .and. nblk(1,ib).le.3 ) then
          write(iow,2002) head,(nblk(i,ib),i=6,7),
     &                    lside(2,nblksd(1,ib)),(j,j=1,jm)
          do is = onubk,nurbk
            write(iow,2001) is,nblk(2,is),
     &                      (nblksd(j,is),j=1,nblk(4,is))
          end do ! is
        endif
      endif

!     Formats

2000  format(/1x,19a4,a3//5x,'B L O C K   S i d e   N o d e s'//
     &  '    Block   Matl.',6(i5,'-Side')/(17x,6(i5,'-Side')))

2001  format(2i8,6i10/(16x,6i10))

2002  format(/1x,19a4,a3//5x,'B L O C K   S i d e   N o d e s'//
     &  '    Knot Vectors: 1 =',i5,' 2 =',i5,' 3 =',i5/
     &  '    Block   Matl.',6(i5,'-Side')/(17x,6(i5,'-Side')))

2003  format(/5x,'Side    Knot       Control Points')

3001  format(' **ERROR* NPATCH: Missing surface data'/
     &       5x,'Material    = ',i4/
     &       5x,'Dimension 1 = ',i4/
     &       5x,'Knot 1      = ',i4)

3002  format(' **ERROR* NPATCH: Missing surface data'/
     &       5x,'Material    = ',i4/
     &       5x,'Dimension 1 = ',i4/
     &       5x,'Dimension 2 = ',i4/
     &       5x,'Knot 1      = ',i4/
     &       5x,'Knot 2      = ',i4)

3003  format(' **ERROR* NPATCH: Missing solid data'/
     &       5x,'Material    = ',i4/
     &       5x,'Dimension 1 = ',i4/
     &       5x,'Dimension 2 = ',i4/
     &       5x,'Dimension 3 = ',i4/
     &       5x,'Knot 1      = ',i4/
     &       5x,'Knot 2      = ',i4/
     &       5x,'Knot 3      = ',i4)

      end
