!$Id:$
      subroutine plblock(isw)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose:  2-d and 3-d surface loads for NURBS

!      Use:
!        NLOAd
!          NORMal   blk face value prop
!          TRACtion blk face value direc prop

!      Inputs:
!        isw    - Switch for input or compute

!      Outputs:
!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      include   'iofile.h'
      include   'qudshp.h'
      include   'sdata.h'

      include   'pointer.h'
      include   'comblk.h'

      logical    pcomp,errck,tinput
      character  tx*15
      integer    isw, nfl,n, lval
      integer    pfac(5,10)
      real*8     td(6),npres(3,10)

      save

!     Input data

      if(    isw.eq.0) then
        nfl   = 0

!     Input data

      elseif(isw.eq.1) then

        tx = 'start'
        do while(.not.pcomp(tx,'    ',4))
          errck = tinput(tx,1,td,6)
          if(pcomp(tx,'norm',4)) then
            nfl          = nfl + 1
            pfac(1,nfl)  = nint(td(1))   ! Block
            pfac(2,nfl)  = nint(td(2))   ! Face
            npres(1,nfl) = td(3)         ! Pressure value
            npres(3,nfl) = 1.0d0         ! Normal loading
            lval = nint(td(4))
            if(lval.eq.0) then
              if(ndm.eq.2) then
                lval = 7
              else
                lval = 4
              endif
            endif
            write(iow,2001) pfac(1:2,nfl),npres(1,nfl),lval
            if(pfac(2,nfl).gt.ndm) then  ! Negative face pressure
              npres(1,nfl) = -npres(1,nfl)
            endif
          elseif(pcomp(tx,'trac',4)) then
            nfl   = nfl + 1
            pfac(1,nfl) = nint(td(1))   ! Block
            pfac(2,nfl) = nint(td(2))   ! Face
            npres(1,nfl) = td(3)        ! Traction value
            npres(2,nfl) = td(4)        ! Traction direction
            npres(3,nfl) = 2.0d0        ! Traction loading
            npres(2,ndf) = max(1,nint(npres(2,nfl)))
            lval = nint(td(5))
            if(lval.eq.0) then
              if(ndm.eq.2) then
                lval = 7
              else
                lval = 4
              endif
            endif
            write(iow,2002) pfac(1:2,nfl),
     &                      npres(1,nfl),nint(npres(2,nfl)),lval
          elseif(pcomp(tx,'user',4)) then
            nfl   = nfl + 1
            pfac(1,nfl) = nint(td(1))   ! Block
            pfac(2,nfl) = nint(td(2))   ! Face
            npres(1,nfl) = td(3)        ! Function value 1
            npres(2,nfl) = td(4)        ! Function value 2
            npres(3,nfl) = 3.0d0        ! User loading
            lval = nint(td(5))
            if(lval.eq.0) then
              if(ndm.eq.2) then
                lval = 7
              else
                lval = 4
              endif
            endif
            write(iow,2003) pfac(1:2,nfl),
     &                      npres(1,nfl),npres(2,nfl),lval
          endif
          pfac(3,nfl) = 0
          pfac(4,nfl) = 0
          pfac(5,nfl) = 0
        end do ! while

!     Compute nodal loads for nurb blocks

      else

!       Set quadrature for all surface elements

        if(ndm.eq.2) then
          call int1d(lval,sg1)
          do n = 1,nfl
            call plblk2d(pfac(1,n),npres(1,n),mr(np(308)),mr(np(309)),
     &                   mr(np(299)),mr(np(312)),mr(np(310)))
          end do ! n
        elseif(ndm.eq.3) then
          call int2d(lval,lint,sg2)
          do n = 1,nfl
            call plblk3d(pfac(1,n),npres(1,n),mr(np(308)),mr(np(309)),
     &                   mr(np(299)),mr(np(312)),mr(np(310)))
          end do ! n
        endif

      endif

!     Formats

2001  format(/5x,'NURB Normal Surface Loading'/
     &       10x,'Block No.  =',i8/
     &       10x,'Face  No.  =',i8/
     &       10x,'Pressure   =',1p,1e12.4/
     &       10x,'Quadrature =',i5)

2002  format(/5x,'NURB Traction Surface Loading'/
     &       10x,'Block No.  =',i8/
     &       10x,'Face  No.  =',i8/
     &       10x,'Traction   =',1p,1e12.4/
     &       10x,'Direction  =',i8/
     &       10x,'Quadrature =',i5)

2003  format(/5x,'NURB User Surface Loading'/
     &       10x,'Block No.  =',i8/
     &       10x,'Face  No.  =',i8/
     &       10x,'Value 1    =',1p,1e12.4/
     &       10x,'Value 2    =',1p,1e12.4/
     &       10x,'Quadrature =',i5)

      end

      subroutine plblk2d(pfac,npres,lknot,lside,nsides,nblksd,nblk)

      implicit   none

      include   'igdata.h'
      include   'iofile.h'

      include   'cdata.h'
      include   'cnurb.h'
      include   'sdata.h'

      include   'p_point.h'
      include   'pointer.h'
      include   'comblk.h'

      integer    blk,fac
      integer    sid1,kno1,lek1,ord1,len1
      integer    sid2,kno2,lek2,ord2,len2
      integer    i, ii, l1
      real*8     npres(3)
      integer    pfac(5),pko(2),psides(300)
      integer    lknot(0:4,*), lside(2,*), nsides(dsideig,*)
      integer    nblk(14,*), nblksd(dblokig,*)

      blk  = pfac(1)
      fac  = pfac(2)

      sid1 = nblksd(1,blk)
      kno1 = lside(2,sid1)          ! kside(sid1)
      lek1 = lknot(1,kno1)
      ord1 = lknot(2,kno1)
      len1 = lside(1,sid1)

      len2 = nblk(4,blk)            ! lblksd(blk)
      sid2 = nblksd(len2+1,blk)
      kno2 = lside(2,sid2)
      lek2 = lknot(1,kno2)
      ord2 = lknot(2,kno2)
      len2 = lside(1,sid2)

      if(fac.eq.1 .or. fac.eq.3) then
        pko(1) = kno2
        if(fac.eq.1) then
          ii = len1
        else
          ii = 1
        endif
        l1 = len2
        do i = 1,len2
          psides(i) = nsides(ii,nblksd(i,blk))
        end do ! i
      elseif(fac.eq.2 .or. fac.eq.4) then
        pko(1) = kno1
        if(fac.eq.2) then
          ii = len2
        else
          ii = 1
        endif
        l1 = len1
        do i = 1,len1
          psides(i) = nsides(i,nblksd(ii,blk))
        end do ! i

      else
        write(*,*) ' PLBLK2D: Illegal face number = ',fac
        return
      endif

      if(np(79).ne.0) then
        call psidpos(psides,l1,1,mr(np(79)))
      endif

      point = np(27)
      call pblkld2d(pko,psides, npres, pfac, hr(np(43)),hr(np(263)),
     &              hr(np(44)),hr(np(264)),mr(np(31)+nneq),hr(point),
     &              hr(np(41)))

      end

      subroutine plblk3d(pfac,npres,lknot,lside,nsides,nblksd,nblk)

      implicit   none

      include   'cdata.h'
      include   'cnurb.h'
      include   'sdata.h'
      include   'igdata.h'

      include   'p_point.h'
      include   'pointer.h'
      include   'comblk.h'

      integer    blk,fac, nod
      integer    kno1,lek1,ord1,len1
      integer    kno2,lek2,ord2,len2
      integer    kno3,lek3,ord3,len3,sid3
      integer    i,j, ii,jj, l1,l2
      real*8     npres(3)
      integer    pfac(5),pko(2),psides(300,300)
      integer    lknot(0:4,*), lside(2,*), nsides(dsideig,*)
      integer    nblk(14,*), nblksd(dblokig,*)

!     Set control parameters

      blk   = pfac(1)
      fac   = pfac(2)

!     Set knot parameters

      kno1 = nblk(6,blk)      ! nbk3d(1,blk)
      lek1 = lknot(1,kno1)
      ord1 = lknot(2,kno1)
      len1 = lknot(3,kno1)

      kno2 = nblk(7,blk)      ! nbk3d(2,blk)
      lek2 = lknot(1,kno2)
      ord2 = lknot(2,kno2)
      len2 = lknot(3,kno2)

      sid3 = nblksd(1,blk)
      kno3 = lside(2,sid3)    ! kside(sid3)
      lek3 = lknot(1,kno3)
      ord3 = lknot(2,kno3)
      len3 = lside(1,sid3)

!     Check face numbers and set surface parameters

      if(fac.eq.1 .or. fac.eq.4) then
        pko(1) = kno2
        pko(2) = kno3
        if(fac.eq.1) then
          nod = len1
        else
          nod = 1
        endif
        l1 = len2
        l2 = len3
        ii = 1
        do j = 1,len3
          jj = nod
          do i = 1,len2
            psides(i,j) = nsides(ii,nblksd(jj,blk))
            jj          = jj + len1
          end do ! i
          ii = ii + 1
        end do ! j
      elseif(fac.eq.2 .or. fac.eq.5) then
        pko(1) = kno3
        pko(2) = kno1
        if(fac.eq.2) then
          nod = len1*(len2 - 1) + 1
        else
          nod = 1
        endif
        l1 = len3
        l2 = len1
        jj = nod
        do j = 1,len1
          do i = 1,len3
            psides(i,j) = nsides(i,nblksd(jj,blk))
          end do ! i
          jj = jj + 1
        end do ! j
      elseif(fac.eq.3 .or. fac.eq.6) then
        pko(1) = kno1
        pko(2) = kno2
        if(fac.eq.3) then
          ii = len3
        else
          ii = 1
        endif
        l1 = len1
        l2 = len2
        jj = 1
        do j = 1,len2
          do i = 1,len1
            psides(i,j) = nsides(ii,nblksd(jj,blk))
            jj          = jj + 1
          end do ! i
        end do ! j
      else
        write(*,*) ' PLBLK3D: Illegal face number = ',fac
        return
      endif

      if(np(79).ne.0) then
        call psidpos(psides,l1,l2,mr(np(79)))
      endif

!     Set load type: table or nodal forces

      point = np(27)

      call pblkld3d(pko,psides, npres, pfac, hr(np(43)),hr(np(263)),
     &              hr(np(44)),hr(np(264)),mr(np(31)+nneq),hr(point),
     &              hr(np(41)))

      end

      subroutine psidpos(psides,l1,l2,ipos)

      implicit   none

      integer    l1,l2, psides(300,*),ipos(*), i,j, nod

      do j = 1,l2
        do i = 1,l1
          nod         = ipos(psides(i,j))
          psides(i,j) = nod
        end do ! i
      end do ! j

      end

      subroutine pblkld2d(pko,psides, npres, pfac, x,wt, xl,wl, id,
     &                    f,fl)

      implicit   none

      include   'cnurb.h'
      include   'eldata.h'
      include   'iofile.h'
      include   'print.h'
      include   'qudshp.h'
      include   'sdata.h'

      include   'pointer.h'
      include   'comblk.h'

      integer    pko(2),psides(300), pfac(*), id(ndf,*)
      real*8     npres(3),pres,fnorm, de, engy, rarc
      real*8     x(ndm,*),wt(*),xl(ndm,*),wl(*),f(ndf,*), fl(ndf,*)
      real*8     xx(3)

      integer    i,j,l, i1, node, dir, blk
      integer    ktlen(2),ktdiv(4,300),ix(64)

!     Set block number

      blk = pfac(1)

!     call pknotel(knots,lknot,pko, ktlen,ktdiv,1)
      call pknotel(hr(np(298)),mr(np(308)),pko, ktlen,ktdiv,1)

      fnorm = npres(1)

      if(prt) write(iow,2000) (i,i=1,2)

!      write(iow,*) ' KTLEN',ktlen(1),' LINT ',lint
!      write(  *,*) ' KTLEN',ktlen(1),' LINT ',lint

      engy = 0.0d0
      rarc = 0.0d0
      do i1 = 1,ktlen(1)
        nel = 0
        do i = ktdiv(3,i1),ktdiv(4,i1)
          nel     = nel + 1
          ix(nel) = psides(i)
        end do ! i

!       Set element type for interpolation

        eltyp = blk + 500*i1

!       Compute local values for surface element

        do j = 1,nel
          node = ix(j)
          do i = 1,ndm
            xl(i,j) = x(i,node)
          end do ! i
          wl(j) = wt(node)
          do i = 1,ndf
            fl(i,j) = 0.0d0
          end do ! i
        end do ! j

!       Integrate over element

        do l = 1,lint

          call shp1d_nurb(sg1(1,l),xl,wl,shp3(1,1,l),jac(l),
     &                    mr(np(308)),mr(np(311)),hr(np(298)),
     &                    ndm, .true.)

!         Multiply by quadrature weight

          jac(l) = jac(l)*sg1(2,l)
          do j = 1,3
            do i = 1,3
              dxdxi(i,j) = dxdxi(i,j)*sg1(2,l)
            end do ! i
          end do ! j

!         Accumulate into control point forces

          if(nint(npres(3)).eq.1) then ! Normal loading
            do j = 1,nel
              pres = fnorm*shp3(4,j,l)
              do i = 1,2
                fl(i,j) = fl(i,j) + pres*dxdxi(i,2)
              end do ! i
            end do ! j
          elseif(nint(npres(3)).eq.2) then ! Traction loading
            dir  = nint(npres(2))
            pres = fnorm*dxdxi(3-dir,dir)
            do j = 1,nel
              fl(dir,j) = fl(dir,j) + pres*shp3(4,j,l)
            end do ! j
          elseif(nint(npres(3)).eq.3) then ! User loading
            do i = 1,2
              xx(i) = 0.0d0
              do j = 1,nel
              xx(i) = xx(i) + xl(i,j)*shp3(4,j,l)
              end do ! j
            end do ! i
            call pnurbld2d(shp3(1,1,l),xx,dxdxi,npres,
     &                     nel,ndf, fl, de)
            engy = engy + de*jac(l)
            rarc = rarc + jac(l)
          endif
        end do ! l

!       Assemble loads

        do j = 1,nel
          node = ix(j)
          do i = 1,2
            if(id(i,node).eq.0) then
              f(i,node) = f(i,node) + fl(i,j)
            endif
          end do ! i
        end do ! j

!       Output loads

        if(prt) then
          do j = 1,nel
            node = ix(j)
            write(iow,2001) node,(id(i,node),fl(i,j),i=1,2)
          end do ! j
          write(iow,*) ' ---------------------- '
        endif

      end do ! i1

!     write(iow,*) ' ENERGY = ',engy

!     Formats

2000  format(//5x,'NURB   S u r f a c e   L o a d s'/
     &         5x,'Node',2(' B.C.',i3,'-Force',3x))

2001  format(i8,6(i5,1p,1e12.4:)/(8x,6(i5,1p,1e12.4:)))

      end

      subroutine pblkld3d(pko,psides, npres,pfac, x,wt, xl,wl, id,
     &                    f,fl)

      implicit   none

      include   'cnurb.h'
      include   'eldata.h'
      include   'iofile.h'
      include   'print.h'
      include   'prlod.h'
      include   'prld1.h'
      include   'qudshp.h'
      include   'sdata.h'

      include   'pointer.h'
      include   'comblk.h'

      integer    pfac(5),pko(2),psides(300,300), id(ndf,*)
      real*8     npres(3),pres,pshp,fnorm
      real*8     x(ndm,*),wt(*),xl(ndm,*),wl(*),f(ndf,*), fl(ndf,*)

      integer    blk
      integer    i,j,l, i1,j1, node
      integer    ktlen(2),ktdiv(4,300),ix(64)

!     Set parameters

      blk = pfac(1)

!     call pknotel(knots,lknot,pko, ktlen,ktdiv,2)
      call pknotel(hr(np(298)),mr(np(308)),pko, ktlen,ktdiv,2)

      fnorm = npres(1)

      if(prt) write(iow,2002) pfac(1:2),npres(1)
      if(prt) write(iow,2000) (i,i=1,3)

      do j1 = ktlen(1)+1,ktlen(2)
        do i1 = 1,ktlen(1)
          nel = 0

          eltyp = blk + 500*i1
          elty2 = j1

          do j = ktdiv(3,j1),ktdiv(4,j1)
            do i = ktdiv(3,i1),ktdiv(4,i1)
              nel     = nel + 1
              ix(nel) = psides(i,j)
            end do ! i
          end do ! j

!         Compute local values for surface element

          do j = 1,nel
            node = ix(j)
            do i = 1,ndm
              xl(i,j) = x(i,node)
            end do ! i
            wl(j) = wt(node)
            do i = 1,ndf
              fl(i,j) = 0.0d0
            end do ! i
          end do ! j

!         Integrate over element

          do l = 1,lint

            call shp2d_nurb(sg2(1,l),xl,wl,shp2(1,1,l),shpm(1,l),jac(l),
     &                      mr(np(308)),mr(np(311)),hr(np(298)),
     &                      ndm, .true.)

!           Accumulate into control point forces

            pres = fnorm*sg2(3,l)
            do j = 1,nel
              pshp = pres*shp2(3,j,l)
              do i = 1,3
                fl(i,j) = fl(i,j) + pshp*dxdxi(i,3)
              end do ! i
            end do ! j

          end do ! l

!         Assemble loads

          do j = 1,nel
            node = ix(j)
            do i = 1,3
              if(id(i,node).eq.0) then
                f(i,node) = f(i,node) + fl(i,j)
              endif
            end do ! i
          end do ! j

!         Output loads

          if(prt) then
            do j = 1,nel
              node = ix(j)
              write(iow,2001) node,(id(i,node),fl(i,j),i=1,3)
            end do ! j
          endif

        end do ! i1
      end do ! j1

!     Formats

2000  format(/5x,'NURB   S u r f a c e   L o a d s'/
     &        5x,'Node',3(' B.C.',i3,'-Force',3x))

2001  format(i8,6(i5,1p,1e12.4:)/(8x,6(i5,1p,1e12.4:)))

2002  format(/5x,'NURB   N o r m a l   S u r f a c e   L o a d i n g'/
     &        8x,'Block No. =',i8/
     &        8x,'Face  No. =',i8/
     &        8x,'Pressure  =',1p,1e12.4)

      end
