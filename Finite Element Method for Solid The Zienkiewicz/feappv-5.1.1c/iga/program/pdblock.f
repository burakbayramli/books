!$Id:$
      subroutine pdblock(isw)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose:  2-d  surface displacement for NURBS

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
      integer    isw, ndfl,n
      integer    pdfac(3,10)
      real*8     td(11),ndisp(9,10)

      save

!     Input data

      if(    isw.eq.0) then
        ndfl = 0

!     Input data

      elseif(isw.eq.1) then

        tx = 'start'
        do while(.not.pcomp(tx,'    ',4))
          errck = tinput(tx,1,td,11)
          if(pcomp(tx,'norm',4)) then
            ndfl          = ndfl + 1
            pdfac(1,ndfl) = nint(td(1))   ! Block
            pdfac(2,ndfl) = nint(td(2))   ! Face
            pdfac(3,ndfl) = nint(td(3))   ! B.C. indicator
            ndisp(1,ndfl) = td(4)         ! Normal displacement
            ndisp(8,ndfl) = 1.0d0         ! Normal type
            write(iow,2001) (pdfac(n,ndfl),n=1,3),ndisp(1,ndfl)
            if(pdfac(2,ndfl).gt.ndm) then  ! Negative face displacement
              ndisp(1,ndfl) = -ndisp(1,ndfl)
            endif
          elseif(pcomp(tx,'tang',4)) then
            ndfl          = ndfl + 1
            pdfac(1,ndfl) = nint(td(1))   ! Block
            pdfac(2,ndfl) = nint(td(2))   ! Face
            pdfac(3,ndfl) = nint(td(3))   ! B.C. indicator
            ndisp(1,ndfl) = td(4)         ! Tangential displacement
            ndisp(2,ndfl) = td(5)         ! Tangential direction
            ndisp(8,ndfl) = 2.0d0         ! Tangential type
            ndisp(2,ndf) = max(1,nint(ndisp(2,ndfl)))
            write(iow,2002) (pdfac(n,ndfl),n=1,3),
     &                       ndisp(1,ndfl),nint(ndisp(2,ndfl))
          elseif(pcomp(tx,'user',4)) then
            ndfl          = ndfl + 1
            pdfac(1,ndfl) = nint(td(2))   ! Block
            pdfac(2,ndfl) = nint(td(3))   ! Face
            pdfac(3,ndfl) = nint(td(4))   ! B.C. indicator
            do n = 1,7
              ndisp(n,ndfl) = td(n+4)
            end do ! n
            ndisp(8,ndfl) = 3.0d0         ! User loading
            ndisp(9,ndfl) = nint(td(1))   ! User model number
            write(iow,2003) nint(ndisp(9,ndfl)),(pdfac(n,ndfl),n=1,3),
     &                          (ndisp(n,ndfl),n=1,7)
          endif
        end do ! while

!     Compute nodal displacements for nurb blocks

      elseif(isw.eq.2) then

!       Set quadrature for all surface elements

        if(ndm.eq.2) then
          do n = 1,ndfl
            call pdblk2d(pdfac(1,n),ndisp(1,n),mr(np(308)),mr(np(309)),
     &                   mr(np(299)),mr(np(312)),mr(np(310)))
          end do ! n
        elseif(ndm.eq.3) then
          call int2d(5,lint,sg2)
          do n = 1,ndfl
            call pdblk3d(pdfac(1,n),ndisp(1,n),mr(np(308)),mr(np(309)),
     &                   mr(np(299)),mr(np(312)),mr(np(310)))
          end do ! n
        endif

!     Report approximate and exact boundary conditions

      elseif(isw.eq.3) then

        do n = 1,ndfl
          call pdblk_out(ndisp(1,n))
        end do ! n

!     Error: Incorrect option requested

      else

        write(iow,3000) isw
        call plstop(.true.)

      endif

!     Formats

2001  format(/5x,'NURB Normal Surface Displacement'/
     &       10x,' Block No.    =',i8/
     &       10x,' Face  No.    =',i8/
     &       10x,' Set B.C. (1) =',i8/
     &       10x,' Displacement =',1p,1e12.4)

2002  format(/5x,'NURB Tangential Surface Displacement'/
     &       10x,' Block No.    =',i8/
     &       10x,' Face  No.    =',i8/
     &       10x,' Set B.C. (1) =',i8/
     &       10x,' Displacement =',1p,1e12.4/
     &       10x,' Direction =',i8)

2003  format(/5x,'NURB User Surface Displacement'/
     &       10x,' Model no. =',i8/
     &       10x,' Block No. =',i8/
     &       10x,' Face  No. =',i8/
     &       10x,' Set B.C. (1) =',i8/
     &       10x,' Value 1   =',1p,1e12.4/
     &       10x,' Value 2   =',1p,1e12.4/
     &       10x,' Value 3   =',1p,1e12.4/
     &       10x,' Value 4   =',1p,1e12.4/
     &       10x,' Value 5   =',1p,1e12.4/
     &       10x,' Value 6   =',1p,1e12.4/
     &       10x,' Value 7   =',1p,1e12.4)

3000  format(/5x,'* ERROR * in NURB User Surface Displacement'/
     &       10x,' Option requested =',i3)

      end

      subroutine pdblk2d(pdfac,ndisp,lknot,lside,nsides,nblksd,nblk)

      implicit   none

      include   'iofile.h'

      include   'cnurb.h'
      include   'cdata.h'
      include   'igdata.h'
      include   'qudshp.h'
      include   'sdata.h'

      include   'p_point.h'
      include   'pointer.h'
      include   'comblk.h'

      logical    setval, palloc
      integer    blk,fac,bcset
      integer    sid1,kno1,lek1,ord1,len1
      integer    sid2,kno2,lek2,ord2,len2
      integer    i, ii, l1
      real*8     ndisp(*)
      integer    pdfac(3),pko(2),psides(300)
      integer    lknot(0:4,*), lside(2,*), nsides(dsideig,*)
      integer    nblk(14,*), nblksd(dblokig,*)

      blk   = pdfac(1)
      fac   = pdfac(2)
      bcset = pdfac(3)

      sid1 = nblksd(1,blk)
      kno1 = lside(2,sid1)         ! kside
      lek1 = lknot(1,kno1)
      ord1 = lknot(2,kno1)
      len1 = lside(1,sid1)

      len2 = nblk(4,blk)           ! lblksd(blk)
      sid2 = nblksd(len2+1,blk)
      kno2 = lside(2,sid2)
      lek2 = lknot(1,kno2)
      ord2 = lknot(2,kno2)
      len2 = lside(1,sid2)

!     write(  *,*) 'SIDES:',sid1,sid2,' LEN',len1,len2
!     write(iow,*) 'SIDES:',sid1,sid2,' LEN',len1,len2
!     write(  *,*) 'LENSD',lblksd(blk),nblksd(lblksd(blk),blk),
!    &                                 nblksd(lblksd(blk)+1,blk)
!     write(iow,*) 'LENSD',lblksd(blk),nblksd(lblksd(blk),blk),
!    &                                 nblksd(lblksd(blk)+1,blk)

      if(fac.eq.1 .or. fac.eq.3) then
        lint = ord2 + 1
        call int1d(lint,sg1)
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
        lint = ord1 + 1
        call int1d(lint,sg1)
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
      endif

      if(np(79).ne.0) then
        call psidpos(psides,l1,1,mr(np(79)))
      endif

!     Allocate temporary array for averages

      setval = palloc(120,'TEMP0',numnp,2)

      point = np(27) + nneq
!     N.B. mr(np(79)) not used???
      call pblkdi2d(pko,psides, ndisp, bcset, hr(np(43)),hr(np(263)),
     &              hr(np(44)),hr(np(264)),mr(np(31)+nneq),hr(point),
     &              hr(np(41)),hr(np(120)),mr(np(308)),hr(np(298)))
      setval = palloc(120,'TEMP0',    0,2)

      end

      subroutine pdblk3d(pdfac,ndisp,lknot,lside,nblksd,nsides,nblk)

      implicit   none

      include   'cnurb.h'
      include   'igdata.h'
      include   'sdata.h'

      include   'pointer.h'
      include   'comblk.h'

      integer    blk,fac, nod
      integer    kno1,lek1,ord1,len1
      integer    kno2,lek2,ord2,len2
      integer    kno3,lek3,ord3,len3,sid3
      integer    i,j, ii,jj, l1,l2
      real*8     ndisp(8)
      integer    pdfac(3),pko(2),psides(300,300)
      integer    lknot(0:4,*), lside(2,*), nsides(dsideig,*)
      integer    nblk(14,*), nblksd(dblokig,*)

      blk  = pdfac(1)
      fac  = pdfac(2)

      kno1 = nblk(6,blk)    ! nbk3d(1,blk
      lek1 = lknot(1,kno1)
      ord1 = lknot(2,kno1)
      len1 = lknot(3,kno1)

      kno2 = nblk(7,blk)    ! nbk3d(2,blk
      lek2 = lknot(1,kno2)
      ord2 = lknot(2,kno2)
      len2 = lknot(3,kno2)

      sid3 = nblksd(1,blk)
      kno3 = lside(2,sid3)
      lek3 = lknot(1,kno3)
      ord3 = lknot(2,kno3)
      len3 = lside(1,sid3)

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
      endif

      if(np(79).ne.0) then
        call psidpos(psides,l1,l2,mr(np(79)))
      endif

      call pblkdi3d(pko,psides, ndisp, hr(np(43)),hr(np(263)),
     &              hr(np(44)),hr(np(264)),mr(np(31)+nneq),hr(np(30)),
     &              hr(np(41)))

      end

      subroutine pblkdi2d(pko,psides, ndisp, bcset, x,wt, xl,wl, id,
     &                    f,fl, av, lknot,knots)

      implicit   none

      include   'cdata.h'
      include   'cnurb.h'
      include   'eldata.h'
      include   'igdata.h'
      include   'iofile.h'
      include   'print.h'
      include   'qudshp.h'
      include   'sdata.h'

      include   'pointer.h'
      include   'comblk.h'

      integer    bcset
      integer    pko(2),psides(300), id(ndf,*)
      real*8     ndisp(*),pres,pshp,fnorm, ux,uy
      real*8     x(ndm,*),wt(*),xl(ndm,*),wl(*),f(ndf,*), fl(ndf,*)
      real*8     xx(3), ss(nen,nen), av(*), ff(nen)

      integer    i,j,l, i1, node, dir
      integer    ktlen(2),ktdiv(4,300),ix(64), lknot(0:4,*)
      real*8     knots(dknotig,*)

      call pknotel(knots,lknot,pko, ktlen,ktdiv,1)

      fnorm = ndisp(1)

      if(prt) write(iow,2000) (i,i=1,2)

!     Compute averager

      do i1 = 1,numnp
        av(i1) = 0.0d0
      end do ! i1

      do i1 = 1,ktlen(1)
        do i = ktdiv(3,i1),ktdiv(4,i1)
          node     = psides(i)
          av(node) = av(node) + 1.0d0
        end do ! i
      end do ! i1

      do i1 = 1,numnp
        if(nint(av(i1)).gt.0) then
          av(i1) = 1.0d0/av(i1)
        endif
      end do ! i1

!     Compute over 'elements'

      do i1 = 1,ktlen(1)

!       Compute number of nodes on edge and set connection list
        nel = 0
        do i = ktdiv(3,i1),ktdiv(4,i1)
          nel     = nel + 1
          ix(nel) = psides(i)
        end do ! i

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
          do i = 1,nel
            ss(i,j) = 0.0d0
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

!         Compute least squares matrix

          do j = 1,nel
            pres = shp3(4,j,l)*jac(l)
            do i = 1,nel
              ss(i,j) = ss(i,j) + shp3(4,i,l)*pres
            end do ! i
          end do ! j

!         Accumulate into control point displacements

          if(nint(ndisp(8)).eq.1) then ! Normal displacement
            pres = fnorm*sg1(2,l)
            do j = 1,nel
              pshp = pres*shp3(4,j,l)
              do i = 1,2
                fl(i,j) = fl(i,j) + pshp*dxdxi(i,2)
              end do ! i
            end do ! j
          elseif(nint(ndisp(8)).eq.2) then ! Tangential displacements
            dir  = nint(ndisp(2))
            pres = fnorm*sg1(2,l)*dxdxi(3-dir,1)
            do j = 1,nel
              fl(dir,j) = fl(dir,j) - pres*shp3(4,j,l)
            end do ! j
          elseif(nint(ndisp(8)).eq.3) then ! User  displacements
            do i = 1,2
              xx(i) = 0.0d0
              do j = 1,nel
              xx(i) = xx(i) + xl(i,j)*shp3(4,j,l)
              end do ! j
            end do ! i
            call pnurbdi2d(shp3(1,1,l),xx,jac(l),ndisp,nel,ndf, fl)
          endif
        end do ! l

!       Set nodal boundary conditions

        if(bcset.gt.0) then
          call pdisetbc(ix,nel,id,ndf)
        endif

!       Average 'fl' and invert 'ss'

        call invert(ss,nel,nen)
        do i = 1,ndf
          do j = 1,nel
            ff(j) = 0.0d0
            do l = 1,nel
              ff(j) = ff(j) + ss(j,l)*fl(i,l)
            end do ! l
          end do ! i
          do j = 1,nel
            fl(i,j) = ff(j)*av(ix(j))
          end do ! j
        end do ! i

!       Assemble displacements

        do j = 1,nel
          node = ix(j)
          do i = 1,2
            if(id(i,node).ne.0) then
              f(i,node) = f(i,node) + fl(i,j)
            endif
          end do ! i
        end do ! j

!       Output displacements

        if(prt) then
          do j = 1,nel
            node = ix(j)
            xx(1) = x(1,node)
            xx(2) = x(2,node)
            call pcircle(xx,ndisp, ux,uy, .false.)
            write(iow,2001) node,(id(i,node),fl(i,j),i=1,2),ux,uy
          end do ! j
          write(iow,*) ' ---------------------- '
        endif

      end do ! i1

!     Formats

2000  format(//5x,'NURB   S u r f a c e   D i s p l a c e m e n t s'/
     &         5x,'Node',2(' B.C.',i3,'-Disp',3x))

2001  format(i8,2(i5,1p,1e12.4),1p,2e12.4:)

      end

      subroutine pblkdi3d(pko,psides, ndisp, x,wt, xl,wl, id, f,fl)

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

      integer    pko(2),psides(300,300), id(ndf,*)
      real*8     ndisp(8),pres,pshp,fnorm
      real*8     x(ndm,*),wt(*),xl(ndm,*),wl(*),f(ndf,*), fl(ndf,*)

      integer    i,j,l, i1,j1, node
      integer    ktlen(2),ktdiv(4,300),ix(64)

!     call pknotel(knots,lknot,pko, ktlen,ktdiv,2)
      call pknotel(hr(np(298)),mr(np(308)),pko, ktlen,ktdiv,2)

      fnorm = ndisp(1)

      if(prt) write(iow,2000) (i,i=1,3)

      do j1 = ktlen(1)+1,ktlen(2)
        do i1 = 1,ktlen(1)
          nel = 0
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
     &                      mr(np(308)),mr(np(311)),
     &                      hr(np(298)), ndm, .true.)

!           Accumulate into control point forces

            pres = fnorm*sg2(3,l)
            do j = 1,nel
              pshp = pres*shp2(3,j,l)
              do i = 1,3
                fl(i,j) = fl(i,j) + pshp*dxdxi(i,3)
              end do ! i
            end do ! j

          end do ! l

!         Assemble displacements

          do j = 1,nel
            node = ix(j)
            do i = 1,3
              if(id(i,node).eq.0) then
                f(i,node) = f(i,node) + fl(i,j)
              endif
            end do ! i
          end do ! j

!         Output displacements

          if(prt) then
            do j = 1,nel
              node = ix(j)
              write(iow,2001) node,(id(i,node),fl(i,j),i=1,3)
            end do ! j
          endif

        end do ! i1
      end do ! j1

!     Formats

2000  format(//5x,'NURB   S u r f a c e   L o a d s'/
     &         5x,'Node',3(' B.C.',i3,'-Force',3x))

2001  format(i8,6(i5,1p,1e12.4:)/(8x,6(i5,1p,1e12.4:)))

      end

      subroutine pdisetbc(ix,nel,id,ndf)

      implicit   none

      integer    nel,ndf, i,n
      integer    ix(nel),id(ndf,*)

!     Set all dof's to fixed

      do n = 1,nel
        if(ix(n).gt.0) then
          do i = 1,ndf
            id(i,ix(n)) = 1
          end do ! i
        endif
      end do ! n

      end
