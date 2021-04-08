!$Id:$
      subroutine umesh1(tx,prt)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Compute nodal loads for NURB edge

!      Inputs:
!         tx(*)  - Command line input data
!         prt    - Flag, output results if true

!      Outputs:
!         Nodal forces
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'umac1.h'

      include  'pointer.h'
      include  'comblk.h'

      logical   pcomp,prt
      character tx(*)*15

      save

!     Set command

      if(pcomp(uct,'mes1',4)) then      ! Usual    form
        uct = 'nsur'                    ! Specify 'nsur'face loading
      elseif(ucount) then               ! Count elements and nodes

      elseif(urest.eq.1) then           ! Read  restart data

      elseif(urest.eq.2) then           ! Write restart data

      else                              ! Perform user operation

        call u1nsurf(prt,
     &               hr(np(298)),mr(np(299)),mr(np(308)),mr(np(309)))
      endif

      end

      subroutine u1nsurf(prt, knots,nsides,lknot,lside)

      implicit  none

      include  'bdata.h'
      include  'cnurb.h'
      include  'igdata.h'
      include  'iofile.h'
      include  'pconstant.h'
      include  'p_int.h'
      include  'qudshp.h'
      include  'sdata.h'
      include  'umac1.h'

      include  'pointer.h'
      include  'comblk.h'

      logical   pcomp,prt, errck, tinput, pinput, solve
      character txt(2)*15, lab1*6, lab2a(2)*6, lab2b(2)*6

      integer   i,ii,j,jj,j2,l,ll, side1,kno,len,ord,lek
      real*8    u1,u2, qq
      real*8    xl(ndm,240),wt(240), td(16),xp(ndm,3),pp(3),pu(2),uu(2)
      real*8    ff(ndf,240),fl(240),sh(240),ss(2,30),dx(3), xx(3)
      real*8    knots(dknotig,*),sg(2,30)
      integer   nsides(dsideig,*),lknot(0:4,*),lside(2,*)

      save

      data      lab1 / '-Coord' /
      data      lab2a/ 'Pr'    , '  ' /
      data      lab2b/ 'essure', 'Force ' /

!     Input side number and type

      txt(1) = 'start'
      do while(.not.pcomp(txt(1),'    ',4))
        errck = tinput(txt,2,td,1)

        side1 = nint(td(1))
        len   = lside(1,side1)
        kno   = lside(2,side1)
        ord   = lknot(2,kno)

!       Input span and load intensities

        if(pcomp(txt,'side',4)) then
          if(pcomp(txt(2),'poin',4)) then       ! Point load on line
            jj = 1
            j2 = 2
          elseif(pcomp(txt(2),'line',4)) then   ! Linear load on line
            jj = 2
            j2 = 1
          elseif(pcomp(txt(2),'quad',4)) then   ! Quadr load on line
            jj = 3
            j2 = 1
          endif
          write(iow,2000) head,(char(48+j),lab1,j=1,ndm),
     &                    lab2a(j2),lab2b(j2)
          if(jj.eq.1) then
            errck = pinput(td,ndm+ndf+1)
            ii = nint(td(1))
            do i = 1,ndm
              xp(i,ii) = td(i+1)
            end do
            write(iow,2002) ii,(xp(i,ii),i=1,ndm),
     &                         (td(ndm+1+i),i=1,ndf)
          else
            do j = 1,jj
              errck = pinput(td,ndm+2)
              ii = max(1,min(jj,nint(td(1))))
              do i = 1,ndm
                xp(i,ii) = td(i+1)
              end do
              pp(ii) = td(ndm+2)
              write(iow,2001) ii,(xp(i,ii),i=1,ndm),pp(ii)
            end do ! j
            if(jj.eq.2) then
              do i = 1,ndm
                xp(i,3) = 0.5d0*(xp(i,1) + xp(i,2))
              end do
              pp(3) = 0.5d0*(pp(1) + pp(2))
              write(iow,2001) 3,(xp(i,3),i=1,ndm),pp(3)
            endif
          endif ! jj

!         Set control point vector and weight

          call psetxlwt(nsides(1,side1) ,hr(np(43)),hr(np(263)),
     &                  xl,wt, len,ndm)

!         Initialize load vector

          do j = 1,len
            do i = 1,ndf
              ff(i,j) = 0.0d0
            end do ! i
          end do ! j

!         Point load case

          if(jj.eq.1) then

            lek = len + ord + 1
            call ptinvert(xp(1,j),xl,wt,knots(1,kno),
     &                    ord,lek,ndm, pu(1))

!           Locate knot spans to apply load to

            u1 = pu(1)
            ll = -1
            do jj = 1,lek
              if(knots(jj+1,kno).gt.knots(jj,kno)) then
                ll    = ll + 1
                solve = .false.
                uu(1) = 0.0d0
                uu(2) = 0.0d0
                if(u1.ge.knots(jj  ,kno) .and.
     &             u1.lt.knots(jj+1,kno)) then
                  uu(1) = knots(jj  ,kno)
                  uu(2) = knots(jj+1,kno)
                  solve = .true.
                else
                  solve = .false.
                endif
              endif

!             Compute load values betwen uu(1) and uu(2)

              if(solve) then

                sg1(1,l) = u1
                sg1(2,l) = 0.0d0

                call nurb_sh1(sg1(1,l),jj,knots(1,kno),ord,lek,
     &                   xl(1,ll+1),wt(ll+1),shp1,jac(1),dx,ndm)

!               Accumulate loads

                j2 = ndm+1
                do j = 1,ord+1
                  do i = 1,ndf
                    ff(i,j+ll) = ff(i,j+ll) + shp1(2,j,1)*td(i+j2)
                  end do ! i
                end do ! j
              endif ! solve

            end do ! jj

!         Line traction case

          elseif(jj.gt.1) then

!           Find end points of traction on knot vector

            lek = len + ord + 1
            do j = 1,2
              call ptinvert(xp(1,j),xl,wt,knots(1,kno),
     &                      ord,lek,ndm, pu(j))
            end do ! j

            call mprint(pu,1,2,1,'PU_nsurf')

!           Locate knot spans to apply load to

            u1 = min(pu(1),pu(2))
            u2 = max(pu(1),pu(2))
            ll = -1
            do jj = 1,lek
              if(knots(jj+1,kno).gt.knots(jj,kno)) then
                ll    = ll + 1
                solve = .false.
                if(    u1.le.knots(jj  ,kno) .and.
     &                 u2.ge.knots(jj+1,kno)) then
                  uu(1) = knots(jj  ,kno)
                  uu(2) = knots(jj+1,kno)
                  solve = .true.
                elseif(u1.gt.knots(jj  ,kno) .and.
     &                 u1.lt.knots(jj+1,kno) .and.
     &                 u2.ge.knots(jj+1,kno)) then
                  uu(1) = u1
                  uu(2) = knots(jj+1,kno)
                  solve = .true.
                elseif(u1.le.knots(jj  ,kno) .and.
     &                 u2.gt.knots(jj  ,kno) .and.
     &                 u2.le.knots(jj+1,kno)) then
                  uu(1) = knots(jj  ,kno)
                  uu(2) = u2
                  solve = .true.
                elseif(u1.gt.knots(jj  ,kno) .and.
     &                 u2.lt.knots(jj+1,kno)) then
                  uu(1) = u1
                  uu(2) = u2
                  solve = .true.
                endif
                if(uu(1).ge.uu(2)) then
                  solve = .false.
                endif

!               Compute load values betwen uu(1) and uu(2)

                if(solve) then

                  lint = ord + 1
                  call int1d(lint,sg)
                  ss(1,1:lint) = sg(1,1:lint)
                  ss(2,1:lint) = sg(2,1:lint)

                  do j = 1,len
                    fl(j) = 0.0d0
                  end do ! j

                  call mprint(ss,2,lint,2,'SS_nsurf')

                  do l = 1,lint
                    sh(1) = 0.5d0*(1.d0 - ss(1,l))
                    sh(2) = 0.5d0*(1.d0 + ss(1,l))

                    sg1(1,l) = sh(1)*uu(1) + sh(2)*uu(2)
                    sg1(2,l) = ss(2,l)*(uu(2) - uu(1))

                    call nurb_sh1(sg1(1,l),jj,knots(1,kno),ord,lek,
     &                       xl(1,ll+1),wt(ll+1),shp1,jac(1),dx,ndm)
                    jac(1) = jac(1)*sg1(2,l)

!                   Compute coordinate for load point

                    do i = 1,ndm
                      xx(i) = 0.0d0
                      do j = 1,ord+1
                        xx(i) = xx(i) + shp1(2,j,1)*xl(i,j+ll)
                      end do ! j
                    end do ! i

                    call mprint(xx,1,ndm,1,'XX_nsurf')
                    call mprint(dx,1,ndm,1,'DX_nsurf')

!                   Get location on load curve and load intensity

                    call pgetqq(xx,xp,pp, ndm, qq)

                    call mprint(qq,1,  1,1,'QQ_nsurf')
                    call mprint(jac(1),1,  1,1,'JAC_nsurf')

!                   Accumulate loading

                    qq = qq*jac(1)
                    do j = 1,ord+1
                      fl(j+ll) = fl(j+ll) + shp1(2,j,1)*qq
                    end do ! j
                  end do ! l

!                 Transform to normal loading

                  if(ndm.eq.1) then
                    do j = 1,len
                      ff(1,j) = ff(1,j) + fl(j)
                    end do ! j
                  elseif(ndm.ge.2) then
                    do j = 1,len
                      ff(1,j) = ff(1,j) - fl(j)*dx(2)
                      ff(2,j) = ff(2,j) + fl(j)*dx(1)
                    end do ! j
                  endif
                endif
              endif
            end do ! jj

          endif

!         Assemble load vector

          fp(1) = np(27)
          call pnsetfor(ff,hr(fp(1)),nsides(1,side1),ndf,len,prt)
        endif

      end do ! while

!     Format

2000  format(/1x,20a4//'   N U R B    S u r f a c e   L o a d s'//
     &        9x,'Node',5x,5(a2,a6,4x))
2001  format(i12,2x,1p,5e12.4)
2002  format(i12,2x,1p,6e12.4/(14x,6e12.4))

      end

      subroutine pnsetfor(ff,f,nsides,ndf,len,prt)

!-----[--+---------+---------+---------+---------+---------+---------+-]
!     Purpose: Assemble load values into nodal load vector 'f'

!     Inputs:
!       ff(ndf,*)  - Local load vector
!       nsides(*)  - Control point number of load side
!       ndf        - DOF's of load vector
!       len        - Length of local load side
!       prt        - Print flag

!     Outputs:
!       f(ndf,*)   - Nodal loads on control points
!-----[--+---------+---------+---------+---------+---------+---------+-]
      implicit   none

      include   'iofile.h'

      logical    prt
      integer    ndf,len, l,j, nsides(*)
      real*8     ff(ndf,*),f(ndf,*)

!     Output and store nodal forces

      if(prt) write(iow,2000) (j,j=1,ndf)
      do l = 1,len
        do j = 1,ndf
          f(j,nsides(l)) = ff(j,l)
        end do ! j
        if(prt) write(iow,2001) nsides(l),(ff(j,l),j=1,ndf)
      end do ! l

!     Formats

2000  format(/5x,'Nodal Forces'//6x,'Node',6(i5,'-Force')/
     &      (10x,6(i5,'-Force')))

2001  format(i10,1p,6e11.3:/(10x,1p,6e11.4))

      end

      subroutine pgetqq(xx,xp,pp, ndm, qq)

!-----[--+---------+---------+---------+---------+---------+---------+-]
!     Purpose: Compute load intensity at location 'xx'

!     Inputs:
!        xx(ndm)   - Coordinate of load point
!        xp(ndm,3) - Nodal positions of load function
!        pp(3)     - Load intensity at node points
!        ndm       - Space dimension of load

!     Outputs:
!        qq        - Load intensity at point closest to 'xx'
!-----[--+---------+---------+---------+---------+---------+---------+-]
      implicit   none

      logical    noconv
      integer    ndm, its, nits, i
      real*8     qq, xi, dx, rr,kk, tol
      real*8     xx(3),xp(ndm,*),pp(*), shp(0:2,3), ff(3),df(3),dd(3)

      data       nits / 10 /
      data       tol  / 1.d-10 /

      do i = ndm+1,3
        ff(i) = 0.0d0
        df(i) = 0.0d0
        dd(i) = 0.0d0
      end do ! i

      shp(2,1) =  1.0d0
      shp(2,2) =  1.0d0
      shp(2,3) = -2.0d0

      xi     = 0.0d0
      its    = 0
      noconv = .true.
      do while(noconv .and. its.le.nits)

        its = its + 1

        shp(0,1) = 0.5d0*xi*(xi - 1.0d0)
        shp(0,2) = 0.5d0*xi*(xi + 1.0d0)
        shp(0,3) = 1.0d0 - xi*xi

        shp(1,1) = xi - 0.5d0
        shp(1,2) = xi + 0.5d0
        shp(1,3) = -2.0d0*xi

        do i = 1,ndm
          ff(i) = shp(0,1)*xp(i,1) + shp(0,2)*xp(i,2) + shp(0,3)*xp(i,3)
     &          - xx(i)
          df(i) = shp(1,1)*xp(i,1) + shp(1,2)*xp(i,2) + shp(1,3)*xp(i,3)
          dd(i) = shp(2,1)*xp(i,1) + shp(2,2)*xp(i,2) + shp(2,3)*xp(i,3)
        end do ! i

        rr = ff(1)*df(1) + ff(2)*df(2) + ff(3)*df(3)
        kk = df(1)*df(1) + df(2)*df(2) + df(3)*df(3)
     &     + ff(1)*dd(1) + ff(2)*dd(2) + ff(3)*dd(3)
        dx = -rr/kk
        xi = xi + dx
        if(abs(dx) .lt. tol) then
          noconv = .false.
        endif
      end do ! while

!     Compute load intensity at closest point

      qq = shp(0,1)*pp(1) + shp(0,2)*pp(2) + shp(0,3)*pp(3)

      end
