!$Id:$
      subroutine pesurf(id,ix,ip,ep,xin,x,f,ang,ndf,ndm,nen,nen1,
     &                  numnp,numel,prt,prth,isw)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Input surface loads or displacements from specified
!               coordinates and pressures.  Surface b.c. from
!               specified coordinates and pattern

!      Inputs:
!         ix(nen1,*)  - Element nodal connection data
!         x(ndm,*)    - Nodal coordinates of mesh
!         ndf         - Number dof/node
!         ndm         - Spatial dimension of mesh
!         nen         - Maximum number of nodes/element
!         nen1        - Dimension of ix array
!         numnp       - Number of nodes in mesh
!         numel       - Number of elements in mesh
!         prt         - Output results if true
!         prth        - Output title/header data if true
!         isw         - Switch controling type of data to generate
!                       1 = Surface load/displ values
!                       2 = Boundary conditions
!                       3 = Angle (sloping)
!                       4 = Displacement conditions
!                       5 = Force conditions
!                       6 = Proportional load number conditions

!      Scratch:
!         ip(*)       - Nodal integer list storage
!         ep(numel,*) - Element list storage
!         xin(*)      - Nodal real list storage

!      Outputs:
!         id(ndf,*)   - Nodal boundary restraint conditions
!         f(ndf,*)    - Nodal force and boundary values
!         ang(*)      - Angles for sloping boundary nodes
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'comfil.h'
      include  'conval.h'
      include  'cornum.h'
      include  'iodata.h'
      include  'iofile.h'
      include  'ioincl.h'

      include  'p_point.h'
      include  'pointer.h'
      include  'comblk.h'

      character (len=128) :: fnam
      character (len=15)  :: ptype
      character (len=4)   :: fext, wd(14), vtype

      logical       :: prt,prth,errck,pinput,tinput,pcomp,lsave, polfl
      logical       :: setmem,palloc, oprt,oprth, wdflg, axifl
      integer       :: wdlist,fnorm,ddof
      integer       :: ndf,ndm,nen,nen1,numnp,numel,nipt,isw
      integer       :: i,i1,i2,i3,j,m,n,nn,nel
      integer       :: iosave,ntyp,ntot,numprt,l,lint
      real (kind=8) :: cn,sn, tol0,tol,tolxi
      real (kind=8) :: ff,df,xi, d,gap0, ra
      real (kind=8) :: ximin,ximax,xia,xib
      real (kind=8) :: yc, xi1,xi2, zeta,zeta1,zeta2

      integer   id(ndf,numnp,*),ix(nen1,*),ip(*),ep(numel,*)
      integer   nend(2,2)
      real (kind=8) :: xin(*),x(ndm,*),f(ndf,numnp,*),ang(*)
      real (kind=8) :: xe(2,3),xl(2,3), xp(2),x0(3),pl(30)
      real (kind=8) :: shp(2,3),sg(2,3), fl(2,3), td(5)
      real (kind=8) :: xld(2),xqd(2)

      save

      data      tol0 / 1.0d-5/, tolxi / 1.0d-8 /

      data      wdlist /14/
      data      wd / 'gap ', 'node', 'line', 'quad', 'pola', 'cart',
     &               'disp', 'tang', 'norm', 'surf', 'segm', 'dofs',
     &               'axis', 'plan' /

!     Coordinate surface loading, boundary condition, and angle inputs
      if     (isw.eq.1) then
        ntot = nsurf - 1
      else if(isw.eq.2) then
        ntot = nbouf - 1
      else if(isw.eq.3) then
        ntot = nangf - 1
      else if(isw.eq.4) then
        ntot = ndisf - 1
      else if(isw.eq.5) then
        ntot = nforf - 1
      else if(isw.eq.6) then
        ntot = nprof - 1
      else
        ntot = -1
      end if

      do nn = 0,ntot

!       Default to normal loading
        fnorm  = 1
        numprt = 0

!       Set file extender and number of input items
        if(isw.eq.1) then
          fext = 'sl0'
          ntyp = max(3,ndm+ndf)
        elseif(isw.eq.2) then
          fext = 'bn0'
          ntyp = ndm+ndf
        elseif(isw.eq.3) then
          fext = 'an0'
          ntyp = ndm+1
        elseif(isw.eq.4) then
          fext = 'ds0'
          ntyp = ndm+ndf
        elseif(isw.eq.5) then
          fext = 'fr0'
          ntyp = ndm+ndf
        elseif(isw.eq.6) then
          fext = 'yp0'
          ntyp = ndm+ndf
        endif

!       Check for file numbers
        if(nn.le.9) then
          write(fext(3:3),'(i1)') nn
        elseif(nn.le.99) then
          write(fext(2:3),'(i2)') nn
        endif
        fnam = fsav
        call addext(fnam,fext,18,4)
        call opnfil(fext,fnam,-2,ios,lsave)
        if(lsave) then
          iosave = ior
          ior    = ios

          oprt   = prt
          oprth  = prth
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

        gap0  = 0.0d0
        polfl = .false.
        axifl = .false.

!       Input type and pressure on surface
1       ptype = ' '
        errck = tinput(ptype,1,pl,ntyp)
        if(errck) then
          write(iow,3003)
          call plstop(.true.)
        endif

!       Check for legal data
        wdflg = .false.
        do i = 1,wdlist
          if(pcomp(ptype,wd(i),4)) wdflg = .true.
        end do

!       Delete file and go to next list
        if(.not.wdflg) then
          close(ior,status = 'delete')
          ior = iosave
          go to 300
        elseif( pcomp(ptype,'norm',4)) then
          fnorm = 1
          go to 1
        elseif( pcomp(ptype,'tang',4)) then
          fnorm = 2
          go to 1
        elseif( pcomp(ptype,'axis',4)) then
          axifl = .true.
          go to 1
        elseif( pcomp(ptype,'plan',4)) then
          axifl = .false.
          go to 1
        elseif( pcomp(ptype,'disp',4) .or. pcomp(ptype,'dofs',4)) then
          ddof  = max(1,min(ndf,nint(pl(1))))
          fnorm = 3
          go to 1
        elseif( pcomp(ptype,'node',4)) then
          point = np(190)
          if(isw.eq.1 .or. isw.eq. 5) then
            call pfboun(pl,x,f(1,1,1),mr(point),ndm,ndf,numnp,numprt,
     &                  vtype,prt,prth)
          elseif(isw.eq.2) then
            call pcboun(pl,x,id(1,1,2),mr(point),ndm,ndf,numnp,numprt,
     &                  gap0,vtype,prt,prth,'-B.C.')
          elseif(isw.eq.3) then
            call paboun(pl,x,ang,mr(point),ndm,numnp,numprt,
     &                  prt,prth)
          elseif(isw.eq.4) then
            call pcdisp(pl,x,f(1,1,2),mr(point),ndm,ndf,numnp,numprt,
     &                  gap0,prt,prth)
          elseif(isw.eq.6) then
            call pcprop(pl,x,mr(np(29)),mr(point),ndm,ndf,numnp,numprt,
     &                  prt,prth)
          endif
          go to 1
        elseif( pcomp(ptype,'surf',4)) then
          setmem = palloc(113,'TEMP3',numnp,1)
          call prj3dl(pl,ix,x,x0,id(1,1,2),mr(np(32)),f,mr(np(113)),
     &                hr(np(45)),gap0,nen,nen1,ndm,ndf,numnp,numel,
     &                prt,prth,fnorm,polfl,ddof,isw)
          setmem = palloc(113,'TEMP3',0,1)
          go to 1
        elseif( pcomp(ptype,'gap', 3)) then
          gap0 = pl(1)
          if(prt) then
            write(iow,*) '  GAP =',gap0
            if(ior.lt.0) then
              write(*,*) '  GAP =',gap0
            endif
          endif
          go to 1
        elseif( pcomp(ptype,'pola',4)) then
          polfl = .true.
          do i = 1,3
            x0(i) = pl(i)
          end do
          go to 1
        elseif( pcomp(ptype,'cart',4)) then
          polfl = .false.
          go to 1
        elseif( pcomp(ptype,'segm',4)) then
          i  = 1
          i1 = 0
          do while (i.ne.0)
            errck = pinput(td,5)
            if(errck) then
              backspace ior
              i = 0
            else
              i  = max(0,nint(td(1)))
              i1 = max(i,i1)
              if(i.gt.3) then
                write(iow,3001) i
                call plstop(.true.)
              elseif(i.gt.0) then
                xl(1,i) = td(2)
                xl(2,i) = td(3)
                if(isw.ne.2) pl(i)   = td(4)
              endif
            endif
          end do ! while

!         For linear segments average to get mid value/point
          if(i1.eq.2) then
            xl(1,3) = 0.5d0*(xl(1,1) + xl(1,2))
            xl(2,3) = 0.5d0*(xl(2,1) + xl(2,2))
            if(isw.ne.2) pl(3)   = 0.5d0*(pl(1) + pl(2))
          endif

!       Quadratic segment
        elseif( pcomp(ptype,'quad',4)) then
          do i = 1,3
            errck = pinput(td,4)
            i1 = nint(td(1))
            xl(1,i1) = td(2)
            xl(2,i1) = td(3)
            if(isw.ne.2) pl(i1)   = td(4)
          end do

!       Linear segment
        elseif( pcomp(ptype,'line',4)) then
          do i = 1,2
            errck = pinput(td,4)
            i1 = nint(td(1))
            xl(1,i1) = td(2)
            xl(2,i1) = td(3)
            if(isw.ne.2) pl(i1)   = td(4)
          end do
          xl(1,3) = 0.5d0*(xl(1,1) + xl(1,2))
          xl(2,3) = 0.5d0*(xl(2,1) + xl(2,2))
          if(isw.ne.2) pl(3) = 0.5d0*(pl(1) + pl(2))
        endif

!       Output data
        if(prt) then
          if(isw.eq.1 .or. isw.eq.5) then
            if(fnorm.le.2) then
              call prtitl(prth)
              write(iow,2000) xl,(pl(i),i=1,3)
              if(ior.lt.0) then
                write(*,2000) xl,(pl(i),i=1,3)
              end if
            elseif(fnorm.eq.3) then
              call prtitl(prth)
              write(iow,2030) xl,(pl(i),i=1,3)
              if(ior.lt.0) then
                write(*,2030) xl,(pl(i),i=1,3)
              end if
            end if
          elseif(isw.eq.2) then
            call prtitl(prth)
            write(iow,2010) xl,(i,i=1,ndf)
            write(iow,2011) (nint(pl(i)),i=1,ndf)
            if(ior.lt.0) then
              write(*,2010) xl,(i,i=1,ndf)
              write(*,2011) (nint(pl(i)),i=1,ndf)
            end if
          elseif(isw.eq.3) then
            call prtitl(prth)
            write(iow,2020) xl,(pl(i),i=1,3)
            if(ior.lt.0) then
              write(*,2020) xl,(pl(i),i=1,3)
            end if
          elseif(isw.eq.4) then
            call prtitl(prth)
            write(iow,2030) xl,(pl(i),i=1,3)
            if(ior.lt.0) then
              write(*,2030) xl,(pl(i),i=1,3)
            end if
          elseif(isw.eq.6) then
            call prtitl(prth)
            write(iow,2040) xl,(i,i=1,ndf)
            write(iow,2041) (nint(pl(i)),i=1,ndf)
            if(ior.lt.0) then
              write(*,2040) xl,(i,i=1,ndf)
              write(*,2041) (nint(pl(i)),i=1,ndf)
            end if
          endif
        endif

!       Projections nodes to points
        errck = .false.
        tol   =  tol0

100     call prj2dl(gap0,tol0,tol,x0,ip,xin,x,xl,ndm,numnp,polfl)

!       Check if end points found
        ximin = +1.d0
        ximax = -1.d0
        do n = 1,numnp
          if(xin(n).ne.0.0d0) then
            ximin = min(ximin,abs(xin(n)))
            ximax = max(ximax,abs(xin(n)))
          end if
        end do

!       For cylindrical coordinates check number of 1 and -1 values
        if(polfl) then
          i1 = 0
          i2 = 0
          do n = 1,numnp
            if(xin(n).ge.  1.d0 - tol0) then
              i1 = i1 + 1
            endif
            if(xin(n).le. -1.d0 + tol0) then
              i2 = i2 + 1
            endif
          end do ! n
          if(i1.ge.2) then
            ximin = -1.d0
          endif
          if(i2.ge.2) then
            ximax =  1.d0
          endif
        endif

        tol = tol0 + max(1.d0+ximin,1.d0-ximax,0.0d0)

        errck = .not.errck
        errck = .false.
        if(errck .and. tol.gt.tol0+tolxi) go to 100

!       Loop through elements to find adjacent nodes
        do n = 1,numel
          ep(n,1) = 0
          ep(n,2) = 0
          do j = 1,min(4,nen)
            i1 = ix(j,n)
            if(i1.ne.0) then
              if(ip(i1).ne.0) then
                if(j.lt.min(4,nen)) then
                  i2 = ix(j+1,n)
                  if(i2.eq.0) then
                    i2 = ix(1,n)
                  endif
                else
                  i2 = ix(1,n)
                endif
                if(ip(i2).gt.0 .and. i1.ne.i2) then
!                 if(ep(n,1).le.0 .and. ep(n,2).le.0 ) then
                  if(ep(n,1).le.0 .and. ep(n,2).le.0 .and.
     &              (min(abs(xin(i1)),abs(xin(i2))).le.1.d0+tolxi)) then
                    ep(n,1) = i1
                    ep(n,2) = i2
                    if(j+4.le.nen) then
                      i3 = ix(j+4,n)
                      if(i3.gt.0) then
                        if(ip(i3).gt.0) ep(n,3) = i3
                      endif
                    endif
                  endif
                endif
              endif
            endif
          end do ! j
        end do ! n

!       Remove duplicates
        do n = 1,numel-1
          if(ep(n,1).gt.0) then
            do m = n+1,numel
              if(ep(m,1).eq.ep(n,1) .and. ep(m,2).eq.ep(n,2) .or.
     &           ep(m,1).eq.ep(n,2) .and. ep(m,2).eq.ep(n,1)) then
                ep(m,1) = 0
                ep(m,2) = 0
                ep(m,3) = 0
              end if
            end do
          end if
        end do

!       Check for polar projections: Possible that angle 0 and 360 both
!       project to 'xi' of -1.0d0
        if(polfl) then
          do n = 1,numel
            if(ep(n,1).gt.0) then
              xia = xin(ep(n,1))
              xib = xin(ep(n,2))
              if(xib.lt.xia) then
                if(abs(xib).ge.1.d0-tol0) then
                  xin(ep(n,2)) = abs(xib)
                else
                  write(iow,3002) n,ep(n,1),ep(n,2)
                  if(ior.lt.0) then
                    write(*,3002) n,ep(n,1),ep(n,2)
                  endif
                endif
              endif
            endif
          end do ! n
        endif

!       Compute nodal forces from surface pressures
        if(isw.eq.1 .and. fnorm.le.2) then

!         Find end points
          do n = 1,numnp
            ip(n) = 0
          end do

          xld(1) = 0.5d0*(xl(1,2) - xl(1,1))
          xld(2) = 0.5d0*(xl(2,2) - xl(2,1))
          xqd(1) = xl(1,1) + xl(1,2) - 2.d0*xl(1,3)
          xqd(2) = xl(2,1) + xl(2,2) - 2.d0*xl(2,3)
          do n = 1,numel
            if(ep(n,1).gt.0) then

!             Order segments along master direction
              if(polfl) then
                yc = xl(2,3) + xin(ep(n,1))*(xld(2)
     &                       + 0.5d0*xin(ep(n,1))*xqd(2))
                if(xl(2,2).gt.xl(2,1)) then
                  call pdegree(yc, cn,sn)
                  cn = -cn
                else
                  call pdegree(yc, cn,sn)
                  sn = -sn
                endif
              else
                cn = xld(1) + xin(ep(n,1))*xqd(1)
                sn = xld(2) + xin(ep(n,1))*xqd(2)
              endif

              d = (x(1,ep(n,2)) - x(1,ep(n,1)))*cn
     &          + (x(2,ep(n,2)) - x(2,ep(n,1)))*sn

              if(d.lt.0.0d0) then
                ep(n,1) = 0
                ep(n,2) = 0
                ep(n,3) = 0
              end if

!             Count occurances of node
              if(ep(n,1).gt.0) ip(ep(n,1)) = ip(ep(n,1)) + 1
              if(ep(n,2).gt.0) ip(ep(n,2)) = ip(ep(n,2)) + 1
            end if
          end do

!         Set end point array
          j         = 0
          nend(1,1) = 0
          nend(2,1) = 0
          do n = 1,numnp
            if(ip(n).eq.1) then
              j         = j + 1
              nend(j,1) = n
            end if
          end do

!         Check for error
          if(nend(1,1).eq.0 .or. nend(2,1).eq.0) then
            write(iow,*) '  *ERROR* No surface located'
            if(ior.lt.0) then
              write(*,*) '  *ERROR* No surface located'
            endif
          endif

!         Compute nodal forces for pressures
          if(prt) then
            write(iow,2001)
            if(ior.lt.0) write(*,2001)
          endif

          do n = 1,numel

            i1 = ep(n,1)

            if(i1.gt.0) then

              i2     = ep(n,2)
              xia    = xin(i1)
              xib    = xin(i2)

!             Numerically integrated edge loading

!             Set order
              if(ep(n,3).ne.0) then
                nel = 3
              else
                nel = 2
              endif

!             Compute end values for each coordinate
              do i = 1,nel
                xe(1,i) = x(1,ep(n,i))
                xe(2,i) = x(2,ep(n,i))
                fl(1,i) = 0.0d0
                fl(2,i) = 0.0d0
              end do

              zeta1 = xia
              zeta2 = xib

              xi1 = max(-1.d0, -(2.d0+zeta1+zeta2)/(zeta2-zeta1))
              xi2 = min( 1.d0,  (2.d0-zeta1-zeta2)/(zeta2-zeta1))

              df  = 0.5d0*(xi2 - xi1)

              lint = nel
              call int1d(lint,sg)
              do l = 1,lint

!               Coordinates for facet and interpolation surface
                xi   = 0.5d0*((1.d0 - sg(1,l))*xi1
     &                      + (1.d0 + sg(1,l))*xi2)
                zeta = 0.5d0*((1.d0 - xi)*zeta1 + (1.d0 + xi)*zeta2)

!               Magnitude of loading
                ff    = (pl(1)*0.5d0*zeta*(zeta-1.d0)
     &                +  pl(2)*0.5d0*zeta*(zeta+1.d0)
     &                +  pl(3)*(1.d0-zeta*zeta))*sg(2,l)*df

!               Shape functions for facet
                call shap1d(xi,nel,shp)

!               Tangent vector to facet
                xp(1) = shp(1,1)*xe(1,1)
                xp(2) = shp(1,1)*xe(2,1)
                do i = 2,nel
                  xp(1) = xp(1) + shp(1,i)*xe(1,i)
                  xp(2) = xp(2) + shp(1,i)*xe(2,i)
                end do ! i

!               Axisymmetric coordinates
                if(axifl) then
                  ra = 0.0d0
                  do i = 1,nel
                    ra = ra + shp(2,i)*xe(1,i)
                  end do ! i
                  ff = ff*ra  ! multiply load intensity by radius
                endif

!               Tangential loading values
                do i = 1,nel
                  fl(1,i) = fl(1,i) + shp(2,i)*xp(1)*ff
                  fl(2,i) = fl(2,i) + shp(2,i)*xp(2)*ff
                end do ! i
              end do ! l

!             Transform for normal loading
              if(fnorm.eq.1) then
                do i = 1,3
                  ff      =  fl(1,i)
                  fl(1,i) =  fl(2,i)
                  fl(2,i) = -ff
                end do
              endif

!             Perform outputs and add to nodal forces
              do i = 1,nel
                i1 = ep(n,i)

                if(prt) then
                  write(iow,2002) i1,fl(1,i),fl(2,i)
                  if(ior.lt.0) then
                    write(*,2002) i1,fl(1,i),fl(2,i)
                  endif
                endif

!               Sloping boundary conditions
                if(ang(i1).ne.0.0d0) then
                  call pdegree(ang(i1), sn,cn)
                  ff      =  cn*fl(1,i) + sn*fl(2,i)
                  fl(2,i) = -sn*fl(1,i) + cn*fl(2,i)
                  fl(1,i) =  ff
                endif

!               Add to nodal forces
                f(1,i1,1) =  f(1,i1,1) + fl(1,i)
                f(2,i1,1) =  f(2,i1,1) + fl(2,i)
              end do

            end if

          end do

!       Other options
        else

          do n = 1,numnp
            ip(n) = 0
          end do

          do n = 1,numel
            if(ep(n,1).gt.0) then
              ip(ep(n,1)) = 1
              ip(ep(n,2)) = 1
              if(ep(n,3).gt.0) then
                ip(ep(n,3)) = 1
              endif
            end if
          end do

!         Set specified displacements on line
          if(isw.eq.1 .and. fnorm.eq.3) then

            if(prt) then
              write(iow,2031)
              if(ior.lt.0) write(*,2031)
            end if

            nipt = 0
            do n = 1,numnp
              if(ip(n).gt.0) then
                nipt   = nipt + 1
                xia    = xin(n)
                f(ddof,n,2) = pl(1)*0.5d0*xia*(xia-1.d0)
     &                      + pl(3)*(1.d0-xia*xia)
     &                      + pl(2)*0.5d0*xia*(xia+1.d0)
                if(prt) then
                  write(iow,2032) n,ddof,f(ddof,n,2)
                  if(ior.lt.0) then
                    write(*,2032) n,ddof,f(ddof,n,2)
                  endif
                endif

              endif
            end do ! n

            if(nipt.eq.0) then ! No data found
              write(  *,3004) 'CDISpl'
              write(iow,3004) 'CDISpl'
            endif

!         Set boundary conditions for nodes on surface
          elseif(isw.eq.2) then

            if(prt) then
              write(iow,2012) (i,i=1,ndf)
              if(ior.lt.0) write(*,2012) (i,i=1,ndf)
            end if
            do n = 1,numnp
              if(ip(n).gt.0 .and. abs(xin(n)).le.1.d0+tol) then
                do i = 1,ndf
                  if(nint(pl(i)).ge.0) then
                    id(i,n,2) = id(i,n,2) + nint(pl(i))
                  else
                    id(i,n,2) = 0
                  endif
                end do
                if(prt) then
                  write(iow,2013) n,(id(i,n,2),i=1,ndf)
                  if(ior.lt.0) then
                    write(*,2013) n,(id(i,n,2),i=1,ndf)
                  endif
                endif
              endif
            end do

!         Set boundary angles for nodes on surface
          elseif(isw.eq.3) then

            if(prt) then
              write(iow,2021)
              if(ior.lt.0) write(*,2021)
            endif

            do n = 1,numnp
              if(ip(n).gt.0) then
                xia    = xin(n)
                ang(n) = pl(1)*0.5d0*xia*(xia-1.d0)
     &                 + pl(3)*(1.d0-xia*xia)
     &                 + pl(2)*0.5d0*xia*(xia+1.d0)
                if(prt) then
                  write(iow,2002) n,ang(n)
                  if(ior.lt.0) then
                    write(*,2002) n,ang(n)
                  endif
                endif

              endif
            enddo

!         Set displacements for nodes on surface
          elseif(isw.eq.4) then

            if(prt) then
              write(iow,2021)
              if(ior.lt.0) write(*,2021)
            endif

            do n = 1,numnp
              if(ip(n).gt.0) then
                do i = 1,ndf
                  f(i,n,2) = pl(i)
                end do ! i
                if(prt) then
                  write(iow,2042) n,(f(i,n,2),i=1,ndf)
                  if(ior.lt.0) then
                    write(*,2042) n,(f(i,n,2),i=1,ndf)
                  endif
                endif

              endif

            enddo

!         Set proportional load numbers for surface
          elseif(isw.eq.6) then

            if(prt) then
              write(iow,2021)
              if(ior.lt.0) write(*,2021)
            endif

            do n = 1,numnp
              if(ip(n).gt.0) then
                point = np(29) + ndf*(n-1) - 1
                do i = 1,ndf
                  mr(i+point) = nint(pl(i))
                end do ! i
                if(prt) then
                  write(iow,2042) n,(mr(i+point),i=1,ndf)
                  if(ior.lt.0) then
                    write(*,2042) n,(mr(i+point),i=1,ndf)
                  endif
                endif

              endif
            enddo

          endif

        endif

        go to 1

!       End of current file
300     continue

      end do

!     Restore parameter values
      do i = 0,36
        do n = 1,26
          vvv(n,i) = vvsave(n,i)
        end do
      end do
      prt  = oprt
      prth = oprth

!     Formats

1000  format(a4,2x,a12,i8,2l5)
1001  format(1p,4e20.12)

2000  format('   C o o r d i n a t e    S u r f a c e   L o a d s'/
     &       '   x_1 Coord.   y_1 Coord.   x_2 Coord.   y_2 Coord.',
     &       '   x_3 Coord.   y_3 Coord.'/1p,6e13.4/
     &       '   p_1 Press.                p_2 Press.             ',
     &       '   p_3 Press.'/3(1p,1e13.4,13x))

2001  format(/'       N o d a l    F o r c e s'//
     &       '       a_Node    x_a Force    y_a Force       b_Node',
     &       '    x_b Force    y_b Force'/)

2002  format(2(i13,1p,2e13.4))

2010  format('   C o o r d i n a t e    S u r f a c e',
     &       '   C o n d i t i o n'/
     &       '   x_1 Coord.   y_1 Coord.   x_2 Coord.   y_2 Coord.',
     &       '   x_3 Coord.   y_3 Coord.'/1p,6e13.4/
     &       (10(i3,' B.C.',:)))

2011  format(10i8)

2012  format(/'       N o d a l    B o u n d a r y    C o d e s'//
     &       '      Node',6(i5,'-B.C.':)/(10x,6(i5,'-B.C.':)))

2013  format(7i10/(10x,6i10))

2020  format('   C o o r d i n a t e    S u r f a c e   L o a d s'/
     &       '   x_1 Coord.   y_1 Coord.   x_2 Coord.   y_2 Coord.',
     &       '   x_3 Coord.   y_3 Coord.'/1p,6e13.4/
     &       '   a_1 Angle                 a_2 Angle              ',
     &       '   a_3 Angle '/3(1p,1e13.4,13x))

2021  format(/'       N o d a l    A n g l e s'//
     &       '       a_Node    t_a Angle')

2030  format('   C o o r d i n a t e    S u r f a c e   D i s p l.'/
     &       '   x_1 Coord.   y_1 Coord.   x_2 Coord.   y_2 Coord.',
     &       '   x_3 Coord.   y_3 Coord.'/1p,6e13.4/
     &       '   d_1 Displ                 d_2 Displ              ',
     &       '   d_3 Displ '/3(1p,1e13.4,13x))

2031  format(/'       N o d a l    D i s p l a c e m e n t s'//
     &       '       a_Node    a_dof   d_a Displ')

2032  format(i13,i8,1p,e13.4)

2040  format('   C o o r d i n a t e    S u r f a c e',
     &       '   C o n d i t i o n'/
     &       '   x_1 Coord.   y_1 Coord.   x_2 Coord.   y_2 Coord.',
     &       '   x_3 Coord.   y_3 Coord.'/1p,6e13.4/
     &       (10(i3,' Disp',:)))

2041  format(10i8)

2042  format(/'       N o d a l    B o u n d a r y    D i s p l.'//
     &       '    Node',6(i6,'-Displ':)/(10x,6(i6,'-Displ':)))

3000  format(' *ERROR* PESURF:Surface loading file',a,' does not exist')

3001  format(' *ERROR* PESURF: Segment node too large, node =',i5)

3002  format(' *ERROR* PESURF: Segment for element',i8,
     &       ' has bad projection values for nodes',i8,' and',i8)

3003  format(' *ERROR* PESURF: In surface condition inputs')

3004  format(' --> WARNING: No nodes found for ',a,' data type')
      end subroutine pesurf
