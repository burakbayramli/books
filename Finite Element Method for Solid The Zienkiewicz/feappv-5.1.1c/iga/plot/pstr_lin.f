!$Id:$
      subroutine pstr_lin(ie,ix, x,xl, wt,wtl, ix_lin, st, s_lin, p_lin,
     &                    u, u_lin, x_lin)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Projects element variables for T-spline/NURBS plots

!      Inputs:
!        ie(nie,*)    - Element type properties
!        ix(nen1,*)   - Mesh connection list
!        x(ndm,*)     - Mesh control points
!        xl(ndm,*)    - Element control points
!        wt(*)        - Mesh weights
!        wtl(*)       - Element weights
!        ix_lin(19,*) - Linear element connectionlist
!        st(numnp,*)  - Mesh projected values
!        u(ndf,*)     - Mesh solution values
!        x_lin(ndm,*) - Linear mesh coordinates

!      Outputs:
!        s_lin(nd_lin,*) - Plot projected values
!        p_lin(nd_lin,*) - Plot projected values
!        u_lin(ndf,*)    - Plot projected values
!-----[--+---------+---------+---------+---------+---------+---------+-]
      implicit   none

      include   'cdata.h'
      include   'cdat1.h'
      include   'cnurb.h'
      include   'eldata.h'
      include   'iofile.h'
      include   'pbody.h'
      include   'pdata3.h'
      include   'qudshp.h'
      include   'sdata.h'
      include   'pointer.h'
      include   'comblk.h'

      integer    ie(nie,*), ix(nen1,*), ix_lin(19,*)
      real*8     x(ndm,*), xl(ndm,*), wt(*), wtl(*), st(numnp,*), xx(3)
      real*8     s_lin(nd_lin,*),p_lin(nd_lin,*),u_lin(ndf,*),u(ndf,*)
      real*8     x_lin(ndm,*)

      logical    noskip
      integer    nd, ne, is,js,ks, nn, ns, i,j, na
      integer    ixl(3,8)
      real*8     gp(9), du, dist

      integer    e_lin, d_lin, d_min

      save

      data       ixl / 0,0,0,   1,0,0,  1,1,0,   0,1,0,
     &                 0,0,1,   1,0,1,  1,1,1,   0,1,1 /

!     Get Gaussian points and weights

      lint   = 1
      du     = 2.0d0/dble(npl_int)
      gp(1)  = -1.000d0
      do i = 1,npl_int-1
        gp(i+1) = gp(i) + du
      end do ! i
      gp(npl_int+1) = 0.999999999999d0 ! Keep point inside element

      d_lin = 0
      d_min = 1
      e_lin = 0
      do na = 1,nummat
        do ne = 1,numel
          n_el = ne   ! Element number for use
          ma   = ix(nen1,ne)
          if((ma.eq.na .and. maplt.eq.0) .or.
     &       (ma.eq.na .and. ma.eq.maplt)) then
            eltyp = ix(nen+7,ne)
            if(eltyp.gt.0) then

              elty2 = ix(nen+8,ne)
              elty3 = ix(nen+9,ne)

!             Set up local values

              nel = 0
              do i = 1,nen
                if(ix(i,ne).gt.0) then
                  xl(1,i) = x(1,ix(i,ne))
                  xl(2,i) = x(2,ix(i,ne))
                  if(ndm.eq.3) xl(3,i) = x(3,ix(i,ne))
                  wtl(i)  = wt(ix(i,ne))
                  nel     = i
                else
                  xl(1,i) = 0.0d0
                  xl(2,i) = 0.0d0
                  if(ndm.eq.3) xl(3,i) = 0.0d0
                  wtl(i)  = 1.0d0
                endif
              end do ! i

!             Do 1-d plots

              if(ie(1,ma).eq.1) then

!               For element loop over number of subdivisions 'npl_int'

                do is = 1,npl_int

                  e_lin = e_lin + 1

!                 Construct 2-node 'linear elements'

                  do ns = 1,2

                    nd = ix_lin(ns,e_lin)

                    sg1(1,1) = gp(is+ixl(1,ns))
                    sg1(2,1) = 1.0d0

                    call interp1d(1, xl, ndm,nel, .false.)

!                   Compute position

                    do i = 1,ndm
                      xx(i) = 0.0d0
                      do nn = 1,nel
                        xx(i) = xx(i) + shp1(2,nn,1)*xl(i,nn)
                      end do ! nn
                    end do ! i

!                   Check for repeated nodes

                    noskip = .true.
                    do i = d_lin,d_min,-1
                      dist = (x_lin(1,i) - xx(1))**2
                      if(ndm.ge.2) dist = dist + (x_lin(2,i)-xx(2))**2
                      if(ndm.eq.3) dist = dist + (x_lin(3,i)-xx(3))**2

                      if(dist.le.dist_min) then
                        noskip = .false.
                        exit
                      endif
                    end do ! i

!                   New node

                    if(noskip) then
                      d_lin = d_lin + 1

!                     Project dependent variables

                      do j = 1,ndf
                        u_lin(j,d_lin) = 0.0d0
                      end do ! j
                      do nn = 1,nel
                        if(ix(nn,ne).gt.0) then
                          do j = 1,ndf
                            u_lin(j,d_lin) = u_lin(j,d_lin)
     &                                 + shp1(2,nn,1)*u(j,ix(nn,ne))
                          end do ! j
                        endif
                      end do ! nn

!                     Project stress variables

                      s_lin(d_lin,1) = 1.0d0
                      do i = 2,npstr
                        s_lin(d_lin,i) = 0.0d0
                      end do ! i
                      do nn = 1,nel
                        if(ix(nn,ne).gt.0) then
                          do i = 2,npstr
                            s_lin(d_lin,i) = s_lin(d_lin,i)
     &                              + shp1(2,nn,1)*st(ix(nn,ne),i)
                          end do ! i
                        endif ! ix > 0
                      end do ! nn
                    endif ! noskip

                  end do ! ns
                end do ! is

!             Do 2-d plots

              elseif(ie(1,ma).eq.2) then

!               For element loop over number of subdivisions 'npl_int'

                do js = 1,npl_int
                  do is = 1,npl_int

                    e_lin = e_lin + 1

!                   Construct 4-node 'linear elements'

                    do ns = 1,4

                      sg2(1,1) = gp(is+ixl(1,ns))
                      sg2(2,1) = gp(js+ixl(2,ns))
                      sg2(3,1) = 1.0d0

                      call interp2d(1, xl, ix(1,ne), ndm,nel, .false.)

!                     Compute position

                      do i = 1,ndm
                        xx(i) = 0.0d0
                        do nn = 1,nel
                          xx(i) = xx(i) + shp2(3,nn,1)*xl(i,nn)
                        end do ! nn
                      end do ! i

!                     Check for repeated nodes

                      noskip = .true.
                      do i = d_lin,d_min,-1
                        dist = (x_lin(1,i) - xx(1))**2
     &                       + (x_lin(2,i) - xx(2))**2
                        if(ndm.eq.3) then
                          dist = dist + (x_lin(3,i) - xx(3))**2
                        endif

                        if(dist.le.dist_min) then
                          noskip = .false.
                          exit
                        endif
                      end do ! i

!                     New node

                      if(noskip) then
                        d_lin = d_lin + 1

!                       Project dependent variables

                        do j = 1,ndf
                          u_lin(j,d_lin) = 0.0d0
                        end do ! j
                        do nn = 1,nel
                          if(ix(nn,ne).gt.0) then
                            do j = 1,ndf
                              u_lin(j,d_lin) = u_lin(j,d_lin)
     &                                  + shp2(3,nn,1)*u(j,ix(nn,ne))
                            end do ! j
                          endif
                        end do ! nn

                        s_lin(d_lin,1) = 1.0d0
                        do i = 2,npstr
                          s_lin(d_lin,i) = 0.0d0
                        end do ! i
                        do nn = 1,nel
                          if(ix(nn,ne).gt.0) then
                            do i = 2,npstr
                              s_lin(d_lin,i) = s_lin(d_lin,i)
     &                                + shp2(3,nn,1)*st(ix(nn,ne),i)
                            end do ! i
                          endif ! ix > 0
                        end do ! nn
                      endif ! noskip

                    end do ! ns

                  end do ! is
                end do ! js

!             Do 3-d plots

              elseif(ie(1,ma).eq.3) then

!               For element loop over number of subdivisions 'npl_int'

                do ks = 1,npl_int
                  do js = 1,npl_int
                    do is = 1,npl_int

                      e_lin = e_lin + 1

!                     Construct 8-node 'linear elements'

                      do ns = 1,8

                        sg3(1,1) = gp(is+ixl(1,ns))
                        sg3(2,1) = gp(js+ixl(2,ns))
                        sg3(3,1) = gp(ks+ixl(3,ns))
                        sg3(4,1) = 1.0d0

                        call interp3d(1, xl, ndm,nel)

!                       Project stress variables

!                       Compute position

                        do i = 1,ndm
                          xx(i) = 0.0d0
                          do nn = 1,nel
                            xx(i) = xx(i)+shp3(4,nn,1)*xl(i,nn)
                          end do ! nn
                        end do ! i

!                       Check for repeated nodes

                        noskip = .true.
                        do i = d_lin,d_min,-1
                          dist = (x_lin(1,i) - xx(1))**2
     &                         + (x_lin(2,i) - xx(2))**2
     &                         + (x_lin(3,i) - xx(3))**2

                          if(dist.le.dist_min) then
                            noskip = .false.
                            exit
                          endif
                        end do ! i

!                       New node

                        if(noskip) then
                          d_lin = d_lin + 1

!                         Project dependent variables

                          do j = 1,ndf
                            u_lin(j,d_lin) = 0.0d0
                          end do ! j
                          do nn = 1,nel
                            if(ix(nn,ne).gt.0) then
                              do j = 1,ndf
                                u_lin(j,d_lin) = u_lin(j,d_lin)
     &                                  + shp3(4,nn,1)*u(j,ix(nn,ne))
                              end do ! j
                            endif
                          end do ! nn

!                         Project stress variables

                          s_lin(d_lin,1) = 1.0d0
                          do i = 2,npstr
                            s_lin(d_lin,i) = 0.0d0
                          end do ! i
                          do nn = 1,nel
                            if(ix(nn,ne).gt.0) then
                              do i = 2,npstr
                                s_lin(d_lin,i) = s_lin(d_lin,i)
     &                                  + shp3(4,nn,1)*st(ix(nn,ne),i)
                              end do ! i
                            endif ! ix > 0
                          end do ! nn
                        endif

                      end do ! ns

                    end do ! is
                  end do ! js
                end do ! ks

              endif

            endif ! eltyp
          endif ! ma tests

        end do ! ne
        d_min = d_lin + 1

      end do ! na

      call pltstr(s_lin,p_lin(1,2),s_lin(1,2),nd_lin,ie(1,ma))

      end
!$Id:$
      subroutine sparse_wb(is,ib,js,jb,cs,cb, nuni,nbiv, lb,
     &                     wl, wb)

      implicit   none

      include   'eldata.h'

      integer    nuni,nbiv, lb
      integer    is(*),ib(*),js(*),jb(*)
      real*8     cs(*),cb(*), wb(*), wl(*)

      integer    i1,i2, j1,j2, ii,jj

!     Compute Bezier weights

      do i1 = 1,nuni
        do j1 = 1,nbiv
          ii = nbiv*i1 - nbiv + j1

!         Bezier weight

          do i2 = is(i1),is(i1+1)-1
            do j2 = ib(j1),ib(j1+1)-1
              jj     = lb*js(i2) - lb + jb(j2)
              wb(jj) = wb(jj) + cb(j2)*cs(i2)*wl(ii)
            end do ! j2
          end do ! i2
        end do ! j1
      end do ! i1

      end

      subroutine full3d_wb(cs, cb, nuni,nbiv,nll, wl,wb)

      implicit   none

      integer    nuni,nbiv,nll
      real*8     cs(nuni,nuni),cb(nll,nbiv)
      real*8     wb(*), wl(*)

      integer    i1,i2, j1,j2, ii,jj

!     Compute Bezier weights

      do i1 = 1,nuni
        ii = nbiv*i1 - nbiv
        do j1 = 1,nbiv

!         Bezier weight

          do i2 = 1,nuni
            jj = nll*i2 - nll
            do j2 = 1,nll
              wb(jj+j2) = wb(jj+j2) + cb(j2,j1)*cs(i2,i1)*wl(ii+j1)
            end do ! j2
          end do ! i2
        end do ! j1
      end do ! i1

      end

      subroutine full3d_w(cb, nbiv,nll, wl,wb)

      implicit   none

      integer    nbiv,nll
      real*8     cb(nll,nbiv)
      real*8     wb(*), wl(*)

      integer    j1,j2

!     Compute Bezier weights

      do j1 = 1,nbiv

!       Bezier weight

        do j2 = 1,nll
          wb(j2) = wb(j2) + cb(j2,j1)*wl(j1)
        end do ! j2
      end do ! j1

      end
