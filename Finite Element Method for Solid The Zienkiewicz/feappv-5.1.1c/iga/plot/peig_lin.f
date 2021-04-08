!$Id:$
      subroutine peig_lin(ie,ix, x,xl, wt,wtl, x_lin)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Projects eigvectors for T-spline/NURBS plots

!      Inputs:
!        ie(nie,*)     - Element type properties
!        x_lin(ndm,*)  - Position on linear mesh

!      Outputs:
!-----[--+---------+---------+---------+---------+---------+---------+-]
      implicit   none

      include   'cdata.h'
      include   'cdat1.h'
      include   'cnurb.h'
      include   'eldata.h'
      include   'evdata.h'
      include   'idptr.h'
      include   'qudshp.h'
      include   'sdata.h'
      include   'pbody.h'
      include   'pointer.h'
      include   'comblk.h'

      integer    ie(nie,*), ix(nen1,*)
      real*8     x(ndm,*), xl(ndm,*), wt(*), wtl(*), x_lin(ndm,*)

      logical    setvar, palloc, noskip
      integer    d_lin,d_min,e_lin, ne, is,js,ks, nn, ns, nod, i,j, na
      integer    ixl(3,8)

      real*8     du, dist, gp(9), xx(3)

      save

      data       ixl / 0,0,0,   1,0,0,  1,1,0,   0,1,0,
     &                 0,0,1,   1,0,1,  1,1,1,   0,1,1 /

!     Allocate temporary variables for eigenvectors

      setvar = palloc(112, 'TEMP2',mq*ndf, 2)
      setvar = palloc(114, 'TEMP4',mq*ndf*numnp,2)      ! eigv full
      if(np(283).eq.0) then
        setvar = palloc(283, 'E_LIN', ndf*nd_lin*mq, 2) ! e_lin
      endif
      call pneweigv(mr(id31),hr(np(77)),hr(np(114)),
     &              mq,neq,ndf,numnp)

!     Get Gaussian points and weights

      lint = 1
      du  = 2.0d0/dble(npl_int)
      gp(1)  = -1.000d0
      do i = 1,npl_int-1
        gp(i+1) = gp(i) + du
      end do ! i
      gp(npl_int+1) = 0.999999999999d0 ! Keep point inside element

      e_lin = 0
      d_lin = 0
      d_min = 1
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
                  do j = 1,ndm
                    xl(j,i) = x(j,ix(i,ne))
                  end do ! j
                  wtl(i)  = wt(ix(i,ne))
                  nel     = i
                else
                  do j = 1,ndm
                    xl(j,i) = 0.0d0
                  end do ! j
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

!                     Project eigenvectors

                      call pzero(hr(np(112)),mq*ndf)
                      do nn = 1,nel
                        nod = ix(nn,ne)
                        if(nod.gt.0) then
                          call setleigv(nod,ndf,mq,numnp,
     &                         shp1(2,nn,1),hr(np(112)),hr(np(114)))
                        endif ! nod > 0
                      end do ! nn

                      call setgeigv(hr(np(112)),hr(np(283)),
     &                              mq,ndf,d_lin,d_lin)
                    endif

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
                        if(ndm.eq.3) dist = dist + (x_lin(3,i)-xx(3))**2

                        if(dist.le.dist_min) then
                          noskip = .false.
                          exit
                        endif
                      end do ! i

!                     New node

                      if(noskip) then
                        d_lin = d_lin + 1

!                       Project eigenvectors

                        call pzero(hr(np(112)),mq*ndf)
                        do nn = 1,nel
                          nod = ix(nn,ne)
                          if(nod.gt.0) then
                            call setleigv(nod,ndf,mq,numnp,
     &                           shp2(3,nn,1),hr(np(112)),hr(np(114)))
                          endif ! nod > 0
                        end do ! nn

                        call setgeigv(hr(np(112)),hr(np(283)),
     &                                mq,ndf,nd_lin,d_lin)

                      endif

                    end do ! ns
                  end do ! is
                end do ! js

!             Do 3-d plots

              elseif(ie(1,ma).eq.3) then

!               For each element loop over number subdivisions 'npl_int'

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

!                         Project eigenvectors

                          call pzero(hr(np(112)),mq*ndf)
                          do nn = 1,nel
                            nod = ix(nn,ne)
                            if(nod.gt.0) then
                              call setleigv(nod,ndf,mq,numnp,
     &                             shp3(4,nn,1),hr(np(112)),hr(np(114)))
                            endif ! nod > 0
                          end do ! nn

                          call setgeigv(hr(np(112)),hr(np(283)),
     &                                  mq,ndf,nd_lin,d_lin)
                        endif

                      end do ! ns
                    end do ! is
                  end do ! js
                end do ! ks

              endif ! ie-type

            endif ! eltyp
          endif ! ma test

        end do ! ne
        d_min = d_lin + 1
      end do ! na

      setvar = palloc(112, 'TEMP2',  0, 2)
      setvar = palloc(114, 'TEMP4',  0, 2)

      end

      subroutine setleigv(nod,ndf,mq,numnp, shp,ev,evec)

      implicit   none

      integer    nod,ndf,mq,numnp, i,j
      real*8     shp,ev(ndf,mq), evec(ndf,numnp,*)

      do j = 1,mq
        do i = 1,ndf
          ev(i,j) = ev(i,j) + shp*evec(i,nod,j)
        end do ! i
      end do ! j

      end

      subroutine setgeigv(ev,ev_lin,mq,ndf,nd_lin,nd)

      implicit   none

      integer    mq,ndf,nd_lin,nd, i,j
      real*8     ev(ndf,mq), ev_lin(ndf,nd_lin,mq)

      do j = 1,mq
        do i = 1,ndf
          ev_lin(i,nd,j) = ev(i,j)
        end do ! i
      end do ! j

      end

      subroutine pneweigv(id,evec,evex,mq,neq,ndf,numnp)

      implicit   none

      integer    mq,neq,ndf,numnp, n,i,j
      integer    id(ndf,*)
      real*8     evec(neq,*), evex(ndf,numnp,*)

      do n = 1,numnp
        do j = 1,ndf
          if(id(j,n).gt.0) then
            do i = 1,mq
              evex(j,n,i) = evec(id(j,n),i)
            end do ! i
          else
            do i = 1,mq
              evex(j,n,i) = 0.0d0
            end do ! i
          endif
        end do ! j
      end do ! n

      end
