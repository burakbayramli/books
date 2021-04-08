!$Id:$
      subroutine pprojpp(ixl,xl,xs,p,
     &                   s,g,ht,ndm,ndf,nel,nst,neq,nss)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!     Modification log                                Date (dd/mm/year)
!       Original version                                    01/05/2018
!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Set Coupling and diagonal arrays to compute averaged
!               TAU stress and tangent modulus arrays: Periodic case

!      Inputs:
!        ixl(ndf,*)   - DOF indicators: -1 = no equation
!                                       >0 = Eq. number of dof
!                                        0 = boundary dof
!        xl(ndm,nel)  - Element nodal coordinates
!        xs(ndm,nel)  - Element nodal coordinates
!        p(ndf,nel)   - Element residual
!        s(nst,nst)   - Element tangent matrix
!        ndm          - Spatial dimension of mesh
!        nel          - Number of maximum node on element
!        nst          - Dimension of tangent matrix
!        neq          - Number of active equations in mesh
!        nss          - Number of modes to project (generally = 9)

!      Working:
!        finflg       - Finite deformaiton flag
!        prtype       - Problem type
!        xs(ndm,nel)  - Element nodal coordinates

!      Outputs:
!        g(neq,nss,1) - Coupling matrix
!        h(nss,nss)   - Block matrix
!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      include   'debugs.h'
      include   'elpers.h'
      include   'iofile.h'
      include   'pglob1.h'
      include   'qudshp.h'

      integer       :: ndm,ndf,nel,nst, neq, nss
      integer       :: i,j,ib,jb,ir,jc,ia,a,b

      integer       :: ixl(ndf,*),is(64),isIb(3,3)
      real (kind=8) :: xl(ndm,nel),xs(ndm,nel),p(ndf,nel),s(nst,nst)
      real (kind=8) :: g(neq,nss),ht(nss,nss)

      save

      data       isIb / 1, 4, 6,
     &                  4, 2, 5,
     &                  6, 5, 3/

      if(debug) then
        call iprint(ixl,ndf,nel,ndf,'IXL')
        call mprint( xl,ndm,nel,ndm,' XL')
        call mprint( gradu,3,3,3,' GRADU')
      endif

!     Set assembly pointers

      if(prtype.eq.1) then
        is(1) = 1
      else
        is(1) = 0
      endif
      do i = 2,nel
        is(i) = is(i-1) + ndf
      end do ! i

!     Thermal problem

      if(prtype.eq.1) then

!       Compute thermal flux

        do ir = 1,nel
          if(ixl(1,ir).eq.0) then  ! Assemble P1
            do ib = 1,ndm
              pflux(ib) = pflux(ib) - p(1,ir)*xl(ib,ir)  ! Thermal flux
            end do ! ib
          endif
        end do ! ir

!       Form g_ib array

        do ir = 1,nel
          ia = ixl(1,ir)
          if(ia.gt.0) then ! Equation number exists: Assemble G
            do jc = 1,nel
              if(ixl(1,jc).eq.0) then
                do jb = 1,ndm
                  g(ia,jb) = g(ia,jb)
     &                     + s(is(ir),is(jc))*xl(jb,jc)
                end do ! jb
              endif
            end do ! jc
          endif
        end do ! ir

!       Form h_ab array

        do ir = 1,nel
          if(ixl(1,ir).eq.0) then  ! Assemble H1
            do ib = 1,ndm
              do jc = 1,nel
                if(ixl(1,jc).eq.0) then
                  do jb = 1,ndm
                    ht(ib,jb) = ht(ib,jb) + xl(ib,ir)
     &                        * s(is(ir),is(jc))*xl(jb,jc)
                  end do ! jb
                endif
              end do ! jc
            end do ! ib
          endif
        end do ! ir

!     Mechanical problem

      elseif(prtype.eq.2) then

!       Form current coordinates

        do ir = 1,nel
          do i = 1,ndm
            xs(i,ir) = xl(i,ir)
            if(finflg) then
              do j = 1,ndm
                xs(i,ir) = xs(i,ir) + gradu(i,j)*xl(j,ir)
              end do ! j
            endif
          end do ! i
        end do ! ir

        if(debug) call mprint(xs,ndm,nel,ndm,'XS')
        if(debug) call mprint(s,nst,nst,nst,'S')
        if(debug) call mprint(p,ndf,nst,ndf,'P')

!       Compute stress

        do ir = 1,nel
          do i = 1,ndm
            if(ixl(i,ir).eq.0) then
              do ib = 1,ndm
                a       = isIb(i,ib)
                ptau(a) = ptau(a) - p(i,ir)*xs(ib,ir)  ! Stress
              end do ! ib
            endif
          end do ! i
        end do ! ir

!       Form g_ib array

        do ir = 1,nel
          do i = 1,ndm
            ia = ixl(i,ir)
            if(ia.gt.0) then ! Equation number exists
              do jc = 1,nel
                do j = 1,ndm
                  if(ixl(j,jc).eq.0) then
                    do jb = 1,ndm
                      b       = isIb(j,jb)
                      g(ia,b) = g(ia,b)
     &                        + s(is(ir)+i,is(jc)+j)*xs(jb,jc)
                    end do ! jb
                  endif
                end do ! j
              end do ! jc
           endif
          end do ! i
        end do ! ir

!       Form h_ab array

        do ir = 1,nel
          do i = 1,ndm
            if(ixl(i,ir).eq.0) then  ! Assemble H1
              do ib = 1,ndm
                a = isIb(i,ib)
                do jc = 1,nel
                  do j = 1,ndm
                    if(ixl(j,jc).eq.0) then
                      do jb = 1,ndm
                        b       = isIb(j,jb)
                        ht(a,b) = ht(a,b) + xs(ib,ir)
     &                          * s(is(ir)+i,is(jc)+j)*xs(jb,jc)
                      end do ! jb
                    endif
                  end do ! j
                end do ! jc
              end do ! ib
            endif
          end do ! i
        end do ! ir

      endif ! prtype

      if(debug) call mprint(g,neq,nss,neq,'G1')

      end subroutine pprojpp
