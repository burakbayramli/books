!$Id:$
      subroutine psets(ixl,ix,id,g,  xs, nss, tangfl)
!                      331 33 31 332 333 <--- Pr no in call

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!     Modification log                                Date (dd/mm/year)
!       Original version                                    01/05/2018
!-----[--+---------+---------+---------+---------+---------+---------+-]
!      Purpose: Compute sums of boundary displacements for fine scale
!               model to obtain stress and its tangent moduli
!               (both are returned in matrix form)

!      Inputs:
!        ix(nen1,*)   - Element connection data
!        id(ndf,*)    - Equation numbers
!        nss          - Number tau stress components
!        tangfl       - Compute tangent if true

!      Working:
!        finflg       - Finite deformation flag
!        prtype       - Problem type

!      Outputs:
!        ixl(ndf,*)   - DOF indicators: -1 = no equation
!                                       >0 = dof eq. number
!                                        0 = boundary dof
!-----[--+---------+---------+---------+---------+---------+---------+-]
      implicit   none

      include   'cdata.h'
      include   'debugs.h'
      include   'elpers.h'
      include   'iofile.h'
      include   'hdatam.h'
      include   'oelmt.h'
      include   'pglob1.h'
      include   'sdata.h'
      include   'setups.h'
      include   'tdata.h'
      include   'pointer.h'
      include   'comblk.h'

      include   'counts.h'

      logical       :: sflg, tangfl
      integer       :: i,j,nn, nel, nss
      integer       :: ix(nen1,*)  , id(ndf,numnp)
      integer       :: ixl(ndf,nen)
      real (kind=8) :: g(neq,nss,2),ht(144), xs(ndm,*)
      real (kind=8) :: volsv, sigsv

      save

!     Initialize averaged flux, stress and tangent modulus arrays
      pflux(1:3)    = 0.0d0
      ptau(1:6)     = 0.0d0
      ht(1:nss*nss) = 0.0d0                          ! Zero h array
      if(neq.gt.0) g(:,:,:) = 0.0d0                  ! Zero g arrays

      volsv = v_avg
      sigsv = sig_33
      do nn = 1,numel

!       Set 'ixl' array to mark dofs with fixed boundaries

        sflg = .false.
        nel  = 0
        do i = 1,nen
          if(ix(i,nn).gt.0) then
            nel    = i
            do j = 1,ndf
              if(id(j,ix(i,nn)).gt.0) then  ! Look at eq. no.
                ixl(j,i) = id(j,ix(i,nn))  ! Eq. numbers
              else
                ixl(j,i) = 0 ! Fixed boundary node
                sflg   = .true.
              endif
            end do ! j
          else !  No node
            ixl(1:ndf,i) = -1
          endif
        end do ! i

!       Get element tangent and residual: No assembly

        if(sflg) then

          hflgu  = .false.
          h3flgu = .false.

!                        U      B
          call formfe(np(40),np(26),np(26),np(26),
     &                .false.,.false.,.false.,3,nn,nn,1)

!         Project to Kirchhoff stress and tangent modulus

          if(tangfl) then

!           Displacement boundary case

            call pprojpp(ixl,hr(np(44)),xs,
     &                   hr(np(35)),hr(np(36)),g, ht,
     &                   ndm,ndf,nel,nst, neq, nss)

!         Compute stress only

          else

            call pprojp(ixl,hr(np(44)),xs,
     &                  hr(np(35)),ndm,ndf,nel)

          endif
        endif ! sflg
      end do ! nn
      v_avg  = volsv
      sig_33 = sigsv

!     Solve for tangent moduli
      if(tangfl) then

!       Modify shear terms
        if(prtype.eq.2) then

          call ptang(g,ht, neq,nss)   ! Include half factors on shears

!         Inform warning is o.k.
          if(rank.eq.0 .and. ndm.lt.3) then
            if(ior.lt.0) then
              write(  *,2000)
            else
              write(iow,2000)
            endif
          endif

        endif

        if(neq.gt.0) then

!         Copy G_1 to G_2
          g(:,:,2) = g(:,:,1)

          if(debug) then
            call mprint(g ,neq,nss,neq,'G1/2_unreduced')
            call mprint(ht,nss,nss,nss,'H_unreduced')
          endif

!         Form material moduli by static condensation
!         N.B. Moduli returned in h array.
!                     h  g g_cols
          call formhh(ht,g,nss,neq)

          if(debug) then
            call mprint(g(1,1,1),neq,nss,neq,'G1_reduced')
            call mprint(g(1,1,2),neq,nss,neq,'G2_reduced')
            call mprint(ht      ,nss,nss,nss,'H_reduced')
          endif

        endif

!       Store into cflux tangent

        if(prtype.eq.1) then

          nn = 0
          do i = 1,nss
            do j = 1,nss
              nn = nn + 1
              pcflux(j,i) = ht(nn)
            end do ! j
          end do ! i

        endif

!       Store into pctau tangent

        if(prtype.eq.2) then

          nn = 0
          do i = 1,nss
            do j = 1,nss
              nn = nn + 1
              pctau(j,i) = ht(nn)
            end do ! j
          end do ! i

        endif

!     Other cases

      else

!       Modify shear values

        if(prtype.eq.2) then
          do i = 4,nss
            ptau(i) = ptau(i)*0.5d0
          end do ! i
        endif

      endif

!     Formats

2000  format(/'-->N.B. G-has no entries for thickness direction.'/
     &     8x,'Results in following zero right-hand-side warning and'/
     &     8x,'no entries in third row/column of tangent moduli.')

      end subroutine psets
