!$Id:$
      subroutine nodlnum(lagre,eq,ibc,ix,ren,ndfl)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!     Modification log                                Date (dd/mm/year)
!       Original version                                    01/11/2006
!       1. Remove set of lagre(2:3,i) -- i = 0              23/08/2007
!       2. Change 'id' to 'eq' (used as eq. nos.)           27/02/2019
!-----[--.----+----.----+----.-----------------------------------------]
!      Compute equation structure for nodal lagrange multiplier unknowns

!      Inputs:
!        eq(ndf,*)     - Nodal equation numbers
!        ibc(ndf,*)    - Nodal boundary conditions
!        ix(nen1,*)    - Element connection array
!        ren(*)        - Nodal reorder list
!        ndfl(ndf)     - Nodal Lagrange multipliers

!      Outputs
!        lagre(3,numnp)- First nodal equation number
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'cdata.h'
      include  'iofile.h'
      include  'sdata.h'

      logical      :: adjust, lagmul
      integer      :: eqad,eqng,i,m,mm,n,nd,nn,ll
      integer      :: eq(ndf,numnp),ibc(ndf,numnp),ix(nen1,*)
      integer      :: ren(numnp),lagre(3,numnp), ndfl(*)

      save

!     Test for Lagrange multipliers
      adjust = .true.
      do i = 1,ndf
        if(ndfl(i).gt.0) then
          adjust = .false.
        endif
      end do ! i
      if(adjust) return

!     Zero nodal array
      do nn = 1,numnp
        lagre(1,nn) = 0
        lagre(2,nn) = 0
        lagre(3,nn) = 0
      end do ! nn

!     Search elements
      do n = 1,numel

!       Check for active multipliers on element
        lagmul = .false.
        do i = 1,nen
          if(ix(i,n).gt.0) then
            do m = 1,ndf
              if(ndfl(m).gt.0 .and. ibc(m,ix(i,n)).eq.0) then
                lagmul = .true.
                go to 100
              endif
            end do ! m
          endif
        end do ! i

!       Find maximum reordered node number on element
100     if(lagmul) then
          nn = 0
          do i = 1,nen
            if(ix(i,n).gt.0) then
              do m = 1,ndf
                if(ndfl(m).eq.0 .and. ibc(m,ix(i,n)).eq.0) then
                  nn = max(nn,ren(ix(i,n)))
                  exit
                endif
              end do ! m
            endif
          end do ! i
          do i = 1,nen
            if(ix(i,n).gt.0) then
              do m = 1,ndf
                if(ndfl(m).gt.0 .and. ibc(m,ix(i,n)).eq.0) then
                  lagre(1,ren(ix(i,n))) = max(lagre(1,ren(ix(i,n))),nn)
                endif
              end do ! m
            endif
          end do ! i
        endif

      end do ! n

!     Set lagre(1,n) greater than number of nodes if zero
      do nd = 1,numnp
        if(lagre(1,nd).eq.0) then
          lagre(1,nd) = numnp + 1
        endif
      end do ! nd

!     Set equation numbers for non-Lagrange multipliers
      eqad  = 0
      do nd = 1,numnp
        n = numnp+1
        i = 0
        do nn = 1,numnp
          if(lagre(2,nn).eq.0 .and. lagre(1,nn).lt.n) then
            n = lagre(1,nn)
            i = nn
          endif
        end do ! nn
        if(i.gt.0 .and. i.le.numnp) then
          lagre(2, i) = 1
          lagre(3,nd) = i
        endif
        nn = ren(nd)
        do i = 1,ndf
          if(ndfl(i).eq.0 .and.ibc(i,nn).eq.0) then
            eqad     = eqad + 1
            eq(i,nn) = eqad
          else
            eq(i,nn) = 0
          endif
        end do ! i
      end do ! nd

      eqad  = 0
      do n = 1,numnp
        ll = lagre(3,n)
        if(ll.gt.0) then
          nn = lagre(1,ll)
          if(nn.gt.0 .and. nn.le.numnp) then

!           Adjust all other equations
            do m = 1,numnp
              mm = ren(m)
              if(mm.gt.nn .and. mm.le.numnp) then
                do i = 1,ndf
                  if(eq(i,mm).gt.0 .and. ndfl(i).eq.0) then
                    eq(i,mm) = eq(i,mm) + 1
                  endif
                end do ! i
              else
                do i = 1,ndf
                  if(eq(i,mm).gt.0) then
                    eqad     = max(eqad,eq(i,mm))
                  endif
                end do ! i
              endif
            end do ! m

!           Set numbers for node
            do i = 1,ndf
              if(ibc(i,ll).eq.0 .and. ndfl(i).gt.0) then
                eqad     = eqad + 1
                eq(i,ll) = eq(i,ll) + eqad
              endif
            end do ! i
          endif
        endif

      end do ! n

!     Set zero equations to negative to force boundary modifications
      eqng = 0
      do n = 1,numnp
        do i = 1,ndf
          if(eq(i,n).eq.0) then
            eqng    = eqng - 1
            eq(i,n) = eqng
          endif
        end do ! i
      end do ! n

      end subroutine nodlnum
