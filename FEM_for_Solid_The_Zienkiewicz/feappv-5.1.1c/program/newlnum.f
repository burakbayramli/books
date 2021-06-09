!$Id:$
      subroutine newlnum(lagbc,lagrn,lagmn,eq,ix,ie,ren)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Compute equation structure for lagrange multiplier unknowns

!      Inputs:
!        eq(ndf,*)     - Solid element equation numbers
!        ix(nen1,*)    - Element connection array
!        ie(nie,*)     - Element control data
!        ren(*)        - Nodal reorder list

!      Outputs
!        lagbc(ndl,numel)- First element equation number
!        lagrn(numel)    - Number equations after node number
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'cdata.h'
      include  'cdat1.h'
      include  'sdata.h'

      logical     :: eflag,pflag
      integer     :: eqad,neqad,i,m,ma,mm,e,n,nn,     nlm
      integer     :: lagbc(ndl,numel,2),lagrn(numnp),lagmn(numel)
      integer     :: eq(ndf,numnp),ix(nen1,*),ie(nie,*)
      integer     :: ren(numnp)

      save

!     Zero nodal and element array
      do nn = 1,numnp
        lagrn(nn) = 0
      end do ! nn

!     Set maximum node number on element
      do e = 1,numel
        if(lagbc(1,e,1).gt.0) then

!         Find maximum reordered node number on element
          nn = 0
          do i = 1,nen
            if(ix(i,e).gt.0) then
              nn = max(nn,ren(ix(i,e)))
            endif
          end do ! i
          lagmn(e) = nn
        else
          lagmn(e) = numnp + 1
        endif
      end do ! e

!     Search elements
      neqad = 0
      eqad  = 0
      eflag = .true.
      do while (eflag)

        eflag = .false.
        nn    = numnp + 1
        do mm = 1,numel
          if(lagmn(mm).lt.nn) then
            e  = mm               ! Next element to process
            nn = lagmn(mm)        ! Element maximum node number
          endif
        end do ! mm

        if(nn.le.numnp) then
          lagmn(e) = numnp + 1
          eflag    = .true.
        endif

!       Adjust all other equations
        if(eflag) then
          pflag = .true.
          ma    = ix(nen1,e)
          do m = 1,nummat
            if(ie(nie-2,m).eq.ma .and. ie(nie-8,m).gt.0) then
!             Compute number of added multipliers in each element with eqs.
              nlm = 0
              do i = 1,ie(nie-8,m)
                if(lagbc(i,e,2).eq.0) then
                  nlm   = nlm + 1
                endif
              end do ! i
!             Do adjustments
              pflag = .false.
              do n = 1,numnp
                mm = ren(n)
                if(mm.gt.nn) then
                  do i = 1,ndf
                    if(eq(i,mm).gt.0) then
                      eq(i,mm) = eq(i,mm) + nlm
                    endif
                  end do ! i
                else
                  do i = 1,ndf
                    if(eq(i,mm).gt.0) then
                      eqad     = max(eqad,eq(i,mm))
                    endif
                  end do ! i
                endif
              end do ! n

!             Set numbers for element
              if(lagrn(nn).eq.0) then
                do i = 1,nlm
                  if(lagbc(i,e,2).eq.0) then
                    lagbc(i,e,1)  = eqad  + i
                  endif
                end do ! i
              else
                do i = 1,nlm
                  if(lagbc(i,e,2).eq.0) then
                    lagbc(i,e,1)  = lagrn(nn) + i
                  endif
                end do ! i
              endif
              neqad     = neqad + nlm ! mm
              eqad      = eqad  + nlm ! mm
              lagrn(nn) = eqad
            endif
          end do ! m
          if(pflag) then
            lagmn(e) = numnp + 1
          endif
        endif ! eflag
      end do ! while

      end subroutine newlnum
