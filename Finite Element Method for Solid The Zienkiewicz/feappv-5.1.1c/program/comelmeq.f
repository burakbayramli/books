!$Id:$
      subroutine comelmeq(ie,ix, lagbc, ir, kpo,kp,nn,neq,
     &                    bycol,wdiag,rc_all)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose:  Compute equation numbers from elements

!      Inputs:
!         ie(nie,*)    -  Element identifier terms
!         ix(*)        -  Element connection list for element 'nn'
!         lagbc(ndl,*) -  Multiplier equation numbers
!         kpo          -  Initial row entry
!         neq          -  Number of equation to assemble
!         nn           -  Element number
!         bycol        -  Storage by columns if true
!         wdiag        -  Include diagonal if true
!         rc_all       -  All terms in row/col if true

!      Outputs:
!         ir(*)        -  Row number of each nonzero in stiffness matrix.
!         kp           -  Last entry in ir array
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'cdat1.h'     ! nie
      include  'sdata.h'     ! nen1, ndl
      include  'iofile.h'

      logical   :: addeq, bycol, wdiag, rc_all
      integer   :: nlm
      integer   :: kpo,kp,neq, i,l, neqj, nn, ma
      integer   :: ie(nie,*), ix(*), ir(*), lagbc(ndl,*)

      save

!     Extract equation
      ma  = ix(nen1)
      nlm = ie(nie-8,ma)
      if(nlm.gt.0) then

!       Assemble element equations
        do l = 1,nlm
          if(lagbc(l,nn).gt.0) then
            neqj = lagbc(l,nn)

            if(rc_all) then                        ! all terms
              addeq   = neqj.gt.0
            elseif(bycol) then                     ! by columns
              if(wdiag) then
                addeq = neqj.le.neq.and.neqj.gt.0  ! diagonal in
              else
                addeq = neqj.lt.neq.and.neqj.gt.0  ! diagonal out
              endif
            else                                   ! by rows
              if(wdiag) then
                addeq = neqj.ge.neq                ! diagonal in
              else
                addeq = neqj.gt.neq                ! diagonal out
              endif
            endif

!           Add equation to list
            if(addeq) then

!             Check if equation already in list.
              do i = kpo, kp
                if(ir(i).eq.neqj) go to 300
              end do ! i

!             New equation, add to list
              kp     = kp + 1
              ir(kp) = neqj
300           continue
            endif
          endif
        end do ! l

      endif ! nlm > 0

      end subroutine comelmeq
