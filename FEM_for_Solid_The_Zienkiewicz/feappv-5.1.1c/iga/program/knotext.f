!$Id:$
      subroutine knotext(knots,lknot)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose:  Compute extraction operator for 1-d forms.

!      Use:      EXKNots (outputs extraction operator for all knots)

!      Inputs:
!         knots(*,*)   - Knot vecotrs
!         lknot(0:4,*) - Knot dimensioning

!      Outputs:
!         Extraction arrays and their pointers through hr(*) and mr(*)
!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      include   'bdata.h'
      include   'cnurb.h'
      include   'cdata.h'
      include   'igdata.h'
      include   'iofile.h'
      include   'print.h'
      include   'p_point.h'
      include   'sdata.h'

      include   'pointer.h'
      include   'comblk.h'

      logical    setvar, palloc

      integer    nb, n, nce
      real*8     knots(dknotig,*)
      integer    lknot(0:4,*)

      integer    knot, ord, lek, lth, nelm, ce_dim, de_dim, ktyp
      integer    cm_dim, dm_dim
      integer    ler, ltr
      real*8     knotr(dknotig)

      save

      setvar = palloc(273,'CEPTR',nknotig+1, 1)   ! Pointer for C_e
      setvar = palloc(334,'CPTMX',nknotig+1, 1)   ! Pointer for C_emx
      ce_dim = 0
      de_dim = 0
      cm_dim = 0
      dm_dim = 0
      do knot = 1,nknotig

        mr(np(273)+knot-1) = ce_dim  ! Pointer for start of C_e(knot)
        mr(np(334)+knot-1) = cm_dim  ! Pointer for start of C_e(knot)

        ktyp = lknot(0,knot)
        lek  = lknot(1,knot)
        ord  = lknot(2,knot)
        lth  = lek - ord - 1

!       Compute number of elements

        call setnelm(knots(1,knot),lek,ord, nelm)

        ce_dim = ce_dim + (ord+1)*(ord+1)*nelm
        de_dim = max(de_dim,(ord+1)*(ord+1))

        cm_dim = cm_dim + ord*ord*nelm
        dm_dim = max(dm_dim,ord*ord)

      end do !knot

      mr(np(273)+nknotig) = ce_dim  ! Pointer for end of C_e(knot)
      mr(np(334)+nknotig) = cm_dim  ! Pointer for end of C_e(knot)

!     Allocate storage for 1-d extraction operator

      setvar = palloc(289,'C_E  ',ce_dim+de_dim,2)
      setvar = palloc(335,'C_EMX',cm_dim+dm_dim,2)

      if(prt) write(iow,2000) head
      do knot = 1,nknotig

        ktyp = lknot(0,knot)
        lek  = lknot(1,knot)
        ord  = lknot(2,knot)
        lth  = lek - ord - 1

!       Determine number of elements
        call setnelm(knots(1,knot),lek,ord, nelm)

!       Set extraction operator array
        point = np(289) + mr(np(273)+knot-1)
        if(ord.eq.1) then ! Extraction are identities
          call ExtractIdentity(hr(point),nelm)
        else
          call ExtractCurve(lth, ord, knots(1,knot), nb, hr(point) )
        endif

!       Output extraction operators

        if(prt) then

!         Do output

          write(iow,2001) knot,nelm
          nce = 0
          do n = 1,nelm
            call mprint(hr(point+nce),ord+1,ord+1,ord+1,'C_E(knot)')
            nce = nce + (ord+1)**2
          end do ! n
        endif ! prt

!       Mixed model extraction array
        point = np(335) + mr(np(334)+knot-1)
        if(ord.eq.1) then ! Extraction are identities
          call ExtractConstant(hr(point),nelm)
        elseif(ord.eq.2) then ! Extraction are identities
          call ExtractIdentity(hr(point),nelm)
        else
          call rknot(knots(1,knot), knotr, lek, ler)
          ltr  = ler - ord - 2
          call ExtractCurve(ltr, ord-1, knotr, nb, hr(point) )
        endif

!       Output extraction operators

        if(prt) then

!         Do output

          write(iow,2001) knot,nelm
          nce = 0
          do n = 1,nelm
            call mprint(hr(point+nce),ord,ord,ord,'C_EMX(knot)')
            nce = nce + (ord)**2
          end do ! n
        endif ! prt

      end do ! knot

!     Truncate storage to remove extra unused value at end

      setvar = palloc(289,'C_E  ',ce_dim,2)
      setvar = palloc(335,'C_EMX',cm_dim,2)

!     Formats

2000  format(/1x,20a4//'   Extraction Operators for Knots')
2001  format(/10x,'Knot Number    =',i5/
     &        10x,'No. Increments =',i5)

      end

      subroutine ExtractIdentity(c_e,nelm)

      implicit   none

      integer    nelm, n
      real*8     c_e(2,2,nelm)

!     Initialize

      c_e = 0.0d0

!     Set to identitiy

      do n = 1,nelm
        c_e(1,1,n) = 1.0d0
        c_e(2,2,n) = 1.0d0
      end do ! n

      end

      subroutine ExtractConstant(c_e,nelm)

      implicit   none

      integer    nelm, n
      real*8     c_e(1,1,nelm)

!     Set to identitiy

      do n = 1,nelm
        c_e(1,1,n) = 1.0d0
      end do ! n

      end
      subroutine rknot(knots, knotr, lek, ler)

!     Compute order reduced knot vector from knots

      implicit   none

      logical    redfl
      integer    lek,ler, l
      real*8     knots(lek),knotr(lek)
      real*8     tol

!     Set tolerance and initial parameters

      tol   = max(abs(knots(1)),abs(knots(lek)))*1.d-8
      redfl = .false.
      ler   = 0
      do l = 2,lek

!       Eliminate a repeated knot
        if(knots(l)-knots(l-1).lt.tol .and. redfl) then
          redfl = .false.
!       Add a repeated knot
        elseif(knots(l)-knots(l-1).lt.tol .and. .not.redfl) then
          ler        = ler + 1
          knotr(ler) = knots(l)
!       Add unique knot
        else
          ler        = ler + 1
          knotr(ler) = knots(l)
          redfl      = .true.
        endif
      end do ! l

      end
