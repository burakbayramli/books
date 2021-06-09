!$Id:$
      subroutine pelmin(tx,idl,ix,nen1, nmat,nnty,nord,
     &                  prt,prth,error)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Data input routine for element connections

!      Inputs:
!         tx        - Option identifier
!         nen1      - Dimension for ix array
!         prt       - Flag, print input data if true
!         prth      - Flag, print title/header if true

!      Scratch:
!         idl(*)    - Local degree of freedom integer data

!      Outputs:
!         ix(*)     - Element nodal connection lists
!         error     - True if error occurs during input
!-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'cblktr.h'
      include  'cdata.h'
      include  'chdata.h'
      include  'dstars.h'
      include  'iofile.h'
      include  'region.h'

      character (len=69) :: mtype
      character (len=15) :: tx
      character (len=5)  :: etype, pelabl

      logical       :: prt,prth,error,errck,fprt,genfl, norec
      logical       :: pcomp,pinput,vinput
      integer       :: i,j,k,l,n,nen1,nnty,nmat,nord
      integer       :: ii,il,is,ilx,lg,lk,llx,ma,ng

      integer       :: ixg(16)
      integer       :: idl(*),ix(nen1,*), nel
      real (kind=8) :: td(16)

      save

!     Check for input format descriptor

      if(pcomp(tx,'gene',4).and. nen.le.16) then
        do j = 3,80
          if(xxx(j:j).eq.' ' .or. xxx(j:j).eq.',') then
            do i = j+1,80
              if(xxx(i:i).eq.' ' .or. xxx(i:i).eq.',') go to 200
            end do
          endif
        end do
        i = 69
200     mtype = xxx(i+1:70)
        fprt = vinput(mtype,69,td,nen)
        do n = 1,nen
          ixg(n) = nint(td(n))
        end do
        if(prt) call iprint(ixg,1,nen,1,'Element Generation Array')
        genfl = .true.
      else
        genfl = .false.
      endif

!     Perform input of data for element connections

      norec = .true.
      ilx = 0
      l   = 0
      ma  = 0
      if(ior.lt.0) write(*,2011)
      do i = 1, numel, 50
        j = min(numel,i+49)
        do n = i, j
          fprt = .false.
          if (l .lt. n) then
            llx = ilx

!           Input element records - N.B. limit is 16 nos. / record

            il = min(nen+3,16)
201         errck = pinput(td,il)
            if(errck) go to 201

!           Discontinue input of elements on blank record or negative number.

            l  = nint(td(1))
            if(l .le. 0) then
              if(norec) then
                write(  *,4001)
                write(iow,4001)
              endif
              return
            else
              l   = l + starel
              neo = max(neo,l)
            endif

!           Set element group number

            is = nint(td(2))

!           Transfer to element integer data

            do k = 3,il
              idl(k-2) = nint(td(k))
            end do

!           Input additional records

            norec = .false.
            do ii = 14,nen,16
              il = min(nen+1-ii,16)
202           errck = pinput(td,il)
              if(errck) go to 202

!             Transfer to element integer data

              do k = 1,il
                idl(ii+k) = nint(td(k))
              end do
            end do

!           Check if old sequence

            if(pcomp(tx,'old',3)) then

              lg = idl(nen+1)
              lk = is

!           Else change order

            else

              lg = is
              lk = idl(1)
              do k = 1,nen
                idl(k) = idl(k+1)
              end do ! k

            endif

!           Add star node number to inputs

            do k = 1,nen
              if(idl(k).gt.0) idl(k) = idl(k) + starnd
            end do ! k

            if (lg .eq. 0) then
              lg = 1
            endif
            ilx = lg
          endif

!         Error in input data

          if (l .lt. n) then
            write(iow,3001) l,n
            if(ior.lt.0) then
              write(*,3001) l,n
            endif
            error = .true.

!         Generate missing elements

          else if ((l .gt. n) .and. (llx .ne. 0)) then
            do k = 1, nen
              if(genfl) then
                ix(k,n) = ix(k,n-1) + ixg(k)
              else
                ix(k,n) = ix(k,n-1) + ng
              endif
              if (ix(k,n-1) .eq. 0) then
                ix(k,n) = 0
              endif
              if ((ix(k,n) .gt. numnp) .or. (ix(k,n) .lt. 0)) then
                write(iow,3002) n
                if(ior.lt.0) then
                  write(*,3002) n
                endif
                error = .true.
              endif
            end do
            ix(nen1,n)   = ix(nen1,n-1)
            ix(nen1-1,n) = nreg
            ix(nen+7 ,n) = nnty
            ix(nen+8 ,n) = nord
            fprt         = .true.

!         Transfer input to current element

          else if (l.eq.n) then
            ng = lg
            do k = 1, nen
              if ((idl(k) .gt. numnp) .or. (idl(k) .lt. 0)) then
                write(iow,3002) n
                if(ior.lt.0) then
                  write(*,3002) n
                endif
                error = .true.
              endif
              ix(k,l) = idl(k)
            end do
            if(nmat.eq.0) then
              ix(nen1,l) = lk
            else
              ix(nen1,l) = nmat
            endif
            ix(nen1-1,l) = nreg
            ix(nen+7 ,l) = nnty
            ix(nen+8 ,l) = nord
            fprt         = .true.
          endif

!         Output element list

          if ((prt) .and. (.not. error) .and. fprt) then
            if(mod(ma,50).eq.0) then
              call prtitl(prth)
              write(iow,2001) (k,k=1,nen)
              if(ior.lt.0) then
                write(*,2001) (k,k=1,nen)
              endif
            endif
            ma = ma + 1
            etype = pelabl(ix(nen+7,n))
            nel   = 0
            do k = 1,nen
              if(ix(k,i).gt.0) nel = k
            end do ! k
            write(iow,2002) n,ix(nen1,n),ix(nen1-1,n),etype,
     &                      (ix(k,n),k=1,nel)
            if(ior.lt.0) then
              write(*,2002) n,ix(nen1,n),ix(nen1-1,n),etype,
     &                      (ix(k,n),k=1,nel)
            endif
          endif
        end do
      end do

!     Formats

2001  format(5x,'E l e m e n t s'//3x,'Elmt Mat Reg  Type',
     &           7(i3,' Node'):/(21x,7(i3,' Node')))

2002  format(i7,2i4,1x,a5,7i8:/(21x,7i8))

2011  format(' Input: elmt#, matl#, (ix(i),i=1,nen), inc'/3x,'>')

3001  format(' *ERROR* Element',i5,' appears after element',i5)

3002  format(' *ERROR* Element',i5,' has illegal nodes')

4001  format(' *WARNING* PELMIN: No data found for an -> ELEM',
     &       ' <- data set.')

      end subroutine pelmin
