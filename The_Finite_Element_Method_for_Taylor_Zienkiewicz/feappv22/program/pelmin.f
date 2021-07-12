c$Id:$
      subroutine pelmin(tx,idl,ix,nen1,prt,prth,error)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Data input routine for element connections

c      Inputs:
c         tx        - Option identifier
c         nen1      - Dimension for ix array
c         prt       - Flag, print input data if true
c         prth      - Flag, print title/header if true

c      Scratch:
c         idl(*)    - Local degree of freedom integer data

c      Outputs:
c         ix(*)     - Element nodal connection lists
c         error     - True if error occurs during input
c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'cblktr.h'
      include  'cdata.h'
      include  'chdata.h'
      include  'iofile.h'
      include  'region.h'

      logical   prt,prth,error,errck,fprt,genfl
      logical   pcomp,pinput,vinput
      character tx*15,mtype*69
      integer   i,j,k,l,n,nen1
      integer   ii,il,is,ilx,lg,lk,llx,ma,ng

      integer   ixg(16)
      integer   idl(*),ix(nen1,*)
      real*8    td(16)

      save

c     Check for input format descriptor

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
          ixg(n) = td(n)
        end do
        if(prt) call iprint(ixg,1,nen,1,'Element Generation Array')
        genfl = .true.
      else
        genfl = .false.
      endif

c     Perform input of data for element connections

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

c           Input element records - N.B. limit is 16 nos. / record

            il = min(nen+3,16)
201         errck = pinput(td,il)
            if(errck) go to 201

c           Discontinue input of elements on blank record or negative number.

            l  = td(1)
            if(l .le. 0) then
              return
            else
              neo = max(neo,l)
            endif

c           Set element group number

            is = td(2)

c           Transfer to element integer data

            do k = 3,il
              idl(k-2) = td(k)
            end do

c           Input additional records

            do ii = 14,nen,16
              il = min(nen+1-ii,16)
202           errck = pinput(td,il)
              if(errck) go to 202

c             Transfer to element integer data

              do k = 1,il
                idl(ii+k) = td(k)
              end do
            end do

c           Check if old sequence

            if(pcomp(tx,'old',3)) then

              lg = idl(nen+1)
              lk = is

c           Else change order

            else

              lg = is
              lk = idl(1)
              do k = 1,nen
                idl(k) = idl(k+1)
              end do

            endif
            if (lg .eq. 0) then
              lg = 1
            endif
            ilx = lg
          endif

c         Error in input data

          if (l .lt. n) then
            write(iow,3001) l,n
            if(ior.lt.0) then
              write(*,3001) l,n
            endif
            error = .true.

c         Generate missing elements

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
            fprt         = .true.

c         Transfer input to current element

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
            ix(nen1,l)   = lk
            ix(nen1-1,l) = nreg
            fprt         = .true.
          endif

c         Output element list

          if ((prt) .and. (.not. error) .and. fprt) then
            if(mod(ma,50).eq.0) then
              call prtitl(prth)
              write(iow,2001) (k,k=1,nen)
              if(ior.lt.0) then
                write(*,2001) (k,k=1,nen)
              endif
            endif
            ma = ma + 1
            write(iow,2002) n,ix(nen1,n),ix(nen1-1,n),(ix(k,n),k=1,nen)
            if(ior.lt.0) then
              write(*,2002) n,ix(nen1,n),ix(nen1-1,n),(ix(k,n),k=1,nen)
            endif
          endif
        end do
      end do

c     Formats

2001  format(5x,'E l e m e n t s'//3x,'Elmt Mat Reg',
     &           8(i3,' Node'):/(15x,8(i3,' Node')))

2002  format(i7,2i4,8i8:/(15x,8i8))

2011  format(' Input: elmt#, matl#, (ix(i),i=1,nen), inc'/3x,'>')

3001  format(' *ERROR* Element',i5,' appears after element',i5)

3002  format(' *ERROR* Element',i5,' has illegal nodes')

      end
