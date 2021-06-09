!$Id:$
      logical function setmem(list,mlist,rlist,
     &                        num,mname,length,precis)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Define, delete, or resize a dictionary entry.
!               Pointer defined for integer (single) and real
!               (double precision arrays.

!      Inputs:
!         list       - Number of entries in variables
!         mlist(2,*) - Location entries for defined arrays
!         rlist(*)   - Admissible names for arrays
!         num        - Entry number for array (see below)
!         mname      - Name of array          (see below)
!         length     - Length of array defined: =0 for delete
!         precis     - Precision of array: 1 = integers; ipr = reals
!                      N.B. if ipr = 1, all arrays padded by one word
!                           to pervent overlaps.

!      Outputs:
!         np(num)    - Pointer to first word of array in blank common
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'allotd.h'
      include  'allotn.h'
      include  'cdata.h'
      include  'iodata.h'
      include  'iofile.h'
      include  'errchk.h'
      include  'pointer.h'
      include  'comblk.h'
      include  'w_int.h'

      include  'p_point.h'

      character (len=5) :: dname, rlist(list)
      character         :: mname*(*)

      logical       :: pcomp
      integer       :: list,num,length,precis,ip,ipa, n,i, iot, lensav
      integer       :: dicloc, mlist(2,list), irp(2,2)

      save

      data      irp / 8,8,4,8 /, iot / 9 /

!     Find variable

      mmax = 0

      dname  = mname
      n      = max(1,min(num,list))

!     Check match of number with name of array

      if(pcomp(dname,rlist(n),5)) then

!       Set pointer for arrays stored in Blank Common

        setmem = .true.

        if(mlist(1,n).eq.0) then
          if(length.le.0) then
            write(  *,*) '  *WARNING* Length allocation for:',dname,
     &                   ' Length =',length
            write(iow,*) '  *WARNING* Length allocation for:',dname,
     &                   ' Length =',length
          endif
          ip     = abs(precis)
          ipa    = irp(ip,ipr)

!         Use Malloc to allocate space for length*ipa bytes

          adr(n) = malloc(length*ipa)

!         Set pointer for array use

          if(ip.eq.1) then
            np(n) = 1 + (adr(n) - loc(mr(1))) / ipa
          else
            np(n) = 1 + (adr(n) - loc(hr(1))) / ipa
          endif

!         Add new array into dictionary

          ndict      = ndict+1
          mlist(1,n) = ndict
          mlist(2,n) = length

          if(ndict.le.200) then
            dict(ndict)   = dname
            ipoint(ndict) = length
            iprec(ndict)  = ip
            dlist(ndict)  = n
            if(num.le.llist) then
              ddict(ndict) = num
            else
              ddict(ndict) = num-llist
            endif
            pdict(ndict) = num
          else
            write(iow,2001) dname
            if(ior.lt.0) then
              write(*,2001) dname
            endif
          endif

!         Memory allocation available - Initialize to zero

          if(adr(n).ne.0) then
            if(ip.eq.1) then
              call pzeroi(mr(np(n)),length)
            else
              call pzero (hr(np(n)),length)
            endif

!         Sufficient memory does not exist - Write ERROR message and STOP

          else
            write(iow,2000) mname,length
            if(ior.lt.0) then
              write(*,2000) mname,length
            endif
            if(eralloc) then
              setmem = .false.
            else
              call plstop(.true.)
            endif

          endif

!       Pointer already exists: Delete if length = 0;

        elseif(length.eq.0) then

          dicloc = mlist(1,n)

          call free(adr(n))

          np(n)      = 0
          mlist(1,n) = 0
          mlist(2,n) = 0

          do i = dicloc,ndict-1
            dict(i)           = dict(i+1)
            ipoint(i)         = ipoint(i+1)
            iprec(i)          = iprec(i+1)
            dlist(i)          = dlist(i+1)
            ddict(i)          = ddict(i+1)
            pdict(i)          = pdict(i+1)
            mlist(1,dlist(i)) = mlist(1,dlist(i)) - 1
          end do ! i

!         Set last entry and reduce entries in dictionary

          dict(ndict)   = '     '
          ipoint(ndict) = 0
          dlist(ndict)  = 0
          ddict(ndict)  = 0
          pdict(ndict)  = 0
          iprec(ndict)  = 0

!         Reset dictionary lengths

          ndict = ndict - 1

!       Pointer already exists: resize in place

        elseif(length.ne.mlist(2,n)) then

          dicloc  = mlist(1,n)

!         Expand array if space available

          ip    =  abs(precis)
          ipa   =  irp(ip,ipr)

          if(length.gt.mlist(2,n)) then
            lensav = mlist(2,n)
          else
            lensav = length
          endif

!         Save current values

          open (unit = iot, file = 'scratch', form = 'unformatted')
          if(ip.eq.1) then
            write(iot) (mr(point),point = np(n),np(n)+lensav-1)
          else
            write(iot) (hr(point),point = np(n),np(n)+lensav-1)
          endif
          call free(adr(n))
          adr(n) = malloc(length*ipa)
          if(adr(n).ne.0) then
            rewind iot
            if(ip.eq.1) then
              np(n) = 1 + (adr(n) - loc(mr(1))) / ipa
              read(iot) (mr(point),point = np(n),np(n)+lensav-1)
              do i = mlist(2,n),length-1
                mr(np(n)+i) = 0
              end do ! i
            else
              np(n) = 1 + (adr(n) - loc(hr(1))) / ipa
              read(iot) (hr(point),point = np(n),np(n)+lensav-1)
              do i = mlist(2,n),length-1
                hr(np(n)+i) = 0.0d0
              end do ! i
            endif
          endif
          close(iot,status = 'delete')

!         Set new pointers

          mlist(2,n)      = length
          ipoint(dicloc)  = length

!         Cannot expand array, not enough space available

          if(adr(n).eq.0) then
            write(iow,2000) mname,length
            if(ior.lt.0) then
              write(*,2000) mname,length
            endif
            if(eralloc) then
              setmem = .false.
            else
              call plstop(.true.)
            endif
          endif

        endif

      else

!       Error indicator

        setmem = .false.
        if(num.le.llist) then
          write(  *,3000) num,dname
          write(iow,3000) num,dname
        else
          write(  *,3001) num-llist,dname
          write(iow,3001) num-llist,dname
        endif

      endif

!     Formats

2000  format(' **ERROR** Insufficient storage to allocate ',a/,
     &              11x,'Required size =',i12/,
     &       '           Check data or choose other option.')

2001  format(' *ERROR* No more room in dictionary for ',a5)

3000  format(' *ERROR* No allocation for array number',i4,' named: ',a)

3001  format(' *ERROR* No allocation for user array number',i4,
     &       ' named: ',a)

      end function setmem
