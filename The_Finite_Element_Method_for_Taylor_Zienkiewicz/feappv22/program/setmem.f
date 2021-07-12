c$Id:$
      logical function setmem(list,mlist,rlist,
     &                        num,name,length,precis)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Define, delete, or resize a dictionary entry.
c               Pointer defined for integer (single) and real
c               (double precision arrays.

c      Inputs:
c         list       - Number of entries in variables
c         mlist(2,*) - Location entries for defined arrays
c         rlist(*)   - Admissible names for arrays
c         num        - Entry number for array (see below)
c         name       - Name of array          (see below)
c         length     - Length of array defined: =0 for delete
c         precis     - Precision of array: 1 = integers; ipr = reals
c                      N.B. if ipr = 1, all arrays padded by one word
c                           to pervent overlaps.

c      Outputs:
c         np(num)    - Pointer to first word of array in blank common
c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'allotd.h'
      include  'allotn.h'
      include  'cdata.h'
      include  'iofile.h'
      include  'psize.h'
      include  'debugs.h'
      include  'pointer.h'
      include  'comblk.h'

      logical   pcomp
      character name*(*),dname*5
      integer   list,num,length,precis,ip,ipa,n
      integer   dicloc,ddiff,idiff,dmin,dmax

      integer   mlist(2,list), irp(2,2)
      character rlist(list)*5
      logical   reduce

      save

      data      irp / 3*1, 2 /

c     Find variable

100   dname  = name
      n      = max(1,min(num,list))

c     Set flag to not reduce array

      reduce = .false.

      if(pcomp(dname,rlist(n),5)) then

c       Set pointer for arrays stored in Blank Common

        if(mlist(1,n).eq.0) then
          if(length.le.0) then
            write(  *,*) '  *WARNING* Length allocation for:',dname,
     &                   ' Length =',length
            write(iow,*) '  *WARNING* Length allocation for:',dname,
     &                   ' Length =',length
          endif
          ip    =  abs(precis)
          ipa   =  irp(ip,ipr)
          np(n) = (mmax + ipa - 1)/ipa - ipr*(2 - ip)
          mmax  =  mmax + length*ipa + mod(length*ipa,ipr)

c         Add new array into dictionary

          ndict      = ndict+1
          mlist(1,n) = ndict
          mlist(2,n) = length

          if(ndict.le.200) then
            dict(ndict)     = dname
            ipoint(ndict+1) = mmax
            iprec(ndict)    = ip
            dlist(ndict)    = n
            if(num.le.llist) then
              ddict(ndict)  = num
            else
              ddict(ndict)  = num-llist
            endif
          else
            write(iow,2001) dname
            if(ior.lt.0) then
              write(*,2001) dname
            endif
          endif

c         Memory allocation available - Initialize to zero

          if(mmax.le.maxm) then
            if(precis.eq.1) then
              call pzeroi(mr(np(n)),length)
            elseif(precis.eq.2) then
              call pzero (hr(np(n)),length)
            endif

c         Sufficient memory does not exist - Write ERROR message and STOP

          else
            write(iow,2000) mmax,maxm
            if(ior.lt.0) then
              write(*,2000) mmax,maxm
            endif
            call plstop()

          endif

c       Pointer already exists: resize in place

        elseif(length.gt.mlist(2,n)) then

          dicloc     = mlist(1,n)
          idiff      = (length    + mod(length,ipr)
     &               - mlist(2,n) - mod(mlist(2,n),ipr))*abs(precis)
          ddiff      = idiff/ipr
          dmin       = ipoint(dicloc+1) - 2
          dmax       = ipoint( ndict+1) - 2

c         Expand array if space available

          if(mmax+idiff.le.maxm) then
            mlist(2,n) = length
            do n = dmax,dmin,-1
              mr(n+idiff) = mr(n)
              if(precis.lt.0) then
                mr(n)     = 0
              endif
            end do

            do n = dicloc+1,ndict
              ipoint(n)    = ipoint(n) + idiff
              if    (iprec(n).eq.1) then
                np(dlist(n)) = np(dlist(n)) + idiff
              elseif(iprec(n).eq.2) then
                np(dlist(n)) = np(dlist(n)) + ddiff
              endif
            end do
            ipoint(ndict+1) = ipoint(ndict+1) + idiff
            mmax            = mmax + idiff

c         Cannot expand array, not enough space available

          else
            write(iow,2000) mmax,maxm
            if(ior.lt.0) then
              write(*,2000) mmax,maxm
            endif
            call plstop()
          endif

c       Pointer already exists: Delete if length = 0;
c                               Delete if length < oldlength - incred,
c                               then set flag to reallocate later.

        elseif(length.le.0 .or. (length+incred).lt.mlist(2,n)) then

          if(length.le.0) then
            reduce = .false.
          else
            reduce = .true.
          endif

          dicloc     =  mlist(1,n)
          idiff      =  ipoint(dicloc+1) - ipoint(dicloc)
          ddiff      =  idiff/ipr

          dmin       =  ipoint(dicloc+1) -2
          dmax       =  mmax - 3

          np(n)      = 0
          mlist(1,n) = 0
          mlist(2,n) = 0

          do n = dmin,dmax
            mr(n-idiff) = mr(n)
          end do

          do n = dicloc,ndict-1
            dict(n)     = dict(n+1)
            ipoint(n)   = ipoint(n+1) - idiff
            iprec(n)    = iprec(n+1)
            dlist(n)    = dlist(n+1)
            ddict(n)    = ddict(n+1)
            if(iprec(n).eq.1) then
              np(dlist(n)) = np(dlist(n)) - idiff
            elseif(iprec(n).eq.2) then
              np(dlist(n)) = np(dlist(n)) - ddiff
            endif
            mlist(1,dlist(n)) = mlist(1,dlist(n)) - 1
          end do

c         Set last entry and reduce entries in dictionary

          dict(ndict)     = '     '
          ipoint(ndict)   = ipoint(ndict+1) - idiff
          ipoint(ndict+1) = 0
          dlist(ndict)    = 0
          ddict(ndict)    = 0
          iprec(ndict)    = 0

c         Reset dictionary lengths

          ndict = ndict - 1
          mmax  = mmax  - idiff

        endif

        setmem = .true.

      else

c       Error indicator

        setmem = .false.
        if(num.le.llist) then
          write(  *,3000) num,dname
          write(iow,3000) num,dname
        else
          write(  *,3001) num-llist,dname
          write(iow,3001) num-llist,dname
        endif

      endif

c     Reallocate array if the size is being reduced
c     N.B. Original values are destroyed.

      if(reduce) go to 100

c     Formats

2000  format(' *ERROR* Insufficient space to run this problem.',/,
     &            17x,'Required  =',i8,/,17x,'Available =',i8,/,
     &       '         Check data or reset parameter MMAX in main',
     &       ' program')

2001  format(' *ERROR* No more room in dictionary for ',a5)

3000  format(' *ERROR* No allocation for array number',i4,' named: ',a)

3001  format(' *ERROR* No allocation for user array number',i4,
     &       ' named: ',a)

      end


