!$Id:$
      subroutine phist(wd,clab2,jct,lct,ct,ll,is)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Controls history lists for command language execution

!      Inputs:
!         wd(*)    - List of command language options
!         clab2    - option for history request
!         ct(3,*)  - Numerical value of command option request

!      Outputs:
!         jct(*)   - Stores list of commands for history
!         lct(*)   - Stores list of options to commands for history
!         ll       - Number of active comands
!         is       - Output: = 0 for command execution, otherwise = 1
!-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'chdata.h'
      include  'comfil.h'
      include  'idata1.h'
      include  'iodata.h'
      include  'prflag.h'

      character (len=15) :: lct(*)
      character (len=4)  :: wd(*),clab2
      character (len=1)  :: y

      logical       :: errck, vinput, cinput
      integer       :: ll,is,n,i
      integer       :: jct(*)
      real (kind=8) :: ct(3,*), vjs(3)

      save

      is = 1
      nl1 = nint(abs(ct(1,ll)))
      nl2 = nint(abs(ct(2,ll)))
      if  (clab2.eq.'read') then
        open(ios,file='feap.his',status='unknown')
        do n = 1,200
          read(ios,1000,end=110) js(n),ljs(n),lzs(n)
        end do
110     nn = n - 1
        close(ios)
      elseif (clab2.eq.'save') then
        open(ios,file='feap.his',status='unknown')
        rewind ios
        do n = 1,nn
          write(ios,1000) js(n),ljs(n),lzs(n)
        end do
        close(ios)
      elseif (clab2.eq.'list'.or.
     &       (clab2.eq.'    '.and.nl1+nl2.eq.0)) then
        nl1 = min(nn,max(1,nl1))
        if(nl1.gt.0) then
          nl2 = max(nl1,min(nn,nl2))
          if(ct(1,ll).eq.0.d0.and.ct(2,ll).eq.0.d0) nl2 = nn
          write(*,2000)
          do n = nl1,nl2
            errck = vinput(lzs(n),80,vjs,3)
            write(*,2001) n,wd(js(n)),ljs(n),(vjs(i),i=1,3)
          end do
        else
          write(*,3001) nn
        endif
      elseif (clab2.eq.'edit') then
        errck = vinput(lzs(nl1),80,vjs,3)
        write(*,2002) nl1,wd(js(nl1)),ljs(nl1),(vjs(i),i=1,3)
!        read(*,1001) y
        if(.not.cinput()) then
          write(*,*) 'CINPUT error in PHIST'
        end if
        y = record(1:1)
        if(y.eq.'y' .or. y.eq.'Y') then
          if(nl1.gt.0.and.nl1.le.nn) then
            nl2 = max(nl1,min(nn,nl2))
            nl1 = nl2 - nl1 + 1
            if(nl2.lt.nn) then
              do n = nl2+1,nn
                js(n - nl1) = js(n)
                ljs(n - nl1) = ljs(n)
                lzs(n - nl1) = lzs(n)
              end do
            endif
            nn = nn - nl1
          else
            write(*,2005) nn
          endif
        endif
      elseif (clab2.eq.'add ') then
        hadd = .true.
      elseif (clab2.eq.'noad') then
        hadd = .false.
      else
        if(nl1.gt.0.and.nl1.le.nn) then
          nl2 = max(nl1,min(nn,nl2))
          do n = nl1,nl2
            jct(ll) = js(n)
            lct(ll) = ljs(n)
            lzz(ll) = lzs(n)
            errck   = vinput(lzz(ll),80,ct(1,ll),3)
            ll = ll + 1
          end do
          is = 0
        else
          write(*,3002) nn
        endif
      endif

!     Formats

1000  format(i5,1x,a15,a50)

!1001  format(a1)

2000  format(' No. Macro Option',11x,'value-1     value-2     value-3')

2001  format(i4,1x,a4,1x,a15,1p,3e12.4)

2002  format('  Remove command:'/i9,1x,a4,1x,a15,1p,3e12.4/
     &       3x,'(y or n)? >',$)

2005  format(' *ERROR* Not that many items in list, nn =',i4)

3001  format(' *ERROR* No items in list, nn = ',i3)

3002  format(5x,'Currently history list contains',i4,' items.'/
     &     7x,'Options: hist,list,n1,n2 - list items n1 to n2'/
     &    16x,'hist            - list all items'/
     &    16x,'hist,,n1,n2     - execute items n1 to n2'/
     &    16x,'hist,,n1        - execute item n1'/
     &    16x,'hist,edit,n1    - remove item n1'/
     &    16x,'hist,add        - add new macros to list'/
     &    16x,'hist,noad       - do not add macros to list'/
     &    16x,'hist,save       - save current list on disk'/
     &    16x,'hist,read       - read current list on disk'/
     &    34x,'(file = feap.his)'/1x)

      end subroutine phist
