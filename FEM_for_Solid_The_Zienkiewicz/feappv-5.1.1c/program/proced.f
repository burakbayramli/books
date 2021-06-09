!$Id:$
      subroutine proced (pname,vv,wd,nwd,ll,jct,lct,ct,lzc,flg,iopl)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Define or execute a procedure for command language
!               solution

!      Inputs:
!         pname     - Procedure name (1-8 characters)
!         vv        - Character form of procedure parameters
!         wd(*)     - List of possible commands
!         nwd       - Number of possible commands in wd
!         ll        - Number of procedure command
!         ct(3,*)   - Parameters for procedure execution
!         flg       - Flag, true if procedure already exists
!         iopl      - Switch: Define procedure if = 1; else execute.

!      Outputs:
!         jct(*)    - List of commands to execute
!         lct(*)    - List of command options
!         lzc(*)    - Character form of command parameters
!-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'chdata.h'
      include  'iodata.h'
      include  'iofile.h'

      character (len=80) :: tx, lzc(*)
      character (len=18) :: fnam
      character (len=15) :: v(3),lct(*), vv(3)
      character (len=12) :: mac,pname
      character (len=4)  :: wd(nwd)

      logical       :: pcomp,flg,errck,tinput,vinput
      integer       :: nwd,ll,iopl, i,j,n

      integer       :: jct(*)
      real (kind=8) :: ct(3,*),va(3),vp(3)

      save

!     Set file name to store procedure

      mac = pname
      do i = 1,8
        if(pname(i:i).eq.' ') go to 100
      end do
      i = 9
100   pname(i:i+3) = '.pcd'

!     Move name to fnam to open file

      fnam = pname

!     Check if file exists and open file

      call opnfil(mac,fnam,iopl,ios,flg)

!     Write a new procedure

      if(iopl.eq.1) then

!       If not ignored by inputs

        if(mac(1:1).ne.'0') then

!         Write variable names onto file

          write(ios,2000) vv

!         Input procedure commands and add to file

          if(ior.lt.0) write(*,3000)
          n = 0
          j = 0
110       n = n + 1
          if(ior.lt.0) write(*,3001) n
          errck = tinput(tx,5,va,0)
          if(pcomp(tx,'loop',4))  j = j + 1
          if(pcomp(tx,'next',4))  j = j - 1
          if(j.lt.0) then
            if(ior.lt.0) then
              write(*,3002)
              j = j + 1
              n = n - 1
              go to 110
            else
              write(iow,3002)
              call plstop(.true.)
            endif
          endif
          if(pcomp(tx,'end ',4)) go to 130
          do i = 1,nwd
           if(pcomp(tx,wd(i),4)) go to 120
          end do
          if(ior.lt.0) then
            write(*,3003) tx
          else
            write(iow,3003) tx
            call plstop(.true.)
          endif
          n = n - 1
          go to 110
120       write(ios,2001) xxx
          go to 110
130       if(j.ne.0) then
            if(ior.lt.0) then
              write(*,3004)
              n = n - 1
              go to 110
            else
              write(iow,3004)
              call plstop(.true.)
            endif
          endif
          close(ios)
        endif

!     Read an existing procedure

      elseif(flg) then
        va(1) = ct(1,ll)
        va(2) = ct(2,ll)
        va(3) = ct(3,ll)
        ll = ll - 1
        read(ios,2000) v
200     read(ios,2001,end=300) xxx

!       Check for leading blanks

        do i = 1,256
          if(xxx(i:i).ne.' ') go to 210
        end do
        go to 200

210     call acheck(xxx(i:i),tx,15,81-i,75)

!       If second field is a number move to left of field

        if(tx(16:16).eq.' ' .and. tx(29:30).ne.' 0' ) then

         do i = 30,16,-1
           if(tx(i:i).eq.' ') go to 220
         end do
         i = 16
220      j = 29 - i
         tx(16:16+j) = tx(30-j:30)
         tx(17+j:30) = ' '
        endif

!       Check for parameters in statements

        if(.not.pcomp(v(1),'    ',4)) call setpcd(tx,v(1),vv(1))
        if(.not.pcomp(v(2),'    ',4)) call setpcd(tx,v(2),vv(2))
        if(.not.pcomp(v(3),'    ',4)) call setpcd(tx,v(3),vv(3))

!       Move to 'lzc' for execution of commands

        call setmac(tx,wd,nwd,ll,jct,lct,lzc)

!       Output new command

        if(ior.gt.0) then
          errck = vinput(tx(31:45),15,vp(1),1)
          errck = vinput(tx(46:60),15,vp(2),1)
          errck = vinput(tx(61:75),15,vp(3),1)
          write(iow,2002) tx(1:4),tx(16:29),vp
        endif

        go to 200
300     close(ios)
      endif

!     Formats

2000  format(3a15)

2001  format(a)

2002  format(7x,a4,1x,a14,1x,1p,3e12.4)

3000  format('  Procedure Definition - Terminate with "end". ')

3001  format('    Procedure Statement',i3,' >',$)

3002  format(' *ERROR* "loop" must precede "next" instruction')

3003  format(' *WARNING* Illegal command ',a4,' in procedure.')

3004  format(' *ERROR* "loop" instruction has no matching "next"'
     &      ,' instruction')

      end subroutine proced
