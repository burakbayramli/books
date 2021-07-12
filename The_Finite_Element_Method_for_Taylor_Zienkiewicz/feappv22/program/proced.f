c$Id:$
      subroutine proced (name,vv,wd,nwd,ll,jct,lct,ct,lzc,flg,iopl)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Define or execute a procedure for command language
c               solution

c      Inputs:
c         name      - Procedure name (1-8 characters)
c         vv        - Character form of procedure parameters
c         wd(*)     - List of possible commands
c         nwd       - Number of possible commands in wd
c         ll        - Number of procedure command
c         ct(3,*)   - Parameters for procedure execution
c         flg       - Flag, true if procedure already exists
c         iopl      - Switch: Define procedure if = 1; else execute.

c      Outputs:
c         jct(*)    - List of commands to execute
c         lct(*)    - List of command options
c         lzc(*)    - Character form of command parameters
c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'chdata.h'
      include  'iodata.h'
      include  'iofile.h'

      logical   pcomp,flg,errck,tinput,vinput
      integer   nwd,ll,iopl, i,j,n

      character mac*8,name*12,v(3)*15,wd(nwd)*4,lct(*)*15
      character tx*80
      character fnam*18, lzc*50, vv(3)*15
      integer   jct(*)
      real*8    ct(3,*),va(3),vp(3)

      save

c     Set file name to store procedure

      mac = name
      do i = 1,8
        if(name(i:i).eq.' ') go to 100
      end do
      i = 9
100   name(i:i+3) = '.pcd'

c     Move name to fnam to open file

      fnam = name

c     Check if file exists and open file

      call opnfil(mac,fnam,iopl,ios,flg)

c     Write a new procedure

      if(iopl.eq.1) then

c       If not ignored by inputs

        if(mac(1:1).ne.'0') then

c         Write variable names onto file

          write(ios,2000) vv

c         Input procedure commands and add to file

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
              call plstop()
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
            call plstop()
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
              call plstop()
            endif
          endif
          close(ios)
        endif

c     Read an existing procedure

      elseif(flg) then
        va(1) = ct(1,ll)
        va(2) = ct(2,ll)
        va(3) = ct(3,ll)
        ll = ll - 1
        read(ios,2000) v
200     read(ios,2001,end=300) xxx

c       Check for leading blanks

        do i = 1,255
          if(xxx(i:i).ne.' ') go to 210
        end do
        go to 200

210     call acheck(xxx(i:i),tx,15,81-i,75)

c       If second field is a number move to left of field

        if(tx(16:16).eq.' ' .and. tx(29:30).ne.' 0' ) then

         do i = 30,16,-1
           if(tx(i:i).eq.' ') go to 220
         end do
         i = 16
220      j = 29 - i
         tx(16:16+j) = tx(30-j:30)
         tx(17+j:30) = ' '
        endif

c       Check for parameters in statements

        if(.not.pcomp(v(1),'    ',4)) call setpcd(tx,v(1),vv(1))
        if(.not.pcomp(v(2),'    ',4)) call setpcd(tx,v(2),vv(2))
        if(.not.pcomp(v(3),'    ',4)) call setpcd(tx,v(3),vv(3))

c       Move to 'lzc' for execution of commands

        call setmac(tx,wd,nwd,ll,jct,lct,lzc)

c       Output new command

        if(ior.gt.0) then
          errck = vinput(tx(31:45),15,vp(1),1)
          errck = vinput(tx(46:60),15,vp(2),1)
          errck = vinput(tx(61:75),15,vp(3),1)
          write(iow,2002) tx(1:4),tx(16:29),vp
        endif

        go to 200
300     close(ios)
      endif

c     Formats

2000  format(3a15)

2001  format(a)

2002  format(7x,a4,1x,a14,1x,1p,3e12.4)

3000  format('  Procedure Definition - Terminate with "end". ')

3001  format('    Procedure Statement',i3,' >',$)

3002  format(' *ERROR* "loop" must precede "next" instruction')

3003  format(' *WARNING* Illegal command ',a4,' in procedure.')

3004  format(' *ERROR* "loop" instruction has no matching "next"'
     &      ,' instruction')

      end
