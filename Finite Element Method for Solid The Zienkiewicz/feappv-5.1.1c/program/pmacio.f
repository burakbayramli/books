!$Id:$
      subroutine pmacio (jct,lct,ct,wd,ed,nwd,nlp,nif,ll,prth)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Command language input subprogram

!      Inputs:
!         wd(*)     - List of command languate names
!         ed(*)     - Manual level indicators for commands
!         nwd       - Number of commands in wd
!         nlp       - Location of 'loop' command
!         nif       - Location of 'if' command
!         prth      - Flag, print title/header if true

!      Scratch:
!         ct(3,*)   - Values of command parameters

!      Outputs:
!         jct(ll)   - Command numbers to execute
!         lct(ll)   - Command options
!-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'chdata.h'
      include  'fdata.h'
      include  'idata1.h'
      include  'iofile.h'
      include  'prflag.h'

      character (len=15) :: lct(*),clab(2),vv(3)
      character (len=4)  :: wd(nwd)

      logical       :: pcomp,flg,errck,tinput,vinput,pflg,prth
      integer       :: nwd,nlp,nnx,ll,llo,l,i,lc,lo,j,k,is
      integer       :: nif,nel,nei, ncmds

      integer       :: jct(*),ed(nwd)
      real (kind=8) :: ct(3,*), sv(1), td(4)

      save

      data      ncmds / 200 /

!     Initiate read of command statements

      if(ior.gt.0) then
        call prtitl(prth)
        write(iow,2001)
      endif

!     Read command records

      ll      = 1
      nn      = nnn
      nnx     = nlp + 1
      nel     = nif + 1
      nei     = nif + 2
      jct(1)  = nlp
      ct(1,1) = 1.0
      ct(2,1) = 0.0
      flg     = .true.
100   if(ior.lt.0 .and. flg .and. pfr) then
        write(*,2002)
        write(*,2003)
      endif
      if(nn.gt.ncmds-1) nn = 0
      if(ll.gt.ncmds-5.and.ior.lt.0) write(*,3001) ncmds,ncmds
      ll = ll + 1
      if(ll.gt.ncmds) then
        write (*,3002) ncmds
        ll = -1
        return
      endif

!     Input command

      clab(1) = ' '
      do while( pcomp(clab(1),' ',1) )
        if(ior.lt.0) write(*,2004) nn+1,ll-1
        errck = tinput(clab,2,td,4)
        ct(1,ll) = td(1)
        ct(2,ll) = td(2)
        ct(3,ll) = td(3)
      end do ! while
      lzz(ll) = zzz
      pflg    = ior.gt.0
      if(ior.lt.0) then

!       Help: Show active command set

        if(pcomp(clab(1),'help',4)) then
          call phelp(clab(2),wd,ed,nwd,'MACR')
          ll = ll - 1
          go to 100

!       Repeat previous or match command

        elseif(pcomp(clab(1),'!',1)) then
          call histex(wd,clab(1),jct,lct,ct,nwd,nlp,nnx,ll,is)
          if(is.eq.0) go to 130
          ll = ll - 1
          go to 100
        endif
      endif

!     Hist: Perform set of commands from history or show/edit

      llo = ll

      if(pcomp(clab(1),'hist',4)) then

         call phist(wd,clab(2),jct,lct,ct,ll,is)
         nnn = nn
         if(is.eq.0) then
            go to 140
         else
            ll = ll - 1
            go to 100
         endif

!     Proc: Enter or execute procedure

      elseif(pcomp(clab(1),'proc',4)) then

         vv(1) = yyy(31:45)
         vv(2) = yyy(46:60)
         vv(3) = yyy(61:75)
         call proced(clab(2),vv,wd,nwd,ll,jct,lct,ct,lzz,flg,1)
         ll = ll - 1
         go to 100

      endif

!     End: Quit (q), Exit (e) or Stop (s)

      if(ior.lt.0) then

        if(pcomp(clab(1),'quit',4).or.pcomp(clab(1),'q   ',4)) then
          ll = -2
          go to 210
        elseif(pcomp(clab(1),'exit',4)
     &     .or.pcomp(clab(1),'e   ',4)) then
          write(*,2006)
          errck = tinput(clab,1,sv(1),0)
          if(pcomp(clab(1),'y',1)) then
            ll = -1
            go to 210
          else
            go to 100
          endif
        elseif(pcomp(clab(1),'stop',4)
     &     .or.pcomp(clab(1),'s   ',4)) then
          if(ior.lt.0) then
            call pprint('   Stop execution? (y or n) :')
            errck = tinput(clab,1,sv,0)
            if(pcomp(clab(1),'y',1)) then
              call plstop(.false.)
            endif
          endif
        endif
      elseif(pcomp(clab(1),'end ',4)) then
        go to 140
      endif

!     Set execution flag

      lo      = ll
      lct(ll) = clab(2)
      do j = 1,nwd
        if(pcomp(clab(1),wd(j),4)) then
          jct(ll) = j
          go to 120
        endif
      end do

!     Look at existing procedures

      vv(1) = yyy(31:45)
      vv(2) = yyy(46:60)
      vv(3) = yyy(61:75)
      call proced(clab(1),vv,wd,nwd,ll,jct,lct,ct,lzz,flg,2)
      pflg = .false.

!     Error no procedure with name clab(1)

      if(.not.flg) then
        call errclr('PMACIO')
        ll = ll - 1
        go to 100
      endif

!     Save information for history

120   if (hadd) then
        do lc = lo,ll
          nn = nn + 1
          js(nn)  = jct(lc)
          ljs(nn) = lct(lc)
          lzs(nn) = lzz(lc)
        end do
      endif
      nnn = nn
      if(pflg) then
        write(iow,2000) clab,ct(1,ll),ct(2,ll),ct(3,ll)
      endif
      if (ior.gt.0) go to 100
130   ll = ll + 1
140   jct(ll)= nnx

!     Check loop/next pairs

      j = 0
      do l = 1,ll
        if(jct(l).eq.nlp) j = j + 1
        if(jct(l).eq.nnx) j = j - 1
        if(j.lt.0) then
          if(ior.gt.0) then
            go to 402
          else
            ll  = ll - 2
            flg = .false.
            write(*,4002)
            go to 100
          endif
        endif
      end do
      if(j.ne.0) then
        if(ior.gt.0) then
          go to 400
        else
          ll = ll - 1
          flg = .false.
          go to 100
        endif
      endif

!     Set loop/next markers

      flg = .true.
      do l = 1,ll-1
        if(jct(l).eq.nlp) then
          j     = 1
          k     = l + 1
          sv(1) = ct(2,l)
          do i = k,ll
            if(jct(i).eq.nlp) j = j + 1
            if(j.gt.9) go to 401
            if(jct(i).eq.nnx) j = j - 1
            if(j.eq.0) go to 200
          end do
          go to 400

!         Store loop information in 'lzz'

200       ct(1,i) = 0.0d0
          ct(2,i) = l
          ct(3,i) = sv(1)
          write(zzz,2005) (ct(k,i),k=1,3)
          lzz(i)  = zzz

          errck   = vinput(lzz(l),80,ct(1,l),3)
          ct(2,l) = i
          write(zzz,2005) (ct(k,l),k=1,3)
          lzz(l)  = zzz

        endif
      end do ! l
!     Check if/endif pairs

      j = 0
      do l = 1,ll
        if(jct(l).eq.nif) j = j + 1
        if(jct(l).eq.nei) j = j - 1
        if(j.lt.0) then
          if(ior.gt.0) then
            go to 403
          else
            ll  = ll - 2
            flg = .false.
            write(*,4003)
            go to 100
          endif
        endif
      end do ! l
      if(j.ne.0) then
        if(ior.gt.0) then
          go to 404
        else
          ll = ll - 1
          flg = .false.
          go to 100
        endif
      endif

!     Set if/else/endif markers

      flg = .true.
      do l = 1,ll-1
        if(jct(l).eq.nif .or.jct(l).eq.nel) then

!         Locate the matching endif command

          j  = 1
          k  = 0
          do i = l+1,ll
            if(jct(i).eq.nel .and. k.eq.0) then
              k = i
            endif
            if(jct(i).eq.nif) j = j + 1
            if(j.gt.9) go to 401
            if(jct(i).eq.nei) j = j - 1
            if(j.eq.0) go to 205
          end do ! i
          go to 404

!         Store endif information in 'lzz'

205       errck   = vinput(lzz(l),80,ct(1,l),3)
          if(k.eq.0) then
            ct(2,l) = i - 1
          else
            ct(2,l) = k - 1
          endif
          ct(3,l) = i - 1
          write(zzz,2005) (ct(k,l),k=1,3)
          lzz(l)  = zzz

        endif
      end do ! l

!     Exit

210   return

!     Error messages

400   write(iow,4000)
      if(ior.lt.0) write(*,4000)
      call plstop(.true.)

401   write(iow,4001)
      if(ior.lt.0) write(*,4001)
      call plstop(.true.)

402   write(iow,4002)
      if(ior.lt.0) write(*,4002)
      call plstop(.true.)

403   write(iow,4003)
      if(ior.lt.0) write(*,4003)
      call plstop(.true.)

404   write(iow,4004)
      if(ior.lt.0) write(*,4004)
      call plstop(.true.)

!     Formats

2000  format(7x,a9,1x,a9,1x,1p,3e12.4)

2001  format(5x,'Solution Commands',7x,'Variable 1  Variable 2  ',
     &      'Variable 3')

2002  format(' Input a solution command: Enter "help" for list of ',
     & 'commands.')
2003  format(' Enter "exit" to end with restart save, "quit" to ',
     & 'end without restart save.')

2004  format('   List',i3,'  Command',i3,'> ',$)

2005  format(1p,3e15.7)

2006  format('   Exit with restart file save (y or n) :',$)

3001  format(' *WARNING* Maximum number of command statements =',i4
     &     ,/'    Use history edit to reduce or program will stop'
     &     ,' when',i4,' is reached')

3002  format(' *ERROR* PMACIO: Maximum number of command instructions',
     &       '                  is limited to',i4)

4000  format(' *ERROR* PMACIO: Unbalanced loop/next commands')

4001  format(' *ERROR* PMACIO: Loops nested deeper than 8')

4002  format(' *ERROR* PMACIO: "loop" must precede "next" instruction')

4003  format(' *ERROR* PMACIO: "if" must precede "endi"f instruction')

4004  format(' *ERROR* PMACIO: Unbalanced if/endif commands')

      end subroutine pmacio
