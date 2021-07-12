c$Id:$
      subroutine pmacio (jct,lct,ct,wd,ed,nwd,nlp,ll,prth)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Macro instruction input subprogram

c      Inputs:
c         wd(*)     - List of command languate names
c         ed(*)     - Manual level indicators for commands
c         nwd       - Number of commands in wd
c         nlp       - Location of 'loop' command
c         prth      - Flag, print title/header if true

c      Scratch:
c         ct(3,*)   - Values of command parameters

c      Outputs:
c         jct(ll)   - Command numbers to execute
c         lct(ll)   - Command options
c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'chdata.h'
      include  'fdata.h'
      include  'idata1.h'
      include  'iofile.h'
      include  'prflag.h'

      logical   pcomp,flg,errck,tinput,vinput,pflg,prth
      integer   nwd,nlp,nnx,nnn,ll,llo,l,i,lc,lo,j,k,is
      real*8    sv

      character lct(*)*15,wd(nwd)*4,clab(2)*15,vv(3)*15
      integer   jct(*),ed(nwd)
      real*8    ct(3,*)

      save

      data nnn/0/

c     Initiate read of command statements

      if(ior.gt.0) then
        call prtitl(prth)
        write(iow,2001)
      endif

c     Read command records

      ll = 1
      nn = nnn
      nnx = nlp + 1
      jct(1) = nlp
      ct(1,1) = 1.0
      ct(2,1) = 0.0
      flg = .true.
100   if(ior.lt.0 .and. flg .and. pfr) then
        write(*,2002)
        write(*,2003)
      endif
      if(nn.gt.199) nn = 0
      if(ll.gt.195.and.ior.lt.0) write(*,3001)
      ll = ll + 1
      if(ll.gt.200) then
        write (*,3002)
        ll = -1
        return
      endif

c     Input command

      clab(1) = ' '
      do while( pcomp(clab(1),' ',1) )
        if(ior.lt.0) write(*,2004) nn+1,ll-1
        errck = tinput(clab,2,ct(1,ll),3)
      end do ! while
      lzz(ll) = zzz
      pflg    = ior.gt.0
      if(ior.lt.0) then

c       Help: Show active command set

        if(pcomp(clab(1),'help',4)) then
          call phelp(clab(2),wd,ed,nwd,'MACR')
          ll = ll - 1
          go to 100

c       Repeat previous or match command

        elseif(pcomp(clab(1),'!',1)) then
          call histex(wd,clab(1),jct,lct,ct,nwd,nlp,nnx,ll,is)
          if(is.eq.0) go to 130
          ll = ll - 1
          go to 100
        endif
      endif

c     Hist: Perform set of commands from history or show/edit

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

c     Proc: Enter or execute procedure

      elseif(pcomp(clab(1),'proc',4)) then

         vv(1) = yyy(31:45)
         vv(2) = yyy(46:60)
         vv(3) = yyy(61:75)
         call proced(clab(2),vv,wd,nwd,ll,jct,lct,ct,lzz,flg,1)
         ll = ll - 1
         go to 100

      endif

c     End: Quit (q) or Exit (e)

      if(ior.lt.0) then

        if(pcomp(clab(1),'quit',4).or.pcomp(clab(1),'q   ',4)) then
          ll = -2
          go to 210
        elseif(pcomp(clab(1),'exit',4)
     &     .or.pcomp(clab(1),'e   ',4)) then
          write(*,2006)
		  errck = tinput(clab,1,sv,0)
		  if(pcomp(clab,'y',1)) then
            ll = -1
            go to 210
          else
            go to 100
		  endif
        endif
      elseif(pcomp(clab(1),'end ',4)) then
        go to 140
      endif

c     Set execution flag

      lo      = ll
      lct(ll) = clab(2)
      do j = 1,nwd
        if(pcomp(clab(1),wd(j),4)) then
          jct(ll) = j
          go to 120
        endif
      end do

c     Look at existing procedures

      vv(1) = yyy(31:45)
      vv(2) = yyy(46:60)
      vv(3) = yyy(61:75)
      call proced(clab(1),vv,wd,nwd,ll,jct,lct,ct,lzz,flg,2)
      pflg = .false.

c     Error no procedure with name clab(1)

      if(.not.flg) then
        call errclr('PMACIO')
        ll = ll - 1
        go to 100
      endif

c     Save information for history

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

c     Check loop/next pairs

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

c     Set loop/next markers

      flg = .true.
      do l = 1,ll-1
        if(jct(l).eq.nlp) then
          j  = 1
          k  = l + 1
          sv = ct(2,l)
          do i = k,ll
            if(jct(i).eq.nlp) j = j + 1
            if(j.gt.9) go to 401
            if(jct(i).eq.nnx) j = j - 1
            if(j.eq.0) go to 200
          end do
          go to 400

c         Store loop information in 'lzz'

200       ct(1,i) = 0.0d0
          ct(2,i) = l
          ct(3,i) = sv
          write(zzz,2005) (ct(k,i),k=1,3)
          lzz(i)  = zzz

          errck   = vinput(lzz(l),50,ct(1,l),3)
          ct(2,l) = i
          write(zzz,2005) (ct(k,l),k=1,3)
          lzz(l)  = zzz

        endif
      end do

210   return

c     Error messages

400   write(iow,4000)
      if(ior.lt.0) write(*,4000)
      call plstop()

401   write(iow,4001)
      if(ior.lt.0) write(*,4001)
      call plstop()

402   write(iow,4002)
      call plstop()

c     Formats

2000  format(7x,a4,1x,a14,1x,1p,3e12.4)

2001  format('  M a c r o   I n s t r u c t i o n s'//
     & '  Macro Statement',12x,'Variable 1  Variable 2  Variable 3')

2002  format(' Input a macro instruction: Enter "help" for list of ',
     & 'commands.')
2003  format(' Enter "exit" to end with restart save, "quit" to ',
     & 'end without restart save.')

2004  format('   List',i3,'  Macro',i3,'> ',$)

2005  format(1p,3e15.7)

2006  format('   Exit with restart file save (y or n) :',$)

3001  format(' *WARNING* Maximum number of macro statements = 200'
     &     ,/'    Use history edit to reduce or program will stop'
     &     ,' when 200 is reached')

3002  format(' *ERROR* Maximum number of macro instructions',
     &       '                  is limited to 200')

4000  format(' *ERROR* Unbalanced loop/next commands')

4001  format(' *ERROR* Loops nested deeper than 8')

4002  format(' *ERROR* "loop" must precede "next" instruction')

      end
