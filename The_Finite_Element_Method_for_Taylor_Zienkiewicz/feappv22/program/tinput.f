c$Id:$
      logical function tinput(tx,mt,d,nn)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c     Data input device: Returns true on error
c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Input routine for real data.: returns true on error

c      Inputs:
c         mt        - Number of text data items to extract from input
c         nn        - Number of real data items to extract from input
c                     N.B. Input performed by this function

c      Outputs:
c         tx(*)     - Values of text data input
c         d(*)      - Values of real data input
c         tinput    - Flag, returns true if error occurs during input
c-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'chdata.h'
      include  'comfil.h'
      include  'ioincl.h'
      include  'iofile.h'
      include  'iosave.h'

      logical   vinput, pcomp, cksep, first
      integer   i,j,mc,mm,mt,nn,lrec, iskip
      character txl*15,tx(*)*15,tl(16)*15
      real*8    d(*)

      save

      data      lrec /255/

c     Check on number of items

      if(mt+nn.gt.16) then
        tinput = .true.
        if(ior.lt.0) then
          write(*,2000) mt+nn
          return
        else
          write(iow,2000) mt+nn
          call plstop()
        endif
      else
        tinput = .false.
      endif

c     Initialize arrays

10    tx(1) = ' '
      do j = 1,nn
        d(j) = 0.0d0
      end do ! j
      record = ' '

11    if(ior.gt.0) then
        read (ior,1000,err=901,end=902) record
        irecrd(isf) = irecrd(isf) + 1
        iskip       = 1
      else
        read (  *,1000,err=901,end=902) record
        iskip       = 3
      endif

c     Strip horizontal tab character (Ctrl-I = ASCII Character 9)

      yyy = record
      do i = 1,lrec
        if(ichar(yyy(i:i)).eq. 9) yyy(i:i) = ' '
      end do ! i

c     Strip leading blanks and comments

      call pstrip(xxx,yyy,iskip)

12    do i = lrec,1,-1
        if(xxx(i:i).ne.' ') go to 13
      end do ! i
      i = 1
13    if(lsave) write(lfile,1000) xxx(1:i)

c     Load character variables

      if(pcomp(xxx,'incl',4)) then
        mm = max(2,mt)
      else
        mm = mt
      endif

      tl(1) = ' '
      if(mm.gt.0) then
        mc = 1
        txl = ' '
        j   = 0
        first = .false.

c       String text between double quotes (")

        do i = 1,lrec
          if(xxx(i:i).eq.'"' .or. first) then
            if(first) then
              if(xxx(i:i).eq.'"') then
                first    = .false.
              else
                j        = j + 1
                txl(j:j) = xxx(i:i)
              endif
            else
              first      = .true.
            endif

c         Non-separator string data

          elseif(.not.cksep(xxx(i:i))) then
            j          = j + 1
            txl(j:j)   = xxx(i:i)

c         Separator encountered: save character string data

          else
            tl(mc)     = txl
            txl        = ' '
            j          = 0
            mc         = mc + 1
            if(mc.gt.mm) go to 14
          endif
        end do ! i
      else
        i = 0
      end if

14    do j = 1,mt
        tx(j) = tl(j)
      end do ! j

c     Change to an include file

      if(pcomp(tl(1),'incl',4)) then
        if(  pcomp(tl(2),'end',3) ) then
          if(ior.eq.icf) then
            call pincld('end')
          endif
          return
        else
          call pincld(tl(2))
          go to 10
        endif
      endif

c     Finish inputs for parameters

      call acheck(xxx,yyy,15,75,75)
      j   = min(i+50,80)
      zzz = xxx(i+1:j)
      if(nn.gt.0 .and. .not.pcomp(tl(1),'proc',4)) then
        tinput = vinput(xxx(i+1:lrec),lrec-i,d,nn)
      else
        tinput = .false.
      end if

      return

c     Read error encoutered

901   call  errclr ('TINPUT')
      goto  11

c     EOF encountered

902   if(eofile) then
        tinput = .true.
      elseif(ior.eq.icf) then
        call pincld('end')
        tinput = .false.
        tx(1)  = ' '
      else
        call  endclr ('TINPUT',yyy)
        goto  12
      endif

c     Formats

1000  format(a)
1001  format(4x,a)

2000  format(' *ERROR* TINPUT: Too many items requested, limit = 16:',
     &       ' Requested',i8)

      end
