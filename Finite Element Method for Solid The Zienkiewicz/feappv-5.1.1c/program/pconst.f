!$Id:$
      subroutine pconst(prt)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Input parameter expressions:  let = expression

!      Inputs:
!         prt    - Print input values if true

!      Outputs:
!         Values of parameters a-z are stored in array vvv(26)
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'comfil.h'
      include  'iofile.h'
      include  'ioincl.h'
      include  'iodata.h'

      logical       :: prt, redo, pconset, lopn, cinput
      integer       :: i

      save

!     Input record from file "ior" or keyboard "*"

      if(prt) then
        write(iow,2000)
      endif
      inquire(unit=iwd, opened = lopn)
      if(lopn) write(iwd,'(a)') 'Parameters'

1     redo = .false.
      record = ' '
      if(ior.gt.0) then
        read (ior,1000,err=901,end=902) record
        irecrd(isf) = irecrd(isf) + 1
      else
        write(*,3000)
!        read (  *,1000,err=901,end=902) record
        if(.not.cinput()) then
          goto 902
        endif
      endif

      if(lopn) then
        do i = 256,1,-1
          if(record(i:i).ne.' ') go to 100
        end do ! i
        i = 1
100     write(iwd,'(a)') record(1:i)
      endif

      redo = pconset(prt)
      if(redo) go to 1
      return

!     Error on read

901   call  errclr ('PCONST')
      if (ior.lt.0)  goto 1
      return

!     EOF encountered

902   return

!     Formats

 1000 format(a)

 2000 format(/'  C o n s t a n t    V a l u e s'/1x)

 3000 format(' Use "list" to give current values - <CR> to exit'/
     &       ' Input: letter=expression (no blanks)'/'  -->',$)

      end subroutine pconst

      logical function pconset(prt)

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: set parameters from 'record'

!      Inputs:
!         prt      - Print input values if true

!      Outputs:
!         pconset  - .true. indicates continue inputs from 'pconst'
!         Values of parameters a*-z* are stored in array vvv(26,0:36)
!-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'comfil.h'
      include  'corfil.h'
      include  'conval.h'
      include  'errchk.h'
      include  'iofile.h'
      include  'iosave.h'

      logical   pcomp, prt, rflag, wflag
      character eql*1,x*256,y*256
      integer   i,j,n, ial,izl,iau,izu,id,iq, i0,i9
      real (kind=8) :: val

      save

      pconset = .false.

!     Set numeric and upper/lower case locations

      i0  = ichar('0')
      i9  = ichar('9')
      iq  = ichar('=')
      ial = ichar('a')
      izl = ichar('z')
      iau = ichar('A')
      izu = ichar('Z')
      id  = ial - iau

!     Remove blanks and upper case letters

      call pstrip(x,record,1)
      y = ' '
      n = 0
      do i = 1,256
        if(x(i:i).ne.' ') then
          n      = n + 1
          y(n:n) = x(i:i)
        endif
      end do ! i

      let = ' '
      x   = ' '

      rflag = .false.
      wflag = .true.
      i = 0
      n = 0
      do while (wflag .and. i.lt.256)
        i = i + 1

        if(ichar(y(i:i)).eq.9) y(i:i) = ' '  ! Strip horizontal tab

        j   = ichar(y(i:i))
        if(y(i:i).eq.' ') then
          wflag = .false.
        elseif(y(i:i).ne.' ') then
          if(rflag) then

            if(j.eq.iq) then
              if(ior.lt.0) then
                write(  *,3000) record(1:77)
                return
              else
                write(iow,3000) record(1:77)
                call plstop(.true.)
              endif
            else
              n      = n + 1
              x(n:n) = y(i:i)
            endif
          elseif(j.eq.iq) then
            eql   = y(i:i)
            rflag = .true.
            n     = 0
          else
            if(j.ge.iau .and. j.le.izu) then  ! First character of name
              let(1:1) = char(j+id)
            else
              let(1:1) = y(i:i)
            endif
            j   = ichar(y(i+1:i+1))           ! Second character of name
            if(j.ge.iau .and. j.le.izu) then
              let(2:2) = char(j+id)
              i        = i + 1
            elseif(j.ge.ial .and. j.le.izl) then
              let(2:2) = char(j)
              i        = i + 1
            elseif(j.ge.i0 .and. j.le.i9) then
              let(2:2) = char(j)
              i        = i + 1
            else
              let(2:2) = ' '
            endif
          endif
        endif
      end do ! while

!     Converts all characters to lower case

      do i = 1,n
        n = ichar(x(i:i))
        if(n.ge.iau .and. n.le.izu) then
          x(i:i) = char(n+id)
        endif
      end do ! i

!     Save the command

      if(lsave) write(lfile,1000) let,eql,x

!     Check for blank character or null character = blank line

      if(let(1:1).eq.' '.or.ichar(let(1:1)).eq.0) then
        x       = ' '
        let     = ' '
        pconset = .false.
        return
      endif

!     Compare 'x' for match to li'st' = list values to screen

      if(pcomp(y,'list',4)) then
        if(ior.lt.0) then
          do i = 1,26
            if(vvv(i,0).ne.0.0d0) then
              write(*,2000) char(i+96),' ',vvv(i,0)
            endif
            do j = 1,26
              if(vvv(i,j).ne.0.0d0) then
                write(*,2000) char(i+96),char(j+96),vvv(i,j)
              endif
            end do ! j
            do j = 27,36
              if(vvv(i,j).ne.0.0d0) then
                write(*,2000) char(i+96),char(j+i0-27),vvv(i,j)
              endif
            end do ! j
          end do ! i
        endif
        pconset = .true.
        return
      endif

!     Check upper/lower case - convert to lower case if necessary

      n = ichar( let(1:1) ) - ial + 1
      if(n.gt.26) go to 901
      errck = .false.
      call setval(x,75, val)

!     Locate correct location for the addition

      if(let(2:2).eq.' ') then
        j        = 0
      else
        j = ichar( let(2:2) )
        if(j.ge.ial .and. j.le.izl) then
          j = j - ial + 1
        elseif(j.ge.i0 .and. j.le.i9) then
          j = j - i0 + 27
        endif
      endif
      vvv(n,j) = val

      if(prt) then
        write(iow,2000) let(1:1),let(2:2),vvv(n,j)
        if(ior.lt.0) then
          write(*,2000) let(1:1),let(2:2),vvv(n,j)
        endif
      endif
      pconset = .true.
      return

!     Error on read

901   call  errclr ('PCONST')
      if (ior.lt.0)  pconset = .true.
      return

!     Formats

 1000 format(a2,75a1)

 2000 format(5x,'Constant ',a1,a1,' = ',e15.8)

 3000 format(/'  *ERROR* PCONST: Parameter input error: More than',
     &        ' one equal in expression'/2x,a/1x)

      end function pconset
