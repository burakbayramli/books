c$Id:$
      logical function vinput(xxx,nc,d,nn)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Compute array of real values from string inputs

c      Inputs:
c         xxx(*)   - String of input data
c         nc       - Number of characters in string to search
c         nn       - Number of real values to compute

c      Outputs:
c         d(*)     - Array of computed real values
c-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      character xxx*(*)
      logical   cksep,pcomp
      integer   is,iv,n,nn,nc,nl,no,nv,nu, iau,izu,id
      real*8    d(nn)

      save

c     Initialize values

      vinput = .false.

      do n = 1,nn
        d(n) = 0.0d0
      end do ! n

c     If no characters in list return

      if (xxx(1:1).eq.char(0))  return

c     Check for on/off

      is = 0
      iv = 1
      if(nc.ge.3) then
        if(pcomp(xxx,'off',3)) then
          d(1) = 1.0d0
          iv   = 2
          do is = 4,nc
            if(cksep(xxx(is:is))) go to 10
          end do ! is
          is   = nc - 2
        endif
      endif
      if(nc.ge.2) then
        if(pcomp(xxx,'on',2)) then
          d(1) = 0.0d0
          iv   = 2
          do is = 3,nc
            if(cksep(xxx(is:is))) go to 10
          end do ! is
          is   = nc - 2
        endif
      endif

10    is = is + 1
      do nl = nc,is,-1
        if(xxx(nl:nl).ne.' ') go to 20
      end do ! nl
      return

c     Parse instruction

20    no = is
      nv = iv

c     Skip leading blank characters

      do n = is,nl
        if(xxx(n:n).ne.' ') go to 25
      end do ! n

c     Check characters

25    iau = ichar('A')
      izu = ichar('Z')
      id  = ichar('a') - iau

c     Check for upper case letters

30    nu = ichar(xxx(n:n))
      if(nu.ge.iau .and. nu.le.izu) then
        xxx(n:n) = char(nu+id)
      endif

c     Format separators are blanks, commas, or equals

      if(cksep(xxx(n:n))) then
        if(n.gt.no) call setval(xxx(no:n),n-no,d(nv))
31      n  = n  + 1
        if(n.lt.nl.and.xxx(n:n).eq.' ') go to 31
        no = n
        nv = nv + 1
      else
        n  = n + 1
      endif
      if(n.le.nl.and.nv.le.nn) go to 30

c     Fill in last value if needed

      if(n.gt.no.and.nv.le.nn) call setval(xxx(no:n),n-no,d(nv))

      end
