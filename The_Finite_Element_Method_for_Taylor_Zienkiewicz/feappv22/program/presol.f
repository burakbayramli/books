c$Id:$
      subroutine presol(cfr, error)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

      implicit   none

      include   'allotd.h'
      include   'cdata.h'
      include   'compas.h'
      include   'eqsym.h'
      include   'fdata.h'
      include   'iofile.h'
      include   'iodata.h'
      include   'ldata.h'
      include   'ndata.h'
      include   'pathn.h'
      include   'prflag.h'
      include   'psize.h'
      include   'setups.h'

      include   'pointer.h'
      include   'comblk.h'

      logical    cfr, error, palloc, setvar, flags(5)
      integer    ip,kp,len
      integer    one

      data       one / 1 /

c     Active equations case


c     Program initialization
      if(solver) then
        error    = .false.
        floop(2) = .true.

c       Symmetric part allocation

        if(.not.compfl) then
          if(np(1).gt.0) then
            call pgetd('TANGS',len,kp,ip,setvar)
            len = kp*ipr
          else
            len = 0
          endif
          kp = (mr(np(21)+neq-1)+neq)
          if(kp*ipr-len.gt.maxm-mmax) then
            write(iow,3007) 'Symmetric',kp*ipr+mmax,maxm
            if(ior.lt.0) then
              write(*,3007) 'Symmetric',kp*ipr+mmax,maxm
            endif
            if(ior.lt.0 .and. l.eq.2) then
              error = .true.
              return
            else
              call plstop()
            endif
          endif
          setvar = palloc(1,'TANGS',kp,2)
          call pzero(hr(np(1)),kp)

c         Unsymmetric part allocation

          if(cfr) then
            if(np(5).gt.0) then
              call pgetd('UTANG',len,kp,ip,setvar)
              len = kp*ipr
            else
              len = 0
            endif
            kp = max(1,(mr(np(21)+neq-1)))
            if(kp*ipr-len.gt.maxm-mmax) then
              write(iow,3007) 'Unsymmetric',kp*ipr+mmax,maxm
              if(ior.lt.0) then
                write(*,3007) 'Unsymmetric',kp*ipr+mmax,maxm
              endif
              if(ior.lt.0 .and. l.eq.2) then
                error = .true.
                return
              else
                call plstop()
              endif
            endif
            setvar = palloc(5,'UTANG',kp,2)
            call pzero(hr(np(5)),kp)
            na   = np(1)
            nau  = na + neq
            nal  = np(5)
            neqs = 1
          else
            na   = np(1)
            nau  = na + neq
            nal  = nau
            neqs = neq
          endif
          fl(4) = .false.
        else
          neqs = neq
          call pzero(hr(np(1)),nnr)
        endif

      else
        flags(1) = .true.
        flags(2) = .false.
        flags(3) =  cfr
        flags(4) = .false.
        flags(5) = .false.
        call usolve(flags,hr(one))
        error = flags(5)
      endif

c     Format

3007  format(' *WARNING* Insufficient Storage for ',a,' Profile:'/
     &      '             Need      =',i9/'             Available =',i9)

      end
