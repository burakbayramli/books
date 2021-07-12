c$Id:$
      subroutine restrt(fres,u,ndm,ndf,nneq,isw)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Read/save restart files for resolutions

c      Inputs:
c         fres    - Name of restart file to read/save
c         u(*)    - Solution state to save
c         ndm     - Spatial dimension of mesh
c         ndf     - Number dof/node
c         nneq    - Total dumber of parameters in solutions
c         isw     - Switch: = 1 for read; =2 for save.

c      Outputs:
c         u(*)    - Solution state read
c         none    - from/to blank common
c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'arclel.h'
      include  'arcler.h'
      include  'cdata.h'
      include  'counts.h'
      include  'ddata.h'
      include  'dyndat.h'
      include  'evdata.h'
      include  'fdata.h'
      include  'gltran.h'
      include  'iodata.h'
      include  'iofile.h'
      include  'ndata.h'
      include  'pointer.h'
      include  'prlod.h'
      include  'rdata.h'
      include  'tdata.h'
      include  'comblk.h'

      logical   exst,sfl,fl9,setvar,palloc
      integer   i,ndm,ndf,nneq,isw
      integer   nnpo,nnlo,nnmo,ndmo,ndfo,nh1,nh2, ii
      character fres*17,yorn*1

      real*8    u(*)

      save

c     Check file status

1     inquire(file=fres,exist=exst)
      if(.not.exst.and.isw.eq.1) then
        write(iow,3002) fres
        if(ior.lt.0) then
          write(*,3002) fres
          write(*,3003)
          read (*,1000) yorn
          if(yorn.eq.'y' .or. yorn.eq.'Y') then
            write(*,3004)
            read (*,1000) fres
            goto  1
          endif
        endif
        return
      endif

c     Open file

      if(exst) then
        open (ios,file=fres,form='unformatted',status='old')
      else
        open (ios,file=fres,form='unformatted',status='new')
      endif
      rewind ios

c     Read restart files

      if(isw.eq.1) then

c       Control information

        read(ios) nnpo,nnlo,nnmo,ndmo,ndfo,fl(9)
        if((nnpo.eq.numnp).and.(nnlo.eq.numel).and.(nnmo.eq.nummat)
     1          .and.(ndmo.eq.ndm).and.(ndfo.eq.ndf)) then
          read(ios) theta,nrk,nrc,nrm,nrt,noi,numint,gtan

c         Solution state

          read(ios) nstep,ttim,dt,(u(i),i=1,nneq*3)
          write(iow,2000) nstep,ttim,dt
          if(ior.lt.0) write(*,2000) nstep,ttim,dt

c         Eigenpairs

          read(ios) md,mv,mf,mq
          if(md.ne.0) then
            setvar = palloc( 76,'EVAL ',mq    ,2)
            setvar = palloc( 77,'EVEC ',mq*neq,2)
            md = np(76)
            mv = np(77)
            read(ios) (hr(ii),ii=md,md+mq-1)
            read(ios) (hr(ii),ii=mv,mv+mq*neq-1)
            write(iow,2001) mf,mq
            if(ior.lt.0) write(*,2001) mf,mq

          endif

c         Transient and arc length data

          read(ios) prop,rlnew,c0,cs01,cs02,ds0,r,det0,xn,fl9
          write(iow,2002) prop,rlnew
          if(ior.lt.0) write(*,2002) prop,rlnew
          if(fl9) then
            setvar = palloc( 42,'VEL  ',nrt*nneq,2)
            read(ios) (hr(ii),ii=np(42),np(42)+nrt*nneq-1)
            write(iow,2003) noi
            if(ior.lt.0) write(*,2003) noi
          endif

c         Current load state

          write(iow,2004)
          if(ior.lt.0) write(*,2004)
          read(ios) (hr(ii),ii=np(30),np(30)+4*nneq-1)

c         History data

          read(ios) rnmax,nh1,nh2,(hr(ii),ii=nh1,nh2)
          refl   = .true.
          fl(11) = .false.
          write(iow,2005)
          if(ior.lt.0) write(*,2005)

        else
          write(iow,3001)
          if(ior.lt.0) write(*,3001)
        endif

c     Save information for restart

      elseif(isw.eq.2) then

c       Control information

        write(ios) numnp,numel,nummat,ndm,ndf,fl(9)
        write(ios) theta,nrk,nrc,nrm,nrt,noi,numint,gtan

c       Solution state

        write(ios) nstep,ttim,dt,(u(i),i=1,nneq*3)
        write(iow,2010) nstep,ttim,dt
        if(ior.lt.0) write(*,2010) nstep,ttim,dt

c       Eigenpairs

        call pgetd('EVAL',md,mq , i,sfl)
        if(sfl) then
          call pgetd('EVEC',mv,nh1, i,sfl)
          write(ios) md,mv,mf,mq
          write(ios) (hr(ii),ii=md,md+mq-1)
          write(ios) (hr(ii),ii=mv,mv+nh1-1)
          write(iow,2011) mf,mq
          if(ior.lt.0) write(*,2011) mf,mq
        else
          write(ios) md,mv,mf,mq
        endif

c       Transient and arc length data

        fl9 = fl(9)
        write(ios) prop,rlnew,c0,cs01,cs02,ds0,r,det0,xn,fl9
        write(iow,2012) prop,rlnew
        if(ior.lt.0) write(*,2012) prop,rlnew
        if(fl9) then
          write(ios) (hr(ii),ii=np(42),np(42)+nrt*nneq-1)
          write(iow,2013) noi
          if(ior.lt.0) write(*,2013) noi
        endif

c       Current load state

        write(ios) (hr(ii),ii=np(30),np(30)+4*nneq-1)
        write(iow,2014)
        if(ior.lt.0) write(*,2014)

c       History data

        call pgetd('H   ',nh1,nh2, i,sfl)
        if(sfl) then
          nh2 = nh1 + nh2 - 1
        else
          nh1 = 1
          nh2 = 1
        endif
        write(ios) rnmax,nh1,nh2,(hr(ii),ii=nh1,nh2)
        write(iow,2015)
        if(ior.lt.0) write(*,2015)

      endif

c     Close file

      close(ios)

c     Formats

1000  format(a)

2000  Format('   R e s t a r t    I n p u t   D a t a'/
     &       10x,'Time step number  =',i8/
     &       10x,'Time at restart   =',1p,1e12.5/
     &       10x,'Time increment    =',1p,1e12.5/
     &       10x,'Displacements input')

2001  Format(10x,'Eigenpairs input for',i4,' modes',i4,' total values')

2002  Format(10x,'Proportional load =',1p,1e12.5/
     &       10x,'Arc-length   load =',1p,1e12.5)

2003  Format(10x,'Transient states input (noi =',i2,')')

2004  Format(10x,'Force vector input')

2005  Format(10x,'History data input')

2010  Format('   R e s t a r t    O u t p u t   D a t a'/
     &       10x,'Time step number  =',i8/
     &       10x,'Time for restart  =',1p,1e12.5/
     &       10x,'Time increment    =',1p,1e12.5/
     &       10x,'Displacements output')

2011  Format(10x,'Eigenpairs output for',i4,' modes',
     &       i4,' total values')

2012  Format(10x,'Proportional load =',1p,1e12.5/
     &       10x,'Arc-length   load =',1p,1e12.5)

2013  Format(10x,'Transient states output (noi =',i2,')')

2014  Format(10x,'Force vector output')

2015  Format(10x,'History data output')

3001  format(' *ERROR* Incorrect information in a restart')

3002  format(' *ERROR* Restart file ',a17,' does not exist')

3003  format(11x,'Specify new name for restart file? (y or n) >',$)

3004  format(11x,'New Restart File Name >',$)

      end
