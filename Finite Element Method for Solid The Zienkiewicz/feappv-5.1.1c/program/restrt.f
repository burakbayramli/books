!$Id:$
      subroutine restrt(fresx,u,ndm,ndf,nneq,isw)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Read/save restart files for resolutions

!      Inputs:
!         fresx   - Name of restart file to read/save
!         u(*)    - Solution state to save
!         ndm     - Spatial dimension of mesh
!         ndf     - Number dof/node
!         nneq    - Total dumber of parameters in solutions
!         isw     - Switch: = 1 for read; =2 for save.

!      Outputs:
!         u(*)    - Solution state read
!         none    - from/to blank common
!-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'arclel.h'
      include  'arcler.h'
      include  'cdata.h'
      include  'comfil.h'
      include  'counts.h'
      include  'ddata.h'
      include  'dyndat.h'
      include  'evdata.h'
      include  'fdata.h'
      include  'gltran.h'
      include  'hdata.h'
      include  'iodata.h'
      include  'iofile.h'
      include  'ndata.h'
      include  'p_point.h'
      include  'p_int.h'
      include  'pointer.h'
      include  'prlod.h'
      include  'rdata.h'
      include  'tdata.h'
      include  'comblk.h'

      character (len=1) :: yorn
      character         :: fresx*(*)

      logical       :: exst,sfl,fl9,setvar,palloc,cinput
      integer       :: i,ndm,ndf,nneq,isw
      integer       :: nnpo,nnlo,nnmo,ndmo,ndfo, nlen

      real (kind=8) :: u(*)

      save

!     Check file status

1     inquire(file=fresx,exist=exst)
      if(.not.exst.and.isw.eq.1) then
        write(iow,3002) fresx
        if(ior.lt.0) then
          write(*,3002) fresx
          write(*,3003)
!         read (*,1000) yorn
          if(.not.cinput()) then
            write(*,*) 'CINPUT error in RESTRT'
          end if
          yorn = record(1:1)
          if(yorn.eq.'y' .or. yorn.eq.'Y') then
            write(*,3004)
!           read (*,1000) fresx
            if(.not.cinput()) then
              write(*,*) 'CINPUT error in RESTRT'
            end if
            fresx = record
            goto  1
          endif
        endif
        return
      endif

!     Open file

      if(exst) then
        open (ios,file=fresx,form='unformatted',status='old')
      else
        open (ios,file=fresx,form='unformatted',status='new')
      endif
      rewind ios

!     Read restart files

      if(isw.eq.1) then

!       Control information

        read(ios) nnpo,nnlo,nnmo,ndmo,ndfo,fl(9)
        if((nnpo.eq.numnp).and.(nnlo.eq.numel).and.(nnmo.eq.nummat)
     1          .and.(ndmo.eq.ndm).and.(ndfo.eq.ndf)) then
          read(ios) theta,nrk,nrc,nrm,nrt,noi,numint,gtan

!         Solution state

          read(ios) nstep,ttim,dt,(u(i),i=1,nneq*3)
          write(iow,2000) nstep,ttim,dt
          if(ior.lt.0) write(*,2000) nstep,ttim,dt

!         Eigenpairs

          read(ios) fp(1),fp(2),mf,mq
          if(fp(1).ne.0) then
            setvar = palloc( 76,'EVAL ',mq    ,2)
            setvar = palloc( 77,'EVEC ',mq*neq,2)
            fp(1) = np(76)
            fp(2) = np(77)
            read(ios) (hr(point),point=fp(1),fp(1)+mq-1)
            read(ios) (hr(point),point=fp(2),fp(2)+mq*neq-1)
            write(iow,2001) mf,mq
            if(ior.lt.0) write(*,2001) mf,mq

          endif

!         Transient and arc length data

          read(ios) prop,rlnew,c0,cs01,cs02,ds0,r,det0,xn,fl9
          write(iow,2002) prop,rlnew
          if(ior.lt.0) write(*,2002) prop,rlnew
          if(fl9) then
            setvar = palloc( 42,'VEL  ',nrt*nneq,2)
            read(ios) (hr(point),point=np(42),np(42)+nrt*nneq-1)
            write(iow,2003) noi
            if(ior.lt.0) write(*,2003) noi
          endif

!         Current load state

          write(iow,2004)
          if(ior.lt.0) write(*,2004)
          read(ios) (hr(point),point=np(30),np(30)+4*nneq-1)

!         History data

          read(ios) rnmax,nlen,sfl
          if(sfl) then
            read(ios) (hr(np(49)+point),point=0,nlen-1)
            refl   = .true.
            fl(11) = .false.
            write(iow,2005)
            if(ior.lt.0) write(*,2005)
          endif

        else
          write(iow,3001)
          if(ior.lt.0) write(*,3001)
        endif

!     Save information for restart

      elseif(isw.eq.2) then

!       Control information

        write(ios) numnp,numel,nummat,ndm,ndf,fl(9)
        write(ios) theta,nrk,nrc,nrm,nrt,noi,numint,gtan

!       Solution state

        write(ios) nstep,ttim,dt,(u(i),i=1,nneq*3)
        write(iow,2010) nstep,ttim,dt
        if(ior.lt.0) write(*,2010) nstep,ttim,dt

!       Eigenpairs

        call pgetd('EVAL',fp(1),mq , i,sfl)
        if(sfl) then
          call pgetd('EVEC',fp(2),nlen, i,sfl)
          write(ios) fp(1),fp(2),mf,mq
          write(ios) (hr(point),point=fp(1),fp(1)+mq-1)
          write(ios) (hr(point),point=fp(2),fp(2)+nlen-1)
          write(iow,2011) mf,mq
          if(ior.lt.0) write(*,2011) mf,mq
        else
          write(ios) fp(1),fp(2),mf,mq
        endif

!       Transient and arc length data

        fl9 = fl(9)
        write(ios) prop,rlnew,c0,cs01,cs02,ds0,r,det0,xn,fl9
        write(iow,2012) prop,rlnew
        if(ior.lt.0) write(*,2012) prop,rlnew
        if(fl9) then
          write(ios) (hr(point),point=np(42),np(42)+nrt*nneq-1)
          write(iow,2013) noi
          if(ior.lt.0) write(*,2013) noi
        endif

!       Current load state

        write(ios) (hr(point),point=np(30),np(30)+4*nneq-1)
        write(iow,2014)
        if(ior.lt.0) write(*,2014)

!       History data

        call pgetd('H   ',fp(1),nlen, i,sfl)
        write(ios) rnmax,nlen,sfl
        if(sfl) then
          write(ios) (hr(fp(1)+point),point=0,nlen-1)
          write(iow,2015)
          if(ior.lt.0) write(*,2015)
        endif

      endif

!     Close file

      close(ios)

!     Formats

!1000  format(a)

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

      end subroutine restrt
