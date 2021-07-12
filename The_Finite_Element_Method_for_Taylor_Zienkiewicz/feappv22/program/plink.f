c$Id:$
      subroutine plink(id,x,ndm,ndf,numnp,neq,prt)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Link degrees of freedom to have same solution value

c      Inputs:
c         x(ndm,*)  - Nodal coordinates of mesh
c         ndm       - Spatial dimension of mesh
c         ndf       - Number dof/node
c         numnp     - Number of nodes in mesh
c         prt       - Output links performed if true

c      Outputs:
c         id(ndf,*) - Equation numbers for each dof after link
c         neq       - Number of equations active after link
c-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'comfil.h'
      include  'conval.h'
      include  'iodata.h'
      include  'iofile.h'
      include  'ioincl.h'

      logical   prt,lsave,errck, pinput, oprt,prth
      character fnamr*18,fext*4, type*4
      integer   ndm, ndf, numnp, neq, iosfil
      integer   i, ii, i1,i2, j, j1,j2
      integer   m1,m2, n1,n2, nmax

      integer   id(ndf,*),idl(6),jdl(12)
      real*8    td(16),x(ndm,*)

      save

c     Routine to link degrees of freedom together

      fnamr =  fsav
      fext  =  'lnk '
      call addext(fnamr,fext,18,4)
      call opnfil(fext,fnamr,-2,ios,lsave)
      if(lsave) then
        iosfil = ior
        ior    = ios

        oprt = prt
        do i = 0,36
          do j = 1,26
            vvsave(j,i) = vvv(j,i)
          end do
        end do

        read(ior,1000) type,fincld(isf),irecrd(isf),prt,prth
        read(ior,1001) vvv

c       Output header information

        if(prt) then
          write(iow,2000) (i,i=1,ndf)
          if(iosfil.lt.0) then
            write(*,2000) (i,i=1,ndf)
          endif
        endif

c       Start link search

        m1 = 0
10      errck = pinput(td,4+ndf)
        n1  = td(1)
        n2  = td(2)
        i1 = td(3)
        i2 = td(4)
        do i = 1,ndf
          idl(i) = td(i+4)
        end do
        if(n1*n2.eq.0 .or. n1.gt.numnp .or. n2.gt.numnp) then
          close(ior)
          ior = iosfil
          if(n1.gt.numnp. or. n2.gt.numnp) then
            if(prt) then
              write(iow,3001) n1,n2
              if(ior.lt.0) write(*,3001) n1,n2
            endif
            n1 = 0
            n2 = 0
          endif
        endif
        if(m1.gt.0) then
20        if(x(1,m1).eq.-999.d0 .or. x(1,m2).eq.-999.d0) then
            if(prt) then
              write(iow,3001) m1,m2
              if(iosfil.lt.0) write(*,3001) m1,m2
            endif
          elseif(m1.eq.m2) then
            if(prt) then
              write(iow,3002) m1,m2
              if(iosfil.lt.0) write(*,3002) m1,m2
            endif
          else
            if(prt) then
              write(iow,2001) m1,m2,(jdl(i),i=1,ndf)
              if(iosfil.lt.0) then
                write(*,2001) m1,m2,(jdl(i),i=1,ndf)
              endif
            endif

c           Check that node pair has not already linked d.o.f.

            do j = 1,ndf
              if(jdl(j).eq.0) then
                if(id(j,m1).gt.0 .and. id(j,m2).gt.0) then

c                 Select node to renumber dof

                  if(id(j,m1).eq.id(j,m2)) then
                    if(prt) then
                      write(iow,3004) m1,m2,j
                      if(iosfil.lt.0) then
                        write(*,3004) m1,m2,j
                      endif
                    endif
                    go to 30
                  elseif(id(j,m1).lt.id(j,m2)) then
                    nmax     = id(j,m2)
                    id(j,m2) = id(j,m1)
                  else
                    nmax     = id(j,m1)
                    id(j,m1) = id(j,m2)
                  endif
                  do ii = 1,numnp
                    if(id(j,ii).eq.nmax) then
                      id(j,ii) = id(j,m1)
                    end if
                  end do

c                 Loop through all nodes to reduce equation numbers

                  errck = .false.
                  do i = 1,ndf
                    do ii = 1,numnp
                      if(id(i,ii).gt.nmax) then
                        id(i,ii) = id(i,ii) - 1
                        errck    = .true.
                      endif
                    end do
                  end do
                  if(errck) neq = neq - 1
                else

c                 Error

                  write(iow,3003) m1,m2,j
                  if(ior.lt.0) then
                    write(  *,3003) m1,m2,j
                    go to 999
                  endif
                  call plstop()

                endif
              endif
30            continue
            end do
          endif
          m1 = m1 + j1
          m2 = m2 + j2
          if( (j1.gt.0 .and. m1.lt.n1)  .or.
     &        (j2.gt.0 .and. m2.lt.n2) ) go to 20
          if(n1*n2.le.0) go to 999
        endif
        m1 = n1
        m2 = n2
        j1 = i1
        j2 = i2
        do i = 1,ndf
          jdl(i) = idl(i)
        end do
        go to 10
      else
        write(iow,3000)
        call plstop()
      endif

c     Reset parameter values

999   do i = 0,36
        do j = 1,26
          vvv(j,i) = vvsave(j,i)
        end do
      end do
      prt = oprt

c     Check that the number of equations is correct

      neq = 0
      do i = 1,ndf
        do ii = 1,numnp
          neq = max(neq,id(i,ii))
        end do
      end do

c     Formats

1000  format(a4,2x,a12,i8,2l5)
1001  format(1p,4e20.12)

2000  format('    N o d a l    L i n k    O u t p u t s'//
     &    '     Linked  Pairs     DOF Link Pattern (0=link; 1=no link)'/
     &    '    1-Node   2-Node',8(i3,'-dof')/(18x,8(i3,'-dof')))

2001  format(2i9,8i7:/(18x,8i7))

3000  format(5x,'*ERROR* Link file does not exist')

3001  format(5x,'*WARNING* Nodes',i8,' and',i8,' are not active.')

3002  format(5x,'*WARNING* Nodes',i8,' and',i8,' are same.')

3003  format(5x,'*ERROR* Attempt to link restrained DOF to active DOF'/
     &       5x,'        Nodes are',i8,' and',i8,'; DOF =',i4)

3004  format(5x,'*WARNING* Nodes',i8,' and',i8,
     &          ' already linked for DOF =',i4)

      end
