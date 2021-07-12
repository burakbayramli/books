c$Id:$
      subroutine elmlib(d,u,x,ix,t,s,p,i,j,k,jel,isw)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Element library driver routine
c               N.B. Must set library flags in Subroutine PELNUM
c                    for new program modules

c      Inputs:
c         d(*)    - Material parameters
c         u(*)    - Element solution parameters
c         x(*)    - Element nodal coordinates
c         ix(*)   - Element nodal numbers
c         t(*)    - Element temperatures
c         i       - Number dof/node           (ndf)
c         j       - Spatial dimension of mesh (ndm)
c         k       - Size of element arrays    (nst)
c         jel     - Element type number

c      Outputs:
c         d(*)    - Material parameters (isw = 1 only)
c         s(*,*)  - Element array
c         p(*)    - Element vector
c-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'eldata.h'
      include  'iofile.h'

      integer   i,j,k,jel,isw
      integer   ix(*)
      real*8    p(*),s(*),d(*),u(*),x(*),t(*)

      save

      if(isw.ge.3 .and. k.gt.0) then
        call pzero(s,k*k)
        call pzero(p,k  )
      endif

      if(jel.gt.0) then

        go to ( 1, 2, 3, 4, 5), jel
        go to 400

1       call elmt01(d,u,x,ix,t,s,p,i,j,k,isw)
        go to 100
2       call elmt02(d,u,x,ix,t,s,p,i,j,k,isw)
        go to 100
3       call elmt03(d,u,x,ix,t,s,p,i,j,k,isw)
        go to 100
4       call elmt04(d,u,x,ix,t,s,p,i,j,k,isw)
        go to 100
5       call elmt05(d,u,x,ix,t,s,p,i,j,k,isw)

      else

        if(j.eq.1) then

          go to(101,102,103,104,105,106,107,108), -jel

          go to 400

c         1-D solid

101       write(*,*) ' No 1-d solid element available.'
          call plstop()

102       call trussnd(d,u,x,ix,t,s,p,i,j,k,isw)
          go to 100

103       write(*,*) ' No 1-d frame element available.'
          call plstop()

104       write(*,*) ' No 1-d plate element available.'
          call plstop()

105       write(*,*) ' No 1-d shell element available.'
          call plstop()

106       write(*,*) ' No 1-d membrane element available.'
          call plstop()

107       write(*,*) ' No 1-d thermal element available.'
          call plstop()

108       write(*,*) ' No 1-d thermal convection element available.'
          call plstop()

        elseif(j.eq.2) then

          go to(201,202,203,204,205,206,207,208), -jel

          go to 400

c         2-D solid

201       call solid2d(d,u,x,ix,t,s,p,i,j,k,isw)
          go to 100

202       call trussnd(d,u,x,ix,t,s,p,i,j,k,isw)
          go to 100

203       call frame2d(d,u,x,ix,s,p,i,j,k,isw)
          go to 100

204       call plate2d(d,u,x,ix,s,p,i,j,k,isw)
          go to 100

205       call shell2d(d,u,x,ix,s,p,i,j,k,isw)
          go to 100

206       write(*,*) ' No 2-d membrane element available.'
          call plstop()

207       call therm2d(d,u,x,ix,s,p,i,j,k,isw)
          go to 100

208       call convec2d(d,u,x,ix,s,p,i,j,k,isw)
          go to 100

        elseif(j.eq.3) then

          go to(301,302,303,304,305,306,307,308), -jel

          go to 400

c         3-D solid

301       call solid3d(d,u,x,ix,t,s,p,i,j,k,isw)
          go to 100

302       call trussnd(d,u,x,ix,t,s,p,i,j,k,isw)
          go to 100

303       call frame3d(d,u,x,ix,s,p,i,j,k,isw)
          go to 100

304       write(*,*) ' No 3-d plate element available: Use SHELL.'
          call plstop()

305       call shell3d(d,u,x,ix,s,p,i,j,k,isw)
          go to 100

306       call membr3d(d,u,x,ix,s,p,i,j,k,isw)
          go to 100

307       call therm3d(d,u,x,ix,s,p,i,j,k,isw)
          go to 100

308       call convec3d(d,u,x,ix,s,p,i,j,k,isw)
          go to 100

        endif
      endif

100   return

c     Error

400   if(ior.gt.0) write(iow,4000) n,jel
      if(ior.lt.0) write(  *,4000) n,jel
      call plstop()

c     Format

4000  format('  *ERROR* Element:',i6,', type number',i3,' input')

      end
