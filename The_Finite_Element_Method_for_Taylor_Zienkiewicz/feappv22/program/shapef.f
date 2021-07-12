c$Id:$
      subroutine shapef(s,t,xl,shp,xsj,ndm,flg)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Shape function routine for 4-node isoparametric
c               quadrilaterals

c      Inputs:
c         s,t       - Natural coordinates of point
c         xl(ndm,*) - Nodal coordinates for element
c         ndm       - Spatial dimension of mesh
c         flg       - Flag, Compute global derivatives if true,
c                           else compute derivatives w/r natural coords.

c      Outputs:
c         shp(3,*)  - Shape functions and derivatives at point
c                     shp(1,i) = dN_i/dx  or dN_i/dxi_1
c                     shp(2,i) = dN_i/dy  or dN_i/dxi_2
c                     shp(3,i) = N_i
c         xsj       - Jacobian determinant at point
c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      logical   flg
      integer   ndm
      real*8    s,t, xsj,xsj1, sh,th,sp,tp,sm,tm
      real*8    xo,xs,xt, yo,ys,yt
      real*8    xsm,xsp,xtm,xtp, ysm,ysp,ytm,ytp

      real*8    xl(ndm,4),shp(3,4)

      save

c     Set up interpolations

      sh = 0.5d0*s
      th = 0.5d0*t
      sp = 0.5d0 + sh
      tp = 0.5d0 + th
      sm = 0.5d0 - sh
      tm = 0.5d0 - th
      shp(3,1) =   sm*tm
      shp(3,2) =   sp*tm
      shp(3,3) =   sp*tp
      shp(3,4) =   sm*tp

c     Set up natural coordinate functions (times 4)

      xo =  xl(1,1)-xl(1,2)+xl(1,3)-xl(1,4)
      xs = -xl(1,1)+xl(1,2)+xl(1,3)-xl(1,4) + xo*t
      xt = -xl(1,1)-xl(1,2)+xl(1,3)+xl(1,4) + xo*s
      yo =  xl(2,1)-xl(2,2)+xl(2,3)-xl(2,4)
      ys = -xl(2,1)+xl(2,2)+xl(2,3)-xl(2,4) + yo*t
      yt = -xl(2,1)-xl(2,2)+xl(2,3)+xl(2,4) + yo*s

c     Compute jacobian (times 16)

      xsj1 = xs*yt - xt*ys

c     Divide jacobian by 16 (multiply by .0625)

      xsj = 0.0625d0*xsj1
      if(.not.flg) then
        if(xsj1.eq.0.0d0) then
          xsj1 = 1.0d0
        else
          xsj1 = 1.0d0/xsj1
        endif

c       Divide functions by jacobian

        xs  = (xs+xs)*xsj1
        xt  = (xt+xt)*xsj1
        ys  = (ys+ys)*xsj1
        yt  = (yt+yt)*xsj1

c       Multiply by interpolations

        ytm =  yt*tm
        ysm =  ys*sm
        ytp =  yt*tp
        ysp =  ys*sp
        xtm =  xt*tm
        xsm =  xs*sm
        xtp =  xt*tp
        xsp =  xs*sp

c       Compute shape functions

        shp(1,1) = - ytm+ysm
        shp(1,2) =   ytm+ysp
        shp(1,3) =   ytp-ysp
        shp(1,4) = - ytp-ysm
        shp(2,1) =   xtm-xsm
        shp(2,2) = - xtm-xsp
        shp(2,3) = - xtp+xsp
        shp(2,4) =   xtp+xsm
      endif

      end
