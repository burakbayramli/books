C *****************************************************************************
C
C  Include file for Eul1D:
C
C  - file names
C  - numerical settings
C  - physical constants
C  - convergence
C
C *****************************************************************************
C
C  Last update: Nov. 27, 2004
C
C  This program is free software; you can redistribute it and/or
C  modify it under the terms of the GNU General Public License
C  as published by the Free Software Foundation; either version 2
C  of the License, or (at your option) any later version.
C
C  This program is distributed in the hope that it will be useful,
C  but WITHOUT ANY WARRANTY; without even the implied warranty of
C  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
C  GNU General Public License for more details.
C
C  You should have received a copy of the GNU General Public License
C  along with this program; if not, write to the Free Software
C  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
C
C *****************************************************************************
C
      CHARACTER*(1) lrest, kdissip
      CHARACTER*256 fngrid, fnplot, fnconv, fnrest

      INTEGER ldiss(5), lsmoo(5)
      INTEGER ncells, maxiter, iorder, iter, nrk

      REAL*8 ark(5), betrk(5)
      REAL*8 p01, t01, p2, gamma, gam1, gap1, cpgas, rgas, cfl, epsirs,
     &       vis2, vis4, limfac, epsentr, convtol, drho, drho1,
     &       volref, rhoref, uref, pref

      COMMON /paramc/ fngrid,fnplot,fnconv,fnrest,lrest,kdissip
      COMMON /parami/ ncells,maxiter,iorder,iter,nrk,ldiss,lsmoo
      COMMON /paramr/ p01,t01,p2,gamma,gam1,gap1,cpgas,rgas,cfl,epsirs,
     &                vis2,vis4,convtol,limfac,epsentr,ark,betrk,drho,
     &                drho1,volref,rhoref,uref,pref
