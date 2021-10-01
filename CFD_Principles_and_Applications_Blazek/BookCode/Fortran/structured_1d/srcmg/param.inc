C *****************************************************************************
C
C  Include file for Eul1Dmg:
C
C  - file names
C  - numerical settings
C  - physical constants
C  - convergence
C
C *****************************************************************************
C
C  Last update: Dec. 28, 2004
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
      CHARACTER*(1) lrest, kdissipf, kdissipc, kmgcyc
      CHARACTER*256 fngrid, fnplot, fnconv, fnrest

      INTEGER ilevbase, maxsgiter, maxcgiter, maxfgiter
      INTEGER iorderf, iorderc, nrkf, nrkc
      INTEGER ldissf(5), ldissc(5), lsmoof(5), lsmooc(5)

      REAL*8 p01, t01, p2, gamma, gam1, gap1, cpgas, rgas
      REAL*8 cflf, cflc, epsirsf, epsirsc, vis2f, vis2c, vis4f, vis4c,
     &       limfacf, limfacc, epsentrf, epsentrc
      REAL*8 convtol, drho, drho1, volref, rhoref, uref, pref
      REAL*8 arkf(5), arkc(5), betrkf(5), betrkc(5)

      COMMON /paramc/ fngrid,fnplot,fnconv,fnrest,lrest,kdissipf,
     &                kdissipc,kmgcyc
      COMMON /parami/ ldissf,ldissc,lsmoof,lsmooc,ilevbase,
     &                maxsgiter,maxcgiter,maxfgiter,iorderf,iorderc,
     &                nrkf,nrkc
      COMMON /paramr/ p01,t01,p2,gamma,gam1,gap1,cpgas,rgas,cflf,cflc,
     &                epsirsf,epsirsc,vis2f,vis2c,vis4f,vis4c,limfacf,
     &                limfacc,epsentrf,epsentrc,convtol,drho,drho1,
     &                volref,rhoref,uref,pref,arkf,arkc,betrkf,betrkc
