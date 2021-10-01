      PROGRAM Ugrid

C *****************************************************************************
C
C   CONVERTS STRUCTURED GRID INTO UNSTRUCTURED TRIANGULAR GRID
C   ==========================================================
C
C   (c) Jiri Blazek, CFD Consulting & Analysis, www.cfd-ca.de
C   Created December 28, 2000
C   Version 1.3 from February 17, 2014
C
C *****************************************************************************
C
C   Features:
C   ~~~~~~~~~
C   # reads structured grid and topology file in STRUCT2D format
C   # triangulates grid by connecting diagonal nodes with shortest distance
C   # nodes at interior cuts are NOT deleted (not perfect, but ...)
C   # writes unstructured grid in UNSTR2D format
C   # writes plot file in Vis2D format
C
C   I/O channels:
C   ~~~~~~~~~~~~~
C   5  = user parameters (input)
C   6  = control output
C   10 = structured grid data (fnstr)
C   20 = grid topology (fntop)
C   30 = unstructured grid (fnunstr)
C   40 = plot data (fnplot, Vis2D format)
C
C *****************************************************************************
C
C  Subroutines called: Check_peri, Setbc, Setbc_peri
C
C  Functions called: Length
C
C *****************************************************************************
C
C   This program is free software; you can redistribute it and/or
C   modify it under the terms of the GNU General Public License
C   as published by the Free Software Foundation; either version 2
C   of the License, or (at your option) any later version.
C
C   This program is distributed in the hope that it will be useful,
C   but WITHOUT ANY WARRANTY; without even the implied warranty of
C   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
C   GNU General Public License for more details.
C
C   You should have received a copy of the GNU General Public License
C   along with this program; if not, write to the Free Software
C   Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
C
C *****************************************************************************

      IMPLICIT NONE

C ... parameters - set as appropriate
      INTEGER im, jm, mxsegs, mxbfaces, mxbnodes
      PARAMETER (im=387, jm=67, mxsegs=20,
     &           mxbfaces=5000, mxbnodes=5000)

C ... global variables
      CHARACTER*80 title, bcstr(mxsegs), bname(mxsegs)
      CHARACTER*256 fnstr, fntop, fnunstr, fnplot
      INTEGER lbsegs(mxsegs,7), pnt(0:im,0:jm), tria(im*jm*2,3),
     &        btype(mxsegs), ibound(mxsegs,2), bface(mxbfaces,2),
     &        bnode(mxbnodes,2)
      INTEGER il, jl, nsegs, itype, lb, lbeg, lend, lbs, lbegs, lends,
     &        nnodes, ncells, nbounds
      REAL*8 x(0:im,0:jm), y(0:im,0:jm), xyu(im*jm,2)

C ... loop variables
      INTEGER i, j, iseg, ib, ic, in, ibf, ibn, ibegf, iendf,
     &        ibegn, iendn

C ... local variables
      INTEGER nc
      LOGICAL done
      REAL*8  dx, dy, d1, d2

C ... functions
      INTEGER Length

C *****************************************************************************

      WRITE(*,*) ' '
      WRITE(*,*) '*************************************************'
      WRITE(*,*) '*                                               *'
      WRITE(*,*) '*     TRIANGULATION OF 2-D STRUCTURED GRIDS     *'
      WRITE(*,*) '*                                               *'
      WRITE(*,*) '*  (c) Jiri Blazek, CFD Consulting & Analysis   *'
      WRITE(*,*) '*                 www.cfd-ca.de                 *'
      WRITE(*,*) '*                                               *'
      WRITE(*,*) '*          Version 1.3 from 02/17/2014          *'
      WRITE(*,*) '*                                               *'
      WRITE(*,*) '*************************************************'
      WRITE(*,*) ' '

C --- read parameters ---------------------------------------------------------

      READ(*,'(1X)')
      READ(*,'(A80)') title
      READ(*,'(1X)')
      READ(*,'(1X)')
      READ(*,'(A256)') fnstr
      READ(*,'(1X)')
      READ(*,'(1X)')
      READ(*,'(A256)') fntop
      READ(*,'(1X)')
      READ(*,'(1X)')
      READ(*,'(A256)') fnunstr
      READ(*,'(1X)')
      READ(*,'(1X)')
      READ(*,'(A256)') fnplot

C --- read structured grid and topology ---------------------------------------

      WRITE(*,'(A)') ' Reading structured grid ...'

      OPEN(10,file=fnstr,form='formatted',status='old')
      READ(10,'(1X)')
      READ(10,'(1X)')
      READ(10,'(1X)')
      READ(10,*) il,jl
      il = il + 2
      jl = jl + 2
      IF (il.GT.im .OR. jl.GT.jm) THEN
        WRITE(*,'(A)') 'ERROR - no. of grid points exeeds dimensions'
        STOP
      ENDIF
      READ(10,'(1X)')
      READ(10,*) ((x(i,j),y(i,j), i=2,il), j=2,jl)
      CLOSE(10)

C --- topology

      WRITE(*,'(A)') ' Reading topology file ...'

      OPEN(20,file=fntop,form='formatted',status='old')
      READ(20,'(1X)')
      READ(20,'(1X)')
      READ(20,'(1X)')
      READ(20,*) nsegs
      IF (nsegs .GT. mxsegs) THEN
        WRITE(*,'(A)') 'ERROR - no. of segments exeeds dimensions'
        STOP
      ENDIF

      READ(20,'(1X)')
      READ(20,'(1X)')
      READ(20,'(1X)')
      READ(20,'(1X)')
      READ(20,'(1X)')
      READ(20,'(1X)')
      READ(20,'(1X)')
      READ(20,'(1X)')
      DO iseg=1,nsegs
        READ(20,'(2X,A)') bcstr(iseg)
        READ(20,*) (lbsegs(iseg,j), j=1,7)
      ENDDO
      CLOSE(20)

C --- triangulate grid --------------------------------------------------------

      WRITE(*,'(A)') ' Triangulating grid ...'

C --- store grid coordinates into xyu()

      nnodes = 0
      DO j=2,jl
        DO i=2,il
          nnodes        = nnodes + 1
          pnt(i,j)      = nnodes
          xyu(nnodes,1) = x(i,j)
          xyu(nnodes,2) = y(i,j)
        ENDDO
      ENDDO

C --- generate triangles

      ncells = 0
      DO j=2,jl-1
        DO i=2,il-1
          dx = x(i  ,j) - x(i+1,j+1)
          dy = y(i  ,j) - y(i+1,j+1)
          d1 = dx*dx + dy*dy            ! 1st diagonal
          dx = x(i+1,j) - x(i  ,j+1)
          dy = y(i+1,j) - y(i  ,j+1)
          d2 = dx*dx + dy*dy            ! 2nd diagonal
          IF (d1 .LT. d2) THEN
            ncells         = ncells + 1
            tria(ncells,1) = pnt(i  ,j  )
            tria(ncells,2) = pnt(i+1,j  )
            tria(ncells,3) = pnt(i+1,j+1)
            ncells         = ncells + 1
            tria(ncells,1) = pnt(i  ,j  )
            tria(ncells,2) = pnt(i+1,j+1)
            tria(ncells,3) = pnt(i  ,j+1)
          ELSE
            ncells         = ncells + 1
            tria(ncells,1) = pnt(i  ,j  )
            tria(ncells,2) = pnt(i+1,j  )
            tria(ncells,3) = pnt(i  ,j+1)
            ncells         = ncells + 1
            tria(ncells,1) = pnt(i+1,j  )
            tria(ncells,2) = pnt(i+1,j+1)
            tria(ncells,3) = pnt(i  ,j+1)
          ENDIF
        ENDDO
      ENDDO

C --- store boundary conditions -----------------------------------------------

      WRITE(*,'(A)') ' Translating boundary conditions ...'

      nbounds = 0

      DO iseg=1,nsegs
        itype = lbsegs(iseg,1)
        lb    = lbsegs(iseg,2)
        lbeg  = lbsegs(iseg,3)
        lend  = lbsegs(iseg,4)

        IF      (itype.GE.100 .AND. itype.LT.200) THEN   ! inlet
          CALL Setbc( il,jl,im,jm,mxsegs,mxbfaces,
     &                nbounds,lb,lbeg,lend,pnt,ibound,bface )
          bname(nbounds) = bcstr(iseg)
          btype(nbounds) = itype
        ELSE IF (itype.GE.200 .AND. itype.LT.300) THEN   ! outlet
         CALL Setbc( il,jl,im,jm,mxsegs,mxbfaces,
     &                nbounds,lb,lbeg,lend,pnt,ibound,bface )
          bname(nbounds) = bcstr(iseg)
          btype(nbounds) = itype
        ELSE IF (itype.GE.300 .AND. itype.LT.500) THEN   ! wall
          CALL Setbc( il,jl,im,jm,mxsegs,mxbfaces,
     &                nbounds,lb,lbeg,lend,pnt,ibound,bface )
          bname(nbounds) = bcstr(iseg)
          btype(nbounds) = itype
        ELSE IF (itype.GE.500 .AND. itype.LT.600) THEN   ! symmetry
          CALL Setbc( il,jl,im,jm,mxsegs,mxbfaces,
     &                nbounds,lb,lbeg,lend,pnt,ibound,bface )
          bname(nbounds) = bcstr(iseg)
          btype(nbounds) = itype
        ELSE IF (itype.GE.600 .AND. itype.LT.700) THEN   ! farfield
          CALL Setbc( il,jl,im,jm,mxsegs,mxbfaces,
     &                nbounds,lb,lbeg,lend,pnt,ibound,bface )
          bname(nbounds) = bcstr(iseg)
          btype(nbounds) = itype
        ELSE IF (itype.GE.700 .AND. itype.LT.800) THEN   ! periodic
          lbs   = lbsegs(iseg,5)
          lbegs = lbsegs(iseg,6)
          lends = lbsegs(iseg,7)
          CALL Check_peri( mxsegs,iseg,lbs,lbegs,lends,lbsegs,done )
          IF (.NOT. done) THEN
            CALL Setbc_peri( il,jl,im,jm,mxsegs,mxbnodes,
     &                       nbounds,lb,lbeg,lend,lbs,lbegs,lends,
     &                       pnt,ibound,bnode )
            bname(nbounds) = bcstr(iseg)
            btype(nbounds) = itype
          ENDIF
        ELSE IF (itype.GE.800 .AND. itype.LT.899) THEN   ! injection
          CALL Setbc( il,jl,im,jm,mxsegs,mxbfaces,
     &                nbounds,lb,lbeg,lend,pnt,ibound,bface )
          bname(nbounds) = bcstr(iseg)
          btype(nbounds) = itype
        ENDIF
      ENDDO

C --- write unstructured grid -------------------------------------------------

      WRITE(*,'(A)') ' Saving unstructured grid ...'

      OPEN(30,file=fnunstr,form="formatted",status="unknown")

      nc = Length( title )
      WRITE(30,1020) title(1:nc),nnodes,ncells,nbounds
      DO i=1,nbounds
        WRITE(30,1025) btype(i),ibound(i,1),ibound(i,2),bname(i)
      ENDDO

      WRITE(30,1040)
      ibegf = 1
      ibegn = 1
      DO ib=1,nbounds
        iendf = ibound(ib,1)
        iendn = ibound(ib,2)
        IF (btype(ib).GE.700 .AND. btype(ib).LT.800) THEN   ! periodic nodes
          DO ibn=ibegn,iendn
            WRITE(30,1045) bnode(ibn,1),bnode(ibn,2)
          ENDDO
        ELSE                                                ! boundary faces
          DO ibf=ibegf,iendf
            WRITE(30,1045) bface(ibf,1),bface(ibf,2)
          ENDDO
        ENDIF
        ibegf = iendf + 1
        ibegn = iendn + 1
      ENDDO

      WRITE(30,1030)
      DO in=1,nnodes
        WRITE(30,1005) xyu(in,1),xyu(in,2)
      ENDDO
      WRITE(30,1035)
      DO ic=1,ncells
        WRITE(30,1010) tria(ic,1),tria(ic,2),tria(ic,3)
      ENDDO
      CLOSE(30)

C --- write plot file ---------------------------------------------------------

      WRITE(*,'(A)') ' Saving plot file ...'

      OPEN(40,FILE=fnplot,FORM='formatted',STATUS='unknown')
      nc = Length( title )
      WRITE(40,1050) title(1:nc),nnodes,ncells
      DO in=1,nnodes
        WRITE(40,1005) xyu(in,1),xyu(in,2)
      ENDDO
      DO ic=1,ncells
        WRITE(40,1010) tria(ic,1)-1,tria(ic,2)-1,tria(ic,3)-1
      ENDDO
      CLOSE(40)

      WRITE(*,'(/,A,/)') ' Finished.'


1005  FORMAT(2E17.9)
1010  FORMAT(3I8)
1020  FORMAT('# ',A,/,'#',/,'# no. of nodes, cells, boundaries:',/,
     &  3I8,/,'# boundaries: type, last face, last node, name:')
1025  FORMAT(3I8,/,A80)
1030  FORMAT('# coordinates (x,y):')
1035  FORMAT('# triangles (node1, node2, node3):')
1040  FORMAT('# boundary faces / periodic nodes (node1, node2):')
1045  FORMAT(2I8)
1050  FORMAT(A,/,'1',/,'Unstructured Grid',/,'1 2',/,'x',/,'y',/,
     &       '0 0',/,I8,I8,' 0',/,'grid')
      STOP
      END

C #############################################################################

      SUBROUTINE Setbc( il,jl,im,jm,mxsegs,mxbfaces,
     &                  nbounds,lb,lbeg,lend,pnt,ibound,bface )

C *****************************************************************************
C
C  Sets no. of boundary faces and nodes; stores face nodes.
C
C *****************************************************************************
C
C  Subroutines called: none
C
C  Functions called: none
C
C *****************************************************************************

      IMPLICIT NONE

C ... parameter list
      INTEGER il, jl, im, jm, mxsegs, mxbfaces, nbounds, lb, lbeg, lend
      INTEGER pnt(0:im,0:jm), ibound(mxsegs,2), bface(mxbfaces,2)

C ... loop variables
      INTEGER i, j

C ... local variables
      INTEGER ib, jb, lstep, nbfaces

C *****************************************************************************
C --- index of boundary nodes

      IF      (lb .EQ. 1) THEN
        jb = 2
      ELSE IF (lb .EQ. 2) THEN
        ib = il
      ELSE IF (lb .EQ. 3) THEN
        jb = jl
      ELSE IF (lb .EQ. 4) THEN
        ib = 2
      ENDIF

C --- last index in list of faces and list of nodes

      nbounds = nbounds + 1
      IF (nbounds .EQ. 1) THEN
        nbfaces           = 0
        ibound(nbounds,1) = ABS(lend-lbeg) + 1
        ibound(nbounds,2) = ABS(lend-lbeg) + 2
      ELSE
        nbfaces           = ibound(nbounds-1,1)
        ibound(nbounds,1) = ibound(nbounds-1,1) + ABS(lend-lbeg) + 1
        ibound(nbounds,2) = ibound(nbounds-1,2) + ABS(lend-lbeg) + 2
      ENDIF

C --- loop over boundary faces

      lstep = 1
      IF (lbeg .GT. lend) lstep = -1

      IF (lb.EQ.1 .OR. lb.EQ.3) THEN

C ----- boundary j=2 / j=j2 and i=lbeg,lend

        DO i=lbeg,lend,lstep
          nbfaces = nbfaces + 1
          IF (nbfaces .GT. mxbfaces) THEN
            WRITE(*,1000)
            STOP
          ENDIF
          bface(nbfaces,1) = pnt(i  ,jb)
          bface(nbfaces,2) = pnt(i+1,jb)
        ENDDO

      ELSE IF (lb.EQ.2 .OR. lb.EQ.4) THEN

C ----- boundary i=2 / i=i2 and j=lbeg,lend

        DO j=lbeg,lend,lstep
          nbfaces = nbfaces + 1
          IF (nbfaces .GT. mxbfaces) THEN
            WRITE(*,1000)
            STOP
          ENDIF
          bface(nbfaces,1) = pnt(ib,j  )
          bface(nbfaces,2) = pnt(ib,j+1)
        ENDDO

      ENDIF

1000  FORMAT('ERROR - no. of boundary faces exeeds dimensions')
      RETURN
      END

C #############################################################################

      SUBROUTINE Setbc_peri( il,jl,im,jm,mxsegs,mxbnodes,
     &                       nbounds,lb,lbeg,lend,lbs,lbegs,lends,
     &                       pnt,ibound,bnode )

C *****************************************************************************
C
C  Sets no. of boundary faces and nodes; stores periodic node pairs.
C
C *****************************************************************************
C
C  Subroutines called: none
C
C  Functions called: none
C
C *****************************************************************************

      IMPLICIT NONE

C ... parameter list
      INTEGER il, jl, im, jm, mxsegs, mxbnodes, nbounds, lb, lbeg,
     &        lend, lbs, lbegs, lends
      INTEGER pnt(0:im,0:jm), ibound(mxsegs,2), bnode(mxbnodes,2)

C ... loop variables
      INTEGER i, j, is, js

C ... local variables
      INTEGER ib, jb, lstep, ibs, jbs, lsteps, nbnodes, l1, l2

C *****************************************************************************

      IF      (lb .EQ. 1) THEN
        jb = 2
      ELSE IF (lb .EQ. 2) THEN
        ib = il
      ELSE IF (lb .EQ. 3) THEN
        jb = jl
      ELSE IF (lb .EQ. 4) THEN
        ib = 2
      ENDIF

      IF      (lbs .EQ. 1) THEN
        jbs = 2
      ELSE IF (lbs .EQ. 2) THEN
        ibs = il
      ELSE IF (lbs .EQ. 3) THEN
        jbs = jl
      ELSE IF (lbs .EQ. 4) THEN
        ibs = 2
      ENDIF

C --- last index in list of faces and list of nodes

      nbounds = nbounds + 1
      IF (nbounds .EQ. 1) THEN
        nbnodes           = 0
        ibound(nbounds,1) = 0
        ibound(nbounds,2) = ABS(lend-lbeg) + 2
      ELSE
        nbnodes           = ibound(nbounds-1,2)
        ibound(nbounds,1) = ibound(nbounds-1,1)
        ibound(nbounds,2) = ibound(nbounds-1,2) + ABS(lend-lbeg) + 2
      ENDIF

C --- loop over boundary faces

      IF (lbeg  .GT. lend ) THEN
        lstep = -1
        l1    = lbeg + 1
        l2    = lend
      ELSE
        lstep = 1
        l1    = lbeg
        l2    = lend + 1
      ENDIF
      IF (lbegs .GT. lends) THEN
        lsteps = -1
        is     = lbegs + 1
        js     = lbegs + 1
      ELSE
        lsteps = 1
        is     = lbegs
        js     = lbegs
      ENDIF

      IF (lb.EQ.1 .OR. lb.EQ.3) THEN

C ----- boundary j=2 / j=j2 and i=lbeg,lend

        DO i=l1,l2,lstep
          nbnodes = nbnodes + 1
          IF (nbnodes .GT. mxbnodes) THEN
            WRITE(*,1000)
            STOP
          ENDIF
          bnode(nbnodes,1) = pnt(i ,jb )
          bnode(nbnodes,2) = pnt(is,jbs)
          is = is + lsteps
        ENDDO

      ELSE IF (lb.EQ.2 .OR. lb.EQ.4) THEN

C ----- boundary i=2 / i=i2 and j=lbeg,lend

        DO j=l1,l2,lstep
          nbnodes = nbnodes + 1
          IF (nbnodes .GT. mxbnodes) THEN
            WRITE(*,1000)
            STOP
          ENDIF
          bnode(nbnodes,1) = pnt(ib, j )
          bnode(nbnodes,2) = pnt(ibs,js)
          js = js + lsteps
        ENDDO

      ENDIF

1000  FORMAT('ERROR - no. of boundary nodes exeeds dimensions')
      RETURN
      END

C #############################################################################

      SUBROUTINE Check_peri( mxsegs,iseg,lbs,lbegs,lends,lbsegs,done )

C *****************************************************************************
C
C  Checks if the "partner" of the current periodic/cut boundary was already
C  processed.
C
C *****************************************************************************
C
C  Subroutines called: none
C
C  Functions called: none
C
C *****************************************************************************

      IMPLICIT NONE

C ... parameter list
      INTEGER mxsegs, iseg, lbs, lbegs, lends
      INTEGER lbsegs(mxsegs,7)
      LOGICAL done

C ... loop variables
      INTEGER i

C ... local variables
      INTEGER lb, lbeg, lend

C *****************************************************************************

      done = .false.

      DO i=1,iseg-1                ! see all previous segments
        lb    = lbsegs(i,2)
        lbeg  = lbsegs(i,3)
        lend  = lbsegs(i,4)
        IF (lb .EQ. lbs) THEN      ! OK, same domain boundary
          IF (lbeg.EQ.lbegs .AND. lend.EQ.lends) done = .true.
          IF (lbeg.EQ.lends .AND. lend.EQ.lbegs) done = .true.
          IF (done) RETURN
        ENDIF
      ENDDO

      RETURN
      END

C #############################################################################

      INTEGER FUNCTION Length( str )

C *****************************************************************************
C
C  Delivers length of the string 'str' (without trailing blanks).
C
C *****************************************************************************
C
C  Subroutines called: none
C
C  Functions called: none
C
C *****************************************************************************

      IMPLICIT NONE

C ... parameter list
      CHARACTER*(*) str

C ... loop variables
      INTEGER i

C *****************************************************************************

      Length = 0
      DO i=1,LEN(str)
        IF (str(i:i) .NE. ' ') Length = i
      ENDDO

      RETURN
      END
