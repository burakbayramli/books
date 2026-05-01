      subroutine sorter( x, L )
      real*8 x(*), L
! sorter - Routine to sort particles into cells
! Inputs
!    x       Positions of particles
!    L       System size
! Outputs
!  cell_n    Number of particles in a cell (SortList common block)
!  index     Indexing list (SortList common block)
!  Xref      Cross-reference list to locate particles in cells
!            (SortList common block)

      integer*4 MAXnpart, MAXncell
      parameter( MAXnpart = 10000, MAXncell = 500 )
      integer*4 ipart, jx(MAXnpart), j, jcell, m, temp(MAXncell), k
      integer*4 ncell, npart
      integer*4 cell_n(MAXncell), index(MAXncell), Xref(MAXnpart)
      common /SortList/ ncell, npart, cell_n, index, Xref

      !* Find the cell address for each particle
      do ipart=1,npart
        j = int(x(ipart)*ncell/L) + 1
        jx(ipart) = min0( j, ncell )
      enddo

      !* Count the number of particles in each cell
      do jcell=1,ncell
        cell_n(jcell) = 0
      enddo
      do ipart=1,npart
        cell_n( jx(ipart) ) = cell_n( jx(ipart) ) + 1
      enddo

      !* Build index list as cumulative sum of the
      !  number of particles in each cell
      m = 1
      do jcell=1,ncell
        index(jcell) = m
        m = m + cell_n(jcell)
      enddo

      !* Build cross-reference list
      do jcell=1,ncell
        temp(jcell) = 0
      enddo
      do ipart=1,npart
        jcell = jx(ipart)       ! Cell address of ipart
        k = index(jcell) + temp(jcell)
        Xref(k) = ipart
        temp(jcell) = temp(jcell) + 1
      enddo

      return
      end
