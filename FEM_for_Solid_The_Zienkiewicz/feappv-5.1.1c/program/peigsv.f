!$Id:$
      subroutine peigsv( lct, isw)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--+---------+---------+---------+---------+---------+---------+-]
!     Purpose: Save eigen-pairs on disk for use in another problem.

!     Inputs:
!              lct  - Filename for pairs
!              isw  - 1 - write eigenpairs to   "filename"
!                     2 - read  eigenpairs from "filename"

!     Outputs:
!              Eigen-pairs on file: "filename"
!-----[--+---------+---------+---------+---------+---------+---------+-]
      implicit none

      include  'cdata.h'
      include  'comblk.h'
      include  'evdata.h'
      include  'iofile.h'
      include  'pointer.h'

      character (len=15) :: lct

      logical       :: exst,palloc
      integer       :: neqold,isw,i

!     Write a file

      if(isw.eq.1) then
        open (unit = 35, file = lct , form = 'unformatted')

        write(35) mf,mq,neq,neqold
        write(35) (hr(np(76)+ i),i=0,mq-1),(hr(np(77)+i),i=0,mq*neq-1)
        close(35)

        write(iow,2000) lct
        if(ior.lt.0) then
          write(*,2000) lct
        endif

!     Read a set of eigen pairs

      elseif(isw.eq.2) then
        inquire(file = lct , exist = exst )
        if(exst) then
          open (unit = 35, file = lct , form = 'unformatted')

          read (35) mf,mq,neqold
          if(neq.ne.neqold) then
            if(ior.lt.0) then
              write(*,3000) neq,neqold
            else
              write(iow,3000) neq,neqold
              call plstop(.true.)
            endif
          else
!           Allocate space for eigenpairs if necessary
            exst = palloc( 76,'EVAL',mq    , 2 )
            exst = palloc( 77,'EVEC',mq*neq, 2 )
            read(35) (hr(np(76)+ i),i=0,mq-1),
     &               (hr(np(77)+i),i=0,mq*neq-1)
            close(35)
            write(iow,2001) lct
            if(ior.lt.0) then
              write(*,2001) lct
            endif
          endif
        else
          if(ior.lt.0) then
            write(*,3001) lct
          else
            write(iow,3001) lct
            call plstop(.true.)
          endif
        endif
      endif

!     Formats

2000  format(/5x,'Eigenpairs saved on file:',a/)

2001  format(/5x,'Eigenpairs read from file:',a/)

3000  format(' *ERROR* Number of equations differs from current problem'
     &     /,'         Current neq =',i9,': Old neq =',i9)

3001  format(' *ERROR* File:',a,' does not exist, respecify')

      end subroutine peigsv
