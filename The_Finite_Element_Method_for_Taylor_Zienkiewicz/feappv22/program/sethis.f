c$Id:$
      subroutine sethis(ie,ix,nie,nen,nen1,
     &                  numel,nummat,prt)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Set up history addresses in ix array

c      Inputs:
c         ie(nie,*) - Material set assembly information
c         nie       - Dimension of ie array
c         nen       - Number of nodes/element
c         nen1      - Dimension of ix array
c         numel     - Number of elements in mesh
c         nummat    - Number of material sets in mesh
c         prt       - Flag, output results if true

c      Outputs:
c         ix(nen1,*)- History data pointers added to possitions nen+1
c                     and nen+2
c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

c     include  'allotd.h'
      include  'hdatam.h'
      include  'iofile.h'

      logical   flag,prt,setvar,palloc
      integer   i,n,nh0,nhf,nie,nen,nen1,numel,nummat,ma
      integer   ie(nie,*),ix(nen1,*)

      save

c     Output amount of memory used in each user element

      if(prt) then
        flag = .true.
        do n = 1,nummat

c         Outputs

          if(ie(nie-1,n).gt.0) then
            if(flag) then
              flag = .false.
              if(ior.lt.0) write(*,2000)
              write(iow,2000)
            endif
            if(ior.lt.0) then
              write(*,2001) n,ie(nie-2,n),ie(nie-1,n),ie(nie,n),
     &                      ie(nie-5,n)
            endif
            write(iow,2001) n,ie(nie-2,n),ie(nie-1,n),ie(nie,n),
     &                      ie(nie-5,n)
          endif
        end do
      endif

c     Compute maximum length necessary to store history variables

      nhmax  = 0
      nh3max = 0
      do n = 1,nummat
        nh0 = 0
        nhf = 0
        do i = 1,nummat
          if(ie(nie-2,i).eq.n) then
            ie(nie-3,i) = nh0
            ie(nie-4,i) = nhf
            nh0         = nh0 + ie(nie,  i)
            nhf         = nhf + ie(nie-5,i)
          end if
        end do
        nhmax  = max(nhmax, ie(nie,n))
        nh3max = max(nh3max,ie(nie-5,n))
      end do

c     Set pointers for history variables into ix-array

      nh0 = 0
      do n = 1,numel

        ma  = ix(nen1,n)

c       Variable storage history

        nhf = 0
        do i = 1,nummat
          if(ie(nie-2,i).eq.ma) nhf = nhf + ie(nie,ma)
        end do
        if(nhf.gt.0) then
          ix(nen+1,n) = nh0
          nh0 = nh0 + nhf
          ix(nen+2,n) = nh0
          nh0 = nh0 + nhf
          nhf = 0
        endif

c       Fixed storage history

        do i = 1,nummat
          if(ie(nie-2,i).eq.ma) nhf = nhf + ie(nie-5,ma)
        end do
        if(nhf.gt.0) then
          ix(nen+3,n) = nh0
          nh0 = nh0 + nhf
        endif
      end do
      nhf = nh0
      if (nhf.gt.0) then
        setvar = palloc( 50,'H    ',nhf,2)
      endif

c     Formats

2000  format(/10x,'Material    Element Tag   Element Type',
     &            '  History Terms  Element Terms')

2001  format(5i15)

      end
