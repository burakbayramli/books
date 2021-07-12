c$Id:$
      subroutine pltord(ix,iel, iju,jplt)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Sets plot order for element type iel

c      Inputs:
c         ix(*)     - Nodal connection list
c         iel       - Element type

c      Outputs:
c         iju       - Number of points to describe element plot
c         jplt(*)   - Element nodal plot sequence
c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'cdata.h'
      include  'pdata3.h'
      include  'pdata5.h'
      include  'pdata6.h'

      integer   iel, iiu, iju, ij, iplt(9), jplt(*)
      integer   ix(*)

c     Default orders for 3-9 node 2-d elements

      data iplt/1,5,2,6,3,7,4,8,1/

c     Get number of plot points to go around element

      if(iel.gt.0) then
        iiu = inord(iel)
        iju = 0

        if(iiu.ne.0 .and. .not.hide) then

c         Set plot table for specified values

          do ij = 1,abs(iiu)
            if(ipord(ij,iel).gt.0 .and. ipord(ij,iel).le.nen) then
              if(ix(ipord(ij,iel)).gt.0) then
                iju       = iju + 1
                jplt(iju) = ipord(ij,iel)
              endif
            endif
          end do

        elseif(hide) then

c         Set plot table for hidden surface

          if(iiu .gt. 0 .and. iiu.lt.5) then
            do ij = 1,abs(iiu)
              if(ipord(ij,iel).gt.0 .and. ipord(ij,iel).le.nen) then
                if(ix(ipord(ij,iel)).gt.0) then
                  iju       = iju + 1
                  jplt(iju) = ipord(ij,iel)
                endif
              endif
            end do
          else

c           Set plot table for 4 node element

            do ij = 1,4
              if(ix(ij).gt.0) then
                iju       = iju + 1
                jplt(iju) = ij
              endif
            end do
            iju       = iju + 1
            jplt(iju) = 1

          endif

        elseif(nen.eq.3) then

c         Set plot table for 3 node element

          do ij = 1,3
            if(ix(ij).gt.0) then
              iju       = iju + 1
              jplt(iju) = ij
            endif
          end do
          iju       = iju + 1
          jplt(iju) = 1

        else

c       Set plot table for 3-9 node element

          do ij = 1,9
            if(iplt(ij).le.nen) then
              if(ix(iplt(ij)).gt.0) then
                iju       = iju + 1
                jplt(iju) = iplt(ij)
              endif
            endif
          end do

        endif
      elseif(iel.lt.0) then
        iiu = exord(-iel)
        iju = 0

        if(iiu.ne.0 .and. .not.hide) then

c         Set plot table for specified values

          do ij = 1,abs(iiu)
            if(epord(ij,-iel).gt.0 .and. epord(ij,-iel).le.nen) then
              if(ix(epord(ij,-iel)).gt.0) then
                iju       = iju + 1
                jplt(iju) = epord(ij,-iel)
              endif
            endif
          end do

        elseif(hide) then

c         Set plot table for hidden surface

          if(iiu .gt. 0 .and. iiu.lt.5) then
            do ij = 1,abs(iiu)
              if(epord(ij,-iel).gt.0 .and. epord(ij,-iel).le.nen) then
                if(ix(epord(ij,-iel)).gt.0) then
                  iju       = iju + 1
                  jplt(iju) = epord(ij,-iel)
                endif
              endif
            end do
          else

c           Set plot table for 4 node element

            do ij = 1,4
              if(ix(ij).gt.0) then
                iju       = iju + 1
                jplt(iju) = ij
              endif
            end do
            iju       = iju + 1
            jplt(iju) = 1

          endif

        elseif(nen.eq.3) then

c         Set plot table for 3 node element

          do ij = 1,3
            if(ix(ij).gt.0) then
              iju       = iju + 1
              jplt(iju) = ij
            endif
          end do
          iju       = iju + 1
          jplt(iju) = 1

        else

c       Set plot table for 3-9 node element

          do ij = 1,9
            if(iplt(ij).le.nen) then
              if(ix(iplt(ij)).gt.0) then
                iju       = iju + 1
                jplt(iju) = iplt(ij)
              endif
            endif
          end do

        endif

      endif

      end
