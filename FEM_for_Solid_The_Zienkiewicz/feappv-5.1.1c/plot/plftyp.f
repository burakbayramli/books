!$Id:$
      subroutine plftyp(pstyp,nel,iel)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Set the plot for this element

!      Inputs:
!         pstyp   - Element topology
!         nel     - Number of element nodes
!         iel     - Element number

!      Output:
!         inord   - Number of plot
!-----[--.----+----.----+----.-----------------------------------------]
       implicit   none

       integer        :: pstyp,nel,iel

!      One dimensional plot sets

       if(    pstyp.eq.1) then
         if(nel.eq.1) then
           call pltpt1(iel)
         elseif(nel.eq.3) then
           call pltln3(iel)
         else
           call pltln2(iel)
         endif

!      Two dimensional plot sets

       elseif(pstyp.eq.2) then
         if(nel.eq.3) then
           call pltri3(iel)
         elseif(nel.eq.6 .or. nel.eq.7) then
           call pltri6(iel)
         elseif(nel.eq.8 .or. nel.eq.9) then
           call plqud8(iel)
         elseif(nel.eq.16) then
           call pltq16(iel)
         else
           call plqud4(iel)
         endif

!      Three dimensional plot sets

       elseif(pstyp.eq.3) then
         if(nel.eq.4) then
           call pltet4(iel)
         elseif(nel.eq.10 .or. nel.eq.11) then
           call pltet10(iel)
         elseif(nel.eq.14 .or. nel.eq.15) then
           call pltet10(iel)
         elseif(nel.eq.20 .or. nel.eq.27) then
           call plbk27(iel)
         elseif(nel.eq.64) then
           call plbkpqr(3,iel)
         else
           call plbrk8(iel)
         endif

!      User plots

       elseif(pstyp.lt.0) then

         call uftyplib(pstyp,nel,iel)

       endif

       end subroutine plftyp
