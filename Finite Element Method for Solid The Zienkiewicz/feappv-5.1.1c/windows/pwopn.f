!$Id:$
      subroutine pwopn ()

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!     Modification log                                Date (dd/mm/year)
!       Original version                                    01/11/2006
!       1. Change DFLIB to IFQWIN                           10/04/2014
!       2. Add View menu option back                        28/09/2017
!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose:

!      Inputs:

!      Outputs:
!-----[--.----+----.----+----.-----------------------------------------]
      use        IFQWIN

      implicit   none

      type(windowconfig)  :: textscreen
      logical             :: vstatus

      save

!     Open Child Console Window for text inputs

      open (unit = 5, file = 'CON')

!     Get Current Input Data Window Size

      vstatus  = getwindowconfig(textscreen)

!     Set the x & y pixels & font to give 80 x 50 text window

      textscreen.numxpixels   =  800
      textscreen.numypixels   =  800
      textscreen.numtextcols  =  -1
      textscreen.numtextrows  =  -1
      textscreen.fontsize     = #000A0010
      textscreen.numcolors    =  -1
      textscreen.title =
     &"Finite  Element  Analysis  Program - Personal  Version"C

      textscreen.mode = qwin$scrolldown

      vstatus = setwindowconfig(textscreen)
      if(.not.vstatus) vstatus = setwindowconfig(textscreen)

      vstatus = deletemenuqq(4, 0)

      vstatus = displaycursor($gcursoron)

      end subroutine pwopn
