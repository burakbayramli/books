!$Id:$
      logical function ualloc(num,name,length,precis)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Define, delete, or resize a user dictionary entry.
!               Pointer defined for integer (single) and real
!               (double precision) arrays.

!               N.B. Currently limited to 200 names by dimension of
!               common blocks 'allotd','allotn','pointer'

!      Inputs:
!         num        - Entry number for array (see below)
!         name       - Name of array          (see below)
!         length     - Length of array defined: =0 for delete
!         precis     - Precision of array: 1 = integers; 2 = reals

!      Output:
!         up(num)    - Pointer to first word of array in blank common
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'allotd.h'

      logical   usetmem
      character name*(*)
      integer   i, num,length,precis

!     Storage definitions for UALLOC variables

      integer   list
      parameter (list = 20)

      character names(list)*5

      save

!     Define and store names

      data   (names(i),i=1,list)/

     &         'CIXSP','CIXSE','CIXMP','CIXME','CMCLS','CSLGN',

!     Short description of variables

!              'CIXSP',     !     1: Number elmts connected to surface 1
!              'CIXSE',     !     2: Elmts list connected to surface 1
!              'CIXMP',     !     3: Number elmts connected to surface 2
!              'CIXME',     !     4: Elmts list connected to surface 2
!              'CMCLS',     !     5: Closest control pt to master facet
!              'CSLGN',     !     6: Slave gap integrals

     &         'C_EXT','C_ELM','UDUM1','UDUM1',

!              'C_EXT',     !     7: Extraction operator for Bezier mesh
!              'C_ELM',     !     8: Element extraciton map
!              'UDUM2',     !     9: Unused
!              'UDUM3',     !    10: Unused

     &         'UTEM1','UTEM2','UTEM3','UTEM4','UTEM5',
     &         'UTEM6','UTEM7','UTEM8','UTEM9','UTEM0' /

!              'UTEM1',     !    11: Temporarey user array
!              'UTEM2',     !    12: Temporarey user array
!              'UTEM3',     !    13: Temporarey user array
!              'UTEM4',     !    14: Temporarey user array
!              'UTEM5',     !    15: Temporarey user array
!              'UTEM6',     !    16: Temporarey user array
!              'UTEM7',     !    17: Temporarey user array
!              'UTEM8',     !    18: Temporarey user array
!              'UTEM9',     !    19: Temporarey user array
!              'UTEM0',     !    20: Temporarey user array

!     Do memory management operations

      ualloc = usetmem(list,names,num,name,length,precis)

      end
