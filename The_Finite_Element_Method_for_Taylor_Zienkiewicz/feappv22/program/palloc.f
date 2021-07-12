c$Id:$
      logical function palloc(num,name,length,precis)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Define, delete, or resize a dictionary entry.
c               Pointer defined for integer (single) and real
c               (double precision arrays.

c      Inputs:
c         num        - Entry number for array (see below)          (int*4)
c         name       - Name of array          (see below)          (char*
c         length     - Length of array defined: =0 for delete      (int*8)
c         precis     - Precision of array: 1 = integers; 2 = reals (int*4)

c      Outputs:
c         np(num)    - Pointer to first word of array in blank common
c-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      integer    list
      parameter (list = 91)

      include  'allotd.h'
      include  'allotn.h'
      include  'pointer.h'

      logical   ualloc
      character name*(*)
      integer   i, num,precis
      integer   length

      save

c     Set active arrays for FEAPpv into list

      data   (nlist(i),i=1,list)/

     &         'TANGS', 'OINUC', 'OINUO', 'OINUP', 'UTANG', 'OINLC',
     &         'OINLO', 'OINLP', 'CMASS', 'OINMC', 'OINMO', 'OINMP',

     &         'LMASS', 'OINDC', 'OINDO', 'OINDP', 'DAMPS', 'OINCC',
     &         'OINCO', 'OINCP', 'JPROF', 'PROP0', 'PROP1', 'PROP2',

c     Solution arrays

c              'TANGS',     !     1: Symmetric tangent
c              'OINUC',     !     2: Compact tangent end pointers
c              'OINUO',     !     3: Compact tangent entries/equation
c              'OINUP',     !     4: Compact tangent pointer

c              'UTANG',     !     5: Unsymmetric tangent
c              'OINLC',     !     6: Compact tangent end pointers
c              'OINLO',     !     7: Compact tangent entries/equation
c              'OINLP',     !     8: Compact tangent pointer

c              'CMASS',     !     9: Consistent mass
c              'OINMC',     !    10: Compact upper mass end pointers
c              'OINMO',     !    11: Compact upper mass entries/equation
c              'OINMP',     !    12: Compact upper mass pointer

c              'LMASS',     !    13: Diagonal mass
c              'OINDC',     !    14: Compact lower mass end pointers
c              'OINDO',     !    15: Compact lower mass entries/equation
c              'OINDP',     !    16: Compact lower mass pointer

c              'DAMPS',     !    17: Symmetric damping
c              'OINCC',     !    18: Compact damping end pointers
c              'OINCO',     !    19: Compact damping entries/equation
c              'OINCP',     !    20: Compact damping pointer

c              'JPROF',     !    21: Profile pointer
c              'PROP0',     !    22: Prop. load offset table
c              'PROP1',     !    23: Prop. load values table
c              'PROP2',     !    24: Prop. load temporary use

c     Mesh arrays

     &         'D    ', 'DR   ', 'F    ', 'F0   ', 'FPRO ', 'FTN  ',
     &         'ID   ', 'IE   ', 'IX   ', 'LD   ', 'P    ', 'S    ',
     &         'NORMV', 'T    ', 'TL   ', 'U    ', 'UL   ', 'VEL  ',
     &         'X    ', 'XL   ', 'ANG  ', 'ANGL ', 'NREN ', 'IPOS ',
     &         'NDTYP',

c              'D    ',     !    25: Material parameters
c              'DR   ',     !    26: Residual/reactions

c              'F    ',     !    27: Nodal load/displacement, current
c              'F0   ',     !    28: Nodal load/displace pattern, base
c              'FPRO ',     !    29: DOF proportional load numbers
c              'FTN  ',     !    30: Nodal load/displacement, current

c              'ID   ',     !    31: Equation numbers/boundary conds
c              'IE   ',     !    32: Element assembly information
c              'IX   ',     !    33: Element connection data

c              'LD   ',     !    34: Element local/global eq numbers

c              'P    ',     !    35: Element vector

c              'S    ',     !    36: Element array
c              'NORMV'      !    37: Normal vector (shell use)

c              'T    ',     !    38: Nodal temperatures
c              'TL   ',     !    39: Nodal temperaturese, element

c              'U    ',     !    40: Nodal solutions/increments
c              'UL   ',     !    41: Nodal solutions/increments,element

c              'VEL'  ,     !    42: Nodal transient solution values

c              'X    ',     !    43: Nodal coordinates
c              'XL   ',     !    44: Nodal coordinates, element

c              'ANG  ',     !    45: Nodal angles
c              'ANGL ',     !    46: Nodal angles, element
c              'NREN ',     !    47: Node renumber list
c              'IPOS ',     !    48: Tie node list

c              'NDTYP',     !    49: Node coordinate type

c     History data

     &         'H    ', 'NH1  ', 'NH2  ', 'NH3  ',

c              'H    ',     !    50: Element history parameters
c              'NH1  ',     !    51: Element history data at t_n
c              'NH2  ',     !    52: Element history data at t_n+1
c              'NH3  ',     !    53: Element history data, time independ.

c     Plot data

     &         'CT   ', 'FCIX ', 'FCZM ', 'NDER ', 'NDNP ', 'VISN ',
     &         'NSCR ', 'OUTL ', 'SYMM ',


c              'CT   ',     !    54: Plot deformed coordinate storage.
c              'FCIX ',     !    55: Face nodal connection list.
c              'FCZM ',     !    56: Face z-sort coordinates.
c              'NDER ',     !    57: Error estimator projections.
c              'NDNP ',     !    58: Stress projection array.
c              'VISN ',     !    59: Visible face list
c              'NSCR ',     !    60: TRI2D size projection data.
c              'OUTL ',     !    61: Outline construction.
c              'SYMM ',     !    62: Symmetric reflections table.

c     Blending arrays

     &         'BNODE', 'BSIDE', 'BTRAN', 'BLEND', 'BFACE', 'BNILR',

c              'BNODE',     !    63:  Super nodes for blending functions
c              'BSIDE',     !    64:  Sides for blending functions
c              'BTRAN',     !    65:  Transformations for blends
c              'BLEND',     !    66:  Blending function storage
c              'BFACE',     !    65:  Blending function storage
c              'BNILR',     !    66:  Blending layer storage

c     Solver support data

     &         'BFGD ', 'BFGO ', 'BFGS ', 'BFGT ', 'BFGV ', 'BFGW ',
     &         'EIGE ', 'EVAL ', 'EVEC ', 'EXTND', 'MU1  ', 'MU2  ',

c              'BFGD ',     !    69: BFGS working vector
c              'BFGO ',     !    70: BFGS working vector
c              'BFGS ',     !    71: BFGS working vector
c              'BFGT ',     !    72: BFGS working vector
c              'BFGV ',     !    73: BFGS vectors, U
c              'BFGW ',     !    74: BFGS vectors, W

c              'EIGE ',     !    75: Element eigenpairs
c              'EVAL ',     !    76: Subspace eigenvalues
c              'EVEC ',     !    77: Subspace eigenvectors
c              'EXTND',     !    78: External nodes

c              'MU1  ',     !    79:
c              'MU2  ',     !    80:

c     Temporay arrays

     &         'TEMP1', 'TEMP2', 'TEMP3', 'TEMP4', 'TEMP5', 'TEMP6',
     &         'TEMP7', 'TEMP8', 'TEMP9', 'TEMP0',

c              'TEMP1',     !    81:  Temporary array
c              'TEMP2',     !    82:  Temporary array
c              'TEMP3',     !    83:  Temporary array
c              'TEMP4',     !    84:  Temporary array
c              'TEMP5',     !    85:  Temporary array
c              'TEMP6',     !    86:  Temporary array
c              'TEMP7',     !    87:  Temporary array
c              'TEMP8',     !    88:  Temporary array
c              'TEMP9',     !    89:  Temporary array
c              'TEMP0',     !    90:  Temporary array

c     Follower nodal loads

     &         'APLOT'/

c              'APLOT',     !    91:  Tag for active plot elements

c     Set memory pointers in blank common

      if(num.eq.0) then

        do i = 1,list
          np(i)      = 0
          ilist(1,i) = 0
          ilist(2,i) = 0
        end do
        llist  =  list

      endif

c     Check user allocations then do allocation operation

      palloc = ualloc(num-llist,name,length,precis)

      end
