!$Id:$
      logical function palloc(num,name,length,precis)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Define, delete, or resize a dictionary entry.
!               Pointer defined for integer (single) and real
!               (double precision arrays).

!      Inputs:
!         num        - Entry number for array (see below)
!         name       - Name of array          (see below)
!         length     - Length of array defined: =0 for delete
!         precis     - Precision of array: 1 = integers; 2 = reals

!      Outputs:
!         np(num)    - Pointer to first word of array in memory.
!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      integer    list
      parameter (list = 335)

      include   'allotd.h'
      include   'allotn.h'
      include   'pointer.h'

      logical        :: ualloc
      character      :: name*(*)
      integer        :: i, num,length,precis

      save

!     Set active arrays for FEAP into list

      data   (nlist(i),i=1,list)/

!     Solution arrays
     &         'TANG1', 'TANG2', 'TANG3', 'TANG4', 'UTAN1', 'UTAN2',
     &         'UTAN3', 'UTAN4', 'CMAS1', 'CMAS2', 'CMAS3', 'CMAS4',

     &         'LMAS1', 'LMAS2', 'LMAS3', 'LMAS4', 'DAMP1', 'DAMP2',
     &         'DAMP3', 'DAMP4', 'JP1  ', 'JP2  ', 'JP3  ', 'JP4  ',

!              'TANG1',     !     1: Symm. tangent, partition 1
!              'TANG2',     !     2: Symm. tangent, partition 2
!              'TANG3',     !     3: Symm. tangent, partition 3
!              'TANG4',     !     4: Symm. tangent, partition 4

!              'UTAN1',     !     5: Unsym tangent, partition 1
!              'UTAN2',     !     6: Unsym tangent, partition 2
!              'UTAN3',     !     7: Unsym tangent, partition 3
!              'UTAN4',     !     8: Unsym tangent, partition 4

!              'CMAS1',     !     9: Consist. mass, partition 1
!              'CMAS2',     !    10: Consist. mass, partition 2
!              'CMAS3',     !    11: Consist. mass, partition 3
!              'CMAS4',     !    12: Consist. mass, partition 4

!              'LMAS1',     !    13: Diagonal mass, partition 1
!              'LMAS2',     !    14: Diagonal mass, partition 2
!              'LMAS3',     !    15: Diagonal mass, partition 3
!              'LMAS4',     !    16: Diagonal mass, partition 4

!              'DAMP1',     !    17: Symm. damping, partition 1
!              'DAMP2',     !    18: Symm. damping, partition 2
!              'DAMP3',     !    19: Symm. damping, partition 3
!              'DAMP4',     !    20: Symm. damping, partition 4

!              'JP1  ',     !    21: Profile pointer, partition 1
!              'JP2  ',     !    22: Profile pointer, partition 2
!              'JP3  ',     !    23: Profile pointer, partition 3
!              'JP4  ',     !    24: Profile pointer, partition 4

!     Mesh arrays
     &         'D    ', 'DR   ', 'F    ', 'F0   ', 'FPRO ', 'FTN  ',
     &         'ID   ', 'IE   ', 'IX   ', 'LD   ', 'P    ', 'S    ',
     &         'SLODI', 'T    ', 'TL   ', 'U    ', 'UL   ', 'VEL  ',
     &         'X    ', 'XL   ', 'ANG  ', 'ANGL ',


!              'D    ',     !    25: Material parameters
!              'DR   ',     !    26: Residual/reactions

!              'F    ',     !    27: Nodal load/displacement, current
!              'F0   ',     !    28: Nodal load/displace pattern, base
!              'FPRO ',     !    29: DOF proportional load numbers
!              'FTN  ',     !    30: Nodal load/displacement, current

!              'ID   ',     !    31: Equation numbers/boundary conds
!              'IE   ',     !    32: Element assembly information
!              'IX   ',     !    33: Element connection data

!              'LD   ',     !    34: Element local/global eq numbers

!              'P    ',     !    35: Element vector

!              'S    ',     !    36: Element array
!              'SLODI',     !    37: Surface load data

!              'T    ',     !    38: Nodal temperatures
!              'TL   ',     !    39: Nodal temperaturese, element

!              'U    ',     !    40: Nodal solutions/increments
!              'UL   ',     !    41: Nodal solutions/increments,element

!              'VEL'  ,     !    42: Nodal transient solution values

!              'X    ',     !    43: Nodal coordinates
!              'XL   ',     !    44: Nodal coordinates, element

!              'ANG  ',     !    45: Nodal angles
!              'ANGL ',     !    46: Nodal angles, element

!     Sparse solution data
     &         'PNTER', 'INVPT',

!              'PNTER',     !    47: Pointer array
!              'INVPT',     !    48: Inverse pointer array

!     History data
     &         'H    ', 'NH1  ', 'NH2  ', 'NH3  ',

!              'H    ',     !    49: Element history parameters
!              'NH1  ',     !    50: Element history data at t_n
!              'NH2  ',     !    51: Element history data at t_n+1
!              'NH3  ',     !    52: Element history data, time ind

!     Plot data
     &         'CT   ', 'FCIX ', 'FCZM ', 'FIDP ', 'NDER ', 'NDNP ',
     &         'NPAX ', 'NDNS ', 'OUTL ', 'SYMM ', 'TECON', 'TEFAC',
     &         'TENRM', 'VISN ',

!              'CT   ',     !    53: Plot deformed coordinate storage.
!              'FCIX ',     !    54: Face nodal connection list.
!              'FCZM ',     !    55: Face z-sort coordinates.
!              'FIDP ',     !    56: Faces for each node.
!              'NDER ',     !    57: Error estimator projections.
!              'NDNP ',     !    58: Stress projection array.
!              'NPAX ',     !    59: Principal axis projections.
!              'NDNS ',     !    60: Local stress projection array.
!              'OUTL ',     !    61: Outline construction.
!              'SYMM ',     !    62: Symmetric reflections table.
!              'TECON',     !    63: 3D edge definitions.
!              'TEFAC',     !    64: 3D edge factors.
!              'TENRM',     !    65: 3D edge normals
!              'VISN ',     !    66: Visible face list

!     Other data
     &         'SPTAN', 'AUR  ', 'BFGD ', 'BFGO ', 'BFGS ', 'BFGT ',
     &         'BFGV ', 'BFGW ', 'EIGE ', 'EVAL ', 'EVEC ', 'EXTND',
     &         'IPOS ', 'JPR  ', 'MO   ', 'MR   ', 'MT   ', 'MU1  ',
     &         'MU2  ', 'NDAM ', 'NMAS ', 'NSTI ', 'NREN ', 'OINMC',
     &         'OINMO', 'OINB ', 'OINC ', 'OINO ',

!              'SPTAN',     !    67: Sparse Tangent Array
!              'AUR  ',     !    68: Preconditioner Array

!              'BFGD ',     !    69: BFGS working vector
!              'BFGO ',     !    70: BFGS working vector
!              'BFGS ',     !    71: BFGS working vector
!              'BFGT ',     !    72: BFGS working vector
!              'BFGV ',     !    73: BFGS vectors, U
!              'BFGW ',     !    74: BFGS vectors, W

!              'EIGE ',     !    75: Element eigenpairs
!              'EVAL ',     !    76: Subspace eigenvalues
!              'EVEC ',     !    77: Subspace eigenvectors
!              'EXTND',     !    78: External nodes

!              'IPOS ',     !    79: Tie node list

!              'JPR  ',     !    80: Preconditioner column pointers

!              'MO   ',     !    81: Rotational DOF option
!              'MR   ',     !    82: Rotational DOF lambda
!              'MT   ',     !    83: Rotational DOF thickness
!              'MU1  ',     !    84:                             (mu1)
!              'MU2  ',     !    85:                             (mu2)

!              'NDAM ',     !    86: Nodal damping
!              'NMAS ',     !    87: Nodal mass
!              'NSTI ',     !    88: Nodal stiffness
!              'NREN ',     !    89: Nodal renumbering list

!              'OINMC',     !    90: Consistent mass equation pointers
!              'OINMO',     !    91: Consistent mass entries/equation

!              'OINB ',     !    92: Block information for out-of-core
!              'OINC ',     !    93: Sparse equation pointers
!              'OINO ',     !    94: Sparse entries/equation

!     Rigid Body data
     &         'RCG  ', 'REQRB', 'REVO ', 'RINER', 'RIRB ', 'RIXT ',
     &         'RJNT ', 'RJNX ', 'RJTU ', 'RLAMB', 'RLIST', 'RLOAD',
     &         'RMASS', 'RUROT', 'REXMS', 'REXIN',

!              'RCG  ',     !    95: Rigid body: center of mass
!              'REQRB',     !    96: Rigid body: equation numbers
!              'REVO ',     !    97: Rigid body: revolutes
!              'RINER',     !    98: Rigid body: inertia dyadic
!              'RIRB ',     !    99: Rigid body: rigid body nodes
!              'RIXT ',     !   100: Rigid body: rigid node indicators
!              'RJNT ',     !   101: Rigid body: joint data
!              'RJNX ',     !   102: Rigid body: joint coordinates
!              'RJTU ',     !   103: Rigid body: joint solution values
!              'RLAMB',     !   104: Rigid body: rotation lambda values
!              'RLIST',     !   105: Rigid body:                (nrlist)
!              'RLOAD',     !   106: Rigid body: loads
!              'RMASS',     !   107: Rigid body: mass
!              'RUROT',     !   108: Rigid body: rotational solutions

!              'REXMS',     !   109: Rigid body: Explicit mass coupling
!              'REXIN',     !   110: Rigid body: Explicit 6x6 inertias

!     Temporay arrays
     &         'TEMP1', 'TEMP2', 'TEMP3', 'TEMP4', 'TEMP5', 'TEMP6',
     &         'TEMP7', 'TEMP8', 'TEMP9', 'TEMP0',

!              'TEMP1',     !   111:  Temporary array
!              'TEMP2',     !   112:  Temporary array
!              'TEMP3',     !   113:  Temporary array
!              'TEMP4',     !   114:  Temporary array
!              'TEMP5',     !   115:  Temporary array
!              'TEMP6',     !   116:  Temporary array
!              'TEMP7',     !   117:  Temporary array
!              'TEMP8',     !   118:  Temporary array
!              'TEMP9',     !   119:  Temporary array
!              'TEMP0',     !   120:  Temporary array

!     Proportional loading table arrays (Type 2 prop. loads)
     &         'PROP0', 'PROP1', 'PROP2',

!              'PROP0',     !   121:  Prop. load offset table
!              'PROP1',     !   122:  Prop. load values table
!              'PROP2',     !   123:  Prop. load temporary use

!     Multiple support base excitations
     &         'PROBS', 'NUBAS', 'MASBS', 'PHIBS',

!              'PROBS',     !   124:  Base proportional factors
!              'NUBAS',     !   125:  Base pattern indicators
!              'MASBS',     !   126:  Static Mass projections
!              'PHIBS',     !   127:  Static modes

!     Follower nodal loads
     &         'APLOT', 'FOLLI', 'FOLLR',

!              'APLOT',     !   128:  Tag active plot elements
!              'FOLLI ',    !   129:  Follower force nodes
!              'FOLLR ',    !   130:  Follower force values

!     Contact arrays
     &         'C0   ', 'CM   ', 'ICS  ', 'HIC  ', 'CH   ',

!              'C0   ',     !   131:  Command control table      (ncc0)
!              'CM   ',     !   132:  Material table              (ncm)
!              'ICS  ',     !   133:  List of nodes for geometry (nics)
!              'HIC  ',     !   134:  History correspond vector  (nhic)
!              'CH   ',     !   135:  History values      (ch1,ch2,ch3)

!     Contact temporary arrays
     &         'CTEM1', 'CTEM2', 'CTEM3', 'CTEM4', 'CTEM5','CTEM6',
     &         'CTEM7', 'CTEM8', 'CTEM9', 'CTE10', 'CTE11','CTE12',
     &         'CTE13', 'CTE14', 'CTE15',

!              'CTEM1',     !   136:  Contact temporary
!              'CTEM2',     !   137:  Contact temporary
!              'CTEM3',     !   138:  Contact temporary
!              'CTEM4',     !   139:  Contact temporary
!              'CTEM5',     !   140:  Contact temporary
!              'CTEM6',     !   141:  Contact temporary
!              'CTEM7',     !   142:  Contact temporary
!              'CTEM8',     !   143:  Contact temporary
!              'CTEM9',     !   144:  Contact temporary
!              'CTE10',     !   145:  Contact temporary
!              'CTE11',     !   146:  Contact temporary
!              'CTE12',     !   147:  Contact temporary
!              'CTE13',     !   148:  Contact temporary
!              'CTE14',     !   149:  Contact temporary
!              'CTE15',     !   150:  Contact temporary

!     User temporary arrays
     &         'USER1', 'USER2', 'USER3', 'USER4', 'USER5', 'USER6',
     &         'USER7', 'USER8', 'USER9', 'USER0',

!              'USER1',     !   151:  Temporary array
!              'USER2',     !   152:  Temporary array
!              'USER3',     !   153:  Temporary array
!              'USER4',     !   154:  Temporary array
!              'USER5',     !   155:  Temporary array
!              'USER6',     !   156:  Temporary array
!              'USER7',     !   157:  Temporary array
!              'USER8',     !   158:  Temporary array
!              'USER9',     !   159:  Temporary array
!              'USER0',     !   160:  Temporary array

!     Blending arrays
     &         'BNODE', 'BSIDE', 'BTRAN', 'BLEND', 'BFACE', 'BNILR',

!              'BNODE',     !   161:  Super nodes for blending functions
!              'BSIDE',     !   162:  Sides for blending functions
!              'BTRAN',     !   163:  Transformations for blends
!              'BLEND',     !   164:  Blending function storage
!              'BFACE',     !   165:  Blending function storage
!              'BNILR',     !   166:  Blending layer storage

!     Rigid array
     &         'RLINK',

!              'RLINK',     !   167:  Rigid body link definitions.

!     Contact element connection array (total active)
     &         'IXC  ',

!              'IXC  ',     !   168:  Contact connection array

!     Control arrays for modal analyses
     &         'MCTRL', 'CCTRL', 'KCTRL',

!              'MCTRL',     !   169:  Mass      control array
!              'CCTRL',     !   170:  Damping   control array
!              'KCTRL',     !   171:  Stiffness control array

     &         'SVDA ', 'SVDV ', 'SVDW ', 'SVDX ',

!              'SVDA ',     !   172:  Singular valued decomp: A
!              'SVDV ',     !   173:  Singular valued decomp: V
!              'SVDW ',     !   174:  Singular valued decomp: W
!              'SVDX ',     !   175:  Singular valued decomp: X

!     Modal/Rigid node number array
     &         'IMODF', 'AFD  ', 'AFL  ', 'AFU  ', 'BFORC',

!              'IMODF',     |   176:  Modal equation numbers
!              'AFD  ',     |   177:  Modal stiffness diagonal
!              'AFL  ',     |   178:  Modal lower stiffness
!              'AFU  ',     |   179:  Modal upper stiffness
!              'BFORC',     |   180:  Modal force vector

!     Additional rigid body and modal arrays
     &         'RBEN ','RBOU ','UMOD ',

!              'RBEN',      |   181:  Rigid body number of element
!              'RBOU',      |   182:  Rigid body boundary restraints
!              'UMOD',      |   183:  Modal displacement value

!     Modal data
     &         'CTEMP', 'KTEMP', 'MTEMP', 'FSMOD', 'YYMOD', 'WBASE',

!              'CTEMP',     !   184: Modal damping parameters
!              'KTEMP',     !   185: Modal stiffness parameters
!              'MTEMP',     !   186: Modal mass parameters
!              'FSMOD',     !   187: Modal forces                (mfs)
!              'YYMOD',     !   188: Modal solution parameters   (my)
!              'WBASE',     !   189: Modal base solution parameters

!     Node type data
     &         'NDTYP',

!              'NDTYP'      !   190: Node type tags

!     Contact variables for surface descriptors
     &         'KNOTN', 'SURFP', 'INSEG', 'CNSEG', 'PNSEG', 'XISEG',

!              'KNOTN',     !   191: Node - surface listing
!              'SURFP',     !   192: Surface points
!              'INSEG',     !   193: In segment indicator
!              'CNSEG',     !   194: Contact flag
!              'PNSEG',     !   195: Point   flag
!              'XISEG',     !   196: Surface coordinates

!     Stress projection arrays
     &         'NS1  ', 'NS2  ',

!              'NS1  ',     !   197:                             (ns1)
!              'NS2  ',     !   198:                             (ns2)

!     Beam surface plot arrays
     &         'MXBEA', 'XBEAM', 'SBEAM', 'WBEAM',

!              'MXBEA',     !   199: Mesh for surface mesh of beams
!              'XBEAM',     !   200: Coordinates for surface mesh
!              'SBEAM',     !   201: Stress for surface mesh
!              'WBEAM',     !   202: Weights for surface mesh

!     Consistent damping arrays
     &         'OINDC', 'OINDO',

!              'OINDC',     !   203: Consistent damping eq pointers
!              'OINDO',     !   204: Consistent damping entry/eqn

!     Slave boundary array
     &         'NSLAV',

!              'NSLAV',     !   205: Slave node numbers

!     Normal vector
     &         'NORMV','NSCR ','VTILD','DELTX',

!              'NORMV',     !   206: Normal vector (shell use)
!              'NSCR ',     !   207: TRI2D size projection data.
!              'VTILD',     !   208: Broyden vector 1
!              'DELTX',     !   209: Broyden vector 2

!     Interface storage and Lagrange multiplier
     &         'LAGBC','LAGRE','LAGRN','ULAGR','HINTE','HINT1',
     &         'HINT2','HINT3',

!              'LAGBC',     !   210: Lagrange multiplier B.C.
!              'LAGRE',     !   211: Lagrange multiplier equations
!              'LAGRN',     !   212: Lagrange multiplier equations
!              'ULAGR',     !   213: Lagrange multiplier solutions
!              'HINTE',     !   214: Interface history variables
!              'HINT1',     !   215: Interface history variables
!              'HINT2',     !   216: Interface history variables
!              'HINT3',     !   217: Interface history variables

!     Zienkiewicz-Zhu Projector arrays
     &         'ZZIB ','ZZIP ',

!              'ZZIB ',     !   218: Zienkiewicz-Zhu boundary nodes
!              'ZZIP ',     !   219: Zienkiewicz-Zhu active nodes

!     Auto contact pointers
     &         'ACON2','ASLD2','ACIQ2','ACON3',

!              'ACON2',     !   220: Autocon array: length = numnp
!              'ASLD2',     !   221: Autocon slideline: lg = 2*numnp+8
!              'ACIQ2',     !   222: Autocon IP   :     lg = ip(numnp)
!              'ACON3',     !   223: Autocon array :    lg = ip(numnp)*2

!     Contact lagrange multipliers
     &         'IAD  ',

!              'IAD  ',     !   224: Contact lagrange multiplier array

!     User solver pointers
     &         'USOL1','USOL2','USOL3','USOL4','USOL5','USOL6',
     &         'USOL7','USOL8','USOL9','USOL0',

!              'USOL1',     !   225: User solver array
!              'USOL2',     !   226: User solver array
!              'USOL3',     !   227: User solver array
!              'USOL4',     !   228: User solver array
!              'USOL5',     !   229: User solver array
!              'USOL6',     !   230: User solver array
!              'USOL7',     !   231: User solver array
!              'USOL8',     !   232: User solver array
!              'USOL9',     !   233: User solver array
!              'USOL0',     !   234: User solver array

!     Diagonal scaling array
     &         'DSCA1','DSCA2','DSCA3','DSCA4',

!              'DSCA1',     !   235: Reciprocal to diagonal sqare root
!              'DSCA2',     !   236: Reciprocal to diagonal sqare root
!              'DSCA3',     !   237: Reciprocal to diagonal sqare root
!              'DSCA4',     !   238: Reciprocal to diagonal sqare root

!     Interface type array
     &         'ITYPE','IEDOF',

!              'ITYPE',     !   239: Interface types
!              'IEDOF',     !   240: Interface types

!     Surface load real data
     &         'SLODR','EULER','LEULR',

!              'SLODR',     !   241: Surface load real data
!              'EULER',     !   242: Euler angles
!              'LEULR',     !   243: Euler angles, element

!     Parallel solver arrays
     &         'GN   ','EQ   ','DNNZ ','ONNZ ','GETP ',
     &         'GETV ','SENP ','SENV ',

!              'GN   ',     !   244: Local to Global node mapping
!              'EQ   ',     !   245: Local node to Global eq mapping
!              'DNNZ ',     !   246: Number non-zero diag-block entries
!              'ONNZ ',     !   247: No. non-zero off-diag-block entries
!              'GETP ',     !   248: Get data partition pointers
!              'GETV ',     !   249: Get data node values
!              'SENP ',     !   250: Send data partition pointers
!              'SENV ',     !   251: Send data node values

!     Mesh partioner (METIS/PARMETIS) arrays
     &         'XADJN','NODG ','NODPT','VXDST',

!              'XADJN',     !   252: Pointer for nodal adjacency list
!              'NODG ',     !   253: Nodal adjacency list
!              'NODPT',     !   254: Nodal partition numbers
!              'VXDST',     !   255: Distribution array

!     Nodal follower forces
     &         'NFORC','ELINK','GUVAL',

!              'NFORC',     !   256: Nodal follower forces
!              'ELINK',     !   257: Edge link direction list
!              'GUVAL',     !   258: Global equation values

!     Representative volume element for multi-scale analysis
     &         'RVELM','FRVEL','SRVEL',

!              'RVELM'      !   259: Representative volume elements
!              'FRVEL'      !   260: Deformation gradient for RVE
!              'SRVEL'      !   261: Stress and tangent from RVE

     &         'IDSAV',

!              'IDSAV'      !   262: Eq numbers without multipliers

!     NURB coordinate storage
     &         'NURBS','LNURB',

!              'NURBS'      !   263: Nurb weights
!              'LNURB'      !   264: Nurb local weights

!     Load table storage
     &         'LDTAB','LDNOD','LDVAL','SPINS',

!              'LDTAB'      !   265: Load table pointers
!              'LDNOD'      !   266: Load node list
!              'LDVAL'      !   267: Load node values
!              'SPINS'      !   268: Spin node values

!     Representative Volume Element storage
     &         'RVEMA','RVESD' ,

!              'RVEMA'      !   269: List of material numbers for RVE
!              'RVESD'      !   270: List of material numbers for RVE

!     E-spin data
     &         'ESPIN','ESPTR','CEPTR','TRIAD','LTRIA',

!              'ESPIN'      !   271: List of nodes to spin
!              'ESPTR'      !   272: Pointer to start of spin node lists
!              'CEPTR'      !   273: Pointer array ofr extraction matrix

!     3-d Boundary rotation triad
!              'TRIAD'      !   274: 3-d boundary triads global
!              'LTRIA'      !   275: 3-d boundary triads element

!     Plot arrays for NURBS/T-spline projections
     &         'I_LIN','X_LIN','U_LIN','N_LIN','T_LIN','S_LIN',
     &         'P_LIN','E_LIN','ID_LN','LKNOT','NELEM',

!              'I_LIN',     !   276: NURB 3-d graphics elements
!              'X_LIN',     !   277: NURB 3-d graphics coordinatates
!              'U_LIN',     !   278: NURB 3-d graphics displacements
!              'N_LIN',     !   279: NURB 3-d graphics renumber lists
!              'T_LIN',     !   280: NURB 3-d graphics node type
!              'S_LIN',     !   281: NURB 3-d graphics stresses
!              'P_LIN',     !   282: NURB 3-d graphics stresses
!              'E_LIN',     !   283: NURB 3-d graphics eigenvectors
!              'ID_LN',     !   284: NURB 3-d graphics id-array
!              'LKNOT',     !   285: NURB local knot definitions
!              'NELEM',     !   286: NURB submesh nodes

!     T-Spline storage for extraction operators and Bezier extraction
     &         'KNOTP','KNOTV','C_E  ','RC_E ','IC_E ','P_BEZ',
     &         'X_BEZ','W_BEZ','IXBEZ',

!              'KNOTP'      !   287: NURB knot pointer
!              'KNOTV'      !   288: NURB knot values
!              'C_E  '      !   289: NURB Extraction operator
!              'RC_E '      !   290: NURB Extraction operator
!              'IC_E '      !   291: NURB Extraction pointer
!              'P_BEZ'      !   292: NURB Bezier orders
!              'X_BEZ'      !   293: NURB Bezier nodes
!              'W_BEZ'      !   294: NURB Bezier nodes
!              'IXBEZ'      !   295: NURB Bezier elements

!     Plot arrays for NURBS/T-spline transient
     &         'V_LIN','EC_E ' ,

!              'V_LIN',     !   296: NURB 3-d graphics displacements
!              'EC_E ',     !   297: NURB 3-d graphics displacements

!     Set memory for nurb nodes, sides, and blends
     &         'KNOTS','NSIDE','KTDIV','NBSID','LKSID',

!              'KNOTS'      !   298: List of knot vectors for NURBS
!              'NSIDE'      !   299: List of side vectors for NURBS
!              'KTDIV'      !   300: Knot Div for NURB mesh builds
!              'NBSID'      !   301: NURB Block side numbers
!              'LKSID'      !   302: NURB Block length and knot numbers

!     Plot arrays for NURBS/T-spline history variables
     &         'HPLTB','HSELM','HDNP ','H_LIN',

!              'HPLTB'      !   303: History plot table
!              'HSELM'      !   304: Element array for history plots
!              'HDNP '      !   305: Global  array for history plots
!              'H_LIN'      !   306: Global  array for history plots

!     Sparse solver integer array
     &         'IPTAN',

!              'IPTAN',     !   307: Sparse integer tangent array

!     NURBS integer array size arrays
     &         'KNOTL', 'SIDEL', 'BLOKL', 'KTNUM', 'NBLOK',
     &         'NESID',

!              'KNOTL',     !   308:  KNOT (lknot)
!              'SIDEL',     !   309:  SIDE (lside,kside)
!              'BLOKL',     !   310:  BLOCK (nblkdm,nurmat,nuregn->nblk)
!              'KTNUM',     !   311:  KNOT (knotnum,knotlen)
!              'NBLOK',     !   312:  NURB (nblksd)
!              'NESID',     !   313:  NURB (nepatch)

!     NURBS Unused
     &         'UNUR1', 'UNUR2', 'UNUR3', 'UNUR4', 'UNUR5',

!              'UNUR1',     !   314:  NURB Unused
!              'UNUR2',     !   315:  NURB Unused
!              'UNUR3',     !   316:  NURB Unused
!              'UNUR4',     !   317:  NURB Unused
!              'UNUR5',     !   318:  NURB Unused

!     Diagonal Stiffness for nodal stiffness/damper/mass
     &         'KNOTE', 'DTANG',

!              'KNOTE',     !   319:  NURB element size
!              'DTANG',     !   320:  Diagonal for nodal LHS

!     Plot temporary arrays
     &         'PTEM1', 'PTEM2', 'PTEM3', 'PTEM4', 'PTEM5',
     &         'PTEM6', 'PTEM7', 'PTEM8', 'PTEM9', 'PTEM0',

!              'PTEM1',     !   321: Plot temporary variable
!              'PTEM2',     !   322: Plot temporary variable
!              'PTEM3',     !   323: Plot temporary variable
!              'PTEM4',     !   324: Plot temporary variable
!              'PTEM5',     !   325: Plot temporary variable
!              'PTEM6',     !   326: Plot temporary variable
!              'PTEM7',     !   327: Plot temporary variable
!              'PTEM8',     !   328: Plot temporary variable
!              'PTEM9',     !   329: Plot temporary variable
!              'PTEM0',     !   330: Plot temporary variable

!     Hill-Mandel arrays
     &         'HILLI', 'HILLG', 'HILLX',

!              'HILLI',     !   331: Hill-Mandel IXL array
!              'HILLG',     !   332: Hill-Mandel   G array
!              'HILLX',     !   333: Hill-Mandel  XS array

!     Mixed model extraction arrays
     &         'CPTMX', 'C_EMX'/

!              'CPTMX',     !   334: Mixed model extraction pointer
!              'C_EMX',     !   335: Mixed model extraction operator

      if(num.eq.0) then

!       Zero pointer arrays
        do i = 1,num_nps
          np(i) = 0
        end do ! i

        do i = 1,num_ups
          up(i) = 0
        end do ! i

        do i = list+1,600
          nlist(i) = '     '
        end do ! i

        do i = 1,600
          ilist(1,i) = 0
          ilist(2,i) = 0
        end do ! i
        llist  =  num_nps

      endif

!     Check user allocations then do allocation operation
      palloc = ualloc(num-llist,name,length,precis)

      end function palloc
