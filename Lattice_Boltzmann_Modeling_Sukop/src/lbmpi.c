//##############################################################################
//
// Copyright (C), 2005, Michael Sukop and Danny Thorne
//
// lbmpi.c
//
//  - Note that this is adapted from a previous implementation in an
//    old mcmp code from last summer (2004).
//

void lbmpi_construct(
       lbmpi_ptr lbmpi,
       lattice_ptr lattice,
       int argc,
       char **argv)
{
  int i, j;
  int ierr;

  // Routine inititialization calls.
  MPI_Init( &argc, &argv);
  MPI_Comm_size( MPI_COMM_WORLD, &lbmpi->NumProcs);
  MPI_Comm_rank( MPI_COMM_WORLD, &lbmpi->ProcID);

  // Determine coordinates (PX,PY) of subdomain in
  // PX-by-PY array of subdomains.
  lbmpi->PX = lbmpi->ProcID%lbmpi->NPX;
  lbmpi->PY = (int)floor((double)lbmpi->ProcID/(double)lbmpi->NPX);

  // Determine ID of processes with adjacent subdomains.
  lbmpi->NorthID = lbmpi->NPX*((lbmpi->PY+1)%lbmpi->NPY) + (lbmpi->PX);
  lbmpi->SouthID = lbmpi->NPX*((lbmpi->PY-1+lbmpi->NPY)%lbmpi->NPY) + (lbmpi->PX);
  lbmpi->EastID  = lbmpi->NPX*((lbmpi->PY)) + (lbmpi->PX+1)%lbmpi->NPX;
  lbmpi->WestID  = lbmpi->NPX*((lbmpi->PY)) + (lbmpi->PX-1+lbmpi->NPX)%lbmpi->NPX;

  // Say "hi".
  printf(
   "Proc %d (%d,%d) of %d (%d-by-%d) says, \"Hi!\" "
   "// (N,S,E,W) = (%d,%d,%d,%d)\n",
    lbmpi->ProcID,   lbmpi->PX, lbmpi->PY,
    lbmpi->NumProcs, lbmpi->NPX,  lbmpi->NPY,
    lbmpi->NorthID, lbmpi->SouthID, lbmpi->EastID, lbmpi->WestID );

  // Allocate space for datatypes.
  lbmpi_allocate_datatypes( lbmpi, lattice);

  // Create data type structure MPI_South2North.
  //
  //  - This will be used in the communication step after
  //    streaming to send/recv elements from north subdomains
  //    to south subdomains.
  //

  printf("\n%d: Making MPI_South2North... \n", lbmpi_get_ProcID(lbmpi));
  MPI_Address(
    get_ftemp_ptr( lattice,
                   /*subs*/0,
                   /*j*/get_sj(lattice),
                   /*i*/get_si(lattice),
                   /*a*/0),
    (MPI_Aint *)lbmpi_get_Index0_ptr(lbmpi));

  // Acquire memory addresses of all the elements.  With these
  // we will compute the indices needed by MPI_Type_struct
  // below.
  for(i=get_si(lattice); i<=get_ei(lattice); i++)
  {
    MPI_Address( get_ftemp_ptr(lattice,0,get_sj(lattice),i,2),
                 &( lbmpi->AddrsNS[ 3*(i-0)+0]) );
    MPI_Address( get_ftemp_ptr(lattice,0,get_sj(lattice),i,5),
                 &( lbmpi->AddrsNS[ 3*(i-0)+1]) );
    MPI_Address( get_ftemp_ptr(lattice,0,get_sj(lattice),i,6),
                 &( lbmpi->AddrsNS[ 3*(i-0)+2]) );

  } /* for(i=1; i<=get_LX(lattice); i++) */

  // Stuff needed by MPI_Type_struct.
  for(i=1; i<=get_LX(lattice); i++)
  {
    // All the block lengths are one (1).
    lbmpi->BlockLengthsNS[ 3*(i-1)+0] = 1;
    lbmpi->BlockLengthsNS[ 3*(i-1)+1] = 1;
    lbmpi->BlockLengthsNS[ 3*(i-1)+2] = 1;

    // Compute offsets from the first element.
    lbmpi->IndicesNS[ 3*(i-1)+0] = lbmpi->AddrsNS[ 3*(i-1)+0]-lbmpi->AddrsNS[0];
    lbmpi->IndicesNS[ 3*(i-1)+1] = lbmpi->AddrsNS[ 3*(i-1)+1]-lbmpi->AddrsNS[0];
    lbmpi->IndicesNS[ 3*(i-1)+2] = lbmpi->AddrsNS[ 3*(i-1)+2]-lbmpi->AddrsNS[0];

    // All the types are doubles.
    lbmpi->TypesNS[ 3*(i-1)+0] = MPI_DOUBLE;
    lbmpi->TypesNS[ 3*(i-1)+1] = MPI_DOUBLE;
    lbmpi->TypesNS[ 3*(i-1)+2] = MPI_DOUBLE;

  } /* for(i=1; i<=get_LX(lattice); i++) */


  ierr = MPI_Type_struct(
           /* int count */                3*get_LX(lattice),
           /* int blocklens[] */          lbmpi->BlockLengthsNS,
           /* MPI_Aint indices[] */       lbmpi->IndicesNS,
           /* MPI_Datatype old_types[] */ lbmpi->TypesNS,
           /* MPI_Datatype *newtype */    &lbmpi->MPI_South2North    );

  ierr = MPI_Type_commit(
           /* MPI_Datatype *datatype */   &lbmpi->MPI_South2North    );

  printf("\n%d: Done making MPI_South2North\n", lbmpi_get_ProcID(lbmpi));

#if 1
  // Output the indices for inspection...
  sprintf( lbmpi->iobuf, " ");
  for(i=1; i<=get_LX(lattice); i++)
  {
    sprintf( lbmpi->iobuf, "%s%d %d %d ",
      lbmpi->iobuf,
      lbmpi->IndicesNS[ 3*(i-1)+0],
      lbmpi->IndicesNS[ 3*(i-1)+1],
      lbmpi->IndicesNS[ 3*(i-1)+2] );
  }
  printf("\n%d: MPI_South2North { %s }\n",
    lbmpi_get_ProcID(lbmpi),
    lbmpi->iobuf);
#endif

  // Create data type structure MPI_North2South.
  //
  //  - This will be used in the communication step after
  //    streaming to send/recv elements from south subdomains
  //    to north subdomains.
  //

  printf("\n%d: Making MPI_North2South... \n", lbmpi_get_ProcID(lbmpi));
  MPI_Address(
    get_ftemp_ptr( lattice,
                   /*subs*/0,
                   /*j*/get_ej(lattice),
                   /*i*/get_si(lattice),
                   /*a*/4),
    (MPI_Aint *)lbmpi_get_Index0_ptr(lbmpi));

  // Acquire memory addresses of all the elements.  With these
  // we will compute the indices needed by MPI_Type_struct
  // below.
  for(i=get_si(lattice); i<=get_ei(lattice); i++)
  {
    MPI_Address(
      get_ftemp_ptr(lattice,0,get_ej(lattice),i,4),
      &( lbmpi->AddrsNS[ 3*(i-0)+0]));
    MPI_Address(
      get_ftemp_ptr(lattice,0,get_ej(lattice),i,7),
      &( lbmpi->AddrsNS[ 3*(i-0)+1]));
    MPI_Address(
      get_ftemp_ptr(lattice,0,get_ej(lattice),i,8),
      &( lbmpi->AddrsNS[ 3*(i-0)+2]));

  } /* for(i=1; i<=get_LX(lattice); i++) */

  // Stuff needed by MPI_Type_struct.
  for(i=1; i<=get_LX(lattice); i++)
  {
    // All the block lengths are one (1).
    lbmpi->BlockLengthsNS[ 3*(i-1)+0] = 1;
    lbmpi->BlockLengthsNS[ 3*(i-1)+1] = 1;
    lbmpi->BlockLengthsNS[ 3*(i-1)+2] = 1;

    // Compute offsets from the first element.
    lbmpi->IndicesNS[ 3*(i-1)+0] = lbmpi->AddrsNS[ 3*(i-1)+0]-lbmpi->AddrsNS[0];
    lbmpi->IndicesNS[ 3*(i-1)+1] = lbmpi->AddrsNS[ 3*(i-1)+1]-lbmpi->AddrsNS[0];
    lbmpi->IndicesNS[ 3*(i-1)+2] = lbmpi->AddrsNS[ 3*(i-1)+2]-lbmpi->AddrsNS[0];

    // All the types are doubles.
    lbmpi->TypesNS[ 3*(i-1)+0] = MPI_DOUBLE;
    lbmpi->TypesNS[ 3*(i-1)+1] = MPI_DOUBLE;
    lbmpi->TypesNS[ 3*(i-1)+2] = MPI_DOUBLE;

  } /* for(i=1; i<=get_LX(lattice); i++) */

  ierr = MPI_Type_struct(
           /* int count */                3*get_LX(lattice),
           /* int blocklens[] */          lbmpi->BlockLengthsNS,
           /* MPI_Aint indices[] */       lbmpi->IndicesNS,
           /* MPI_Datatype old_types[] */ lbmpi->TypesNS,
           /* MPI_Datatype *newtype */    &lbmpi->MPI_North2South    );

  ierr = MPI_Type_commit(
           /* MPI_Datatype *datatype */   &lbmpi->MPI_North2South    );

  printf("\n%d: Done making MPI_North2South\n", lbmpi->ProcID);

#if 1
  // Output the indices for inspection...
  sprintf( lbmpi->iobuf, " ");
  for(i=1; i<=get_LX(lattice); i++)
  {
    sprintf( lbmpi->iobuf, "%s%d %d %d ",
      lbmpi->iobuf,
      lbmpi->IndicesNS[ 3*(i-1)+0],
      lbmpi->IndicesNS[ 3*(i-1)+1],
      lbmpi->IndicesNS[ 3*(i-1)+2] );
  }
  printf("\n%d: MPI_North2South { %s }\n",
    lbmpi_get_ProcID(lbmpi),
    lbmpi->iobuf);
#endif

  // Create data type structure MPI_East2West.
  //
  //  - This will be used in the communication step after
  //    streaming to send/recv elements from east subdomains
  //    to west subdomains.
  //

  printf("\n%d: Making MPI_East2West... \n", lbmpi_get_ProcID(lbmpi));
  //MPI_Address( &( ftemp[1][1][get_LX(lattice)][3]), &Index0);
  MPI_Address(
    get_ftemp_ptr( lattice,
                   /*subs*/0,
                   /*j*/get_sj(lattice),
                   /*i*/get_ei(lattice),
                   /*a*/3),
    (MPI_Aint *)lbmpi_get_Index0_ptr(lbmpi));

  // Acquire memory addresses of all the elements.  With these
  // we will compute the indices needed by MPI_Type_struct
  // below.
  for(j=get_sj(lattice); j<=get_ej(lattice); j++)
  {
    MPI_Address(
      get_ftemp_ptr(lattice,0,j,get_ei(lattice),3),
      &( lbmpi->AddrsEW[ 3*(j-0)+0]));
    MPI_Address(
      get_ftemp_ptr(lattice,0,j,get_ei(lattice),6),
      &( lbmpi->AddrsEW[ 3*(j-0)+1]));
    MPI_Address(
      get_ftemp_ptr(lattice,0,j,get_ei(lattice),7),
      &( lbmpi->AddrsEW[ 3*(j-0)+2]));

  } /* for(j=1; j<=get_LY(lattice); j++) */

  // Stuff needed by MPI_Type_struct.
  for(j=get_sj(lattice); j<=get_ej(lattice); j++)
  {
    // All the block lengths are one (1).
    lbmpi->BlockLengthsEW[ 3*j+0] = 1;
    lbmpi->BlockLengthsEW[ 3*j+1] = 1;
    lbmpi->BlockLengthsEW[ 3*j+2] = 1;

    // Compute offsets from the first element.
    lbmpi->IndicesEW[ 3*j+0] = lbmpi->AddrsEW[ 3*j+0]-lbmpi->AddrsEW[0];
    lbmpi->IndicesEW[ 3*j+1] = lbmpi->AddrsEW[ 3*j+1]-lbmpi->AddrsEW[0];
    lbmpi->IndicesEW[ 3*j+2] = lbmpi->AddrsEW[ 3*j+2]-lbmpi->AddrsEW[0];

    // All the types are doubles.
    lbmpi->TypesEW[ 3*j+0] = MPI_DOUBLE;
    lbmpi->TypesEW[ 3*j+1] = MPI_DOUBLE;
    lbmpi->TypesEW[ 3*j+2] = MPI_DOUBLE;

  } /* for(j=1; j<=get_LY(lattice); j++) */

  ierr = MPI_Type_struct(
           /* int count */                3*get_LY(lattice),
           /* int blocklens[] */          lbmpi->BlockLengthsEW,
           /* MPI_Aint indices[] */       lbmpi->IndicesEW,
           /* MPI_Datatype old_types[] */ lbmpi->TypesEW,
           /* MPI_Datatype *newtype */    &lbmpi->MPI_East2West    );

  ierr = MPI_Type_commit(
           /* MPI_Datatype *datatype */   &lbmpi->MPI_East2West    );

  printf("\n%d: Done making MPI_East2West\n", lbmpi_get_ProcID(lbmpi));

#if 1
  // Output the indices for inspection...
  sprintf( lbmpi->iobuf, " ");
  for(j=1; j<=get_LY(lattice); j++)
  {
    sprintf( lbmpi->iobuf, "%s%d %d %d ",
      lbmpi->iobuf,
      lbmpi->IndicesEW[ 3*(j-1)+0],
      lbmpi->IndicesEW[ 3*(j-1)+1],
      lbmpi->IndicesEW[ 3*(j-1)+2] );
  }
  printf("\n%d: MPI_East2West { %s }\n", lbmpi_get_ProcID(lbmpi), lbmpi->iobuf);
#endif


  // Create data type structure MPI_West2East.
  //
  //  - This will be used in the communication step after
  //    streaming to send/recv elements from west subdomains
  //    to east subdomains.
  //

  printf("\n%d: Making MPI_West2East... \n", lbmpi_get_ProcID(lbmpi));
  //MPI_Address( &( ftemp[1][1][1][1]), &Index0);
  MPI_Address(
    get_ftemp_ptr( lattice,
                   /*subs*/0,
                   /*j*/get_sj(lattice),
                   /*i*/get_si(lattice),
                   /*a*/1),
    (MPI_Aint *)lbmpi_get_Index0_ptr(lbmpi));

  // Acquire memory addresses of all the elements.  With these
  // we will compute the indices needed by MPI_Type_struct
  // below.
  for(j=get_sj(lattice); j<=get_ej(lattice); j++)
  {
    MPI_Address(
      get_ftemp_ptr(lattice,0,j,get_si(lattice),1),
      &( lbmpi->AddrsEW[ 3*(j-1)+0]));
    MPI_Address(
      get_ftemp_ptr(lattice,0,j,get_si(lattice),5),
      &( lbmpi->AddrsEW[ 3*(j-1)+1]));
    MPI_Address(
      get_ftemp_ptr(lattice,0,j,get_si(lattice),8),
      &( lbmpi->AddrsEW[ 3*(j-1)+2]));

  } /* for(j=1; j<=get_LY(lattice); j++) */

  // Stuff needed by MPI_Type_struct.
  for(j=get_sj(lattice); j<=get_ej(lattice); j++)
  {
    // All the block lengths are one (1).
    lbmpi->BlockLengthsEW[ 3*j+0] = 1;
    lbmpi->BlockLengthsEW[ 3*j+1] = 1;
    lbmpi->BlockLengthsEW[ 3*j+2] = 1;

    // Compute offsets from the first element.
    lbmpi->IndicesEW[ 3*j+0] = lbmpi->AddrsEW[ 3*j+0]-lbmpi->AddrsEW[0];
    lbmpi->IndicesEW[ 3*j+1] = lbmpi->AddrsEW[ 3*j+1]-lbmpi->AddrsEW[0];
    lbmpi->IndicesEW[ 3*j+2] = lbmpi->AddrsEW[ 3*j+2]-lbmpi->AddrsEW[0];

    // All the types are doubles.
    lbmpi->TypesEW[ 3*j+0] = MPI_DOUBLE;
    lbmpi->TypesEW[ 3*j+1] = MPI_DOUBLE;
    lbmpi->TypesEW[ 3*j+2] = MPI_DOUBLE;

  } /* for(j=1; j<=get_LY(lattice); j++) */

  ierr = MPI_Type_struct(
         /* int count */                3*get_LY(lattice),
         /* int blocklens[] */          lbmpi->BlockLengthsEW,
         /* MPI_Aint indices[] */       lbmpi->IndicesEW,
         /* MPI_Datatype old_types[] */ lbmpi->TypesEW,
         /* MPI_Datatype *newtype */    &lbmpi->MPI_West2East    );

  ierr = MPI_Type_commit(
         /* MPI_Datatype *datatype */   &lbmpi->MPI_West2East    );

  printf("\n%d: Done making MPI_West2East\n", lbmpi_get_ProcID(lbmpi));


#if 1
  // Output the indices for inspection...
  sprintf( lbmpi->iobuf, " ");
  //for(j=get_sj(lattice); j<=get_ej(lattice); j++)
  for(j=1; j<=get_LY(lattice); j++)
  {
    sprintf( lbmpi->iobuf, "%s%d %d %d ",
      lbmpi->iobuf,
      lbmpi->IndicesEW[ 3*(j-1)+0],
      lbmpi->IndicesEW[ 3*(j-1)+1],
      lbmpi->IndicesEW[ 3*(j-1)+2] );
  }
  printf("\n%d: MPI_West2East { %s }\n", lbmpi_get_ProcID(lbmpi), lbmpi->iobuf);
#endif

} /* void lbmpi_construct( lbmpi_ptr lbmpi, lattice_ptr lattice, argc, argv) */

// void lbmpi_communicate( lbmpi_ptr lbmpi, lattice_ptr lattice)
//
// Communicate data between processes.
//
void lbmpi_communicate( lbmpi_ptr lbmpi, lattice_ptr lattice)
{
} /* void lbmpi_communicate( lbmpi_ptr lbmpi, lattice_ptr lattice) */

// void lbmpi_allocate_data_structures( lbmpi, lattice)
//
// Allocate datatypes for
void lbmpi_allocate_datatypes( lbmpi_ptr lbmpi, lattice_ptr lattice)
{
  //int BlockLengthsEW[3*LY];
  lbmpi->BlockLengthsEW = (int*)malloc( 3*get_LY(lattice)*sizeof(int));

  //MPI_Aint AddrsEW[3*LY];
  lbmpi->AddrsEW = (MPI_Aint*)malloc( 3*get_LY(lattice)*sizeof(MPI_Aint));

  //MPI_Aint IndicesEW[3*LY];
  lbmpi->IndicesEW = (MPI_Aint*)malloc( 3*get_LY(lattice)*sizeof(MPI_Aint));

  //MPI_Datatype TypesEW[3*LY];
  lbmpi->TypesEW = (MPI_Datatype*)malloc( 3*get_LY(lattice)*sizeof(MPI_Datatype));


  //int BlockLengthsNS[3*LX];
  lbmpi->BlockLengthsNS = (int*)malloc( 3*get_LX(lattice)*sizeof(int));

  //MPI_Aint AddrsNS[3*LX];
  lbmpi->AddrsNS = (MPI_Aint*)malloc( 3*get_LX(lattice)*sizeof(MPI_Aint));

  //MPI_Aint IndicesNS[3*LX];
  lbmpi->IndicesNS = (MPI_Aint*)malloc( 3*get_LX(lattice)*sizeof(MPI_Aint));

  //MPI_Datatype TypesNS[3*LX];
  lbmpi->TypesNS = (MPI_Datatype*)malloc( 3*get_LX(lattice)*sizeof(MPI_Datatype));


#if DUMP_AFTER_COMM || DUMP_BEFORE_COMM
  //double fmat[3*LY][3*LX];
  fmat = (double**)malloc(3*get_LY(lattice)*sizeof(double*))
  for(j=1;j<=get_LY(lattice);j++)
  {
    lbmpi->fmat[j] = (double*)malloc(3*get_LX(lattice)*sizeof(double));
  }

#endif /* DUMP_AFTER_COMM || DUMP_BEFORE_COMM */

} /* void lbmpi_allocate_datatypes( lbmpi, lattice) */

int lbmpi_get_ProcID( lbmpi_ptr lbmpi)
{
  return lbmpi->ProcID;
}

MPI_Aint *lbmpi_get_Index0_ptr( lbmpi_ptr lbmpi)
{
  return (MPI_Aint *)(&(lbmpi->Index0));
}

int get_NumProcs( lbmpi_ptr lbmpi) { return lbmpi->NumProcs;}
int get_ProcID( lbmpi_ptr lbmpi) { return lbmpi->ProcID;}

int get_NPX( lbmpi_ptr lbmpi) { return lbmpi->NPX;}
int get_NPY( lbmpi_ptr lbmpi) { return lbmpi->NPY;}
int get_PX( lbmpi_ptr lbmpi) { return lbmpi->PX;}
int get_PY( lbmpi_ptr lbmpi) { return lbmpi->PY;}
int set_GLX( lbmpi_ptr lbmpi, int arg_GLX) { lbmpi->GLX = arg_GLX;}
int set_GLY( lbmpi_ptr lbmpi, int arg_GLY) { lbmpi->GLY = arg_GLY;}
int get_GLX( lbmpi_ptr lbmpi) { return lbmpi->GLX;}
int get_GLY( lbmpi_ptr lbmpi) { return lbmpi->GLY;}
int get_GSX( lbmpi_ptr lbmpi) { return lbmpi->GSX;}
int get_GSY( lbmpi_ptr lbmpi) { return lbmpi->GSY;}
int get_GEX( lbmpi_ptr lbmpi) { return lbmpi->GEX;}
int get_GEY( lbmpi_ptr lbmpi) { return lbmpi->GEY;}

void compute_LX( lattice_ptr lattice, lbmpi_ptr lbmpi)
{
  // First store the global domain size in (GLX,GLY).
  set_GLX( lbmpi, get_LX( lattice));
  set_GLY( lbmpi, get_LY( lattice));

  if( get_GLX(lbmpi) % get_NPX(lbmpi) != 0)
  {
    printf("%s %d >> ERROR: Currently require global domain size"
        "to be divisible by number of processes."
        "LX %% NPX = %d %% %d = %d\n",
      __FILE__,__LINE__,
      get_GLX(lbmpi),  get_NPX(lbmpi),
      get_GLX(lbmpi) % get_NPX(lbmpi)
      );
    process_exit(1);
  }

  set_LX( lattice, get_GLX(lbmpi) / get_NPX(lbmpi));

} /* void compute_LX( lattice_ptr lattice) */

void compute_LY( lattice_ptr lattice, lbmpi_ptr lbmpi)
{
  if( get_GLY(lbmpi) % get_NPY(lbmpi) != 0)
  {
    printf("%s %d >> ERROR: Currently require global domain size"
        "to be divisible by number of processes."
        "GLY %% NPY = %d %% %d = %d\n",
      __FILE__,__LINE__,
      get_GLY(lbmpi),  get_NPY(lbmpi),
      get_GLY(lbmpi) % get_NPY(lbmpi)
      );
    process_exit(1);
  }

  set_LY( lattice, get_GLY(lbmpi) / get_NPY(lbmpi));

} /* void compute_LX( lattice_ptr lattice) */

// void compute_global_coords( lattice_ptr lattice)
void compute_global_coords( lattice_ptr lattice)
{
  lbmpi_ptr lbmpi;

  lbmpi = lattice->lbmpi;

  printf("%s %d >> \n",__FILE__,__LINE__);
  printf("%s %d >> (PX,PY) = (%d,%d)\n",__FILE__,__LINE__,lbmpi->PX,lbmpi->PY);
  printf("%s %d >> \n",__FILE__,__LINE__);

  lbmpi->GSX = lbmpi->PX*get_LX(lattice);
  lbmpi->GEX = lbmpi->GSX + get_LX(lattice)-1;
  lbmpi->GSY = lbmpi->PY*get_LY(lattice);
  lbmpi->GEY = lbmpi->GEY + get_LY(lattice)-1;

  printf("%s %d >> (GSX,GSY)..(GEX,GEY) = (%d,%d)..(%d,%d)\n",
    __FILE__,__LINE__,
    get_GSX(lbmpi),
    get_GSY(lbmpi),
    get_GEX(lbmpi),
    get_GEY(lbmpi) );

} /* void compute_global_coords( lattice_ptr lattice) */

// void lbmpi_distribute_domain( lattice_ptr lattice)
//##############################################################################
//
// When this subroutine is called, sub_matrix_ptr points to the global
// matrix representing solids and pores in the domain.
//
// This routine makes a temporary copy of this global information and
// then reallocates sub_matrix_ptr to point to space for solids/pores
// information for just this process' portion of the domain and
// copies that local info out of the temporary global copy.
//
void lbmpi_distribute_domain( lattice_ptr lattice, int ***sub_matrix_ptr)
{
  lbmpi_ptr lbmpi;
  lbmpi = lattice->lbmpi;
  int **matrix;

  int i,  j;
  int ii, jj;

  // Allocate space for temporary copy of global matrix.
  matrix = (int**)malloc( get_GLY(lbmpi)*sizeof(int*));
  for( j=0; j<get_GLY(lbmpi); j++)
  {
    matrix[j] = (int*)malloc( get_GLX(lbmpi)*sizeof(int));
  }

  // Make temporary copy of global matrix.
  for( j=0; j<get_GLY( lbmpi); j++)
  {
    for( i=0; i<get_GLX( lbmpi); i++)
    {
      matrix[j][i] = (*sub_matrix_ptr)[j][i];
      printf(" %d",matrix[j][i]);
    }
    printf("\n");
  }

  // Free the sub_matrix and reallocate at local dimensions.
  for( j=0; j<get_GLY(lbmpi); j++)
  {
    free( (*sub_matrix_ptr)[j]);
  }
  free( (*sub_matrix_ptr));

  (*sub_matrix_ptr) = (int**)malloc( get_GLY(lbmpi)*sizeof(int*));
  for( j=0; j<get_GLY(lbmpi); j++)
  {
    (*sub_matrix_ptr)[j] = (int*)malloc( get_GLX(lbmpi)*sizeof(int));
  }

  // Copy the local data out of the global matrix.
  printf("%s %d >> jj=%d..%d\n",
      __FILE__,__LINE__, get_GSY(lbmpi), get_GEY(lbmpi));
  printf("%s %d >> ii=%d..%d\n",
      __FILE__,__LINE__, get_GSX(lbmpi), get_GEX(lbmpi));
  j = 0;
  for( jj=get_GSY(lbmpi); jj<=get_GEY(lbmpi); jj++)
  {
    i = 0;
    for( ii=get_GSX(lbmpi); ii<=get_GEX(lbmpi); ii++)
    {
      printf("%s %d >> %d: (i,j)<--(ii,jj), (%d,%d)<--(%d,%d)\n",
        __FILE__,__LINE__,lbmpi->ProcID,i,j,ii,jj);

      (*sub_matrix_ptr)[j][i] = matrix[jj][ii];
      i++;
    }
    j++;
  }

  // Print the newly constructed submatrix.
  printf("\n");
  for( j=0; j<get_LY( lattice); j++)
  {
    for( i=0; i<get_LX( lattice); i++)
    {
      printf(" %d",(*sub_matrix_ptr)[j][i]);
    }
    printf("\n");
  }

} /* void lbmpi_distribute_domain( lattice_ptr lattice) */

// void lbmpi_write_local_bmp( lattice_ptr lattice, int **sub_matrix)
void lbmpi_write_local_bmp( lattice_ptr lattice, int **sub_matrix)
{
  lbmpi_ptr lbmpi;
  bmp_hdr_ptr bmp_hdr;
  char b, g, r;
  int n;
  char filename[1024];
  FILE *bmp;

  printf("lbmpi_write_local_bmp() -- hi!\n");

  lbmpi = lattice->lbmpi;

  // TODO: Create bmp file for this process' piece of the domain.
  printf("\nTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODO\n\n");
  printf("%s %d >> TODO: Implement lbmpi_write_local_bmp\n",__FILE__,__LINE__);
  printf("\n\nTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODO\n");

  bmp_hdr = (bmp_hdr_ptr)malloc( sizeof(struct bmp_hdr_struct));

  sprintf( filename, "./in/%dx%d_id%05d_px%05d_py%05d.bmp",
    get_LX(lattice),
    get_LY(lattice),
    get_NumProcs(lbmpi),
    get_PX(lbmpi),
    get_PY(lbmpi) );

  bmp = fopen(filename,"w+");

  bmp_write_header( bmp, bmp_hdr, get_LX(lattice), get_LY(lattice), /*bits*/24);

  printf("lbmpi_write_local_bmp() -- nn = %d.\n", get_NumNodes(lattice));

  for( n=0; n<get_NumNodes(lattice); n++)
  {
    printf("lbmpi_write_local_bmp() -- n = %d.\n", n);
    if( *(*sub_matrix+n) == 1)
    {
      r = 0;
      g = 0;
      b = 0;
    }
    else
    {
      r = 255;
      g = 255;
      b = 255;
    }
    bmp_write_entry( bmp, bmp_hdr, n, r, g, b);
  }
  fclose(bmp);

  printf("lbmpi_write_local_bmp() -- bye!\n");

} /* void lbmpi_write_local_bmp( lattice_ptr lattice, int **sub_matrix) */

// vim: foldmethod=syntax
