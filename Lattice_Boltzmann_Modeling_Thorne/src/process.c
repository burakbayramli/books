//##############################################################################
//
// process.c
//
//##############################################################################
void process_init( lattice_ptr lattice, int argc, char **argv)
{
#if PARALLEL
  // Routine inititialization calls.
  MPI_Init( &argc, &argv);
  MPI_Comm_size( MPI_COMM_WORLD, &(lattice->process.num_procs));
  MPI_Comm_rank( MPI_COMM_WORLD, &(lattice->process.id));
#else
  lattice->process.id = 0;
  lattice->process.num_procs = 1;
#endif
#if VERBOSITY_LEVEL > 0
  // Say hi.
  printf("Hello >>  ProcID = %d, NumProcs = %d.\n",
    get_proc_id( lattice),
    get_num_procs( lattice) );
#endif
} /* void process_init( lattice_ptr lattice, int argc, char **argv) */

//##############################################################################
void process_compute_local_params( lattice_ptr lattice)
{
#if PARALLEL
  int NumLayersOnRoot;
  int NumLayersPerProc;

  // Save a copy of global dimensions.
  set_g_LX( lattice, get_LX( lattice));
  set_g_LY( lattice, get_LY( lattice));
//3D   set_g_LZ( lattice, get_LZ( lattice));
  set_g_SX( lattice, 0);
  set_g_SY( lattice, 0);
//3D   set_g_SZ( lattice, 0);
  set_g_EX( lattice, get_LX( lattice) - 1);
  set_g_EY( lattice, get_LY( lattice) - 1);
//3D   set_g_EZ( lattice, get_LZ( lattice) - 1);
  set_g_NumNodes( lattice, get_NumNodes( lattice));

  // Adjust local y-dimension according to local subdomain.
  // NOTE: Currently only supports partitioning in y-direction.
  NumLayersOnRoot = get_g_LY( lattice) % get_num_procs( lattice);
  if( NumLayersOnRoot != 0)
  {
    NumLayersPerProc =
      ( get_g_LY( lattice) - NumLayersOnRoot) / (get_num_procs(lattice)-1);
  }
  else
  {
    NumLayersPerProc =
      ( get_g_LY( lattice) - NumLayersOnRoot) / (get_num_procs(lattice));
    NumLayersOnRoot = NumLayersPerProc;
  }

  if( is_on_root_proc( lattice))
  {
    // Assign the left-over (modulus) layers.
    set_LY( lattice, NumLayersOnRoot);
    set_g_SY( lattice, 0);
    set_g_EY( lattice, 0 + NumLayersOnRoot - 1);
    set_g_StartNode( lattice, 0);
  }
  else
  {
    set_LY( lattice, NumLayersPerProc);
    set_g_SY( lattice,
              NumLayersOnRoot
            + NumLayersPerProc*(get_proc_id(lattice)-1) );
    set_g_EY( lattice,
              NumLayersOnRoot
            + NumLayersPerProc*(get_proc_id(lattice)-1)
            + NumLayersPerProc
            - 1);

    set_g_StartNode( lattice,
                     get_g_SY( lattice)*( get_LX(lattice)) );
  }

  set_NumNodes( lattice);

  lattice->process.y_pos_pdf_to_send =
    (double*)malloc( 3*(get_LX(lattice))*sizeof(double));
  lattice->process.y_pos_pdf_to_recv =
    (double*)malloc( 3*(get_LX(lattice))*sizeof(double));
  lattice->process.y_neg_pdf_to_send =
    (double*)malloc( 3*(get_LX(lattice))*sizeof(double));
  lattice->process.y_neg_pdf_to_recv =
    (double*)malloc( 3*(get_LX(lattice))*sizeof(double));
#endif

#if VERBOSITY_LEVEL > 0
#if PARALLEL
  printf(
    "Proc %04d"
    ", g_SX = %d"
    ", g_EX = %d"
    ", g_LX = %d"
    ", g_SY = %d"
    ", g_EY = %d"
    ", g_LY = %d"
    ",   LX = %d"
    ",   LY = %d"
//3D     ", g_SZ = %d"
//3D     ", g_EZ = %d"
//3D     ", g_LZ = %d"
    ", g_StartNode = %d"
    ", g_NumNodes = %d"
    ",   NumNodes = %d"
    ".\n"
    ,get_proc_id( lattice)
    ,get_g_SX( lattice)
    ,get_g_EX( lattice)
    ,get_g_LX( lattice)
    ,get_g_SY( lattice)
    ,get_g_EY( lattice)
    ,get_g_LY( lattice)
    ,get_LX( lattice)
    ,get_LY( lattice)
//3D     ,get_g_SZ( lattice)
//3D     ,get_g_EZ( lattice)
//3D     ,get_g_LZ( lattice)
    ,get_g_StartNode( lattice)
    ,get_g_NumNodes( lattice)
    ,get_NumNodes( lattice)
    );
#endif
#endif

} /* void process_compute_local_params( lattice_ptr lattice) */

//##############################################################################
void process_send_recv_begin( lattice_ptr lattice, const int subs)
{
#if PARALLEL
  int n;
  int i, j, k;
  int ni = get_LX( lattice),
      nj = get_LY( lattice);
  int mpierr;

  // A C C U M U L A T E   P D F S   T O   S E N D
  //#########################################################################
  n = 0;
  j = get_LY(lattice)-1;
//3D   for( j=0; j<nj; j++)
//3D   {
    for( i=0; i<ni; i++)
    {
      lattice->process.y_pos_pdf_to_send[n] =
        lattice->pdf[subs][ XY2N( i , j , ni)].f[ N];
      lattice->process.y_neg_pdf_to_send[n] =
        lattice->pdf[subs][ XY2N( i , 0 , ni)].f[ S];
      n++;
      lattice->process.y_pos_pdf_to_send[n] =
        lattice->pdf[subs][ XY2N( i , j , ni)].f[NW];
      lattice->process.y_neg_pdf_to_send[n] =
        lattice->pdf[subs][ XY2N( i , 0 , ni)].f[SW];
      n++;
      lattice->process.y_pos_pdf_to_send[n] =
        lattice->pdf[subs][ XY2N( i , j , ni)].f[NE];
      lattice->process.y_neg_pdf_to_send[n] =
        lattice->pdf[subs][ XY2N( i , 0 , ni)].f[SE];
      n++;
//3D       lattice->process.y_pos_pdf_to_send[n] =
//3D         lattice->pdf[subs][ XY2N( i , j , ni)].f[TN];
//3D       lattice->process.y_neg_pdf_to_send[n] =
//3D         lattice->pdf[subs][ XY2N( i , j , ni)].f[BN];
//3D       n++;
//3D       lattice->process.y_pos_pdf_to_send[n] =
//3D         lattice->pdf[subs][ XY2N( i , j , ni)].f[TS];
//3D       lattice->process.y_neg_pdf_to_send[n] =
//3D         lattice->pdf[subs][ XY2N( i , j , ni)].f[BS];
//3D       n++;

    } /* if( i=0; i<ni; i++) */
//3D   } /* if( j=0; j<nj; j++) */
#if 0
  // Contrived debug data...
  display_warning_about_contrived_data( lattice);
  n = 0;
  j = 1;
    for( i=0; i<ni; i++)
    {
#if 0
      lattice->process.y_pos_pdf_to_send[n] = 1;
      lattice->process.y_neg_pdf_to_send[n] = 1;
      n++;
      lattice->process.y_pos_pdf_to_send[n] = 2;
      lattice->process.y_neg_pdf_to_send[n] = 2;
      n++;
      lattice->process.y_pos_pdf_to_send[n] = 3;
      lattice->process.y_neg_pdf_to_send[n] = 3;
      n++;
#endif
#if 1
      lattice->process.y_pos_pdf_to_send[n] = j;
      lattice->process.y_neg_pdf_to_send[n] = j;
      n++;
      lattice->process.y_pos_pdf_to_send[n] = j;
      lattice->process.y_neg_pdf_to_send[n] = j;
      n++;
      lattice->process.y_pos_pdf_to_send[n] = j;
      lattice->process.y_neg_pdf_to_send[n] = j;
      n++;

      j++;
#endif
  } /* if( j=0; j<nj; j++) */
#endif

  //process_dump_pdfs_to_send( lattice, "Before Send/Recv");

     // S E N D   I N   P O S I T I V E   D I R E C T I O N
     //#########################################################################
#if VERBOSITY_LEVEL > 1
     printf( "%s %d %04d >> "
       "MPI_Isend( %04d)"
       "\n",
       __FILE__,__LINE__,get_proc_id(lattice),
       (get_proc_id(lattice)+get_num_procs(lattice)+1)%get_num_procs(lattice));
#endif
     mpierr =
       MPI_Isend(
       /*void *buf*/          lattice->process.y_pos_pdf_to_send,
       /*int count*/          3*(get_LX(lattice)),
       /*MPI_Datatype dtype*/ MPI_DOUBLE,
       /*int dest*/         ( get_proc_id(lattice)
                            + get_num_procs(lattice)+1)
                            % get_num_procs(lattice),
       /*int tag*/            0,
       /*MPI_Comm comm*/      MPI_COMM_WORLD,
       /*MPI_Request *req*/   &(lattice->process.send_req_0)
       );
     if( mpierr != MPI_SUCCESS)
     {
       printf( "%s %d %04d >> "
         "ERROR: %d <-- MPI_Isend( %04d)"
         "\n",
         __FILE__,__LINE__,get_proc_id(lattice),
         mpierr,
         (get_proc_id(lattice)+get_num_procs(lattice)+1)%get_num_procs(lattice));
       process_exit(1);
     }
     // R E C V   F R O M   N E G A T I V E   D I R E C T I O N
     //#########################################################################
#if VERBOSITY_LEVEL > 1
     printf( "%s %d %04d >> "
       "MPI_Irecv( %04d)"
       "\n",
       __FILE__,__LINE__,get_proc_id(lattice),
       (get_proc_id(lattice)+get_num_procs(lattice)-1)%get_num_procs(lattice));
#endif
     mpierr =
       MPI_Irecv(
       /*void *buf*/          lattice->process.y_pos_pdf_to_recv,
       /*int count*/          3*(get_LX(lattice)),
       /*MPI_Datatype dtype*/ MPI_DOUBLE,
       /*int src*/          ( get_proc_id(lattice)
                            + get_num_procs(lattice)-1)
                            % get_num_procs(lattice),
       /*int tag*/            0,
       /*MPI_Comm comm*/      MPI_COMM_WORLD,
       /*MPI_Request *req*/   &(lattice->process.recv_req_0)
       );
     if( mpierr != MPI_SUCCESS)
     {
       printf( "%s %d %04d >> "
         "ERROR: %d <-- MPI_Irecv( %04d)"
         "\n",
         __FILE__,__LINE__,get_proc_id(lattice),
         mpierr,
         (get_proc_id(lattice)+get_num_procs(lattice)-1)%get_num_procs(lattice));
       process_exit(1);
     }
     // S E N D   I N   N E G A T I V E   D I R E C T I O N
     //#########################################################################
#if VERBOSITY_LEVEL > 1
     printf( "%s %d %04d >> "
       "MPI_Isend( %04d)"
       "\n",
       __FILE__,__LINE__,get_proc_id(lattice),
       (get_proc_id(lattice)+get_num_procs(lattice)-1)%get_num_procs(lattice));
#endif
     mpierr =
       MPI_Isend(
       /*void *buf*/          lattice->process.y_neg_pdf_to_send,
       /*int count*/          3*(get_LX(lattice)),
       /*MPI_Datatype dtype*/ MPI_DOUBLE,
       /*int dest*/         ( get_proc_id(lattice)
                            + get_num_procs(lattice)-1)
                            % get_num_procs(lattice),
       /*int tag*/            1,
       /*MPI_Comm comm*/      MPI_COMM_WORLD,
       /*MPI_Request *req*/   &(lattice->process.send_req_1)
       );
     if( mpierr != MPI_SUCCESS)
     {
       printf( "%s %d %04d >> "
         "ERROR: %d <-- MPI_Isend( %04d)"
         "\n",
         __FILE__,__LINE__,get_proc_id(lattice),
         mpierr,
         (get_proc_id(lattice)+get_num_procs(lattice)-1)%get_num_procs(lattice));
       process_exit(1);
     }
     // R E C V   F R O M   P O S I T I V E   D I R E C T I O N
     //#########################################################################
#if VERBOSITY_LEVEL > 1
     printf( "%s %d %04d >> "
       "MPI_Irecv( %04d)"
       "\n",
       __FILE__,__LINE__,get_proc_id(lattice),
       (get_proc_id(lattice)+get_num_procs(lattice)+1)%get_num_procs(lattice));
#endif
     mpierr =
       MPI_Irecv(
       /*void *buf*/          lattice->process.y_neg_pdf_to_recv,
       /*int count*/          3*(get_LX(lattice)),
       /*MPI_Datatype dtype*/ MPI_DOUBLE,
       /*int src*/          ( get_proc_id(lattice)
                            + get_num_procs(lattice)+1)
                            % get_num_procs(lattice),
       /*int tag*/            1,
       /*MPI_Comm comm*/      MPI_COMM_WORLD,
       /*MPI_Request *req*/   &(lattice->process.recv_req_1)
       );
     if( mpierr != MPI_SUCCESS)
     {
       printf( "%s %d %04d >> "
         "ERROR: %d <-- MPI_Irecv( %04d)"
         "\n",
         __FILE__,__LINE__,get_proc_id(lattice),
         mpierr,
         (get_proc_id(lattice)+get_num_procs(lattice)+1)%get_num_procs(lattice));
       process_exit(1);
     }
#endif
} /* void process_send_recv_begin( lattice_ptr lattice, const int subs) */

//##############################################################################
void process_send_recv_end( lattice_ptr lattice, const int subs)
{
#if PARALLEL
  int n;
  int i, j; //3D , k;
  int ni = get_LX( lattice),
      nj = get_LY( lattice);
  int ip, in;
  int jp, jn;

  int mpierr;

  mpierr = MPI_Wait(
  /* MPI_Request *req */&(lattice->process.send_req_0),
  /* MPI_Status *stat */&(lattice->process.mpi_status));
  mpierr = MPI_Wait(
  /* MPI_Request *req */&(lattice->process.recv_req_0),
  /* MPI_Status *stat */&(lattice->process.mpi_status));
  mpierr = MPI_Wait(
  /* MPI_Request *req */&(lattice->process.send_req_1),
  /* MPI_Status *stat */&(lattice->process.mpi_status));
  mpierr = MPI_Wait(
  /* MPI_Request *req */&(lattice->process.recv_req_1),
  /* MPI_Status *stat */&(lattice->process.mpi_status));

  //process_dump_pdfs_to_recv( lattice, "After Send/Recv, Before Stream");
  //dump_north_pointing_pdfs( lattice, subs, -1,
  //    "After Send/Recv, Before Stream", 2);
  //dump_south_pointing_pdfs( lattice, subs, -1,
  //    "After Send/Recv, Before Stream", 2);

  // S T R E A M   I N   T H E   B O U N D A R I E S
  //###########################################################################
  n = 0;
  j = get_LY(lattice)-1;
//3D   for( j=0; j<nj; j++)
//3D   {
//3D     jp = ( j<nj-1)?( j+1):( 0   );
//3D     jn = ( j>0   )?( j-1):( nj-1);

    for( i=0; i<ni; i++)
    {
      ip = ( i<ni-1)?( i+1):( 0   );
      in = ( i>0   )?( i-1):( ni-1);

      lattice->pdf[subs][ XY2N( i , 0 , ni)].ftemp[ N] =
        lattice->process.y_pos_pdf_to_recv[n];
      lattice->pdf[subs][ XY2N( i , j , ni)].ftemp[ S] =
        lattice->process.y_neg_pdf_to_recv[n];
      n++;
      lattice->pdf[subs][ XY2N( in, 0 , ni)].ftemp[NW] =
        lattice->process.y_pos_pdf_to_recv[n];
      lattice->pdf[subs][ XY2N( in, j , ni)].ftemp[SW] =
        lattice->process.y_neg_pdf_to_recv[n];
      n++;
      lattice->pdf[subs][ XY2N( ip, 0 , ni)].ftemp[NE] =
        lattice->process.y_pos_pdf_to_recv[n];
      lattice->pdf[subs][ XY2N( ip, j , ni)].ftemp[SE] =
        lattice->process.y_neg_pdf_to_recv[n];
      n++;
//3D       lattice->pdf[subs][ XY2N( i , jp, ni)].ftemp[TN] =
//3D         lattice->process.y_pos_pdf_to_recv[n];
//3D       lattice->pdf[subs][ XY2N( i , jp, ni)].ftemp[BN] =
//3D         lattice->process.y_neg_pdf_to_recv[n];
//3D       n++;
//3D       lattice->pdf[subs][ XY2N( i , jn, ni)].ftemp[TS] =
//3D         lattice->process.y_pos_pdf_to_recv[n];
//3D       lattice->pdf[subs][ XY2N( i , jn, ni)].ftemp[BS] =
//3D         lattice->process.y_neg_pdf_to_recv[n];
//3D       n++;

    } /* if( i=0; i<ni; i++) */
//3D   } /* if( j=0; j<nj; j++) */

#if 0
  // Copy back to check with a call to process_dump_pdfs...
#if 0
  n = 0;
  j = get_LY(lattice)-1;
//3D   for( j=0; j<nj; j++)
//3D   {
    for( i=0; i<ni; i++)
    {
      lattice->process.y_pos_pdf_to_recv[n] =
        lattice->pdf[subs][ XY2N( i , j , ni)].ftemp[ N];
      lattice->process.y_neg_pdf_to_recv[n] =
        lattice->pdf[subs][ XY2N( i , j , ni)].ftemp[ S];
      n++;
      lattice->process.y_pos_pdf_to_recv[n] =
        lattice->pdf[subs][ XY2N( i , j , ni)].ftemp[NW];
      lattice->process.y_neg_pdf_to_recv[n] =
        lattice->pdf[subs][ XY2N( i , j , ni)].ftemp[SW];
      n++;
      lattice->process.y_pos_pdf_to_recv[n] =
        lattice->pdf[subs][ XY2N( i , j , ni)].ftemp[NE];
      lattice->process.y_neg_pdf_to_recv[n] =
        lattice->pdf[subs][ XY2N( i , j , ni)].ftemp[SE];
      n++;
//3D       lattice->process.y_pos_pdf_to_recv[n] =
//3D         lattice->pdf[subs][ XY2N( i , j , ni)].ftemp[TN];
//3D       lattice->process.y_neg_pdf_to_recv[n] =
//3D         lattice->pdf[subs][ XY2N( i , j , ni)].ftemp[BN];
//3D       n++;
//3D       lattice->process.y_pos_pdf_to_recv[n] =
//3D         lattice->pdf[subs][ XY2N( i , j , ni)].ftemp[TS];
//3D       lattice->process.y_neg_pdf_to_recv[n] =
//3D         lattice->pdf[subs][ XY2N( i , j , ni)].ftemp[BS];
//3D       n++;

    } /* if( i=0; i<ni; i++) */
//3D   } /* if( j=0; j<nj; j++) */
#else
  gather_north_pointing_pdfs( lattice,
                              lattice->process.y_pos_pdf_to_recv,
                              subs, get_LY(lattice)-1, 2);
  gather_south_pointing_pdfs( lattice,
                              lattice->process.y_neg_pdf_to_recv,
                              subs, 0, 2);
#endif

  //process_dump_pdfs_to_recv( lattice, "After Stream");

#endif

#endif
} /* void process_send_recv_end( lattice_ptr lattice, const int subs) */

//##############################################################################
void process_dump_pdfs_to_recv( lattice_ptr lattice, char *comment_str)
{
#if PARALLEL
  char new_comment[1024];
  sprintf( new_comment, "pos pdfs to recv, %s", comment_str);
  process_dump_pdfs(
    lattice,
    new_comment,
    lattice->process.y_pos_pdf_to_recv);
  sprintf( new_comment, "neg pdfs to recv, %s", comment_str);
  process_dump_pdfs(
    lattice,
    new_comment,
    lattice->process.y_neg_pdf_to_recv);
#endif
} /* void process_dump_pdfs_to_recv( lattice_ptr lattice)  */

//##############################################################################
void process_dump_pdfs_to_send( lattice_ptr lattice, char *comment_str)
{
#if PARALLEL
  char new_comment[1024];
  sprintf( new_comment, "pos pdfs to send, %s", comment_str);
  process_dump_pdfs(
    lattice,
    new_comment,
    lattice->process.y_pos_pdf_to_send);
  sprintf( new_comment, "neg pdfs to send, %s", comment_str);
  process_dump_pdfs(
    lattice,
    new_comment,
    lattice->process.y_neg_pdf_to_send);
#endif
} /* void process_dump_pdfs_to_send( lattice_ptr lattice)  */

//##############################################################################
void process_dump_pdfs( lattice_ptr lattice, char *comment_str, double *pdfs)
{
#if PARALLEL
  int n, p;
  int i, j, k;
  int ni = get_LX( lattice),
      nj = get_LY( lattice);
  int mpierr;

  for( p=0; p<get_num_procs( lattice); p++)
  {
    MPI_Barrier( MPI_COMM_WORLD);

    if( p == get_proc_id(lattice))
    {
      printf("\n\n// Proc %d, \"%s\".", get_proc_id( lattice), comment_str);
      printf("\n ");
      for( i=0; i<ni; i++)
      {
        printf("+");
        printf("---");
        printf("---");
        printf("---");
        printf("-");
      }
      printf("+");

//3D       for( j=0; j<nj; j++)
//3D       {
        // 0 1 2 3 4
        // O W E N S

        // South
//3D         n = 5*j*ni + 4;
//3D         printf("\n ");
//3D         for( i=0; i<ni; i++)
//3D         {
//3D           printf("|");
//3D           printf("   ");
//3D           printf(" %2.0f", pdfs[n]);
//3D           printf("   ");
//3D           printf(" ");
//3D           n+=5;
//3D         }
//3D         printf("|");

        // West/O/East
        n = 0;
        printf("\n ");
        for( i=0; i<ni; i++)
        {
          printf("|");
          printf(" %2.0f", pdfs[n+1]);
          printf(" %2.0f", pdfs[n]);
          printf(" %2.0f", pdfs[n+2]);
          printf(" ");
          n+=3;
        }
        printf("|");

        // North
//3D         n = 5*j*ni + 3;
//3D         printf("\n ");
//3D         for( i=0; i<ni; i++)
//3D         {
//3D           printf("|");
//3D           printf("   ");
//3D           printf(" %2.0f", pdfs[n]);
//3D           printf("   ");
//3D           printf(" ");
//3D           n+=5;
//3D         }
//3D         printf("|");

        printf("\n ");
        for( i=0; i<ni; i++)
        {
          printf("+");
          printf("---");
          printf("---");
          printf("---");
          printf("-");
        }
        printf("+");

//3D       } /* if( j=0; j<nj; j++) */
    } /* if( p == get_proc_id(lattice)) */
  } /* for( p=0; p<get_num_procs( lattice); p++) */

  MPI_Barrier( MPI_COMM_WORLD);

#endif
} /* void process_dump_pdfs_to_recv( lattice_ptr lattice)  */

//##############################################################################
void gather_north_pointing_pdfs(
       lattice_ptr lattice,
       double *north,
       const int subs,
       const int k,
       const int which_pdf)
{
  int i, j, n;
  int ni = get_LX( lattice),
      nj = get_LY( lattice);

  switch(which_pdf)
  {
    case 0:
      n = 0;
//3D       for( j=0; j<nj; j++)
//3D       {
        for( i=0; i<ni; i++)
        {
          north[n] = lattice->pdf[subs][ XY2N(i,j, ni)].feq[ N]; n++;
          north[n] = lattice->pdf[subs][ XY2N(i,j, ni)].feq[NW]; n++;
          north[n] = lattice->pdf[subs][ XY2N(i,j, ni)].feq[NE]; n++;
//3D           north[n] = lattice->pdf[subs][ XY2N(i,j, ni)].feq[TN]; n++;
//3D           north[n] = lattice->pdf[subs][ XY2N(i,j, ni)].feq[TS]; n++;

        } /* if( i=0; i<ni; i++) */
//3D       } /* if( j=0; j<nj; j++) */
      break;
    case 1:
      n = 0;
//3D       for( j=0; j<nj; j++)
//3D       {
        for( i=0; i<ni; i++)
        {
          north[n] = lattice->pdf[subs][ XY2N(i,j, ni)].f[ N]; n++;
          north[n] = lattice->pdf[subs][ XY2N(i,j, ni)].f[NW]; n++;
          north[n] = lattice->pdf[subs][ XY2N(i,j, ni)].f[NE]; n++;
//3D           north[n] = lattice->pdf[subs][ XY2N(i,j, ni)].f[TN]; n++;
//3D           north[n] = lattice->pdf[subs][ XY2N(i,j, ni)].f[TS]; n++;

        } /* if( i=0; i<ni; i++) */
//3D       } /* if( j=0; j<nj; j++) */
      break;
    case 2:
      n = 0;
//3D       for( j=0; j<nj; j++)
//3D       {
        for( i=0; i<ni; i++)
        {
          north[n] = lattice->pdf[subs][ XY2N(i,j, ni)].ftemp[ N]; n++;
          north[n] = lattice->pdf[subs][ XY2N(i,j, ni)].ftemp[NW]; n++;
          north[n] = lattice->pdf[subs][ XY2N(i,j, ni)].ftemp[NE]; n++;
//3D           north[n] = lattice->pdf[subs][ XY2N(i,j, ni)].ftemp[TN]; n++;
//3D           north[n] = lattice->pdf[subs][ XY2N(i,j, ni)].ftemp[TS]; n++;

        } /* if( i=0; i<ni; i++) */
//3D       } /* if( j=0; j<nj; j++) */
      break;
    default:
      printf("%s %d %04d >> ERROR: Unhandled case which_pdf=%d. Exiting!",
        __FILE__,__LINE__,get_proc_id(lattice), which_pdf);
      process_exit(1);
      break;
  } /* switch(which_pdf) */
} /* void gather_north_pointing_pdfs( lattice_ptr lattice, double *north) */

//##############################################################################
void gather_south_pointing_pdfs(
       lattice_ptr lattice,
       double *south,
       const int subs,
       const int k,
       const int which_pdf)
{
  int i, j, n;
  int ni = get_LX( lattice),
      nj = get_LY( lattice);
  switch(which_pdf)
  {
    case 0:
      n = 0;
//3D       for( j=0; j<nj; j++)
//3D       {
        for( i=0; i<ni; i++)
        {
          south[n] = lattice->pdf[subs][ XY2N(i,j, ni)].feq[ S]; n++;
          south[n] = lattice->pdf[subs][ XY2N(i,j, ni)].feq[SW]; n++;
          south[n] = lattice->pdf[subs][ XY2N(i,j, ni)].feq[SE]; n++;
//3D           south[n] = lattice->pdf[subs][ XY2N(i,j, ni)].feq[BN]; n++;
//3D           south[n] = lattice->pdf[subs][ XY2N(i,j, ni)].feq[BS]; n++;

        } /* if( i=0; i<ni; i++) */
//3D       } /* if( j=0; j<nj; j++) */
      break;
    case 1:
      n = 0;
//3D       for( j=0; j<nj; j++)
//3D       {
        for( i=0; i<ni; i++)
        {
          south[n] = lattice->pdf[subs][ XY2N(i,j, ni)].f[ S]; n++;
          south[n] = lattice->pdf[subs][ XY2N(i,j, ni)].f[SW]; n++;
          south[n] = lattice->pdf[subs][ XY2N(i,j, ni)].f[SE]; n++;
//3D           south[n] = lattice->pdf[subs][ XY2N(i,j, ni)].f[BN]; n++;
//3D           south[n] = lattice->pdf[subs][ XY2N(i,j, ni)].f[BS]; n++;

        } /* if( i=0; i<ni; i++) */
//3D       } /* if( j=0; j<nj; j++) */
      break;
    case 2:
      n = 0;
//3D       for( j=0; j<nj; j++)
//3D       {
        for( i=0; i<ni; i++)
        {
          south[n] = lattice->pdf[subs][ XY2N(i,j, ni)].ftemp[ S]; n++;
          south[n] = lattice->pdf[subs][ XY2N(i,j, ni)].ftemp[SW]; n++;
          south[n] = lattice->pdf[subs][ XY2N(i,j, ni)].ftemp[SE]; n++;
//3D           south[n] = lattice->pdf[subs][ XY2N(i,j, ni)].ftemp[BN]; n++;
//3D           south[n] = lattice->pdf[subs][ XY2N(i,j, ni)].ftemp[BS]; n++;

        } /* if( i=0; i<ni; i++) */
//3D       } /* if( j=0; j<nj; j++) */
      break;
    default:
      printf("%s %d %04d >> ERROR: Unhandled case which_pdf=%d. Exiting!",
        __FILE__,__LINE__,get_proc_id(lattice), which_pdf);
      process_exit(1);
      break;
  }
} /* void gather_south_pointing_pdfs( lattice_ptr lattice, double *south) */

void process_reduce_double_sum( lattice_ptr lattice, double *arg_x)
{
#if PARALLEL
  double sum_x;
  int mpierr;

  //
  // INPUT PARAMETERS
  //        sbuf   - address of send buffer (choice)
  //        count  - number of elements in send buffer (integer)
  //        dtype  - data type of elements of send buffer (handle)
  //        op     - reduce operation (handle)
  //        root   - rank of root process (integer)
  //        comm   - communicator (handle)
  //
  // OUTPUT PARAMETER
  //        rbuf   - address of receive buffer (choice, sig't only at root )
  //
  mpierr =
    MPI_Allreduce(
    /*void *sbuf*/         arg_x,
    /*void* rbuf*/        &sum_x,
    /*int count*/          1,
    /*MPI_Datatype dtype*/ MPI_DOUBLE,
    /*MPI_Op op*/          MPI_SUM,
    /*MPI_Comm comm*/      MPI_COMM_WORLD
    );
  if( mpierr != MPI_SUCCESS)
  {
    printf( "%s %d %04d >> "
      "ERROR: %d <-- MPI_Reduce( ave_rho, MPI_DOUBLE, MPI_SUM)"
      "\n",
      __FILE__,__LINE__,get_proc_id(lattice), mpierr);
    process_exit(1);
  }
#if 0
  if( is_on_root_proc( lattice))
  {
    *arg_x = sum_x;
  }
  mpierr =
    MPI_Bcast(
    /*void *buffer*/          arg_x,
    /*int count*/             1,
    /*MPI_Datatype datatype*/ MPI_DOUBLE,
    /*int root*/              0,
    /*MPI_Comm comm*/         MPI_COMM_WORLD
    );
  if( mpierr != MPI_SUCCESS)
  {
    printf( "%s %d %04d >> "
      "ERROR: %d <-- MPI_Reduce( ave_rho, MPI_DOUBLE, MPI_SUM)"
      "\n",
      __FILE__,__LINE__,get_proc_id(lattice), mpierr);
    process_exit(1);
  }
#endif
#endif
} /* void process_reduce_double_sum( lattice_ptr lattice, double &arg_x) */

void process_reduce_int_sum( lattice_ptr lattice, int *arg_n)
{
#if PARALLEL
  double sum_n;
  int mpierr;

  //
  // INPUT PARAMETERS
  //        sbuf   - address of send buffer (choice)
  //        count  - number of elements in send buffer (integer)
  //        dtype  - data type of elements of send buffer (handle)
  //        op     - reduce operation (handle)
  //        root   - rank of root process (integer)
  //        comm   - communicator (handle)
  //
  // OUTPUT PARAMETER
  //        rbuf   - address of receive buffer (choice, sig't only at root )
  //
  mpierr =
    MPI_Allreduce(
    /*void *sbuf*/         arg_n,
    /*void* rbuf*/        &sum_n,
    /*int count*/          1,
    /*MPI_Datatype dtype*/ MPI_INT,
    /*MPI_Op op*/          MPI_SUM,
    /*MPI_Comm comm*/      MPI_COMM_WORLD
    );
  if( mpierr != MPI_SUCCESS)
  {
    printf( "%s %d %04d >> "
      "ERROR: %d <-- MPI_Reduce( ave_rho, MPI_DOUBLE, MPI_SUM)"
      "\n",
      __FILE__,__LINE__,get_proc_id(lattice), mpierr);
    process_exit(1);
  }
#if 0
  if( is_on_root_proc( lattice))
  {
    *arg_n = sum_n;
  }
  mpierr =
    MPI_Bcast(
    /*void *buffer*/          arg_n,
    /*int count*/             1,
    /*MPI_Datatype datatype*/ MPI_DOUBLE,
    /*int root*/              0,
    /*MPI_Comm comm*/         MPI_COMM_WORLD
    );
  if( mpierr != MPI_SUCCESS)
  {
    printf( "%s %d %04d >> "
      "ERROR: %d <-- MPI_Reduce( ave_rho, MPI_DOUBLE, MPI_SUM)"
      "\n",
      __FILE__,__LINE__,get_proc_id(lattice), mpierr);
    process_exit(1);
  }
#endif
#endif
} /* void process_reduce_int_sum( lattice_ptr lattice, int *arg_n) */

void process_reduce_double_max( lattice_ptr lattice, double *arg_x)
{
#if PARALLEL
  double max_x;
  int mpierr;

  //
  // INPUT PARAMETERS
  //        sbuf   - address of send buffer (choice)
  //        count  - number of elements in send buffer (integer)
  //        dtype  - data type of elements of send buffer (handle)
  //        op     - reduce operation (handle)
  //        root   - rank of root process (integer)
  //        comm   - communicator (handle)
  //
  // OUTPUT PARAMETER
  //        rbuf   - address of receive buffer (choice, sig't only at root )
  //
  mpierr =
    MPI_Allreduce(
    /*void *sbuf*/         arg_x,
    /*void* rbuf*/        &max_x,
    /*int count*/          1,
    /*MPI_Datatype dtype*/ MPI_DOUBLE,
    /*MPI_Op op*/          MPI_MAX,
    /*MPI_Comm comm*/      MPI_COMM_WORLD
    );
  if( mpierr != MPI_SUCCESS)
  {
    printf( "%s %d %04d >> "
      "ERROR: %d <-- MPI_Reduce( ave_rho, MPI_DOUBLE, MPI_SUM)"
      "\n",
      __FILE__,__LINE__,get_proc_id(lattice), mpierr);
    process_exit(1);
  }
#if 0
  if( is_on_root_proc( lattice))
  {
    *arg_x = max_x;
  }
  mpierr =
    MPI_Bcast(
    /*void *buffer*/          arg_x,
    /*int count*/             1,
    /*MPI_Datatype datatype*/ MPI_DOUBLE,
    /*int root*/              0,
    /*MPI_Comm comm*/         MPI_COMM_WORLD
    );
  if( mpierr != MPI_SUCCESS)
  {
    printf( "%s %d %04d >> "
      "ERROR: %d <-- MPI_Reduce( ave_rho, MPI_DOUBLE, MPI_SUM)"
      "\n",
      __FILE__,__LINE__,get_proc_id(lattice), mpierr);
    process_exit(1);
  }
#endif
#endif
} /* void process_reduce_double_sum( lattice_ptr lattice, double &arg_x) */

void process_reduce_double_min( lattice_ptr lattice, double *arg_x)
{
#if PARALLEL
  double min_x;
  int mpierr;

  //
  // INPUT PARAMETERS
  //        sbuf   - address of send buffer (choice)
  //        count  - number of elements in send buffer (integer)
  //        dtype  - data type of elements of send buffer (handle)
  //        op     - reduce operation (handle)
  //        root   - rank of root process (integer)
  //        comm   - communicator (handle)
  //
  // OUTPUT PARAMETER
  //        rbuf   - address of receive buffer (choice, sig't only at root )
  //
  mpierr =
    MPI_Allreduce(
    /*void *sbuf*/         arg_x,
    /*void* rbuf*/        &min_x,
    /*int count*/          1,
    /*MPI_Datatype dtype*/ MPI_DOUBLE,
    /*MPI_Op op*/          MPI_MIN,
    /*MPI_Comm comm*/      MPI_COMM_WORLD
    );
  if( mpierr != MPI_SUCCESS)
  {
    printf( "%s %d %04d >> "
      "ERROR: %d <-- MPI_Reduce( ave_rho, MPI_DOUBLE, MPI_SUM)"
      "\n",
      __FILE__,__LINE__,get_proc_id(lattice), mpierr);
    process_exit(1);
  }
#if 0
  if( is_on_root_proc( lattice))
  {
    *arg_x = min_x;
  }
  mpierr =
    MPI_Bcast(
    /*void *buffer*/          arg_x,
    /*int count*/             1,
    /*MPI_Datatype datatype*/ MPI_DOUBLE,
    /*int root*/              0,
    /*MPI_Comm comm*/         MPI_COMM_WORLD
    );
  if( mpierr != MPI_SUCCESS)
  {
    printf( "%s %d %04d >> "
      "ERROR: %d <-- MPI_Reduce( ave_rho, MPI_DOUBLE, MPI_SUM)"
      "\n",
      __FILE__,__LINE__,get_proc_id(lattice), mpierr);
    process_exit(1);
  }
#endif
#endif
} /* void process_reduce_double_sum( lattice_ptr lattice, double &arg_x) */

//##############################################################################
void process_barrier()
{
#if PARALLEL
  MPI_Barrier( MPI_COMM_WORLD);
#endif
}

//##############################################################################
void process_finalize()
{
#if PARALLEL
  MPI_Finalize();
#endif
}

//##############################################################################
void process_exit( int exit_val)
{
  process_finalize();
  exit( exit_val);
}

//##############################################################################
//
// Accessor methods for the process struct.
//

int get_proc_id( lattice_ptr lattice) { return lattice->process.id;}
int get_num_procs( lattice_ptr lattice) { return lattice->process.num_procs;}
int is_on_root_proc( lattice_ptr lattice) { return !(lattice->process.id);}

#if PARALLEL
int get_g_LX( lattice_ptr lattice) { return lattice->process.g_LX;}
int get_g_LY( lattice_ptr lattice) { return lattice->process.g_LY;}
//3D int get_g_LZ( lattice_ptr lattice) { return lattice->process.g_LZ;}
int get_g_SX( lattice_ptr lattice) { return lattice->process.g_SX;}
int get_g_SY( lattice_ptr lattice) { return lattice->process.g_SY;}
//3D int get_g_SZ( lattice_ptr lattice) { return lattice->process.g_SZ;}
int get_g_EX( lattice_ptr lattice) { return lattice->process.g_EX;}
int get_g_EY( lattice_ptr lattice) { return lattice->process.g_EY;}
//3D int get_g_EZ( lattice_ptr lattice) { return lattice->process.g_EZ;}

void set_g_LX( lattice_ptr lattice, const int arg_LX)
{
  lattice->process.g_LX = arg_LX;
}
void set_g_LY( lattice_ptr lattice, const int arg_LY)
{
  lattice->process.g_LY = arg_LY;
}
//3D void set_g_LZ( lattice_ptr lattice, const int arg_LZ)
//3D {
//3D   lattice->process.g_LZ = arg_LZ;
//3D }

void set_g_SX( lattice_ptr lattice, const int arg_SX)
{
  lattice->process.g_SX = arg_SX;
}
void set_g_SY( lattice_ptr lattice, const int arg_SY)
{
  lattice->process.g_SY = arg_SY;
}
//3D void set_g_SZ( lattice_ptr lattice, const int arg_SZ)
//3D {
//3D   lattice->process.g_SZ = arg_SZ;
//3D }

void set_g_EX( lattice_ptr lattice, const int arg_EX)
{
  lattice->process.g_EX = arg_EX;
}
void set_g_EY( lattice_ptr lattice, const int arg_EY)
{
  lattice->process.g_EY = arg_EY;
}
//3D void set_g_EZ( lattice_ptr lattice, const int arg_EZ)
//3D {
//3D   lattice->process.g_EZ = arg_EZ;
//3D }

int get_g_NumNodes( lattice_ptr lattice) { return lattice->process.g_NumNodes;}
void set_g_NumNodes( lattice_ptr lattice, const int arg_NumNodes)
{
  lattice->process.g_NumNodes = arg_NumNodes;
}
void set_g_StartNode( lattice_ptr lattice, const int arg_n)
{
  lattice->process.g_StartNode = arg_n;
}
int get_g_StartNode( lattice_ptr lattice)
{
  return lattice->process.g_StartNode;
}

double g2lx( lattice_ptr lattice, double g_x)
{
  //  1 g_ey=7 -o
  //    g_y =6  o- y = g_y - g_sy = 6 - 4 = 2
  //            o
  //  1 g_sy=4 -o
  //  0 g_ey=3 -o
  //            o
  //            o
  //  0 g_sy=0 -o
  return g_x - get_g_SX(lattice);
}
double g2ly( lattice_ptr lattice, double g_y)
{
  return g_y - get_g_SY(lattice);
}

#else
// Defaults for non-parallel runs.
int get_g_LX( lattice_ptr lattice) { return get_LX( lattice);}
int get_g_LY( lattice_ptr lattice) { return get_LY( lattice);}
//3D int get_g_LZ( lattice_ptr lattice) { return get_LZ( lattice);}
int get_g_SX( lattice_ptr lattice) { return 0;}
int get_g_SY( lattice_ptr lattice) { return 0;}
//3D int get_g_SZ( lattice_ptr lattice) { return 0;}
int get_g_EX( lattice_ptr lattice) { return get_LX( lattice)-1;}
int get_g_EY( lattice_ptr lattice) { return get_LY( lattice)-1;}
//3D int get_g_EZ( lattice_ptr lattice) { return get_LZ( lattice)-1;}
int get_g_NumNodes( lattice_ptr lattice) { return get_NumNodes( lattice);}
int get_g_StartNode( lattice_ptr lattice) { return 0;}
double g2lx( lattice_ptr lattice, double g_x) { return g_x;}
double g2ly( lattice_ptr lattice, double g_y) { return g_y;}
#endif

// vim: foldmethod=syntax
