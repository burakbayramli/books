//##############################################################################
//
// Copyright (C), 2005, Michael Sukop and Danny Thorne
//
// all_nodes_latman.c
//
//  - Lattice Manager.
//
//  - Routines for managing a lattice:
//
//    - construct
//    - init
//    - destruct
//
//  - This file, with prefix "all_nodes_", is for the version of the code
//    that stores all nodes of the domain even if they are interior solid
//    nodes that are not involved in any computations.  This is more
//    efficient, in spite of storing unused nodes, if the ratio of
//    interior solid nodes to total nodes is sufficiently low.  How
//    low is sufficient? is a difficult question.  If storage is the
//    only consideration, then the two approaches balance at somewhere
//    around .75 .  But more observations need to be made to characterize
//    the trade-off in terms of computational efficiency.
//

// void process_matrix( struct lattice_struct *lattice, int **matrix)
//##############################################################################
//
// P R O C E S S   M A T R I X
//
//  - Process the soil matrix.
//
//  - Convert the full matrix representation into sparse lattice form.
//
//  - This routine is useful when the domain is read from a BMP file
//    representing the full lattice.  If more general mechanisms
//    are developed (e.g., reading parameterized fracture information),
//    then determining the sparse lattice data structure will require a
//    corresponding mechanism (and storing the full lattice, even for
//    pre-/post-processing, may become undesirable if not impossible).
//
void process_matrix( struct lattice_struct *lattice, int **matrix, int subs)
{
  // Variable declarations.
  int i,  j;
  int in, jn;
  int ip, jp;
  int ei, ej;
  int n;
  int NumActiveNodes;

  // Ending indices.
  ei = get_LX(lattice)-1;
  ej = get_LY(lattice)-1;

  // Mark solid nodes that have fluid nodes as neighbors and
  // count the total number of nodes requiring storage.
  NumActiveNodes = 0;
  if( lattice->periodic_y[subs])
  {
    for( j=0; j<=ej; j++)
    {
      jp = ( j<ej) ? ( j+1) : ( 0 );
      jn = ( j>0 ) ? ( j-1) : ( ej);

      for( i=0; i<=ei; i++)
      {
        ip = ( i<ei) ? ( i+1) : ( 0 );
        in = ( i>0 ) ? ( i-1) : ( ei);

        if( matrix[j ][i ] == 0)
        {
          NumActiveNodes++;

          if( matrix[j ][ip] == 1) { matrix[j ][ip] = 2; NumActiveNodes++;}
          if( matrix[jp][i ] == 1) { matrix[jp][i ] = 2; NumActiveNodes++;}
          if( matrix[j ][in] == 1) { matrix[j ][in] = 2; NumActiveNodes++;}
          if( matrix[jn][i ] == 1) { matrix[jn][i ] = 2; NumActiveNodes++;}
          if( matrix[jp][ip] == 1) { matrix[jp][ip] = 2; NumActiveNodes++;}
          if( matrix[jp][in] == 1) { matrix[jp][in] = 2; NumActiveNodes++;}
          if( matrix[jn][in] == 1) { matrix[jn][in] = 2; NumActiveNodes++;}
          if( matrix[jn][ip] == 1) { matrix[jn][ip] = 2; NumActiveNodes++;}

        }

      } /* for( i=0; i<=ei; i++) */
    } /* for( j=0; j<=ej; j++) */
  } /* if( lattice->periodic_y[subs]) */

  else /* !lattice->periodic_y[subs] */
  {
    j  = 0;
    jp = 1;
    for( i=0; i<=ei; i++)
    {
      ip = ( i<ei) ? ( i+1) : ( 0 );
      in = ( i>0 ) ? ( i-1) : ( ei);

      if( matrix[j ][i ] == 0)
      {
        NumActiveNodes++;

        if( matrix[j ][ip] == 1) { matrix[j ][ip] = 2; NumActiveNodes++;}
        if( matrix[jp][i ] == 1) { matrix[jp][i ] = 2; NumActiveNodes++;}
        if( matrix[j ][in] == 1) { matrix[j ][in] = 2; NumActiveNodes++;}
        //if( matrix[jn][i ] == 1) { matrix[jn][i ] = 2; NumActiveNodes++;}
        if( matrix[jp][ip] == 1) { matrix[jp][ip] = 2; NumActiveNodes++;}
        if( matrix[jp][in] == 1) { matrix[jp][in] = 2; NumActiveNodes++;}
        //if( matrix[jn][in] == 1) { matrix[jn][in] = 2; NumActiveNodes++;}
        //if( matrix[jn][ip] == 1) { matrix[jn][ip] = 2; NumActiveNodes++;}

      }

    } /* for( i=0; i<=ei; i++) */


    for( j=1; j<ej; j++)
    {
      jp = j+1;
      jn = j-1;

      for( i=0; i<=ei; i++)
      {
        ip = ( i<ei) ? ( i+1) : ( 0 );
        in = ( i>0 ) ? ( i-1) : ( ei);

        if( matrix[j ][i ] == 0)
        {
          NumActiveNodes++;

          if( matrix[j ][ip] == 1) { matrix[j ][ip] = 2; NumActiveNodes++;}
          if( matrix[jp][i ] == 1) { matrix[jp][i ] = 2; NumActiveNodes++;}
          if( matrix[j ][in] == 1) { matrix[j ][in] = 2; NumActiveNodes++;}
          if( matrix[jn][i ] == 1) { matrix[jn][i ] = 2; NumActiveNodes++;}
          if( matrix[jp][ip] == 1) { matrix[jp][ip] = 2; NumActiveNodes++;}
          if( matrix[jp][in] == 1) { matrix[jp][in] = 2; NumActiveNodes++;}
          if( matrix[jn][in] == 1) { matrix[jn][in] = 2; NumActiveNodes++;}
          if( matrix[jn][ip] == 1) { matrix[jn][ip] = 2; NumActiveNodes++;}

        }

      } /* for( i=0; i<=ei; i++) */
    } /* for( j=0; j<=ej; j++) */

    j  = ej;
    jn = ej-1;
    for( i=0; i<=ei; i++)
    {
      ip = ( i<ei) ? ( i+1) : ( 0 );
      in = ( i>0 ) ? ( i-1) : ( ei);

      if( matrix[j ][i ] == 0)
      {
        NumActiveNodes++;

        if( matrix[j ][ip] == 1) { matrix[j ][ip] = 2; NumActiveNodes++;}
        //if( matrix[jp][i ] == 1) { matrix[jp][i ] = 2; NumActiveNodes++;}
        if( matrix[j ][in] == 1) { matrix[j ][in] = 2; NumActiveNodes++;}
        if( matrix[jn][i ] == 1) { matrix[jn][i ] = 2; NumActiveNodes++;}
        //if( matrix[jp][ip] == 1) { matrix[jp][ip] = 2; NumActiveNodes++;}
        //if( matrix[jp][in] == 1) { matrix[jp][in] = 2; NumActiveNodes++;}
        if( matrix[jn][in] == 1) { matrix[jn][in] = 2; NumActiveNodes++;}
        if( matrix[jn][ip] == 1) { matrix[jn][ip] = 2; NumActiveNodes++;}

      }

    } /* for( i=0; i<=ei; i++) */

  } /* if( lattice->periodic_y[subs]) else */

#if VERBOSITY_LEVEL > 0
  printf( "[%s,%d] process_matrix() -- NumActiveNodes = %d\n",
    __FILE__, __LINE__, NumActiveNodes);
#endif /* VERBOSITY_LEVEL > 0 */


#if 0 // Dump the matrix contents to the screen.
  for( j=0; j<=ej; j++)
  {
    for( i=0; i<=ei; i++)
    {
      printf(" %d", matrix[j][i]);
    }
    printf("\n");
  }
  //process_exit(1);
#endif


  // Set lattice->NumNodes in the lattice.
  lattice->NumNodes = get_LX(lattice) * get_LY(lattice);

#if VERBOSITY_LEVEL > 0
  printf("[%s,%d] process_matrix() -- NumNodes = %d\n",
    __FILE__,__LINE__, lattice->NumNodes);
#endif /* VERBOSITY_LEVEL > 0 */

  // Allocate memory for lattice->NumNodes boundary conditions.
  lattice->bc[subs]=
    ( struct bc_struct*)malloc( lattice->NumNodes*sizeof( struct bc_struct));
  assert( lattice->bc[subs]!=NULL);

  // Set coordinates and index of lattice nodes.
  // Use matrix entries to store pointers to associated nodes to
  // facilitate finding neighbors on a following traversal.
  for( j=0; j<=ej; j++)
  {
    n = j*get_LX(lattice);

    for( i=0; i<=ei; i++, n++)
    {
      switch( matrix[j][i])
      {
        case 0: lattice->bc[subs][n].bc_type = 0;             break;
        case 1: lattice->bc[subs][n].bc_type = INACTIVE_NODE; break;
        case 2: lattice->bc[subs][n].bc_type = BC_SOLID_NODE; break;
        default:
          printf("%s %d >> process_matrix() -- "
              "Unhandled case matrix[%d][%d]=%d. Exiting!\n",
              __FILE__,__LINE__,
              j, i, matrix[j][i] );
          process_exit(1);
          break;
      }
    } /* for( i=0; i<=ei; i++) */
  } /* for( j=0; j<=ej; j++) */


#if 0 // Dump the matrix contents to the screen.
  for( j=0; j<=ej; j++)
  {
    for( i=0; i<=ei; i++)
    {
      printf(" %d", matrix[j][i]);
    }
    printf("\n");
  }
#endif

#if 0 // Dump BCs to screen.
  for( n=0; n<lattice->NumNodes; n++)
  {
    printf("%d (%d,%d), %d\n",
      n,
      n%get_LX(lattice),
      n/get_LX(lattice),
      lattice->bc[subs][n].bc_type);
  }
  printf("\n");
  for( n=0, j=0; j<=ej; j++)
  {
    for( i=0; i<=ei; i++, n++)
    {
      printf(" %d", lattice->bc[subs][n].bc_type);
    }
    printf("\n");
  }
#endif

} /* void process_matrix( struct lattice_struct *lattice, int **matrix) */

// void construct_lattice( struct lattice_struct *lattice)
//##############################################################################
//
// C O N S T R U C T   L A T T I C E
//
//  - Construct lattice.
//
void construct_lattice( lattice_ptr *lattice, int argc, char **argv)
{
  // Variable declarations
  int    **matrix;
  int    i,
         j;
  int    n;
  int    subs;
  int    width,
         height;
  char   filename[1024];
#if POROUS_MEDIA || FREED_POROUS_MEDIA
  FILE   *in;
  char   r, g, b;
  struct bitmap_info_header bmih;
#endif /* POROUS_MEDIA */

  assert(*lattice!=NULL);

  process_init( *lattice, argc, argv);

  if( argc == 2)
  {
    printf("argv = \"%s\"\n", argv[1]);
    strcpy( filename, argv[1]);
    printf("filename = \"%s\"\n", filename);
  }
  else if( argc == 1)
  {
    sprintf(filename, "./in/%s", "params.in");
    printf("filename = \"%s\"\n", filename);
  }
  else
  {
    printf("\n\nusage: ./lb2d [infile]\n\n\n");
    process_exit(1);
  }

  // Read problem parameters
  read_params( *lattice, filename);

  process_compute_local_params( *lattice);

  // Allocate matrix for storing information from bmp file.
  matrix = (int**)malloc( get_LY(*lattice)*sizeof(int*));
  for( j=0; j<get_LY(*lattice); j++)
  {
    matrix[j] = (int*)malloc( get_LX(*lattice)*sizeof(int));
  }

  // Initialize matrix[][].
  for( j=0; j<get_LY(*lattice); j++)
  {
    for( i=0; i<get_LX(*lattice); i++)
    {
      matrix[j][i] = 0;
    }
  }

 for( subs=0; subs<NUM_FLUID_COMPONENTS; subs++)
 {
  // Default periodicity. This will be adjusted in read_bcs()
  // or process_bcs() depending on flow boundaries.
  (*lattice)->periodic_x[subs] = 1;
  (*lattice)->periodic_y[subs] = 1;

  // Get solids.
  sprintf( filename, "./in/%dx%d.bmp", get_g_LX(*lattice), get_g_LY(*lattice));
  spy_bmp( filename, *lattice, matrix);

  // Determine active nodes.
  process_matrix( *lattice, matrix, subs);
  assert( (*lattice)->bc[subs]!=NULL);

#if 0
  // Read boundary conditions from BMP files.
  // Eventually this mechanism will be very general to handle
  // (somewhat?) arbitrary arrangements of boundary conditions.
  read_bcs( *lattice, matrix);
#else
  // Process boundary conditions based on flags read from params.in .
  // This will only support standard inflow and outflow boundaries along
  // entire sides of the lattice.
  process_bcs( *lattice, subs);
#endif

 } /* for( subs=0; subs<NUM_FLUID_COMPONENTS; subs++) */

  // Deallocate memory used for storing the full matrix.
#if VERBOSITY_LEVEL > 0
  printf("latman.c: contruct_lattice() -- Free the matrix.\n");
#endif /* VERBOSITY_LEVEL > 0 */
  for( n=0; n<get_LY(*lattice); n++)
  {
    free( matrix[n]);
  }
  free( matrix);
#if VERBOSITY_LEVEL > 0
  printf("latman.c: contruct_lattice() -- Matrix is free.\n");
#endif /* VERBOSITY_LEVEL > 0 */

 for( subs=0; subs<NUM_FLUID_COMPONENTS; subs++)
 {
  // Allocate NumNodes particle distribution functions.
  (*lattice)->pdf[subs] =
    ( struct pdf_struct*)malloc(
        (*lattice)->NumNodes*sizeof( struct pdf_struct));
  if( (*lattice)->pdf[subs] == NULL)
  {
    printf(
      "construct_lattice() -- ERROR:  "
      "Attempt to allocate %d struct pdf_struct types failed.  "
      "Exiting!\n",
      (*lattice)->NumNodes
      );
    process_exit(1);
  }

  // Allocate NumNodes macroscopic variables.
  (*lattice)->macro_vars[subs] =
    ( struct macro_vars_struct*)malloc(
        (*lattice)->NumNodes*sizeof( struct macro_vars_struct));
  if( (*lattice)->macro_vars[subs]==NULL)
  {
    printf(
      "construct_lattice() -- ERROR:  "
      "Attempt to allocate %d struct macro_vars_struct types failed.  "
      "Exiting!\n",
      (*lattice)->NumNodes
      );
    process_exit(1);
  }

#if NON_LOCAL_FORCES
  // Allocate NumNodes elements for force.
  (*lattice)->force[subs] =
    ( struct force_struct*)malloc(
        (*lattice)->NumNodes*sizeof( struct force_struct));
  if( (*lattice)->force[subs]==NULL)
  {
    printf(
      "construct_lattice() -- ERROR:  "
      "Attempt to allocate %d struct force_struct types failed.  "
      "Exiting!\n",
      (*lattice)->NumNodes
      );
    process_exit(1);
  }
#endif /* NON_LOCAL_FORCES */
 } /* for( subs=0; subs<NUM_FLUID_COMPONENTS; subs++) */

#if STORE_U_COMPOSITE
  // Allocate NumNodes elements for upr.
  (*lattice)->upr =
    ( struct upr_struct*)malloc(
        (*lattice)->NumNodes*sizeof( struct upr_struct));
  if( (*lattice)->upr==NULL)
  {
    printf(
      "construct_lattice() -- ERROR:  "
      "Attempt to allocate %d struct upr_struct types failed.  "
      "Exiting!\n",
      (*lattice)->NumNodes
      );
    process_exit(1);
  }
#endif /* STORE_U_COMPOSITE */

#if TAU_ZHANG_ANISOTROPIC_DISPERSION
  (*lattice)->tau_zhang =
    (double*)malloc( 9*sizeof(double));
#endif

#if POROUS_MEDIA || FREED_POROUS_MEDIA
  switch( (*lattice)->param.ns_flag)
  {
    case 0:
    {
      if( (*lattice)->param.ns > 1.)
      {
        printf(
          "latman.c: construct_lattice() -- "
          "ERROR: ns = %f. "
          "Should have 0 <= ns <=1. "
          "Exiting!\n", (*lattice)->param.ns
          );
        process_exit(1);
      }
      break;
    }

    case 1:
    {
    // Allocate space for ns values.
    (*lattice)->ns =
      (struct ns_struct*)malloc( (*lattice)->NumNodes*sizeof(struct ns_struct));
    if( (*lattice)->ns==NULL)
    {
      printf(
        "construct_lattice() -- ERROR:  "
        "Attempt to allocate %d struct ns_struct types failed.  "
        "Exiting!\n",
        (*lattice)->NumNodes
        );
      process_exit(1);
    }

    // Try to read ns<LX>x<LY>.bmp file.
#if PARALLEL
    sprintf( filename, "./in/ns%dx%d_proc%04d.bmp",
           get_LX(*lattice),
           get_LY(*lattice), get_proc_id(*lattice));
#else
    sprintf( filename, "./in/ns%dx%d.bmp",
           get_LX(*lattice),
           get_LY(*lattice));
#endif
    if( in = fopen( filename, "r+"))
    {
      printf("%s %d >> Reading file \"%s\".\n",__FILE__,__LINE__,filename);
      bmp_read_header( in, &bmih);
      for( n=0; n<(*lattice)->NumNodes; n++)
      {
        bmp_read_entry( in, &bmih, &r, &g, &b);

        // Verify grayscale.
        if(    (double)r != (double)g
            || (double)g != (double)b
            || (double)r != (double)b)
        {
          printf(
            "%s %d >> latman.c: construct_lattice() -- "
            "n=%d:  [ r g b] = [ %3u %3u %3u]\n",__FILE__,__LINE__,
            n, (unsigned int)r%256, (unsigned int)g%256, (unsigned int)b%256);
          printf(
            "%s %d >> latman.c: construct_lattice() -- "
            "ERROR: File %s needs to be grayscale. "
            "Exiting!\n",__FILE__,__LINE__, filename);
          process_exit(1);
        }

        // Assign ns value.
        (*lattice)->ns[n].ns = ((double)((unsigned int)r%256))/255.;
#if 0 && VERBOSITY_LEVEL>0
        printf("%s %d >> n=%d, ns=%f\n",
            __FILE__, __LINE__, n, (*lattice)->ns[n].ns);
#endif /* 1 && VERBOSITY_LEVEL>0 */

      } /* for( n=0; n<(*lattice)->NumNodes; n++) */

      fclose( in);

    } /* if( in = fopen( filename, "r+")) */

    else /* !( in = fopen( filename, "r+")) */
    {
      // Can't read ns.bmp file, so use default values.
      printf("%s %d >> WARNING: Can't read \"%s\". "
          "Using default ns values.\n",__FILE__,__LINE__,filename);
    } /* if( in = fopen( filename, "r+")) else */


      break;
    }

    case 2:
    {
    // Allocate space for ns values.
    (*lattice)->ns =
      (struct ns_struct*)malloc( (*lattice)->NumNodes*sizeof(struct ns_struct));
    if( (*lattice)->ns==NULL)
    {
      printf(
        "construct_lattice() -- ERROR:  "
        "Attempt to allocate %d struct ns_struct types failed.  "
        "Exiting!\n",
        (*lattice)->NumNodes
        );
      process_exit(1);
    }

    // Try to read ns<LX>x<LY>.bmp file.
#if PARALLEL
    sprintf( filename, "./in/ns%dx%d_proc%04d.bmp",
           get_LX(*lattice),
           get_LY(*lattice), get_proc_id(*lattice));
#else
    sprintf( filename, "./in/ns%dx%d.bmp",
           get_LX(*lattice),
           get_LY(*lattice));
#endif
    if( in = fopen( filename, "r+"))
    {
      printf("%s %d >> Reading file \"%s\".\n",__FILE__,__LINE__,filename);
      bmp_read_header( in, &bmih);
      for( n=0; n<(*lattice)->NumNodes; n++)
      {
        bmp_read_entry( in, &bmih, &r, &g, &b);

        // Verify grayscale.
        if(    (double)r != (double)g
            || (double)g != (double)b
            || (double)r != (double)b)
        {
          printf(
            "%s %d >> latman.c: construct_lattice() -- "
            "n=%d:  [ r g b] = [ %3u %3u %3u]\n",__FILE__,__LINE__,
            n, (unsigned int)r%256, (unsigned int)g%256, (unsigned int)b%256);
          printf(
            "%s %d >> latman.c: construct_lattice() -- "
            "ERROR: File %s needs to be grayscale. "
            "Exiting!\n",__FILE__,__LINE__, filename);
          process_exit(1);
        }

        if( ((unsigned int)r%256) != 0 && ((unsigned int)r%256) != 255 )
        {
          printf(
            "%s %d >> latman.c: construct_lattice() -- "
            "ERROR: File %s needs to be black and white. "
            "Exiting!\n",__FILE__,__LINE__, filename);
          process_exit(1);
        }

        // Assign ns value.
        if( ((unsigned int)r%256) == 0)
        {
          (*lattice)->ns[n].ns = (*lattice)->param.ns;
        }
        else
        {
          (*lattice)->ns[n].ns = 0.;
        }
#if 0 && VERBOSITY_LEVEL>0
        printf("%s %d >> n=%d, ns=%f\n",
            __FILE__, __LINE__, n, (*lattice)->ns[n].ns);
#endif /* 1 && VERBOSITY_LEVEL>0 */

      } /* for( n=0; n<(*lattice)->NumNodes; n++) */

      fclose( in);

    } /* if( in = fopen( filename, "r+")) */

    else /* !( in = fopen( filename, "r+")) */
    {
      // Can't read ns.bmp file, so use default values.
      printf("%s %d >> WARNING: Can't read \"%s\". "
          "Using default ns values.\n",__FILE__,__LINE__,filename);
    } /* if( in = fopen( filename, "r+")) else */
      break;
    }

    default:
    {
      printf("%s %d >> construct_lattice() -- Unhandled case: "
        "ns_flag = %d . (Exiting!)\n",
        __FILE__,__LINE__,(*lattice)->param.ns_flag < 0.);
      process_exit(1);
      break;
    }

  } /* switch( (*lattice)->param.ns_flag) */
#endif /* POROUS_MEDIA */

#if INAMURO_SIGMA_COMPONENT && DETERMINE_FLOW_DIRECTION
  //
  // Try to determine the direction of flow.
  //
  // NOTE: This determination informs the breakthrough curve mechanism which
  // should be used in a simple situation with either pressure/velocity
  // boundaries driving the flow in one direction or gravity driving the flow
  // in one direction.  If the direction of flow cannot be determined, FlowDir
  // will be set to indeterminate (=0) and a BTC will not be stored.
  //
  // NOTE: This determination also informs the sigma slip boundary which
  // should only be used in the simple situation of flow through a channel
  // where geometry is trivial and the direction of flow is obvious.
  //

  int north_bcs =
     (*lattice)->param.pressure_n_in[0]
  || (*lattice)->param.pressure_n_out[0]
  || (*lattice)->param.velocity_n_in[0]
  || (*lattice)->param.velocity_n_out[0];

  int south_bcs =
     (*lattice)->param.pressure_s_out[0]
  || (*lattice)->param.pressure_s_in[0]
  || (*lattice)->param.velocity_s_out[0]
  || (*lattice)->param.velocity_s_in[0];

  int east_bcs =
     (*lattice)->param.pressure_e_in[0]
  || (*lattice)->param.pressure_e_out[0]
  || (*lattice)->param.velocity_e_in[0]
  || (*lattice)->param.velocity_e_out[0];

  int west_bcs =
     (*lattice)->param.pressure_w_out[0]
  || (*lattice)->param.pressure_w_in[0]
  || (*lattice)->param.velocity_w_out[0]
  || (*lattice)->param.velocity_w_in[0];

  if( // Pressure/Velocity boundaries moving the flow vertically.
  ( north_bcs || south_bcs ) && !( east_bcs || west_bcs))
  {
    (*lattice)->FlowDir = /*Vertical*/2;
  }
  else if( // Pressure/Velocity boundaries moving the flow horizontally.
  !( north_bcs || south_bcs ) && ( east_bcs || west_bcs))
  {
    (*lattice)->FlowDir = /*Horizontal*/1;
  }
  else
  {
    if( // Gravity driving flow vertically.
        (*lattice)->param.gval[0][1] != 0.
     && (*lattice)->param.gval[0][0] == 0.
      )
    {
      (*lattice)->FlowDir = /*Vertical*/2;
    }
    else if( // Gravity driving flow horizontally.
             (*lattice)->param.gval[0][0] != 0.
          && (*lattice)->param.gval[0][1] == 0.
           )
    {
      (*lattice)->FlowDir = /*Horizontal*/1;
    }
    else // Cannot determine direction of flow.
    {
      (*lattice)->FlowDir = /*Indeterminate*/0;
    }
  }
  // INITIALIZE_WITH_UX_IN or INITIALIZE_WITH_UY_IN can override
  // the flow direction calculations.
#if INITIALIZE_WITH_UX_IN
  (*lattice)->FlowDir = /*Horizontal*/1;
#endif /* INITIALIZE_WITH_UX_IN */
#if INITIALIZE_WITH_UY_IN
  (*lattice)->FlowDir = /*Vertical*/2;
#endif /* INITIALIZE_WITH_UY_IN */
#endif /* INAMURO_SIGMA_COMPONENT && DETERMINE_FLOW_DIRECTION */

#if INAMURO_SIGMA_COMPONENT && STORE_BTC
  // Allocate space for a break through curve if necessary.
  if( (*lattice)->param.sigma_btc_rate > 0 && (*lattice)->FlowDir!=0)
  {
    // Compute "size" of break through curve: number of readings
    // to store.
    (*lattice)->SizeBTC =
      (int)ceil((double)(
                         (
        (*lattice)->NumTimeSteps
      -
        (((*lattice)->param.sigma_start>0)
        ?((*lattice)->param.sigma_start)
        :(0))
                         )
       / (*lattice)->param.sigma_btc_rate))+1;

    //
    // Allocate 4*SizeBTC elements.
    //
    // Readings will come in groups of four (r1,r2,r3,r4):
    //
    //   r0: Timestep.
    //
    //   r1: Concentration at sigma_spot-1
    //
    //   r2: Concentration at sigma_spot-0
    //
    //   r3: Concentration at sigma_spot+1
    //
    //   r4: Velocity in direction of flow.
    //
    // Then
    //
    //   Cf = ( C*v - D*dCdx)/v = ( (r2+r3)/(2*r4) - D*(r3-r2)) / r4
    //
    (*lattice)->param.sigma_btc =
      ( double*)malloc( 5*(*lattice)->SizeBTC*sizeof(double));

  } /* if( sigma_btc_rate > 0) */
#endif /* INAMURO_SIGMA_COMPONENT && STORE_BTC */


	//for reading north boundary pressure from file ./in/pressure_n_in0.in
	//and assigning it to the appropriate variable
  if( (*lattice)->param.pressure_n_in[0] == 2)
  {
    sprintf(filename,"./in/pressure_n_in0.in");
    printf("[%s,%d] construct_lattice() -- Reading %s\n", __FILE__, __LINE__
          , filename);
    FILE *in;
    in = fopen(filename,"r");
    if( !( in = fopen(filename,"r+")))
    {
      printf("%s %d >> WARNING: Can't load \"%s\".\n",
        __FILE__,__LINE__,filename);
      return;
    }
    *(num_pressure_n_in0_ptr(*lattice,0)) = 0;
    double temp;
    fscanf(in,"%lf",&temp);
    while( !feof(in))
    {
      (*(num_pressure_n_in0_ptr(*lattice,0)))++;
      fscanf(in,"%lf",&temp);
    }

    printf("num_pressure_n_in0_ptr = %d\n", num_pressure_n_in0(*lattice,0));

    *pressure_n_in0_ptr(*lattice,0) =
      (double*)malloc( num_pressure_n_in0(*lattice,0)*sizeof(double));

    rewind(in);
    int i;
    for( i=0; i<num_pressure_n_in0(*lattice,0); i++)
    {
      fscanf(in,"%lf", pressure_n_in0(*lattice,0) + i);
    }
    fclose(in);

    //for( i=0; i<num_pressure_n_in0(*lattice,0); i++)
    //{
    //  printf("%f\n",*( pressure_n_in0(*lattice,0) + i));
    //}

  }

	//for reading south boundary pressure from file ./in/pressure_s_in0.in
	//and assigning it to the appropriate variable
  if( (*lattice)->param.pressure_s_in[0] == 2)
  {
    sprintf(filename,"./in/pressure_s_in0.in");
    printf("[%s,%d] construct_lattice() -- Reading %s\n", __FILE__, __LINE__
          , filename);
    FILE *in;
    in = fopen(filename,"r");
    if( !( in = fopen(filename,"r+")))
    {
      printf("%s %d >> WARNING: Can't load \"%s\".\n",
        __FILE__,__LINE__,filename);
      return;
    }
    *(num_pressure_s_in0_ptr(*lattice,0)) = 0;
    double temp;
    fscanf(in,"%lf",&temp);
    while( !feof(in))
    {
      (*(num_pressure_s_in0_ptr(*lattice,0)))++;
      fscanf(in,"%lf",&temp);
    }

    printf("num_pressure_s_in0_ptr = %d\n", num_pressure_s_in0(*lattice,0));

    *pressure_s_in0_ptr(*lattice,0) =
      (double*)malloc( num_pressure_s_in0(*lattice,0)*sizeof(double));

    rewind(in);
    int i;
    for( i=0; i<num_pressure_s_in0(*lattice,0); i++)
    {
      fscanf(in,"%lf", pressure_s_in0(*lattice,0) + i);
    }
    fclose(in);

    //for( i=0; i<num_pressure_n_in0(*lattice,0); i++)
    //{
    //  printf("%f\n",*( pressure_n_in0(*lattice,0) + i));
    //}

  }

  if( do_user_stuff((*lattice)))
  {
    (*lattice)->user_stuff =
      (user_stuff_ptr)malloc( sizeof(struct user_stuff_struct));
  }

  dump_params( *lattice);

} /* void construct_lattice( struct lattice_struct **lattice) */

// void read_PEST_in_files( lattice_ptr *lattice, int argc, char **argv)
//##############################################################################
//
// READ PEST IN FILES
//
//  - Read the files timestep_file.in, x_coord_file.in and y_coord_file.in
//
//  - The function write_PEST_out_data will then save fluid 1 rho values
//    (concentration) to an output file.
//
void read_PEST_in_files( lattice_ptr *lattice, int argc, char **argv)
{
#if PEST_OUTPUT_ON
  //for reading concentration data from files in ./in/
  //for use with PEST

    char   filename[1024];
    FILE *in;
  int i;
  //begin with timesteps
    sprintf(filename,"./in/timestep_file.in");
    printf("[%s,%d] construct_lattice() -- Reading %s\n", __FILE__, __LINE__
          , filename);
  
    in = fopen(filename,"r");
    if( !( in = fopen(filename,"r+")))
    {
      printf("%s %d >> WARNING: Can't load \"%s\".\n",
        __FILE__,__LINE__,filename);
      return;
    }
  

    double temp;
  
  (*lattice)->conc_array_size = 0;
  
    fscanf(in,"%lf",&temp);
    while( !feof(in))
    {
      (*lattice)->conc_array_size++;
      fscanf(in,"%lf",&temp);
    }
  
    printf("Number of PEST points = %d\n", (*lattice)->conc_array_size);
  
  
    (*lattice)->concentration_data =
    ( struct conc_data_struct*)malloc(
        (*lattice)->conc_array_size*sizeof( struct conc_data_struct));

    rewind(in);

    for( i=0; i<(*lattice)->conc_array_size; i++)
    {
      (*lattice)->concentration_data[i].countervar = i;
    fscanf(in,"%d", &((*lattice)->concentration_data[i].timestep));
    }
    fclose(in);

  for( i=1; i<(*lattice)->conc_array_size; i++)
    {
      if((*lattice)->concentration_data[i].timestep < (*lattice)->concentration_data[i-1].timestep)
      {
    printf("[%s,%d] PEST Failure - Concentration data not in time order\n", __FILE__, __LINE__);
        exit (1);
      }
    }

    //now do the space coordinates, starting with x
    sprintf(filename,"./in/x_coord_file.in");
    printf("[%s,%d] construct_lattice() -- Reading %s\n", __FILE__, __LINE__
          , filename);
    
  in = fopen(filename,"r");
    if( !( in = fopen(filename,"r+")))
    {
      printf("%s %d >> WARNING: Can't load \"%s\".\n",
        __FILE__,__LINE__,filename);
      return;
    }

    for( i=0; i<(*lattice)->conc_array_size; i++)
    {
      fscanf(in,"%d", &((*lattice)->concentration_data[i].x_coord));
    }
    fclose(in);

    //now do y
    sprintf(filename,"./in/y_coord_file.in");
    printf("[%s,%d] construct_lattice() -- Reading %s\n", __FILE__, __LINE__
          , filename);
    
  in = fopen(filename,"r");
    if( !( in = fopen(filename,"r+")))
    {
      printf("%s %d >> WARNING: Can't load \"%s\".\n",
        __FILE__,__LINE__,filename);
      return;
    }

    for( i=0; i<(*lattice)->conc_array_size; i++)
    {
      fscanf(in,"%d", &((*lattice)->concentration_data[i].y_coord));
    }
    fclose(in);

//Now we must reduce our arrays so that they only contain concs valid for the local domain

    int tempint;

    for( i=0; i<(*lattice)->conc_array_size; i++)
    {
      if((*lattice)->concentration_data[i].y_coord < get_g_SY(*lattice)
      || (*lattice)->concentration_data[i].y_coord > get_g_EY(*lattice)
      || (*lattice)->concentration_data[i].x_coord < get_g_SX(*lattice)
      || (*lattice)->concentration_data[i].x_coord > get_g_EX(*lattice)) 
      {
        (*lattice)->concentration_data[i].timestep = -1;
      }
      else
      {
           tempint = g2ly(*lattice, (*lattice)->concentration_data[i].y_coord);  
           (*lattice)->concentration_data[i].y_coord = tempint;
      }
    }

  int newcount = 0;
    for( i=0; i<(*lattice)->conc_array_size; i++)
    {
      tempint = (*lattice)->concentration_data[i].timestep;
      if(tempint > -1) 
      {
        (*lattice)->concentration_data[newcount].countervar = (*lattice)->concentration_data[i].countervar;
        (*lattice)->concentration_data[newcount].timestep = (*lattice)->concentration_data[i].timestep;
        (*lattice)->concentration_data[newcount].x_coord = (*lattice)->concentration_data[i].x_coord;
        (*lattice)->concentration_data[newcount].y_coord = (*lattice)->concentration_data[i].y_coord;
  newcount++;
      }
    }

  (*lattice)->conc_array_size = newcount;

        printf("Concentration array size for processor %d is %d. \n", get_proc_id(*lattice), (*lattice)->conc_array_size);

  (*lattice)->array_position = 0;


#endif
}  /* void read_PEST_in_files */


// void write_PEST_out_data( lattice_ptr *lattice, int argc, char **argv)
//##############################################################################
//
// WRITE_PEST_OUT_DATA
//
//  - Write pest data to (*lattice)->concentration_data[0].norm_conc
//
//  - The function write_PEST_out_data will then save fluid 1 rho values
//    (concentration) to an output file.
//
void write_PEST_out_data( lattice_ptr *lattice, int argc, char **argv)
{
#if PEST_OUTPUT_ON //problem with not allocating space for time_array_position?

    while((*lattice)->concentration_data[(*lattice)->array_position].timestep == (*lattice)->time - 1)
    {
    printf("Process %d has found a match between time %d and conc data timestep %d. \n", get_proc_id(*lattice),
                              (*lattice)->time-1, (*lattice)->concentration_data[(*lattice)->array_position].timestep);
    printf("Process %d is recording concentration at %d x %d. \n", get_proc_id(*lattice), 
                         (*lattice)->concentration_data[(*lattice)->array_position].x_coord,
                         (*lattice)->concentration_data[(*lattice)->array_position].y_coord);

      (*lattice)->concentration_data[(*lattice)->array_position].norm_conc = *(&((*lattice)->macro_vars[1][0].rho) 
                    + 3 * (*lattice)->concentration_data[(*lattice)->array_position].y_coord * get_LX(*lattice)
                    + 3 * (*lattice)->concentration_data[(*lattice)->array_position].x_coord);
      (*lattice)->array_position++;
    
    }
#endif
}

// void write_PEST_out_file( lattice_ptr *lattice, int argc, char **argv)
//##############################################################################
//
// WRITE_PEST_OUT_FILE
//
//  - Write pest data to (*lattice)->concentration_data[0].norm_conc
//
//  - The function write_PEST_out_file will then save fluid 1 rho values
//    (concentration) to an output file.
//
void write_PEST_out_file( lattice_ptr *lattice, int argc, char **argv)
{
#if PEST_OUTPUT_ON
    FILE *fp;
      char   filename[1024];
    int aa;
    sprintf( filename, "./out/conc_data_proc%04d.dat", (*lattice)->process.id);
    fp=fopen(filename, "w+");
    if( !( fp = fopen(filename,"w+")))
    {
      printf("%s %d >> WARNING: Can't load \"%s\".\n",
        __FILE__,__LINE__,filename);
      return;
    }
    for( aa = 0; aa < (*lattice)->conc_array_size; aa++)
    {
      fprintf(fp,"%20.17f\n", (*lattice)->concentration_data[aa].norm_conc);
    }
    fclose(fp);
    sprintf( filename, "./out/conc_order_proc%04d.dat", (*lattice)->process.id);
    fp=fopen(filename, "w+");
    if( !( fp = fopen(filename,"w+")))
    {
      printf("%s %d >> WARNING: Can't load \"%s\".\n",
        __FILE__,__LINE__,filename);
      return;
    }
    for( aa = 0; aa < (*lattice)->conc_array_size; aa++)
    {
      fprintf(fp,"%d\n", (*lattice)->concentration_data[aa].countervar);
    }
    fclose(fp);
#endif
}



// void init_problem( struct lattice_struct *lattice)
//##############################################################################
//
// I N I T   P R O B L E M
//
//  - Initialize the problem on the lattice.
//
//    - Set the initial density and velocity.
//
//    - Compute the initial feq.
//
void init_problem( struct lattice_struct *lattice)
{
#if WRITE_CHEN_DAT_FILES
  FILE   *o;
  char   filename[1024];
#endif /* WRITE_CHEN_DAT_FILES */
  int    n, i, j;
  double a, x, u_max, K, drho, m;
  double *macro_var_ptr;
  double *f, *feq, *ftemp;
#if STORE_U_COMPOSITE
  double *upr;
#endif /* STORE_U_COMPOSITE */
#if NON_LOCAL_FORCES
  double *force;
#endif /* NON_LOCAL_FORCES */
  bc_ptr bc;
  int    subs;
  double kappa;
  double ti;
  double y;

  FILE   *ic_in;
  char   ic_filename[1024];
  struct bitmap_info_header bmih;
  char   r, g, b;

  if( lattice->param.initial_condition == IC_WOLF_GLADROW_DIFFUSION)
  {
    kappa = 1.*( lattice->param.tau[1] - .5);
    ti = 15./kappa;
    printf("IC_WOLF_GLADROW_DIFFUSION\n");
    printf("  ti = 15./kappa = 15./%f = %f\n", kappa, ti);
    printf("  tf = 75./kappa = 75./%f = %f\n", kappa, 75./kappa);
    printf("  tf-ti = (75.-15.)/kappa = 60./%f = %f\n", kappa, 60./kappa);
    printf("\n");
  }

#if VERBOSITY_LEVEL > 0
  printf("init_problem() -- Initilizing problem...\n");
#endif /* VERBOSITY_LEVEL > 0 */

#if WRITE_CHEN_DAT_FILES
  //
  // Create empty chen_*.dat files.
  //
  sprintf( filename, "%s", "./out/chen_xyrho.dat");
  if( !( o = fopen( filename,"w+")))
  {
    printf("Error creating \"%s\".  Exiting!\n", filename);
    process_exit(1);
  }
  fclose( o);

  sprintf( filename, "%s", "./out/chen_xy_ux_uy.dat");
  if( !( o = fopen( filename,"w+")))
  {
    printf("Error creating \"%s\".  Exiting!\n", filename);
    process_exit(1);
  }
  fclose( o);

  sprintf( filename, "%s", "./out/chen_time.dat");
  if( !( o = fopen( filename,"w+")))
  {
    printf("Error creating \"%s\".  Exiting!\n", filename);
    process_exit(1);
  }
  fclose( o);

#endif /* WRITE_CHEN_DAT_FILES */

#if 0 // Want to allow mix of velocity and pressure boundaries.
 if( lattice->param.ic_poiseuille)
 {
   if( lattice->param.uy_in != lattice->param.uy_out)
   {
     printf("\n");
     printf("\n");
     printf("%s (%d) -- ERROR: "
       "Need uy_in == uy_out to initialize with poiseuille profile.  "
       "Exiting.\n",
       __FILE__,__LINE__);
     printf("\n");
     printf("\n");
     process_exit(1);

   } /* if( lattice->param.uy_in != lattice->param.uy_out) */

 } /* if( lattice->param.ic_poiseuille) */
#endif

 for( subs=0; subs<NUM_FLUID_COMPONENTS; subs++)
 {

  macro_var_ptr = &( lattice->macro_vars[subs][0].rho);
  bc            = lattice->bc[subs];
#if STORE_U_COMPOSITE
  upr           = lattice->upr[0].u;
#endif /* STORE_U_COMPOSITE */

  if( lattice->param.initial_condition == IC_READ_FROM_FILE)
  {
    // Try to read <LX>x<LY>ic.bmp file.
    sprintf( ic_filename, "./in/%dx%dic.bmp", get_LX(lattice), get_LY(lattice));
    if( ic_in = fopen( ic_filename, "r+"))
    {
      printf("%s %d >> Reading file \"%s\".\n",__FILE__,__LINE__,ic_filename);
      bmp_read_header( ic_in, &bmih);

    } /* if( ic_in = fopen( ic_filename, "r+")) */

    else /* !( ic_in = fopen( ic_filename, "r+")) */
    {
      // Can't read ic file.
      printf("%s %d >> ERROR: Can't read \"%s\". "
          "Exiting!\n",__FILE__,__LINE__,ic_filename);
      process_exit(1);
    } /* if( ic_in = fopen( ic_filename, "r+")) else */

  } /* if( lattice->param.initial_condition == IC_READ_FROM_FILE) */

  for( n=0; n<lattice->NumNodes; n++)
  {
    i = n%get_LX(lattice);
    j = n/get_LX(lattice);

    // Set initial density.
    if( ( 1 || !( bc->bc_type & BC_SOLID_NODE)) )
    {
      switch( lattice->param.initial_condition)
      {
        case IC_UNIFORM_RHO_A:
        {
#if INAMURO_SIGMA_COMPONENT
          if( subs==0)
          {
      if( 1 && lattice->param.ic_poiseuille)
      {
        if( !lattice->periodic_x[subs])
        {
        // Density gradient corresponding to the desired velocity.
        // Based on formula for poiseuille velocity profile
        //
        //   u(x) = K*( a^2 - x^2)
        //
        // where, in terms of the density/pressure gradient,
        //
        //   K = dP/(2 L rho0 nu) ==> drho = 6 L rho0 nu K
        //
        // using the equation of state rho = 3 P.
        //

        // u_max = (3/2)ux_in
        u_max = 1.5*(lattice->param.ux_in);

        // a = .5*(LY-2) .
        a     = .5*(get_LY(lattice)-2);

        //     u(y) = K(a^2-y^2)
        // ==> u_max = Ka^2
        // ==> K = u_max/a^2
        K     = u_max/(a*a);

        drho = 6.*get_LX(lattice)
                 *lattice->param.rho_A[subs]
                 *((1./3.)*(lattice->param.tau[subs]-.5))
                 *K;
        if( lattice->param.incompressible)
        {
          drho = drho/lattice->param.rho_A[subs];
        }
        //drho = .0188121666666667;
        //drho = .62680456666658;
        //drho = 6.2680456666658;
        //printf("%s (%d) -- drho = %f\n",__FILE__,__LINE__,drho);

        m = drho/(get_LX(lattice)-1);

        *macro_var_ptr++ =
          ( lattice->param.rho_A[subs] + drho/2.) - m*i;

#if 0
        ( 1.5*( lattice->param.ux_in)
             /( .25*(get_LY(lattice)-2)*(get_LY(lattice)-2)) )
             *(
                .25*( get_LY(lattice)-2)*( get_LY(lattice)-2)
              -
                (i-.5*( get_LY(lattice)-2)-.5)
               *(i-.5*( get_LY(lattice)-2)-.5)
              )
          ;
#endif
//printf("%s (%d) -- %d %f\n", __FILE__, __LINE__, i, *(macro_var_ptr-1));

        }
        else if( !lattice->periodic_y[subs])
        {
        // Density gradient corresponding to the desired velocity.
        // Based on formula for poiseuille velocity profile
        //
        //   u(x) = K*( a^2 - x^2)
        //
        // where, in terms of the density/pressure gradient,
        //
        //   K = dP/(2 L rho0 nu) ==> drho = 6 L rho0 nu K
        //
        // using the equation of state rho = 3 P.
        //

        // u_max = (3/2)uy_in
        u_max = 1.5*(lattice->param.uy_in);

        // a = .5*(LX-2) .
        a     = .5*(get_LX(lattice)-2);

        //     u(x) = K(a^2-x^2)
        // ==> u_max = Ka^2
        // ==> K = u_max/a^2
        K     = u_max/(a*a);

        drho = 6.*get_LY(lattice)
                 *lattice->param.rho_A[subs]
                 *((1./3.)*(lattice->param.tau[subs]-.5))
                 *K;
        if( lattice->param.incompressible)
        {
          drho = drho/lattice->param.rho_A[subs];
        }
        //drho = .0188121666666667;
        //drho = .62680456666658;
        //drho = 6.2680456666658;
        //printf("%s (%d) -- drho = %f\n",__FILE__,__LINE__,drho);

        m = drho/(get_LY(lattice)-1);

        *macro_var_ptr++ =
          ( lattice->param.rho_A[subs] + drho/2.) - m*j;

#if 0
        ( 1.5*( lattice->param.uy_in)
             /( .25*(get_LX(lattice)-2)*(get_LX(lattice)-2)) )
             *(
                .25*( get_LX(lattice)-2)*( get_LX(lattice)-2)
              -
                (i-.5*( get_LX(lattice)-2)-.5)
               *(i-.5*( get_LX(lattice)-2)-.5)
              )
          ;
#endif
//printf("%s (%d) -- %d %f\n", __FILE__, __LINE__, i, *(macro_var_ptr-1));


        }
        else
        {
        }

      } /* if( lattice->param.ic_poiseuille) */

      else // !lattice->param.ic_poiseuille
      {
        if( hydrostatic( lattice))
        {
          //*macro_var_ptr++ = ( 1.00216 - (j-1)*.00054);
          if( hydrostatic_compressible( lattice))
          {
            if( hydrostatic_compute_rho_ref(lattice))
            {
              // Reference density computed in terms of average density
              lattice->param.rho_out =
#if 0
                ( 3.*lattice->param.gval[0][1]
#if INAMURO_SIGMA_COMPONENT
                  *( 1. + (get_buoyancy(lattice))
                    *(get_beta(lattice))
                    *(get_C(lattice)-get_C0(lattice)) )
#endif
                  *(get_LY(lattice)-2)
                  *lattice->param.rho_A[0])
                /
                ( 1. - exp( -3.*lattice->param.gval[0][1]
#if INAMURO_SIGMA_COMPONENT
                            *( 1. + (get_buoyancy(lattice))
                              *(get_beta(lattice))
                              *(get_C_out(lattice)-get_C0(lattice)) )
#endif
                            *(get_LY(lattice)-2)));

#else
                ( 3.*lattice->param.gval[0][1]
#if INAMURO_SIGMA_COMPONENT
                  *( 1. + (get_buoyancy(lattice))
                    *(get_beta(lattice))
                    *(get_C(lattice)-get_C0(lattice)) )
#endif
                  *(get_LY(lattice)-2)
                  *lattice->param.rho_A[0])
                /
                ( 1. - exp( -3.*lattice->param.gval[0][1]
#if INAMURO_SIGMA_COMPONENT
                            *( 1. + (get_buoyancy(lattice))
                              *(get_beta(lattice))
                              *(get_C_out(lattice)-get_C0(lattice)) )
#endif
                            *(get_LY(lattice)-2)));
#endif
              //printf("rho_ref = %20.17f\n", lattice->param.rho_out);
            }
            *macro_var_ptr++ =
#if 0
              lattice->param.rho_out
              *exp( -3.*lattice->param.gval[0][1]
#if INAMURO_SIGMA_COMPONENT
                  //*(1.+(get_buoyancy(lattice))*lattice->param.rho_sigma)
                  *(1. + (get_buoyancy(lattice))
                    *get_beta(lattice)
                    *( get_C(lattice)
                      - get_C0(lattice)))
#endif
                  //*(0.5*(get_LY(lattice)-1.)-1.))
                  *( (get_LY(lattice)-1.)-0.))
                  *exp(  3.*lattice->param.gval[0][1]
#if INAMURO_SIGMA_COMPONENT
                      //*(1.+(get_buoyancy(lattice))*lattice->param.rho_sigma)
                      *(1. + (get_buoyancy(lattice))
                        *get_beta(lattice)
                        *( get_C(lattice)
                          - get_C0(lattice)))
#endif
                      *(j+1.0));
#else
              3.*lattice->param.gval[0][1]
#if INAMURO_SIGMA_COMPONENT
                  //*(1.+(get_buoyancy(lattice))*lattice->param.rho_sigma)
                  *(1. + (get_buoyancy(lattice))
                    *get_beta(lattice)
                    *( get_C(lattice)
                      - get_C0(lattice)))
#endif
                *(get_LY(lattice)-2)
                *lattice->param.rho_A[0]
                *exp( -3.*lattice->param.gval[0][1]
                         *( ( get_LY(lattice)-2.) - (j-.5)) )
              /
              ( 1. - exp( -3.*lattice->param.gval[0][1]
#if INAMURO_SIGMA_COMPONENT
                      //*(1.+(get_buoyancy(lattice))*lattice->param.rho_sigma)
                      *(1. + (get_buoyancy(lattice))
                        *get_beta(lattice)
                        *( get_C(lattice)
                          - get_C0(lattice)))
#endif
                          *(get_LY(lattice)-2)));
#endif
          }
          else
          {
            *macro_var_ptr++ =
              lattice->param.rho_out
              * ( 1. - 3.*lattice->param.gval[0][1]
#if INAMURO_SIGMA_COMPONENT
                  //*(1.+(get_buoyancy(lattice))*lattice->param.rho_sigma)
                  *(1. + (get_buoyancy(lattice))
                    *get_beta(lattice)
                    *( get_C(lattice)
                      - get_C0(lattice)))
#endif
                  *( ( get_LY(lattice)
                      + ((get_LY(lattice)%2)?(-1.):(1.)))/2.
                    - j ) );
          }
        }
        else
        {
          *macro_var_ptr++ = lattice->param.rho_A[subs];
        }
      } /* if( lattice->param.ic_poiseuille) else */

          }
          else // subs==1
          {
            *macro_var_ptr++ = lattice->param.rho_sigma;
          }
#else /* !( INAMURO_SIGMA_COMPONENT) */

        if( hydrostatic( lattice))
        {
          if( hydrostatic_compressible( lattice))
          {
            if( hydrostatic_compute_rho_ref(lattice))
            {
              // Reference density computed in terms of average density
              lattice->param.rho_out =
                ( 3.*lattice->param.gval[0][1]
                  *(get_LY(lattice)-2)
                  *lattice->param.rho_A[0])
                /
                ( 1. - exp( -3.*lattice->param.gval[0][1]
                            *(get_LY(lattice)-2)));
            }
            *macro_var_ptr++ =
#if 0
              lattice->param.rho_out
              *exp( -3.*lattice->param.gval[0][1]
                  *( (get_LY(lattice)-1.)-0.))
                  *exp(  3.*lattice->param.gval[0][1]
                      *(j+1.0));
#else
              3.*lattice->param.gval[0][1]
                *(get_LY(lattice)-2)
                *lattice->param.rho_A[0]
                *exp( -3.*lattice->param.gval[0][1]
                         *( ( get_LY(lattice)-2.) - (j-.5)) )
              /
              ( 1. - exp( -3.*lattice->param.gval[0][1]
                          *(get_LY(lattice)-2)));
#endif
          }
          else
          {
            *macro_var_ptr++ =
              lattice->param.rho_out
              * ( 1. - 3.*lattice->param.gval[0][1]
                  *( ( get_LY(lattice)
                      + ((get_LY(lattice)%2)?(-1.):(1.)))/2.
                    - j ) );
          }
        }
        else
        {
          *macro_var_ptr++ = lattice->param.rho_A[subs];
        }

#endif /* INAMURO_SIGMA_COMPONENT */
          break;
        }
        case IC_UNIFORM_RHO_B:
        {
#if INAMURO_SIGMA_COMPONENT
          if( subs==0)
          {
      if( 1 && lattice->param.ic_poiseuille)
      {
        if( !lattice->periodic_x[subs])
        {
        // Density gradient corresponding to the desired velocity.
        // Based on formula for poiseuille velocity profile
        //
        //   u(x) = K*( a^2 - x^2)
        //
        // where, in terms of the density/pressure gradient,
        //
        //   K = dP/(2 L rho0 nu) ==> drho = 6 L rho0 nu K
        //
        // using the equation of state rho = 3 P.
        //

        // u_max = (3/2)ux_in
        u_max = 1.5*(lattice->param.ux_in);

        // a = .5*(LY-2) .
        a     = .5*(get_LY(lattice)-2);

        //     u(y) = K(a^2-y^2)
        // ==> u_max = Ka^2
        // ==> K = u_max/a^2
        K     = u_max/(a*a);

        drho = 6.*get_LX(lattice)
                 *lattice->param.rho_B[subs]
                 *((1./3.)*(lattice->param.tau[subs]-.5))
                 *K;
        if( lattice->param.incompressible)
        {
          drho = drho/lattice->param.rho_B[subs];
        }
        //drho = .0188121666666667;
        //drho = .62680456666658;
        //drho = 6.2680456666658;
        //printf("%s (%d) -- drho = %f\n",__FILE__,__LINE__,drho);

        m = drho/(get_LX(lattice)-1);

        *macro_var_ptr++ =
          ( lattice->param.rho_B[subs] + drho/2.) - m*i;

#if 0
        ( 1.5*( lattice->param.ux_in)
             /( .25*(get_LY(lattice)-2)*(get_LY(lattice)-2)) )
             *(
                .25*( get_LY(lattice)-2)*( get_LY(lattice)-2)
              -
                (i-.5*( get_LY(lattice)-2)-.5)
               *(i-.5*( get_LY(lattice)-2)-.5)
              )
          ;
#endif
//printf("%s (%d) -- %d %f\n", __FILE__, __LINE__, i, *(macro_var_ptr-1));

        }
        else if( !lattice->periodic_y[subs])
        {
        // Density gradient corresponding to the desired velocity.
        // Based on formula for poiseuille velocity profile
        //
        //   u(x) = K*( a^2 - x^2)
        //
        // where, in terms of the density/pressure gradient,
        //
        //   K = dP/(2 L rho0 nu) ==> drho = 6 L rho0 nu K
        //
        // using the equation of state rho = 3 P.
        //

        // u_max = (3/2)uy_in
        u_max = 1.5*(lattice->param.uy_in);

        // a = .5*(LX-2) .
        a     = .5*(get_LX(lattice)-2);

        //     u(x) = K(a^2-x^2)
        // ==> u_max = Ka^2
        // ==> K = u_max/a^2
        K     = u_max/(a*a);

        drho = 6.*get_LY(lattice)
                 *lattice->param.rho_B[subs]
                 *((1./3.)*(lattice->param.tau[subs]-.5))
                 *K;

        if( lattice->param.incompressible)
        {
          drho = drho/lattice->param.rho_B[subs];
        }
        //drho = .0188121666666667;
        //drho = .62680456666658;
        //drho = 6.2680456666658;
        //printf("%s (%d) -- drho = %f\n",__FILE__,__LINE__,drho);

        m = drho/(get_LY(lattice)-1);

        *macro_var_ptr++ =
          ( lattice->param.rho_B[subs] + drho/2.) - m*j;

        if(    lattice->param.pressure_n_out[0] == 1
            && lattice->param.velocity_s_in[0]  == 1)
        {
          lattice->param.rho_out =
              ( lattice->param.rho_B[subs] + drho/2.)
          - m*( get_LY(lattice)-1);
        }

#if 0
        ( 1.5*( lattice->param.uy_in)
             /( .25*(get_LX(lattice)-2)*(get_LX(lattice)-2)) )
             *(
                .25*( get_LX(lattice)-2)*( get_LX(lattice)-2)
              -
                (i-.5*( get_LX(lattice)-2)-.5)
               *(i-.5*( get_LX(lattice)-2)-.5)
              )
          ;
#endif
//printf("%s (%d) -- %d %f\n", __FILE__, __LINE__, i, *(macro_var_ptr-1));


        }
        else
        {
        }

      } /* if( lattice->param.ic_poiseuille) */

      else // !lattice->param.ic_poiseuille
      {
#if 0
        *macro_var_ptr++ = lattice->param.rho_B[subs];
#else
       if( 0)//i<get_LX(lattice)-1)
       {
        *macro_var_ptr++ =
        (
          lattice->param.rho_B[subs]
			  * ( 1.
          - 3.
           *((get_LY(lattice)-2. + 1.*((get_LY(lattice)%2)?(-1.):(1.)))/2.+1.-j)
           *lattice->param.gval[0][1]
          )
        );
       }
       else
       {
        *macro_var_ptr++ =
        (
          lattice->param.rho_B[subs]
			  * ( 1.
          - 3.
           *((get_LY(lattice)-2. + 1.*((get_LY(lattice)%2)?(-1.):(1.)) )/2.+1.-j)
           *(1.+lattice->param.rho_sigma_out)
           *lattice->param.gval[0][1]
          )
        );
       }
#endif

      } /* if( lattice->param.ic_poiseuille) else */

          }
          else // subs==1
          {
            //*macro_var_ptr++ = 0.;
            if( 0)//i==get_LX(lattice)-1)//j>5 && j<get_LY(lattice)-1 && i!=5)
            {
              *macro_var_ptr++ = lattice->param.rho_sigma_out;
            }
            else
            {
              *macro_var_ptr++ = 0.;
            }
          }
#else /* !( INAMURO_SIGMA_COMPONENT) */
          if( 0)//hydrostatic( lattice))
          {
            *macro_var_ptr++ =
            (
              lattice->param.rho_B[subs]
			      * ( 1.
              - 3.
               *((get_LY(lattice)-2. + 1.*((get_LY(lattice)%2)?(-1.):(1.)) )/2.+1.-j)
               *lattice->param.gval[0][1]
              )
            );
          }
          else
          {
            *macro_var_ptr++ = lattice->param.rho_B[subs];
          }
#endif /* INAMURO_SIGMA_COMPONENT */
          break;
        }
        case IC_UNIFORM_RHO_IN:
        {
#if INAMURO_SIGMA_COMPONENT
          if( subs==0)
          {
            *macro_var_ptr++ = lattice->param.rho_in;
          }
          else // subs==1
          {
            *macro_var_ptr++ = lattice->param.rho_in;
          }
#else /* !( INAMURO_SIGMA_COMPONENT) */
          *macro_var_ptr++ = lattice->param.rho_in;
#endif /* INAMURO_SIGMA_COMPONENT */
          break;
        }
        case IC_BUBBLE:
        {
#if INAMURO_SIGMA_COMPONENT
          if( subs==0)
          {
            *macro_var_ptr++ = lattice->param.rho_A[subs];
          }
          else
          {
            if( (i-lattice->param.x0)*(i-lattice->param.x0)
              + (get_g_SY(lattice) + j-lattice->param.y0)*(get_g_SY(lattice) + j-lattice->param.y0)
              < lattice->param.r0*lattice->param.r0)
            {
              *macro_var_ptr++ = lattice->param.rho_sigma;
            }
            else
            {
              *macro_var_ptr++ = 0.;
            }
          }
#else /* !( INAMURO_SIGMA_COMPONENT) */
          if( (i-lattice->param.x0)*(i-lattice->param.x0)
            + (get_g_SY(lattice) + j-lattice->param.y0)*(get_g_SY(lattice) + j-lattice->param.y0)
            < lattice->param.r0*lattice->param.r0)
          {
            *macro_var_ptr++ = lattice->param.rho_A[subs];
          }
          else
          {
            *macro_var_ptr++ = lattice->param.rho_B[subs];
          }
#endif /* INAMURO_SIGMA_COMPONENT */
          break;
        }
        case IC_YIN_YANG:
        {
          if( i < get_LX(lattice)/2)
          {
            if( (i-lattice->param.x0/2.)*(i-lattice->param.x0/2.)
              + (j-lattice->param.y0)*(j-lattice->param.y0)
              < lattice->param.r0*lattice->param.r0/4.)
            {
              *macro_var_ptr++ = lattice->param.rho_A[subs];
            }
            else
            {
              *macro_var_ptr++ = lattice->param.rho_B[subs];
            }
          }
          else
          {
            if( (i-3.*lattice->param.x0/2.)*(i-3.*lattice->param.x0/2.)
              + (j-lattice->param.y0)*(j-lattice->param.y0)
              < lattice->param.r0*lattice->param.r0/4.)
            {
              *macro_var_ptr++ = lattice->param.rho_B[subs];
            }
            else
            {
              *macro_var_ptr++ = lattice->param.rho_A[subs];
            }
          }

          break;
        }
        case IC_DIAGONAL:
        {
          if( i+j < get_LX(lattice))
          {
            *macro_var_ptr++ = lattice->param.rho_A[subs];
          }
          else
          {
            *macro_var_ptr++ = lattice->param.rho_B[subs];
          }
          break;
        }
        case IC_2X2_CHECKERS:
        {
          if( ( ( i < get_LX(lattice)/2) && ( j < get_LX(lattice)/2))
           || ( ( i >= get_LX(lattice)/2) && ( j >= get_LX(lattice)/2)) )
          {
            *macro_var_ptr++ = lattice->param.rho_A[subs];
          }
          else
          {
            *macro_var_ptr++ = lattice->param.rho_B[subs];
          }
          break;
        }
        case IC_STATIC:
        {
          if( NUM_FLUID_COMPONENTS==2)
          {
            if( ((double)rand()/(double)RAND_MAX) < lattice->param.cut)
            {
              *macro_var_ptr++ = lattice->param.rho_A[subs];
            }
            else
            {
              *macro_var_ptr++ = lattice->param.rho_B[subs];
            }
          }
          else if( NUM_FLUID_COMPONENTS==1)
          {
            *macro_var_ptr++ = lattice->param.rho_in
                             + ((double)rand()/(double)RAND_MAX);
          }
          else
          {
            printf(
              "%s %d >> "
              "init_lattice() -- "
              "Unhandled case NUM_FLUID_COMPONENTS = %d .  "
              "Exiting!\n",__FILE__,__LINE__, NUM_FLUID_COMPONENTS);
            process_exit(1);
          }
          break;
        }
        case IC_RECTANGLE:
        {
#if INAMURO_SIGMA_COMPONENT
#if ZHANG_AND_CHEN_ENERGY_TRANSPORT
          if( subs==0)
          {
            if( ( i >= lattice->param.x1) && ( get_g_SY(lattice) + j >= lattice->param.y1)
             && ( i <= lattice->param.x2) && ( get_g_SY(lattice) + j <= lattice->param.y2))
            {
              *macro_var_ptr++ = lattice->param.rho_A[subs];
            }
            else
            {
              *macro_var_ptr++ = lattice->param.rho_B[subs];
            }
          }
          else // subs==1
          {
            if( ( i >= lattice->param.x1) && ( get_g_SY(lattice) + j >= lattice->param.y1)
             && ( i <= lattice->param.x2) && ( get_g_SY(lattice) + j <= lattice->param.y2))
            {
              *macro_var_ptr++ = lattice->param.rho_sigma;
            }
            else
            {
              *macro_var_ptr++ = lattice->param.rho_sigma;
            }
          }
#else /* !( ZHANG_AND_CHEN_ENERGY_TRANSPORT) */
          if( subs==0)
          {
            *macro_var_ptr++ = lattice->param.rho_A[subs];
          }
          else
          {
            if( ( i >= g2lx( lattice, lattice->param.x1))
             && ( j >= g2ly( lattice, lattice->param.y1))
             && ( i <= g2lx( lattice, lattice->param.x2))
             && ( j <= g2ly( lattice, lattice->param.y2)) )
            {
              *macro_var_ptr++ = lattice->param.rho_sigma;
            }
            else
            {
              //*macro_var_ptr++ = 0.;
              *macro_var_ptr++ = lattice->param.rho_A[subs];
            }
          }
#endif /* ZHANG_AND_CHEN_ENERGY_TRANSPORT */
#else /* !( INAMURO_SIGMA_COMPONENT) */
            if( ( i >= lattice->param.x1) && ( get_g_SY(lattice) + j >= lattice->param.y1)
             && ( i <= lattice->param.x2) && ( get_g_SY(lattice) + j <= lattice->param.y2))
          {
            *macro_var_ptr++ = lattice->param.rho_A[subs];
          }
          else
          {
            *macro_var_ptr++ = lattice->param.rho_B[subs];
          }
#endif /* INAMURO_SIGMA_COMPONENT */
          break;
        }
        case IC_DOT:
        {
          if( i == lattice->param.x0 && get_g_SY(lattice) + j == lattice->param.y0)
          {
            *macro_var_ptr++ = lattice->param.rho_A[subs];
          }
          else
          {
            *macro_var_ptr++ = lattice->param.rho_B[subs];
          }
          break;
        }
        case IC_WOLF_GLADROW_DIFFUSION:
        {
#if INAMURO_SIGMA_COMPONENT
          if( subs==0)
          {
            *macro_var_ptr++ = lattice->param.rho_A[subs];
          }
          else // subs==1
          {
            y = (j - ( get_LY(lattice)-1)/2.)/((get_LY(lattice)-1)/200.);
            *macro_var_ptr++ = (1./(2.*sqrt(PI*15.)))*exp(-y*y/(4.*15.));
          }
#else /* !( INAMURO_SIGMA_COMPONENT) */
          printf("%s %d >> Unhandled case. Exiting!\n", __FILE__, __LINE__);
          process_exit(1);
#endif /* INAMURO_SIGMA_COMPONENT */
          break;
        }
        case IC_HYDROSTATIC:
        {
#if INAMURO_SIGMA_COMPONENT
          if( subs==0)
          {
            *macro_var_ptr++ =
              lattice->param.rho_A[subs]
            -
            j*( lattice->param.rho_A[subs]
              - lattice->param.rho_B[subs] )
              / get_LY(lattice);
          }
          else // subs==1
          {
            *macro_var_ptr++ = 0.;
          }
#else /* !( INAMURO_SIGMA_COMPONENT) */
          printf("%s %d >> Unhandled case. Exiting!\n", __FILE__, __LINE__);
          process_exit(1);
#endif /* INAMURO_SIGMA_COMPONENT */
          break;
        }
        case IC_READ_FROM_FILE:
        {
          bmp_read_entry( ic_in, &bmih, &r, &g, &b);
printf("%s %d >> %d: rgb = (%d,%d,%d)\n",__FILE__,__LINE__, subs,
    (unsigned int)r%256,
    (unsigned int)g%256,
    (unsigned int)b%256 );

          if( NUM_FLUID_COMPONENTS==1)
          {
            // Verify grayscale.
            if(    (double)r != (double)g
                || (double)g != (double)b
                || (double)r != (double)b)
            {
              //printf(
              //  "%s %d >> latman.c: construct_lattice() -- "
              //  "n=%d:  [ r g b] = [ %3u %3u %3u]\n",__FILE__,__LINE__,
              //  n, (unsigned int)r%256,
              //  (unsigned int)g%256, (unsigned int)b%256);
            //printf(
            //  "%s %d >> latman.c: construct_lattice() -- "
            //  "WARNING: File %s is not grayscale. "
            //  "Using the blue channel as the initial condition.\n",
            //  __FILE__,__LINE__, ic_filename);
            }

            *macro_var_ptr++ =
              get_rho_A(lattice,subs)*((double)((unsigned int)b%256))/255.;

          }
          else if( NUM_FLUID_COMPONENTS==2)
          {
#if INAMURO_SIGMA_COMPONENT
            if(subs==0)
            {
              *macro_var_ptr++ = get_rho_A(lattice,subs);
            }
            else
            {
              // Verify grayscale.
              if(    (double)r != (double)g
                  || (double)g != (double)b
                  || (double)r != (double)b)
              {
                //printf(
                //  "%s %d >> latman.c: construct_lattice() -- "
                //  "n=%d:  [ r g b] = [ %3u %3u %3u]\n",__FILE__,__LINE__,
                //  n, (unsigned int)r%256,
                //  (unsigned int)g%256, (unsigned int)b%256);
              //printf(
              //  "%s %d >> latman.c: construct_lattice() -- "
              //  "WARNING: File %s is not grayscale. "
              //  "Using the red channel as the initial condition.\n",
              //  __FILE__,__LINE__, ic_filename);
              }

              *macro_var_ptr++ =
                get_rho_sigma(lattice)*(1.-(((double)((unsigned int)b%256))/255.));
            }


#else /* !( INAMURO_SIGMA_COMPONENT) */
            // Use blue channel for subs 0 and red channel for subs 1.
            if(subs==0)
            {
              *macro_var_ptr++ =
                get_rho_A(lattice,subs)*((double)((unsigned int)b%256))/255.;
//printf("%s %d >> subs 0, b=%d, rho = %f\n",__FILE__,__LINE__, (unsigned int)b%256, *(macro_var_ptr-1));
            }
            else if( subs==1)
            {
              *macro_var_ptr++ =
                get_rho_B(lattice,subs)*((double)((unsigned int)r%256))/255.;
//printf("%s %d >> subs 1, r=%d, rho = %f\n",__FILE__,__LINE__, (unsigned int)r%256, *(macro_var_ptr-1));
            }
            else
            {
              printf("%s %d >> ERROR: Unhandled case! Exiting!\n",
                __FILE__,__LINE__);
              process_exit(1);
            }
#endif /* INAMURO_SIGMA_COMPONENT */
          }
          else
          {
            printf("%s %d >> ERROR: Unhandled case! Exiting!\n",
              __FILE__,__LINE__);
            process_exit(1);
          }

          break;
        }
        default:
        {
          printf(
            "%s %d >> init_problem() -- Unhandled case  "
            "lattice->param.initial_condition = %d.  "
            "Exiting!\n", __FILE__, __LINE__,
            lattice->param.initial_condition );
          process_exit(1);
          break;
        }
      } /* switch( lattice->param.initial_condition) */
    } /* if( ( 1 || !( bc->bc_type & BC_SOLID_NODE)) ) */
    else
    {
    //if( bc->bc_type & BC_SOLID_NODE)
    //{
    //  //*macro_var_ptr++ = lattice->param.rho_A[subs];
    //  *macro_var_ptr++ = lattice->param.rho_in;
    //}
    //else
    //{
        *macro_var_ptr++ = 0.;
    //}
    } /* if( ( 1 || !( bc->bc_type & BC_SOLID_NODE)) ) else */

    // Set initial velocty.
    if( ( 0 || !( bc->bc_type & BC_SOLID_NODE)) )
    {
#if 1
      // u_x
      if(    lattice->param.ic_poiseuille
          && !lattice->periodic_x[subs])
      {
        // Poiseuille flow profile in the x- direction.  Assuming
        // one-lattice-unit walls on both sides.

        // a = .5*(LY-2) .
        a     = .5*(get_LY(lattice)-2);

        // u_max = (3/2)ux_in
        u_max = 1.5*(lattice->param.ux_in);

        // u(x) = K(a^2-x^2) ==> u_max = Ka^2 ==> K = u_max/a^2
        K     = u_max/(a*a);

        // u(x) = K(a^2-x^2)
        x = (j-.5) - a;
        *macro_var_ptr++ = K * ( a*a - x*x);

#if 0
        ( 1.5*( lattice->param.ux_in)
             /( .25*(get_LY(lattice)-2)*(get_LY(lattice)-2)) )
             *(
                .25*( get_LY(lattice)-2)*( get_LY(lattice)-2)
              -
                (j-.5*( get_LY(lattice)-2)-.5)
               *(j-.5*( get_LY(lattice)-2)-.5)
              )
          ;
#endif
//printf("%s (%d) -- %d %f\n", __FILE__, __LINE__, j, *(macro_var_ptr-1));

      }
      else
      {
#if INITIALIZE_WITH_UX_IN
        *macro_var_ptr++ = lattice->param.ux_in;
#else /* !( INITIALIZE_WITH_UX_IN) */
        *macro_var_ptr++ = 0.;
#endif /* INITIALIZE_WITH_UX_IN */
      }

      // u_y
      if(    lattice->param.ic_poiseuille
          && !lattice->periodic_y[subs])
      {
        // Poiseuille flow profile in the vertical/y- direction.  Assuming
        // one-lattice-unit walls on both sides.

        // a = .5*(LX-2) .
        a     = .5*(get_LX(lattice)-2);

        // u_max = (3/2)uy_in
        u_max = 1.5*(lattice->param.uy_in);

        // u(x) = K(a^2-x^2) ==> u_max = Ka^2 ==> K = u_max/a^2
        K     = u_max/(a*a);

        // u(x) = K(a^2-x^2)
        x = (i-.5) - a;
        *macro_var_ptr++ = K * ( a*a - x*x);

#if 0
        ( 1.5*( lattice->param.uy_in)
             /( .25*(get_LX(lattice)-2)*(get_LX(lattice)-2)) )
             *(
                .25*( get_LX(lattice)-2)*( get_LX(lattice)-2)
              -
                (i-.5*( get_LX(lattice)-2)-.5)
               *(i-.5*( get_LX(lattice)-2)-.5)
              )
          ;
#endif
//printf("%s (%d) -- %d %f\n", __FILE__, __LINE__, i, *(macro_var_ptr-1));

      } /* if( lattice->param.ic_poiseuille) */

      else // !lattice->param.ic_poiseuille
      {
#if INITIALIZE_WITH_UY_IN
        *macro_var_ptr++ = lattice->param.uy_in;
#else /* !( INITIALIZE_WITH_UY_IN) */
        *macro_var_ptr++ = 0.;
#endif /* INITIALIZE_WITH_UY_IN */

      } /* if( lattice->param.ic_poiseuille) else */

#else
      *macro_var_ptr++ = ( lattice->param.ux_in + lattice->param.ux_out)/2.;
      *macro_var_ptr++ = ( lattice->param.uy_in + lattice->param.uy_out)/2.;
#endif
    }
    else
    {
      *macro_var_ptr++ = 0.;
      *macro_var_ptr++ = 0.;
    }

    bc++;

#if STORE_U_COMPOSITE
    *upr++ = 0.;
    *upr++ = 0.;
#endif /* STORE_U_COMPOSITE */

  } /* for( n=0; n<lattice->NumNodes; n++) */

  if( lattice->param.initial_condition == IC_READ_FROM_FILE)
  {
    fclose( ic_in);
  }

 } /* for( subs=0; subs<NUM_FLUID_COMPONENTS; subs++) */

  // Compute initial feq.
  compute_feq( lattice, 0);

 for( subs=0; subs<NUM_FLUID_COMPONENTS; subs++)
 {
  for( n=0; n<lattice->NumNodes; n++)
  {
    f = &(lattice->pdf[subs][n].f[0]);
    feq = &(lattice->pdf[subs][n].feq[0]);
    ftemp = &(lattice->pdf[subs][n].ftemp[0]);
    if( 1 || !( bc[n].bc_type & BC_SOLID_NODE))
    {
#if 1
      // Copy feq to f.
      f[0]= feq[0];
      f[1]= feq[1];
      f[2]= feq[2];
      f[3]= feq[3];
      f[4]= feq[4];
      f[5]= feq[5];
      f[6]= feq[6];
      f[7]= feq[7];
      f[8]= feq[8];

      // Initialize ftemp.
      ftemp[0]= 0.; //*(f-9);
      ftemp[1]= 0.; //*(f-9);
      ftemp[2]= 0.; //*(f-9);
      ftemp[3]= 0.; //*(f-9);
      ftemp[4]= 0.; //*(f-9);
      ftemp[5]= 0.; //*(f-9);
      ftemp[6]= 0.; //*(f-9);
      ftemp[7]= 0.; //*(f-9);
      ftemp[8]= 0.; //*(f-9);
#else
      // Debug info.  To track during the streaming step.

      // f
      f[0] = (double)n + 0./10.;// *(f-9);
      f[1] = (double)n + 1./10.;// *(f-9);
      f[2] = (double)n + 2./10.;// *(f-9);
      f[3] = (double)n + 3./10.;// *(f-9);
      f[4] = (double)n + 4./10.;// *(f-9);
      f[5] = (double)n + 5./10.;// *(f-9);
      f[6] = (double)n + 6./10.;// *(f-9);
      f[7] = (double)n + 7./10.;// *(f-9);
      f[8] = (double)n + 8./10.;// *(f-9);

      // ftemp
      ftemp[0] = (double)n;// *(f-9);
      ftemp[1] = (double)n;// *(f-9);
      ftemp[2] = (double)n;// *(f-9);
      ftemp[3] = (double)n;// *(f-9);
      ftemp[4] = (double)n;// *(f-9);
      ftemp[5] = (double)n;// *(f-9);
      ftemp[6] = (double)n;// *(f-9);
      ftemp[7] = (double)n;// *(f-9);
      ftemp[8] = (double)n;// *(f-9);

#endif
    }
    else
    {
      // f = 0.
      f[0] = 0.;
      f[1] = 0.;
      f[2] = 0.;
      f[3] = 0.;
      f[4] = 0.;
      f[5] = 0.;
      f[6] = 0.;
      f[7] = 0.;
      f[8] = 0.;

      // ftemp = 0.
      ftemp[0] = 0.;
      ftemp[1] = 0.;
      ftemp[2] = 0.;
      ftemp[3] = 0.;
      ftemp[4] = 0.;
      ftemp[5] = 0.;
      ftemp[6] = 0.;
      ftemp[7] = 0.;
      ftemp[8] = 0.;

    }

  } /* for( n=0; n<lattice->NumNodes; n++) */

#if NON_LOCAL_FORCES
  force = lattice->force[subs][0].force;
  for( n=0; n<lattice->NumNodes; n++)
  {
    *force++ = 0.;
    *force++ = 0.;
    *force++ = 0.;
    *force++ = 0.;
  }
#endif /* NON_LOCAL_FORCES */

 } /* for( subs=0; subs<NUM_FLUID_COMPONENTS; subs++) */

#if NON_LOCAL_FORCES
  if( NUM_FLUID_COMPONENTS == 2)
  {
    if(    lattice->param.Gads[0] != 0.
        ||
           lattice->param.Gads[1] != 0.)
    {
      compute_double_fluid_solid_force( lattice);
    }
  }
  else if( NUM_FLUID_COMPONENTS == 1)
  {
  }
  else
  {
    printf(
      "%s %d >> "
      "compute_feq() -- "
      "Unhandled case NUM_FLUID_COMPONENTS = %d .  "
      "Exiting!\n",__FILE__,__LINE__, NUM_FLUID_COMPONENTS);
    process_exit(1);
  }
#endif /* NON_LOCAL_FORCES */

  //dump_pdf( lattice, /*time=*/ 9000*lattice->param.FrameRate);

  //compute_macro_vars( lattice);

  //dump_macro_vars( lattice, /*time=*/ 0);

#if VERBOSITY_LEVEL > 0
  printf("init_problem() -- Problem initialized.\n");
#endif /* VERBOSITY_LEVEL > 0 */

} /* void init_problem( struct lattice_struct *lattice) */

// void destruct_lattice( struct lattice_struct *lattice)
//##############################################################################
//
// D E S T R U C T   L A T T I C E
//
//  - Destruct lattice.
//
void destruct_lattice( struct lattice_struct *lattice)
{
  int subs;

  assert( lattice!=NULL);

 for( subs=0; subs<NUM_FLUID_COMPONENTS; subs++)
 {
  assert( lattice->pdf[subs]!=NULL);
  free(   lattice->pdf[subs]);

  assert( lattice->macro_vars[subs]!=NULL);
  free(   lattice->macro_vars[subs]);

  assert( lattice->bc[subs]!=NULL);
  free(   lattice->bc[subs]);
#if NON_LOCAL_FORCES
  assert( lattice->force[subs]!=NULL);
  free(   lattice->force[subs]);
#endif /* NON_LOCAL_FORCES */
 }

#if STORE_U_COMPOSITE
  free(   lattice->upr);
#endif /* STORE_U_COMPOSITE */

  process_finalize();

  free(   lattice);

} /* void destruct_lattice( struct lattice_struct *lattice) */

#if INAMURO_SIGMA_COMPONENT && STORE_BTC
#if 0
void sigma_stuff( lattice_ptr lattice)
{
  int    btc_time;
  int    i, j, n;
  double btc_val01, btc_val02, btc_val03, btc_val04;
  int    width01,   width02,   width03,   width04;

  // Turn off concentration boundaries when time is up.
  if(    lattice->param.sigma_stop >= 0
      && lattice->time > lattice->param.sigma_stop)
  {
    lattice->param.rho_sigma_in  = 0.;
    lattice->param.rho_sigma_out = 0.;

  } /* if( lattice->param.sigma_stop >= 0 && ... */

  // Accumulate break through curve.
  if( !( lattice->param.sigma_btc_rate <= 0 || lattice->FlowDir==0))
  {
    if(    lattice->param.sigma_start <= lattice->time
       &&  (
              lattice->param.sigma_stop  < 0
            ||
              lattice->param.sigma_stop  >= lattice->time
           )
       && !( lattice->time % lattice->param.sigma_btc_rate))
    {
      btc_time = ( lattice->time
                 - ((lattice->param.sigma_start>0)
                   ?(lattice->param.sigma_start)
                   :(0))                           )
                 / lattice->param.sigma_btc_rate;

//printf("%s (%d) >> btc_time = %d (%d)\n",
//    __FILE__,__LINE__,btc_time,lattice->SizeBTC);

      btc_val01 = 0.;
      btc_val02 = 0.;
      btc_val03 = 0.;
      btc_val04 = 0.;

      if( lattice->FlowDir == /*Horizontal*/1)
      {
        i = ( (lattice->param.sigma_btc_spot >= 0)
            ? (lattice->param.sigma_btc_spot)
            : (get_LX(lattice)-2));
        width01 = 0;
        width02 = 0;
        width03 = 0;
        width04 = 0;
        for( j=0; j<get_LY(lattice); j++)
        {
          //
          // Concentration at sigma_spot-1
          //
          n = i-1 + j*get_LX(lattice);
          if( !( lattice->bc[0][n].bc_type & BC_SOLID_NODE))
          {
            btc_val01 += lattice->macro_vars[1][n].rho;
            width01++;

          } /* if( !( lattice->bc[n].bc_type & BC_SOLID_NODE)) */

          //
          // Concentration at sigma_spot
          //
          n = i   + j*get_LX(lattice);
          if( !( lattice->bc[0][n].bc_type & BC_SOLID_NODE))
          {
            btc_val02 += lattice->macro_vars[1][n].rho;
            width02++;

          } /* if( !( lattice->bc[n].bc_type & BC_SOLID_NODE)) */

          //
          // Concentration at sigma_spot+1
          //
          n = i+1 + j*get_LX(lattice);
          if( !( lattice->bc[0][n].bc_type & BC_SOLID_NODE))
          {
            btc_val03 += lattice->macro_vars[1][n].rho;
            width03++;

          } /* if( !( lattice->bc[n].bc_type & BC_SOLID_NODE)) */

          //
          // Velocity at sigma_spot
          //
          n = i + j*get_LX(lattice);
          if( !( lattice->bc[0][n].bc_type & BC_SOLID_NODE))
          {
            btc_val04 += lattice->macro_vars[1][n].u[0];
            width04++;

          } /* if( !( lattice->bc[n].bc_type & BC_SOLID_NODE)) */


        } /* for( j=0; j<get_LY(lattice); j++) */

        btc_val01 /= width01;
        btc_val02 /= width02;
        btc_val03 /= width03;
        btc_val04 /= width04;

      } /* if( lattice->FlowDir == 1) */

      else if( lattice->FlowDir == /*Vertical*/2)
      {
        j = ( (lattice->param.sigma_btc_spot >= 0)
            ? (lattice->param.sigma_btc_spot)
            : (get_LY(lattice)-2));
        width01 = 0;
        width02 = 0;
        width03 = 0;
        width04 = 0;
        for( i=0; i<get_LX(lattice); i++)
        {
          //
          // Concentration at sigma_spot-1
          //
          n = i + (j-1)*get_LX(lattice);
          if( !( lattice->bc[0][n].bc_type & BC_SOLID_NODE))
          {
            btc_val01 += lattice->macro_vars[1][n].rho;
            width01++;

          } /* if( !( lattice->bc[n].bc_type & BC_SOLID_NODE)) */

          //
          // Concentration at sigma_spot
          //
          n = i + (j  )*get_LX(lattice);
          if( !( lattice->bc[0][n].bc_type & BC_SOLID_NODE))
          {
            btc_val02 += lattice->macro_vars[1][n].rho;
            width02++;

          } /* if( !( lattice->bc[n].bc_type & BC_SOLID_NODE)) */

          //
          // Concentration at sigma_spot+1
          //
          n = i + (j+1)*get_LX(lattice);
          if( !( lattice->bc[0][n].bc_type & BC_SOLID_NODE))
          {
            btc_val03 += lattice->macro_vars[1][n].rho;
            width03++;

          } /* if( !( lattice->bc[n].bc_type & BC_SOLID_NODE)) */

          //
          // Velocity at sigma_spot
          //
          n = i + j*get_LX(lattice);
          if( !( lattice->bc[0][n].bc_type & BC_SOLID_NODE))
          {
            btc_val04 += lattice->macro_vars[1][n].u[1];
            width04++;

          } /* if( !( lattice->bc[n].bc_type & BC_SOLID_NODE)) */


        } /* for( i=0; i<get_LX(lattice); i++) */

        btc_val01 /= width01;
        btc_val02 /= width02;
        btc_val03 /= width03;
        btc_val04 /= width04;

      } /* else if( lattice->FlowDir == 2) */

      else // Flow direction is undetermined.
      {
        // Unhandled case.
        // TODO: Warning message?

      } /* if( lattice->FlowDir == ?) else */

//printf("%s (%d) >> btc_val01 = %f\n",__FILE__,__LINE__,btc_val01);
      lattice->param.sigma_btc[5*btc_time+0] = lattice->time;
      lattice->param.sigma_btc[5*btc_time+1] = btc_val01;
      lattice->param.sigma_btc[5*btc_time+2] = btc_val02;
      lattice->param.sigma_btc[5*btc_time+3] = btc_val03;
      lattice->param.sigma_btc[5*btc_time+4] = btc_val04;

    } /* if( !( lattice->time % lattice->param.sigma_btc_rate)) */

  } /* if( lattice->param.sigma_start <= lattice->time) */

} /* void sigma_stuff( lattice_ptr lattice) */
#else
void sigma_stuff( lattice_ptr lattice)
{
  int    btc_time;
  int    i, j, n, nm, np;
  double btc_val01, btc_val02, btc_val03, btc_val04;
  int    width;
#if 0
  // Turn off concentration boundaries when time is up.
  if(    lattice->param.sigma_stop >= 0
      && lattice->time > lattice->param.sigma_stop)
  {
    lattice->param.rho_sigma_in  = 0.;
    lattice->param.rho_sigma_out = 0.;

  } /* if( lattice->param.sigma_stop >= 0 && ... */
#endif
  // Accumulate break through curve.
  if( !( lattice->param.sigma_btc_rate <= 0 || lattice->FlowDir==0))
  {
    if(    lattice->param.sigma_start <= lattice->time
//       &&  (
//              lattice->param.sigma_stop  < 0
//            ||
//              lattice->param.sigma_stop  >= lattice->time
//           )
       && !( lattice->time % lattice->param.sigma_btc_rate))
    {
      btc_time = ( lattice->time
                 - ((lattice->param.sigma_start>0)
                   ?(lattice->param.sigma_start)
                   :(0))                           )
                 / lattice->param.sigma_btc_rate;

//printf("%s (%d) >> btc_time = %d (%d)\n",
//    __FILE__,__LINE__,btc_time,lattice->SizeBTC);

      btc_val01 = 0.;
      btc_val02 = 0.;
      btc_val03 = 0.;
      btc_val04 = 0.;

      if( lattice->FlowDir == /*Horizontal*/1)
      {
        i = ( (lattice->param.sigma_btc_spot >= 0)
            ? (lattice->param.sigma_btc_spot)
            : (get_LX(lattice)-2));
        width = 0;
        for( j=0; j<get_LY(lattice); j++)
        {
          nm = i-1 + j*get_LX(lattice);
          n  = i   + j*get_LX(lattice);
          np = i+1 + j*get_LX(lattice);
          if( !(   lattice->bc[0][nm].bc_type & BC_SOLID_NODE
                || lattice->bc[0][n ].bc_type & BC_SOLID_NODE
                || lattice->bc[0][np].bc_type & BC_SOLID_NODE ) )
          {
            // Concentration at sigma_spot-1
            btc_val01 += lattice->macro_vars[1][nm].rho;

            // Concentration at sigma_spot
            btc_val02 += lattice->macro_vars[1][n ].rho;

            // Concentration at sigma_spot+1
            btc_val03 += lattice->macro_vars[1][np].rho;

            // Velocity at sigma_spot
            btc_val04 += lattice->macro_vars[1][n ].u[0];

            width++;

          } /* if( !( lattice->bc[n].bc_type & BC_SOLID_NODE)) */

        } /* for( j=0; j<get_LY(lattice); j++) */

        btc_val01 /= width;
        btc_val02 /= width;
        btc_val03 /= width;
        btc_val04 /= width;

      } /* if( lattice->FlowDir == 1) */

      else if( lattice->FlowDir == /*Vertical*/2)
      {
        j = ( (lattice->param.sigma_btc_spot >= 0)
            ? (lattice->param.sigma_btc_spot)
            : (get_LY(lattice)-2));
        width = 0;
        for( i=0; i<get_LX(lattice); i++)
        {
          nm = i + (j-1)*get_LX(lattice);
          n  = i + (j  )*get_LX(lattice);
          np = i + (j+1)*get_LX(lattice);
          if( !(   lattice->bc[0][nm].bc_type & BC_SOLID_NODE
                || lattice->bc[0][n ].bc_type & BC_SOLID_NODE
                || lattice->bc[0][np].bc_type & BC_SOLID_NODE ) )
          {
            // Concentration at sigma_spot-1
            btc_val01 += lattice->macro_vars[1][nm].rho;

            // Concentration at sigma_spot
            btc_val02 += lattice->macro_vars[1][n ].rho;

            // Concentration at sigma_spot+1
            btc_val03 += lattice->macro_vars[1][np].rho;

            // Velocity at sigma_spot
            btc_val04 += lattice->macro_vars[1][n ].u[1];

            width++;

          } /* if( !( lattice->bc[n].bc_type & BC_SOLID_NODE)) */

        } /* for( i=0; i<get_LX(lattice); i++) */

        btc_val01 /= width;
        btc_val02 /= width;
        btc_val03 /= width;
        btc_val04 /= width;

      } /* else if( lattice->FlowDir == 2) */

      else // Flow direction is undetermined.
      {
        // Unhandled case.
        // TODO: Warning message?

      } /* if( lattice->FlowDir == ?) else */

//printf("%s (%d) >> btc_val01 = %f\n",__FILE__,__LINE__,btc_val01);
      lattice->param.sigma_btc[5*btc_time+0] = lattice->time;
      lattice->param.sigma_btc[5*btc_time+1] = btc_val01;
      lattice->param.sigma_btc[5*btc_time+2] = btc_val02;
      lattice->param.sigma_btc[5*btc_time+3] = btc_val03;
      lattice->param.sigma_btc[5*btc_time+4] = btc_val04;

    } /* if( !( lattice->time % lattice->param.sigma_btc_rate)) */

  } /* if( lattice->param.sigma_start <= lattice->time) */

} /* void sigma_stuff( lattice_ptr lattice) */
#endif
#endif /* INAMURO_SIGMA_COMPONENT && STORE_BTC */

// int get_sizeof_lattice_structure( lattice_ptr lattice)
//##############################################################################
//
// G E T _ S I Z E O F _ L A T T I C E _ S T R U C T U R E
//
//  - Return size of struct lattice_struct in bytes.
//
int get_sizeof_lattice_structure( lattice_ptr lattice)
{
  return sizeof( struct lattice_struct);

} /* int get_sizeof_lattice_structure( lattice_ptr lattice) */

// int get_sizeof_lattice( lattice_ptr lattice)
//##############################################################################
//
// G E T _ S I Z E O F _ L A T T I C E
//
//  - Return size of lattice in bytes.
//
int get_sizeof_lattice( lattice_ptr lattice)
{
  return
      sizeof(int) // NumNodes
    + sizeof(int) // NumTimeSteps
    + sizeof(int) // time
    + sizeof(int) // frame
    + sizeof(int)*NUM_FLUID_COMPONENTS // periodic_x
    + sizeof(int)*NUM_FLUID_COMPONENTS // periodic_y

#if INAMURO_SIGMA_COMPONENT
    + sizeof(int) // SizeBTC
    + sizeof(int) // FlowDir
#endif

    + sizeof(struct param_struct)

    + lattice->NumNodes
    * (
        NUM_FLUID_COMPONENTS*sizeof(struct pdf_struct)
      + NUM_FLUID_COMPONENTS*sizeof(struct macro_vars_struct)
      + NUM_FLUID_COMPONENTS*sizeof(struct bc_struct)
#if NON_LOCAL_FORCES
      + NUM_FLUID_COMPONENTS*sizeof(struct force_struct)
#endif /* NON_LOCAL_FORCES */
#if STORE_U_COMPOSITE
      + sizeof(struct upr_struct)
#endif /* STORE_U_COMPOSITE */
#if POROUS_MEDIA || FREED_POROUS_MEDIA
      + sizeof(struct ns_struct)
#endif /* POROUS_MEDIA */
      )
    // Include lbmpi_ptr ?
    // Include user_stuff_struct ?
    ;

} /* int get_sizeof_lattice( lattice_ptr lattice) */

// int get_num_active_nodes( lattice_ptr lattice)
//##############################################################################
//
// G E T _ N U M B E R _ A C T I V E _ N O D E S
//
//  - Return number of active nodes.
//
int get_num_active_nodes( lattice_ptr lattice)
{
  int n, k;

  k = 0;

  for( n=0; n<lattice->NumNodes; n++)
  {
    if( lattice->bc[0][n].bc_type != INACTIVE_NODE)
    {
      k++;
    }
  }

  return k;

} /* int get_num_active_nodes( lattice_ptr lattice) */

// void check_point_save( lattice_ptr lattice)
void check_point_save( lattice_ptr lattice)
{
  FILE *o;
  char filename[1024];
  int n, a;

  printf("############################################################\n");
  printf("                                                            \n");
  printf("%s %d >> Saving check point.\n",__FILE__,__LINE__);
  printf("                                                            \n");
  printf("############################################################\n");
  printf("                                                            \n");
  printf("   ####  ### ### #######   ####  ###  ## ######    ###    #####  ##  ### #######\n");
  printf("  #    #  #   #   #    #  #    #  #   #   #    #  #   #     #     #   #  #  #  #\n");
  printf(" #        #   #   #      #        #  #    #    # #     #    #     ##  #     #   \n");
  printf(" #        #   #   #  #   #        #  #    #    # #     #    #     ##  #     #   \n");
  printf(" #        #####   ####   #        # #     #####  #     #    #     # # #     #   \n");
  printf(" #        #   #   #  #   #        ###     #      #     #    #     #  ##     #   \n");
  printf(" #        #   #   #      #        #  #    #      #     #    #     #  ##     #   \n");
  printf("  #    #  #   #   #    #  #    #  #   #   #       #   #     #     #   #     #   \n");
  printf("   ####  ### ### #######   ####  ###  ## ####      ###    #####  ###  #    ###  \n");
  printf("                                                            \n");
  printf("############################################################\n");


  sprintf(filename, "./%s/checkpoint_%dx%d.dat",
      get_out_path(lattice),
      get_LX(lattice),
      get_LY(lattice));
  if( !( o = fopen(filename,"w+")))
  {
    printf("%s %d >> ERROR: Can't save checkpoint file \"%s\".\n",
      __FILE__,__LINE__,filename);
    return;
  }
  // Write check point information.
  fprintf( o, "%d\n", lattice->frame);
  fprintf( o, "%d\n", get_LX(lattice));
  fprintf( o, "%d\n", get_LY(lattice));
  for( n=0; n<get_NumNodes(lattice); n++)
  {
    for( a=0; a<9; a++)
    {
      fprintf( o, "%20.17f\n",lattice->pdf[0][n].feq[a]);
    }
    for( a=0; a<9; a++)
    {
      fprintf( o, "%20.17f\n",lattice->pdf[0][n].f[a]);
    }
    for( a=0; a<9; a++)
    {
      fprintf( o, "%20.17f\n",lattice->pdf[0][n].ftemp[a]);
    }
    if(NUM_FLUID_COMPONENTS==2)
    {
      for( a=0; a<9; a++)
      {
        fprintf( o, "%20.17f\n",lattice->pdf[1][n].feq[a]);
      }
      for( a=0; a<9; a++)
      {
        fprintf( o, "%20.17f\n",lattice->pdf[1][n].f[a]);
      }
      for( a=0; a<9; a++)
      {
        fprintf( o, "%20.17f\n",lattice->pdf[1][n].ftemp[a]);
      }
    } /* if(NUM_FLUID_COMPONENTS==2) */
  } /* for( n=0; n<get_NumNodes(lattice); n++) */

#if INAMURO_SIGMA_COMPONENT && STORE_BTC
  fprintf( o, "%d\n", lattice->SizeBTC);
  for( n=0; n<5*lattice->SizeBTC; n++)
  {
    fprintf( o, "%20.17f\n", lattice->param.sigma_btc[n]);
  }
#endif /* INAMURO_SIGMA_COMPONENT && STORE_BTC */

  fclose(o);

  printf("%s %d >> Saving check point done.\n",__FILE__,__LINE__);

} /* void check_point_save( lattice_ptr lattice) */

// void check_point_load( lattice_ptr lattice)
void check_point_load( lattice_ptr lattice)
{
  FILE *in;
  char filename[1024];
  int n, a;
  int LX_in, LY_in;
  int SizeBTC;

  printf("############################################################\n");
  printf("                                                            \n");
  printf("%s %d >> Loading check point.\n",__FILE__,__LINE__);
  printf("                                                            \n");
  printf("############################################################\n");
  printf("                                                            \n");
  printf("   ####  ### ### #######   ####  ###  ## ######    ###    #####  ##  ### #######\n");
  printf("  #    #  #   #   #    #  #    #  #   #   #    #  #   #     #     #   #  #  #  #\n");
  printf(" #        #   #   #      #        #  #    #    # #     #    #     ##  #     #   \n");
  printf(" #        #   #   #  #   #        #  #    #    # #     #    #     ##  #     #   \n");
  printf(" #        #####   ####   #        # #     #####  #     #    #     # # #     #   \n");

  printf(" #        #   #   #  #   #        ###     #      #     #    #     #  ##     #   \n");
  printf(" #        #   #   #      #        #  #    #      #     #    #     #  ##     #   \n");
  printf("  #    #  #   #   #    #  #    #  #   #   #       #   #     #     #   #     #   \n");
  printf("   ####  ### ### #######   ####  ###  ## ####      ###    #####  ###  #    ###  \n");
  printf("                                                            \n");
  printf("############################################################\n");

  sprintf(filename, "./%s/checkpoint.dat", get_out_path(lattice));
  if( !( in = fopen(filename,"r+")))
  {
    printf("%s %d >> WARNING: Can't load checkpoint \"%s\".\n",
      __FILE__,__LINE__,filename);
    return;
  }
  // Read check point information.
  fscanf( in, "%d", &(lattice->frame));
  fscanf( in, "%d", &LX_in);
  fscanf( in, "%d", &LY_in);
  if( LX_in != get_LX(lattice))
  {
    printf(
      "%s %d >> ERROR: "
      "Checkpoint LX %d does not match current domain LX %d.\n",
      __FILE__,__LINE__, LX_in, get_LX(lattice));
    if( LY_in != get_LY(lattice))
    {
      printf(
        "%s %d >> ERROR: "
        "Checkpoint LY %d does not match current domain LY %d.\n",
        __FILE__,__LINE__, LY_in, get_LY(lattice));
    }
    process_exit(1);
  }
  if( LY_in != get_LY(lattice))
  {
    printf(
      "%s %d >> ERROR: "
      "Checkpoint LY %d does not match current domain LY %d.\n",
      __FILE__,__LINE__, LY_in, get_LY(lattice));
    process_exit(1);
  }
  for( n=0; n<get_NumNodes(lattice); n++)
  {
    for( a=0; a<9; a++)
    {
      fscanf( in, "%lf",lattice->pdf[0][n].feq+a);
    }
    for( a=0; a<9; a++)
    {
      fscanf( in, "%lf",lattice->pdf[0][n].f+a);
    }
    for( a=0; a<9; a++)
    {
      fscanf( in, "%lf",lattice->pdf[0][n].ftemp+a);
    }
    if(NUM_FLUID_COMPONENTS==2)
    {
      for( a=0; a<9; a++)
      {
        fscanf( in, "%lf",lattice->pdf[1][n].feq+a);
      }
      for( a=0; a<9; a++)
      {
        fscanf( in, "%lf",lattice->pdf[1][n].f+a);
      }
      for( a=0; a<9; a++)
      {
        fscanf( in, "%lf",lattice->pdf[1][n].ftemp+a);
      }
    } /* if(NUM_FLUID_COMPONENTS==2) */
  } /* for( n=0; n<get_NumNodes(lattice); n++) */

#if INAMURO_SIGMA_COMPONENT && STORE_BTC
  fscanf( in, "%d", &SizeBTC);
  for( n=0; n<5*SizeBTC; n++)
  {
    fscanf( in, "%lf", lattice->param.sigma_btc+n);
  }
#endif /* INAMURO_SIGMA_COMPONENT && STORE_BTC */

  fclose(in);

  printf("%s %d >> Loading check point done.\n",__FILE__,__LINE__);

} /* void check_point_load( lattice_ptr lattice) */

// vim: foldmethod=syntax
