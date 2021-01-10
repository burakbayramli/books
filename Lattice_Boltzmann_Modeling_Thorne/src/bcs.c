//##############################################################################
//
// Copyright (C), 2005, Michael Sukop and Danny Thorne
//
// bcs.c
//
//  - Boundary conditions.
//
//  - void bcs( lattice_ptr lattice);
//
//  - void process_bcs( char *filename, int **bcs);
//

#define RHO0_TEST 0

#if 0 // {{{
// B C S  {{{
void bcs( lattice_ptr lattice)
{
  int    n;
  int    subs;
  int    *bc;
  double *ftemp;
  double u_x,
         u_y,
         rho;

 for( subs=0; subs<NUM_FLUID_COMPONENTS; subs++)
 {

  bc = &( lattice->bc[subs][0].bc_type);
  ftemp = lattice->pdf[subs][0].ftemp;

  for( n=0; n<lattice->NumNodes; n++)
  {
    if(    *bc != BC_FLUID_NODE
        && *bc != ( BC_FLUID_NODE | BC_FILM_NODE)
        && *bc != BC_SOLID_NODE)
    {
      if( *bc & BC_PRESSURE_N_IN )
      {
        // North, Inflow

//printf("bcs() -- North, Inflow at n = %d, ( %d, %d)\n",
//  n, lattice->node[n].i, lattice->node[n].j);
//printf("  Before>> %f %f %f %f [%f] %f %f [%f] [%f]\n",
//    ftemp[0], ftemp[1], ftemp[2],
//    ftemp[3], ftemp[4], ftemp[5],
//    ftemp[6], ftemp[7], ftemp[8]  );

        u_y = -1.
            + ( ftemp[0] + ftemp[1] + ftemp[3]
              + 2.*( ftemp[2] + ftemp[5] + ftemp[6]))
            / lattice->param.rho_in;

        ftemp[4] = ftemp[2] - (2./3.)*lattice->param.rho_in*u_y;

        ftemp[7] = ftemp[5] + (1./2.)*( ftemp[1] - ftemp[3])
                            - (1./6.)*lattice->param.rho_in*u_y;

        ftemp[8] = ftemp[6] + (1./2.)*( ftemp[3] - ftemp[1])
                            - (1./6.)*lattice->param.rho_in*u_y;

//printf("  Before>> %f %f %f %f [%f] %f %f [%f] [%f]\n",
//    ftemp[0], ftemp[1], ftemp[2],
//    ftemp[3], ftemp[4], ftemp[5],
//    ftemp[6], ftemp[7], ftemp[8]  );

      } /* if( *bc & BC_PRESSURE_N_IN ) */

      if( *bc & BC_PRESSURE_S_IN )
      {
        printf("bcs() -- ERROR: Support for South, Inflow pressure "
            "boundaries is pending. (Exiting!)");
        process_exit(1);
      }
      if( *bc & BC_PRESSURE_E_IN )
      {
        printf("bcs() -- ERROR: Support for East, Inflow pressure "
            "boundaries is pending. (Exiting!)");
        process_exit(1);
      }
      if( *bc & BC_PRESSURE_W_IN )
      {
        printf("bcs() -- ERROR: Support for West, Inflow pressure "
            "boundaries is pending. (Exiting!)");
        process_exit(1);
      }
      if( *bc & BC_PRESSURE_N_OUT)
      {
        printf("bcs() -- ERROR: Support for North, Outflow pressure "
            "boundaries is pending. (Exiting!)");
        process_exit(1);
      }

      if( *bc & BC_PRESSURE_S_OUT)
      {
        // South, Outflow

//printf("bcs() -- South, Outflow at n = %d, ( %d, %d)\n",
//  n, lattice->node[n].i, lattice->node[n].j);
//printf("  Before>> %f %f [%f] %f %f [%f] [%f] %f %f\n",
//    ftemp[0], ftemp[1], ftemp[2],
//    ftemp[3], ftemp[4], ftemp[5],
//    ftemp[6], ftemp[7], ftemp[8]  );

        u_y = 1.
            - ( ftemp[0] + ftemp[1] + ftemp[3]
              + 2.*( ftemp[4] + ftemp[7] + ftemp[8]))
            / lattice->param.rho_out;

        ftemp[2] = ftemp[4] + (2./3.)*lattice->param.rho_out*u_y;

        ftemp[5] = ftemp[7] + (1./2.)*( ftemp[3] - ftemp[1])
                            + (1./6.)*lattice->param.rho_out*u_y;

        ftemp[6] = ftemp[8] + (1./2.)*( ftemp[1] - ftemp[3])
                            + (1./6.)*lattice->param.rho_out*u_y;

//printf("  Before>> %f %f [%f] %f %f [%f] [%f] %f %f\n",
//    ftemp[0], ftemp[1], ftemp[2],
//    ftemp[3], ftemp[4], ftemp[5],
//    ftemp[6], ftemp[7], ftemp[8]  );

      } /* if( *bc & BC_PRESSURE_S_OUT) */

      if( *bc & BC_PRESSURE_E_OUT)
      {
        printf("bcs() -- ERROR: Support for East, Outflow pressure "
            "boundaries is pending. (Exiting!)");
        process_exit(1);
      }
      if( *bc & BC_PRESSURE_W_OUT)
      {
        printf("bcs() -- ERROR: Support for West, Outflow pressure "
            "boundaries is pending. (Exiting!)");
        process_exit(1);
      }

    } /* if( *bc != 0 && *bc != BC_SOLID_NODE) */

    ftemp+=27;

    bc++;

  } /* for( n=0; n<lattice->NumNodes; n++) */

 } /* for( subs=0; subs<NUM_FLUID_COMPONENTS; subs++) */

} /* void bcs( lattice_ptr lattice) */
// }}}
#else
                                                                           //}}}
#if 0 // {{{
// B C S  {{{
void bcs( lattice_ptr lattice)
{
  int    i, j, n;
  int    subs;
  int    *bc;
  double *ftemp, *ftemp_end, *ftemp_mid;
  double u_x,
         u_y;
  double u_in[2][2],
         u_out[2][2],
         u,
         rho;

#if 0
 u_in[0][0]  =  lattice->param.uy_in;
 u_in[1][0]  =  lattice->param.uy_in;
 u_in[0][1]  = -lattice->param.uy_in;
 u_in[1][1]  = -lattice->param.uy_in;

 u_out[0][0] =  lattice->param.uy_out;
 u_out[1][0] =  lattice->param.uy_out;
 u_out[0][1] = -lattice->param.uy_out;
 u_out[1][1] = -lattice->param.uy_out;
#else
 u_in[0][0]  =  0.;
 u_in[1][0]  =  lattice->param.uy_in;
 u_in[0][1]  = -lattice->param.uy_in;
 u_in[1][1]  =  0.;

 u_out[0][0] =  lattice->param.uy_out;
 u_out[1][0] =  0.;
 u_out[0][1] =  0.;
 u_out[1][1] = -lattice->param.uy_out;
#endif

 for( subs=0; subs<NUM_FLUID_COMPONENTS; subs++)
 {

#if 0
  bc = &( lattice->bc[subs][0].bc_type);
  ftemp = lattice->pdf[subs][0].ftemp;

  for( n=0; n<lattice->NumNodes; n++)
  {
    if(    *bc != BC_FLUID_NODE
        && *bc != ( BC_FLUID_NODE | BC_FILM_NODE)
        && *bc != BC_SOLID_NODE)
    {
      if( *bc & BC_PRESSURE_N_IN )
      {
        // North, Inflow
        u_y = -1.
            + ( ftemp[0] + ftemp[1] + ftemp[3]
              + 2.*( ftemp[2] + ftemp[5] + ftemp[6]))
            / lattice->param.rho_in;

        ftemp[4] = ftemp[2] - (2./3.)*lattice->param.rho_in*u_y;

        ftemp[7] = ftemp[5] + (1./2.)*( ftemp[1] - ftemp[3])
                            - (1./6.)*lattice->param.rho_in*u_y;

        ftemp[8] = ftemp[6] + (1./2.)*( ftemp[3] - ftemp[1])
                            - (1./6.)*lattice->param.rho_in*u_y;

      } /* if( *bc & BC_PRESSURE_N_IN ) */

      if( *bc & BC_PRESSURE_S_IN )
      {
        printf("bcs() -- ERROR: Support for South, Inflow pressure "
            "boundaries is pending. (Exiting!)");
        process_exit(1);
      }
      if( *bc & BC_PRESSURE_E_IN )
      {
        printf("bcs() -- ERROR: Support for East, Inflow pressure "
            "boundaries is pending. (Exiting!)");
        process_exit(1);
      }
      if( *bc & BC_PRESSURE_W_IN )
      {
        printf("bcs() -- ERROR: Support for West, Inflow pressure "
            "boundaries is pending. (Exiting!)");
        process_exit(1);
      }
      if( *bc & BC_PRESSURE_N_OUT)
      {
        printf("bcs() -- ERROR: Support for North, Outflow pressure "
            "boundaries is pending. (Exiting!)");
        process_exit(1);
      }

      if( *bc & BC_PRESSURE_S_OUT)
      {
        // South, Outflow
        u_y = 1.
            - ( ftemp[0] + ftemp[1] + ftemp[3]
              + 2.*( ftemp[4] + ftemp[7] + ftemp[8]))
            / lattice->param.rho_out;

        ftemp[2] = ftemp[4] + (2./3.)*lattice->param.rho_out*u_y;

        ftemp[5] = ftemp[7] + (1./2.)*( ftemp[3] - ftemp[1])
                            + (1./6.)*lattice->param.rho_out*u_y;

        ftemp[6] = ftemp[8] + (1./2.)*( ftemp[1] - ftemp[3])
                            + (1./6.)*lattice->param.rho_out*u_y;

      } /* if( *bc & BC_PRESSURE_S_OUT) */

      if( *bc & BC_PRESSURE_E_OUT)
      {
        printf("bcs() -- ERROR: Support for East, Outflow pressure "
            "boundaries is pending. (Exiting!)");
        process_exit(1);
      }
      if( *bc & BC_PRESSURE_W_OUT)
      {
        printf("bcs() -- ERROR: Support for West, Outflow pressure "
            "boundaries is pending. (Exiting!)");
        process_exit(1);
      }

    } /* if( *bc != 0 && *bc != BC_SOLID_NODE) */

    ftemp+=27;

    bc++;

  } /* for( n=0; n<lattice->NumNodes; n++) */
#else
  // NOTE: Should previously (in initialization stage) have checked to
  // insure no solid nodes on inflow/outflow boundaries. Do not do it here
  // inside the loop!
  if( lattice->param.pressure_n_in[subs] )
  {
//printf("bcs() -- pressure_n_in[%d]\n", subs);
    ftemp = lattice->pdf[subs][lattice->NumNodes-lattice->param.LX].ftemp;
    ftemp_end = lattice->pdf[subs][lattice->NumNodes].ftemp;
    while( ftemp < ftemp_end)
    {
      // North, Inflow
      u_y = -1.
          + ( ftemp[0] + ftemp[1] + ftemp[3]
            + 2.*( ftemp[2] + ftemp[5] + ftemp[6]))
          / lattice->param.rho_in;

      ftemp[4] = ftemp[2] - (2./3.)*lattice->param.rho_in*u_y;

      ftemp[7] = ftemp[5] + (1./2.)*( ftemp[1] - ftemp[3])
                          - (1./6.)*lattice->param.rho_in*u_y;

      ftemp[8] = ftemp[6] + (1./2.)*( ftemp[3] - ftemp[1])
                          - (1./6.)*lattice->param.rho_in*u_y;

      ftemp += ( sizeof(struct pdf_struct)/8);

    } /* while( ftemp < ftemp_end) */
  } /* if( lattice->param.pressure_n_in[subs] ) */

  if( lattice->param.pressure_s_in[subs] )
  {
  }
  if( lattice->param.pressure_n_out[subs])
  {
  }
  if( lattice->param.pressure_s_out[subs])
  {
//printf("bcs() -- pressure_s_out[%d]\n", subs);
    ftemp = lattice->pdf[subs][0].ftemp;
    ftemp_end = lattice->pdf[subs][lattice->param.LX].ftemp;
    while( ftemp < ftemp_end)
    {
      u_y = 1.
          - ( ftemp[0] + ftemp[1] + ftemp[3]
            + 2.*( ftemp[4] + ftemp[7] + ftemp[8]))
          / lattice->param.rho_out;

      ftemp[2] = ftemp[4] + (2./3.)*lattice->param.rho_out*u_y;

      ftemp[5] = ftemp[7] + (1./2.)*( ftemp[3] - ftemp[1])
                          + (1./6.)*lattice->param.rho_out*u_y;

      ftemp[6] = ftemp[8] + (1./2.)*( ftemp[1] - ftemp[3])
                          + (1./6.)*lattice->param.rho_out*u_y;

      ftemp += ( sizeof(struct pdf_struct)/8);

    } /* while( ftemp < ftemp_end) */

  } /* if( pressure_s_out[subs]) */

  if( lattice->param.velocity_n_in[subs] )
  {
//printf("bcs() -- velocity_n_in[%d]\n", subs);
    ftemp = lattice->pdf[subs][lattice->NumNodes-lattice->param.LX].ftemp;
ftemp_mid = lattice->pdf[subs][lattice->NumNodes-7*lattice->param.LX/8].ftemp;
    ftemp_end = lattice->pdf[subs][lattice->NumNodes].ftemp;
    //u = ((subs==1)?(-1):(1))*lattice->param.uy_in;
    while( ftemp < ftemp_mid)
    {
      // North, Inflow
      //u = u_in[((double)rand()/(double)RAND_MAX<.5)?(0):(1)][subs];
      u = 0.;
      rho = ( ftemp[0] + ftemp[1] + ftemp[3]
             + 2.*( ftemp[2] + ftemp[5] + ftemp[6]))
           / ( 1. + u);

      ftemp[4] = ftemp[2] - (2./3.)*rho*u;

      ftemp[7] = ftemp[5] + (1./2.)*( ftemp[1] - ftemp[3])
                          - (1./6.)*rho*u;

      ftemp[8] = ftemp[6] + (1./2.)*( ftemp[3] - ftemp[1])
                          - (1./6.)*rho*u;

      ftemp += 27;//( sizeof(struct pdf_struct)/8);

    } /* while( ftemp < ftemp_end) */
ftemp_mid = lattice->pdf[subs][lattice->NumNodes-3*lattice->param.LX/4].ftemp;
    while( ftemp < ftemp_mid)
    {
      // North, Inflow
      //u = u_in[((double)rand()/(double)RAND_MAX<.5)?(0):(1)][subs];
      u = u_in[0][subs];
      rho = ( ftemp[0] + ftemp[1] + ftemp[3]
             + 2.*( ftemp[2] + ftemp[5] + ftemp[6]))
           / ( 1. + u);

      ftemp[4] = ftemp[2] - (2./3.)*rho*u;

      ftemp[7] = ftemp[5] + (1./2.)*( ftemp[1] - ftemp[3])
                          - (1./6.)*rho*u;

      ftemp[8] = ftemp[6] + (1./2.)*( ftemp[3] - ftemp[1])
                          - (1./6.)*rho*u;

      ftemp += 27;//( sizeof(struct pdf_struct)/8);

    } /* while( ftemp < ftemp_end) */
ftemp_mid = lattice->pdf[subs][lattice->NumNodes-lattice->param.LX/2].ftemp;
    while( ftemp < ftemp_mid)
    {
      // North, Inflow
      //u = u_in[((double)rand()/(double)RAND_MAX<.5)?(0):(1)][subs];
      u = 0.;
      rho = ( ftemp[0] + ftemp[1] + ftemp[3]
             + 2.*( ftemp[2] + ftemp[5] + ftemp[6]))
           / ( 1. + u);

      ftemp[4] = ftemp[2] - (2./3.)*rho*u;

      ftemp[7] = ftemp[5] + (1./2.)*( ftemp[1] - ftemp[3])
                          - (1./6.)*rho*u;

      ftemp[8] = ftemp[6] + (1./2.)*( ftemp[3] - ftemp[1])
                          - (1./6.)*rho*u;

      ftemp += 27;//( sizeof(struct pdf_struct)/8);

    } /* while( ftemp < ftemp_end) */
ftemp_mid = lattice->pdf[subs][lattice->NumNodes-3*lattice->param.LX/8].ftemp;
    while( ftemp < ftemp_mid)
    {
      // North, Inflow
      //u = u_in[((double)rand()/(double)RAND_MAX<.5)?(0):(1)][subs];
      u = u_in[1][subs];
      rho = ( ftemp[0] + ftemp[1] + ftemp[3]
             + 2.*( ftemp[2] + ftemp[5] + ftemp[6]))
           / ( 1. + u);

      ftemp[4] = ftemp[2] - (2./3.)*rho*u;

      ftemp[7] = ftemp[5] + (1./2.)*( ftemp[1] - ftemp[3])
                          - (1./6.)*rho*u;

      ftemp[8] = ftemp[6] + (1./2.)*( ftemp[3] - ftemp[1])
                          - (1./6.)*rho*u;

      ftemp += 27;//( sizeof(struct pdf_struct)/8);

    } /* while( ftemp < ftemp_end) */
    while( ftemp < ftemp_end)
    {
      // North, Inflow
      //u = u_in[((double)rand()/(double)RAND_MAX<.5)?(0):(1)][subs];
      u = 0.;
      rho = ( ftemp[0] + ftemp[1] + ftemp[3]
             + 2.*( ftemp[2] + ftemp[5] + ftemp[6]))
           / ( 1. + u);

      ftemp[4] = ftemp[2] - (2./3.)*rho*u;

      ftemp[7] = ftemp[5] + (1./2.)*( ftemp[1] - ftemp[3])
                          - (1./6.)*rho*u;

      ftemp[8] = ftemp[6] + (1./2.)*( ftemp[3] - ftemp[1])
                          - (1./6.)*rho*u;

      ftemp += 27;//( sizeof(struct pdf_struct)/8);

    } /* while( ftemp < ftemp_end) */
  }
  if( lattice->param.velocity_s_in[subs] )
  {
  }
  if( lattice->param.velocity_n_out[subs])
  {
  }
  if( lattice->param.velocity_s_out[subs])
  {
//printf("bcs() -- velocity_s_out[%d]\n", subs);
    ftemp = lattice->pdf[subs][0].ftemp;
    ftemp_mid = lattice->pdf[subs][lattice->param.LX/8].ftemp;
    ftemp_end = lattice->pdf[subs][lattice->param.LX].ftemp;
    //u = ((subs==1)?(-1):(1))*lattice->param.uy_out;
    while( ftemp < ftemp_mid)
    {
      //u = u_out[((double)rand()/(double)RAND_MAX<.5)?(0):(1)][subs];
      u = 0.;
      rho = ( ftemp[0] + ftemp[1] + ftemp[3]
             + 2.*( ftemp[4] + ftemp[7] + ftemp[8]))
           / ( 1. - u);

      ftemp[2] = ftemp[4] + (2./3.)*rho*u;

      ftemp[5] = ftemp[7] + (1./2.)*( ftemp[3] - ftemp[1])
                          + (1./6.)*rho*u;

      ftemp[6] = ftemp[8] + (1./2.)*( ftemp[1] - ftemp[3])
                          + (1./6.)*rho*u;

      ftemp += 27;//( sizeof(struct pdf_struct)/8);

    } /* while( ftemp < ftemp_end) */
    ftemp_mid = lattice->pdf[subs][lattice->param.LX/4].ftemp;
    while( ftemp < ftemp_mid)
    {
      //u = u_out[((double)rand()/(double)RAND_MAX<.5)?(0):(1)][subs];
      u = u_out[0][subs];
      rho = ( ftemp[0] + ftemp[1] + ftemp[3]
             + 2.*( ftemp[4] + ftemp[7] + ftemp[8]))
           / ( 1. - u);

      ftemp[2] = ftemp[4] + (2./3.)*rho*u;

      ftemp[5] = ftemp[7] + (1./2.)*( ftemp[3] - ftemp[1])
                          + (1./6.)*rho*u;

      ftemp[6] = ftemp[8] + (1./2.)*( ftemp[1] - ftemp[3])
                          + (1./6.)*rho*u;

      ftemp += 27;//( sizeof(struct pdf_struct)/8);

    } /* while( ftemp < ftemp_end) */
    ftemp_mid = lattice->pdf[subs][lattice->param.LX/2].ftemp;
    while( ftemp < ftemp_mid)
    {
      //u = u_out[((double)rand()/(double)RAND_MAX<.5)?(0):(1)][subs];
      u = 0.;
      rho = ( ftemp[0] + ftemp[1] + ftemp[3]
             + 2.*( ftemp[4] + ftemp[7] + ftemp[8]))
           / ( 1. - u);

      ftemp[2] = ftemp[4] + (2./3.)*rho*u;

      ftemp[5] = ftemp[7] + (1./2.)*( ftemp[3] - ftemp[1])
                          + (1./6.)*rho*u;

      ftemp[6] = ftemp[8] + (1./2.)*( ftemp[1] - ftemp[3])
                          + (1./6.)*rho*u;

      ftemp += 27;//( sizeof(struct pdf_struct)/8);

    } /* while( ftemp < ftemp_end) */
    ftemp_mid = lattice->pdf[subs][5*lattice->param.LX/8].ftemp;
    while( ftemp < ftemp_mid)
    {
      //u = u_out[((double)rand()/(double)RAND_MAX<.5)?(0):(1)][subs];
      u = u_out[1][subs];
      rho = ( ftemp[0] + ftemp[1] + ftemp[3]
             + 2.*( ftemp[4] + ftemp[7] + ftemp[8]))
           / ( 1. - u);

      ftemp[2] = ftemp[4] + (2./3.)*rho*u;

      ftemp[5] = ftemp[7] + (1./2.)*( ftemp[3] - ftemp[1])
                          + (1./6.)*rho*u;

      ftemp[6] = ftemp[8] + (1./2.)*( ftemp[1] - ftemp[3])
                          + (1./6.)*rho*u;

      ftemp += 27;//( sizeof(struct pdf_struct)/8);

    } /* while( ftemp < ftemp_end) */
    while( ftemp < ftemp_end)
    {
      //u = u_out[((double)rand()/(double)RAND_MAX<.5)?(0):(1)][subs];
      u = 0.;
      rho = ( ftemp[0] + ftemp[1] + ftemp[3]
             + 2.*( ftemp[4] + ftemp[7] + ftemp[8]))
           / ( 1. - u);

      ftemp[2] = ftemp[4] + (2./3.)*rho*u;

      ftemp[5] = ftemp[7] + (1./2.)*( ftemp[3] - ftemp[1])
                          + (1./6.)*rho*u;

      ftemp[6] = ftemp[8] + (1./2.)*( ftemp[1] - ftemp[3])
                          + (1./6.)*rho*u;

      ftemp += 27;//( sizeof(struct pdf_struct)/8);

    } /* while( ftemp < ftemp_end) */

  } /* if( lattice->param.velocity_s_out[subs]) */
#endif

 } /* for( subs=0; subs<NUM_FLUID_COMPONENTS; subs++) */

} /* void bcs( lattice_ptr lattice) */
// }}}
#else
                                                                          // }}}
// B C S  {{{
//##############################################################################
// void bcs( lattice_ptr lattice)
//
// B C S
//
//  - Apply boundary conditions.
//
void bcs( lattice_ptr lattice)
{
  int    i, j, n, a;
  int    subs;
  int    *bc_type;
  double *ftemp, *ftemp_end, *ftemp_mid;
  double  v;
  double  c0;
  double  D;
  double  c2;

#if RHO0_TEST
//------------------------------------------------------------------[ TEST ]----
  double *rho0;
//------------------------------------------------------------------[ TEST ]----
#endif /* RHO0_TEST */
  double u_x,
         u_y;
  double u_in[2][2],
         u_out[2][2],
         u,
         rho;
  double c;
  int    id;

#if PARALLEL
  id = get_proc_id(lattice);
#else
  id = get_num_procs(lattice)-1;
#endif

  // NOTE: Should previously (in initialization stage) have checked to
  // insure no solid nodes on inflow/outflow boundaries. Do not do it here
  // inside the loop!

 for( subs=0; subs<(NUM_FLUID_COMPONENTS)-(INAMURO_SIGMA_COMPONENT); subs++)
 {
  // P R E S S U R E   N O R T H   I N   B C  {{{
  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  // pressure north inflow
  //  -- Pressure boundary on north side using inflow pressure condition.
  if( (id==get_num_procs(lattice)-1) && lattice->param.pressure_n_in[subs] )
  {
//printf("bcs() %s %d >> pressure_n_in[%d]\n", __FILE__, __LINE__, subs);
    if( lattice->param.pressure_n_in[subs]==2)
    {
      rho = *( pressure_n_in0( lattice, subs)
             + get_time(lattice)%num_pressure_n_in0(lattice,subs));
    }
    else
    {
      rho = lattice->param.rho_in;
    }
    ftemp = lattice->pdf[subs][lattice->NumNodes-lattice->param.LX].ftemp;
    ftemp_end = lattice->pdf[subs][lattice->NumNodes].ftemp;
    while( ftemp < ftemp_end)
    {
      if( bcs_on_solids(lattice) || is_not_solid_node(lattice,0,n))
      {
        // North, Inflow
        if( lattice->param.incompressible)
        {
          u_y = -rho
              + ( ftemp[0] + ftemp[1] + ftemp[3]
                + 2.*( ftemp[2] + ftemp[5] + ftemp[6]));
          c = u_y;
        }
        else // compressible
        {
          u_y = -1.
              + ( ftemp[0] + ftemp[1] + ftemp[3]
                + 2.*( ftemp[2] + ftemp[5] + ftemp[6]))
              / rho;
          c = u_y*rho;
        }

        ftemp[4] = ftemp[2] - (2./3.)*c;

        ftemp[7] = ftemp[5] + (1./2.)*( ftemp[1] - ftemp[3])
                            - (1./6.)*c;

        ftemp[8] = ftemp[6] + (1./2.)*( ftemp[3] - ftemp[1])
                            - (1./6.)*c;

      }

      ftemp += ( sizeof(struct pdf_struct)/sizeof(double));

    } /* while( ftemp < ftemp_end) */

  } /* if( lattice->param.pressure_n_in[subs] ) */
                                                                          // }}}
  // P R E S S U R E   S O U T H   I N   B C  {{{
  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  // pressure south inflow
  //  -- Pressure boundary on south side using inflow pressure condition.
  if( (id==0) && lattice->param.pressure_s_in[subs] )
  {
    if( lattice->param.pressure_s_in[subs]==2)
    {
      rho = *( pressure_s_in0( lattice, subs)
             + get_time(lattice)%num_pressure_s_in0(lattice,subs));
    }
    else
    {
      rho = lattice->param.rho_in;
    }
//printf("bcs() %s %d >> pressure_s_in[%d]\n", __FILE__, __LINE__, subs);
    ftemp = lattice->pdf[subs][0].ftemp;
    ftemp_end = lattice->pdf[subs][lattice->param.LX].ftemp;
    while( ftemp < ftemp_end)
    {
      if( bcs_on_solids(lattice) || is_not_solid_node(lattice,0,n))
      {
        // South, Inflow
        if( lattice->param.incompressible)
        {
          u_y = rho
              - ( ftemp[0] + ftemp[1] + ftemp[3]
                + 2.*( ftemp[4] + ftemp[7] + ftemp[8]));
          c = u_y;
        }
        else // compressible
        {
          u_y = 1.
              - ( ftemp[0] + ftemp[1] + ftemp[3]
                + 2.*( ftemp[4] + ftemp[7] + ftemp[8]))
              / rho;
          c = u_y*rho;
        }

        ftemp[2] = ftemp[4] + (2./3.)*c;

        ftemp[5] = ftemp[7] + (1./2.)*( ftemp[3] - ftemp[1])
                            + (1./6.)*c;

        ftemp[6] = ftemp[8] + (1./2.)*( ftemp[1] - ftemp[3])
                            + (1./6.)*c;
      }

      ftemp += ( sizeof(struct pdf_struct)/8);

    } /* while( ftemp < ftemp_end) */

  } /* if( lattice->param.pressure_s_in[subs] ) */
                                                                          // }}}
  // P R E S S U R E   N O R T H   O U T   B C  {{{
  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  // pressure north outflow
  //  -- Pressure boundary on north side using outflow pressure condition.
  if( (id==get_num_procs(lattice)-1) && lattice->param.pressure_n_out[subs])
  {
//printf("bcs() %s %d >> pressure_n_out[%d]\n", __FILE__, __LINE__, subs);
    ftemp = lattice->pdf[subs][lattice->NumNodes-lattice->param.LX].ftemp;
    ftemp_end = lattice->pdf[subs][lattice->NumNodes].ftemp;
    while( ftemp < ftemp_end)
    {
      if( bcs_on_solids(lattice) || is_not_solid_node(lattice,0,n))
      {
        // North, Inflow
        if( lattice->param.incompressible)
        {
          u_y = -lattice->param.rho_out
              + ( ftemp[0] + ftemp[1] + ftemp[3]
                + 2.*( ftemp[2] + ftemp[5] + ftemp[6]));
          c = u_y;
        }
        else // compressible
        {
          u_y = -1.
              + ( ftemp[0] + ftemp[1] + ftemp[3]
                + 2.*( ftemp[2] + ftemp[5] + ftemp[6]))
              / lattice->param.rho_out;
          c = u_y*lattice->param.rho_out;
        }

        ftemp[4] = ftemp[2] - (2./3.)*c;

        ftemp[7] = ftemp[5] + (1./2.)*( ftemp[1] - ftemp[3])
                            - (1./6.)*c;

        ftemp[8] = ftemp[6] + (1./2.)*( ftemp[3] - ftemp[1])
                            - (1./6.)*c;
      }

      ftemp += ( sizeof(struct pdf_struct)/8);

    } /* while( ftemp < ftemp_end) */

  } /* if( lattice->param.pressure_n_out[subs]) */
                                                                          // }}}
  // P R E S S U R E   S O U T H   O U T   B C  {{{
  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  // pressure south outflow
  //  -- Pressure boundary on south side using outflow pressure condition.
  if( (id==0) && lattice->param.pressure_s_out[subs])
  {
//printf("bcs() %s %d >> pressure_s_out[%d]\n", __FILE__, __LINE__, subs);
    ftemp = lattice->pdf[subs][0].ftemp;
    ftemp_end = lattice->pdf[subs][lattice->param.LX].ftemp;
    while( ftemp < ftemp_end)
    {
      if( bcs_on_solids(lattice) || is_not_solid_node(lattice,0,n))
      {
        // South, Outflow
        if( lattice->param.incompressible)
        {
          u_y = lattice->param.rho_out
              - ( ftemp[0] + ftemp[1] + ftemp[3]
                + 2.*( ftemp[4] + ftemp[7] + ftemp[8]));
          c = u_y;
        }
        else // compressible
        {
          u_y = 1.
              - ( ftemp[0] + ftemp[1] + ftemp[3]
                + 2.*( ftemp[4] + ftemp[7] + ftemp[8]))
              / lattice->param.rho_out;
          c = u_y*lattice->param.rho_out;
        }

        ftemp[2] = ftemp[4] + (2./3.)*c;

        ftemp[5] = ftemp[7] + (1./2.)*( ftemp[3] - ftemp[1])
                            + (1./6.)*c;

        ftemp[6] = ftemp[8] + (1./2.)*( ftemp[1] - ftemp[3])
                            + (1./6.)*c;

      }

      ftemp += ( sizeof(struct pdf_struct)/sizeof(double));

    } /* while( ftemp < ftemp_end) */

  } /* if( pressure_s_out[subs]) */
                                                                          // }}}

  // V E L O C I T Y   N O R T H   I N   B C   {{{
  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  // velocity north inflow
  //  -- Velocity boundary on north side using inflow velocity condition.
  if( (id==get_num_procs(lattice)-1) && lattice->param.velocity_n_in[subs])
  {
//printf("bcs() %s %d >> velocity_n_in[%d]\n", __FILE__, __LINE__, subs);
    ftemp = lattice->pdf[subs][lattice->NumNodes-lattice->param.LX].ftemp;
    ftemp_end = lattice->pdf[subs][lattice->NumNodes].ftemp;
    bc_type = &( lattice->bc[subs][lattice->NumNodes-lattice->param.LX].bc_type);
#if RHO0_TEST
//------------------------------------------------------------------[ TEST ]----
    rho0 =
      &( lattice->macro_vars[subs][lattice->NumNodes-lattice->param.LX].rho);
//------------------------------------------------------------------[ TEST ]----
#endif /* RHO0_TEST */
    //u = ((subs==1)?(-1):(1))*lattice->param.uy_in;
    if( lattice->param.bc_poiseuille)
    {
      i = 0;
    }
    else
    {
      u = lattice->param.uy_in;
    }
    while( ftemp < ftemp_end)
    {
      // North, Inflow
     if( bcs_on_solids(lattice) || is_not_solid_node(lattice,0,n))
     {
      //u = u_in[((double)rand()/(double)RAND_MAX<.5)?(0):(1)][subs];
      //u = 0.;
      if( lattice->param.bc_poiseuille)
      {
        u = ( 1.5*( lattice->param.uy_in)
                 /( .25*(lattice->param.LX-2)*(lattice->param.LX-2)) )
                 *(
                    .25*( lattice->param.LX-2)*( lattice->param.LX-2)
                  -
                    (i-.5*( lattice->param.LX-2)-.5)
                   *(i-.5*( lattice->param.LX-2)-.5)
                  )
              ;
//printf("%s (%d) -- %d %f\n", __FILE__, __LINE__, i, u);
        i++;
      }
      if( lattice->param.incompressible)
      {
        rho = -u + (      ftemp[0] + ftemp[1] + ftemp[3]
                   + 2.*( ftemp[2] + ftemp[5] + ftemp[6]));
        c = u;
      }
      else // compressible
      {
        rho = (      ftemp[0] + ftemp[1] + ftemp[3]
              + 2.*( ftemp[2] + ftemp[5] + ftemp[6]))
              / ( 1. + u);
        c = rho*u;
      }

      ftemp[4] = ftemp[2] - (2./3.)*c;

      ftemp[7] = ftemp[5] + (1./2.)*( ftemp[1] - ftemp[3])
                          - (1./6.)*c;

      ftemp[8] = ftemp[6] + (1./2.)*( ftemp[3] - ftemp[1])
                          - (1./6.)*c;

#if RHO0_TEST
//------------------------------------------------------------------[ TEST ]----
      ftemp[0] = *rho0 - ( ftemp[1]
                         + ftemp[2]
                         + ftemp[3]
                         + ftemp[4]
                         + ftemp[5]
                         + ftemp[6]
                         + ftemp[7]
                         + ftemp[8]);
      rho0 += ( sizeof(struct macro_vars_struct)/8);
//------------------------------------------------------------------[ TEST ]----
#endif /* RHO0_TEST */

      ftemp += ( sizeof(struct pdf_struct)/8);

     }
     else
     {
        if( lattice->param.bc_poiseuille) { i++;}
#if RHO0_TEST
//------------------------------------------------------------------[ TEST ]----
        rho0 += ( sizeof(struct macro_vars_struct)/8);
//------------------------------------------------------------------[ TEST ]----
#endif /* RHO0_TEST */
        ftemp += ( sizeof(struct pdf_struct)/8);
     }

     bc_type++;

    } /* while( ftemp < ftemp_end) */

#if 0
    if( lattice->param.bc_poiseuille)
    {
      // Fix corners
      ftemp = lattice->pdf[subs][lattice->NumNodes-lattice->param.LX].ftemp;
      ftemp[4] = ( ftemp[27+4] + ftemp[-27*lattice->param.LX + 4]) /2;
      ftemp[8] = ( ftemp[27+8] + ftemp[-27*lattice->param.LX + 8]) /2;

      ftemp = lattice->pdf[subs][lattice->NumNodes-1].ftemp;
      ftemp[4] = ( ftemp[-27+4] + ftemp[-27*lattice->param.LX + 4]) /2;
      ftemp[7] = ( ftemp[-27+7] + ftemp[-27*lattice->param.LX + 7]) /2;

    }
#endif


  }
                                                                          // }}}
  // V E L O C I T Y   S O U T H   I N   B C   {{{
  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  // velocity south inflow
  //  -- Velocity boundary on south side using inflow velocity condition.
  if( (id==0) && lattice->param.velocity_s_in[subs] )
  {
//printf("bcs() %s %d >> velocity_s_in[%d]\n", __FILE__, __LINE__, subs);
    ftemp = lattice->pdf[subs][0].ftemp;
    ftemp_end = lattice->pdf[subs][lattice->param.LX].ftemp;
    bc_type = &( lattice->bc[subs][0].bc_type);
#if RHO0_TEST
//------------------------------------------------------------------[ TEST ]----
    rho0 = &( lattice->macro_vars[subs][0].rho);
//------------------------------------------------------------------[ TEST ]----
#endif /* RHO0_TEST */
    //u = ((subs==1)?(-1):(1))*lattice->param.uy_in;
    if( lattice->param.bc_poiseuille)
    {
      i = 0;
    }
    else
    {
      u = lattice->param.uy_in;
    }
    while( ftemp < ftemp_end)
    {
      // South, Inflow
     if( bcs_on_solids(lattice) || is_not_solid_node(lattice,0,n))
     {
      //u = u_in[((double)rand()/(double)RAND_MAX<.5)?(0):(1)][subs];
      //u = 0.;
      if( lattice->param.bc_poiseuille)
      {
        u = ( 1.5*( lattice->param.uy_in)
                 /( .25*(lattice->param.LX-2)*(lattice->param.LX-2)) )
                 *(
                    .25*( lattice->param.LX-2)*( lattice->param.LX-2)
                  -
                    (i-.5*( lattice->param.LX-2)-.5)
                   *(i-.5*( lattice->param.LX-2)-.5)
                  )
              ;
        i++;
      }
      if( lattice->param.incompressible)
      {
        rho = u + (      ftemp[0] + ftemp[1] + ftemp[3]
                  + 2.*( ftemp[4] + ftemp[7] + ftemp[8]));
        c = u;
      }
      else
      {
        rho = ( ftemp[0] + ftemp[1] + ftemp[3]
              + 2.*( ftemp[4] + ftemp[7] + ftemp[8]))
              / ( 1. - u);
        c = rho*u;
      }

      ftemp[2] = ftemp[4] + (2./3.)*c;

      ftemp[5] = ftemp[7] + (1./2.)*( ftemp[3] - ftemp[1])
                          + (1./6.)*c;

      ftemp[6] = ftemp[8] + (1./2.)*( ftemp[1] - ftemp[3])
                          + (1./6.)*c;
#if RHO0_TEST
//------------------------------------------------------------------[ TEST ]----
      ftemp[0] = *rho0 - ( ftemp[1]
                         + ftemp[2]
                         + ftemp[3]
                         + ftemp[4]
                         + ftemp[5]
                         + ftemp[6]
                         + ftemp[7]
                         + ftemp[8]);
      rho0 += ( sizeof(struct macro_vars_struct)/8);
//------------------------------------------------------------------[ TEST ]----
#endif /* RHO0_TEST */

      ftemp += ( sizeof(struct pdf_struct)/8);

     }
     else
     {
        if( lattice->param.bc_poiseuille) { i++;}
#if RHO0_TEST
//------------------------------------------------------------------[ TEST ]----
        rho0 += ( sizeof(struct macro_vars_struct)/8);
//------------------------------------------------------------------[ TEST ]----
#endif /* RHO0_TEST */
        ftemp += ( sizeof(struct pdf_struct)/8);
     }

     bc_type++;

    } /* while( ftemp < ftemp_end) */

#if 0
    if( lattice->param.bc_poiseuille)
    {
      // Fix corners
      ftemp = lattice->pdf[subs][0].ftemp;
      ftemp[2] = ( ftemp[27+2] + ftemp[27*lattice->param.LX + 2]) /2;
      ftemp[5] = ( ftemp[27+5] + ftemp[27*lattice->param.LX + 5]) /2;

      ftemp = lattice->pdf[subs][lattice->param.LX-1].ftemp;
      ftemp[2] = ( ftemp[-27+2] + ftemp[27*lattice->param.LX + 2]) /2;
      ftemp[6] = ( ftemp[-27+6] + ftemp[27*lattice->param.LX + 6]) /2;

    }
#endif

  }
                                                                          // }}}
  // V E L O C I T Y   N O R T H   O U T   B C  {{{
  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  // velocity north outflow
  //  -- Velocity boundary on north side using outflow velocity condition.
  if( (id==get_num_procs(lattice)-1) && lattice->param.velocity_n_out[subs])
  {
//printf("bcs() %s %d >> velocity_n_in[%d]\n", __FILE__, __LINE__, subs);
    ftemp = lattice->pdf[subs][lattice->NumNodes-lattice->param.LX].ftemp;
    ftemp_end = lattice->pdf[subs][lattice->NumNodes].ftemp;
    bc_type = &( lattice->bc[subs][lattice->NumNodes-lattice->param.LX].bc_type);
    i = 0;
#if RHO0_TEST
//------------------------------------------------------------------[ TEST ]----
    rho0 =
      &( lattice->macro_vars[subs][lattice->NumNodes-lattice->param.LX].rho);
//------------------------------------------------------------------[ TEST ]----
#endif /* RHO0_TEST */
    //u = ((subs==1)?(-1):(1))*lattice->param.uy_out;
    if( lattice->param.bc_poiseuille)
    {
      i = 0;
    }
    else
    {
      u = lattice->param.uy_out;
    }
    while( ftemp < ftemp_end)
    {
      // North, Inflow
     if( bcs_on_solids(lattice) || is_not_solid_node(lattice,0,n))
     {
      //u = u_out[((double)rand()/(double)RAND_MAX<.5)?(0):(1)][subs];
      //u = 0.;
      if( lattice->param.bc_poiseuille)
      {
        u = ( 1.5*( lattice->param.uy_out)
                 /( .25*(lattice->param.LX-2)*(lattice->param.LX-2)) )
                 *(
                    .25*( lattice->param.LX-2)*( lattice->param.LX-2)
                  -
                    (i-.5*( lattice->param.LX-2)-.5)
                   *(i-.5*( lattice->param.LX-2)-.5)
                  )
              ;
//printf("%s (%d) -- %d %f\n", __FILE__, __LINE__, i, u);
        i++;
      }
      if( lattice->param.incompressible)
      {
        //rho = -u + (      ftemp[0] + ftemp[1] + ftemp[3]
        //           + 2.*( ftemp[2] + ftemp[5] + ftemp[6]));
        c = u;
      }
      else // compressible
      {
        rho = (      ftemp[0] + ftemp[1] + ftemp[3]
              + 2.*( ftemp[2] + ftemp[5] + ftemp[6]))
              / ( 1. + u);
        c = rho*u;
      }

      ftemp[4] = ftemp[2] - (2./3.)*c;

      ftemp[7] = ftemp[5] + (1./2.)*( ftemp[1] - ftemp[3])
                          - (1./6.)*c;

      ftemp[8] = ftemp[6] + (1./2.)*( ftemp[3] - ftemp[1])
                          - (1./6.)*c;

#if 0
      // Enforce a prescribed pressure by adjusting the resting distribution.
      ftemp[0] = 1.00054
               -
               (
                 ftemp[1] + ftemp[2] + ftemp[3] + ftemp[4]
               + ftemp[5] + ftemp[6] + ftemp[7] + ftemp[8]
               );
#endif

#if RHO0_TEST
//------------------------------------------------------------------[ TEST ]----
      ftemp[0] = *rho0 - ( ftemp[1]
                         + ftemp[2]
                         + ftemp[3]
                         + ftemp[4]
                         + ftemp[5]
                         + ftemp[6]
                         + ftemp[7]
                         + ftemp[8]);
      rho0 += ( sizeof(struct macro_vars_struct)/8);
//------------------------------------------------------------------[ TEST ]----
#endif /* RHO0_TEST */

      ftemp += ( sizeof(struct pdf_struct)/8);

     }
     else
     {
        if( lattice->param.bc_poiseuille) { i++;}
#if RHO0_TEST
//------------------------------------------------------------------[ TEST ]----
        rho0 += ( sizeof(struct macro_vars_struct)/8);
//------------------------------------------------------------------[ TEST ]----
#endif /* RHO0_TEST */
        ftemp += ( sizeof(struct pdf_struct)/8);
     }

     bc_type++;
     if( !( lattice->param.bc_poiseuille)) { i++;}

    } /* while( ftemp < ftemp_end) */

#if 0
    if( lattice->param.bc_poiseuille)
    {
      // Fix corners
      ftemp = lattice->pdf[subs][lattice->NumNodes-lattice->param.LX].ftemp;
      ftemp[4] = ( ftemp[27+4] + ftemp[-27*lattice->param.LX + 4]) /2;
      ftemp[8] = ( ftemp[27+8] + ftemp[-27*lattice->param.LX + 8]) /2;

      ftemp = lattice->pdf[subs][lattice->NumNodes-1].ftemp;
      ftemp[4] = ( ftemp[-27+4] + ftemp[-27*lattice->param.LX + 4]) /2;
      ftemp[7] = ( ftemp[-27+7] + ftemp[-27*lattice->param.LX + 7]) /2;

    }
#endif

  }
                                                                          // }}}
  // V E L O C I T Y   S O U T H   O U T   B C  {{{
  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  // velocity south outflow
  //  -- Velocity boundary on south side using outflow velocity condition.
  if( (id==0) && lattice->param.velocity_s_out[subs])
  {
//printf("bcs() %s %d >> velocity_s_out[%d]\n", __FILE__, __LINE__, subs);
    ftemp = lattice->pdf[subs][0].ftemp;
    ftemp_end = lattice->pdf[subs][lattice->param.LX].ftemp;
    bc_type = &( lattice->bc[subs][0].bc_type);
    i = 0;
#if RHO0_TEST
//------------------------------------------------------------------[ TEST ]----
    rho0 = &( lattice->macro_vars[subs][0].rho);
//------------------------------------------------------------------[ TEST ]----
#endif /* RHO0_TEST */
    //u = ((subs==1)?(-1):(1))*lattice->param.uy_out;
    if( lattice->param.bc_poiseuille)
    {
      i = 0;
    }
    else
    {
      u = lattice->param.uy_out;
    }
    while( ftemp < ftemp_end)
    {
      // South, Outflow
     if( bcs_on_solids(lattice) || is_not_solid_node(lattice,0,n))
     {
      //u = u_out[((double)rand()/(double)RAND_MAX<.5)?(0):(1)][subs];
      //u = 0.;
      if( lattice->param.bc_poiseuille)
      {
        u = ( 1.5*( lattice->param.uy_out)
                 /( .25*(lattice->param.LX-2)*(lattice->param.LX-2)) )
                 *(
                    .25*( lattice->param.LX-2)*( lattice->param.LX-2)
                  -
                    (i-.5*( lattice->param.LX-2)-.5)
                   *(i-.5*( lattice->param.LX-2)-.5)
                  )
              ;
        i++;
      }
      if( lattice->param.incompressible)
      {
      //rho = u + (      ftemp[0] + ftemp[1] + ftemp[3]
      //          + 2.*( ftemp[4] + ftemp[7] + ftemp[8]));
        c = u;
      }
      else
      {
        rho = ( ftemp[0] + ftemp[1] + ftemp[3]
              + 2.*( ftemp[4] + ftemp[7] + ftemp[8]))
              / ( 1. - u);
        c = rho*u;
      }

      ftemp[2] = ftemp[4] + (2./3.)*c;

      ftemp[5] = ftemp[7] + (1./2.)*( ftemp[3] - ftemp[1])
                          + (1./6.)*c;

      ftemp[6] = ftemp[8] + (1./2.)*( ftemp[1] - ftemp[3])
                          + (1./6.)*c;

#if 0
      // Enforce a prescribed pressure by adjusting the resting distribution.
      ftemp[0] = 1.0027
               -
               (
                 ftemp[1] + ftemp[2] + ftemp[3] + ftemp[4]
               + ftemp[5] + ftemp[6] + ftemp[7] + ftemp[8]
               );
#endif

#if RHO0_TEST
//------------------------------------------------------------------[ TEST ]----
      ftemp[0] = *rho0 - ( ftemp[1]
                         + ftemp[2]
                         + ftemp[3]
                         + ftemp[4]
                         + ftemp[5]
                         + ftemp[6]
                         + ftemp[7]
                         + ftemp[8]);
      rho0 += ( sizeof(struct macro_vars_struct)/8);
//------------------------------------------------------------------[ TEST ]----
#endif /* RHO0_TEST */

      ftemp += ( sizeof(struct pdf_struct)/8);

     }
     else
     {
        if( lattice->param.bc_poiseuille) { i++;}
#if RHO0_TEST
//------------------------------------------------------------------[ TEST ]----
        rho0 += ( sizeof(struct macro_vars_struct)/8);
//------------------------------------------------------------------[ TEST ]----
#endif /* RHO0_TEST */
        ftemp += ( sizeof(struct pdf_struct)/8);
     }

     bc_type++;
     if( !( lattice->param.bc_poiseuille)) { i++;}

    } /* while( ftemp < ftemp_end) */

#if 0
    if( lattice->param.bc_poiseuille)
    {
      // Fix corners
      ftemp = lattice->pdf[subs][0].ftemp;
      ftemp[2] = ( ftemp[27+2] + ftemp[27*lattice->param.LX + 2]) /2;
      ftemp[5] = ( ftemp[27+5] + ftemp[27*lattice->param.LX + 5]) /2;

      ftemp = lattice->pdf[subs][lattice->param.LX-1].ftemp;
      ftemp[2] = ( ftemp[-27+2] + ftemp[27*lattice->param.LX + 2]) /2;
      ftemp[6] = ( ftemp[-27+6] + ftemp[27*lattice->param.LX + 6]) /2;

    }
#endif

  } /* if( lattice->param.velocity_s_out[subs]) */
                                                                          // }}}

  // P R E S S U R E   E A S T   I N   B C  {{{
  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  // pressure east inflow
  //  -- Pressure boundary on east side using inflow pressure condition.
  if( lattice->param.pressure_e_in[subs] )
  {
//printf("bcs() %s %d >> pressure_e_in[%d]\n", __FILE__, __LINE__, subs);
    ftemp = lattice->pdf[subs][lattice->param.LX-1].ftemp;
    ftemp_end = lattice->pdf[subs][lattice->NumNodes].ftemp;
    while( ftemp < ftemp_end)
    {
      if( bcs_on_solids(lattice) || is_not_solid_node(lattice,0,n))
      {
        // East, Inflow
        if( lattice->param.incompressible)
        {
          u_x = -lattice->param.rho_in
              + (      ftemp[0] + ftemp[2] + ftemp[4]
                + 2.*( ftemp[1] + ftemp[5] + ftemp[8]));
          c = u_x;
        }
        else // compressible
        {
          u_x = -1.
              + (      ftemp[0] + ftemp[2] + ftemp[4]
                + 2.*( ftemp[1] + ftemp[5] + ftemp[8]))
              / lattice->param.rho_in;
          c = u_x*lattice->param.rho_in;
        }

        ftemp[3] = ftemp[1] - (2./3.)*c;

        ftemp[7] = ftemp[5] + (1./2.)*( ftemp[2] - ftemp[4])
                            - (1./6.)*c;

        ftemp[6] = ftemp[8] + (1./2.)*( ftemp[4] - ftemp[2])
                            - (1./6.)*c;
      }

      ftemp += ( sizeof(struct pdf_struct)/8)*lattice->param.LX;

    } /* while( ftemp < ftemp_end) */

  } /* if( lattice->param.pressure_e_in[subs] ) */
                                                                          // }}}
  // P R E S S U R E   W E S T   I N   B C  {{{
  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  // pressure west inflow
  //  -- Pressure boundary on west side using inflow pressure condition.
  if( lattice->param.pressure_w_in[subs] )
  {
//printf("bcs() %s %d >> pressure_w_in[%d]\n", __FILE__, __LINE__, subs);
    ftemp     = lattice->pdf[subs][0].ftemp;
    ftemp_end =
      lattice->pdf[subs][(lattice->param.LY+1)*lattice->param.LX].ftemp;
    while( ftemp < ftemp_end)
    {
      if( bcs_on_solids(lattice) || is_not_solid_node(lattice,0,n))
      {
        // West, Inflow
        if( lattice->param.incompressible)
        {
          u_x = lattice->param.rho_in
              - (      ftemp[0] + ftemp[2] + ftemp[4]
                + 2.*( ftemp[3] + ftemp[7] + ftemp[6]));
          c = u_x;
        }
        else // compressible
        {
          u_x = 1.
              - (      ftemp[0] + ftemp[2] + ftemp[4]
                + 2.*( ftemp[3] + ftemp[7] + ftemp[6]))
              / lattice->param.rho_in;
          c = u_x*lattice->param.rho_in;
        }

        ftemp[1] = ftemp[3] + (2./3.)*c;

        ftemp[5] = ftemp[7] + (1./2.)*( ftemp[4] - ftemp[2])
                            + (1./6.)*c;

        ftemp[8] = ftemp[6] + (1./2.)*( ftemp[2] - ftemp[4])
                            + (1./6.)*c;
      }

      ftemp += ( sizeof(struct pdf_struct)/8)*lattice->param.LX;

    } /* while( ftemp < ftemp_end) */

  } /* if( lattice->param.pressure_w_in[subs] ) */
                                                                          // }}}
  // P R E S S U R E   E A S T   O U T   B C  {{{
  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  // pressure east outflow
  //  -- Pressure boundary on east side using outflow pressure condition.
  if( lattice->param.pressure_e_out[subs])
  {
#if 1
//printf("bcs() %s %d >> pressure_e_out[%d]\n", __FILE__, __LINE__, subs);
    ftemp = lattice->pdf[subs][lattice->param.LX-1].ftemp;
    ftemp_end = lattice->pdf[subs][lattice->NumNodes].ftemp;
    n = get_LX( lattice) - 1;
    j = 0;
    while( ftemp < ftemp_end)
    {
      if( bcs_on_solids(lattice) || is_not_solid_node(lattice,0,n))
      {
        // East, Outflow
        if( hydrostatic( lattice))
        {
          if( hydrostatic_compressible( lattice))
          {
          //if( get_time( lattice) == 6400)
          //{ // Try to adjust for discrepancy between rho_out and rho_ave.
          //  compute_ave_rho( lattice, &(lattice->param.rho_out), /*subs*/0);
          //}
            if( hydrostatic_compute_rho_ref(lattice))
            {
              // Reference density computed in terms of average density
              lattice->param.rho_out =
                 ( 3.*lattice->param.gval[0][1]
#if INAMURO_SIGMA_COMPONENT
                      *( 1. + (get_buoyancy(lattice))
                             *(get_beta(lattice))
                             *(get_C_out(lattice)-get_C0(lattice)) )
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
            }
            rho =
#if 0
                  lattice->param.rho_out
                 *exp( -3.*lattice->param.gval[0][1]
#if INAMURO_SIGMA_COMPONENT
                    *( 1. + (get_buoyancy(lattice))
                           *(get_beta(lattice))
                           *(get_C_out(lattice)-get_C0(lattice)) )
#endif
                          *( (get_LY(lattice)-1.)-0.))
                 *exp(  3.*lattice->param.gval[0][1]
#if INAMURO_SIGMA_COMPONENT
                    *( 1. + (get_buoyancy(lattice))
                           *(get_beta(lattice))
                           *(get_C_out(lattice)-get_C0(lattice)) )
#endif
                          *(j+1.0));
#else
              3.*lattice->param.gval[0][1]
#if INAMURO_SIGMA_COMPONENT
                    *( 1. + (get_buoyancy(lattice))
                           *(get_beta(lattice))
                           *(get_C_out(lattice)-get_C0(lattice)) )
#endif
                *(get_LY(lattice)-2)
                *lattice->param.rho_A[0]
                *exp( -3.*lattice->param.gval[0][1]
                         *( ( get_LY(lattice)-2.) - (j-.5)) )
              /
              ( 1. - exp( -3.*lattice->param.gval[0][1]
#if INAMURO_SIGMA_COMPONENT
                    *( 1. + (get_buoyancy(lattice))
                           *(get_beta(lattice))
                           *(get_C_out(lattice)-get_C0(lattice)) )
#endif
                          *(get_LY(lattice)-2)));
#endif
          }
          else
          {
            rho = lattice->param.rho_out
            * ( 1. - 3.*lattice->param.gval[0][1]
#if INAMURO_SIGMA_COMPONENT
                    *( 1. + (get_buoyancy(lattice))
                           *(get_beta(lattice))
                           *(get_C_out(lattice)-get_C0(lattice)) )
#endif
                    *( ( get_LY(lattice)
                       + ((get_LY(lattice)%2)?(-1.):(1.)))/2.
                     - j ) );
          }
        }
        else
        {
          rho = lattice->param.rho_out;
        }

        if( lattice->param.incompressible)
        {
          u_x = -rho + (      ftemp[0] + ftemp[2] + ftemp[4]
                       + 2.*( ftemp[1] + ftemp[5] + ftemp[8]) );
          c = u_x;
        }
        else // compressible
        {
          u_x = -1.  + (      ftemp[0] + ftemp[2] + ftemp[4]
                       + 2.*( ftemp[1] + ftemp[5] + ftemp[8]) ) / rho;
          c = u_x*rho;
        }

        if( j==1)
        {
          //c = 0.;
        }

        ftemp[3] = ftemp[1] - (2./3.)*c;

        ftemp[7] = ftemp[5] + (1./2.)*( ftemp[2] - ftemp[4])
#if 1 // Body force term
                            - (1./2.)*(-1./2.)
                             *lattice->param.gval[subs][1]*rho
#if NUM_FLUID_COMPONENTS==2 && INAMURO_SIGMA_COMPONENT==1
                    *( 1. + (get_buoyancy(lattice))
                           *(get_beta(lattice))
                           *(get_C_out(lattice)-get_C0(lattice)) )
#endif
#endif
                            - (1./6.)*c;

        ftemp[6] = ftemp[8] + (1./2.)*( ftemp[4] - ftemp[2])
#if 1 // Body force term
                            + (1./2.)*(-1./2.)
                             *lattice->param.gval[subs][1]*rho
#if NUM_FLUID_COMPONENTS==2 && INAMURO_SIGMA_COMPONENT==1
                    *( 1. + (get_buoyancy(lattice))
                           *(get_beta(lattice))
                           *(get_C_out(lattice)-get_C0(lattice)) )
#endif
#endif
                            - (1./6.)*c;

        if( j==1)
        {
        }

    } /* if( is_not_solid_node( lattice, subs, n)) */

      ftemp += ( sizeof(struct pdf_struct)/8)*lattice->param.LX;
      j++;
      n += get_LX( lattice);

    } /* while( ftemp < ftemp_end) */

#else
    // Old version for comparison.
    ftemp = lattice->pdf[subs][lattice->param.LX-1].ftemp;
    ftemp_end = lattice->pdf[subs][lattice->NumNodes].ftemp;
    while( ftemp < ftemp_end)
    {
      // East, Outflow
      if( lattice->param.incompressible)
      {
        u_x = -lattice->param.rho_out
            + (      ftemp[0] + ftemp[2] + ftemp[4]
              + 2.*( ftemp[1] + ftemp[5] + ftemp[8]));
        c = u_x;
      }
      else // compressible
      {
        u_x = -1.
            + (      ftemp[0] + ftemp[2] + ftemp[4]
              + 2.*( ftemp[1] + ftemp[5] + ftemp[8]))
            / lattice->param.rho_out;
        c = u_x*lattice->param.rho_out;
      }

      ftemp[3] = ftemp[1] - (2./3.)*c;

      ftemp[7] = ftemp[5] + (1./2.)*( ftemp[2] - ftemp[4])
                          - (1./6.)*c;

      ftemp[6] = ftemp[8] + (1./2.)*( ftemp[4] - ftemp[2])
                          - (1./6.)*c;

      ftemp += ( sizeof(struct pdf_struct)/8)*lattice->param.LX;

    } /* while( ftemp < ftemp_end) */


#endif
  } /* if( lattice->param.pressure_e_out[subs]) */
                                                                          // }}}
  // P R E S S U R E   W E S T   O U T   B C  {{{
  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  // pressure west outflow
  //  -- Pressure boundary on west side using outflow pressure condition.
  if( lattice->param.pressure_w_out[subs])
  {
//printf("bcs() %s %d >> pressure_w_out[%d]\n", __FILE__, __LINE__, subs);
    ftemp     = lattice->pdf[subs][0].ftemp;
    ftemp_end =
      lattice->pdf[subs][(lattice->param.LY+1)*lattice->param.LX].ftemp;
    j = 0;
    n = 0;
    while( ftemp < ftemp_end)
    {
      // West, Outflow
      if( bcs_on_solids(lattice) || is_not_solid_node(lattice,0,n))
      {
        if( hydrostatic_west( lattice))
        {
          if( hydrostatic_compressible( lattice))
          {
          //if( get_time( lattice) == 6400)
          //{ // Try to adjust for discrepancy between rho_out and rho_ave.
          //  compute_ave_rho( lattice, &(lattice->param.rho_out), /*subs*/0);
          //}
            if( hydrostatic_compute_rho_ref(lattice))
            {
              // Reference density computed in terms of average density
              lattice->param.rho_out =
                 ( 3.*lattice->param.gval[0][1]
#if INAMURO_SIGMA_COMPONENT
                      *( 1. + (get_buoyancy(lattice))
                             *(get_beta(lattice))
                             *(get_C_out(lattice)-get_C0(lattice)) )
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
              //printf("rho_ref = %20.17f\n", lattice->param.rho_out);
            }
            rho =
#if 0
                  lattice->param.rho_out
                 *exp( -3.*lattice->param.gval[0][1]
#if INAMURO_SIGMA_COMPONENT
                    //*(1.+(get_buoyancy(lattice))*lattice->param.C_out)
                    *( 1. + (get_buoyancy(lattice))
                           *(get_beta(lattice))
                           *(get_C_out(lattice)-get_C0(lattice)) )
#endif
                          //*(0.5*(get_LY(lattice)-1.)-1.))
                          *( (get_LY(lattice)-1.)-0.))
                 *exp(  3.*lattice->param.gval[0][1]
#if INAMURO_SIGMA_COMPONENT
                    //*(1.+(get_buoyancy(lattice))*lattice->param.C_out)
                    *( 1. + (get_buoyancy(lattice))
                           *(get_beta(lattice))
                           *(get_C_out(lattice)-get_C0(lattice)) )
#endif
                          *(j+1.0));
               //*exp( -3.*lattice->param.gval[0][1]
               //         *(0.5*(get_LY(lattice)-1)-1))
               //*exp(  3.*lattice->param.gval[0][1]
               //         *(get_LY(lattice)+.5-j));
#else
              3.*lattice->param.gval[0][1]
#if INAMURO_SIGMA_COMPONENT
                    //*(1.+(get_buoyancy(lattice))*lattice->param.C_out)
                    *( 1. + (get_buoyancy(lattice))
                           *(get_beta(lattice))
                           *(get_C_out(lattice)-get_C0(lattice)) )
#endif
                *(get_LY(lattice)-2)
                *lattice->param.rho_A[0]
                *exp( -3.*lattice->param.gval[0][1]
                         *( ( get_LY(lattice)-2.) - (j-.5)) )
              /
              ( 1. - exp( -3.*lattice->param.gval[0][1]
#if INAMURO_SIGMA_COMPONENT
                    //*(1.+(get_buoyancy(lattice))*lattice->param.C_out)
                    *( 1. + (get_buoyancy(lattice))
                           *(get_beta(lattice))
                           *(get_C_out(lattice)-get_C0(lattice)) )
#endif
                          *(get_LY(lattice)-2)));
#endif
          }
          else
          {
          rho = lattice->param.rho_out
              * ( 1. - 3.*lattice->param.gval[0][1]
#if INAMURO_SIGMA_COMPONENT
                    //*(1.+(get_buoyancy(lattice))*lattice->param.C_out)
                    *( 1. + (get_buoyancy(lattice))
                           *(get_beta(lattice))
                           *(get_C_out(lattice)-get_C0(lattice)) )
#endif
                    *( ( get_LY(lattice)
                       + ((get_LY(lattice)%2)?(-1.):(1.)))/2.
                     - j ) );
          }
        }
        else
        {
          rho = lattice->param.rho_out;
        }

        if( lattice->param.incompressible)
        {
          u_x = rho - (      ftemp[0] + ftemp[2] + ftemp[4]
                      + 2.*( ftemp[3] + ftemp[7] + ftemp[6]) );
          c = u_x;
        }
        else // compressible
        {
          u_x = 1.  - (      ftemp[0] + ftemp[2] + ftemp[4]
                      + 2.*( ftemp[3] + ftemp[7] + ftemp[6]) ) / rho;
          c = u_x*rho;
        }

        ftemp[1] = ftemp[3] + (2./3.)*c;

        ftemp[5] = ftemp[7] + (1./2.)*( ftemp[4] - ftemp[2])
#if 1 // Body force term
                            + (1./2.)*(-1./2.)
                             *lattice->param.gval[subs][1]*rho
#if NUM_FLUID_COMPONENTS==2 && INAMURO_SIGMA_COMPONENT==1
     //*(1.+(get_buoyancy(lattice))*(lattice->macro_vars[/*subs*/1][n].rho))
     //*(1.+(get_buoyancy(lattice))*(lattice->param.C_out))
                    *( 1. + (get_buoyancy(lattice))
                           *(get_beta(lattice))
                           *(get_C_out(lattice)-get_C0(lattice)) )
#endif
#endif
                            + (1./6.)*c;

        ftemp[8] = ftemp[6] + (1./2.)*( ftemp[2] - ftemp[4])
#if 1 // Body force term
                            - (1./2.)*(-1./2.)
                             *lattice->param.gval[subs][1]*rho
#if NUM_FLUID_COMPONENTS==2 && INAMURO_SIGMA_COMPONENT==1
     //*(1.+(get_buoyancy(lattice))*(lattice->macro_vars[/*subs*/1][n].rho))
     //*(1.+(get_buoyancy(lattice))*(lattice->param.C_out))
                    *( 1. + (get_buoyancy(lattice))
                           *(get_beta(lattice))
                           *(get_C_out(lattice)-get_C0(lattice)) )
#endif
#endif
                            + (1./6.)*c;

        if( j==1)
        {
        }

          if( 0)//j==1) // Debug output {{{
          {
            if( lattice->time==1)
            {
              printf("BCSW\n");
              printf("BCSW time ftemp[0] ftemp[2] ftemp[4]"
                            " ftemp[3] ftemp[7] ftemp[6] --> u_x"
                            " ftemp[1] ftemp[5] ftemp[8] \n");
              printf("BCSW ---- -------- -------- --------"
                            " -------- -------- --------     ---"
                            " -------- -------- --------\n");
            }
            printf("BCSW %4d %f %f %f %f %f %f --> %f %f %f %f\n",
              lattice->time,
              ftemp[0], ftemp[2], ftemp[4],
              ftemp[3], ftemp[7], ftemp[6], u_x,
              ftemp[1], ftemp[5], ftemp[8] );

          } /* if( j==1) }}} */

      } /* if( is_not_solid_node( lattice, subs, n)) */

      ftemp += ( sizeof(struct pdf_struct)/8)*lattice->param.LX;
      n+=get_LX( lattice);
      j++;

    } /* while( ftemp < ftemp_end) */

  } /* if( pressure_w_out[subs]) */
                                                                          // }}}

  // V E L O C I T Y   E A S T   I N   B C  {{{
  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  // velocity east inflow
  //  -- Velocity boundary on east side using inflow velocity condition.
  if( lattice->param.velocity_e_in[subs])
  {
//printf("bcs() %s %d >> velocity_e_in[%d]\n", __FILE__, __LINE__, subs);
    ftemp = lattice->pdf[subs][lattice->param.LX-1].ftemp;
    ftemp_end = lattice->pdf[subs][lattice->NumNodes].ftemp;
    bc_type = &( lattice->bc[subs][lattice->param.LX-1].bc_type);
#if RHO0_TEST
//------------------------------------------------------------------[ TEST ]----
    rho0 =
      &( lattice->macro_vars[subs][lattice->param.LX-1].rho);
//------------------------------------------------------------------[ TEST ]----
#endif /* RHO0_TEST */
    //u = ((subs==1)?(-1):(1))*lattice->param.ux_in;
    if( lattice->param.bc_poiseuille)
    {
      i = 0;
    }
    else
    {
      u = lattice->param.ux_in;
    }
    while( ftemp < ftemp_end)
    {
      // East, Inflow
     if( bcs_on_solids(lattice) || is_not_solid_node(lattice,0,n))
     {
      //u = u_in[((double)rand()/(double)RAND_MAX<.5)?(0):(1)][subs];
      //u = 0.;
      if( lattice->param.bc_poiseuille)
      {
        u = ( 1.5*( lattice->param.ux_in)
                 /( .25*(lattice->param.LY-2)*(lattice->param.LY-2)) )
                 *(
                    .25*( lattice->param.LY-2)*( lattice->param.LY-2)
                  -
                    (i-.5*( lattice->param.LY-2)-.5)
                   *(i-.5*( lattice->param.LY-2)-.5)
                  )
              ;
//printf("%s (%d) -- %d %f\n", __FILE__, __LINE__, i, u);
        i++;
      }
      if( lattice->param.incompressible)
      {
        rho = -u + (      ftemp[0] + ftemp[2] + ftemp[4]
                   + 2.*( ftemp[1] + ftemp[5] + ftemp[8]));
        c = u;
      }
      else // compressible
      {
        rho = (      ftemp[0] + ftemp[2] + ftemp[4]
              + 2.*( ftemp[1] + ftemp[5] + ftemp[8]))
              / ( 1. + u);
        c = rho*u;
      }

      ftemp[3] = ftemp[1] - (2./3.)*c;

      ftemp[7] = ftemp[5] + (1./2.)*( ftemp[2] - ftemp[4])
                          - (1./6.)*c;

      ftemp[6] = ftemp[8] + (1./2.)*( ftemp[4] - ftemp[2])
                          - (1./6.)*c;

#if RHO0_TEST
//------------------------------------------------------------------[ TEST ]----
      ftemp[0] = *rho0 - ( ftemp[1]
                         + ftemp[2]
                         + ftemp[3]
                         + ftemp[4]
                         + ftemp[5]
                         + ftemp[6]
                         + ftemp[7]
                         + ftemp[8]);
      rho0 += ( sizeof(struct macro_vars_struct)/8)*lattice->param.LX;
//------------------------------------------------------------------[ TEST ]----
#endif /* RHO0_TEST */

      ftemp += ( sizeof(struct pdf_struct)/8)*lattice->param.LX;

     }
     else
     {
        if( lattice->param.bc_poiseuille) { i++;}
#if RHO0_TEST
//------------------------------------------------------------------[ TEST ]----
        rho0 += ( sizeof(struct macro_vars_struct)/8)*lattice->param.LX;
//------------------------------------------------------------------[ TEST ]----
#endif /* RHO0_TEST */
        ftemp += ( sizeof(struct pdf_struct)/8)*lattice->param.LX;
     }

     bc_type+=lattice->param.LX;

    } /* while( ftemp < ftemp_end) */

#if 0
    if( lattice->param.bc_poiseuille)
    {
      // Fix corners
      ftemp = lattice->pdf[subs][lattice->NumNodes-lattice->param.LX].ftemp;
      ftemp[4] = ( ftemp[27+4] + ftemp[-27*lattice->param.LX + 4]) /2;
      ftemp[8] = ( ftemp[27+8] + ftemp[-27*lattice->param.LX + 8]) /2;

      ftemp = lattice->pdf[subs][lattice->NumNodes-1].ftemp;
      ftemp[4] = ( ftemp[-27+4] + ftemp[-27*lattice->param.LX + 4]) /2;
      ftemp[7] = ( ftemp[-27+7] + ftemp[-27*lattice->param.LX + 7]) /2;

    }
#endif

  } /* if( lattice->param.velocity_e_in[subs]) */
                                                                          // }}}
  // V E L O C I T Y   W E S T   I N   B C  {{{
  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  // velocity west inflow
  //  -- Velocity boundary on west side using inflow velocity condition.
  if( lattice->param.velocity_w_in[subs] )
  {
//printf("bcs() %s %d >> velocity_w_in[%d]\n", __FILE__, __LINE__, subs);
    ftemp     = lattice->pdf[subs][0].ftemp;
    ftemp_end =
      lattice->pdf[subs][(lattice->param.LY+1)*lattice->param.LX].ftemp;
    bc_type = &( lattice->bc[subs][0].bc_type);
    j = 0;
    n = 0;
#if RHO0_TEST
//------------------------------------------------------------------[ TEST ]----
    rho0 = &( lattice->macro_vars[subs][0].rho);
//------------------------------------------------------------------[ TEST ]----
#endif /* RHO0_TEST */
    //u = ((subs==1)?(-1):(1))*lattice->param.ux_in;
    if( lattice->param.bc_poiseuille)
    {
      i = 0;
    }
    else
    {
      u = lattice->param.ux_in;
    }
    while( ftemp < ftemp_end)
    {
      // West, Inflow
     if( bcs_on_solids(lattice) || is_not_solid_node(lattice,0,n))
     {
      //u = u_in[((double)rand()/(double)RAND_MAX<.5)?(0):(1)][subs];
      //u = 0.;
      if( lattice->param.bc_poiseuille)
      {
        u = ( 1.5*( lattice->param.ux_in)
                 /( .25*( lattice->param.LY-2)*( lattice->param.LY-2)) )
                 *(
                    .25*( lattice->param.LY-2)*( lattice->param.LY-2)
                  -
                    (i-.5*( lattice->param.LY-2)-.5)
                   *(i-.5*( lattice->param.LY-2)-.5)
                  )
              ;
        i++;
      }
      if( lattice->param.incompressible)
      {
        rho = u + (      ftemp[0] + ftemp[2] + ftemp[4]
                  + 2.*( ftemp[3] + ftemp[7] + ftemp[6]));
        c = u;
      }
      else
      {
        rho = (      ftemp[0] + ftemp[2] + ftemp[4]
              + 2.*( ftemp[3] + ftemp[7] + ftemp[6]))
              / ( 1. - u);
        c = rho*u;
      }

      ftemp[1] = ftemp[3] + (2./3.)*c;

      ftemp[5] = ftemp[7] + (1./2.)*( ftemp[4] - ftemp[2])
                          + (1./2.)*(-1./2.)*lattice->param.gval[subs][1]
                                            *rho
#if NUM_FLUID_COMPONENTS==2 && INAMURO_SIGMA_COMPONENT==1
//*(1.+(get_buoyancy(lattice))*(lattice->macro_vars[/*subs*/1][n].rho))
                    *( 1. + (get_buoyancy(lattice))
                           *(get_beta(lattice))
                           //*(get_rhon(lattice,n,/*subs*/1)-get_C0(lattice)) )
                           *(get_C_in(lattice)-get_C0(lattice)) )
#endif
                          + (1./6.)*c;

      ftemp[8] = ftemp[6] + (1./2.)*( ftemp[2] - ftemp[4])
                          - (1./2.)*(-1./2.)*lattice->param.gval[subs][1]
                                            *rho
#if NUM_FLUID_COMPONENTS==2 && INAMURO_SIGMA_COMPONENT==1
//*(1.+(get_buoyancy(lattice))*(lattice->macro_vars[/*subs*/1][n].rho))
                    *( 1. + (get_buoyancy(lattice))
                           *(get_beta(lattice))
                           //*(get_rhon(lattice,n,/*subs*/1)-get_C0(lattice)) )
                           *(get_C_in(lattice)-get_C0(lattice)) )
#endif
                          + (1./6.)*c;
#if RHO0_TEST
//------------------------------------------------------------------[ TEST ]----
      ftemp[0] = *rho0 - ( ftemp[1]
                         + ftemp[2]
                         + ftemp[3]
                         + ftemp[4]
                         + ftemp[5]
                         + ftemp[6]
                         + ftemp[7]
                         + ftemp[8]);
      rho0 += ( sizeof(struct macro_vars_struct)/8)*lattice->param.LX;
//------------------------------------------------------------------[ TEST ]----
#endif /* RHO0_TEST */

      ftemp += ( sizeof(struct pdf_struct)/8)*lattice->param.LX;

     }
     else
     {
        if( lattice->param.bc_poiseuille) { i++;}
#if RHO0_TEST
//------------------------------------------------------------------[ TEST ]----
        rho0 += ( sizeof(struct macro_vars_struct)/8)*lattice->param.LX;
//------------------------------------------------------------------[ TEST ]----
#endif /* RHO0_TEST */
        ftemp += ( sizeof(struct pdf_struct)/8)*lattice->param.LX;
     }

     bc_type++;
     j++;
     n+=get_LX(lattice);

    } /* while( ftemp < ftemp_end) */

#if 0
    if( lattice->param.bc_poiseuille)
    {
      // Fix corners
      ftemp = lattice->pdf[subs][0].ftemp;
      ftemp[2] = ( ftemp[27+2] + ftemp[27*lattice->param.LX + 2]) /2;
      ftemp[5] = ( ftemp[27+5] + ftemp[27*lattice->param.LX + 5]) /2;

      ftemp = lattice->pdf[subs][lattice->param.LX-1].ftemp;
      ftemp[2] = ( ftemp[-27+2] + ftemp[27*lattice->param.LX + 2]) /2;
      ftemp[6] = ( ftemp[-27+6] + ftemp[27*lattice->param.LX + 6]) /2;

    }
#endif

  }
                                                                          // }}}
  // V E L O C I T Y   E A S T   O U T   B C  {{{
  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  // velocity east outflow
  //  -- Velocity boundary on east side using outflow velocity condition.
  if( lattice->param.velocity_e_out[subs])
  {
//printf("bcs() %s %d >> velocity_e_in[%d]\n", __FILE__, __LINE__, subs);
    ftemp = lattice->pdf[subs][lattice->param.LX-1].ftemp;
    ftemp_end = lattice->pdf[subs][lattice->NumNodes].ftemp;
    bc_type = &( lattice->bc[subs][lattice->param.LX-1].bc_type);
#if RHO0_TEST
//------------------------------------------------------------------[ TEST ]----
    rho0 =
      &( lattice->macro_vars[subs][lattice->param.LX-1].rho);
//------------------------------------------------------------------[ TEST ]----
#endif /* RHO0_TEST */
    //u = ((subs==1)?(-1):(1))*lattice->param.ux_out;
    if( lattice->param.bc_poiseuille)
    {
      i = 0;
    }
    else
    {
      u = lattice->param.ux_out;
    }
    while( ftemp < ftemp_end)
    {
      // East, Outflow
     if( bcs_on_solids(lattice) || is_not_solid_node(lattice,0,n))
     {
      //u = u_out[((double)rand()/(double)RAND_MAX<.5)?(0):(1)][subs];
      //u = 0.;
      if( lattice->param.bc_poiseuille)
      {
        u = ( 1.5*( lattice->param.ux_out)
                 /( .25*(lattice->param.LY-2)*(lattice->param.LY-2)) )
                 *(
                    .25*( lattice->param.LY-2)*( lattice->param.LY-2)
                  -
                    (i-.5*( lattice->param.LY-2)-.5)
                   *(i-.5*( lattice->param.LY-2)-.5)
                  )
              ;
//printf("%s (%d) -- %d %f\n", __FILE__, __LINE__, i, u);
        i++;
      }
      if( lattice->param.incompressible)
      {
        rho = -u + (      ftemp[0] + ftemp[2] + ftemp[4]
                   + 2.*( ftemp[1] + ftemp[5] + ftemp[8]));
        c = u;
      }
      else // compressible
      {
        rho = (      ftemp[0] + ftemp[2] + ftemp[4]
              + 2.*( ftemp[1] + ftemp[5] + ftemp[8]))
              / ( 1. + u);
        c = rho*u;
      }

      ftemp[3] = ftemp[1] - (2./3.)*c;

      ftemp[7] = ftemp[5] + (1./2.)*( ftemp[2] - ftemp[4])
                          - (1./2.)*(-1./2.)*lattice->param.gval[subs][1]
                                            *rho
#if NUM_FLUID_COMPONENTS==2 && INAMURO_SIGMA_COMPONENT==1
//*(1.+(get_buoyancy(lattice))*(lattice->macro_vars[/*subs*/1][n].rho))
                    *( 1. + (get_buoyancy(lattice))
                           *(get_beta(lattice))
                           //*(get_rhon(lattice,n,/*subs*/1)-get_C0(lattice)) )
                           *(get_C_out(lattice)-get_C0(lattice)) )
#endif
                          - (1./6.)*c;

      ftemp[6] = ftemp[8] + (1./2.)*( ftemp[4] - ftemp[2])
                          - (1./2.)*(-1./2.)*lattice->param.gval[subs][1]
                                            *rho
#if NUM_FLUID_COMPONENTS==2 && INAMURO_SIGMA_COMPONENT==1
//*(1.+(get_buoyancy(lattice))*(lattice->macro_vars[/*subs*/1][n].rho))
                    *( 1. + (get_buoyancy(lattice))
                           *(get_beta(lattice))
                           //*(get_rhon(lattice,n,/*subs*/1)-get_C0(lattice)) )
                           *(get_C_out(lattice)-get_C0(lattice)) )
#endif
                          - (1./6.)*c;

#if RHO0_TEST
//------------------------------------------------------------------[ TEST ]----
      ftemp[0] = *rho0 - ( ftemp[1]
                         + ftemp[2]
                         + ftemp[3]
                         + ftemp[4]
                         + ftemp[5]
                         + ftemp[6]
                         + ftemp[7]
                         + ftemp[8]);
      rho0 += ( sizeof(struct macro_vars_struct)/8)*lattice->param.LX;
//------------------------------------------------------------------[ TEST ]----
#endif /* RHO0_TEST */

      ftemp += ( sizeof(struct pdf_struct)/8)*lattice->param.LX;

     }
     else
     {
        if( lattice->param.bc_poiseuille) { i++;}
#if RHO0_TEST
//------------------------------------------------------------------[ TEST ]----
        rho0 += ( sizeof(struct macro_vars_struct)/8)*lattice->param.LX;
//------------------------------------------------------------------[ TEST ]----
#endif /* RHO0_TEST */
        ftemp += ( sizeof(struct pdf_struct)/8)*lattice->param.LX;
     }

     bc_type++;

    } /* while( ftemp < ftemp_end) */

#if 0
    if( lattice->param.bc_poiseuille)
    {
      // Fix corners
      ftemp = lattice->pdf[subs][lattice->NumNodes-lattice->param.LX].ftemp;
      ftemp[4] = ( ftemp[27+4] + ftemp[-27*lattice->param.LX + 4]) /2;
      ftemp[8] = ( ftemp[27+8] + ftemp[-27*lattice->param.LX + 8]) /2;

      ftemp = lattice->pdf[subs][lattice->NumNodes-1].ftemp;
      ftemp[4] = ( ftemp[-27+4] + ftemp[-27*lattice->param.LX + 4]) /2;
      ftemp[7] = ( ftemp[-27+7] + ftemp[-27*lattice->param.LX + 7]) /2;

    }
#endif

  } /* if( lattice->param.velocity_e_out[subs]) */
                                                                          // }}}
  // V E L O C I T Y   W E S T   O U T   B C  {{{
  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  // velocity west outflow
  //  -- Velocity boundary on west side using outflow velocity condition.
  if( lattice->param.velocity_w_out[subs])
  {
//printf("bcs() %s %d >> velocity_w_out[%d]\n", __FILE__, __LINE__, subs);
    ftemp     = lattice->pdf[subs][0].ftemp;
    ftemp_end =
      lattice->pdf[subs][(lattice->param.LY+1)*lattice->param.LX].ftemp;
    bc_type = &( lattice->bc[subs][0].bc_type);
#if RHO0_TEST
//------------------------------------------------------------------[ TEST ]----
    rho0 = &( lattice->macro_vars[subs][0].rho);
//------------------------------------------------------------------[ TEST ]----
#endif /* RHO0_TEST */
    //u = ((subs==1)?(-1):(1))*lattice->param.ux_out;
    if( lattice->param.bc_poiseuille)
    {
      i = 0;
    }
    else
    {
      u = lattice->param.ux_out;
    }
    while( ftemp < ftemp_end)
    {
      // West, Outflow
     if( bcs_on_solids(lattice) || is_not_solid_node(lattice,0,n))
     {
      //u = u_out[((double)rand()/(double)RAND_MAX<.5)?(0):(1)][subs];
      //u = 0.;
      if( lattice->param.bc_poiseuille)
      {
        u = ( 1.5*( lattice->param.ux_out)
                 /( .25*(lattice->param.LY-2)*(lattice->param.LY-2)) )
                 *(
                    .25*( lattice->param.LY-2)*( lattice->param.LY-2)
                  -
                    (i-.5*( lattice->param.LY-2)-.5)
                   *(i-.5*( lattice->param.LY-2)-.5)
                  )
              ;
        i++;
      }
      if( lattice->param.incompressible)
      {
        rho = u + (      ftemp[0] + ftemp[2] + ftemp[4]
                  + 2.*( ftemp[3] + ftemp[7] + ftemp[6]));
        c = u;
      }
      else
      {
        rho = (      ftemp[0] + ftemp[2] + ftemp[4]
              + 2.*( ftemp[3] + ftemp[7] + ftemp[6]))
              / ( 1. - u);
        c = rho*u;
      }

      ftemp[1] = ftemp[3] + (2./3.)*c;

      ftemp[5] = ftemp[7] + (1./2.)*( ftemp[4] - ftemp[2])
                          + (1./6.)*c;

      ftemp[8] = ftemp[6] + (1./2.)*( ftemp[2] - ftemp[4])
                          + (1./6.)*c;
#if RHO0_TEST
//------------------------------------------------------------------[ TEST ]----
      ftemp[0] = *rho0 - ( ftemp[1]
                         + ftemp[2]
                         + ftemp[3]
                         + ftemp[4]
                         + ftemp[5]
                         + ftemp[6]
                         + ftemp[7]
                         + ftemp[8]);
      rho0 += ( sizeof(struct macro_vars_struct)/8)*lattice->param.LX;
//------------------------------------------------------------------[ TEST ]----
#endif /* RHO0_TEST */

      ftemp += ( sizeof(struct pdf_struct)/8)*lattice->param.LX;

     }
     else
     {
        if( lattice->param.bc_poiseuille) { i++;}
#if RHO0_TEST
//------------------------------------------------------------------[ TEST ]----
        rho0 += ( sizeof(struct macro_vars_struct)/8)*lattice->param.LX;
//------------------------------------------------------------------[ TEST ]----
#endif /* RHO0_TEST */
        ftemp += ( sizeof(struct pdf_struct)/8)*lattice->param.LX;
     }

     bc_type++;

    } /* while( ftemp < ftemp_end) */

#if 0
    if( lattice->param.bc_poiseuille)
    {
      // Fix corners
      ftemp = lattice->pdf[subs][0].ftemp;
      ftemp[2] = ( ftemp[27+2] + ftemp[27*lattice->param.LX + 2]) /2;
      ftemp[5] = ( ftemp[27+5] + ftemp[27*lattice->param.LX + 5]) /2;

      ftemp = lattice->pdf[subs][lattice->param.LX-1].ftemp;
      ftemp[2] = ( ftemp[-27+2] + ftemp[27*lattice->param.LX + 2]) /2;
      ftemp[6] = ( ftemp[-27+6] + ftemp[27*lattice->param.LX + 6]) /2;

    }
#endif

  } /* if( lattice->param.velocity_w_out[subs]) */
                                                                          // }}}
#if 0
  // C O R N E R S
  //############################################################################
  // S O U T H   W E S T   C O R N E R   B C  {{{
  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  // south west corner
  //  -- Average adjacent cells (east side and north side cells)
  //  -- This is only for when boundary conditions are applied on the
  //     adjecent sides, south and west, so that the corners overlap.
  if( (id==0)
     &&
      (
          1 ||
        (
          (
            lattice->param.velocity_w_out[subs]
          ||lattice->param.velocity_w_in[subs]
          ||lattice->param.pressure_w_out[subs]
          ||lattice->param.pressure_w_in[subs]
          )
        &&
          (
            lattice->param.velocity_s_out[subs]
          ||lattice->param.velocity_s_in[subs]
          ||lattice->param.pressure_s_out[subs]
          ||lattice->param.pressure_s_in[subs]
          )
        )
      )
    )
  {
    ftemp = lattice->pdf[subs][0].ftemp;
#if 0
    // Leverage the existing supplementary pointers, even though the
    // names don't make sense in this context.
    ftemp_end = lattice->pdf[subs][1].ftemp;
    ftemp_mid = lattice->pdf[subs][get_LX(lattice)].ftemp;

    for( a=0; a<9; a++)
    {
      ftemp[a] = ( ftemp_end[a] + ftemp_mid[a]) / 2.;
    }

#if 1
      // Enforce a prescribed pressure by adjusting the resting distribution.
      ftemp[0] = 1.0027
               -
               (
                 ftemp[1] + ftemp[2] + ftemp[3] + ftemp[4]
               + ftemp[5] + ftemp[6] + ftemp[7] + ftemp[8]
               );
#endif
#else
    //  6 2 5
    //   \|/
    //  3 o-1
    //     \
    //  7 4 8
    ftemp[1] = ftemp[3];
    ftemp[2] = ftemp[4];
    ftemp[8] = ftemp[7];
    ftemp[6] = 0.;
    ftemp[5] = 1.0027
    //ftemp[5] = 1.00054
             -
             (
               ftemp[0]
             + ftemp[1] + ftemp[2] + ftemp[3] + ftemp[4]
             + ftemp[6] + ftemp[7] + ftemp[8]
             );
    ftemp[5] /= 2.;
    ftemp[6] = ftemp[5];
#endif

  }
                                                                          // }}}
  // S O U T H   E A S T   C O R N E R   B C  {{{
  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  // south east corner
  //  -- Average adjacent cells (west side and north side cells)
  //  -- This is only for when boundary conditions are applied on the
  //     adjecent sides, south and east, so that the corners overlap.
  if( (id==0)
     &&
      (
          1 ||
        (
          (
            lattice->param.velocity_e_out[subs]
          ||lattice->param.velocity_e_in[subs]
          ||lattice->param.pressure_e_out[subs]
          ||lattice->param.pressure_e_in[subs]
          )
        &&
          (
            lattice->param.velocity_s_out[subs]
          ||lattice->param.velocity_s_in[subs]
          ||lattice->param.pressure_s_out[subs]
          ||lattice->param.pressure_s_in[subs]
          )
        )
      )
    )
  {
    ftemp = lattice->pdf[subs][get_LX(lattice)-1].ftemp;
#if 0
    // Leverage the existing supplementary pointers, even though the
    // names don't make sense in this context.
    ftemp_end = lattice->pdf[subs][get_LX(lattice)-2].ftemp;
    ftemp_mid = lattice->pdf[subs][2*get_LX(lattice)-1].ftemp;

    for( a=0; a<9; a++)
    {
      ftemp[a] = ( ftemp_end[a] + ftemp_mid[a]) / 2.;
    }

#if 1
      // Enforce a prescribed pressure by adjusting the resting distribution.
      ftemp[0] = 1.0027
               -
               (
                 ftemp[1] + ftemp[2] + ftemp[3] + ftemp[4]
               + ftemp[5] + ftemp[6] + ftemp[7] + ftemp[8]
               );
#endif
#else
    //  6 2 5
    //   \|/
    //  3-o 1
    //   /
    //  7 4 8
    ftemp[3] = ftemp[1];
    ftemp[2] = ftemp[4];
    ftemp[7] = ftemp[8];
    ftemp[5] = 0.;
    ftemp[6] = 1.0027
    //ftemp[6] = 1.00054
             -
             (
               ftemp[0]
             + ftemp[1] + ftemp[2] + ftemp[3] + ftemp[4]
             + ftemp[5] + ftemp[7] + ftemp[8]
             );
    ftemp[6] /= 2.;
    ftemp[5] = ftemp[6];

#endif

  }
                                                                          // }}}
  // N O R T H   W E S T   C O R N E R   B C  {{{
  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  // north west corner
  //  -- Average adjacent cells (east side and south side cells)
  //  -- This is only for when boundary conditions are applied on the
  //     adjecent sides, north and west, so that the corners overlap.
  if( (id==get_num_procs(lattice)-1)
     &&
      (
          1 ||
        (
          (
            lattice->param.velocity_w_out[subs]
          ||lattice->param.velocity_w_in[subs]
          ||lattice->param.pressure_w_out[subs]
          ||lattice->param.pressure_w_in[subs]
          )
        &&
          (
            lattice->param.velocity_n_out[subs]
          ||lattice->param.velocity_n_in[subs]
          ||lattice->param.pressure_n_out[subs]
          ||lattice->param.pressure_n_in[subs]
          )
        )
      )
    )
  {
    ftemp = lattice->pdf[subs][get_NumNodes(lattice)-get_LX(lattice)].ftemp;
#if 0
    // Leverage the existing supplementary pointers, even though the
    // names don't make sense in this context.
    ftemp_end = lattice->pdf[subs][get_NumNodes(lattice)-get_LX(lattice)+1].ftemp;
    ftemp_mid = lattice->pdf[subs][get_NumNodes(lattice)-2*get_LX(lattice)].ftemp;

    for( a=0; a<9; a++)
    {
      ftemp[a] = ( ftemp_end[a] + ftemp_mid[a]) / 2.;
    }

#if 1
      // Enforce a prescribed pressure by adjusting the resting distribution.
      ftemp[0] = 1.00054
               -
               (
                 ftemp[1] + ftemp[2] + ftemp[3] + ftemp[4]
               + ftemp[5] + ftemp[6] + ftemp[7] + ftemp[8]
               );
#endif
#else
    //  6 2 5
    //     /
    //  3 o-1
    //   /|\
    //  7 4 8
    ftemp[1] = ftemp[3];
    ftemp[4] = ftemp[2];
    ftemp[5] = ftemp[6];
    ftemp[7] = 0.;
    //ftemp[8] = 1.00054
    ftemp[8] = ( 1.0027 - (get_LY(lattice)-1)*.00054)
             -
             (
               ftemp[0]
             + ftemp[1] + ftemp[2] + ftemp[3] + ftemp[4]
             + ftemp[5] + ftemp[6] + ftemp[7]
             );
    ftemp[8] /= 2.;
    ftemp[7] = ftemp[8];

#endif

  }
                                                                          // }}}
  // N O R T H   E A S T   C O R N E R   B C  {{{
  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  // north east corner
  //  -- Average adjacent cells (west side and south side cells)
  //  -- This is only for when boundary conditions are applied on the
  //     adjecent sides, north and east, so that the corners overlap.
  if( (id==get_num_procs(lattice)-1)
     &&
      (
          1 ||
        (
          (
            lattice->param.velocity_e_out[subs]
          ||lattice->param.velocity_e_in[subs]
          ||lattice->param.pressure_e_out[subs]
          ||lattice->param.pressure_e_in[subs]
          )
        &&
          (
            lattice->param.velocity_n_out[subs]
          ||lattice->param.velocity_n_in[subs]
          ||lattice->param.pressure_n_out[subs]
          ||lattice->param.pressure_n_in[subs]
          )
        )
      )
    )
  {
    ftemp = lattice->pdf[subs][get_NumNodes(lattice)-1].ftemp;
#if 0
    // Leverage the existing supplementary pointers, even though the
    // names don't make sense in this context.
    ftemp_end = lattice->pdf[subs][get_NumNodes(lattice)-2].ftemp;
    ftemp_mid = lattice->pdf[subs][get_NumNodes(lattice)-get_LX(lattice)-1].ftemp;

    for( a=0; a<9; a++)
    {
      ftemp[a] = ( ftemp_end[a] + ftemp_mid[a]) / 2.;
    }

#if 1
      // Enforce a prescribed pressure by adjusting the resting distribution.
      ftemp[0] = 1.00054
               -
               (
                 ftemp[1] + ftemp[2] + ftemp[3] + ftemp[4]
               + ftemp[5] + ftemp[6] + ftemp[7] + ftemp[8]
               );
#endif
#else
    //  6 2 5
    //   \
    //  3-o 1
    //   /|\
    //  7 4 8
    ftemp[3] = ftemp[1];
    ftemp[4] = ftemp[2];
    ftemp[6] = ftemp[5];
    ftemp[8] = 0.;
    //ftemp[7] = 1.00054
    ftemp[7] = ( 1.0027 - (get_LY(lattice)-1)*.00054)
             -
             (
               ftemp[1] + ftemp[2] + ftemp[3] + ftemp[4]
             + ftemp[5] + ftemp[6] + ftemp[0] + ftemp[8]
             );
    ftemp[7] /= 2.;
    ftemp[8] = ftemp[7];
#endif

  }
                                                                           // }}}
#endif
 } /* for( subs=0; subs<NUM_FLUID_COMPONENTS; subs++) */

#if INAMURO_SIGMA_COMPONENT
  subs=1;
#if SIGMA_BULK_FLAG
	if(lattice->time > lattice->param.sigma_bulk_on)
	{
#endif

  // C O N S T   C O N C   N O R T H   I N   B C  {{{
  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  // constant conc north inflow
  //  -- Constant concentration boundary on north side using inflow value.
  if( (id==get_num_procs(lattice)-1) && lattice->param.constcon_n_in )
  {
    //printf("%s %d -- constcon_n_in\n", __FILE__, __LINE__);
    ftemp = lattice->pdf[subs][lattice->NumNodes-lattice->param.LX].ftemp;
    ftemp_end = lattice->pdf[subs][lattice->NumNodes].ftemp;
    while( ftemp < ftemp_end)
    {
      // North, Inflow

      if( lattice->time >= lattice->param.sigma_start
           && lattice->time <= lattice->param.sigma_stop)
      {
        c = lattice->param.C_in;
      }
      else
      {
        c = 0.;
      }

      if( lattice->param.simple_diffusion)
      {
        rho = 8.*( c - ( ftemp[0] + ftemp[1] + ftemp[3] + ftemp[2]));
        ftemp[4] = (1./8.)*rho;
      }
      else
      {
        rho = 6.*( c - ( ftemp[0] + ftemp[1] + ftemp[3]
                       + ftemp[2] + ftemp[5] + ftemp[6]));
        ftemp[4] = (1./9.)*rho;
        ftemp[7] = (1./36.)*rho;
        ftemp[8] = (1./36.)*rho;
      }

      ftemp += ( sizeof(struct pdf_struct)/8);

    } /* while( ftemp < ftemp_end) */

  } /* if( lattice->param.constcon_n_in ) */
                                                                          // }}}
  // C O N S T   C O N C   S O U T H   I N   B C  {{{
  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  // constant conc south inflow
  //  -- Constant concentration boundary on south side using inflow value.
  if( (id==0) && lattice->param.constcon_s_in )
  {
    //printf("bcs() %s %d >> constcon_s_in\n", __FILE__, __LINE__);
    ftemp = lattice->pdf[subs][0].ftemp;
    ftemp_end = lattice->pdf[subs][lattice->param.LX].ftemp;
    while( ftemp < ftemp_end)
    {
      // South, Inflow

      if( lattice->time >= lattice->param.sigma_start
           && lattice->time <= lattice->param.sigma_stop)
      {
        c = lattice->param.C_in;
      }
      else
      {
        c = 0.;
      }

      if( lattice->param.simple_diffusion)
      {
        rho = 8.*( c - ( ftemp[0] + ftemp[1] + ftemp[3] + ftemp[4]));
        ftemp[2] = (1./8.)*rho;
      }
      else
      {
        rho = 6.*( c - ( ftemp[0] + ftemp[1] + ftemp[3]
                       + ftemp[7] + ftemp[4] + ftemp[8]));
        ftemp[2] = (1./9.)*rho;
        ftemp[5] = (1./36.)*rho;
        ftemp[6] = (1./36.)*rho;
      }

      ftemp += ( sizeof(struct pdf_struct)/8);

    } /* while( ftemp < ftemp_end) */

  } /* if( lattice->param.constcon_s_in ) */
// }}}
  // C O N S T   C O N C   N O R T H   O U T   B C  {{{
  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  // constant conc north outflow
  //  -- Constant concentration boundary on north side using outflow value.
  if( (id==get_num_procs(lattice)-1) && lattice->param.constcon_n_out)
  {
    //printf("%s %d -- constcon_n_out\n", __FILE__, __LINE__);
    ftemp = lattice->pdf[subs][lattice->NumNodes-lattice->param.LX].ftemp;
    ftemp_end = lattice->pdf[subs][lattice->NumNodes].ftemp;
    i=0;
    while( ftemp < ftemp_end)
    {
     //if( i>0 && i<get_LX(lattice)-1)
     //{
      // North, Outflow
      if( lattice->time >= lattice->param.sigma_start
           && lattice->time <= lattice->param.sigma_stop)
      {
        c = lattice->param.C_out;
      }
      else
      {
        c = 0.;
      }

      if( lattice->param.simple_diffusion)
      {
        rho = 8.*( c - ( ftemp[0] + ftemp[1] + ftemp[3] + ftemp[2]));
        ftemp[4] = (1./8.)*rho;
      }
      else
      {
//printf("%s %d >> constcon_n_out\n",__FILE__,__LINE__);
        rho = 6.*( c - ( ftemp[0] + ftemp[1] + ftemp[3]
                       + ftemp[2] + ftemp[5] + ftemp[6]));
        ftemp[4] = (1./9.)*rho;
        ftemp[7] = (1./36.)*rho;
        ftemp[8] = (1./36.)*rho;
      }
     //}

      ftemp += ( sizeof(struct pdf_struct)/8);
      i++;

    } /* while( ftemp < ftemp_end) */

  } /* if( lattice->param.constcon_n_out) */
                                                                          // }}}
  // C O N S T   C O N C   S O U T H   O U T   B C  {{{
  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  // constant conc south outflow
  //  -- Constant concentration boundary on south side with outflow value.
  if( (id==0) && lattice->param.constcon_s_out)
  {
    //printf("bcs() %s %d >> constcon_s_out\n", __FILE__, __LINE__);
    ftemp = lattice->pdf[subs][0].ftemp;
    ftemp_end = lattice->pdf[subs][lattice->param.LX].ftemp;
    i=0;
    while( ftemp < ftemp_end)
    {
      // South, Outflow

     //if( i>0 && i<get_LX(lattice)-1)
     //{
      if( lattice->time >= lattice->param.sigma_start
           && lattice->time <= lattice->param.sigma_stop)
      {
        c = lattice->param.C_out;
      }
      else
      {
        c = 0.;
      }

      if( lattice->param.simple_diffusion)
      {
        rho = 8.*( c - ( ftemp[0] + ftemp[1] + ftemp[3] + ftemp[4]));
        ftemp[2] = (1./8.)*rho;
      }
      else
      {
        rho = 6.*( c - ( ftemp[0] + ftemp[1] + ftemp[3]
                       + ftemp[7] + ftemp[4] + ftemp[8]));
        ftemp[2] = (1./9.)*rho;
        ftemp[5] = (1./36.)*rho;
        ftemp[6] = (1./36.)*rho;
      }
     //}

      ftemp += ( sizeof(struct pdf_struct)/8);
      i++;

    } /* while( ftemp < ftemp_end) */

  } /* if( constcon_s_out) */
                                                                          // }}}

  // C O N S T   F L U X   N O R T H   I N   B C  {{{
  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  // constant flux north inflow
  //  -- Constant flux boundary on north side with inflow value.
  if( (id==get_num_procs(lattice)-1) && lattice->param.constflx_n_in )
  {
    //printf("%s %d -- constflx_n_in\n", __FILE__, __LINE__);
    ftemp = lattice->pdf[subs][lattice->NumNodes-lattice->param.LX].ftemp;
    ftemp_end = lattice->pdf[subs][lattice->NumNodes].ftemp;
    while( ftemp < ftemp_end)
    {
      // North, Inflow

      if( lattice->time >= lattice->param.sigma_start
           && lattice->time <= lattice->param.sigma_stop)
      {
        u = lattice->param.u_sigma_in;
      }
      else
      {
        u = 0.;
      }

      if( lattice->param.simple_diffusion)
      {
        rho = 8.*( u + ftemp[2]);
        ftemp[4] = (1./ 8.)*rho;
      }
      else
      {
        rho = 6.*( u + ftemp[2] + ftemp[5] + ftemp[6]);

        ftemp[4] = (1./ 9.)*rho;
        ftemp[7] = (1./36.)*rho;
        ftemp[8] = (1./36.)*rho;
      }

      ftemp += ( sizeof(struct pdf_struct)/8);

    } /* while( ftemp < ftemp_end) */

  } /* if( lattice->param.constflx_n_in ) */
                                                                          // }}}
  // C O N S T   F L U X   S O U T H   I N   B C  {{{
  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  // constant flux south inflow
  //  -- Constant flux boundary on south side with inflow value.
  if( (id==0) && lattice->param.constflx_s_in )
  {
    //printf("bcs() %s %d >> constflx_s_in\n", __FILE__, __LINE__);
    ftemp = lattice->pdf[subs][0].ftemp;
    ftemp_end = lattice->pdf[subs][lattice->param.LX].ftemp;
    while( ftemp < ftemp_end)
    {
      // South, Inflow

      if( lattice->time >= lattice->param.sigma_start
           && lattice->time <= lattice->param.sigma_stop)
      {
        u = lattice->param.u_sigma_in;
      }
      else
      {
        u = 0.;
      }

      if( lattice->param.simple_diffusion)
      {
        rho = 8.*( u + ftemp[4]);
        ftemp[2] = (1./8.)*rho;
      }
      else
      {
        rho = 6.*( u + ftemp[7] + ftemp[4] + ftemp[8]);

        ftemp[2] = (1./9.)*rho;
        ftemp[5] = (1./36.)*rho;
        ftemp[6] = (1./36.)*rho;
      }

      ftemp += ( sizeof(struct pdf_struct)/8);

    } /* while( ftemp < ftemp_end) */

  } /* if( lattice->param.constflx_s_in ) */
                                                                          // }}}
  // C O N S T   F L U X   N O R T H   O U T   B C  {{{
  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  // constant flux north outflow
  //  -- Constant flux boundary on north side with outflow value.
  if( (id==get_num_procs(lattice)-1) && lattice->param.constflx_n_out)
  {
    //printf("%s %d -- constflx_n_out\n", __FILE__, __LINE__);
    ftemp = lattice->pdf[subs][lattice->NumNodes-lattice->param.LX].ftemp;
    ftemp_end = lattice->pdf[subs][lattice->NumNodes].ftemp;
    n = get_NumNodes(lattice) - get_LX(lattice);
    i = 0;
    while( ftemp < ftemp_end)
    {
      // North, Outflow

     if( (i>0 && i<get_LX(lattice)-1) && is_not_solid_node(lattice,subs,n))
     {
      if( lattice->time >= lattice->param.sigma_start
           && lattice->time <= lattice->param.sigma_stop)
      {
        u = lattice->param.u_sigma_out;
      }
      else
      {
        u = 0.;
      }

      if( lattice->param.simple_diffusion)
      {
        rho = 8.*( u + ftemp[2]);
        ftemp[4] = (1./8.)*rho;
      }
      else
      {
        rho = 6.*( u + ftemp[2] + ftemp[5] + ftemp[6]);

        ftemp[4] = (1./9.)*rho;
        ftemp[7] = (1./36.)*rho;
        ftemp[8] = (1./36.)*rho;
      }
     }
     else
     {
       // Corners
       if( i!=0)
       {
         // East corner
         ftemp[6] = ftemp[5];
         rho = 6.*( u + ftemp[2] + ftemp[5] + ftemp[6]);

         ftemp[4] = (1./9.)*rho;
         ftemp[7] = (1./36.)*rho;
         ftemp[8] = (1./36.)*rho;
       }
       else
       {
         // West corner
         ftemp[5] = ftemp[6];
         rho = 6.*( u + ftemp[2] + ftemp[5] + ftemp[6]);

         ftemp[4] = (1./9.)*rho;
         ftemp[7] = (1./36.)*rho;
         ftemp[8] = (1./36.)*rho;
       }
     }

      ftemp += ( sizeof(struct pdf_struct)/8);
      n++;
      i++;

    } /* while( ftemp < ftemp_end) */

  } /* if( lattice->param.constflx_n_out) */
                                                                          // }}}
  // C O N S T   F L U X   S O U T H   O U T   B C  {{{
  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  // constant flux south outflow
  //  -- Constant flux boundary on south side with outflow value.
  if( (id==0) && lattice->param.constflx_s_out)
  {
    //printf("bcs() %s %d >> constflx_s_out\n", __FILE__, __LINE__);
    ftemp = lattice->pdf[subs][0].ftemp;
    ftemp_end = lattice->pdf[subs][lattice->param.LX].ftemp;
    n = 0;
    i = 0;
    while( ftemp < ftemp_end)
    {
      // South, Outflow

     if( ( i>0 && i<get_LX(lattice)-1) && is_not_solid_node(lattice,subs,n))
     {
      if( lattice->time >= lattice->param.sigma_start
           && lattice->time <= lattice->param.sigma_stop)
      {
        u = lattice->param.u_sigma_out;
      }
      else
      {
        u = 0.;
      }

      if( lattice->param.simple_diffusion)
      {
        rho = 8.*( u + ftemp[4]);
        ftemp[2] = (1./8.)*rho;
      }
      else
      {
        rho = 6.*( u + ftemp[7] + ftemp[4] + ftemp[8]);

        ftemp[2] = (1./9.)*rho;
        ftemp[5] = (1./36.)*rho;
        ftemp[6] = (1./36.)*rho;
      }
     }
     else
     {
       // Corners
       if( i!=0)
       {
         // East corner
         ftemp[7] = ftemp[8];
         rho = 6.*( u + ftemp[7] + ftemp[4] + ftemp[8]);

         ftemp[2] = (1./9.)*rho;
         ftemp[5] = (1./36.)*rho;
         ftemp[6] = (1./36.)*rho;
       }
       else
       {
         // West corner
         ftemp[8] = ftemp[7];
         rho = 6.*( u + ftemp[7] + ftemp[4] + ftemp[8]);

         ftemp[2] = (1./9.)*rho;
         ftemp[5] = (1./36.)*rho;
         ftemp[6] = (1./36.)*rho;
       }
     }

      ftemp += ( sizeof(struct pdf_struct)/8);
      n++;
      i++;

    } /* while( ftemp < ftemp_end) */

  } /* if( lattice->param.constflx_s_out) */
                                                                          // }}}

  // Z E R O   C O N C   G R A D I E N T   S O U T H   B C  {{{
  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  // zero conc gradient south
  //  -- Zero concentration gradient boundary on south side.
  if( (id==0) && lattice->param.zeroconcgrad_s)
  {
    //printf("bcs() %s %d >> zeroconcgrad_s_out\n", __FILE__, __LINE__);
    ftemp = lattice->pdf[subs][0].ftemp;
    ftemp_end = lattice->pdf[subs][lattice->param.LX].ftemp;
    while( ftemp < ftemp_end)
    {
      // South

      // Copy from the adjacent interior neighbor values for the
      // unknown distributions.
      ftemp[2] = ftemp[ 2 + (sizeof(struct pdf_struct)/8)*lattice->param.LX];
      ftemp[5] = ftemp[ 5 + (sizeof(struct pdf_struct)/8)*lattice->param.LX];
      ftemp[6] = ftemp[ 6 + (sizeof(struct pdf_struct)/8)*lattice->param.LX];

      if( lattice->param.zeroconcgrad_full)
      {
        // Copy all the rest of the distribution functions from the adjacent
        // interior neighbor.
        ftemp[0] = ftemp[ 0 + (sizeof(struct pdf_struct)/8)*lattice->param.LX];
        ftemp[1] = ftemp[ 1 + (sizeof(struct pdf_struct)/8)*lattice->param.LX];
        ftemp[3] = ftemp[ 3 + (sizeof(struct pdf_struct)/8)*lattice->param.LX];
        ftemp[4] = ftemp[ 4 + (sizeof(struct pdf_struct)/8)*lattice->param.LX];
        ftemp[7] = ftemp[ 7 + (sizeof(struct pdf_struct)/8)*lattice->param.LX];
        ftemp[8] = ftemp[ 8 + (sizeof(struct pdf_struct)/8)*lattice->param.LX];
      }

      ftemp += ( sizeof(struct pdf_struct)/8);

    } /* while( ftemp < ftemp_end) */

  } /* if( lattice->param.zerograd_s) */
                                                                          // }}}
  // Z E R O   C O N C   G R A D I E N T   N O R T H   B C  {{{
  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  // zero conc gradient north
  //  -- Zero concentration gradient boundary on north side.
  if( (id==get_num_procs(lattice)-1) && lattice->param.zeroconcgrad_n)
  {
    //printf("bcs() %s %d >> zeroconcgrad_n\n", __FILE__, __LINE__);
    ftemp = lattice->pdf[subs][lattice->NumNodes-lattice->param.LX].ftemp;
    ftemp_end = lattice->pdf[subs][lattice->NumNodes].ftemp;
    while( ftemp < ftemp_end)
    {
      // North

      // Copy from the adjacent interior neighbor values for the
      // unknown distributions.
      ftemp[4] = ftemp[ 4 - (sizeof(struct pdf_struct)/8)*lattice->param.LX];
      ftemp[7] = ftemp[ 7 - (sizeof(struct pdf_struct)/8)*lattice->param.LX];
      ftemp[8] = ftemp[ 8 - (sizeof(struct pdf_struct)/8)*lattice->param.LX];

      if( lattice->param.zeroconcgrad_full)
      {
        // Copy all the rest of the distribution functions from the adjacent
        // interior neighbor.
        ftemp[0] = ftemp[ 0 - (sizeof(struct pdf_struct)/8)*lattice->param.LX];
        ftemp[1] = ftemp[ 1 - (sizeof(struct pdf_struct)/8)*lattice->param.LX];
        ftemp[2] = ftemp[ 2 - (sizeof(struct pdf_struct)/8)*lattice->param.LX];
        ftemp[3] = ftemp[ 3 - (sizeof(struct pdf_struct)/8)*lattice->param.LX];
        ftemp[5] = ftemp[ 5 - (sizeof(struct pdf_struct)/8)*lattice->param.LX];
        ftemp[6] = ftemp[ 6 - (sizeof(struct pdf_struct)/8)*lattice->param.LX];
      }

      ftemp += ( sizeof(struct pdf_struct)/8);

    } /* while( ftemp < ftemp_end) */

  } /* if( lattice->param.zerograd_n) */
                                                                          // }}}

  // C O N S T   C O N C   E A S T   I N   B C  {{{
  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  // constant conc east inflow
  //  -- Constant concentration boundary on east side using inflow value.
  if( lattice->param.constcon_e_in )
  {
    //printf("%s %d -- constcon_e_in\n", __FILE__, __LINE__);
    ftemp = lattice->pdf[subs][lattice->param.LX-1].ftemp;
    ftemp_end = lattice->pdf[subs][lattice->NumNodes].ftemp;
    n = get_LX(lattice)-1;
    j = 0;
    while( ftemp < ftemp_end)
    {
      // East, Inflow

     if( ( j>0 && j<get_LY(lattice)-1) && is_not_solid_node( lattice, subs, n))
     {
      if( lattice->time >= lattice->param.sigma_start
           && lattice->time <= lattice->param.sigma_stop)
      {
        c = lattice->param.C_in;
      }
      else
      {
        c = 0.;
      }

      if( lattice->param.simple_diffusion)
      {
          rho = 8.*( c - ( ftemp[0] + ftemp[2] + ftemp[4] + ftemp[1]));
          ftemp[3] = (1./8.)*rho;
      }
      else
      {
          rho = 6.*( c - ( ftemp[0] + ftemp[2] + ftemp[4]
                         + ftemp[1] + ftemp[5] + ftemp[8]));

          ftemp[3] = (1./9.)*rho;
          ftemp[7] = (1./36.)*rho;
          ftemp[6] = (1./36.)*rho;
      }
     }

      ftemp += ( sizeof(struct pdf_struct)/8)*lattice->param.LX;
      n+=get_LX(lattice);
      j++;

    } /* while( ftemp < ftemp_end) */

  } /* if( lattice->param.constcon_e_in ) */
                                                                          // }}}
  // C O N S T   C O N C   W E S T   I N   B C  {{{
  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  // constant conc west inflow
  //  -- Constant concentration boundary on west side using inflow value.
  if( lattice->param.constcon_w_in )
  {
    //printf("bcs() %s %d >> constcon_w_in\n", __FILE__, __LINE__);
    n = 0;
    ftemp     = lattice->pdf[subs][0].ftemp;
    ftemp_end =
      lattice->pdf[subs][(lattice->param.LY+1)*lattice->param.LX].ftemp;
    j = 0;
    while( ftemp < ftemp_end)
    {
      // West, Inflow

      bc_type   = &( lattice->bc[0][n].bc_type);

      if( 1)//( j>0 && j<get_LY(lattice)-1) || is_not_solid_node(lattice,subs,n))
      {
        if( lattice->time >= lattice->param.sigma_start
           && lattice->time <= lattice->param.sigma_stop)
        {
          if(lattice->param.constcon_w_in==1)
          {
            c = lattice->param.C_in;
          }
          else if(lattice->param.constcon_w_in==3)
          {
            //
            // (-D*(dC/dx) + v*C) = v*C0 ==>
            //
            //   c = (v*c0 + D*c2)/( D + v)
            //
            v  = lattice->macro_vars[0][n].u[0];
            c0 = lattice->param.C_in;
            D  = (1./3.)*( lattice->param.tau[1] - .5);
            c2 = lattice->macro_vars[1][n+1].rho;
            c = (v*c0 + D*c2)/( D + v);
          }
          else
          {
            printf(
              "%s (%d) >> ERROR:  "
              "constcon_w_in = %d type BC not valid. Exiting!\n",
              __FILE__,__LINE__,lattice->param.constcon_w_in);
            process_exit(1);
          }
        }
        else
        {
          c = 0.;
        }

        if( lattice->param.simple_diffusion)
        {
          rho = 8.*( c - ( ftemp[0] + ftemp[2] + ftemp[4] + ftemp[3]));
          ftemp[1] = (1./8.)*rho;
        }
        else
        {
          rho = 6.*( c - ( ftemp[0] + ftemp[2] + ftemp[4]
                         + ftemp[3] + ftemp[6] + ftemp[7]));

#if 0
          ftemp[1] = (1./9.)*rho;
          ftemp[5] = (1./36.)*rho;
          ftemp[8] = (1./36.)*rho;
#else
          ftemp[1] =
            (1./6.)*rho*lattice->pdf[0][n].ftemp[1]
             /(lattice->pdf[0][n].ftemp[1]
             + lattice->pdf[0][n].ftemp[5]
             + lattice->pdf[0][n].ftemp[8]);

          ftemp[5] =
            (1./6.)*rho*lattice->pdf[0][n].ftemp[5]
             /(lattice->pdf[0][n].ftemp[1]
             + lattice->pdf[0][n].ftemp[5]
             + lattice->pdf[0][n].ftemp[8]);

          ftemp[8] =
            (1./6.)*rho*lattice->pdf[0][n].ftemp[8]
             /(lattice->pdf[0][n].ftemp[1]
             + lattice->pdf[0][n].ftemp[5]
             + lattice->pdf[0][n].ftemp[8]);
#endif
        }
      } /* if( 0 || !(*bc_type && BC_SOLID_NODE)) */

      else // Solid node
      {
        //printf(
        //  "%s (%d) >> constcon_w_in: Skipping n = %d. "
        //  "(bc_type=%d) \n",
        //  __FILE__, __LINE__, n, *bc_type);

      } /* if( 0 || !(*bc_type && BC_SOLID_NODE)) else */

      n       +=   lattice->param.LX;
      ftemp   += ( sizeof(struct pdf_struct)/8)*lattice->param.LX;
      bc_type += ( sizeof(struct  bc_struct)/8)*lattice->param.LX;
      j++;

    } /* while( ftemp < ftemp_end) */

  } /* if( lattice->param.constcon_w_in ) */
                                                                          // }}}
  // C O N S T   C O N C   E A S T   O U T   B C  {{{
  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  // constant conc east outflow
  //  -- Constant concentration boundary on east side using outflow value.
  if( lattice->param.constcon_e_out)
  {
    //printf("%s %d -- constcon_e_out\n", __FILE__, __LINE__);
    ftemp = lattice->pdf[subs][lattice->param.LX-1].ftemp;
    ftemp_end = lattice->pdf[subs][lattice->NumNodes].ftemp;
    n = get_LX(lattice)-1;
    j = 0;
    while( ftemp < ftemp_end)
    {
      // East, Outflow

     if( 1)//( j>0 && j<get_LX(lattice)-1) && is_not_solid_node(lattice, subs, n))
     {
      if( lattice->time >= lattice->param.sigma_start
           && lattice->time <= lattice->param.sigma_stop)
      {
          if(lattice->param.constcon_e_out==1)
          {
            //c = lattice->param.C_in;
            c = lattice->param.C_out;
          }
          else if(lattice->param.constcon_e_out==3)
          {
            //
            // (-D*(dC/dx) + v*C) = v*C0 ==>
            //
            //   c = (v*c0 + D*c2)/( D + v)
            //
            v  = lattice->macro_vars[0][n].u[0];
            c0 = lattice->param.C_out;
            D  = (1./3.)*( lattice->param.tau[1] - .5);
            c2 = lattice->macro_vars[1][n-1].rho;
            c = (v*c0 + D*c2)/( D + v);
          }
          else
          {
            printf(
              "%s (%d) >> ERROR:  "
              "constcon_e_out = %d type BC not valid. Exiting!\n",
              __FILE__,__LINE__,lattice->param.constcon_e_out);
            process_exit(1);
          }
      }
      else
      {
        c = 0.;
      }

      if( lattice->param.simple_diffusion)
      {
        rho = 8.*( c - ( ftemp[0] + ftemp[2] + ftemp[4] + ftemp[1]));
        ftemp[3] = (1./8.)*rho;
      }
      else
      {
        rho = 6.*( c - ( ftemp[0] + ftemp[2] + ftemp[4]
                       + ftemp[1] + ftemp[5] + ftemp[8]));

        ftemp[3] = (1./9.)*rho;
        ftemp[7] = (1./36.)*rho;
        ftemp[6] = (1./36.)*rho;
      }
     }

      ftemp += ( sizeof(struct pdf_struct)/8)*lattice->param.LX;
      n+=get_LX(lattice);
      j++;

    } /* while( ftemp < ftemp_end) */

  } /* if( lattice->param.constcon_e_out) */
                                                                          // }}}
  // C O N S T   C O N C   W E S T   O U T   B C  {{{
  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  // constant conc west outflow
  //  -- Constant concentration boundary on west side with outflow value.
  if( lattice->param.constcon_w_out)
  {
    //printf("bcs() %s %d >> constcon_w_out\n", __FILE__, __LINE__);
    ftemp     = lattice->pdf[subs][0].ftemp;
    ftemp_end =
      lattice->pdf[subs][(lattice->param.LY+1)*lattice->param.LX].ftemp;
    while( ftemp < ftemp_end)
    {
      // West, Outflow

      if( lattice->time >= lattice->param.sigma_start
           && lattice->time <= lattice->param.sigma_stop)
      {
        c = lattice->param.C_out;
      }
      else
      {
        c = 0.;
      }

      if( lattice->param.simple_diffusion)
      {
        rho = 8.*( c - ( ftemp[0] + ftemp[2] + ftemp[4] + ftemp[3]));
        ftemp[1] = (1./8.)*rho;
      }
      else
      {
        rho = 6.*( c - ( ftemp[0] + ftemp[2] + ftemp[4]
                       + ftemp[3] + ftemp[6] + ftemp[7]));

        ftemp[1] = (1./9.)*rho;
        ftemp[5] = (1./36.)*rho;
        ftemp[8] = (1./36.)*rho;
      }

      ftemp += ( sizeof(struct pdf_struct)/8)*lattice->param.LX;

    } /* while( ftemp < ftemp_end) */

  } /* if( constcon_w_out) */
                                                                          // }}}

  // C O N S T   F L U X   E A S T   I N   B C  {{{
  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  // constant flux east inflow
  //  -- Constant flux boundary on east side with inflow value.
  if( lattice->param.constflx_e_in )
  {
    //printf("%s %d -- constflx_e_in\n", __FILE__, __LINE__);
    ftemp = lattice->pdf[subs][lattice->param.LX-1].ftemp;
    ftemp_end = lattice->pdf[subs][lattice->NumNodes].ftemp;
    while( ftemp < ftemp_end)
    {
      // East, Inflow

      if( lattice->time >= lattice->param.sigma_start
           && lattice->time <= lattice->param.sigma_stop)
      {
        u = lattice->param.u_sigma_in;
      }
      else
      {
        u = 0.;
      }

      if( lattice->param.simple_diffusion)
      {
        rho = 8.*( u + ftemp[1]);
        ftemp[3] = (1./ 8.)*rho;
      }
      else
      {
        rho = 6.*( u + ftemp[1] + ftemp[5] + ftemp[8]);

        ftemp[3] = (1./ 9.)*rho;
        ftemp[7] = (1./36.)*rho;
        ftemp[6] = (1./36.)*rho;
      }

      ftemp += ( sizeof(struct pdf_struct)/8)*lattice->param.LX;

    } /* while( ftemp < ftemp_end) */

  } /* if( lattice->param.constflx_e_in ) */
                                                                          // }}}
  // C O N S T   F L U X   W E S T   I N   B C  {{{
  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  // constant flux west inflow
  //  -- Constant flux boundary on west side with inflow value.
  if( lattice->param.constflx_w_in )
  {
    //printf("bcs() %s %d >> constflx_w_in\n", __FILE__, __LINE__);
    n = 0;
    ftemp     = lattice->pdf[subs][0].ftemp;
    ftemp_end =
      lattice->pdf[subs][(lattice->param.LY+1)*lattice->param.LX].ftemp;
    while( ftemp < ftemp_end)
    {
      // West, Inflow

     if( lattice->param.constflx_w_in==1)
     {
      if( lattice->time >= lattice->param.sigma_start
           && lattice->time <= lattice->param.sigma_stop)
      {
        u = lattice->param.u_sigma_in;
      }
      else
      {
        u = 0.;
      }

      if( lattice->param.simple_diffusion)
      {
        rho = 8.*( u + ftemp[3]);
        ftemp[1] = (1./8.)*rho;
      }
      else
      {
        rho = 6.*( u + ftemp[7] + ftemp[3] + ftemp[6]);

        ftemp[1] = (1./9.)*rho;
        ftemp[5] = (1./36.)*rho;
        ftemp[8] = (1./36.)*rho;
      }
     }

     else if( lattice->param.constflx_w_in==3)
     {
      v = lattice->macro_vars[0][n].u[0];

      if( lattice->time >= lattice->param.sigma_start
           && lattice->time <= lattice->param.sigma_stop)
      {
        c0 = lattice->param.C_in;
      }
      else
      {
        c0 = 0.;
      }

      rho = 6.*(        c0 - ftemp[0] - ftemp[2] - ftemp[4]
               - (1.-1./v)*( ftemp[3] + ftemp[6] + ftemp[7])) / (1.+1./v);

      ftemp[1] = (1./9.)*rho;
      ftemp[5] = (1./36.)*rho;
      ftemp[8] = (1./36.)*rho;

     }
     else
     {
       printf(
         "%s (%d) >> ERROR:  "
         "constflx_w_in = %d type BC not valid. Exiting!\n",
         __FILE__,__LINE__,lattice->param.constflx_w_in);
       process_exit(1);
     }

      n     +=   lattice->param.LX;
      ftemp += ( sizeof(struct pdf_struct)/8)*lattice->param.LX;

    } /* while( ftemp < ftemp_end) */

  } /* if( lattice->param.constflx_w_in ) */
                                                                          // }}}
  // C O N S T   F L U X   E A S T   O U T   B C  {{{
  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  // constant flux east outflow
  //  -- Constant flux boundary on east side with outflow value.
  if( lattice->param.constflx_e_out)
  {
    //printf("%s %d -- constflx_e_out\n", __FILE__, __LINE__);
    ftemp = lattice->pdf[subs][lattice->param.LX-1].ftemp;
    ftemp_end = lattice->pdf[subs][lattice->NumNodes].ftemp;
    while( ftemp < ftemp_end)
    {
      // East, Outflow

      if( lattice->time >= lattice->param.sigma_start
           && lattice->time <= lattice->param.sigma_stop)
      {
        u = lattice->param.u_sigma_out;
      }
      else
      {
        u = 0.;
      }

      if( lattice->param.simple_diffusion)
      {
        rho = 8.*( u + ftemp[1]);
        ftemp[3] = (1./8.)*rho;
      }
      else
      {
        rho = 6.*( u + ftemp[1] + ftemp[5] + ftemp[8]);

        ftemp[3] = (1./9.)*rho;
        ftemp[7] = (1./36.)*rho;
        ftemp[6] = (1./36.)*rho;
      }

      ftemp += ( sizeof(struct pdf_struct)/8)*lattice->param.LX;

    } /* while( ftemp < ftemp_end) */

  } /* if( lattice->param.constflx_e_out) */
                                                                          // }}}
  // C O N S T   F L U X   W E S T   O U T   B C  {{{
  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  // constant flux west outflow
  //  -- Constant flux boundary on west side with outflow value.
  if( lattice->param.constflx_w_out)
  {
    //printf("bcs() %s %d >> constflx_w_out\n", __FILE__, __LINE__);
    ftemp     = lattice->pdf[subs][0].ftemp;
    ftemp_end =
      lattice->pdf[subs][(lattice->param.LY+1)*lattice->param.LX].ftemp;
    while( ftemp < ftemp_end)
    {
      // West, Outflow

      if( lattice->time >= lattice->param.sigma_start
           && lattice->time <= lattice->param.sigma_stop)
      {
        u = lattice->param.u_sigma_out;
      }
      else
      {
        u = 0.;
      }

      if( lattice->param.simple_diffusion)
      {
        rho = 8.*( u + ftemp[3]);
        ftemp[1] = (1./8.)*rho;
      }
      else
      {
        rho = 6.*( u + ftemp[7] + ftemp[3] + ftemp[6]);

        ftemp[1] = (1./9.)*rho;
        ftemp[5] = (1./36.)*rho;
        ftemp[8] = (1./36.)*rho;
      }

      ftemp += ( sizeof(struct pdf_struct)/8)*lattice->param.LX;

    } /* while( ftemp < ftemp_end) */

  } /* if( lattice->param.constflx_w_out) */
                                                                          // }}}

  // Z E R O   C O N C   G R A D I E N T   W E S T   B C  {{{
  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  // zero conc gradient west
  //  -- Zero concentration gradient boundary on west side.
  if( lattice->param.zeroconcgrad_w)
  {
    //printf("bcs() %s %d >> zeroconcgrad_w_out\n", __FILE__, __LINE__);
    ftemp     = lattice->pdf[subs][0].ftemp;
    ftemp_end =
      lattice->pdf[subs][(lattice->param.LY)*lattice->param.LX].ftemp;
    while( ftemp < ftemp_end)
    {
      // West

     if( 1 || is_not_solid_node( lattice, subs, n))
     {
      // Copy from the adjacent interior neighbor values for the
      // unknown distributions.
      ftemp[1] = ftemp[ 1 + (sizeof(struct pdf_struct)/8)];
      ftemp[5] = ftemp[ 5 + (sizeof(struct pdf_struct)/8)];
      ftemp[8] = ftemp[ 8 + (sizeof(struct pdf_struct)/8)];

      if( lattice->param.zeroconcgrad_full)
      {
        // Copy all the rest of the distribution functions from the adjacent
        // interior neighbor.
        ftemp[0] = ftemp[ 0 + (sizeof(struct pdf_struct)/8)];
        ftemp[2] = ftemp[ 2 + (sizeof(struct pdf_struct)/8)];
        ftemp[3] = ftemp[ 3 + (sizeof(struct pdf_struct)/8)];
        ftemp[4] = ftemp[ 4 + (sizeof(struct pdf_struct)/8)];
        ftemp[6] = ftemp[ 6 + (sizeof(struct pdf_struct)/8)];
        ftemp[7] = ftemp[ 7 + (sizeof(struct pdf_struct)/8)];
      }
     } /* if( 0 || !( (*bc_type) && BC_SOLID_NODE)) */

     else // Solid node
     {
       // TODO: What?

     } /* if( 0 || !( (*bc_type) && BC_SOLID_NODE)) else */

      ftemp += ( sizeof(struct pdf_struct)/8)*lattice->param.LX;

    } /* while( ftemp < ftemp_end) */

  } /* if( lattice->param.zerograd_s) */
                                                                          // }}}
  // Z E R O   C O N C   G R A D I E N T   E A S T   B C  {{{
  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  // zero conc gradient east
  //  -- Zero concentration gradient boundary on east side.
  if( lattice->param.zeroconcgrad_e)
  {
    //printf("bcs() %s %d >> zeroconcgrad_n\n", __FILE__, __LINE__);
    n = lattice->param.LX-1;
    ftemp = lattice->pdf[subs][lattice->param.LX-1].ftemp;
    ftemp_end = lattice->pdf[subs][lattice->NumNodes].ftemp;
    while( ftemp < ftemp_end)
    {
      // East

      bc_type   = &( lattice->bc[0][n].bc_type);

      if( 1 || is_not_solid_node( lattice, subs, n))
      {
        // Copy from the adjacent interior neighbor values for the
        // unknown distributions.
        ftemp[3] = ftemp[ 3 - (sizeof(struct pdf_struct)/8)];
        ftemp[6] = ftemp[ 6 - (sizeof(struct pdf_struct)/8)];
        ftemp[7] = ftemp[ 7 - (sizeof(struct pdf_struct)/8)];

        if( lattice->param.zeroconcgrad_full)
        {
          // Copy all the rest of the distribution functions from the adjacent
          // interior neighbor.
          ftemp[0] = ftemp[ 0 - (sizeof(struct pdf_struct)/8)];
          ftemp[1] = ftemp[ 1 - (sizeof(struct pdf_struct)/8)];
          ftemp[2] = ftemp[ 2 - (sizeof(struct pdf_struct)/8)];
          ftemp[4] = ftemp[ 4 - (sizeof(struct pdf_struct)/8)];
          ftemp[5] = ftemp[ 5 - (sizeof(struct pdf_struct)/8)];
          ftemp[8] = ftemp[ 8 - (sizeof(struct pdf_struct)/8)];
        }
      } /* if( 0 || !( (*bc_type) && BC_SOLID_NODE)) */

      else // Solid node
      {
        // TODO: What?

      } /* if( 0 || !( (*bc_type) && BC_SOLID_NODE)) else */

      if( is_first_timestep( lattice) && adjust_zero_flux_for_btc( lattice))
      {
        // To apply a zero gradient and measure the breakthrough curve there
        // properly, the last three columns of the domain need to have the same
        // solids pattern. The following artificially enforces that pattern.
        // Alternatively, the user may leave three empty columns at the end of
        // the domain.
        if( is_solid_node( lattice, subs, n-2)
            && is_not_solid_node( lattice, subs, n-1) )     {
                  make_solid_node( lattice,/*subs*/0, n-1);
                  make_solid_node( lattice,/*subs*/1, n-1); }
        if( is_solid_node( lattice, subs, n-1)
            && is_not_solid_node( lattice, subs, n  ) )     {
                  make_solid_node( lattice,/*subs*/0, n  );
                  make_solid_node( lattice,/*subs*/1, n  ); }
        if( is_not_solid_node( lattice, subs, n-1)
            && is_solid_node( lattice, subs, n) )           {
                  make_solid_node( lattice,/*subs*/0, n-1);
                  make_solid_node( lattice,/*subs*/1, n-1); }
        if( is_not_solid_node( lattice, subs, n-2)
            && is_solid_node( lattice, subs, n-1) )         {
                  make_solid_node( lattice,/*subs*/0, n-2);
                  make_solid_node( lattice,/*subs*/1, n-2); }
      }

      n     +=   lattice->param.LX;
      ftemp += ( sizeof(struct pdf_struct)/8)*lattice->param.LX;

    } /* while( ftemp < ftemp_end) */

  } /* if( lattice->param.zerograd_n) */
                                                                          // }}}

#if 0
  // C O R N E R S
  //############################################################################
  // S O U T H   W E S T   C O R N E R   B C  {{{
  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  // south west corner
  //  -- Average adjacent cells (east side and north side cells)
  //  -- This is only for when boundary conditions are applied on the
  //     adjecent sides, south and west, so that the corners overlap.
  if( (id==0)
     &&
      ( 1 ||
        (
          (
            lattice->param.constcon_w_out
          ||lattice->param.constcon_w_in
          ||lattice->param.constflx_w_out
          ||lattice->param.constflx_w_in
          ||lattice->param.zeroconcgrad_w
          )
        &&
          (
            lattice->param.constcon_s_out
          ||lattice->param.constcon_s_in
          ||lattice->param.constflx_s_out
          ||lattice->param.constflx_s_in
          ||lattice->param.zeroconcgrad_s
          )
        )
      )
    )
  {
    printf("BING! SW corner...");
    ftemp = &(lattice->pdf[subs][0].ftemp[0]);
    for( a=0; a<9; a++)
    {
      ftemp[a] = 0.;
    }

  }
                                                                          // }}}
#endif

#if SIGMA_BULK_FLAG
	}
#endif

#endif /* INAMURO_SIGMA_COMPONENT */

} /* void bcs( lattice_ptr lattice) */
// }}}
#endif
#endif

// P R O C E S S _ B C S  {{{
//##############################################################################
// void process_bcs( char *filename, int **bcs)
//
// P R O C E S S   B C S
//
//  - Process boundary condition information based on flags read
//    from params.in .
//
//  - More specifically, make sure that the requested boundary conditions
//    are not contradictory or incompatible with current implementation.
//
//  - And set the periodicity flags.
//
void process_bcs( lattice_ptr lattice, int subs)
{
  FILE *in;
  char filename[1024];
  int i, j, n, m;
  int ei, ej;
  int pad, bytes_per_row;
  char k;
  char b, g, r;
  struct bitmap_file_header bmfh;
  struct bitmap_info_header bmih;
  struct rgb_quad rgb;
  int *int_ptr;
  short int *short_int_ptr;
  int *width_ptr;
  int *height_ptr;
  short int *bitcount_ptr;

#if SAY_HI
  printf("process_bcs() -- Hi!\n");
#endif /* SAY_HI */

  //############################################################################
  //
  // Report any incompatible boundary conditions.
  // If there are incompatible bcs, process_exit after issuing message.
  //

#if 0 // Want to allow mix of velocity and pressure boundaries.

  // Can't have coincident pressure and velocity boundaries.
  //----------------------------------------------------------------------------

  // North
  if(  lattice->param.pressure_n_in[subs]
    && (  lattice->param.velocity_n_in[subs]
       || lattice->param.velocity_n_out[subs]))
  {
    printf("ERROR:  "
        "Coincident pressure and velocity north boundaries.  "
        "Exiting!\n");
    process_exit(1);
  }

  // South
  if(  lattice->param.pressure_s_in[subs]
    && (  lattice->param.velocity_s_in[subs]
       || lattice->param.velocity_s_out[subs]))
  {
    printf("ERROR:  "
        "Coincident pressure and velocity south boundaries.  "
        "Exiting!\n");
    process_exit(1);
  }

  // North
  if(  lattice->param.pressure_n_out[subs]
    && (  lattice->param.velocity_n_out[subs]
       || lattice->param.velocity_n_in[subs] ))
  {
    printf("ERROR:  "
        "Coincident pressure and velocity north boundaries.  "
        "Exiting!\n");
    process_exit(1);
  }

  // South
  if(  lattice->param.pressure_s_out[subs]
    && (  lattice->param.velocity_s_out[subs]
       || lattice->param.velocity_s_in[subs] ))
  {
    printf("ERROR:  "
        "Coincident pressure and velocity south boundaries.  "
        "Exiting!\n");
    process_exit(1);
  }

  // East
  if(  lattice->param.pressure_e_in[subs]
    && (  lattice->param.velocity_e_in[subs]
       || lattice->param.velocity_e_out[subs]))
  {
    printf("ERROR:  "
        "Coincident pressure and velocity east boundaries.  "
        "Exiting!\n");
    process_exit(1);
  }

  // West
  if(  lattice->param.pressure_w_in[subs]
    && (  lattice->param.velocity_w_in[subs]
       || lattice->param.velocity_w_out[subs]))
  {
    printf("ERROR:  "
        "Coincident pressure and velocity west boundaries.  "
        "Exiting!\n");
    process_exit(1);
  }

  // East
  if(  lattice->param.pressure_e_out[subs]
    && (  lattice->param.velocity_e_out[subs]
       || lattice->param.velocity_e_in[subs] ))
  {
    printf("ERROR:  "
        "Coincident pressure and velocity east boundaries.  "
        "Exiting!\n");
    process_exit(1);
  }

  // West
  if(  lattice->param.pressure_w_out[subs]
    && (  lattice->param.velocity_w_out[subs]
       || lattice->param.velocity_w_in[subs] ))
  {
    printf("ERROR:  "
        "Coincident pressure and velocity west boundaries.  "
        "Exiting!\n");
    process_exit(1);
  }

  // Can't have both inflow and outflow condition on same boundary.
  //----------------------------------------------------------------------------

  // North
  if(  lattice->param.pressure_n_in[subs]
    && lattice->param.pressure_n_out[subs])
  {
    printf("ERROR:  "
        "Coincident inflow and outflow condition on north boundary.  "
        "Exiting!\n");
    process_exit(1);
  }

  // South
  if(  lattice->param.pressure_s_in[subs]
    && lattice->param.pressure_s_out[subs])
  {
    printf("ERROR:  "
        "Coincident inflow and outflow condition on south boundary.  "
        "Exiting!\n");
    process_exit(1);
  }

  // North
  if(  lattice->param.velocity_n_in[subs]
    && lattice->param.velocity_n_out[subs])
  {
    printf("ERROR:  "
        "Coincident inflow and outflow condition on north boundary.  "
        "Exiting!\n");
    process_exit(1);
  }

  // South
  if(  lattice->param.velocity_s_in[subs]
    && lattice->param.velocity_s_out[subs])
  {
    printf("ERROR:  "
        "Coincident inflow and outflow condition on south boundary.  "
        "Exiting!\n");
    process_exit(1);
  }

  // East
  if(  lattice->param.pressure_e_in[subs]
    && lattice->param.pressure_e_out[subs])
  {
    printf("ERROR:  "
        "Coincident inflow and outflow condition on east boundary.  "
        "Exiting!\n");
    process_exit(1);
  }

  // West
  if(  lattice->param.pressure_w_in[subs]
    && lattice->param.pressure_w_out[subs])
  {
    printf("ERROR:  "
        "Coincident inflow and outflow condition on west boundary.  "
        "Exiting!\n");
    process_exit(1);
  }

  // East
  if(  lattice->param.velocity_e_in[subs]
    && lattice->param.velocity_e_out[subs])
  {
    printf("ERROR:  "
        "Coincident inflow and outflow condition on east boundary.  "
        "Exiting!\n");
    process_exit(1);
  }

  // West
  if(  lattice->param.velocity_w_in[subs]
    && lattice->param.velocity_w_out[subs])
  {
    printf("ERROR:  "
        "Coincident inflow and outflow condition on west boundary.  "
        "Exiting!\n");
    process_exit(1);
  }
#endif

  //############################################################################
  //
  // Update periodicity settings.
  //

  // North/South
  if(   lattice->param.pressure_n_in[subs]
     || lattice->param.pressure_n_out[subs]
     || lattice->param.velocity_n_in[subs]
     || lattice->param.velocity_n_out[subs] )
  {
    if( !(   lattice->param.pressure_s_in[subs]
          || lattice->param.pressure_s_out[subs]
          || lattice->param.velocity_s_in[subs]
          || lattice->param.velocity_s_out[subs]))
    {
      // TODO: Need to prohibit flow boundaries on only one end of the domain?
      //process_exit(1);
    }
    else
    {
      lattice->periodic_y[subs] = 0;
    }
  }

  // South/North
  if(   lattice->param.pressure_s_in[subs]
     || lattice->param.pressure_s_out[subs]
     || lattice->param.velocity_s_in[subs]
     || lattice->param.velocity_s_out[subs] )
  {
    if( !(   lattice->param.pressure_n_in[subs]
          || lattice->param.pressure_n_out[subs]
          || lattice->param.velocity_n_in[subs]
          || lattice->param.velocity_n_out[subs]))
    {
      // TODO: Need to prohibit flow boundaries on only one end of the domain?
      //process_exit(1);
    }
    else
    {
      lattice->periodic_y[subs] = 0;
    }
  }

  // East/West
  if(   lattice->param.pressure_e_in[subs]
     || lattice->param.pressure_e_out[subs]
     || lattice->param.velocity_e_in[subs]
     || lattice->param.velocity_e_out[subs] )
  {
    if( !(   lattice->param.pressure_w_in[subs]
          || lattice->param.pressure_w_out[subs]
          || lattice->param.velocity_w_in[subs]
          || lattice->param.velocity_w_out[subs]))
    {
      // TODO: Need to prohibit flow boundaries on only one end of the domain?
      //process_exit(1);
    }
    else
    {
      lattice->periodic_x[subs] = 0;
    }
  }

  // West/East
  if(   lattice->param.pressure_w_in[subs]
     || lattice->param.pressure_w_out[subs]
     || lattice->param.velocity_w_in[subs]
     || lattice->param.velocity_w_out[subs] )
  {
    if( !(   lattice->param.pressure_e_in[subs]
          || lattice->param.pressure_e_out[subs]
          || lattice->param.velocity_e_in[subs]
          || lattice->param.velocity_e_out[subs]))
    {
      // TODO: Need to prohibit flow boundaries on only one end of the domain?
      //process_exit(1);
    }
    else
    {
      lattice->periodic_x[subs] = 0;
    }
  }

  if( lattice->param.pressure_n_in[subs] == 2)
  {
    // Read from file.
  }

#if SAY_HI
  printf("process_bcs() -- Bye!\n");
#endif /* SAY_HI */
  printf("\n");

} /* void process_bcs( lattice_ptr lattice, int subs) */
// }}}

//##############################################################################
// vim: foldmethod=marker:foldlevel=0
