//##############################################################################
//
// lbio.c
//
//  - Lattice Boltzmann I/O routines.
//
//  - Mainly, dump the data to files that can be read by Matlab.
//
//  - Also, output routines for facilitating debugging.
//
//  - Should have a routine that dumps a matlab script?
//

// Some compilers, e.g., VC++, don't have the usual round() function
// in their math library.  Alternatively, ROUND can be defined as
// ceil or floor or some other rounding function.  It is used in
// the below routines for converting the real number valued of
// quantities at a lattice node into integer RGB values for writing
// to BMP files.
#define ROUND floor

//#if SWAP_BYTE_ORDER || OSTYPE==darwin
#if SWAP_BYTE_ORDER
// Swap byte order.
#define ENDIAN2(w) ((((w)&0x00ff)<<8)|(((w)&0xff00)>>8))
#define ENDIAN4(w) ((((w)&0x000000ff)<<24)|(((w)&0xff000000)>>24)|(((w)&0x0000ff00)<<8)|(((w)&0x00ff0000)>>8))
#else /* !( SWAP_BYTE_ORDER) */
#define ENDIAN2(w) (w)
#define ENDIAN4(w) (w)
#endif /* SWAP_BYTE_ORDER */

#define X2N( x, d) (int)( (((double)d-1.)/(double)d)*floor( ((double)(d))*((double)(x))))

// O U T P U T   F R A M E  {{{
//##############################################################################
//void output_frame( lattice_ptr lattice)
//
void output_frame( lattice_ptr lattice)
{
  double s, u[2];
  double nu;
  int L;
#if VERBOSITY_LEVEL > 0
      printf("\n");
      printf( "========================================"
              "========================================\n");
      printf("Begin file I/O at time = %d, frame = %d.\n",
          lattice->time, lattice->frame);
      printf("\n");
#endif /* VERBOSITY_LEVEL > 0 */
      dump_frame_summary( lattice);
#if WRITE_MACRO_VAR_DAT_FILES
      dump_macro_vars( lattice, lattice->time);
#endif /* WRITE_MACRO_VAR_DAT_FILES */
#if WRITE_PDF_DAT_FILES
      dump_pdf( lattice, lattice->time);
#endif /* WRITE_PDF_DAT_FILES */
      if( lattice->param.dump_rho) { rho2bmp( lattice, lattice->time);}
      if( lattice->param.dump_u  ) { u2bmp( lattice, lattice->time);}
      if( lattice->param.dump_vor) { vor2bmp( lattice, lattice->time);}
#if NON_LOCAL_FORCES
      if( lattice->param.G != 0.)
      {
        if( lattice->param.dump_force) { force2bmp( lattice);}
      }
      if(    lattice->param.Gads[0] != 0.
          || lattice->param.Gads[1] != 0.)
      {
        if( lattice->param.dump_force) { sforce2bmp( lattice);}
      }
#endif /* NON_LOCAL_FORCES */
      slice( lattice);
#if WRITE_CHEN_DAT_FILES
      chen_output( lattice);
#endif /* WRITE_CHEN_DAT_FILES */
#if VERBOSITY_LEVEL > 0
      printf("\n");
      printf("File I/O done.\n");
      printf("--\n");
#endif /* VERBOSITY_LEVEL > 0 */

  nu = (1./3.)*(lattice->param.tau[0] - .5);
  L = lattice->param.length_scale;

  compute_ave_u( lattice, u, 0);
  s = sqrt( u[0]*u[0] + u[1]*u[1]);
  printf("p%02d, subs 0: Re = ux_ave*L/nu = %20.17f * %d / %20.17f = %20.17f\n",
    get_proc_id(lattice), u[0], L, nu, u[0]*L/nu );
  printf("p%02d, subs 0: Re = uy_ave*L/nu = %20.17f * %d / %20.17f = %20.17f\n",
    get_proc_id(lattice), u[1], L, nu, u[1]*L/nu );
  printf("p%02d, subs 0: Re = u_ave*L/nu  = %20.17f * %d / %20.17f = %20.17f\n",
    get_proc_id(lattice), s, L, nu, s*L/nu );

#if NUM_FLUID_COMPONENTS == 2
  compute_ave_u( lattice, u, 1);
  s = sqrt( u[0]*u[0] + u[1]*u[1]);
  printf("p%02d, subs 1: Re = ux_ave*L/nu = %20.17f * %d / %20.17f = %20.17f\n",
    get_proc_id(lattice), u[0], L, nu, u[0]*L/nu );
  printf("p%02d, subs 1: Re = uy_ave*L/nu = %20.17f * %d / %20.17f = %20.17f\n",
    get_proc_id(lattice), u[1], L, nu, u[1]*L/nu );
  printf("p%02d, subs 1: Re = u_ave*L/nu  = %20.17f * %d / %20.17f = %20.17f\n",
    get_proc_id(lattice), s, L, nu, s*L/nu );
#endif /* NUM_FLUID_COMPONENTS == 2 */

#if STORE_U_COMPOSITE
  compute_ave_upr( lattice, u);
  s = sqrt( u[0]*u[0] + u[1]*u[1]);
  printf("p%02d, eq:     Re = ux_ave*L/nu = %20.17f * %d / %20.17f = %20.17f\n",
    get_proc_id(lattice), u[0], L, nu, u[0]*L/nu );
  printf("p%02d, eq:     Re = uy_ave*L/nu = %20.17f * %d / %20.17f = %20.17f\n",
    get_proc_id(lattice), u[1], L, nu, u[1]*L/nu );
  printf("p%02d, eq:     Re = u_ave*L/nu  = %20.17f * %d / %20.17f = %20.17f\n",
    get_proc_id(lattice), s, L, nu, s*L/nu );
#endif /* STORE_U_COMPOSITE */

} /* void output_frame( lattice_ptr lattice) */
                                                                        // }}}
// D U M P   F R A M E   I N F O  {{{
//##############################################################################
// void dump_frame_info( struct lattice_struct *lattice)
//
void dump_frame_summary( struct lattice_struct *lattice)
{
  char   filename[1024];
  FILE   *o;
  double min_u[5], max_u[5], ave_u[5], flux[3];
  double min_rho,  max_rho,  ave_rho;
  double rho_ratio, u_x_ratio, u_y_ratio;
  int    subs;

 for( subs = 0; subs < NUM_FLUID_COMPONENTS; subs++)
 {
  if( is_incompressible( lattice)
      && annotate_incompressible_filenames( lattice))
  {
   sprintf( filename, "%s/frames%dx%di_subs%02d_proc%04d.dat", get_out_path(lattice),
       get_LX(lattice), get_LY(lattice), subs, get_proc_id(lattice));
  }
  else
  {
   sprintf( filename, "%s/frames%dx%d_subs%02d_proc%04d.dat", get_out_path(lattice),
       get_LX(lattice), get_LY(lattice), subs, get_proc_id(lattice));
  }
  // On the first timestep, make sure we start with a new file.
  if( lattice->time==0)
  {
      if( !( o = fopen(filename,"w+")))
      {
        printf("ERROR: fopen(\"%s\",\"w+\") = NULL.  Bye, bye!\n", filename);
        process_exit(1);
      }
      else
      {
        // Put a header on the file.
        fprintf( o, "\n");
        fprintf( o, "       time "
                    "        |j|          "
                    "        j_x          "
                    "        j_y          "
                    "    ave |u|          "
                    "   ave |u_x|         "
                    "   ave |u_y|         "
                    "    ave u_x          "
                    "    ave u_y          "
                    "    min |u|          "
                    "   min |u_x|         "
                    "   min |u_y|         "
                    "    min u_x          "
                    "    min u_y          "
                    "    max |u|          "
                    "   max |u_x|         "
                    "   max |u_y|         "
                    "    max u_x          "
                    "    max u_y          "
                    "  max/ave_x          "
                    "  max/ave_y          "
                    "    min rho          "
                    "    max rho          "
                    "    ave rho          "
                    "    max/ave          "
                    "\n");
        fprintf( o, " -----------"
                    " --------------------"
                    " --------------------"
                    " --------------------"
                    " --------------------"
                    " --------------------"
                    " --------------------"
                    " --------------------"
                    " --------------------"
                    " --------------------"
                    " --------------------"
                    " --------------------"
                    " --------------------"
                    " --------------------"
                    " --------------------"
                    " --------------------"
                    " --------------------"
                    " --------------------"
                    " --------------------"
                    " --------------------"
                    " --------------------"
                    " --------------------"
                    " --------------------"
                    " --------------------"
                    " --------------------"
                    "\n");
        fclose(o);
      }
  }
  if( !( o = fopen(filename,"a+")))
  {
    printf("ERROR: fopen(\"%s\",\"a+\") = NULL.  Bye, bye!\n", filename);
    process_exit(1);
  }
  compute_min_u_all( lattice, min_u, subs);
  compute_max_u_all( lattice, max_u, subs);
  compute_ave_u_all( lattice, ave_u, subs);
  compute_min_rho( lattice, &min_rho, subs);
  compute_max_rho( lattice, &max_rho, subs);
  compute_ave_rho( lattice, &ave_rho, subs);
  rho_ratio = ( ave_rho  != 0.) ? ( max_rho /ave_rho ):( 1.);
  u_x_ratio = ( ave_u[1] != 0.) ? ( max_u[1]/ave_u[0]):( 1.);
  u_y_ratio = ( ave_u[2] != 0.) ? ( max_u[2]/ave_u[1]):( 1.);

  compute_flux( lattice, flux, subs);

  fprintf( o,
    "%12d "
    "%20.17f %20.17f %20.17f %20.17f %20.17f %20.17f %20.17f %20.17f %20.17f "
    "%20.17f %20.17f %20.17f %20.17f %20.17f %20.17f %20.17f %20.17f %20.17f "
    "%20.17f %20.17f %20.17f %20.17f %20.17f %20.17f\n",
    lattice->time,
    flux [0], flux [1], flux [2],
    ave_u[0], ave_u[1], ave_u[2], ave_u[3], ave_u[4],
    min_u[0], min_u[1], min_u[2], min_u[3], min_u[4],
    max_u[0], max_u[1], max_u[2], max_u[3], max_u[4],
    (u_x_ratio<=9999.)?(u_x_ratio):(9999.),
    (u_y_ratio<=9999.)?(u_y_ratio):(9999.),
    min_rho,
    max_rho,
    ave_rho,
    (rho_ratio<=9999.)?(rho_ratio):(9999.) );

  fclose(o);

#if VERBOSITY_LEVEL > 0
  printf("dump_frame_info() -- Wrote file \"%s\"\n", filename);
#endif /* VERBOSITY_LEVEL > 0 */

#if VERBOSITY_LEVEL > 0
  printf("dump_frame_info() -- frame = %d/%d = %d\n",
      lattice->time,
      lattice->param.FrameRate,
      (int)((double)lattice->time/(double)lattice->param.FrameRate));
#endif /* VERBOSITY_LEVEL > 0 */
 }

} /* void dump_frame_info( struct lattice_struct *lattice) */
                                                                        // }}}
// D U M P   M A C R O S C O P I C  {{{
//##############################################################################
// void dump_macro_vars( struct lattice_struct *lattice)
//
//  - Output the macro_vars variables to files.
//
void dump_macro_vars( struct lattice_struct *lattice, int time)
{
  char   filename[1024];
  FILE   *o, *o_u, *o_rho, *o_ux, *o_uy, *o_upr, *o_upr_x, *o_upr_y;
  int    *node_ptr;
  int    n;
  double *macro_vars_ptr;
  double *upr;
  int    frame;
#if WRITE_MACRO_VAR_DAT_FILES || WRITE_PDF_DAT_FILES || WRITE_RHO_AND_U_TO_TXT
  int    i, j;
#endif
  double min_u[2], max_u[2],  ave_u[2];
  double min_rho, max_rho,   ave_rho;
  double rho_ratio, u_x_ratio, u_y_ratio;
  int    subs;

  frame = (int)((double)lattice->time/(double)lattice->param.FrameRate);

 for( subs = 0; subs < NUM_FLUID_COMPONENTS; subs++)
 {

  // W R I T E   R H O   A N D   U
  //
  //  - Write the density and velocity values at the active nodes to
  //    the rho and u dat files.
  //
  if(    is_incompressible( lattice)
      && annotate_incompressible_filenames( lattice))
  {
    sprintf( filename, "%s/rho%dx%di_frame%04d_subs%02d_proc%04d.dat", get_out_path(lattice),
        get_LX( lattice), get_LY( lattice), frame, subs, get_proc_id(lattice));
  }
  else
  {
    sprintf( filename, "%s/rho%dx%d_frame%04d_subs%02d_proc%04d.dat", get_out_path(lattice),
        get_LX( lattice), get_LY( lattice), frame, subs, get_proc_id(lattice));
  }
  if( !( o_rho = fopen( filename, "w+")))
  {
    printf("ERROR: fopen( \"%s\", \"w+\") = NULL.  Bye, bye!\n", filename);
    process_exit(1);
  }

  sprintf( filename, "%s/u%dx%d_frame%04d_subs%02d_proc%04d.dat", get_out_path(lattice),
      get_LX( lattice), get_LY( lattice), frame, subs, get_proc_id(lattice));
  if( !( o_u = fopen( filename, "w+")))
  {
    printf("ERROR: fopen( \"%s\", \"w+\") = NULL.  Bye, bye!\n", filename);
    process_exit(1);
  }

#if STORE_U_COMPOSITE
  sprintf( filename, "%s/upr%dx%d_frame%04d_subs%02d_proc%04d.dat", get_out_path(lattice),
      get_LX( lattice), get_LY( lattice), frame, subs, get_proc_id(lattice));
  if( !( o_upr = fopen( filename, "w+")))
  {
    printf("ERROR: fopen( \"%s\", \"w+\") = NULL.  Bye, bye!\n", filename);
    process_exit(1);
  }

  upr = lattice->upr[0].u;

#endif /* STORE_U_COMPOSITE */

  macro_vars_ptr = &( lattice->macro_vars[subs][0].rho);

  for( n=0; n<lattice->NumNodes; n++)
  {

    if( is_solid_node( lattice, subs, n))
    {
      fprintf( o_rho, "%20.17f\n", 0.);
      *macro_vars_ptr++;

      fprintf( o_u, "%20.17f ", 0.);
      *macro_vars_ptr++;

      fprintf( o_u, "%20.17f\n", 0.);
      *macro_vars_ptr++;

#if STORE_U_COMPOSITE
      fprintf( o_upr, "%20.17f ", 0.);
      *upr++;
      fprintf( o_upr, "%20.17f\n", 0.);
      *upr++;
#endif /* STORE_U_COMPOSITE */
    }
    else
    {
      fprintf( o_rho, "%20.17f\n", *macro_vars_ptr++);

      fprintf( o_u, "%20.17f ", *macro_vars_ptr++);

      fprintf( o_u, "%20.17f\n", *macro_vars_ptr++);

#if STORE_U_COMPOSITE
      fprintf( o_upr, "%20.17f ", *upr++);
      fprintf( o_upr, "%20.17f\n", *upr++);
#endif /* STORE_U_COMPOSITE */
    }

  }

  fclose(o_u);
#if VERBOSITY_LEVEL > 0
  sprintf( filename, "%s/u%dx%d_frame%04d_subs%02d_proc%04d.dat", get_out_path(lattice),
      get_LX(lattice), get_LY(lattice), frame, subs, get_proc_id(lattice));
#endif /* VERBOSITY_LEVEL > 0 */
  printf("dump_macro_vars() -- Wrote file \"%s\"\n", filename);
  fclose(o_rho);
#if VERBOSITY_LEVEL > 0
  sprintf( filename, "%s/rho%dx%d_frame%04d_subs%02d_proc%04d.dat", get_out_path(lattice),
      get_LX(lattice), get_LY(lattice), frame, subs, get_proc_id(lattice));
#endif /* VERBOSITY_LEVEL > 0 */
  printf("dump_macro_vars() -- Wrote file \"%s\"\n", filename);
#if STORE_U_COMPOSITE
  fclose(o_upr);
#if VERBOSITY_LEVEL > 0
  sprintf( filename, "%s/upr%dx%d_frame%04d_subs%02d_proc%04d.dat", get_out_path(lattice),
      get_LX(lattice), get_LY(lattice), frame, subs, get_proc_id(lattice));
#endif /* VERBOSITY_LEVEL > 0 */
  printf("dump_macro_vars() -- Wrote file \"%s\"\n", filename);
#endif /* STORE_U_COMPOSITE */

#if WRITE_RHO_AND_U_TO_TXT
  // NOTE: This is very inefficient.  But it's only intended
  // for debugging purposes on small problems.
  sprintf( filename, "%s/rho%dx%d_frame%04d_subs%02d_proc%04d.txt", get_out_path(lattice),
      get_LX(lattice), get_LY(lattice), frame, subs, get_proc_id(lattice));
  if( !( o_rho = fopen( filename, "w+")))
  {
    printf("ERROR: fopen( \"%s\", \"w+\") = NULL.  Bye, bye!\n", filename);
    process_exit(1);
  }
  sprintf( filename, "%s/ux%dx%d_frame%04d_subs%02d_proc%04d.txt", get_out_path(lattice),
      get_LX(lattice), get_LY(lattice), frame, subs, get_proc_id(lattice));
  if( !( o_ux = fopen( filename, "w+")))
  {
    printf("ERROR: fopen( \"%s\", \"w+\") = NULL.  Bye, bye!\n", filename);
    process_exit(1);
  }
  sprintf( filename, "%s/uy%dx%d_frame%04d_subs%02d_proc%04d.txt", get_out_path(lattice),
      get_LX(lattice), get_LY(lattice), frame, subs, get_proc_id(lattice));
  if( !( o_uy = fopen( filename, "w+")))
  {
    printf("ERROR: fopen( \"%s\", \"w+\") = NULL.  Bye, bye!\n", filename);
    process_exit(1);
  }
#if STORE_U_COMPOSITE
  sprintf( filename, "%s/upr_x%dx%d_frame%04d_subs%02d_proc%04d.txt", get_out_path(lattice),
      get_LX(lattice), get_LY(lattice), frame, subs, get_proc_id(lattice));
  if( !( o_upr_x = fopen( filename, "w+")))
  {
    printf("ERROR: fopen( \"%s\", \"w+\") = NULL.  Bye, bye!\n", filename);
    process_exit(1);
  }
  sprintf( filename, "%s/upr_y%dx%d_frame%04d_subs%02d_proc%04d.txt", get_out_path(lattice),
      get_LX(lattice), get_LY(lattice), frame, subs, get_proc_id(lattice));
  if( !( o_upr_y = fopen( filename, "w+")))
  {
    printf("ERROR: fopen( \"%s\", \"w+\") = NULL.  Bye, bye!\n", filename);
    process_exit(1);
  }
#endif /* STORE_U_COMPOSITE */
  for( j=get_LY(lattice)-1; j>=0; j--)
  {
    n = j*get_LX(lattice);
    for( i=0; i<get_LX(lattice); i++, n++)
    {
      if( is_not_solid_node(lattice, subs, n))
      {
        fprintf( o_rho, "%10.8f ", lattice->macro_vars[subs][n].rho);
        fprintf( o_ux,  "%10.8f ", lattice->macro_vars[subs][n].u[0]);
        fprintf( o_uy,  "%10.8f ", lattice->macro_vars[subs][n].u[1]);
      }
      else
      {
        fprintf( o_rho, "---------- ");
        fprintf( o_ux,  "---------- ");
        fprintf( o_uy,  "---------- ");
      }
#if STORE_U_COMPOSITE
      fprintf( o_upr_x,  "%10.8f ", lattice->upr[n].u[0]);
      fprintf( o_upr_y,  "%10.8f ", lattice->upr[n].u[1]);
#endif /* STORE_U_COMPOSITE */
      if( n==lattice->NumNodes)
      {
        fprintf( o_rho, "%10.8f ", 0.);
        fprintf( o_ux, "%10.8f ", 0.);
        fprintf( o_uy, "%10.8f ", 0.);
#if STORE_U_COMPOSITE
        fprintf( o_upr_x, "%10.8f ", 0.);
        fprintf( o_upr_y, "%10.8f ", 0.);
#endif /* STORE_U_COMPOSITE */
      }
    }
    fprintf( o_rho, "\n");
    fprintf( o_ux, "\n");
    fprintf( o_uy, "\n");
#if STORE_U_COMPOSITE
    fprintf( o_upr_x, "\n");
    fprintf( o_upr_y, "\n");
#endif /* STORE_U_COMPOSITE */
  }

  fclose(o_ux);
#if VERBOSITY_LEVEL > 0
  sprintf( filename, "%s/ux%dx%d_frame%04d_subs%02d_proc%04d.txt", get_out_path(lattice),
      get_LX(lattice), get_LY(lattice), frame, subs, get_proc_id(lattice));
#endif /* VERBOSITY_LEVEL > 0 */
  printf("dump_macro_vars() -- Wrote file \"%s\"\n", filename);
  fclose(o_uy);
#if VERBOSITY_LEVEL > 0
  sprintf( filename, "%s/uy%dx%d_frame%04d_subs%02d_proc%04d.txt", get_out_path(lattice),
      get_LX(lattice), get_LY(lattice), frame, subs, get_proc_id(lattice));
#endif /* VERBOSITY_LEVEL > 0 */
  printf("dump_macro_vars() -- Wrote file \"%s\"\n", filename);
  fclose(o_rho);
#if VERBOSITY_LEVEL > 0
  sprintf( filename, "%s/rho%dx%d_frame%04d_subs%02d_proc%04d.txt", get_out_path(lattice),
      get_LX(lattice), get_LY(lattice), frame, subs, get_proc_id(lattice));
#endif /* VERBOSITY_LEVEL > 0 */
  printf("dump_macro_vars() -- Wrote file \"%s\"\n", filename);
#if STORE_U_COMPOSITE
  fclose(o_upr_x);
#if VERBOSITY_LEVEL > 0
  sprintf( filename, "%s/upr_x%dx%d_frame%04d_subs%02d_proc%04d.txt", get_out_path(lattice),
      get_LX(lattice), get_LY(lattice), frame, subs, get_proc_id(lattice));
#endif /* VERBOSITY_LEVEL > 0 */
  printf("dump_macro_vars() -- Wrote file \"%s\"\n", filename);
  fclose(o_upr_y);
#if VERBOSITY_LEVEL > 0
  sprintf( filename, "%s/upr_y%dx%d_frame%04d_subs%02d_proc%04d.txt", get_out_path(lattice),
      get_LX(lattice), get_LY(lattice), frame, subs, get_proc_id(lattice));
#endif /* VERBOSITY_LEVEL > 0 */
  printf("dump_macro_vars() -- Wrote file \"%s\"\n", filename);
#endif /* STORE_U_COMPOSITE */

#endif /* WRITE_RHO_AND_U_TO_TXT */

 } /* for( subs = 0; subs < NUM_FLUID_COMPONENTS; subs++) */

#if WRITE_MACRO_VAR_DAT_FILES || WRITE_PDF_DAT_FILES
  sprintf( filename, "%s/obst%dx%d_proc%04d.dat", get_out_path(lattice),
      get_LX(lattice), get_LY(lattice), get_proc_id(lattice));
  if( !( o = fopen( filename, "w+")))
  {
    printf("ERROR: fopen( \"%s\", \"w+\") = NULL.  Bye, bye!\n", filename);
    process_exit(1);
  }
  for( j=0; j<get_LY( lattice); j++)
  {
    for( i=0; i<get_LX( lattice); i++)
    {
      fprintf( o, " %d", is_solid_node( lattice, /*subs*/0, IJ2N( i, j)));
    }
    fprintf( o, "\n");
  }
  fclose(o);
#endif

} /* void dump_macro_vars( struct lattice_struct *lattice, int time) */
                                                                        // }}}
#if 1
// R E A D   M A C R O S C O P I C  {{{
//##############################################################################
// void read_macro_vars( struct lattice_struct *lattice)
//
//  - Read the macro_vars variables from files.
//
void read_macro_vars( struct lattice_struct *lattice, int time)
{
  char   filename[1024];
  FILE   *in, *rho_in, *u_in;
  int    *node_ptr;
  int    n;
  double *macro_vars_ptr;
  int    frame;
  double max_u[2],  ave_u[2];
  double max_rho,   ave_rho;
  double rho_ratio, u_x_ratio, u_y_ratio;
  int    subs;

 for( subs=0; subs<NUM_FLUID_COMPONENTS; subs++)
 {

  frame = (int)((double)time/(double)lattice->param.FrameRate);
#if VERBOSITY_LEVEL > 0
  printf("read_macro_vars() -- frame = %d/%d = %d\n",
      time,
      lattice->param.FrameRate,
      frame);
#endif /* VERBOSITY_LEVEL > 0 */

  // R E A D   R H O   A N D   U
  //
  //  - Read the density and velocity values at the active nodes to
  //    the rho and u dat files.
  //
  sprintf( filename, "%s/rho%dx%d_frame%04d_subs%02d_proc%04d.dat", get_out_path(lattice),
      get_LX(lattice), get_LY(lattice), frame, subs, get_proc_id(lattice));
  if( !( rho_in = fopen( filename, "r+")))
  {
    printf("ERROR: fopen( \"%s\", \"r+\") = NULL.  Bye, bye!\n", filename);
    process_exit(1);
  }

  sprintf( filename, "%s/u%dx%d_frame%04d_subs%02d_proc%04d.dat", get_out_path(lattice),
      get_LX(lattice), get_LY(lattice), frame, subs, get_proc_id(lattice));
  if( !( u_in = fopen( filename, "r+")))
  {
    printf("ERROR: fopen( \"%s\", \"r+\") = NULL.  Bye, bye!\n", filename);
    process_exit(1);
  }

  macro_vars_ptr = &( lattice->macro_vars[subs][0].rho);

  for( n=0; n<lattice->NumNodes; n++)
  {

    fscanf( rho_in, "%lf\n", macro_vars_ptr++);

    fscanf( u_in,   "%lf ",  macro_vars_ptr++);

    fscanf( u_in,   "%lf\n", macro_vars_ptr++);

  }

  fclose(u_in);
#if VERBOSITY_LEVEL > 0
  sprintf( filename, "%s/u%dx%d_frame%04d_subs%02d_proc%04d.dat", get_out_path(lattice),
      get_LX(lattice), get_LY(lattice), frame, subs, get_proc_id(lattice));
#endif /* VERBOSITY_LEVEL > 0 */
  printf("read_macro_vars() -- Read file \"%s\"\n", filename);
  fclose(rho_in);
#if VERBOSITY_LEVEL > 0
  sprintf( filename, "%s/rho%dx%d_frame%04d_subs%02d_proc%04d.dat", get_out_path(lattice),
      get_LX(lattice), get_LY(lattice), frame, subs, get_proc_id(lattice));
#endif /* VERBOSITY_LEVEL > 0 */
  printf("read_macro_vars() -- Read file \"%s\"\n", filename);

 } /* for( subs=0; subs<NUM_FLUID_COMPONENTS; subs++) */

} /* void read_macro_vars( struct lattice_struct *lattice, int time) */
                                                                        // }}}
#endif
// D U M P   P D F  {{{
//##############################################################################
// void dump_pdf( struct lattice_struct *lattice, int time)
//
//  - Output the particle distribution functions to a text file.
//
//  - This is useful mainly for debugging with small problems.
//
void dump_pdf( struct lattice_struct *lattice, int time)
{
  char   filename[1024];
  FILE   *o_feq,
         *o_f,
         *o_ftemp,
         *o_fdiff;
  double *fdiff;
  double *fptr,
         *end_ptr;
  bc_ptr bc;
  int    frame;
  int    subs;
  int    a;
  int    i, j, n;
#if WRITE_PDF_TO_TXT
  double max_feq,
         max_f,
         max_ftemp,
         max_fdiff;
  double max_feq0,
         max_f0,
         min_f0,
         max_ftemp0,
         max_fdiff0;
  double max_feq1234,
         max_f1234,
         max_ftemp1234,
         max_fdiff1234;
  double max_feq5678,
         max_f5678,
         max_ftemp5678,
         max_fdiff5678;
  double the_max;
  int    s;
#endif /* WRITE_PDF_TO_TXT */

 fdiff = (double*)malloc( 9*get_NumNodes(lattice)*sizeof(double));

 for( subs=0; subs<NUM_FLUID_COMPONENTS; subs++)
 {

  frame = time/lattice->param.FrameRate;

  sprintf( filename, "%s/feq%dx%d_frame%04d_subs%02d_proc%04d.dat", get_out_path(lattice),
      get_LX(lattice), get_LY(lattice), frame, subs, get_proc_id(lattice));
  if( !( o_feq = fopen( filename, "w+")))
  {
    printf("ERROR: fopen( \"%s\", \"w+\") = NULL.  Bye, bye!\n", filename);
    process_exit(1);
  }

  sprintf( filename, "%s/f%dx%d_frame%04d_subs%02d_proc%04d.dat", get_out_path(lattice),
      get_LX(lattice), get_LY(lattice), frame, subs, get_proc_id(lattice));
  if( !( o_f = fopen( filename, "w+")))
  {
    printf("ERROR: fopen( \"%s\", \"w+\") = NULL.  Bye, bye!\n", filename);
    process_exit(1);
  }

  sprintf( filename, "%s/ftemp%dx%d_frame%04d_subs%02d_proc%04d.dat", get_out_path(lattice),
      get_LX(lattice), get_LY(lattice), frame, subs, get_proc_id(lattice));
  if( !( o_ftemp = fopen( filename, "w+")))
  {
    printf("ERROR: fopen( \"%s\", \"w+\") = NULL.  Bye, bye!\n", filename);
    process_exit(1);
  }

  sprintf( filename, "%s/fdiff%dx%d_frame%04d_subs%02d_proc%04d.dat", get_out_path(lattice),
      get_LX(lattice), get_LY(lattice), frame, subs, get_proc_id(lattice));
  if( !( o_fdiff = fopen( filename, "w+")))
  {
    printf("ERROR: fopen( \"%s\", \"w+\") = NULL.  Bye, bye!\n", filename);
    process_exit(1);
  }

  fptr  = lattice->pdf[subs][0].feq;
  for( n=0; n<get_NumNodes(lattice); n++)
  {
    for( a=0; a<9; a++)
    {
      fdiff[ 9*n + a] =
        fptr[ 27*n +  9 + a] // f
      - fptr[ 27*n + 18 + a] // ftemp
      ;
    }
  }

  bc      =   lattice->bc[subs];
  fptr    =   lattice->pdf[subs][0].feq;
  end_ptr = &(lattice->pdf[subs][ lattice->NumNodes-1].ftemp[8]) + 1;
  while( fptr!=end_ptr)
  {
    if( COMPUTE_ON_SOLIDS || !( bc++->bc_type & BC_SOLID_NODE))
    {
      fprintf( o_feq  , "%10.17f ", *fptr++);
      fprintf( o_feq  , "%10.17f ", *fptr++);
      fprintf( o_feq  , "%10.17f ", *fptr++);
      fprintf( o_feq  , "%10.17f ", *fptr++);
      fprintf( o_feq  , "%10.17f ", *fptr++);
      fprintf( o_feq  , "%10.17f ", *fptr++);
      fprintf( o_feq  , "%10.17f ", *fptr++);
      fprintf( o_feq  , "%10.17f ", *fptr++);
      fprintf( o_feq  , "%10.17f ", *fptr++);
    }
    else
    {
      fprintf( o_feq  , "%10.17f ", 0.);
      fprintf( o_feq  , "%10.17f ", 0.);
      fprintf( o_feq  , "%10.17f ", 0.);
      fprintf( o_feq  , "%10.17f ", 0.);
      fprintf( o_feq  , "%10.17f ", 0.);
      fprintf( o_feq  , "%10.17f ", 0.);
      fprintf( o_feq  , "%10.17f ", 0.);
      fprintf( o_feq  , "%10.17f ", 0.);
      fprintf( o_feq  , "%10.17f ", 0.);
      fptr+=9;
    }
    fprintf( o_feq  , "\n");

    fprintf( o_f    , "%10.17f ", *fptr++);
    fprintf( o_f    , "%10.17f ", *fptr++);
    fprintf( o_f    , "%10.17f ", *fptr++);
    fprintf( o_f    , "%10.17f ", *fptr++);
    fprintf( o_f    , "%10.17f ", *fptr++);
    fprintf( o_f    , "%10.17f ", *fptr++);
    fprintf( o_f    , "%10.17f ", *fptr++);
    fprintf( o_f    , "%10.17f ", *fptr++);
    fprintf( o_f    , "%10.17f ", *fptr++);
    fprintf( o_f    , "\n");

    fprintf( o_ftemp, "%10.17f ", *fptr++);
    fprintf( o_ftemp, "%10.17f ", *fptr++);
    fprintf( o_ftemp, "%10.17f ", *fptr++);
    fprintf( o_ftemp, "%10.17f ", *fptr++);
    fprintf( o_ftemp, "%10.17f ", *fptr++);
    fprintf( o_ftemp, "%10.17f ", *fptr++);
    fprintf( o_ftemp, "%10.17f ", *fptr++);
    fprintf( o_ftemp, "%10.17f ", *fptr++);
    fprintf( o_ftemp, "%10.17f ", *fptr++);
    fprintf( o_ftemp, "\n");

  } /* while( fptr!=end_ptr) */

  fclose( o_feq);
  fclose( o_f);
  fclose( o_ftemp);
  fclose( o_fdiff);

 } /* for( subs=0; subs<NUM_FLUID_COMPONENTS; subs++) */

#if WRITE_PDF_TO_TXT
 for( subs=0; subs<NUM_FLUID_COMPONENTS; subs++)
 {

  frame = time/lattice->param.FrameRate;

  sprintf( filename, "%s/feq%dx%d_frame%04d_subs%02d_proc%04d.txt", get_out_path(lattice),
      get_LX(lattice), get_LY(lattice), frame, subs, get_proc_id(lattice));
  if( !( o_feq = fopen( filename, "w+")))
  {
    printf("ERROR: fopen( \"%s\", \"w+\") = NULL.  Bye, bye!\n", filename);
    process_exit(1);
  }

  sprintf( filename, "%s/f%dx%d_frame%04d_subs%02d_proc%04d.txt", get_out_path(lattice),
      get_LX(lattice), get_LY(lattice), frame, subs, get_proc_id(lattice));
  if( !( o_f = fopen( filename, "w+")))
  {
    printf("ERROR: fopen( \"%s\", \"w+\") = NULL.  Bye, bye!\n", filename);
    process_exit(1);
  }

  sprintf( filename, "%s/ftemp%dx%d_frame%04d_subs%02d_proc%04d.txt", get_out_path(lattice),
      get_LX(lattice), get_LY(lattice), frame, subs, get_proc_id(lattice));
  if( !( o_ftemp = fopen( filename, "w+")))
  {
    printf("ERROR: fopen( \"%s\", \"w+\") = NULL.  Bye, bye!\n", filename);
    process_exit(1);
  }

  sprintf( filename, "%s/fdiff%dx%d_frame%04d_subs%02d_proc%04d.txt", get_out_path(lattice),
      get_LX(lattice), get_LY(lattice), frame, subs, get_proc_id(lattice));
  if( !( o_fdiff = fopen( filename, "w+")))
  {
    printf("ERROR: fopen( \"%s\", \"w+\") = NULL.  Bye, bye!\n", filename);
    process_exit(1);
  }

  fptr  = lattice->pdf[subs][0].feq;
  for( n=0; n<get_NumNodes(lattice); n++)
  {
    for( a=0; a<9; a++)
    {
      fdiff[ 9*n + a] =
        fptr[ 27*n +  9 + a] // f
      - fptr[ 27*n + 18 + a] // ftemp
      ;
    }
  }

  fprintf( o_feq  , "+");
  fprintf( o_f    , "+");
  fprintf( o_ftemp, "+");
  fprintf( o_fdiff, "+");
  for( i=0; i<get_LX(lattice); i++)
  {
    fprintf( o_feq  , "----");
    fprintf( o_feq  , "----");
    fprintf( o_feq  , "---+");
    fprintf( o_f    , "----");
    fprintf( o_f    , "----");
    fprintf( o_f    , "---+");
    fprintf( o_ftemp, "----");
    fprintf( o_ftemp, "----");
    fprintf( o_ftemp, "---+");
    fprintf( o_fdiff, "----");
    fprintf( o_fdiff, "----");
    fprintf( o_fdiff, "---+");
  }
  fprintf( o_feq  , "\n");
  fprintf( o_f    , "\n");
  fprintf( o_ftemp, "\n");
  fprintf( o_fdiff, "\n");

  if( /*scale_f_vals*/ 0)
  {
   compute_max_f( lattice, &(lattice->pdf[subs][0].feq[0]), &max_feq, subs);
   compute_max_f0( lattice, &(lattice->pdf[subs][0].feq[0]), &max_feq0, subs);
   compute_max_f1234(lattice,&(lattice->pdf[subs][0].feq[0]),&max_feq1234, subs);
   compute_max_f5678(lattice,&(lattice->pdf[subs][0].feq[0]),&max_feq5678, subs);

   compute_max_f( lattice, &(lattice->pdf[subs][0].f[0]), &max_f, subs);
   compute_max_f0( lattice, &(lattice->pdf[subs][0].f[0]), &max_f0, subs);
   if( max_f0 == 0.) { max_f0 = 1.;}
   compute_max_f1234(lattice,&(lattice->pdf[subs][0].f[0]),&max_f1234, subs);
   compute_max_f5678(lattice,&(lattice->pdf[subs][0].f[0]),&max_f5678, subs);

   compute_max_f( lattice, &(lattice->pdf[subs][0].ftemp[0]), &max_ftemp, subs);
   compute_max_f0( lattice,&(lattice->pdf[subs][0].ftemp[0]), &max_ftemp0, subs);
 compute_max_f1234(lattice,&(lattice->pdf[subs][0].ftemp[0]),&max_ftemp1234,subs);
 compute_max_f5678(lattice,&(lattice->pdf[subs][0].ftemp[0]),&max_ftemp5678,subs);

   compute_max_f(    lattice, fdiff, &max_fdiff,     subs);
   if( max_fdiff == 0.) { max_fdiff = 1.;}
   compute_max_f0(   lattice, fdiff, &max_fdiff0,    subs);
   if( max_fdiff0 == 0.) { max_fdiff0 = 1.;}
   compute_max_f1234(lattice, fdiff, &max_fdiff1234, subs);
   if( max_fdiff1234 == 0.) { max_fdiff1234 = 1.;}
   compute_max_f5678(lattice, fdiff, &max_fdiff5678, subs);
   if( max_fdiff5678 == 0.) { max_fdiff5678 = 1.;}

   s = 1000;
  }
  else
  {
    the_max = 1.;
    max_feq = the_max;
    max_feq0 = the_max;
    max_feq1234 = the_max;
    max_feq5678 = the_max;
    max_f = the_max;
    max_f0 = the_max;
    max_f1234 = the_max;
    max_f5678 = the_max;
    max_ftemp = the_max;
    max_ftemp0 = the_max;
    max_ftemp1234 = the_max;
    max_ftemp5678 = the_max;
    max_fdiff = the_max;
    max_fdiff0 = the_max;
    max_fdiff1234 = the_max;
    max_fdiff5678 = the_max;
    s = 100;
  }

  for( j=get_LY(lattice)-1; j>=0; j--)
  {
    n = j*get_LX(lattice);
    fprintf( o_feq  , "|");
    fprintf( o_f    , "|");
    fprintf( o_ftemp, "|");
    fprintf( o_fdiff, "|");
    for( i=0; i<get_LX(lattice); i++, n++)
    {
#if 0
fprintf(o_feq,  "%3d ",X2N(lattice->pdf[subs][n].feq[6]  /max_feq5678  ,s));
fprintf(o_feq,  "%3d ",X2N(lattice->pdf[subs][n].feq[2]  /max_feq1234  ,s));
fprintf(o_feq,  "%3d|",X2N(lattice->pdf[subs][n].feq[5]  /max_feq5678  ,s));
fprintf(o_f,    "%3d ",X2N(lattice->pdf[subs][n].f[6]    /max_f5678    ,s));
fprintf(o_f,    "%3d ",X2N(lattice->pdf[subs][n].f[2]    /max_f1234    ,s));
fprintf(o_f,    "%3d|",X2N(lattice->pdf[subs][n].f[5]    /max_f5678    ,s));
fprintf(o_ftemp,"%3d ",X2N(lattice->pdf[subs][n].ftemp[6]/max_ftemp5678,s));
fprintf(o_ftemp,"%3d ",X2N(lattice->pdf[subs][n].ftemp[2]/max_ftemp1234,s));
fprintf(o_ftemp,"%3d|",X2N(lattice->pdf[subs][n].ftemp[5]/max_ftemp5678,s));
fprintf(o_fdiff,"%3d ",X2N(fdiff[9*n+6]/max_fdiff5678,s));
fprintf(o_fdiff,"%3d ",X2N(fdiff[9*n+2]/max_fdiff1234,s));
fprintf(o_fdiff,"%3d|",X2N(fdiff[9*n+5]/max_fdiff5678,s));
#else
fprintf(o_feq,  "%.15f ", lattice->pdf[subs][n].feq[6]  );
fprintf(o_feq,  "%.15f ", lattice->pdf[subs][n].feq[2]  );
fprintf(o_feq,  "%.15f|", lattice->pdf[subs][n].feq[5]  );
fprintf(o_f,    "%.15f ", lattice->pdf[subs][n].f[6]    );
fprintf(o_f,    "%.15f ", lattice->pdf[subs][n].f[2]    );
fprintf(o_f,    "%.15f|", lattice->pdf[subs][n].f[5]    );
fprintf(o_ftemp,"%.15f ", lattice->pdf[subs][n].ftemp[6]);
fprintf(o_ftemp,"%.15f ", lattice->pdf[subs][n].ftemp[2]);
fprintf(o_ftemp,"%.15f|", lattice->pdf[subs][n].ftemp[5]);
fprintf(o_fdiff,"%.15f ", fdiff[9*n+6]                  );
fprintf(o_fdiff,"%.15f ", fdiff[9*n+2]                  );
fprintf(o_fdiff,"%.15f|", fdiff[9*n+5]                  );
#endif
    } /* for( i=0; i<get_LX(lattice); i++, n++) */

    fprintf( o_feq,  "\n");
    fprintf( o_f,    "\n");
    fprintf( o_ftemp,"\n");
    fprintf( o_fdiff,"\n");

    n = j*get_LX(lattice);
    fprintf( o_feq  , "|");
    fprintf( o_f    , "|");
    fprintf( o_ftemp, "|");
    fprintf( o_fdiff, "|");
    for( i=0; i<get_LX(lattice); i++, n++)
    {
#if 0
fprintf(o_feq,  "%3d ",X2N( lattice->pdf[subs][n].feq[3]  /max_feq1234  ,s));
fprintf(o_feq,  "%3d ",X2N( lattice->pdf[subs][n].feq[0]  /max_feq0     ,s));
fprintf(o_feq,  "%3d|",X2N( lattice->pdf[subs][n].feq[1]  /max_feq1234  ,s));
fprintf(o_f,    "%3d ",X2N( lattice->pdf[subs][n].f[3]    /max_f1234    ,s));
fprintf(o_f,    "%3d ",X2N( lattice->pdf[subs][n].f[0]    /max_f0       ,s));
fprintf(o_f,    "%3d|",X2N( lattice->pdf[subs][n].f[1]    /max_f1234    ,s));
fprintf(o_ftemp,"%3d ",X2N( lattice->pdf[subs][n].ftemp[3]/max_ftemp1234,s));
fprintf(o_ftemp,"%3d ",X2N( lattice->pdf[subs][n].ftemp[0]/max_ftemp0   ,s));
fprintf(o_ftemp,"%3d|",X2N( lattice->pdf[subs][n].ftemp[1]/max_ftemp1234,s));
fprintf(o_fdiff,"%3d ",X2N( fdiff[9*n+3]/max_fdiff1234,s));
fprintf(o_fdiff,"%3d ",X2N( fdiff[9*n+0]/max_fdiff0   ,s));
fprintf(o_fdiff,"%3d|",X2N( fdiff[9*n+1]/max_fdiff1234,s));
#else
fprintf(o_feq,  "%.15f ", lattice->pdf[subs][n].feq[3]  );
fprintf(o_feq,  "%.15f ", lattice->pdf[subs][n].feq[0]  );
fprintf(o_feq,  "%.15f|", lattice->pdf[subs][n].feq[1]  );
fprintf(o_f,    "%.15f ", lattice->pdf[subs][n].f[3]    );
fprintf(o_f,    "%.15f ", lattice->pdf[subs][n].f[0]    );
fprintf(o_f,    "%.15f|", lattice->pdf[subs][n].f[1]    );
fprintf(o_ftemp,"%.15f ", lattice->pdf[subs][n].ftemp[3]);
fprintf(o_ftemp,"%.15f ", lattice->pdf[subs][n].ftemp[0]);
fprintf(o_ftemp,"%.15f|", lattice->pdf[subs][n].ftemp[1]);
fprintf(o_fdiff,"%.15f ", fdiff[9*n+3]                  );
fprintf(o_fdiff,"%.15f ", fdiff[9*n+0]                  );
fprintf(o_fdiff,"%.15f|", fdiff[9*n+1]                  );
#endif
    } /* for( i=0; i<get_LX(lattice); i++, n++) */

    fprintf( o_feq,  "\n");
    fprintf( o_f,    "\n");
    fprintf( o_ftemp,"\n");
    fprintf( o_fdiff,"\n");

    n = j*get_LX(lattice);
    fprintf( o_feq  , "|");
    fprintf( o_f    , "|");
    fprintf( o_ftemp, "|");
    fprintf( o_fdiff, "|");
    for( i=0; i<get_LX(lattice); i++, n++)
    {
#if 0
fprintf(o_feq,  "%3d ",X2N(lattice->pdf[subs][n].feq[7]  /max_feq5678  ,s));
fprintf(o_feq,  "%3d ",X2N(lattice->pdf[subs][n].feq[4]  /max_feq1234  ,s));
fprintf(o_feq,  "%3d|",X2N(lattice->pdf[subs][n].feq[8]  /max_feq5678  ,s));
fprintf(o_f,    "%3d ",X2N(lattice->pdf[subs][n].f[7]    /max_f5678    ,s));
fprintf(o_f,    "%3d ",X2N(lattice->pdf[subs][n].f[4]    /max_f1234    ,s));
fprintf(o_f,    "%3d|",X2N(lattice->pdf[subs][n].f[8]    /max_f5678    ,s));
fprintf(o_ftemp,"%3d ",X2N(lattice->pdf[subs][n].ftemp[7]/max_ftemp5678,s));
fprintf(o_ftemp,"%3d ",X2N(lattice->pdf[subs][n].ftemp[4]/max_ftemp1234,s));
fprintf(o_ftemp,"%3d|",X2N(lattice->pdf[subs][n].ftemp[8]/max_ftemp5678,s));
fprintf(o_fdiff,"%3d ",X2N(fdiff[9*n+7]/max_fdiff5678,s));
fprintf(o_fdiff,"%3d ",X2N(fdiff[9*n+4]/max_fdiff1234,s));
fprintf(o_fdiff,"%3d|",X2N(fdiff[9*n+8]/max_fdiff5678,s));
#else
fprintf(o_feq,  "%.15f ", lattice->pdf[subs][n].feq[7]  );
fprintf(o_feq,  "%.15f ", lattice->pdf[subs][n].feq[4]  );
fprintf(o_feq,  "%.15f|", lattice->pdf[subs][n].feq[8]  );
fprintf(o_f,    "%.15f ", lattice->pdf[subs][n].f[7]    );
fprintf(o_f,    "%.15f ", lattice->pdf[subs][n].f[4]    );
fprintf(o_f,    "%.15f|", lattice->pdf[subs][n].f[8]    );
fprintf(o_ftemp,"%.15f ", lattice->pdf[subs][n].ftemp[7]);
fprintf(o_ftemp,"%.15f ", lattice->pdf[subs][n].ftemp[4]);
fprintf(o_ftemp,"%.15f|", lattice->pdf[subs][n].ftemp[8]);
fprintf(o_fdiff,"%.15f ", fdiff[9*n+7]                  );
fprintf(o_fdiff,"%.15f ", fdiff[9*n+4]                  );
fprintf(o_fdiff,"%.15f|", fdiff[9*n+8]                  );
#endif
    } /* for( i=0; i<get_LX(lattice); i++, n++) */

    fprintf( o_feq,  "\n");
    fprintf( o_f,    "\n");
    fprintf( o_ftemp,"\n");
    fprintf( o_fdiff,"\n");

    fprintf( o_feq  , "+");
    fprintf( o_f    , "+");
    fprintf( o_ftemp, "+");
    fprintf( o_fdiff, "+");
    for( i=0; i<get_LX(lattice); i++)
    {
      fprintf( o_feq  , "----");
      fprintf( o_feq  , "----");
      fprintf( o_feq  , "---+");
      fprintf( o_f    , "----");
      fprintf( o_f    , "----");
      fprintf( o_f    , "---+");
      fprintf( o_ftemp, "----");
      fprintf( o_ftemp, "----");
      fprintf( o_ftemp, "---+");
      fprintf( o_fdiff, "----");
      fprintf( o_fdiff, "----");
      fprintf( o_fdiff, "---+");
    }
    fprintf( o_feq  , "\n");
    fprintf( o_f    , "\n");
    fprintf( o_ftemp, "\n");
    fprintf( o_fdiff, "\n");

  } /* for( j=get_LY(lattice)-1; j>=0; j--) */

  fclose( o_feq);
  fclose( o_f);
  fclose( o_ftemp);
  fclose( o_fdiff);

 } /* for( subs=0; subs<NUM_FLUID_COMPONENTS; subs++) */


#endif /* WRITE_PDF_TO_TXT */

} /* void dump_pdf( struct lattice_struct *lattice, int time) */
                                                                        // }}}
#if NON_LOCAL_FORCES
// D U M P   F O R C E S {{{
//##############################################################################
// void dump_forces( struct lattice_struct *lattice)
//
//  - Output the interactive force values to file.
//
void dump_forces( struct lattice_struct *lattice)
{
  char   filename[1024];
  FILE   *ox, *oy;
  int    n;
  double *force;
  int    frame;
#if WRITE_RHO_AND_U_TO_TXT
  int    i, j;
#endif /* WRITE_RHO_AND_U_TO_TXT */
  int    subs;

 for( subs = 0; subs < NUM_FLUID_COMPONENTS; subs++)
 {
  frame = (int)((double)lattice->time/(double)lattice->param.FrameRate);
#if VERBOSITY_LEVEL > 0
  printf("dump_forces() -- frame = %d/%d = %d\n",
      lattice->time,
      lattice->param.FrameRate,
      frame);
#endif /* VERBOSITY_LEVEL > 0 */

  sprintf( filename, "%s/force_x%dx%d_frame%04d_subs%02d_proc%04d.dat", get_out_path(lattice),
      get_LX(lattice), get_LY(lattice), frame, subs, get_proc_id(lattice));
  if( !( ox = fopen( filename, "w+")))
  {
    printf("ERROR: fopen( \"%s\", \"w+\") = NULL.  Bye, bye!\n", filename);
    process_exit(1);
  }
  sprintf( filename, "%s/force_y%dx%d_frame%04d_subs%02d_proc%04d.dat", get_out_path(lattice),
      get_LX(lattice), get_LY(lattice), frame, subs, get_proc_id(lattice));
  if( !( oy = fopen( filename, "w+")))
  {
    printf("ERROR: fopen( \"%s\", \"w+\") = NULL.  Bye, bye!\n", filename);
    process_exit(1);
  }

  force = lattice->force[subs][0].force;

  for( n=0; n<lattice->NumNodes; n++)
  {
    fprintf( ox, "%20.17f\n", *force++);
    fprintf( oy, "%20.17f\n", *force++);
    force += ( sizeof( struct force_struct)/8 - 2);
  }

  fclose(ox);
#if VERBOSITY_LEVEL > 0
  sprintf( filename, "%s/force_x%dx%d_frame%04d_subs%02d_proc%04d.dat", get_out_path(lattice),
      get_LX(lattice), get_LY(lattice), frame, subs, get_proc_id(lattice));
#endif /* VERBOSITY_LEVEL > 0 */
  printf("dump_forces() -- Wrote file \"%s\"\n", filename);
  fclose(oy);
#if VERBOSITY_LEVEL > 0
  sprintf( filename, "%s/force_y%dx%d_frame%04d_subs%02d_proc%04d.dat", get_out_path(lattice),
      get_LX(lattice), get_LY(lattice), frame, subs, get_proc_id(lattice));
#endif /* VERBOSITY_LEVEL > 0 */
  printf("dump_forces() -- Wrote file \"%s\"\n", filename);

#if WRITE_RHO_AND_U_TO_TXT
  // NOTE: This is very inefficient.  But it's only intended
  // for debugging purposes on small problems.
  sprintf( filename, "%s/force_x%dx%d_frame%04d_subs%02d_proc%04d.txt", get_out_path(lattice),
      get_LX(lattice), get_LY(lattice), frame, subs, get_proc_id(lattice));
  if( !( ox = fopen( filename, "w+")))
  {
    printf("ERROR: fopen( \"%s\", \"w+\") = NULL.  Bye, bye!\n", filename);
    process_exit(1);
  }
  sprintf( filename, "%s/force_y%dx%d_frame%04d_subs%02d_proc%04d.txt", get_out_path(lattice),
      get_LX(lattice), get_LY(lattice), frame, subs, get_proc_id(lattice));
  if( !( oy = fopen( filename, "w+")))
  {
    printf("ERROR: fopen( \"%s\", \"w+\") = NULL.  Bye, bye!\n", filename);
    process_exit(1);
  }
  for( j=0; j<get_LY(lattice); j++)
  {
    n = j*get_LX(lattice);
    for( i=0; i<get_LX(lattice); i++, n++)
    {
      fprintf( ox, "%10.7f ", lattice->force[subs][n].force[0]);
      fprintf( oy, "%10.7f ", lattice->force[subs][n].force[1]);
      if( n==lattice->NumNodes)
      {
        fprintf( ox, "%10.7f ", 0.);
        fprintf( oy, "%10.7f ", 0.);
      }
    }
    fprintf( ox, "\n");
    fprintf( oy, "\n");
  }

  fclose(ox);
#if VERBOSITY_LEVEL > 0
  sprintf( filename, "%s/force_x%dx%d_frame%04d_subs%02d_proc%04d.txt", get_out_path(lattice),
      get_LX(lattice), get_LY(lattice), frame, subs, get_proc_id(lattice));
#endif /* VERBOSITY_LEVEL > 0 */
  printf("dump_forces() -- Wrote file \"%s\"\n", filename);
  fclose(oy);
#if VERBOSITY_LEVEL > 0
  sprintf( filename, "%s/force_y%dx%d_frame%04d_subs%02d_proc%04d.txt", get_out_path(lattice),
      get_LX(lattice), get_LY(lattice), frame, subs, get_proc_id(lattice));
#endif /* VERBOSITY_LEVEL > 0 */
  printf("dump_forces() -- Wrote file \"%s\"\n", filename);

#endif /* WRITE_RHO_AND_U_TO_TXT */

 } /* for( subs = 0; subs < NUM_FLUID_COMPONENTS; subs++) */

} /* void dump_forces( struct lattice_struct *lattice) */
                                                                        // }}}
#endif /* NON_LOCAL_FORCES */
// D U M P   C H E C K P O I N T  {{{
//##############################################################################
// void dump_checkpoint( struct lattice_struct *lattice, int time, char *fn)
//
//  - Write lattice to a checkpoint file.
//
//  - Should be binary and store all information necessary to
//    restart the current run at this point.
//
void dump_checkpoint( struct lattice_struct *lattice, int time, char *fn)
{

} /* void dump_checkpoint( struct lattice_struct *lattice, ...) */
                                                                        // }}}
// R E A D   C H E C K P O I N T  {{{
//##############################################################################
// void read_checkpoint( struct lattice_struct *lattice)
//
//  - Read lattice from a checkpoint file (as written by dump_checkpoint).
//
//  - With this information, should be able to restart where
//    the previous run stopped.
//
void read_checkpoint( struct lattice_struct *lattice)
{

} /* void read_checkpoint( struct lattice_struct *lattice) */
                                                                        // }}}

// S P Y   B M P  {{{
//##############################################################################
// void spy_bmp( char *filename, int ***spy)
//
//  - Returns matrix 'spy' of ones and zeros.
//
//  - Zeros for white pixels.
//
//  - Ones for non-white pixels.
//
void spy_bmp( char *filename, lattice_ptr lattice, int **matrix)
{
  FILE *in, *o;
  int i, j, n, m;
  int g_i, g_j;
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
  char ctemp;
  int  itemp;
  int **spy;

  printf("spy_bmp() -- Hi!\n");

  spy = (int**)malloc( get_g_LY(lattice)*sizeof(int*));
  for( j=0; j<get_g_LY(lattice); j++)
  {
    spy[j] = (int*)malloc( get_g_LX(lattice)*sizeof(int));
  }

  // Clear the spy array.
  for( j=0; j<get_g_LY(lattice); j++)
  {
    for( i=0; i<get_g_LX(lattice); i++)
    {
      spy[j][i] = 0;
    }
  }

  // Clear the matrix array.
  for( j=0; j<get_LY(lattice); j++)
  {
    for( i=0; i<get_LX(lattice); i++)
    {
      matrix[j][i] = 0;
    }
  }

  if(/*ignore_solids*/0) return;

  if( !( in = fopen( filename, "r")))
  {
#if 1
    printf("%s %d >> spy_bmp() -- Error opening file \"%s\".\n",
      __FILE__, __LINE__, filename);
    process_exit(1);
#else
    printf(" %s::spy_bmp() %d >> File \"%s\" cannot be opened for reading.\n",
        __FILE__, __LINE__, filename);
    if( !( o = fopen( filename, "w+")))
    {
      // TODO: Write blank bmp file.
    }
    printf(" %s::spy_bmp() %d >> Wrote a blank \"%s\" file.\n",
        __FILE__, __LINE__, filename);
    printf(" %s::spy_bmp() %d >> Returning all zeros!\n", __FILE__, __LINE__);
    fclose( o);
    return;
#endif
  }

  // n = fread( void *BUF, size_t SIZE, size_t COUNT, FILE *FP);

  n = fread( &bmfh, sizeof(struct bitmap_file_header), 1, in );
  if( strncmp(bmfh.bfType,"BM",2))
  {
    printf("ERROR: Can't process this file type.  Exiting!\n");
    printf("\n");
    process_exit(1);
  }
  n = fread( &bmih, sizeof(struct bitmap_info_header), 1, in );

#if 0
printf("%s %d >> sizeof(int) = %d \n", __FILE__, __LINE__, sizeof(int));
printf("%s %d >> biWidth = %d \n", __FILE__, __LINE__, (int)*(int*)bmih.biWidth);
printf("%s %d >> biWidth = [ '%c' '%c' '%c' '%c'] \n", __FILE__, __LINE__,
    bmih.biWidth[0], bmih.biWidth[1], bmih.biWidth[2], bmih.biWidth[3] );
printf("%s %d >> biWidth = [ '%d' '%d' '%d' '%d'] \n", __FILE__, __LINE__,
    bmih.biWidth[0], bmih.biWidth[1], bmih.biWidth[2], bmih.biWidth[3] );
ctemp = bmih.biWidth[0];
bmih.biWidth[0] = bmih.biWidth[3];
bmih.biWidth[3] = ctemp;
ctemp = bmih.biWidth[1];
bmih.biWidth[1] = bmih.biWidth[2];
bmih.biWidth[2] = ctemp;
   itemp = 0xaabbccdd;//(int)*(int*)bmih.biWidth;
printf("%s %d >> itemp = %d\n",__FILE__,__LINE__, itemp);
printf("%s %d >> itemp = [ '%d' '%d' '%d' '%d'] \n", __FILE__, __LINE__,
   (itemp&0xff000000)>>24,
   (itemp&0x00ff0000)>>16,
   (itemp&0x0000ff00)>> 8,
   (itemp&0x000000ff)>> 0 );
  itemp = ENDIAN4(itemp);
printf("%s %d >> itemp = %d\n",__FILE__,__LINE__, itemp);
printf("%s %d >> itemp = [ '%d' '%d' '%d' '%d'] \n", __FILE__, __LINE__,
   (itemp&0xff000000)>>24,
   (itemp&0x00ff0000)>>16,
   (itemp&0x0000ff00)>> 8,
   (itemp&0x000000ff)>> 0 );
printf("%s %d >> biWidth = [ '%d' '%d' '%d' '%d'] \n", __FILE__, __LINE__,
    bmih.biWidth[0], bmih.biWidth[1], bmih.biWidth[2], bmih.biWidth[3] );
printf("%s %d >> biWidth = %d \n", __FILE__, __LINE__, (int)*(int*)bmih.biWidth);

printf("%s %d >> sizeof(int) = %d \n", __FILE__, __LINE__, sizeof(int));
printf("%s %d >> biHeight = %d \n", __FILE__, __LINE__, (int)*(int*)bmih.biHeight);
printf("%s %d >> biHeight = [ '%c' '%c' '%c' '%c'] \n", __FILE__, __LINE__,
    bmih.biHeight[0], bmih.biHeight[1], bmih.biHeight[2], bmih.biHeight[3] );
printf("%s %d >> biHeight = [ '%d' '%d' '%d' '%d'] \n", __FILE__, __LINE__,
    bmih.biHeight[0], bmih.biHeight[1], bmih.biHeight[2], bmih.biHeight[3] );
ctemp = bmih.biHeight[0];
bmih.biHeight[0] = bmih.biHeight[3];
bmih.biHeight[3] = ctemp;
ctemp = bmih.biHeight[1];
bmih.biHeight[1] = bmih.biHeight[2];
bmih.biHeight[2] = ctemp;
printf("%s %d >> biHeight = [ '%d' '%d' '%d' '%d'] \n", __FILE__, __LINE__,
    bmih.biHeight[0], bmih.biHeight[1], bmih.biHeight[2], bmih.biHeight[3] );
printf("%s %d >> biHeight = %d \n", __FILE__, __LINE__, (int)*(int*)bmih.biHeight);

ctemp = bmih.biBitCount[0];
bmih.biBitCount[0] = bmih.biBitCount[1];
bmih.biBitCount[1] = ctemp;

#endif

  *((int*)(bmih.biWidth)) = ENDIAN4(((int)(*((int*)(bmih.biWidth)))));
  *((int*)(bmih.biHeight)) = ENDIAN4(((int)(*((int*)(bmih.biHeight)))));
  *((short int*)(bmih.biBitCount)) = ENDIAN2(((short int)(*((short int*)(bmih.biBitCount)))));

  int_ptr = (int*)bmih.biCompression;
  if( *int_ptr != 0)
  {
    printf("%s %d >> ERROR: Can't handle compression.  Exiting!\n",__FILE__,__LINE__);
    printf("\n");
    process_exit(1);
  }

  width_ptr = (int*)bmih.biWidth;
  height_ptr = (int*)bmih.biHeight;
  bitcount_ptr = (short int*)bmih.biBitCount;

//LBMPI #if PARALLEL
//LBMPI   if( *width_ptr != lattice->lbmpi->GLX)
//LBMPI   {
//LBMPI     printf("%s %d >> ERROR: GLX %d does not match the "
//LBMPI         "width %d of the BMP file. Exiting!\n",
//LBMPI         __FILE__, __LINE__, lattice->lbmpi->GLX, *width_ptr);
//LBMPI     process_exit(1);
//LBMPI   }
//LBMPI #else /* !(PARALLEL) */
  if( *width_ptr != get_g_LX(lattice))
  {
    printf("%s %d >> ERROR: LX %d does not match the "
        "width %d of the BMP file. Exiting!\n"
        "Note that, if the width stated here seems absurd, you\n"
        "might need to recompile with the SWAP_BYTE_ORDER flag.\n"
        "This can be done by \"make swap\".\n",
        __FILE__, __LINE__, get_g_LX(lattice), *width_ptr);
    process_exit(1);
  }
//LBMPI #endif /* (PARALLEL) */
printf("%s %d >> biWidth = %d \n", __FILE__, __LINE__, (int)*bmih.biWidth);
printf("%s %d >> width_ptr = %d \n", __FILE__, __LINE__, (int)*width_ptr);

//LBMPI #if PARALLEL
//LBMPI   if( *height_ptr != lattice->lbmpi->GLY)
//LBMPI   {
//LBMPI     printf("%s %d >> ERROR: GLY %d does not match the "
//LBMPI         "height %d of the BMP file. Exiting!\n",
//LBMPI         __FILE__, __LINE__, lattice->lbmpi->GLY, *height_ptr);
//LBMPI     process_exit(1);
//LBMPI   }
//LBMPI #else /* !(PARALLEL) */
  if( *height_ptr != get_g_LY(lattice))
  {
    printf("%s %d >> ERROR: LY %d does not match the "
        "height %d of the BMP file. Exiting!\n",
        __FILE__, __LINE__, get_g_LY(lattice), *height_ptr);
    process_exit(1);
  }
//LBMPI #endif /* (PARALLEL) */

  if( (*bitcount_ptr) < 24)
  {
    n = (int)pow(2.,(double)(*bitcount_ptr)); // Num palette entries.
    for( i=0; i<n; i++)
    {
      k = fread( &rgb, sizeof(struct rgb_quad), 1, in );
      if( k!=1)
      {
        printf("%s %d >> Error reading palette entry %d.  Exiting!\n", __FILE__, __LINE__, i);
        process_exit(1);
      }
    }
  }

  // Bytes per row of the bitmap.
  bytes_per_row =
    ((int)ceil(( (((double)(*width_ptr))*((double)((*bitcount_ptr))))/8.)));

  // Bitmaps pad rows to preserve 4-byte boundaries.
  // The length of a row in the file will be bytes_per_row + pad .
  pad = ((4) - bytes_per_row%4)%4;

  n = 0;
  m = 0;
  n+=( k = fread( &b, 1, 1, in ));
  i = 0;
  j = 0;
  while( !feof(in))
  {
    switch((*bitcount_ptr))
    {
      case 1: // Monochrome.
        printf("%s %d >> spy_bmp() -- "
            "Support for Monochrome BMPs is pending.  "
            "Exiting!\n", __FILE__, __LINE__);
        process_exit(1);

        if( i < *width_ptr) { (spy)[j][i] = ( (b & 0x80) == 0); }
        i++;
        if( i < *width_ptr) { (spy)[j][i] = ( (b & 0x40) == 0); }
        i++;
        if( i < *width_ptr) { (spy)[j][i] = ( (b & 0x20) == 0); }
        i++;
        if( i < *width_ptr) { (spy)[j][i] = ( (b & 0x10) == 0); }
        i++;
        if( i < *width_ptr) { (spy)[j][i] = ( (b & 0x08) == 0); }
        i++;
        if( i < *width_ptr) { (spy)[j][i] = ( (b & 0x04) == 0); }
        i++;
        if( i < *width_ptr) { (spy)[j][i] = ( (b & 0x02) == 0); }
        i++;
        if( i < *width_ptr) { (spy)[j][i] = ( (b & 0x01) == 0); }
        i++;
        break;

      case 4: // 16 colors.
        printf("%s %d >> spy_bmp() -- "
            "Support for 16 color BMPs is pending.  "
            "Exiting!\n", __FILE__, __LINE__);
        process_exit(1);

        if( i < *width_ptr) { (spy)[j][i] = ( (b&0xf0)>>4 != 15); }
        i++;
        if( i < *width_ptr) { (spy)[j][i] = ( (b&0x0f) != 15); }
        i++;
        break;

      case 8: // 256 colors.
        printf("%s %d >> spy_bmp() -- "
            "Support for 256 color BMPs is pending.  "
            "Exiting!\n", __FILE__, __LINE__);
        process_exit(1);

        if( i < *width_ptr) { (spy)[j][i] = ( (b&0xff) != 255); }
        i++;
        break;

      case 24: // 24-bit colors.
        if( i < 3*(*width_ptr))
        {
          i++; n+=( k = fread( &g, 1, 1, in ));
          i++; n+=( k = fread( &r, 1, 1, in ));

          if( ( (b&0xff) == 0) &&( (g&0xff) == 0) &&( (r&0xff) == 0) )
          {
            (spy)[j][(int)floor((double)i/3.)] = 1;
          }

#if 0
          if( ( (b&0xff) == 0) &&( (g&0xff) == 0) &&( (r&0xff) == 255) )
          {
            // Red ==> Inflow, Pressure boundaries.
            if(    (int)floor((double)i/3.) == 0
                || (int)floor((double)i/3.) == get_g_LX(lattice)-1 )
            {
              if( !( j==0 || j == get_g_LY(lattice)-1))
              {
                lattice->periodic_x[subs] = 0;
              }
            }
            if(    j == 0
                || j == get_g_LY(lattice)-1 )
            {
              if( !(   (int)floor((double)i/3.) == 0
                    || (int)floor((double)i/3.) == get_g_LX(lattice)-1))
              {
                lattice->periodic_y[subs] = 0;
              }
            }
          }

          if( ( (b&0xff) == 0) &&( (g&0xff) == 255) &&( (r&0xff) == 0) )
          {
            // Green ==> Outflow, Pressure boundaries.
            if(    (int)floor((double)i/3.) == 0
                || (int)floor((double)i/3.) == get_g_LX(lattice)-1 )
            {
              if( !( j==0 || j == get_g_LY(lattice)-1))
              {
                lattice->periodic_x[subs] = 0;
              }
            }
            if(    j == 0
                || j == get_g_LY(lattice)-1 )
            {
              if( !(   (int)floor((double)i/3.) == 0
                    || (int)floor((double)i/3.) == get_g_LX(lattice)-1))
              {
                lattice->periodic_y[subs] = 0;
              }
            }
          }
#endif
        }
        i++;
        break;

      default: // 32-bit colors?
        printf("%s %d >> ERROR: Unhandled color depth, "
            "BitCount = %d. Exiting!\n", __FILE__, __LINE__, *bitcount_ptr);
        process_exit(1);
        break;

    } /* switch(*(bmih.biBitCount)) */

    if( !(n%(bytes_per_row+pad))) { m++; i=0; j++;}
    n+=( k = fread( &b, 1, 1, in ));

  } /* while( !feof(in)) */

  if( (bytes_per_row+pad)*m!=n)
  {
    printf("WARNING: Num bytes read = %d versus num bytes predicted = %d .\n",
        n, (bytes_per_row+pad)*m);
  }

  if( m != *height_ptr)
  {
    printf("WARNING: m (%d) != bmih.biHeight (%d).\n", m, *height_ptr);
  }

  fclose(in);

  for( j=0, g_j=get_g_SY(lattice); j<get_LY(lattice); j++, g_j++)
  {
    for( i=0, g_i=get_g_SX(lattice); i<get_LX(lattice); i++, g_i++)
    {
      matrix[j][i] = spy[g_j][g_i];
    }
  }

  for( j=0; j<get_g_LY(lattice); j++)
  {
    free(spy[j]);
  }
  free(spy);

  printf("spy_bmp() -- Bye!\n");
  printf("\n");

} /* spy_bmp( char *filename, int **spy) */
                                                                        // }}}
// R E A D   B C S {{{
//##############################################################################
// void read_bcs( char *filename, int **bcs)
//
//  - Read boundary condition information from file.
//
void read_bcs( lattice_ptr lattice, int **bcs)
{
  FILE   *in;
  char   filename[1024];
  int    i, j, n, m;
  int    ei, ej;
  int    pad, bytes_per_row;
  char   k;
  char   b, g, r;
  struct bitmap_file_header bmfh;
  struct bitmap_info_header bmih;
  struct rgb_quad rgb;
  int    *int_ptr;
  short  int *short_int_ptr;
  int    *width_ptr;
  int    *height_ptr;
  short  int *bitcount_ptr;
  int    subs;

  printf("read_bcs() -- Hi!\n");

 for( subs=0; subs<NUM_FLUID_COMPONENTS; subs++)
 {

  // Clear the bcs array.
  for( j=0; j<get_LY(lattice); j++)
  {
    for( i=0; i<get_LX(lattice); i++)
    {
      bcs[j][i] = 0;
    }
  }


  sprintf( filename, "./in/%dx%dbc_subs%02d_proc%04d.bmp",
      get_LX(lattice), get_LY(lattice), subs, get_proc_id(lattice));
  if( !( in = fopen( filename, "r"), get_proc_id(lattice)))
  {
    printf("%s %d >> read_bcs() -- Error opening file \"%s\".\n",
      __FILE__,__LINE__,filename);
    process_exit(1);
  }

  // Read the headers.
  n = fread( &bmfh, sizeof(struct bitmap_file_header), 1, in );
  if( strncmp(bmfh.bfType,"BM",2))
  {
    printf("ERROR: Can't process this file type.  Exiting!\n");
    printf("\n");
    process_exit(1);
  }
  n = fread( &bmih, sizeof(struct bitmap_info_header), 1, in );
  int_ptr = (int*)bmih.biCompression;
  if( *int_ptr != 0)
  {
    printf("ERROR: Can't handle compression.  Exiting!\n");
    printf("\n");
    process_exit(1);
  }

  width_ptr = (int*)bmih.biWidth;
  height_ptr = (int*)bmih.biHeight;
  bitcount_ptr = (short int*)bmih.biBitCount;

  if( ENDIAN4(*height_ptr) != get_LY(lattice))
  {
    printf("ERROR: Lattice height does not match "
        "soil matrix data \"%s\".  (%d!=%d)  Exiting!\n",
        filename,
        get_LY(lattice), ENDIAN4(*height_ptr) );
    printf("\n");
    process_exit(1);
  }

  if( ENDIAN4(*width_ptr) != get_LX(lattice))
  {
    printf("ERROR: Lattice width does not match "
        "soil matrix data \"%s\".  (%d!=%d)  Exiting!\n",
        filename,
        get_LX(lattice), ENDIAN4(*width_ptr) );
    printf("\n");
    process_exit(1);
  }

  // Read the palette, if necessary.
  if( ENDIAN2(*bitcount_ptr) < 24)
  {
    n = (int)pow(2.,(double)ENDIAN2(*bitcount_ptr)); // Num palette entries.
    for( i=0; i<n; i++)
    {
      k = fread( &rgb, sizeof(struct rgb_quad), 1, in );
      if( k!=1)
      {
        printf("Error reading palette entry %d.  Exiting!\n", i);
        process_exit(1);
      }
    }
  }

  // Bytes per row of the bitmap.
  bytes_per_row =
    ((int)ceil(( (((double)(ENDIAN4(*width_ptr)))*((double)(ENDIAN2(*bitcount_ptr))))/8.)));

  // Bitmaps pad rows to preserve 4-byte boundaries.
  // The length of a row in the file will be bytes_per_row + pad .
  pad = ((4) - bytes_per_row%4)%4;

  n = 0;
  m = 0;
  n+=( k = fread( &b, 1, 1, in ));
  i = 0;
  //j = *height_ptr-1;
  j = 0;//*height_ptr-1;
  while( !feof(in))
  {
    switch(ENDIAN2(*bitcount_ptr))
    {
      case 1: // Monochrome.
        printf("read_bcs() -- "
            "Support for Monochrome BMPs is pending.  "
            "Exiting!\n");
        process_exit(1);

        if( i < ENDIAN4(*width_ptr)) { (bcs)[j][i] = ( (b & 0x80) == 0); }
        i++;
        if( i < ENDIAN4(*width_ptr)) { (bcs)[j][i] = ( (b & 0x40) == 0); }
        i++;
        if( i < ENDIAN4(*width_ptr)) { (bcs)[j][i] = ( (b & 0x20) == 0); }
        i++;
        if( i < ENDIAN4(*width_ptr)) { (bcs)[j][i] = ( (b & 0x10) == 0); }
        i++;
        if( i < ENDIAN4(*width_ptr)) { (bcs)[j][i] = ( (b & 0x08) == 0); }
        i++;
        if( i < ENDIAN4(*width_ptr)) { (bcs)[j][i] = ( (b & 0x04) == 0); }
        i++;
        if( i < ENDIAN4(*width_ptr)) { (bcs)[j][i] = ( (b & 0x02) == 0); }
        i++;
        if( i < ENDIAN4(*width_ptr)) { (bcs)[j][i] = ( (b & 0x01) == 0); }
        i++;
        break;

      case 4: // 16 colors.
        printf("read_bcs() -- "
            "Support for 16 color BMPs is pending.  "
            "Exiting!\n");
        process_exit(1);

        if( i < ENDIAN4(*width_ptr)) { (bcs)[j][i] = ( (b&0xf0)>>4 != 15); }
        i++;
        if( i < ENDIAN4(*width_ptr)) { (bcs)[j][i] = ( (b&0x0f) != 15); }
        i++;
        break;

      case 8: // 256 colors.
        printf("read_bcs() -- "
            "Support for 256 color BMPs is pending.  "
            "Exiting!\n");
        process_exit(1);

        if( i < ENDIAN4(*width_ptr)) { (bcs)[j][i] = ( (b&0xff) != 255); }
        i++;
        break;

      case 24: // 24-bit colors.
        if( i < 3*(ENDIAN4(*width_ptr)))
        {
          i++; n+=( k = fread( &g, 1, 1, in ));
          i++; n+=( k = fread( &r, 1, 1, in ));
          if( ( (b&0xff) == 0) &&( (g&0xff) == 0) &&( (r&0xff) == 255) )
          { // R E D ==> Inflow, Pressure boundaries.
            bcs[j][(int)floor((double)i/3.)] = 1;
          }
          if( ( (b&0xff) == 0) &&( (g&0xff) == 255) &&( (r&0xff) == 0) )
          { // G R E E N ==> Outflow, Pressure boundaries.
            bcs[j][(int)floor((double)i/3.)] = 2;
          }

        }
        i++;
        break;

      default: // 32-bit colors?
        printf("ERROR: Unhandled color depth, "
            "BitCount = %d. Exiting!\n", ENDIAN2(*bitcount_ptr));
        process_exit(1);
        break;

    } /* switch(*(bmih.biBitCount)) */

    if( !(n%(bytes_per_row+pad))) { m++; i=0; j++;}
    n+=( k = fread( &b, 1, 1, in ));

  } /* while( !feof(in)) */

  if( (bytes_per_row+pad)*m!=n)
  {
    printf("WARNING: Num bytes read = %d versus num bytes predicted = %d .\n",
        n, (bytes_per_row+pad)*m);
  }

  if( m != ENDIAN4(*height_ptr))
  {
    printf("WARNING: m (%d) != bmih.biHeight (%d).\n", m, ENDIAN4(*height_ptr));
  }

  fclose(in);

  ei = get_LX(lattice)-1;
  ej = get_LY(lattice)-1;

  for( n=0; n<lattice->NumNodes; n++)
  {
    i = n%get_LX(lattice);
    j = n/get_LX(lattice);

    if( bcs[ j][ i] != 0)
    {
//printf("read_bcs() -- n = %d, ( %d, %d) of ( %d, %d).\n", n, i, j, ei, ej);
#if 0
      if(    ( i==0  && j==0 )
          || ( i==ei && j==0 )
          || ( i==ei && j==ej)
          || ( i==0  && j==ej)  )
      {
        // Skip corners for now.
        printf("read_bcs() -- WARNING: Skipping corner ( %d, %d).", i, j);
      }
      else
      {
#endif
#if 0
        if( i==0)
        {
          // West
          if( bcs[ j][ i] == 1)
          {
            // Inflow
            lattice->bc[n].bc_type |= BC_PRESSURE_W_IN;
            //lattice->periodic_x = 0;
          }
          else if( bcs[ j][ i] == 2)
          {
            // Outflow
            lattice->bc[n].bc_type |= BC_PRESSURE_W_OUT;
            //lattice->periodic_x = 0;
          }
          else
          {
            // Unhandled case.
            printf("read_bcs() -- Unhandled case: "
                "bcs[ %d][ %d] = %d .  Exiting!\n",
                i, j, bcs[j][i]);
            process_exit(1);
          }

        }
        else if( i==ei)
        {
          // East
          if( bcs[ j][ i] == 1)
          {
            // Inflow
            lattice->bc[n].bc_type |= BC_PRESSURE_E_IN;
            //lattice->periodic_x = 0;
          }
          else if( bcs[ j][ i] == 2)
          {
            // Outflow
            lattice->bc[n].bc_type |= BC_PRESSURE_E_OUT;
            //lattice->periodic_x = 0;
          }
          else
          {
            // Unhandled case.
            printf("read_bcs() -- Unhandled case: "
                "bcs[ %d][ %d] = %d .  Exiting!\n",
                i, j, bcs[j][i]);
            process_exit(1);
          }

        }
        else
#endif
        if( j==0)
        {
//printf("read_bcs() -- South at i=%d\n", i);
          // South
          if( bcs[ j][ i] == 1)
          {
            // Inflow
            lattice->bc[subs][n].bc_type |= BC_PRESSURE_S_IN;
            //lattice->periodic_y = 0;
          }
          else if( bcs[ j][ i] == 2)
          {
            // Outflow
            lattice->bc[subs][n].bc_type |= BC_PRESSURE_S_OUT;
            //lattice->periodic_y = 0;
          }
          else
          {
            // Unhandled case.
            printf("read_bcs() -- Unhandled case: "
                "bcs[ %d][ %d] = %d .  Exiting!\n",
                i, j, bcs[j][i]);
            process_exit(1);
          }

        }
        else if( j==ej)
        {
//printf("read_bcs() -- North at i=%d\n", i);
          // North
          if( bcs[ j][ i] == 1)
          {
            // Inflow
            lattice->bc[subs][n].bc_type |= BC_PRESSURE_N_IN;
            //lattice->periodic_y = 0;
          }
          else if( bcs[ j][ i] == 2)
          {
            // Outflow
            lattice->bc[subs][n].bc_type |= BC_PRESSURE_N_OUT;
            //lattice->periodic_y = 0;
          }
          else
          {
            // Unhandled case.
            printf("read_bcs() -- Unhandled case: "
                "bcs[ %d][ %d] = %d .  Exiting!\n",
                i, j, bcs[j][i]);
            process_exit(1);
          }

        }
        else
        {
          // Unhandled case.
          printf("read_bcs() -- WARNING: "
              "Support for interior flow bcs is pending! "
              "Skipping ( i, j) = ( %d, %d).\n", i, j);
        }
#if 0
      }
#endif

    }

  }

 } /* for( subs=0; subs<NUM_FLUID_COMPONENTS; subs++) */

  printf("read_bcs() -- Bye!\n");
  printf("\n");

} /* read_bcs( char *filename, int ***bcs, int *height, int *width) */
                                                                        // }}}

#if 1
// R H O 2 B M P  {{{
//##############################################################################
// void rho2bmp( char *filename, int time)
//
void rho2bmp( lattice_ptr lattice, int time)
{
  FILE   *in,
         *o;
  int    i, j,
         n, m;
  int    pad,
         bytes_per_row;
  int    frame;
  char   k;
  char   b;
  struct bitmap_file_header bmfh;
  struct bitmap_info_header bmih;
  struct rgb_quad rgb;
  int    *int_ptr;
  short  int *short_int_ptr;
  int    *width_ptr;
  int    *height_ptr;
  short  int *bitcount_ptr;
  char   filename[1024];
  char   red_val,
         green_val,
         blue_val,
         val;
  double fval;
  double min_rho, max_rho;
  int    subs;
  double **colormap;
  int    num_colors;

#if SAY_HI
  printf("rho2bmp() -- Hi!\n");
#endif /* SAY_HI */

  if( lattice->param.use_colormap)
  {
    count_colormap(      &num_colors);
    allocate_colormap(   &colormap, num_colors);
    read_colormap(       colormap, num_colors);
  }

 for( subs=0; subs<NUM_FLUID_COMPONENTS; subs++)
 {

  frame = time/lattice->param.FrameRate;

#if 0
  sprintf( filename, "./in/%dx%d_proc%04d.bmp",
    get_LX(lattice), get_LY(lattice), get_proc_id(lattice));
  if( !( in = fopen( filename, "r")))
  {
    printf("%s %d >> rho2bmp() -- Error opening file \"%s\".\n",
      __FILE__,__LINE__,filename);
    process_exit(1);
  }

  // n = fread( void *BUF, size_t SIZE, size_t COUNT, FILE *FP);

  n = fread( &bmfh, sizeof(struct bitmap_file_header), 1, in );
  if( strncmp(bmfh.bfType,"BM",2))
  {
    printf("ERROR: Can't process this file type.  Exiting!\n");
    printf("\n");
    process_exit(1);
  }
  n = fread( &bmih, sizeof(struct bitmap_info_header), 1, in );
  int_ptr = (int*)bmih.biCompression;
  if( *int_ptr != 0)
  {
    printf("ERROR: Can't handle compression.  Exiting!\n");
    printf("\n");
    process_exit(1);
  }

  width_ptr = (int*)bmih.biWidth;
  height_ptr = (int*)bmih.biHeight;
  bitcount_ptr = (short int*)bmih.biBitCount;

  // Read palette entries, if applicable.
  if( ENDIAN2(*bitcount_ptr) < 24)
  {
    n = (int)pow(2.,(double)ENDIAN2(*bitcount_ptr)); // Num palette entries.
    for( i=0; i<n; i++)
    {
      k = fread( &rgb, sizeof(struct rgb_quad), 1, in );
      if( k!=1)
      {
        printf("Error reading palette entry %d.  Exiting!\n", i);
        process_exit(1);
      }
    }
  }

  fclose(in);

#else
  bmfh.bfType[0] = 'B';
  bmfh.bfType[1] = 'M';
  *((int*)bmfh.bfSize)= get_LY(lattice)*(
    (int)ceil( ( ((double)get_LX(lattice))*( /*depth*/24.))/8.) // bytes per row
  + ( 4 - (int)ceil( ( ((double)get_LX(lattice))*( /*depth*/24.))/8.) % 4) % 4 // pad
  );
  *((short int*)bmfh.bfReserved1) = 0;
  *((short int*)bmfh.bfReserved2) = 0;
  *((int*)bmfh.bfOffBits) = 54; // 14 byte file header and 40 byte info header

  *((int*)bmih.biSize) = 40;
  *((int*)bmih.biWidth) = get_LX(lattice);
  *((int*)bmih.biHeight) = get_LY(lattice);
  *((short int*)bmih.biPlanes) = 1;
  *((short int*)bmih.biBitCount) = 24;
  *((int*)bmih.biCompression) = 0;
  *((int*)bmih.biSizeImage) = 0;
  *((int*)bmih.biXPelsPerMeter) = 0;
  *((int*)bmih.biYPelsPerMeter) = 0;
  *((int*)bmih.biClrUsed) = 0;
  *((int*)bmih.biClrImportant) = 0;

  width_ptr = (int*)bmih.biWidth;
  height_ptr = (int*)bmih.biHeight;
  bitcount_ptr = (short int*)bmih.biBitCount;

#endif

  // Bytes per row of the bitmap.
  bytes_per_row =
    ((int)ceil(( (((double)(ENDIAN4(*width_ptr)))*((double)(ENDIAN2(*bitcount_ptr))))/8.)));

  // Bitmaps pad rows to preserve 4-byte boundaries.
  // The length of a row in the file will be bytes_per_row + pad .
  pad = ((4) - bytes_per_row%4)%4;

  compute_min_rho( lattice, &min_rho, subs);
  compute_max_rho( lattice, &max_rho, subs);

  sprintf( filename, "%s/rho%dx%d_frame%04d_subs%02d_proc%04d.bmp", get_out_path(lattice),
      get_LX(lattice),
      get_LY(lattice),
      frame, subs, get_proc_id(lattice));
  if( !( o = fopen( filename, "w+")))
  {
    printf("ERROR: fopen( \"%s\", \"w+\") = NULL.  Bye, bye!\n", filename);
    process_exit(1);
  }

  fwrite( &bmfh, sizeof(struct bitmap_file_header), 1, o );
  fwrite( &bmih, sizeof(struct bitmap_info_header), 1, o );

  for( j=0; j<get_LY(lattice); j++)
  {
    n = j*get_LX(lattice);

    for( i=0; i<get_LX(lattice); i++, n++)
    {
      if( lattice->bc[subs][ n].bc_type == /*FLUID_NODE*/0)
      {
        if( lattice->param.use_colormap)
        {
          if( lattice->param.plot_scale_dynamic)
          {
            if( max_rho!=min_rho)
            {
              get_color(
                colormap,
                num_colors,
                (lattice->macro_vars[subs][ n].rho - min_rho)/(max_rho-min_rho),
                &red_val,
                &green_val,
                &blue_val   );
            }
            else
            {
              get_color(
                colormap,
                num_colors,
                1.,
                &red_val,
                &green_val,
                &blue_val   );
            }
          }
          else
          {
            get_color(
              colormap,
              num_colors,
              (lattice->macro_vars[subs][ n].rho
             /( (lattice->param.rho_A[subs]>lattice->param.rho_B[subs])
               ?(lattice->param.rho_A[subs])
               :(lattice->param.rho_B[subs]) )),
              &red_val,
              &green_val,
              &blue_val   );
          }

        }
        else
        {
          if( subs==0)
          {
            if( lattice->param.plot_scale_dynamic)
            {
              if( max_rho!=min_rho)
              {
                fval = ROUND( 255.*( lattice->macro_vars[subs][ n].rho
                                   - min_rho)
                                  /( max_rho-min_rho));
              }
              else
              {
                fval = 255.;
              }
            }
            else
            {
              fval = ROUND( 255.*(lattice->macro_vars[subs][ n].rho
             /( (lattice->param.rho_A[subs]>lattice->param.rho_B[subs])
               ?(lattice->param.rho_A[subs])
               :(lattice->param.rho_B[subs]) )
                                 ));
            }
            if( fval >= 0.)
            {
              if( fval <= 255.)
              {
                red_val   = (char)((int)(255. - fval)%256);
                green_val = (char)((int)(255. - fval)%256);
                blue_val  = (char)255;
              }
              else
              {
                red_val   = (char)0;
                green_val = (char)0;
                blue_val  = (char)255;
              }
            }
            else
            {
              red_val   = (char)((int)(255. + fval)%256);
              green_val = (char)((int)(255. + fval)%256);
              blue_val  = (char)((int)(255. + fval)%256);
              // TODO: Issue warning or something? Potential instability?
            }
          } /* if( subs==0) */

          else // subs == 1
          {
            if( lattice->param.plot_scale_dynamic)
            {
              if( max_rho!=min_rho)
              {
                fval = ROUND( 255.*( lattice->macro_vars[subs][ n].rho
                                        - min_rho)
                                       /( max_rho-min_rho));
              }
              else
              {
                fval = 0.;
              }
            }
            else
            {
//printf("%s (%d) >> fval = %f -> ", __FILE__, __LINE__, fval);
#if INAMURO_SIGMA_COMPONENT
              fval = ROUND( 255.*(lattice->macro_vars[subs][ n].rho)
                                 /(lattice->param.rho_sigma));
#else /* !( INAMURO_SIGMA_COMPONENT) */
              fval = ROUND( 255.*(lattice->macro_vars[subs][ n].rho
             /( (lattice->param.rho_A[subs]>lattice->param.rho_B[subs])
               ?(lattice->param.rho_A[subs])
               :(lattice->param.rho_B[subs]) )
                                 ));
#endif /* INAMURO_SIGMA_COMPONENT */
//printf("%f\n", fval);
            }
            if( fval >= 0.)
            {
              if( fval <= 255.)
              {
                red_val   = (char)255;
                green_val = (char)((int)(255. - fval)%256);
                blue_val  = (char)((int)(255. - fval)%256);
              }
              else
              {
                red_val   = (char)255;//((int)(255. - (fval - 255.))%256);
                green_val = (char)  0;//((int)(255. - (fval - 255.))%256);
                blue_val  = (char)  0;//((int)(255. - (fval - 255.))%256);
              }
            }
            else
            {
              red_val   = (char)((int)(255. + fval)%256);
              green_val = (char)((int)(255. + fval)%256);
              blue_val  = (char)((int)(255. + fval)%256);
              // TODO: Issue a warning or something?  Potential instability?
            }

          } /* if( subs==0) else */
        }


      } /* if( lattice->bc[subs][ n].bc_type == 0) */

      else // lattice->bc[subs][ n].bc_type != 0
      {
#if SOLID_COLOR_IS_CHECKERBOARD
        // Checkerboard pattern over the solids and boundary conditions.
        if( (i+j)%2)
        {
          red_val   = (char)200;
          green_val = (char)200;
          blue_val  = (char)200;
          val       = (char)200;
        }
        else
        {
          red_val   = (char)184;
          green_val = (char)184;
          blue_val  = (char)184;
          val       = (char)184;
        }
#else /* !( SOLID_COLOR_IS_CHECKERBOARD) */
#if SOLID_COLOR_IS_BLACK
        red_val   = (char)0;
        green_val = (char)0;
        blue_val  = (char)0;
        val       = (char)0;
#else /* !( SOLID_COLOR_IS_BLACK) */
        red_val   = (char)255;
        green_val = (char)255;
        blue_val  = (char)255;
        val       = (char)255;
#endif /* SOLID_COLOR_IS_BLACK */
#endif /* SOLID_COLOR_IS_CHECKERBOARD */

      } /* if( lattice->bc[subs][ n].bc_type == 0) else */

//printf("blue_val( %d, %d) = %d\n", i, j, (int)blue_val);

      if( fwrite( &blue_val, 1, 1, o) != 1) { printf("BOOM!\n"); process_exit(1);}
      //printf("BING %d %d\n", i, j);
      if( fwrite( &green_val, 1, 1, o) != 1) { printf("BOOM!\n"); process_exit(1);}
      //printf("BING %d %d\n", i, j);
      if( fwrite( &red_val, 1, 1, o) != 1) { printf("BOOM!\n"); process_exit(1);}
      //printf("BING %d %d\n", i, j);

    } /* for( i=0; i<get_LX(lattice); i++) */

    // Pad for 4-byte boundaries.
    val = (char)0;
    for( i=0; i<pad; i++)
    {
      if( fwrite( &val, 1, 1, o) != 1) { printf("BOOM!\n"); process_exit(1);}
    }

  } /* for( j=0; j<get_LY(lattice); j++) */

  fclose(o);

#if VERBOSITY_LEVEL > 0
  printf("rho2bmp() -- Wrote file \"%s\".\n", filename);
#endif /* VERBOSITY_LEVEL > 0 */

 } /* for( subs=0; subs<NUM_FLUID_COMPONENTS; subs++) */

  if( lattice->param.use_colormap)
  {
    deallocate_colormap( &colormap, num_colors);
  }

#if SAY_HI
  printf("rho2bmp() -- Bye!\n");
  printf("\n");
#endif /* SAY_HI */

} /* rho2bmp( lattice_ptr lattice, int time) */
                                                                        // }}}
#else
// R H O 2 B M P  {{{
//##############################################################################
// void rho2bmp( char *filename, int time)
//
void rho2bmp( lattice_ptr lattice, int time)
{
  FILE   *in,
         *o;
  int    i, j,
         n, m;
  int    pad,
         bytes_per_row;
  int    frame;
  char   k;
  char   b;
  struct bitmap_file_header bmfh;
  struct bitmap_info_header bmih;
  struct rgb_quad rgb;
  int    *int_ptr;
  short  int *short_int_ptr;
  int    *width_ptr;
  int    *height_ptr;
  short  int *bitcount_ptr;
  char   filename[1024];
  char   red_val,
         green_val,
         blue_val,
         val;
  double min_rho, max_rho;
  int    subs;

#if SAY_HI
  printf("rho2bmp() -- Hi!\n");
#endif /* SAY_HI */

 compute_max_rho( lattice, &min_rho, 0);
 compute_max_rho( lattice, &max_rho, 1);

 for( subs=0; subs<NUM_FLUID_COMPONENTS; subs++)
 {

  frame = time/lattice->param.FrameRate;

  sprintf( filename, "./in/%dx%d_proc%04d.bmp",
    get_LX(lattice), get_LY(lattice), get_proc_id(lattice));
  if( !( in = fopen( filename, "r")))
  {
    printf("%s %d >> rho2bmp() -- Error opening file \"%s\".\n",
      __FILE__,__LINE__,filename);
    process_exit(1);
  }

  // n = fread( void *BUF, size_t SIZE, size_t COUNT, FILE *FP);

  n = fread( &bmfh, sizeof(struct bitmap_file_header), 1, in );
  if( strncmp(bmfh.bfType,"BM",2))
  {
    printf("ERROR: Can't process this file type.  Exiting!\n");
    printf("\n");
    process_exit(1);
  }
  n = fread( &bmih, sizeof(struct bitmap_info_header), 1, in );
  int_ptr = (int*)bmih.biCompression;
  if( *int_ptr != 0)
  {
    printf("ERROR: Can't handle compression.  Exiting!\n");
    printf("\n");
    process_exit(1);
  }

  width_ptr = (int*)bmih.biWidth;
  height_ptr = (int*)bmih.biHeight;
  bitcount_ptr = (short int*)bmih.biBitCount;

  // Read palette entries, if applicable.
  if( ENDIAN2(*bitcount_ptr) < 24)
  {
    n = (int)pow(2.,(double)ENDIAN2(*bitcount_ptr)); // Num palette entries.
    for( i=0; i<n; i++)
    {
      k = fread( &rgb, sizeof(struct rgb_quad), 1, in );
      if( k!=1)
      {
        printf("Error reading palette entry %d.  Exiting!\n", i);
        process_exit(1);
      }
    }
  }

  fclose(in);

  // Bytes per row of the bitmap.
  bytes_per_row =
    ((int)ceil(( (((double)(ENDIAN4(*width_ptr)))*((double)(ENDIAN2(*bitcount_ptr))))/8.)));

  // Bitmaps pad rows to preserve 4-byte boundaries.
  // The length of a row in the file will be bytes_per_row + pad .
  pad = ((4) - bytes_per_row%4)%4;

  sprintf( filename, "%s/rho%dx%d_frame%04d_subs%02d.bmp", get_out_path(lattice),
      get_LX(lattice),
      get_LY(lattice),
      frame, subs);
  if( !( o = fopen( filename, "w+")))
  {
    printf("ERROR: fopen( \"%s\", \"w+\") = NULL.  Bye, bye!\n", filename);
    process_exit(1);
  }

  fwrite( &bmfh, sizeof(struct bitmap_file_header), 1, o );
  fwrite( &bmih, sizeof(struct bitmap_info_header), 1, o );

  for( j=0; j<get_LY(lattice); j++)
  {
    n = j*get_LX(lattice);

    for( i=0; i<get_LX(lattice); i++, n++)
    {
      if( lattice->bc[subs][ n].bc_type == /*FLUID_NODE*/0)
      {
        red_val = (char)0;
        green_val = (char)0;

        red_val =
(char)ROUND( 255.*(lattice->macro_vars[subs][ n].rho - min_rho)/(max_rho-min_rho));
        blue_val = (char)255-red_val;

      } /* if( lattice->bc[subs][ n].bc_type == 0) */

      else // lattice->bc[subs][ n].bc_type != 0
      {
#if SOLID_COLOR_IS_CHECKERBOARD
        // Checkerboard pattern over the solids and boundary conditions.
        if( (i+j)%2)
        {
          red_val   = (char)200;
          green_val = (char)200;
          blue_val  = (char)200;
          val       = (char)200;
        }
        else
        {
          red_val   = (char)184;
          green_val = (char)184;
          blue_val  = (char)184;
          val       = (char)184;
        }
#else /* !( SOLID_COLOR_IS_CHECKERBOARD) */
#if SOLID_COLOR_IS_BLACK
        red_val   = (char)0;
        green_val = (char)0;
        blue_val  = (char)0;
        val       = (char)0;
#else /* !( SOLID_COLOR_IS_BLACK) */
        red_val   = (char)255;
        green_val = (char)255;
        blue_val  = (char)255;
        val       = (char)255;
#endif /* SOLID_COLOR_IS_BLACK */
#endif /* SOLID_COLOR_IS_CHECKERBOARD */

      } /* if( lattice->bc[subs][ n].bc_type == 0) else */

//printf("blue_val( %d, %d) = %d\n", i, j, (int)blue_val);

      if( fwrite( &blue_val, 1, 1, o) != 1) { printf("BOOM!\n"); process_exit(1);}
      //printf("BING %d %d\n", i, j);
      if( fwrite( &green_val, 1, 1, o) != 1) { printf("BOOM!\n"); process_exit(1);}
      //printf("BING %d %d\n", i, j);
      if( fwrite( &red_val, 1, 1, o) != 1) { printf("BOOM!\n"); process_exit(1);}
      //printf("BING %d %d\n", i, j);

    } /* for( i=0; i<get_LX(lattice); i++) */

    // Pad for 4-byte boundaries.
    val = (char)0;
    for( i=0; i<pad; i++)
    {
      if( fwrite( &val, 1, 1, o) != 1) { printf("BOOM!\n"); process_exit(1);}
    }

  } /* for( j=0; j<get_LY(lattice); j++) */

  fclose(o);

#if VERBOSITY_LEVEL > 0
  printf("rho2bmp() -- Wrote file \"%s\".\n", filename);
#endif /* VERBOSITY_LEVEL > 0 */

 } /* for( subs=0; subs<NUM_FLUID_COMPONENTS; subs++) */

#if SAY_HI
  printf("rho2bmp() -- Bye!\n");
  printf("\n");
#endif /* SAY_HI */

} /* rho2bmp( lattice_ptr lattice, int time) */
                                                                        // }}}
#endif
// U 2 B M P  {{{
//##############################################################################
// void u2bmp( char *filename, int time)
//
void u2bmp( lattice_ptr lattice, int time)
{
  FILE   *in,
         *o_u,
         *o_ux,
         *o_uy;
  int    i, j,
         n, m;
  int    pad,
         bytes_per_row;
  int    frame;
  char   k;
  char   b;
  struct bitmap_file_header bmfh;
  struct bitmap_info_header bmih;
  struct rgb_quad rgb;
  int    *int_ptr;
  short  int *short_int_ptr;
  int    *width_ptr;
  int    *height_ptr;
  short  int *bitcount_ptr;
  char   filename[1024];
  char   red_val,
         green_val,
         blue_val,
         val;
  double max_u[2], maxu;
  double u_x, u_y, u;
  int    subs;

#if SAY_HI
  printf("u2bmp() -- Hi!\n");
#endif /* SAY_HI */

 for( subs=0; subs<NUM_FLUID_COMPONENTS; subs++)
 {

  frame = time/lattice->param.FrameRate;

#if 0
  sprintf( filename, "./in/%dx%d_proc%04d.bmp",
      get_LX(lattice), get_LY(lattice), get_proc_id(lattice));
  if( !( in = fopen( filename, "r")))
  {
    printf("%s %d >> u2bmp() -- Error opening file \"%s\".\n",
      __FILE__,__LINE__,filename);
    process_exit(1);
  }

  // n = fread( void *BUF, size_t SIZE, size_t COUNT, FILE *FP);

  n = fread( &bmfh, sizeof(struct bitmap_file_header), 1, in );
  if( strncmp(bmfh.bfType,"BM",2))
  {
    printf("ERROR: Can't process this file type.  Exiting!\n");
    printf("\n");
    process_exit(1);
  }
  n = fread( &bmih, sizeof(struct bitmap_info_header), 1, in );
  int_ptr = (int*)bmih.biCompression;
  if( *int_ptr != 0)
  {
    printf("ERROR: Can't handle compression.  Exiting!\n");
    printf("\n");
    process_exit(1);
  }

#if 0
  *((int*)(bmih.biWidth)) = ENDIAN4(((int)(*((int*)(bmih.biWidth)))));
  *((int*)(bmih.biHeight)) = ENDIAN4(((int)(*((int*)(bmih.biHeight)))));
  *((short int*)(bmih.biBitCount)) = ENDIAN2(((short int)(*((short int*)(bmih.biBitCount)))));
#endif

  width_ptr    = (int*)bmih.biWidth;
  height_ptr   = (int*)bmih.biHeight;
  bitcount_ptr = (short int*)bmih.biBitCount;

printf("%s %d >> width    = %d\n",__FILE__,__LINE__, ENDIAN4(*width_ptr)   );
printf("%s %d >> height   = %d\n",__FILE__,__LINE__, ENDIAN4(*height_ptr)  );
printf("%s %d >> bitcount = %d\n",__FILE__,__LINE__, ENDIAN2(*bitcount_ptr));

  // Read palette entries, if applicable.
  if( ENDIAN2(*bitcount_ptr) < 24)
  {
    n = (int)pow(2.,(double)ENDIAN2(*bitcount_ptr)); // Num palette entries.
    for( i=0; i<n; i++)
    {
      k = fread( &rgb, sizeof(struct rgb_quad), 1, in );
      if( k!=1)
      {
        printf("Error reading palette entry %d.  Exiting!\n", i);
        process_exit(1);
      }
    }
  }

  fclose(in);

#else
  bmfh.bfType[0] = 'B';
  bmfh.bfType[1] = 'M';
  *((int*)bmfh.bfSize)= get_LY(lattice)*(
    (int)ceil( ( ((double)get_LX(lattice))*( /*depth*/24.))/8.) // bytes per row
  + ( 4 - (int)ceil( ( ((double)get_LX(lattice))*( /*depth*/24.))/8.) % 4) % 4 // pad
  );
  *((short int*)bmfh.bfReserved1) = 0;
  *((short int*)bmfh.bfReserved2) = 0;
  *((int*)bmfh.bfOffBits) = 54; // 14 byte file header and 40 byte info header

  *((int*)bmih.biSize) = 40;
  *((int*)bmih.biWidth) = get_LX(lattice);
  *((int*)bmih.biHeight) = get_LY(lattice);
  *((short int*)bmih.biPlanes) = 1;
  *((short int*)bmih.biBitCount) = 24;
  *((int*)bmih.biCompression) = 0;
  *((int*)bmih.biSizeImage) = 0;
  *((int*)bmih.biXPelsPerMeter) = 0;
  *((int*)bmih.biYPelsPerMeter) = 0;
  *((int*)bmih.biClrUsed) = 0;
  *((int*)bmih.biClrImportant) = 0;

  width_ptr = (int*)bmih.biWidth;
  height_ptr = (int*)bmih.biHeight;
  bitcount_ptr = (short int*)bmih.biBitCount;

#endif

  // Bytes per row of the bitmap.
  bytes_per_row =
    ((int)ceil(( (((double)(ENDIAN4(*width_ptr)))*((double)(ENDIAN2(*bitcount_ptr))))/8.)));

  // Bitmaps pad rows to preserve 4-byte boundaries.
  // The length of a row in the file will be bytes_per_row + pad .
  pad = ((4) - bytes_per_row%4)%4;

  compute_max_u( lattice, max_u, subs);

  sprintf( filename, "%s/u%dx%d_frame%04d_subs%02d_proc%04d.bmp", get_out_path(lattice),
      get_LX(lattice),
      get_LY(lattice),
      frame, subs, get_proc_id(lattice));
  if( !( o_u = fopen( filename, "w+")))
  {
    printf("ERROR: fopen( \"%s\", \"w+\") = NULL.  Bye, bye!\n", filename);
    process_exit(1);
  }

  sprintf( filename, "%s/u_x%dx%d_frame%04d_subs%02d_proc%04d.bmp", get_out_path(lattice),
      get_LX(lattice),
      get_LY(lattice),
      frame, subs, get_proc_id(lattice));
  if( !( o_ux = fopen( filename, "w+")))
  {
    printf("ERROR: fopen( \"%s\", \"w+\") = NULL.  Bye, bye!\n", filename);
    process_exit(1);
  }

  sprintf( filename, "%s/u_y%dx%d_frame%04d_subs%02d_proc%04d.bmp", get_out_path(lattice),
      get_LX(lattice),
      get_LY(lattice),
      frame, subs, get_proc_id(lattice));
  if( !( o_uy = fopen( filename, "w+")))
  {
    printf("ERROR: fopen( \"%s\", \"w+\") = NULL.  Bye, bye!\n", filename);
    process_exit(1);
  }

  fwrite( &bmfh, sizeof(struct bitmap_file_header), 1, o_u );
  fwrite( &bmih, sizeof(struct bitmap_info_header), 1, o_u );

  fwrite( &bmfh, sizeof(struct bitmap_file_header), 1, o_ux );
  fwrite( &bmih, sizeof(struct bitmap_info_header), 1, o_ux );

  fwrite( &bmfh, sizeof(struct bitmap_file_header), 1, o_uy);
  fwrite( &bmih, sizeof(struct bitmap_info_header), 1, o_uy);

  //for( j=get_LY(lattice)-1; j>=0; j--)
  for( j=0; j<get_LY(lattice); j++)
  {
    n = j*get_LX(lattice);

    for( i=0; i<get_LX(lattice); i++, n++)
    {
      if( lattice->bc[subs][ n].bc_type == /*FLUID_NODE*/0)
      {
#if 1
        blue_val  = (char)0;
        green_val = (char)0;
        red_val   = (char)0;

        u_x = (lattice->macro_vars[subs][ n].u[0]);
        u_y = (lattice->macro_vars[subs][ n].u[1]);

        u = sqrt(u_x*u_x + u_y*u_y);
        maxu = sqrt( max_u[0]*max_u[0] + max_u[1]*max_u[1]);

        if( lattice->bc[subs][ n].bc_type & BC_SOLID_NODE)
        {
          blue_val  = (char)ROUND( 128.*fabs(u_x)/max_u[0]);
          green_val = (char)ROUND( 128.*fabs(u_y)/max_u[1]);

        } /* if( lattice->bc[subs][ n].bc_type & BC_SOLID_NODE) */

        else // !( lattice->bc[subs][ n].bc_type & BC_SOLID_NODE)
        {
#if 0
          blue_val  = (char)ROUND(  255.*fabs(u_x)/max_u[0]);
          green_val = (char)ROUND(  255.*fabs(u_y)/max_u[1]);
          red_val   = 0.;//(char)ROUND( 128.*fabs(u)/maxu);
#else
          blue_val  = (char)ROUND(  255.*((fabs(u_x)!=0.)?(fabs(u_x)/max_u[0]):(0.)));
          green_val = (char)ROUND(  255.*((fabs(u_y)!=0.)?(fabs(u_y)/max_u[1]):(0.)));
          red_val   = 0.;//(char)ROUND( 128.*((fabs(u  )!=0.)?(fabs(u  )/maxu):(0.)));
#endif

        } /* if( lattice->bc[subs][ n].bc_type & BC_SOLID_NODE) else */
#else
        blue_val  = (char)255;
        green_val = (char)255;
        red_val   = (char)255;

        u = sqrt(u_x*u_x + u_y*u_y);
        maxu = sqrt( max_u[0]*max_u[0] + max_u[1]*max_u[1]);

        //if( fabs(u) > .1*maxu)
        //{
          green_val  = (char)ROUND( 255.-255.*fabs(u)/maxu);
          red_val    = (char)ROUND( 255.-255.*fabs(u)/maxu);
          blue_val    = (char)ROUND( 255.-255.*fabs(u)/maxu);
        //}
        //else
        //{
        //  green_val  = (char)0;
        //  red_val    = (char)0;
        //  blue_val    = (char)0;
        //}
#endif

        val = (char)0;

      } /* if( lattice->bc[subs][ n].bc_type == 0) */

      else // lattice->bc[subs][ n].bc_type != 0
      {
#if SOLID_COLOR_IS_CHECKERBOARD
        // Checkerboard pattern over the solids and boundary conditions.
        if( (i+j)%2)
        {
          red_val   = (char)200;
          green_val = (char)200;
          blue_val  = (char)200;
          val       = (char)200;
        }
        else
        {
          red_val   = (char)184;
          green_val = (char)184;
          blue_val  = (char)184;
          val       = (char)184;
        }
#else /* !( SOLID_COLOR_IS_CHECKERBOARD) */
#if SOLID_COLOR_IS_BLACK
        red_val   = (char)0;
        green_val = (char)0;
        blue_val  = (char)0;
        val       = (char)0;
#else /* !( SOLID_COLOR_IS_BLACK) */
        red_val   = (char)255;
        green_val = (char)255;
        blue_val  = (char)255;
        val       = (char)255;
#endif /* SOLID_COLOR_IS_BLACK */
#endif /* SOLID_COLOR_IS_CHECKERBOARD */

      } /* if( lattice->bc[subs][ n].bc_type == 0) else */

#if MARK_ORIGIN_FOR_REFERENCE
 // Mark the origin for reference.
 if( ( i == 0 && j == 0))
 {
   red_val   = (char)255;
   green_val = (char)255;
   blue_val  = (char)255;
   val       = (char)255;
 }
#endif /* MARK_ORIGIN_FOR_REFERENCE */

      if( fwrite( &blue_val,  1, 1, o_u ) != 1) { printf("BOOM!\n"); process_exit(1);}
      if( fwrite( &green_val, 1, 1, o_u ) != 1) { printf("BOOM!\n"); process_exit(1);}
      if( fwrite( &red_val,   1, 1, o_u ) != 1) { printf("BOOM!\n"); process_exit(1);}

      if( lattice->bc[subs][ n].bc_type == /*FLUID_NODE*/0)
      {
        blue_val  = (char)0;
        green_val = (char)0;
        red_val   = (char)0;

        u_x = (lattice->macro_vars[subs][ n].u[0]);
        u_y = (lattice->macro_vars[subs][ n].u[1]);

        val = (char)0;

        if( lattice->bc[subs][ n].bc_type & BC_SOLID_NODE)
        {
          if( u_x > 0)
          {
            red_val = val;
            blue_val = (char)ROUND( 128.*fabs(u_x)/max_u[0]);
          }
          else
          {
            red_val = (char)ROUND( 128.*fabs(u_x)/max_u[0]);
            blue_val = val;
          }

        } /* if( lattice->bc[subs][ n].bc_type & BC_SOLID_NODE) */

        else // !( lattice->bc[subs][ n].bc_type & BC_SOLID_NODE)
        {
          if( u_x > 0)
          {
            red_val = val;
            blue_val = (char)ROUND( 255.*((fabs(u_x)!=0.)?(fabs(u_x)/max_u[0]):(0.)));
          }
          else
          {
            red_val = (char)ROUND( 255.*((fabs(u_x)!=0.)?(fabs(u_x)/max_u[0]):(0.)));
            blue_val = val;
          }

        } /* if( lattice->bc[subs][ n].bc_type & BC_SOLID_NODE) else */

      } /* if( lattice->bc[subs][ n].bc_type == 0) */

      else // lattice->bc[subs][ n].bc_type != 0
      {
#if SOLID_COLOR_IS_CHECKERBOARD
        // Checkerboard pattern over the solids and boundary conditions.
        if( (i+j)%2)
        {
          red_val   = (char)200;
          green_val = (char)200;
          blue_val  = (char)200;
          val       = (char)200;
        }
        else
        {
          red_val   = (char)184;
          green_val = (char)184;
          blue_val  = (char)184;
          val       = (char)184;
        }
#else /* !( SOLID_COLOR_IS_CHECKERBOARD) */
#if SOLID_COLOR_IS_BLACK
        red_val   = (char)0;
        green_val = (char)0;
        blue_val  = (char)0;
        val       = (char)0;
#else /* !( SOLID_COLOR_IS_BLACK) */
        red_val   = (char)255;
        green_val = (char)255;
        blue_val  = (char)255;
        val       = (char)255;
#endif /* SOLID_COLOR_IS_BLACK */
#endif /* SOLID_COLOR_IS_CHECKERBOARD */

      } /* if( lattice->bc[subs][ n].bc_type == 0) else */

#if MARK_ORIGIN_FOR_REFERENCE
 // Mark the origin for reference.
 if( ( i == 0 && j == 0))
 {
   red_val   = (char)255;
   green_val = (char)255;
   blue_val  = (char)255;
   val       = (char)255;
 }
#endif /* MARK_ORIGIN_FOR_REFERENCE */

      if( fwrite( &blue_val,  1, 1, o_ux) != 1) { printf("BOOM!\n"); process_exit(1);}
      if( fwrite( &val,       1, 1, o_ux) != 1) { printf("BOOM!\n"); process_exit(1);}
      if( fwrite( &red_val,   1, 1, o_ux) != 1) { printf("BOOM!\n"); process_exit(1);}

      if( lattice->bc[subs][ n].bc_type == /*FLUID_NODE*/0)
      {
        blue_val  = (char)0;
        green_val = (char)0;
        red_val   = (char)0;

        u_x = (lattice->macro_vars[subs][ n].u[0]);
        u_y = (lattice->macro_vars[subs][ n].u[1]);

        val = (char)0;

        if( lattice->bc[subs][ n].bc_type & BC_SOLID_NODE)
        {
          if( u_y > 0)
          {
            red_val = val;
            green_val = (char)ROUND( 128.*((fabs(u_y)!=0.)?(fabs(u_y)/max_u[1]):(0.)));
          }
          else
          {
            red_val = (char)ROUND( 128.*((fabs(u_y)!=0.)?(fabs(u_y)/max_u[1]):(0.)));
            green_val = val;
          }

        } /* if( lattice->bc[subs][ n].bc_type & BC_SOLID_NODE) */

        else // !( lattice->bc[subs][ n].bc_type & BC_SOLID_NODE)
        {
          if( u_y > 0)
          {
            blue_val = (char)ROUND( 255.*((fabs(u_y)!=0.)?(fabs(u_y)/max_u[1]):(0.)));
            green_val = val;
          }
          else
          {
            blue_val = val;
            green_val = (char)ROUND( 255.*((fabs(u_y)!=0.)?(fabs(u_y)/max_u[1]):(0.)));
          }

        } /* if( lattice->bc[subs][ n].bc_type & BC_SOLID_NODE) else */

      } /* if( lattice->bc[subs][ n].bc_type == 0) */

      else // lattice->bc[subs][ n].bc_type != 0
      {
#if SOLID_COLOR_IS_CHECKERBOARD
        // Checkerboard pattern over the solids and boundary conditions.
        if( (i+j)%2)
        {
          red_val   = (char)200;
          green_val = (char)200;
          blue_val  = (char)200;
          val       = (char)200;
        }
        else
        {
          red_val   = (char)184;
          green_val = (char)184;
          blue_val  = (char)184;
          val       = (char)184;
        }
#else /* !( SOLID_COLOR_IS_CHECKERBOARD) */
#if SOLID_COLOR_IS_BLACK
        red_val   = (char)0;
        green_val = (char)0;
        blue_val  = (char)0;
        val       = (char)0;
#else /* !( SOLID_COLOR_IS_BLACK) */
        red_val   = (char)255;
        green_val = (char)255;
        blue_val  = (char)255;
        val       = (char)255;
#endif /* SOLID_COLOR_IS_BLACK */
#endif /* SOLID_COLOR_IS_CHECKERBOARD */

      } /* if( lattice->bc[subs][ n].bc_type == 0) else */

#if MARK_ORIGIN_FOR_REFERENCE
 // Mark the origin for reference.
 if( ( i == 0 && j == 0))
 {
   red_val   = (char)255;
   green_val = (char)255;
   blue_val  = (char)255;
   val       = (char)255;
 }
#endif /* MARK_ORIGIN_FOR_REFERENCE */

      if( fwrite( &blue_val,  1, 1, o_uy) != 1) { printf("BOOM!\n"); process_exit(1);}
      if( fwrite( &green_val, 1, 1, o_uy) != 1) { printf("BOOM!\n"); process_exit(1);}
      if( fwrite( &red_val,   1, 1, o_uy) != 1) { printf("BOOM!\n"); process_exit(1);}

    } /* for( i=0; i<get_LY(lattice); i++) */

    // Pad for 4-byte boundaries.
    val = (char)0;
    for( i=0; i<pad; i++)
    {
      if( fwrite( &val, 1, 1, o_u ) != 1) { printf("BOOM!\n"); process_exit(1);}
      if( fwrite( &val, 1, 1, o_ux) != 1) { printf("BOOM!\n"); process_exit(1);}
      if( fwrite( &val, 1, 1, o_uy) != 1) { printf("BOOM!\n"); process_exit(1);}
    }

  } /* for( j=0; j<get_LY(lattice); j++) */

  fclose(o_u );
  fclose(o_ux);
  fclose(o_uy);

#if VERBOSITY_LEVEL > 0
  sprintf( filename, "%s/u%dx%d_frame%04d_subs%02d_proc%04d.bmp", get_out_path(lattice),
      get_LX(lattice),
      get_LY(lattice),
      frame, subs, get_proc_id(lattice));
  printf("u2bmp()   -- Wrote file \"%s\".\n", filename);

  sprintf( filename, "%s/u_x%dx%d_frame%04d_subs%02d_proc%04d.bmp", get_out_path(lattice),
      get_LX(lattice),
      get_LY(lattice),
      frame, subs, get_proc_id(lattice));
  printf("u2bmp()   -- Wrote file \"%s\".\n", filename);

  sprintf( filename, "%s/u_y%dx%d_frame%04d_subs%02d_proc%04d.bmp", get_out_path(lattice),
      get_LX(lattice),
      get_LY(lattice),
      frame, subs, get_proc_id(lattice));
  printf("u2bmp()   -- Wrote file \"%s\".\n", filename);
#endif /* VERBOSITY_LEVEL > 0 */

 } /* for( subs=0; subs<NUM_FLUID_COMPONENTS; subs++) */

#if STORE_U_COMPOSITE

  frame = time/lattice->param.FrameRate;

#if !(PARALLEL)
  sprintf( filename, "./in/%dx%d.bmp",
      get_LX(lattice), get_LY(lattice));
#else /*#if PARALLEL*/
  sprintf( filename, "./in/%dx%d_proc%04d.bmp",
      get_LX(lattice), get_LY(lattice), get_proc_id(lattice));
#endif
  if( !( in = fopen( filename, "r")))
  {
    printf("%s %d >> u2bmp() -- Error opening file \"%s\".\n",
      __FILE__,__LINE__,filename);
    process_exit(1);
  }

  // n = fread( void *BUF, size_t SIZE, size_t COUNT, FILE *FP);

  n = fread( &bmfh, sizeof(struct bitmap_file_header), 1, in );
  if( strncmp(bmfh.bfType,"BM",2))
  {
    printf("ERROR: Can't process this file type.  Exiting!\n");
    printf("\n");
    process_exit(1);
  }
  n = fread( &bmih, sizeof(struct bitmap_info_header), 1, in );
  int_ptr = (int*)bmih.biCompression;
  if( *int_ptr != 0)
  {
    printf("ERROR: Can't handle compression.  Exiting!\n");
    printf("\n");
    process_exit(1);
  }

  width_ptr = (int*)bmih.biWidth;
  height_ptr = (int*)bmih.biHeight;
  bitcount_ptr = (short int*)bmih.biBitCount;

  // Read palette entries, if applicable.
  if( ENDIAN2(*bitcount_ptr) < 24)
  {
    n = (int)pow(2.,(double)ENDIAN2(*bitcount_ptr)); // Num palette entries.
    for( i=0; i<n; i++)
    {
      k = fread( &rgb, sizeof(struct rgb_quad), 1, in );
      if( k!=1)
      {
        printf("Error reading palette entry %d.  Exiting!\n", i);
        process_exit(1);
      }
    }
  }

  fclose(in);

  // Bytes per row of the bitmap.
  bytes_per_row =
    ((int)ceil(( (((double)(ENDIAN4(*width_ptr)))*((double)(ENDIAN2(*bitcount_ptr))))/8.)));

  // Bitmaps pad rows to preserve 4-byte boundaries.
  // The length of a row in the file will be bytes_per_row + pad .
  pad = ((4) - bytes_per_row%4)%4;

  compute_max_upr( lattice, max_u);

  sprintf( filename, "%s/upr%dx%d_frame%04d_proc%04d.bmp", get_out_path(lattice),
      get_LX(lattice),
      get_LY(lattice),
      frame, get_proc_id(lattice));
  if( !( o_u = fopen( filename, "w+")))
  {
    printf("ERROR: fopen( \"%s\", \"w+\") = NULL.  Bye, bye!\n", filename);
    process_exit(1);
  }

  sprintf( filename, "%s/upr_x%dx%d_frame%04d_proc%04d.bmp", get_out_path(lattice),
      get_LX(lattice),
      get_LY(lattice),
      frame, get_proc_id(lattice));
  if( !( o_ux = fopen( filename, "w+")))
  {
    printf("ERROR: fopen( \"%s\", \"w+\") = NULL.  Bye, bye!\n", filename);
    process_exit(1);
  }

  sprintf( filename, "%s/upr_y%dx%d_frame%04d_proc%04d.bmp", get_out_path(lattice),
      get_LX(lattice),
      get_LY(lattice),
      frame, get_proc_id(lattice));
  if( !( o_uy = fopen( filename, "w+")))
  {
    printf("ERROR: fopen( \"%s\", \"w+\") = NULL.  Bye, bye!\n", filename);
    process_exit(1);
  }

  fwrite( &bmfh, sizeof(struct bitmap_file_header), 1, o_u );
  fwrite( &bmih, sizeof(struct bitmap_info_header), 1, o_u );

  fwrite( &bmfh, sizeof(struct bitmap_file_header), 1, o_ux );
  fwrite( &bmih, sizeof(struct bitmap_info_header), 1, o_ux );

  fwrite( &bmfh, sizeof(struct bitmap_file_header), 1, o_uy);
  fwrite( &bmih, sizeof(struct bitmap_info_header), 1, o_uy);

  //for( j=get_LY(lattice)-1; j>=0; j--)
  for( j=0; j<get_LY(lattice); j++)
  {
    n = j*get_LX(lattice);

    for( i=0; i<get_LX(lattice); i++, n++)
    {
      if( lattice->bc[0][ n].bc_type == /*FLUID_NODE*/0)
      {
#if 1
        blue_val  = (char)0;
        green_val = (char)0;
        red_val   = (char)0;

        u_x = (lattice->upr[ n].u[0]);
        u_y = (lattice->upr[ n].u[1]);

        u = sqrt(u_x*u_x + u_y*u_y);
        maxu = sqrt( max_u[0]*max_u[0] + max_u[1]*max_u[1]);

        if( lattice->bc[0][ n].bc_type & BC_SOLID_NODE)
        {
          blue_val  = (char)ROUND( 128.*fabs(u_x)/max_u[0]);
          green_val = (char)ROUND( 128.*fabs(u_y)/max_u[1]);

        } /* if( lattice->bc[subs][ n].bc_type & BC_SOLID_NODE) */

        else // !( lattice->bc[subs][ n].bc_type & BC_SOLID_NODE)
        {
          blue_val  = (char)ROUND(  255.*fabs(u_x)/max_u[0]);
          green_val = (char)ROUND(  255.*fabs(u_y)/max_u[1]);
          //red_val    = (char)ROUND( 128.*fabs(u)/maxu);

        } /* if( lattice->bc[subs][ n].bc_type & BC_SOLID_NODE) else */
#else
        blue_val  = (char)255;
        green_val = (char)255;
        red_val   = (char)255;

        u = sqrt(u_x*u_x + u_y*u_y);
        maxu = sqrt( max_u[0]*max_u[0] + max_u[1]*max_u[1]);

        //if( fabs(u) > .1*maxu)
        //{
          green_val  = (char)ROUND( 255.-255.*fabs(u)/maxu);
          red_val    = (char)ROUND( 255.-255.*fabs(u)/maxu);
          blue_val    = (char)ROUND( 255.-255.*fabs(u)/maxu);
        //}
        //else
        //{
        //  green_val  = (char)0;
        //  red_val    = (char)0;
        //  blue_val    = (char)0;
        //}
#endif

        val = (char)0;

      } /* if( lattice->bc[subs][ n].bc_type == 0) */

      else // lattice->bc[subs][ n].bc_type != 0
      {
#if SOLID_COLOR_IS_CHECKERBOARD
        // Checkerboard pattern over the solids and boundary conditions.
        if( (i+j)%2)
        {
          red_val   = (char)200;
          green_val = (char)200;
          blue_val  = (char)200;
          val       = (char)200;
        }
        else
        {
          red_val   = (char)184;
          green_val = (char)184;
          blue_val  = (char)184;
          val       = (char)184;
        }
#else /* !( SOLID_COLOR_IS_CHECKERBOARD) */
#if SOLID_COLOR_IS_BLACK
        red_val   = (char)0;
        green_val = (char)0;
        blue_val  = (char)0;
        val       = (char)0;
#else /* !( SOLID_COLOR_IS_BLACK) */
        red_val   = (char)255;
        green_val = (char)255;
        blue_val  = (char)255;
        val       = (char)255;
#endif /* SOLID_COLOR_IS_BLACK */
#endif /* SOLID_COLOR_IS_CHECKERBOARD */

      } /* if( lattice->bc[subs][ n].bc_type == 0) else */

#if MARK_ORIGIN_FOR_REFERENCE
 // Mark the origin for reference.
 if( ( i == 0 && j == 0))
 {
   red_val   = (char)255;
   green_val = (char)255;
   blue_val  = (char)255;
   val       = (char)255;
 }
#endif /* MARK_ORIGIN_FOR_REFERENCE */

      if( fwrite( &blue_val,  1, 1, o_u ) != 1) { printf("BOOM!\n"); process_exit(1);}
      if( fwrite( &green_val, 1, 1, o_u ) != 1) { printf("BOOM!\n"); process_exit(1);}
      if( fwrite( &red_val,   1, 1, o_u ) != 1) { printf("BOOM!\n"); process_exit(1);}

      if( lattice->bc[0][ n].bc_type == /*FLUID_NODE*/0)
      {
        blue_val  = (char)0;
        green_val = (char)0;
        red_val   = (char)0;

        u_x = (lattice->upr[ n].u[0]);
        u_y = (lattice->upr[ n].u[1]);

        val = (char)0;

        if( lattice->bc[0][ n].bc_type & BC_SOLID_NODE)
        {
          if( u_x > 0)
          {
            red_val = val;
            blue_val = (char)ROUND( 128.*fabs(u_x)/max_u[0]);
          }
          else
          {
            red_val = (char)ROUND( 128.*fabs(u_x)/max_u[0]);
            blue_val = val;
          }

        } /* if( lattice->bc[subs][ n].bc_type & BC_SOLID_NODE) */

        else // !( lattice->bc[subs][ n].bc_type & BC_SOLID_NODE)
        {
          if( u_x > 0)
          {
            red_val = val;
            blue_val = (char)ROUND( 255.*fabs(u_x)/max_u[0]);
          }
          else
          {
            red_val = (char)ROUND( 255.*fabs(u_x)/max_u[0]);
            blue_val = val;
          }

        } /* if( lattice->bc[subs][ n].bc_type & BC_SOLID_NODE) else */

      } /* if( lattice->bc[subs][ n].bc_type == 0) */

      else // lattice->bc[subs][ n].bc_type != 0
      {
#if SOLID_COLOR_IS_CHECKERBOARD
        // Checkerboard pattern over the solids and boundary conditions.
        if( (i+j)%2)
        {
          red_val   = (char)200;
          green_val = (char)200;
          blue_val  = (char)200;
          val       = (char)200;
        }
        else
        {
          red_val   = (char)184;
          green_val = (char)184;
          blue_val  = (char)184;
          val       = (char)184;
        }
#else /* !( SOLID_COLOR_IS_CHECKERBOARD) */
#if SOLID_COLOR_IS_BLACK
        red_val   = (char)0;
        green_val = (char)0;
        blue_val  = (char)0;
        val       = (char)0;
#else /* !( SOLID_COLOR_IS_BLACK) */
        red_val   = (char)255;
        green_val = (char)255;
        blue_val  = (char)255;
        val       = (char)255;
#endif /* SOLID_COLOR_IS_BLACK */
#endif /* SOLID_COLOR_IS_CHECKERBOARD */

      } /* if( lattice->bc[subs][ n].bc_type == 0) else */

#if MARK_ORIGIN_FOR_REFERENCE
 // Mark the origin for reference.
 if( ( i == 0 && j == 0))
 {
   red_val   = (char)255;
   green_val = (char)255;
   blue_val  = (char)255;
   val       = (char)255;
 }
#endif /* MARK_ORIGIN_FOR_REFERENCE */

      if( fwrite( &blue_val,  1, 1, o_ux) != 1) { printf("BOOM!\n"); process_exit(1);}
      if( fwrite( &val,       1, 1, o_ux) != 1) { printf("BOOM!\n"); process_exit(1);}
      if( fwrite( &red_val,   1, 1, o_ux) != 1) { printf("BOOM!\n"); process_exit(1);}

      if( lattice->bc[0][ n].bc_type == /*FLUID_NODE*/0)
      {
        blue_val  = (char)0;
        green_val = (char)0;
        red_val   = (char)0;

        u_x = (lattice->upr[ n].u[0]);
        u_y = (lattice->upr[ n].u[1]);

        val = (char)0;

        if( lattice->bc[0][ n].bc_type & BC_SOLID_NODE)
        {
          if( u_y > 0)
          {
            red_val = val;
            green_val = (char)ROUND( 128.*fabs(u_y)/max_u[1]);
          }
          else
          {
            red_val = (char)ROUND( 128.*fabs(u_y)/max_u[1]);
            green_val = val;
          }

        } /* if( lattice->bc[subs][ n].bc_type & BC_SOLID_NODE) */

        else // !( lattice->bc[subs][ n].bc_type & BC_SOLID_NODE)
        {
          if( u_y > 0)
          {
            blue_val = (char)ROUND( 255.*fabs(u_y)/max_u[1]);
            green_val = val;
          }
          else
          {
            blue_val = val;
            green_val = (char)ROUND( 255.*fabs(u_y)/max_u[1]);
          }

        } /* if( lattice->bc[subs][ n].bc_type & BC_SOLID_NODE) else */

      } /* if( lattice->bc[subs][ n].bc_type == 0) */

      else // lattice->bc[subs][ n].bc_type != 0
      {
#if SOLID_COLOR_IS_CHECKERBOARD
        // Checkerboard pattern over the solids and boundary conditions.
        if( (i+j)%2)
        {
          red_val   = (char)200;
          green_val = (char)200;
          blue_val  = (char)200;
          val       = (char)200;
        }
        else
        {
          red_val   = (char)184;
          green_val = (char)184;
          blue_val  = (char)184;
          val       = (char)184;
        }
#else /* !( SOLID_COLOR_IS_CHECKERBOARD) */
#if SOLID_COLOR_IS_BLACK
        red_val   = (char)0;
        green_val = (char)0;
        blue_val  = (char)0;
        val       = (char)0;
#else /* !( SOLID_COLOR_IS_BLACK) */
        red_val   = (char)255;
        green_val = (char)255;
        blue_val  = (char)255;
        val       = (char)255;
#endif /* SOLID_COLOR_IS_BLACK */
#endif /* SOLID_COLOR_IS_CHECKERBOARD */

      } /* if( lattice->bc[subs][ n].bc_type == 0) else */

#if MARK_ORIGIN_FOR_REFERENCE
 // Mark the origin for reference.
 if( ( i == 0 && j == 0))
 {
   red_val   = (char)255;
   green_val = (char)255;
   blue_val  = (char)255;
   val       = (char)255;
 }
#endif /* MARK_ORIGIN_FOR_REFERENCE */

      if( fwrite( &blue_val,  1, 1, o_uy) != 1) { printf("BOOM!\n"); process_exit(1);}
      if( fwrite( &green_val, 1, 1, o_uy) != 1) { printf("BOOM!\n"); process_exit(1);}
      if( fwrite( &red_val,   1, 1, o_uy) != 1) { printf("BOOM!\n"); process_exit(1);}

    } /* for( i=0; i<get_LY(lattice); i++) */

    // Pad for 4-byte boundaries.
    val = (char)0;
    for( i=0; i<pad; i++)
    {
      if( fwrite( &val, 1, 1, o_u ) != 1) { printf("BOOM!\n"); process_exit(1);}
      if( fwrite( &val, 1, 1, o_ux) != 1) { printf("BOOM!\n"); process_exit(1);}
      if( fwrite( &val, 1, 1, o_uy) != 1) { printf("BOOM!\n"); process_exit(1);}
    }

  } /* for( j=0; j<get_LY(lattice); j++) */

  fclose(o_u );
  fclose(o_ux);
  fclose(o_uy);

#if VERBOSITY_LEVEL > 0
  sprintf( filename, "%s/upr%dx%d_frame%04d_proc%04d.bmp", get_out_path(lattice),
      get_LX(lattice), get_LY(lattice), frame, get_proc_id(lattice));
  printf("u2bmp()   -- Wrote file \"%s\".\n", filename);

  sprintf( filename, "%s/upr_x%dx%d_frame%04d_proc%04d.bmp", get_out_path(lattice),
      get_LX(lattice), get_LY(lattice), frame, get_proc_id(lattice));
  printf("u2bmp()   -- Wrote file \"%s\".\n", filename);

  sprintf( filename, "%s/upr_y%dx%d_frame%04d_proc%04d.bmp", get_out_path(lattice),
      get_LX(lattice), get_LY(lattice), frame, get_proc_id(lattice));
  printf("u2bmp()   -- Wrote file \"%s\".\n", filename);
#endif /* VERBOSITY_LEVEL > 0 */
#endif /* STORE_U_COMPOSITE */


#if SAY_HI
  printf("u2bmp() -- Bye!\n");
  printf("\n");
#endif /* SAY_HI */

} /* u2bmp( lattice_ptr lattice, int time) */
                                                                        // }}}
// V O R 2 B M P  {{{
//##############################################################################
// void vor2bmp( char *filename, int time)
//
void vor2bmp( lattice_ptr lattice, int time)
{
  FILE   *in,
         *o_vor;
  int    i, j,
         n, m;
  int    pad,
         bytes_per_row;
  int    frame;
  char   k;
  char   b;
  struct bitmap_file_header bmfh;
  struct bitmap_info_header bmih;
  struct rgb_quad rgb;
  int    *int_ptr;
  short  int *short_int_ptr;
  int    *width_ptr;
  int    *height_ptr;
  short  int *bitcount_ptr;
  char   filename[1024];
  char   red_val,
         green_val,
         blue_val,
         val;
  double max_vor_p, max_vor_n;
  double ave_vor_p, ave_vor_n;
  double vor;
  int    subs;

#if SAY_HI
  printf("vor2bmp() -- Hi!\n");
#endif /* SAY_HI */

 for( subs=0; subs<NUM_FLUID_COMPONENTS; subs++)
 {

  frame = time/lattice->param.FrameRate;

#if 0
  sprintf( filename, "./in/%dx%d_proc%04d.bmp",
      get_LX(lattice), get_LY(lattice), get_proc_id(lattice));
  if( !( in = fopen( filename, "r")))
  {
    printf("%s %d >> vor2bmp() -- Error opening file \"%s\".\n",
      __FILE__,__LINE__,filename);
    process_exit(1);
  }

  // n = fread( void *BUF, size_t SIZE, size_t COUNT, FILE *FP);

  n = fread( &bmfh, sizeof(struct bitmap_file_header), 1, in );
  if( strncmp(bmfh.bfType,"BM",2))
  {
    printf("ERROR: Can't process this file type.  Exiting!\n");
    printf("\n");
    process_exit(1);
  }
  n = fread( &bmih, sizeof(struct bitmap_info_header), 1, in );
  int_ptr = (int*)bmih.biCompression;
  if( *int_ptr != 0)
  {
    printf("ERROR: Can't handle compression.  Exiting!\n");
    printf("\n");
    process_exit(1);
  }

  width_ptr = (int*)bmih.biWidth;
  height_ptr = (int*)bmih.biHeight;
  bitcount_ptr = (short int*)bmih.biBitCount;

  // Read palette entries, if applicable.
  if( ENDIAN2(*bitcount_ptr) < 24)
  {
    n = (int)pow(2.,(double)ENDIAN2(*bitcount_ptr)); // Num palette entries.
    for( i=0; i<n; i++)
    {
      k = fread( &rgb, sizeof(struct rgb_quad), 1, in );
      if( k!=1)
      {
        printf("Error reading palette entry %d.  Exiting!\n", i);
        process_exit(1);
      }
    }
  }

  fclose(in);
#else
  bmfh.bfType[0] = 'B';
  bmfh.bfType[1] = 'M';
  *((int*)bmfh.bfSize)= get_LY(lattice)*(
    (int)ceil( ( ((double)get_LX(lattice))*( /*depth*/24.))/8.) // bytes per row
  + ( 4 - (int)ceil( ( ((double)get_LX(lattice))*( /*depth*/24.))/8.) % 4) % 4 // pad
  );
  *((short int*)bmfh.bfReserved1) = 0;
  *((short int*)bmfh.bfReserved2) = 0;
  *((int*)bmfh.bfOffBits) = 54; // 14 byte file header and 40 byte info header

  *((int*)bmih.biSize) = 40;
  *((int*)bmih.biWidth) = get_LX(lattice);
  *((int*)bmih.biHeight) = get_LY(lattice);
  *((short int*)bmih.biPlanes) = 1;
  *((short int*)bmih.biBitCount) = 24;
  *((int*)bmih.biCompression) = 0;
  *((int*)bmih.biSizeImage) = 0;
  *((int*)bmih.biXPelsPerMeter) = 0;
  *((int*)bmih.biYPelsPerMeter) = 0;
  *((int*)bmih.biClrUsed) = 0;
  *((int*)bmih.biClrImportant) = 0;

  width_ptr = (int*)bmih.biWidth;
  height_ptr = (int*)bmih.biHeight;
  bitcount_ptr = (short int*)bmih.biBitCount;

#endif

  // Bytes per row of the bitmap.
  bytes_per_row =
    ((int)ceil(( (((double)(ENDIAN4(*width_ptr)))*((double)(ENDIAN2(*bitcount_ptr))))/8.)));

  // Bitmaps pad rows to preserve 4-byte boundaries.
  // The length of a row in the file will be bytes_per_row + pad .
  pad = ((4) - bytes_per_row%4)%4;

  compute_max_vor( lattice, &max_vor_p, &max_vor_n, subs);
#if 0 && VERBOSITY_LEVEL > 0
  printf("vor2bmp() -- max_vor_p = %f\n", max_vor_p);
  printf("vor2bmp() -- max_vor_n = %f\n", max_vor_n);
#endif /* 0 && VERBOSITY_LEVEL > 0 */
  compute_ave_vor( lattice, &ave_vor_p, &ave_vor_n, subs);
#if 0 && VERBOSITY_LEVEL > 0
  printf("vor2bmp() -- ave_vor_p = %f\n", ave_vor_p);
  printf("vor2bmp() -- ave_vor_n = %f\n", ave_vor_n);
#endif /* 0 && VERBOSITY_LEVEL > 0 */

  sprintf( filename, "%s/vor%dx%d_frame%04d_subs%02d_proc%04d.bmp", get_out_path(lattice),
      get_LX(lattice), get_LY(lattice), frame, subs, get_proc_id(lattice));
  if( !( o_vor = fopen( filename, "w+")))
  {
    printf("ERROR: fopen( \"%s\", \"w+\") = NULL.  Bye, bye!\n", filename);
    process_exit(1);
  }

  fwrite( &bmfh, sizeof(struct bitmap_file_header), 1, o_vor );
  fwrite( &bmih, sizeof(struct bitmap_info_header), 1, o_vor );

  //for( j=get_LY(lattice)-1; j>=0; j--)
  for( j=0; j<get_LY(lattice); j++)
  {
    n = j*get_LX(lattice);

    for( i=0; i<get_LX(lattice); i++, n++)
    {
      if( lattice->bc[subs][ n].bc_type == /*FLUID_NODE*/0)
      {
        //u_x = (lattice->macro_vars[subs][ n].u[0]);
        //u_y = (lattice->macro_vars[subs][ n].u[1]);

          compute_vorticity( lattice, i, j, n, &vor, subs);
//if( fabs(vor)/max_vor_p > .5)
//{
// printf("vor2bmp() -- vor/max_vor_p = %f/%f = %f\n", vor, max_vor_p, vor/max_vor_p);
//}
#if 0
        blue_val  = (char)255;
        green_val = (char)255;
        red_val   = (char)255;

          if( vor > 0)
          {
            if( 100.*vor/ave_vor_p > 1.)
            {
//printf("vor2bmp() -- vor/ave_vor_p = %f > 1.\n", vor/ave_vor_p);
              red_val  = (char)0;
              green_val  = (char)0;
            }
            else
            {
//printf("vor2bmp() -- vor/ave_vor_p = %f <= 1.\n", vor/ave_vor_p);
red_val  = (char)ROUND( 255.*( 1. - 10000.*(vor/ave_vor_p)*(vor/ave_vor_p)));
green_val  = (char)ROUND( 255.*( 1. - 10000.*(vor/ave_vor_p)*(vor/ave_vor_p)));
            }
          }
          else
          {
            if( 100.*vor/ave_vor_n > 1.)
            {
//printf("vor2bmp() -- vor/ave_vor_n = %f > 1.\n", vor/ave_vor_n);
              red_val = (char)0;
              blue_val = (char)0;
            }
            else
            {
//printf("vor2bmp() -- vor/ave_vor_n = %f <= 1.\n", vor/ave_vor_n);
red_val = (char)ROUND( 255.*( 1. - 10000.*(vor/ave_vor_n)*(vor/ave_vor_n)));
blue_val = (char)ROUND( 255.*( 1. - 10000.*(vor/ave_vor_n)*(vor/ave_vor_n)));
            }
          }
#else
#if 0
        blue_val  = (char)0;
        green_val = (char)0;
        red_val   = (char)0;

//        blue_val =
//(char)ROUND( 255.*(vor - max_vor_n)/(max_vor_p-max_vor_n));
        if( vor >= 0.)
        {
          blue_val =
(char)ROUND( 255.*(vor)/(max_vor_p));

        }
        else
        {
          red_val   =
(char)ROUND( 255.*(vor)/(max_vor_n));
        }
#else
        red_val   = (char)ROUND( 255.*(vor - max_vor_n)/(max_vor_p-max_vor_n));
        green_val = (char)ROUND( 255.*(vor - max_vor_n)/(max_vor_p-max_vor_n));
        blue_val  = (char)ROUND( 255.*(vor - max_vor_n)/(max_vor_p-max_vor_n));
#endif

#endif

        val = (char)0;

      } /* if( lattice->bc[subs][ n].bc_type == 0) */

      else // lattice->bc[subs][ n].bc_type != 0
      {
#if SOLID_COLOR_IS_CHECKERBOARD
        // Checkerboard pattern over the solids and boundary conditions.
        if( (i+j)%2)
        {
          red_val   = (char)200;
          green_val = (char)200;
          blue_val  = (char)200;
          val       = (char)200;
        }
        else
        {
          red_val   = (char)184;
          green_val = (char)184;
          blue_val  = (char)184;
          val       = (char)184;
        }
#else /* !( SOLID_COLOR_IS_CHECKERBOARD) */
//#if SOLID_COLOR_IS_BLACK
//        red_val   = (char)0;
//        green_val = (char)0;
//        blue_val  = (char)0;
//        val       = (char)0;
//#else /* !( SOLID_COLOR_IS_BLACK) */
        red_val   = (char)255;
        green_val = (char)255;
        blue_val  = (char)255;
        val       = (char)255;
//#endif /* SOLID_COLOR_IS_BLACK */
#endif /* SOLID_COLOR_IS_CHECKERBOARD */

      } /* if( lattice->bc[subs][ n].bc_type == 0) else */

#if MARK_ORIGIN_FOR_REFERENCE
 // Mark the origin for reference.
 if( ( i == 0 && j == 0))
 {
   red_val   = (char)255;
   green_val = (char)255;
   blue_val  = (char)255;
   val       = (char)255;
 }
#endif /* MARK_ORIGIN_FOR_REFERENCE */

      if( fwrite( &blue_val,  1, 1, o_vor ) != 1) { printf("BOOM!\n"); process_exit(1);}
      if( fwrite( &green_val, 1, 1, o_vor ) != 1) { printf("BOOM!\n"); process_exit(1);}
      if( fwrite( &red_val,   1, 1, o_vor ) != 1) { printf("BOOM!\n"); process_exit(1);}

    } /* for( i=0; i<get_LY(lattice); i++) */

    // Pad for 4-byte boundaries.
    val = (char)0;
    for( i=0; i<pad; i++)
    {
      if( fwrite( &val, 1, 1, o_vor ) != 1) { printf("BOOM!\n"); process_exit(1);}
    }

  } /* for( j=0; j<get_LY(lattice); j++) */

  fclose(o_vor );

#if VERBOSITY_LEVEL > 0
  sprintf( filename, "%s/vor%dx%d_frame%04d_subs%02d_proc%04d.bmp", get_out_path(lattice),
      get_LX(lattice), get_LY(lattice), frame, subs, get_proc_id(lattice));
  printf("vor2bmp()   -- Wrote file \"%s\".\n", filename);
#endif /* VERBOSITY_LEVEL > 0 */

 } /* for( subs=0; subs<NUM_FLUID_COMPONENTS; subs++) */

#if SAY_HI
  printf("vor2bmp() -- Bye!\n");
  printf("\n");
#endif /* SAY_HI */

} /* vor2bmp( lattice_ptr lattice, int time) */
                                                                        // }}}
#if NON_LOCAL_FORCES
// F O R C E   2 B M P  {{{
//##############################################################################
// void force2bmp( char *filename, int time)
//
void force2bmp( lattice_ptr lattice)
{
  FILE   *in,
         *o_u,
         *o_ux,
         *o_uy;
  int    i, j,
         n, m;
  int    pad,
         bytes_per_row;
  int    frame;
  char   k;
  char   b;
  struct bitmap_file_header bmfh;
  struct bitmap_info_header bmih;
  struct rgb_quad rgb;
  int    *int_ptr;
  short  int *short_int_ptr;
  int    *width_ptr;
  int    *height_ptr;
  short  int *bitcount_ptr;
  char   filename[1024];
  char   red_val,
         green_val,
         blue_val,
         val;
  double max_force[2], maxforce;
  double u_x, u_y, u;
  int    subs;

#if SAY_HI
  printf("force2bmp() -- Hi!\n");
#endif /* SAY_HI */

 for( subs=0; subs<NUM_FLUID_COMPONENTS; subs++)
 {

  frame = lattice->time/lattice->param.FrameRate;

#if 0
  sprintf( filename, "./in/%dx%d_proc%04d.bmp",
      get_LX(lattice), get_LY(lattice), get_proc_id(lattice));
  if( !( in = fopen( filename, "r")))
  {
    printf("%s %d >> force2bmp() -- Error opening file \"%s\".\n",
      __FILE__,__LINE__,filename);
    process_exit(1);
  }

  // n = fread( void *BUF, size_t SIZE, size_t COUNT, FILE *FP);

  n = fread( &bmfh, sizeof(struct bitmap_file_header), 1, in );
  if( strncmp(bmfh.bfType,"BM",2))
  {
    printf("ERROR: Can't process this file type.  Exiting!\n");
    printf("\n");
    process_exit(1);
  }
  n = fread( &bmih, sizeof(struct bitmap_info_header), 1, in );
  int_ptr = (int*)bmih.biCompression;
  if( *int_ptr != 0)
  {
    printf("ERROR: Can't handle compression.  Exiting!\n");
    printf("\n");
    process_exit(1);
  }

  width_ptr = (int*)bmih.biWidth;
  height_ptr = (int*)bmih.biHeight;
  bitcount_ptr = (short int*)bmih.biBitCount;

  // Read palette entries, if applicable.
  if( ENDIAN2(*bitcount_ptr) < 24)
  {
    n = (int)pow(2.,(double)ENDIAN2(*bitcount_ptr)); // Num palette entries.
    for( i=0; i<n; i++)
    {
      k = fread( &rgb, sizeof(struct rgb_quad), 1, in );
      if( k!=1)
      {
        printf("Error reading palette entry %d.  Exiting!\n", i);
        process_exit(1);
      }
    }
  }

  fclose(in);
#else
  bmfh.bfType[0] = 'B';
  bmfh.bfType[1] = 'M';
  *((int*)bmfh.bfSize)= get_LY(lattice)*(
    (int)ceil( ( ((double)get_LX(lattice))*( /*depth*/24.))/8.) // bytes per row
  + ( 4 - (int)ceil( ( ((double)get_LX(lattice))*( /*depth*/24.))/8.) % 4) % 4 // pad
  );
  *((short int*)bmfh.bfReserved1) = 0;
  *((short int*)bmfh.bfReserved2) = 0;
  *((int*)bmfh.bfOffBits) = 54; // 14 byte file header and 40 byte info header

  *((int*)bmih.biSize) = 40;
  *((int*)bmih.biWidth) = get_LX(lattice);
  *((int*)bmih.biHeight) = get_LY(lattice);
  *((short int*)bmih.biPlanes) = 1;
  *((short int*)bmih.biBitCount) = 24;
  *((int*)bmih.biCompression) = 0;
  *((int*)bmih.biSizeImage) = 0;
  *((int*)bmih.biXPelsPerMeter) = 0;
  *((int*)bmih.biYPelsPerMeter) = 0;
  *((int*)bmih.biClrUsed) = 0;
  *((int*)bmih.biClrImportant) = 0;

  width_ptr = (int*)bmih.biWidth;
  height_ptr = (int*)bmih.biHeight;
  bitcount_ptr = (short int*)bmih.biBitCount;

#endif

  // Bytes per row of the bitmap.
  bytes_per_row =
    ((int)ceil(( (((double)(ENDIAN4(*width_ptr)))*((double)(ENDIAN2(*bitcount_ptr))))/8.)));

  // Bitmaps pad rows to preserve 4-byte boundaries.
  // The length of a row in the file will be bytes_per_row + pad .
  pad = ((4) - bytes_per_row%4)%4;

  compute_max_force( lattice, max_force, subs);

  sprintf( filename, "%s/force_%dx%d_frame%04d_subs%02d_proc%04d.bmp", get_out_path(lattice),
      get_LX(lattice), get_LY(lattice), frame, subs, get_proc_id(lattice));
  if( !( o_u = fopen( filename, "w+")))
  {
    printf("ERROR: fopen( \"%s\", \"w+\") = NULL.  Bye, bye!\n", filename);
    process_exit(1);
  }

  sprintf( filename, "%s/force_x_%dx%d_frame%04d_subs%02d_proc%04d.bmp", get_out_path(lattice),
      get_LX(lattice), get_LY(lattice), frame, subs, get_proc_id(lattice));
  if( !( o_ux = fopen( filename, "w+")))
  {
    printf("ERROR: fopen( \"%s\", \"w+\") = NULL.  Bye, bye!\n", filename);
    process_exit(1);
  }

  sprintf( filename, "%s/force_y_%dx%d_frame%04d_subs%02d_proc%04d.bmp", get_out_path(lattice),
      get_LX(lattice), get_LY(lattice), frame, subs, get_proc_id(lattice));
  if( !( o_uy = fopen( filename, "w+")))
  {
    printf("ERROR: fopen( \"%s\", \"w+\") = NULL.  Bye, bye!\n", filename);
    process_exit(1);
  }

  fwrite( &bmfh, sizeof(struct bitmap_file_header), 1, o_u );
  fwrite( &bmih, sizeof(struct bitmap_info_header), 1, o_u );

  fwrite( &bmfh, sizeof(struct bitmap_file_header), 1, o_ux );
  fwrite( &bmih, sizeof(struct bitmap_info_header), 1, o_ux );

  fwrite( &bmfh, sizeof(struct bitmap_file_header), 1, o_uy);
  fwrite( &bmih, sizeof(struct bitmap_info_header), 1, o_uy);

  //for( j=get_LY(lattice)-1; j>=0; j--)
  for( j=0; j<get_LY(lattice); j++)
  {
    n = j*get_LX(lattice);

    for( i=0; i<get_LX(lattice); i++, n++)
    {
      if( lattice->bc[subs][ n].bc_type == /*FLUID_NODE*/0)
      {
#if 1
        blue_val  = (char)0;
        green_val = (char)0;
        red_val   = (char)0;

        u_x = (lattice->force[subs][ n].force[0]);
        u_y = (lattice->force[subs][ n].force[1]);

        u = sqrt(u_x*u_x + u_y*u_y);
        maxforce = sqrt( max_force[0]*max_force[0] + max_force[1]*max_force[1]);

        if( lattice->bc[subs][ n].bc_type & BC_SOLID_NODE)
        {
          blue_val  = (char)ROUND( 128.*fabs(u_x)/max_force[0]);
          green_val = (char)ROUND( 128.*fabs(u_y)/max_force[1]);

        } /* if( lattice->bc[subs][ n].bc_type & BC_SOLID_NODE) */

        else // !( lattice->bc[subs][ n].bc_type & BC_SOLID_NODE)
        {
          blue_val  = (char)ROUND(  255.*fabs(u_x)/max_force[0]);
          green_val = (char)ROUND(  255.*fabs(u_y)/max_force[1]);
          //red_val    = (char)ROUND( 128.*fabs(u)/maxforce);
          red_val    = (char)ROUND( 255.*fabs(u)/maxforce);

        } /* if( lattice->bc[subs][ n].bc_type & BC_SOLID_NODE) else */
#else
        blue_val  = (char)255;
        green_val = (char)255;
        red_val   = (char)255;

        u = sqrt(u_x*u_x + u_y*u_y);
        maxforce = sqrt( max_force[0]*max_force[0] + max_force[1]*max_force[1]);

        //if( fabs(u) > .1*maxforce)
        //{
          green_val  = (char)ROUND( 255.-255.*fabs(u)/maxforce);
          red_val    = (char)ROUND( 255.-255.*fabs(u)/maxforce);
          blue_val    = (char)ROUND( 255.-255.*fabs(u)/maxforce);
        //}
        //else
        //{
        //  green_val  = (char)0;
        //  red_val    = (char)0;
        //  blue_val    = (char)0;
        //}
#endif

        val = (char)0;

      } /* if( lattice->bc[subs][ n].bc_type == 0) */

      else // lattice->bc[subs][ n].bc_type != 0
      {
#if SOLID_COLOR_IS_CHECKERBOARD
        // Checkerboard pattern over the solids and boundary conditions.
        if( (i+j)%2)
        {
          red_val   = (char)200;
          green_val = (char)200;
          blue_val  = (char)200;
          val       = (char)200;
        }
        else
        {
          red_val   = (char)184;
          green_val = (char)184;
          blue_val  = (char)184;
          val       = (char)184;
        }
#else /* !( SOLID_COLOR_IS_CHECKERBOARD) */
#if SOLID_COLOR_IS_BLACK
        red_val   = (char)0;
        green_val = (char)0;
        blue_val  = (char)0;
        val       = (char)0;
#else /* !( SOLID_COLOR_IS_BLACK) */
        red_val   = (char)255;
        green_val = (char)255;
        blue_val  = (char)255;
        val       = (char)255;
#endif /* SOLID_COLOR_IS_BLACK */
#endif /* SOLID_COLOR_IS_CHECKERBOARD */

      } /* if( lattice->bc[subs][ n].bc_type == 0) else */

#if MARK_ORIGIN_FOR_REFERENCE
 // Mark the origin for reference.
 if( ( i == 0 && j == 0))
 {
   red_val   = (char)255;
   green_val = (char)255;
   blue_val  = (char)255;
   val       = (char)255;
 }
#endif /* MARK_ORIGIN_FOR_REFERENCE */

      if( fwrite( &blue_val,  1, 1, o_u ) != 1) { printf("BOOM!\n"); process_exit(1);}
      if( fwrite( &green_val, 1, 1, o_u ) != 1) { printf("BOOM!\n"); process_exit(1);}
      if( fwrite( &red_val,   1, 1, o_u ) != 1) { printf("BOOM!\n"); process_exit(1);}

      if( lattice->bc[subs][ n].bc_type == /*FLUID_NODE*/0)
      {
        blue_val  = (char)0;
        green_val = (char)0;
        red_val   = (char)0;

        u_x = (lattice->force[subs][ n].force[0]);
        u_y = (lattice->force[subs][ n].force[1]);

        val = (char)0;

        if( lattice->bc[subs][ n].bc_type & BC_SOLID_NODE)
        {
          if( u_x > 0)
          {
            red_val = val;
            blue_val = (char)ROUND( 128.*fabs(u_x)/max_force[0]);
          }
          else
          {
            red_val = (char)ROUND( 128.*fabs(u_x)/max_force[0]);
            blue_val = val;
          }

        } /* if( lattice->bc[subs][ n].bc_type & BC_SOLID_NODE) */

        else // !( lattice->bc[subs][ n].bc_type & BC_SOLID_NODE)
        {
          if( u_x > 0)
          {
            red_val = val;
            blue_val = (char)ROUND( 255.*fabs(u_x)/max_force[0]);
          }
          else
          {
            red_val = (char)ROUND( 255.*fabs(u_x)/max_force[0]);
            blue_val = val;
          }

        } /* if( lattice->bc[subs][ n].bc_type & BC_SOLID_NODE) else */

      } /* if( lattice->bc[subs][ n].bc_type == 0) */

      else // lattice->bc[subs][ n].bc_type != 0
      {
#if SOLID_COLOR_IS_CHECKERBOARD
        // Checkerboard pattern over the solids and boundary conditions.
        if( (i+j)%2)
        {
          red_val   = (char)200;
          green_val = (char)200;
          blue_val  = (char)200;
          val       = (char)200;
        }
        else
        {
          red_val   = (char)184;
          green_val = (char)184;
          blue_val  = (char)184;
          val       = (char)184;
        }
#else /* !( SOLID_COLOR_IS_CHECKERBOARD) */
#if SOLID_COLOR_IS_BLACK
        red_val   = (char)0;
        green_val = (char)0;
        blue_val  = (char)0;
        val       = (char)0;
#else /* !( SOLID_COLOR_IS_BLACK) */
        red_val   = (char)255;
        green_val = (char)255;
        blue_val  = (char)255;
        val       = (char)255;
#endif /* SOLID_COLOR_IS_BLACK */
#endif /* SOLID_COLOR_IS_CHECKERBOARD */

      } /* if( lattice->bc[subs][ n].bc_type == 0) else */

#if MARK_ORIGIN_FOR_REFERENCE
 // Mark the origin for reference.
 if( ( i == 0 && j == 0))
 {
   red_val   = (char)255;
   green_val = (char)255;
   blue_val  = (char)255;
   val       = (char)255;
 }
#endif /* MARK_ORIGIN_FOR_REFERENCE */

      if( fwrite( &blue_val,  1, 1, o_ux) != 1) { printf("BOOM!\n"); process_exit(1);}
      if( fwrite( &val,       1, 1, o_ux) != 1) { printf("BOOM!\n"); process_exit(1);}
      if( fwrite( &red_val,   1, 1, o_ux) != 1) { printf("BOOM!\n"); process_exit(1);}

      if( lattice->bc[subs][ n].bc_type == /*FLUID_NODE*/0)
      {
        blue_val  = (char)0;
        green_val = (char)0;
        red_val   = (char)0;

        u_x = (lattice->force[subs][ n].force[0]);
        u_y = (lattice->force[subs][ n].force[1]);

        val = (char)0;

        if( lattice->bc[subs][ n].bc_type & BC_SOLID_NODE)
        {
          if( u_y > 0)
          {
            red_val = val;
            green_val = (char)ROUND( 128.*fabs(u_y)/max_force[1]);
          }
          else
          {
            red_val = (char)ROUND( 128.*fabs(u_y)/max_force[1]);
            green_val = val;
          }

        } /* if( lattice->bc[subs][ n].bc_type & BC_SOLID_NODE) */

        else // !( lattice->bc[subs][ n].bc_type & BC_SOLID_NODE)
        {
          if( u_y > 0)
          {
            blue_val = (char)ROUND( 255.*fabs(u_y)/max_force[1]);
            green_val = val;
          }
          else
          {
            blue_val = val;
            green_val = (char)ROUND( 255.*fabs(u_y)/max_force[1]);
          }

        } /* if( lattice->bc[subs][ n].bc_type & BC_SOLID_NODE) else */

      } /* if( lattice->bc[subs][ n].bc_type == 0) */

      else // lattice->bc[subs][ n].bc_type != 0
      {
#if SOLID_COLOR_IS_CHECKERBOARD
        // Checkerboard pattern over the solids and boundary conditions.
        if( (i+j)%2)
        {
          red_val   = (char)200;
          green_val = (char)200;
          blue_val  = (char)200;
          val       = (char)200;
        }
        else
        {
          red_val   = (char)184;
          green_val = (char)184;
          blue_val  = (char)184;
          val       = (char)184;
        }
#else /* !( SOLID_COLOR_IS_CHECKERBOARD) */
#if SOLID_COLOR_IS_BLACK
        red_val   = (char)0;
        green_val = (char)0;
        blue_val  = (char)0;
        val       = (char)0;
#else /* !( SOLID_COLOR_IS_BLACK) */
        red_val   = (char)255;
        green_val = (char)255;
        blue_val  = (char)255;
        val       = (char)255;
#endif /* SOLID_COLOR_IS_BLACK */
#endif /* SOLID_COLOR_IS_CHECKERBOARD */

      } /* if( lattice->bc[subs][ n].bc_type == 0) else */

#if MARK_ORIGIN_FOR_REFERENCE
 // Mark the origin for reference.
 if( ( i == 0 && j == 0))
 {
   red_val   = (char)255;
   green_val = (char)255;
   blue_val  = (char)255;
   val       = (char)255;
 }
#endif /* MARK_ORIGIN_FOR_REFERENCE */

      if( fwrite( &blue_val,  1, 1, o_uy) != 1) { printf("BOOM!\n"); process_exit(1);}
      if( fwrite( &green_val, 1, 1, o_uy) != 1) { printf("BOOM!\n"); process_exit(1);}
      if( fwrite( &red_val,   1, 1, o_uy) != 1) { printf("BOOM!\n"); process_exit(1);}

    } /* for( i=0; i<get_LY(lattice); i++) */

    // Pad for 4-byte boundaries.
    val = (char)0;
    for( i=0; i<pad; i++)
    {
      if( fwrite( &val, 1, 1, o_u ) != 1) { printf("BOOM!\n"); process_exit(1);}
      if( fwrite( &val, 1, 1, o_ux) != 1) { printf("BOOM!\n"); process_exit(1);}
      if( fwrite( &val, 1, 1, o_uy) != 1) { printf("BOOM!\n"); process_exit(1);}
    }

  } /* for( j=0; j<get_LY(lattice); j++) */

  fclose(o_u );
  fclose(o_ux);
  fclose(o_uy);

#if VERBOSITY_LEVEL > 0
  sprintf( filename, "%s/force_%dx%d_frame%04d_subs%02d_proc%04d.bmp", get_out_path(lattice),
      get_LX(lattice), get_LY(lattice), frame, subs, get_proc_id(lattice));
  printf("force2bmp()   -- Wrote file \"%s\".\n", filename);

  sprintf( filename, "%s/force_x_%dx%d_frame%04d_subs%02d_proc%04d.bmp", get_out_path(lattice),
      get_LX(lattice), get_LY(lattice), frame, subs, get_proc_id(lattice));
  printf("force2bmp()   -- Wrote file \"%s\".\n", filename);

  sprintf( filename, "%s/force_y_%dx%d_frame%04d_subs%02d_proc%04d.bmp", get_out_path(lattice),
      get_LX(lattice), get_LY(lattice), frame, subs, get_proc_id(lattice));
  printf("force2bmp()   -- Wrote file \"%s\".\n", filename);
#endif /* VERBOSITY_LEVEL > 0 */

 } /* for( subs=0; subs<NUM_FLUID_COMPONENTS; subs++) */

#if SAY_HI
  printf("force2bmp() -- Bye!\n");
  printf("\n");
#endif /* SAY_HI */

} /* void force2bmp( lattice_ptr lattice) */
                                                                        // }}}
// S F O R C E   2 B M P  {{{
//##############################################################################
// void sforce2bmp( char *filename, int time)
//
void sforce2bmp( lattice_ptr lattice)
{
  FILE   *in,
         *o_u,
         *o_ux,
         *o_uy;
  int    i, j,
         n, m;
  int    pad,
         bytes_per_row;
  int    frame;
  char   k;
  char   b;
  struct bitmap_file_header bmfh;
  struct bitmap_info_header bmih;
  struct rgb_quad rgb;
  int    *int_ptr;
  short  int *short_int_ptr;
  int    *width_ptr;
  int    *height_ptr;
  short  int *bitcount_ptr;
  char   filename[1024];
  char   red_val,
         green_val,
         blue_val,
         val;
  double max_sforce[2], maxsforce;
  double u_x, u_y, u;
  int    subs;

#if SAY_HI
  printf("sforce2bmp() -- Hi!\n");
#endif /* SAY_HI */

 for( subs=0; subs<NUM_FLUID_COMPONENTS; subs++)
 {

  frame = lattice->time/lattice->param.FrameRate;

#if 0
  sprintf( filename, "./in/%dx%d_proc%04d.bmp",
      get_LX(lattice), get_LY(lattice), get_proc_id(lattice));
  if( !( in = fopen( filename, "r")))
  {
    printf("%s %d >> sforce2bmp() -- Error opening file \"%s\".\n",
      __FILE__,__LINE__,filename);
    process_exit(1);
  }

  // n = fread( void *BUF, size_t SIZE, size_t COUNT, FILE *FP);

  n = fread( &bmfh, sizeof(struct bitmap_file_header), 1, in );
  if( strncmp(bmfh.bfType,"BM",2))
  {
    printf("ERROR: Can't process this file type.  Exiting!\n");
    printf("\n");
    process_exit(1);
  }
  n = fread( &bmih, sizeof(struct bitmap_info_header), 1, in );
  int_ptr = (int*)bmih.biCompression;
  if( *int_ptr != 0)
  {
    printf("ERROR: Can't handle compression.  Exiting!\n");
    printf("\n");
    process_exit(1);
  }

  width_ptr = (int*)bmih.biWidth;
  height_ptr = (int*)bmih.biHeight;
  bitcount_ptr = (short int*)bmih.biBitCount;

  // Read palette entries, if applicable.
  if( ENDIAN2(*bitcount_ptr) < 24)
  {
    n = (int)pow(2.,(double)ENDIAN2(*bitcount_ptr)); // Num palette entries.
    for( i=0; i<n; i++)
    {
      k = fread( &rgb, sizeof(struct rgb_quad), 1, in );
      if( k!=1)
      {
        printf("Error reading palette entry %d.  Exiting!\n", i);
        process_exit(1);
      }
    }
  }

  fclose(in);
#else
  bmfh.bfType[0] = 'B';
  bmfh.bfType[1] = 'M';
  *((int*)bmfh.bfSize)= get_LY(lattice)*(
    (int)ceil( ( ((double)get_LX(lattice))*( /*depth*/24.))/8.) // bytes per row
  + ( 4 - (int)ceil( ( ((double)get_LX(lattice))*( /*depth*/24.))/8.) % 4) % 4 // pad
  );
  *((short int*)bmfh.bfReserved1) = 0;
  *((short int*)bmfh.bfReserved2) = 0;
  *((int*)bmfh.bfOffBits) = 54; // 14 byte file header and 40 byte info header

  *((int*)bmih.biSize) = 40;
  *((int*)bmih.biWidth) = get_LX(lattice);
  *((int*)bmih.biHeight) = get_LY(lattice);
  *((short int*)bmih.biPlanes) = 1;
  *((short int*)bmih.biBitCount) = 24;
  *((int*)bmih.biCompression) = 0;
  *((int*)bmih.biSizeImage) = 0;
  *((int*)bmih.biXPelsPerMeter) = 0;
  *((int*)bmih.biYPelsPerMeter) = 0;
  *((int*)bmih.biClrUsed) = 0;
  *((int*)bmih.biClrImportant) = 0;

  width_ptr = (int*)bmih.biWidth;
  height_ptr = (int*)bmih.biHeight;
  bitcount_ptr = (short int*)bmih.biBitCount;

#endif

  // Bytes per row of the bitmap.
  bytes_per_row =
    ((int)ceil(( (((double)(ENDIAN4(*width_ptr)))*((double)(ENDIAN2(*bitcount_ptr))))/8.)));

  // Bitmaps pad rows to preserve 4-byte boundaries.
  // The length of a row in the file will be bytes_per_row + pad .
  pad = ((4) - bytes_per_row%4)%4;

  compute_max_sforce( lattice, max_sforce, subs);

  sprintf( filename, "%s/sforce_%dx%d_frame%04d_subs%02d_proc%04d.bmp", get_out_path(lattice),
      get_LX(lattice), get_LY(lattice), frame, subs, get_proc_id(lattice));
  if( !( o_u = fopen( filename, "w+")))
  {
    printf("ERROR: fopen( \"%s\", \"w+\") = NULL.  Bye, bye!\n", filename);
    process_exit(1);
  }

  sprintf( filename, "%s/sforce_x_%dx%d_frame%04d_subs%02d_proc%04d.bmp", get_out_path(lattice),
      get_LX(lattice), get_LY(lattice), frame, subs, get_proc_id(lattice));
  if( !( o_ux = fopen( filename, "w+")))
  {
    printf("ERROR: fopen( \"%s\", \"w+\") = NULL.  Bye, bye!\n", filename);
    process_exit(1);
  }

  sprintf( filename, "%s/sforce_y_%dx%d_frame%04d_subs%02d_proc%04d.bmp", get_out_path(lattice),
      get_LX(lattice), get_LY(lattice), frame, subs, get_proc_id(lattice));
  if( !( o_uy = fopen( filename, "w+")))
  {
    printf("ERROR: fopen( \"%s\", \"w+\") = NULL.  Bye, bye!\n", filename);
    process_exit(1);
  }

  fwrite( &bmfh, sizeof(struct bitmap_file_header), 1, o_u );
  fwrite( &bmih, sizeof(struct bitmap_info_header), 1, o_u );

  fwrite( &bmfh, sizeof(struct bitmap_file_header), 1, o_ux );
  fwrite( &bmih, sizeof(struct bitmap_info_header), 1, o_ux );

  fwrite( &bmfh, sizeof(struct bitmap_file_header), 1, o_uy);
  fwrite( &bmih, sizeof(struct bitmap_info_header), 1, o_uy);

  //for( j=get_LY(lattice)-1; j>=0; j--)
  for( j=0; j<get_LY(lattice); j++)
  {
    n = j*get_LX(lattice);

    for( i=0; i<get_LX(lattice); i++, n++)
    {
      if( 1)//lattice->bc[subs][ n].bc_type == /*FLUID_NODE*/0)
      {
#if 1
        blue_val  = (char)0;
        green_val = (char)0;
        red_val   = (char)0;

        u_x = (lattice->force[subs][ n].sforce[0]);
        u_y = (lattice->force[subs][ n].sforce[1]);

        u = sqrt(u_x*u_x + u_y*u_y);
        maxsforce = sqrt( max_sforce[0]*max_sforce[0] + max_sforce[1]*max_sforce[1]);

        if( 0)//lattice->bc[subs][ n].bc_type & BC_SOLID_NODE)
        {
          blue_val  = (char)ROUND( 128.*fabs(u_x)/max_sforce[0]);
          green_val = (char)ROUND( 128.*fabs(u_y)/max_sforce[1]);

        } /* if( lattice->bc[subs][ n].bc_type & BC_SOLID_NODE) */

        else // !( lattice->bc[subs][ n].bc_type & BC_SOLID_NODE)
        {
          blue_val  = (char)ROUND(  255.*fabs(u_x)/max_sforce[0]);
          green_val = (char)ROUND(  255.*fabs(u_y)/max_sforce[1]);
          //red_val    = (char)ROUND( 128.*fabs(u)/maxsforce);
          red_val    = (char)ROUND( 255.*fabs(u)/maxsforce);

        } /* if( lattice->bc[subs][ n].bc_type & BC_SOLID_NODE) else */
#else
        blue_val  = (char)255;
        green_val = (char)255;
        red_val   = (char)255;

        u = sqrt(u_x*u_x + u_y*u_y);
        maxsforce = sqrt( max_sforce[0]*max_sforce[0] + max_sforce[1]*max_sforce[1]);

        //if( fabs(u) > .1*maxsforce)
        //{
          green_val  = (char)ROUND( 255.-255.*fabs(u)/maxsforce);
          red_val    = (char)ROUND( 255.-255.*fabs(u)/maxsforce);
          blue_val    = (char)ROUND( 255.-255.*fabs(u)/maxsforce);
        //}
        //else
        //{
        //  green_val  = (char)0;
        //  red_val    = (char)0;
        //  blue_val    = (char)0;
        //}
#endif

        val = (char)0;

      } /* if( lattice->bc[subs][ n].bc_type == 0) */

      else // lattice->bc[subs][ n].bc_type != 0
      {
#if SOLID_COLOR_IS_CHECKERBOARD
        // Checkerboard pattern over the solids and boundary conditions.
        if( (i+j)%2)
        {
          red_val   = (char)200;
          green_val = (char)200;
          blue_val  = (char)200;
          val       = (char)200;
        }
        else
        {
          red_val   = (char)184;
          green_val = (char)184;
          blue_val  = (char)184;
          val       = (char)184;
        }
#else /* !( SOLID_COLOR_IS_CHECKERBOARD) */
#if SOLID_COLOR_IS_BLACK
        red_val   = (char)0;
        green_val = (char)0;
        blue_val  = (char)0;
        val       = (char)0;
#else /* !( SOLID_COLOR_IS_BLACK) */
        red_val   = (char)255;
        green_val = (char)255;
        blue_val  = (char)255;
        val       = (char)255;
#endif /* SOLID_COLOR_IS_BLACK */
#endif /* SOLID_COLOR_IS_CHECKERBOARD */

      } /* if( lattice->bc[subs][ n].bc_type == 0) else */

#if MARK_ORIGIN_FOR_REFERENCE
 // Mark the origin for reference.
 if( ( i == 0 && j == 0))
 {
   red_val   = (char)255;
   green_val = (char)255;
   blue_val  = (char)255;
   val       = (char)255;
 }
#endif /* MARK_ORIGIN_FOR_REFERENCE */

 //printf("RGB=(%d,%d,%d)\n",red_val,green_val,blue_val);
      if( fwrite( &blue_val,  1, 1, o_u ) != 1) { printf("BOOM!\n"); process_exit(1);}
      if( fwrite( &green_val, 1, 1, o_u ) != 1) { printf("BOOM!\n"); process_exit(1);}
      if( fwrite( &red_val,   1, 1, o_u ) != 1) { printf("BOOM!\n"); process_exit(1);}

      if( lattice->bc[subs][ n].bc_type == /*FLUID_NODE*/0)
      {
        blue_val  = (char)0;
        green_val = (char)0;
        red_val   = (char)0;

        u_x = (lattice->force[subs][ n].sforce[0]);
        u_y = (lattice->force[subs][ n].sforce[1]);

        val = (char)0;

        if( lattice->bc[subs][ n].bc_type & BC_SOLID_NODE)
        {
          if( u_x > 0)
          {
            red_val = val;
            blue_val = (char)ROUND( 128.*fabs(u_x)/max_sforce[0]);
          }
          else
          {
            red_val = (char)ROUND( 128.*fabs(u_x)/max_sforce[0]);
            blue_val = val;
          }

        } /* if( lattice->bc[subs][ n].bc_type & BC_SOLID_NODE) */

        else // !( lattice->bc[subs][ n].bc_type & BC_SOLID_NODE)
        {
          if( u_x > 0)
          {
            red_val = val;
            blue_val = (char)ROUND( 255.*fabs(u_x)/max_sforce[0]);
          }
          else
          {
            red_val = (char)ROUND( 255.*fabs(u_x)/max_sforce[0]);
            blue_val = val;
          }

        } /* if( lattice->bc[subs][ n].bc_type & BC_SOLID_NODE) else */

      } /* if( lattice->bc[subs][ n].bc_type == 0) */

      else // lattice->bc[subs][ n].bc_type != 0
      {
#if SOLID_COLOR_IS_CHECKERBOARD
        // Checkerboard pattern over the solids and boundary conditions.
        if( (i+j)%2)
        {
          red_val   = (char)200;
          green_val = (char)200;
          blue_val  = (char)200;
          val       = (char)200;
        }
        else
        {
          red_val   = (char)184;
          green_val = (char)184;
          blue_val  = (char)184;
          val       = (char)184;
        }
#else /* !( SOLID_COLOR_IS_CHECKERBOARD) */
#if SOLID_COLOR_IS_BLACK
        red_val   = (char)0;
        green_val = (char)0;
        blue_val  = (char)0;
        val       = (char)0;
#else /* !( SOLID_COLOR_IS_BLACK) */
        red_val   = (char)255;
        green_val = (char)255;
        blue_val  = (char)255;
        val       = (char)255;
#endif /* SOLID_COLOR_IS_BLACK */
#endif /* SOLID_COLOR_IS_CHECKERBOARD */

      } /* if( lattice->bc[subs][ n].bc_type == 0) else */

#if MARK_ORIGIN_FOR_REFERENCE
 // Mark the origin for reference.
 if( ( i == 0 && j == 0))
 {
   red_val   = (char)255;
   green_val = (char)255;
   blue_val  = (char)255;
   val       = (char)255;
 }
#endif /* MARK_ORIGIN_FOR_REFERENCE */

      if( fwrite( &blue_val,  1, 1, o_ux) != 1) { printf("BOOM!\n"); process_exit(1);}
      if( fwrite( &val,       1, 1, o_ux) != 1) { printf("BOOM!\n"); process_exit(1);}
      if( fwrite( &red_val,   1, 1, o_ux) != 1) { printf("BOOM!\n"); process_exit(1);}

      if( lattice->bc[subs][ n].bc_type == /*FLUID_NODE*/0)
      {
        blue_val  = (char)0;
        green_val = (char)0;
        red_val   = (char)0;

        u_x = (lattice->force[subs][ n].sforce[0]);
        u_y = (lattice->force[subs][ n].sforce[1]);

        val = (char)0;

        if( lattice->bc[subs][ n].bc_type & BC_SOLID_NODE)
        {
          if( u_y > 0)
          {
            red_val = val;
            green_val = (char)ROUND( 128.*fabs(u_y)/max_sforce[1]);
          }
          else
          {
            red_val = (char)ROUND( 128.*fabs(u_y)/max_sforce[1]);
            green_val = val;
          }

        } /* if( lattice->bc[subs][ n].bc_type & BC_SOLID_NODE) */

        else // !( lattice->bc[subs][ n].bc_type & BC_SOLID_NODE)
        {
          if( u_y > 0)
          {
            blue_val = (char)ROUND( 255.*fabs(u_y)/max_sforce[1]);
            green_val = val;
          }
          else
          {
            blue_val = val;
            green_val = (char)ROUND( 255.*fabs(u_y)/max_sforce[1]);
          }

        } /* if( lattice->bc[subs][ n].bc_type & BC_SOLID_NODE) else */

      } /* if( lattice->bc[subs][ n].bc_type == 0) */

      else // lattice->bc[subs][ n].bc_type != 0
      {
#if SOLID_COLOR_IS_CHECKERBOARD
        // Checkerboard pattern over the solids and boundary conditions.
        if( (i+j)%2)
        {
          red_val   = (char)200;
          green_val = (char)200;
          blue_val  = (char)200;
          val       = (char)200;
        }
        else
        {
          red_val   = (char)184;
          green_val = (char)184;
          blue_val  = (char)184;
          val       = (char)184;
        }
#else /* !( SOLID_COLOR_IS_CHECKERBOARD) */
#if SOLID_COLOR_IS_BLACK
        red_val   = (char)0;
        green_val = (char)0;
        blue_val  = (char)0;
        val       = (char)0;
#else /* !( SOLID_COLOR_IS_BLACK) */
        red_val   = (char)255;
        green_val = (char)255;
        blue_val  = (char)255;
        val       = (char)255;
#endif /* SOLID_COLOR_IS_BLACK */
#endif /* SOLID_COLOR_IS_CHECKERBOARD */

      } /* if( lattice->bc[subs][ n].bc_type == 0) else */

#if MARK_ORIGIN_FOR_REFERENCE
 // Mark the origin for reference.
 if( ( i == 0 && j == 0))
 {
   red_val   = (char)255;
   green_val = (char)255;
   blue_val  = (char)255;
   val       = (char)255;
 }
#endif /* MARK_ORIGIN_FOR_REFERENCE */

      if( fwrite( &blue_val,  1, 1, o_uy) != 1) { printf("BOOM!\n"); process_exit(1);}
      if( fwrite( &green_val, 1, 1, o_uy) != 1) { printf("BOOM!\n"); process_exit(1);}
      if( fwrite( &red_val,   1, 1, o_uy) != 1) { printf("BOOM!\n"); process_exit(1);}

    } /* for( i=0; i<get_LY(lattice); i++) */

    // Pad for 4-byte boundaries.
    val = (char)0;
    for( i=0; i<pad; i++)
    {
      if( fwrite( &val, 1, 1, o_u ) != 1) { printf("BOOM!\n"); process_exit(1);}
      if( fwrite( &val, 1, 1, o_ux) != 1) { printf("BOOM!\n"); process_exit(1);}
      if( fwrite( &val, 1, 1, o_uy) != 1) { printf("BOOM!\n"); process_exit(1);}
    }

  } /* for( j=0; j<get_LY(lattice); j++) */

  fclose(o_u );
  fclose(o_ux);
  fclose(o_uy);

#if VERBOSITY_LEVEL > 0
  sprintf( filename, "%s/sforce_%dx%d_frame%04d_subs%02d_proc%04d.bmp", get_out_path(lattice),
      get_LX(lattice), get_LY(lattice), frame, subs, get_proc_id(lattice));
  printf("sforce2bmp()   -- Wrote file \"%s\".\n", filename);

  sprintf( filename, "%s/sforce_x_%dx%d_frame%04d_subs%02d_proc%04d.bmp", get_out_path(lattice),
      get_LX(lattice), get_LY(lattice), frame, subs, get_proc_id(lattice));
  printf("sforce2bmp()   -- Wrote file \"%s\".\n", filename);

  sprintf( filename, "%s/sforce_y_%dx%d_frame%04d_subs%02d_proc%04d.bmp", get_out_path(lattice),
      get_LX(lattice), get_LY(lattice), frame, subs, get_proc_id(lattice));
  printf("sforce2bmp()   -- Wrote file \"%s\".\n", filename);
#endif /* VERBOSITY_LEVEL > 0 */

 } /* for( subs=0; subs<NUM_FLUID_COMPONENTS; subs++) */

#if SAY_HI
  printf("sforce2bmp() -- Bye!\n");
  printf("\n");
#endif /* SAY_HI */

} /* void sforce2bmp( lattice_ptr lattice) */
                                                                        // }}}
#endif /* NON_LOCAL_FORCES */
// P D F 2 B M P  {{{
//##############################################################################
// void pdf2bmp( char *filename, int time)
//
void pdf2bmp( lattice_ptr lattice, int time)
{
  FILE   *in,
         *o;
  int    i, j,
         n, m;
  int    pad,
         bytes_per_row;
  int    frame;
  char   k;
  char   b;
  struct bitmap_file_header bmfh;
  struct bitmap_info_header bmih;
  struct rgb_quad rgb;
  int    *int_ptr;
  short  int *short_int_ptr;
  int    *width_ptr;
  int    *height_ptr;
  short  int *bitcount_ptr;
  char   filename[1024];
  char   red_val,
         green_val,
         blue_val,
         val;
  double fval;
  double min_rho, max_rho;
  int    subs;
  double **colormap;
  int    num_colors;

  int    fx=2, fy=2; // Number of pixels to use to represent a pdf value.

#if SAY_HI
  printf("pdf2bmp() -- Hi!\n");
#endif /* SAY_HI */

  // TODO: Implement pdf2bmp()

  printf("%s (%d) >> pdf2bmp() is not yet implemented. Exiting!\n",
    __FILE__, __LINE__);
  process_exit(1);

#if SAY_HI
  printf("pdf2bmp() -- Bye!\n");
  printf("\n");
#endif /* SAY_HI */

} /* pdf2bmp( lattice_ptr lattice, int time) */
                                                                        // }}}

// S L I C E {{{
//##############################################################################
//void slice( lattice_ptr lattice)
//
// - Extract slice (i0,j0)..(i1,j1) from the macroscopic variables.
//
// - Write to matlab scripts for easy processing.
//
void slice( lattice_ptr lattice)
{
  int    i0, j0,
         i1, j1;
  int    i, j, n;
  int    len;
  double *rho_slice;
  double *u_x_slice;
  double *u_y_slice;
  char   filename[1024];
  FILE   *in, *o;
  int    subs;

  if( use_slice_dot_in_file( lattice))
  {
printf("%s %d >> Using slice.in file.\n",__FILE__,__LINE__);
    if( !( in = fopen( "./in/slice.in", "r")))
    {
      // Default slice.
      i0 = 0;
      j0 = (int)floor((double)get_LY(lattice)/2.);
      i1 = get_LX(lattice)-1;
      j1 = (int)floor((double)get_LY(lattice)/2.);
    }
    else
    {
      // Read slice from file.
      fscanf( in, "%d", &i0);
      fscanf( in, "%d", &j0);
      fscanf( in, "%d", &i1);
      fscanf( in, "%d", &j1);
      fclose( in);
    }

    private_slice( lattice, "slice", i0, j0, i1, j1);

  }
  else
  {
    if( get_slice_x( lattice) >= 0)
    {
      private_slice( lattice, "slice_x",
          get_slice_x( lattice), 0,
          get_slice_x( lattice), get_LY( lattice)-1 );
    }

    if( get_slice_y( lattice) >= 0)
    {
      private_slice( lattice, "slice_y",
          0,                get_slice_y( lattice),
          get_LX( lattice)-1, get_slice_y( lattice) );
    }
  }

} /* void slice( lattice_ptr lattice) */
                                                                        // }}}
// P R I V A T E _ S L I C E {{{
//##############################################################################
//void private_slice( lattice_ptr lattice, int i0, int j0, int i1, int j1)
//
// - Extract slice (i0,j0)..(i1,j1) from the macroscopic variables.
//
// - Write to matlab scripts for easy processing.
//
void private_slice(
       lattice_ptr lattice,
       char *root_word,
       int i0, int j0, int i1, int j1)
{
  int    i, j, k, n;
  int    len, wid;
  double *rho_slice;
  double *rho_ave;
  double *u_x_slice;
  double *u_x_ave;
  double *u_y_slice;
  double *u_y_ave;
#if STORE_U_COMPOSITE
  double *upr_x_slice;
  double *upr_x_ave;
  double *upr_y_slice;
  double *upr_y_ave;
#endif /* (STORE_U_COMPOSITE) */
  char   filename[1024];
  FILE   *in, *o;
  double ave_rho;
  int    subs;
  char   plot_specs[2][4] =
         {
           { '\'', 'b', '\'', '\x0'},
           { '\'', 'r', '\'', '\x0'}
         };

  if( i0 == i1)
  {
    if( i0 < 0 || i0 >= get_LX( lattice))
    {
      printf("lbio.c: private_slice() -- "
          "ERROR: Can't take slice at "
          "i0 = i1 = %d.\n", i0 );
      i0 = get_LX(lattice)/2;
      i1 = get_LX(lattice)/2;
      printf("lbio.c: private_slice() -- Defaulting to i0 = i1 = %d.\n", i0);
      return;
    }

    len = j1 - j0 + 1;

    if( len > get_LY(lattice))
    {
      len = get_LY(lattice);
    }

    rho_slice = (double*)malloc( len*sizeof(double));
    rho_ave   = (double*)malloc( len*sizeof(double));
    u_x_slice = (double*)malloc( len*sizeof(double));
    u_x_ave   = (double*)malloc( len*sizeof(double));
    u_y_slice = (double*)malloc( len*sizeof(double));
    u_y_ave   = (double*)malloc( len*sizeof(double));
#if STORE_U_COMPOSITE
    upr_x_slice = (double*)malloc( len*sizeof(double));
    upr_x_ave   = (double*)malloc( len*sizeof(double));
    upr_y_slice = (double*)malloc( len*sizeof(double));
    upr_y_ave   = (double*)malloc( len*sizeof(double));
#endif /* (STORE_U_COMPOSITE) */

    // Generate matlab script to plot the slices.
    sprintf( filename, "%s/%s%dx%d_frame%04d.m", get_out_path(lattice),
             root_word,
             get_LX(lattice), get_LY(lattice),
             lattice->time/lattice->param.FrameRate);
    if( !( o = fopen( filename, "w+")))
    {
      printf("ERROR: fopen( \"%s\", \"w+\") = NULL.  Bye, bye!\n", filename);
      process_exit(1);
    }

    fprintf(
      o,
      "%% function [ slice_data] = %s%dx%d_frame%04d( plot_stuff)\n",
      root_word,
      get_LX(lattice), get_LY(lattice),
      lattice->time/lattice->param.FrameRate);
    fprintf(
      o,
      "function [ slice_data] = %s%dx%d_frame%04d( plot_stuff)\n\n",
      root_word,
      get_LX(lattice), get_LY(lattice),
      lattice->time/lattice->param.FrameRate);

    for( subs=0; subs<NUM_FLUID_COMPONENTS; subs++)
    {
      // Slice.
      len = 0;

      for( j=j0; j<=j1; j++)
      {
        n = j*get_LX(lattice) + i0;
        if( j>=0 && j<get_LY(lattice))
        {
          rho_slice[ len] = lattice->macro_vars[subs][ n].rho;
          u_x_slice[ len] = lattice->macro_vars[subs][ n].u[0];
          u_y_slice[ len] = lattice->macro_vars[subs][ n].u[1];
#if STORE_U_COMPOSITE
          if(subs==0)
          {
            upr_x_slice[ len] = lattice->upr[ n].u[0];
            upr_y_slice[ len] = lattice->upr[ n].u[1];
          }
#endif /* (STORE_U_COMPOSITE) */

          rho_ave[ len]   = 0.;
          u_x_ave[ len]   = 0.;
          u_y_ave[ len]   = 0.;
#if STORE_U_COMPOSITE
          if(subs==0)
          {
            upr_x_ave[ len]   = 0.;
            upr_y_ave[ len]   = 0.;
          }
#endif /* (STORE_U_COMPOSITE) */
          wid = 0;
          for( i=0; i<get_LX(lattice); i++)
          {
  if( !( lattice->bc[subs][ j*get_LX(lattice) + i].bc_type & BC_SOLID_NODE))
  {
            rho_ave[ len] +=
              lattice->macro_vars[subs][ j*get_LX(lattice)+i].rho;
            u_x_ave[ len] +=
              lattice->macro_vars[subs][ j*get_LX(lattice)+i].u[0];
            u_y_ave[ len] +=
              lattice->macro_vars[subs][ j*get_LX(lattice)+i].u[1];
#if STORE_U_COMPOSITE
          if(subs==0)
          {
            upr_x_ave[ len] +=
              lattice->upr[ j*get_LX(lattice)+i].u[0];
            upr_y_ave[ len] +=
              lattice->upr[ j*get_LX(lattice)+i].u[1];
          }
#endif /* (STORE_U_COMPOSITE) */
            wid++;
  }
          }
          rho_ave[ len] /= wid;
          u_x_ave[ len] /= wid;
          u_y_ave[ len] /= wid;
#if STORE_U_COMPOSITE
          if(subs==0)
          {
            upr_x_ave[ len] /= wid;
            upr_y_ave[ len] /= wid;
          }
#endif /* (STORE_U_COMPOSITE) */

          len++;
        }
      }

      fprintf( o, "rho_slice%02d = [ ", subs);
      for( n=0; n<len; n++)
      {
        fprintf( o, " %20.17f ", rho_slice[ n]);
      }
      fprintf( o, "];\n");

      fprintf( o, "rho_ave%02d = [ ", subs);
      for( n=0; n<len; n++)
      {
        fprintf( o, " %20.17f ", rho_ave[ n]);
      }
      fprintf( o, "];\n");

      fprintf( o, "u_x_slice%02d = [ ", subs);
      for( n=0; n<len; n++)
      {
        fprintf( o, " %20.17f ", u_x_slice[ n]);
      }
      fprintf( o, "];\n");

      fprintf( o, "u_x_ave%02d = [ ", subs);
      for( n=0; n<len; n++)
      {
        fprintf( o, " %20.17f ", u_x_ave[ n]);
      }
      fprintf( o, "];\n");

      fprintf( o, "u_y_slice%02d = [ ", subs);
      for( n=0; n<len; n++)
      {
        fprintf( o, " %20.17f ", u_y_slice[ n]);
      }
      fprintf( o, "];\n");

      fprintf( o, "u_y_ave%02d = [ ", subs);
      for( n=0; n<len; n++)
      {
        fprintf( o, " %20.17f ", u_y_ave[ n]);
      }
      fprintf( o, "];\n");

#if STORE_U_COMPOSITE
          if(subs==0)
          {
      fprintf( o, "upr_x_slice= [ ");
      for( n=0; n<len; n++)
      {
        fprintf( o, " %20.17f ", upr_x_slice[ n]);
      }
      fprintf( o, "];\n");

      fprintf( o, "upr_x_ave= [ ");
      for( n=0; n<len; n++)
      {
        fprintf( o, " %20.17f ", upr_x_ave[ n]);
      }
      fprintf( o, "];\n");

      fprintf( o, "upr_y_slice= [ ");
      for( n=0; n<len; n++)
      {
        fprintf( o, " %20.17f ", upr_y_slice[ n]);
      }
      fprintf( o, "];\n");

      fprintf( o, "upr_y_ave= [ ");
      for( n=0; n<len; n++)
      {
        fprintf( o, " %20.17f ", upr_y_ave[ n]);
      }
      fprintf( o, "];\n");

          }
#endif /* (STORE_U_COMPOSITE) */

    } /* for( subs=0; subs<NUM_FLUID_COMPONENTS; subs++) */

    if( make_octave_scripts(lattice))
    {
    } /* if( make_octave_scripts(lattice)) */

    else // make Matlab scripts
    {
    fprintf( o, "slice_data = zeros(%d,%d,%d);\n",
             len, 10, NUM_FLUID_COMPONENTS);
    for( subs=0; subs<NUM_FLUID_COMPONENTS; subs++)
    {
      fprintf( o, "slice_data(:,1,%d) = rho_slice%02d;\n", subs+1, subs);
      fprintf( o, "slice_data(:,2,%d) = rho_ave%02d;\n"  , subs+1, subs);
      fprintf( o, "slice_data(:,3,%d) = u_x_slice%02d;\n", subs+1, subs);
      fprintf( o, "slice_data(:,4,%d) = u_x_ave%02d;\n"  , subs+1, subs);
      fprintf( o, "slice_data(:,5,%d) = u_y_slice%02d;\n", subs+1, subs);
      fprintf( o, "slice_data(:,6,%d) = u_y_ave%02d;\n"  , subs+1, subs);
#if STORE_U_COMPOSITE
          if(subs==0)
          {
      fprintf( o, "slice_data(:,7,%d) = upr_x_slice;\n", subs+1);
      fprintf( o, "slice_data(:,8,%d) = upr_x_ave;\n"  , subs+1);
      fprintf( o, "slice_data(:,9,%d) = upr_y_slice;\n", subs+1);
      fprintf( o, "slice_data(:,10,%d) = upr_y_ave;\n"  , subs+1);
          }
#endif /* (STORE_U_COMPOSITE) */
    fprintf( o, "disp('slice_data(:,1,%d) = rho_slice%02d');\n", subs+1, subs);
    fprintf( o, "disp('slice_data(:,2,%d) = rho_ave%02d'  );\n", subs+1, subs);
    fprintf( o, "disp('slice_data(:,3,%d) = u_x_slice%02d');\n", subs+1, subs);
    fprintf( o, "disp('slice_data(:,4,%d) = u_x_ave%02d'  );\n", subs+1, subs);
    fprintf( o, "disp('slice_data(:,5,%d) = u_y_slice%02d');\n", subs+1, subs);
    fprintf( o, "disp('slice_data(:,6,%d) = u_y_ave%02d'  );\n", subs+1, subs);
#if STORE_U_COMPOSITE
          if(subs==0)
          {
  fprintf( o, "disp('slice_data(:,7,%d) = upr_x_slice');\n", subs+1);
  fprintf( o, "disp('slice_data(:,8,%d) = upr_x_ave'  );\n", subs+1);
  fprintf( o, "disp('slice_data(:,9,%d) = upr_y_slice');\n", subs+1);
  fprintf( o, "disp('slice_data(:,10,%d) = upr_y_ave'  );\n", subs+1);
          }
#endif /* (STORE_U_COMPOSITE) */
    } /* for( subs=0; subs<NUM_FLUID_COMPONENTS; subs++) */
    }

  } /* if( i0 == i1) */

  else if( j0 == j1)
  {
    if( j0 < 0 || j0 >= get_LY(lattice))
    {
      printf("lbio.c: private_slice() -- "
          "ERROR: Can't take slice at "
          "j0 = j1 = %d.\n", j0 );
      j0 = get_LY(lattice)/2;
      j1 = get_LY(lattice)/2;
      printf("lbio.c: private_slice() -- Defaulting to j0 = j1 = %d.\n", j0);
      return;
    }

    len = i1 - i0 + 1;

    if( len > get_LX(lattice))
    {
      len = get_LX(lattice);
    }

    rho_slice = (double*)malloc( len*sizeof(double));
    rho_ave   = (double*)malloc( len*sizeof(double));
    u_x_slice = (double*)malloc( len*sizeof(double));
    u_x_ave   = (double*)malloc( len*sizeof(double));
    u_y_slice = (double*)malloc( len*sizeof(double));
    u_y_ave   = (double*)malloc( len*sizeof(double));
#if STORE_U_COMPOSITE
    upr_x_slice = (double*)malloc( len*sizeof(double));
    upr_x_ave   = (double*)malloc( len*sizeof(double));
    upr_y_slice = (double*)malloc( len*sizeof(double));
    upr_y_ave   = (double*)malloc( len*sizeof(double));
#endif /* (STORE_U_COMPOSITE) */

    // Generate matlab script to plot the slices.
    sprintf( filename, "%s/%s%dx%d_frame%04d.m", get_out_path(lattice),
             root_word,
             get_LX(lattice),
             get_LY(lattice),
             lattice->time/lattice->param.FrameRate);
    if( !( o = fopen( filename, "w+")))
    {
      printf("ERROR: fopen( \"%s\", \"w+\") = NULL.  Bye, bye!\n", filename);
      process_exit(1);
    }

    fprintf(
      o,
      "%% function [ slice_data] = %s%dx%d_frame%04d( plot_stuff)\n",
      root_word,
      get_LX(lattice), get_LY(lattice),
      lattice->time/lattice->param.FrameRate);
    fprintf(
      o,
      "function [ slice_data] = %s%dx%d_frame%04d( plot_stuff)\n\n",
      root_word,
      get_LX(lattice), get_LY(lattice),
      lattice->time/lattice->param.FrameRate);

    for( subs=0; subs<NUM_FLUID_COMPONENTS; subs++)
    {
      // Slice.
      len = 0;

      for( i=i0; i<=i1; i++)
      {
        n = j0*get_LX(lattice) + i;
        if( i>=0 && i<get_LX(lattice))
        {
          rho_slice[ len] = lattice->macro_vars[subs][ n].rho;
          u_x_slice[ len] = lattice->macro_vars[subs][ n].u[0];
          u_y_slice[ len] = lattice->macro_vars[subs][ n].u[1];
#if STORE_U_COMPOSITE
          if(subs==0)
          {
            upr_x_slice[ len] = lattice->upr[ n].u[0];
            upr_y_slice[ len] = lattice->upr[ n].u[1];
          }
#endif /* (STORE_U_COMPOSITE) */

          rho_ave[ len]   = 0.;
          u_x_ave[ len]   = 0.;
          u_y_ave[ len]   = 0.;
#if STORE_U_COMPOSITE
          if(subs==0)
          {
            upr_x_ave[ len]   = 0.;
            upr_y_ave[ len]   = 0.;
          }
#endif /* (STORE_U_COMPOSITE) */
          wid = 0;
          for( j=0; j<get_LY(lattice); j++)
          {
  if( !( lattice->bc[subs][ j*get_LX(lattice) + i].bc_type & BC_SOLID_NODE))
  {
            rho_ave[ len] +=
              lattice->macro_vars[subs][ j*get_LX(lattice)+i].rho;
            u_x_ave[ len] +=
              lattice->macro_vars[subs][ j*get_LX(lattice)+i].u[0];
            u_y_ave[ len] +=
              lattice->macro_vars[subs][ j*get_LX(lattice)+i].u[1];
#if STORE_U_COMPOSITE
          if(subs==0)
          {
            upr_x_ave[ len] +=
              lattice->upr[ j*get_LX(lattice)+i].u[0];
            upr_y_ave[ len] +=
              lattice->upr[ j*get_LX(lattice)+i].u[1];
          }
#endif /* (STORE_U_COMPOSITE) */
            wid++;
  }
          }
          rho_ave[ len] /= wid;
          u_x_ave[ len] /= wid;
          u_y_ave[ len] /= wid;
#if STORE_U_COMPOSITE
          if(subs==0)
          {
            upr_x_ave[ len] /= wid;
            upr_y_ave[ len] /= wid;
          }
#endif /* (STORE_U_COMPOSITE) */

          len++;
        }
      }

      fprintf( o, "rho_slice%02d = [ ", subs);
      for( n=0; n<len; n++)
      {
        fprintf( o, " %20.17f ", rho_slice[ n]);
      }
      fprintf( o, "];\n");

      fprintf( o, "rho_ave%02d = [ ", subs);
      for( n=0; n<len; n++)
      {
        fprintf( o, " %20.17f ", rho_ave[ n]);
      }
      fprintf( o, "];\n");

      fprintf( o, "u_x_slice%02d = [ ", subs);
      for( n=0; n<len; n++)
      {
        fprintf( o, " %20.17f ", u_x_slice[ n]);
      }
      fprintf( o, "];\n");

      fprintf( o, "u_x_ave%02d = [ ", subs);
      for( n=0; n<len; n++)
      {
        fprintf( o, " %20.17f ", u_x_ave[ n]);
      }
      fprintf( o, "];\n");

      if(1)
      {
        fprintf( o, "u_y_slice%02d = [ ", subs);
        for( n=0; n<len; n++)
        {
          fprintf( o, " %20.17f ", u_y_slice[ n]);
        }
        fprintf( o, "];\n");
      }
      else
      {
//      // Output a series of slices showing entry length effects.
//      len = 0;
//      fprintf( o, "u_y_slice%02d_j%d = [ ", subs, j);
//      for( i=i0; i<=i1; i++)
//      {
//        n = j0*get_LX(lattice) + i;
//        if( i>=0 && i<get_LX(lattice))
//        {
//          for( j=1;
//               j<get_LY(lattice)-1;
//               j+=(int)floor(((double)get_LY(lattice)/10.)) )
//          {
//              fprintf( o, " %20.17f ", lattice->macro_vars[subs][ n].u[1]);
//          }
//        }
//      }
//      fprintf( o, "];\n");
      }

      fprintf( o, "u_y_ave%02d = [ ", subs);
      for( n=0; n<len; n++)
      {
        fprintf( o, " %20.17f ", u_y_ave[ n]);
      }
      fprintf( o, "];\n");
#if STORE_U_COMPOSITE
          if(subs==0)
          {
      fprintf( o, "upr_x_slice= [ ");
      for( n=0; n<len; n++)
      {
        fprintf( o, " %20.17f ", upr_x_slice[ n]);
      }
      fprintf( o, "];\n");

      fprintf( o, "upr_x_ave= [ ");
      for( n=0; n<len; n++)
      {
        fprintf( o, " %20.17f ", upr_x_ave[ n]);
      }
      fprintf( o, "];\n");

      fprintf( o, "upr_y_slice= [ ");
      for( n=0; n<len; n++)
      {
        fprintf( o, " %20.17f ", upr_y_slice[ n]);
      }
      fprintf( o, "];\n");

      fprintf( o, "upr_y_ave= [ ");
      for( n=0; n<len; n++)
      {
        fprintf( o, " %20.17f ", upr_y_ave[ n]);
      }
      fprintf( o, "];\n");
          }
#endif /* (STORE_U_COMPOSITE) */

    } /* for( subs=0; subs<NUM_FLUID_COMPONENTS; subs++) */

    if( make_octave_scripts(lattice))
    {
    } /* if( make_octave_scripts(lattice)) */

    else // make Matlab scripts
    {
    fprintf( o, "slice_data = zeros(%d,%d,%d);\n",
             len, 10, NUM_FLUID_COMPONENTS);
    for( subs=0; subs<NUM_FLUID_COMPONENTS; subs++)
    {
      fprintf( o, "slice_data(:,1,%d) = rho_slice%02d;\n", subs+1, subs);
      fprintf( o, "slice_data(:,2,%d) = rho_ave%02d;\n"  , subs+1, subs);
      fprintf( o, "slice_data(:,3,%d) = u_x_slice%02d;\n", subs+1, subs);
      fprintf( o, "slice_data(:,4,%d) = u_x_ave%02d;\n"  , subs+1, subs);
      fprintf( o, "slice_data(:,5,%d) = u_y_slice%02d;\n", subs+1, subs);
      fprintf( o, "slice_data(:,6,%d) = u_y_ave%02d;\n"  , subs+1, subs);
#if STORE_U_COMPOSITE
          if(subs==0)
          {
      fprintf( o, "slice_data(:,7,%d) = upr_x_slice;\n", subs+1);
      fprintf( o, "slice_data(:,8,%d) = upr_x_ave;\n"  , subs+1);
      fprintf( o, "slice_data(:,9,%d) = upr_y_slice;\n", subs+1);
      fprintf( o, "slice_data(:,10,%d) = upr_y_ave;\n"  , subs+1);
          }
#endif /* (STORE_U_COMPOSITE) */
    fprintf( o, "disp('slice_data(:,1,%d) = rho_slice%02d');\n", subs+1, subs);
    fprintf( o, "disp('slice_data(:,2,%d) = rho_ave%02d'  );\n", subs+1, subs);
    fprintf( o, "disp('slice_data(:,3,%d) = u_x_slice%02d');\n", subs+1, subs);
    fprintf( o, "disp('slice_data(:,4,%d) = u_x_ave%02d'  );\n", subs+1, subs);
    fprintf( o, "disp('slice_data(:,5,%d) = u_y_slice%02d');\n", subs+1, subs);
    fprintf( o, "disp('slice_data(:,6,%d) = u_y_ave%02d'  );\n", subs+1, subs);
#if STORE_U_COMPOSITE
          if(subs==0)
          {
  fprintf( o, "disp('slice_data(:,7,%d) = upr_x_slice');\n", subs+1);
  fprintf( o, "disp('slice_data(:,8,%d) = upr_x_ave'  );\n", subs+1);
  fprintf( o, "disp('slice_data(:,9,%d) = upr_y_slice');\n", subs+1);
  fprintf( o, "disp('slice_data(:,10,%d) = upr_y_ave'  );\n", subs+1);
          }
#endif /* (STORE_U_COMPOSITE) */
    } /* for( subs=0; subs<NUM_FLUID_COMPONENTS; subs++) */
    }

  } /* if( i0 == i1) else if( j0 == j1) */

  else
  {
    // Count.
    len = 0;
    for( i=i0; i<=i1; i++)
    {
      if( i>=0 && i<get_LX(lattice))
      {
        j = j0 + (i-i0)*((j1-j0)/(i1-i0));

        if( j>=0 && j<get_LY(lattice))
        {
          len++;
        }
      }
    }

    rho_slice = (double*)malloc( len*sizeof(double));
    rho_ave   = (double*)malloc( len*sizeof(double));
    u_x_slice = (double*)malloc( len*sizeof(double));
    u_x_ave   = (double*)malloc( len*sizeof(double));
    u_y_slice = (double*)malloc( len*sizeof(double));
    u_y_ave   = (double*)malloc( len*sizeof(double));
#if STORE_U_COMPOSITE
          if(subs==0)
          {
    upr_x_slice = (double*)malloc( len*sizeof(double));
    upr_x_ave   = (double*)malloc( len*sizeof(double));
    upr_y_slice = (double*)malloc( len*sizeof(double));
    upr_y_ave   = (double*)malloc( len*sizeof(double));
          }
#endif /* (STORE_U_COMPOSITE) */

    // Generate matlab script to plot the slices.
    sprintf( filename, "%s/%s%dx%d_frame%04d.m", get_out_path(lattice),
             root_word,
             get_LX(lattice),
             get_LY(lattice),
             lattice->time/lattice->param.FrameRate);
    if( !( o = fopen( filename, "w+")))
    {
      printf("ERROR: fopen( \"%s\", \"w+\") = NULL.  Bye, bye!\n", filename);
      process_exit(1);
    }

    for( subs=0; subs<NUM_FLUID_COMPONENTS; subs++)
    {
      // Slice.
      len = 0;
      for( i=i0; i<=i1; i++)
      {
        if( i>=0 && i<get_LX(lattice))
        {
          j = j0 + (i-i0)*((j1-j0)/(i1-i0));

          if( j>=0 && j<get_LY(lattice))
          {
            n = j*get_LX(lattice) + i;
            if( n != -1)
            {
              rho_slice[ len] = lattice->macro_vars[subs][ n].rho;
              u_x_slice[ len] = lattice->macro_vars[subs][ n].u[0];
              u_y_slice[ len] = lattice->macro_vars[subs][ n].u[1];
#if STORE_U_COMPOSITE
          if(subs==0)
          {
              upr_x_slice[ len] = lattice->upr[ n].u[0];
              upr_y_slice[ len] = lattice->upr[ n].u[1];
          }
#endif /* (STORE_U_COMPOSITE) */

              rho_ave[ len]   = 0.;
              u_x_ave[ len]   = 0.;
              u_y_ave[ len]   = 0.;
#if STORE_U_COMPOSITE
          if(subs==0)
          {
              upr_x_ave[ len]   = 0.;
              upr_y_ave[ len]   = 0.;
          }
#endif /* (STORE_U_COMPOSITE) */
              wid = 0;
              for( k=0; k<get_LY(lattice); k++)
              {
  if( !( lattice->bc[subs][ j*get_LX(lattice) + i].bc_type & BC_SOLID_NODE))
  {
                rho_ave[ len] +=
                  lattice->macro_vars[subs][ k*get_LX(lattice)+i].rho;
                u_x_ave[ len] +=
                  lattice->macro_vars[subs][ k*get_LX(lattice)+i].u[0];
                u_y_ave[ len] +=
                  lattice->macro_vars[subs][ k*get_LX(lattice)+i].u[1];
#if STORE_U_COMPOSITE
          if(subs==0)
          {
                upr_x_ave[ len] +=
                  lattice->upr[ k*get_LX(lattice)+i].u[0];
                upr_y_ave[ len] +=
                  lattice->upr[ k*get_LX(lattice)+i].u[1];
          }
#endif /* (STORE_U_COMPOSITE) */
                wid++;
  }
              }
              rho_ave[ len] /= wid;
              u_x_ave[ len] /= wid;
              u_y_ave[ len] /= wid;
#if STORE_U_COMPOSITE
          if(subs==0)
          {
              upr_x_ave[ len] /= wid;
              upr_y_ave[ len] /= wid;
          }
#endif /* (STORE_U_COMPOSITE) */

            }
            else
            {
              rho_slice[ len] = 0.;
              rho_ave[   len] = 0.;
              u_x_slice[ len] = 0.;
              u_x_ave[   len] = 0.;
              u_y_slice[ len] = 0.;
              u_y_ave[   len] = 0.;
#if STORE_U_COMPOSITE
          if(subs==0)
          {
              upr_x_slice[ len] = 0.;
              upr_x_ave[   len] = 0.;
              upr_y_slice[ len] = 0.;
              upr_y_ave[   len] = 0.;
          }
#endif /* (STORE_U_COMPOSITE) */
            }
            len++;
          }
        }
      }

      fprintf( o, "rho_slice%02d = [ ", subs);
      for( n=0; n<len; n++)
      {
        fprintf( o, " %20.17f ", rho_slice[ n]);
      }
      fprintf( o, "];\n");

      fprintf( o, "rho_ave%02d = [ ", subs);
      for( n=0; n<len; n++)
      {
        fprintf( o, " %20.17f ", rho_ave[ n]);
      }
      fprintf( o, "];\n");

      fprintf( o, "u_x_slice%02d = [ ", subs);
      for( n=0; n<len; n++)
      {
        fprintf( o, " %20.17f ", u_x_slice[ n]);
      }
      fprintf( o, "];\n");

      fprintf( o, "u_x_ave%02d = [ ", subs);
      for( n=0; n<len; n++)
      {
        fprintf( o, " %20.17f ", u_x_ave[ n]);
      }
      fprintf( o, "];\n");

      fprintf( o, "u_y_slice%02d = [ ", subs);
      for( n=0; n<len; n++)
      {
        fprintf( o, " %20.17f ", u_y_slice[ n]);
      }
      fprintf( o, "];\n");

      fprintf( o, "u_y_ave%02d = [ ", subs);
      for( n=0; n<len; n++)
      {
        fprintf( o, " %20.17f ", u_y_ave[ n]);
      }
      fprintf( o, "];\n");
#if STORE_U_COMPOSITE
          if(subs==0)
          {
      fprintf( o, "upr_x_slice= [ ");
      for( n=0; n<len; n++)
      {
        fprintf( o, " %20.17f ", upr_x_slice[ n]);
      }
      fprintf( o, "];\n");

      fprintf( o, "upr_x_ave= [ ");
      for( n=0; n<len; n++)
      {
        fprintf( o, " %20.17f ", upr_x_ave[ n]);
      }
      fprintf( o, "];\n");

      fprintf( o, "upr_y_slice= [ ");
      for( n=0; n<len; n++)
      {
        fprintf( o, " %20.17f ", upr_y_slice[ n]);
      }
      fprintf( o, "];\n");

      fprintf( o, "upr_y_ave= [ ");
      for( n=0; n<len; n++)
      {
        fprintf( o, " %20.17f ", upr_y_ave[ n]);
      }
      fprintf( o, "];\n");
          }
#endif /* (STORE_U_COMPOSITE) */

    } /* for( subs=0; subs<NUM_FLUID_COMPONENTS; subs++) */

  } /* if( i0 == i1) else if( j0 == j1) else */

  free( rho_slice);
  free( rho_ave  );
  free( u_x_slice);
  free( u_x_ave  );
  free( u_y_slice);
  free( u_y_ave  );
#if STORE_U_COMPOSITE
          if(subs==0)
          {
  free( upr_x_slice);
  free( upr_x_ave  );
  free( upr_y_slice);
  free( upr_y_ave  );
          }
#endif /* (STORE_U_COMPOSITE) */

  fprintf( o, "if( plot_stuff>0)\n");

    if( make_octave_scripts(lattice))
    {
      fprintf( o, "\nif(plot_stuff==1)\n");

      // Plot density.
      fprintf( o, "figure;");
      fprintf( o, "hold on;");
      for( subs=0; subs<NUM_FLUID_COMPONENTS; subs++)
      {
       fprintf( o, "plot( rho_slice%02d, %s);", subs, plot_specs[subs]);
      }
      fprintf( o, "title('\\rho slice (%d,%d)..(%d,%d)');", i0, j0, i1, j1);
      fprintf( o, "hold off;");
      fprintf( o, "\n");

      // Plot x velocity.
      fprintf( o, "figure;");
      fprintf( o, "hold on;");
      for( subs=0; subs<NUM_FLUID_COMPONENTS; subs++)
      {
       fprintf( o, "plot( u_x_slice%02d, %s);", subs, plot_specs[subs]);
      }
      fprintf( o, "title('u_x slice (%d,%d)..(%d,%d)');", i0, j0, i1, j1);
      fprintf( o, "hold off;");
      fprintf( o, "\n");

      // Plot y velocity.
      fprintf( o, "figure;");
      fprintf( o, "hold on;");
      for( subs=0; subs<NUM_FLUID_COMPONENTS; subs++)
      {
       fprintf( o, "plot( u_y_slice%02d, %s);", subs, plot_specs[subs]);
      }
      fprintf( o, "title('u_y slice (%d,%d)..(%d,%d)');", i0, j0, i1, j1);
      fprintf( o, "hold off;");
      fprintf( o, "\n");

      fprintf( o, "\nelse\n");

      // Plot density.
      for( subs=0; subs<NUM_FLUID_COMPONENTS; subs++)
      {
       fprintf( o, "figure;");
       fprintf( o, "plot( rho_slice%02d, %s);", subs, plot_specs[subs]);
       fprintf( o, "title('\\rho slice (%d,%d)..(%d,%d), subs %d');\n",
           i0, j0, i1, j1, subs);
      }

      // Plot x velocity.
      for( subs=0; subs<NUM_FLUID_COMPONENTS; subs++)
      {
       fprintf( o, "figure;");
       fprintf( o, "plot( u_x_slice%02d, %s);", subs, plot_specs[subs]);
       fprintf( o, "title('u_x slice (%d,%d)..(%d,%d), subs %d');\n",
           i0, j0, i1, j1, subs);
      }

      // Plot y velocity.
      for( subs=0; subs<NUM_FLUID_COMPONENTS; subs++)
      {
       fprintf( o, "figure;");
       fprintf( o, "plot( u_y_slice%02d, %s);", subs, plot_specs[subs]);
       fprintf( o, "title('u_y slice (%d,%d)..(%d,%d), subs %d');\n",
           i0, j0, i1, j1, subs);
      }

      fprintf( o, "\nend\n");

    } /* if( make_octave_scripts(lattice)) */

    else // make Matlab scripts
    {
  // Plot density.
  fprintf( o, "figure;");
  fprintf( o, "hold on;");
 for( subs=0; subs<NUM_FLUID_COMPONENTS; subs++)
 {
  fprintf( o, "plot( rho_slice%02d, %s);", subs, plot_specs[subs]);
 }
  fprintf( o, "warning off;");
  fprintf( o, "title('\\rho slice (%d,%d)..(%d,%d)');\n", i0, j0, i1, j1);
  fprintf( o, "warning on;");

 for( subs=0; subs<NUM_FLUID_COMPONENTS; subs++)
 {
  fprintf( o, "figure;");
  fprintf( o, "plot( rho_slice%02d, %s);", subs, plot_specs[subs]);
  fprintf( o, "warning off;");
  fprintf( o, "title('\\rho slice (%d,%d)..(%d,%d), subs %d');\n",
      i0, j0, i1, j1, subs);
  fprintf( o, "warning on;");
 }

  // Plot average density.
  fprintf( o, "figure;");
  fprintf( o, "hold on;");
 for( subs=0; subs<NUM_FLUID_COMPONENTS; subs++)
 {
  fprintf( o, "plot( rho_ave%02d, %s);", subs, plot_specs[subs]);
 }
  fprintf( o, "warning off;");
  fprintf( o, "title('\\rho_{ave} slice (%d,%d)..(%d,%d)');\n", i0, j0, i1, j1);
  fprintf( o, "warning on;");

 for( subs=0; subs<NUM_FLUID_COMPONENTS; subs++)
 {
  fprintf( o, "figure;");
  fprintf( o, "plot( rho_ave%02d, %s);", subs, plot_specs[subs]);
  fprintf( o, "warning off;");
  fprintf( o, "title('\\rho_{ave} slice (%d,%d)..(%d,%d), subs %d');\n",
      i0, j0, i1, j1, subs);
  fprintf( o, "warning on;");
 }

  // Plot x velocity.
  fprintf( o, "figure;");
  fprintf( o, "hold on;");
 for( subs=0; subs<NUM_FLUID_COMPONENTS; subs++)
 {
  fprintf( o, "plot( u_x_slice%02d, %s);", subs, plot_specs[subs]);
 }
  fprintf( o, "warning off;");
  fprintf( o, "title('u_x slice (%d,%d)..(%d,%d)');\n", i0, j0, i1, j1);
  fprintf( o, "warning on;");

 for( subs=0; subs<NUM_FLUID_COMPONENTS; subs++)
 {
  fprintf( o, "figure;");
  fprintf( o, "plot( u_x_slice%02d, %s);", subs, plot_specs[subs]);
  fprintf( o, "warning off;");
  fprintf( o, "title('u_x slice (%d,%d)..(%d,%d), subs %d');\n",
      i0, j0, i1, j1, subs);
  fprintf( o, "warning on;");
 }

  // Plot average x-velocity.
  fprintf( o, "figure;");
  fprintf( o, "hold on;");
 for( subs=0; subs<NUM_FLUID_COMPONENTS; subs++)
 {
  fprintf( o, "plot( u_x_ave%02d, %s);", subs, plot_specs[subs]);
 }
  fprintf( o, "warning off;");
  fprintf( o, "title('ux_{ave} slice (%d,%d)..(%d,%d)');\n", i0, j0, i1, j1);
  fprintf( o, "warning on;");

 for( subs=0; subs<NUM_FLUID_COMPONENTS; subs++)
 {
  fprintf( o, "figure;");
  fprintf( o, "plot( u_x_ave%02d, %s);", subs, plot_specs[subs]);
  fprintf( o, "warning off;");
  fprintf( o, "title('ux_{ave} slice (%d,%d)..(%d,%d), subs %d');\n",
      i0, j0, i1, j1, subs);
  fprintf( o, "warning on;");
 }

  // Plot y velocity.
  fprintf( o, "figure;");
  fprintf( o, "hold on;");
 for( subs=0; subs<NUM_FLUID_COMPONENTS; subs++)
 {
  fprintf( o, "plot( u_y_slice%02d, %s);", subs, plot_specs[subs]);
 }
  fprintf( o, "warning off;");
  fprintf( o, "title('u_y slice (%d,%d)..(%d,%d)');\n", i0, j0, i1, j1);
  fprintf( o, "warning on;");

 for( subs=0; subs<NUM_FLUID_COMPONENTS; subs++)
 {
  fprintf( o, "figure;");
  fprintf( o, "plot( u_y_slice%02d, %s);", subs, plot_specs[subs]);
  fprintf( o, "warning off;");
  fprintf( o, "title('u_y slice (%d,%d)..(%d,%d), subs %d');\n",
      i0, j0, i1, j1, subs);
  fprintf( o, "warning on;");
 }

  // Plot average y-velocity.
  fprintf( o, "figure;");
  fprintf( o, "hold on;");
 for( subs=0; subs<NUM_FLUID_COMPONENTS; subs++)
 {
  fprintf( o, "plot( u_y_ave%02d, %s);", subs, plot_specs[subs]);
 }
  fprintf( o, "warning off;");
  fprintf( o, "title('uy_{ave} slice (%d,%d)..(%d,%d)');\n", i0, j0, i1, j1);
  fprintf( o, "warning on;");

 for( subs=0; subs<NUM_FLUID_COMPONENTS; subs++)
 {
  fprintf( o, "figure;");
  fprintf( o, "plot( u_y_ave%02d, %s);", subs, plot_specs[subs]);
  fprintf( o, "warning off;");
  fprintf( o, "title('uy_{ave} slice (%d,%d)..(%d,%d), subs %d');\n",
      i0, j0, i1, j1, subs);
  fprintf( o, "warning on;");
 }

#if STORE_U_COMPOSITE
  // Plot x velocity.
  fprintf( o, "figure;");
  fprintf( o, "hold on;");
  fprintf( o, "plot( upr_x_slice, %s);", plot_specs[0]);
  fprintf( o, "warning off;");
  fprintf( o, "title('upr_x slice (%d,%d)..(%d,%d)');\n", i0, j0, i1, j1);
  fprintf( o, "warning on;");

  fprintf( o, "figure;");
  fprintf( o, "plot( upr_x_slice, %s);", plot_specs[0]);
  fprintf( o, "warning off;");
  fprintf( o, "title('upr_x slice (%d,%d)..(%d,%d)');\n",
      i0, j0, i1, j1);
  fprintf( o, "warning on;");

  // Plot average x-velocity.
  fprintf( o, "figure;");
  fprintf( o, "hold on;");
  fprintf( o, "plot( upr_x_ave, %s);", plot_specs[0]);
  fprintf( o, "warning off;");
  fprintf( o, "title('uprx_{ave} slice (%d,%d)..(%d,%d)');\n", i0, j0, i1, j1);
  fprintf( o, "warning on;");

  fprintf( o, "figure;");
  fprintf( o, "plot( upr_x_ave, %s);", plot_specs[0]);
  fprintf( o, "warning off;");
  fprintf( o, "title('uprx_{ave} slice (%d,%d)..(%d,%d)');\n",
      i0, j0, i1, j1);
  fprintf( o, "warning on;");

  // Plot y velocity.
  fprintf( o, "figure;");
  fprintf( o, "hold on;");
  fprintf( o, "plot( upr_y_slice, %s);", plot_specs[0]);
  fprintf( o, "warning off;");
  fprintf( o, "title('upr_y slice (%d,%d)..(%d,%d)');\n", i0, j0, i1, j1);
  fprintf( o, "warning on;");

  fprintf( o, "figure;");
  fprintf( o, "plot( upr_y_slice, %s);", plot_specs[0]);
  fprintf( o, "warning off;");
  fprintf( o, "title('upr_y slice (%d,%d)..(%d,%d)');\n",
      i0, j0, i1, j1);
  fprintf( o, "warning on;");

  // Plot average y-velocity.
  fprintf( o, "figure;");
  fprintf( o, "hold on;");
  fprintf( o, "plot( upr_y_ave, %s);", plot_specs[0]);
  fprintf( o, "warning off;");
  fprintf( o, "title('upry_{ave} slice (%d,%d)..(%d,%d)');\n", i0, j0, i1, j1);
  fprintf( o, "warning on;");

  fprintf( o, "figure;");
  fprintf( o, "plot( upr_y_ave, %s);", plot_specs[0]);
  fprintf( o, "warning off;");
  fprintf( o, "title('upry_{ave} slice (%d,%d)..(%d,%d)');\n",
      i0, j0, i1, j1);
  fprintf( o, "warning on;");

#endif /* (STORE_U_COMPOSITE) */

#if 1
  // Compare with analytical poiseuille flow profile.
 for( subs=0; subs<NUM_FLUID_COMPONENTS; subs++)
 {
  if(    lattice->param.gval[subs][0] == 0
      && lattice->param.gval[subs][1] != 0 )
  {
    // Poiseuille in y direction.
    fprintf( o, "disp(sprintf('\\nPoiseuille in y direction:'));\n");
    fprintf( o, "figure;\n");
    fprintf( o, "i0 = %d;\n", i0);
    fprintf( o, "i1 = %d;\n", i1);
    fprintf( o, "disp(sprintf('  [ i0 i1]   = [ %%d %%d]',i0,i1));\n");
    fprintf( o, "%%XXX H = i1 - i0;%% + 1;\n");
    fprintf( o, "H = i1 - i0 - 1; %% Assuming solids on both sides.\n");
    fprintf( o, "disp(sprintf('  H          = %%d', H));\n");
    fprintf( o, "R = H/2;\n");
    fprintf( o, "disp(sprintf('  R = H/2    = %%20.17f', R));\n");
    fprintf( o, "tau = %20.17f;\n",lattice->param.tau[0]);
    fprintf( o, "nu = (1/3)*(tau-.5);\n");
    fprintf( o, "disp(sprintf('  nu         = %%20.17f', nu));\n");
    compute_ave_rho( lattice, &ave_rho, subs);
    fprintf( o, "rho_ave = %20.17f;\n", ave_rho);
    fprintf( o, "disp(sprintf('  rho_ave    = %%20.17f', rho_ave));\n");
    //fprintf( o, "mu = nu*%20.17f;\n", ave_rho);
    //fprintf( o, "disp(sprintf('  mu         = %%20.17f', mu));\n");
    fprintf( o, "gvalval = %20.17f;\n",
      lattice->param.gval[subs][1]/lattice->param.tau[subs]);
    fprintf( o, "disp(sprintf('  gvalval    = %%20.17f', gvalval));\n");
    fprintf( o,
      "i = [i0:1:i1];\n"
      "ucalc = "
      "( gvalval / (2*nu)) * "
      "( R^2 - ( abs( i - (i1+i0)/2 ).^2));\n"
      );
    fprintf( o, "disp(sprintf('  size(ucalc) = %%dx%%d\\n',size(ucalc,1),size(ucalc,2)));\n");
    fprintf( o, "plot( i, ucalc, 'k');");
    fprintf( o, "title('analytical Poiseuille profile');\n");

    fprintf( o, "figure;\n");
    fprintf( o, "hold on;\n");
    fprintf( o, "plot( i, ucalc, 'k');\n");
    fprintf( o, "plot( i, u_y_slice%02d, 'bo');\n", subs);
    fprintf( o, "title(sprintf('LB results overlaying analytical Poiseuille profile, norm(diff)=%%20.17f',norm(u_y_slice%02d-ucalc)));\n", subs);
    fprintf( o, "xlims = get(gca,'xlim'); dx = xlims(2)-xlims(1);");
    fprintf( o, "ylims = get(gca,'ylim'); dy = ylims(2)-ylims(1);");
    fprintf( o, "axis( [ xlims(1)-.05*dx xlims(2)+.05*dx ylims(1) ylims(2)]);");
    fprintf( o, "hold off;\n");
  } /* if( lattice->param.gval[subs][0] == 0 && lattice->param.gval... */

  else if(    lattice->param.gval[subs][1] == 0
           && lattice->param.gval[subs][0] != 0 )
  {
    // Poiseuille in x direction.
    fprintf( o, "disp(sprintf('\\nPoiseuille in x direction:'));\n");
    fprintf( o, "figure;\n");
    fprintf( o, "j0 = %d;\n", j0);
    fprintf( o, "j1 = %d;\n", j1);
    fprintf( o, "disp(sprintf('  [ j0 j1]   = [ %%d %%d]',j0,j1));\n");
    fprintf( o, "H = j1 - j0 - 1; %% Assuming solids on both sides.\n");
    fprintf( o, "disp(sprintf('  H          = %%d', H));\n");
    fprintf( o, "R = H/2;\n");
    fprintf( o, "disp(sprintf('  R = H/2    = %%20.17f', R));\n");
    fprintf( o, "nu = 1/6;\n");
    fprintf( o, "disp(sprintf('  nu         = %%20.17f', nu));\n");
    compute_ave_rho( lattice, &ave_rho, subs);
    fprintf( o, "rho_ave = %20.17f;\n", ave_rho);
    fprintf( o, "disp(sprintf('  rho_ave    = %%20.17f', rho_ave));\n");
    //fprintf( o, "mu = nu*%20.17f;\n", ave_rho);
    //fprintf( o, "disp(sprintf('  mu         = %%20.17f', mu));\n");
    fprintf( o, "gvalval = %20.17f;\n",
      lattice->param.gval[subs][0]/lattice->param.tau[subs]);
    fprintf( o, "disp(sprintf('  gvalval    = %%20.17f', gvalval));\n");
    fprintf( o,
      "j = [j0:1:j1];"
      "ucalc = "
      "( gvalval / (2*nu)) * "
      "( R^2 - ( abs( j - (j1+j0)/2 ).^2));\n"
      );
    fprintf( o, "disp(sprintf('  size(ucalc) = %%dx%%d\\n',"
                "size(ucalc,1),size(ucalc,2)));\n");
    fprintf( o, "plot( j, ucalc, 'k');");
    fprintf( o, "title('analytical Poiseuille profile');\n");

    fprintf( o, "figure;\n");
    fprintf( o, "hold on;\n");
    fprintf( o, "plot( j, ucalc, 'k');\n");
    fprintf( o, "plot( u_x_slice%02d, 'bo');\n", subs);
    fprintf( o, "title(sprintf('LB results overlaying analytical Poiseuille profile, norm(diff)=%%20.17f',norm(u_x_slice%02d-ucalc)));\n", subs);
    fprintf( o, "xlims = get(gca,'xlim'); dx = xlims(2)-xlims(1);");
    fprintf( o, "ylims = get(gca,'ylim'); dy = ylims(2)-ylims(1);");
    fprintf( o, "axis( [ xlims(1) xlims(2) ylims(1)-.05*dy ylims(2)+.05*dy]);");
    fprintf( o, "hold off;\n");
    //printf( o, "figure;\n");
    //fprintf( o, "plot( ucalc(j0+1:10:10*j1+1) - u_x_slice%02d, 'r');", subs);
    //fprintf( o, "title( 'ucalc - u_x_slice%02d');\n", subs);
  } /* if( lattice->param.gval[subs][0] == 0 && lattice->param.gval... */



  // Slice key.  (Shows where in the domain the slice cuts.)
  fprintf( o, "figure; plot( [ %d %d], [ %d %d], 'r-.');", i0, i1, j0, j1);
  fprintf( o, "axis equal;");
  fprintf( o, "axis([ %d %d %d %d]);",
    0, get_LX(lattice)-1, 0, get_LY(lattice)-1);
  fprintf( o, "title('Slice key.');\n");

#if VERBOSITY_LEVEL > 0
  printf("private_slice() -- Wrote file \"%s\".\n", filename);
#endif /* VERBOSITY_LEVEL > 0 */

 } /* for( subs=0; subs<NUM_FLUID_COMPONENTS; subs++) */
#endif
    }

  fprintf( o, "end %% if(plot_stuff>0)\n");

 for( subs=0; subs<NUM_FLUID_COMPONENTS; subs++)
 {
  fprintf( o,
      "disp(sprintf('q_x = %%f',rho_slice%02d*u_x_slice%02d'/max(size(rho_slice%02d))'));\n",
      subs, subs, subs);
  fprintf( o,
      "disp(sprintf('q_y = %%f',rho_slice%02d*u_y_slice%02d'/max(size(rho_slice%02d))'));\n",
      subs, subs, subs);
 }

  fclose(o);

} /* void private_slice( lattice_ptr lattice, int i0, int j0, int i1, int j1) */
                                                                        // }}}

#if INAMURO_SIGMA_COMPONENT && STORE_BTC
// dump_sigma_btc {{{
void dump_sigma_btc( lattice_ptr lattice)
{
  FILE   *o;
  char   fn[1024];
  int    n;
  double D;
  int    btc_spot, start_time;

  if( lattice->param.sigma_btc_rate <= 0 || lattice->FlowDir==0)
  {
    return;
  }

  btc_spot =
    (lattice->param.sigma_btc_spot >= 0)
    ?
    (lattice->param.sigma_btc_spot-0)
    :
    (((lattice->FlowDir==1)?(get_LX(lattice)):(get_LY(lattice)))-2-0);

  start_time =
     (lattice->param.sigma_start>0)
    ?(lattice->param.sigma_start)
    :(0);

  sprintf( fn, "%s/sigma_btc.m", get_out_path(lattice));

  o = fopen( fn, "w+");

  // Timesteps where measurements are taken.
  fprintf( o, "ts = [\n");
  for( n=1; n<lattice->SizeBTC; n++)
  {
    fprintf( o, "%20.17f\n", lattice->param.sigma_btc[5*n+0]-start_time);
  }
  fprintf( o, "];\n");

  // Break through curve at sigma_spot-1.
  fprintf( o, "btc01 = [\n");
  for( n=1; n<lattice->SizeBTC; n++)
  {
    fprintf( o, "%20.17f\n", lattice->param.sigma_btc[5*n+1]);
  }
  fprintf( o, "];\n");

  // Break through curve at sigma_spot.
  fprintf( o, "btc02 = [\n");
  for( n=1; n<lattice->SizeBTC; n++)
  {
    fprintf( o, "%20.17f\n", lattice->param.sigma_btc[5*n+2]);
  }
  fprintf( o, "];\n");

  // Break through curve at sigma_spot+1.
  fprintf( o, "btc03 = [\n");
  for( n=1; n<lattice->SizeBTC; n++)
  {
    fprintf( o, "%20.17f\n", lattice->param.sigma_btc[5*n+3]);
  }
  fprintf( o, "];\n");


  // Velocity at sigma_spot.
  fprintf( o, "btc_v = [\n");
  for( n=1; n<lattice->SizeBTC; n++)
  {
    fprintf( o, "%20.17f\n", lattice->param.sigma_btc[5*n+4]);
  }
  fprintf( o, "];\n");

  // Concentration gradient.
  fprintf( o, "dcdx = [\n");
  for( n=1; n<lattice->SizeBTC; n++)
  {
    fprintf( o, "%20.17f\n",
    .5*( lattice->param.sigma_btc[5*n+3]
       - lattice->param.sigma_btc[5*n+1]) );
  }
  fprintf( o, "];\n");

  // C*v.
  fprintf( o, "Cv = [\n");
  for( n=1; n<lattice->SizeBTC; n++)
  {
    fprintf( o, "%20.17f\n",
      //lattice->param.sigma_btc[4*n+1]*lattice->param.sigma_btc[4*n+3] );
      ( lattice->param.sigma_btc[5*n+2]*lattice->param.sigma_btc[5*n+4]) );
  }
  fprintf( o, "];\n");

  // Cf = ( C*v - D*dcdx) / v.
  D = (1./3.)*(lattice->param.tau[1] - .5);
  fprintf( o, "Cf = [\n");
  for( n=1; n<lattice->SizeBTC; n++)
  {
    fprintf( o, "%20.17f\n",
      (
        ( lattice->param.sigma_btc[5*n+2]*lattice->param.sigma_btc[5*n+4])
      -
        ( D)*.5*( lattice->param.sigma_btc[5*n+3]
                - lattice->param.sigma_btc[5*n+1])
      )
      / ( lattice->param.sigma_btc[5*n+4])
      );
  }
  fprintf( o, "];\n");

  fprintf( o, "D = %20.17f;\n", D);
  fprintf( o, "disp(sprintf('D = %%20.17f',D));\n");

  // Plot btc01.
  //fprintf( o, "figure; plot(btc01);\n");
  //fprintf( o, "axis([ %d %d 0 max(max(btc01),%20.17f)])\n",
  //    1, lattice->SizeBTC+1, lattice->param.rho_sigma);
  //fprintf( o, "title('BTC at L=%d, t=%d:%d:%d');\n",
  //  btc_spot-1, start_time,
  //  lattice->param.sigma_btc_rate,
  //  lattice->NumTimeSteps          );

  if( make_octave_scripts(lattice))
  {
    fprintf( o, "figure(1);\n");
    fprintf( o, "hold on; plot(btc01);\n");
  }
  else
  {
    fprintf( o, "figure;\n");

    // SubPlot btc0{1,2,3}.
    fprintf( o, "subplot(2,2,1);\n");
    fprintf( o, "hold on; plot(btc01);\n");
    fprintf( o, "hnd = get(gca,'Children');\n");
    fprintf( o, "set( hnd(1), 'Color', [ .8 .8 .8]);\n");
  }
  if( make_octave_scripts(lattice))
  {
    fprintf( o, "hold on; plot(btc03);\n");
  }
  else
  {
    fprintf( o, "hold on; plot(btc03);\n");
    fprintf( o, "hnd = get(gca,'Children');\n");
    fprintf( o, "set( hnd(1), 'Color', [ .8 .8 .8]);\n");
  }
  if( make_octave_scripts(lattice))
  {
    fprintf( o, "hold on; plot(btc02);\n");
  }
  else
  {
    fprintf( o, "hold on; plot(btc02);\n");
    fprintf( o, "set(gca,'Xlim',[ 0 size(btc02,1)])\n");
  }
  //fprintf( o, "axis([ %d %d 0 max(max(btc02),%20.17f)])\n",
  //    1, lattice->SizeBTC+1, lattice->param.rho_sigma);
  fprintf( o, "title('BTC at L\\in\\{%d,%d,%d\\}, t=%d:%d:%d');\n",
    btc_spot-1,
    btc_spot,
    btc_spot+1,
    start_time,
    lattice->param.sigma_btc_rate,
    lattice->NumTimeSteps          );

  if( make_octave_scripts( lattice)){ return;}

  // Plot btc03.
  //fprintf( o, "figure; plot(btc03);\n");
  //fprintf( o, "axis([ %d %d 0 max(max(btc03),%20.17f)])\n",
  //    1, lattice->SizeBTC+1, lattice->param.rho_sigma);
  //fprintf( o, "title('BTC at L=%d, t=%d:%d:%d');\n",
  //  btc_spot+1, start_time,
  //  lattice->param.sigma_btc_rate,
  //  lattice->NumTimeSteps          );

  if( make_octave_scripts(lattice))
  {
    fprintf( o, "figure(2);\n");
    fprintf( o, "plot(Cv);\n");
  }
  else
  {
    fprintf( o, "subplot(2,2,2);\n");
    fprintf( o, "plot(Cv);\n");
    fprintf( o, "set(gca,'Xlim',[ 0 size(btc02,1)]);\n");
  }
  fprintf( o, "title('C*v at L=%d, t=%d:%d:%d');\n",
    btc_spot, start_time,
    lattice->param.sigma_btc_rate,
    lattice->NumTimeSteps          );

  if( make_octave_scripts(lattice))
  {
    fprintf( o, "figure(3);\n");
    fprintf( o, "plot(dcdx);\n");
  }
  else
  {
    fprintf( o, "subplot(2,2,3);\n");
    fprintf( o, "plot(dcdx);\n");
    fprintf( o, "set(gca,'Xlim',[ 0 size(btc02,1)]);\n");
  }
  fprintf( o, "title('dc/dx at L=%d, t=%d:%d:%d');\n",
    btc_spot, start_time,
    lattice->param.sigma_btc_rate,
    lattice->NumTimeSteps          );

  if( make_octave_scripts(lattice))
  {
    fprintf( o, "figure(4);\n");
    fprintf( o, "plot(Cf);\n");
  }
  else
  {
    fprintf( o, "subplot(2,2,4);\n");
    fprintf( o, "plot(Cf);\n");
    fprintf( o, "set(gca,'Xlim',[ 0 size(btc02,1)]);\n");
  }
  fprintf( o, "title('Cf=(C*v-D*(dC/dx))/v at L=%d, t=%d:%d:%d');\n",
    btc_spot, start_time,
    lattice->param.sigma_btc_rate,
    lattice->NumTimeSteps          );

  if( make_octave_scripts(lattice))
  {
  }
  else
  {
    // Give figure a name (shows up in title bar).
    fprintf( o, "set(gcf,'Name','Breakthrough curve data at t=%d:%d:%d');\n",
      start_time,
      lattice->param.sigma_btc_rate,
      lattice->NumTimeSteps          );
  }

  // Plot [Cr(x),'r'][Cr(x-dx),'m'][Cv(x),'g'][Cf(x),'b']
  fprintf( o, "figure;\n");
  fprintf( o, "hold on;\n");
  fprintf( o, "plot( btc02, 'rx');\n");
  fprintf( o, "plot(Cf, 'bo');\n");
  fprintf( o, "hold off;\n");
  if( make_octave_scripts(lattice))
  {
  }
  else
  {
    fprintf( o, "set(gca,'Xlim',[ 0 size(btc02,1)]);\n");
  }
  fprintf( o, "title('Cr=''rx'', Cf=''bo''');\n");

  if( make_octave_scripts(lattice))
  {
  }
  else
  {
    // Give figure a name (shows up in title bar).
    fprintf( o,
      "set(gcf,'Name','Resident C_r and flux averaged C_f concentrations "
      "at t=%d:%d:%d');\n",
      start_time,
      lattice->param.sigma_btc_rate,
      lattice->NumTimeSteps          );
    // Turn off figure number in title bar.
    fprintf( o, "set(gcf,'NumberTitle','off');\n");
  }

  fprintf( o, "%% FlowDir = %d;\n", lattice->FlowDir);
  fclose(o);

  printf("\nBTC (size=%d) stored in file \"%s\".\n", lattice->SizeBTC, fn);

} /* void dump_sigma_btc( lattice_ptr lattice) */
                                                                        // }}}
#endif /* INAMURO_SIGMA_COMPONENT && STORE_BTC */

// C O U N T   C O L O R M A P {{{
//##############################################################################
//void count_colormap( int *num_colors)
//
//  - Count colormap entries in file colormap.rgb .
//
void count_colormap( int *num_colors)
{
  FILE   *in;
  char   filename[1024];
  double r, g, b;

  // First, count the number of entries.
  sprintf( filename, "%s", "./in/colormap.rgb");
  if( !( in = fopen( filename, "r+")))
  {
    printf("%s %d >> Error opening file \"%s\" for reading.  Exiting!\n",
      __FILE__,__LINE__,filename);
    process_exit(1);
  }
  *num_colors = 0;
  fscanf( in, "%lf %lf %lf", &r, &g, &b);
  while( !feof(in))
  {
    (*num_colors)++;
    fscanf( in, "%lf %lf %lf", &r, &g, &b);
  }
  fclose(in);

} /* void count_colormap( int *num_colors) */
                                                                        // }}}
// A L L O C A T E   C O L O R M A P {{{
//##############################################################################
//void allocate_colormap( double ***colormap, int *num_colors)
//
void allocate_colormap( double ***colormap, int num_colors)
{
  int i;

  *colormap = (double**)malloc( num_colors*sizeof(double*));
  for( i=0; i<num_colors; i++)
  {
    (*colormap)[i] = (double*)malloc( 3*sizeof(double));
  }

} /* void allocate_colormap( double ***colormap, int num_colors) */
                                                                        // }}}
// R E A D   C O L O R M A P {{{
//##############################################################################
//void read_colormap( double **colormap, int num_colors)
//
//  - Read colormap from file colormap.rgb .
//
//  - Color map values are stored with one set of rgb values per line.
//
//  - RGB values are stored between 0 and 1 .
//
//  - Colormap values could come from Matlab, e.g.
//
//     >> cm = colormap;
//     >> save 'colormap.rgb' cm -ascii;
//
void read_colormap( double **colormap, int num_colors)
{
  FILE   *in;
  char   filename[1024];
  double r, g, b;
  int    n;

  sprintf( filename, "%s", "./in/colormap.rgb");
  if( !( in = fopen( filename, "r+")))
  {
    printf("%s %d >> Error opening file \"%s\" for reading.  Exiting!\n",
      __FILE__,__LINE__,filename);
    process_exit(1);
  }

  n = 0;
  fscanf( in, "%lf %lf %lf", &r, &g, &b);
  while( !feof(in))
  {
    assert( n!=num_colors);
    colormap[n][0] = r;
    colormap[n][1] = g;
    colormap[n][2] = b;
    n++;
    fscanf( in, "%lf %lf %lf", &r, &g, &b);
  }
  fclose(in);

} /* void read_colormap( double **colormap, int num_colors) */
                                                                        // }}}
// D E A L L O C A T E   C O L O R M A P {{{
//##############################################################################
//void deallocate_colormap( double ***colormap, int num_colors)
//
void deallocate_colormap( double ***colormap, int num_colors)
{
  int i;

  for( i=0; i<num_colors; i++)
  {
    free( (*colormap)[i]);
  }
  free( *colormap);

} /* void deallocate_colormap( double ***colormap, int num_colors) */
                                                                        // }}}
// get_color {{{
void get_color(
       double **colormap, int num_colors, double c, char *r, char *g, char *b)
{
  int n;
  double n1, n2;
  double w1, w2;

  if( c>=0. && c<=1.)
  {
#if 1
    n = (int)ROUND( c*((double)num_colors-1.));
//  printf("get_color() -- c = %f, num_colors = %d, n = %d\n", c, num_colors, n);
    *r = (char)ROUND(255.*colormap[ n][0]);
    *g = (char)ROUND(255.*colormap[ n][1]);
    *b = (char)ROUND(255.*colormap[ n][2]);
//  printf("get_color() -- n = %d, (%f,%f,%f)\n", n, (double)*r, (double)*g, (double)*b);
#else
    n1 = floor( c*((double)num_colors-1.));
    n2 = ceil( c*((double)num_colors-1.));
    w1 = c-n1;
    w2 = n2-c;
    *r =
      (char)ROUND(255.* ( w1*colormap[ (int)n1][0] + w2*colormap[ (int)n2][0]));
    *g =
      (char)ROUND(255.* ( w1*colormap[ (int)n1][1] + w2*colormap[ (int)n2][1]));
    *b =
      (char)ROUND(255.* ( w1*colormap[ (int)n1][2] + w2*colormap[ (int)n2][2]));
#endif
  }
  else
  {
    *r = (char)(255.);
    *g = (char)(255.);
    *b = (char)(255.);
  }

} /* void get_color( double **colormap, int num_colors, double c, ... */
                                                                        // }}}

#if WRITE_CHEN_DAT_FILES
// chen_output {{{
void chen_output( lattice_ptr lattice)
{
  int    x, y;

  int    LX = get_LX(lattice),

         LY = get_LY(lattice);

  double *u, *rho[NUM_FLUID_COMPONENTS];

  double ux_sum, uy_sum,

         rho_in, rho_out;

  FILE   *app7, *app8, *app9;

  char   filename[1024];

  double sum_mass[NUM_FLUID_COMPONENTS];

  int    subs;

printf("\n\nWARNING: chen_output() function is deprecated!\n\n");

  ux_sum = 0.;
  uy_sum = 0.;

  for( subs=0; subs<NUM_FLUID_COMPONENTS; subs++)
  {
    sum_mass[ subs] = 0.;
  }

  sprintf( filename, "%s", "%s/chen_xyrho.dat", get_out_path(lattice));
  if( !( app7 = fopen( filename,"a")))
  {
    printf("Error opening \"%s\" for reading.  Exiting!\n", filename);
    process_exit(1);
  }
  sprintf( filename, "%s", "%s/chen_xy_ux_uy.dat", get_out_path(lattice));
  if( !( app8 = fopen( filename,"a")))
  {
    printf("Error opening \"%s\" for reading.  Exiting!\n", filename);
    process_exit(1);
  }
  sprintf( filename, "%s", "%s/chen_time.dat", get_out_path(lattice));
  if( !( app9 = fopen( filename,"a")))
  {
    printf("Error opening \"%s\" for reading.  Exiting!\n", filename);
    process_exit(1);
  }

#if STORE_U_COMPOSITE
  u = lattice->upr[0].u;
#else /* !( STORE_U_COMPOSITE) */
  u = lattice->macro_vars[0][0].u;
#endif /* STORE_U_COMPOSITE */
  for( subs=0; subs<NUM_FLUID_COMPONENTS; subs++)
  {
    rho[subs] = &( lattice->macro_vars[subs][0].rho);
  }
  for( y = 0; y < LY; y++)
  {
    for( x = 0; x < LX; x++)
    {
      fprintf( app8,
              " %9.1f %9.1f %13.5e %13.5e\n",
               (double)(x+1.),
               (double)(y+1.),
               *u,
               *(u+1));

      if( !( lattice->bc[0][ y*LX + x].bc_type & BC_SOLID_NODE))
      {
        ux_sum = ux_sum + *u;
        uy_sum = uy_sum + *(u+1);

        for( subs=0; subs<NUM_FLUID_COMPONENTS; subs++)
        {
          sum_mass[subs] = sum_mass[subs] + *rho[subs];
        }

      } /* if( !obst[y][x]) */

#if STORE_U_COMPOSITE
      u+=2;
#else /* !( STORE_U_COMPOSITE) */
      u+=3;
#endif /* STORE_U_COMPOSITE */
      for( subs=0; subs<NUM_FLUID_COMPONENTS; subs++)
      {
        rho[subs]+=3;
      }

    } /* for( x = 1; x <= LX; x++) */
  } /* for( y = 1; y <= LY; y++) */


  fprintf( app9, "%10d %15.7f %15.7f ", lattice->time, ux_sum, uy_sum);
  for( subs=0; subs<NUM_FLUID_COMPONENTS; subs++)
  {
    fprintf( app9, "%15.7f ", sum_mass[subs]);
  }
  fprintf( app9, "\n");

  for( subs=0; subs<NUM_FLUID_COMPONENTS; subs++)
  {
    rho[subs] = &( lattice->macro_vars[subs][0].rho);
  }

  for( y = 0; y < LY; y++)
  {
    for( x = 0; x < LX; x++)
    {
      if( !( lattice->bc[0][ y*LX + x].bc_type & BC_SOLID_NODE))
      {
        for( subs=0; subs<NUM_FLUID_COMPONENTS; subs++)
        {
          fprintf( app7, "%f ", *rho[subs]);
        }
        fprintf( app7, "\n");
      }
      else
      {
        for( subs=0; subs<NUM_FLUID_COMPONENTS; subs++)
        {
          fprintf( app7, "%f ", 0.);
        }
        fprintf( app7, "\n");
      }

      for( subs=0; subs<NUM_FLUID_COMPONENTS; subs++)
      {
        rho[subs]+=3;
      }

    } /* for( x = 1; x <= LX; x++) */
  } /* for( y = 1; y <= LY; y++) */


  fclose( app7);
  fclose( app8);
  fclose( app9);

#if VERBOSITY_LEVEL > 0
  sprintf( filename, "%s", "%s/chen_xyrho.dat", get_out_path(lattice));
  printf("chen_output() -- Wrote file \"%s\".\n", filename);
  sprintf( filename, "%s", "%s/chen_xy_ux_uy.dat", get_out_path(lattice));
  printf("chen_output() -- Wrote file \"%s\".\n", filename);
  sprintf( filename, "%s", "%s/chen_time.dat", get_out_path(lattice));
  printf("chen_output() -- Wrote file \"%s\".\n", filename);
#endif /* VERBOSITY_LEVEL > 0 */

} /* void chen_output( lattice_ptr lattice) */
                                                                        // }}}
#endif /* WRITE_CHEN_DAT_FILES */

// B M P   R E A D   H E A D E R {{{
//##############################################################################
//void bmp_read_header( FILE *in)
//
void bmp_read_header( FILE *in, struct bitmap_info_header *bmih)
{
  char   filename[1024];
  int    i, j, n, m;
  int    ei, ej;
  int    pad, bytes_per_row;
  char   k;
  char   b, g, r;
  struct bitmap_file_header bmfh;
  struct rgb_quad rgb;
  int    *int_ptr;
  short  int *short_int_ptr;
  int    *width_ptr;
  int    *height_ptr;
  short  int *bitcount_ptr;
  int    subs;

  // Read the headers.
  n = fread( &bmfh, sizeof(struct bitmap_file_header), 1, in );
  if( strncmp(bmfh.bfType,"BM",2))
  {
    printf("ERROR: Can't process this file type.  Exiting!\n");
    printf("\n");
    process_exit(1);
  }
  n = fread( bmih, sizeof(struct bitmap_info_header), 1, in );
  int_ptr = (int*)bmih->biCompression;
  if( *int_ptr != 0)
  {
    printf("ERROR: Can't handle compression.  Exiting!\n");
    printf("\n");
    process_exit(1);
  }

printf("%s %d >> biWidth = %d\n", __FILE__, __LINE__, *((int*)bmih->biWidth));

  width_ptr = (int*)bmih->biWidth;
  height_ptr = (int*)bmih->biHeight;
  bitcount_ptr = (short int*)bmih->biBitCount;

  // Read the palette, if necessary.
  if( ENDIAN2(*bitcount_ptr) < 24)
  {
    n = (int)pow(2.,(double)ENDIAN2(*bitcount_ptr)); // Num palette entries.
    for( i=0; i<n; i++)
    {
      k = fread( &rgb, sizeof(struct rgb_quad), 1, in );
      if( k!=1)
      {
        printf("Error reading palette entry %d.  Exiting!\n", i);
        process_exit(1);
      }
    }
  }

  // Bytes per row of the bitmap.
  bytes_per_row =
    ((int)ceil(( (((double)(ENDIAN4(*width_ptr)))*((double)(ENDIAN2(*bitcount_ptr))))/8.)));

  // Bitmaps pad rows to preserve 4-byte boundaries.
  // The length of a row in the file will be bytes_per_row + pad .
  pad = ((4) - bytes_per_row%4)%4;

} /* void bmp_read_header( FILE *in) */
                                                                        // }}}
// B M P   R E A D   E N T R Y {{{
//##############################################################################
//void bmp_read_entry( FILE *in, char *r, char *g, char *b)
//
// Reads an RGB value from file on 'in' with bmp info header 'bmih'.
// The RGB values are returned in the 'r', 'g', and 'b' char* arguments.
//
// WARNING: As implemented, with the static 'n' counter, this mechanism
// only supports reading from one BMP file at a time. Furthermore, if
// reading is aborted before all entries are read, the counter will
// not be reset and subsequent attempts to read BMP files will be
// screwed up.
//
void bmp_read_entry(
  FILE *in,
  struct bitmap_info_header *bmih,
  char *r, char *g, char *b)
{
  char   filename[1024];
  int    i, j, m;
  static int n=0;
  int    ei, ej;
  int    pad, bytes_per_row;
  char   k, p;
  struct bitmap_file_header bmfh;
  struct rgb_quad rgb;
  int    *int_ptr;
  short  int *short_int_ptr;
  int    *width_ptr;
  int    *height_ptr;
  short  int *bitcount_ptr;
  int    subs;

  width_ptr = (int*)bmih->biWidth;
  height_ptr = (int*)bmih->biHeight;
  bitcount_ptr = (short int*)bmih->biBitCount;

  // Bytes per row of the bitmap.
  bytes_per_row =
    ((int)ceil(( (((double)(ENDIAN4(*width_ptr)))*((double)(ENDIAN2(*bitcount_ptr))))/8.)));

  // Bitmaps pad rows to preserve 4-byte boundaries.
  // The length of a row in the file will be bytes_per_row + pad .
  pad = ((4) - bytes_per_row%4)%4;

  switch(ENDIAN2(*bitcount_ptr))
  {
    case 1: // Monochrome.
      printf("read_bcs() -- "
          "Support for Monochrome BMPs is pending.  "
          "Exiting!\n");
      process_exit(1);
    case 4: // 16 colors.
      printf("read_bcs() -- "
          "Support for 16 color BMPs is pending.  "
          "Exiting!\n");
      process_exit(1);
    case 8: // 256 colors.
      printf("read_bcs() -- "
          "Support for 256 color BMPs is pending.  "
          "Exiting!\n");
      process_exit(1);
    case 24: // 24-bit colors.
    if( !feof(in)) { n+=( k = fread( b, 1, 1, in ));}
    if( !feof(in)) { n+=( k = fread( g, 1, 1, in ));}
    if( !feof(in)) { n+=( k = fread( r, 1, 1, in ));}
      break;

    default: // 32-bit colors?
      printf("ERROR: Unhandled color depth, "
          "BitCount = %d. Exiting!\n", ENDIAN2(*bitcount_ptr));
      process_exit(1);
      break;

  } /* switch(*(bmih->biBitCount)) */

  if( (n%(bytes_per_row+pad)) == 3*(*width_ptr))
  {
    // Read pad bytes first.
    for( i=1; i<=pad; i++)
    {
      if( !feof(in)) { n+=( k = fread( &p, 1, 1, in ));}
    }
  }

  if( feof(in))
  {
    printf(
      "bmp_read_entry() -- ERROR:"
      "Attempt to read past the end of file. "
      "Exiting!\n");
    process_exit(1);
  }

  // If all the entries have been read, reset the counter.
  if( n == (bytes_per_row+pad)*(*height_ptr) )
  {
    n = 0;
  }

} /* void bmp_read_entry( FILE *in, char *r, char *g, char *b) */
                                                                        // }}}
// B M P   W R I T E   H E A D E R {{{
//##############################################################################
//void bmp_write_header( FILE *in)
//
void bmp_write_header(
       FILE *out,
       bmp_hdr_ptr bmp_hdr,
       int ni,
       int nj,
       int bits)
{
  int bytes_per_row;
  int pad;
  int n;
  int *intptr;

  // Bytes per row of the bitmap.
  bytes_per_row = ((int)ceil((( ((double)(ni))*((double)(bits)) )/8.)));

  // Bitmaps pad rows to preserve 4-byte boundaries.
  // The length of a row in the file will be bytes_per_row + pad .
  pad = ((4) - bytes_per_row%4)%4;

  bmp_hdr->type[0]='B';
  bmp_hdr->type[1]='M';
  *(int*)(bmp_hdr->size) = ENDIAN4((int)(/*header*/54 + /*data*/(ni*bits + pad*8)*nj/8));
  *(int*)(bmp_hdr->rsvd) = (int)0;
  *(int*)(bmp_hdr->dptr) = ENDIAN4((int)54);

  *(int*)(bmp_hdr->forty) = ENDIAN4((int)40);
  *(int*)(bmp_hdr->width) = ENDIAN4((int)ni);
  *(int*)(bmp_hdr->height) = ENDIAN4((int)nj);
  *(short int*)(bmp_hdr->planes) = ENDIAN2((short int)1);
  *(short int*)(bmp_hdr->depth) = ENDIAN2((short int)bits);
  *(int*)(bmp_hdr->zeros+0) = (int)0;
  *(int*)(bmp_hdr->zeros+4) = (int)0;
  *(int*)(bmp_hdr->zeros+8) = (int)0;
  *(int*)(bmp_hdr->zeros+12) = (int)0;
  *(int*)(bmp_hdr->zeros+16) = (int)0;
  *(int*)(bmp_hdr->zeros+20) = (int)0;

  printf("size = %d\n", ENDIAN4((int)bmp_hdr->size));
  printf("sizeof(short int) = %d\n",sizeof(short int));
  printf("sizeof(bmp_hdr) = %d\n",sizeof(struct bmp_hdr_struct));
  printf("sizeof(test_struct) = %d\n",sizeof(struct test_struct));

  n = fwrite( bmp_hdr, sizeof(struct bmp_hdr_struct), 1, out);

#if 0
  char   filename[1024];
  int    i, j, n, m;
  int    ei, ej;
  int    pad, bytes_per_row;
  char   k;
  int    *int_ptr;
  short  int *short_int_ptr;
  int    *width_ptr;
  int    *height_ptr;
  short  int *bitcount_ptr;
  int    subs;

  // Read the headers.
  n = fread( &bmfh, sizeof(struct bitmap_file_header), 1, out );
  if( strncmp(bmfh.bfType,"BM",2))
  {
    printf("ERROR: Can't process this file type.  Exiting!\n");
    printf("\n");
    process_exit(1);
  }
  n = fread( bmih, sizeof(struct bitmap_info_header), 1, out );
  int_ptr = (int*)bmih->biCompression;
  if( *int_ptr != 0)
  {
    printf("ERROR: Can't handle compression.  Exiting!\n");
    printf("\n");
    process_exit(1);
  }

printf("%s %d >> biWidth = %d\n", __FILE__, __LINE__, *((int*)bmih->biWidth));

  width_ptr = (int*)bmih->biWidth;
  height_ptr = (int*)bmih->biHeight;
  bitcount_ptr = (short int*)bmih->biBitCount;

  // Read the palette, if necessary.
  if( ENDIAN2(*bitcount_ptr) < 24)
  {
    n = (int)pow(2.,(double)ENDIAN2(*bitcount_ptr)); // Num palette entries.
    for( i=0; i<n; i++)
    {
      k = fread( &rgb, sizeof(struct rgb_quad), 1, out );
      if( k!=1)
      {
        printf("Error reading palette entry %d.  Exiting!\n", i);
        process_exit(1);
      }
    }
  }

  // Bytes per row of the bitmap.
  bytes_per_row =
    ((int)ceil(( (((double)(ENDIAN4(*width_ptr)))*((double)(ENDIAN2(*bitcount_ptr))))/8.)));

  // Bitmaps pad rows to preserve 4-byte boundaries.
  // The length of a row in the file will be bytes_per_row + pad .
  pad = ((4) - bytes_per_row%4)%4;
#endif

} /* void bmp_write_header( FILE *out) */
                                                                        // }}}
// B M P   W R I T E   E N T R Y {{{
//##############################################################################
//void bmp_write_entry( FILE *in, char *r, char *g, char *b)
//
// Writes an RGB value to file on 'out' with bmp info header 'bmih'.
// The RGB values are passed in the 'r', 'g', and 'b' char arguments.
//
//
void bmp_write_entry(
  FILE *out,
  bmp_hdr_ptr bmp_hdr,
  int n,
  char r, char g, char b)
{
  int bytes_per_row;
  int pad;
  int k;
  char z;
  int i;

  // Bytes per row of the bitmap.
  bytes_per_row =
    ((int)ceil((( ((double)*(bmp_hdr->width))
                * ((double)*(bmp_hdr->depth)) )/8.)));

  // Bitmaps pad rows to preserve 4-byte boundaries.
  // The length of a row in the file will be bytes_per_row + pad .
  pad = ((4) - bytes_per_row%4)%4;


  k = fwrite( &b, 1, 1, out);
  printf("k %d, b %d\n", k, (int)b);
  k = fwrite( &g, 1, 1, out);
  printf("k %d, g %d\n", k, (int)g);
  k = fwrite( &r, 1, 1, out);
  printf("k %d, r %d\n", k, (int)r);

  // If end of row, add padding.
  if( (n+1)%(*(bmp_hdr->width)) == 0)
  {
    z = (char)0;
    for( i=1; i<=pad; i++)
    {
      k = fwrite( &z, 1, 1, out);
      printf("i %d, k %d, z %d\n", i, k, (int)z);
    }
  }

#if 0
  char   filename[1024];
  int    i, j, m;
  static int n=0;
  int    ei, ej;
  int    pad, bytes_per_row;
  char   k, p;
  struct bitmap_file_header bmfh;
  struct rgb_quad rgb;
  int    *int_ptr;
  short  int *short_int_ptr;
  int    *width_ptr;
  int    *height_ptr;
  short  int *bitcount_ptr;
  int    subs;


  width_ptr = (int*)bmih->biWidth;
  height_ptr = (int*)bmih->biHeight;
  bitcount_ptr = (short int*)bmih->biBitCount;

  // Bytes per row of the bitmap.
  bytes_per_row =
    ((int)ceil(( (((double)(ENDIAN4(*width_ptr)))*((double)(ENDIAN2(*bitcount_ptr))))/8.)));

  // Bitmaps pad rows to preserve 4-byte boundaries.
  // The length of a row in the file will be bytes_per_row + pad .
  pad = ((4) - bytes_per_row%4)%4;

  switch(ENDIAN2(*bitcount_ptr))
  {
    case 1: // Monochrome.
      printf("write_bcs() -- "
          "Support for Monochrome BMPs is pending.  "
          "Exiting!\n");
      process_exit(1);
    case 4: // 16 colors.
      printf("write_bcs() -- "
          "Support for 16 color BMPs is pending.  "
          "Exiting!\n");
      process_exit(1);
    case 8: // 256 colors.
      printf("write_bcs() -- "
          "Support for 256 color BMPs is pending.  "
          "Exiting!\n");
      process_exit(1);
    case 24: // 24-bit colors.
    if( !feof(out)) { n+=( k = fread( b, 1, 1, out ));}
    if( !feof(out)) { n+=( k = fread( g, 1, 1, out ));}
    if( !feof(out)) { n+=( k = fread( r, 1, 1, out ));}
      break;

    default: // 32-bit colors?
      printf("ERROR: Unhandled color depth, "
          "BitCount = %d. Exiting!\n", ENDIAN2(*bitcount_ptr));
      process_exit(1);
      break;

  } /* switch(*(bmih->biBitCount)) */

  if( (n%(bytes_per_row+pad)) == 3*(*width_ptr))
  {
    // Read pad bytes first.
    for( i=1; i<=pad; i++)
    {
      if( !feof(out)) { n+=( k = fread( &p, 1, 1, out ));}
    }
  }

  if( feof(out))
  {
    printf(
      "bmp_write_entry() -- ERROR:"
      "Attempt to write past the end of file. "
      "Exiting!\n");
    process_exit(1);
  }

  // If all the entries have been write, reset the counter.
  if( n == (bytes_per_row+pad)*(*height_ptr) )
  {
    n = 0;
  }
#endif

} /* void bmp_write_entry( FILE *out, char *r, char *g, char *b) */
                                                                        // }}}

// report_open {{{
void report_open( report_ptr report, char *name)
{
  sprintf( report->name, "%s.txt", name);

#if VERBOSITY_LEVEL >=1
  printf( "\n");
  printf( "%s\n", HRULE1);
  printf( "  R E P O R T\n");
  printf( "%s\n", HRULE1);
  printf( "\n");
#endif /* VERBOSITY_LEVEL > 1 */

  if( !( report->file = fopen( report->name, "w+")))
  {
    printf("%s %d: ERROR: fopen( %s, \"w+\") = %x\n",
      __FILE__, __LINE__, report->name, (report->file));
  }
  else
  {
    fprintf( report->file, "\n");
    fprintf( report->file, "%s\n", HRULE1);
    fprintf( report->file, "  R E P O R T\n");
    fprintf( report->file, "%s\n", HRULE1);
    fprintf( report->file, "\n");
  }

} /* void report_open( report_ptr report, char *name) */
                                                                        // }}}
// report_close {{{
void report_close( report_ptr report)
{
#if VERBOSITY_LEVEL >=1
  printf( "%s\n", HRULE1);
  printf( "\n");
#endif /* VERBOSITY_LEVEL >=1 */

  if( report->file)
  {
    //fprintf( report->file, "");
    fprintf( report->file, "\n");

    fclose( report->file);

#if VERBOSITY_LEVEL >=1
    printf( "See file \"%s\".\n", report->name);
#endif /* VERBOSITY_LEVEL >=1 */

  } /* if( report->file) */

} /* void report_close( report_ptr report) */
                                                                        // }}}
// report_entry {{{
void report_entry( report_ptr report, char *entry_left, char *entry_right)
{
  char dots[80];
  const int left_col_width = 50;
  int  n;

  // Fill with dots between columns.  Careful if length of left
  // entry is too long.
  if( left_col_width > strlen(entry_left) + 2)
  {
    dots[ 0] = ' ';
    for( n=1; n<left_col_width-strlen(entry_left); n++)
    {
      dots[n]='.';
    }
    dots[ n  ] = ' ';
    dots[ n+1] = '\x0';
  }
  else if( left_col_width > strlen(entry_left))
  {
    dots[ 0] = ' ';
    dots[ left_col_width-strlen(entry_left)-1] = ' ';
    dots[ left_col_width-strlen(entry_left)  ] = ' ';
    dots[ left_col_width-strlen(entry_left)+1] = '\x0';
  }
  else
  {
    dots[0] = ' ';
    dots[1] = '\x0';
  }

#if VERBOSITY_LEVEL >=1
  printf("  %s%s%s\n", entry_left, dots, entry_right);
  //printf("\n");
#endif /* VERBOSITY_LEVEL >=1 */

  if( report->file)
  {
    fprintf( report->file, "  %s%s%s\n", entry_left, dots, entry_right);
    //fprintf( report->file, "\n");
  }

} /* void report_entry( char *entry_left, char *entry_right) */
                                                                        // }}}
// report_integer_entry {{{
void report_integer_entry( report_ptr report, char *label, int value, char *units)
{
  char entry[1024];

  sprintf( entry, "%d %s", value, units);

  report_entry( report, label, entry);

} /* void report_integer_entry( char *label, int value, char *units) */
                                                                        // }}}
// report_ratio_entry {{{
void report_ratio_entry(
       report_ptr report, char *label, double num, double den, char *units)
{
  char entry[1024];
  if( den!=0)
  {
    sprintf( entry, "%f %s", num/den, units);
  }
  else
  {
    sprintf( entry, "UNDEF %s", units);
  }

  report_entry( report, label, entry);

} /* void report_integer_entry( char *label, int value, char *units) */
                                                                        // }}}
// report_partition {{{
void report_partition( report_ptr report)
{
  printf( "%s\n", HRULE0);
  printf( "\n");

  if( report->file)
  {
    fprintf( report->file, "%s\n", HRULE0);
    fprintf( report->file, "\n");
  }

} /* void report_partition( report_ptr report) */
                                                                        // }}}


//##############################################################################
// vim: foldmethod=marker:foldlevel=0
