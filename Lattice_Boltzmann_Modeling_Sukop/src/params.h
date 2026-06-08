//##############################################################################
//
// Copyright (C), 2005, Michael Sukop and Danny Thorne
//
// params.h
//
//  - Routines for reading and writing params.
//
//  - Reads from params.in
//
//  - Writes to params.dat
//

// void assign_default_param_vals( lattice_ptr lattice)
void assign_default_param_vals( lattice_ptr lattice)
{
#if 1
  lattice->param.LX = 0;
  lattice->param.LY = 0;
//LBMPI #if PARALLEL
//LBMPI   lattice->lbmpi->NPX = 0;
//LBMPI   lattice->lbmpi->NPY = 0;
//LBMPI #endif /* PARALLEL */
  lattice->param.length_scale = 0;
  lattice->param.NumFrames = 0;
  lattice->param.FrameRate = 0;
  lattice->param.tau[0] = 0.;
  lattice->param.gval[0][0] = 0.;
  lattice->param.gval[0][1] = 0.;
  lattice->param.end_grav[0] = 0;
#if NUM_FLUID_COMPONENTS==2
  lattice->param.tau[1] = 0.;
  lattice->param.gval[1][0] = 0.;
  lattice->param.gval[1][1] = 0.;
  lattice->param.end_grav[1] = 0;
#endif /* NUM_FLUID_COMPONENTS==2 */
  lattice->param.buoyancy = 0;
  lattice->param.buoy_subs = 0;
  lattice->param.incompressible = 0;
  lattice->param.simple_diffusion = 0;
  lattice->param.rho_A[0] = 0.;
  lattice->param.rho_B[0] = 0.;
#if SOURCE_ON
  lattice->param.source_strength = 0.;
#endif /*SOURCE_ON*/
#if SINK_ON
  lattice->param.sink_strength = 0.;
#endif /*SINK_ON*/
#if INAMURO_SIGMA_COMPONENT
  lattice->param.rho_sigma = 0.;
  lattice->param.beta = 0.;
  lattice->param.C0 = 0.;
  lattice->param.rho0 = 0.;
  lattice->param.drhodC = 0.;
  lattice->param.rho_sigma_in = 0.;
  lattice->param.C_in = 0.;
  lattice->param.rho_sigma_out = 0.;
  lattice->param.C_out = 0.;
  lattice->param.u_sigma = 0.;
  lattice->param.u_sigma_in = 0.;
  lattice->param.u_sigma_out = 0.;
#if SIGMA_BULK_FLAG
  lattice->param.sigma_bulk_on = 0;
#endif /*SIGMA_BULK_FLAG*/
  lattice->param.sigma_start = 0;
  lattice->param.sigma_stop = 0;
  lattice->param.sigma_btc_rate = 0;
  lattice->param.sigma_btc_spot = 0;
#endif /* INAMURO_SIGMA_COMPONENT */
  lattice->param.rho_in = 0.;
  lattice->param.rho_out = 0.;
  lattice->param.ux_in = 0.;
  lattice->param.ux_out = 0.;
  lattice->param.uy_in = 0.;
  lattice->param.uy_out = 0.;
  lattice->param.G = 0.;
  lattice->param.Gads[0] = 0.;
#if NUM_FLUID_COMPONENTS==2
  lattice->param.Gads[1] = 0.;
#endif /* NUM_FLUID_COMPONENTS==2 */
  lattice->param.ns_flag = 0;
  lattice->param.ns = 0.;
  lattice->param.slice_x = 0;
  lattice->param.slice_y = 0;
  lattice->param.ic_poiseuille = 0;
  lattice->param.bc_poiseuille = 0;
  lattice->param.bc_slip_north = 0;
#if INAMURO_SIGMA_COMPONENT
  lattice->param.bc_sigma_slip = 0;
  lattice->param.bc_sigma_walls = 0;
#if TAU_ZHANG_ANISOTROPIC_DISPERSION
  lattice->param.Dl = 1.;
  lattice->param.Dt = 1.;
#endif
#endif /* INAMURO_SIGMA_COMPONENT */
  lattice->param.pressure_n_in[0] = 0;
  lattice->param.pressure_s_in[0] = 0;
  lattice->param.pressure_n_out[0] = 0;
  lattice->param.pressure_s_out[0] = 0;
  lattice->param.velocity_n_in[0] = 0;
  lattice->param.velocity_s_in[0] = 0;
  lattice->param.velocity_n_out[0] = 0;
  lattice->param.velocity_s_out[0] = 0;
  lattice->param.pressure_e_in[0] = 0;
  lattice->param.pressure_w_in[0] = 0;
  lattice->param.pressure_e_out[0] = 0;
  lattice->param.pressure_w_out[0] = 0;
  lattice->param.velocity_e_in[0] = 0;
  lattice->param.velocity_w_in[0] = 0;
  lattice->param.velocity_e_out[0] = 0;
  lattice->param.velocity_w_out[0] = 0;
#if NUM_FLUID_COMPONENTS==2
  lattice->param.pressure_n_in[1] = 0;
  lattice->param.pressure_s_in[1] = 0;
  lattice->param.pressure_n_out[1] = 0;
  lattice->param.pressure_s_out[1] = 0;
  lattice->param.velocity_n_in[1] = 0;
  lattice->param.velocity_s_in[1] = 0;
  lattice->param.velocity_n_out[1] = 0;
  lattice->param.velocity_s_out[1] = 0;
  lattice->param.pressure_e_in[1] = 0;
  lattice->param.pressure_w_in[1] = 0;
  lattice->param.pressure_e_out[1] = 0;
  lattice->param.pressure_w_out[1] = 0;
  lattice->param.velocity_e_in[1] = 0;
  lattice->param.velocity_w_in[1] = 0;
  lattice->param.velocity_e_out[1] = 0;
  lattice->param.velocity_w_out[1] = 0;
  lattice->param.constcon_n_in = 0;
  lattice->param.constcon_s_in = 0;
  lattice->param.constcon_n_out = 0;
  lattice->param.constcon_s_out = 0;
  lattice->param.constflx_n_in = 0;
  lattice->param.constflx_s_in = 0;
  lattice->param.constflx_n_out = 0;
  lattice->param.constflx_s_out = 0;
  lattice->param.constcon_e_in = 0;
  lattice->param.constcon_w_in = 0;
  lattice->param.constcon_e_out = 0;
  lattice->param.constcon_w_out = 0;
  lattice->param.constflx_e_in = 0;
  lattice->param.constflx_w_in = 0;
  lattice->param.constflx_e_out = 0;
  lattice->param.constflx_w_out = 0;
  lattice->param.zeroconcgrad_n = 0;
  lattice->param.zeroconcgrad_s = 0;
  lattice->param.zeroconcgrad_e = 0;
  lattice->param.zeroconcgrad_w = 0;
  lattice->param.zeroconcgrad_full = 0;
#endif /* NUM_FLUID_COMPONENTS==2 */
  lattice->param.plot_scale_dynamic = 0;
  lattice->param.use_colormap = 0;
  lattice->param.initial_condition = 0;
  lattice->param.x0 = 0.;
  lattice->param.y0 = 0.;
  lattice->param.r0 = 0.;
  lattice->param.cut = 0.;
  lattice->param.x1 = 0;
  lattice->param.x2 = 0;
  lattice->param.y1 = 0;
  lattice->param.y2 = 0;
  lattice->param.rel_x1 = 0.;
  lattice->param.rel_x2 = 0.;
  lattice->param.rel_y1 = 0.;
  lattice->param.rel_y2 = 0.;
  lattice->param.dump_rho = 0;
  lattice->param.dump_u = 0;
  lattice->param.dump_force = 0;
  lattice->param.dump_vor = 0;
  lattice->param.do_user_stuff = 0;
  lattice->param.make_octave_scripts = 0;
  strncpy(lattice->param.out_path,"./out",1024);
#endif
} /* void assign_default_param_vals( lattice_ptr lattice) */

// void skip_label( FILE *in)
//##############################################################################
//
// S K I P   L A B E L
//
void skip_label( FILE *in)
{
  char c;

  c = fgetc( in);

  if( ( c >= '0' && c <='9') || c == '-' || c == '.')
  {
    // Digit or Sign or Decimal
    ungetc( c, in);
  }
  else
  {
    while( ( c = fgetc( in)) != ' ');
    while( ( c = fgetc( in)) == ' ');
    ungetc( c, in);
  }

} /* void skip_label( FILE *in) */

void get_rest_of_line_as_string( FILE *in, char **str, int maxstrlen)
{
  int n;
  char c;

  n = 0;
  c = fgetc(in);
  while( n< maxstrlen && c!='\n')
  {
    printf("%c",c);
    (*str)[n] = c;
    n++;
    c = fgetc(in);
  }
  printf("%c",c);
}

// void skip_rest_of_line( FILE *in)
//##############################################################################
//
// S K I P   R E S T   O F   L I N E
//
void skip_rest_of_line( FILE *in)
{
  char c;

  c = fgetc( in);

  while( ( c = fgetc( in)) != '\n');

} /* void skip_rest_of_line( FILE *in) */

// void read_param_label( const char *infile, char *param_label, int max_length)
//##############################################################################
//
// R E A D   P A R A M   L A B E L
//
//  - Read parameter label from parameter input file.
//
//  - Returns non-zero if label successfully read.
//
//  - Returns zero if end of file.
//
//  - Aborts with error message otherwise.
//
int read_param_label( FILE *in, char *param_label, int max_length)
{
  char c;
  int  i;

  if( max_length<=0)
  {
    printf("%s %d >> ERROR: max_length<=0 .\n",__FILE__,__LINE__);
    process_exit(1);
  }

  c = fgetc( in);

  if( feof(in)) { return 0;}

  if( ( c >= '0' && c <='9') || c == '-' || c == '.')
  {
    // Digit or Sign or Decimal. Labels cannot begin with these.
    // TODO: Check for other illegal initial characters.
    printf("%s %d >> Error reading label from parameters input file. "
        "Exiting!\n",__FILE__,__LINE__);
    fclose(in);
    process_exit(1);

  } /* if( ( c >= '0' && c <='9') || c == '-' || c == '.') */

  else
  {
    i = 0;
    param_label[i] = c;

    // Read label consisting of a string of non-whitespace characters.
    while( ( c = fgetc( in)) != ' ')
    {
      if( feof(in)) { return 0;}

      i++;
      if( i+1 > max_length)
      {
        printf("%s %d >> ERROR: i+1 > max_length .\n",__FILE__,__LINE__);
        process_exit(1);
      }
      param_label[i] = c;
    }

    // Terminate the string.
    i++;
    param_label[i] = '\x0';

    // Discard whitespace between label and its value. Calling routine
    // will read the value.
    while( ( c = fgetc( in)) == ' ')
    {
      if( feof(in)) { return 0;}
    }
    ungetc( c, in);

    //printf("%s %d >> Read label \"%s\"\n",__FILE__,__LINE__,param_label);

    return 1;

  } /* if( ( c >= '0' && c <='9') || c == '-' || c == '.') else */

} /* void read_param_label( FILE *in, char *param_label, int ... ) */

// void read_params( struct lattice_struct *lattice)
//##############################################################################
//
// R E A D   P A R A M S
//
//  - Read the problem parameters from a file.
//
void read_params( lattice_ptr lattice, const char *infile)
{
  FILE   *in;
  int    blank;
  double dblank;
  int    subs;
  char   param_label[81];

  if( !( in = fopen( infile, "r")))
  {
    printf("ERROR: fopen(\"%s\",\"r\") = NULL.  Bye, bye!\n", infile);
    process_exit(1);
  }

#if NEW_PARAMS_INPUT_ROUTINE

  // First assign default values to the parameters since this new mechanism
  // doesn't require all parameters to be specified.
  assign_default_param_vals( lattice);

  while( read_param_label( in, param_label, 80))
  {
#if 1
    printf("param_label = \"%s\"\n", param_label);
#endif
    if( !strncmp(param_label,"LX",80))
    {
      fscanf( in, "%d\n", &(lattice->param.LX));
      printf("%s %d >> LX = %d\n",__FILE__,__LINE__, lattice->param.LX);
    }
    else if( !strncmp(param_label,"LY",80))
    {
      fscanf( in, "%d\n", &(lattice->param.LY));
      printf("%s %d >> LY = %d\n",__FILE__,__LINE__, lattice->param.LY);
    }
    else if( !strncmp(param_label,"NPX",80))
    {
//LBMPI #if PARALLEL
//LBMPI       fscanf( in, "%d\n", &(lattice->lbmpi->NPX));
//LBMPI       printf("%s %d >> NPX = %d\n",__FILE__,__LINE__, lattice->lbmpi->NPX);
//LBMPI #else /* !(PARALLEL) */
//LBMPI       fscanf( in, "%d\n", &(blank));
//LBMPI       printf("%s %d >> NPX = %d // unused: not PARALELL\n",
//LBMPI         __FILE__,__LINE__, blank);
//LBMPI #endif /* (PARALLEL) */
    }
    else if( !strncmp(param_label,"NPY",80))
    {
//LBMPI #if PARALLEL
//LBMPI       fscanf( in, "%d\n", &(lattice->lbmpi->NPY));
//LBMPI       printf("%s %d >> NPY = %d\n",__FILE__,__LINE__, lattice->lbmpi->NPY);
//LBMPI #else /* !(PARALLEL) */
//LBMPI       fscanf( in, "%d\n", &(blank));
//LBMPI       printf("%s %d >> NPY = %d // unused: not PARALELL\n",
//LBMPI         __FILE__,__LINE__, blank);
//LBMPI #endif /* (PARALLEL) */
    }
    else if( !strncmp(param_label,"characteristic_length",80))
    {
      fscanf( in, "%d\n", &(lattice->param.length_scale));
      printf("%s %d >> characteristic_length = %d\n",__FILE__,__LINE__,
         lattice->param.length_scale);
    }
    else if( !strncmp(param_label,"NumFrames",80))
    {
      fscanf( in, "%d\n", &(lattice->param.NumFrames));
      printf("%s %d >> NumFrames = %d\n",__FILE__,__LINE__,
         lattice->param.NumFrames);
    }
    else if( !strncmp(param_label,"FrameRate",80))
    {
      fscanf( in, "%d\n", &(lattice->param.FrameRate));
      printf("%s %d >> FrameRate = %d\n",__FILE__,__LINE__,
         lattice->param.FrameRate);
    }
    else if( !strncmp(param_label,"tau[0]",80))
    {
      fscanf( in, "%lf\n", &(lattice->param.tau[0]));
      printf("%s %d >> tau[0] = %f\n",__FILE__,__LINE__,
         lattice->param.tau[0]);
    }
    else if( !strncmp(param_label,"gval_x[0]",80))
    {
      fscanf( in, "%lf\n", &(lattice->param.gval[0][0]));
      printf("%s %d >> gval_x[0] = %f\n",__FILE__,__LINE__,
         lattice->param.gval[0][0]);
    }
    else if( !strncmp(param_label,"gval_y[0]",80))
    {
      fscanf( in, "%lf\n", &(lattice->param.gval[0][1]));
      printf("%s %d >> gval_y[0] = %f\n",__FILE__,__LINE__,
         lattice->param.gval[0][1]);
    }
    else if( !strncmp(param_label,"end_grav[0]",80))
    {
      fscanf( in, "%d\n", &(lattice->param.end_grav[0]));
      printf("%s %d >> end_grav[0] = %d\n",__FILE__,__LINE__,
         lattice->param.end_grav[0]);
    }
    else if( !strncmp(param_label,"tau[1]",80))
    {
      if( NUM_FLUID_COMPONENTS==2)
      {
        fscanf( in, "%lf\n", &(lattice->param.tau[1]));
        printf("%s %d >> tau[1] = %f\n",__FILE__,__LINE__,
           lattice->param.tau[1]);
      }
      else
      {
        fscanf( in, "%lf\n", &(dblank));
        printf("%s %d >> tau[1] = %f // unused: NUM_FLUID_COMPONENTS==1\n",__FILE__,__LINE__, dblank);
      }
    }
    else if( !strncmp(param_label,"gval_x[1]",80))
    {
      if( NUM_FLUID_COMPONENTS==2)
      {
        fscanf( in, "%lf\n", &(lattice->param.gval[1][0]));
        printf("%s %d >> gval_x[1] = %f\n",__FILE__,__LINE__,
           lattice->param.gval[1][0]);
      }
      else
      {
        fscanf( in, "%lf\n", &(dblank));
        printf("%s %d >> gval_y[0] = %f // unused: NUM_FLUID_COMPONENTS==1\n",__FILE__,__LINE__, dblank);
      }
    }
    else if( !strncmp(param_label,"gval_y[1]",80))
    {
      if( NUM_FLUID_COMPONENTS==2)
      {
        fscanf( in, "%lf\n", &(lattice->param.gval[1][1]));
        printf("%s %d >> gval_y[1] = %f\n",__FILE__,__LINE__,
           lattice->param.gval[1][1]);
      }
      else
      {
        fscanf( in, "%lf\n", &(dblank));
        printf("%s %d >> gval_y[1] = %f // unused: NUM_FLUID_COMPONENTS==1\n",__FILE__,__LINE__, dblank);
      }
    }
    else if( !strncmp(param_label,"end_grav[1]",80))
    {
      if( NUM_FLUID_COMPONENTS==2)
      {
        fscanf( in, "%d\n", &(lattice->param.end_grav[1]));
        printf("%s %d >> end_grav[1] = %d\n",__FILE__,__LINE__,
           lattice->param.end_grav[1]);
      }
      else
      {
        fscanf( in, "%d\n", &(blank));
        printf("%s %d >> end_grav[1] = %d // unused: NUM_FLUID_COMPONENTS==1\n",__FILE__,__LINE__, blank);
      }
    }
    else if( !strncmp(param_label,"buoyancy",80))
    {
      fscanf( in, "%d\n", &(lattice->param.buoyancy));
      printf("%s %d >> buoyancy = %d\n",__FILE__,__LINE__,
         lattice->param.buoyancy);
    }
    else if( !strncmp(param_label,"buoy_subs",80))
    {
      fscanf( in, "%d\n", &(lattice->param.buoy_subs));
      printf("%s %d >> buoy_subs = %d\n",__FILE__,__LINE__,
         lattice->param.buoy_subs);
    }
    else if( !strncmp(param_label,"incompressible",80))
    {
      fscanf( in, "%d\n", &(lattice->param.incompressible));
      printf("%s %d >> incompressible = %d\n",__FILE__,__LINE__,
         lattice->param.incompressible);
    }
    else if( !strncmp(param_label,"simple_diffusion",80))
    {
      fscanf( in, "%d\n", &(lattice->param.simple_diffusion));
      printf("%s %d >> simple_diffusion = %d\n",__FILE__,__LINE__,
         lattice->param.simple_diffusion);
    }
    else if( !strncmp(param_label,"rho_A",80))
    {
      fscanf( in, "%lf\n", &(lattice->param.rho_A[0]));
      printf("%s %d >> rho_A = %f\n",__FILE__,__LINE__,
         lattice->param.rho_A[0]);
    }
    else if( !strncmp(param_label,"rho_B",80))
    {
      fscanf( in, "%lf\n", &(lattice->param.rho_B[0]));
      printf("%s %d >> rho_B = %f\n",__FILE__,__LINE__,
         lattice->param.rho_B[0]);
    }
    else if( !strncmp(param_label,"source_strength",80))
	{
#if SOURCE_ON
      fscanf( in, "%lf\n", &(lattice->param.source_strength));
      printf("%s %d >> source_strength = %f\n",__FILE__,__LINE__,
         lattice->param.source_strength);
#else /* !(SOURCE_ON) */
      fscanf( in, "%lf\n", &(dblank));
      printf("%s %d >> source_strength = %f "
        "// unused: not SOURCE_ON\n",
        __FILE__,__LINE__, dblank);
#endif /* (SOURCE_ON) */
	}
    else if( !strncmp(param_label,"sink_strength",80))
	{
#if SINK_ON
      fscanf( in, "%lf\n", &(lattice->param.sink_strength));
      printf("%s %d >> sink_strength = %f\n",__FILE__,__LINE__,
         lattice->param.sink_strength);
#else /* !(SINK_ON) */
      fscanf( in, "%lf\n", &(dblank));
      printf("%s %d >> sink_strength = %f "
        "// unused: not SINK_ON\n",
        __FILE__,__LINE__, dblank);
#endif /* (SINK_ON) */
	}
    else if( !strncmp(param_label,"rho_sigma",80))
    {
#if INAMURO_SIGMA_COMPONENT
      fscanf( in, "%lf\n", &(lattice->param.rho_sigma));
      printf("%s %d >> rho_sigma = %f\n",__FILE__,__LINE__,
         lattice->param.rho_sigma);
#else /* !(INAMURO_SIGMA_COMPONENT) */
      fscanf( in, "%lf\n", &(dblank));
      printf("%s %d >> rho_sigma = %f "
        "// unused: not INAMURO_SIGMA_COMPONENT\n",
        __FILE__,__LINE__, dblank);
#endif /* (INAMURO_SIGMA_COMPONENT) */
    }
    else if( !strncmp(param_label,"beta",80))
    {
#if INAMURO_SIGMA_COMPONENT
      fscanf( in, "%lf\n", &(lattice->param.beta));
      printf("%s %d >> beta = %f\n",__FILE__,__LINE__,
         lattice->param.beta);
#else /* !(INAMURO_SIGMA_COMPONENT) */
      fscanf( in, "%lf\n", &(dblank));
      printf("%s %d >> beta = %f "
        "// unused: not INAMURO_SIGMA_COMPONENT\n",
        __FILE__,__LINE__, dblank);
#endif /* (INAMURO_SIGMA_COMPONENT) */
    }
    else if( !strncmp(param_label,"C0",80))
    {
#if INAMURO_SIGMA_COMPONENT
      fscanf( in, "%lf\n", &(lattice->param.C0));
      printf("%s %d >> C0 = %f\n",__FILE__,__LINE__,
         lattice->param.C0);
#else /* !(INAMURO_SIGMA_COMPONENT) */
      fscanf( in, "%lf\n", &(dblank));
      printf("%s %d >> C0 = %f "
        "// unused: not INAMURO_SIGMA_COMPONENT\n",
        __FILE__,__LINE__, dblank);
#endif /* (INAMURO_SIGMA_COMPONENT) */
    }
    else if( !strncmp(param_label,"rho0",80))
    {
#if INAMURO_SIGMA_COMPONENT
      fscanf( in, "%lf\n", &(lattice->param.rho0));
      printf("%s %d >> rho0 = %f\n",__FILE__,__LINE__,
         lattice->param.rho0);
#else /* !(INAMURO_SIGMA_COMPONENT) */
      fscanf( in, "%lf\n", &(dblank));
      printf("%s %d >> rho0 = %f "
        "// unused: not INAMURO_SIGMA_COMPONENT\n",
        __FILE__,__LINE__, dblank);
#endif /* (INAMURO_SIGMA_COMPONENT) */
    }
    else if( !strncmp(param_label,"drhodC",80))
    {
#if INAMURO_SIGMA_COMPONENT
      fscanf( in, "%lf\n", &(lattice->param.drhodC));
      printf("%s %d >> drhodC = %f\n",__FILE__,__LINE__,
         lattice->param.drhodC);
#else /* !(INAMURO_SIGMA_COMPONENT) */
      fscanf( in, "%lf\n", &(dblank));
      printf("%s %d >> drhodC = %f "
        "// unused: not INAMURO_SIGMA_COMPONENT\n",
        __FILE__,__LINE__, dblank);
#endif /* (INAMURO_SIGMA_COMPONENT) */
    }
    else if( !strncmp(param_label,"rho_sigma_in",80))
    {
#if INAMURO_SIGMA_COMPONENT
      fscanf( in, "%lf\n", &(lattice->param.rho_sigma_in));
      printf("%s %d >> rho_sigma_in = %f\n",__FILE__,__LINE__,
         lattice->param.rho_sigma_in);
#else /* !(INAMURO_SIGMA_COMPONENT) */
      fscanf( in, "%lf\n", &(dblank));
      printf("%s %d >> rho_sigma_in = %f "
        "// unused: not INAMURO_SIGMA_COMPONENT\n",
        __FILE__,__LINE__, dblank);
#endif /* (INAMURO_SIGMA_COMPONENT) */
    }
    else if( !strncmp(param_label,"C_in",80))
    {
#if INAMURO_SIGMA_COMPONENT
      fscanf( in, "%lf\n", &(lattice->param.C_in));
      printf("%s %d >> C_in = %f\n",__FILE__,__LINE__,
         lattice->param.C_in);
#else /* !(INAMURO_SIGMA_COMPONENT) */
      fscanf( in, "%lf\n", &(dblank));
      printf("%s %d >> C_in = %f "
        "// unused: not INAMURO_SIGMA_COMPONENT\n",
        __FILE__,__LINE__, dblank);
#endif /* (INAMURO_SIGMA_COMPONENT) */
    }
    else if( !strncmp(param_label,"rho_sigma_out",80))
    {
#if INAMURO_SIGMA_COMPONENT
      fscanf( in, "%lf\n", &(lattice->param.rho_sigma_out));
      printf("%s %d >> rho_sigma_out = %f\n",__FILE__,__LINE__,
         lattice->param.rho_sigma_out);
#else /* !(INAMURO_SIGMA_COMPONENT) */
      fscanf( in, "%lf\n", &(dblank));
      printf("%s %d >> rho_sigma_out = %f "
        "// unused: not INAMURO_SIGMA_COMPONENT\n",
        __FILE__,__LINE__, dblank);
#endif /* (INAMURO_SIGMA_COMPONENT) */
    }
    else if( !strncmp(param_label,"C_out",80))
    {
#if INAMURO_SIGMA_COMPONENT
      fscanf( in, "%lf\n", &(lattice->param.C_out));
      printf("%s %d >> C_out = %f\n",__FILE__,__LINE__,
         lattice->param.C_out);
#else /* !(INAMURO_SIGMA_COMPONENT) */
      fscanf( in, "%lf\n", &(dblank));
      printf("%s %d >> C_out = %f "
        "// unused: not INAMURO_SIGMA_COMPONENT\n",
        __FILE__,__LINE__, dblank);
#endif /* (INAMURO_SIGMA_COMPONENT) */
    }
    else if( !strncmp(param_label,"u_sigma",80))
    {
#if INAMURO_SIGMA_COMPONENT
      fscanf( in, "%lf\n", &(lattice->param.u_sigma));
      printf("%s %d >> u_sigma = %f\n",__FILE__,__LINE__,
         lattice->param.u_sigma);
#else /* !(INAMURO_SIGMA_COMPONENT) */
      fscanf( in, "%lf\n", &(dblank));
      printf("%s %d >> u_sigma = %f "
        "// unused: not INAMURO_SIGMA_COMPONENT\n",
        __FILE__,__LINE__, dblank);
#endif /* (INAMURO_SIGMA_COMPONENT) */
    }
    else if( !strncmp(param_label,"u_sigma_in",80))
    {
#if INAMURO_SIGMA_COMPONENT
      fscanf( in, "%lf\n", &(lattice->param.u_sigma_in));
      printf("%s %d >> u_sigma_in = %f\n",__FILE__,__LINE__,
         lattice->param.u_sigma_in);
#else /* !(INAMURO_SIGMA_COMPONENT) */
      fscanf( in, "%lf\n", &(dblank));
      printf("%s %d >> u_sigma_in = %f "
        "// unused: not INAMURO_SIGMA_COMPONENT\n",
        __FILE__,__LINE__, dblank);
#endif /* (INAMURO_SIGMA_COMPONENT) */
    }
    else if( !strncmp(param_label,"u_sigma_out",80))
    {
#if INAMURO_SIGMA_COMPONENT
      fscanf( in, "%lf\n", &(lattice->param.u_sigma_out));
      printf("%s %d >> u_sigma_out = %f\n",__FILE__,__LINE__,
         lattice->param.u_sigma_out);
#else /* !(INAMURO_SIGMA_COMPONENT) */
      fscanf( in, "%lf\n", &(dblank));
      printf("%s %d >> u_sigma_out = %f "
        "// unused: not INAMURO_SIGMA_COMPONENT\n",
        __FILE__,__LINE__, dblank);
#endif /* (INAMURO_SIGMA_COMPONENT) */
    }
    else if( !strncmp(param_label,"sigma_bulk_on",80))
    {
#if SIGMA_BULK_FLAG
      fscanf( in, "%d\n", &(lattice->param.sigma_bulk_on));
      printf("%s %d >> sigma_bulk_on = %d\n",__FILE__,__LINE__,
         lattice->param.sigma_bulk_on);
#else /* !(SIGMA_BULK_FLAG) */
      fscanf( in, "%d\n", &(blank));
      printf("%s %d >> sigma_bulk_on = %d "
        "// unused: not SIGMA_BULK_FLAG\n",
        __FILE__,__LINE__, blank);
#endif /* (SIGMA_BULK_FLAG) */
    }
    else if( !strncmp(param_label,"sigma_start",80))
    {
#if INAMURO_SIGMA_COMPONENT
      fscanf( in, "%d\n", &(lattice->param.sigma_start));
      printf("%s %d >> sigma_start = %d\n",__FILE__,__LINE__,
         lattice->param.sigma_start);
#else /* !(INAMURO_SIGMA_COMPONENT) */
      fscanf( in, "%d\n", &(blank));
      printf("%s %d >> sigma_start = %d "
        "// unused: not INAMURO_SIGMA_COMPONENT\n",
        __FILE__,__LINE__, blank);
#endif /* (INAMURO_SIGMA_COMPONENT) */
    }
    else if( !strncmp(param_label,"sigma_stop",80))
    {
#if INAMURO_SIGMA_COMPONENT
      fscanf( in, "%d\n", &(lattice->param.sigma_stop));
      printf("%s %d >> sigma_stop = %d\n",__FILE__,__LINE__,
         lattice->param.sigma_stop);
#else /* !(INAMURO_SIGMA_COMPONENT) */
      fscanf( in, "%d\n", &(blank));
      printf("%s %d >> sigma_stop = %d "
        "// unused: not INAMURO_SIGMA_COMPONENT\n",
        __FILE__,__LINE__, blank);
#endif /* (INAMURO_SIGMA_COMPONENT) */
    }
    else if( !strncmp(param_label,"sigma_btc_rate",80))
    {
#if INAMURO_SIGMA_COMPONENT
      fscanf( in, "%d\n", &(lattice->param.sigma_btc_rate));
      printf("%s %d >> sigma_btc_rate = %d\n",__FILE__,__LINE__,
         lattice->param.sigma_btc_rate);
#else /* !(INAMURO_SIGMA_COMPONENT) */
      fscanf( in, "%d\n", &(blank));
      printf("%s %d >> sigma_btc_rate = %d "
        "// unused: not INAMURO_SIGMA_COMPONENT\n",
        __FILE__,__LINE__, blank);
#endif /* (INAMURO_SIGMA_COMPONENT) */
    }
    else if( !strncmp(param_label,"sigma_btc_spot",80))
    {
#if INAMURO_SIGMA_COMPONENT
      fscanf( in, "%d\n", &(lattice->param.sigma_btc_spot));
      printf("%s %d >> sigma_btc_spot = %d\n",__FILE__,__LINE__,
         lattice->param.sigma_btc_spot);
#else /* !(INAMURO_SIGMA_COMPONENT) */
      fscanf( in, "%d\n", &(blank));
      printf("%s %d >> sigma_btc_spot = %d "
        "// unused: not INAMURO_SIGMA_COMPONENT\n",
        __FILE__,__LINE__, blank);
#endif /* (INAMURO_SIGMA_COMPONENT) */
    }
    else if( !strncmp(param_label,"rho_in",80))
    {
      fscanf( in, "%lf\n", &(lattice->param.rho_in));
      printf("%s %d >> rho_in = %f\n",__FILE__,__LINE__,
         lattice->param.rho_in);
    }
    else if( !strncmp(param_label,"rho_out",80))
    {
      fscanf( in, "%lf\n", &(lattice->param.rho_out));
      printf("%s %d >> rho_out = %f\n",__FILE__,__LINE__,
         lattice->param.rho_out);
    }
    else if( !strncmp(param_label,"ux_in",80))
    {
      fscanf( in, "%lf\n", &(lattice->param.ux_in));
      printf("%s %d >> ux_in = %f\n",__FILE__,__LINE__,
         lattice->param.ux_in);
    }
    else if( !strncmp(param_label,"ux_out",80))
    {
      fscanf( in, "%lf\n", &(lattice->param.ux_out));
      printf("%s %d >> ux_out = %f\n",__FILE__,__LINE__,
         lattice->param.ux_out);
    }
    else if( !strncmp(param_label,"uy_in",80))
    {
      fscanf( in, "%lf\n", &(lattice->param.uy_in));
      printf("%s %d >> uy_in = %f\n",__FILE__,__LINE__,
         lattice->param.uy_in);
    }
    else if( !strncmp(param_label,"uy_out",80))
    {
      fscanf( in, "%lf\n", &(lattice->param.uy_out));
      printf("%s %d >> uy_out = %f\n",__FILE__,__LINE__,
         lattice->param.uy_out);
    }
    else if( !strncmp(param_label,"G",80))
    {
      fscanf( in, "%lf\n", &(lattice->param.G));
      printf("%s %d >> G = %f\n",__FILE__,__LINE__,
         lattice->param.G);
    }
    else if( !strncmp(param_label,"Gads[0]",80))
    {
      fscanf( in, "%lf\n", &(lattice->param.Gads[0]));
      printf("%s %d >> Gads[0] = %f\n",__FILE__,__LINE__,
         lattice->param.Gads[0]);
    }
    else if( !strncmp(param_label,"Gads[1]",80))
    {
      if( NUM_FLUID_COMPONENTS==2)
      {
        fscanf( in, "%lf\n", &(lattice->param.Gads[1]));
        printf("%s %d >> Gads[1] = %f\n",__FILE__,__LINE__,
           lattice->param.Gads[1]);
      }
      else
      {
        fscanf( in, "%lf\n", &(dblank));
        printf("%s %d >> Gads[1] = %f // unused: NUM_FLUID_COMPONENTS==1\n",__FILE__,__LINE__, dblank);
      }
    }
    else if( !strncmp(param_label,"ns_flag",80))
    {
      fscanf( in, "%d\n", &(lattice->param.ns_flag));
      printf("%s %d >> ns_flag = %d\n",__FILE__,__LINE__,
        lattice->param.ns_flag);
    }
    else if( !strncmp(param_label,"ns",80))
    {
      fscanf( in, "%lf\n", &(lattice->param.ns));
      printf("%s %d >> ns = %f\n",__FILE__,__LINE__,
         lattice->param.ns);
    }
    else if( !strncmp(param_label,"slice_x",80))
    {
      fscanf( in, "%d\n", &(lattice->param.slice_x));
      printf("%s %d >> slice_x = %d\n",__FILE__,__LINE__,
        lattice->param.slice_x);
    }
    else if( !strncmp(param_label,"slice_y",80))
    {
      fscanf( in, "%d\n", &(lattice->param.slice_y));
      printf("%s %d >> slice_y = %d\n",__FILE__,__LINE__,
        lattice->param.slice_y);
    }
    else if( !strncmp(param_label,"ic_poiseuille",80))
    {
      fscanf( in, "%d\n", &(lattice->param.ic_poiseuille));
      printf("%s %d >> ic_poiseuille = %d\n",__FILE__,__LINE__,
        lattice->param.ic_poiseuille);
    }
    // Including mispelling for backward compatibility.
    else if( !strncmp(param_label,"ic_poisseuille",80))
    {
      fscanf( in, "%d\n", &(lattice->param.ic_poiseuille));
      printf("%s %d >> ic_poiseuille = %d\n",__FILE__,__LINE__,
        lattice->param.ic_poiseuille);
    }
    else if( !strncmp(param_label,"bc_poiseuille",80))
    {
      fscanf( in, "%d\n", &(lattice->param.bc_poiseuille));
      printf("%s %d >> bc_poiseuille = %d\n",__FILE__,__LINE__,
        lattice->param.bc_poiseuille);
    }
    // Including mispelling for backward compatibility.
    else if( !strncmp(param_label,"bc_poisseuille",80))
    {
      fscanf( in, "%d\n", &(lattice->param.bc_poiseuille));
      printf("%s %d >> bc_poiseuille = %d\n",__FILE__,__LINE__,
        lattice->param.bc_poiseuille);
    }
    else if( !strncmp(param_label,"bc_slip_north",80))
    {
      fscanf( in, "%d\n", &(lattice->param.bc_slip_north));
      printf("%s %d >> bc_slip_north = %d\n",__FILE__,__LINE__,
        lattice->param.bc_slip_north);
    }
    else if( !strncmp(param_label,"bc_sigma_slip",80))
    {
#if INAMURO_SIGMA_COMPONENT
      fscanf( in, "%d\n", &(lattice->param.bc_sigma_slip));
      printf("%s %d >> bc_sigma_slip = %d\n",__FILE__,__LINE__,
        lattice->param.bc_sigma_slip);
#else /* !(INAMURO_SIGMA_COMPONENT) */
      fscanf( in, "%d\n", &(blank));
      printf("%s %d >> bc_sigma_slip = %d "
        "// unused: not INAMURO_SIGMA_COMPONENT\n",
        __FILE__,__LINE__, blank);
#endif /* (INAMURO_SIGMA_COMPONENT) */
    }
    else if( !strncmp(param_label,"bc_sigma_walls",80))
    {
#if INAMURO_SIGMA_COMPONENT
      fscanf( in, "%d\n", &(lattice->param.bc_sigma_walls));
      printf("%s %d >> bc_sigma_walls = %d\n",__FILE__,__LINE__,
        lattice->param.bc_sigma_walls);
#else /* !(INAMURO_SIGMA_COMPONENT) */
      fscanf( in, "%d\n", &(blank));
      printf("%s %d >> bc_sigma_walls = %d "
        "// unused: not INAMURO_SIGMA_COMPONENT\n",
        __FILE__,__LINE__, blank);
#endif /* (INAMURO_SIGMA_COMPONENT) */
    }
    else if( !strncmp(param_label,"Dl",80))
    {
#if TAU_ZHANG_ANISOTROPIC_DISPERSION
      fscanf( in, "%f\n", &(lattice->param.Dl));
      printf("%s %d >> Dl = %f\n",__FILE__,__LINE__,
        lattice->param.Dl);
#else
      fscanf( in, "%lf\n", &(dblank));
      printf("%s %d >> Dl = %f "
        "// unused: not INAMURO_SIGMA_COMPONENT\n",
        __FILE__,__LINE__, dblank);
#endif
    }
    else if( !strncmp(param_label,"Dt",80))
    {
#if TAU_ZHANG_ANISOTROPIC_DISPERSION
      fscanf( in, "%f\n", &(lattice->param.Dt));
      printf("%s %d >> Dt = %f\n",__FILE__,__LINE__,
        lattice->param.Dt);
#else
      fscanf( in, "%lf\n", &(dblank));
      printf("%s %d >> Dt = %f "
        "// unused: not INAMURO_SIGMA_COMPONENT\n",
        __FILE__,__LINE__, dblank);
#endif
    }
    else if( !strncmp(param_label,"pressure_n_in[0]",80))
    {
      fscanf( in, "%d\n", &(lattice->param.pressure_n_in[0]));
      printf("%s %d >> pressure_n_in[0] = %d\n",__FILE__,__LINE__,
        lattice->param.pressure_n_in[0]);
    }
    else if( !strncmp(param_label,"pressure_s_in[0]",80))
    {
      fscanf( in, "%d\n", &(lattice->param.pressure_s_in[0]));
      printf("%s %d >> pressure_s_in[0] = %d\n",__FILE__,__LINE__,
        lattice->param.pressure_s_in[0]);
    }
    else if( !strncmp(param_label,"pressure_n_out[0]",80))
    {
      fscanf( in, "%d\n", &(lattice->param.pressure_n_out[0]));
      printf("%s %d >> pressure_n_out[0] = %d\n",__FILE__,__LINE__,
        lattice->param.pressure_n_out[0]);
    }
    else if( !strncmp(param_label,"pressure_s_out[0]",80))
    {
      fscanf( in, "%d\n", &(lattice->param.pressure_s_out[0]));
      printf("%s %d >> pressure_s_out[0] = %d\n",__FILE__,__LINE__,
        lattice->param.pressure_s_out[0]);
    }
    else if( !strncmp(param_label,"velocity_n_in[0]",80))
    {
      fscanf( in, "%d\n", &(lattice->param.velocity_n_in[0]));
      printf("%s %d >> velocity_n_in[0] = %d\n",__FILE__,__LINE__,
        lattice->param.velocity_n_in[0]);
    }
    else if( !strncmp(param_label,"velocity_s_in[0]",80))
    {
      fscanf( in, "%d\n", &(lattice->param.velocity_s_in[0]));
      printf("%s %d >> velocity_s_in[0] = %d\n",__FILE__,__LINE__,
        lattice->param.velocity_s_in[0]);
    }
    else if( !strncmp(param_label,"velocity_n_out[0]",80))
    {
      fscanf( in, "%d\n", &(lattice->param.velocity_n_out[0]));
      printf("%s %d >> velocity_n_out[0] = %d\n",__FILE__,__LINE__,
        lattice->param.velocity_n_out[0]);
    }
    else if( !strncmp(param_label,"velocity_s_out[0]",80))
    {
      fscanf( in, "%d\n", &(lattice->param.velocity_s_out[0]));
      printf("%s %d >> velocity_s_out[0] = %d\n",__FILE__,__LINE__,
        lattice->param.velocity_s_out[0]);
    }
    else if( !strncmp(param_label,"pressure_e_in[0]",80))
    {
      fscanf( in, "%d\n", &(lattice->param.pressure_e_in[0]));
      printf("%s %d >> pressure_e_in[0] = %d\n",__FILE__,__LINE__,
        lattice->param.pressure_e_in[0]);
    }
    else if( !strncmp(param_label,"pressure_w_in[0]",80))
    {
      fscanf( in, "%d\n", &(lattice->param.pressure_w_in[0]));
      printf("%s %d >> pressure_w_in[0] = %d\n",__FILE__,__LINE__,
        lattice->param.pressure_w_in[0]);
    }
    else if( !strncmp(param_label,"pressure_e_out[0]",80))
    {
      fscanf( in, "%d\n", &(lattice->param.pressure_e_out[0]));
      printf("%s %d >> pressure_e_out[0] = %d\n",__FILE__,__LINE__,
        lattice->param.pressure_e_out[0]);
    }
    else if( !strncmp(param_label,"pressure_w_out[0]",80))
    {
      fscanf( in, "%d\n", &(lattice->param.pressure_w_out[0]));
      printf("%s %d >> pressure_w_out[0] = %d\n",__FILE__,__LINE__,
        lattice->param.pressure_w_out[0]);
    }
    else if( !strncmp(param_label,"velocity_e_in[0]",80))
    {
      fscanf( in, "%d\n", &(lattice->param.velocity_e_in[0]));
      printf("%s %d >> velocity_e_in[0] = %d\n",__FILE__,__LINE__,
        lattice->param.velocity_e_in[0]);
    }
    else if( !strncmp(param_label,"velocity_w_in[0]",80))
    {
      fscanf( in, "%d\n", &(lattice->param.velocity_w_in[0]));
      printf("%s %d >> velocity_w_in[0] = %d\n",__FILE__,__LINE__,
        lattice->param.velocity_w_in[0]);
    }
    else if( !strncmp(param_label,"velocity_e_out[0]",80))
    {
      fscanf( in, "%d\n", &(lattice->param.velocity_e_out[0]));
      printf("%s %d >> velocity_e_out[0] = %d\n",__FILE__,__LINE__,
        lattice->param.velocity_e_out[0]);
    }
    else if( !strncmp(param_label,"velocity_w_out[0]",80))
    {
      fscanf( in, "%d\n", &(lattice->param.velocity_w_out[0]));
      printf("%s %d >> velocity_w_out[0] = %d\n",__FILE__,__LINE__,
        lattice->param.velocity_w_out[0]);
    }
    else if( !strncmp(param_label,"pressure_n_in[1]",80))
    {
      if( NUM_FLUID_COMPONENTS==2)
      {
        fscanf( in, "%d\n", &(lattice->param.pressure_n_in[1]));
        printf("%s %d >> pressure_n_in[1] = %d\n",__FILE__,__LINE__,
          lattice->param.pressure_n_in[1]);
      }
      else
      {
        fscanf( in, "%lf\n", &(dblank));
        printf("%s %d >> pressure_n_in[1] = %f // unused: NUM_FLUID_COMPONENTS==1\n",__FILE__,__LINE__, dblank);
      }
    }
    else if( !strncmp(param_label,"pressure_s_in[1]",80))
    {
      if( NUM_FLUID_COMPONENTS==2)
      {
        fscanf( in, "%d\n", &(lattice->param.pressure_s_in[1]));
        printf("%s %d >> pressure_s_in[1] = %d\n",__FILE__,__LINE__,
          lattice->param.pressure_s_in[1]);
      }
      else
      {
        fscanf( in, "%lf\n", &(dblank));
        printf("%s %d >> pressure_s_in[1] = %f // unused: NUM_FLUID_COMPONENTS==1\n",__FILE__,__LINE__, dblank);
      }
    }
    else if( !strncmp(param_label,"pressure_n_out[1]",80))
    {
      if( NUM_FLUID_COMPONENTS==2)
      {
        fscanf( in, "%d\n", &(lattice->param.pressure_n_out[1]));
        printf("%s %d >> pressure_n_out[1] = %d\n",__FILE__,__LINE__,
          lattice->param.pressure_n_out[1]);
      }
      else
      {
        fscanf( in, "%lf\n", &(dblank));
        printf("%s %d >> pressure_n_out[1] = %f // unused: NUM_FLUID_COMPONENTS==1\n",__FILE__,__LINE__, dblank);
      }
    }
    else if( !strncmp(param_label,"pressure_s_out[1]",80))
    {
      if( NUM_FLUID_COMPONENTS==2)
      {
        fscanf( in, "%d\n", &(lattice->param.pressure_s_out[1]));
        printf("%s %d >> pressure_s_out[1] = %d\n",__FILE__,__LINE__,
          lattice->param.pressure_s_out[1]);
      }
      else
      {
        fscanf( in, "%lf\n", &(dblank));
        printf("%s %d >> pressure_s_out[1] = %f // unused: NUM_FLUID_COMPONENTS==1\n",__FILE__,__LINE__, dblank);
      }
    }
    else if( !strncmp(param_label,"velocity_n_in[1]",80))
    {
      if( NUM_FLUID_COMPONENTS==2)
      {
        fscanf( in, "%d\n", &(lattice->param.velocity_n_in[1]));
        printf("%s %d >> velocity_n_in[1] = %d\n",__FILE__,__LINE__,
          lattice->param.velocity_n_in[1]);
      }
      else
      {
        fscanf( in, "%lf\n", &(dblank));
        printf("%s %d >> velocity_n_in[1] = %f // unused: NUM_FLUID_COMPONENTS==1\n",__FILE__,__LINE__, dblank);
      }
    }
    else if( !strncmp(param_label,"velocity_s_in[1]",80))
    {
      if( NUM_FLUID_COMPONENTS==2)
      {
        fscanf( in, "%d\n", &(lattice->param.velocity_s_in[1]));
        printf("%s %d >> velocity_s_in[1] = %d\n",__FILE__,__LINE__,
          lattice->param.velocity_s_in[1]);
      }
      else
      {
        fscanf( in, "%lf\n", &(dblank));
        printf("%s %d >> velocity_s_in[1] = %f // unused: NUM_FLUID_COMPONENTS==1\n",__FILE__,__LINE__, dblank);
      }
    }
    else if( !strncmp(param_label,"velocity_n_out[1]",80))
    {
      if( NUM_FLUID_COMPONENTS==2)
      {
        fscanf( in, "%d\n", &(lattice->param.velocity_n_out[1]));
        printf("%s %d >> velocity_n_out[1] = %d\n",__FILE__,__LINE__,
          lattice->param.velocity_n_out[1]);
      }
      else
      {
        fscanf( in, "%lf\n", &(dblank));
        printf("%s %d >> velocity_n_out[1] = %f // unused: NUM_FLUID_COMPONENTS==1\n",__FILE__,__LINE__, dblank);
      }
    }
    else if( !strncmp(param_label,"velocity_s_out[1]",80))
    {
      if( NUM_FLUID_COMPONENTS==2)
      {
        fscanf( in, "%d\n", &(lattice->param.velocity_s_out[1]));
        printf("%s %d >> velocity_s_out[1] = %d\n",__FILE__,__LINE__,
          lattice->param.velocity_s_out[1]);
      }
      else
      {
        fscanf( in, "%lf\n", &(dblank));
        printf("%s %d >> velocity_s_out[1] = %f // unused: NUM_FLUID_COMPONENTS==1\n",__FILE__,__LINE__, dblank);
      }
    }
    else if( !strncmp(param_label,"pressure_e_in[1]",80))
    {
      if( NUM_FLUID_COMPONENTS==2)
      {
        fscanf( in, "%d\n", &(lattice->param.pressure_e_in[1]));
        printf("%s %d >> pressure_e_in[1] = %d\n",__FILE__,__LINE__,
          lattice->param.pressure_e_in[1]);
      }
      else
      {
        fscanf( in, "%lf\n", &(dblank));
        printf("%s %d >> pressure_e_in[1] = %f // unused: NUM_FLUID_COMPONENTS==1\n",__FILE__,__LINE__, dblank);
      }
    }
    else if( !strncmp(param_label,"pressure_w_in[1]",80))
    {
      if( NUM_FLUID_COMPONENTS==2)
      {
        fscanf( in, "%d\n", &(lattice->param.pressure_w_in[1]));
        printf("%s %d >> pressure_w_in[1] = %d\n",__FILE__,__LINE__,
          lattice->param.pressure_w_in[1]);
      }
      else
      {
        fscanf( in, "%lf\n", &(dblank));
        printf("%s %d >> pressure_w_in[1] = %f // unused: NUM_FLUID_COMPONENTS==1\n",__FILE__,__LINE__, dblank);
      }
    }
    else if( !strncmp(param_label,"pressure_e_out[1]",80))
    {
      if( NUM_FLUID_COMPONENTS==2)
      {
        fscanf( in, "%d\n", &(lattice->param.pressure_e_out[1]));
        printf("%s %d >> pressure_e_out[1] = %d\n",__FILE__,__LINE__,
          lattice->param.pressure_e_out[1]);
      }
      else
      {
        fscanf( in, "%lf\n", &(dblank));
        printf("%s %d >> pressure_e_out[1] = %f // unused: NUM_FLUID_COMPONENTS==1\n",__FILE__,__LINE__, dblank);
      }
    }
    else if( !strncmp(param_label,"pressure_w_out[1]",80))
    {
      if( NUM_FLUID_COMPONENTS==2)
      {
        fscanf( in, "%d\n", &(lattice->param.pressure_w_out[1]));
        printf("%s %d >> pressure_w_out[1] = %d\n",__FILE__,__LINE__,
          lattice->param.pressure_w_out[1]);
      }
      else
      {
        fscanf( in, "%lf\n", &(dblank));
        printf("%s %d >> pressure_w_out[1] = %f // unused: NUM_FLUID_COMPONENTS==1\n",__FILE__,__LINE__, dblank);
      }
    }
    else if( !strncmp(param_label,"velocity_e_in[1]",80))
    {
      if( NUM_FLUID_COMPONENTS==2)
      {
        fscanf( in, "%d\n", &(lattice->param.velocity_e_in[1]));
        printf("%s %d >> velocity_e_in[1] = %d\n",__FILE__,__LINE__,
          lattice->param.velocity_e_in[1]);
      }
      else
      {
        fscanf( in, "%lf\n", &(dblank));
        printf("%s %d >> velocity_e_in[1] = %f // unused: NUM_FLUID_COMPONENTS==1\n",__FILE__,__LINE__, dblank);
      }
    }
    else if( !strncmp(param_label,"velocity_w_in[1]",80))
    {
      if( NUM_FLUID_COMPONENTS==2)
      {
        fscanf( in, "%d\n", &(lattice->param.velocity_w_in[1]));
        printf("%s %d >> velocity_w_in[1] = %d\n",__FILE__,__LINE__,
          lattice->param.velocity_w_in[1]);
      }
      else
      {
        fscanf( in, "%lf\n", &(dblank));
        printf("%s %d >> velocity_w_in[1] = %f // unused: NUM_FLUID_COMPONENTS==1\n",__FILE__,__LINE__, dblank);
      }
    }
    else if( !strncmp(param_label,"velocity_e_out[1]",80))
    {
      if( NUM_FLUID_COMPONENTS==2)
      {
        fscanf( in, "%d\n", &(lattice->param.velocity_e_out[1]));
        printf("%s %d >> velocity_e_out[1] = %d\n",__FILE__,__LINE__,
          lattice->param.velocity_e_out[1]);
      }
      else
      {
        fscanf( in, "%lf\n", &(dblank));
        printf("%s %d >> velocity_e_out[1] = %f // unused: NUM_FLUID_COMPONENTS==1\n",__FILE__,__LINE__, dblank);
      }
    }
    else if( !strncmp(param_label,"velocity_w_out[1]",80))
    {
      if( NUM_FLUID_COMPONENTS==2)
      {
        fscanf( in, "%d\n", &(lattice->param.velocity_w_out[1]));
        printf("%s %d >> velocity_w_out[1] = %d\n",__FILE__,__LINE__,
          lattice->param.velocity_w_out[1]);
      }
      else
      {
        fscanf( in, "%lf\n", &(dblank));
        printf("%s %d >> velocity_w_out[1] = %f // unused: NUM_FLUID_COMPONENTS==1\n",__FILE__,__LINE__, dblank);
      }
    }
    else if( !strncmp(param_label,"constcon_n_in",80))
    {
#if INAMURO_SIGMA_COMPONENT
        fscanf( in, "%d\n", &(lattice->param.constcon_n_in));
        printf("%s %d >> constcon_n_in = %d\n",__FILE__,__LINE__,
          lattice->param.constcon_n_in);
#else /* !(INAMURO_SIGMA_COMPONENT) */
      fscanf( in, "%d\n", &(blank));
      printf("%s %d >> constcon_n_in = %d // unused: not INAMURO_SIGMA_COMPONENT\n",__FILE__,__LINE__, blank);
#endif /* (INAMURO_SIGMA_COMPONENT) */
    }
    else if( !strncmp(param_label,"constcon_s_in",80))
    {
#if INAMURO_SIGMA_COMPONENT
        fscanf( in, "%d\n", &(lattice->param.constcon_s_in));
        printf("%s %d >> constcon_s_in = %d\n",__FILE__,__LINE__,
          lattice->param.constcon_s_in);
#else /* !(INAMURO_SIGMA_COMPONENT) */
      fscanf( in, "%d\n", &(blank));
      printf("%s %d >> constcon_s_in = %d // unused: not INAMURO_SIGMA_COMPONENT\n",__FILE__,__LINE__, blank);
#endif /* (INAMURO_SIGMA_COMPONENT) */
    }
    else if( !strncmp(param_label,"constcon_n_out",80))
    {
#if INAMURO_SIGMA_COMPONENT
        fscanf( in, "%d\n", &(lattice->param.constcon_n_out));
        printf("%s %d >> constcon_n_out = %d\n",__FILE__,__LINE__,
          lattice->param.constcon_n_out);
#else /* !(INAMURO_SIGMA_COMPONENT) */
      fscanf( in, "%d\n", &(blank));
      printf("%s %d >> constcon_n_out = %d // unused: not INAMURO_SIGMA_COMPONENT\n",__FILE__,__LINE__, blank);
#endif /* (INAMURO_SIGMA_COMPONENT) */
    }
    else if( !strncmp(param_label,"constcon_s_out",80))
    {
#if INAMURO_SIGMA_COMPONENT
        fscanf( in, "%d\n", &(lattice->param.constcon_s_out));
        printf("%s %d >> constcon_s_out = %d\n",__FILE__,__LINE__,
          lattice->param.constcon_s_out);
#else /* !(INAMURO_SIGMA_COMPONENT) */
      fscanf( in, "%d\n", &(blank));
      printf("%s %d >> constcon_s_out = %d // unused: not INAMURO_SIGMA_COMPONENT\n",__FILE__,__LINE__, blank);
#endif /* (INAMURO_SIGMA_COMPONENT) */
    }
    else if( !strncmp(param_label,"constflx_n_in",80))
    {
#if INAMURO_SIGMA_COMPONENT
        fscanf( in, "%d\n", &(lattice->param.constflx_n_in));
        printf("%s %d >> constflx_n_in = %d\n",__FILE__,__LINE__,
          lattice->param.constflx_n_in);
#else /* !(INAMURO_SIGMA_COMPONENT) */
      fscanf( in, "%d\n", &(blank));
      printf("%s %d >> constflx_n_in = %d // unused: not INAMURO_SIGMA_COMPONENT\n",__FILE__,__LINE__, blank);
#endif /* (INAMURO_SIGMA_COMPONENT) */
    }
    else if( !strncmp(param_label,"constflx_s_in",80))
    {
#if INAMURO_SIGMA_COMPONENT
        fscanf( in, "%d\n", &(lattice->param.constflx_s_in));
        printf("%s %d >> constflx_s_in = %d\n",__FILE__,__LINE__,
          lattice->param.constflx_s_in);
#else /* !(INAMURO_SIGMA_COMPONENT) */
      fscanf( in, "%d\n", &(blank));
      printf("%s %d >> constflx_s_in = %d // unused: not INAMURO_SIGMA_COMPONENT\n",__FILE__,__LINE__, blank);
#endif /* (INAMURO_SIGMA_COMPONENT) */
    }
    else if( !strncmp(param_label,"constflx_n_out",80))
    {
#if INAMURO_SIGMA_COMPONENT
        fscanf( in, "%d\n", &(lattice->param.constflx_n_out));
        printf("%s %d >> constflx_n_out = %d\n",__FILE__,__LINE__,
          lattice->param.constflx_n_out);
#else /* !(INAMURO_SIGMA_COMPONENT) */
      fscanf( in, "%d\n", &(blank));
      printf("%s %d >> constflx_n_out = %d // unused: not INAMURO_SIGMA_COMPONENT\n",__FILE__,__LINE__, blank);
#endif /* (INAMURO_SIGMA_COMPONENT) */
    }
    else if( !strncmp(param_label,"constflx_s_out",80))
    {
#if INAMURO_SIGMA_COMPONENT
        fscanf( in, "%d\n", &(lattice->param.constflx_s_out));
        printf("%s %d >> constflx_s_out = %d\n",__FILE__,__LINE__,
          lattice->param.constflx_s_out);
#else /* !(INAMURO_SIGMA_COMPONENT) */
      fscanf( in, "%d\n", &(blank));
      printf("%s %d >> constflx_s_out = %d // unused: not INAMURO_SIGMA_COMPONENT\n",__FILE__,__LINE__, blank);
#endif /* (INAMURO_SIGMA_COMPONENT) */
    }
    else if( !strncmp(param_label,"constcon_e_in",80))
    {
#if INAMURO_SIGMA_COMPONENT
        fscanf( in, "%d\n", &(lattice->param.constcon_e_in));
        printf("%s %d >> constcon_e_in = %d\n",__FILE__,__LINE__,
          lattice->param.constcon_e_in);
#else /* !(INAMURO_SIGMA_COMPONENT) */
      fscanf( in, "%d\n", &(blank));
      printf("%s %d >> constcon_e_in = %d // unused: not INAMURO_SIGMA_COMPONENT\n",__FILE__,__LINE__, blank);
#endif /* (INAMURO_SIGMA_COMPONENT) */
    }
    else if( !strncmp(param_label,"constcon_w_in",80))
    {
#if INAMURO_SIGMA_COMPONENT
        fscanf( in, "%d\n", &(lattice->param.constcon_w_in));
        printf("%s %d >> constcon_w_in = %d\n",__FILE__,__LINE__,
          lattice->param.constcon_w_in);
#else /* !(INAMURO_SIGMA_COMPONENT) */
      fscanf( in, "%d\n", &(blank));
      printf("%s %d >> constcon_w_in = %d // unused: not INAMURO_SIGMA_COMPONENT\n",__FILE__,__LINE__, blank);
#endif /* (INAMURO_SIGMA_COMPONENT) */
    }
    else if( !strncmp(param_label,"constcon_e_out",80))
    {
#if INAMURO_SIGMA_COMPONENT
        fscanf( in, "%d\n", &(lattice->param.constcon_e_out));
        printf("%s %d >> constcon_e_out = %d\n",__FILE__,__LINE__,
          lattice->param.constcon_e_out);
#else /* !(INAMURO_SIGMA_COMPONENT) */
      fscanf( in, "%d\n", &(blank));
      printf("%s %d >> constcon_e_out = %d // unused: not INAMURO_SIGMA_COMPONENT\n",__FILE__,__LINE__, blank);
#endif /* (INAMURO_SIGMA_COMPONENT) */
    }
    else if( !strncmp(param_label,"constcon_w_out",80))
    {
#if INAMURO_SIGMA_COMPONENT
        fscanf( in, "%d\n", &(lattice->param.constcon_w_out));
        printf("%s %d >> constcon_w_out = %d\n",__FILE__,__LINE__,
          lattice->param.constcon_w_out);
#else /* !(INAMURO_SIGMA_COMPONENT) */
      fscanf( in, "%d\n", &(blank));
      printf("%s %d >> constcon_w_out = %d // unused: not INAMURO_SIGMA_COMPONENT\n",__FILE__,__LINE__, blank);
#endif /* (INAMURO_SIGMA_COMPONENT) */
    }
    else if( !strncmp(param_label,"constflx_e_in",80))
    {
#if INAMURO_SIGMA_COMPONENT
        fscanf( in, "%d\n", &(lattice->param.constflx_e_in));
        printf("%s %d >> constflx_e_in = %d\n",__FILE__,__LINE__,
          lattice->param.constflx_e_in);
#else /* !(INAMURO_SIGMA_COMPONENT) */
      fscanf( in, "%d\n", &(blank));
      printf("%s %d >> constflx_e_in = %d // unused: not INAMURO_SIGMA_COMPONENT\n",__FILE__,__LINE__, blank);
#endif /* (INAMURO_SIGMA_COMPONENT) */
    }
    else if( !strncmp(param_label,"constflx_w_in",80))
    {
#if INAMURO_SIGMA_COMPONENT
        fscanf( in, "%d\n", &(lattice->param.constflx_w_in));
        printf("%s %d >> constflx_w_in = %d\n",__FILE__,__LINE__,
          lattice->param.constflx_w_in);
#else /* !(INAMURO_SIGMA_COMPONENT) */
      fscanf( in, "%d\n", &(blank));
      printf("%s %d >> constflx_w_in = %d // unused: not INAMURO_SIGMA_COMPONENT\n",__FILE__,__LINE__, blank);
#endif /* (INAMURO_SIGMA_COMPONENT) */
    }
    else if( !strncmp(param_label,"constflx_e_out",80))
    {
#if INAMURO_SIGMA_COMPONENT
        fscanf( in, "%d\n", &(lattice->param.constflx_e_out));
        printf("%s %d >> constflx_e_out = %d\n",__FILE__,__LINE__,
          lattice->param.constflx_e_out);
#else /* !(INAMURO_SIGMA_COMPONENT) */
      fscanf( in, "%d\n", &(blank));
      printf("%s %d >> constflx_e_out = %d // unused: not INAMURO_SIGMA_COMPONENT\n",__FILE__,__LINE__, blank);
#endif /* (INAMURO_SIGMA_COMPONENT) */
    }
    else if( !strncmp(param_label,"constflx_w_out",80))
    {
#if INAMURO_SIGMA_COMPONENT
        fscanf( in, "%d\n", &(lattice->param.constflx_w_out));
        printf("%s %d >> constflx_w_out = %d\n",__FILE__,__LINE__,
          lattice->param.constflx_w_out);
#else /* !(INAMURO_SIGMA_COMPONENT) */
      fscanf( in, "%d\n", &(blank));
      printf("%s %d >> constflx_w_out = %d // unused: not INAMURO_SIGMA_COMPONENT\n",__FILE__,__LINE__, blank);
#endif /* (INAMURO_SIGMA_COMPONENT) */
    }
    else if( !strncmp(param_label,"zeroconcgrad_n",80))
    {
#if INAMURO_SIGMA_COMPONENT
        fscanf( in, "%d\n", &(lattice->param.zeroconcgrad_n));
        printf("%s %d >> zeroconcgrad_n = %d\n",__FILE__,__LINE__,
          lattice->param.zeroconcgrad_n);
#else /* !(INAMURO_SIGMA_COMPONENT) */
      fscanf( in, "%d\n", &(blank));
      printf("%s %d >> zeroconcgrad_n = %d // unused: not INAMURO_SIGMA_COMPONENT\n",__FILE__,__LINE__, blank);
#endif /* (INAMURO_SIGMA_COMPONENT) */
    }
    else if( !strncmp(param_label,"zeroconcgrad_s",80))
    {
#if INAMURO_SIGMA_COMPONENT
        fscanf( in, "%d\n", &(lattice->param.zeroconcgrad_s));
        printf("%s %d >> zeroconcgrad_s = %d\n",__FILE__,__LINE__,
          lattice->param.zeroconcgrad_s);
#else /* !(INAMURO_SIGMA_COMPONENT) */
      fscanf( in, "%d\n", &(blank));
      printf("%s %d >> zeroconcgrad_s = %d // unused: not INAMURO_SIGMA_COMPONENT\n",__FILE__,__LINE__, blank);
#endif /* (INAMURO_SIGMA_COMPONENT) */
    }
    else if( !strncmp(param_label,"zeroconcgrad_e",80))
    {
#if INAMURO_SIGMA_COMPONENT
        fscanf( in, "%d\n", &(lattice->param.zeroconcgrad_e));
        printf("%s %d >> zeroconcgrad_e = %d\n",__FILE__,__LINE__,
          lattice->param.zeroconcgrad_e);
#else /* !(INAMURO_SIGMA_COMPONENT) */
      fscanf( in, "%d\n", &(blank));
      printf("%s %d >> zeroconcgrad_e = %d // unused: not INAMURO_SIGMA_COMPONENT\n",__FILE__,__LINE__, blank);
#endif /* (INAMURO_SIGMA_COMPONENT) */
    }
    else if( !strncmp(param_label,"zeroconcgrad_w",80))
    {
#if INAMURO_SIGMA_COMPONENT
        fscanf( in, "%d\n", &(lattice->param.zeroconcgrad_w));
        printf("%s %d >> zeroconcgrad_w = %d\n",__FILE__,__LINE__,
          lattice->param.zeroconcgrad_w);
#else /* !(INAMURO_SIGMA_COMPONENT) */
      fscanf( in, "%d\n", &(blank));
      printf("%s %d >> zeroconcgrad_w = %d // unused: not INAMURO_SIGMA_COMPONENT\n",__FILE__,__LINE__, blank);
#endif /* (INAMURO_SIGMA_COMPONENT) */
    }
    else if( !strncmp(param_label,"zeroconcgrad_full",80))
    {
#if INAMURO_SIGMA_COMPONENT
        fscanf( in, "%d\n", &(lattice->param.zeroconcgrad_full));
        printf("%s %d >> zeroconcgrad_full = %d\n",__FILE__,__LINE__,
          lattice->param.zeroconcgrad_full);
#else /* !(INAMURO_SIGMA_COMPONENT) */
      fscanf( in, "%d\n", &(blank));
      printf("%s %d >> zeroconcgrad_full = %d // unused: not INAMURO_SIGMA_COMPONENT\n",__FILE__,__LINE__, blank);
#endif /* (INAMURO_SIGMA_COMPONENT) */
    }
    else if( !strncmp(param_label,"plot_scale_dynamic",80))
    {
      fscanf( in, "%d\n", &(lattice->param.plot_scale_dynamic));
      printf("%s %d >> plot_scale_dynamic = %d\n",__FILE__,__LINE__,
        lattice->param.plot_scale_dynamic);
    }
    else if( !strncmp(param_label,"use_colormap",80))
    {
      fscanf( in, "%d\n", &(lattice->param.use_colormap));
      printf("%s %d >> use_colormap = %d\n",__FILE__,__LINE__,
        lattice->param.use_colormap);
    }
    else if( !strncmp(param_label,"initial_condition",80))
    {
      fscanf( in, "%d\n", &(lattice->param.initial_condition));
      printf("%s %d >> initial_condition = %d\n",__FILE__,__LINE__,
        lattice->param.initial_condition);
    }
    else if( !strncmp(param_label,"x0",80))
    {
      fscanf( in, "%lf\n", &(lattice->param.x0));
      printf("%s %d >> x0 = %f\n",__FILE__,__LINE__,
         lattice->param.x0);
    }
    else if( !strncmp(param_label,"y0",80))
    {
      fscanf( in, "%lf\n", &(lattice->param.y0));
      printf("%s %d >> y0 = %f\n",__FILE__,__LINE__,
         lattice->param.y0);
    }
    else if( !strncmp(param_label,"r0",80))
    {
      fscanf( in, "%lf\n", &(lattice->param.r0));
      printf("%s %d >> r0 = %f\n",__FILE__,__LINE__,
         lattice->param.r0);
    }
    else if( !strncmp(param_label,"cut",80))
    {
      fscanf( in, "%lf\n", &(lattice->param.cut));
      printf("%s %d >> cut = %f\n",__FILE__,__LINE__,
         lattice->param.cut);
    }
    else if( !strncmp(param_label,"x1",80))
    {
      fscanf( in, "%lf\n", &(lattice->param.x1));
      printf("%s %d >> x1 = %f\n",__FILE__,__LINE__,
         lattice->param.x1);
    }
    else if( !strncmp(param_label,"x2",80))
    {
      fscanf( in, "%lf\n", &(lattice->param.x2));
      printf("%s %d >> x2 = %f\n",__FILE__,__LINE__,
         lattice->param.x2);
    }
    else if( !strncmp(param_label,"y1",80))
    {
      fscanf( in, "%lf\n", &(lattice->param.y1));
      printf("%s %d >> y1 = %f\n",__FILE__,__LINE__,
         lattice->param.y1);
    }
    else if( !strncmp(param_label,"y2",80))
    {
      fscanf( in, "%lf\n", &(lattice->param.y2));
      printf("%s %d >> y2 = %f\n",__FILE__,__LINE__,
         lattice->param.y2);
    }
    else if( !strncmp(param_label,"rel_x1",80))
    {
      fscanf( in, "%lf\n", &(lattice->param.rel_x1));
      printf("%s %d >> rel_x1 = %f\n",__FILE__,__LINE__,
         lattice->param.rel_x1);
    }
    else if( !strncmp(param_label,"rel_x2",80))
    {
      fscanf( in, "%lf\n", &(lattice->param.rel_x2));
      printf("%s %d >> rel_x2 = %f\n",__FILE__,__LINE__,
         lattice->param.rel_x2);
    }
    else if( !strncmp(param_label,"rel_y1",80))
    {
      fscanf( in, "%lf\n", &(lattice->param.rel_y1));
      printf("%s %d >> rel_y1 = %f\n",__FILE__,__LINE__,
         lattice->param.rel_y1);
    }
    else if( !strncmp(param_label,"rel_y2",80))
    {
      fscanf( in, "%lf\n", &(lattice->param.rel_y2));
      printf("%s %d >> rel_y2 = %f\n",__FILE__,__LINE__,
         lattice->param.rel_y2);
    }
    else if( !strncmp(param_label,"dump_rho",80))
    {
      fscanf( in, "%d\n", &(lattice->param.dump_rho));
      printf("%s %d >> dump_rho = %d\n",__FILE__,__LINE__,
        lattice->param.dump_rho);
    }
    else if( !strncmp(param_label,"dump_u",80))
    {
      fscanf( in, "%d\n", &(lattice->param.dump_u));
      printf("%s %d >> dump_u = %d\n",__FILE__,__LINE__,
        lattice->param.dump_u);
    }
    else if( !strncmp(param_label,"dump_force",80))
    {
      fscanf( in, "%d\n", &(lattice->param.dump_force));
      printf("%s %d >> dump_force = %d\n",__FILE__,__LINE__,
        lattice->param.dump_force);
    }
    else if( !strncmp(param_label,"dump_vor",80))
    {
      fscanf( in, "%d\n", &(lattice->param.dump_vor));
      printf("%s %d >> dump_vor = %d\n",__FILE__,__LINE__,
        lattice->param.dump_vor);
    }
    else if( !strncmp(param_label,"do_user_stuff",80))
    {
      fscanf( in, "%d\n", &(lattice->param.do_user_stuff));
      printf("%s %d >> do_user_stuff = %d\n",__FILE__,__LINE__,
        lattice->param.do_user_stuff);
    }
    else if( !strncmp(param_label,"out_path",80))
    {
      //fscanf( in, "%s\n", &(lattice->param.out_path));
      char *str;
      str = (char*)malloc( 1024*sizeof(char));
      get_rest_of_line_as_string( in, &str, 1024);
      strncpy( lattice->param.out_path, str, 1024);
      free(str);
      printf("%s %d >> out_path = %s\n",__FILE__,__LINE__,
        lattice->param.out_path);
    }
    else if( !strncmp(param_label,"make_octave_scripts",80))
    {
      fscanf( in, "%d\n", &(lattice->param.make_octave_scripts));
      printf("%s %d >> make_octave_scripts = %d\n",__FILE__,__LINE__,
        lattice->param.make_octave_scripts);
    }
    else
    {
      printf("%s %d >> WARNING: Unhandled parameter \"%s\".\n",
        __FILE__,__LINE__,param_label);
      skip_rest_of_line( in);
    }
  }
#else /* !(NEW_PARAMS_INPUT_ROUTINE) */

  skip_label( in); fscanf( in, "%d", &( lattice->param.LX)             );
  skip_label( in); fscanf( in, "%d", &( lattice->param.LY)             );
  skip_label( in); fscanf( in, "%d", &( lattice->param.length_scale    ));
  skip_label( in); fscanf( in, "%d", &( lattice->param.NumFrames)      );
  skip_label( in); fscanf( in, "%d", &( lattice->param.FrameRate)      );
  skip_label( in); fscanf( in, "%lf",&( lattice->param.tau[0])         );
  skip_label( in); fscanf( in, "%lf",   lattice->param.gval[0]       );
  skip_label( in); fscanf( in, "%lf",   lattice->param.gval[0]+1     );
  skip_label( in); fscanf( in, "%d",    lattice->param.end_grav        );
  if( NUM_FLUID_COMPONENTS==2)
  {
  skip_label( in); fscanf( in, "%lf",&( lattice->param.tau[1])         );
  skip_label( in); fscanf( in, "%lf",   lattice->param.gval[1]       );
  skip_label( in); fscanf( in, "%lf",   lattice->param.gval[1]+1     );
  skip_label( in); fscanf( in, "%d",    lattice->param.end_grav+1      );
  }
  else if( NUM_FLUID_COMPONENTS==1)
  {
  skip_label( in); fscanf( in, "%lf",   &dblank                        );
  skip_label( in); fscanf( in, "%lf",   &dblank                        );
  skip_label( in); fscanf( in, "%lf",   &dblank                        );
  skip_label( in); fscanf( in, "%d",    &blank                         );
  }
  else
  {
    printf(
      "read_params() -- "
      "Unhandled case "
      "NUM_FLUID_COMPONENTS = %d .  "
      "Exiting!\n",
      NUM_FLUID_COMPONENTS);
    process_exit(1);
  }
    skip_label( in); fscanf( in, "%d ", &(lattice->param.buoyancy      ) );
    skip_label( in); fscanf( in, "%d ", &(lattice->param.buoy_subs     ) );
    skip_label( in); fscanf( in, "%d ", &(lattice->param.incompressible) );
    skip_label( in); fscanf( in, "%d ", &(lattice->param.simple_diffusion) );
    skip_label( in); fscanf( in, "%lf",   lattice->param.rho_A           );
    skip_label( in); fscanf( in, "%lf",   lattice->param.rho_B           );
#if SOURCE_ON
    skip_label( in); fscanf( in, "%lf",   lattice->param.source_strength );
#endif /*SOURCE_ON*/
#if SINK_ON
    skip_label( in); fscanf( in, "%lf",   lattice->param.sink_strength   );
#endif /*SINK_ON*/
#if INAMURO_SIGMA_COMPONENT
    skip_label( in); fscanf( in, "%lf",&( lattice->param.rho_sigma)      );
    skip_label( in); fscanf( in, "%lf",&( lattice->param.beta     )      );
    skip_label( in); fscanf( in, "%lf",&( lattice->param.C0)             );
    skip_label( in); fscanf( in, "%lf",&( lattice->param.rho0)           );
    skip_label( in); fscanf( in, "%lf",&( lattice->param.drhodC)         );
    skip_label( in); fscanf( in, "%lf",&( lattice->param.rho_sigma_in)   );
    skip_label( in); fscanf( in, "%lf",&( lattice->param.C_in)          );
    skip_label( in); fscanf( in, "%lf",&( lattice->param.rho_sigma_out)  );
    skip_label( in); fscanf( in, "%lf",&( lattice->param.C_out)         );
    skip_label( in); fscanf( in, "%lf",&( lattice->param.u_sigma)        );
    skip_label( in); fscanf( in, "%lf",&( lattice->param.u_sigma_in)     );
    skip_label( in); fscanf( in, "%lf",&( lattice->param.u_sigma_out)    );
#if SIGMA_BULK_FLAG
    skip_label( in); fscanf( in, "%d ",&( lattice->param.sigma_bulk_on)    );
#else/*!(SIGMA_BULK_FLAG)*/
    skip_label( in); fscanf( in, "%d ",   &blank                         );
#endif /*SIGMA_BULK_FLAG*/
    skip_label( in); fscanf( in, "%d ",&( lattice->param.sigma_start)    );
    skip_label( in); fscanf( in, "%d ",&( lattice->param.sigma_stop )    );
    skip_label( in); fscanf( in, "%d ",&( lattice->param.sigma_btc_rate ));
    skip_label( in); fscanf( in, "%d ",&( lattice->param.sigma_btc_spot ));
#else /* !( INAMURO_SIGMA_COMPONENT) */
    skip_label( in); fscanf( in, "%lf",   &dblank                        );
    skip_label( in); fscanf( in, "%lf",   &dblank                        );
    skip_label( in); fscanf( in, "%lf",   &dblank                        );
    skip_label( in); fscanf( in, "%lf",   &dblank                        );
    skip_label( in); fscanf( in, "%lf",   &dblank                        );
    skip_label( in); fscanf( in, "%lf",   &dblank                        );
    skip_label( in); fscanf( in, "%d ",   &blank                         );
    skip_label( in); fscanf( in, "%d ",   &blank                         );
    skip_label( in); fscanf( in, "%d ",   &blank                         );
    skip_label( in); fscanf( in, "%d ",   &blank                         );
    skip_label( in); fscanf( in, "%d ",   &blank                         );
#endif /* INAMURO_SIGMA_COMPONENT */
    skip_label( in); fscanf( in, "%lf",&( lattice->param.rho_in)         );
    skip_label( in); fscanf( in, "%lf",&( lattice->param.rho_out)        );
    skip_label( in); fscanf( in, "%lf",&( lattice->param.ux_in)          );
    skip_label( in); fscanf( in, "%lf",&( lattice->param.ux_out)         );
//printf(" %s %d >> %f\n",__FILE__,__LINE__,
//  lattice->param.ux_out);
    skip_label( in); fscanf( in, "%lf",&( lattice->param.uy_in)          );
    skip_label( in); fscanf( in, "%lf",&( lattice->param.uy_out)         );
    skip_label( in); fscanf( in, "%lf",&( lattice->param.G)         );
    skip_label( in); fscanf( in, "%lf",   lattice->param.Gads+0  );
  if( NUM_FLUID_COMPONENTS==2)
  {
    skip_label( in); fscanf( in, "%lf",   lattice->param.Gads+1  );
  }
  else if( NUM_FLUID_COMPONENTS==1)
  {
    skip_label( in); fscanf( in, "%lf",  &dblank                         );
  }
  else
  {
    printf(
      "read_params() -- "
      "Unhandled case "
      "NUM_FLUID_COMPONENTS = %d .  "
      "Exiting!\n",
      NUM_FLUID_COMPONENTS);
    process_exit(1);
  }
    skip_label( in); fscanf( in, "%d", &( lattice->param.ns_flag)        );
    skip_label( in); fscanf( in, "%lf",&( lattice->param.ns)             );
    skip_label( in); fscanf( in, "%d", &( lattice->param.slice_x       ) );
    skip_label( in); fscanf( in, "%d", &( lattice->param.slice_y       ) );
    skip_label( in); fscanf( in, "%d", &( lattice->param.ic_poiseuille) );
    skip_label( in); fscanf( in, "%d", &( lattice->param.bc_poiseuille) );
    skip_label( in); fscanf( in, "%d", &( lattice->param.bc_slip_north ) );
#if INAMURO_SIGMA_COMPONENT
    skip_label( in); fscanf( in, "%d", &( lattice->param.bc_sigma_slip ) );
    skip_label( in); fscanf( in, "%d", &( lattice->param.bc_sigma_walls) );
#if TAU_ZHANG_ANISOTROPIC_DISPERSION
    skip_label( in); fscanf( in, "%lf",&( lattice->param.Dl            ) );
    skip_label( in); fscanf( in, "%lf",&( lattice->param.Dt            ) );
#endif
#else /* !( INAMURO_SIGMA_COMPONENT) */
    skip_label( in); fscanf( in, "%d",   &blank                          );
    skip_label( in); fscanf( in, "%d",   &blank                          );
#if TAU_ZHANG_ANISOTROPIC_DISPERSION
    skip_label( in); fscanf( in, "%lf",  &dblank                         );
    skip_label( in); fscanf( in, "%lf",  &dblank                         );
#endif
#endif /* INAMURO_SIGMA_COMPONENT */
    skip_label( in); fscanf( in, "%d",    lattice->param.pressure_n_in+0 );
    skip_label( in); fscanf( in, "%d",    lattice->param.pressure_s_in+0 );
    skip_label( in); fscanf( in, "%d",    lattice->param.pressure_n_out+0);
    skip_label( in); fscanf( in, "%d",    lattice->param.pressure_s_out+0);
    skip_label( in); fscanf( in, "%d",    lattice->param.velocity_n_in+0 );
    skip_label( in); fscanf( in, "%d",    lattice->param.velocity_s_in+0 );
    skip_label( in); fscanf( in, "%d",    lattice->param.velocity_n_out+0);
    skip_label( in); fscanf( in, "%d",    lattice->param.velocity_s_out+0);
    skip_label( in); fscanf( in, "%d",    lattice->param.pressure_e_in+0 );
    skip_label( in); fscanf( in, "%d",    lattice->param.pressure_w_in+0 );
    skip_label( in); fscanf( in, "%d",    lattice->param.pressure_e_out+0);
    skip_label( in); fscanf( in, "%d",    lattice->param.pressure_w_out+0);
    skip_label( in); fscanf( in, "%d",    lattice->param.velocity_e_in+0 );
    skip_label( in); fscanf( in, "%d",    lattice->param.velocity_w_in+0 );
    skip_label( in); fscanf( in, "%d",    lattice->param.velocity_e_out+0);
    skip_label( in); fscanf( in, "%d",    lattice->param.velocity_w_out+0);
  if( NUM_FLUID_COMPONENTS==2)
  {
    skip_label( in); fscanf( in, "%d",    lattice->param.pressure_n_in+1 );
    skip_label( in); fscanf( in, "%d",    lattice->param.pressure_s_in+1 );
    skip_label( in); fscanf( in, "%d",    lattice->param.pressure_n_out+1);
    skip_label( in); fscanf( in, "%d",    lattice->param.pressure_s_out+1);
    skip_label( in); fscanf( in, "%d",    lattice->param.velocity_n_in+1 );
    skip_label( in); fscanf( in, "%d",    lattice->param.velocity_s_in+1 );
    skip_label( in); fscanf( in, "%d",    lattice->param.velocity_n_out+1);
    skip_label( in); fscanf( in, "%d",    lattice->param.velocity_s_out+1);
    skip_label( in); fscanf( in, "%d",    lattice->param.pressure_e_in+1 );
    skip_label( in); fscanf( in, "%d",    lattice->param.pressure_w_in+1 );
    skip_label( in); fscanf( in, "%d",    lattice->param.pressure_e_out+1);
    skip_label( in); fscanf( in, "%d",    lattice->param.pressure_w_out+1);
    skip_label( in); fscanf( in, "%d",    lattice->param.velocity_e_in+1 );
    skip_label( in); fscanf( in, "%d",    lattice->param.velocity_w_in+1 );
    skip_label( in); fscanf( in, "%d",    lattice->param.velocity_e_out+1);
    skip_label( in); fscanf( in, "%d",    lattice->param.velocity_w_out+1);
  }
  else if( NUM_FLUID_COMPONENTS==1)
  {
    skip_label( in); fscanf( in, "%d",    &blank                         );
    skip_label( in); fscanf( in, "%d",    &blank                         );
    skip_label( in); fscanf( in, "%d",    &blank                         );
    skip_label( in); fscanf( in, "%d",    &blank                         );
    skip_label( in); fscanf( in, "%d",    &blank                         );
    skip_label( in); fscanf( in, "%d",    &blank                         );
    skip_label( in); fscanf( in, "%d",    &blank                         );
    skip_label( in); fscanf( in, "%d",    &blank                         );
    skip_label( in); fscanf( in, "%d",    &blank                         );
    skip_label( in); fscanf( in, "%d",    &blank                         );
    skip_label( in); fscanf( in, "%d",    &blank                         );
    skip_label( in); fscanf( in, "%d",    &blank                         );
    skip_label( in); fscanf( in, "%d",    &blank                         );
    skip_label( in); fscanf( in, "%d",    &blank                         );
    skip_label( in); fscanf( in, "%d",    &blank                         );
    skip_label( in); fscanf( in, "%d",    &blank                         );
  }
  else
  {
    printf(
      "read_params() -- "
      "Unhandled case "
      "NUM_FLUID_COMPONENTS = %d .  "
      "Exiting!\n",
      NUM_FLUID_COMPONENTS);
    process_exit(1);
  }
  if( INAMURO_SIGMA_COMPONENT)
  {
    skip_label( in); fscanf( in, "%d",    &lattice->param.constcon_n_in     );
    skip_label( in); fscanf( in, "%d",    &lattice->param.constcon_s_in     );
    skip_label( in); fscanf( in, "%d",    &lattice->param.constcon_n_out    );
    skip_label( in); fscanf( in, "%d",    &lattice->param.constcon_s_out    );
    skip_label( in); fscanf( in, "%d",    &lattice->param.constflx_n_in     );
    skip_label( in); fscanf( in, "%d",    &lattice->param.constflx_s_in     );
    skip_label( in); fscanf( in, "%d",    &lattice->param.constflx_n_out    );
    skip_label( in); fscanf( in, "%d",    &lattice->param.constflx_s_out    );
    skip_label( in); fscanf( in, "%d",    &lattice->param.constcon_e_in     );
    skip_label( in); fscanf( in, "%d",    &lattice->param.constcon_w_in     );
    skip_label( in); fscanf( in, "%d",    &lattice->param.constcon_e_out    );
    skip_label( in); fscanf( in, "%d",    &lattice->param.constcon_w_out    );
    skip_label( in); fscanf( in, "%d",    &lattice->param.constflx_e_in     );
    skip_label( in); fscanf( in, "%d",    &lattice->param.constflx_w_in     );
    skip_label( in); fscanf( in, "%d",    &lattice->param.constflx_e_out    );
    skip_label( in); fscanf( in, "%d",    &lattice->param.constflx_w_out    );
    skip_label( in); fscanf( in, "%d",    &lattice->param.zeroconcgrad_n    );
    skip_label( in); fscanf( in, "%d",    &lattice->param.zeroconcgrad_s    );
    skip_label( in); fscanf( in, "%d",    &lattice->param.zeroconcgrad_e    );
    skip_label( in); fscanf( in, "%d",    &lattice->param.zeroconcgrad_w    );
    skip_label( in); fscanf( in, "%d",    &lattice->param.zeroconcgrad_full );
  }
  else
  {
    skip_label( in); fscanf( in, "%d",    &blank                         );
    skip_label( in); fscanf( in, "%d",    &blank                         );
    skip_label( in); fscanf( in, "%d",    &blank                         );
    skip_label( in); fscanf( in, "%d",    &blank                         );
    skip_label( in); fscanf( in, "%d",    &blank                         );
    skip_label( in); fscanf( in, "%d",    &blank                         );
    skip_label( in); fscanf( in, "%d",    &blank                         );
    skip_label( in); fscanf( in, "%d",    &blank                         );
    skip_label( in); fscanf( in, "%d",    &blank                         );
    skip_label( in); fscanf( in, "%d",    &blank                         );
    skip_label( in); fscanf( in, "%d",    &blank                         );
    skip_label( in); fscanf( in, "%d",    &blank                         );
    skip_label( in); fscanf( in, "%d",    &blank                         );
    skip_label( in); fscanf( in, "%d",    &blank                         );
    skip_label( in); fscanf( in, "%d",    &blank                         );
    skip_label( in); fscanf( in, "%d",    &blank                         );
    skip_label( in); fscanf( in, "%d",    &blank                         );
    skip_label( in); fscanf( in, "%d",    &blank                         );
    skip_label( in); fscanf( in, "%d",    &blank                         );
    skip_label( in); fscanf( in, "%d",    &blank                         );
    skip_label( in); fscanf( in, "%d",    &blank                         );
  }
  skip_label( in); fscanf( in, "%d", &( lattice->param.plot_scale_dynamic)   );
  skip_label( in); fscanf( in, "%d", &( lattice->param.use_colormap      )   );
  skip_label( in); fscanf( in, "%d", &( lattice->param.initial_condition )   );
  skip_label( in); fscanf( in, "%lf",&( lattice->param.x0                )   );
  skip_label( in); fscanf( in, "%lf",&( lattice->param.y0                )   );
  skip_label( in); fscanf( in, "%lf",&( lattice->param.r0                )   );
  skip_label( in); fscanf( in, "%lf",&( lattice->param.cut               )   );
  skip_label( in); fscanf( in, "%lf",&( lattice->param.x1                )   );
  skip_label( in); fscanf( in, "%lf",&( lattice->param.x2                )   );
  skip_label( in); fscanf( in, "%lf",&( lattice->param.y1                )   );
  skip_label( in); fscanf( in, "%lf",&( lattice->param.y2                )   );
  skip_label( in); fscanf( in, "%lf",&( lattice->param.rel_x1            )   );
  skip_label( in); fscanf( in, "%lf",&( lattice->param.rel_x2            )   );
  skip_label( in); fscanf( in, "%lf",&( lattice->param.rel_y1            )   );
  skip_label( in); fscanf( in, "%lf",&( lattice->param.rel_y2            )   );
  skip_label( in); fscanf( in, "%d", &( lattice->param.dump_rho          )   );
  skip_label( in); fscanf( in, "%d", &( lattice->param.dump_u            )   );
  skip_label( in); fscanf( in, "%d", &( lattice->param.dump_force        )   );
  skip_label( in); fscanf( in, "%d", &( lattice->param.dump_vor          )   );
  skip_label( in); fscanf( in, "%d", &( lattice->param.do_user_stuff     )   );
  skip_label( in); fscanf( in, "%s", &( lattice->param.out_path          )   );
  skip_label( in); fscanf( in, "%s", &( lattice->param.make_octave_scripts)  );
#endif /* (NEW_PARAMS_INPUT_ROUTINE) */

//LBMPI #if PARALLEL
//LBMPI   // Determine local domain size.
//LBMPI   compute_LX( lattice, lattice->lbmpi);
//LBMPI   compute_LY( lattice, lattice->lbmpi);
//LBMPI   printf("%s %d >> (GLX,GLY)=(%d,%d)\n",
//LBMPI     __FILE__,__LINE__,
//LBMPI     get_GLX(lattice->lbmpi),
//LBMPI     get_GLY(lattice->lbmpi));
//LBMPI   printf("%s %d >> (LX,LY)=(%d,%d)\n",
//LBMPI     __FILE__,__LINE__,
//LBMPI     get_LX(lattice),
//LBMPI     get_LY(lattice));
//LBMPI
//LBMPI   // Determine global coordinates of local domain.
//LBMPI   compute_global_coords( lattice);
//LBMPI
//LBMPI #endif /* (PARALLEL) */

  lattice->NumTimeSteps =
    lattice->param.NumFrames * lattice->param.FrameRate;

#if 0
  for( subs=0; subs<NUM_FLUID_COMPONENTS; subs++)
  {
    lattice->param.gval[subs][0] *= lattice->param.tau[subs];
    lattice->param.gval[subs][1] *= lattice->param.tau[subs];
  }
#endif

  if( NUM_FLUID_COMPONENTS==2)
  {
    lattice->param.rho_A[1] = lattice->param.rho_B[0];
    lattice->param.rho_B[1] = lattice->param.rho_A[0];
  }

  if( lattice->param.incompressible)
  {
    if( INAMURO_SIGMA_COMPONENT==0 && NUM_FLUID_COMPONENTS > 1)
    {
      printf("\n");
      printf("\n");
      printf("%s (%d) -- ERROR: "
          "Support for incompressible flow with "
          "two fluid components (as opposed to Inamuro solute component) "
          "is pending.  (Exiting!)\n",
          __FILE__, __LINE__);
      printf("\n");
      printf("\n");
      process_exit(1);
    }
  }

  if( lattice->param.simple_diffusion && NUM_FLUID_COMPONENTS > 1)
  {
    if( INAMURO_SIGMA_COMPONENT==0)
    {
      printf("\n");
      printf("\n");
      printf("%s (%d) -- ERROR: "
          "Support for simplified diffusion with "
          "two fluid components (as opposed to Inamuro solute component) "
          "is pending.  (Exiting!)\n",
          __FILE__, __LINE__);
      printf("\n");
      printf("\n");
      process_exit(1);
    }
  }

  // Set default values for x0, y0 and r0 if they are negative.
  if( lattice->param.x0 < 0.)
  {
    lattice->param.x0 = lattice->param.LX/2.;
  }
  if( lattice->param.y0 < 0.)
  {
    lattice->param.y0 = lattice->param.LY/2.;
  }
  if( lattice->param.r0 < 0.)
  {
    lattice->param.r0 = (lattice->param.LX+lattice->param.LY)/8.;
  }

  // Set default value for cut if it is negative.
  if( lattice->param.cut < 0.)
  {
    lattice->param.cut = .4;
  }

  // Set default values for x1, x2, y1, y2 if they are negative.
  if( lattice->param.x1 < 0.)
  {
    if( lattice->param.rel_x1 < 0.)
    {
      lattice->param.x1 = lattice->param.LX/4.-1;
    }
    else
    {
      lattice->param.x1 = lattice->param.rel_x1*lattice->param.LX-1;
    }
  }
  if( lattice->param.x2 < 0.)
  {
    if( lattice->param.rel_x2 < 0.)
    {
      lattice->param.x2 = lattice->param.LX/2.-1;
    }
    else
    {
      lattice->param.x2 = lattice->param.rel_x2*lattice->param.LX-1;
    }
  }
  if( lattice->param.y1 < 0.)
  {
    if( lattice->param.rel_y1 < 0.)
    {
      lattice->param.y1 = lattice->param.LY/4.-1;
    }
    else
    {
      lattice->param.y1 = lattice->param.rel_y1*lattice->param.LY-1;
    }
  }
  if( lattice->param.y2 < 0.)
  {
    if( lattice->param.rel_y2 < 0.)
    {
      lattice->param.y2 = lattice->param.LY/2.-1;
    }
    else
    {
      lattice->param.y2 = lattice->param.rel_y2*lattice->param.LY-1;
    }
  }

#if INAMURO_SIGMA_COMPONENT
  if( lattice->param.beta != 0.)
  {
    if( lattice->param.rho0 != 0. || lattice->param.drhodC != 0.)
    {
      printf("ERROR: beta nonzero and rho0,drhodC nonzero. "
          "Choose on or the other.");
      process_exit(1);
    }
  }
  else
  {
    if( lattice->param.rho0 != 0. && lattice->param.drhodC != 0.)
    {
      lattice->param.beta = (1./lattice->param.rho0)*(lattice->param.drhodC);
    }
  }

  if( lattice->param.rho_sigma_in != 0.)
  {
    if( lattice->param.C_in != 0.)
    {
      if( lattice->param.rho_sigma_in != lattice->param.C_in)
      {
        printf("%s %d >> ERROR: 0.!=rho_sigma_in!=C_in. Exiting.",__FILE__,__LINE__);
        process_exit(1);
      }
    }
    else
    {
      lattice->param.C_in  = lattice->param.rho_sigma_in;
    }
  }

  if( lattice->param.rho_sigma_out != 0.)
  {
    if( lattice->param.C_out != 0.)
    {
      if( lattice->param.rho_sigma_out != lattice->param.C_out)
      {
        printf("%s %d >> ERROR: 0.!=rho_sigma_out!=C_out. Exiting.",__FILE__,__LINE__);
        process_exit(1);
      }
    }
    else
    {
      lattice->param.C_out  = lattice->param.rho_sigma_out;
    }
  }
#endif

  fclose(in);

#if VERBOSITY_LEVEL >= 1
  printf("lbio.c: read_params() -- Read file \"params.in\".\n");
#endif /* VERBOSITY_LEVEL >= 1 */

} /* void read_params( struct lattice_struct *lattice) */

// void dump_params( struct lattice_struct *lattice)
//##############################################################################
//
// D U M P   P A R A M S
//
//  - Output the problem parameters to a file.
//
void dump_params( struct lattice_struct *lattice)
{
  FILE *o;
  char filename[1024];

  sprintf( filename
         , "%s/params%dx%d_proc%04d.dat"
         , get_out_path(lattice)
         , get_LX(lattice), get_LY(lattice), get_proc_id(lattice)
         );

  if( !( o = fopen(filename,"w+")))
  {
    printf("\n%s %d >> ERROR: fopen(\"%s\",\"w+\") = NULL.\n",
        __FILE__, __LINE__, filename);
    printf("\n%s %d >> NOTE: You might need to create directory \"%s\"\n",
        __FILE__, __LINE__, get_out_path(lattice));
    printf("%s %d >> "
       "If the directory just shown is not \"./out\" and does not\n"
       "look familiar as a custom directory you requested, edit\n"
       "\"./in/params.in\" and either change the \"out_path\" parameter or\n"
       "delete it. (Deleting the \"out_path\" entry in \"params.in\" will\n"
       "cause the code to revert to the default \"./out\" folder.)",
       __FILE__, __LINE__);
    process_exit(1);
  }

  fprintf( o, "LX                   %d\n", lattice->param.LX             );
  fprintf( o, "LY                   %d\n", lattice->param.LY             );
  fprintf( o, "length_scale         %d\n", lattice->param.length_scale   );
  fprintf( o, "NumNodes             %d\n", lattice->NumNodes             );
  fprintf( o, "NumFrames            %d\n", lattice->param.NumFrames      );
  fprintf( o, "FrameRate            %d\n", lattice->param.FrameRate      );
  fprintf( o, "NumTimeSteps         %d\n", lattice->NumTimeSteps         );
  fprintf( o, "tau[0]               %f\n", lattice->param.tau[0]         );
  fprintf( o, "gval[0][0]         %f\n", lattice->param.gval[0][0]   );
  fprintf( o, "gval[0][1]         %f\n", lattice->param.gval[0][1]   );
  fprintf( o, "end_grav[0]          %d\n", lattice->param.end_grav[0]    );
  if( NUM_FLUID_COMPONENTS==2)
  {
  fprintf( o, "tau[1]               %f\n", lattice->param.tau[1]         );
  fprintf( o, "gval[1][0]         %f\n", lattice->param.gval[1][0]   );
  fprintf( o, "gval[1][1]         %f\n", lattice->param.gval[1][1]   );
  fprintf( o, "end_grav[1]          %d\n", lattice->param.end_grav[1]    );
  }
  else if( NUM_FLUID_COMPONENTS==1)
  {
  fprintf( o, "tau[1]               %s\n", "--"                          );
  fprintf( o, "gval[1][0]         %s\n", "--"                          );
  fprintf( o, "gval[1][1]         %s\n", "--"                          );
  fprintf( o, "end_grav[1]          %s\n", "--"                          );
  }
  else
  {
    printf(
      "read_params() -- "
      "Unhandled case "
      "NUM_FLUID_COMPONENTS = %d .  "
      "Exiting!\n",
      NUM_FLUID_COMPONENTS);
    process_exit(1);
  }
  fprintf( o, "buoyancy             %d\n", lattice->param.buoyancy        );
  fprintf( o, "buoy_subs            %d\n", lattice->param.buoy_subs       );
  fprintf( o, "incompressible       %d\n", lattice->param.incompressible  );
  fprintf( o, "simple_diffusion     %d\n", lattice->param.simple_diffusion);
  fprintf( o, "rho_A[0]             %f\n", lattice->param.rho_A[0]       );
  fprintf( o, "rho_B[0]             %f\n", lattice->param.rho_B[0]       );
  if( NUM_FLUID_COMPONENTS==2)
  {
  fprintf( o, "rho_A[1]             %f\n", lattice->param.rho_A[1]       );
  fprintf( o, "rho_B[1]             %f\n", lattice->param.rho_B[1]       );
  }
  else if( NUM_FLUID_COMPONENTS==1)
  {
  fprintf( o, "rho_A[1]             %s\n", "--"                          );
  fprintf( o, "rho_B[1]             %s\n", "--"                          );
  }
  else
  {
    printf(
      "read_params() -- "
      "Unhandled case "
      "NUM_FLUID_COMPONENTS = %d .  "
      "Exiting!\n",
      NUM_FLUID_COMPONENTS);
    process_exit(1);
  }

#if SOURCE_ON
  fprintf( o, "source_strength            %f\n", lattice->param.source_strength      );
#else /*if !(SOURCE_ON)*/
  fprintf( o, "source_strength            %s\n", "--"                          );
#endif /*SOURCE_ON*/

#if SINK_ON
  fprintf( o, "sink_strength            %f\n", lattice->param.sink_strength      );
#else /*if !(SINK_ON)*/
  fprintf( o, "sink_strength            %s\n", "--"                          );
#endif /*SINK_ON*/

#if INAMURO_SIGMA_COMPONENT
  fprintf( o, "rho_sigma            %f\n", lattice->param.rho_sigma      );
  fprintf( o, "beta                 %f\n", lattice->param.beta           );
  fprintf( o, "C0                   %f\n", lattice->param.C0             );
  fprintf( o, "rho0                 %f\n", lattice->param.rho0           );
  fprintf( o, "drhodC               %f\n", lattice->param.drhodC         );
  fprintf( o, "rho_sigma_in         %f\n", lattice->param.rho_sigma_in   );
  fprintf( o, "C_in                %f\n", lattice->param.C_in          );
  fprintf( o, "rho_sigma_out        %f\n", lattice->param.rho_sigma_out  );
  fprintf( o, "C_out               %f\n", lattice->param.C_out         );
  fprintf( o, "u_sigma              %f\n", lattice->param.u_sigma        );
  fprintf( o, "u_sigma_in           %f\n", lattice->param.u_sigma_in     );
  fprintf( o, "u_sigma_out          %f\n", lattice->param.u_sigma_out    );
#if SIGMA_BULK_FLAG
  fprintf( o, "sigma_bulk_on          %d\n", lattice->param.sigma_bulk_on);
#else /*!(SIGMA_BULK_FLAG)*/
  fprintf( o, "sigma_bulk_on          %s\n", "--"                        );
#endif /*SIGMA_BULK_FLAG*/
  fprintf( o, "sigma_start          %d\n", lattice->param.sigma_start    );
  fprintf( o, "sigma_stop           %d\n", lattice->param.sigma_stop     );
  fprintf( o, "sigma_btc_rate       %d\n", lattice->param.sigma_btc_rate );
  fprintf( o, "sigma_btc_spot       %d\n", lattice->param.sigma_btc_spot );
#else /* !( INAMURO_SIGMA_COMPONENT) */
  fprintf( o, "rho_sigma            %s\n", "--"                          );
  fprintf( o, "beta                 %s\n", "--"                          );
  fprintf( o, "C0                   %s\n", "--"                          );
  fprintf( o, "rho0                 %s\n", "--"                          );
  fprintf( o, "drhodC               %s\n", "--"                          );
  fprintf( o, "rho_sigma_in         %s\n", "--"                          );
  fprintf( o, "C_in                %s\n", "--"                          );
  fprintf( o, "rho_sigma_out        %s\n", "--"                          );
  fprintf( o, "C_out               %s\n", "--"                          );
  fprintf( o, "u_sigma              %s\n", "--"                          );
  fprintf( o, "u_sigma_in           %s\n", "--"                          );
  fprintf( o, "u_sigma_out          %s\n", "--"                          );
  fprintf( o, "sigma_bulk_on          %s\n", "--"                          );
  fprintf( o, "sigma_start          %s\n", "--"                          );
  fprintf( o, "sigma_stop           %s\n", "--"                          );
  fprintf( o, "sigma_btc_rate       %s\n", "--"                          );
  fprintf( o, "sigma_btc_spot       %s\n", "--"                          );
#endif /* INAMURO_SIGMA_COMPONENT */
  fprintf( o, "rho_in               %f\n", lattice->param.rho_in         );
  fprintf( o, "rho_out              %f\n", lattice->param.rho_out        );
  fprintf( o, "ux_in                %f\n", lattice->param.ux_in          );
  fprintf( o, "ux_out               %f\n", lattice->param.ux_out         );
  fprintf( o, "uy_in                %f\n", lattice->param.uy_in          );
  fprintf( o, "uy_out               %f\n", lattice->param.uy_out         );
  fprintf( o, "G               %f\n", lattice->param.G         );
  fprintf( o, "Gads[0]      %f\n", lattice->param.Gads[0]);
  if( NUM_FLUID_COMPONENTS==2)
  {
  fprintf( o, "Gads[1]      %f\n", lattice->param.Gads[1]);
  }
  else if( NUM_FLUID_COMPONENTS==1)
  {
  fprintf( o, "Gads[1]      %s\n", "--"                          );
  }
  else
  {
    printf(
      "read_params() -- "
      "Unhandled case "
      "NUM_FLUID_COMPONENTS = %d .  "
      "Exiting!\n",
      NUM_FLUID_COMPONENTS);
    process_exit(1);
  }
  fprintf( o, "periodic_x[0]        %d\n", lattice->periodic_x[0]        );
  fprintf( o, "periodic_y[0]        %d\n", lattice->periodic_y[0]        );
  if( NUM_FLUID_COMPONENTS==2)
  {
  fprintf( o, "periodic_x[1]        %d\n", lattice->periodic_x[1]        );
  fprintf( o, "periodic_y[1]        %d\n", lattice->periodic_y[1]        );
  }
  else if( NUM_FLUID_COMPONENTS==1)
  {
  fprintf( o, "periodic_x[1]        %s\n", "--"                          );
  fprintf( o, "periodic_y[1]        %s\n", "--"                          );
  }
  else
  {
    printf(
      "read_params() -- "
      "Unhandled case "
      "NUM_FLUID_COMPONENTS = %d .  "
      "Exiting!\n",
      NUM_FLUID_COMPONENTS);
    process_exit(1);
  }
  fprintf( o, "ns                   %d\n", lattice->param.ns_flag        );
  fprintf( o, "ns                   %f\n", lattice->param.ns             );
  fprintf( o, "slice_x              %d\n", lattice->param.slice_x        );
  fprintf( o, "slice_y              %d\n", lattice->param.slice_y        );
  fprintf( o, "ic_poiseuille        %d\n", lattice->param.ic_poiseuille  );
  fprintf( o, "bc_poiseuille        %d\n", lattice->param.bc_poiseuille  );
  fprintf( o, "bc_slip_north        %d\n", lattice->param.bc_slip_north  );
#if INAMURO_SIGMA_COMPONENT
  fprintf( o, "bc_sigma_slip        %d\n", lattice->param.bc_sigma_slip  );
  fprintf( o, "bc_sigma_walls       %d\n", lattice->param.bc_sigma_walls );
#else /* !( INAMURO_SIGMA_COMPONENT) */
  fprintf( o, "bc_sigma_slip        %s\n", "--"                          );
  fprintf( o, "bc_sigma_walls       %s\n", "--"                          );
#endif /* INAMURO_SIGMA_COMPONENT */
  fprintf( o, "pressure_n_in[0]     %d\n", lattice->param.pressure_n_in[0]  );
  fprintf( o, "pressure_s_in[0]     %d\n", lattice->param.pressure_s_in[0]  );
  fprintf( o, "pressure_n_out[0]    %d\n", lattice->param.pressure_n_out[0] );
  fprintf( o, "pressure_s_out[0]    %d\n", lattice->param.pressure_s_out[0] );
  fprintf( o, "velocity_n_in[0]     %d\n", lattice->param.velocity_n_in[0]  );
  fprintf( o, "velocity_s_in[0]     %d\n", lattice->param.velocity_s_in[0]  );
  fprintf( o, "velocity_n_out[0]    %d\n", lattice->param.velocity_n_out[0] );
  fprintf( o, "velocity_s_out[0]    %d\n", lattice->param.velocity_s_out[0] );
  fprintf( o, "pressure_e_in[0]     %d\n", lattice->param.pressure_e_in[0]  );
  fprintf( o, "pressure_w_in[0]     %d\n", lattice->param.pressure_w_in[0]  );
  fprintf( o, "pressure_e_out[0]    %d\n", lattice->param.pressure_e_out[0] );
  fprintf( o, "pressure_w_out[0]    %d\n", lattice->param.pressure_w_out[0] );
  fprintf( o, "velocity_e_in[0]     %d\n", lattice->param.velocity_e_in[0]  );
  fprintf( o, "velocity_w_in[0]     %d\n", lattice->param.velocity_w_in[0]  );
  fprintf( o, "velocity_e_out[0]    %d\n", lattice->param.velocity_e_out[0] );
  fprintf( o, "velocity_w_out[0]    %d\n", lattice->param.velocity_w_out[0] );
  if( NUM_FLUID_COMPONENTS==2)
  {
  fprintf( o, "pressure_n_in[1]     %d\n", lattice->param.pressure_n_in[1]  );
  fprintf( o, "pressure_s_in[1]     %d\n", lattice->param.pressure_s_in[1]  );
  fprintf( o, "pressure_n_out[1]    %d\n", lattice->param.pressure_n_out[1] );
  fprintf( o, "pressure_s_out[1]    %d\n", lattice->param.pressure_s_out[1] );
  fprintf( o, "velocity_n_in[1]     %d\n", lattice->param.velocity_n_in[1]  );
  fprintf( o, "velocity_s_in[1]     %d\n", lattice->param.velocity_s_in[1]  );
  fprintf( o, "velocity_n_out[1]    %d\n", lattice->param.velocity_n_out[1] );
  fprintf( o, "velocity_s_out[1]    %d\n", lattice->param.velocity_s_out[1] );
  fprintf( o, "pressure_e_in[1]     %d\n", lattice->param.pressure_e_in[1]  );
  fprintf( o, "pressure_w_in[1]     %d\n", lattice->param.pressure_w_in[1]  );
  fprintf( o, "pressure_e_out[1]    %d\n", lattice->param.pressure_e_out[1] );
  fprintf( o, "pressure_w_out[1]    %d\n", lattice->param.pressure_w_out[1] );
  fprintf( o, "velocity_e_in[1]     %d\n", lattice->param.velocity_e_in[1]  );
  fprintf( o, "velocity_w_in[1]     %d\n", lattice->param.velocity_w_in[1]  );
  fprintf( o, "velocity_e_out[1]    %d\n", lattice->param.velocity_e_out[1] );
  fprintf( o, "velocity_w_out[1]    %d\n", lattice->param.velocity_w_out[1] );
  }
  else if( NUM_FLUID_COMPONENTS==1)
  {
  fprintf( o, "pressure_n_in[1]     %s\n", "--"                             );
  fprintf( o, "pressure_s_in[1]     %s\n", "--"                             );
  fprintf( o, "pressure_n_out[1]    %s\n", "--"                             );
  fprintf( o, "pressure_s_out[1]    %s\n", "--"                             );
  fprintf( o, "velocity_n_in[1]     %s\n", "--"                             );
  fprintf( o, "velocity_s_in[1]     %s\n", "--"                             );
  fprintf( o, "velocity_n_out[1]    %s\n", "--"                             );
  fprintf( o, "velocity_s_out[1]    %s\n", "--"                             );
  fprintf( o, "pressure_e_in[1]     %s\n", "--"                             );
  fprintf( o, "pressure_w_in[1]     %s\n", "--"                             );
  fprintf( o, "pressure_e_out[1]    %s\n", "--"                             );
  fprintf( o, "pressure_w_out[1]    %s\n", "--"                             );
  fprintf( o, "velocity_e_in[1]     %s\n", "--"                             );
  fprintf( o, "velocity_w_in[1]     %s\n", "--"                             );
  fprintf( o, "velocity_e_out[1]    %s\n", "--"                             );
  fprintf( o, "velocity_w_out[1]    %s\n", "--"                             );
  }
  else
  {
    printf(
      "read_params() -- "
      "Unhandled case "
      "NUM_FLUID_COMPONENTS = %d .  "
      "Exiting!\n",
      NUM_FLUID_COMPONENTS);
    process_exit(1);
  }
  if( INAMURO_SIGMA_COMPONENT)
  {
  fprintf( o, "constcon_n_in        %d\n", lattice->param.constcon_n_in  );
  fprintf( o, "constcon_s_in        %d\n", lattice->param.constcon_s_in  );
  fprintf( o, "constcon_n_out       %d\n", lattice->param.constcon_n_out );
  fprintf( o, "constcon_s_out       %d\n", lattice->param.constcon_s_out );
  fprintf( o, "constflx_n_in        %d\n", lattice->param.constflx_n_in  );
  fprintf( o, "constflx_s_in        %d\n", lattice->param.constflx_s_in  );
  fprintf( o, "constflx_n_out       %d\n", lattice->param.constflx_n_out );
  fprintf( o, "constflx_s_out       %d\n", lattice->param.constflx_s_out );
  fprintf( o, "constcon_n_in        %d\n", lattice->param.constcon_e_in  );
  fprintf( o, "constcon_s_in        %d\n", lattice->param.constcon_w_in  );
  fprintf( o, "constcon_n_out       %d\n", lattice->param.constcon_e_out );
  fprintf( o, "constcon_s_out       %d\n", lattice->param.constcon_w_out );
  fprintf( o, "constflx_n_in        %d\n", lattice->param.constflx_e_in  );
  fprintf( o, "constflx_s_in        %d\n", lattice->param.constflx_w_in  );
  fprintf( o, "constflx_n_out       %d\n", lattice->param.constflx_e_out );
  fprintf( o, "constflx_s_out       %d\n", lattice->param.constflx_w_out );
  fprintf( o, "zeroconcgrad_n       %d\n", lattice->param.zeroconcgrad_n );
  fprintf( o, "zeroconcgrad_s       %d\n", lattice->param.zeroconcgrad_s );
  fprintf( o, "zeroconcgrad_n       %d\n", lattice->param.zeroconcgrad_e );
  fprintf( o, "zeroconcgrad_s       %d\n", lattice->param.zeroconcgrad_w );
  fprintf( o, "zeroconcgrad_full    %d\n", lattice->param.zeroconcgrad_full );
  }
  else
  {
  fprintf( o, "constcon_n_in        %s\n", "--"                             );
  fprintf( o, "constcon_s_in        %s\n", "--"                             );
  fprintf( o, "constcon_n_out       %s\n", "--"                             );
  fprintf( o, "constcon_s_out       %s\n", "--"                             );
  fprintf( o, "constflx_n_in        %s\n", "--"                             );
  fprintf( o, "constflx_s_in        %s\n", "--"                             );
  fprintf( o, "constflx_n_out       %s\n", "--"                             );
  fprintf( o, "constflx_s_out       %s\n", "--"                             );
  fprintf( o, "constcon_e_in        %s\n", "--"                             );
  fprintf( o, "constcon_w_in        %s\n", "--"                             );
  fprintf( o, "constcon_e_out       %s\n", "--"                             );
  fprintf( o, "constcon_w_out       %s\n", "--"                             );
  fprintf( o, "constflx_e_in        %s\n", "--"                             );
  fprintf( o, "constflx_w_in        %s\n", "--"                             );
  fprintf( o, "constflx_e_out       %s\n", "--"                             );
  fprintf( o, "constflx_w_out       %s\n", "--"                             );
  fprintf( o, "zeroconcgrad_n       %s\n", "--"                             );
  fprintf( o, "zeroconcgrad_s       %s\n", "--"                             );
  fprintf( o, "zeroconcgrad_e       %s\n", "--"                             );
  fprintf( o, "zeroconcgrad_w       %s\n", "--"                             );
  fprintf( o, "zeroconcgrad_full    %s\n", "--"                             );
  }

  fprintf( o, "plot_scale_dynamic   %d\n", lattice->param.plot_scale_dynamic);
  fprintf( o, "use_colormap         %d\n", lattice->param.use_colormap      );
  fprintf( o, "initial_condition    %d\n", lattice->param.initial_condition );
  fprintf( o, "x0                   %f\n", lattice->param.x0                );
  fprintf( o, "y0                   %f\n", lattice->param.y0                );
  fprintf( o, "r0                   %f\n", lattice->param.r0                );
  fprintf( o, "cut                  %f\n", lattice->param.cut               );
  fprintf( o, "x1                   %f\n", lattice->param.x1                );
  fprintf( o, "x2                   %f\n", lattice->param.x2                );
  fprintf( o, "y1                   %f\n", lattice->param.y1                );
  fprintf( o, "y2                   %f\n", lattice->param.y2                );
  fprintf( o, "rel_x1               %f\n", lattice->param.rel_x1            );
  fprintf( o, "rel_x2               %f\n", lattice->param.rel_x2            );
  fprintf( o, "rel_y1               %f\n", lattice->param.rel_y1            );
  fprintf( o, "rel_y2               %f\n", lattice->param.rel_y2            );
  fprintf( o, "dump_rho             %d\n", lattice->param.dump_rho          );
  fprintf( o, "dump_u               %d\n", lattice->param.dump_u            );
  fprintf( o, "dump_force           %d\n", lattice->param.dump_force        );
  fprintf( o, "dump_vor             %d\n", lattice->param.dump_vor          );
  fprintf( o, "do_user_stuff        %d\n", lattice->param.do_user_stuff     );
  fprintf( o, "out_path             %s\n", lattice->param.out_path          );
  fprintf( o, "make_octave_scripts  %d\n", lattice->param.make_octave_scripts);

  fclose(o);

#if VERBOSITY_LEVEL >= 1
  printf("lbio.c: dump_params() -- Wrote file \"%s\".\n",filename);
#endif /* VERBOSITY_LEVEL >= 1 */

} /* void dump_params( struct lattice_struct *lattice) */


