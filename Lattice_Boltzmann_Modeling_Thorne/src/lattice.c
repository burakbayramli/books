//##############################################################################
//
// Copyright (C), 2005, Michael Sukop and Danny Thorne
//
// lattice.c
//
// Accessor functions for members of the data structure in lattice.h
//

int get_LX( lattice_ptr lattice) { return lattice->param.LX;}
int get_LY( lattice_ptr lattice) { return lattice->param.LY;}
int set_LX( lattice_ptr lattice, int arg_LX) { lattice->param.LX = arg_LX;}
int set_LY( lattice_ptr lattice, int arg_LY) { lattice->param.LY = arg_LY;}
int get_si( lattice_ptr lattice) { return 0;}
int get_sj( lattice_ptr lattice) { return 0;}
int get_ei( lattice_ptr lattice) { return lattice->param.LX-1;}
int get_ej( lattice_ptr lattice) { return lattice->param.LY-1;}

int get_NumFrames( lattice_ptr lattice) { return lattice->param.NumFrames;}
int get_FrameRate( lattice_ptr lattice) { return lattice->param.FrameRate;}

int get_NumNodes( lattice_ptr lattice)
{
  if(lattice->NumNodes>0)
  {
    return lattice->NumNodes;
  }
  else
  {
    return lattice->param.LX*lattice->param.LY;
  }
}
void set_NumNodes( lattice_ptr lattice)
{
  lattice->NumNodes = get_LX( lattice)*get_LY( lattice);
}

double get_G( lattice_ptr lattice) { return lattice->param.G;}
double get_Gads( lattice_ptr lattice, int subs)
{
  return lattice->param.Gads[subs];
}

double *get_ftemp_ptr( lattice_ptr lattice, int subs, int j, int i, int a)
{
  return (double *)(lattice->pdf[subs]->ftemp + IJ2N(i,j));
}

int get_time( lattice_ptr lattice) { return lattice->time;}
int get_frame( lattice_ptr lattice) { return lattice->frame;}

int is_first_timestep( lattice_ptr lattice)
{
  return ( lattice->time == 1);
}

int adjust_zero_flux_for_btc( lattice_ptr lattice)
{
  // Toggle the adjustment in zeroconcgrad_w to make sure btc is measured
  // correctly.
  return 0;
}

int is_last_step_of_frame( lattice_ptr lattice)
{
  if( (get_time(lattice)+0) == get_frame(lattice)*get_FrameRate(lattice))
  {
    return 1;
  }
  else
  {
    return 0;
  }
}

int is_solid_node( lattice_ptr lattice, int subs, int n)
{
  return (lattice->bc[subs][n%get_NumNodes(lattice)].bc_type&BC_SOLID_NODE)?(1):(0);
}

int is_not_solid_node( lattice_ptr lattice, int subs, int n)
{
  return !is_solid_node( lattice, subs, n);
}
void make_solid_node( lattice_ptr lattice, const int subs, const int n)
{
  lattice->bc[subs][n].bc_type|=BC_SOLID_NODE;
}

int bcs_on_solids( lattice_ptr lattice)
{
  // TODO: Input this from params.
  return 0;
}

int is_incompressible( lattice_ptr lattice)
{
  return lattice->param.incompressible;
}
int annotate_incompressible_filenames( lattice_ptr lattice)
{
  // Put an 'i' in the filenames of outputs from incompressible runs to
  // distinguish from filenames of corresponding compressible runs.
  // This might be useful to compare results between compressible and
  // incompressible runs. But some scripts might not know about the 'i' and
  // so they would need the filenames to be sans the 'i' to work. Could
  // make this an option in 'params.in'...
  //return 1;
  return 0;
}

int hydrostatic( lattice_ptr lattice)
{
  // Pressure boundaries enforce a hydrostatic condition. This is
  // experimental and not automatic -- it requires manual fiddling
  // in the bcs.c file.
  return 0;
}
int hydrostatic_west( lattice_ptr lattice)
{
  // Pressure boundaries enforce a hydrostatic condition. This is
  // experimental and not automatic -- it requires manual fiddling
  // in the bcs.c file.
  return hydrostatic( lattice);
}
int hydrostatic_compressible( lattice_ptr lattice)
{
  // Use compressible version of density profile as opposed to linear
  // approximation.
  return 0;
}

int hydrostatic_compute_rho_ref( lattice_ptr lattice)
{
  // Compute the reference density for the compressible density profile.
  // If this switch is off, the value of rho_out will be used as the
  // reference density.
  return 0;
}

double get_tau( lattice_ptr lattice, const int subs)
{
  return lattice->param.tau[subs];
}

int gravitationally_adjacent_to_a_solid( lattice_ptr lattice, int subs, int n, int dir)
{
  if( lattice->param.gval[subs][dir] != 0)
  {
    return ( is_solid_node( lattice, subs, n-get_LX(lattice))
           ||is_solid_node( lattice, subs, n+get_LX(lattice)));
  }
  else
  {
    return 0;
  }
}

int on_the_east( lattice_ptr lattice, const int n)
{
  return( /*east*/(n+1)%get_LX(lattice) == 0);
}

int on_the_east_or_west( lattice_ptr lattice, const int n)
{
  return(   /*west*/(n  )%get_LX(lattice) == 0
         || /*east*/(n+1)%get_LX(lattice) == 0);
}

int on_the_north_or_south( lattice_ptr lattice, const int n)
{
  return(   /*north*/ n >= get_NumNodes(lattice) - get_LX(lattice)
         || /*south*/ n <  get_LX(lattice) );
}

#if INAMURO_SIGMA_COMPONENT
int flow_dir_is_vertical( lattice_ptr lattice)
{
  return( lattice->FlowDir == /*Vertical*/2);
}

int flow_dir_is_horizontal( lattice_ptr lattice)
{
  return( lattice->FlowDir == /*Horizontal*/1);
}
#endif

double get_rho_A( lattice_ptr lattice, int subs)
{
  return lattice->param.rho_A[subs];
}
double get_rho_B( lattice_ptr lattice, int subs)
{
  return lattice->param.rho_B[subs];
}
#if INAMURO_SIGMA_COMPONENT
double get_rho_sigma( lattice_ptr lattice) { return lattice->param.rho_sigma;}
double get_C( lattice_ptr lattice) { return get_rho_sigma(lattice);}
double get_C0( lattice_ptr lattice) { return lattice->param.C0;}
double get_rho0( lattice_ptr lattice) { return lattice->param.rho0;}
double get_drhodC( lattice_ptr lattice) { return lattice->param.drhodC;}
double get_C_in( lattice_ptr lattice) { return lattice->param.C_in;}
double get_rho_sigma_in( lattice_ptr lattice) { return lattice->param.rho_sigma_in;}
double get_C_out( lattice_ptr lattice) { return lattice->param.C_out;}
double get_rho_sigma_out( lattice_ptr lattice) { return lattice->param.rho_sigma_out;}
double get_beta( lattice_ptr lattice) { return lattice->param.beta;}
#endif /* INAMURO_SIGMA_COMPONENT */

int is_periodic_in_x( lattice_ptr lattice, int subs)
{
  return lattice->periodic_x[subs];
}

int is_periodic_in_y( lattice_ptr lattice, int subs)
{
  return lattice->periodic_y[subs];
}

int get_ns_flag( lattice_ptr lattice) { return lattice->param.ns_flag;}

int get_slice_x( lattice_ptr lattice) { return lattice->param.slice_x;}

int get_slice_y( lattice_ptr lattice) { return lattice->param.slice_y;}

int make_octave_scripts( lattice_ptr lattice)
{
  // TODO: return value based on (pending) "struct param_struct" entry.
  return lattice->param.make_octave_scripts;
}

#if INAMURO_SIGMA_COMPONENT
int bc_sigma_walls( lattice_ptr lattice)
   { return lattice->param.bc_sigma_walls;}
#endif /* (INAMURO_SIGMA_COMPONENT) */

int get_buoyancy( lattice_ptr lattice)
  { return lattice->param.buoyancy;}

int get_buoyancy_flag( lattice_ptr lattice)
  { return ( lattice->param.buoyancy!=0)?(1):(0); }

int get_buoyancy_sign( lattice_ptr lattice)
  { return ( lattice->param.buoyancy!=0)
          ?((lattice->param.buoyancy>0)?(1):(-1))
          :(0); }
int get_buoy_subs( lattice_ptr lattice)
{
  return lattice->param.buoy_subs;
}

int use_slice_dot_in_file( lattice_ptr lattice)
{
  if( get_slice_x(lattice) < 0 && get_slice_y(lattice) < 0)
  {
    return 1;
  }
  else
  {
    return 0;
  }

} /* int use_slice_dot_in_file( lattice_ptr lattice) */

int do_user_stuff( lattice_ptr lattice)
{
  return lattice->param.do_user_stuff;
}

int do_check_point_save( lattice_ptr lattice)
{
  // Later, have a check point frequency input from params.in.
  return 0;
}

int do_check_point_load( lattice_ptr lattice)
{
  FILE *o;
  char filename[1024];

  sprintf(filename, "./out/checkpoint.dat");
  if( o=fopen(filename,"r"))
  {
    fclose(o);
    // Later, have a toggle switch input from params.in.
    return 0;
  }
  else
  {
    return 0;
  }
}

double get_rhoij( lattice_ptr lattice, int i, int j, int subs)
{
  return lattice->macro_vars[subs][IJ2N(i,j)].rho;
}
double get_rho( lattice_ptr lattice, int i, int j, int subs)
{
  // TODO: Deprecate this function.
  return get_rhoij( lattice, i, j, subs);
}
double get_rhon( lattice_ptr lattice, int n, int subs)
{
  return lattice->macro_vars[subs][n].rho;
}

const char* get_out_path( lattice_ptr lattice)
{
  return lattice->param.out_path;
}

double* pressure_n_in0( lattice_ptr lattice, int subs)
{
  return lattice->bcs_in[subs].pressure_n_in0;
}

double** pressure_n_in0_ptr( lattice_ptr lattice, int subs)
{
  return &( lattice->bcs_in[subs].pressure_n_in0);
}

int num_pressure_n_in0( lattice_ptr lattice, int subs)
{
  return lattice->bcs_in[subs].num_pressure_n_in0;
}

int* num_pressure_n_in0_ptr( lattice_ptr lattice, int subs)
{
  return &( lattice->bcs_in[subs].num_pressure_n_in0);
}

double* pressure_s_in0( lattice_ptr lattice, int subs)
{
  return lattice->bcs_in[subs].pressure_s_in0;
}

double** pressure_s_in0_ptr( lattice_ptr lattice, int subs)
{
  return &( lattice->bcs_in[subs].pressure_s_in0);
}

int num_pressure_s_in0( lattice_ptr lattice, int subs)
{
  return lattice->bcs_in[subs].num_pressure_s_in0;
}

int* num_pressure_s_in0_ptr( lattice_ptr lattice, int subs)
{
  return &( lattice->bcs_in[subs].num_pressure_s_in0);
}
