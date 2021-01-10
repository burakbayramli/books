//##############################################################################
//
// Copyright (C), 2005, Michael Sukop and Danny Thorne
//
// runman.c
//
//  - Run-time management
//

//void manage_body_force( lattice_ptr lattice)
//##############################################################################
//
// M A N A G E   B O D Y   F O R C E
//
//  - Reduce gravity to avoid run-away velocities and numerical
//    instability, if lattice->param.end_grav is specified.
//
inline void manage_body_force( lattice_ptr lattice)
{
#if MANAGE_BODY_FORCE
  int subs;
  double u[2];
  double s;

  compute_ave_u( lattice, u, 0);
  s = sqrt( u[0]*u[0] + u[1]*u[1]);
  if( s > .0091 )
  {
    lattice->param.gval[0][0] = 0.;
  }
  else
  {
    lattice->param.gval[0][0] = 0.00001;
  }

  for( subs=0; subs<NUM_FLUID_COMPONENTS; subs++)
  {
   if( lattice->param.end_grav[subs] > 0.)
   {
    if( lattice->time >= lattice->param.end_grav[subs])
    {
      if( lattice->param.gval[subs][0]<EPS)
      {
        lattice->param.gval[subs][0] = 0.;
      }
      else
      {
        lattice->param.gval[subs][0]/=2.;
#if VERBOSITY_LEVEL>0
printf("subs %02d: gval_x = %f\n", subs, lattice->param.gval[subs][0]);
#endif /* VERBOSITY_LEVEL>0 */
      }

      if( lattice->param.gval[subs][1]<EPS)
      {
        lattice->param.gval[subs][1] = 0.;
      }
      else
      {
        lattice->param.gval[subs][1]/=2.;
#if VERBOSITY_LEVEL>0
printf("subs %02d: gval_y = %f\n", subs, lattice->param.gval[subs][1]);
#endif /* VERBOSITY_LEVEL>0 */
      }

    } /* if( lattice->time>=lattice->param.end_grav[subs]) */
   } /* if( lattice->param.end_grav[subs] > 0.) */
  } /* for( subs=0; subs<NUM_FLUID_COMPONENTS; subs++) */

#endif /* MANAGE_BODY_FORCE */
} /* void manage_body_force( lattice_ptr lattice) */

inline void run_man( lattice_ptr lattice)
{
    manage_body_force( lattice);
#if INAMURO_SIGMA_COMPONENT && STORE_BTC
    sigma_stuff( lattice);
#endif /* INAMURO_SIGMA_COMPONENT && STORE_BTC */
}

inline void dump( lattice_ptr lattice, int dump_num)
{
#if WRITE_PDF_DAT_FILES
  int time=get_time(lattice);
  if( 1 && time>=DELAY && !(time%get_FrameRate(lattice)))
  {
    dump_pdf( lattice, get_FrameRate(lattice)*1000*dump_num + time);
  }
#endif /* WRITE_PDF_DAT_FILES */
}
