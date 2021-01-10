//##############################################################################
//
// Copyright (C), 2005, Michael Sukop and Danny Thorne
//
// lb2d_prime.c
//
//  - Lattice Boltzmann
//

#include "lb2d_prime.h"

int main( int argc, char **argv)
{
  int n;
  double k;
  int time, frame;
  clock_t tic, ticf;
  double t0, t1, tf;
  int start_frame;

  struct lattice_struct *lattice;

  struct report_struct report;

//LBMPI #if PARALLEL
//LBMPI   lbmpi_ptr lbmpi;
//LBMPI #endif /* (PARALLEL) */

  setbuf( stdout, (char*)NULL); // Don't buffer screen output.

// TODO: OSTYPE is not defined...?
//#if OSTYPE==darwin
//  printf("%s %d >> Darwin \n",__FILE__,__LINE__);
//#endif /* (OSTYPE=="DARWIN") */

  // Allocate the lattice structure.
  lattice = ( struct lattice_struct*)malloc( sizeof(struct lattice_struct));

//LBMPI #if PARALLEL
//LBMPI   // Allocate the lbmpi structure.
//LBMPI   lbmpi = ( struct lbmpi_struct*)malloc( sizeof(struct lbmpi_struct));
//LBMPI
//LBMPI   // Give the lattice a pointer to the lbmpi structure.
//LBMPI   lattice->lbmpi = lbmpi;
//LBMPI #endif /* (PARALLEL) */

  tic = clock();

  construct_lattice( &lattice, argc, argv);

  report_flags(lattice);

//LBMPI #if PARALLEL
//LBMPI   lbmpi_construct( lbmpi, lattice, argc, argv);
//LBMPI #endif /* (PARALLEL) */

  lattice->time = 0;
  lattice->frame = 0;

  init_problem( lattice);

  output_frame( lattice);

  printf("\n");

  t0 = ((double)clock() - (double)tic)/(double)CLK_TCK;
  printf("Overhead time:  %f seconds\n", t0);

  tic = clock();
  ticf = clock();

  if( do_check_point_load( lattice)){ check_point_load( lattice);}
  start_frame=lattice->frame;

  if( do_user_stuff(lattice)) { user_stuff_pre_frames(lattice);}


  read_PEST_in_files( &lattice, argc, argv);

  for( frame = start_frame+1; frame<=get_NumFrames( lattice); frame++)
  {
    lattice->frame = frame;

    if( do_user_stuff(lattice)) { user_stuff_pre_times(lattice); }

    for( time = (frame-1)*get_FrameRate(lattice)+1;
         time<= frame*get_FrameRate(lattice); time++)
    {
      lattice->time = time;                                run_man( lattice);

      write_PEST_out_data( &lattice, argc, argv);

      stream( lattice); /* ftemp <- f */                   dump( lattice, 1);
//LBMPI #if PARALLEL
//LBMPI       lbmpi_communicate( lbmpi, lattice);
//LBMPI #endif /* (PARALLEL) */
      bcs( lattice);                                       dump( lattice, 2);
      compute_macro_vars( lattice, /*which_f=*/ 1); // solute/buoyancy/grav
      //compute_macro_vars( lattice, /*which_f=*/ 1);
      compute_feq( lattice, 1);                            dump( lattice, 3);
      collide( lattice); /* f <- ftemp */

      if( do_user_stuff(lattice)) { user_stuff_time(lattice); }

    } /* for( time=frame*get_FrameRate(lattice); ... */

    if( do_user_stuff(lattice)) { user_stuff_post_times(lattice); }

#if POROUS_MEDIA || FREED_POROUS_MEDIA
    // Before application of the ns term, f after collision is stored in ftemp
    // so which_f=1.
    compute_macro_vars( lattice, /*which_f=*/ 2);
#else /* !( POROUS_MEDIA) */
    // After collision so use f (which_f=0).
#if GUO_ZHENG_SHI_BODY_FORCE
    compute_macro_vars( lattice, /*which_f=*/ 2); // solute/buoyancy/grav
#else
    compute_macro_vars( lattice, /*which_f=*/ 2); // solute/buoyancy/grav
#endif
    //compute_macro_vars( lattice, /*which_f=*/ 0);
#endif /* POROUS_MEDIA */

    output_frame( lattice);

    if( do_check_point_save( lattice)){ check_point_save( lattice);}

    tf = ((double)clock() - (double)ticf)/(double)CLK_TCK;
    ticf = clock();

    if( do_user_stuff(lattice)) { user_stuff_frame(lattice); }

    printf("Time for frame: %f\n", tf);

  } /* for( frame = 0; frame<get_NumFrames( lattice); time++) */

  write_PEST_out_file( &lattice, argc, argv);

  if( do_user_stuff(lattice)) { user_stuff_post_frames(lattice);}

  t1 = ((double)clock() - (double)tic)/(double)CLK_TCK;

  report_open(          &report, "./out/report");
  report_ratio_entry(   &report,"Overhead time", t0, 1., "seconds");
  report_ratio_entry(   &report,"Time in loop", t1, 1., "seconds");
  report_ratio_entry(   &report,"Time in loop", t1, 60., "minutes");
  report_ratio_entry(   &report,"Relative overhead time", 100.*t0, t1, "%");
  report_integer_entry( &report,"Number of frames", frame, "");
  report_ratio_entry(   &report,"Average time per frame",
                         t1, (double)frame , "seconds");
  report_integer_entry( &report,"Number of timesteps", --time, "");
  report_ratio_entry(   &report,"Average time per timestep",
                         t1, (double)time, "seconds");
  report_partition(     &report);
  report_integer_entry( &report,"Size of lattice structure",
                         get_sizeof_lattice_structure( lattice), "bytes");
  report_integer_entry( &report,"Size of lattice",
                         get_sizeof_lattice( lattice), "bytes");
  report_ratio_entry(   &report,"Size of lattice",
                         get_sizeof_lattice( lattice),
                         pow(2.,20.),
                         "MB");
  report_ratio_entry(   &report,"Percentage of active nodes",
                         100.*(double)get_num_active_nodes( lattice),
                         (double)lattice->NumNodes,
                         "%");
  report_close(         &report);

#if INAMURO_SIGMA_COMPONENT
  dump_sigma_btc( lattice);
#endif /* INAMURO_SIGMA_COMPONENT */

  destruct_lattice( lattice);

#if VERBOSITY_LEVEL > 0
  printf("\n");
  printf("lb2d_prime.c: main() -- Terminating normally.\n");
  printf("\n");
#endif /* VERBOSITY_LEVEL > 0 */

  return 0;

} /* int main( int argc, char **argv) */
