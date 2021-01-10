//##############################################################################
//
// user_stuff.c
//
//  - This file has function definitions of functions that will be called
//    at specified times (see individual function documentation) if the
//    lattice->param.do_user_stuff flag is on.
//
//  - The user can fill these routines with whatever they want.
//

// void user_stuff_pre_frames( lattice_ptr lattice)
//
//  This function is called before the frame loop in lb2d_prime.c.
//
void user_stuff_pre_frames( lattice_ptr lattice)
{

  lattice->user_stuff->rho_c_start   = 83.70420;
  lattice->user_stuff->rho_c_reverse = 85.70420;
  lattice->user_stuff->rho_c_inc     =
    ( lattice->user_stuff->rho_c_reverse
    - lattice->user_stuff->rho_c_start   )/15.;//0.2138;

  lattice->user_stuff->tol = .000001;

  compute_ave_rho( lattice, &(lattice->user_stuff->rho_ave_prev), 0);
  compute_ave_u  ( lattice, lattice->user_stuff->u_ave_prev  , 0);

  lattice->user_stuff->rho_c = lattice->user_stuff->rho_c_start;

  lattice->param.rho_in  =
    lattice->user_stuff->rho_c;
  lattice->param.rho_out =
    lattice->user_stuff->rho_c;

  lattice->user_stuff->o = fopen( "rho_c_frames.txt", "w+");
  fprintf(
    lattice->user_stuff->o,
    "%04d %f %f\n",
    /*frames*/0,
    lattice->user_stuff->rho_c,
    lattice->user_stuff->rho_c);
  fclose(lattice->user_stuff->o);

  printf("\n\n%s %d >> frame %4d: Boundary pressure, rho_c = %f\n\n",
      __FILE__, __LINE__, lattice->frame, lattice->param.rho_in);

} /* void user_stuff_pre_frames( lattice_ptr lattice) */

// void user_stuff_frame( lattice_ptr lattice)
//
// If lattice->param.do_user_stuff is on, this function is called at the
// end of each frame in lb2d_prime.c.
//
void user_stuff_frame( lattice_ptr lattice)
{
  double drho_ave;
  double du_ave[2];
  double rho_c0;

  compute_ave_rho( lattice, &(lattice->user_stuff->rho_ave), 0);
  compute_ave_u  ( lattice, lattice->user_stuff->u_ave, 0);

  printf("\n");
  printf("  rho_ave      = %f\n", lattice->user_stuff->rho_ave);
  printf("  rho_ave_prev = %f\n", lattice->user_stuff->rho_ave_prev);

  drho_ave  = fabs( lattice->user_stuff->rho_ave
                  - lattice->user_stuff->rho_ave_prev);
  du_ave[0] = fabs( lattice->user_stuff->u_ave[0]
                  - lattice->user_stuff->u_ave_prev[0]);
  du_ave[1] = fabs( lattice->user_stuff->u_ave[1]
                  - lattice->user_stuff->u_ave_prev[1]);

  printf("| rho_ave  - rho_ave_prev  | = %f\n", drho_ave);
  printf("| u_ave[0] - u_ave_prev[0] | = %f\n", du_ave[0]);
  printf("| u_ave[1] - u_ave_prev[1] | = %f\n", du_ave[1]);

  if( drho_ave  < lattice->user_stuff->tol
   && du_ave[0] < lattice->user_stuff->tol
   && du_ave[1] < lattice->user_stuff->tol)
  {
    rho_c0 = lattice->user_stuff->rho_c;

    if( lattice->user_stuff->rho_c < lattice->user_stuff->rho_c_reverse)
    {
      printf("\n\n%s %d >> Bing! Incrementing rho_c.\n\n", __FILE__, __LINE__);

      lattice->user_stuff->rho_c += lattice->user_stuff->rho_c_inc;
    }
    else if( lattice->user_stuff->rho_c_reverse > 0.)
    {
      printf("\n\n%s %d >> Bing! Decrementing rho_c.\n\n", __FILE__, __LINE__);

      lattice->user_stuff->rho_c_reverse = 0.;
      lattice->user_stuff->rho_c -= lattice->user_stuff->rho_c_inc;
    }
    else
    {
      if( lattice->user_stuff->rho_c >= lattice->user_stuff->rho_c_reverse)
      {
        printf("\n\n%s %d >> Bing! Decrementing rho_c.\n\n",
          __FILE__, __LINE__);
        lattice->user_stuff->rho_c -= lattice->user_stuff->rho_c_inc;
      }
      else
      {
        printf("\n\n%s %d >> WARNING: rho_c == 0.\n\n", __FILE__, __LINE__);
        lattice->user_stuff->rho_c = 0.;
      }
    }

    lattice->param.rho_in  = lattice->user_stuff->rho_c;
    lattice->param.rho_out = lattice->user_stuff->rho_c;

    lattice->user_stuff->o = fopen( "rho_c_frames.txt", "a+");
    fprintf( lattice->user_stuff->o, "%04d %f %f\n",
      get_frame( lattice),
      rho_c0,
      lattice->user_stuff->rho_c);
    fclose( lattice->user_stuff->o);

  } /* if( drho_ave  < lattice->user_stuff->tol && ... */

  lattice->user_stuff->rho_ave_prev = lattice->user_stuff->rho_ave;
  lattice->user_stuff->u_ave_prev[0]   = lattice->user_stuff->u_ave[0];
  lattice->user_stuff->u_ave_prev[1]   = lattice->user_stuff->u_ave[1];

  printf("  rho_ave      = %f\n", lattice->user_stuff->rho_ave);
  printf("  rho_ave_prev = %f\n", lattice->user_stuff->rho_ave_prev);

  //t1 = ((double)clock() - (double)tic)/(double)CLK_TCK;
  //printf("Resuming at time %f\n", t1);

  printf("\n\n%s %d >> frame %4d: Boundary pressure, rho_c = %f\n\n",
      __FILE__, __LINE__, lattice->frame, lattice->param.rho_in);

} /* void user_stuff_frame( lattice_ptr lattice) */

// void user_stuff_post_frames( lattice_ptr lattice)
//
// If lattice->param.do_user_stuff is on, this function is called after
// the frame loop in lb2d_prime.c.
//
void user_stuff_post_frames( lattice_ptr lattice)
{
} /* void user_stuff_post_frames( lattice_ptr lattice) */

// void user_stuff_pre_times( lattice_ptr lattice)
//
// If lattice->param.do_user_stuff is on, this function is called before
// the time loop in lb2d_prime.c.
//
void user_stuff_pre_times( lattice_ptr lattice)
{
} /* void user_stuff_pre_times( lattice_ptr lattice) */

// void user_stuff_time( lattice_ptr lattice)
//
// If lattice->param.do_user_stuff is on, this function is called at the
// end of each time loop in lb2d_prime.c.
//
void user_stuff_time( lattice_ptr lattice)
{
  if( is_last_step_of_frame( lattice))
  {
    compute_ave_rho( lattice, &(lattice->user_stuff->rho_ave_prev), 0);
    compute_ave_u  ( lattice, lattice->user_stuff->u_ave_prev, 0);
    printf("  rho_ave_prev = %f\n", lattice->user_stuff->rho_ave_prev);
  }

} /* void user_stuff_time( lattice_ptr lattice) */

// void user_stuff_post_times( lattice_ptr lattice)
//
// If lattice->param.do_user_stuff is on, this function is called after
// the time loop in lb2d_prime.c.
//
void user_stuff_post_times( lattice_ptr lattice)
{
} /* void user_stuff_post_times( lattice_ptr lattice) */
