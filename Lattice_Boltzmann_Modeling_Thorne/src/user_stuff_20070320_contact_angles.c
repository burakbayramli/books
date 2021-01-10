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

// void compute_drop( lattice_ptr lattice)
//
// Compute the width and height of the drop.
//
void compute_drop( lattice_ptr lattice)
{
  int i, j;
  double rho;
  const double rho_v=85.7042;
  const double rho_l=524.3905;
  const double rho_min=100.;
  //const double rho_cut=450.;
  const double rho_cut=(rho_v+rho_l)/2.;
  //const double rho_cut=212.36;

  //psi(rho_l)=4.*exp(-200/524.3905)
  //psi(rho_v)=4.*exp(-200/85.7042)

  int width=0, width_temp=0, max_width=0;
  int height=0;
  double theta;
  double max_rho=0.;

  for( j=get_LY(lattice)-1; j>=0; j--)
  {
    max_rho=0.;
    for( i=0; i<get_LX( lattice); i++)
    //for( i=0; i<170; i++)
    {
      //printf("%f ",get_rho(lattice,i,/*j=*/2,/*subs=*/0));
      if( max_rho < fabs( get_rho(lattice,i,/*j=*/j,/*subs=*/0)))
      {   max_rho = fabs( get_rho(lattice,i,/*j=*/j,/*subs=*/0)); }
    }
    if( max_rho > rho_min)
    {
      if( max_rho >= rho_cut){ height++;}
      width_temp=0;
      for( i=0; i<get_LX( lattice); i++)
      //for( i=0; i<170; i++)
      {
        if(/*ascii display of the drop*/0)
        {
          if( is_solid_node( lattice, /*subs=*/0, IJ2N(i,j)))
          {
            printf("%%");
            if( j!=0)
            {
              printf("%s %d >> WARNING: "
                  "Function compute_drop() is designed to compute the "
                  "width and height of a drop forming on a "
                  "surface at the bottom of the domain. "
                  "Solid detected at (%d,%d) does not conform to that "
                  "configuration.\n",
                  __FILE__, __LINE__, i, j );
            }
          }
          else
          {
            if(get_rho(lattice,i,/*j=*/j,/*subs=*/0) > rho_cut)
            { printf("X"); width_temp++;}
            else if(get_rho(lattice,i,/*j=*/j,/*subs=*/0) > rho_min)
            { printf("x"); }
            else
            { printf("-"); }
          }
        }
        else
        {
          if(get_rho(lattice,i,/*j=*/j,/*subs=*/0) > rho_cut)
          { width_temp++;}
        }
      }
      if( width_temp > 0)
      {
        width = width_temp;
        if( max_width < width)
        {
          max_width = width;
        }
      }
      if(/*ascii display of the drop*/0) { printf("\n");}
    }
  }
  printf("width=%d, height=%d, ", width, height);
  printf("width/height=%f\n", (double)width/(double)height);
  printf("max_width=%d\n", max_width);
  printf("\n");

  if( height > width)
  {
    // TODO: Need to double check this formula:
    theta = PI/2. + atan( ((double)height/(double)width)
                        - ((double)width/(4.*(double)height)));
  }
  else if( height < width)
  {
    // TODO: Need to double check this formula:
    theta = atan( 1./( ((double)width/(4.*(double)height))
                     - ((double)height/(double)width)      ));
  }
  else if( height == width)
  {
    theta = PI/2.;
  }
  else
  {
    printf("%s %d >> Unhandled case: height=%d, width=%d\n",
      __FILE__, __LINE__, height, width);
    theta = 999.;
  }

  printf("theta = %f\n", theta);

  printf("\n");
  printf("rho_cut = %f\n", rho_cut);
  printf("psi(rho_cut) = %f\n", 4.*exp(-200./rho_cut));
  printf("psi(rho_v) = %f\n", 4.*exp(-200./85.7042));
  printf("psi(rho_l) = %f\n", 4.*exp(-200./524.3905));
  printf("psi((rho_l+rho_v)/2.) = %f\n", 4.*exp(-200./((85.7042+524.3905)/2.)));
  printf("(psi(rho_v)+psi(rho_l))/2. = %f\n",
    (4.*exp(-200./85.7042)+4.*exp(-200./524.3905))/2.);
  rho = 212.36; printf("psi(%f) = %f\n", rho, 4.*exp(-200./rho));

  //4.*exp(-200/524.3905)
  //4.*exp(-200/85.7042)

} /* void compute_drop( lattice_ptr lattice) */

// void user_stuff_pre_frames( lattice_ptr lattice)
//
//  This function is called before the frame loop in lb2d_prime.c.
//
void user_stuff_pre_frames( lattice_ptr lattice)
{
  compute_drop( lattice);

} /* void user_stuff_pre_frames( lattice_ptr lattice) */

// void user_stuff_frame( lattice_ptr lattice)
//
// If lattice->param.do_user_stuff is on, this function is called at the
// end of each frame in lb2d_prime.c.
//
void user_stuff_frame( lattice_ptr lattice)
{
} /* void user_stuff_frame( lattice_ptr lattice) */

// void user_stuff_post_frames( lattice_ptr lattice)
//
// If lattice->param.do_user_stuff is on, this function is called after
// the frame loop in lb2d_prime.c.
//
void user_stuff_post_frames( lattice_ptr lattice)
{
  compute_drop( lattice);

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
} /* void user_stuff_time( lattice_ptr lattice) */

// void user_stuff_post_times( lattice_ptr lattice)
//
// If lattice->param.do_user_stuff is on, this function is called after
// the time loop in lb2d_prime.c.
//
void user_stuff_post_times( lattice_ptr lattice)
{
  compute_drop( lattice);

} /* void user_stuff_post_times( lattice_ptr lattice) */
