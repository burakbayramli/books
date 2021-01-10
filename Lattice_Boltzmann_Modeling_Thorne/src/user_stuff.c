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

double theta_of_height_width( const double height, const double width)
{
  double theta;

  if( height > width/2.)
  {
    // TODO: Need to double check this formula:
    theta = PI/2. + atan( ((double)height/(double)width)
                        - ((double)width/(4.*(double)height)));
  }
  else if( height < width/2.)
  {
    // TODO: Need to double check this formula:
    theta = atan( 1./( ((double)width/(4.*(double)height))
                     - ((double)height/(double)width)      ));
  }
  else if( height == width/2.)
  {
    theta = PI/2.;
  }
  else
  {
    printf("%s %d >> Unhandled case: height=%f, width=%f\n",
      __FILE__, __LINE__, height, width);
    theta = 999.; // Bogus value instead of process_exiting.
  }

  return theta;

} // double theta_of_height_width( const double height, const double width)

void ascii_display_of_the_drop(
       lattice_ptr lattice, int j, int rho_cut, int rho_min)
{
  double rho;
  const double rho_v=85.7042;
  const double rho_l=524.3905;
  int i, max_i = 80;
  max_i = (max_i > get_LX(lattice))?( get_LX(lattice)):(max_i);

  for( i=0; i<max_i; i++)
  {
    if( is_solid_node( lattice, /*subs=*/0, IJ2N(i,j)))
    {
      printf("%2d",i%100);
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
      rho = get_rho(lattice,i,j,/*subs=*/0);

      if( rho > rho_l)
      { printf("++"); }
      else if( rho > rho_cut)
      { printf("Xx"); }
      else if( rho > rho_min)
      { printf("o "); }
      else if( rho < rho_v)
      { printf("--"); }
      else
      { printf(". "); }
    }
  }
  printf("%3d\n",j);

} // void ascii_display_of_the_drop( lattice_ptr lattice, int j, ...

// void compute_drop( lattice_ptr lattice)
//
// Compute the width and height of the drop. (SCMP)
//
void compute_drop(
       lattice_ptr lattice,
       int output_to_file,
       int jbase,
       double rho_cut,
       int header)
{
  int i, j;

  double rho;

  const double rho_v=85.7042;
  const double rho_l=524.3905;

  const double psi_v = 4.*exp(-200./(rho_v));
  const double psi_l = 4.*exp(-200./(rho_l));
  const double psi_cut=(psi_v+psi_l)/2.;

  //const double rho_cut=(rho_v+rho_l)/2.;
  //const double rho_cut= -200./log(.25*psi_cut);
  //const double rho_cut=450.;
  //const double rho_cut=100.;

  double i1, i2, icut_left, icut_right;
  double j1, j2, jcut;
  double rho1, rho2;

  // INPUTE PARAM NOW: const int jbase = 4;
  //  jbase frame_rate  theta_low       theta  theta_high
  //  ----- ---------- ----------  ----------  ----------
  //      1        100  91.909683   93.327450   94.732496
  //      1       1000  90.947016   92.357518   93.755755
  //      2        100  90.962932   92.396963   93.818305
  //      2       1000  90.000000   91.426377   92.840531
  //      3        100  89.037068   90.479460   91.909683
  //      3       1000  89.037068   90.479460   91.909683
  //      4        100  88.057956   89.516494   90.962932
  //      4       1000  88.057956   89.516494   90.962932

  const double rho_min=100.;
  //const double rho_min=rho_v;

  //psi(rho_l)=4.*exp(-200/524.3905)
  //psi(rho_v)=4.*exp(-200/85.7042)

  int width=0, width_temp=0, max_width=0, max_width_j=0;
  int height=0;
  double theta;
  double max_rho=0.;
  int imax;
  int imax1;
  int imax2;

  double G = get_G( lattice);
  double Gads = get_Gads( lattice, /*subs*/0);

  jcut = -1.; // Flag value.

  // Start at about 3/4 the height of the domain to skip over any condensing
  // blobs on the ceiling. Main drop being measured should not be that high.
  for( j=(int)floor((3./4.)*get_LY(lattice)); j>0; j--)
  {
    max_rho=0.;
    for( i=0; i<get_LX( lattice); i++)
    { rho = get_rho(lattice,i,/*j=*/j,/*subs=*/0);
      if( max_rho < fabs(rho))
      {   max_rho = fabs(rho); imax = i; } }

    if( /*row j crosses the drop*/ max_rho > rho_min)
    {
      if( max_rho > rho_cut && jcut < 0.)
      {
        // rho1 was assigned max_rho from row j-1.
        // j1 was assigned max_rho from row j-1.
        rho2 = max_rho;
        imax2 = imax;
        j2 = j;
        // Linear interpolation of jcut:
        jcut = j1 + ((j2-j1)/(rho2-rho1))*( rho_cut - rho1);
      }

      if( j>=jbase)
      {
        if( max_rho >= rho_cut){ height++;}
        width_temp=0;

        icut_left = -1;
        icut_right = -1;
        for( i=0; i<get_LX(lattice); i++)
        {
          rho = get_rho(lattice,i,j,/*subs=*/0);
          if( rho > rho_cut)
          {
            if( icut_left < 0. )
            {
              // i1 and rho1 were set at i-1.
              rho2 = rho;
              i2 = i;
              // Linear interpolation of icut_left:
              icut_left = i1 + ((i2-i1)/(rho2-rho1))*( rho_cut - rho1);
            }
            else { rho1 = rho; i1 = i; /*save for interp of icut_right*/}

            width_temp++;
          }
          else
          {
            if( icut_left < 0.)
            {
              i1 = i; rho1 = rho; /*save for interp of icut_left*/
            }
            else
            {
              if( icut_right < 0.)
              {
                // i1 and rho1 were set at i-1.
                rho2 = rho;
                i2 = i;
                // Linear interpolation of icut_right:
                icut_right = i1 + ((i2-i1)/(rho2-rho1))*( rho_cut - rho1);
              }
            }
          }
        }

        if( width_temp > 0)
        { width = width_temp;
          if( max_width < width) { max_width = width; max_width_j = j;} }
      }
    }
    else
    {
      rho1 = max_rho; // save rho1 to use for interpolation of jcut.
      imax1 = imax;
      j1 = j;
    }

  }
//for( i=0; i<get_LX( lattice); i++)
//{
//  printf("%f ",get_rho(lattice,i,/*j=*/max_width_j,/*subs=*/0));
//}

  if(/*verbose*/0)
  {
    printf("\n");
    printf("jbase = %d\n", jbase);
    printf("width=%d, height=%d, ", width, height);
    printf("width/height=%f, ", (double)width/(double)height);
    printf("max_width=%d at j=%d, ", max_width, max_width_j);

    theta = theta_of_height_width( jcut-jbase, icut_right-icut_left);
    printf("%f = %f^o ", theta, theta*(180./PI));

    theta = theta_of_height_width( height-1, width+1);
    printf("theta_low = %f = %f^o, ", theta, theta*(180./PI));

    theta = theta_of_height_width( height, width);
    printf("theta = %f = %f^o, ", theta, theta*(180./PI));

    theta = theta_of_height_width( height+1, width-1);
    printf("theta_high = %f = %f^o, ", theta, theta*(180./PI));

    theta = acos( ( ( psi_v - Gads/G) - ( Gads/G - psi_l) )/(psi_v-psi_l));
    printf("theta_predicted_youngs = %f = %f^o ", theta, theta*(180./PI));

    //theta = PI*( ( Gads/G - psi_l) / ( psi_v - psi_l));
    //printf("theta_predicted_combination = %f = %f^o ", theta, theta*(180./PI));

    printf("\n");
    printf("rho_cut = %f\n", rho_cut);
    printf("(icut_left,icut_right,jcut) = ( %9.6f, %9.6f, %9.6f); "
        "(imax1,imax2)=(%d,%d); (icut_left+icut_right)/2 = %9.6f\n",
        icut_left, icut_right, jcut, imax1, imax2,(icut_left+icut_right)/2.);
    printf("psi(rho_cut) = %f\n", 4.*exp(-200./rho_cut));
    printf("psi(rho_v) = %f\n", 4.*exp(-200./85.7042));
    printf("psi(rho_l) = %f\n", 4.*exp(-200./524.3905));
    printf("psi((rho_l+rho_v)/2.) = %f\n", 4.*exp(-200./((85.7042+524.3905)/2.)));
    printf("(psi(rho_v)+psi(rho_l))/2. = %f\n",
        (4.*exp(-200./85.7042)+4.*exp(-200./524.3905))/2.);
    rho = 212.36; printf("psi(%f) = %f\n", rho, 4.*exp(-200./rho));
  }
  else
  {
    if( header)
    {
      printf("\n");
      printf(
                          " DROP "
          "                 jbase"
          "               rho_cut"
          "                 width"
          "                height"
          "       theta pixelated"
          "    theta interpolated"
          "       theta predicted"
          "\n");
      printf(
                          " DROP "
          " ---------------------"
          " ---------------------"
          " ---------------------"
          " ---------------------"
          " ---------------------"
          " ---------------------"
          " ---------------------"
          "\n");
    }

    printf(" DROP ");
    printf(" %21.17f", (double)jbase);
    printf(" %21.17f", rho_cut);

    printf(" %21.17f", icut_right-icut_left);
    printf(" %21.17f", jcut - jbase);

    theta = theta_of_height_width( height, width);
    printf(" %21.17f", theta*(180/PI));

    theta = theta_of_height_width( jcut-jbase, icut_right-icut_left);
    printf(" %21.17f", theta*(180/PI));

    theta = acos( ( ( psi_v - Gads/G) - ( Gads/G - psi_l) )/(psi_v-psi_l));
    printf(" %21.17f", theta*(180/PI));

    printf("\n");

  }

  if( output_to_file)
  {
    FILE *o;
    o = fopen( "compute_drop.dat", "a+");

    fprintf( o, "%d ", get_NumFrames( lattice));
    fprintf( o, "%d ", get_FrameRate( lattice));
    fprintf( o, "%8.4f ", get_G( lattice));
    fprintf( o, "%8.4f ", get_Gads( lattice, /*subs*/0));
    fprintf( o, "%d ", jbase);
    fprintf( o, "%d ", width);
    fprintf( o, "%d ", height);

    theta = theta_of_height_width( jcut-jbase, icut_right-icut_left);
    fprintf( o, "%20.17f ", theta*(180./PI));

    theta = theta_of_height_width( height-1, width+1);
    fprintf( o, "%20.17f ", theta*(180./PI));
    theta = theta_of_height_width( height, width);
    fprintf( o, "%20.17f ", theta*(180./PI));
    theta = theta_of_height_width( height+1, width-1);
    fprintf( o, "%20.17f ", theta*(180./PI));

    theta = acos( ( ( psi_v - Gads/G) - ( Gads/G - psi_l) )/(psi_v-psi_l));
    //fprintf( o, "acos(%20.17f) = ", ( ( psi_v - Gads/G) - ( Gads/G - psi_l) )/(psi_v-psi_l));
    fprintf( o, "%20.17f ", theta*(180./PI));

    //theta = PI*( ( Gads/G - psi_l) / ( psi_v - psi_l));
    //fprintf( o, "%20.17f ", theta*(180./PI));

    fprintf( o, "\n");
    fclose(o);
  }

  //4.*exp(-200/524.3905)
  //4.*exp(-200/85.7042)

} /* void compute_drop( lattice_ptr lattice, bool output_to_file) */

void compute_sequence_of_drops( lattice_ptr lattice, int fileio)
{
  double rho_v=85.7042;
  double rho_l=524.3905;

  const double psi_v = 4.*exp(-200./(rho_v));
  const double psi_l = 4.*exp(-200./(rho_l));
  const double psi_mid = (psi_v+psi_l)/2.;
  double psi_range = psi_l - psi_v;
  int num_psi_intervals = 4;
  int half_psi_intervals = num_psi_intervals/2;
  double dpsi = psi_range / num_psi_intervals;

  double rho_cut;
  double psi_cut;

  int jbase;
  int min_jbase = 2;
  int max_jbase = 6;

  for( psi_cut =  psi_mid - half_psi_intervals*dpsi;
       psi_cut <= psi_mid + half_psi_intervals*dpsi;
       psi_cut += dpsi)
  {
    for( jbase = min_jbase; jbase <= max_jbase; jbase++)
    {
      rho_cut = -200./log(.25*psi_cut);
      compute_drop( lattice, fileio, jbase, rho_cut,
        /*display header?*/(   psi_cut == psi_mid-half_psi_intervals*dpsi
                            &&   jbase == min_jbase                      ));
    }
  }
}

// void user_stuff_pre_frames( lattice_ptr lattice)
//
//  This function is called before the frame loop in lb2d_prime.c.
//
void user_stuff_pre_frames( lattice_ptr lattice)
{
#if 0
  compute_drop( lattice, /*ascii*/1, /*file-io*/0, /*jbase*/1);
  compute_drop( lattice, /*ascii*/0, /*file-io*/0, /*jbase*/2);
  compute_drop( lattice, /*ascii*/0, /*file-io*/0, /*jbase*/3);
  compute_drop( lattice, /*ascii*/0, /*file-io*/0, /*jbase*/4);
  compute_drop( lattice, /*ascii*/0, /*file-io*/0, /*jbase*/5);
  compute_drop( lattice, /*ascii*/0, /*file-io*/0, /*jbase*/6);
#else
  //ascii_display_of_the_drop( lattice, j, rho_cut, rho_min);
  compute_sequence_of_drops( lattice, /*file-io*/0);
#endif

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
#if 0
  compute_drop( lattice, /*ascii*/1, /*file-io*/1, /*jbase*/1);
  compute_drop( lattice, /*ascii*/0, /*file-io*/1, /*jbase*/2);
  compute_drop( lattice, /*ascii*/0, /*file-io*/1, /*jbase*/3);
  compute_drop( lattice, /*ascii*/0, /*file-io*/1, /*jbase*/4);
  compute_drop( lattice, /*ascii*/0, /*file-io*/1, /*jbase*/5);
  compute_drop( lattice, /*ascii*/0, /*file-io*/1, /*jbase*/6);
#else
  //ascii_display_of_the_drop( lattice, j, rho_cut, rho_min);
  compute_sequence_of_drops( lattice, /*file-io*/1);
#endif


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
#if 0
  compute_drop( lattice, /*ascii*/1, /*file-io*/0, /*jbase*/1);
  compute_drop( lattice, /*ascii*/0, /*file-io*/0, /*jbase*/2);
  compute_drop( lattice, /*ascii*/0, /*file-io*/0, /*jbase*/3);
  compute_drop( lattice, /*ascii*/0, /*file-io*/0, /*jbase*/4);
  compute_drop( lattice, /*ascii*/0, /*file-io*/0, /*jbase*/5);
  compute_drop( lattice, /*ascii*/0, /*file-io*/0, /*jbase*/6);
#else
  //ascii_display_of_the_drop( lattice, j, rho_cut, rho_min);
  compute_sequence_of_drops( lattice, /*file-io*/0);
#endif


} /* void user_stuff_post_times( lattice_ptr lattice) */
