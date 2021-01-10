//##############################################################################
//
// Copyright (C), 2005, Michael Sukop and Danny Thorne
//
// collide.c
//

#if 0//POROUS_MEDIA
//##############################################################################
//
// void collide( lattice_ptr lattice)
//
void collide( lattice_ptr lattice)
{
  double *f;

  double omega;

  int    bc_type;

  int    n, a;

  int    subs;

  double ns;

  double *ftemp, *feq;

  double *nsterm;

  int    i,  j;
  int    ip, jp,
         in, jn;
  int    LX = lattice->param.LX,
         LY = lattice->param.LY;

#if SAY_HI
  printf("collide() -- Hi!\n");
#endif /* SAY_HI */

 for( subs=0; subs<NUM_FLUID_COMPONENTS; subs++)
 {
  for( n=0; n<lattice->NumNodes; n++)
  {
    feq   = lattice->pdf[subs][n].feq;
    f     = lattice->pdf[subs][n].f;
    ftemp = lattice->pdf[subs][n].ftemp;
    bc_type = lattice->bc[subs][n].bc_type;
#if ZHANG_AND_CHEN_ENERGY_TRANSPORT
    force = lattice->force[subs][n].force;
#endif /* ZHANG_AND_CHEN_ENERGY_TRANSPORT */

    if( !( bc_type & BC_SOLID_NODE))
    {
        // C O L L I D E

        // f = ftemp - (1/tau[subs])( ftemp - feq)
        for( a=0; a<=8; a++)
        {
#if 1
          f[a] = ftemp[a] - ( ( ftemp[a] / lattice->param.tau[subs] )
                            - ( feq[a]   / lattice->param.tau[subs] ) );
#else
          f[a] = ftemp[a] - ( ( ftemp[a] )
                            - ( feq[a]   ) ) / lattice->param.tau[subs];
#endif
        } /* for( a=0; a<=8; a++) */

#if ZHANG_AND_CHEN_ENERGY_TRANSPORT
        if( subs==0)
        {
          //
          // Add the body force term, equation (8),
          //
          //   f_i = f_i + \Delta f_i
          //
          //       = f_i + \frac{w_i}{T_0} c_i \dot F
          //
          // Assuming the weights, w_i, are the ones from compute_feq.
          //
          // Zhang & Chen state T_0 to be 1/3 for D3Q19.  The same in D2Q9.
          //

          f[1] += .00032*(vx[1]*3.*2.*force[0]);
          f[2] += .00032*(vy[2]*3.*2.*force[1]);
          f[3] += .00032*(vx[3]*3.*2.*force[0]);
          f[4] += .00032*(vy[4]*3.*2.*force[1]);
          f[5] += .00032*( 3.*( vx[5]*force[0] + vy[5]*force[1]));
          f[6] += .00032*( 3.*( vx[6]*force[0] + vy[6]*force[1]));
          f[7] += .00032*( 3.*( vx[7]*force[0] + vy[7]*force[1]));
          f[8] += .00032*( 3.*( vx[8]*force[0] + vy[8]*force[1]));
        }
#endif /* ZHANG_AND_CHEN_ENERGY_TRANSPORT */

#if PUKE_NEGATIVE_DENSITIES
        for( a=0; a<=8; a++)
        {
          if( *f < 0.)
          {
            printf("\n");
            printf(
              "collide() -- Node %d (%d,%d), subs %d, "
              "has negative density %20.17f "
              "in direction %d "
              "at timestep %d. Exiting!\n",
              n, n%lattice->param.LX,
                 n/lattice->param.LX,
                 subs,
                 f[a], a,
                 lattice->time             );
            printf("\n");
            process_exit(1);
          }
        } /* for( a=0; a<=8; a++) */
#endif /* PUKE_NEGATIVE_DENSITIES */

    } /* if( !( bc_type & BC_SOLID_NODE)) */

    else // bc_type & BC_SOLID_NODE
    {
      // B O U N C E B A C K

      if(   lattice->param.bc_slip_north
         && n >= lattice->NumNodes - lattice->param.LX)
      {
        // Slip condition on north boundary.
        /*
        //   A B C
        //    \|/           \|/
        //   D-o-E   -->   D-o-E
        //    /|\           /|\
        //                 A B C
        */
        f[1] = ftemp[1];
        f[2] = ftemp[4];
        f[3] = ftemp[3];
        f[4] = ftemp[2];
        f[5] = ftemp[8];
        f[6] = ftemp[7];
        f[7] = ftemp[6];
        f[8] = ftemp[5];

      } /* if(   lattice->param.bc_slip_north && ... ) */

      else
      {
        if( subs==0)
        {
          // Usual non-slip bounce-back condition.
          /*
          //   A B C         H G F
          //    \|/           \|/
          //   D-o-E   -->   E-o-D
          //    /|\           /|\
          //   F G H         C B A
          */
          f[1] = ftemp[3];
          f[2] = ftemp[4];
          f[3] = ftemp[1];
          f[4] = ftemp[2];
          f[5] = ftemp[7];
          f[6] = ftemp[8];
          f[7] = ftemp[5];
          f[8] = ftemp[6];

        } /* if( subs==0) */

#if NUM_FLUID_COMPONENTS==2
        else // subs==1
        {
#if INAMURO_SIGMA_COMPONENT
          if( lattice->param.bc_sigma_slip)
          {
            //
            // Slip BC for solute on side walls.
            // Will this make a difference on Taylor dispersion?
            //
            if( lattice->FlowDir == /*Vertical*/2)
            {
              if(   /*west*/(n  )%lattice->param.LX == 0
                 || /*east*/(n+1)%lattice->param.LX == 0)
              {
                // Slip condition on east/west boundary.
                /*
                //   A B C         C B A
                //    \|/           \|/
                //   D-o-E   -->   E-o-D
                //    /|\           /|\
                //   F G H         H G F
                */
                f[1] = ftemp[3];
                f[2] = ftemp[2];
                f[3] = ftemp[1];
                f[4] = ftemp[4];
                f[5] = ftemp[6];
                f[6] = ftemp[5];
                f[7] = ftemp[8];
                f[8] = ftemp[7];

              }
            }
            else if( lattice->FlowDir == /*Horizontal*/1)
            {
              if(   /*north*/ n >= lattice->NumNodes - lattice->param.LX
                 || /*south*/ n <  lattice->param.LX )
              {
                // Slip condition on north/south boundary.
                /*
                //   A B C         F G H
                //    \|/           \|/
                //   D-o-E   -->   D-o-E
                //    /|\           /|\
                //   F G H         A B C
                */
                f[1] = ftemp[1];
                f[2] = ftemp[4];
                f[3] = ftemp[3];
                f[4] = ftemp[2];
                f[5] = ftemp[8];
                f[6] = ftemp[7];
                f[7] = ftemp[6];
                f[8] = ftemp[5];

              }
              else
              {
                // ERROR: Solid exists somewhere other than as side walls.
                printf("%s (%d) >> "
                  "ERROR: "
                  "bc_sigma_slip is on. "
                  "FlowDir is determined to be horizontal. "
                  "Encountered solid node somewhere other than side walls. "
                  "That situation is not supported. "
                  "Exiting!", __FILE__, __LINE__);
                process_exit(1);
              }
            }
            else
            {
              printf("%s (%d) >> "
                "FlowDir is indeterminate. "
                "Cannot apply slip BC (bc_sigma_slip). "
                "Exiting!", __FILE__, __LINE__);
              process_exit(1);
            }

          } /* if( lattice->param.bc_sigma_slip) */

          else
          {
#endif /* INAMURO_SIGMA_COMPONENT */
            // Usual non-slip bounce-back condition.
            /*
            //   A B C         H G F
            //    \|/           \|/
            //   D-o-E   -->   E-o-D
            //    /|\           /|\
            //   F G H         C B A
            */
            f[1] = ftemp[3];
            f[2] = ftemp[4];
            f[3] = ftemp[1];
            f[4] = ftemp[2];
            f[5] = ftemp[7];
            f[6] = ftemp[8];
            f[7] = ftemp[5];
            f[8] = ftemp[6];
#if INAMURO_SIGMA_COMPONENT
          } /* if( lattice->param.bc_sigma_slip) else */
#endif /* INAMURO_SIGMA_COMPONENT */

        } /* if( subs==0) else*/
#endif /* NUM_FLUID_COMPONENTS==2 */

      } /* if(   lattice->param.bc_slip_north && ... ) else */

    } /* if( !( bc_type & BC_SOLID_NODE)) else */

  } /* for( n=0; n<lattice_NumNodes; n++) */

  if( INAMURO_SIGMA_COMPONENT!=0 || subs==0)
  {
    // Need separate temp space for this?
    nsterm = (double*)malloc( 9*lattice->NumNodes*sizeof(double));

    // Compute the solid density term for fluid component.
    for( n=0; n<lattice->NumNodes; n++)
    {
      bc_type = lattice->bc [subs][n].bc_type;

      i = n%LX;
      j = n/LX;

      jp = ( j<LY-1)?( j+1):( 0   );
      jn = ( j>0   )?( j-1):( LY-1);

      ip = ( i<LX-1)?( i+1):( 0   );
      in = ( i>0   )?( i-1):( LX-1);

      if( !( bc_type & BC_SOLID_NODE))
      {
        if( lattice->param.ns_flag == 0)
        {
          ns = lattice->param.ns;

  /* 1 */ nsterm[9*n+1] = ns*( lattice->pdf[subs][ j *LX + ip].f[3]
                             - lattice->pdf[subs][ j *LX + i ].f[1]);
  /* 2 */ nsterm[9*n+2] = ns*( lattice->pdf[subs][ jp*LX + i ].f[4]
                             - lattice->pdf[subs][ j *LX + i ].f[2]);
  /* 3 */ nsterm[9*n+3] = ns*( lattice->pdf[subs][ j *LX + in].f[1]
                             - lattice->pdf[subs][ j *LX + i ].f[3]);
  /* 4 */ nsterm[9*n+4] = ns*( lattice->pdf[subs][ jn*LX + i ].f[2]
                             - lattice->pdf[subs][ j *LX + i ].f[4]);
  /* 5 */ nsterm[9*n+5] = ns*( lattice->pdf[subs][ jp*LX + ip].f[7]
                             - lattice->pdf[subs][ j *LX + i ].f[5]);
  /* 6 */ nsterm[9*n+6] = ns*( lattice->pdf[subs][ jp*LX + in].f[8]
                             - lattice->pdf[subs][ j *LX + i ].f[6]);
  /* 7 */ nsterm[9*n+7] = ns*( lattice->pdf[subs][ jn*LX + in].f[5]
                             - lattice->pdf[subs][ j *LX + i ].f[7]);
  /* 8 */ nsterm[9*n+8] = ns*( lattice->pdf[subs][ jn*LX + ip].f[6]
                             - lattice->pdf[subs][ j *LX + i ].f[8]);
        }
        else /* ns_flag==1 || ns_flag==2 */
        {
          // Variable solid density.
          ns = lattice->ns[n].ns;
//printf("%s %d >> ns = %f\n",__FILE__,__LINE__,ns);

  /* 1 */ nsterm[9*n+1] = ns*( lattice->pdf[subs][ j *LX + ip].f[3]
                             - lattice->pdf[subs][ j *LX + i ].f[1]);
  /* 2 */ nsterm[9*n+2] = ns*( lattice->pdf[subs][ jp*LX + i ].f[4]
                             - lattice->pdf[subs][ j *LX + i ].f[2]);
  /* 3 */ nsterm[9*n+3] = ns*( lattice->pdf[subs][ j *LX + in].f[1]
                             - lattice->pdf[subs][ j *LX + i ].f[3]);
  /* 4 */ nsterm[9*n+4] = ns*( lattice->pdf[subs][ jn*LX + i ].f[2]
                             - lattice->pdf[subs][ j *LX + i ].f[4]);
  /* 5 */ nsterm[9*n+5] = ns*( lattice->pdf[subs][ jp*LX + ip].f[7]
                             - lattice->pdf[subs][ j *LX + i ].f[5]);
  /* 6 */ nsterm[9*n+6] = ns*( lattice->pdf[subs][ jp*LX + in].f[8]
                             - lattice->pdf[subs][ j *LX + i ].f[6]);
  /* 7 */ nsterm[9*n+7] = ns*( lattice->pdf[subs][ jn*LX + in].f[5]
                             - lattice->pdf[subs][ j *LX + i ].f[7]);
  /* 8 */ nsterm[9*n+8] = ns*( lattice->pdf[subs][ jn*LX + ip].f[6]
                             - lattice->pdf[subs][ j *LX + i ].f[8]);
        }

      } /* if( !( *bc_type++ & BC_SOLID_NODE)) */
    } /* for( n=0; n<lattice_NumNodes; n++) */

    for( n=0; n<lattice->NumNodes; n++)
    {
      f       = lattice->pdf[subs][n].f;
      bc_type = lattice->bc [subs][n].bc_type;

      if( !( bc_type & BC_SOLID_NODE))
      {
        for( a=1; a<9; a++)
        {
          // Store f in ftemp, because the compute_macro vars before
          // output_frames needs to have the pre-ns version of f.
          if( is_last_step_of_frame(lattice))
          {
            // This temp copy of f in ftemp prior to application
            // of ns only needs to be done before outputting a frame,
            // not after each timestep.
            lattice->pdf[subs][n].ftemp[a] = f[a];
          }
          f[a] += nsterm[9*n+a];

        } /* for( a=1; a<9; a++) */
      } /* if( !( bc_type & BC_SOLID_NODE)) */
    } /* for( n=0; n<lattice->NumNodes; n++, f+=18) */

    free( nsterm);

  } /* if( INAMURO_SIGMA_COMPONENT!=0 || subs==0) */

 } /* for( subs=0; subs<NUM_FLUID_COMPONENTS; subs++) */

#if SAY_HI
  printf("collide() -- Bye!\n");
#endif /* SAY_HI */

} /* void collide( lattice_ptr lattice) */

#else /* !( POROUS_MEDIA) */

//##############################################################################
//
// void collide( lattice_ptr lattice)
//
void collide( lattice_ptr lattice)
{
  double *feq;
  double *f;
  double *ftemp;
#if ZHANG_AND_CHEN_ENERGY_TRANSPORT
  double *force;
#endif /* ZHANG_AND_CHEN_ENERGY_TRANSPORT */

  double omega;

  int    bc_type;

  int    n, a;

  int    subs;

  int    i,  j;

#if POROUS_MEDIA
  double ns;
  double *nsterm;

  int    ip, jp,
         in, jn;
  int    LX = lattice->param.LX,
         LY = lattice->param.LY;
#endif

#if GUO_ZHENG_SHI_BODY_FORCE
  double Fx, Fy, F;
  double vx, vy;
  double ex, ey, edotv;
  double rho;
#if INAMURO_SIGMA_COMPONENT
  double conc;
#endif
#endif

  double rho_diff;

#if SAY_HI
  printf("collide() -- Hi!\n");
#endif /* SAY_HI */

 for( subs=0; subs<NUM_FLUID_COMPONENTS; subs++)
 {
#if SIGMA_BULK_FLAG
	if(subs == 0 || lattice->time > lattice->param.sigma_bulk_on)
	{
#endif

#if INAMURO_SIGMA_COMPONENT
   if( subs==1)
   {
     compute_macro_vars( lattice, 2);
     compute_feq( lattice, 0);
   }
#endif

  for( n=0; n<lattice->NumNodes; n++)
  {
    i = N2I(n);
    j = N2J(n);
    feq   = lattice->pdf[subs][n].feq;
    f     = lattice->pdf[subs][n].f;
    ftemp = lattice->pdf[subs][n].ftemp;
    bc_type = lattice->bc[subs][n].bc_type;
#if ZHANG_AND_CHEN_ENERGY_TRANSPORT
    force = lattice->force[subs][n].force;
#endif /* ZHANG_AND_CHEN_ENERGY_TRANSPORT */

#if INAMURO_SIGMA_COMPONENT
    if(  ( bc_sigma_walls(lattice) && subs==1)
       || is_not_solid_node( lattice, subs, n)
#if 1
       || ( subs==1
           && ( i==0 || i==get_LX(lattice)-1)
           && ( j==0 || j==get_LY(lattice)-1) )
#endif
       )
#else /* !(INAMURO_SIGMA_COMPONENT) */
    if( is_not_solid_node( lattice, subs, n))
#endif /* (INAMURO_SIGMA_COMPONENT) */
    {
        // C O L L I D E

#if 0//INAMURO_SIGMA_COMPONENT
        // Testing alternative way of applying body force...
        if( subs==0)
        {
          feq[4] += -(1./3.)*lattice->param.gval[0][1]
                  * (lattice->macro_vars[1][n].rho)
                  ;
        }
#endif

        // f = ftemp - (1/tau[subs])( ftemp - feq)
        for( a=0; a<=8; a++)
        {
#if TAU_ZHANG_ANISOTROPIC_DISPERSION
          if( subs==1  &&  ns > 1e-12)
          {
            f[a] = ftemp[a] - ( ( ftemp[a] / lattice->tau_zhang[a] )
                              - ( feq[a]   / lattice->tau_zhang[a] ) );
          }
          else
          {
            f[a] = ftemp[a] - ( ( ftemp[a] / lattice->param.tau[subs] )
                              - ( feq[a]   / lattice->param.tau[subs] ) );
          }
#else
          f[a] = ftemp[a] - ( ( ftemp[a] / lattice->param.tau[subs] )
                            - ( feq[a]   / lattice->param.tau[subs] ) );
          //f[a] = ftemp[a] - ( ( ftemp[a] )
          //                  - ( feq[a]   ) ) / lattice->param.tau[subs];
#endif
        } /* for( a=0; a<=8; a++) */

#if GUO_ZHENG_SHI_BODY_FORCE
        if( subs==0)
        {
        // Guo, Zheng & Shi: PRE 65 2002
        // Equations (4), (19) & (20)
#if INAMURO_SIGMA_COMPONENT
      //conc = lattice->pdf[/*sigma*/ 1 ][n].ftemp[0]
      //     + lattice->pdf[/*sigma*/ 1 ][n].ftemp[1]
      //     + lattice->pdf[/*sigma*/ 1 ][n].ftemp[2]
      //     + lattice->pdf[/*sigma*/ 1 ][n].ftemp[3]
      //     + lattice->pdf[/*sigma*/ 1 ][n].ftemp[4]
      //     + lattice->pdf[/*sigma*/ 1 ][n].ftemp[5]
      //     + lattice->pdf[/*sigma*/ 1 ][n].ftemp[6]
      //     + lattice->pdf[/*sigma*/ 1 ][n].ftemp[7]
      //     + lattice->pdf[/*sigma*/ 1 ][n].ftemp[8]
      //     + lattice->pdf[/*sigma*/ 1 ][n].ftemp[9];

        conc = lattice->macro_vars[1][n].rho;

#endif

        rho = f[0] + f[1] + f[2] + f[3] + f[4]
                   + f[5] + f[6] + f[7] + f[8];

        Fx = lattice->param.gval[subs][0]*rho
#if INAMURO_SIGMA_COMPONENT
            *( 1. + ( get_buoyancy(lattice))
                   *( get_beta(lattice))
                   *( (conc) - get_C0(lattice)))
#endif
          ;
        Fy = lattice->param.gval[subs][1]*rho
#if INAMURO_SIGMA_COMPONENT
            *( 1. + ( get_buoyancy(lattice))
                   *( get_beta(lattice))
                   *( (conc) - get_C0(lattice)))
#endif
          ;
        vx = ( 1.*f[1]           - 1.*f[3]
             + 1.*f[5] - 1.*f[6] - 1.*f[7] + 1.*f[8] + .5*Fx ) / rho;
        vy = (           1.*f[2]           - 1.*f[4]
             + 1.*f[5] + 1.*f[6] - 1.*f[7] - 1.*f[8] + .5*Fy ) / rho;

        // Major directions ---------------------------------------------------
        a = 3.*(1.-1./(2.*get_tau(lattice,0)))*WM;

        /* e=( 1, 0) */ ex = 1.; ey = 0.; edotv = ex*vx + ey*vy;
        F = a*( ( (ex-vx) + 3.*(edotv)*ex)*Fx + ( (ey-vy) + 3.*(edotv)*ey)*Fy);
        f[1] += F;

        /* e=( 0, 1) */ ex = 0.; ey = 1.; edotv = ex*vx + ey*vy;
        F = a*( ( (ex-vx) + 3.*(edotv)*ex)*Fx + ( (ey-vy) + 3.*(edotv)*ey)*Fy);
        f[2] += F;

        /* e=(-1, 0) */ ex =-1.; ey = 0.; edotv = ex*vx + ey*vy;
        F = a*( ( (ex-vx) + 3.*(edotv)*ex)*Fx + ( (ey-vy) + 3.*(edotv)*ey)*Fy);
        f[3] += F;

        /* e=( 0,-1) */ ex = 0.; ey =-1.; edotv = ex*vx + ey*vy;
        F = a*( ( (ex-vx) + 3.*(edotv)*ex)*Fx + ( (ey-vy) + 3.*(edotv)*ey)*Fy);
        f[4] += F;

        // Diagonal directions ------------------------------------------------
        a = 3.*(1.-1./(2.*get_tau(lattice,0)))*WD;

        /* e=( 1, 1) */ ex = 1.; ey = 1.; edotv = ex*vx + ey*vy;
        F = a*( ( (ex-vx) + 3.*(edotv)*ex)*Fx + ( (ey-vy) + 3.*(edotv)*ey)*Fy);
        f[5] += F;

        /* e=(-1, 1) */ ex =-1.; ey = 1.; edotv = ex*vx + ey*vy;
        F = a*( ( (ex-vx) + 3.*(edotv)*ex)*Fx + ( (ey-vy) + 3.*(edotv)*ey)*Fy);
        f[6] += F;

        /* e=(-1,-1) */ ex =-1.; ey =-1.; edotv = ex*vx + ey*vy;
        F = a*( ( (ex-vx) + 3.*(edotv)*ex)*Fx + ( (ey-vy) + 3.*(edotv)*ey)*Fy);
        f[7] += F;

        /* e=( 1,-1) */ ex = 1.; ey =-1.; edotv = ex*vx + ey*vy;
        F = a*( ( (ex-vx) + 3.*(edotv)*ex)*Fx + ( (ey-vy) + 3.*(edotv)*ey)*Fy);
        f[8] += F;

        }
#endif

#if ZHANG_AND_CHEN_ENERGY_TRANSPORT
        if( subs==0)
        {
          //
          // Add the body force term, equation (8),
          //
          //   f_i = f_i + \Delta f_i
          //
          //       = f_i + \frac{w_i}{T_0} c_i \dot F
          //
          // Assuming the weights, w_i, are the ones from compute_feq.
          //
          // Zhang & Chen state T_0 to be 1/3 for D3Q19.  The same in D2Q9.
          //

          f[1] += .00032*(vx[1]*3.*2.*force[0]);
          f[2] += .00032*(vy[2]*3.*2.*force[1]);
          f[3] += .00032*(vx[3]*3.*2.*force[0]);
          f[4] += .00032*(vy[4]*3.*2.*force[1]);
          f[5] += .00032*( 3.*( vx[5]*force[0] + vy[5]*force[1]));
          f[6] += .00032*( 3.*( vx[6]*force[0] + vy[6]*force[1]));
          f[7] += .00032*( 3.*( vx[7]*force[0] + vy[7]*force[1]));
          f[8] += .00032*( 3.*( vx[8]*force[0] + vy[8]*force[1]));
        }
#endif /* ZHANG_AND_CHEN_ENERGY_TRANSPORT */

#if PUKE_NEGATIVE_DENSITIES
        for( a=0; a<=8; a++)
        {
          if( *f < 0.)
          {
            printf("\n");
            printf(
              "collide() -- Node %d (%d,%d), subs %d, "
              "has negative density %20.17f "
              "in direction %d "
              "at timestep %d. Exiting!\n",
              n, n%lattice->param.LX,
                 n/lattice->param.LX,
                 subs,
                 f[a], a,
                 lattice->time             );
            printf("\n");
            process_exit(1);
          }
        } /* for( a=0; a<=8; a++) */
#endif /* PUKE_NEGATIVE_DENSITIES */

    } /* if( !( bc_type & BC_SOLID_NODE)) */

    else // bc_type & BC_SOLID_NODE
    {
      // B O U N C E B A C K

      if(   lattice->param.bc_slip_north
         && ( n >= lattice->NumNodes - lattice->param.LX
            ||n < get_LX(lattice)
            ))
      {
        // Slip condition on north boundary.
        /*
        //   A B C
        //    \|/           \|/
        //   D-o-E   -->   D-o-E
        //    /|\           /|\
        //                 A B C
        */
        f[1] = ftemp[1];
        f[2] = ftemp[4];
        f[3] = ftemp[3];
        f[4] = ftemp[2];
        f[5] = ftemp[8];
        f[6] = ftemp[7];
        f[7] = ftemp[6];
        f[8] = ftemp[5];

      } /* if(   lattice->param.bc_slip_north && ... ) */

      else
      {
        if( subs==0)
        {
#define APPLY_DEL_FEQ 0
#define APPLY_DEL_FEQ_HIGH_ORDER 0

          // Usual non-slip bounce-back condition.
          /*
          //   A B C         H G F
          //    \|/           \|/
          //   D-o-E   -->   E-o-D
          //    /|\           /|\
          //   F G H         C B A
          */
#if !( APPLY_DEL_FEQ || APPLY_DEL_FEQ_HIGH_ORDER)
#if 1
#if 1
          f[1] = ftemp[3];
          f[2] = ftemp[4];
          f[3] = ftemp[1];
          f[4] = ftemp[2];
          f[5] = ftemp[7];
          f[6] = ftemp[8];
          f[7] = ftemp[5];
          f[8] = ftemp[6];
#else
          // Try doing regular collision on the solid before applying
          // bounce-back
          double ftemptemp[9];
          for( a=1; a<9; a++)
          {
            ftemptemp[a] = ftemp[a] - ( ( ftemp[a] / get_tau(lattice,subs) )
                                      - ( feq[a]   / get_tau(lattice,subs) ) );
          }
          f[1] = ftemptemp[3];
          f[2] = ftemptemp[4];
          f[3] = ftemptemp[1];
          f[4] = ftemptemp[2];
          f[5] = ftemptemp[7];
          f[6] = ftemptemp[8];
          f[7] = ftemptemp[5];
          f[8] = ftemptemp[6];
#endif
#else
          // Experimenting: Trying to compensate for loss of mass into
          // the solid nodes.
          compute_ave_rho( lattice, &rho_diff, 0);
          rho_diff -= lattice->param.rho_A[0];

          if( j==0)
          {
            f[1] = ( ftemp[3]);
            f[2] = ( ftemp[4] - .50*rho_diff/2./get_LX(lattice));
            f[3] = ( ftemp[1]);
            f[4] = ( ftemp[2]);
            f[5] = ( ftemp[7] - .25*rho_diff/2./get_LX(lattice));
            f[6] = ( ftemp[8] - .25*rho_diff/2./get_LX(lattice));
            f[7] = ( ftemp[5]);
            f[8] = ( ftemp[6]);
          }
          else if( j==get_LY(lattice)-1)
          {
            f[1] = ( ftemp[3]);
            f[2] = ( ftemp[4]);
            f[3] = ( ftemp[1]);
            f[4] = ( ftemp[2] - .50*rho_diff/2./get_LX(lattice));
            f[5] = ( ftemp[7]);
            f[6] = ( ftemp[8]);
            f[7] = ( ftemp[5] - .25*rho_diff/2./get_LX(lattice));
            f[8] = ( ftemp[6] - .25*rho_diff/2./get_LX(lattice));
          }
          else
          {
            printf("ERROR: Experimental bounceback only works on top and "
               "bottom of domain.");
            process_exit(1);
          }
#endif
#else
          f[1] = ftemp[3];
          f[2] =
#if APPLY_DEL_FEQ
#if 1
                 ftemp[4]
               - (1./2.)
#else
                 ( .25
                  *lattice->param.gval[subs][1]
                 +
                   .25
                  *lattice->param.gval[subs][1]
                  *lattice->param.gval[subs][1]
                 )
               + (1.)//2.)
#endif
               *
               ( (-1.)
                *lattice->param.gval[subs][1]
#if APPLY_DEL_FEQ_HIGH_ORDER
               + (1.)
                *lattice->param.gval[subs][1]
                *lattice->param.gval[subs][1]
#endif
               )
#else
                 ftemp[4]
#endif
               ;
          f[3] = ftemp[1];
          f[4] = ftemp[2]
#if APPLY_DEL_FEQ
               - (1./2.)
               *
               ( (1.)
                *lattice->param.gval[subs][1]
#if APPLY_DEL_FEQ_HIGH_ORDER
               + (1.)
                *lattice->param.gval[subs][1]
                *lattice->param.gval[subs][1]
#endif
               )
#endif
               ;
          f[5] = ftemp[7]
#if APPLY_DEL_FEQ
               - (1./2.)
               *
               ( (-1./12.)
                *lattice->param.gval[subs][1]
#if APPLY_DEL_FEQ_HIGH_ORDER
               + (1./8.)
                *lattice->param.gval[subs][1]
                *lattice->param.gval[subs][1]
               + (-1./24.)
                *lattice->param.gval[subs][1]
                *lattice->param.gval[subs][1]
                *lattice->param.gval[subs][1]
                *lattice->param.gval[subs][1]
#endif
               )
#endif
               ;
          f[6] = ftemp[8]
#if APPLY_DEL_FEQ
               - (1./2.)
               *
               ( (-1./12.)
                *lattice->param.gval[subs][1]
#if APPLY_DEL_FEQ_HIGH_ORDER
               + (1./8.)
                *lattice->param.gval[subs][1]
                *lattice->param.gval[subs][1]
               + (-1./24.)
                *lattice->param.gval[subs][1]
                *lattice->param.gval[subs][1]
                *lattice->param.gval[subs][1]
                *lattice->param.gval[subs][1]
#endif
               )
#endif
               ;
          f[7] = ftemp[5]
#if APPLY_DEL_FEQ
               - (1./2.)
               *
               ( (1./12.)
                *lattice->param.gval[subs][1]
#if APPLY_DEL_FEQ_HIGH_ORDER
               + (1./8.)
                *lattice->param.gval[subs][1]
                *lattice->param.gval[subs][1]
               + (-1./24.)
                *lattice->param.gval[subs][1]
                *lattice->param.gval[subs][1]
                *lattice->param.gval[subs][1]
                *lattice->param.gval[subs][1]
#endif
               )
#endif
               ;
          f[8] = ftemp[6]
#if APPLY_DEL_FEQ
               - (1./2.)
               *
               ( (1./12.)
                *lattice->param.gval[subs][1]
#if APPLY_DEL_FEQ_HIGH_ORDER
               + (1./8.)
                *lattice->param.gval[subs][1]
                *lattice->param.gval[subs][1]
               + (-1./24.)
                *lattice->param.gval[subs][1]
                *lattice->param.gval[subs][1]
                *lattice->param.gval[subs][1]
                *lattice->param.gval[subs][1]
#endif
               )
#endif
               ;

#endif
        } /* if( subs==0) */

#if NUM_FLUID_COMPONENTS==2
        else // subs==1
        {
#if INAMURO_SIGMA_COMPONENT
          if( lattice->param.bc_sigma_slip)
          {
            //
            // Slip BC for solute on side walls.
            // Will this make a difference on Taylor dispersion?
            //
            if( flow_dir_is_vertical( lattice))
            {
              if( on_the_east_or_west( lattice, n))
              {
                // Slip condition on east/west boundary.
                /*
                //   A B C         C B A
                //    \|/           \|/
                //   D-o-E   -->   E-o-D
                //    /|\           /|\
                //   F G H         H G F
                */
                f[1] = ftemp[3];
                f[2] = ftemp[2];
                f[3] = ftemp[1];
                f[4] = ftemp[4];
                f[5] = ftemp[6];
                f[6] = ftemp[5];
                f[7] = ftemp[8];
                f[8] = ftemp[7];

              }
            }
            else if( flow_dir_is_horizontal( lattice))
            {
              if( on_the_north_or_south( lattice, n))
              {
                // Slip condition on north/south boundary.
                /*
                //   A B C         F G H
                //    \|/           \|/
                //   D-o-E   -->   D-o-E
                //    /|\           /|\
                //   F G H         A B C
                */
                f[1] = ftemp[1];
                f[2] = ftemp[4];
                f[3] = ftemp[3];
                f[4] = ftemp[2];
                f[5] = ftemp[8];
                f[6] = ftemp[7];
                f[7] = ftemp[6];
                f[8] = ftemp[5];

              }
              else
              {
                // ERROR: Solid exists somewhere other than as side walls.
                printf("%s (%d) >> "
                  "ERROR: "
                  "bc_sigma_slip is on. "
                  "FlowDir is determined to be horizontal. "
                  "Encountered solid node somewhere other than side walls. "
                  "That situation is not supported. "
                  "Exiting!", __FILE__, __LINE__);
                process_exit(1);
              }
            }
            else
            {
              // Bail
              printf("%s (%d) >> "
                "FlowDir is indeterminate. "
                "Cannot apply slip BC (bc_sigma_slip). "
                "Exiting!", __FILE__, __LINE__);
              process_exit(1);
            }

          } /* if( lattice->param.bc_sigma_slip) */

          else
          {
#endif /* INAMURO_SIGMA_COMPONENT */

            // Usual non-slip bounce-back condition.
            /*
            //   A B C         H G F
            //    \|/           \|/
            //   D-o-E   -->   E-o-D
            //    /|\           /|\
            //   F G H         C B A
            */
            f[1] = ftemp[3];
            f[2] = ftemp[4];
            f[3] = ftemp[1];
            f[4] = ftemp[2];
            f[5] = ftemp[7];
            f[6] = ftemp[8];
            f[7] = ftemp[5];
            f[8] = ftemp[6];
#if INAMURO_SIGMA_COMPONENT
          } /* if( lattice->param.bc_sigma_slip) else */
#endif /* INAMURO_SIGMA_COMPONENT */

        } /* if( subs==0) else*/
#endif /* NUM_FLUID_COMPONENTS==2 */

      } /* if(   lattice->param.bc_slip_north && ... ) else */

    } /* if( !( bc_type & BC_SOLID_NODE)) else */

  } /* for( n=0; n<lattice_NumNodes; n++) */

#if POROUS_MEDIA
  if( INAMURO_SIGMA_COMPONENT!=0 || subs==0)
  {
    // Need separate temp space for this?
    nsterm = (double*)malloc( 9*lattice->NumNodes*sizeof(double));

    // Compute the solid density term for fluid component.
    for( n=0; n<lattice->NumNodes; n++)
    {
      bc_type = lattice->bc [subs][n].bc_type;

      i = n%LX;
      j = n/LX;

      jp = ( j<LY-1)?( j+1):( 0   );
      jn = ( j>0   )?( j-1):( LY-1);

      ip = ( i<LX-1)?( i+1):( 0   );
      in = ( i>0   )?( i-1):( LX-1);

      if( is_not_solid_node(lattice, subs, n))
      {
        if( lattice->param.ns_flag == 0)
        {
          ns = lattice->param.ns;
#if 0 // OLD POROUS_MEDIA TERM
  /* 1 */ nsterm[9*n+1] = ns*( lattice->pdf[subs][ j *LX + ip].f[3]
                             - lattice->pdf[subs][ j *LX + i ].f[1]);
  /* 2 */ nsterm[9*n+2] = ns*( lattice->pdf[subs][ jp*LX + i ].f[4]
                             - lattice->pdf[subs][ j *LX + i ].f[2]);
  /* 3 */ nsterm[9*n+3] = ns*( lattice->pdf[subs][ j *LX + in].f[1]
                             - lattice->pdf[subs][ j *LX + i ].f[3]);
  /* 4 */ nsterm[9*n+4] = ns*( lattice->pdf[subs][ jn*LX + i ].f[2]
                             - lattice->pdf[subs][ j *LX + i ].f[4]);
  /* 5 */ nsterm[9*n+5] = ns*( lattice->pdf[subs][ jp*LX + ip].f[7]
                             - lattice->pdf[subs][ j *LX + i ].f[5]);
  /* 6 */ nsterm[9*n+6] = ns*( lattice->pdf[subs][ jp*LX + in].f[8]
                             - lattice->pdf[subs][ j *LX + i ].f[6]);
  /* 7 */ nsterm[9*n+7] = ns*( lattice->pdf[subs][ jn*LX + in].f[5]
                             - lattice->pdf[subs][ j *LX + i ].f[7]);
  /* 8 */ nsterm[9*n+8] = ns*( lattice->pdf[subs][ jn*LX + ip].f[6]
                             - lattice->pdf[subs][ j *LX + i ].f[8]);
#else
  /* 1 */ nsterm[9*n+1] = ns*( lattice->pdf[subs][ j*LX + i].ftemp[3]
                             - lattice->pdf[subs][ j*LX + i].f    [1]);
  /* 2 */ nsterm[9*n+2] = ns*( lattice->pdf[subs][ j*LX + i].ftemp[4]
                             - lattice->pdf[subs][ j*LX + i].f    [2]);
  /* 3 */ nsterm[9*n+3] = ns*( lattice->pdf[subs][ j*LX + i].ftemp[1]
                             - lattice->pdf[subs][ j*LX + i].f    [3]);
  /* 4 */ nsterm[9*n+4] = ns*( lattice->pdf[subs][ j*LX + i].ftemp[2]
                             - lattice->pdf[subs][ j*LX + i].f    [4]);
  /* 5 */ nsterm[9*n+5] = ns*( lattice->pdf[subs][ j*LX + i].ftemp[7]
                             - lattice->pdf[subs][ j*LX + i].f    [5]);
  /* 6 */ nsterm[9*n+6] = ns*( lattice->pdf[subs][ j*LX + i].ftemp[8]
                             - lattice->pdf[subs][ j*LX + i].f    [6]);
  /* 7 */ nsterm[9*n+7] = ns*( lattice->pdf[subs][ j*LX + i].ftemp[5]
                             - lattice->pdf[subs][ j*LX + i].f    [7]);
  /* 8 */ nsterm[9*n+8] = ns*( lattice->pdf[subs][ j*LX + i].ftemp[6]
                             - lattice->pdf[subs][ j*LX + i].f    [8]);
#endif
        }
        else /* ns_flag==1 || ns_flag==2 */
        {
          // Variable solid density.
          ns = lattice->ns[n].ns;
//printf("%s %d >> ns = %f\n",__FILE__,__LINE__,ns);
#if 0 // OLD POROUS_MEDIA TERM
  /* 1 */ nsterm[9*n+1] = ns*( lattice->pdf[subs][ j *LX + ip].f[3]
                             - lattice->pdf[subs][ j *LX + i ].f[1]);
  /* 2 */ nsterm[9*n+2] = ns*( lattice->pdf[subs][ jp*LX + i ].f[4]
                             - lattice->pdf[subs][ j *LX + i ].f[2]);
  /* 3 */ nsterm[9*n+3] = ns*( lattice->pdf[subs][ j *LX + in].f[1]
                             - lattice->pdf[subs][ j *LX + i ].f[3]);
  /* 4 */ nsterm[9*n+4] = ns*( lattice->pdf[subs][ jn*LX + i ].f[2]
                             - lattice->pdf[subs][ j *LX + i ].f[4]);
  /* 5 */ nsterm[9*n+5] = ns*( lattice->pdf[subs][ jp*LX + ip].f[7]
                             - lattice->pdf[subs][ j *LX + i ].f[5]);
  /* 6 */ nsterm[9*n+6] = ns*( lattice->pdf[subs][ jp*LX + in].f[8]
                             - lattice->pdf[subs][ j *LX + i ].f[6]);
  /* 7 */ nsterm[9*n+7] = ns*( lattice->pdf[subs][ jn*LX + in].f[5]
                             - lattice->pdf[subs][ j *LX + i ].f[7]);
  /* 8 */ nsterm[9*n+8] = ns*( lattice->pdf[subs][ jn*LX + ip].f[6]
                             - lattice->pdf[subs][ j *LX + i ].f[8]);
#else
  /* 1 */ nsterm[9*n+1] = ns*( lattice->pdf[subs][ j*LX + i].ftemp[3]
                             - lattice->pdf[subs][ j*LX + i].f    [1]);
  /* 2 */ nsterm[9*n+2] = ns*( lattice->pdf[subs][ j*LX + i].ftemp[4]
                             - lattice->pdf[subs][ j*LX + i].f    [2]);
  /* 3 */ nsterm[9*n+3] = ns*( lattice->pdf[subs][ j*LX + i].ftemp[1]
                             - lattice->pdf[subs][ j*LX + i].f    [3]);
  /* 4 */ nsterm[9*n+4] = ns*( lattice->pdf[subs][ j*LX + i].ftemp[2]
                             - lattice->pdf[subs][ j*LX + i].f    [4]);
  /* 5 */ nsterm[9*n+5] = ns*( lattice->pdf[subs][ j*LX + i].ftemp[7]
                             - lattice->pdf[subs][ j*LX + i].f    [5]);
  /* 6 */ nsterm[9*n+6] = ns*( lattice->pdf[subs][ j*LX + i].ftemp[8]
                             - lattice->pdf[subs][ j*LX + i].f    [6]);
  /* 7 */ nsterm[9*n+7] = ns*( lattice->pdf[subs][ j*LX + i].ftemp[5]
                             - lattice->pdf[subs][ j*LX + i].f    [7]);
  /* 8 */ nsterm[9*n+8] = ns*( lattice->pdf[subs][ j*LX + i].ftemp[6]
                             - lattice->pdf[subs][ j*LX + i].f    [8]);
#endif
        }

      } /* if( !( *bc_type++ & BC_SOLID_NODE)) */
    } /* for( n=0; n<lattice_NumNodes; n++) */

    for( n=0; n<lattice->NumNodes; n++)
    {
      f       = lattice->pdf[subs][n].f;
      bc_type = lattice->bc [subs][n].bc_type;

      if( !( bc_type & BC_SOLID_NODE))
      {
        for( a=1; a<9; a++)
        {
#if 0
          // Store f in ftemp, because the compute_macro vars before
          // output_frames needs to have the pre-ns version of f.
          if( is_last_step_of_frame(lattice))
          {
            // This temp copy of f in ftemp prior to application
            // of ns only needs to be done before outputting a frame,
            // not after each timestep.
            lattice->pdf[subs][n].ftemp[a] = f[a];
          }
#endif
          // OLD: fout = fc + ns*( fc_(x+cdt) - fc(x))
          //        f +=      ns*( f_(x+cdt)  - f(x))
          //
          //  - - - >o< - - -   <------o------>   <--<---o--->-->
          //        1 2         3             4   3  5       6  4
          //                    ------>o<------
          //                          5 6
          //
          // CUR: fout = fc + ns*( fc_(x)     - fc(x)) = (1-ns)*fc + ns*fc_(x)
          //        f +=      ns*( f_(x)      - f(x))
          //
          //  - - - >o< - - -   <------o------>   <--<---o--->-->
          //        1 2         3             4   3  4       3  4
          //
          // NEW: fout = fc + ns*( fin_(x)    - fc(x)) = (1-ns)*fc + ns*fin_(x)
          //        f +=      ns*( ftemp_(x)  - f(x))
          //
          //  - - - >o< - - -   <------o------>   <--<- -o- ->-->
          //        1 2         3             4   3  1       2  4
          //
          // c.f., Walsh, et al., Computers and Geosciences
          f[a] += nsterm[9*n+a];

        } /* for( a=1; a<9; a++) */
      } /* if( !( bc_type & BC_SOLID_NODE)) */
    } /* for( n=0; n<lattice->NumNodes; n++, f+=18) */

    free( nsterm);

  } /* if( INAMURO_SIGMA_COMPONENT!=0 || subs==0) */
#endif

#if SIGMA_BULK_FLAG
	}
#endif

 } /* for( subs=0; subs<NUM_FLUID_COMPONENTS; subs++) */

#if SAY_HI
  printf("collide() -- Bye!\n");
#endif /* SAY_HI */

} /* void collide( lattice_ptr lattice) */
#endif /* POROUS_MEDIA */
