//##############################################################################
//
// Copyright (C), 2005, Michael Sukop and Danny Thorne
//
// compute.c
//
//  - Routines for computing on the lattice:
//
//    - compute_rho_and_u
//    - compute_feq
//    - compute_big_u
//    - compute_gval
//    - compute_fluid_fluid_force
//    - etc...
//

// P R E P R O C E S S O R   M A C R O S
#if NON_LOCAL_FORCES


#if ZHANG_AND_CHEN_ENERGY_TRANSPORT

#define BIG_U_X( u_)				\
    (u_)					\
    + lattice->param.tau[subs]			\
    * lattice->param.gval[subs][0]

#define BIG_U_Y( u_)				\
    (u_)					\
    + lattice->param.tau[subs]			\
    * lattice->param.gval[subs][1]

#define BIG_U_X_BUOY( u_, rho1_, rho2_) 1.
#define BIG_U_Y( u_, rho1_, rho2_) 1.

#else /* !( ZHANG_AND_CHEN_ENERGY_TRANSPORT) */

#if NUM_FLUID_COMPONENTS == 2
#define BIG_U_X( u_, rho_)			\
    (u_)					\
    + lattice->param.tau[subs]			\
    * lattice->force[subs][n].force[0]/(rho_)	\
    + lattice->param.tau[subs]			\
    * lattice->force[subs][n].sforce[0]		\
    + lattice->param.tau[subs]			\
    * lattice->param.gval[subs][0]

#define BIG_U_Y( u_, rho_)			\
    (u_)					\
    + lattice->param.tau[subs]			\
    * lattice->force[subs][n].force[1]/(rho_)	\
    + lattice->param.tau[subs]			\
    * lattice->force[subs][n].sforce[1]		\
    + lattice->param.tau[subs]			\
    * lattice->param.gval[subs][1]
#else
#define BIG_U_X( u_, rho_)			\
    (u_)					\
    + lattice->param.tau[subs]			\
    * lattice->force[subs][n].force[0]/(rho_)	\
    + lattice->param.tau[subs]			\
    * lattice->force[subs][n].sforce[0]/(rho_)	\
    + lattice->param.tau[subs]			\
    * lattice->param.gval[subs][0]

#define BIG_U_Y( u_, rho_)			\
    (u_)					\
    + lattice->param.tau[subs]			\
    * lattice->force[subs][n].force[1]/(rho_)	\
    + lattice->param.tau[subs]			\
    * lattice->force[subs][n].sforce[1]/(rho_)	\
    + lattice->param.tau[subs]			\
    * lattice->param.gval[subs][1]

#endif /* NUM_FLUID_COMPONENTS == 2 */

#endif /* ZHANG_AND_CHEN_ENERGY_TRANSPORT */

#else /* !( NON_LOCAL_FORCES) */

#if INAMURO_SIGMA_COMPONENT

#if GUO_ZHENG_SHI_BODY_FORCE
#define BIG_U_X( u_, rho1_, rho2_) (u_) + .5*F(0,rho1_,rho2_)
#define BIG_U_Y( u_, rho1_, rho2_) (u_) + .5*F(1,rho1_,rho2_)
#else
#define BIG_U_X( u_, rho1_, rho2_)			\
    (u_) + get_tau(lattice,subs) * F(0,rho1_,rho2_)
#define BIG_U_Y( u_, rho1_, rho2_)			\
    (u_) + get_tau(lattice,subs) * F(1,rho1_,rho2_)
#endif

#else /* !( INAMURO_SIGMA_COMPONENT) */

#if GUO_ZHENG_SHI_BODY_FORCE
#define BIG_U_X( u_, rho_) (u_) + .5*F(0)
#define BIG_U_Y( u_, rho_) (u_) + .5*F(1)
#else
#define BIG_U_X( u_, rho_) (u_) + get_tau(lattice,subs) * F(0,rho_)
#define BIG_U_Y( u_, rho_) (u_) + get_tau(lattice,subs) * F(1,rho_)
#endif

#endif /* INAMURO_SIGMA_COMPONENT */

#endif /* NON_LOCAL_FORCES */

#if INAMURO_SIGMA_COMPONENT
// C O M P U T E _ M A C R O _ V A R S  {{{1
void compute_macro_vars( struct lattice_struct *lattice, int which_f)
{
    int n, a;

    double *rho[ NUM_FLUID_COMPONENTS],
	*u_x[ NUM_FLUID_COMPONENTS],
	*u_y[ NUM_FLUID_COMPONENTS];

    double *f, *ftemp;

    bc_ptr bc;

    int    subs;

    double c;

#if TAU_ZHANG_ANISOTROPIC_DISPERSION
    double Dxx, Dyy, Dxy, Dl, Dt, ns;
    double factor=0.0, lamdax, lamday;
    double wt[9]={4./9.,WM,WM,WM,WM,WD,WD,WD,WD};
#endif

#if GUO_ZHENG_SHI_BODY_FORCE
    double conc; // For computing concentration to use in body force.
#endif

    //############################################################################
    //
    // For first component, compute rho and u.
    //
    subs=0;

    rho[subs]   = &( lattice->macro_vars[subs][0].rho);
    u_x[subs]   =    lattice->macro_vars[subs][0].u;
    u_y[subs]   =    lattice->macro_vars[subs][0].u + 1;
    switch(which_f)
    {
    case 0: // Compute from post-collision f.
	ftemp = lattice->pdf[subs][0].f;
	break;
    case 1: // Compute from pre-collision f.
	ftemp = lattice->pdf[subs][0].ftemp;
	break;
    case 2: // Compute average from pre- and post-collision f.
	f = lattice->pdf[subs][0].f;
	ftemp = lattice->pdf[subs][0].ftemp;
	break;
    default:
	break;
    }
    bc          =    lattice->bc[subs];

    for( n=0; n<lattice->NumNodes; n++)
    {

	*rho[subs] = 0.;
	*u_x[subs] = 0.;
	*u_y[subs] = 0.;

	if( COMPUTE_ON_SOLIDS || is_not_solid_node( lattice, subs, n))
	{
	    for( a=0; a<9; a++)
	    {
		(*rho[subs]) += (*ftemp);
		(*u_x[subs]) += vx[a]*(*ftemp);
		(*u_y[subs]) += vy[a]*(*ftemp);
		ftemp++;

		if( which_f == 2)
		{
		    (*rho[subs]) += (*f);
		    (*u_x[subs]) += vx[a]*(*f);
		    (*u_y[subs]) += vy[a]*(*f);
		    f++;
		}

	    } /* for( a=0; a<9; a++) */

#if SOURCE_ON
	    (*rho[subs]) += lattice->param.source_strength * lattice->param.tau[0];
	    if(which_f == 2)
	    {
		(*rho[subs]) += lattice->param.source_strength * lattice->param.tau[0];
	    } 
#endif /*if SOURCE_ON*/


	    if( which_f == 2)
	    {
		(*rho[subs]) /= 2.;
		(*u_x[subs]) /= 2.;
		(*u_y[subs]) /= 2.;
	    }
	    else
	    {
#if GUO_ZHENG_SHI_BODY_FORCE
		//conc = lattice->pdf[/*sigma*/ 1 ][n].f[0]
		//     + lattice->pdf[/*sigma*/ 1 ][n].f[1]
		//     + lattice->pdf[/*sigma*/ 1 ][n].f[2]
		//     + lattice->pdf[/*sigma*/ 1 ][n].f[3]
		//     + lattice->pdf[/*sigma*/ 1 ][n].f[4]
		//     + lattice->pdf[/*sigma*/ 1 ][n].f[5]
		//     + lattice->pdf[/*sigma*/ 1 ][n].f[6]
		//     + lattice->pdf[/*sigma*/ 1 ][n].f[7]
		//     + lattice->pdf[/*sigma*/ 1 ][n].f[8]
		//     + lattice->pdf[/*sigma*/ 1 ][n].f[9];

		conc = lattice->macro_vars[1][n].rho;

		*u_x[subs] += .5*lattice->param.gval[subs][0]
		    *(*rho[subs])
#if 1
		    *( 1. + ( get_buoyancy(lattice))
		       *( get_beta(lattice))
		       *( conc - get_C0(lattice)))
#endif
		    ;

		*u_y[subs] += .5*lattice->param.gval[subs][1]
		    *(*rho[subs])
#if 1
		    *( 1. + ( get_buoyancy(lattice))
		       *( get_beta(lattice))
		       *( conc - get_C0(lattice)))
#endif
		    ;
#endif
	    }

#if PUKE_NEGATIVE_DENSITIES
	    if( *rho[subs] < 0.)
	    {
		printf("\n");
		printf(
		       "compute_macro_vars() -- "
		       "Node %d (%d,%d) has negative density %20.17f "
		       "at timestep %d. Exiting!\n",
		       n, n%lattice->param.LX,
		       n/lattice->param.LX, *rho[subs],
		       lattice->time             );
		printf("\n");
		process_exit(1);
	    }
#endif /* PUKE_NEGATIVE_DENSITIES */

	    if( 0)//*rho[subs] == 0)
	    {
		printf("\n");
		printf("\n");
		printf("%s (%d) -- "
		       "ERROR:  rho[subs=%d][j=%d][i=%d] = 0. "
		       "at timestep %d.  "
		       "Exiting!\n",
		       __FILE__,__LINE__,
		       subs,
		       n/lattice->param.LX,
		       n%lattice->param.LX,
		       lattice->time        );
		printf("\n");
		printf("\n");
		process_exit(1);
	    }

	} /* if( !( bc++->bc_type & BC_SOLID_NODE)) */

	else // bc++->bc_type & BC_SOLID_NODE
	{
	    //printf("RHO: n=%d, Solid node.\n", n);
	    ftemp+=9;
	    if( which_f)
	    {
		f+=9;
	    }

	} /* if( !( bc++->bc_type & BC_SOLID_NODE)) else */

	rho[subs]+=3;
	u_x[subs]+=3;
	u_y[subs]+=3;
	ftemp+=18;
	if( which_f == 2)
	{
	    f+=18;
	}

    } /* for( n=0; n<lattice->NumNodes; n++) */

    //############################################################################
    //
    // For second component, compute just rho.
    //
    subs=1;

    //#if SIGMA_BULK_FLAG
    //	if(lattice->time > lattice->param.sigma_bulk_on)
    //	{
    //#endif

    rho[subs]   = &( lattice->macro_vars[subs][0].rho);
    switch(which_f)
    {
    case 0: // Compute from pre-collision f.
	ftemp = lattice->pdf[subs][0].ftemp;
	break;
    case 1: // Compute from pre-collision f.
	ftemp = lattice->pdf[subs][0].ftemp;
	break;
    case 2: // Compute from pre-collision f.
	ftemp = lattice->pdf[subs][0].ftemp;
	break;
    default:
	break;
    }
    bc          =    lattice->bc[subs];

#if TAU_ZHANG_ANISOTROPIC_DISPERSION
    if( (lattice->param.ns_flag == 0)){ ns = lattice->param.ns;}
#endif

    for( n=0; n<lattice->NumNodes; n++)
    {
#if  TAU_ZHANG_ANISOTROPIC_DISPERSION

	//Dl=.81;Dt=0.21;
	//*u_x[0] = (*u_x[0] < 1e-12 ? 0. : *u_x[0]);
	//*u_y[0] = (*u_y[0] < 1e-12 ? 0. : *u_y[0]);

	Dxx = lattice->param.Dt *sqrt(pow((*u_x[0]),2) + pow((*u_y[0]),2) ) + ( lattice->param.Dl - lattice->param.Dt)*(*u_x[0] * (*u_x[0]))/sqrt(pow((*u_x[0]),2) +
																		  pow((*u_y[0]),2));


	Dyy= lattice->param.Dt *sqrt(pow((*u_x[0]),2) + pow((*u_y[0]),2) ) + ( lattice->param.Dl - lattice->param.Dt)*(*u_y[0] * (*u_y[0]))/sqrt(pow((*u_x[0]),2) +
																		 pow((*u_y[0]),2));

	Dxy=                                ( lattice->param.Dl - lattice->param.Dt )*(*u_x[0] * (*u_y[0]))/sqrt(pow((*u_x[0]),2) + pow((*u_y[0]),2));

	//printf("Dl=%f\n",lattice->param.Dl);

	lattice->tau_zhang [0] = 1.;

	lamdax = (18. * Dxx + 3. - 18. * Dxy)/ 6.;
	lamday = (18. * Dyy + 3. - 18. * Dxy)/ 6.;

    	if(lamdax <0.5 | lamday <0.5)
	{
	    printf("%s (%d)\n""lamda<0.5 lamdax=%f lamday=%f at n=%d, time=%d",__FILE__,__LINE__,lamdax,lamday,n,lattice->time);
	    printf("\nux=%f uy=%f Dxx=%f  Dxy=%f  Dyy=%f \n",*u_x[0],*u_y[0],Dxx,Dxy,Dyy);
	    exit(1);	
	}

	if (Dxy > 1e-12)
	{


	    if (lamdax < lamday)

		lattice->tau_zhang [6] = lattice->tau_zhang [8] = lamdax;

	    else

		lattice->tau_zhang [6] = lattice->tau_zhang [8] = lamday;



	    lattice->tau_zhang [5] = lattice->tau_zhang [7] = ( 18.* Dxy +       lattice->tau_zhang [6]);
	    lattice->tau_zhang [2] = lattice->tau_zhang [4] = ( 18.* Dyy + 3. - (lattice->tau_zhang [5]+lattice->tau_zhang [6]) )/4.;
	    lattice->tau_zhang [1] = lattice->tau_zhang [3] = ( 18.* Dxx + 3. - (lattice->tau_zhang [5]+lattice->tau_zhang [6]) )/4.;
	}

	else if (Dxy < 1e-12)
	{
	    if (lamdax < lamday)

		lattice->tau_zhang [6] = lattice->tau_zhang [8] = lamday;

	    else

		lattice->tau_zhang [6] = lattice->tau_zhang [8] = lamdax;

	    lattice->tau_zhang [5] = lattice->tau_zhang [7] = 0.5001;

	    lattice->tau_zhang [6] = lattice->tau_zhang [8] = ( 18.* Dxy +       lattice->tau_zhang [5]);
	    lattice->tau_zhang [2] = lattice->tau_zhang [4] = ( 18.* Dyy + 3. - (lattice->tau_zhang [5]+lattice->tau_zhang [6]) )/4.;
	    lattice->tau_zhang [1] = lattice->tau_zhang [3] = ( 18.* Dxx + 3. - (lattice->tau_zhang [5]+lattice->tau_zhang [6]) )/4.;
	}

	else if ( Dxy == 0.)
	{

	    lattice->tau_zhang [6] = lattice->tau_zhang [8] =  0.5001;
	    lattice->tau_zhang [5] = lattice->tau_zhang [7] = ( 18.* Dxy +       lattice->tau_zhang [6]);
	    lattice->tau_zhang [2] = lattice->tau_zhang [4] = ( 18.* Dyy + 3. - (lattice->tau_zhang [5]+lattice->tau_zhang [6]) )/4.;
	    lattice->tau_zhang [1] = lattice->tau_zhang [3] = ( 18.* Dxx + 3. - (lattice->tau_zhang [5]+lattice->tau_zhang [6]) )/4.;
	}

	else
	{
	    printf("%s (%d)""at Dxy = %f  n=%d, time=%d",__FILE__,__LINE__,Dxy,n,lattice->time);
	    printf("\nux=%f uy=%f Dxx=%f  Dxy=%f  Dyy=%f \n",*u_x[0],*u_y[0],Dxx,Dxy,Dyy);
	    printf("\n%s (%d)\n""lamda<0.5 lamdax=%f lamday=%f at n=%d, time=%d",__FILE__,__LINE__,lamdax,lamday,n,lattice->time);
	    exit(1);
	}

	factor=0.;

	for(a=0;a<9;a++)
	{
	    factor=factor+wt[a]/lattice->tau_zhang [a];

	}

	factor=1./factor;
#endif

	*rho[subs] = 0.;

	if( COMPUTE_ON_SOLIDS || is_not_solid_node( lattice, subs, n))
	{
#if TAU_ZHANG_ANISOTROPIC_DISPERSION
	    if( lattice->param.ns_flag >= 1 ) { ns = lattice->ns[n].ns;}
	    for( a=0; a<9; a++)
	    {
		(*rho[subs]) += (*ftemp)*((ns>1e-12)
					  ?(factor/lattice->tau_zhang[a])
					  :(1.));

		ftemp++;

	    } /* for( a=0; a<9; a++) */
#else

	    for( a=0; a<9; a++)
	    {
		(*rho[subs]) += (*ftemp);
		ftemp++;
	    } /* for( a=0; a<9; a++) */
#endif

#if SINK_ON
	    (*rho[subs]) *= (1. - lattice->param.sink_strength * lattice->param.tau[1]);
#endif

#if PUKE_NEGATIVE_CONCENTRATIONS
	    if( *rho[subs] < 0.)
	    {
		printf("\n");
		printf(
		       "compute_macro_vars() -- "
		       "Node %d (%d,%d) has negative density %20.17f "
		       "at timestep %d. Exiting!\n",
		       n, n%lattice->param.LX,
		       n/lattice->param.LX, *rho[subs],
		       lattice->time             );
		printf("\n");
		process_exit(1);
	    }
#endif /* PUKE_NEGATIVE_CONCENTRATIONS */
	    //assert( *rho[subs] != 0);

	} /* if( !( bc++->bc_type & BC_SOLID_NODE)) */

	else // bc++->bc_type & BC_SOLID_NODE
	{
	    //printf("RHO: n=%d, Solid node.\n", n);
	    ftemp+=9;

	} /* if( !( bc++->bc_type & BC_SOLID_NODE)) else */

	rho[subs]+=3;
	ftemp+=18;

    } /* for( n=0; n<lattice->NumNodes; n++) */

    rho[0]   = &( lattice->macro_vars[0][0].rho);
    u_x[0]   =    lattice->macro_vars[0][0].u;
    u_x[1]   =    lattice->macro_vars[1][0].u;
    u_y[0]   =    lattice->macro_vars[0][0].u + 1;
    u_y[1]   =    lattice->macro_vars[1][0].u + 1;

    for( n=0; n<lattice->NumNodes; n++)
    {
	if( COMPUTE_ON_SOLIDS || is_not_solid_node( lattice, subs, n))
	{
	    if( 0)//*rho[0] == 0)
	    {
		printf("\n");
		printf("\n");
		printf("%s (%d) -- "
		       "ERROR:  rho[subs=%d][j=%d][i=%d] = 0. "
		       "at timestep %d.  "
		       "Exiting!\n",
		       __FILE__,__LINE__,
		       0,
		       n/lattice->param.LX,
		       n%lattice->param.LX,
		       lattice->time        );
		printf("\n");
		printf("\n");
		process_exit(1);
	    }

	    if( lattice->param.incompressible)
	    {
		c = 1.;
	    }
	    else
	    {
		c = *rho[0];
	    }

	    if( c!=0&&*u_x[0]!=0.) { *u_x[0] /= c;} else { *u_x[0] = 0.;}
	    if( c!=0&&*u_y[0]!=0.) { *u_y[0] /= c;} else { *u_y[0] = 0.;}
	    *u_x[1] = *u_x[0];
	    *u_y[1] = *u_y[0];

	} /* if( 1 || !( bc[n].bc_type & BC_SOLID_NODE)) */

	rho[0]+=3;
	u_x[0]+=3;
	u_x[1]+=3;
	u_y[0]+=3;
	u_y[1]+=3;

    } /* for( n=0; n<lattice->NumNodes; n++) */

    //#if SIGMA_BULK_FLAG
    //	}
    //#endif

} /* void compute_macro_vars( struct lattice_struct *lattice) */
// }}}
#else /* !( INAMURO_SIGMA_COMPONENT) */
// C O M P U T E _ M A C R O _ V A R S  {{{1
void compute_macro_vars( struct lattice_struct *lattice, int which_f)
{
    int n, a;

    double *rho[ NUM_FLUID_COMPONENTS],
	*u_x[ NUM_FLUID_COMPONENTS],
	*u_y[ NUM_FLUID_COMPONENTS];

    double ux_sum, uy_sum;

    double *upr;

    double *f, *ftemp;

    bc_ptr bc;

    int    subs;

    double tau0,
	tau1;

    double c;



    //
    // Compute for substance 0:
    //
    subs = 0;

    rho[subs]   = &( lattice->macro_vars[subs][0].rho);
    u_x[subs]   =    lattice->macro_vars[subs][0].u;
    u_y[subs]   =    lattice->macro_vars[subs][0].u + 1;
    bc          =    lattice->bc[subs];
    switch(which_f)
    {
    case 0: // Compute from post-collision f.
	ftemp = lattice->pdf[subs][0].f;
	break;
    case 1: // Compute from pre-collision f.
	ftemp = lattice->pdf[subs][0].ftemp;
	break;
    case 2: // Compute average from pre- and post-collision f.
	f = lattice->pdf[subs][0].f;
	ftemp = lattice->pdf[subs][0].ftemp;
	break;
    default:
	break;
    }

    for( n=0; n<lattice->NumNodes; n++)
    {
	*rho[subs] = 0.;
	*u_x[subs] = 0.;
	*u_y[subs] = 0.;

	if( COMPUTE_ON_SOLIDS || is_not_solid_node( lattice, subs, n))
	{
	    for( a=0; a<9; a++)
	    {
		(*rho[subs]) += (*ftemp);
		(*u_x[subs]) += vx[a]*(*ftemp);
		(*u_y[subs]) += vy[a]*(*ftemp);
		ftemp++;

		if( which_f == 2)
		{
		    (*rho[subs]) += (*f);
		    (*u_x[subs]) += vx[a]*(*f);
		    (*u_y[subs]) += vy[a]*(*f);
		    f++;
		}

	    } /* for( a=0; a<9; a++) */	

#if SOURCE_ON
	    (*rho[subs]) += lattice->param.source_strength * lattice->param.tau[0];
	    if( which_f == 2)
	    {
		(*rho[subs]) += lattice->param.source_strength * lattice->param.tau[0];
	    }
#endif /*if SOURCE_ON*/

	    if( which_f == 2)
	    {
		(*rho[subs]) /= 2.;
		(*u_x[subs]) /= 2.;
		(*u_y[subs]) /= 2.;
	    }
	    else
	    {
#if GUO_ZHENG_SHI_BODY_FORCE
		*u_x[subs] += .5*lattice->param.gval[subs][0]*(*rho[subs]);
		*u_y[subs] += .5*lattice->param.gval[subs][1]*(*rho[subs]);
#endif
	    }

	    // PUKE_NEGATIVE_DENSITIES {{{
#if PUKE_NEGATIVE_DENSITIES
	    if( *rho[subs] < 0.)
	    {
		printf("\n");
		printf(
		       "compute_macro_vars() -- "
		       "Node %d (%d,%d) has negative density %20.17f "
		       "at timestep %d. Exiting!\n",
		       n, n%lattice->param.LX,
		       n/lattice->param.LX, *rho[subs],
		       lattice->time             );
		printf("\n");
		process_exit(1);
	    }
#endif
	    // PUKE_NEGATIVE_DENSITIES }}}
	    //assert( *rho[subs] != 0);

	} /* if( !( bc++->bc_type & BC_SOLID_NODE)) */

	else // bc++->bc_type & BC_SOLID_NODE {{{
	{
	    //printf("RHO: n=%d, Solid node.\n", n);
	    ftemp+=9;
	    if( which_f)
	    {
		f+=9;
	    }

	} /* if( !( bc++->bc_type & BC_SOLID_NODE)) else }}} */

	rho[subs]+=3;
	u_x[subs]+=3;
	u_y[subs]+=3;
	ftemp+=18;
	if( which_f==2)
	{
	    f+=18;
	}

    } /* for( n=0; n<lattice->NumNodes; n++) */


    //
    // Compute for substance 1:
    //
#if NUM_FLUID_COMPONENTS==2
    subs = 1;

    rho[subs]   = &( lattice->macro_vars[subs][0].rho);
    u_x[subs]   =    lattice->macro_vars[subs][0].u;
    u_y[subs]   =    lattice->macro_vars[subs][0].u + 1;
    bc          =    lattice->bc[subs]; 
    switch(which_f)
    {
    case 0: // Compute from post-collision f.
	ftemp = lattice->pdf[subs][0].f;
	break;
    case 1: // Compute from pre-collision f.
	ftemp = lattice->pdf[subs][0].ftemp;
	break;
    case 2: // Compute average from pre- and post-collision f.
	f = lattice->pdf[subs][0].f;
	ftemp = lattice->pdf[subs][0].ftemp;
	break;
    default:
	break;
    }

    for( n=0; n<lattice->NumNodes; n++)
    {
	*rho[subs] = 0.;
	*u_x[subs] = 0.;
	*u_y[subs] = 0.;

	if( COMPUTE_ON_SOLIDS || is_not_solid_node( lattice, subs, n))
	{
	    for( a=0; a<9; a++)
	    {
		(*rho[subs]) += (*ftemp);
		(*u_x[subs]) += vx[a]*(*ftemp);
		(*u_y[subs]) += vy[a]*(*ftemp);
		ftemp++;

		if( which_f == 2)
		{
		    (*rho[subs]) += (*f);
		    (*u_x[subs]) += vx[a]*(*f);
		    (*u_y[subs]) += vy[a]*(*f);
		    f++;
		}

	    } /* for( a=0; a<9; a++) */

	    if( which_f == 2)
	    {
		(*rho[subs]) /= 2.;
		(*u_x[subs]) /= 2.;
		(*u_y[subs]) /= 2.;
	    }
	    else
	    {
#if GUO_ZHENG_SHI_BODY_FORCE
		*u_x[subs] += .5*lattice->param.gval[subs][0]*(*rho[subs]);
		*u_y[subs] += .5*lattice->param.gval[subs][1]*(*rho[subs]);
#endif
	    }

	    // PUKE_NEGATIVE_DENSITIES {{{
#if PUKE_NEGATIVE_DENSITIES
	    if( *rho[subs] < 0.)
	    {
		printf("\n");
		printf(
		       "compute_macro_vars() -- "
		       "Node %d (%d,%d) has negative density %20.17f "
		       "at timestep %d. Exiting!\n",
		       n, n%lattice->param.LX,
		       n/lattice->param.LX, *rho[subs],
		       lattice->time             );
		printf("\n");
		process_exit(1);
	    }
#endif
	    // PUKE_NEGATIVE_DENSITIES }}}
	    //assert( *rho[subs] != 0);

	} /* if( !( bc++->bc_type & BC_SOLID_NODE)) */

	else // bc++->bc_type & BC_SOLID_NODE {{{
	{
	    //printf("RHO: n=%d, Solid node.\n", n);
	    ftemp+=9;
	    if( which_f)
	    {
		f+=9;
	    }

	} /* if( !( bc++->bc_type & BC_SOLID_NODE)) else }}} */

	rho[subs]+=3;
	u_x[subs]+=3;
	u_y[subs]+=3;
	ftemp+=18;
	if( which_f==2)
	{
	    f+=18;
	}

    } /* for( n=0; n<lattice->NumNodes; n++) */
#endif /*NUM_FLUID_COMPONENTS==2*/

    for( subs=0; subs<NUM_FLUID_COMPONENTS; subs++)
    {
	rho[subs]   = &( lattice->macro_vars[subs][0].rho);
	u_x[subs]   =    lattice->macro_vars[subs][0].u;
	u_y[subs]   =    lattice->macro_vars[subs][0].u + 1;

    } /* for( subs=0; subs<NUM_FLUID_COMPONENTS; subs++) */

#if STORE_U_COMPOSITE
    upr      =    lattice->upr[0].u;
#endif /* STORE_U_COMPOSITE */

    if( NUM_FLUID_COMPONENTS==2) // {{{
    {
	tau0 = lattice->param.tau[0];
	tau1 = lattice->param.tau[1];
	for( n=0; n<lattice->NumNodes; n++)
	{
	    if( COMPUTE_ON_SOLIDS || is_not_solid_node( lattice, subs, n))
	    {
		ux_sum =  *u_x[0]/tau0 + *u_x[1]/tau1;
		uy_sum =  *u_y[0]/tau0 + *u_y[1]/tau1;

#if STORE_U_COMPOSITE
		//assert( *rho[0] + *rho[1] != 0.);
		if( *rho[0] + *rho[1] != 0)
		{
		    *upr++ = ( ux_sum) / ( *rho[0]/tau0 + *rho[1]/tau1);
		    *upr++ = ( uy_sum) / ( *rho[0]/tau0 + *rho[1]/tau1);
		}
		else
		{
		    *upr++ = 0.;
		    *upr++ = 0.;
		}
#endif /* STORE_U_COMPOSITE */

		//assert( *rho[0] != 0.);
		//assert( *rho[1] != 0.);

		if( ux_sum != 0.)
		{
		    if( *rho[0] != 0)
		    {
			*u_x[0] = ux_sum / *rho[0];
		    }
		    else
		    {
			*u_x[0] = 0.;
		    }

		    if( *rho[1] != 0)
		    {
			*u_x[1] = ux_sum / *rho[1];
		    }
		    else
		    {
			*u_x[1] = 0.;
		    }
		}
		else
		{
		    *u_x[0] = 0.;
		    *u_x[1] = 0.;
		}

		if( uy_sum != 0.)
		{
		    if( *rho[0] != 0)
		    {
			*u_y[0] = uy_sum / *rho[0];
		    }
		    else
		    {
			*u_y[0] = 0.;
		    }

		    if( *rho[1] != 0)
		    {
			*u_y[1] = uy_sum / *rho[1];
		    }
		    else
		    {
			*u_y[1] = 0.;
		    }
		}
		else
		{
		    *u_y[0] = 0.;
		    *u_y[1] = 0.;
		}

	    } /* if( 1 || !( bc[n].bc_type & BC_SOLID_NODE)) */
#if STORE_U_COMPOSITE
	    else
	    {
		upr+=2;
	    } /* if( 1 || !( bc[n].bc_type & BC_SOLID_NODE)) else */
#endif /* STORE_U_COMPOSITE */

	    rho[0]+=3;
	    u_x[0]+=3;
	    u_y[0]+=3;

	    rho[1]+=3;
	    u_x[1]+=3;
	    u_y[1]+=3;

	} /* for( n=0; n<lattice->NumNodes; n++) */
    } // }}}
    else if( NUM_FLUID_COMPONENTS == 1) // {{{
    {
	for( n=0; n<lattice->NumNodes; n++)
	{
	    if( COMPUTE_ON_SOLIDS || is_not_solid_node( lattice, /*subs*/0, n))
	    {
		//assert( *rho[0] != 0.);

		if( lattice->param.incompressible) { c = 1.;} else { c = *rho[0];}
		if( c!=0&&*u_x[0]!=0.) { *u_x[0] /= c;} else { *u_x[0] = 0.;}
		if( c!=0&&*u_y[0]!=0.) { *u_y[0] /= c;} else { *u_y[0] = 0.;}

	    } /* if( 1 || !( bc[n].bc_type & BC_SOLID_NODE)) */

	    rho[0]+=3;
	    u_x[0]+=3;
	    u_y[0]+=3;

	} /* for( n=0; n<lattice->NumNodes; n++) */
    } // }}}
    else // {{{
    {
	printf(
	       "compute_macro_vars() -- "
	       "Unhandled case "
	       "NUM_FLUID_COMPONENTS = %d . "
	       "Exiting!\n",
	       NUM_FLUID_COMPONENTS);
	process_exit(1);
    } // }}}

} /* void compute_macro_vars( struct lattice_struct *lattice) */
// }}}
#endif /* INAMURO_SIGMA_COMPONENT */

// C O M P U T E _ F E Q  {{{1
//##############################################################################
//
// void compute_feq( struct lattice_struct *lattice)
//
//  - Compute equilibrium distribution function, feq.
//
void compute_feq( struct lattice_struct *lattice, int skip_sigma)
{
    int n, a;

    double rt0,  rt1,  rt2;
    double f1,   f2,   f3;
    double ux,   uy,
	uxsq, uysq, usq;
    double c;

    double *macro_var;
#if INAMURO_SIGMA_COMPONENT
    double *rho, *u, *u0, *u1;
#endif /* INAMURO_SIGMA_COMPONENT */
    double *feq;
#if STORE_U_COMPOSITE
    double *upr;
#endif /* STORE_U_COMPOSITE */
    bc_ptr bc;
    int    subs;

#if FREED_POROUS_MEDIA
    double Rx, Ry;
    double Gx, Gy;
    double u_x_p, u_y_p;
    double u_x_m, u_y_m;
    double edotu;
    double edotu_sq;
    double tau0 = lattice->param.tau[0];
    double nu = (1./3.)*(tau0-.5);
    double rho0;
    double wt[9]={4./9.,1./9.,1./9.,1./9.,1./9.,1./36.,1./36.,1./36.,1./36.};
#endif

#if NON_LOCAL_FORCES
    if( NUM_FLUID_COMPONENTS == 2)
    {
#if ZHANG_AND_CHEN_ENERGY_TRANSPORT
	compute_phase_force( lattice, 0);
#else /* !( ZHANG_AND_CHEN_ENERGY_TRANSPORT) */
	if( lattice->param.G != 0.)
	{
	    compute_fluid_fluid_force( lattice);
	}
	// NOTE: fluid/solid force is computed once in latman.c.
#endif /* ZHANG_AND_CHEN_ENERGY_TRANSPORT */
    }
    else if( NUM_FLUID_COMPONENTS == 1)
    {
	if( lattice->param.G != 0.)
	{
	    compute_phase_force( lattice, 0);
	}
	if(    lattice->param.Gads[0] != 0.)
	{
	    compute_single_fluid_solid_force( lattice, 0);
	}
    }
    else
    {
	printf(
	       "compute_feq() -- "
	       "Unhandled case NUM_FLUID_COMPONENTS = %d .  "
	       "Exiting!\n", NUM_FLUID_COMPONENTS);
	process_exit(1);
    }
    //dump_forces( lattice);
    //force2bmp( lattice);
    //printf("%s %d >> rand() = %f\n", __FILE__, __LINE__,
    //  .00001*(double)rand()/(double)RAND_MAX);
#endif

    f1 = 3.;
    f2 = 9./2.;
    f3 = 3./2.;

    for( subs=0; subs<(NUM_FLUID_COMPONENTS)-(INAMURO_SIGMA_COMPONENT); subs++)
    {

	macro_var = &( lattice->macro_vars[subs][0].rho);
#if INAMURO_SIGMA_COMPONENT
	u0         =    lattice->macro_vars[0][0].u;
	u1         =    lattice->macro_vars[1][0].u;
#endif /* INAMURO_SIGMA_COMPONENT */
	feq       =    lattice->pdf[subs][0].feq;
#if STORE_U_COMPOSITE
	upr       =    lattice->upr[0].u;
#endif /* STORE_U_COMPOSITE */
	bc        =    lattice->bc[subs];

	for( n=0; n<lattice->NumNodes; n++)
	{
	    if( COMPUTE_ON_SOLIDS || is_not_solid_node( lattice, subs, n))
	    {
		rt0 = *macro_var++; // Preserve raw density until after BIG_U.

		if( COMPUTE_ON_SOLIDS || is_not_solid_node( lattice, subs, n))
		{
#if STORE_U_COMPOSITE
		    ux = BIG_U_X( *upr, ((rt0!=0)?(rt0):(1))); *upr++;// = ux;
		    uy = BIG_U_Y( *upr, ((rt0!=0)?(rt0):(1))); *upr++;// = uy;
		    macro_var+=2;
#else /* !( STORE_U_COMPOSITE) */

#if INAMURO_SIGMA_COMPONENT
		    ux = BIG_U_X( *macro_var, rt0, lattice->macro_vars[1][n].rho);
		    macro_var++;
		    *u1++ = *u0++;
#else /* !( INAMURO_SIGMA_COMPONENT) */
		    ux = BIG_U_X( *macro_var, rt0); macro_var++;
#endif /* INAMURO_SIGMA_COMPONENT */
#if INAMURO_SIGMA_COMPONENT
#if 0
		    if(  //gravitationally_adjacent_to_a_solid( lattice, subs, n, 1) ||
		       //on_the_east_or_west( lattice, n)
		       on_the_east( lattice, n)
		       //0
			 )
		    {
			// Preliminary, experimental mechanism to skip applying the
			// gravity term when adjacent to a solid and/or on a node that
			// has a boundary condition enforced.
			uy = *macro_var;
		    }
#else
		    uy = BIG_U_Y( *macro_var, rt0, lattice->macro_vars[1][n].rho);
#endif
		    macro_var++;
		    *u1++ = *u0++;
#else /* !( INAMURO_SIGMA_COMPONENT) */

#if 0
		    if(  //gravitationally_adjacent_to_a_solid( lattice, subs, n, 1)
		       //||
		       on_the_east_or_west( lattice, n)
		       //on_the_east( lattice, n)
		       //0
			 )
		    {
			// Preliminary, experimental mechanism to skip applying the
			// gravity term when adjacent to a solid and/or on a node that
			// has a boundary condition enforced.
			uy = *macro_var;
		    }
#else
		    uy = BIG_U_Y( *macro_var, rt0);
#endif
		    macro_var++;

#endif /* INAMURO_SIGMA_COMPONENT */

#endif /* STORE_U_COMPOSITE */
		}
		else
		{
#if STORE_U_COMPOSITE
		    ux = *upr++;
		    uy = *upr++;
		    macro_var+=2;
#else /* !( STORE_U_COMPOSITE) */

		    ux = *macro_var++;
#if INAMURO_SIGMA_COMPONENT
		    *u1++ = *u0++;
#endif /* INAMURO_SIGMA_COMPONENT */
		    uy = *macro_var++;
#if INAMURO_SIGMA_COMPONENT
		    *u1++ = *u0++;
#endif /* INAMURO_SIGMA_COMPONENT */

#endif /* STORE_U_COMPOSITE */
		}

#if FREED_POROUS_MEDIA
		if( lattice->param.incompressible)
		{
		    printf("ERROR: Can't have incompressible with Freed PM!");
		    process_exit(1);
		    c  = rt0; // rt0 == rho
		    rt1 = 1./9.;
		    rt2 = 1./36.;
		    rt0 = 4./9.; // Overwrite rt0.
		}
		else // compressible
		{
		    rho0 = rt0;
		    c  = 1.;
		    rt1 = rt0/(9.); // rt0 == rho
		    rt2 = rt0/(36.);// rt0 == rho
		    rt0 *= (4./9.); // Update rt0 now that raw density is no longer needed.
		}


		Rx = lattice->param.ns;
		Ry = Rx;

		Gx = (1. - 3.*nu*Rx)/(1. + 0.5*Rx);
		Gy = (1. - 3.*nu*Ry)/(1. + 0.5*Ry);

		u_x_p = Gx*ux //u_x_p is post-collision velocity,
		    + lattice->param.gval[subs][0]*tau0/rho0;
		u_y_p = Gy*uy // u_x is pre-collision velocity
		    + lattice->param.gval[subs][1]*tau0/rho0;

		u_x_m = (1. - 0.5/tau0)*ux + 0.5*u_x_p/tau0; // u_x_m is mean velocity
		u_y_m = (1. - 0.5/tau0)*uy + 0.5*u_y_p/tau0;
		usq = pow(u_x_m, 2) + pow(u_y_m, 2);

		//   printf("ux =%12.10f ux_p= %12.10f ux_m=%12.10f \n", u_x, u_x_p, u_x_m);

		for(a=0; a<9; a++)
		{
		    edotu = *(vx + a)*u_x_p + *(vy +a)*u_y_p;
		    edotu_sq = pow( ( *(vx + a)*u_x_m + *(vy +a)*u_y_m ), 2.);

		    *feq++ = *(wt +a)*rho0 *(1. + 3.*edotu + (9./2.)*edotu_sq - (3./2.)*usq );

		}

		ux = u_x_m;
		uy = u_y_m;
		lattice->macro_vars[subs][n].u[0] = ux;
		lattice->macro_vars[subs][n].u[1] = uy;

#else

		if( lattice->param.incompressible)
		{
		    c  = rt0; // rt0 == rho
		    rt1 = 1./9.;
		    rt2 = 1./36.;
		    rt0 = 4./9.; // Overwrite rt0.
		}
		else // compressible
		{
		    c  = 1.;
		    rt1 = rt0/(9.); // rt0 == rho
		    rt2 = rt0/(36.);// rt0 == rho
		    rt0 *= (4./9.); // Update rt0 now that raw density is no longer needed.
		}


		uxsq = ux*ux;
		uysq = uy*uy;

		usq = uxsq + uysq;

		*feq++ = rt0 * (c - f3*usq);
		*feq++ = rt1 * (c + f1*ux + f2*uxsq - f3*usq);
		*feq++ = rt1 * (c + f1*uy + f2*uysq - f3*usq);
		*feq++ = rt1 * (c - f1*ux + f2*uxsq - f3*usq);
		*feq++ = rt1 * (c - f1*uy + f2*uysq - f3*usq);
		*feq++ = rt2*(c+f1*( ux+uy)+f2*( ux+uy)*( ux+uy)-f3*usq);
		*feq++ = rt2*(c+f1*(-ux+uy)+f2*(-ux+uy)*(-ux+uy)-f3*usq);
		*feq++ = rt2*(c+f1*(-ux-uy)+f2*(-ux-uy)*(-ux-uy)-f3*usq);
		*feq++ = rt2*(c+f1*( ux-uy)+f2*( ux-uy)*( ux-uy)-f3*usq);
#endif

		feq+=18;

#if INAMURO_SIGMA_COMPONENT
		u0++;
		u1++;
#endif /* INAMURO_SIGMA_COMPONENT */
	    }
	    else
	    {
#if 1
		*feq++ = 0.;
		*feq++ = 0.;
		*feq++ = 0.;
		*feq++ = 0.;
		*feq++ = 0.;
		*feq++ = 0.;
		*feq++ = 0.;
		*feq++ = 0.;
		*feq++ = 0.;
		feq+=18;
#else
		feq+=27;
#endif
		macro_var+=3;
#if INAMURO_SIGMA_COMPONENT
		u0+=3;
		u1+=3;
#endif /* INAMURO_SIGMA_COMPONENT */
#if STORE_U_COMPOSITE
		upr+=2;
#endif /* STORE_U_COMPOSITE */
	    }

	} /* for( n=0; n<lattice->NumNodes; n++) */

    } /* for( subs=0; subs<NUM_FLUID_COMPONENTS; subs++) */

#if INAMURO_SIGMA_COMPONENT

    if( !skip_sigma)
    {
	subs=1;

	c  = 1.;

#if ZHANG_AND_CHEN_ENERGY_TRANSPORT
	f1 = 3.;
	f2 = 9./2.;
	f3 = 3./2.;
#else /* !( ZHANG_AND_CHEN_ENERGY_TRANSPORT) */
	f1 = 3.;
	f2 = 0.;
	f3 = 0.;
#endif /* ZHANG_AND_CHEN_ENERGY_TRANSPORT */

	rho       = &( lattice->macro_vars[subs][0].rho);
	u         =    lattice->macro_vars[subs][0].u;
	feq       =    lattice->pdf[subs][0].feq;
	bc        =    lattice->bc[subs];

	for( n=0; n<lattice->NumNodes; n++)
	{
	    if( COMPUTE_ON_SOLIDS || is_not_solid_node( lattice, subs, n))
	    {
		rt0 = *rho; // Preserve raw density until after BIG_U.
		rho+=3;

		ux = *u++;
		uy = *u++;

		if( lattice->param.simple_diffusion)
		{
		    rt1 = rt0/(8.); // rt0 == rho
		    rt0 *= (1./2.); // Update rt0 now that raw density is no longer needed.

		    *feq++ = rt0 * ( 1.             );

		    *feq++ = rt1 * ( c + f1*ux      );
		    *feq++ = rt1 * ( c + f1*uy      );
		    *feq++ = rt1 * ( c - f1*ux      );
		    *feq++ = rt1 * ( c - f1*uy      );

		    *feq++ = 0.; //rt2 * ( c + f1*( ux+uy));
		    *feq++ = 0.; //rt2 * ( c + f1*(-ux+uy));
		    *feq++ = 0.; //rt2 * ( c + f1*(-ux-uy));
		    *feq++ = 0.; //rt2 * ( c + f1*( ux-uy));
		}
		else
		{
		    rt1 = rt0/(9.); // rt0 == rho
		    rt2 = rt0/(36.);// rt0 == rho
		    rt0 *= (4./9.); // Update rt0 now that raw density is no longer needed.

#if ZHANG_AND_CHEN_ENERGY_TRANSPORT
		    uxsq = ux*ux;
		    uysq = uy*uy;

		    usq = uxsq + uysq;

		    *feq++ = rt0 * (c - f3*usq);
		    *feq++ = rt1 * (c + f1*ux + f2*uxsq - f3*usq);
		    *feq++ = rt1 * (c + f1*uy + f2*uysq - f3*usq);
		    *feq++ = rt1 * (c - f1*ux + f2*uxsq - f3*usq);
		    *feq++ = rt1 * (c - f1*uy + f2*uysq - f3*usq);
		    *feq++ = rt2*(c+f1*( ux+uy)+f2*( ux+uy)*( ux+uy)-f3*usq);
		    *feq++ = rt2*(c+f1*(-ux+uy)+f2*(-ux+uy)*(-ux+uy)-f3*usq);
		    *feq++ = rt2*(c+f1*(-ux-uy)+f2*(-ux-uy)*(-ux-uy)-f3*usq);
		    *feq++ = rt2*(c+f1*( ux-uy)+f2*( ux-uy)*( ux-uy)-f3*usq);
#else /* !( ZHANG_AND_CHEN_ENERGY_TRANSPORT) */
		    *feq++ = rt0 * ( 1.             );

		    *feq++ = rt1 * ( c + f1*ux      );
		    *feq++ = rt1 * ( c + f1*uy      );
		    *feq++ = rt1 * ( c - f1*ux      );
		    *feq++ = rt1 * ( c - f1*uy      );

		    *feq++ = rt2 * ( c + f1*( ux+uy));
		    *feq++ = rt2 * ( c + f1*(-ux+uy));
		    *feq++ = rt2 * ( c + f1*(-ux-uy));
		    *feq++ = rt2 * ( c + f1*( ux-uy));
#endif /* ZHANG_AND_CHEN_ENERGY_TRANSPORT */
		}

		feq+=18;
		u++;
	    }
	    else
	    {
#if 1
		*feq++ = 0.;
		*feq++ = 0.;
		*feq++ = 0.;
		*feq++ = 0.;
		*feq++ = 0.;
		*feq++ = 0.;
		*feq++ = 0.;
		*feq++ = 0.;
		*feq++ = 0.;
		feq+=18;
#else
		feq+=27;
#endif
		rho+=1;
		u+=3;
	    }

	} /* for( n=0; n<lattice->NumNodes; n++) */
    }

#endif /* INAMURO_SIGMA_COMPONENT */

    //dump_pdf( lattice, lattice->time);

} /* void compute_feq( struct lattice_struct *lattice) */
// }}}

#if NON_LOCAL_FORCES
#if 1
// C O M P U T E _ F L U I D _ F L U I D _ F O R C E  {{{1
void compute_fluid_fluid_force( lattice_ptr lattice)
{

    printf("BING: compute_fluid_fluid_force()\n");

    double ***psi; //psi[ NUM_FLUID_COMPONENTS][LX][LY];
    double psi_temp;

    double *rho;

    double *force[2];

    int    i,  j,
	in, jn,
	ip, jp;

    int    n, LX, LY;

    int    sj, ej;

    int    subs;

    LX = lattice->param.LX;
    LY = lattice->param.LY;

    psi =    ( double***)malloc( (NUM_FLUID_COMPONENTS)*sizeof(double**));
    for( subs=0; subs<NUM_FLUID_COMPONENTS; subs++)
    {
	psi[subs] = ( double**)malloc( LY*sizeof(double*));
    }
    for( subs=0; subs<NUM_FLUID_COMPONENTS; subs++)
    {
	for( j=0; j<LY; j++)
	{
	    psi[subs][j] = ( double*)malloc( LX*sizeof(double));
	}

    } /* for( subs=0; subs<NUM_FLUID_COMPONENTS; subs++) */

    // Initialize psi.
    for( subs=0; subs<NUM_FLUID_COMPONENTS; subs++)
    {
	for( j=0; j<LY; j++)
	{
	    for( i=0; i<LX; i++, n++)
	    {
		psi[subs][j][i] = 0.;
	    }
	}

	if( lattice->periodic_y[subs])
	{
	    sj = 0;
	    ej = LY-1;
	    rho = &( lattice->macro_vars[subs][0].rho);
	}
	else
	{
	    sj = 1;
	    ej = LY-2;
	    rho = &( lattice->macro_vars[subs][LX].rho);
	}
	if( !lattice->periodic_x[subs])
	{
	    printf("%s %d >> Need to be periodic in x-direction to use "
		   "NON_LOCAL_FORCES for now.  Exiting!\n",
		   __FILE__, __LINE__);
	    process_exit(1);
	}

	for( j=sj; j<=ej; j++)
	{
	    n = j*LX;
	    for( i=0; i<LX; i++, n++)
	    {
		if( is_not_solid_node( lattice, subs, n))
		{
		    psi[subs][j][i] = *rho;
		}

		rho+=3;

	    } /* for( i=0; i<LX; i++) */
	} /* for( j=0; j<LY; j++) */
    } /* for( subs=0; subs<NUM_FLUID_COMPONENTS; subs++) */

    //printf("sizeof( struct force_struct) = %d\n", sizeof( struct force_struct));
    //printf("NumNodes*sizeof( struct force_struct) = %d\n",
    //    lattice->NumNodes*sizeof( struct force_struct));

    for( subs=0; subs<NUM_FLUID_COMPONENTS; subs++)
    {
	if( lattice->periodic_y[subs])
	{
	    sj = 0;
	    ej = LY-1;
	    force[subs] = lattice->force[subs][0].force;
	}
	else
	{
	    sj = 1;
	    ej = LY-2;
	    force[subs] = lattice->force[subs][LX].force;
	}

	for( j=sj; j<=ej; j++)
	{
	    jp = ( j<LY-1)?( j+1):( 0   );
	    jn = ( j>0   )?( j-1):( LY-1);

	    for( i=0; i<LX; i++)
	    {
		ip = ( i<LX-1)?( i+1):( 0   );
		in = ( i>0   )?( i-1):( LX-1);

		//printf("compute_fluid_fluid_force() -- "
		//    "subs %d, ( i, j) = ( %2d, %2d), | f - f0| = %d\n",
		//    subs, i, j,
		//    force[subs] - lattice->force[subs][0].force );

		*( force[subs]  ) = 0.;
		*( force[subs]+1) = 0.;

		if( !( lattice->bc[subs][ j*LX+i].bc_type & BC_SOLID_NODE))
		{
		    /* 1 */ if( !( lattice->bc[subs][ j *LX+ip].bc_type & BC_SOLID_NODE)) {
			*( force[subs]  ) += WM*vx[1]*psi[subs][j ][ip];
			*( force[subs]+1) += WM*vy[1]*psi[subs][j ][ip]; }
		    /* 2 */ if( !( lattice->bc[subs][ jp*LX+i ].bc_type & BC_SOLID_NODE)) {
			*( force[subs]  ) += WM*vx[2]*psi[subs][jp][i ];
			*( force[subs]+1) += WM*vy[2]*psi[subs][jp][i ]; }
		    /* 3 */ if( !( lattice->bc[subs][ j *LX+in].bc_type & BC_SOLID_NODE)) {
			*( force[subs]  ) += WM*vx[3]*psi[subs][j ][in];
			*( force[subs]+1) += WM*vy[3]*psi[subs][j ][in]; }
		    /* 4 */ if( !( lattice->bc[subs][ jn*LX+i ].bc_type & BC_SOLID_NODE)) {
			*( force[subs]  ) += WM*vx[4]*psi[subs][jn][i ];
			*( force[subs]+1) += WM*vy[4]*psi[subs][jn][i ]; }
		    /* 5 */ if( !( lattice->bc[subs][ jp*LX+ip].bc_type & BC_SOLID_NODE)) {
			*( force[subs]  ) += WD*vx[5]*psi[subs][jp][ip];
			*( force[subs]+1) += WD*vy[5]*psi[subs][jp][ip]; }
		    /* 6 */ if( !( lattice->bc[subs][ jp*LX+in].bc_type & BC_SOLID_NODE)) {
			*( force[subs]  ) += WD*vx[6]*psi[subs][jp][in];
			*( force[subs]+1) += WD*vy[6]*psi[subs][jp][in]; }
		    /* 7 */ if( !( lattice->bc[subs][ jn*LX+in].bc_type & BC_SOLID_NODE)) {
			*( force[subs]  ) += WD*vx[7]*psi[subs][jn][in];
			*( force[subs]+1) += WD*vy[7]*psi[subs][jn][in]; }
		    /* 8 */ if( !( lattice->bc[subs][ jn*LX+ip].bc_type & BC_SOLID_NODE)) {
			*( force[subs]  ) += WD*vx[8]*psi[subs][jn][ip];
			*( force[subs]+1) += WD*vy[8]*psi[subs][jn][ip]; }

		} /* if( !( lattice->bc[subs][n].bc_type & BC_SOLID_NODE)) */

		force[subs] += ( sizeof( struct force_struct)/8);

	    } /* for( i=0; i<LX; i++) */
	} /* for( j=0; j<LY; j++) */
    } /* for( subs=0; subs<NUM_FLUID_COMPONENTS; subs++) */

    force[0] = lattice->force[0][0].force;
    force[1] = lattice->force[1][0].force;
    for( j=0; j<LY; j++)
    {
	for( i=0; i<LX; i++)
	{
	    if( !( lattice->bc[0][ j*LX+i].bc_type & BC_SOLID_NODE))
	    {
		psi_temp = *( force[1]  );
		*( force[1]  ) = -lattice->param.G*psi[1][j][i]*( *(force[0]  ));
		*( force[0]  ) = -lattice->param.G*psi[0][j][i]*( psi_temp     );

		psi_temp = *( force[1]+1);
		*( force[1]+1) = -lattice->param.G*psi[1][j][i]*( *(force[0]+1));
		*( force[0]+1) = -lattice->param.G*psi[0][j][i]*( psi_temp     );

	    } /* if( !( lattice->bc[0][n].bc_type & BC_SOLID_NODE)) */

	    force[0] += ( sizeof( struct force_struct)/8);
	    force[1] += ( sizeof( struct force_struct)/8);

	} /* for( i=0; i<LX; i++) */
    } /* for( j=0; j<LY; j++) */

    for( subs=0; subs<NUM_FLUID_COMPONENTS; subs++)
    {
	for( j=0; j<LY; j++)
	{
	    free( psi[subs][j]);
	}
    }
    for( subs=0; subs<NUM_FLUID_COMPONENTS; subs++)
    {
	free( psi[subs]);
    }
    free( psi);

} /* void compute_fluid_fluid_force( lattice_ptr lattice) */
// }}}
#else
// C O M P U T E _ F L U I D _ F L U I D _ F O R C E  {{{1
void compute_fluid_fluid_force( lattice_ptr lattice)
{
    double ***psi; //psi[ NUM_FLUID_COMPONENTS][LX][LY];
    double psi_x[2];
    double psi_y[2];

    double *rho;

    double *force[2];

    int    i,  j,
	in, jn,
	ip, jp;

    int    n, LX, LY;

    int    subs;

    LX = lattice->param.LX;
    LY = lattice->param.LY;

    psi =    ( double***)malloc( (NUM_FLUID_COMPONENTS)*sizeof(double**));
    for( subs=0; subs<NUM_FLUID_COMPONENTS; subs++)
    {
	psi[subs] = ( double**)malloc( LY*sizeof(double*));
    }
    for( subs=0; subs<NUM_FLUID_COMPONENTS; subs++)
    {
	for( j=0; j<LY; j++)
	{
	    psi[subs][j] = ( double*)malloc( LY*sizeof(double));
	}
    }

    // Initialize psi.
    for( subs=0; subs<NUM_FLUID_COMPONENTS; subs++)
    {
	rho = &( lattice->macro_vars[subs][0].rho);

	if( !lattice->periodic_x[subs] || !lattice->periodic_y[subs])
	{
	    printf("%s %d >> Need to be fully periodic to use "
		   "NON_LOCAL_FORCES for now.  Exiting!\n"
		   __FILE__, __LINE__);
	    process_exit(1);
	}

	for( j=0; j<LY; j++)
	{
	    n = j*LX;
	    for( i=0; i<LX; i++, n++)
	    {
		if( !( lattice->bc[subs][n].bc_type & BC_SOLID_NODE))
		{
		    psi[subs][j][i] = *rho;
		}
		else // lattice->bc[subs][n].bc_type & BC_SOLID_NODE
		{
		    psi[subs][j][i] = 0.;
		}

		rho+=3;

	    } /* for( i=0; i<LX; i++) */
	} /* for( j=0; j<LY; j++) */
    } /* for( subs=0; subs<NUM_FLUID_COMPONENTS; subs++) */

    //printf("sizeof( struct force_struct) = %d\n", sizeof( struct force_struct));
    //printf("NumNodes*sizeof( struct force_struct) = %d\n",
    //    lattice->NumNodes*sizeof( struct force_struct));


    for( j=0; j<LY; j++)
    {
	jp = ( j<LY-1)?( j+1):( 0   );
	jn = ( j>0   )?( j-1):( LY-1);

	for( i=0; i<LX; i++)
	{
	    ip = ( i<LX-1)?( i+1):( 0   );
	    in = ( i>0   )?( i-1):( LX-1);

	    if( !( lattice->bc[0][ j*LX+i].bc_type & BC_SOLID_NODE))
	    {

		for( subs=0; subs<NUM_FLUID_COMPONENTS; subs++)
		{
		    force[subs] = lattice->force[subs][j*LX+i].force;

		    psi_x[subs] = 0.;
		    psi_y[subs] = 0.;

		    /* 1 */ if( !( lattice->bc[subs][ j *LX+ip].bc_type & BC_SOLID_NODE)) {
			psi_x[subs] += 2.*vx[1]*psi[subs][j ][ip];
			psi_y[subs] += 2.*vy[1]*psi[subs][j ][ip]; }
		    /* 2 */ if( !( lattice->bc[subs][ jp*LX+i ].bc_type & BC_SOLID_NODE)) {
			psi_x[subs] += 2.*vx[2]*psi[subs][jp][i ];
			psi_y[subs] += 2.*vy[2]*psi[subs][jp][i ]; }
		    /* 3 */ if( !( lattice->bc[subs][ j *LX+in].bc_type & BC_SOLID_NODE)) {
			psi_x[subs] += 2.*vx[3]*psi[subs][j ][in];
			psi_y[subs] += 2.*vy[3]*psi[subs][j ][in]; }
		    /* 4 */ if( !( lattice->bc[subs][ jn*LX+i ].bc_type & BC_SOLID_NODE)) {
			psi_x[subs] += 2.*vx[4]*psi[subs][jn][i ];
			psi_y[subs] += 2.*vy[4]*psi[subs][jn][i ]; }
		    /* 5 */ if( !( lattice->bc[subs][ jp*LX+ip].bc_type & BC_SOLID_NODE)) {
			psi_x[subs] +=    vx[5]*psi[subs][jp][ip];
			psi_y[subs] +=    vy[5]*psi[subs][jp][ip]; }
		    /* 6 */ if( !( lattice->bc[subs][ jp*LX+in].bc_type & BC_SOLID_NODE)) {
			psi_x[subs] +=    vx[6]*psi[subs][jp][in];
			psi_y[subs] +=    vy[6]*psi[subs][jp][in]; }
		    /* 7 */ if( !( lattice->bc[subs][ jn*LX+in].bc_type & BC_SOLID_NODE)) {
			psi_x[subs] +=    vx[7]*psi[subs][jn][in];
			psi_y[subs] +=    vy[7]*psi[subs][jn][in]; }
		    /* 8 */ if( !( lattice->bc[subs][ jn*LX+ip].bc_type & BC_SOLID_NODE)) {
			psi_x[subs] +=    vx[8]*psi[subs][jn][ip];
			psi_y[subs] +=    vy[8]*psi[subs][jn][ip]; }

		} /* for( subs=0; subs<NUM_FLUID_COMPONENTS; subs++) */

		lattice->force[0][j*LX+i].force[0]
		    = -lattice->param.G*psi[0][j][i]*( psi_x[1]);
		lattice->force[0][j*LX+i].force[1]
		    = -lattice->param.G*psi[0][j][i]*( psi_y[1]);
		lattice->force[1][j*LX+i].force[0]
		    = -lattice->param.G*psi[1][j][i]*( psi_x[0]);
		lattice->force[1][j*LX+i].force[1]
		    = -lattice->param.G*psi[1][j][i]*( psi_y[0]);
	    }
	    else
	    {
		lattice->force[0][j*LX+i].force[0] = 0.;
		lattice->force[0][j*LX+i].force[1] = 0.;
		lattice->force[1][j*LX+i].force[0] = 0.;
		lattice->force[1][j*LX+i].force[1] = 0.;
	    }

	} /* for( i=0; i<LX; i++) */
    } /* for( j=0; j<LY; j++) */

    for( subs=0; subs<NUM_FLUID_COMPONENTS; subs++)
    {
	for( j=0; j<LY; j++)
	{
	    free( psi[subs][j]);
	}
    }
    for( subs=0; subs<NUM_FLUID_COMPONENTS; subs++)
    {
	free( psi[subs]);
    }
    free( psi);

} /* void compute_fluid_fluid_force( lattice_ptr lattice) */
// }}}
#endif

#if ZHANG_AND_CHEN_ENERGY_TRANSPORT
// C O M P U T E _ P H A S E _ F O R C E  {{{1
//
// Based on Zhang & Chen, PRE 67, 0066711 (2003)
//
// Supposed to give thermodynamic consistency unlike old Shan & Chen method.
// And supports general equation of state P = P(rho,T).
// Utilizes the Inamuro component for evolution of the energy transport
// equation.  Employs modified compute_phase_force routine to compute
// body force term representing non-local interaction potential U among
// particles.
//
void compute_phase_force( lattice_ptr lattice, int subs)
{
    double **U;

    double *rho;
    double *T;

    double *force;

    double a, b, R;

    int    i,  j,
	in, jn,
	ip, jp;

    int    n, LX, LY;

    int    sj, ej;

    double y;

    LX = lattice->param.LX;
    LY = lattice->param.LY;

    U = ( double**)malloc( LY*sizeof(double*));
    for( j=0; j<LY; j++)
    {
	U[j] = ( double*)malloc( LX*sizeof(double));
    }

    // Initialize U.
    for( j=0; j<LY; j++)
    {
	for( i=0; i<LX; i++, n++)
	{
	    U[j][i] = 0.;
	}
    }

    if( !lattice->periodic_x[0] || !lattice->periodic_y[0])
    {
	printf("%s %d >> Need to be fully periodic to use "
	       "ZHANG_AND_CHEN_ENERGY_TRANSPORT for now.  Exiting!\n"
	       __FILE__, __LINE__);
	process_exit(1);

    } /* if( !lattice->periodic_x[0] || !lattice->periodic_y[0]) */

    sj = 0;
    ej = LY-1;

    //############################################################################
    //
    // Set U = P(rho,T) - rho*T0
    //
    //       = R*T*( rho/(1-rho*b)) - a*rho^2
    //
    // Maxwell ==> rho_l = 10.8657, rho_v = 4.98648
    //
    a  = 3.592;
    b  = 0.04267;
    R  = 0.082057;
    rho = &( lattice->macro_vars[/*subs*/0][0].rho);
    T   = &( lattice->macro_vars[/*subs*/1][0].rho);
    for( j=sj; j<=ej; j++)
    {
	n = j*LX;

	for( i=0; i<LX; i++, n++)
	{
	    if( !( lattice->bc[0][n].bc_type & BC_SOLID_NODE))
	    {
#if 0
		// Van der Walls
		U[j][i] = R*(*T)*( (*rho)/( 1. - (*rho)*b))
		    - a*(*rho)*(*rho);
#else
		// Carnahan-Starling
		y = (*rho)*b/4.;
		U[j][i] = R*(*T)*(*rho)*( ( 1. + y + y*y - y*y*y)
					  / ( (1.-y)*(1.-y)*(1.-y)))
		    - a*(*rho)*(*rho);
		//printf("%s (%d) >> U[%d][%d](rho=%f,T=%f) = %20.17f\n",
		//    __FILE__, __LINE__, j, i, *rho, *T, U[j][i]);
#endif

	    } /* if( !( lattice->bc[0][n].bc_type & BC_SOLID_NODE)) */

	    rho+=3;
	    T  +=3;

	} /* for( i=0; i<LX; i++) */
    } /* for( j=0; j<LY; j++) */

    //printf("sizeof( struct force_struct) = %d\n", sizeof( struct force_struct));
    //printf("NumNodes*sizeof( struct force_struct) = %d\n",
    //    lattice->NumNodes*sizeof( struct force_struct));

    //############################################################################
    //
    // Compute F(x,t) = -\sum_i ( D/(b*c_i^2)) e_i U( x+e_i,t)
    //

    sj = 0;
    ej = LY-1;
    force = ( lattice->force[/*subs*/0][0].force);

    for( j=sj; j<=ej; j++)
    {
	jp = ( j<LY-1)?( j+1):( 0   );
	jn = ( j>0   )?( j-1):( LY-1);

	for( i=0; i<LX; i++)
	{
	    ip = ( i<LX-1)?( i+1):( 0   );
	    in = ( i>0   )?( i-1):( LX-1);

	    *( force  ) = 0.;
	    *( force+1) = 0.;

	    if( !( lattice->bc[0][ j*LX+i].bc_type & BC_SOLID_NODE))
	    {
		/* 1 */ if( !( lattice->bc[0][ j *LX+ip].bc_type & BC_SOLID_NODE)) {
		    *( force  ) += 1.*vx[1]*U[j ][ip];
		    *( force+1) += 1.*vy[1]*U[j ][ip]; }
		/* 2 */ if( !( lattice->bc[0][ jp*LX+i ].bc_type & BC_SOLID_NODE)) {
		    *( force  ) += 1.*vx[2]*U[jp][i ];
		    *( force+1) += 1.*vy[2]*U[jp][i ]; }
		/* 3 */ if( !( lattice->bc[0][ j *LX+in].bc_type & BC_SOLID_NODE)) {
		    *( force  ) += 1.*vx[3]*U[j ][in];
		    *( force+1) += 1.*vy[3]*U[j ][in]; }
		/* 4 */ if( !( lattice->bc[0][ jn*LX+i ].bc_type & BC_SOLID_NODE)) {
		    *( force  ) += 1.*vx[4]*U[jn][i ];
		    *( force+1) += 1.*vy[4]*U[jn][i ]; }
		/* 5 */ if( !( lattice->bc[0][ jp*LX+ip].bc_type & BC_SOLID_NODE)) {
		    *( force  ) +=    vx[5]*U[jp][ip];
		    *( force+1) +=    vy[5]*U[jp][ip]; }
		/* 6 */ if( !( lattice->bc[0][ jp*LX+in].bc_type & BC_SOLID_NODE)) {
		    *( force  ) +=    vx[6]*U[jp][in];
		    *( force+1) +=    vy[6]*U[jp][in]; }
		/* 7 */ if( !( lattice->bc[0][ jn*LX+in].bc_type & BC_SOLID_NODE)) {
		    *( force  ) +=    vx[7]*U[jn][in];
		    *( force+1) +=    vy[7]*U[jn][in]; }
		/* 8 */ if( !( lattice->bc[0][ jn*LX+ip].bc_type & BC_SOLID_NODE)) {
		    *( force  ) +=    vx[8]*U[jn][ip];
		    *( force+1) +=    vy[8]*U[jn][ip]; }

		*( force  ) = -(1./8.)*( *(force  ));
		*( force+1) = -(1./8.)*( *(force+1));
		//printf("%s (%d) >> Fx = %20.17f\n", __FILE__, __LINE__, *(force  ));
		//printf("%s (%d) >> Fy = %20.17f\n", __FILE__, __LINE__, *(force+1));

	    } /* if( !( lattice->bc[0][n].bc_type & BC_SOLID_NODE)) */

	    else
	    {
		*( force  ) = 0.;
		*( force+1) = 0.;
		//printf("%s (%d) >> Fx = ZERO\n", __FILE__, __LINE__);
		//printf("%s (%d) >> Fy = ZERO\n", __FILE__, __LINE__);
	    }

	    force += ( sizeof( struct force_struct)/8);

	} /* for( i=0; i<LX; i++) */
    } /* for( j=0; j<LY; j++) */

    for( j=0; j<LY; j++)
    {
	free( U[j]);
    }
    free( U);

} /* void compute_phase_force( lattice_ptr lattice, int subs) */
// }}}
#else /* !( ZHANG_AND_CHEN_ENERGY_TRANSPORT) */
// C O M P U T E _ P H A S E _ F O R C E  {{{1
void compute_phase_force( lattice_ptr lattice, int subs)
{
    double **psi; //psi[ NUM_FLUID_COMPONENTS][LX][LY];
    double psi_temp;

    double *rho;

    double *force;

    int    i,  j,
	in, jn,
	ip, jp;

    int    n, LX, LY;

    int    sj, ej;

    LX = lattice->param.LX;
    LY = lattice->param.LY;

    psi =    ( double**)malloc( LY*sizeof(double**));
    for( j=0; j<LY; j++)
    {
	psi[j] = ( double*)malloc( LX*sizeof(double));
    }

    // Initialize psi.
    for( j=0; j<LY; j++)
    {
	for( i=0; i<LX; i++, n++)
	{
	    psi[j][i] = 0.;
	}
    }

    //if( lattice->periodic_y[subs])
    //{
    sj = 0;
    ej = LY-1;
    rho = &( lattice->macro_vars[subs][0].rho);
    //}
    //else
    //{
    //  sj = 1;
    //  ej = LY-2;
    //  rho = &( lattice->macro_vars[subs][LX].rho);
    //}

    for( j=sj; j<=ej; j++)
    {
	n = j*LX;
	for( i=0; i<LX; i++, n++)
	{
	    if( *rho!=0 && !( lattice->bc[subs][n].bc_type & BC_SOLID_NODE))
	    {
		psi[j][i] = 4.*exp(-200./(*rho));
	    }
	    else
	    {
		psi[j][i] = 0.;
	    }

	    rho+=3;

	} /* for( i=0; i<LX; i++) */
    } /* for( j=0; j<LY; j++) */

    if( is_periodic_in_y(lattice,subs))
    {
	sj = 0;
	ej = LY-1;
	force = lattice->force[subs][0].force;

    } /* if( is_periodic_in_y(lattice,subs)) */

    else
	//{
	//  sj = 1;
	//  ej = LY-2;
	//  force = lattice->force[subs][LX].force;
	//}
    {
	j = 0;
	jp = j+1;
	force = lattice->force[subs][0].force;
	for( i=0; i<LX; i++)
	{
	    ip = ( i<LX-1)?( i+1):( 0   );
	    in = ( i>0   )?( i-1):( LX-1);

	    *( force  ) = 0.;
	    *( force+1) = 0.;

	    if( !( lattice->bc[subs][ j*LX+i].bc_type & BC_SOLID_NODE))
	    {
		if( !( lattice->bc[subs][ j *LX+ip].bc_type & BC_SOLID_NODE)) {
		    *( force  ) += WM*vx[1]*psi[j ][ip];
		    *( force+1) += WM*vy[1]*psi[j ][ip]; }
		if( !( lattice->bc[subs][ jp*LX+i ].bc_type & BC_SOLID_NODE)) {
		    *( force  ) += WM*vx[2]*psi[jp][i ];
		    *( force+1) += WM*vy[2]*psi[jp][i ]; }
		if( !( lattice->bc[subs][ j *LX+in].bc_type & BC_SOLID_NODE)) {
		    *( force  ) += WM*vx[3]*psi[j ][in];
		    *( force+1) += WM*vy[3]*psi[j ][in]; }
		/**/    if( !( lattice->bc[subs][ j *LX+i ].bc_type & BC_SOLID_NODE)) {
		    /**/      *( force  ) += WM*vx[4]*psi[j ][i ];
		    /**/      *( force+1) += WM*vy[4]*psi[j ][i ]; }
		if( !( lattice->bc[subs][ jp*LX+ip].bc_type & BC_SOLID_NODE)) {
		    *( force  ) += WD*vx[5]*psi[jp][ip];
		    *( force+1) += WD*vy[5]*psi[jp][ip]; }
		if( !( lattice->bc[subs][ jp*LX+in].bc_type & BC_SOLID_NODE)) {
		    *( force  ) += WD*vx[6]*psi[jp][in];
		    *( force+1) += WD*vy[6]*psi[jp][in]; }
		/**/    if( !( lattice->bc[subs][ j *LX+in].bc_type & BC_SOLID_NODE)) {
		    /**/      *( force  ) += WD*vx[7]*psi[j ][in];
		    /**/      *( force+1) += WD*vy[7]*psi[j ][in]; }
		/**/    if( !( lattice->bc[subs][ j *LX+ip].bc_type & BC_SOLID_NODE)) {
		    /**/      *( force  ) += WD*vx[8]*psi[j ][ip];
		    /**/      *( force+1) += WD*vy[8]*psi[j ][ip]; }

		*( force  ) = -lattice->param.G*psi[j][i]*( *(force  ));
		*( force+1) = -lattice->param.G*psi[j][i]*( *(force+1));

	    } /* if( !( lattice->bc[subs][n].bc_type & BC_SOLID_NODE)) */

	    else
	    {
		*( force  ) = 0.;
		*( force+1) = 0.;
	    }

	    force += ( sizeof( struct force_struct)/8);

	} /* for( i=0; i<LX; i++) */

	j = LY-1;
	jn = j-1;
	force = lattice->force[subs][j*LX].force;
	for( i=0; i<LX; i++)
	{
	    ip = ( i<LX-1)?( i+1):( 0   );
	    in = ( i>0   )?( i-1):( LX-1);

	    *( force  ) = 0.;
	    *( force+1) = 0.;

	    if( !( lattice->bc[subs][ j*LX+i].bc_type & BC_SOLID_NODE))
	    {
		if( !( lattice->bc[subs][ j *LX+ip].bc_type & BC_SOLID_NODE)) {
		    *( force  ) += WM*vx[1]*psi[j ][ip];
		    *( force+1) += WM*vy[1]*psi[j ][ip]; }
		/**/    if( !( lattice->bc[subs][ j *LX+i ].bc_type & BC_SOLID_NODE)) {
		    /**/      *( force  ) += WM*vx[2]*psi[j ][i ];
		    /**/      *( force+1) += WM*vy[2]*psi[j ][i ]; }
		if( !( lattice->bc[subs][ j *LX+in].bc_type & BC_SOLID_NODE)) {
		    *( force  ) += WM*vx[3]*psi[j ][in];
		    *( force+1) += WM*vy[3]*psi[j ][in]; }
		if( !( lattice->bc[subs][ jn*LX+i ].bc_type & BC_SOLID_NODE)) {
		    *( force  ) += WM*vx[4]*psi[jn][i ];
		    *( force+1) += WM*vy[4]*psi[jn][i ]; }
		/**/    if( !( lattice->bc[subs][ j *LX+ip].bc_type & BC_SOLID_NODE)) {
		    /**/      *( force  ) += WD*vx[5]*psi[j ][ip];
		    /**/      *( force+1) += WD*vy[5]*psi[j ][ip]; }
		/**/    if( !( lattice->bc[subs][ j *LX+in].bc_type & BC_SOLID_NODE)) {
		    /**/      *( force  ) += WD*vx[6]*psi[j ][in];
		    /**/      *( force+1) += WD*vy[6]*psi[j ][in]; }
		if( !( lattice->bc[subs][ jn*LX+in].bc_type & BC_SOLID_NODE)) {
		    *( force  ) += WD*vx[7]*psi[jn][in];
		    *( force+1) += WD*vy[7]*psi[jn][in]; }
		if( !( lattice->bc[subs][ jn*LX+ip].bc_type & BC_SOLID_NODE)) {
		    *( force  ) += WD*vx[8]*psi[jn][ip];
		    *( force+1) += WD*vy[8]*psi[jn][ip]; }

		*( force  ) = -lattice->param.G*psi[j][i]*( *(force  ));
		*( force+1) = -lattice->param.G*psi[j][i]*( *(force+1));

	    } /* if( !( lattice->bc[subs][n].bc_type & BC_SOLID_NODE)) */

	    else
	    {
		*( force  ) = 0.;
		*( force+1) = 0.;
	    }

	    force += ( sizeof( struct force_struct)/8);

	} /* for( i=0; i<LX; i++) */

	sj = 1;
	ej = LY-2;
	force = lattice->force[subs][LX].force;

    } /* if( is_periodic_in_y(lattice,subs)) else */

    for( j=sj; j<=ej; j++)
    {
	jp = ( j<LY-1)?( j+1):( 0   );
	jn = ( j>0   )?( j-1):( LY-1);

	for( i=0; i<LX; i++)
	{
	    ip = ( i<LX-1)?( i+1):( 0   );
	    in = ( i>0   )?( i-1):( LX-1);

	    *( force  ) = 0.;
	    *( force+1) = 0.;

	    if( !( lattice->bc[subs][ j*LX+i].bc_type & BC_SOLID_NODE))
	    {
		/* 1 */ if( !( lattice->bc[subs][ j *LX+ip].bc_type & BC_SOLID_NODE)) {
		    *( force  ) += WM*vx[1]*psi[j ][ip];
		    *( force+1) += WM*vy[1]*psi[j ][ip]; }
		/* 2 */ if( !( lattice->bc[subs][ jp*LX+i ].bc_type & BC_SOLID_NODE)) {
		    *( force  ) += WM*vx[2]*psi[jp][i ];
		    *( force+1) += WM*vy[2]*psi[jp][i ]; }
		/* 3 */ if( !( lattice->bc[subs][ j *LX+in].bc_type & BC_SOLID_NODE)) {
		    *( force  ) += WM*vx[3]*psi[j ][in];
		    *( force+1) += WM*vy[3]*psi[j ][in]; }
		/* 4 */ if( !( lattice->bc[subs][ jn*LX+i ].bc_type & BC_SOLID_NODE)) {
		    *( force  ) += WM*vx[4]*psi[jn][i ];
		    *( force+1) += WM*vy[4]*psi[jn][i ]; }
		/* 5 */ if( !( lattice->bc[subs][ jp*LX+ip].bc_type & BC_SOLID_NODE)) {
		    *( force  ) += WD*vx[5]*psi[jp][ip];
		    *( force+1) += WD*vy[5]*psi[jp][ip]; }
		/* 6 */ if( !( lattice->bc[subs][ jp*LX+in].bc_type & BC_SOLID_NODE)) {
		    *( force  ) += WD*vx[6]*psi[jp][in];
		    *( force+1) += WD*vy[6]*psi[jp][in]; }
		/* 7 */ if( !( lattice->bc[subs][ jn*LX+in].bc_type & BC_SOLID_NODE)) {
		    *( force  ) += WD*vx[7]*psi[jn][in];
		    *( force+1) += WD*vy[7]*psi[jn][in]; }
		/* 8 */ if( !( lattice->bc[subs][ jn*LX+ip].bc_type & BC_SOLID_NODE)) {
		    *( force  ) += WD*vx[8]*psi[jn][ip];
		    *( force+1) += WD*vy[8]*psi[jn][ip]; }

		*( force  ) = -lattice->param.G*psi[j][i]*( *(force  ));
		*( force+1) = -lattice->param.G*psi[j][i]*( *(force+1));

	    } /* if( !( lattice->bc[subs][n].bc_type & BC_SOLID_NODE)) */

	    else
	    {
		*( force  ) = 0.;
		*( force+1) = 0.;
	    }

	    force += ( sizeof( struct force_struct)/8);

	} /* for( i=0; i<LX; i++) */
    } /* for( j=0; j<LY; j++) */

    for( j=0; j<LY; j++)
    {
	free( psi[j]);
    }
    free( psi);

} /* void compute_phase_force( lattice_ptr lattice) */
// }}}
#endif /* ZHANG_AND_CHEN_ENERGY_TRANSPORT */

// C O M P U T E _ D O U B L E _ F L U I D _ S O L I D _ F O R C E  {{{
//##############################################################################
// Eq. 20 of Martys and Chen, 1996
void compute_double_fluid_solid_force( lattice_ptr lattice)
{
    // Declare local variables.
    double sum_x,
	sum_y;
    int    x, y,
	xn, yn,
	xp, yp;

    int subs;

    int LX = lattice->param.LX;
    int LY = lattice->param.LY;

    //printf("BING: compute_double_fluid_solid_force()\n");
    //printf("SFORCE: %f %f \n",
    //            lattice->param.Gads[0],
    //            lattice->param.Gads[1] );

    for( y = 0; y < LY; y++)
    {
	yp = ( y<LY-1)?( y+1):( 0   );
	yn = ( y>0   )?( y-1):( LY-1);

	for( x = 0; x < LX; x++)
	{
	    xp = ( x<LX-1)?( x+1):( 0   );
	    xn = ( x>0   )?( x-1):( LX-1);

	    //if( !( lattice->bc[0][ y*LX + x].bc_type & BC_SOLID_NODE))
	    if( is_not_solid_node( lattice, /*subs*/0, IJ2N( x, y)))
	    {
		sum_x=0.;
		sum_y=0.;

		// neighbor 1
		//if( b[y][xp])
		//if( lattice->bc[0][ y*LX + xp].bc_type & BC_SOLID_NODE)
		if( is_solid_node( lattice, /*subs*/0, IJ2N( xp, y)))
		{
		    sum_x = sum_x + WM*vx[1] ;
		    sum_y = sum_y + WM*vy[1] ;
		    //printf("SFORCE 1: sum_x sum_y %f %f\n", sum_x, sum_y);
		}
		// neighbor 2
		//if( b[yp][x])
		//if( lattice->bc[0][ yp*LX + x].bc_type & BC_SOLID_NODE)
		if( is_solid_node( lattice, /*subs*/0, IJ2N( x, yp)))
		{
		    sum_x = sum_x + WM*vx[2] ;
		    sum_y = sum_y + WM*vy[2] ;
		    //printf("SFORCE 2: sum_x sum_y %f %f\n", sum_x, sum_y);
		}
		// neighbor 3
		//if( b[y][xn])
		//if( lattice->bc[0][ y*LX + xn].bc_type & BC_SOLID_NODE)
		if( is_solid_node( lattice, /*subs*/0, IJ2N( xn, y)))
		{
		    sum_x = sum_x + WM*vx[3] ;
		    sum_y = sum_y + WM*vy[3] ;
		    //printf("SFORCE 3: sum_x sum_y %f %f\n", sum_x, sum_y);
		}
		// neighbor 4
		//if( b[yn][x])
		//if( lattice->bc[0][ yn*LX + x].bc_type & BC_SOLID_NODE)
		if( is_solid_node( lattice, /*subs*/0, IJ2N( x, yn)))
		{
		    sum_x = sum_x + WM*vx[4] ;
		    sum_y = sum_y + WM*vy[4] ;
		    //printf("SFORCE 4: sum_x sum_y %f %f\n", sum_x, sum_y);
		}
		// neighbor 5
		//if( b[yp][xp])
		//if( lattice->bc[0][ yp*LX + xp].bc_type & BC_SOLID_NODE)
		if( is_solid_node( lattice, /*subs*/0, IJ2N( xp, yp)))
		{
		    sum_x = sum_x + WD*vx[5] ;
		    sum_y = sum_y + WD*vy[5] ;
		    //printf("SFORCE 5: sum_x sum_y %f %f\n", sum_x, sum_y);
		}
		// neighbor 6
		//if( b[yp][xn])
		//if( lattice->bc[0][ yp*LX + xn].bc_type & BC_SOLID_NODE)
		if( is_solid_node( lattice, /*subs*/0, IJ2N( xn, yp)))
		{
		    sum_x = sum_x + WD*vx[6] ;
		    sum_y = sum_y + WD*vy[6] ;
		    //printf("SFORCE 6: sum_x sum_y %f %f\n", sum_x, sum_y);
		}
		// neighbor 7
		//if( b[yn][xn])
		//if( lattice->bc[0][ yn*LX + xn].bc_type & BC_SOLID_NODE)
		if( is_solid_node( lattice, /*subs*/0, IJ2N( xn, yn)))
		{
		    sum_x = sum_x + WD*vx[7] ;
		    sum_y = sum_y + WD*vy[7] ;
		    //printf("SFORCE 7: sum_x sum_y %f %f\n", sum_x, sum_y);
		}
		// neighbor 8
		//if( b[yn][xp])
		//if( lattice->bc[0][ yn*LX + xp].bc_type & BC_SOLID_NODE)
		if( is_solid_node( lattice, /*subs*/0, IJ2N( xp, yn)))
		{
		    sum_x = sum_x + WD*vx[8] ;
		    sum_y = sum_y + WD*vy[8] ;
		    //printf("SFORCE 8: sum_x sum_y %f %f\n", sum_x, sum_y);
		}

		for( subs=0; subs<NUM_FLUID_COMPONENTS; subs++)
		{
		    if( lattice->macro_vars[subs][IJ2N(x,y)].rho != 0.)
		    {
			lattice->force[ subs][ IJ2N(x,y)].sforce[0]
			    = -lattice->param.Gads[subs]*sum_x;
			lattice->force[ subs][ IJ2N(x,y)].sforce[1]
			    = -lattice->param.Gads[subs]*sum_y;

			if( 0
			    &&( lattice->force[subs][ IJ2N(x,y)].sforce[0]!=0.
				||lattice->force[subs][ IJ2N(x,y)].sforce[1]!=0. ) )
			{
			    printf("SFORCE: %f %f\n",
				   lattice->force[subs][ IJ2N(x,y)].sforce[0],
				   lattice->force[subs][ IJ2N(x,y)].sforce[1] );
			}
		    }
		    else
		    {
			lattice->force[ subs][ IJ2N(x,y)].sforce[0] = 0.;
			lattice->force[ subs][ IJ2N(x,y)].sforce[1] = 0.;
		    }
		}

	    } /* if( !obst[y][x]) */

	    if( 0
		&&( lattice->force[ 0][ IJ2N(x,y)].sforce[0]!=0.
		    ||lattice->force[ 0][ IJ2N(x,y)].sforce[1]!=0.
		    ||lattice->force[ 1][ IJ2N(x,y)].sforce[0]!=0.
		    ||lattice->force[ 1][ IJ2N(x,y)].sforce[1]!=0. ) )
	    {
		printf("SFORCE (%d,%d): %f %f %f %f\n", x,y,
		       lattice->force[ 0][ IJ2N(x,y)].sforce[0],
		       lattice->force[ 0][ IJ2N(x,y)].sforce[1],
		       lattice->force[ 1][ IJ2N(x,y)].sforce[0],
		       lattice->force[ 1][ IJ2N(x,y)].sforce[1] );
	    }
	} /* for( x = 1; x <= LX; x++) */
    } /* for( y = 1; y <= LY; y++) */

} /* void compute_double_fluid_solid_force( lattice_ptr lattice) */
// }}}

// C O M P U T E _ S I N G L E _ F L U I D _ S O L I D _ F O R C E  {{{
//##############################################################################
// Eq. 20 of Martys and Chen, 1996
void compute_single_fluid_solid_force( lattice_ptr lattice, int subs)
{
    // Declare local variables.
    double sum_x,
	sum_y;
    int    x, y,
	xn, yn,
	xp, yp;

    int LX = lattice->param.LX;
    int LY = lattice->param.LY;

    int i, j, si, sj, ei, ej;

    int n;

    double **psi;

    double *rho;

    psi = ( double**)malloc( LY*sizeof(double*));
    for( j=0; j<LY; j++)
    {
	psi[j] = ( double*)malloc( LX*sizeof(double));
    }

    // Initialize psi.
    for( j=0; j<LY; j++)
    {
	for( i=0; i<LX; i++)
	{
	    psi[j][i] = 0.;
	}
    }

    if( lattice->periodic_y[subs])
    {
	sj = 0;
	ej = LY-1;
	rho = &( lattice->macro_vars[subs][0].rho);
    }
    else
    {
	sj = 1;
	ej = LY-2;
	rho = &( lattice->macro_vars[subs][LX].rho);
    }

    for( j=sj; j<=ej; j++)
    {
	n = j*LX;
	for( i=0; i<LX; i++, n++)
	{
	    if( *rho != 0 && !( lattice->bc[subs][n].bc_type & BC_SOLID_NODE))
	    {
		psi[j][i] = 4.*exp(-200./(*rho));
	    }
	    else
	    {
		psi[j][i] = 0.;
	    }

	    rho+=3;

	} /* for( i=0; i<LX; i++) */
    } /* for( j=0; j<LY; j++) */

    //(MOSIX) write(6,*) "Sforce", LX,LY

    //rho = &( lattice->macro_vars[subs][0].rho);
    for( y = 0; y < LY; y++)
    {
	yp = ( y<LY-1)?( y+1):( 0   );
	yn = ( y>0   )?( y-1):( LY-1);

	for( x = 0; x < LX; x++)//, rho+=3)
	{
	    xp = ( x<LX-1)?( x+1):( 0   );
	    xn = ( x>0   )?( x-1):( LX-1);

	    if( !( lattice->bc[0][ y*LX + x].bc_type & BC_SOLID_NODE))
	    {
		sum_x=0.;
		sum_y=0.;

		// neighbor 1
		//if( b[y][xp])
		if( lattice->bc[0][ y*LX + xp].bc_type & BC_SOLID_NODE)
		{
		    sum_x = WM*vx[1] ;
		    sum_y = WM*vy[1] ;
		}
		// neighbor 2
		//if( b[yp][x])
		if( lattice->bc[0][ yp*LX + x].bc_type & BC_SOLID_NODE)
		{
		    sum_x = sum_x + WM*vx[2] ;
		    sum_y = sum_y + WM*vy[2] ;
		}
		// neighbor 3
		//if( b[y][xn])
		if( lattice->bc[0][ y*LX + xn].bc_type & BC_SOLID_NODE)
		{
		    sum_x = sum_x + WM*vx[3] ;
		    sum_y = sum_y + WM*vy[3] ;
		}
		// neighbor 4
		//if( b[yn][x])
		if( lattice->bc[0][ yn*LX + x].bc_type & BC_SOLID_NODE)
		{
		    sum_x = sum_x + WM*vx[4] ;
		    sum_y = sum_y + WM*vy[4] ;
		}
		// neighbor 5
		//if( b[yp][xp])
		if( lattice->bc[0][ yp*LX + xp].bc_type & BC_SOLID_NODE)
		{
		    sum_x = sum_x + WD*vx[5] ;
		    sum_y = sum_y + WD*vy[5] ;
		}
		// neighbor 6
		//if( b[yp][xn])
		if( lattice->bc[0][ yp*LX + xn].bc_type & BC_SOLID_NODE)
		{
		    sum_x = sum_x + WD*vx[6] ;
		    sum_y = sum_y + WD*vy[6] ;
		}
		// neighbor 7
		//if( b[yn][xn])
		if( lattice->bc[0][ yn*LX + xn].bc_type & BC_SOLID_NODE)
		{
		    sum_x = sum_x + WD*vx[7] ;
		    sum_y = sum_y + WD*vy[7] ;
		}
		// neighbor 8
		//if( b[yn][xp])
		if( lattice->bc[0][ yn*LX + xp].bc_type & BC_SOLID_NODE)
		{
		    sum_x = sum_x + WD*vx[8] ;
		    sum_y = sum_y + WD*vy[8] ;
		}

		if( lattice->macro_vars[subs][y*LX+x].rho != 0)
		{
		    lattice->force[ subs][ y*LX+x].sforce[0]
			= -lattice->param.Gads[subs]*psi[y][x]*sum_x;///(*rho);
		    lattice->force[ subs][ y*LX+x].sforce[1]
			= -lattice->param.Gads[subs]*psi[y][x]*sum_y;///(*rho);
		}
		else
		{
		    lattice->force[ subs][ y*LX+x].sforce[0] = 0.;
		    lattice->force[ subs][ y*LX+x].sforce[1] = 0.;
		}

	    } /* if( !obst[y][x]) */
	} /* for( x = 1; x <= LX; x++) */
    } /* for( y = 1; y <= LY; y++) */

    for( j=0; j<LY; j++)
    {
	free( psi[j]);
    }
    free( psi);

} /* void compute_single_fluid_solid_force( lattice_ptr lattice) */
// }}}
#endif /* NON_LOCAL_FORCES */

//COMPUTE_MAX_F {{{1
void compute_max_f( lattice_ptr lattice, double *fptr, double *max_f, int subs)
{
    int n, a;
    *max_f = 0.;
    for( n=0; n<lattice->NumNodes; n++)
    {
	if( 1)//is_not_solid_node( lattice, subs, n))
	{
	    for( a=0; a<9; a++)
	    {
		if( fptr[a] > *max_f)
		{
		    *max_f = fptr[a];
		}
	    }
	}
	fptr += ( sizeof(struct pdf_struct)/8);
    }
} /* void compute_max_f( lattice_ptr lattice, double *fptr, double *max_f) */
// }}}
// COMPUTE_MAX_F0 {{{1
void compute_max_f0( lattice_ptr lattice, double *fptr, double *max_f, int subs)
{
    int n, a;
    *max_f = 0.;
    for( n=0; n<lattice->NumNodes; n++)
    {
	if( 1)//is_not_solid_node( lattice, subs, n))
	{
	    if( fptr[a] > *max_f)
	    {
		*max_f = fptr[a];
	    }
	}
	fptr += ( sizeof(struct pdf_struct)/8);
    }
} /* void compute_max_f0( lattice_ptr lattice, double *fptr, double *max_f) */
// }}}
// COMPUTE_MIN_F0 {{{1
void compute_min_f0( lattice_ptr lattice, double *fptr, double *min_f, int subs)
{
    int n, a;
    compute_max_f0( lattice, fptr, min_f, subs);
    for( n=0; n<lattice->NumNodes; n++)
    {
	if( 1)//is_not_solid_node( lattice, subs, n))
	{
	    if( fptr[a] < *min_f)
	    {
		*min_f = fptr[a];
	    }
	}
	fptr += ( sizeof(struct pdf_struct)/8);
    }
} /* void compute_min_f0( lattice_ptr lattice, double *fptr, double *min_f) */
// }}}
// COMPUTE_MAX_F1234 {{{1
void compute_max_f1234( lattice_ptr lattice, double *fptr, double *max_f, int subs)
{
    int n, a;
    *max_f = 0.;
    for( n=0; n<lattice->NumNodes; n++)
    {
	if( 1)//is_not_solid_node( lattice, subs, n))
	{
	    for( a=1; a<=4; a++)
	    {
		if( fptr[a] > *max_f)
		{
		    *max_f = fptr[a];
		}
	    }
	}
	fptr += ( sizeof(struct pdf_struct)/8);
    }
} /* void compute_max_f1234( lattice_ptr lattice, double *fptr, ...) */
// }}}
// COMPUTE_MAX_F5678 {{{1
void compute_max_f5678( lattice_ptr lattice, double *fptr, double *max_f, int subs)
{
    int n, a;
    *max_f = 0.;
    for( n=0; n<lattice->NumNodes; n++)
    {
	if( 1)//is_not_solid_node( lattice, subs, n))
	{
	    for( a=5; a<=8; a++)
	    {
		if( fptr[a] > *max_f)
		{
		    *max_f = fptr[a];
		}
	    }
	}
	fptr += ( sizeof(struct pdf_struct)/8);
    }
} /* void compute_max_f5678( lattice_ptr lattice, double *fptr, ...) */
// }}}
// COMPUTE_MAX_RHO {{{1
void compute_max_rho( lattice_ptr lattice, double *max_rho, int subs)
{
    int n;
    *max_rho = 0.;
    for( n=0; n<lattice->NumNodes; n++)
    {
	if( !( lattice->bc[subs][n].bc_type & BC_SOLID_NODE))
	{
	    if(     ( lattice->macro_vars[subs][n].rho) > *max_rho)
	    {
		*max_rho =     ( lattice->macro_vars[subs][n].rho);
	    }
	}
    }

    process_reduce_double_max( lattice, max_rho);

} /* void compute_max_rho( lattice_ptr lattice, double *max_rho, int subs) */
// }}}
// COMPUTE_MIN_RHO {{{1
void compute_min_rho( lattice_ptr lattice, double *min_rho, int subs)
{
    int n;
    compute_max_rho( lattice, min_rho, subs);

    for( n=0; n<lattice->NumNodes; n++)
    {
	if( !( lattice->bc[subs][n].bc_type & BC_SOLID_NODE))
	{
	    if(     ( lattice->macro_vars[subs][n].rho) < *min_rho)
	    {
		*min_rho =     ( lattice->macro_vars[subs][n].rho);
	    }
	}
    }

    process_reduce_double_min( lattice, min_rho);

} /* void compute_min_rho( lattice_ptr lattice, double *min_rho, int subs) */
// }}}
// COMPUTE_AVE_RHO {{{1
void compute_ave_rho( lattice_ptr lattice, double *ave_rho, int subs)
{
    int n, nn;
    *ave_rho = 0.;
    nn = 0;
    for( n=0; n<lattice->NumNodes; n++)
    {
	if( !( lattice->bc[subs][n].bc_type & BC_SOLID_NODE))
	{
	    *ave_rho +=     ( lattice->macro_vars[subs][n].rho);
	    nn++;
	}
    }

    process_reduce_double_sum( lattice, ave_rho);
    process_reduce_int_sum( lattice, &nn);

    if( nn != 0) { *ave_rho = (*ave_rho) / nn;}


} /* void compute_ave_rho( lattice_ptr lattice, double *ave_rho, int subs) */
// }}}
// COMPUTE_MAX_U {{{1
void compute_max_u( lattice_ptr lattice, double *max_u, int subs)
{
    int n;
    *max_u = 0.;
    *(max_u+1) = 0.;
    for( n=0; n<lattice->NumNodes; n++)
    {
	if( !( lattice->bc[subs][n].bc_type & BC_SOLID_NODE))
	{
	    if( fabs( lattice->macro_vars[subs][n].u[0]) > *(max_u))
	    {
		*max_u = fabs( lattice->macro_vars[subs][n].u[0]);
	    }
	    if( fabs( lattice->macro_vars[subs][n].u[1]) > *(max_u+1))
	    {
		*(max_u+1) = fabs( lattice->macro_vars[subs][n].u[1]);
	    }
	}
    }

    process_reduce_double_max( lattice, max_u+0);
    process_reduce_double_max( lattice, max_u+1);

} /* void compute_max_u( lattice_ptr lattice, double *max_u, int subs) */
// }}}
// COMPUTE_MAX_U_ALL {{{1
void compute_max_u_all( lattice_ptr lattice, double *max_u, int subs)
{
    int n;
    double rho, u, u_x, u_y;
    *(max_u+0) = 0.;
    *(max_u+1) = 0.;
    *(max_u+2) = 0.;
    *(max_u+3) = 0.;
    *(max_u+4) = 0.;
    for( n=0; n<lattice->NumNodes; n++)
    {
	if( !( lattice->bc[subs][n].bc_type & BC_SOLID_NODE))
	{
	    rho = lattice->macro_vars[subs][n].rho;
	    u_x = lattice->macro_vars[subs][n].u[0];
	    u_y = lattice->macro_vars[subs][n].u[1];

	    u = sqrt(u_x*u_x+u_y*u_y);
	    if( u > *(max_u+0)) { *(max_u+0) = u; }

	    if( fabs( u_x) > *(max_u+1)) { *(max_u+1) = fabs( u_x); }
	    if( fabs( u_y) > *(max_u+2)) { *(max_u+2) = fabs( u_y); }

	    if(     ( u_x) > *(max_u+3)) { *(max_u+3) =     ( u_x); }
	    if(     ( u_y) > *(max_u+4)) { *(max_u+4) =     ( u_y); }
	}
    }

} /* void compute_max_u_all( lattice_ptr lattice, double *max_u, int subs) */
// }}}
// COMPUTE_MIN_U {{{1
void compute_min_u( lattice_ptr lattice, double *min_u, int subs)
{
    int n;
    compute_max_u( lattice, min_u,   subs);
    //compute_max_u( lattice, min_u+1, subs);
    for( n=0; n<lattice->NumNodes; n++)
    {
	if( !( lattice->bc[subs][n].bc_type & BC_SOLID_NODE))
	{
	    if( fabs( lattice->macro_vars[subs][n].u[0]) < *(min_u))
	    {
		*min_u = fabs( lattice->macro_vars[subs][n].u[0]);
	    }
	    if( fabs( lattice->macro_vars[subs][n].u[1]) < *(min_u+1))
	    {
		*(min_u+1) = fabs( lattice->macro_vars[subs][n].u[1]);
	    }
	}
    }

    process_reduce_double_min( lattice, min_u+0);
    process_reduce_double_min( lattice, min_u+1);

} /* void compute_min_u( lattice_ptr lattice, double *min_u) */
// }}}
// COMPUTE_MIN_U_ALL {{{1
void compute_min_u_all( lattice_ptr lattice, double *min_u, int subs)
{
    int n;
    double rho, u, u_x, u_y;

    compute_max_u_all( lattice, min_u,   subs);

    for( n=0; n<lattice->NumNodes; n++)
    {
	if( !( lattice->bc[subs][n].bc_type & BC_SOLID_NODE))
	{
	    rho = lattice->macro_vars[subs][n].rho;
	    u_x = lattice->macro_vars[subs][n].u[0];
	    u_y = lattice->macro_vars[subs][n].u[1];

	    u = sqrt(u_x*u_x+u_y*u_y);
	    if( u < *(min_u+0)) { *(min_u+0) = u; }

	    if( fabs( u_x) < *(min_u+1)) { *(min_u+1) = fabs( u_x); }
	    if( fabs( u_y) < *(min_u+2)) { *(min_u+2) = fabs( u_y); }

	    if(     ( u_x) < *(min_u+3)) { *(min_u+3) =     ( u_x); }
	    if(     ( u_y) < *(min_u+4)) { *(min_u+4) =     ( u_y); }

	}
    }
} /* void compute_min_u_all( lattice_ptr lattice, double *min_u) */
// }}}
// COMPUTE_AVE_U {{{1
void compute_ave_u( lattice_ptr lattice, double *ave_u, int subs)
{
    int n, nn;
    *(ave_u+0) = 0.;
    *(ave_u+1) = 0.;
    nn = 0;
    for( n=0; n<lattice->NumNodes; n++)
    {
	if( !( lattice->bc[subs][n].bc_type & BC_SOLID_NODE))
	{
	    *(ave_u+0) += lattice->macro_vars[subs][n].u[0];
	    *(ave_u+1) += lattice->macro_vars[subs][n].u[1];
	    nn++;
	}
    }

    process_reduce_double_sum( lattice, ave_u+0);
    process_reduce_double_sum( lattice, ave_u+1);
    process_reduce_int_sum( lattice, &nn);

    if( nn != 0)
    {
	*(ave_u+0) = (*(ave_u+0))/nn;
	*(ave_u+1) = (*(ave_u+1))/nn;
    }

} /* void compute_ave_u( lattice_ptr lattice, double *ave_u, int subs) */
// }}}
// COMPUTE_AVE_U_ALL {{{1
void compute_ave_u_all( lattice_ptr lattice, double *ave_u, int subs)
{
    int n, nn;
    double rho, u_x, u_y;
    *(ave_u+0) = 0.;
    *(ave_u+1) = 0.;
    *(ave_u+2) = 0.;
    *(ave_u+3) = 0.;
    *(ave_u+4) = 0.;

    nn = 0;
    for( n=0; n<lattice->NumNodes; n++)
    {
	if( !( lattice->bc[subs][n].bc_type & BC_SOLID_NODE))
	{
	    rho = lattice->macro_vars[subs][n].rho;
	    u_x = lattice->macro_vars[subs][n].u[0];
	    u_y = lattice->macro_vars[subs][n].u[1];

	    *(ave_u+0) += sqrt(u_x*u_x+u_y*u_y);

	    *(ave_u+1) += fabs(u_x);
	    *(ave_u+2) += fabs(u_y);

	    *(ave_u+3) += u_x;
	    *(ave_u+4) += u_y;

	    nn++;
	}
    }
    if( nn != 0)
    {
	*(ave_u+0) = (*(ave_u+0))/nn;
	*(ave_u+1) = (*(ave_u+1))/nn;
	*(ave_u+2) = (*(ave_u+2))/nn;
	*(ave_u+3) = (*(ave_u+3))/nn;
	*(ave_u+4) = (*(ave_u+4))/nn;
    }

} /* void compute_ave_u( lattice_ptr lattice, double *ave_u, int subs) */
// }}}
// COMPUTE_FLUX {{{1
void compute_flux( lattice_ptr lattice, double *flux, int subs)
{
    int n, nn;
    double rho, u_x, u_y;
    *(flux+0) = 0.;
    *(flux+1) = 0.;
    *(flux+2) = 0.;
    nn = 0;
    for( n=0; n<lattice->NumNodes; n++)
    {
	rho = lattice->macro_vars[subs][n].rho;
	u_x = lattice->macro_vars[subs][n].u[0];
	u_y = lattice->macro_vars[subs][n].u[1];

	if( !( lattice->bc[subs][n].bc_type & BC_SOLID_NODE))
	{
	    *(flux+0) += rho*sqrt(u_x*u_x+u_y*u_y);
	    *(flux+1) += rho*u_x;
	    *(flux+2) += rho*u_y;
	    nn++;
	}
    }
    if( nn != 0)
    {
	*(flux+0) = (*(flux+0))/nn;
	*(flux+1) = (*(flux+1))/nn;
	*(flux+2) = (*(flux+2))/nn;
    }

} /* void compute_flux( lattice_ptr lattice, double *flux, int subs) */
// }}}
#if NON_LOCAL_FORCES
// COMPUTE_MAX_SFORCE {{{1
void compute_max_force( lattice_ptr lattice, double *max_force, int subs)
{
    int n;
    *max_force = 0.;
    *(max_force+1) = 0.;
    for( n=0; n<lattice->NumNodes; n++)
    {
	if( !( lattice->bc[subs][n].bc_type & BC_SOLID_NODE))
	{
	    if( fabs( lattice->force[subs][ n].force[0]) > *(max_force))
	    {
		*max_force = fabs( lattice->macro_vars[subs][n].u[0]);
	    }
	    if( fabs( lattice->force[subs][ n].force[1]) > *(max_force+1))
	    {
		*(max_force+1) = fabs( lattice->macro_vars[subs][n].u[1]);
	    }
	}
    }
} /* void compute_max_u( lattice_ptr lattice, double *max_u, int subs) */
// }}}
// COMPUTE_MAX_SFORCE {{{1
void compute_max_sforce( lattice_ptr lattice, double *max_sforce, int subs)
{
    int n;
    *max_sforce = 0.;
    *(max_sforce+1) = 0.;
    for( n=0; n<lattice->NumNodes; n++)
    {
	if( !( lattice->bc[subs][n].bc_type & BC_SOLID_NODE))
	{
	    if( fabs( lattice->force[subs][ n].sforce[0]) > *(max_sforce))
	    {
		*max_sforce = fabs( lattice->macro_vars[subs][n].u[0]);
	    }
	    if( fabs( lattice->force[subs][ n].sforce[1]) > *(max_sforce+1))
	    {
		*(max_sforce+1) = fabs( lattice->macro_vars[subs][n].u[1]);
	    }
	}
    }
} /* void compute_max_u( lattice_ptr lattice, double *max_u, int subs) */
// }}}
#endif

#if STORE_U_COMPOSITE
// COMPUTE_MAX_UPR {{{1
void compute_max_upr( lattice_ptr lattice, double *max_u)
{
    int n;
    *max_u = 0.;
    *(max_u+1) = 0.;
    for( n=0; n<lattice->NumNodes; n++)
    {
	if( !( lattice->bc[0][n].bc_type & BC_SOLID_NODE))
	{
	    if( fabs( lattice->upr[n].u[0]) > *(max_u))
	    {
		*max_u = fabs( lattice->upr[n].u[0]);
	    }
	    if( fabs( lattice->upr[n].u[1]) > *(max_u+1))
	    {
		*(max_u+1) = fabs( lattice->upr[n].u[1]);
	    }
	}
    }
} /* void compute_max_upr( lattice_ptr lattice, double *max_u) */
// }}}
// COMPUTE_MIN_UPR {{{1
void compute_min_upr( lattice_ptr lattice, double *min_u)
{
    int n;
    compute_max_upr( lattice, min_u);
    for( n=0; n<lattice->NumNodes; n++)
    {
	if( !( lattice->bc[0][n].bc_type & BC_SOLID_NODE))
	{
	    if( fabs( lattice->upr[n].u[0]) < *(min_u))
	    {
		*min_u = fabs( lattice->upr[n].u[0]);
	    }
	    if( fabs( lattice->upr[n].u[1]) < *(min_u+1))
	    {
		*(min_u+1) = fabs( lattice->upr[n].u[1]);
	    }
	}
    }
} /* void compute_min_upr( lattice_ptr lattice, double *min_u) */
// }}}
// COMPUTE_AVE_UPR {{{1
void compute_ave_upr( lattice_ptr lattice, double *ave_u)
{
    int n, nn;
    *ave_u = 0.;
    *(ave_u+1) = 0.;
    nn = 0;
    for( n=0; n<lattice->NumNodes; n++)
    {
	if( !( lattice->bc[0][n].bc_type & BC_SOLID_NODE))
	{
	    *ave_u += fabs( lattice->upr[n].u[0]);
	    *(ave_u+1) += fabs( lattice->upr[n].u[1]);
	    nn++;
	}
    }
    if( nn != 0)
    {
	*ave_u     = (*ave_u)/nn;
	*(ave_u+1) = (*(ave_u+1))/nn;
    }

} /* void compute_ave_upr( lattice_ptr lattice, double *ave_u) */
// }}}
#endif /* STORE_U_COMPOSITE */

// COMPUTE_VORTICITY {{{1
void compute_vorticity(
		       lattice_ptr lattice, int i, int j, int n, double *vor, int subs)
{
    double duyx, duxy;
    int    nn[5]; // Indices of neighbors;

    int    LX=lattice->param.LX,
	LY=lattice->param.LY;

    int    ip, in,
	jp, jn;

    ip = ( i<LX-1)?(i+1):(0   );
    in = ( i>0   )?(i-1):(LX-1);
    jp = ( j<LY-1)?(j+1):(0   );
    jn = ( j>0   )?(j-1):(LY-1);

    nn[1] = j *LX + ip;
    nn[2] = jp*LX + i ;
    nn[3] = j *LX + in;
    nn[4] = jn*LX + i ;

    // NOTE: Assuming dx=1 .  TODO: Generalize dx?

    // Derivative of uy wrt x.
    if(    lattice->bc[subs][nn[1]].bc_type == BC_FLUID_NODE
	   && lattice->bc[subs][nn[3]].bc_type == BC_FLUID_NODE)
    {
	// Forward difference.
	duyx = lattice->macro_vars[subs][nn[1]].u[1]
	    - lattice->macro_vars[subs][n].u[1];
    }
    //else if(    lattice->bc[subs][nn[3]].bc_type == BC_FLUID_NODE)
    //{
    //  // Backward difference.
    //  duyx = lattice->macro_vars[subs][n].u[1]
    //      - lattice->macro_vars[subs][nn[3]].u[1];
    //}
    else
    {
	duyx = 0.;
    }

    //printf("compute_vorticity() -- "
    //    "n=%d, (i,j)=(%d,%d), duyx=%f, "
    //    "nn1 = %d, nn3 = %d,"
    //    "bc1 = %d, bc3 = %d"
    //    "\n",
    //    n, i, j, duyx,
    //    nn[1], nn[3],
    //    lattice->bc[subs][nn[1]].bc_type,
    //    lattice->bc[subs][nn[3]].bc_type);

    // Derivative of ux wrt y.
    if(    lattice->bc[subs][nn[2]].bc_type == BC_FLUID_NODE
	   && lattice->bc[subs][nn[4]].bc_type == BC_FLUID_NODE)
    {
	// Forward difference.
	duxy = lattice->macro_vars[subs][nn[2]].u[0]
	    - lattice->macro_vars[subs][n].u[0];
    }
    //else if(    lattice->bc[subs][nn[4]].bc_type == BC_FLUID_NODE)
    //{
    //  // Backward difference.
    //  duxy = lattice->macro_vars[subs][n].u[0]
    //      - lattice->macro_vars[subs][nn[4]].u[0];
    //}
    else
    {
	duxy = 0.;
    }

    if( duxy*duyx != 0.)
    {
	*vor = duyx - duxy;
    }
    else
    {
	*vor = 0.;
    }

} /* void compute_vorticity( lattice_ptr lattice, int i, int j, int n, ... */
// }}}
// COMPUTE_MAX_VOR {{{1
void compute_max_vor(
		     lattice_ptr lattice, double *max_vor_p, double *max_vor_n, int subs)
{
    int n;
    double vor;
    int nnz;

    *max_vor_p = 0.;
    *max_vor_n = 0.;
    nnz = 0;

    for( n=0; n<=lattice->NumNodes; n++)
    {
	if(    lattice->bc[subs][n].bc_type == BC_FLUID_NODE)
	{
	    compute_vorticity( lattice,
			       n%lattice->param.LX,
			       n/lattice->param.LX,
			       n,
			       &vor,
			       subs  );
	    if( vor != 0.) { nnz++;}

	    if( vor > *max_vor_p)
	    {
		*max_vor_p = vor;

	    } /* if( vor > *max_vor_p) */

	    else if( vor < *max_vor_n)
	    {
		*max_vor_n = vor;

	    } /* if( vor > *max_vor_p) */

	} /* if( lattice->bc[subs][n].bc_type == 0) */

    } /* for( n=0; n<=lattice->NumNodes; n++) */

#if 0 && VERBOSITY_LEVEL > 0
    printf("compute_max_vor() -- nnz = %d.  nnz/NumNodes = %f\n",
	   nnz, (double)nnz/(double)lattice->NumNodes);
#endif /* 0 && VERBOSITY_LEVEL > 0 */

} /* void compute_max_vor( lattice_ptr lattice, double *max_vor_p, ... */
// }}}
// COMPUTE_AVE_VOR {{{1
void compute_ave_vor(
		     lattice_ptr lattice, double *ave_vor_p, double *ave_vor_n, int subs)
{
    int n;
    double vor;
    int nnz;
    int num_p;
    int num_n;

    *ave_vor_p = 0.;
    *ave_vor_n = 0.;
    nnz = 0;
    num_p = 0;
    num_n = 0;

    for( n=0; n<=lattice->NumNodes; n++)
    {
	if(    lattice->bc[subs][n].bc_type == BC_FLUID_NODE)
	{
	    compute_vorticity( lattice,
			       n%lattice->param.LX,
			       n/lattice->param.LX,
			       n,
			       &vor,
			       subs  );
	    if( vor != 0.) { nnz++;}

	    if( vor > *ave_vor_p)
	    {
		*ave_vor_p += vor;
		num_p++;

	    } /* if( vor > *ave_vor_p) */

	    else if( vor < *ave_vor_n)
	    {
		*ave_vor_n += vor;
		num_n++;

	    } /* if( vor > *ave_vor_p) */

	} /* if( lattice->bc[subs][n].bc_type == 0) */

    } /* for( n=0; n<=lattice->NumNodes; n++) */

    if( num_p > 0) { *ave_vor_p /= num_p;}
    if( num_n > 0) { *ave_vor_n /= num_n;}

#if 0 && VERBOSITY_LEVEL > 0
    printf("compute_ave_vor() -- nnz = %d.  nnz/NumNodes = %f\n",
	   nnz, (double)nnz/(double)lattice->NumNodes);
#endif /* VERBOSITY_LEVEL > 0 */

} /* void compute_ave_vor( lattice_ptr lattice, double *ave_vor_p, ... */
// }}}

//##############################################################################
// vim: foldmethod=syntax:foldlevel=0
