//##############################################################################
//
// Copyright (C), 2005, Michael Sukop and Danny Thorne
//
// lattice.h
//
//  - Lattice structure and supporting structures for lb2d_prime.
//

// struct pdf_struct
//
//  - Structure to hold the particle distribution functions.
//

#ifndef LATTICE_H
#define LATTICE_H

struct pdf_struct
{
  double feq[   /*NUM_DIRS*/ 9];
  double f[     /*NUM_DIRS*/ 9];
  double ftemp[ /*NUM_DIRS*/ 9];

};
typedef struct pdf_struct *pdf_ptr;

// struct macro_vars_struct
//
//  - Structure to hold the macroscopic variables.
//
struct macro_vars_struct
{
  double rho;
  double u[   /*NUM_DIMS*/ 2];

};

#if PEST_OUTPUT_ON
// struct conc_data_struct
//
//  - Structure to hold concentration data from an experiment.
//
struct conc_data_struct
{
  int countervar;
  int timestep;
  int x_coord;
  int y_coord;
  double norm_conc;
};
#endif

#if STORE_U_COMPOSITE
// struct upr_struct
//
//  - Structure to hold the composite velocity used to compute feq.
//
struct upr_struct
{
  double u[   /*NUM_DIMS*/ 2];

};
#endif /* STORE_U_COMPOSITE */

// struct bc_struct
//
//  - Structure to hold boundary condition information.
//
//  - Currently it is just an integer flag. I'm putting it inside
//    a struct in case we want a more complicated boundary
//    conditions mechanism later.
//
struct bc_struct
{
  int bc_type;

};
typedef struct bc_struct *bc_ptr;

// struct bcs_in_struct
//
//  - Structure to hold input values for boundary conditions.
//
//  - These are activated by setting the corresponding flag to 2 in the
//    params.in file. There must be a file with the corresponding name and
//    suffix 'dat' in the 'in' folder, e.g., pressure_n_in0.in. The values
//    in the input file will be used for the boundary conditions, successive
//    values on successive time steps. If there are more timesteps than input
//    values, the input values will be cycled. This is particularly useful for
//    imposing temporally periodic boundary conditions, e.g., to simulate tidal
//    periods.
//
struct bcs_in_struct
{
  double*  pressure_n_in0;
  int      num_pressure_n_in0;

  double*  pressure_s_in0;
  int      num_pressure_s_in0;
#if 0
  double*  pressure_s_in0;
  double* pressure_n_out0;
  double* pressure_s_out0;
  double*  velocity_n_in0;
  double*  velocity_s_in0;
  double* velocity_n_out0;
  double* velocity_s_out0;
  double*  pressure_e_in0;
  double*  pressure_w_in0;
  double* pressure_e_out0;
  double* pressure_w_out0;
  double*  velocity_e_in0;
  double*  velocity_w_in0;
  double* velocity_e_out0;
  double* velocity_w_out0;
  double*  pressure_n_in1;
  double*  pressure_s_in1;
  double* pressure_n_out1;
  double* pressure_s_out1;
  double*  velocity_n_in1;
  double*  velocity_s_in1;
  double* velocity_n_out1;
  double* velocity_s_out1;
  double*  pressure_e_in1;
  double*  pressure_w_in1;
  double* pressure_e_out1;
  double* pressure_w_out1;
  double*  velocity_e_in1;
  double*  velocity_w_in1;
  double* velocity_e_out1;
  double* velocity_w_out1;
  double*  constcon_n_in;
  double*  constcon_s_in;
  double* constcon_n_out;
  double* constcon_s_out;
  double*  constflx_n_in;
  double*  constflx_s_in;
  double* constflx_n_out;
  double* constflx_s_out;
  double*  constcon_e_in;
  double*  constcon_w_in;
  double* constcon_e_out;
  double* constcon_w_out;
  double*  constflx_e_in;
  double*  constflx_w_in;
  double* constflx_e_out;
  double* constflx_w_out;
#endif
};

typedef struct bcs_in_struct* bcs_in_ptr;


#if DO_NOT_STORE_SOLIDS
// struct node_struct
//
//  - Structure to hold node information.
//
struct node_struct
{
  int i, j; // Lattice coordinates of node.

  int n; // Node index.

  int nn[8]; // Indices of neighboring nodes.

};
typedef struct node_struct *node_ptr;
#endif /* DO_NOT_STORE_SOLIDS */

#if NON_LOCAL_FORCES
struct force_struct
{
  double force[  /*NUM_DIMS*/2];
  double sforce[ /*NUM_DIMS*/2];
};
#endif /* NON_LOCAL_FORCES */
#if POROUS_MEDIA || FREED_POROUS_MEDIA
struct ns_struct
{
  double ns;
};
#endif /* POROUS_MEDIA */

struct process_struct
{
  int id;
  int num_procs;
#if PARALLEL
  int g_LX;
  int g_LY;
//3D   int g_LZ;
  int g_NumNodes;

  int g_SX, g_EX;
  int g_SY, g_EY;
//3D   int g_SZ, g_EZ;
  int g_StartNode;

  double *y_pos_pdf_to_send;
  double *y_pos_pdf_to_recv;
  double *y_neg_pdf_to_send;
  double *y_neg_pdf_to_recv;

  MPI_Request send_req_0;
  MPI_Request recv_req_0;
  MPI_Request send_req_1;
  MPI_Request recv_req_1;
  MPI_Status mpi_status;
  int mpierr;

#endif
};
typedef struct process_struct *process_ptr;


// struct param_struct
//
// Structure to hold parameters that specify the configuration of the problem
// to solve.
//
// Values for these parameters are read from file "./in/params.in". (Note that
// there are also some compile-time configuration options in file
// "./in/flags.h".)
//
// Each parameter is preceeded by documentation consisting of three kinds of
// fields: "Param", "Type", and "Comments". There may be multiple "Param"
// fields for sets of related parameters, e.g. LX and LY. There is only one
// occurance of the "Type" field and "Comments" field for each (set of)
// parameter(s). The "Param" field(s) and the "Type" field are each one line.
// The "Comments" field can be multiple lines.
//
// Distinct fluids are referred to both as components and as substances.
// Normally in conversation we talk about fluid components, but variables are
// indexed by "subs". The term "component" can be used to refer to a fluid
// component or a solute component (c.f. Inamuro) as well as the more broadly
// conventional mathematical usage of a vector component. The reader should be
// wary of context, therefore.
//
// In the "Param" field(s) of the documentation preceeding each parameter, the
// parameters that are arrays are stated with typical and/or suggestive index
// names between square brackets. For example, tau[subs] is written to suggest
// the fact that tau contains an array of values, one for each substance or
// fluid component, where "subs" is a typical index name for indexing fluid
// components. The commentary section ("Comments" field) for each parameter is
// explicit about the meaning of the indices.
//
// A common mistake made when configuring a problem is to specify in the file
// "params.in" an integer number for a type double parameter or vice versa.
// This can cause mystifying symptoms. Be careful to include a decimal point in
// all type double parameter values in "params.in" regardless of whether there
// is a fractional part to the value. Likewise, do not include decimal points
// in type integer parameter values. Remember! If things have been going
// smoothly and then suddenly go bad in weird ways (sorry I can't recollect any
// specific examples) be sure to check this issue in "params.in". Note that
// file "params.dat" is output by the code mostly as a sanity check: if values
// in "params.dat" do not match values in "params.in", the double versus
// integer parameter type issue may be the reason.
//
// Here are some useful Unix/Linux command lines for processing this
// documentation (the "params.txt" file):
//
//  >> grep Param params.txt
//
//  >> grep Comment params.txt
//
//  >> TODO
//
//  >> TODO
//
// Here is the command line that generates the "params.txt" file from the
// "lattice.h" file:
//
//  >> cat ./src/lattice.h | \
//     sed -n '/^\/\/ struct param_struct/,/\/\* struct param_struct/p' | \
//     grep "\/\/" | \
//     sed 's/[ ]*\/\/ //; s/[ ]*\/\/$//' > ./doc/params.txt
//
// If you are developing under Windows, I recommend Cygwin
//
//   http://www.cygwin.com
//
// or something comparable. The Unix/Linux command line is indispensable. See
//
//   http://www.cs.usfca.edu/~parrt/course/601/lectures/unix.util.html
//   http://www.student.northpark.edu/pemente/sed/sed1line.txt
//
// for example.
//
// -----------------------------------------------------------------------------
//
struct param_struct
{
  // Param: LX
  // Param: LY
  // Type: int
  // Comments: LX is the number of lattice nodes in x-direction. LY is the
  // number of lattice nodes in y-direction. These are be used to construct
  // the file name of the BMP file "<LX>x<LY>.bmp" containing the domain. For
  // example, if LX=160 and LY=120, then the file name is "160x120.bmp".
  //
  int    LX,
         LY;

  // Param: length_scale
  // Type: int
  // Comments: length_scale is a characteristic length. This is used for
  // computing Re.
  //
  int    length_scale;

  // Param: NumFrames
  // Type: int
  // Comments: NumFrames is the number of frames to output.
  //
  int    NumFrames;

  // Param: FrameRate
  // Type: int
  // Comments: FrameRate is the number of timesteps per frame.
  //
  int    FrameRate;

  // Param: NumTimeSteps
  // Type: int
  // Comments: NumTimeSteps is the number of time steps computed as
  // NumFrames*FrameRate.
  //
  int    NumTimeSteps;

  // Param: tau[subs]
  // Type: double*
  // Comments: tau is the relaxation time(s). If NUM_FLUID_COMPONENTS==1, then
  // tau[0] is the relaxation time. If NUM_FLUID_COMPONENTS==2, then tau[0] is
  // the relaxation time for component 0 and tau[1] is the relaxation time for
  // component 1.
  //
  double tau[ NUM_FLUID_COMPONENTS];

  // Param: gval[subs][dir]
  // Type: double**
  // Comments: gval[subs][dir] is the gravitational (body) force(s). If
  // NUM_FLUID_COMPONENTS==1, then gval[0][dir] holds the gravitational force.
  // If NUM_FLUID_COMPONENTS==2, then gval[0][dir] holds the gravitational
  // force for component 0 and gval[1][dir] holds the gravitational force for
  // component 1. In either case, gval[subs][0] is the gravitational force
  // in the x-direction and gval[subs][1] is the gravitational force in the
  // y-direction.
  //
  double gval[   NUM_FLUID_COMPONENTS][2];

  // Param: end_grav[subs]
  // Type: int
  // Comments: end_grav[subs] is the timestep at which to end the gravitational
  // force. If NUM_FLUID_COMPONENTS==1, then end_grav[0] holds the ending
  // timestep. If NUM_FLUID_COMPONENTS==2, then end_grav[0] holds the ending
  // timestep for fluid compenent 0 and end_grav[1] holds the ending timestep
  // for fluid component 1. This mechanism is only enforced when the
  // MANAGE_BODY_FORCE flag is turned on in "flags.h". It is implemented as a
  // call to the "manage_body_force()" function at each time step, so it is a
  // special feature that should generally be turned off. In the future this
  // mechanism should mature.
  //
  int    end_grav[ NUM_FLUID_COMPONENTS];

  // Param: buoyancy
  // Type: int
  // Comments: The "buoyancy" flag toggles buoyancy effects due to the density
  // of solute. This flag is only relevant when INAMURO_SIGMA_COMPONENT is
  // turned on in "flags.h". More recently, the buoyancy flag can be +1 or -1
  // to switch between solute induced buoyancy effects and temperature induced
  // buoyancy effects. Use the accessor functions get_buoyancy_flag(lattice),
  // get_buoyancy_sign(lattice) and get_buoyancy(lattice).
  //
  // NOTE: Deprecated. Should just use beta for this mechanism now, right?
  //
  int    buoyancy;

  // Param: buoy_subs
  // Type int
  // Comment: Which substances gravity value to use in the buoyance term.
  // Originally we used subs 0, the fluid, gravity. Then we switched to using
  // the solutes gravity (subs 1) to avoid fluid compressibility effects.
  int    buoy_subs;

  // Param: incompressible
  // Type: int
  // Comments: The "incompressible" flag toggles an incompressible lattice
  // Boltzmann method (c.f. TODO: cite). The usual method that we experiment
  // with is the common "weakly" compressible BGK method. For some things
  // (e.g., TODO) an incompressible model is desired.
  //
  int    incompressible;

  // Param: simple_diffusion
  // Type: int Comments: The simple_diffusion flag toggles a simplified
  // diffusion computation. It simplifies the computation of feq for the solute
  // component. It is based on the observation that the process of diffusion
  // requires less symmetry than flow (c.f. TODO: cite).
  //
  int    simple_diffusion;

  // Param: rho_A[subs]
  // Param: rho_B[subs]
  // Type: double*
  // Comments: rho_A[subs] and rho_B[subs] are the initial density values. The
  // usage of these values varies with respect to the chosen initial condition
  // (see the initial_condition parameter below). If NUM_FLUID_COMPONENTS==1,
  // then rho_A[0] and/or rho_B[0] are used to initialize the fluid density.
  // If NUM_FLUID_COMPONENTS==2, then rho_A[0] and/or rho_B[0] are used to
  // initialize density of fluid component 0 and rho_A[1] and/or rho_B[1] are
  // used to initialize density of fluid component 1. All the values are not
  // necessarily required for all the initial conditions. See the documentation
  // related to the initial_condition parameter for specifics.
  //
  double rho_A[    NUM_FLUID_COMPONENTS];
  double rho_B[    NUM_FLUID_COMPONENTS];

#if SOURCE_ON
  double source_strength;
#endif /*SOURCE_ON*/

#if SINK_ON
  double sink_strength;
#endif /*SINK_ON*/

#if INAMURO_SIGMA_COMPONENT
  // Initial/boundary condition values for solute component.
  double rho_sigma;
  double C0;
  double rho0;
  double drhodC;
  double rho_sigma_in;
  double C_in;
  double rho_sigma_out;
  double C_out;
  double beta;
  double u_sigma;
  double u_sigma_in;
  double u_sigma_out;

  //int sigma_bulk_on
  //timestep at which solute in the bulk will begin evolution.
#if SIGMA_BULK_FLAG
  int    sigma_bulk_on;
#endif /*SIGMA_BULK_FLAG*/

  // Param: sigma_start;
  // Type: int    sigma_start;
  // Comments: Timestep at which solute is activated. If sigma_start == 0, solute will be activated from the beginning. The reason for this option is to make it easy to allow the fluid to equilibrate before introducing solute into the flow.
  //
  int    sigma_start;

  // Param: sigma_stop;
  // Type: int    sigma_stop;
  // Comments: Timestep at which solute is deactivated. If sigma_stop < 0 then the solute will never stop. WARNING: if 0 < sigma_stop < sigma_start, solute will never start. This option is useful, for instance, if it is desired to observe how a solute washes out of the domain.
  //
  int    sigma_stop;

  // Param: sigma_btc_rate;
  // Type: int    sigma_btc_rate;
  // Comments: Rate at which to accumulate temporal breakthrough curve. If sigma_btc_rate <= 0, no btc is stored.
  //
  int    sigma_btc_rate;

  // Param: sigma_btc_spot;
  // Type: int    sigma_btc_spot;
  // Comments: The spot in the domain (as distance from inflow) for measuring btc. If sigma_btc_spot is greater than the length of the domain or <0 then the btc will be measured at the outflow.
  //
  int    sigma_btc_spot;

  // Param: *sigma_btc;
  // Type: double *sigma_btc;
  // Comments: Pointer to array for storing break through curve. If sigma_btc_rate <= 0, this will remain unallocated.
  //
  double *sigma_btc;

#endif /* INAMURO_SIGMA_COMPONENT */

  // Inflow and outflow conditions for density and velocity.
  double rho_in,   rho_out;
  double ux_in,    ux_out;
  double uy_in,    uy_out;

  // Param: G;
  // Type: double G;
  // Comments: Interaction parameter.
  //
  double G;

  // Param: Gads[NUM_FLUID_COMPONENTS];
  // Type: double Gads[NUM_FLUID_COMPONENTS];
  // Comments: Adsorption parameters.
  //
  double Gads[NUM_FLUID_COMPONENTS];

  // Param: ns_flag;
  // Type: int ns_flag;
  // Comments: Flag for how to use the solid density parameter.
  // If ns_flag = 0, then initialize the domain uniformly with ns.
  // If ns_flag = 1, then read ns values from the ns<LX>x<LY>.bmp file.
  // If ns_flag = 2, then read ns mask from ns<LX>x<LY>.bmp -- that is,
  // initialize the domain with ns where there are black pixels in the
  // ns<LX>x<LY>.bmp file and 0 otherwise.
  //
  int    ns_flag;

  // Param: ns;
  // Type: double ns;
  // Comments: Solid density parameter for porous media.
  //
  double ns;

  // Param: slice_x
  // Param: slice_y
  // Type: int
  // Comments: Positions of x and y slices to be output to the slice*.m files.
  // These override the old slice.in file unless both are set to -1.
  int    slice_x;
  int    slice_y;

  // Param: ic_poiseuille;
  // Param: bc_poiseuille;
  // Type: int    ic_poiseuille;
  // Comments: Flags for poiseuille flow initial condition (ic) and boundary condition (bc).
  //
  int    ic_poiseuille;
  int    bc_poiseuille;

  // Param: bc_slip_north;
  // Type: int    bc_slip_north;
  // Comments: Toggle slip condition on north wall.
  //
  int    bc_slip_north;
#if INAMURO_SIGMA_COMPONENT
  // Param: bc_sigma_slip;
  // Type: int    bc_sigma_slip;
  // Comments: Slip BC for solute on side walls. Will this make a difference on Taylor dispersion results?  NOTE: Only use this for flow through a channel. Slip BC is not implemented for arbitrary geometries.
  //
  int    bc_sigma_slip;

  // Param: bc_sigma_walls;
  // Type: int    bc_sigma_walls;
  // Comments: Enforce concentration (sigma component) boundary conditions on walls. This will mean skipping bounce back for the sigma component on solids. WARNING: Should only be used for walls on the sides of the domain!
  //
  int    bc_sigma_walls;

#if TAU_ZHANG_ANISOTROPIC_DISPERSION
  double Dl;
  double Dt;
#endif
#endif /* INAMURO_SIGMA_COMPONENT */

  // Flags for pressure and velocity boundaries.
  int    pressure_n_in[  NUM_FLUID_COMPONENTS];
  int    pressure_s_in[  NUM_FLUID_COMPONENTS];
  int    pressure_n_out[ NUM_FLUID_COMPONENTS];
  int    pressure_s_out[ NUM_FLUID_COMPONENTS];
  int    velocity_n_in[  NUM_FLUID_COMPONENTS];
  int    velocity_s_in[  NUM_FLUID_COMPONENTS];
  int    velocity_n_out[ NUM_FLUID_COMPONENTS];
  int    velocity_s_out[ NUM_FLUID_COMPONENTS];
  int    pressure_e_in[  NUM_FLUID_COMPONENTS];
  int    pressure_w_in[  NUM_FLUID_COMPONENTS];
  int    pressure_e_out[ NUM_FLUID_COMPONENTS];
  int    pressure_w_out[ NUM_FLUID_COMPONENTS];
  int    velocity_e_in[  NUM_FLUID_COMPONENTS];
  int    velocity_w_in[  NUM_FLUID_COMPONENTS];
  int    velocity_e_out[ NUM_FLUID_COMPONENTS];
  int    velocity_w_out[ NUM_FLUID_COMPONENTS];

  // Flags for (inamuro) concentration boundaries.
  int    constcon_n_in;
  int    constcon_s_in;
  int    constcon_n_out;
  int    constcon_s_out;
  int    constflx_n_in;
  int    constflx_s_in;
  int    constflx_n_out;
  int    constflx_s_out;
  int    constcon_e_in;
  int    constcon_w_in;
  int    constcon_e_out;
  int    constcon_w_out;
  int    constflx_e_in;
  int    constflx_w_in;
  int    constflx_e_out;
  int    constflx_w_out;

  // Flags for zero concentration gradient boundaries.
  // These differ from inamuro's concentration flux boundaries
  // in that they allow advective flux of the concentration
  // across the boundary.
  int    zeroconcgrad_n;
  int    zeroconcgrad_s;
  int    zeroconcgrad_e;
  int    zeroconcgrad_w;

  // Param: zeroconcgrad_full;
  // Type: int    zeroconcgrad_full;
  // Comments: The zero concentration gradient can be computed by copying (from the adjacent interior neighbor) either just the unknown distribution functions after streaming or all the distributions after streaming. This flag toggles between those methods (where "full" denotes the latter method).
  //
  int    zeroconcgrad_full;

  // Param: use_colormap;
  // Type: int    use_colormap;
  // Comments: See the colormap routines in lbio.c to learn about colormap support.
  //
  int    use_colormap;

  // Param: plot_scale_dynamic;
  // Type: int    plot_scale_dynamic;
  // Comments: Toggle dynamic scaling of the plots. If this is on, color values to be plotted will be dynamically scaled to range between the minimum and maximum values of the data for the current plot. If this is off, color values will be scaled only by the initial value for that quantity (e.g. rho_sigma, rho_A[subs], etc...). NOTE: This is only implemented in rho2bmp() as of 10/15/2004.
  //
  int    plot_scale_dynamic;

  // Param: initial_condition;
  // Type: int    initial_condition;
  // Comments: See bottom of flags.h for listing/description of initial conditions.
  //
  int    initial_condition;

  // Center and radius of a circle. Used in the initial conditions for
  // initializing a bubble in the domain, for instance.
  double x0;
  double y0;
  double r0;

  // Param: cut;
  // Type: double cut;
  // Comments: Cut-off value. Used in the initial conditions for generating static in the two-component case. Sometimes the static needs to be biased toward one component in order for it to evolve stabily.
  //
  double cut;

  // Corner coordinates for a rectangle. Used by initial conditions for
  // initializing a square-shaped region in the domain.
  double x1, x2;
  double y1, y2;

  // Factors for setting coordinates relative to the domain size. If
  // the absolute coordinates are negative, use these values to set
  // the coordinates, e.g. x1 = rel_x1*LX. If these rel_* values are
  // also negative, then revert to some default absolute coordinates.
  double rel_x1, rel_x2;
  double rel_y1, rel_y2;

  // Param: dump_rho;
  // Type: int    dump_rho;
  // Comments: Flag to toggle output of the macroscopic density.
  //
  int    dump_rho;

  // Param: dump_u;
  // Type: int    dump_u;
  // Comments: Flag to toggle output of the macroscopic velocity.
  //
  int    dump_u;

  // Param: dump_force;
  // Type: int    dump_force;
  // Comments: Flag to toggle output of the interaction and adsorption forces.
  //
  int    dump_force;

  // Param: dump_vor;
  // Type: int    dump_vor;
  // Comments: Flag to toggle output of vorticity.
  //
  int    dump_vor;

  // Param: do_user_stuff;
  // Type: int do_user_stuff;
  // Comments: Flag to toggle use of user defined stuff. The user can
  // define a user_stuff_struct in user_stuff.h and can put stuff in
  // routines
  //   void do_user_stuff_pre_frames( lattice_ptr lattice);
  //   void do_user_stuff_frame( lattice_ptr lattice);
  //   void do_user_stuff_post_frames( lattice_ptr lattice);
  //   void do_user_stuff_pre_times( lattice_ptr lattice);
  //   void do_user_stuff_time( lattice_ptr lattice);
  //   void do_user_stuff_post_times( lattice_ptr lattice);
  // which are called in lb2d_prime.c.
  //
  int    do_user_stuff;

  char   out_path[1024];

  // Param: make_octave_scripts;
  // Type: int make_octave_scripts;
  // Comments: Flag to toggle generation of Octave scripts instead of Matlab
  // scripts. If make_octave_scripts is off (=0) then Matlab scripts will be
  // generated by default.  If make_octave_scripts is on (=1) then Octave
  // scripts will be generated. Currently (06162005) Octave scripts are
  // minimal -- just a few plots. This will be extended in the future.
  //
  int    make_octave_scripts;

}; /* struct param_struct */

// struct user_stuff_struct;
//  - Forward declaration for user_stuff_struct. If do_user_stuff is
//    on in param_struct, then the user will need to have defined the
//    user_stuff_struct in user_stuff.h.
struct user_stuff_struct;

// struct lattice_struct
//
//  - Structure with all the lattice information.
//
//  - Contains arrays of the previously defined structures.
//
struct lattice_struct
{
  int    NumNodes;
  int    NumTimeSteps;
  int    time;
  int    frame;
  int    periodic_x[ NUM_FLUID_COMPONENTS];
  int    periodic_y[ NUM_FLUID_COMPONENTS];

#if TAU_ZHANG_ANISOTROPIC_DISPERSION
  double *tau_zhang;
#endif

#if INAMURO_SIGMA_COMPONENT
  int    SizeBTC; // Number of BTC measurements to store.
  int    FlowDir; // Direction {1,2} ==> {Horiz,Vert} of flow.
#endif /* INAMURO_SIGMA_COMPONENT */

  struct param_struct      param;

#if DO_NOT_STORE_SOLIDS
  struct node_struct       *node;
#endif /* DO_NOT_STORE_SOLIDS */
  struct pdf_struct        *pdf[        NUM_FLUID_COMPONENTS];
  struct macro_vars_struct *macro_vars[ NUM_FLUID_COMPONENTS];
#if PEST_OUTPUT_ON
  int array_position;
  int conc_array_size;
  struct conc_data_struct  *concentration_data;
#endif
  struct bc_struct         *bc[         NUM_FLUID_COMPONENTS];
  struct bcs_in_struct      bcs_in[     NUM_FLUID_COMPONENTS];
#if NON_LOCAL_FORCES
  struct force_struct      *force[      NUM_FLUID_COMPONENTS];
#endif /* NON_LOCAL_FORCES */
#if STORE_U_COMPOSITE
  struct upr_struct        *upr;
#endif /* STORE_U_COMPOSITE */
#if POROUS_MEDIA || FREED_POROUS_MEDIA
  struct ns_struct         *ns;
#endif /* POROUS_MEDIA */

//LBMPI #if PARALLEL
//LBMPI   lbmpi_ptr lbmpi;
//LBMPI #endif /* (PARALLEL) */

  struct user_stuff_struct *user_stuff;

  struct process_struct    process;

};
//typedef struct lattice_struct *lattice_ptr;

struct report_struct
{
  FILE *file;
  char name[1024];
};
//typedef struct report_struct *report_ptr;

struct bitmap_file_header
{
  // 1 2 bfType 19778 must always be set to 'BM' to declare that this is
  // a .bmp-file.
  char bfType[2];
  // 3 4 bfSize ?? specifies the size of the file in bytes.
  char bfSize[4];
  // 7 2 bfReserved1 0 must always be set to zero.
  char bfReserved1[2];
  // 9 2 bfReserved2 0 must always be set to zero.
  char bfReserved2[2];
  // 11 4 bfOffBits 1078 specifies the offset from the beginning of the
  // file to the bitmap data.
  char bfOffBits[4];

};

struct bitmap_info_header
{
  // 15 4 biSize 40 specifies the size of the BITMAPINFOHEADER structure,
  // in bytes.
  char biSize[4];
  // 19 4 biWidth 100 specifies the width of the image, in pixels.
  char biWidth[4];
  // 23 4 biHeight 100 specifies the height of the image, in pixels.
  char biHeight[4];
  // 27 2 biPlanes 1 specifies the number of planes of the target device,
  // must be set to zero. [DT: Should be set to one, right? Not zero.]
  char biPlanes[2];
  // 29 2 biBitCount 8 specifies the number of bits per pixel.
  char biBitCount[2];
  // 31 4 biCompression 0 Specifies the type of compression, usually set
  // to zero (no compression).
  char biCompression[4];
  // 35 4 biSizeImage 0 specifies the size of the image data, in bytes.
  // If there is no compression, it is valid to set this member to zero.
  char biSizeImage[4];
  // 39 4 biXPelsPerMeter 0 specifies the the horizontal pixels per meter
  // on the designated targer device, usually set to zero.
  char biXPelsPerMeter[4];
  // 43 4 biYPelsPerMeter 0 specifies the the vertical pixels per meter
  // on the designated targer device, usually set to zero.
  char biYPelsPerMeter[4];
  // 47 4 biClrUsed 0 specifies the number of colors used in the bitmap,
  // if set to zero the number of colors is calculated using the biBitCount
  // member.
  char biClrUsed[4];
  // 51 4 biClrImportant 0 specifies the number of color that are
  // 'important' for the bitmap, if set to zero, all colors are important.
  char biClrImportant[4];

};

struct bmp_hdr_struct
{
  char type[2];  // 'BM'
  char size[4];  // Size of file in bytes.
  char rsvd[4];  // 0
  char dptr[4];  // Offset of data in bytes.

  char forty[4]; // Size of info header (=40).
  char width[4]; // Width of image in pixels
  char height[4];// Height of image in pixels.
  char planes[2];// Must be set to one.
  char depth[2]; // Color depth: bits per pixel.
  char zeros[24];// Unused parameters.

}; /* struct bmp_hdr_struct */

struct test_struct
{
  char type[2];  // 'BM'
  int size;  // Size of file in bytes.
#if 0
  int rsvd;  // 0
  int dptr;  // Offset of data in bytes.

  int forty; // Size of info header (=40).
  int width; // Width of image in pixels
  int height;// Height of image in pixels.
  short int planes;// Must be set to one.
  short int depth; // Color depth: bits per pixel.
  int zeros[6];// Unused parameters.
#endif

}; /* struct test_struct*/




struct rgb_quad
{
  // 1 1 rgbBlue - specifies the blue part of the color.
  char Blue;
  // 2 1 rgbGreen - specifies the green part of the color.
  char Green;
  // 3 1 rgbRed - specifies the red part of the color.
  char Red;
  // 4 1 rgbReserved - must always be set to zero.
  char Reserved;
};

#include "lattice.c"

#endif
