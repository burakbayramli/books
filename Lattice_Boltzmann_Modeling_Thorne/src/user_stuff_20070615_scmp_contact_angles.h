//##############################################################################
//
// user_stuff.h
//
//  - This file includes a definition of the struct user_struct, which the
//    user should fill in with whatever they want. It will be accessed
//    from the lattice structure as lattice->user_stuff->whatever
//
//  - This file also has forward declarations for the user_stuff functions.
//    These functions are defined in user_stuff.c. The user should put in
//    them whatever they want. See user_stuff.c for more information.
//
//

struct user_stuff_struct
{
  double rho_c;
  double rho_c_start;
  double rho_c_inc;
  double rho_c_reverse;

  double rho_ave;
  double rho_ave_prev;
  double u_ave[2];
  double u_ave_prev[2];

  double tol;

  FILE *o;
};
typedef struct user_stuff_struct *user_stuff_ptr;

void user_stuff_pre_frames(   lattice_ptr lattice);
void user_stuff_frame( lattice_ptr lattice);
void user_stuff_post_frames(  lattice_ptr lattice);
void user_stuff_pre_times(   lattice_ptr lattice);
void user_stuff_time( lattice_ptr lattice);
void user_stuff_post_times(  lattice_ptr lattice);

