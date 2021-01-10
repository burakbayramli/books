//##############################################################################
//
// Copyright (C), 2005, Michael Sukop and Danny Thorne
//
// forward_declarations.h
//
//  - Forward declarations of routines for lb_prime.
//

#ifndef FORWARD_DECLARATIONS_H
#define FORWARD_DECLARATIONS_H

struct lattice_struct;
typedef struct lattice_struct *lattice_ptr;
struct bitmap_file_header;
struct bitmap_info_header;
struct bmp_hdr_struct;
typedef struct bmp_hdr_struct *bmp_hdr_ptr;
struct rgb_quad;
struct report_struct;
typedef struct report_struct *report_ptr;

void dump_frame_summary( struct lattice_struct *lattice);
void dump_macro_vars( struct lattice_struct *lattice, int time);
void collide( struct lattice_struct *lattice);
void compute_macro_vars( struct lattice_struct *lattice, int which_f);
void compute_feq( struct lattice_struct *lattice, int skip_sigma);
void compute_max_rho( lattice_ptr lattice, double *max_rho, int subs);
void compute_max_u( lattice_ptr lattice, double *max_u, int subs);
void compute_ave_u( lattice_ptr lattice, double *max_u, int subs);
void compute_ave_upr( lattice_ptr lattice, double *max_u);
void compute_ave_rho( lattice_ptr lattice, double *ave_rho, int subs);
void process_matrix( struct lattice_struct *lattice, int **matrix, int subs);
void process_bcs( lattice_ptr lattice, int subs);
void read_params( lattice_ptr lattice, const char *infile);
void construct_lattice( lattice_ptr *lattice, int argc, char **argv);
void init_problem( struct lattice_struct *lattice);
void destruct_lattice( struct lattice_struct *lattice);
void init_problem( struct lattice_struct *lattice);
void destruct_lattice( struct lattice_struct *lattice);
void dump_macro_vars( struct lattice_struct *lattice, int time);
void dump_pdf( struct lattice_struct *lattice, int time);
void dump_lattice_info( struct lattice_struct *lattice);
#if DO_NOT_STORE_SOLIDS
void dump_node_info( struct lattice_struct *lattice);
#endif /* DO_NOT_STORE_SOLIDS */
void dump_checkpoint( struct lattice_struct *lattice, int time, char *fn);
void read_checkpoint( struct lattice_struct *lattice);
void stream( struct lattice_struct *lattice);
void slice( lattice_ptr lattice);
void private_slice( lattice_ptr lattice,
     char *root_word, int i0, int j0, int i1, int j1);
#if NON_LOCAL_FORCES
void compute_phase_force( lattice_ptr lattice, int subs);
void compute_fluid_fluid_force( lattice_ptr lattice);
void compute_double_fluid_solid_force( lattice_ptr lattice);
void compute_single_fluid_solid_force( lattice_ptr lattice, int subs);
void dump_forces( struct lattice_struct *lattice);
void force2bmp( lattice_ptr lattice);
void sforce2bmp( lattice_ptr lattice);
#endif /* NON_LOCAL_FORCES */
void rho2bmp( lattice_ptr lattice, int time);
void u2bmp( lattice_ptr lattice, int time);
void vor2bmp( lattice_ptr lattice, int time);
void count_colormap( int *num_colors);
void allocate_colormap( double ***colormap, int num_colors);
void read_colormap( double **colormap, int num_colors);
void deallocate_colormap( double ***colormap, int num_colors);
void get_color(
       double **colormap, int num_colors,
       double c, char *r, char *g, char *b);
#if WRITE_CHEN_DAT_FILES
void chen_output( lattice_ptr lattice);
#endif /* WRITE_CHEN_DAT_FILES */
#if MANAGE_BODY_FORCE
inline void manage_body_force( lattice_ptr lattice);
#endif /* MANAGE_BODY_FORCE */
#if INAMURO_SIGMA_COMPONENT && STORE_BTC
void sigma_stuff( lattice_ptr lattice);
#endif /* INAMURO_SIGMA_COMPONENT && STORE_BTC */

int do_check_point_save( lattice_ptr lattice);
int do_check_point_load( lattice_ptr lattice);

void bmp_read_header( FILE *in, struct bitmap_info_header *bmih);
void bmp_read_entry(
  FILE *in,
  struct bitmap_info_header *bmih,
  char *r, char *g, char *b);
void bmp_write_header( FILE *out, bmp_hdr_ptr bmp_hdr, int ni, int nj, int bits);
void bmp_write_entry(
  FILE *out,
  bmp_hdr_ptr bmp_hdr,
  int n,
  char r, char g, char b);

void report_open( report_ptr report, char *name);
void report_integer_entry(
       report_ptr report, char *label, int value, char *units);
void report_ratio_entry(
       report_ptr report, char *label, double num, double den, char *units);
void report_entry( report_ptr report, char *entry_left, char *entry_right);
void report_close( report_ptr report);
void report_partition( report_ptr report);

int get_sizeof_lattice_structure( lattice_ptr lattice);
int get_sizeof_lattice(           lattice_ptr lattice);
int get_num_active_nodes(         lattice_ptr lattice);
inline void run_man( lattice_ptr lattice);
inline void dump( lattice_ptr lattice, int tick_num);

//LBMPI #ifdef PARALLEL
//LBMPI struct lbmpi_struct;
//LBMPI typedef struct lbmpi_struct *lbmpi_ptr;
//LBMPI void lbmpi_construct( lbmpi_ptr lbmpi, lattice_ptr lattice, int argc, char **argv);
//LBMPI void lbmpi_allocate_datatypes( lbmpi_ptr lbmpi, lattice_ptr lattice);
//LBMPI MPI_Aint *lbmpi_get_Index0_ptr( lbmpi_ptr lbmpi);
//LBMPI #endif /* (PARALLEL) */


#endif /* FORWARD_DECLARATIONS_H */
