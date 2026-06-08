
#include <stdio.h>
#include <stdlib.h>
#include <memory.h>

#define TWO_SUBS 0

int ni=100;
int nj=100;
int nf=23;

#define ij2n(i_,j_) ((j_)*ni+(i_))

void alloc_rho( double **rho_a, double ***rho_a2d);
void read_rho_file( char *filename, double **rho, double *max, double *min);
void display_density_plot( FILE *o, double *rho_a, int max_width);
void write_rho_profile( FILE *o, double *rho_a, int i);

int main( int argc, char **argv) {

  FILE *o;

  char filename[1024];

  double **rho_a2d;
  double *rho_a;
  double **rho_b2d;
  double *rho_b;

  double rho_a_max;
  double rho_a_min;
  double rho_b_max;
  double rho_b_min;

  int i, j;

  int iprof;
  int max_width;

  switch( argc)
  {
    case 2:
      nf = atoi( argv[1]);
      iprof = ni/2;
      max_width = 85;
      break;
    case 3:
      nf = atoi( argv[1]);
      iprof = atoi( argv[2]);
      max_width = 85;
      break;
    case 4:
      ni = atoi( argv[1]);
      nj = atoi( argv[2]);
      nf = atoi( argv[3]);
      iprof = ni/2;
      max_width = 85;
      break;
    case 5:
      ni = atoi( argv[1]);
      nj = atoi( argv[2]);
      max_width = atoi( argv[3]);
      nf = atoi( argv[4]);
      iprof = ni/2;
      break;
    case 6:
      ni = atoi( argv[1]);
      nj = atoi( argv[2]);
      max_width = atoi( argv[3]);
      nf = atoi( argv[4]);
      iprof = atoi( argv[5]);
      break;
    default:
      printf("ERROR: Unhandled case: argc = %d. Exiting\n", argc);
      process_exit(1);
      break;
  }

  alloc_rho( &rho_a, &rho_a2d);
#if TWO_SUBS
  alloc_rho( &rho_b, &rho_b2d);
#endif

  sprintf(filename,"./out/rho%dx%d_frame%04d_subs00.dat",ni,nj,nf);
  read_rho_file( filename, &rho_a, &rho_a_max, &rho_a_min);

#if TWO_SUBS
  sprintf(filename,"./out/rho%dx%d_frame%04d_subs01.dat",ni,nj,nf);
  read_rho_file( filename, &rho_b, &rho_b_max, &rho_b_min);
#endif

#if TWO_SUBS
  display_density_plot( stdout, rho_b, max_width); printf("\n");
#endif
  display_density_plot( stdout, rho_a, max_width); printf("\n");

  sprintf(filename,"./out/rho%dx%d_frame%04d_subs00_profile%04d.dat",
    ni,nj,nf,iprof);
  o = fopen(filename,"w+");
  write_rho_profile( o, rho_a, iprof);
  fclose(o);

#if TWO_SUBS
  sprintf(filename,"./out/rho%dx%d_frame%04d_subs01_profile%04d.dat",
    ni,nj,nf,iprof);
  o = fopen(filename,"w+");
  write_rho_profile( o, rho_b, iprof);
  fclose(o);
#endif

  printf("rho_a_max = %f, ", rho_a_max);
  printf("rho_a_min = %f, ", rho_a_min);
#if TWO_SUBS
  printf("rho_b_max = %f, ", rho_b_max);
  printf("rho_b_min = %f, ", rho_b_min);
#endif
  printf("Wrote profile at i = %d.\n", iprof);

  free( rho_a);
#if TWO_SUBS
  free( rho_b);
#endif

  return 0;
}

void alloc_rho( double **rho, double ***rho2d) {
  int j;
  *rho = (double*)malloc( ni*nj*sizeof(double));
  *rho2d = (double**)malloc( nj*sizeof(double*));
  for( j=0; j<nj; j++)
  {
    (*rho2d)[j] = &((*rho)[j*ni]);
  }
}

void read_rho_file( char *filename, double **rho, double *max, double *min) {
  FILE *in;
  int i, j;

  if( !( in = fopen(filename,"r")))
  {
    printf("ERROR: Error opening file \"%s\". Exiting.\n",filename);
    process_exit(1);
  }

  j = 0;
  i = 0;
  fscanf( in, "%lf ", ((*rho + ij2n(i,j))));
  *max = *(*rho + ij2n(i,j));
  *min = *(*rho + ij2n(i,j));
  for( i=1; i<ni; i++)
  {
    fscanf( in, "%lf ", ((*rho + ij2n(i,j))));
    if( *max < *(*rho + ij2n(i,j))) { *max = *(*rho + ij2n(i,j));}
    if( *min > *(*rho + ij2n(i,j))) { *min = *(*rho + ij2n(i,j));}
  }

  for( j=1; j<nj; j++)
  {
    for( i=0; i<ni; i++)
    {
      fscanf( in, "%lf ", ((*rho + ij2n(i,j))));
      if( *max < *(*rho + ij2n(i,j))) { *max = *(*rho + ij2n(i,j));}
      if( *min > *(*rho + ij2n(i,j))) { *min = *(*rho + ij2n(i,j));}
    }
  }

  fclose( in);
}

void display_density_plot( FILE *o, double *rho_a, int max_width) {

  int i, j;
  for( j=nj-1; j>=0; j--)
  {
    for( i=0; i<((ni<=max_width)?(ni):(max_width)); i++)
    {
      fprintf( o, " %c",
        (char)(((rho_a[ij2n(i,j)]-85.)/(524.-85.)>1./5.)
             ?(((rho_a[ij2n(i,j)]-85.)/(524.-85.)>2./5.)
             ?(((rho_a[ij2n(i,j)]-85.)/(524.-85.)>3./5.)
             ?(((rho_a[ij2n(i,j)]-85.)/(524.-85.)>4./5.)
             ?('#')
             :('X'))
             :('O'))
             :('+'))
             :('.')));
      //printf("%.1f  ", rho_a2d[ij2n(i,j)]);
    }
    printf("\n");
  }
  for( i=0; i<((ni<=max_width)?(ni):(max_width)); i++)
  {
    fprintf( o, "%2d", i%100);
  }
}

void write_rho_profile( FILE *o, double *rho_a, int i) {

  int j;
  for( j=0; j<nj; j++)
  {
    fprintf( o, "%f\n", rho_a[ ij2n(i,j)]);
  }
}
