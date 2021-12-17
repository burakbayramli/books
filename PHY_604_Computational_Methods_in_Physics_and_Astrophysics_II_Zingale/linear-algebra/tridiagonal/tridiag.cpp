#include <iostream>
#include <array>

int tridiag(const double*,  const double*, const double*, const double*,
	    double *, int);

int tridiag_Ax(const double *, const double *, const double *, const double *,
	       double *, int);


int tridiag(const double* a, const double* b, const double* c, const double* d,
	    double *x, int N) {

  /* solve the tridiagonal system of the form Ax = d:

          a_i x_{i-1} + b_i x_i + c_i x_{i+1} = d_i

     for i = 0, n-1 with a_0 = 0 and c_{n-1} = 0

     In matrix form, b is the main diagonal, a is the subdiagonal and
     c is the superdiagonal. */

  double cprime[N];
  double dprime[N];

  cprime[0] = c[0]/b[0];
  dprime[0] = d[0]/b[0];

  for (int i=0; i<N-1; i++) {
    cprime[i] = c[i]/(b[i] - cprime[i-1]*a[i]);
      dprime[i] = (d[i] - dprime[i-1]*a[i])/(b[i] - cprime[i-1]*a[i]);
  }

  // there is no cprime for N-1
  dprime[N-1] = (d[N-1] - dprime[N-2]*a[N-1])/(b[N-1] - cprime[N-2]*a[N-1]);

  // back substitution
  x[N-1] = dprime[N-1];
  for (int i = N-2; i >= 0; i--) {
    x[i] = dprime[i] - cprime[i]*x[i+1];
  }

  return 0;

}


int tridiag_Ax(const double *a, const double *b, const double *c, const double *x,
	       double *d, int N) {

  /* multiply the tridiagonal matrix A by vector x and return the
     product vector d */

  d[0] = b[0]*x[0] + c[0]*x[1];
  for (int i=1; i < N-1; i++) {
    d[i] = a[i]*x[i-1] + b[i]*x[i] + c[i]*x[i+1];
  }
  d[N-1] = a[N-1]*x[N-2] + b[N-1]*x[N-1];

  return 0;

}


int main() {

  // test things out

  int N = 10;

  double *a, *b, *c, *x, *d;

  a = new double [N];
  b = new double [N];
  c = new double [N];
  d = new double [N];
  x = new double [N];

  // we'll create a test matrix with b = 4, a = c = 1
  for (int i=0; i<N; i++) {
    a[i] = 1.0;
    b[i] = 4.0;
    c[i] = 1.0;
  }

  // we'll make x just be a sequence of ints
  for (int i=0; i<N; i++) {
    x[i] = i;
  }

  // get d from our matrix multiply
  tridiag_Ax(a, b, c, x, d, N);

  // now recover x from our tridiag solve
  tridiag(a, b, c, d, x, N);

  for (int i=0; i<N; i++) {
    std::cout << x[i] << std::endl;
  }

}

