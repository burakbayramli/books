#ifndef matrix_cpp_h
#define matrix_cpp_h

class Matrix
{
  // data for nxn matrix:
  double** A;
  int n;
 public:
  Matrix(int n);
  ~Matrix();
  
  double  operator() (int i, int j) const { return A[i][j]; }
  double& operator() (int i, int j)       { return A[i][j]; }

  void set(int i, int j, double value) { A[i][j] = value; }
  double get(int i, int j) { return A[i][j]; }

  void fill1();  // fill all A entries from i*j type formula
  void fill2();  // fill all A entries from sin/exp type formula
};  
#endif
