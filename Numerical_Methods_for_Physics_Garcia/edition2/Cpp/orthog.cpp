// orthog - Program to test if a pair of vectors 
// is orthogonal.  Assumes vectors are in 3D space
#include <iostream.h>

void main() {

  //* Initialize the vectors a and b
  double a[3+1], b[3+1];
  cout << "Enter the first vector" << endl;
  int i;
  for( i=1; i<=3; i++ ) {
    cout << "  a[" << i << "] = ";
    cin >> a[i];
  }
  cout << "Enter the second vector" << endl;
  for( i=1; i<=3; i++ ) {
    cout << "  b[" << i << "] = ";
    cin >> b[i];
  }

  //* Evaluate the dot product as sum over products of elements
  double a_dot_b = 0.0;
  for( i=1; i<=3; i++ )
    a_dot_b += a[i]*b[i];


  //* Print dot product and state whether vectors are orthogonal
  if( a_dot_b == 0.0 )
    cout << "Vectors are orthogonal" << endl;
  else {
    cout << "Vectors are NOT orthogonal" << endl;
    cout << "Dot product = " << a_dot_b << endl;
  }
}
