//  (NONLINEAR-)HELMHOLTZ SOLVER WITH HIGH ORDER 3D FINITE ELEMENTS
//  ---------------------------------------------------------------
//  This code implements the polynomial,
//  node, high-order finite element,
//  mesh, dynamic-vector,
//  and sparse-matrix objects required
//  to discretize the nonlinear 
//  Helmholtz equation in 3-D
//  on an adaptive mesh of tetrahedra.
//  Readers are advised to add an extra
//  code line and apply the destructor
//  to each dynamic object at the end
//  of the block in which it is defined
//  to release the valuable "heap" memory
//  occupied by it for future use.


#include<stdio.h>
#include<math.h>

const int PrintMesh=0;
const int PrintSol=0;
const int Regular=0;
const int boundaryLevels=0;
const int nonlinearLevels=10;
const int adaptiveLevels=2;
const int refineLevels=0;
const int Dirichlet=1;
const double SlitWidth=.2;
const double SlitLength=.5;
const double HELM=-.01;
double HELMNonlin=-0.0;
int Newton = 1;
const int RowSumStablizer=0;
double thresholdAdaptive=-0.01;
int useILU=1;
const double thresholdILU=0.05;
const double thresholdCG=1.e-6;
double ratioMG=-.5;
const int AMG=0;
const int Circle=1;
const double thresholdMG=0.05;
const double thresholdMGAtilde=0.05;
const double thresholdMGpositive=0.05;
const double thresholdMGQ=0.0;
const double FactorFine=1.;
const int ThrowQ=0;
const int Smooth=0;
const int TruncateAtilde=0;
const int ThrowAtilde=0;
const int Nu1=2;
const int Nu2=2;
const int NuCoarse=1;
const int cycleIndex=1;

int indexK(int i, int j){
  return i>=j ? i * (i + 1) / 2 + j : indexK(j,i);
}

int indexI(int k){
  int i = 0;
  for(; i * (i + 1) / 2 <= k; i++);
  return i - 1;
}

int indexJ(int k){
  return k - indexI(k) * (indexI(k) + 1) / 2;
}

int max(int a, int b){return a>b ? a : b;}
int min(int a, int b){return a<b ? a : b;}

double max(double a, double b){return a>b ? a : b;}
double min(double a, double b){return a<b ? a : b;}

double fabs(double d){return d > 0. ? d : -d;}  //  absolute value

int C(int a, int n){
  return n ? a * C(a-1,n-1) : 1;
}  //  a!/(a-n)!

int factorial(int n){
  return n ? n * factorial(n-1) : 1;
}  //  factorial

int power(int basis, unsigned exp){
  return exp ? basis * power(basis,exp-1) : 1;
}  //  "basis" to the "exp"

template<class T, int N> class vector{
  T component[N];
public:
  vector(const T&);
  vector(const T&a,const T&b){
    component[0] = a; component[1] = b;
  }  // constructor for 2-d vectors
  vector(const T&a,const T&b,const T&c){
    component[0] = a; component[1] = b; component[2] = c;
  }  // constructor for 3-d vectors
  vector(const vector&);
  const vector& operator=(const vector&);
  const vector& operator=(const T&);
  ~vector(){}  //  destructor
  const T& operator[](int i) const{ return component[i]; }  //read only ith component
  T& operator()(int i){ return component[i]; 
  }  //read/write ith component
  void set(int i,const T& a){ component[i] = a; }  //  change ith component
  const vector& operator+=(const vector&);
  const vector& operator-=(const vector&);
  const vector& operator*=(const T&);
  const vector& operator/=(const T&);
};

  template<class T, int N>
  vector<T,N>::vector(const T& a = 0){
     for(int i = 0; i < N; i++)
       component[i] = a;
  }  //  constructor

  template<class T, int N>
  vector<T,N>::vector(const vector<T,N>& v){
    for(int i = 0; i < N; i++)
      component[i] = v.component[i];
  }  //  copy constructor

  template<class T, int N>
  const vector<T,N>& vector<T,N>::operator=(const vector<T,N>& v){
   if(this != &v)
     for(int i = 0; i < N; i++)
       component[i] = v.component[i];
   return *this;
 }  //  assignment operator

  template<class T, int N>
  const vector<T,N>& vector<T,N>::operator=(const T& a){
    for(int i = 0; i < N; i++)
      component[i] = a;
    return *this;
  }  //  assignment operator with a scalar argument

  template<class T, int N>
  const vector<T,N>& vector<T,N>::operator+=(const vector<T,N>&v){
      for(int i = 0; i < N; i++)
	component[i] += v[i];
      return *this;
  }  //  adding a vector to the current vector

  template<class T, int N>
  const vector<T,N>& vector<T,N>::operator-=(const vector<T,N>&v){
      for(int i = 0; i < N; i++)
	component[i] -= v[i];
      return *this;
  }  //  subtracting a vector from the current vector

  template<class T, int N>
  const vector<T,N>& vector<T,N>::operator*=(const T& a){
      for(int i = 0; i < N; i++)
	component[i] *= a;
      return *this;
  }  //  multiplying the current vector by a scalar

  template<class T, int N>
  const vector<T,N>& vector<T,N>::operator/=(const T& a){
      for(int i = 0; i < N; i++)
	component[i] /= a;
      return *this;
  }  //  multiplying the current vector by a scalar

  template<class T, int N>
  const vector<T,N> operator+(const vector<T,N>&u, const vector<T,N>&v){
    return vector<T,N>(u) += v;
  }  //  vector plus vector

  template<class T, int N>
  const vector<T,N> operator-(const vector<T,N>&u, const vector<T,N>&v){
    return vector<T,N>(u) -= v;
  }  //  vector minus vector

  template<class T, int N>
  const vector<T,N> operator*(const vector<T,N>&u, const T& a){
    return vector<T,N>(u) *= a;
  }  //  vector times scalar

  template<class T, int N>
  const vector<T,N> operator*(const T& a, const vector<T,N>&u){
    return vector<T,N>(u) *= a;
  }  //  'T' times vector

  template<class T, int N>
  const vector<T,N> operator/(const vector<T,N>&u, const T& a){
    return vector<T,N>(u) /= a;
  }  //  vector times scalar

  template<class T, int N>
  const vector<T,N>& operator+(const vector<T,N>&u){
    return u;
  }  //  negative of a vector

  template<class T, int N>
  const vector<T,N> operator-(const vector<T,N>&u){
    return vector<T,N>(u) *= -1;
  }  //  negative of a vector

  template<class T, int N>
  const T operator*(const vector<T,N>&u, const vector<T,N>&v){
      T sum = 0;
      for(int i = 0; i < N; i++)
	sum += u[i] * +v[i];
      return sum;
  }  //  vector times vector (inner product)

  template<class T, int N>
  const T squaredNorm(const vector<T,N>&u){
      return u*u;
  }  //  squared l2 norm

  template<class T, int N>
  const T l2norm(const vector<T,N>&u){
      return sqrt(u*u);
  }  //  l2 norm

  template<class T, int N>
  void print(const vector<T,N>&v){
    printf("(");
    for(int i = 0;i < N; i++){
      printf("v[%d]=",i);
      print(v[i]);
    }
    printf(")\n");
  }  //  printing a vector

typedef vector<double,1> point1;

typedef vector<double,2> point;

typedef vector<double,3> point3;

template<class T, int N, int M> class matrix : public vector<vector<T,N>,M>{
  public:
    matrix(){}
    matrix(const vector<vector<T,N>,M>&){}
    matrix(double d,char*){
      set(0,point1(d));
    }  //  constructor
    matrix(double d){
      set(0,point(d,0.));
      set(1,point(0.,d));
    }  //  constructor
    matrix(const vector<T,N>&u, const vector<T,N>&v){
      set(0,u);
      set(1,v);
    }  //  constructor
    matrix(const vector<T,N>&u, const vector<T,N>&v, const vector<T,N>&w){
      set(0,u);
      set(1,v);
      set(2,w);
    }  //  constructor
    matrix(const vector<T,N>&u, const vector<T,N>&v, const vector<T,N>&w, const vector<T,N>&r, const vector<T,N>&s, const vector<T,N>&t){
      set(0,u);
      set(1,v);
      set(2,w);
      set(3,r);
      set(4,s);
      set(5,t);
    }  //  constructor
    ~matrix(){}  //  destructor
    const T& operator()(int i,int j) const{return (*this)[j][i];}//A(i,j) read only
    vector<T,N>& operator()(int i){
      return vector<vector<T,N>,M>::operator()(i);
    }
    T& operator()(int i,int j,char*){
      return (*this)(j)(i);
    }//A(i,j) read/write
    const matrix& operator+=(const matrix&);
    const matrix& operator-=(const matrix&);
    const matrix& operator*=(const T&);
    const matrix& operator/=(const T&);
};

typedef matrix<double,1,1> matrix1;

typedef matrix<double,2,2> matrix2;

typedef matrix<double,3,3> matrix3;

typedef matrix<double,6,6> matrix6;

double det(const matrix2&A){
  return A(0,0)*A(1,1) - A(0,1)*A(1,0);
}  //  determinant of matrix

double det(const matrix3&A){
  return A(0,0) * (A(1,1)*A(2,2)-A(1,2)*A(2,1)) 
      - A(0,1) * (A(1,0)*A(2,2)-A(1,2)*A(2,0)) 
      + A(0,2) * (A(1,0)*A(2,1)-A(1,1)*A(2,0));
}  //  determinant of matrix3

const matrix1 inverse(const matrix1&A){
  point1 column(1./A(0,0));
  return matrix1(column);
}  //  inverse of matrix1

  template<class T, int N, int M>
  const matrix<T,N,M>& matrix<T,N,M>::operator/=(const T&a){
    for(int i=0; i<M; i++)
      set(i,(*this)[i] / a);
    return *this;
  }  //  division by scalar

  template<class T, int N, int M>
  const matrix<T,N,M> operator*(const T&a,const matrix<T,N,M>&m){
    return matrix<T,N,M>(m) *= a;
  }  //  scalar times matrix

  template<class T, int N, int M>
  const matrix<T,N,M> operator*(const matrix<T,N,M>&m, const T&a){
    return matrix<T,N,M>(m) *= a;
  }  //  matrix times scalar

  template<class T, int N, int M>
  const matrix<T,N,M> operator/(const matrix<T,N,M>&m, const T&a){
    return matrix<T,N,M>(m) /= a;
  }  //  matrix divided by scalar

const matrix2 inverse(const matrix2&A){
  point column0(A(1,1),-A(1,0));
  point column1(-A(0,1),A(0,0));
  return matrix2(column0,column1)/det(A);
}  //  inverse of matrix

const matrix3 inverse(const matrix3&A){
  point3 column0(A(1,1)*A(2,2)-A(1,2)*A(2,1),-(A(1,0)*A(2,2)-A(1,2)*A(2,0)),A(1,0)*A(2,1)-A(1,1)*A(2,0));
  point3 column1(-(A(0,1)*A(2,2)-A(0,2)*A(2,1)),A(0,0)*A(2,2)-A(0,2)*A(2,0),-(A(0,0)*A(2,1)-A(0,1)*A(2,0)));
  point3 column2(A(0,1)*A(1,2)-A(0,2)*A(1,1),-(A(0,0)*A(1,2)-A(0,2)*A(1,0)),A(0,0)*A(1,1)-A(0,1)*A(1,0));
  return matrix3(column0,column1,column2)/det(A);
}  //  inverse of matrix3

  template<class T, int N>
  T det(const matrix<T,N,N>&A){
    if(N==1)
      return A(0,0);
    T detA = 0.;
    int sign = 1;
    for(int j=0; j<N; j++){
      detA += sign * A(0,j) * det(minor(0,j,A));
      sign *= -1;
    }
    return detA;
  }  //  determinant of a square matrix

double fabs(const matrix2&A){
  return sqrt(fabs(det(A)));
}  //  upper left element

  template<class T, int N>
  const matrix<T,N-1,N-1>  minor(int k, int l, const matrix<T,N,N>&m){
    matrix<T,N-1,N-1> minorkl;
    int ii=-1;
    for(int i=0; i<N; i++)
      if(i!=k){
        ii++;
        int jj=-1;
        for(int j=0; j<N; j++)
          if(j!=l)minorkl(ii,++jj,"pur") = m(i,j);
      }
    return minorkl;
  }  //  (k,l)th minor of m

  template<class T, int N, int M>
  const matrix<T,N,M>& matrix<T,N,M>::operator+=(const matrix<T,N,M>&m){
    vector<vector<T,N>,M>::operator+=(m);
    return *this;
  }  //  adding a matrix

  template<class T, int N, int M>
  const matrix<T,N,M>& matrix<T,N,M>::operator-=(const matrix<T,N,M>&m){
    vector<vector<T,N>,M>::operator-=(m);
    return *this;
  }  //  subtracting a matrix

  template<class T, int N, int M>
  const matrix<T,N,M>& matrix<T,N,M>::operator*=(const T&a){
    for(int i=0; i<M; i++)
      set(i,(*this)[i] * a);
    return *this;
  }  //  multiplication by scalar

  template<class T, int N, int M>
  const matrix<T,N,M> operator+(const matrix<T,N,M>&m1, const matrix<T,N,M>&m2){
    return matrix<T,N,M>(m1) += m2;
  }  //  matrix plus matrix

  template<class T, int N, int M>
  const matrix<T,N,M> operator-(const matrix<T,N,M>&m1, const matrix<T,N,M>&m2){
    return matrix<T,N,M>(m1) -= m2;
  }  //  matrix minus matrix

  template<class T, int N, int M> const vector<T,M> 
  operator*(const vector<T,N>&v,const matrix<T,N,M>&m){
    vector<T,M> result;
    for(int i=0; i<M; i++)
      result.set(i, v * m[i]);
    return result;
  }  //  vector times matrix

  template<class T, int N, int M> const vector<T,N> 
  operator*(const matrix<T,N,M>&m,const vector<T,M>&v){
    vector<T,N> result;
    for(int i=0; i<M; i++)
      result += v[i] * m[i];
    return result;
  }  //  matrix times vector

  template<class T, int N, int M, int K> const matrix<T,N,K>
  operator*(const matrix<T,N,M>&m1,const matrix<T,M,K>&m2){
    matrix<T,N,K> result;
    for(int i=0; i<K; i++)
      result.set(i,m1 * m2[i]);
    return result;
  }  //  matrix times matrix

  template<class T, int N, int M, int K> const matrix<T,N,K>&
  operator*=(matrix<T,N,M>&m1,const matrix<T,M,K>&m2){
    return m1 = m1 * m2;
  }  //  matrix times matrix

double real(const matrix2&A){
  return A(0,0);
}  //  upper left element

const point3 operator&(const point3&u, const point3&v){
  point3 i(1.,0.,0.);
  point3 j(0.,1.,0.);
  point3 k(0.,0.,1.);
  return i * (u[1]*v[2]-u[2]*v[1])
       - j * (u[0]*v[2]-u[2]*v[0])
       + k * (u[0]*v[1]-u[1]*v[0]);
}  //  vector product

template<class T, int N>
const matrix<T,N,N>
inverse(const matrix<T,N,N>&A){
  matrix<T,N,N> Ainverse;
  for(int i=0; i<N; i++)
    for(int j=0; j<N; j++)
      Ainverse(i,j,"put") = power(-1,i+j) * det(minor(j,i,A));
  return Ainverse/det(A);
}  //  inverse of matrix3

  const point operator/(const point&v, const matrix2&m){
    return inverse(m) * v;
  }  //  vector divided by matrix2

  const point3 operator/(const point3&v, const matrix3&m){
    return inverse(m) * v;
  }  //  vector divided by matrix2

  const point& operator/=(point&v, const matrix2&m){
    return v = v / m;
  }  //  vector divided by matrix2

  const point3& operator/=(point3&v, const matrix3&m){
    return v = v / m;
  }  //  vector divided by matrix2

  const matrix2 operator/(const matrix2&m1, const matrix2&m2){
    return inverse(m2) * m1;
  }  //  matrix2 divided by matrix2

  const matrix3 operator/(const matrix3&m1, const matrix3&m2){
    return inverse(m2) * m1;
  }  //  matrix3 divided by matrix3

  const matrix2& operator/=(matrix2&m1, const matrix2&m2){
    return m1 = m1 / m2;
  }  //  matrix2 divided by matrix2

  const matrix3& operator/=(matrix3&m1, const matrix3&m2){
    return m1 = m1 / m2;
  }  //  matrix3 divided by matrix3

template<class T, int N, int M>
const matrix<T,N,M> transpose(const matrix<T,M,N>&A){
  matrix<T,N,M> At;
  for(int i=0; i<N; i++)
    for(int j=0; j<M; j++)
      At(i,j,"put") = A(j,i);
  return At;
}  //  transpose of a matrix

template<class T> class node{
    T location;
    int index;
    int sharingCells;
  public:
    node(const T&loc=0., int ind=-1, int sharing=0):
      location(loc),index(ind),sharingCells(sharing){}  //  constructor
    node(const node&n):location(n.location),index(n.index),
      sharingCells(n.sharingCells){}  //  copy constructor
    const node& operator=(const node&);
    ~node(){}  //  destructor
    const T& operator()() const{return location;}  //  read the location
    int getIndex() const{return index;}  //  read index
    void setIndex(int i){index=i;}  //  set index
    int getSharingElements() const{return sharingCells;}  //  read it
    void moreSharingElements(){sharingCells++;}  //  increase it
    int lessSharingElements(){
      return
        sharingCells ?
          !(--sharingCells)
        :
          1;
    }  //  decrease it
    int noSharingElement() const{return !sharingCells;}//dangling node
};

    template<class T>
    const node<T>& node<T>::operator=(const node<T>&n){
      if(this != &n){
	location = n.location;
	index = n.index;
	sharingCells = n.sharingCells;
      }
      return *this;
    }  //  assignment operator

    template<class T>
    void print(const node<T>&n){
      print(n());
      printf("index=%d; %d sharing elements\n",
	  n.getIndex(),n.getSharingElements());
    }  //  print a node

template<class T, int N> class cell{
  node<T>* vertex[N];
  int index[56];
public:
  int readMeshIndex(int i) const{
    return index[i];
  }
  int& meshIndex(int i){
    return index[i];
  }
  cell(){
    for(int i=0; i<N; i++)
      vertex[i] = new node<T>(0.,-1,1);
    for(int i=0; i<56; i++)
      index[i] = -1;
  }  //  default constructor
  cell(node<T>&,node<T>&,node<T>&);
  cell(node<T>&,node<T>&,node<T>&,node<T>&);
  cell(cell<T,N>&);
  const cell<T,N>& operator=(cell<T,N>&);
  ~cell();
  node<T>& operator()(int i){return *(vertex[i]);}//read/write ith vertex
  const node<T>& operator[](int i)const{return *(vertex[i]);}//read only
  void resetIndices(){
    for(int i=0; i<N; i++)
      vertex[i]->setIndex(-1);
  }  //  reset indices to -1
  void indexing(int&count){
    for(int i=0; i<N; i++)
      if(vertex[i]->getIndex()<0)vertex[i]->setIndex(count++);
  }  //  indexing the vertices
};

  template<class T, int N>
  cell<T,N>::cell(node<T>&a, node<T>&b, node<T>&c){
    vertex[0] = a.noSharingElement() ? new node<T>(a) : &a;
    vertex[1] = b.noSharingElement() ? new node<T>(b) : &b;
    vertex[2] = c.noSharingElement() ? new node<T>(c) : &c;
    for(int i=0; i<N; i++)
      vertex[i]->moreSharingElements();
    for(int i=0; i<56; i++)
      index[i] = -1;
  }  //  constructor

  template<class T, int N>
  cell<T,N>::cell(node<T>&a, node<T>&b, node<T>&c, node<T>&d){
    vertex[0] = a.noSharingElement() ? new node<T>(a) : &a;
    vertex[1] = b.noSharingElement() ? new node<T>(b) : &b;
    vertex[2] = c.noSharingElement() ? new node<T>(c) : &c;
    vertex[3] = d.noSharingElement() ? new node<T>(d) : &d;
    for(int i=0; i<N; i++)
      vertex[i]->moreSharingElements();
    for(int i=0; i<56; i++)
      index[i] = -1;
  }  //  constructor

  template<class T, int N>
  cell<T,N>::cell(cell<T,N>&e){
    for(int i=0; i<N; i++){
      vertex[i] = e.vertex[i];
      vertex[i]->moreSharingElements();
    }
    for(int i=0; i<56; i++)
      index[i] = e.index[i];
  }  //  copy constructor

  template<class T, int N> const cell<T,N>&
  cell<T,N>::operator=(cell<T,N>&e){
    if(this != &e){
      for(int i=0; i<N; i++)
        if(vertex[i]->lessSharingElements())delete vertex[i];
      for(int i=0; i<N; i++){
        vertex[i] = e.vertex[i];
        vertex[i]->moreSharingElements();
      }
      for(int i=0; i<56; i++)
        index[i] = e.index[i];
    }
    return *this;
  }  //  assignment operator

  template<class T, int N>
  cell<T,N>::~cell(){
    for(int i=0; i<N; i++)
      if(vertex[i]->lessSharingElements())delete vertex[i];
  }  //   destructor

  template<class T, int N>
  int operator<(const node<T>&n, const cell<T,N>&e){
    for(int i=0; i<N; i++)
      if(&n == &(e[i]))return i+1;
    return 0;
  }  //  check whether a node n is in a cell e

  template<class T, int N>
  void print(const cell<T,N>&e){
    for(int i=0; i<N; i++)
      print(e[i]);
    for(int i=0; i<56; i++)
      print(e.readMeshIndex(i));
  }  //  printing a cell

typedef cell<point,3> triangle;

typedef cell<point3,4> tetrahedron;

template<class T> class dynamicVector{
protected:
  int dimension;
  T* component;
public:
  dynamicVector(int, const T&);
  dynamicVector(const dynamicVector&);
  const dynamicVector& operator=(const dynamicVector&);
  const dynamicVector& operator=(const T&);
  ~dynamicVector(){delete [] component;}  //  destructor
  int dim() const{ return dimension; }  //  return the dimension
  T& operator()(int i){ return component[i]; }  //read/write ith component
  const T& operator[](int i) const{ return component[i]; }  //read only
  const dynamicVector& operator+=(const dynamicVector&);
  const dynamicVector& operator-=(const dynamicVector&);
  template<class S>
  const dynamicVector& operator*=(const S&);
  template<class S>
  const dynamicVector& operator/=(const S&);
};

  template<class T>
  int nonZeros(const dynamicVector<T>&v){
    int count = 0;
    for(int i=0; i<v.dim(); i++)
      if(v[i] != 0)
        count++;
    return count;
  } // number of nonzero components

  template<class T>
  dynamicVector<T>::dynamicVector(int dim=0,const T& a=0) : dimension(dim),
       component(dim ? new T[dim] : 0){
     for(int i = 0; i < dim; i++)
       component[i] = a;
  }  //  constructor

  template<class T>
  dynamicVector<T>::dynamicVector(const dynamicVector<T>& v)
  : dimension(v.dimension), component(v.dimension ? new T[v.dimension] : 0){
    for(int i = 0; i < v.dimension; i++)
      component[i] = v.component[i];
  }  //  copy constructor

  template<class T>
const dynamicVector<T>& dynamicVector<T>::operator=(const dynamicVector<T>& v){
   if(this != &v){
     if(dimension != v.dimension){
       delete [] component;
       component = new T[v.dimension];
     }
     for(int i = 0; i < v.dimension; i++)
       component[i] = v.component[i];
     dimension = v.dimension;
   }
   return *this;
 }  //  assignment operator

  template<class T>
  const dynamicVector<T>& dynamicVector<T>::operator=(const T& a){
    for(int i = 0; i < dimension; i++)
      component[i] = a;
    return *this;
  }  //  assignment operator with a scalar argument

  template<class T>
  const dynamicVector<T>&
  dynamicVector<T>::operator+=( const dynamicVector<T>&v){
      for(int i = 0; i < dimension; i++)
	component[i] += v[i];
      return *this;
  }  //  adding a dynamicVector to the current dynamicVector

  template<class T>
  const dynamicVector<T>&
  dynamicVector<T>::operator-=( const dynamicVector<T>&v){
      for(int i = 0; i < dimension; i++)
	component[i] -= v[i];
      return *this;
  }  //  subtracting a dynamicVector from the current dynamicVector

  template<class T>
  template<class S>
  const dynamicVector<T>& dynamicVector<T>::operator*=(const S& a){
      for(int i = 0; i < dimension; i++)
	component[i] *= a;
      return *this;
  }  //  multiplying the current dynamicVector by a scalar

  template<class T>
  template<class S>
  const dynamicVector<T>& dynamicVector<T>::operator/=(const S& a){
      for(int i = 0; i < dimension; i++)
	component[i] /= a;
      return *this;
  }  //  dividing the current dynamicVector by a scalar

  template<class T>
  const dynamicVector<T>
  operator+(const dynamicVector<T>&u, const dynamicVector<T>&v){
    return dynamicVector<T>(u) += v;
  }  //  dynamicVector plus dynamicVector

  template<class T>
  const dynamicVector<T>
  operator-(const dynamicVector<T>&u, const dynamicVector<T>&v){
    return dynamicVector<T>(u) -= v;
  }  //  dynamicVector minus dynamicVector

  template<class T, class S>
  const dynamicVector<T> operator*(const dynamicVector<T>&u, const S& a){
    return dynamicVector<T>(u) *= a;
  }  //  dynamicVector times scalar

  template<class T>
  const dynamicVector<T> operator*(double a, const dynamicVector<T>&u){
    return dynamicVector<T>(u) *= a;
  }  //  scalar times dynamicVector

  template<class T, class S>
  const dynamicVector<T> operator/(const dynamicVector<T>&u, const S& a){
    return dynamicVector<T>(u) /= a;
  }  //  dynamicVector divided by scalar

  template<class T>
  const dynamicVector<T> operator-(const dynamicVector<T>&u){
    return dynamicVector<T>(u) *= -1.;
  }  //  negative of a dynamicVector

  template<class T>
  double operator*(const dynamicVector<T>&u, const dynamicVector<T>&v){
      double sum = 0;
      for(int i = 0; i < u.dim(); i++)
	sum += u[i] * +v[i];
      return sum;
  }  //  inner product

  template<class T>
  void print(const dynamicVector<T>&v){
    printf("(");
    for(int i = 0;i < v.dim(); i++){
      printf("v[%d]=",i);
      print(v[i]);
    }
    printf(")\n");
  }  //  printing a dynamicVector

template<class T> class linkedList{
protected:
  T item;
  linkedList* next;
public:
    T& operator()(int i){
      return (item.getColumn() == i) ? item :
          next&&(item.getColumn() < i) ? (*next)(i) : *(new T(0.));
    }  //  read/write the item at column i
  const T& operator()() const{return item;}  //  read "item" field
  const linkedList* readNext() const{return next;}  //  read "next" field
  linkedList():next(0){}  //  default constructor
  linkedList(T&t, linkedList* N=0)
    :item(t),next(N){}  //  constructor
  linkedList(const linkedList&l):item(l()),next(
    l.next ? new linkedList(*l.next) : 0){}  //  copy constructor
  ~linkedList(){ 
    delete next;
    next = 0;
  }  //  destructor
  const linkedList& operator=(const linkedList&);
  linkedList& last(){return next ? next->last() : *this;}  //  last item
  int length() const{return next ? next->length() + 1 : 1;}//no. of items
  void append(T&t){last().next = new linkedList(t);}//append an item
  void insertNextItem(T&t){next = new linkedList(t,next);}//insert an item
  void insertFirstItem(T&t){
    next = new linkedList(item,next);
    item = t;
  }  //  insert an item at the beginning
  void dropNextItem();
  void dropFirstItem();
  template<class S>
    const S truncateItems(double, const S&);
  template<class S>
    const S dropPositiveItems(int, const S&, double);
  template<class S>
    const S maskItems(const dynamicVector<int>&, const S&);
  template<class S>
  const S maskItemsSum(const dynamicVector<int>&, const dynamicVector<S>&);
  const linkedList& operator+=(linkedList&);
  linkedList& order(int);
};

  template<class T>
  const linkedList<T>&linkedList<T>::operator=(const linkedList<T>&L){
   if(this != &L){
     item = L();
     if(next){
        if(L.next)
          *next = *L.next;
        else{
          delete next;
	  next = 0;
        }
      }
      else
        if(L.next)next = new linkedList(*L.next);
   }
   return *this;
  }  //  assignment operator

  template<class T>
  void linkedList<T>::dropNextItem(){
     if(next){
       if(next->next){
         linkedList<T>* keep = next;
         next = next->next;
         keep->item.~T();
       }
       else{
         delete next;
         next = 0;
       }
     }  //  drop the second item from the linked list
     else
       printf("error: cannot drop next element\n");
  }

  template<class T>
  void linkedList<T>::dropFirstItem(){
      if(next){
	item = next->item;
	dropNextItem();
      }
      else
        printf("error: cannot drop first element\n");
  }  //  drop the first item in the linked list

  template<class T>
  template<class S>
  const S linkedList<T>::truncateItems(double threshold, const S& compare){
    S sum=0.;
    if(next){
      int dropped = 0;
      for(linkedList<T>* iterator = this;
          iterator->next;
	  iterator = dropped ? iterator : iterator->next){
	dropped = 0;
        if(fabs(iterator->next->item.getValue()) <= threshold * fabs(compare)){
	  sum += iterator->next->item.getValue();
	  iterator->dropNextItem();
	  dropped = 1;
	}
      }
    }
    if(next&&(fabs(item.getValue()) <= threshold * fabs(compare))){
      sum += item.getValue();
      dropFirstItem();
    }
    return sum;
  }  //  truncate small items

  template<class T>
  template<class S>
  const S linkedList<T>::dropPositiveItems(
          int diag, const S&center, double threshold){
    S sum=0.;
    if(next){
      int dropped = 0;
      for(linkedList<T>* iterator = this;
          iterator->next;
	  iterator = dropped ? iterator : iterator->next){
	dropped = 0;
        if((real(iterator->next->item.getValue()) / real(center) >= -threshold)&&
	    (iterator->next->item.getColumn() != diag)){
	  sum += iterator->next->item.getValue();
	  iterator->dropNextItem();
	  dropped = 1;
	}
      }
    }
    if(next&&(real(item.getValue()) / real(center) >= -threshold)&&(item.getColumn() != diag)){
      sum += item.getValue();
      dropFirstItem();
    }
    return sum;
  }  //  truncate positive off-diagonal items

  template<class T>
  template<class S>
  const S linkedList<T>::maskItems(const dynamicVector<int>& mask, const S&){
    S sum=0.;
    if(next){
      int dropped = 0;
      for(linkedList<T>* iterator = this;
          iterator->next;
	  iterator = dropped ? iterator : iterator->next){
	dropped = 0;
        if(!mask[iterator->next->item.getColumn()]){
	  sum += iterator->next->item.getValue();
	  iterator->dropNextItem();
	  dropped = 1;
	}
      }
    }
    if(next&&(!mask[item.getColumn()])){
      sum += item.getValue();
      dropFirstItem();
    }
    return sum;
  }  //  truncate masked items

  template<class T>
  template<class S>
  const S linkedList<T>::maskItemsSum(const dynamicVector<int>& mask, const dynamicVector<S>&f){
    S sum=0.;
    if(next){
      int dropped = 0;
      for(linkedList<T>* iterator = this;
          iterator->next;
	  iterator = dropped ? iterator : iterator->next){
	dropped = 0;
        if(!mask[iterator->next->item.getColumn()]){
	  sum += iterator->next->item.getValue() * f[iterator->next->item.getColumn()];
	  iterator->dropNextItem();
	  dropped = 1;
	}
      }
    }
    if(next&&(!mask[item.getColumn()])){
      sum += item.getValue() * f[item.getColumn()];
      dropFirstItem();
    }
    return sum;
  }  //  truncate masked items

  template<class T> const linkedList<T>&
  linkedList<T>::operator+=(linkedList<T>&L){
    linkedList<T>* runner = this;
    linkedList<T>* Lrunner = &L;
    if(L.item < item){
      insertFirstItem(L.item);
      Lrunner = L.next;
    }
    for(; runner->next; runner=runner->next){
      if(Lrunner&&(Lrunner->item == runner->item)){
	runner->item += Lrunner->item;
	Lrunner = Lrunner->next;
      }
      for(; Lrunner&&(Lrunner->item < runner->next->item);
	      Lrunner = Lrunner->next){
	runner->insertNextItem(Lrunner->item);
	runner = runner->next;
      }
    }
    if(Lrunner&&(Lrunner->item == runner->item)){
      runner->item += Lrunner->item;
      Lrunner = Lrunner->next;
    }
    if(Lrunner)runner->next = new linkedList<T>(*Lrunner);
    return *this;
  }  //  merge two linked lists

  template<class T>
  linkedList<T>& linkedList<T>::order(int length){
    if(length>1){
      linkedList<T>* runner = this;
      for(int i=0; i<length/2-1; i++)
	runner = runner->next;
      linkedList<T>* second = runner->next;
      runner->next = 0;
      order(length/2);
      *this += second->order(length-length/2);
    }
    return *this;
  }  //  order a disordered linked list

template<class T>
void print(const linkedList<T>&l){
  printf("item:\n");
  print(l());
  if(l.readNext())print(*l.readNext());
}  //  print a linked list

template<class T> class mesh:public linkedList<T>{
  public:
    mesh(){}  //  default constructor
    mesh(T&e){item = e;}  //  constructor
    int indexing();
    int edgeIndexing(int);
    int sideIndexing(int, int);
    void refineNeighbors(node<point3>&, node<point3>&, node<point3>&);
    void refine(const dynamicVector<point>&, const dynamicVector<int>&, double);
    double error(const dynamicVector<point>&, int);
    double jumpSum(const dynamicVector<point>&);
    double jumpMax(const dynamicVector<point>&);
};

    double mesh<tetrahedron>::jumpSum(const dynamicVector<point>&v){
      double sum = 0.;
      int counter = 0;
      for(mesh<tetrahedron>* runner = this; runner; runner=(mesh<tetrahedron>*)runner->next)
        for(int i=0; i<4; i++)
	  for(int j=i+1; j<4; j++){
	    sum += l2norm(v[runner->item[i].getIndex() * 10] - v[runner->item[j].getIndex() * 10]);
	    counter++;
	  }
      return sum / counter;
    }

    double mesh<tetrahedron>::jumpMax(const dynamicVector<point>&v){
      double maximum = 0.;
      for(mesh<tetrahedron>* runner = this; runner; runner=(mesh<tetrahedron>*)runner->next)
        for(int i=0; i<4; i++)
	  for(int j=i+1; j<4; j++)
	    maximum = max(maximum,l2norm(v[runner->item[i].getIndex() * 10] - v[runner->item[j].getIndex() * 10]));
      return maximum;
    }

    double mesh<tetrahedron>::error(const dynamicVector<point>&v, int nodes){
      dynamicVector<int> done(nodes,0);
      double sum = 0.;
      for(const mesh<tetrahedron>* runner = this; runner;
	    runner=(const mesh<tetrahedron>*)runner->readNext())
        for(int i=0; i<4; i++){
          int I = (*runner)()[i].getIndex();
	  if(!done[I]){
	    printf("I=%d  ",I);
	    print(v[10 * I]);
	    point Ierror = v[10 * I] -
	      point(cos(sqrt(fabs(HELM))*(*runner)()[i]()[0]),-sin(sqrt(fabs(HELM))*(*runner)()[i]()[0]));
	    sum += Ierror*Ierror;
	    done(I) = 1;
	  }
	}
      return sqrt(sum/nodes);
    }

template<class T>
int mesh<T>::edgeIndexing(int nodes){
  int edges = 0;
  for(mesh<T>* runner = this; runner; runner=(mesh<T>*)runner->next){
    for(int i=0; i<10; i++){
      runner->item.meshIndex(i) =
          10 * runner->item[0].getIndex() + i;
      runner->item.meshIndex(10+i) =
          10 * runner->item[1].getIndex() + i;
      runner->item.meshIndex(20+i) =
          10 * runner->item[2].getIndex() + i;
      runner->item.meshIndex(30+i) =
          10 * runner->item[3].getIndex() + i;
    }
    int I = -1;
    for(int i=0; i<3; i++)
      for(int j=i+1; j<=3; j++){
        I++;
        for(mesh<T>* previous = this;
	    previous&&(previous != runner)
	        &&(runner->item.readMeshIndex(40+2*I) < 0);
	    previous=(mesh<T>*)previous->next){
	  int ni = runner->item[i] < previous->item;
	  int nj = runner->item[j] < previous->item;
	  if(ni&&nj){
	    ni--;
	    nj--;
	    int J = min(ni,nj) == 0 ?
	              max(ni,nj) - 1
		    :
		      min(ni,nj) == 1 ?
		        max(ni,nj) + 1
		      :
		        5;
	    runner->item.meshIndex(40+2*I)
	        = previous->item.readMeshIndex(40+2*J);
	    runner->item.meshIndex(40+2*I+1)
	        = previous->item.readMeshIndex(40+2*J+1);
	  }
	}
	if(runner->item.readMeshIndex(40+2*I) < 0){
	  runner->item.meshIndex(40+2*I) =
	        10 * nodes + 2 * edges;
	  runner->item.meshIndex(40+2*I+1) =
	        10 * nodes + 2 * edges++ + 1;
	}
      }
  }
  return edges;
}  //  indexing the edges in the mesh

template<class T>
int mesh<T>::sideIndexing(int nodes, int edges){
  int sides = 0;
  for(mesh<T>* runner = this; runner; runner=(mesh<T>*)runner->next){
    int I = -1;
    for(int i=0; i<=1; i++)
      for(int j=i+1; j<=2; j++)
        for(int k=j+1; k<=3; k++){
	  I++;
          for(mesh<T>* previous = this;
	      previous&&(previous != runner)
	        &&(runner->item.readMeshIndex(40+2*6+I) < 0);
	    previous=(mesh<T>*)previous->next){
	    int ni = runner->item[i] < previous->item;
	    int nj = runner->item[j] < previous->item;
	    int nk = runner->item[k] < previous->item;
	    if(ni&&nj&&nk){
	      ni--;
	      nj--;
	      nk--;
	      int nl = 0;
	      while((nl==ni)||(nl==nj)||(nl==nk))
	        nl++;
	      int J = 3 - nl;
	      runner->item.meshIndex(40+2*6+I)
	          = previous->item.readMeshIndex(40+2*6+J);
	    }
	  }
	  if(runner->item.readMeshIndex(40+2*6+I) < 0)
	    runner->item.meshIndex(40+2*6+I) =
	        10 * nodes + 2 * edges + sides++;
        }
  }
  return sides;
}  //  indexing the sides in the mesh

template<class T>
int mesh<T>::indexing(){
  for(mesh<T>* runner = this; runner; runner=(mesh<T>*)runner->next)
    runner->item.resetIndices();
  int count=0;
  for(mesh<T>* runner = this; runner; runner=(mesh<T>*)runner->next)
    runner->item.indexing(count);
  return count;
}  //  indexing mesh

    void mesh<tetrahedron>::refineNeighbors(node<point3>&nI,
	    node<point3>&nJ, node<point3>&nIJ){
      if(next)
        ((mesh<tetrahedron>*)next)->refineNeighbors(nI,nJ,nIJ);
      int ni = nI < item;
      int nj = nJ < item;
      if(ni&&nj){
	ni--;
	nj--;
	int nk = 0;
	while((nk==ni)||(nk==nj))
	  nk++;
	int nl = 0;
	while((nl==ni)||(nl==nj)||(nl==nk))
	  nl++;
	tetrahedron t1(nI,nIJ,item(nk),item(nl));
	tetrahedron t2(nJ,nIJ,item(nk),item(nl));
	insertNextItem(t2);
	insertNextItem(t1);
	dropFirstItem();
      }
    }  //  refine also the neighbors of a refined tetrahedron

    void mesh<tetrahedron>::refine(const dynamicVector<point>&v,
	    const dynamicVector<int>&DirichletBoundary, double threshold){
        for(int i=0; i<4; i++)
	  for(int j=3; j>i; j--)
	    if((item[i].getIndex() >= 0)&&(item[j].getIndex() >= 0)&&
	     (l2norm(v[item[i].getIndex() * 10] - v[item[j].getIndex() * 10])>threshold)){
	      node<point3> itemij = (item[i]()+item[j]())/2.;
	      int k=0;
	      while((k==i)||(k==j))
	        k++;
	      int l=0;
	      while((l==i)||(l==j)||(l==k))
	        l++;
	      tetrahedron t1(item(i),itemij,item(k),item(l));
	      tetrahedron t2(item(j),t1(1),item(k),item(l));
	      if(next)
	        ((mesh<tetrahedron>*)next)->
	                refineNeighbors(item(i),item(j),t1(1));
	      insertNextItem(t2);
	      insertNextItem(t1);
	      dropFirstItem();
	      refine(v,DirichletBoundary, threshold);
	      return;
	    }
	if(next)((mesh<tetrahedron>*)next)->refine(v,DirichletBoundary, threshold);
    }  //  adaptive refinement

template<class T> class rowElement{
  T value;
  int column;
public:
  rowElement(const T& val=0, int col=-1):value(val),column(col){}//constructor
  rowElement(const rowElement&e):value(e.value),column(e.column){}//copy con.
  const rowElement& operator=(const rowElement&e){
    if(this != &e){
      value = e.value;
      column = e.column;
    }
    return *this;
  }  //  assignment operator
  ~rowElement(){}  //  destructor
  const T& getValue() const{return value;}  //  read the value
  int getColumn() const{return column;}  //  return the column
  const rowElement& operator+=(const T&t){
    value += t;
    return *this;
  }  //  adding a T
  const rowElement& operator+=(const rowElement<T>&e){
    value += e.value;
    return *this;
  }  //  adding a rowElement
  const rowElement& operator-=(const T&t){
    value -= t;
    return *this;
  }  //  subtracting a T
  const rowElement& operator-=(const rowElement<T>&e){
    value -= e.value;
    return *this;
  }  //  subtracting a rowElement
  template<class S>
  const rowElement& operator*=(const S&t){
    value *= t;
    return *this;
  }  //  multiplying by a T
  const rowElement& operator/=(const T&t){
    value /= t;
    return *this;
  }  //  dividing by a T
};

  template<class T>
  int operator<(const rowElement<T>&e, const rowElement<T>&f){
    return e.getColumn() < f.getColumn();
  }  //   smaller column index

  template<class T>
  int operator>(const rowElement<T>&e, const rowElement<T>&f){
    return e.getColumn() > f.getColumn();
  }  //   greater column index

  template<class T>
  int operator==(const rowElement<T>&e, const rowElement<T>&f){
    return e.getColumn() == f.getColumn();
  }  //   same column

  template<class T>
  const rowElement<T> operator+(const rowElement<T>&e, const T&t){
    return rowElement<T>(e) += t;
  }  //   rowElement plus a T

  template<class T>
  const rowElement<T> operator+(const T&t, const rowElement<T>&e){
    return rowElement<T>(e) += t;
  }  //   T plus rowElement

  template<class T>
  const rowElement<T> operator-(const rowElement<T>&e, const T&t){
    return rowElement<T>(e) -= t;
  }  //   rowElement minusa T

  template<class T, class S>
  const rowElement<T> operator*(const rowElement<T>&e, const S&t){
    return rowElement<T>(e) *= t;
  }  //   rowElement times a T

  template<class T, class S>
  const rowElement<T> operator*(const S&t, const rowElement<T>&e){
    return rowElement<T>(e) *= t;
  }  //   T times rowElement

  template<class T>
  const rowElement<T> operator/(const rowElement<T>&e, const T&t){
    return rowElement<T>(e) /= t;
  }  //   rowElement divided by a T

  template<class T>
  void print(const rowElement<T>&e){
    print(e.getValue());
    printf("column=%d\n",e.getColumn());
  }  //  print a rowElement object

  void print(int i){
    printf("%d;\n",i);
  }  //  print an integer variable

  void print(double d){
    printf("%f; ",d);
  }  //  print a double variable

template<class T> class row : public linkedList<rowElement<T> >{
  public:
    row(const T&val=0,int col=-1){item=rowElement<T>(val,col);}//constructor
    ~row(){}  //  destructor
    void append(const T&val, int col){
      rowElement<T> e(val,col);
      linkedList<rowElement<T> >::append(e);
    }  //  append a rowElement at the end of row
    const rowElement<T>& operator()() const{return item;}//read first item
    const T& getValue() const{ return item.getValue(); }//first-item value
    int getColumn() const{ return item.getColumn(); }//first-item column
    const T rowSum() const{
      return next ? getValue() + (*(row<T>*)next).rowSum() : getValue();
    }  //  row sum
    rowElement<T>& operator()(int i){
      return (linkedList<rowElement<T> >::operator())(i);
    }  //  read the rowElement at column i
    const T operator[](int i) const{
      return (getColumn() == i) ? getValue() :
          next&&(getColumn() < i) ? (*(row*)next)[i] : 0;
    }  //  read the rowElement at column i
    const T rowSumCoarse(const row<T>&r,const dynamicVector<int>&coarse)const{
      T contribution = coarse[getColumn()] ? r[getColumn()] : 0.;
      return next ? contribution +
	     ((row<T>*)next)->rowSumCoarse(r,coarse) : contribution;
    }  //  row sum at coarse points
    void addCoarse(const row<T>&r,const dynamicVector<int>&coarse){
      if(coarse[getColumn()])
        item += r[getColumn()];
      if(next) ((row<T>*)next)->addCoarse(r,coarse);
    }  //  add values at coarse points
    void insertNextItem(const T&val, int col){
      rowElement<T> e(val,col);
      linkedList<rowElement<T> >::insertNextItem(e);
    }  //  insert a rowElement
    void insertFirstItem(const T&val, int col){
      rowElement<T> e(val,col);
      linkedList<rowElement<T> >::insertFirstItem(e);
    }  //  insert a rowElement at the beginning
    template<class S>
    const row& operator*=(const S&t){
      item *= t;
      if(next) *(row*)next *= t;
      return *this;
    }  //  multiply by a T
    const row& operator/=(const T&t){
      item /= t;
      if(next) *(row*)next /= t;
      return *this;
    }  //  multiply by a T
    template<class S>
    const S operator*(const dynamicVector<S>&v) const{
      return next ? getValue() * v[getColumn()] + *(row*)next * v
	    : getValue() * v[getColumn()];
    }  //  row times vector (inner product)
    void renumberColumns(const dynamicVector<int>&renumber){
      item = rowElement<T>(getValue(),renumber[getColumn()]-1);
      if(next)(*(row<T>*)next).renumberColumns(renumber);
    }  //  renumber columns
  template<class S>
  const S maskTail(const dynamicVector<int>&,
      const dynamicVector<S>&);
  template<class S>
  const S maskAll(const dynamicVector<int>&,
      const dynamicVector<S>&);
};


  template<class T>
  template<class S>
  const S row<T>::maskTail(
      const dynamicVector<int>& mask,
      const dynamicVector<S>&f){
    S sum=0.;
    if(next){
      if(!mask[(*next)().getColumn()]){

	sum = (*next)().getValue() * 
	       f[(*next)().getColumn()];
        dropNextItem();
        sum += maskTail(mask,f);
      }
      else
        sum = (*(row<T>*)next).maskTail(mask,f);
    }
    return sum;
  }  //  drop certain items

  template<class T>
  template<class S>
  const S row<T>::maskAll(
      const dynamicVector<int>& mask,
      const dynamicVector<S>&f){
    S sum = maskTail(mask,f);
    if(next&&!mask[getColumn()]){
      sum += getValue() * f[getColumn()];
      dropFirstItem();
    }
    return sum;
  }

template<class T>
const row<T> operator*(const row<T>&r, const T&t){
  return row<T>(r) *= t;
}  //  row times T

template<class T>
const row<T> operator*(const T&t, const row<T>&r){
  return row<T>(r) *= t;
}  //  T times row

template<class T>
const row<T> operator/(const row<T>&r, const T&t){
  return row<T>(r) /= t;
}  //  row divided by a T

template<class T> class list{
protected:
  int number;
  T** item;
public:
  list(int n=0):number(n), item(n ? new T*[n] : 0){}  //constructor
  list(int n, const T&t)
    : number(n), item(n ? new T*[n] : 0){
    for(int i=0; i<number; i++)
      item[i] = new T(t);
  }  //constructor
  list(const list<T>&);
  list(int, int, const list<T>&);
  const list<T>& operator=(const list<T>&);
  const list<T>& replace(int, const list<T>&);
  ~list(){
    for(int i=0; i<number; i++)
      delete item[i];
    delete [] item;
  }  //   destructor
  int size() const{ return number; }  //  list size
  T& operator()(int i){ if(item[i])return *(item[i]); }  //read/write ith item
  const T& operator[](int i)const{if(item[i])return *(item[i]);}// read only
};

  template<class S>
  const S
  operator*(const list<S>&l1, const list<S>&l2){
    S sum = l1[0] * l2[0];
    for(int i=1; i<l1.size(); i++)
      sum += l1[i] * l2[i];
    return sum;
  }  //  inner product of lists

  template<class T, class S, int N>
  const list<S>
  operator*(const matrix<T,N,N>&M,const list<S>&l){
    list<S> result(l);
    for(int i=0; i<N; i++){
      result(i) = M(i,0) * l[0];
      for(int j=1; j<N; j++)
        result(i) += M(i,j) * l[j];
    }
    return result;
  }  //  matrix times list

  template<class T>
  list<T>::list(const list<T>&l):number(l.number),
        item(l.number ? new T*[l.number] : 0){
    for(int i=0; i<l.number; i++)
      if(l.item[i]) item[i] = new T(*l.item[i]);
  }  //  copy constructor

  template<class T>
  list<T>::list(int start, int length,
        const list<T>&l):number(length),
        item((length>0) ? new T*[length] : 0){
    for(int i=0; i<length; i++)
      if(l.item[start+i]) item[i] = new T(*l.item[start+i]);
  }  //  constructor

template<class T>
const list<T>& list<T>::operator=(const list<T>& l){
   if(this != &l){
     if(number != l.number){
       delete [] item;
       item = new T*[l.number];
     }
     for(int i = 0; i < l.number; i++)
       if(l.item[i]) item[i] = new T(*l.item[i]);
     number = l.number;
   }
   return *this;
 }  //  assignment operator

template<class T>
const list<T>& list<T>::replace(int start, const list<T>&l){
   for(int i = 0; i < l.number; i++)
     *item[start+i] = *l.item[i];
   return *this;
 }  //  replacing items

  template<class T>
  void print(const list<T>&l){
    for(int i=0; i<l.size(); i++){
      printf("i=%d:\n",i);
      print(l[i]);
    }
  }  //  printing a list

template<class T> class polynomial:public list<T>{
 public:
  polynomial(int n=0){
    number = n;
    item = n ? new T*[n] : 0;
    for(int i=0; i<n; i++)
      item[i] = 0;
  }  //  constructor
  polynomial(int n, const T&a){
    number = n;
    item = n ? new T*[n] : 0;
    for(int i=0; i<n; i++)
      item[i] = new T(a);
  }  //  constructor with 'T' argument
  polynomial(const T&a, const T&b){
    number = 2;
    item = new T*[2];
    item[0] = new T(a);
    item[1] = new T(b);
  }  //  constructor with 'T' argument
  ~polynomial(){}
  int degree() const{return number-1;}
  T& operator()(int i){
    return list<T>::operator()(i);
  }
  template<class S>
    const T HornerArray(T** const&, int, const S&) const;
  template<class S>
    const T operator()(const S&, char*) const;
  template<class S>
    const T operator()(const S&, char*, char*) const;
  template<class S>
    const T operator()(const S&) const;
  template<class S>
    const S operator()(const S&, const S&) const;
  template<class S>
    const S operator()(const S&, const S&, const S&) const;
};

  template<class T>
  template<class S>
  const T
  polynomial<T>::operator()(const S&x,char*) const{
    T result(size(),(*this)[0](x));
    for(int i=1; i<size(); i++)
      result(i) = (*this)[i](x);
    return result;
  }  //  determine x-value in a 2-d polynomial or y-value in a 3-d polynomial

  template<class T>
  template<class S>
  const T
  polynomial<T>::operator()(const S&x,char*, char*) const{
    T result(size(),(*this)[0](x,"x"));
    for(int i=1; i<size(); i++)
      result(i) = (*this)[i](x,"x");
    return result;
  }  //  determine x-value in a 3-d polynomial

  template<class T>
  const polynomial<polynomial<polynomial<T> > >
  producePolynomial(const dynamicVector<T>&x){
    polynomial<T> zero(1,0.);
    polynomial<polynomial<T> > Zero(1,zero);
    polynomial<polynomial<polynomial<T> > > p(6,Zero);
    int J = -1;
    for(int i=0; i<=5; i++){
      polynomial<polynomial<T> > A(6-i,zero);
      for(int j=0; j<=5-i; j++){
        polynomial<T> a(6-i-j,0.);
        for(int k=0; k<=5-i-j; k++){
	  J++;
	  a(k) = x[J];
	}
	A(j) = a;
      }
      p(i) = A;
    }
    return p;
  }  //  produce a polynomial of degree 5 from x

template<class T>
const polynomial<T>& operator+=(polynomial<T>& p, const polynomial<T>&q){
  if(p.degree() >= q.degree())
    for(int i=0; i<=q.degree(); i++)
      p(i) += q[i];
  else{
    polynomial<T> keepQ = q;
    p = keepQ += p;
  }
  return p;
}  //  add polynomial

template<class T>
const polynomial<T> operator+(const polynomial<T>& p, const polynomial<T>&q){
  polynomial<T> keep = p;
  return keep += q;
}  //  add two polynomials

template<class T, class S>
const polynomial<T>& operator*=(polynomial<T>& p, const S&a){
  for(int i=0; i<=p.degree(); i++)
      p(i) *= a;
  return p;
}  //  multiplication by scalar

template<class T, class S>
const polynomial<T> operator*(const S&a, const polynomial<T>&p){
  polynomial<T> keep = p;
  return keep *= a;
}  //  scalar times polynomial

template<class T, class S>
const polynomial<T> operator*(const polynomial<T>&p, const S&a){
  polynomial<T> keep = p;
  return keep *= a;
}  //  polynomial times scalar

  template<class T>
  polynomial<T> 
  operator*(const polynomial<T>&p, const polynomial<T>&q){
    polynomial<T> result(p.degree()+q.degree()+1,0);
    for(int i=0; i<=result.degree(); i++)
      for(int j=max(0,i-q.degree());
          j<=min(i,p.degree()); j++){
	if(j == max(0,i-q.degree()))
          result(i) = p[j] * q[i-j];
	else
          result(i) += p[j] * q[i-j];
      }
    return result;
  }  //  multiply 2 polynomials

  template<class T>
  polynomial<T>&
  operator*=(polynomial<T>&p, const polynomial<T>&q){
    return p = p * q;
  }  //  multiply by polynomial

  template<class T>
  const T
  calculatePolynomial(const polynomial<T>&p, const T&x){
    T powerOfX = 1;
    T sum=0;
    for(int i=0; i<=p.degree(); i++){
      sum += p[i] * powerOfX;
      powerOfX *= x;
    }
    return sum;
  }  //  calculate a polynomial

  template<class T>
  const T
  HornerPolynomial(const polynomial<T>&p, const T&x){
    T result = p[p.degree()];
    for(int i=p.degree(); i>0; i--){
      result *= x;
      result += p[i-1];
    }
    return result;
  }  //  Horner algorithm to calculate a polynomial

  template<class T>
  const polynomial<T>
  operator&(const polynomial<T>&p, const polynomial<T>&q){
    polynomial<T> result(1,p[p.degree()]);
    for(int i=p.degree(); i>0; i--){
      result *= q;
      result += polynomial<T>(1,p[i-1]);
    }
    return result;
  }  //  Horner algorithm to composition of p and q

  template<class T>
  template<class S>
  const T
  polynomial<T>::HornerArray(T** const&p, int n, const S&x) const{
    return n==0 ? *p[0] :
        *p[0] + x * HornerArray(p+1,n-1,x);
  }  //  Horner algorithm for array p

  template<class T>
  template<class S>
  const T
  polynomial<T>::operator()(const S&x) const{
    return HornerArray(item,degree(),x);
  }  //  Horner algorithm to calculate a polynomial

  template<class T>
  template<class S>
  const S
  polynomial<T>::operator()(const S&x, const S&y) const{
    return (*this)(y)(x);
  }  //  compute p(x,y)

  template<class T>
  template<class S>
  const S
  polynomial<T>::operator()(const S&x, const S&y, const S&z) const{
    return (*this)(z)(y)(x);
  }  //  compute p(x,y,z)


  template<class T>
  const polynomial<T>
  indefiniteIntegral(const polynomial<T>&p){
    polynomial<T> result(p.degree()+2,0);
    for(int i=0; i<=p.degree(); i++)
      result(i+1) = (1./(i+1)) * p[i];
    return result;
  }  //  indefinite integral

  template<class T>
  const T
  integral(const polynomial<T>&p){
    return indefiniteIntegral(p)(1.);
  }  //  integral on the unit interval

  template<class T>
  const T
  integral(const polynomial<polynomial<T> >&p){
    polynomial<T> oneMinusx(1.,-1);
    return integral(indefiniteIntegral(p)(oneMinusx));
  }  //  integral on the truangle

  template<class T>
  const T
  integral(const polynomial<polynomial<polynomial<T> > >&p){
    polynomial<T> minus1(1,-1.);
    polynomial<T> oneMinusx(1.,-1.);
    polynomial<polynomial<T> > oneMinusxMinusy(oneMinusx,minus1);
    return integral(indefiniteIntegral(p)(oneMinusxMinusy));
  }  //  integral on the tetrahedron

  template<class T>
  const polynomial<T>
  d(const polynomial<T>&p, int k){
    if(k>p.degree())
      return polynomial<T>(1,0.);
    polynomial<T> dp(p.degree()+1-k,0.);
    for(int n=0; n<=dp.degree(); n++)
      dp(n) = factorial(n+k) / factorial(n) * p[n+k];
    return dp;
  } //  kth derivative

  template<class T>
  const polynomial<polynomial<T> >
  d(const polynomial<polynomial<T> >&p, int j, int k){
    polynomial<T> zero(1,0.);
    if(k>p.degree())
      return polynomial<polynomial<T> >(1,zero);
    polynomial<polynomial<T> > dp(p.degree()+1-k,zero);
    for(int n=0; n<=dp.degree(); n++)
      dp(n) = factorial(n+k) / factorial(n) * d(p[n+k],j);
    return dp;
  } //  (j,k)th partial derivative

  template<class T>
  const polynomial<polynomial<polynomial<T> > >
  d(const polynomial<polynomial<polynomial<T> > >&p, int i, int j, int k){
    polynomial<T> zero(1,0.);
    polynomial<polynomial<T> > Zero(1,zero);
    if(k>p.degree())
      return polynomial<polynomial<polynomial<T> > >(1,Zero);
    polynomial<polynomial<polynomial<T> > > dp(p.degree()+1-k,Zero);
    for(int n=0; n<=dp.degree(); n++)
      dp(n) = factorial(n+k) / factorial(n) * d(p[n+k],i,j);
    return dp;
  } //  (i,j,k)th partial derivative

template<class T> class sparseMatrix:public list<row<T> >{
 public:
  sparseMatrix(int n=0){
    number = n;
    item = n ? new row<T>*[n] : 0;
    for(int i=0; i<n; i++)
      item[i] = 0;
  }  //  constructor
  sparseMatrix(const list<T>&l, int start, int length){
    number = indexI(length);
    item = number ? new row<T>*[number] : 0;
    for(int i=0; i<number; i++){
      item[i] = new row<T>(l[start+indexK(i,0)],0);
      for(int j=1; j<number; j++)
        item[i]->append(l[start+indexK(i,j)],j);
    }
  }  //  constructor from a list
  sparseMatrix(int n, const T&a){
    number = n;
    item = n ? new row<T>*[n] : 0;
    for(int i=0; i<n; i++)
      item[i] = new row<T>(a,i);
  }  //  constructor with 'T' argument
  sparseMatrix(char*);
  sparseMatrix(int,
      const list<polynomial<polynomial<polynomial<double> > > >&,
  mesh<tetrahedron>&, const dynamicVector<point>&, dynamicVector<point>&,dynamicVector<int>&);
  template<class S>
  void setDirichlet(dynamicVector<S>&,dynamicVector<int>&);

  void readSparseMatrix(int);
  ~sparseMatrix(){}  //  destructor
  rowElement<T>& operator()(int i,int j, char*){
    return (*item[i])(j);
  }  //  (i,j)the element (read/write)
  const T operator()(int i,int j) const{
    return (*item[i])[j];
  }  //  (i,j)the element (read only)
  int rowNumber() const{ return number; }  //  number of rows
  int columnNumber() const{
    int maxColumn = -1;
    for(int i=0; i<rowNumber(); i++)
      if(item[i])maxColumn = max(maxColumn, item[i]->last()().getColumn());
      return maxColumn + 1;
  }  //  number of columns
  const sparseMatrix& operator/=(const sparseMatrix<T>&M){
    for(int i=0; i<rowNumber(); i++)
      *item[i] /= M(i,i);
    return *this;
  }  //  divide by diag(M)
  const sparseMatrix& operator+=(const sparseMatrix<T>&M){
    for(int i=0; i<rowNumber(); i++)
      *item[i] += *M.item[i];
    return *this;
  }  //  add a sparse matrix
  const sparseMatrix& operator-=(const sparseMatrix<T>&M){
    for(int i=0; i<rowNumber(); i++){
      row<T> minus = T(-1.) * *M.item[i];
      *item[i] += minus;
    }
    return *this;
  }  //  subtract a sparse matrix
  int order() const{return max(rowNumber(), columnNumber());}//matrix order
  const sparseMatrix<T>& operator*=(const T&t){
    for(int i=0; i<rowNumber(); i++)
      *item[i] *= t;
    return *this;
  }  //  multip;y by T
  friend const sparseMatrix<T>
	  truncate<T>(const sparseMatrix<T>&, double);
  friend const sparseMatrix<T>
	  operator*<T>(const sparseMatrix<T>&, const sparseMatrix<T>&);
  friend const sparseMatrix<T>
	  operator*<T>(const matrix3&, const sparseMatrix<T>&);
  friend const sparseMatrix<T> diagonal<T>(const sparseMatrix<T>&);
  friend const sparseMatrix<T> transpose<T>(const sparseMatrix<T>&);
  friend void GaussSeidel<T>(const sparseMatrix<T>&,
	  const dynamicVector<point>&, dynamicVector<point>&);
  friend void reversedGaussSeidel<T>(const sparseMatrix<T>&,
	  const dynamicVector<point>&, dynamicVector<point>&);
  friend void symmetricGaussSeidel<T>(const sparseMatrix<T>&,
	  const dynamicVector<point>&, dynamicVector<point>&);
  const sparseMatrix factorize(double);
  template<class S>
  const dynamicVector<S> forwardElimination(const dynamicVector<S>&)const;
  template<class S>
  const dynamicVector<S> backSubstitution(const dynamicVector<S>&)const;
  const sparseMatrix<T> createTransfer();
  const dynamicVector<int> coarsen(double) const;
  const dynamicVector<int> coarsen(const dynamicVector<int>&, double) const;
  void truncatePositive(double);
};

  template<class T>
  const list<T>
  produceList(const sparseMatrix<T>&M){
    list<T> l(1+indexK(M.rowNumber()-1,M.rowNumber()-1),M(0,0));
    int k=0;
    for(int i=0; i<M.rowNumber(); i++)
      for(int j=0; j<=i; j++)
        l(k++) = M(i,j);
    return l;
  }  //  produce a list from a square sparse matrix

  template<class T>
  const vector<T,6>
  produceVector(const matrix<T,3,3>&M){
    vector<T,6> v(9.);
    int k=0;
    for(int i=0; i<3; i++)
      for(int j=0; j<=i; j++)
        v(k++) = M(i,j);
    return v;
  }  //  produce a vector from a square matrix

  template<class T>
  sparseMatrix<T>::sparseMatrix(char*){
    item = new row<T>*[number = 56];
    for(int i=0; i<number; i++)
      item[i] = 0;
    int J = -1;
    for(int i=0; i<=5; i++)
      for(int j=0; j<=5-i; j++)
        for(int k=0; k<=5-i-j; k++){
	 J++;
	 int I = -1;
	 for(int q=0; q<=2; q++)
	  for(int l=0; l<=2; l++)
	    for(int m=0; m<=2-l; m++)
	      for(int n=0; n<=2-l-m; n++)
	       if(l+m+n==q){
	        I++;
		if((i==l)&&(j==m)&&(k==n)){
		  T coef = factorial(i) * factorial(j) *
		      factorial(k);
		  if(item[I])
		    item[I]->append(coef,J);
		  else
		    item[I] = new row<T>(coef,J);
		}
		if((i==l)&&(j==m)&&(k>=n)){
		  T coef = factorial(i) * factorial(j) * C(k,n);
		  if(item[10+I])
		    item[10+I]->append(coef,J);
		  else
		    item[10+I] = new row<T>(coef,J);
		}
		if((i==l)&&(j>=m)&&(k==n)){
		  T coef = factorial(i) * C(j,m) * factorial(k);
		  if(item[20+I])
		    item[20+I]->append(coef,J);
		  else
		    item[20+I] = new row<T>(coef,J);
		}
		if((i>=l)&&(j==m)&&(k==n)){
		  T coef = C(i,l) * factorial(j) * factorial(k);
		  if(item[30+I])
		    item[30+I]->append(coef,J);
		  else
		    item[30+I] = new row<T>(coef,J);
	        }
	       }
	  if((i==0)&&(j==1)){
	    T coef = 1. / power(2,k);
	    if(item[40])
	        item[40]->append(coef,J);
            else
	      item[40] = new row<T>(coef,J);
	  }
	  if((i==1)&&(j==0)){
	    T coef = 1. / power(2,k);
	    if(item[41])
	        item[41]->append(coef,J);
	    else
	      item[41] = new row<T>(coef,J);
	  }
	  if((i==0)&&(k==1)){
	    T coef = 1. / power(2,j);
	    if(item[42])
	        item[42]->append(coef,J);
	    else
	      item[42] = new row<T>(coef,J);
	  }
	  if((i==1)&&(k==0)){
	    T coef = 1. / power(2,j);
	    if(item[43])
	        item[43]->append(coef,J);
	    else
	      item[43] = new row<T>(coef,J);
	  }
	  if((j==0)&&(k==1)){
	    T coef = 1. / power(2,i);
	    if(item[44])
	        item[44]->append(coef,J);
	    else
	      item[44] = new row<T>(coef,J);
	  }
	  if((j==1)&&(k==0)){
	    T coef = 1. / power(2,i);
	    if(item[45])
	        item[45]->append(coef,J);
	    else
	      item[45] = new row<T>(coef,J);
	  }
	  if(i==1){
	    T coef = 1. / power(2,j+k);
	    if(item[46])
	        item[46]->append(coef,J);
	    else
	      item[46] = new row<T>(coef,J);
	  }
	  if(!i&&(j||k)){
	    T coef = (j+k) / sqrt(2.) / power(2,j+k-1);
	    if(item[47])
	        item[47]->append(coef,J);
	    else
	      item[47] = new row<T>(coef,J);
	  }
	  if(j==1){
	    T coef = 1. / power(2,i+k);
	    if(item[48])
	        item[48]->append(coef,J);
	    else
	      item[48] = new row<T>(coef,J);
	  }
	  if(!j&&(i||k)){
	    T coef = (i+k) / sqrt(2.) / power(2,i+k-1);
	    if(item[49])
	        item[49]->append(coef,J);
	    else
	      item[49] = new row<T>(coef,J);
	  }
	  if(k==1){
	    T coef = 1. / power(2,i+j);
	    if(item[50])
	        item[50]->append(coef,J);
	    else
	      item[50] = new row<T>(coef,J);
	  }
	  if(!k&&(i||j)){
	    T coef = (i+j) / sqrt(2.) / power(2,i+j-1);
	    if(item[51])
	        item[51]->append(coef,J);
	    else
	      item[51] = new row<T>(coef,J);
	  }
	  if(i==1){
	    T coef = 1. / power(3,j+k);
	    if(item[52])
	        item[52]->append(coef,J);
	    else
	      item[52] = new row<T>(coef,J);
	  }
	  if(j==1){
	    T coef = 1. / power(3,i+k);
	    if(item[53])
	        item[53]->append(coef,J);
	    else
	      item[53] = new row<T>(coef,J);
	  }
	  if(k==1){
	    T coef = 1. / power(3,i+j);
	    if(item[54])
	        item[54]->append(coef,J);
	    else
	      item[54] = new row<T>(coef,J);
	  }
	  if(i||j||k){
	    T coef = (i+j+k) / sqrt(3.) / power(3,i+j+k-1);
	    if(item[55])
	        item[55]->append(coef,J);
	    else
	      item[55] = new row<T>(coef,J);
	  }
        }
  }  //  construct the 56*56 matrix B

  template<class T>
  void sparseMatrix<T>::readSparseMatrix(int nonZeroes){
    dynamicVector<int> columnPtr(rowNumber()+1);
    dynamicVector<int> rowIndex(nonZeroes);
    dynamicVector<float> values(nonZeroes);
    FILE* fp = fopen("boeingmatrix3","r");
    for(int i=0;i<=rowNumber();i++)
      fscanf(fp,"%d",&columnPtr(i));
    for(int i=0;i<nonZeroes;i++)
      fscanf(fp,"%d",&rowIndex(i));
    for(int i=0;i<nonZeroes;i++)
      fscanf(fp,"%f",&values(i));
    for(int i=0;i<rowNumber();i++)
      for(int j=columnPtr[i];j<columnPtr[i+1];j++){
	if(item[i])
	  item[i]->append((T)values[j-1],rowIndex[j-1]-1);
	else
	  item[i] = new row<T>((T)values[j-1],rowIndex[j-1]-1);
      }
  }  //  read a sparse matrix from the Harwell-Boeing collecttion

  template<class T>
  sparseMatrix<T>::sparseMatrix(int level,
      const list<polynomial<polynomial<polynomial<double> > > >&PT,
  mesh<tetrahedron>&m, const dynamicVector<point>&x,dynamicVector<point>&f,dynamicVector<int>&DirichletBoundary){
    dynamicVector<point3> midpoint(56,point3(0.,0.,0.));
    dynamicVector<int> midpointDir(56,0);
    dynamicVector<int> xBoundary(DirichletBoundary.dim(),0);
    dynamicVector<int> yBoundary(DirichletBoundary.dim(),0);
    dynamicVector<int> zBoundary(DirichletBoundary.dim(),0);
    item = new row<T>*[number = f.dim()];
    f = 0.;
    for(int i=0; i<number; i++)
      item[i] = 0;
    polynomial<double> zero(1,0.);
    polynomial<polynomial<double> > Zero(1,zero);
    polynomial<double> minus1(1,-1.);
    polynomial<double> oneMinusx(1.,-1.);
    polynomial<polynomial<double> > oneMinusxMinusy(oneMinusx,minus1);
    matrix3 Ne[6];
    Ne[0] = matrix3(point3(0.,1.,0.),
                   point3(0.,0.,1.),
		   point3(1.,0.,0.));
    Ne[1] = matrix3(point3(1.,0.,0.),
                   point3(0.,0.,1.),
		   point3(0.,1.,0.));
    Ne[2] = matrix3(point3(1.,0.,0.),
                   point3(0.,1.,0.),
		   point3(0.,0.,1.));
    Ne[3] = matrix3(point3(0.,0.,1.),
                   point3(sqrt(.5),sqrt(.5),0.),
                   point3(-sqrt(.5),sqrt(.5),0.));
    Ne[4] = matrix3(point3(0.,1.,0.),
                   point3(sqrt(.5),0.,sqrt(.5)),
                   point3(-sqrt(.5),0.,sqrt(.5)));
    Ne[5] = matrix3(point3(1.,0.,0.),
                   point3(0.,sqrt(.5),sqrt(.5)),
                   point3(0.,-sqrt(.5),sqrt(.5)));
    midpoint(40) = midpoint(41) = point3(.5,0.,0.);
    midpoint(42) = midpoint(43) = point3(0.,.5,0.);
    midpoint(44) = midpoint(45) = point3(0.,0.,.5);
    midpoint(46) = midpoint(47) = point3(.5,.5,0.);
    midpoint(48) = midpoint(49) = point3(.5,0.,.5);
    midpoint(50) = midpoint(51) = point3(0.,.5,.5);
    matrix3 Ns[4];
    Ns[0] = matrix3(point3(0.,0.,1.),
                    point3(1.,0.,0.),
		    point3(0.,1.,0.));
    Ns[1] = matrix3(point3(0.,1.,0.),
                    point3(1.,0.,0.),
		    point3(0.,0.,1.));
    Ns[2] = matrix3(point3(1.,0.,0.),
                    point3(0.,1.,0.),
		    point3(0.,0.,1.));
    Ns[3] = matrix3(point3(1./sqrt(3.),1./sqrt(3.),1./sqrt(3.)),
                    point3(sqrt(.5),-sqrt(.5),0.),
		    point3(-1/sqrt(6.),-1/sqrt(6.),2./sqrt(6.)));
    midpoint(52) = point3(1./3.,1./3.,0.);
    midpoint(53) = point3(1./3.,0.,1./3.);
    midpoint(54) = point3(0.,1./3.,1./3.);
    midpoint(55) = point3(1./3.,1./3.,1./3.);
    for(const mesh<tetrahedron>* runner = &m; runner;
	    runner=(const mesh<tetrahedron>*)runner->readNext()){
      polynomial<polynomial<polynomial<double> > > X(1,Zero);
      polynomial<polynomial<polynomial<double> > > Y(1,Zero);
      polynomial<polynomial<polynomial<double> > > X2pY2(1,Zero);
      polynomial<polynomial<polynomial<double> > > threeX2pY2(1,Zero);
      polynomial<polynomial<polynomial<double> > > X2pThreeY2(1,Zero);
      polynomial<polynomial<polynomial<double> > > twoXY(1,Zero);
      matrix3 S((*runner)()[1]() - (*runner)()[0](),
	      (*runner)()[2]() - (*runner)()[0](),
	      (*runner)()[3]() - (*runner)()[0]());
      matrix3 Sinverse = inverse(S);
      matrix3 SinverseT = transpose(Sinverse);
      matrix6 Z(
          produceVector(SinverseT*matrix3(point3(1.,0.,0.),point3(0.,0.,0.),point3(0.,0.,0.))*Sinverse),
          produceVector(SinverseT*matrix3(point3(0.,1.,0.),point3(1.,0.,0.),point3(0.,0.,0.))*Sinverse),
          produceVector(SinverseT*matrix3(point3(0.,0.,0.),point3(0.,1.,0.),point3(0.,0.,0.))*Sinverse),
          produceVector(SinverseT*matrix3(point3(0.,0.,1.),point3(0.,0.,0.),point3(1.,0.,0.))*Sinverse),
          produceVector(SinverseT*matrix3(point3(0.,0.,0.),point3(0.,0.,1.),point3(0.,1.,0.))*Sinverse),
          produceVector(SinverseT*matrix3(point3(0.,0.,0.),point3(0.,0.,0.),point3(0.,0.,1.))*Sinverse)
	);
      matrix6 ZinverseT = transpose(inverse(Z));
      matrix3 weight=fabs(det(S)) * Sinverse * SinverseT;
      list<polynomial<polynomial<polynomial<double> > > > P(PT);
      for(int q=0; q<40; q+=10){
        list<polynomial<polynomial<polynomial<double> > > > P3(q+1,3,PT);
	P.replace(q+1,S * P3);
        list<polynomial<polynomial<polynomial<double> > > > P6(q+4,6,PT);
	P.replace(q+4,ZinverseT * P6);
      }
      for(int q=40; q<52; q+=2){
        list<polynomial<polynomial<polynomial<double> > > > P2(q,2,PT);
	point3 Se = q < 46 ?
	         S[(q-40)/2]
	       :
		 S[(52-q)/2 % 3] - S[(54-q)/2 % 3];
	int m = fabs(Se[2]) > 1.e-6 ?
	          2 
		:
		  fabs(Se[1]) > 1.e-6 ?
		    1
		  :
		    0;
	midpointDir(q) = m >= 1 ? 0 : 1;
	midpointDir(q+1) = m == 2 ? 1 : 2;
	matrix2 hatY = minor(2,m,inverse(S * Ne[(q-40)/2]));
	P.replace(q,inverse(hatY) * P2);
      }
      for(int q=52; q<56; q++){
	matrix3 SN = S * Ns[q-52];
	point3 sn = SN[1] & SN[2];
	int m = fabs(sn[0]) > 1.e-6 ?
	          0
	        :
		  fabs(sn[1]) > 1.e-6 ?
		    1
		  :
		    2;
	midpointDir(q) = m;
        list<polynomial<polynomial<polynomial<double> > > > P1(q,1,PT);
	matrix1 hatYinverse(1./inverse(SN)(0,m),"one");
	P.replace(q,hatYinverse * P1);
      }
      list<polynomial<polynomial<polynomial<double> > > > P0(3,P[0]);
      list<list<polynomial<polynomial<polynomial<double> > > > > dP(56,P0);
      for(int i=0; i<56; i++){
        dP(i)(0) = d(P[i],1,0,0);
        dP(i)(1) = d(P[i],0,1,0);
        dP(i)(2) = d(P[i],0,0,1);
      }
      for(int q=40; q<52; q++)
        for(int qq=52; qq<56; qq++){
	  double coeff = 
	   (SinverseT * dP[q])[midpointDir[qq]](midpoint[qq][0],midpoint[qq][1],midpoint[qq][2]);
	  coeff = -coeff;
	  P(q) += coeff * P[qq];
        }
      for(int q=0; q<40; q++)
        for(int qq=40; qq<56; qq++){
	  double coeff = 
	   (SinverseT * dP[q])[midpointDir[qq]](midpoint[qq][0],midpoint[qq][1],midpoint[qq][2]);
	  coeff = -coeff;
	  P(q) += coeff * P[qq];
        }
      for(int i=0; i<56; i++){
        dP(i)(0) = d(P[i],1,0,0);
        dP(i)(1) = d(P[i],0,1,0);
        dP(i)(2) = d(P[i],0,0,1);
      }
      for(int i=0; i<4; i++){
        int I = (*runner)()[i].getIndex();
        if((*runner)()[i]()[0] <= 1.e-6){
	  xBoundary(10 * I) = 1;
	}
        if((*runner)()[i]()[0] >= 1. - 1.e-6)
	  xBoundary(10 * I) = 2;
        if(level>=nonlinearLevels){
          if((*runner)()[i]()[1] <= 1.e-6)
	    yBoundary(10 * I) = 1;
          if((*runner)()[i]()[1] >= 1. - 1.e-6)
	    yBoundary(10 * I) = 2;
          if((*runner)()[i]()[2] <= 1.e-6)
	    zBoundary(10 * I) = 1;
          if((*runner)()[i]()[2] >= 1. - 1.e-6)
	    zBoundary(10 * I) = 2;
        }
      }
      if(level>=nonlinearLevels){
        for(int i=0; i<56; i++){
          int I = (*runner)().readMeshIndex(i);
          X += x[I][0] * P[i];
          Y += x[I][1] * P[i];
        }
        polynomial<polynomial<polynomial<double> > > X2 = X * X;
        polynomial<polynomial<polynomial<double> > > Y2 = Y * Y;
	X2pY2 = X2 + Y2;
	threeX2pY2 = 3. * X2 + Y2;
	X2pThreeY2 = X2 + 3. * Y2;
	twoXY = 2. * X * Y;
      }
      double helm=HELM*fabs(det(S));
      double helmNonlin=HELMNonlin*fabs(det(S));
      for(int i=0; i<56; i++){
        int I = (*runner)().readMeshIndex(i);
	if((DirichletBoundary[I])&&
            (level>=nonlinearLevels))
	  f(I) += helmNonlin * point(integral(X2pY2 * X * P[i]),integral(X2pY2 * Y * P[i]));
	for(int j=0; j<56; j++){
          int J = (*runner)().readMeshIndex(j);
          polynomial<polynomial<polynomial<double> > > Pij = P[i] * P[j];
	  T addIJ = integral(helm*Pij+dP[j]*(weight*dP[i]));
	  if(item[I]){
	    row<T> r(addIJ,J);
	    *item[I] += r;
	  }
	  else
	    item[I] = new row<T>(addIJ,J);
	  if(DirichletBoundary[I])
	    f(I) += addIJ * x[J];
	  if((i<40)&&(j<40)&&(i%10==0)&&(j%10==0))
	   for(int k=0; k<40; k+=10){
            int K = (*runner)().readMeshIndex(k);
	    if((i!=j)&&(j!=k)&&(k!=i)
	      &&((xBoundary[I]*xBoundary[J]*xBoundary[K]==1)||(xBoundary[I]*xBoundary[J]*xBoundary[K]==8)
	      ||(yBoundary[I]*yBoundary[J]*yBoundary[K]==1)||(yBoundary[I]*yBoundary[J]*yBoundary[K]==8)
	      ||(zBoundary[I]*zBoundary[J]*zBoundary[K]==1)||(zBoundary[I]*zBoundary[J]*zBoundary[K]==8))){
	      int notijk = 0;
	      while((notijk==i)||(notijk==j)||(notijk==k))
	        notijk += 10;
	      polynomial<polynomial<double>  >
	          Pi = notijk == 10 ?
		         P[i](0.,"x=0","x=0")
		       :
			 notijk == 20 ?
		           P[i](0.,"y=0")
			 :
			   notijk == 30 ?
		             P[i](0.)
			   :
		             P[i](oneMinusxMinusy);
	      polynomial<polynomial<double>  >
	          Pj = notijk == 10 ?
		         P[j](0.,"x=0","x=0")
		       :
			 notijk == 20 ?
		           P[j](0.,"y=0")
			 :
			   notijk == 30 ?
		             P[j](0.)
			   :
		             P[j](oneMinusxMinusy);
              point3 jMinusi= (*runner)()[j/10]() - (*runner)()[i/10]();
              point3 kMinusi= (*runner)()[k/10]() - (*runner)()[i/10]();
              double radiation = sqrt(fabs(HELM)) * l2norm(jMinusi&kMinusi);
	      double radiationI = radiation / 2. * integral(Pi*Pi);
	      double radiationJ = radiation * integral(Pi*Pj);
	      T radiationMatrixI(point(0.,radiationI),point(-radiationI,0.));
	      T radiationMatrixJ(point(0.,radiationJ),point(-radiationJ,0.));
	      row<T> radiationRowI(radiationMatrixI,I);
	      row<T> radiationRowJ(radiationMatrixJ,J);
	      *item[I] += radiationRowI;
	      *item[I] += radiationRowJ;
	      if(DirichletBoundary[I]){
	        f(I) += radiationMatrixI * x[I];
	        f(I) += radiationMatrixJ * x[J];
	      }
	      if(xBoundary[I]*xBoundary[J]*xBoundary[K]==1)
	        f(I) -= point(0.,radiation * integral(Pi));
	    }
	   }
          if(level>=nonlinearLevels){
	   point J1(integral(threeX2pY2 * Pij), integral(twoXY * Pij));
	   point J2(J1[1],integral(X2pThreeY2 * Pij));
	   matrix2 Jacobian(J1,J2);
	   row<T> JacobianJ(helmNonlin * Jacobian,J);
	   *item[I] += JacobianJ;
	  }
	}
      }
    }
  }  //  assembly of stiffness matrix

  template<class T>
  template<class S>
  void sparseMatrix<T>::setDirichlet(dynamicVector<S>&f,dynamicVector<int>&DirichletBoundary){
    if(Dirichlet)
      for(int i=0; i<number; i++){
        if(DirichletBoundary[i])
          f(i) -= item[i]->maskAll(DirichletBoundary,f);
	else
	  *item[i] = row<T>(1.,i);
      }
    }  //  set Dirichlet BC


  template<class T>
  const sparseMatrix<T>
  operator+(const sparseMatrix<T>&M1,const sparseMatrix<T>&M2){
    return sparseMatrix<T>(M1) += M2;
  }  //  matrix plus matrix

  template<class T>
  const sparseMatrix<T>
  operator-(const sparseMatrix<T>&M1,const sparseMatrix<T>&M2){
    return sparseMatrix<T>(M1) -= M2;
  }  //  matrix minus matrix

  template<class T>
  const sparseMatrix<T>
  operator*(const T&t, const sparseMatrix<T>&M){
    return sparseMatrix<T>(M) *= t;
  }  //  scalar times sparse matrix

  template<class T>
  const sparseMatrix<T>
  operator*(const sparseMatrix<T>&M, const T&t){
    return sparseMatrix<T>(M) *= t;
  }  //  sparse matrix times scalar

  template<class T, class S>
  const dynamicVector<S>
  operator*(const sparseMatrix<T>&M,const dynamicVector<S>&v){
    dynamicVector<S> result(M.rowNumber(),0.);
    for(int i=0; i<M.rowNumber(); i++)
      result(i) = M[i] * v;
    return result;
  }  //  sparse matrix times vector

  template<class T>
  void
  sparseMatrix<T>::truncatePositive(double threshold){
    for(int i=0; i<rowNumber(); i++){
      if(ThrowAtilde)
        (*this)(i,i,"write") +=
	  item[i]->dropPositiveItems(i, (*this)(i,i), threshold);
      else
	  item[i]->dropPositiveItems(i, (*this)(i,i), threshold);
    }
  }

  template<class T>
  const sparseMatrix<T>
  truncate(const sparseMatrix<T>&M, double threshold){
    sparseMatrix<T> Q = M;
    for(int i=0; i<Q.rowNumber(); i++){
      if(ThrowQ)
        Q(i,i,"write") += Q.item[i]->truncateItems(threshold, Q(i,i));
      else
        Q.item[i]->truncateItems(threshold, Q(i,i));
    }
    return Q;
  }

  template<class T>
  const sparseMatrix<T>
  operator*(const sparseMatrix<T>&M1, const sparseMatrix<T>&M2){
      sparseMatrix<T> result(M1.rowNumber());
      for(int i = 0; i < M1.rowNumber(); i++){
	result.item[i] = new row<T>(M1.item[i]->getValue() *
				 *M2.item[M1.item[i]->getColumn()]);
          for(const row<T>* runner = (const row<T>*)M1.item[i]->readNext();
	       runner; runner = (const row<T>*)runner->readNext()){
	    row<T> r = runner->getValue() * *M2.item[runner->getColumn()];
	    *result.item[i] += r;
	  }
      }
      return result;
  }  //  sparse matrix times sparse matrix

  template<class T>
  const sparseMatrix<T>
  operator*(const matrix3&M1, const sparseMatrix<T>&M2){
      sparseMatrix<T> result(3);
      for(int i = 0; i < 3; i++){
	result.item[i] = new row<T>(*M2.item[0]);
	*result.item[i] *= M1(i,0);
        for(int j = 1; j < 3; j++){
	  row<T> r = *M2.item[j];
	  r *= M1(i,j);
	  *result.item[i] += r;
	}
      }
      return result;
  }  //  matrix3 times sparse matrix

  template<class T>
  const sparseMatrix<T>
  operator*(const sparseMatrix<T>&M1, const matrix3&M2){
      return transpose(transpose(M2) * transpose(M1));
  }  //  sparse matrix times matrix3

  template<class T>
  const sparseMatrix<T>
  diagonal(const sparseMatrix<T>&M){
    sparseMatrix<T> diag(M.rowNumber());
    for(int i=0; i<M.rowNumber(); i++)
      diag.item[i] = new row<T>(M(i,i),i);
    return diag;
  }  //  copy the diagonal part

  template<class T>
  const sparseMatrix<T>
  transpose(const sparseMatrix<T>&M){
    sparseMatrix<T> Mt(M.columnNumber());
    for(int i=0; i<M.rowNumber(); i++)
      for(const row<T>* runner = M.item[i]; runner;
	      runner = (const row<T>*)runner->readNext()){
        if(Mt.item[runner->getColumn()])
          Mt.item[runner->getColumn()]->append(runner->getValue(),i);
        else
          Mt.item[runner->getColumn()] =
		  new row<T>(runner->getValue(),i);
      }
    return Mt;
  }  //  transpose of matrix

  template<class T>
  const sparseMatrix<T> sparseMatrix<T>::factorize(double threshold){
    sparseMatrix<T> L(rowNumber());
    L.item[0] = new row<T>(0.,0);
    for(int i=1; i<rowNumber(); i++){
       if(item[i-1]->readNext())
	item[i-1]->truncateItems(threshold, item[i-1]->getValue());
      while(item[i]&&(item[i]->getColumn() < i)){
	T factor=item[i]->getValue() / item[item[i]->getColumn()]->getValue();
	if(fabs(factor) >= threshold){
	  row<T> r = (-1. * factor) * *item[item[i]->getColumn()];
	  *item[i] += r;
	  if(L.item[i])
	    L.item[i]->append(factor, item[i]->getColumn());
	  else
	    L.item[i] = new row<T>(factor, item[i]->getColumn());
	}
	item[i]->dropFirstItem();
      }
      if(!L.item[i])L.item[i] = new row<T>(0.,0);
    }
    return L;
  }  //  incomplete LU factorization

  template<class T>
  template<class S>
  const dynamicVector<S>
  sparseMatrix<T>::forwardElimination(const dynamicVector<S>&f)const{
    dynamicVector<S> result(f.dim(),0.);
    result(0) = f[0];
    for(int i=1; i<f.dim(); i++)
      result(i) = item[i] ? f[i] - *item[i] * result : f[i];
    return result;
  }  //  forward elimination in L

  template<class T>
  template<class S>
  const dynamicVector<S>
  sparseMatrix<T>::backSubstitution(const dynamicVector<S>&f)const{
    dynamicVector<S> result(f.dim(),0.);
    for(int i = f.dim() - 1; i>=0; i--){
      result(i) = item[i]->readNext() ?
	    f[i] - *(row<T>*)item[i]->readNext() * result : f[i];
      result(i) /= (*item[i])().getValue();
    }
    return result;
  }  //  beck substitution in U

  template<class T, class S>
  void ILU(const sparseMatrix<T>&A, const sparseMatrix<T>&L,
    const sparseMatrix<T>&U,const dynamicVector<S>&f, dynamicVector<S>&x){
    x += U.backSubstitution(L.forwardElimination(f - A * x));
  }  //  ILU iteration

  template<class T>
  const dynamicVector<T>
  operator/(const dynamicVector<T>&v, const sparseMatrix<T>&A){
    dynamicVector<T> result(v);
    for(int i=0; i<v.dim(); i++)
      result(i) /= A(i,i);
    return result;
  }  //  vector divided by the main diagonal of matrix

  template<class T>
  void Jacobi(const sparseMatrix<T>&A,
	  const dynamicVector<T>&f, dynamicVector<T>&x){
    x += (f - A * x) / A;
  }  //  Jacobi relaxation

  template<class T>
  void GaussSeidel(const sparseMatrix<T>&A,
	  const dynamicVector<point>&f, dynamicVector<point>&x){
    for(int i=0; i<f.dim(); i++)
      x(i) += (f[i] - *A.item[i] * x) / A(i,i);
  }  //  Gauss-Seidel relaxation

  template<class T>
  void reversedGaussSeidel(const sparseMatrix<T>&A,
	  const dynamicVector<point>&f, dynamicVector<point>&x){
    for(int i=f.dim()-1; i>=0; i--)
      x(i) += (f[i] - *A.item[i] * x) / A(i,i);
  }  //  Gauss-Seidel relaxation in reversed order

  template<class T>
  void symmetricGaussSeidel(const sparseMatrix<T>&A,
	  const dynamicVector<point>&f, dynamicVector<point>&x){
    for(int i=0; i<f.dim(); i++)
      x(i) += (f[i] - *A.item[i] * x) / A(i,i);
    for(int i=f.dim()-2; i>=0; i--)
      x(i) += (f[i] - *A.item[i] * x) / A(i,i);
  }  //  symmetric Gauss-Seidel relaxation

  template<class T>
  const sparseMatrix<T>
  sparseMatrix<T>::createTransfer(){
    dynamicVector<T> RowSumA(rowNumber(),(T)0.);
    const dynamicVector<int> coarse = coarsen(thresholdMG);
    const dynamicVector<int> fine = coarsen(coarse, FactorFine * thresholdMG);
    sparseMatrix<T> I(rowNumber(),1.);
    sparseMatrix<T> Atilde = *this;
    if(TruncateAtilde)
      Atilde.truncatePositive(thresholdMGAtilde);
    for(int i=0; i<rowNumber(); i++){
      RowSumA(i) = (T)min(0.,real(item[i]->rowSum()));
      if(!coarse[i])
        item[i]->dropPositiveItems(i, (*item[i])[i], thresholdMGpositive);
    }
    sparseMatrix<T> A = *this;
    sparseMatrix<T> P(nonZeros(fine));
   if(nonZeros(fine) > nonZeros(coarse)){
    for(int i=0; i<rowNumber(); i++){
      if(coarse[i]){
	delete P.item[fine[i]-1];
	P.item[fine[i]-1] = new row<T>((T)1., coarse[i] - 1);
      }
      else if(fine[i]){
	delete P.item[fine[i]-1];
	P.item[fine[i]-1] = new row<T>(*item[i]);
	if(AMG)
	  for(const row<T>* runner = item[i]; runner;
	        runner = (const row<T>*)runner->readNext()){
	    int j = runner->getColumn();
	    if((j != i)&&(!coarse[j])){
	      T Pij = runner->getValue();
	      T AjSum = item[i]->rowSumCoarse(*A.item[j],coarse);
	      if(fabs(AjSum) > 1.e-10)
	        item[i]->addCoarse((Pij / AjSum) * *A.item[j],coarse);
	    }
	  }
        P.item[fine[i]-1]->maskItems(coarse, A(i,i));
        P.item[fine[i]-1]->renumberColumns(coarse);
	if(RowSumStablizer)
	  *P.item[fine[i]-1] /= P.item[fine[i]-1]->rowSum() - RowSumA[fine[i]-1] / 2.;
	else
	  *P.item[fine[i]-1] /= P.item[fine[i]-1]->rowSum();
      }
    }
   }
    for(int i=0; i<rowNumber(); i++){
      if(fine[i]){
	delete item[i];
	item[i] = new row<T>((T)1., fine[i] - 1);
	delete Atilde.item[i];
	Atilde.item[i] = new row<T>((T)0.,i);
      }
      else{
	if(AMG)
	  for(const row<T>* runner = item[i]; runner;
	        runner = (const row<T>*)runner->readNext()){
	    int j = runner->getColumn();
	    if((j != i)&&(!fine[j])){
	      T Pij = runner->getValue();
	      T AjSum = item[i]->rowSumCoarse(*A.item[j],fine);
	      if(fabs(AjSum) > 1.e-10)
	        item[i]->addCoarse((Pij / AjSum) * *A.item[j],fine);
	    }
	  }
        item[i]->maskItems(fine, A(i,i));
        item[i]->renumberColumns(fine);
	if(RowSumStablizer)
	  *item[i] /= item[i]->rowSum() - RowSumA[i] / 2.;
	else
	  *item[i] /= item[i]->rowSum();
      }
    }
    if(nonZeros(fine) > nonZeros(coarse))
      *this = *this * P;
    if(Smooth){
      Atilde /= A;
      *this = (I - Atilde) * *this;
    }
    return transpose(*this);
  }  //  create transfer operators

  template<class T>
  const dynamicVector<int>
  sparseMatrix<T>::coarsen(const dynamicVector<int>&v, double threshold) const{
    dynamicVector<int> coarse = v;
    for(int i=0; i<rowNumber(); i++)
      if(!coarse[i]){
	int drop=1;
        for(const row<T>* runner = item[i]; runner;
	        runner = (const row<T>*)runner->readNext())
	  if((coarse[runner->getColumn()])&&
	      (real(runner->getValue()) / real((*this)(i,i)) <= -threshold))
	    drop=0;
	if(drop) coarse(i) = 1;
      }
    int count = 1;
    for(int i=0; i<rowNumber(); i++)
      if(coarse[i])coarse(i) = count++;
    return coarse;
  }  //  create coarse grid

  template<class T>
  const dynamicVector<int>
  sparseMatrix<T>::coarsen(double threshold) const{
    dynamicVector<int> coarse(rowNumber(), 1);
    for(int i=0; i<rowNumber(); i++)
      if(coarse[i])
        for(const row<T>* runner = item[i]; runner;
	        runner = (const row<T>*)runner->readNext())
	  if((runner->getColumn() != i)&&(
	       fabs(runner->getValue()) > threshold * fabs((*this)(i,i))))
	    coarse(runner->getColumn()) = 0;
    for(int i=0; i<rowNumber(); i++)
      if(!coarse[i]){
	int drop=1;
        for(const row<T>* runner = item[i]; runner;
	        runner = (const row<T>*)runner->readNext())
	  if((coarse[runner->getColumn()])&&
	      (real(runner->getValue()) / real((*this)(i,i)) <= -threshold))
	    drop=0;
	if(drop) coarse(i) = 1;
      }
    int count = 1;
    for(int i=0; i<rowNumber(); i++)
      if(coarse[i])coarse(i) = count++;
    return coarse;
  }  //  create coarse grid

template<class T> class multigrid{
	sparseMatrix<T> A;
	sparseMatrix<T> U;
	sparseMatrix<T> L;
	sparseMatrix<T> P;
	sparseMatrix<T> R;
	multigrid* next;
      public:
	multigrid():next(0){}
	multigrid(const multigrid&mg)
	    :A(mg.A),U(mg.U),L(mg.L),P(mg.P),R(mg.R),
	     next(mg.next ? new multigrid(*mg.next) : 0){}
	multigrid(const sparseMatrix<T>&m):A(m),U(useILU?A:0),
	L(useILU?U.factorize(thresholdILU):0),P(A),
	 R(P.createTransfer()),next(R.rowNumber() < ratioMG*A.rowNumber() ?
	 new multigrid(truncate(R*A*P, thresholdMGQ)) : 0){}  //  constructor with matrix argument
	const multigrid<T>& operator=(const multigrid<T>&);
	~multigrid(){ delete next; next = 0; }  //  destructor
	template<class S>
	const dynamicVector<S>& VcycleILU(const dynamicVector<S>&,
		dynamicVector<S>&) const;
	template<class S>
	const dynamicVector<S>& Vcycle(const dynamicVector<S>&,
		dynamicVector<S>&) const;
	friend void print<T>(const multigrid<T>&);
  };

	template<class T>
	const multigrid<T>& multigrid<T>::operator=(const multigrid<T>&mg){
	  if(this != &mg){
	    A = mg.A;
	    U = mg.U;
	    L = mg.L;
	    P = mg.P;
	    R = mg.R;
            if(next){
              if(mg.next)
                *next = *mg.next;
              else{
                delete next;
	        next = 0;
              }
	    }
            else
              if(mg.next)next = new multigrid(*mg.next);
	  }
          return *this;
        }  //  assignment operator

	template<class T>
	template<class S>
	const dynamicVector<S>&
	multigrid<T>::VcycleILU(const dynamicVector<S>&f,
		   dynamicVector<S>&x) const{
	   if(next){
	     for(int i=0; i<Nu1; i++)
	         ILU(A,L,U,f,x);
	     dynamicVector<S> residual = f - A * x;
	     dynamicVector<S> correction(R.rowNumber(), 0.);
	     for(int i=0; i<cycleIndex; i++)
	       next->VcycleILU(R * residual, correction);
	     x += P * correction;
	     for(int i=0; i<Nu2; i++)
	         ILU(A,L,U,f,x);
	   }
	   else
	     for(int i=0; i<NuCoarse; i++)
	         ILU(A,L,U,f,x);
	   return x;
	}  //  multigrid V-cycle

	template<class T>
	template<class S>
	const dynamicVector<S>&
	multigrid<T>::Vcycle(const dynamicVector<S>&f,
		   dynamicVector<S>&x) const{
	   if(next){
	     for(int i=0; i<Nu1; i++)
                 GaussSeidel(A,f,x);
	     dynamicVector<S> residual = f - A * x;
	     dynamicVector<S> correction(R.rowNumber(), 0.);
	     for(int i=0; i<cycleIndex; i++)
	       next->Vcycle(R * residual, correction);
	     x += P * correction;
	     for(int i=0; i<Nu2; i++)
	         reversedGaussSeidel(A,f,x);
	   }
	   else
	     for(int i=0; i<NuCoarse; i++)
	         symmetricGaussSeidel(A,f,x);
	   return x;
	}  //  multigrid V-cycle

	template<class T>
	void print(const multigrid<T>&mg){
	  print(mg.A);
	  print(mg.P);
	  print(mg.R);
	  if(mg.next)print(*mg.next);
	}  //  print multigrid object


       template<class T>
       void PCG(const multigrid<T>& MG, const sparseMatrix<T>&A,
		 const dynamicVector<T>&f, dynamicVector<T>&x){
         const double eps=1.e-15;
	 const int iterationnumber = 1000;
	 dynamicVector<T> zero(x.dim(),0.);
	 dynamicVector<T> keep(x);
	 dynamicVector<T> rr(MG.VcycleILU(f,keep) - x);
	 dynamicVector<T> r = f - A * x;
	 dynamicVector<T> pp(rr);
	 dynamicVector<T> p = r;
	 double gamma = r * rr;
	 double gammainit = gamma;
	 int count = 0;
	 while((fabs(gamma/gammainit) >= thresholdCG * thresholdCG)&&
	        (count <= iterationnumber)){
	   keep = pp;
	   dynamicVector<T> ww = pp - MG.VcycleILU(zero,keep);
	   dynamicVector<T> w = A * pp;
	   double alpha = gamma / (pp * w);
	   x += alpha * pp;
	   rr -= alpha * ww;
	   r -= alpha * w;
	   double gammaold = gamma;
	   gamma = r * rr;
	   double beta = gamma/gammaold;
	   pp = rr + beta * pp;
	   count++;
	 }
         printf("total MG iterations in PCG=%d\n", count + 1);
       }  //  PCG acceleration

        template<class T, class S>
        void CGS(const multigrid<T>& MG, const sparseMatrix<T>&A, int TFQMR,
                 const dynamicVector<S>&f, dynamicVector<S>&x){
          const double eps=1.e-15;
          const int iterationnumber = 1000;
          double omegainit,omega0,omega1,tau,vv;
          double rho0,sigma,eta,alpha,rho1,beta;
          dynamicVector<S> keep(x);
          dynamicVector<S> rr(MG.Vcycle(f,keep) - x);
          dynamicVector<S> rbar(rr);
          dynamicVector<S> rcgs(rr),u(rr),pp(rr);
          tau = omegainit = omega1 = omega0 =
              sqrt(rcgs * rcgs);
          printf("res0=");
          print(omegainit);
	  printf("\n");
  	  eta=0.;
          vv=0.;
          if(fabs(rho0 = rbar * rr) < eps)
            printf("rho0=%f,mg iteration number=1\n",fabs(rho0));
          dynamicVector<S> zero(x.dim(),0.);
          dynamicVector<S> d(zero),v(zero),q(zero);
          int count = 1;
          do{
            keep = pp;
            v = pp - MG.Vcycle(zero,keep);
            if(fabs(sigma = rbar * v) < eps)
              printf("sigma=%f,mg iteration number=%d\n",fabs(sigma),2 * count);
            if(fabs(alpha=rho0/sigma) < eps)
              printf("alpha=%f,mg iteration number=%d\n",fabs(alpha),2 * count);
            q = u - alpha * v;
            dynamicVector<S> uq = u + q;
            keep = uq;
            rcgs -= alpha * (uq - MG.Vcycle(zero,keep));
            omega1=sqrt(rcgs * rcgs);
            if(!TFQMR){
              x += alpha * uq;
              printf("residual=%f, MG iteration number in CGS=%d\n",fabs(omega1),2 * count + 1);
            }
            else{
              for(int m=2*count+1; m<=2*count+2;m++){
                double omega;
                if(m==2*count+1){
                  omega=sqrt(omega0*omega1);
                  keep=u;
                }
                else{
                  omega=omega1;
                  keep=q;
                }
                double scala=vv*vv*eta/alpha;
                d = keep + scala * d;
                vv=omega/tau;
                double c=1./sqrt(1.+vv*vv);
                tau *= vv*c;
                eta=c*c*alpha;
                x += eta * d;
                printf("residual=%f, MG iteration number in CGS=%d\n",sqrt((A*x-f)*(A*x-f)),2 * count + 1);
              }
            }
            omega0=omega1;
            if(fabs(rho1=rbar*rcgs)<eps)
              printf("rho1=%f,mg iteration number=%d\n",fabs(rho1),2*count+1);
            beta=rho1/rho0;
            rho0=rho1;
            u = rcgs + beta * q;
            pp = u + beta * (q + beta * pp);
         } while((fabs(omega1/omegainit) >= thresholdCG)&&
                 (++count <= iterationnumber));
         printf("total MG iterations in CGS=%d\n",2 * count + 1);
       }  //  CGS and TFQMR acceleration

    template<class T, class S>
    const dynamicVector<S>&
    GMRES(const multigrid<T>&MG, const sparseMatrix<T>&A,
          int preIterations, int K,
          const dynamicVector<S>&f, dynamicVector<S>&x){
      for(int i=0;i<preIterations;i++)
        MG.Vcycle(f,x);
      dynamicVector<S> s = x;
      dynamicVector<S> r = x;
      double R[K+1][K+1];
      for(int i=0;i<=K;i++)
        for(int j=0;j<=K;j++)
          R[i][j]=0.0;
      double Givens[2][K];
      double xi[K];
      for(int i=0;i<K;i++){
        Givens[0][i] = 0.0;
        Givens[1][i] = 0.0;
        xi[i] = 0.0;
      }
      dynamicVector<S>* Q[K+1];
      dynamicVector<S> zero(x.dim(),0.0);
      dynamicVector<S> keep(x);
      double res=sqrt((A*x-f)*(A*x-f));
      for(int k=0;k<=K;k++){
        if(k)keep = *Q[k-1];
        Q[k] = k ?
          new dynamicVector<S>(*Q[k-1] - MG.Vcycle(zero,keep))
          : new dynamicVector<S>(MG.Vcycle(f,keep)-x);
        for(int j=0;j<k;j++)
          *Q[k] -= (R[j][k] = *Q[j] * *Q[k]) * *Q[j];
        *Q[k] *= 1.0/(R[k][k] = sqrt(*Q[k] * *Q[k]));
        double givensa;
        double givensb;
        if(k){
          for(int j=1;j<k;j++){
            givensa=R[j-1][k];
            givensb=R[j][k];
            R[j-1][k]=givensa*Givens[0][j-1]+givensb*Givens[1][j-1];
            R[j][k]=-givensa*Givens[1][j-1]+givensb*Givens[0][j-1];
          }
          double ab=sqrt(R[k-1][k]*R[k-1][k]+R[k][k]*R[k][k]);
          Givens[0][k-1]=R[k-1][k]/ab;
          Givens[1][k-1]=R[k][k]/ab;
          givensa=R[k-1][k];
          givensb=R[k][k];
          R[k-1][k]=givensa*Givens[0][k-1]+givensb*Givens[1][k-1];
          R[k][k]=0.0;
          R[k][0]=-R[k-1][0]*Givens[1][k-1];
          R[k-1][0]=R[k-1][0]*Givens[0][k-1];
        }
        for(int i=k-1;i>=0;i--){
          xi[i]=R[i][0];
          for(int j=i+2;j<=k;j++)
            xi[i] -= R[i][j] * xi[j-1];
          xi[i] /= R[i][i+1];
        }
        s = x;
        for(int j=0;j<k;j++)
          s += xi[j] * *Q[j];
        printf("residual at step k=%d is %f\n",k,res=sqrt((r=A*s-f)*r));
      }
      return x = s;
    }  //  GMRES with initial iterations

  main(){
    polynomial<double> x1(0.,1.);
    polynomial<polynomial<double> > xx(1,x1);
    polynomial<polynomial<polynomial<double> > > 
        xxx(1,xx);
    list<polynomial<polynomial<polynomial<double> > > > 
        P(56,xxx);
    sparseMatrix<double> B("56");
    sparseMatrix<double> Bt = transpose(B);
    sparseMatrix<double> BtB =Bt * B;
    multigrid<double> mg(BtB);
    for(int i=0; i<56; i++){
      dynamicVector<double> Iq(56,0.);
      dynamicVector<double> x = Iq;
      Iq(i) = 1.;
      PCG(mg,BtB,Bt * Iq,x);
      P(i) = producePolynomial(x);
    }
    ratioMG = .95;
    useILU = 0;
    node<point3> a000(point3(0,0,0));
    node<point3> a100(point3(1,0,0));
    node<point3> a010(point3(0,1,0));
    node<point3> a001(point3(0,0,1));
    node<point3> a011(point3(0,1,1));
    node<point3> a111(point3(1,1,1));
    node<point3> a101(point3(1,0,1));
    node<point3> a110(point3(1,1,0));
    tetrahedron t1(a000,a100,a010,a001);
    tetrahedron t2(a111,a011,a101,a110);
    tetrahedron t3(t2(1),t2(2),t1(1),t2(3));
    tetrahedron t4(t2(1),t1(2),t1(1),t2(3));
    tetrahedron t5(t2(1),t2(2),t1(1),t1(3));
    tetrahedron t6(t2(1),t1(2),t1(1),t1(3));
    mesh<tetrahedron> m(t1);
    m.append(t2);
    m.append(t3);
    m.append(t4);
    m.append(t5);
    m.append(t6);
    t1.~tetrahedron();
    t2.~tetrahedron();
    t3.~tetrahedron();
    t4.~tetrahedron();
    t5.~tetrahedron();
    t6.~tetrahedron();
    for(int i=0; i<adaptiveLevels; i++){
      int nodes = m.indexing();
      int edges = m.edgeIndexing(nodes);
      int sides = m.sideIndexing(nodes,edges);
      int degrees = 10 * nodes + 2 * edges + sides;
      dynamicVector<point> f(degrees,point(0.,0.));
      dynamicVector<point> solution(f.dim(),point(0.,0.));
      dynamicVector<int> DirichletBoundary(f.dim(),1);
      if(i>=nonlinearLevels){
        Newton = 100;
	HELMNonlin -= 2.;
      }
      if(i>=refineLevels){
        for(int n=0; n<Newton; n++){
          sparseMatrix<matrix2> A(i,P,m,solution,f,DirichletBoundary);
          A.setDirichlet(f,DirichletBoundary);
          if(PrintMesh){
	    print(m);
	    print(A);
	    print(f);
	    return 0;
          }
          multigrid<matrix2> MG(A);
          dynamicVector<point> x(f.dim(),0.);
          GMRES(MG,A,0,400,f,x);
	  solution -= x;
	  printf("\n");
	  printf("at Newton iteration %d res = %f\n",n,sqrt(f*f));
	  if(i<nonlinearLevels)
	    printf("at Newton iteration %d err = %f\n",n,m.error(solution,nodes));
	  printf("\n");
        }
        //thresholdAdaptive = max(1. * m.jumpSum(solution),0.5 * m.jumpMax(solution));
      }
      if(PrintSol) print(solution);
      printf("at level %d, number of nodes=%d\n",i+1,nodes);
      printf("at level %d, number of degrees=%d\n",i+1,solution.dim());
      if(i<adaptiveLevels-1)
        m.refine(solution, DirichletBoundary, thresholdAdaptive);
    }
    return 0;
  }
