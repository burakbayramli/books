//  (NONLINEAR-)HELMHOLTZ SOLVER IN A 3D ADAPTIVE MESH
//  --------------------------------------------------
//  This code implements the polynomial,
//  node, finite-element, mesh, dynamic-vector,
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
const int adaptiveLevels=13;
const int nonlinearLevels=12;
const int refineLevels=2;
const int Dirichlet=1;
const double SlitWidth=.2;
const double SlitLength=.5;
const double HELM=-80.0;
double HELMNonlin=-0.0;
int Newton = 1;
const int RowSumStablizer=0;
double thresholdAdaptive=-0.02;
const double thresholdILU=0.05;
const int useILU=0;
const double thresholdCG=1.e-6;
const double ratioMG=.95;
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

int max(int a, int b){return a>b ? a : b;}
int min(int a, int b){return a<b ? a : b;}

double max(double a, double b){return a>b ? a : b;}
double min(double a, double b){return a<b ? a : b;}

double fabs(double d){return d > 0. ? d : -d;}  //  absolute value

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
  const T& operator[](int i) const{ return component[i]; }  //ith component
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

typedef vector<double,2> point;

typedef vector<double,3> point3;

template<class T, int N, int M> class matrix : public vector<vector<T,N>,M>{
  public:
    matrix(){}
    matrix(const vector<vector<T,N>,M>&){}
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
    ~matrix(){}  //  destructor
    const T& operator()(int i,int j) const{return (*this)[j][i];}//A(i,j)
    const matrix& operator+=(const matrix&);
    const matrix& operator-=(const matrix&);
    const matrix& operator*=(const T&);
    const matrix& operator/=(const T&);
};

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

typedef matrix<double,2,2> matrix2;

typedef matrix<double,3,3> matrix3;

double real(const matrix2&A){
  return A(0,0);
}  //  upper left element

double det(const matrix2&A){
  return A(0,0)*A(1,1) - A(0,1)*A(1,0);
}  //  determinant of matrix

double fabs(const matrix2&A){
  return sqrt(fabs(det(A)));
}  //  upper left element

double det(const matrix3&A){
  return A(0,0) * (A(1,1)*A(2,2)-A(1,2)*A(2,1)) 
      - A(0,1) * (A(1,0)*A(2,2)-A(1,2)*A(2,0)) 
      + A(0,2) * (A(1,0)*A(2,1)-A(1,1)*A(2,0));
}  //  determinant of matrix3

const point3 operator&(const point3&u, const point3&v){
  point3 i(1.,0.,0.);
  point3 j(0.,1.,0.);
  point3 k(0.,0.,1.);
  return i * (u[1]*v[2]-u[2]*v[1])
       - j * (u[0]*v[2]-u[2]*v[0])
       + k * (u[0]*v[1]-u[1]*v[0]);
}  //  vector product

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

const matrix2 transpose(const matrix2&A){
  return matrix2(point(A(0,0),A(0,1)),point(A(1,0),A(1,1)));
}  //  transpose of a matrix

const matrix3 transpose(const matrix3&A){
  return matrix3(point3(A(0,0),A(0,1),A(0,2)),point3(A(1,0),A(1,1),A(1,2)),point3(A(2,0),A(2,1),A(2,2)));
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
    int getSharingCells() const{return sharingCells;}  //  read it
    void moreSharingCells(){sharingCells++;}  //  increase it
    int lessSharingCells(){
      return
        sharingCells ?
          !(--sharingCells)
        :
          1;
    }  //  decrease it
    int noSharingCell() const{return !sharingCells;}//dangling node
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
	  n.getIndex(),n.getSharingCells());
    }  //  print a node

template<class T, int N> class cell{
  node<T>* vertex[N];
public:
  cell(){
    for(int i=0; i<N; i++)
      vertex[i] = new node<T>(0.,-1,1);
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
    vertex[0] = a.noSharingCell() ? new node<T>(a) : &a;
    vertex[1] = b.noSharingCell() ? new node<T>(b) : &b;
    vertex[2] = c.noSharingCell() ? new node<T>(c) : &c;
    for(int i=0; i<N; i++)
      vertex[i]->moreSharingCells();
  }  //  constructor

  template<class T, int N>
  cell<T,N>::cell(node<T>&a, node<T>&b, node<T>&c, node<T>&d){
    vertex[0] = a.noSharingCell() ? new node<T>(a) : &a;
    vertex[1] = b.noSharingCell() ? new node<T>(b) : &b;
    vertex[2] = c.noSharingCell() ? new node<T>(c) : &c;
    vertex[3] = d.noSharingCell() ? new node<T>(d) : &d;
    for(int i=0; i<N; i++)
      vertex[i]->moreSharingCells();
  }  //  constructor

  template<class T, int N>
  cell<T,N>::cell(cell<T,N>&e){
    for(int i=0; i<N; i++){
      vertex[i] = e.vertex[i];
      vertex[i]->moreSharingCells();
    }
  }  //  copy constructor

  template<class T, int N> const cell<T,N>&
  cell<T,N>::operator=(cell<T,N>&e){
    if(this != &e){
      for(int i=0; i<N; i++)
        if(vertex[i]->lessSharingCells())delete vertex[i];
      for(int i=0; i<N; i++){
        vertex[i] = e.vertex[i];
        vertex[i]->moreSharingCells();
      }
    }
    return *this;
  }  //  assignment operator

  template<class T, int N>
  cell<T,N>::~cell(){
    for(int i=0; i<N; i++)
      if(vertex[i]->lessSharingCells())delete vertex[i];
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
  const T operator&&(const dynamicVector<T>&u, const dynamicVector<T>&v){
      T sum = 0;
      for(int i = 0; i < u.dim(); i++)
	sum += u[i] * v[i];
      return sum;
  }  //  real inner product

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
    void refineNeighbors(node<point3>&, node<point3>&, node<point3>&);
    void refine(const dynamicVector<point>&, const dynamicVector<int>&, double);
    double jumpSum(const dynamicVector<point>&);
    double jumpMax(const dynamicVector<point>&);
};

template<class T>
int mesh<T>::indexing(){
  for(mesh<T>* runner = this; runner; runner=(mesh<T>*)runner->next)
    runner->item.resetIndices();
  int count=0;
  for(mesh<T>* runner = this; runner; runner=(mesh<T>*)runner->next)
    runner->item.indexing(count);
  return count;
}  //  indexing a mesh

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
	     (l2norm(v[item[i].getIndex()] - v[item[j].getIndex()])>threshold)){
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

    double mesh<tetrahedron>::jumpSum(const dynamicVector<point>&v){
      double sum = 0.;
      int counter = 0;
      for(mesh<tetrahedron>* runner = this; runner; runner=(mesh<tetrahedron>*)runner->next)
        for(int i=0; i<4; i++)
	  for(int j=i+1; j<4; j++){
	    sum += l2norm(v[runner->item[i].getIndex()] - v[runner->item[j].getIndex()]);
	    counter++;
	  }
      return sum / counter;
    }

    double mesh<tetrahedron>::jumpMax(const dynamicVector<point>&v){
      double maximum = 0.;
      for(mesh<tetrahedron>* runner = this; runner; runner=(mesh<tetrahedron>*)runner->next)
        for(int i=0; i<4; i++)
	  for(int j=i+1; j<4; j++)
	    maximum = max(maximum,l2norm(v[runner->item[i].getIndex()] - v[runner->item[j].getIndex()]));
      return maximum;
    }

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
  const rowElement& operator*=(const T&t){
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

  template<class T>
  const rowElement<T> operator*(const rowElement<T>&e, const T&t){
    return rowElement<T>(e) *= t;
  }  //   rowElement times a T

  template<class T>
  const rowElement<T> operator*(const T&t, const rowElement<T>&e){
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
    row(const dynamicVector<T>&v, double threshold=0.){
      row<T>* runner = this;
      int first = 1;
      for(int i=0; i<v.dim(); i++)
        if(fabs(v[i])>threshold){
	  if(!first){
	    runner->next = new row<T>;
	    runner = (row<T>*)runner->next;
	  }
	  runner->item = rowElement<T>(v[i],i);
	  first = 0;
	}
    }  //  constructor from a dynamic vector
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
          next&&(getColumn() < i) ? (*(row*)next)[i] : 0.;
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
    const row& operator*=(const T&t){
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
    }  //  row times vector (real inner product)

    const T operator*(const row<T>&r) const{
      return getColumn() > r.getColumn() ?
               r * *this
	     :
	       !next ?
                 getColumn() == r.getColumn() ?
	           getValue() * r.getValue() 
                 :
		   0
               :
                 getColumn() == r.getColumn() ?
                 getValue() * r.getValue() + r * *(row<T>*)readNext()
	       :
                 r * *(row<T>*)readNext();
    }  //  row times row (real inner product)
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
  void operator+=(dynamicVector<T>&v, const row<T>&r){
    for(const row<T>* runner = &r; runner;
        runner = (const row<T>*)runner->readNext())
      v(runner->getColumn()) += runner->getValue();
  }  //  adding a row to a dynamic vector

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
  const list<T>& operator=(const list<T>&);
  ~list(){
    for(int i=0; i<number; i++)
      delete item[i];
    delete [] item;
  }  //   destructor
  int size() const{ return number; }  //  list size
  T& operator()(int i){ if(item[i])return *(item[i]); }  //read/write ith item
  const T& operator[](int i)const{if(item[i])return *(item[i]);}// read only
};
  template<class T>
  list<T>::list(const list<T>&l):number(l.number),
        item(l.number ? new T*[l.number] : 0){
    for(int i=0; i<l.number; i++)
      if(l.item[i]) item[i] = new T(*l.item[i]);
  }  //  copy constructor

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
    const T operator()(const S&) const;
  template<class S>
    const S operator()(const S&, const S&) const;
  template<class S>
    const S operator()(const S&, const S&, const S&) const;
};

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

template<class T> class sparseMatrix:public list<row<T> >{
 public:
  sparseMatrix(int n=0){
    number = n;
    item = n ? new row<T>*[n] : 0;
    for(int i=0; i<n; i++)
      item[i] = 0;
  }  //  constructor
  sparseMatrix(int n, const T&a){
    number = n;
    item = n ? new row<T>*[n] : 0;
    for(int i=0; i<n; i++)
      item[i] = new row<T>(a,i);
  }  //  constructor with 'T' argument
  sparseMatrix(int,mesh<tetrahedron>&, const dynamicVector<point>&, dynamicVector<point>&,dynamicVector<int>&);
  template<class S>
  void setDirichlet(dynamicVector<S>&,dynamicVector<int>&);

  void readSparseMatrix(int);
  ~sparseMatrix(){}  //  destructor
  row<T>& operator()(int i){
    return list<row<T> >::operator()(i);
  }  //  read/wirte ith row
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
  const sparseMatrix& operator/=(const dynamicVector<T>&D){
    for(int i=0; i<rowNumber(); i++)
      *item[i] /= D[i];
    return *this;
  }  //  divide the rows by the components of a vector
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

  void setExponent(mesh<tetrahedron>&m, dynamicVector<point>&expikx){
    for(const mesh<tetrahedron>* runner = &m; runner;
	    runner=(const mesh<tetrahedron>*)runner->readNext())
      for(int i=0; i<4; i++){
        int I = (*runner)()[i].getIndex();
	expikx(I) = point(cos(sqrt(fabs(HELM))*(*runner)()[i]()[0]),-sin(sqrt(fabs(HELM))*(*runner)()[i]()[0]));
      }
  }

  template<class T>
  sparseMatrix<T>::sparseMatrix(int level,mesh<tetrahedron>&m, const dynamicVector<point>&x,dynamicVector<point>&f,dynamicVector<int>&DirichletBoundary){
    dynamicVector<int> xBoundary(DirichletBoundary.dim(),0);
    dynamicVector<int> yBoundary(DirichletBoundary.dim(),0);
    dynamicVector<int> zBoundary(DirichletBoundary.dim(),0);
    item = new row<T>*[number = m.indexing()];
    f = 0.;
    for(int i=0; i<number; i++)
      item[i] = 0;
    point3 gradient[4];
    gradient[0] = point3(-1,-1,-1);
    gradient[1] = point3(1,0,0);
    gradient[2] = point3(0,1,0);
    gradient[3] = point3(0,0,1);
    polynomial<double> zero(1,0.);
    polynomial<polynomial<double> > Zero(1,zero);
    polynomial<double> one(1,1.);
    polynomial<polynomial<double> > One(1,one);
    polynomial<double> minus1(1,-1.);
    polynomial<polynomial<double> > Minus1(1,minus1);
    polynomial<double> oneMinusx(1.,-1.);
    polynomial<polynomial<double> > oneMinusxMinusy(oneMinusx,minus1);
    polynomial<polynomial<double> > yy(zero,one);
    polynomial<double> x1(0.,1.);
    polynomial<polynomial<double> > xx(1,x1);
    polynomial<polynomial<polynomial<double> > > xxx(1,xx);
    list<polynomial<polynomial<polynomial<double> > > > P(4,xxx);
    P(0) = polynomial<polynomial<polynomial<double> > >(oneMinusxMinusy,Minus1);
    P(2) = polynomial<polynomial<polynomial<double> > >(1,yy);
    P(3) = polynomial<polynomial<polynomial<double> > >(Zero, One);
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
      matrix3 weight=fabs(det(S)/6.) * Sinverse * transpose(Sinverse);
      for(int i=0; i<4; i++){
        int I = (*runner)()[i].getIndex();
        if((*runner)()[i]()[0] <= 1.e-6){
	  xBoundary(I) = 1;
	}
        if((*runner)()[i]()[0] >= 1. - 1.e-6){
	  xBoundary(I) = 2;
	}
	if(level>=nonlinearLevels){
          if((*runner)()[i]()[1] <= 1.e-6){
	    yBoundary(I) = 1;
	  }
          if((*runner)()[i]()[1] >= 1. - 1.e-6){
	    yBoundary(I) = 2;
	  }
          if((*runner)()[i]()[2] <= 1.e-6){
	    zBoundary(I) = 1;
	  }
          if((*runner)()[i]()[2] >= 1. - 1.e-6){
	    zBoundary(I) = 2;
	  }
          X += x[I][0] * P[i];
          Y += x[I][1] * P[i];
	}
      }
      if(level>=nonlinearLevels){
        polynomial<polynomial<polynomial<double> > > X2 = X * X;
        polynomial<polynomial<polynomial<double> > > Y2 = Y * Y;
	X2pY2 = X2 + Y2;
	threeX2pY2 = 3. * X2 + Y2;
	X2pThreeY2 = X2 + 3. * Y2;
	twoXY = 2. * X * Y;
      }
      double helmNonlin=HELMNonlin*fabs(det(S));
      for(int i=0; i<4; i++){
        int I = (*runner)()[i].getIndex();
	if((DirichletBoundary[I])&&
	    (level>=nonlinearLevels))
	  f(I) += helmNonlin * point(integral(X2pY2 * X * P[i]),integral(X2pY2 * Y * P[i]));
	for(int j=0; j<4; j++){
          int J = (*runner)()[j].getIndex();
          polynomial<polynomial<polynomial<double> > > Pij = P[i] * P[j];
          double helm=HELM*fabs(det(S))/120.;
	  if(i==j)helm *= 2.;
	  if(item[I]){
	    row<T> r(helm+gradient[j]*weight*gradient[i],J);
	    *item[I] += r;
	  }
	  else
	    item[I] = new row<T>(helm+gradient[j]*weight*gradient[i],J);
	  if(DirichletBoundary[I])
	    f(I) += T(helm+gradient[j]*weight*gradient[i]) * x[J];
	  for(int k=0; k<4; k++){
            int K = (*runner)()[k].getIndex();
	    if((i!=j)&&(j!=k)&&(k!=i)
	      &&((xBoundary[I]*xBoundary[J]*xBoundary[K]==1)||(xBoundary[I]*xBoundary[J]*xBoundary[K]==8)
	      ||(yBoundary[I]*yBoundary[J]*yBoundary[K]==1)||(yBoundary[I]*yBoundary[J]*yBoundary[K]==8)
	      ||(zBoundary[I]*zBoundary[J]*zBoundary[K]==1)||(zBoundary[I]*zBoundary[J]*zBoundary[K]==8))){
              point3 jMinusi= (*runner)()[j]() - (*runner)()[i]();
              point3 kMinusi= (*runner)()[k]() - (*runner)()[i]();
              double radiation = sqrt(fabs(HELM)) * l2norm(jMinusi&kMinusi) / 24.;
	      T radiationMatrix(point(0.,radiation),point(-radiation,0.));
	      row<T> radiationI(radiationMatrix,I);
	      row<T> radiationJ(radiationMatrix,J);
	      *item[I] += radiationI;
	      *item[I] += radiationJ;
	      if(DirichletBoundary[I]){
	        f(I) += radiationMatrix * x[I];
	        f(I) += radiationMatrix * x[J];
	      }
	      if(xBoundary[I]*xBoundary[J]*xBoundary[K]==1)
	        f(I) -= point(0.,4. * radiation);
	    }
	  }
          if(level>=nonlinearLevels){
	    point J1(integral(threeX2pY2 * Pij),
	      integral(twoXY * Pij));
	    point J2(J1[1], integral(X2pThreeY2 * Pij));
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

  template<class T>
  const dynamicVector<T>
  operator*(const sparseMatrix<T>&M,const row<T>&r){
    dynamicVector<T> result(M.rowNumber(),0.);
    for(int i=0; i<M.rowNumber(); i++)
      result(i) = M[i] * r;
    return result;
  }  //  matrix times row

  template<class T, class S>
  const dynamicVector<S>
  operator*(const sparseMatrix<T>&M,const dynamicVector<S>&v){
    dynamicVector<S> result(M.rowNumber(),0.);
    for(int i=0; i<M.rowNumber(); i++)
      result(i) = M[i] * v;
    return result;
  }  //  matrix times vector

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
  }  //  matrix times matrix

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
   if(useILU==1){
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
   }
   if(useILU==2){
    sparseMatrix<T> At = transpose(*this);
    sparseMatrix<T> Wt(rowNumber(),1.);
    sparseMatrix<T> Zt(rowNumber(),1.);
    dynamicVector<T> D(rowNumber(),0.);
    for(int i=0; i<rowNumber(); i++){
      row<T> r =  *item[i];
      for(int j=0; j<i; j++){
        row<T> wj = -1. * (r * Zt[j]) / D[j] * Wt[j];
        Wt(i) += wj;
      }
      row<T> c =  At[i];
      for(int j=0; j<i; j++){
	row<T> zj = -1. * (Wt[j] * c) /D[j] * Zt[j];
	Zt(i) += zj;
      }
      D(i) = Wt[i] * (*this * Zt[i]);
      Zt(i).truncateItems(threshold,matrix2(1.));
      Wt(i).truncateItems(threshold,matrix2(1.));
    }
    *this = transpose(Zt);
    return Wt /= D;
   }
   if(useILU==3){
    sparseMatrix<T> At = transpose(*this);
    sparseMatrix<T> Wt(rowNumber());
    sparseMatrix<T> Zt(rowNumber());
    dynamicVector<T> D(rowNumber(),0.);
    for(int i=0; i<rowNumber(); i++){
      row<T> r =  *item[i];
      //dynamicVector<T> r(rowNumber(),0.);
      //r += *item[i];
      dynamicVector<T> wi(rowNumber(),0.);
      wi(i) = 1.;
      dynamicVector<T> zi(rowNumber(),0.);
      zi(i) = 1.;
      for(int j=0; j<i; j++){
        row<T> wj = -1. * (Zt[j] * r) / D[j] * Wt[j];
        wi += wj;
      }
      row<T> c =  At[i];
      //dynamicVector<T> c(rowNumber(),0.);
      //c +=  At[i];
      for(int j=0; j<i; j++){
	row<T> zj = -1. * (Wt[j] * c) / D[j] * Zt[j];
	zi += zj;
      }
      D(i) = wi && (*this * zi);
      Wt.item[i] = new row<T>(wi,threshold);
      Zt.item[i] = new row<T>(zi,threshold);
      //D(i) = Wt[i] * (*this * Zt[i]);
    }
    *this = transpose(Zt);
    return Wt /= D;
   }
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
    if(useILU==1)
      x += U.backSubstitution(L.forwardElimination(f - A * x));
    else
      x += U * (L * (f - A * x));
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
	multigrid<T>::Vcycle(const dynamicVector<S>&f,
		   dynamicVector<S>&x) const{
	   if(next){
	     for(int i=0; i<Nu1; i++){
	       if(useILU)
	         ILU(A,L,U,f,x);
	       else
                 GaussSeidel(A,f,x);
	     }
	     dynamicVector<S> residual = f - A * x;
	     dynamicVector<S> correction(R.rowNumber(), 0.);
	     for(int i=0; i<cycleIndex; i++)
	       next->Vcycle(R * residual, correction);
	     x += P * correction;
	     for(int i=0; i<Nu2; i++){
	       if(useILU)
	         ILU(A,L,U,f,x);
	       else
	         reversedGaussSeidel(A,f,x);
	     }
	   }
	   else
	     for(int i=0; i<NuCoarse; i++){
	       if(useILU)
	         ILU(A,L,U,f,x);
	       else
	         symmetricGaussSeidel(A,f,x);
	     }
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
	 dynamicVector<T> rr(MG.Vcycle(f,keep) - x);
	 dynamicVector<T> r = f - A * x;
	 dynamicVector<T> pp(rr);
	 dynamicVector<T> p = r;
	 double gamma = r * rr;
	 double gammainit = gamma;
	 int count = 0;
	 while((fabs(gamma/gammainit) >= thresholdCG * thresholdCG)&&
	        (count <= iterationnumber)){
	   keep = pp;
	   dynamicVector<T> ww = pp - MG.Vcycle(zero,keep);
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
           printf("at MG iteration %d in PCG, (r,Pr)=%f\n",count,gamma);
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
      dynamicVector<point> f(m.indexing(),point(0.,0.));
      dynamicVector<point> solution(f.dim(),point(0.,0.));
      dynamicVector<point> expikx(f.dim(),point(0.,0.));
      setExponent(m,expikx);
      dynamicVector<int> DirichletBoundary(f.dim(),1);
      if(i>=refineLevels){
	if(i>=nonlinearLevels)
	  Newton = 2;
	if(i>=nonlinearLevels)
          HELMNonlin -= 20.;
        for(int n=0; n<Newton; n++){
          sparseMatrix<matrix2> A(i,m,solution,f,DirichletBoundary);
          A.setDirichlet(f,DirichletBoundary);
          if(PrintMesh&&(i==adaptiveLevels-5)){
	    printf("cell number=%d\n",m.length());
	    print(m);
	    print(A);
	    print(f);
	    return 0;
          }
          multigrid<matrix2> MG(A);
          dynamicVector<point> x(f.dim(),0.);
          GMRES(MG,A,0,100,f,x);
	  solution -= x;
	  printf("\n");
	  printf("at Newton iteration %d res = %f\n",n,sqrt(f*f/f.dim()));
	  if(i<nonlinearLevels)
	    printf("at Newton iteration %d err = %f\n",n,sqrt((solution-expikx)*(solution-expikx)/f.dim()));
	  printf("\n");
        }
        thresholdAdaptive = max(1. * m.jumpSum(solution),0.5 * m.jumpMax(solution));
      }
      if(PrintSol) print(solution);
      printf("at level %d, number of nodes=%d\n",i+1,solution.dim());
      if(i<adaptiveLevels-1)m.refine(solution, DirichletBoundary, thresholdAdaptive);
    }
    return 0;
  }
