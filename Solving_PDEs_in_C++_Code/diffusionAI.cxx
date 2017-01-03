//  THE DIFFUSION SOLVER (CHAPTER 19)
//  ---------------------------------
//  This program solves the diffusion equation in the
//  complicated domain in Chapter 19. The adaptive
//  refinement algorithm is used in 8 levels,
//  with automatic boundary refinement at the circular
//  part of the boundary and  multigrid,
// ILU, or approximate inverse preconditioner.
//  The parameter "HELM" can be set to a positive
//  number to simulate implicit time marching
//  in the time-dependent case.
//  The detailed documentation is in the book.

#include<stdio.h>
#include<math.h>

const int PrintMesh=0;
const int PrintX=0;
const int Poisson=0;
const int Regular=0;
const int boundaryLevels=1;
const int adaptiveLevels=40;
const int Dirichlet=1;
const double Normal = 1.;
const double SlitWidth=.2;
const double SlitLength=.5;
double HELM=.0;
const int RowSumStablizer=0;
const double Stablizer=0.0;
const double DiffX=100.;
const double DiffY=1000.;
double thresholdAdaptive=0.005;
const double thresholdILU=0.01;
const int useILU=0;
const double thresholdCG=1.e-6;
const double ratioMG=.95;
const int AMG=0;
const int Circle=1;
const double thresholdMG=0.05;
const double FactorFine=1.;
const double FactorCoarse=.0;
const int ThrowQ=0;
const int Smooth=0;
const int TruncateAtilde=0;
const int ThrowAtilde=0;
const int Nu1=1;
const int Nu2=1;
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
  //~vector(){delete [] component;}  //  destructor
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
  T squaredNorm(const vector<T,N>&u){
      return u*u;
  }  //  squared l2 norm

  template<class T, int N>
  void print(const vector<T,N>&v){
    printf("\\put(%f,%f){\\circle*{0.005}}\n",v[0]+1.,v[1]+1.);
  }  //  printing a vector

template<class T, int N, int M> class matrix : public vector<vector<T,N>,M>{
  public:
    matrix(){}
    matrix(const vector<T,N>&u, const vector<T,N>&v){
      set(0,u);
      set(1,v);
    }  //  constructor
    const T& operator()(int i,int j) const{return (*this)[j][i];}//A(i,j)
    const matrix& operator*=(const T&);
    const matrix& operator/=(const T&);
};

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

typedef vector<double,2> point;

typedef vector<double,3> point3d;

typedef matrix<double,2,2> matrix2;

typedef matrix<double,3,3> matrix3;

double det(const matrix2&A){
  return A(0,0)*A(1,1) - A(0,1)*A(1,0);
}  //  determinant of matrix

const matrix2 inverse(const matrix2&A){
  point column0(A(1,1),-A(1,0));
  point column1(-A(0,1),A(0,0));
  return matrix2(column0,column1)/det(A);
}  //  inverse of matrix

const matrix2 transpose(const matrix2&A){
  return matrix2(point(A(0,0),A(0,1)),point(A(1,0),A(1,1)));
}  //  transpose of a matrix

template<class T> class node{
    T location;
    int index;
    int sharingElements;
  public:
    node(const T&loc=0., int ind=-1, int sharing=0):
      location(loc),index(ind),sharingElements(sharing){}  //  constructor
    node(const node&n):location(n.location),index(n.index),
      sharingElements(n.sharingElements){}  //  copy constructor
    const node& operator=(const node&);
    ~node(){}  //  destructor
    const T& operator()() const{return location;}  //  read the location
    int getIndex() const{return index;}  //  read index
    void setIndex(int i){index=i;}  //  set index
    int getSharingElements() const{return sharingElements;}  //  read it
    void moreSharingElements(){sharingElements++;}  //  increase it
    int lessSharingElements(){
      return
        sharingElements ?
          !(--sharingElements)
        :
          1;
    }  //  decrease it
    int noSharingElement() const{return !sharingElements;}//dangling node
};

    template<class T>
    const node<T>& node<T>::operator=(const node<T>&n){
      if(this != &n){
	location = n.location;
	index = n.index;
	sharingElements = n.sharingElements;
      }
      return *this;
    }  //  assignment operator

    template<class T>
    void print(const node<T>&n){
      print(n());
    }  //  print a node

template<class T, int N> class finiteElement{
  node<T>* vertex[N];
public:
  finiteElement(){
    for(int i=0; i<N; i++)
      vertex[i] = new node<T>(0.,-1,1);
  }  //  default constructor
  finiteElement(node<T>&,node<T>&,node<T>&);
  finiteElement(finiteElement<T,N>&);
  const finiteElement<T,N>& operator=(finiteElement<T,N>&);
  ~finiteElement();
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
  double p() const{
    if(Poisson) return 1.;
    for(int i=0; i<N; i++)
      if(((*vertex[i])()[0]>0.)||((*vertex[i])()[1]>0.)) return 1.;
    return DiffX;
  }  //  coefficient of x-derivative
  double q() const{
    if(Poisson) return 1.;
    for(int i=0; i<N; i++)
      if(((*vertex[i])()[0]>0.)||((*vertex[i])()[1]>0.)) return 1.;
    return DiffY;
  }  //  coefficient of y-derivative
};

  template<class T, int N>
  finiteElement<T,N>::finiteElement(node<T>&a, node<T>&b, node<T>&c){
    vertex[0] = a.noSharingElement() ? new node<T>(a) : &a;
    vertex[1] = b.noSharingElement() ? new node<T>(b) : &b;
    vertex[2] = c.noSharingElement() ? new node<T>(c) : &c;
    for(int i=0; i<N; i++)
      vertex[i]->moreSharingElements();
  }  //  constructor

  template<class T, int N>
  finiteElement<T,N>::finiteElement(finiteElement<T,N>&e){
    for(int i=0; i<N; i++){
      vertex[i] = e.vertex[i];
      vertex[i]->moreSharingElements();
    }
  }  //  copy constructor

  template<class T, int N> const finiteElement<T,N>&
  finiteElement<T,N>::operator=(finiteElement<T,N>&e){
    if(this != &e){
      for(int i=0; i<N; i++)
        if(vertex[i]->lessSharingElements())delete vertex[i];
      for(int i=0; i<N; i++){
        vertex[i] = e.vertex[i];
        vertex[i]->moreSharingElements();
      }
    }
    return *this;
  }  //  assignment operator

  template<class T, int N>
  finiteElement<T,N>::~finiteElement(){
    for(int i=0; i<N; i++)
      if(vertex[i]->lessSharingElements())delete vertex[i];
  }  //   destructor

  template<class T, int N>
  int operator<(const node<T>&n, const finiteElement<T,N>&e){
    for(int i=0; i<N; i++)
      if(&n == &(e[i]))return i+1;
    return 0;
  }  //  check whether a node n is in a finite element e

  template<class T, int N>
  void print(const finiteElement<T,N>&e){
    for(int i=0; i<N; i++)
      print(e[i]);
  }  //  printing a finiteElement

typedef finiteElement<point,3> triangle;

typedef finiteElement<point3d,4> tetrahedron;

template<class T> class dynamicVector{
protected:
  int dimension;
  T* component;
public:
  dynamicVector(int, const T&);
  dynamicVector(const dynamicVector&);
  const dynamicVector& operator=(const dynamicVector&);
  const dynamicVector& operator=(const T&);
  ~dynamicVector(){
    delete [] component;
  }  //  destructor
  int dim() const{ return dimension; }  //  return the dimension
  T& operator()(int i){ return component[i]; }  //read/write ith component
  const T& operator[](int i) const{ return component[i]; }  //read only
  const dynamicVector& operator+=(const dynamicVector&);
  const dynamicVector& operator-=(const dynamicVector&);
  const dynamicVector& operator*=(const T&);
  const dynamicVector& operator/=(const T&);
};

  template<class T>
  const T operator&&(const dynamicVector<T>&u, const dynamicVector<T>&v){
      T sum = 0;
      for(int i = 0; i < u.dim(); i++)
	sum += u[i] * v[i];
      return sum;
  }  //  real inner product

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
  const dynamicVector<T>& dynamicVector<T>::operator*=(const T& a){
      for(int i = 0; i < dimension; i++)
	component[i] *= a;
      return *this;
  }  //  multiplying the current dynamicVector by a scalar

  template<class T>
  const dynamicVector<T>& dynamicVector<T>::operator/=(const T& a){
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

  template<class T>
  const dynamicVector<T> operator*(const dynamicVector<T>&u, const T& a){
    return dynamicVector<T>(u) *= a;
  }  //  dynamicVector times scalar

  template<class T>
  const dynamicVector<T> operator*(const T& a, const dynamicVector<T>&u){
    return dynamicVector<T>(u) *= a;
  }  //  T times dynamicVector

  template<class T>
  const dynamicVector<T> operator/(const dynamicVector<T>&u, const T& a){
    return dynamicVector<T>(u) /= a;
  }  //  dynamicVector divided by scalar

  template<class T>
  const dynamicVector<T> operator-(const dynamicVector<T>&u){
    return dynamicVector<T>(u) *= -1.;
  }  //  negative of a dynamicVector

  template<class T>
  T operator*(const dynamicVector<T>&u, const dynamicVector<T>&v){
      T sum = 0;
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

template<class T> class connectedList{
protected:
  T item;
  connectedList* next;
public:
    T& operator()(int i){
      return (item.getColumn() == i) ? item :
          next&&(item.getColumn() < i) ? (*next)(i) : *(new T(0.));
    }  //  read/write the item at column i
  const T& operator()() const{return item;}  //  read "item" field
  const connectedList* readNext() const{return next;}  //  read "next" field
  connectedList():next(0){}  //  default constructor
  connectedList(T&t, connectedList* N=0)
    :item(t),next(N){}  //  constructor
  connectedList(const connectedList&l):item(l()),next(
    l.next ? new connectedList(*l.next) : 0){}  //  copy constructor
  ~connectedList(){delete next; next = 0;}  //  destructor
  const connectedList& operator=(const connectedList&);
  connectedList& last(){return next ? next->last() : *this;}  //  last item
  int length() const{return next ? next->length() + 1 : 1;}//no. of items
  void append(T&t){last().next = new connectedList(t);}//append an item
  void insertNextItem(T&t){next = new connectedList(t,next);}//insert an item
  void insertFirstItem(T&t){
    next = new connectedList(item,next);
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
  double maskItemsSum(const dynamicVector<int>&, const dynamicVector<double>&);
  const connectedList& operator+=(connectedList&);
  connectedList& order(int);
};

  template<class T>
  const connectedList<T>&connectedList<T>::operator=(const connectedList<T>&L){
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
        if(L.next)next = new connectedList(*L.next);
   }
   return *this;
  }  //  assignment operator

  template<class T>
  void connectedList<T>::dropNextItem(){
     if(next){
       if(next->next){
         connectedList<T>* keep = next;
         next = next->next;
         keep->item.~T();
       }
       else{
         delete next;
         //next->item.~T();
         next = 0;
       }
     }  //  drop the second item from the connected list
     else
       printf("error: cannot drop next element\n");
  }

  template<class T>
  void connectedList<T>::dropFirstItem(){
      if(next){
	item = next->item;
	dropNextItem();
      }
      else
        printf("error: cannot drop first element\n");
  }  //  drop the first item in the connected list

  template<class T>
  template<class S>
  const S connectedList<T>::truncateItems(double threshold, const S& compare){
    S sum=0.;
    if(next){
      int dropped = 0;
      for(connectedList<T>* iterator = this;
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
  const S connectedList<T>::dropPositiveItems(
          int diag, const S&center, double threshold){
    S sum=0.;
    if(next){
      int dropped = 0;
      for(connectedList<T>* iterator = this;
          iterator->next;
	  iterator = dropped ? iterator : iterator->next){
	dropped = 0;
        if((double(iterator->next->item.getValue() / center) >= -threshold)&&
	    (iterator->next->item.getColumn() != diag)){
	  sum += iterator->next->item.getValue();
	  iterator->dropNextItem();
	  dropped = 1;
	}
      }
    }
    if(next&&(double(item.getValue() / center) >= -threshold)&&(item.getColumn() != diag)){
      sum += item.getValue();
      dropFirstItem();
    }
    return sum;
  }  //  truncate positive off-diagonal items

  template<class T>
  template<class S>
  const S connectedList<T>::maskItems(const dynamicVector<int>& mask, const S&){
    S sum=0.;
    if(next){
      int dropped = 0;
      for(connectedList<T>* iterator = this;
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
  double connectedList<T>::maskItemsSum(const dynamicVector<int>& mask, const dynamicVector<double>&f){
    double sum=0.;
    if(next){
      int dropped = 0;
      for(connectedList<T>* iterator = this;
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

  template<class T> const connectedList<T>&
  connectedList<T>::operator+=(connectedList<T>&L){
    connectedList<T>* runner = this;
    connectedList<T>* Lrunner = &L;
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
    if(Lrunner)runner->next = new connectedList<T>(*Lrunner);
    return *this;
  }  //  merge two connected lists

  template<class T>
  connectedList<T>& connectedList<T>::order(int length){
    if(length>1){
      connectedList<T>* runner = this;
      for(int i=0; i<length/2-1; i++)
	runner = runner->next;
      connectedList<T>* second = runner->next;
      runner->next = 0;
      order(length/2);
      *this += second->order(length-length/2);
    }
    return *this;
  }  //  order a disordered connected list

template<class T>
void print(const connectedList<T>&l){
  print(l());
  if(l.readNext())print(*l.readNext());
}  //  print a connected list

template<class T> class mesh:public connectedList<T>{
  public:
    mesh(){}  //  default constructor
    mesh(T&e){item = e;}  //  constructor
    int indexing();
    void refineNeighbor(node<point>&, node<point>&, node<point>&);
    void refine(const dynamicVector<double>&, const dynamicVector<int>&, double);
    void refineBoundary(int);
    double maxNorm(const dynamicVector<double>&);
    double jumpSum(const dynamicVector<double>&);
    double jumpMax(const dynamicVector<double>&);
};

    double mesh<triangle>::jumpSum(const dynamicVector<double>&v){
      double sum = 0.;
      int counter = 0;
      for(mesh<triangle>* runner = this; runner; runner=(mesh<triangle>*)runner->next)
        for(int i=0; i<3; i++)
	  for(int j=i+1; j<3; j++){
	    sum += fabs(v[runner->item[i].getIndex()] - v[runner->item[j].getIndex()]);
	    counter++;
	  }
      return sum / counter;
    }

    double mesh<triangle>::jumpMax(const dynamicVector<double>&v){
      double maximum = 0.;
      for(mesh<triangle>* runner = this; runner; runner=(mesh<triangle>*)runner->next)
        for(int i=0; i<3; i++)
	  for(int j=i+1; j<3; j++)
	    maximum = max(maximum,fabs(v[runner->item[i].getIndex()] - v[runner->item[j].getIndex()]));
      return maximum;
    }

template<class T>
int mesh<T>::indexing(){
  for(mesh<T>* runner = this; runner; runner=(mesh<T>*)runner->next)
    runner->item.resetIndices();
  int count=0;
  for(mesh<T>* runner = this; runner; runner=(mesh<T>*)runner->next)
    runner->item.indexing(count);
  return count;
}  //  indexing finite-element mesh

    void mesh<triangle>::refineNeighbor(node<point>&nI,
	    node<point>&nJ, node<point>&nIJ){
      int ni = nI < item;
      int nj = nJ < item;
      if(ni&&nj){
	ni--;
	nj--;
	int nk = 0;
	while((nk==ni)||(nk==nj))
	  nk++;
	triangle t1(nI,nIJ,item(nk));
	triangle t2(nJ,nIJ,item(nk));
	insertNextItem(t2);
	insertNextItem(t1);
	dropFirstItem();
      }
      else{
        if(next)
          ((mesh<triangle>*)next)->refineNeighbor(nI,nJ,nIJ);
        else
          if((fabs(squaredNorm(nI())-1.)<1.e-6)
               &&(fabs(squaredNorm(nJ())-1.)<1.e-6)){
            node<point> newNode((1./sqrt(squaredNorm(nIJ()))) * nIJ());
            triangle t1(nI,nIJ,newNode);
            triangle t2(nJ,nIJ,t1(2));
            insertNextItem(t2);
            insertNextItem(t1);
          }
      }
    }  //  refine also the neighbor of a refined triangle

    void mesh<triangle>::refine(const dynamicVector<double>&v,
	    const dynamicVector<int>&DirichletBoundary, double threshold){
        for(int i=0; i<3; i++)
	  for(int j=2; j>i; j--)
	    if((item[i].getIndex() >= 0)&&(item[j].getIndex() >= 0)&&
	     (fabs(v[item[i].getIndex()] - v[item[j].getIndex()])>threshold)){
	      int k=0;
	      node<point> itemij = (item[i]()+item[j]())/2.;
	      while((k==i)||(k==j))
	        k++;
	      triangle t1(item(i),itemij,item(k));
	      triangle t2(item(j),t1(1),item(k));
	      if(next)
	        ((mesh<triangle>*)next)->
	                refineNeighbor(item(i),item(j),t1(1));
	      insertNextItem(t2);
	      insertNextItem(t1);
	      dropFirstItem();
	      refine(v,DirichletBoundary, threshold);
	      return;
	    }
	if(next)((mesh<triangle>*)next)->refine(v,DirichletBoundary, threshold);
    }  //  adaptive refinement

    void mesh<triangle>::refineBoundary(int levels){
      mesh<triangle>* runner = this;
      for(int i=1; i<levels; i++)
	for(int j=0; j<power(2,i); j++){
	  point vertex0 = (*runner)()[0]();
	  point vertex1 = (*runner)()[1]();
	  point midpoint = (vertex0 + vertex1)/2.;
	  double angle1 = acos(vertex1[0]);
	  if(j >= power(2,i-1))angle1 = -angle1;
	  double angleIncrement = acos(sqrt(squaredNorm(midpoint)));
	  double angleMidpoint = angle1 - angleIncrement;
	  point newPoint(cos(angleMidpoint),sin(angleMidpoint));
	  node<point> newVertex(newPoint);
	  triangle t1(runner->item(0),newVertex,runner->item(1));
	  append(t1);
	  angleMidpoint = angle1 + angleIncrement;
	  newVertex=node<point>(point(cos(angleMidpoint),sin(angleMidpoint)));
	  triangle t2(runner->item(1),newVertex,runner->item(2));
	  append(t2);
	  runner = (mesh<triangle>*)runner->next;
	}
    }  //  refine at the boundary of a circular domain

    double mesh<triangle>::maxNorm(const dynamicVector<double>&v){
      double result=0.;
      for(int i=0; i<3; i++)
	if(squaredNorm(item[i]()-point(Circle,0))>0.01)
	  result=max(result,fabs(v[item[i].getIndex()]));
      if(next)result=max(result,((mesh<triangle>*)next)->maxNorm(v));
      return result;
    }  //  mzximum of values at nodes away from the singularity at (1,0)

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

template<class T> class row : public connectedList<rowElement<T> >{
  public:
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
    row(const T&val=0,int col=-1){item=rowElement<T>(val,col);}//constructor
    void append(const T&val, int col){
      rowElement<T> e(val,col);
      connectedList<rowElement<T> >::append(e);
    }  //  append a rowElement at the end of row
    const rowElement<T>& operator()() const{return item;}//read first item
    const T& getValue() const{ return item.getValue(); }//first-item value
    int getColumn() const{ return item.getColumn(); }//first-item column
    const T rowSum() const{
      return next ? getValue() + (*(row<T>*)next).rowSum() : getValue();
    }  //  row sum
    rowElement<T>& operator()(int i){
      return (connectedList<rowElement<T> >::operator())(i);
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
      connectedList<rowElement<T> >::insertNextItem(e);
    }  //  insert a rowElement
    void insertFirstItem(const T&val, int col){
      rowElement<T> e(val,col);
      connectedList<rowElement<T> >::insertFirstItem(e);
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
    void renumberColumns(const dynamicVector<int>&renumber){
      item = rowElement<T>(getValue(),renumber[getColumn()]-1);
      if(next)(*(row<T>*)next).renumberColumns(renumber);
    }  //  renumber columns
    //double dropItemsSum(const dynamicVector<int>&, const dynamicVector<T>&);
    //void dropItems(const dynamicVector<int>&);
    //void dropPositiveItems(int, const T&, double);
};

  template<class T>
  void operator+=(dynamicVector<T>&v, const row<T>&r){
    for(const row<T>* runner = &r; runner;
        runner = (const row<T>*)runner->readNext())
      v(runner->getColumn()) += runner->getValue();
  }  //  adding a row to a dynamic vector

  template<class T>
  const T
  operator*(const row<T>&r, const row<T>&s){
    T sum = 0;
    for(const row<T>* runner = &r; runner;
        runner = (const row<T>*)runner->readNext())
      sum += s[runner->getColumn()] * runner->getValue();
    return sum;
  }  //  nonrecursive row times row (real inner product)

  template<class T>
  const T
  operator*(const row<T>&r, const dynamicVector<T>&v){
    T sum = 0;
    for(const row<T>* runner = &r; runner;
        runner = (const row<T>*)runner->readNext())
      sum += v[runner->getColumn()] * runner->getValue();
    return sum;
  }  //  nonrecursive row times vector (real inner product)

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

template<class T> class sparseMatrix:public list<row<T> >{
 public:
  const sparseMatrix& operator/=(const dynamicVector<T>&D){
    for(int i=0; i<rowNumber(); i++)
      *item[i] /= D[i];
    return *this;
  }  //  divide each rows by the components of a vector
  row<T>& operator()(int i){
    return list<row<T> >::operator()(i);
  }  //  read/wirte ith row
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
  sparseMatrix(mesh<triangle>&,dynamicVector<T>&,dynamicVector<int>&);
  void setDirichlet(dynamicVector<T>&,dynamicVector<int>&);

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
      row<T> minus = -1. * *M.item[i];
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
	  const dynamicVector<T>&, dynamicVector<T>&);
  friend void reversedGaussSeidel<T>(const sparseMatrix<T>&,
	  const dynamicVector<T>&, dynamicVector<T>&);
  friend void symmetricGaussSeidel<T>(const sparseMatrix<T>&,
	  const dynamicVector<T>&, dynamicVector<T>&);
  const sparseMatrix factorize(double);
  const dynamicVector<T> forwardElimination(const dynamicVector<T>&)const;
  const dynamicVector<T> backSubstitution(const dynamicVector<T>&)const;
  const sparseMatrix<T> createTransfer();
  const dynamicVector<int> coarsen(double) const;
  const dynamicVector<int> coarsen(const dynamicVector<int>&, double) const;
  void truncatePositive(double);
};

  template<class T>
  const dynamicVector<T>
  operator*(const sparseMatrix<T>&M,const row<T>&r){
    dynamicVector<T> result(M.rowNumber(),0.);
    for(int i=0; i<M.rowNumber(); i++)
      result(i) = M[i] * r;
    return result;
  }  //  matrix times row

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
  sparseMatrix<T>::sparseMatrix(mesh<triangle>&m,dynamicVector<T>&f,dynamicVector<int>&DirichletBoundary){
    item = new row<T>*[number = m.indexing()];
    //f = dynamicVector<T>(number,0.);
    for(int i=0; i<number; i++)
      item[i] = 0;
    point gradient[3];
    gradient[0] = point(-1,-1);
    gradient[1] = point(1,0);
    gradient[2] = point(0,1);
    //DirichletBoundary = dynamicVector<int>(number,1);
    for(const mesh<triangle>* runner = &m; runner;
	    runner=(const mesh<triangle>*)runner->readNext()){
      point diffX((*runner)().p(),0.);
      point diffY(0.,(*runner)().q());
      matrix2 diffCoef(diffX,diffY);
      matrix2 S((*runner)()[1]() - (*runner)()[0](),
	      (*runner)()[2]() - (*runner)()[0]());
      matrix2 Sinverse = inverse(S);
      matrix2 weight=fabs(det(S)/2) * Sinverse * diffCoef * transpose(Sinverse);
      for(int i=0; i<3; i++){
        int I = (*runner)()[i].getIndex();
        if(((*runner)()[i]()[0] <= 1.e-6) &&
           ((*runner)()[i]()[0] >= -SlitLength-1.e-6) &&
           (fabs(fabs((*runner)()[i]()[1]) - SlitWidth/4) <= 1.e-6))
	  {
	    DirichletBoundary(I) = 0;
	    f(I) = sin((*runner)()[i]()[0] +
	      (*runner)()[i]()[1]);
	  }
        if((fabs((*runner)()[i]()[1]) <= SlitWidth/4 + 1.e-6)&&
           (fabs((*runner)()[i]()[0] + SlitLength)<=1.e-6))
	  {
	    DirichletBoundary(I) = 0;
	    f(I) = sin(((*runner)()[i]()[0] +
	      (*runner)()[i]()[1]));
	  }
        if((fabs((*runner)()[i]()[1]) >= SlitWidth/4 - 1.e-6)&&
           (fabs((*runner)()[i]()[1]) <= SlitWidth + 1.e-6)&&
           (fabs((*runner)()[i]()[0])<=1.e-6))
	  {
	    DirichletBoundary(I) = 0;
	    f(I) = sin((*runner)()[i]()[0] +
	      (*runner)()[i]()[1]);
	  }
	for(int j=i; j<3; j++){
          int J = (*runner)()[j].getIndex();
	  double helm=HELM*fabs(det(S))/24.;
	  if(i==j)helm *= 2.;
	  if(item[I]){
	    row<T> r(helm+gradient[j]*weight*gradient[i],J);
	    *item[I] += r;
	  }
	  else
	    item[I] = new row<T>(helm+gradient[j]*weight*gradient[i],J);
	}
	for(int j=0; j<3; j++){
          int J = (*runner)()[j].getIndex();
          double distanceij = sqrt(squaredNorm((*runner)()[i]() - (*runner)()[j]()));
          if((j!=i)&&(distanceij<6./(1.5*power(2,boundaryLevels)))&&((fabs(squaredNorm((*runner)()[i]())-1.)<=1.e-6)
             &&(fabs(squaredNorm((*runner)()[j]())-1.)<=1.e-6))){
            double radiation = Normal * distanceij / 6.;
	    row<T> radiationi(2*radiation,I);
	    row<T> radiationj(radiation/2,J);
	    if(item[I])
	      *item[I] += radiationi;
	    else
	      item[I] = new row<T>(radiationi);
	    *item[I] += radiationj;
	  }
	}
      }
    }
  }  //  assembly of stiffness matrix

  template<class T>
  void sparseMatrix<T>::setDirichlet(dynamicVector<T>&f,dynamicVector<int>&DirichletBoundary){
    if(Dirichlet)
      for(int i=0; i<number; i++){
        if(DirichletBoundary[i])
          f(i) -= item[i]->maskItemsSum(DirichletBoundary,f);
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
  operator*(const sparseMatrix<T>&M,const dynamicVector<T>&v){
    dynamicVector<T> result(M.rowNumber(),0.);
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
	//if(M1.item[i]->readNext())
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
      Zt(i).truncateItems(threshold,1.);
      Wt(i).truncateItems(threshold,1.);
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
	//wj.~row();
      }
      row<T> c =  At[i];
      //dynamicVector<T> c(rowNumber(),0.);
      //c +=  At[i];
      for(int j=0; j<i; j++){
	row<T> zj = -1. * (Wt[j] * c) / D[j] * Zt[j];
	zi += zj;
	//zj.~row();
      }
      D(i) = wi && (*this * zi);
      Wt.item[i] = new row<T>(wi,threshold);
      Zt.item[i] = new row<T>(zi,threshold);
      //D(i) = Wt[i] * (*this * Zt[i]);
      //r.~row();
      //c.~row();
      //wi.~dynamicVector();
      //zi.~dynamicVector();
    }
    *this = transpose(Zt);
    return Wt /= D;
   }
  }  //  incomplete LU factorization and approximate inverse

  template<class T>
  const dynamicVector<T>
  sparseMatrix<T>::forwardElimination(const dynamicVector<T>&f)const{
    dynamicVector<T> result(f.dim(),0.);
    result(0) = f[0];
    for(int i=1; i<f.dim(); i++)
      result(i) = item[i] ? f[i] - *item[i] * result : f[i];
    return result;
  }  //  forward elimination in L

  template<class T>
  const dynamicVector<T>
  sparseMatrix<T>::backSubstitution(const dynamicVector<T>&f)const{
    dynamicVector<T> result(f.dim(),0.);
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
	  const dynamicVector<T>&f, dynamicVector<T>&x){
    for(int i=0; i<f.dim(); i++)
      x(i) += (f[i] - *A.item[i] * x) / A(i,i);
  }  //  Gauss-Seidel relaxation

  template<class T>
  void reversedGaussSeidel(const sparseMatrix<T>&A,
	  const dynamicVector<T>&f, dynamicVector<T>&x){
    for(int i=f.dim()-1; i>=0; i--)
      x(i) += (f[i] - *A.item[i] * x) / A(i,i);
  }  //  Gauss-Seidel relaxation in reversed order

  template<class T>
  void symmetricGaussSeidel(const sparseMatrix<T>&A,
	  const dynamicVector<T>&f, dynamicVector<T>&x){
    for(int i=0; i<f.dim(); i++)
      x(i) += (f[i] - *A.item[i] * x) / A(i,i);
    for(int i=f.dim()-2; i>=0; i--)
      x(i) += (f[i] - *A.item[i] * x) / A(i,i);
  }  //  symmetric Gauss-Seidel relaxation

  template<class T>
  const sparseMatrix<T>
  sparseMatrix<T>::createTransfer(){
    *this += sparseMatrix<double>(rowNumber(),Stablizer);
    dynamicVector<double> RowSumA(rowNumber(),0.);
    const dynamicVector<int> coarse = coarsen(thresholdMG);
    const dynamicVector<int> fine = coarsen(coarse, FactorFine * thresholdMG);
    sparseMatrix<T> I(rowNumber(),1.);
    sparseMatrix<T> Atilde = *this;
    if(TruncateAtilde)
      Atilde.truncatePositive( thresholdMG);
    for(int i=0; i<rowNumber(); i++){
      RowSumA(i) = max(0.,item[i]->rowSum());
      if(!coarse[i])
        item[i]->dropPositiveItems(i, (*item[i])[i], thresholdMG);
    }
    sparseMatrix<T> A = *this;
    sparseMatrix<T> P(nonZeros(fine));
   if(nonZeros(fine) > nonZeros(coarse)){
    for(int i=0; i<rowNumber(); i++){
      if(coarse[i]){
	delete P.item[fine[i]-1];
	P.item[fine[i]-1] = new row<T>(1., coarse[i] - 1);
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
	  *P.item[fine[i]-1] /= P.item[fine[i]-1]->rowSum() - RowSumA[fine[i]-1];
	else
	  *P.item[fine[i]-1] /= P.item[fine[i]-1]->rowSum();
      }
    }
   }
    for(int i=0; i<rowNumber(); i++){
      if(fine[i]){
	delete item[i];
	item[i] = new row<T>(1., fine[i] - 1);
	delete Atilde.item[i];
	Atilde.item[i] = new row<T>(0.,i);
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
	  *item[i] /= item[i]->rowSum() - RowSumA[i];
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
	      (runner->getValue() / (*this)(i,i) <= -threshold))
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
	      (runner->getValue() / (*this)(i,i) <= -threshold))
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
	 new multigrid(truncate(R*A*P, FactorCoarse * thresholdMG)) : 0){}  //  constructor with matrix argument
	const multigrid<T>& operator=(const multigrid<T>&);
	~multigrid(){ delete next; next = 0; }  //  destructor
	const dynamicVector<T>& Vcycle(const dynamicVector<T>&,
		dynamicVector<T>&) const;
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
	const dynamicVector<T>&
	multigrid<T>::Vcycle(const dynamicVector<T>&f,
		   dynamicVector<T>&x) const{
	   if(next){
	     for(int i=0; i<Nu1; i++){
	       if(useILU)
	         ILU(A,L,U,f,x);
	       else
                 GaussSeidel(A,f,x);
	     }
	     dynamicVector<T> residual = f - A * x;
	     dynamicVector<T> correction(R.rowNumber(), 0.);
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
	 const int iterationnumber = 10000;
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

        template<class T>
        void CGS(const multigrid<T>& MG, const sparseMatrix<T>&A, int TFQMR,
                 const dynamicVector<T>&f, dynamicVector<T>&x){
          const double eps=1.e-15;
          const int iterationnumber = 1000;
          T omegainit,omega0,omega1,tau,vv;
          T rho0,sigma,eta,alpha,rho1,beta;
          dynamicVector<T> keep(x);
          dynamicVector<T> rr(MG.Vcycle(f,keep) - x);
          dynamicVector<T> rbar(rr);
          dynamicVector<T> rcgs(rr),u(rr),pp(rr);
          tau = omegainit = omega1 = omega0 =
              sqrt(rcgs * rcgs);
          printf("res0=");
          print(omegainit);
	  printf("\n");
  	  eta=0.;
          vv=0.;
          if(fabs(rho0 = rbar * rr) < eps)
            printf("rho0=%f,mg iteration number=1\n",fabs(rho0));
          dynamicVector<T> zero(x.dim(),0.);
          dynamicVector<T> d(zero),v(zero),q(zero);
          int count = 1;
          do{
            keep = pp;
            v = pp - MG.Vcycle(zero,keep);
            if(fabs(sigma = rbar * v) < eps)
              printf("sigma=%f,mg iteration number=%d\n",fabs(sigma),2 * count);
            if(fabs(alpha=rho0/sigma) < eps)
              printf("alpha=%f,mg iteration number=%d\n",fabs(alpha),2 * count);
            q = u - alpha * v;
            dynamicVector<T> uq = u + q;
            keep = uq;
            rcgs -= alpha * (uq - MG.Vcycle(zero,keep));
            omega1=sqrt(rcgs * rcgs);
            if(!TFQMR){
              x += alpha * uq;
              printf("residual=%f, MG iteration number in CGS=%d\n",fabs(omega1),2 * count + 1);
            }
            else{
              for(int m=2*count+1; m<=2*count+2;m++){
                T omega;
                if(m==2*count+1){
                  omega=sqrt(omega0*omega1);
                  keep=u;
                }
                else{
                  omega=omega1;
                  keep=q;
                }
                T scala=vv*vv*eta/alpha;
                d = keep + scala * d;
                vv=omega/tau;
                T c=1./sqrt(1.+vv*vv);
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

    template<class T>
    const dynamicVector<T>&
    GMRES(const multigrid<T>&MG, const sparseMatrix<T>&A,
          int preIterations, int K,
          const dynamicVector<T>&f, dynamicVector<T>&x){
      for(int i=0;i<preIterations;i++)
        MG.Vcycle(f,x);
      dynamicVector<T> s = x;
      dynamicVector<T> r = x;
      T R[K+1][K+1];
      for(int i=0;i<=K;i++)
        for(int j=0;j<=K;j++)
          R[i][j]=0.0;
      T Givens[2][K];
      T xi[K];
      for(int i=0;i<K;i++){
        Givens[0][i] = 0.0;
        Givens[1][i] = 0.0;
        xi[i] = 0.0;
      }
      dynamicVector<T>* Q[K+1];
      dynamicVector<T> zero(x.dim(),0.0);
      dynamicVector<T> keep(x);
      double res=sqrt((A*x-f)*(A*x-f));
      for(int k=0;k<=K;k++){
        if(k)keep = *Q[k-1];
        Q[k] = k ?
          new dynamicVector<T>(*Q[k-1] - MG.Vcycle(zero,keep))
          : new dynamicVector<T>(MG.Vcycle(f,keep)-x);
        for(int j=0;j<k;j++)
          *Q[k] -= (R[j][k] = *Q[j] * *Q[k]) * *Q[j];
        *Q[k] *= 1.0/(R[k][k] = sqrt(*Q[k] * *Q[k]));
        T givensa;
        T givensb;
        if(k){
          for(int j=1;j<k;j++){
            givensa=R[j-1][k];
            givensb=R[j][k];
            R[j-1][k]=givensa*Givens[0][j-1]+givensb*Givens[1][j-1];
            R[j][k]=-givensa*Givens[1][j-1]+givensb*Givens[0][j-1];
          }
          T ab=sqrt(R[k-1][k]*R[k-1][k]+R[k][k]*R[k][k]);
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

        template<class T>
        void CGS2(const multigrid<T>& MG,
                 const dynamicVector<T>&f, dynamicVector<T>&x){
          const double eps=1.e-15;
	  const int iterationnumber = 1000;
	  double omega0,omegainit,rho0,tau,vv,eta,
	      sigma,alpha,omega1,rho1,beta;
	  dynamicVector<T> keep(x);
	  dynamicVector<T> rr(MG.Vcycle(f,keep) - x);
	  dynamicVector<T> rbar(rr),rcgs(rr),u(rr),pp(rr);
	  tau = omegainit = omega1 = omega0 = sqrt(rcgs * rcgs);
          printf("res0=%f\n",omegainit);
  	  eta=vv=0.;
	  if(fabs(rho0 = rbar * rr) < eps)
            printf("rho0=%f,mg iteration number=1\n",rho0);
	  dynamicVector<T> zero(x.dim(),0.);
	  dynamicVector<T> d(zero),v(zero),q(zero);
          int count = 1;
          do{
	    keep = pp;
	    v = pp - MG.Vcycle(zero,keep);
	    if(fabs(sigma = rbar * v) < eps)
              printf("sigma=%f,mg iteration number=%d\n",sigma,2 * count);
	    if(fabs(alpha=rho0/sigma) < eps)
              printf("alpha=%f,mg iteration number=%d\n",alpha,2 * count);
	    q = u - alpha * v;
	    dynamicVector<T> uq = u + q;
	    keep = uq;
	    rcgs -= alpha * (uq - MG.Vcycle(zero,keep));
	    omega1=sqrt(rcgs * rcgs);
	    x += alpha * uq;
            printf("residual=%f, MG iteration number in CGS=%d\n",omega1,
	         2 * count + 1);
	    omega0=omega1;
	    if(fabs(rho1=rbar*rcgs)<eps)
              printf("rho1=%f,mg iteration number=%d\n",rho1,2*count+1);
	    beta=rho1/rho0;
	    rho0=rho1;
	    u = rcgs + beta * q;
	    pp = u + beta * (q + beta * pp);
         } while((omega1/omegainit >= thresholdCG)&&
	         (++count <= iterationnumber));
         printf("total MG iterations in CGS=%d\n",2 * count + 1);
       }  //  CGS acceleration

  main(){
    double angle = asin(SlitWidth);
    double sinAngle=sin(angle);
    double sinAngle2=sin(angle/2);
    double cosAngle=cos(angle);
    double cosAngle2=cos(angle/2);
    node<point> a1(point(1,0));
    node<point> a2(point(sinAngle2,cosAngle2));
    node<point> a3(point(-cosAngle,sinAngle));
    node<point> a4(point(-cosAngle,-sinAngle));
    node<point> a5(point(sinAngle2,-cosAngle2));
    node<point> a7(point(0,SlitWidth));
    node<point> a8(point(0,SlitWidth/4));
    node<point> a9(point(-SlitLength,SlitWidth/4));
    node<point> a10(point(-SlitLength,-SlitWidth/4));
    node<point> a11(point(0,-SlitWidth/4));
    node<point> a12(point(0,-SlitWidth));

    triangle t1(a1,a2,a3);
    triangle t1a(t1(0),a7,t1(1));
    triangle t1b(t1(1),t1a(1),t1(2));
    triangle t2(a4,a5,t1(0));
    triangle t2a(t2(0),a12,t2(1));
    triangle t2b(t2(1),t2a(1),t2(2));
    triangle t3a(t1a(0),a8,t1a(1));
    triangle t3b(t2b(1),a11,t2b(2));
    triangle t4(t3a(1),t3b(1),t2b(2));
    triangle t5a(a9,t3a(1),t3b(1));
    triangle t5b(t5a(0),a10,t3b(1));
    mesh<triangle> m(t1);
    m.append(t2);
    t1.~triangle();
    t2.~triangle();
    if(Circle)m.refineBoundary(boundaryLevels);
    m.dropNextItem();
    m.insertNextItem(t5b);
    m.insertNextItem(t5a);
    m.insertNextItem(t4);
    m.insertNextItem(t3b);
    m.insertNextItem(t3a);
    m.insertNextItem(t2b);
    m.insertNextItem(t2a);
    m.insertNextItem(t1b);
    m.insertNextItem(t1a);
    m.dropFirstItem();
    t1a.~triangle();
    t1b.~triangle();
    t2a.~triangle();
    t2b.~triangle();
    t3a.~triangle();
    t3b.~triangle();
    t4.~triangle();
    t5a.~triangle();
    t5b.~triangle();

    for(int i=0; i<adaptiveLevels; i++){
      dynamicVector<double> f(m.indexing(),0.);
      dynamicVector<int> DirichletBoundary(m.indexing(),1);
      sparseMatrix<double> A(m,f,DirichletBoundary);
      A += transpose(A) - diagonal(A);
      A.setDirichlet(f,DirichletBoundary);
      if(PrintMesh){
	print(m);
      }
      dynamicVector<double> x(A.order(),0.);
      multigrid<double> MG(A);
      PCG(MG,A,f,x);
      //CGS(MG,A,0,f,x);
      //GMRES(MG,A,10,10,f,x);
      if(PrintX) print(x);
      printf("value at singularity=%f\n",x[0]);
      printf("max norm away from singularity=%f\n",m.maxNorm(x));
      printf("at level %d, number of nodes=%d\n",i+1,x.dim());
      //HELM *= 2.;
      thresholdAdaptive = max(1. * m.jumpSum(x),0.5 * m.jumpMax(x));
      if(i<adaptiveLevels-1)m.refine(x, DirichletBoundary, thresholdAdaptive);
    }
    return 0;
  }
