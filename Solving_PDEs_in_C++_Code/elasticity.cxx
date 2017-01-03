//  THE LINEAR-ELASTICITY SOLVER (CHAPTER 20)
//  -----------------------------------------
//  This program solves the linear elasticity equation
//  in a circle (Chapter 20). The Poisson ratio is NU=1/3.
//  The adaptive refinement algorithm uses 7 levels
//  of refinement with automatic boundary refinement
//  and modified multigrid preconditioner.
//  The detailed documentation is in the book.

#include<stdio.h>
#include<math.h>

const int PrintMesh=0;
const int PrintX=0;
const int Regular=0;
const int boundaryLevels=1;
const int adaptiveLevels=7;
const int Dirichlet=1;
const double GammaD=.5;
const double Diag=0.0;
const double NU=1./3.;
const double CoefDelta=1.;
const int RowSumStablizer=0;
const double Stablizer=0.0;
const double DiffX=100.;
const double DiffY=1000.;
const double thresholdAdaptive=0.01;
const double thresholdILU=0.05;
const int useILU=0;
const double thresholdCG=1.e-6;
const double ratioMG=.95;
const int AMG=0;
const int Circle=1;
const double thresholdMG=0.05;
const int Nu1=1;
const int Nu2=1;
const int NuCoarse=1;
const int cycleIndex=1;

int max(int a, int b){return a>b ? a : b;}
int min(int a, int b){return a<b ? a : b;}

double max(double a, double b){return a>b ? a : b;}
double min(double a, double b){return a<b ? a : b;}

double abs(double d){return d > 0. ? d : -d;}  //  absolute value

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
    printf("(");
    for(int i = 0;i < N; i++){
      printf("v[%d]=",i);
      print(v[i]);
    }
    printf(")\n");
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
    int lessSharingElements(){return !(--sharingElements);}  //  decrease it
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
      printf("index=%d; %d sharing elements\n",
	  n.getIndex(),n.getSharingElements());
    }  //  print a node

template<class T, int N> class finiteElement{
  node<T>* vertex[N];
public:
  finiteElement(){
    for(int i=0; i<N; i++)
      vertex[i] = new node<T>;
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
    for(int i=0; i<N; i++)
      if(((*vertex[i])()[0]>0.)||((*vertex[i])()[1]>0.)) return 1.;
    return DiffX;
  }  //  coefficient of x-derivative
  double q() const{
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

template<class T> class connectedList{
protected:
  T item;
  connectedList* next;
public:
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
  void truncateItems(double);
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
  void connectedList<T>::truncateItems(double threshold){
    if(next){
      if(abs(next->item.getValue()) <= threshold){
        dropNextItem();
        truncateItems(threshold);
      }
      else
        next->truncateItems(threshold);
    }
    if(next&&(abs(item.getValue()) <= threshold))
      dropFirstItem();
  }  //  truncate certain items

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
  printf("item:\n");
  print(l());
  if(l.readNext())print(*l.readNext());
}  //  print a connected list

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
  const dynamicVector& operator*=(const T&);
  const dynamicVector& operator/=(const T&);
};

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
     if(dimension > v.dimension)
       delete [] (component + v.dimension);
     if(dimension < v.dimension){
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

template<class T> class mesh:public connectedList<T>{
  public:
    mesh(){}  //  default constructor
    mesh(T&e){item = e;}  //  constructor
    int indexing();
    void refineNeighbor(node<point>&, node<point>&, node<point>&);
    void refine(const dynamicVector<double>&, double);
    void refineBoundary(int);
    double maxNorm(const dynamicVector<double>&);
};

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
	else{
	  node<point> newNode((1./sqrt(squaredNorm(nIJ()))) * nIJ());
	  triangle t1(nI,nIJ,newNode);
	  triangle t2(nJ,nIJ,t1(2));
	  insertNextItem(t2);
	  insertNextItem(t1);
	}
      }
    }  //  refine also the neighbor of a refined triangle

    void mesh<triangle>::refine(const dynamicVector<double>&v,
	    double threshold){
	//vector<int,4> jump(0);
        for(int i=0; i<3; i++)
	  for(int j=2; j>i; j--)
	    if((item[i].getIndex() >= 0)&&(item[j].getIndex() >= 0)&&
	     ((abs(v[item[i].getIndex()] - v[item[j].getIndex()])>threshold)||
	     (abs(v[item[i].getIndex()+v.dim()/2] -
	     v[item[j].getIndex()+v.dim()/2])>threshold))){
	      /*
	      jump.set(i+j,1);
	    }
	if(Regular&&(jump[1]||jump[3])) jump.set(2,1);
        for(int i=0; i<3; i++)
	  //for(int j=2; j>i; j--)
	  for(int j=i+1; j<3; j++)
	    if(jump[i+j]){
	      */
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
	      refine(v, threshold);
	      return;
	    }
	if(next)((mesh<triangle>*)next)->refine(v, threshold);
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
	if(squaredNorm(item[i]()-point(Circle,0))>0.01){
	  result=max(result,abs(v[item[i].getIndex()]));
	  result=max(result,abs(v[item[i].getIndex()+v.dim()/2]));
	}
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
    return rowElement<T>(e) /= e;
  }  //   rowElement divided by a T

  template<class T>
  void print(const rowElement<T>&e){
    print(e.getValue());
    printf("column=%d\n",e.getColumn());
  }  //  print a rowElement object

  void print(int i){
    printf("%d; ",i);
  }  //  print an integer variable

  void print(double d){
    printf("%f; ",d);
  }  //  print a double variable

template<class T> class row : public connectedList<rowElement<T> >{
  public:
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
    const T operator*(const dynamicVector<T>&v) const{
      return next ? getValue() * v[getColumn()] + *(row*)next * v
	    : getValue() * v[getColumn()];
    }  //  row times vector (inner product)
    void renumberColumns(const dynamicVector<int>&renumber){
      item = rowElement<T>(getValue(),renumber[getColumn()]-1);
      if(next)(*(row<T>*)next).renumberColumns(renumber);
    }  //  renumber columns
    void dropItems(const dynamicVector<int>&);
    void dropPositiveItems(int, double);
};

  template<class T>
  void row<T>::dropItems(const dynamicVector<int>&mask){
    if(next){
      if(!mask[(*next)().getColumn()]){
        dropNextItem();
        dropItems(mask);
      }
      else
        (*(row<T>*)next).dropItems(mask);
      if(!mask[getColumn()])dropFirstItem();
    }
  }  //  drop certain items

  template<class T>
  void row<T>::dropPositiveItems(int i, double threshold){
    if(next){
      if(((*next)().getColumn() != i)&&
	      ((*next)().getValue()/(*this)[i] >= -threshold)){
        dropNextItem();
        dropPositiveItems(i, threshold);
      }
      else
        (*(row<T>*)next).dropPositiveItems(i, threshold);
      if((getColumn() != i)&&(getValue()/(*this)[i] >= -threshold))
        dropFirstItem();
    }
  }  //  drop positive off-diagonal elements

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
    print(number);
    for(int i=0; i<l.number; i++)
      if(l.item[i]) item[i] = new T(*l.item[i]);
  }  //  copy constructor

template<class T>
const list<T>& list<T>::operator=(const list<T>& l){
   if(this != &l){
     if(number > l.number)
       delete [] (item + l.number);
     if(number < l.number){
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
  sparseMatrix(mesh<triangle>&);
  void readSparseMatrix(int);
  ~sparseMatrix(){}  //  destructor
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

  template<class T>
  sparseMatrix<T>::sparseMatrix(mesh<triangle>&m){
    item = new row<T>*[number = 2 * m.indexing()];
    for(int i=0; i<number; i++)
      item[i] = 0;
    point gradient[3];
    gradient[0] = point(-1,-1);
    gradient[1] = point(1,0);
    gradient[2] = point(0,1);
    dynamicVector<int> DirichletBoundary(number,1);
    for(const mesh<triangle>* runner = &m; runner;
	    runner=(const mesh<triangle>*)runner->readNext()){
      point diffX(1.,0.);
      point diffY(0.,(1.-NU)/2.);
      point diffX2((1.-NU)/2.,0.);
      point diffY2(0.,1.);
      matrix2 diffCoef(diffX,diffY);
      matrix2 diffCoef2(diffX2,diffY2);
      point mixedX(0.,NU);
      point mixedY((1.-NU)/2.,0.);
      //point mixedX(0.,(1.+NU)/2.);
      //point mixedY(0.,0.);
      matrix2 mixed(mixedX, mixedY);
      matrix2 S((*runner)()[1]() - (*runner)()[0](),
	      (*runner)()[2]() - (*runner)()[0]());
      matrix2 Sinverse = inverse(S);
      matrix2 weight=abs(det(S)/2)*Sinverse*diffCoef*transpose(Sinverse);
      matrix2 weight2=abs(det(S)/2)*Sinverse*diffCoef2*transpose(Sinverse);
      matrix2 weightMixed =
	  abs(det(S)/2) * Sinverse * mixed * transpose(Sinverse);
      for(int i=0; i<3; i++){
        int I = (*runner)()[i].getIndex();
        if((abs(squaredNorm((*runner)()[i]())-1.)<1.e-7)&&
           ((*runner)()[i]()[0] <= -GammaD)){
	  DirichletBoundary(I) = 0;
	  DirichletBoundary(I+number/2) = 0;
	}
	for(int j=0; j<3; j++){
          int J = (*runner)()[j].getIndex();
	  if(j>=i){
	    if(item[I]){
	      row<T> r(gradient[j]*weight*gradient[i],J);
	      *item[I] += r;
	    }
	    else
	      item[I] = new row<T>(gradient[j]*weight*gradient[i],J);
	    if(item[I+number/2]){
	      row<T> r(gradient[j]*weight2*gradient[i],J+number/2);
	      *item[I+number/2] += r;
	    }
	    else
	      item[I+number/2] =
		new row<T>(gradient[j]*weight2*gradient[i],J+number/2);
	  }
	  if(item[I]){
	    row<T> r(gradient[j]*weightMixed*gradient[i],J+number/2);
	    *item[I] += r;
	  }
	  else
	    item[I]=new row<T>(gradient[j]*weightMixed*gradient[i],J+number/2);
	}
      }
    }
    if(Dirichlet)
      for(int i=0; i<number; i++){
        if(DirichletBoundary[i])
          item[i]->dropItems(DirichletBoundary);
	else
	  *item[i] = row<T>(1.,i);
      }
      /*
      dynamicVector<int> first(number,1);
      first(0) = 0;
      for(int i=0; i<number/2; i++)
        item[i]->dropItems(first);
      *item[0] = row<T>(1.,0);
      dynamicVector<int> middle(number,1);
      middle(number/2) = 0;
      for(int i=0; i<number; i++)
        item[i]->dropItems(middle);
      *item[number/2] = row<T>(1.,number/2);
      */
  }  //  assembly of stiffness matrix

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
    sparseMatrix<T> L(rowNumber());
    L.item[0] = new row<T>(0.,0);
    for(int i=1; i<rowNumber(); i++){
      item[i-1]->truncateItems(threshold * abs(item[i-1]->getValue()));
      while(item[i]&&(item[i]->getColumn() < i)){
	T factor=item[i]->getValue() / item[item[i]->getColumn()]->getValue();
	if(abs(factor) >= threshold){
	  row<T> r = (-factor) * *item[item[i]->getColumn()];
	  *item[i] += r;
	  if(L.item[i])
	    L.item[i]->append(factor, item[i]->getColumn());
	  else
	    L.item[i] = new row<T>(factor, item[i]->getColumn());
	}
	item[i]->dropFirstItem();
      }
      if(!L.item[i])L.item[i] = new row<T>(0.,0);
      //item[i]->truncateItems(threshold * abs(item[i]->getValue()));
    }
    return L;
  }  //  incomplete LU factorization

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

  template<class T>
  void ILU(const sparseMatrix<T>&A, const sparseMatrix<T>&L,
    const sparseMatrix<T>&U,const dynamicVector<T>&f, dynamicVector<T>&x){
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
    dynamicVector<int> one(rowNumber(),1);
    dynamicVector<int> two(rowNumber(),1);
    for(int i=0; i<rowNumber()/2; i++)
      two(i) = 0;
    for(int i=rowNumber()/2; i<rowNumber(); i++)
      one(i) = 0;
    for(int i=0; i<rowNumber()/2; i++)
      item[i]->dropItems(one);
    *this += sparseMatrix<double>(rowNumber(),Stablizer);
    for(int i=rowNumber()/2; i<rowNumber(); i++)
      item[i]->dropItems(two);
    dynamicVector<double> RowSumA(rowNumber(),0.);
    const dynamicVector<int> coarse = coarsen(thresholdMG);
    for(int i=0; i<rowNumber(); i++){
      RowSumA(i) = item[i]->rowSum();
      if(!coarse[i])
        item[i]->dropPositiveItems(i, thresholdMG);
    }
    sparseMatrix<T> A;
    if(AMG) A = *this;
    for(int i=0; i<rowNumber(); i++){
      if(coarse[i]){
	delete item[i];
	item[i] = new row<T>(1., coarse[i] - 1);
      }
      else{
	if(AMG)
	  for(const row<T>* runner = item[i]; runner;
	        runner = (const row<T>*)runner->readNext()){
	    int j = runner->getColumn();
	    if((j != i)&&(!coarse[j])){
	      T Pij = runner->getValue();
	      T AjSum = item[i]->rowSumCoarse(*A.item[j],coarse);
	      if(abs(AjSum) > 1.e-10)
	        item[i]->addCoarse((Pij / AjSum) * *A.item[j],coarse);
	    }
	  }
        item[i]->dropItems(coarse);
        item[i]->renumberColumns(coarse);
	if(RowSumStablizer)
	  *item[i] /= item[i]->rowSum() - RowSumA[i];
	else
	  *item[i] /= item[i]->rowSum();
      }
    }
    return transpose(*this);
  }  //  create transfer operators

  template<class T>
  const dynamicVector<int>
  sparseMatrix<T>::coarsen(double threshold) const{
    dynamicVector<int> coarse(rowNumber(), 1);
    for(int i=0; i<rowNumber(); i++)
      if(coarse[i])
        for(const row<T>* runner = item[i]; runner;
	        runner = (const row<T>*)runner->readNext())
	  if((runner->getColumn() != i)&&(
	       abs(runner->getValue()) > threshold * abs((*this)(i,i))))
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
	 new multigrid(R*A*P) : 0){}  //  constructor with matrix argument
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
                 symmetricGaussSeidel(A,f,x);
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
	         symmetricGaussSeidel(A,f,x);
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
        void CGS(const multigrid<T>& MG,
                 const dynamicVector<T>&f, dynamicVector<T>&x){
          const double eps=1.e-15;
	  const int iterationnumber = 10000;
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
    //sparseMatrix<double> A(3948);
    //A.readSparseMatrix(60882);
    /*
    sparseMatrix<double> A(4884);
    A.readSparseMatrix(147631);
    A += transpose(A) - diagonal(A);
    dynamicVector<double> x(A.order(),1.);
    dynamicVector<double> f(A.order(),0.);
    multigrid<double> MG(A);
    PCG(MG,A,f,x);
    printf("l2 norm of the solution is %f\n",sqrt(x*x));
    return 0;
    */
    /*
    node<point> a(point(0,0));
    node<point> b(point(0,1));
    node<point> c(point(1,0));
    node<point> d(point(1,1));
    triangle t1(a,b,d);
    triangle t2(t1(0),c,t1(2));
    */
    node<point> a(point(1,0));
    node<point> b(point(0,1));
    node<point> c(point(-1,0));
    node<point> d(point(0,-1));
    triangle t1(a,b,c);
    triangle t2(t1(2),d,t1(0));
    mesh<triangle> m(t1);
    m.append(t2);
    t1.~triangle();
    t2.~triangle();
    if(Circle)m.refineBoundary(boundaryLevels);
    for(int i=0; i<adaptiveLevels; i++){
      sparseMatrix<double> A(m);
      A += transpose(A) - diagonal(A);
      A += sparseMatrix<double>(A.order(),Diag);
      dynamicVector<double> f(A.rowNumber(),0.);
      f(0) = CoefDelta;
      f(f.dim()/2) = CoefDelta;
      if(PrintMesh){
	print(m);
	print(A);
        if(i>0)return 0;
      }
      dynamicVector<double> x(A.rowNumber(),0.);
      multigrid<double> MG(A);
      PCG(MG,A,f,x);
      if(PrintX) print(x);
      printf("value at singularity=%f\n",x[0]);
      printf("max norm away from singularity=%f\n",m.maxNorm(x));
      printf("at level %d, number of nodes=%d\n",i+1,x.dim()/2);
      if(i<adaptiveLevels-1)m.refine(x, thresholdAdaptive);
    }
    /*
    sparseMatrix<double> A(m);
    A += transpose(A) - diagonal(A) + sparseMatrix<double>(A.order(),.01);
    dynamicVector<double> f(A.order(),0.);
    dynamicVector<double> x(A.order(),1.);
    multigrid<double> MG(A);
    PCG(MG,A,f,x);
    print(x);
    //CGS(MG,f,x);
    sparseMatrix<double> U = A;
    sparseMatrix<double> L = U.factorize(thresholdILU);
    for(int i=0; i<20; i++)
      ILU(A,L,U,f,x);
    print(x);
    m.dropFirstItem();
    printf("number of unknowns=%d\n",m.indexing());
    print(m);
    row<double> rr(4,4);
    rr.append(5,10);
    rr.append(7,7);
    rr.append(35,35);
    rr.append(25,25);
    rr.append(5,30);
    rr.append(45,45);
    rr.append(24,24);
    rr.append(12,12);
    print(rr);
    print(rr.order(rr.length()));
    */
    return 0;
  }
