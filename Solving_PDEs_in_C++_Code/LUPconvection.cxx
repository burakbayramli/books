//  LUP FACTORIZATION (CHAPTER 3) AND
//  CONVECTION-DIFFUSION IN 1-D AND 2-D (CHAPTER 7)
//  -----------------------------------------------
//  This program introduces the "matrix" 
//  and "dynamicMatrix" objects to implement 
//  the LUP factorization and diagonalization
//  of a square matrix and compute its 
//  determinant in 3 different ways 
//  (Chapters 2--3). Furthermore, 
//  it solves the convection-diffusion
//  equation in 1-D and 2-D on uniform grids using finite 
//  differences with the upwind scheme and 
//  semi-implicit time marching (Chapter 7).
//  The output printed to the screen is the final time level.
//  The complete documentation is in the book.

#include<stdio.h>
#include<math.h>

void print(double d){
  printf("%f  ",d);
}

int power(int basis, unsigned exp){
  return exp ? basis * power(basis,exp-1) : 1;
}  //  "basis" to the "exp"

int max(int a, int b){return a>b ? a : b;}
int min(int a, int b){return a<b ? a : b;}

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
    vector<T,N>& operator()(int i){
      return vector<vector<T,N>,M>::operator()(i);
    }  //  read/write ith column
    const T& operator()(int i,int j) const{return (*this)[j][i];}//A(i,j)
    T& operator()(int i,int j, char*){
      return (*this)(j)(i);
    }  //  read/write A(i,j)
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

  template<class T, int N, int M>
  const matrix<T, M, N>
  transpose(const matrix<T, N, M>&A){
    matrix<T, M, N> At;
    for(int i=0; i<N; i++)
      for(int j=0; j<M; j++)
        At(j,i,"write") = A(i,j);
    return At;
  }  //  transpose of a matrix

  template<class T, int N>
  void diagonalize(matrix<T,N,N>&W,
      const matrix<T,N,N>&A,
      matrix<T,N,N>&Z, vector<T,N>&D){
    for(int i=0; i<N; i++){
      W(i,i,"write") = 1.;
      Z(i,i,"write") = 1.;
    }
    matrix<T,N,N> At = transpose(A);
    for(int i=0; i<N; i++){
      vector<T,N> r = At[i];
      for(int j=0; j<i; j++)
        W(i) -= r * Z[j] / D[j] * W[j];
      vector<T,N> c = A[i];
      for(int j=0; j<i; j++)
        Z(i) -= W[j] * c / D[j] * Z[j];
      D(i) = W[i] * (A * Z[i]);
    }
  }  //  diagonalize a matrix

  template<class T, int N>
  const T det(const vector<T,N>&D){
    T result = 1.;
    for(int i=0; i<N; i++)
      result *= D[i];
    return result;
  }  //  determinant of a diagonalized matrix

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
  dynamicVector<T>::dynamicVector(int dim = 0, const T& a = 0) : dimension(dim),
       component(dim ? new T[dim] : 0){
     for(int i = 0; i < dim; i++)
       component[i] = a;
  }  //  constructor

  template<class T>
  dynamicVector<T>::dynamicVector(const dynamicVector<T>& v) : dimension(v.dimension),
      component(v.dimension ? new T[v.dimension] : 0){
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
  const dynamicVector<T>& dynamicVector<T>::operator+=( const dynamicVector<T>&v){
      for(int i = 0; i < dimension; i++)
	component[i] += v[i];
      return *this;
  }  //  adding a dynamicVector to the current dynamicVector

  template<class T>
  const dynamicVector<T>& dynamicVector<T>::operator-=( const dynamicVector<T>&v){
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
  const dynamicVector<T> operator+(const dynamicVector<T>&u, const dynamicVector<T>&v){
    return dynamicVector<T>(u) += v;
  }  //  dynamicVector plus dynamicVector

  template<class T>
  const dynamicVector<T> operator-(const dynamicVector<T>&u, const dynamicVector<T>&v){
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
  }  //  dynamicVector times dynamicVector (inner product)

  template<class T>
  void print(const dynamicVector<T>&v){
    for(int i = 0;i < v.dim(); i++)
      printf("v[%d]=%f;  ",i,(double)v[i]);
    printf("\n");
  }  //  printing a dynamicVector

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

template<class T> class difference:public list<dynamicVector<T> >{
 public:
  difference(int,const T&,const T&,const T&);
  const difference<T>& operator+=(const difference<T>&);
  const difference<T>& operator-=(const difference<T>&);
  const difference& operator*=(const T&);
  T& operator()(int i,int j){return (*item[j-i+1])(i);}//(i,j)th elem.
  const T& operator()(int i,int j,char*) const{
    return (*item[j-i+1])[i];
  }  //  (i,j)the element (read only)
  int width() const{ return item[0]->dim(); }
};
  template<class T>
  difference<T>::difference(int n=0,const T&a=0,const T&b=0,const T&c=0){
    number = 3;
    item = new dynamicVector<T>*[3];
    item[0] = new dynamicVector<T>(n,a);
    item[1] = new dynamicVector<T>(n,b);
    item[2] = new dynamicVector<T>(n,c);
  }

  template<class T>
  const difference<T>& difference<T>::operator+=(const difference<T>&d){
    for(int i=0; i<number; i++)
      *item[i] += d[i];
    return *this;
  }  //  adding another difference to the current one

  template<class T>
  const difference<T>& difference<T>::operator-=(const difference<T>&d){
    for(int i=0; i<number; i++)
      *item[i] -= d[i];
    return *this;
  }  //  subtracting another difference from the current one

  template<class T>
  const difference<T>& difference<T>::operator*=(const T&t){
    for(int i=0; i<size(); i++)
      *item[i] *= t;
    return *this;
  }  //  multiplying the difference operator by a scalar T

  template<class T>
  const difference<T> operator+(const difference<T>&d1,
	  const difference<T>&d2){
    return difference<T>(d1) += d2;
  }  //  addition of two differences

  template<class T>
  const difference<T> operator-(const difference<T>&d1,
	  const difference<T>&d2){
    return difference<T>(d1) -= d2;
  }  //  subtraction of two differences

  template<class T>
  const difference<T> operator*(const T&t, const difference<T>&d){
    return difference<T>(d) *= t;
  }  //  scalar times difference

  template<class T>
  const difference<T> operator*(const difference<T>&d, const T&t){
    return difference<T>(d) *= t;
  }  //  difference times scalar

  template<class T>
  const dynamicVector<T> operator*(const difference<T>&d, const dynamicVector<T>&v){
    dynamicVector<T> dv(v.dim(),0.);
    for(int i=0; i<v.dim(); i++)
      for(int j=max(0,i-1); j<=min(v.dim()-1,i+1); j++)
	dv(i) += d(i,j,"read")*v[j];
    return dv;
  }  //  difference times dynamicVector

  template<class T>
  const dynamicVector<T> operator/(const dynamicVector<T>&f, const difference<T>&d){
    dynamicVector<T> x(f);
    for(int iteration=0; iteration < 100; iteration++)
      for(int i=0; i<f.dim(); i++){
	double residual = f[i];
        for(int j=max(0,i-1); j<=min(f.dim()-1,i+1); j++)
	  residual -= d(i,j,"read")*x[j];
	x(i) += residual/d(i,i,"read");
      }
    return x;
  }  //  solving d*x=f approximately by 10 GS iterations

template<class T> class xtGrid:public list<dynamicVector<T> >{
 public:
  xtGrid(int,int,const T&);
  int timeSteps() const{ return size(); }  // number of time steps
  int width() const{ return item[0]->dim(); }  // width of grid
  dynamicVector<T>& operator()(int i){if(item[i])return *item[i];}//ith line
  T& operator()(int i, int j){ return (*item[i])(j); }// (i,j)th comp.
};

  template<class T>
  xtGrid<T>::xtGrid(int m=0,int n=0,const T&a=0){
    number = m;
    item = m ? new dynamicVector<T>*[m] : 0;
    for(int i=0; i<m; i++)
      item[i] = new dynamicVector<T>(n,a);
  }  //  constructor

double F(double, double){return 0.;}
double C(double, double){return 0.;}
double Alpha(double, double){return 0.;}
double G(double, double){return 0.;}
double Initial(double x){return 1.-x*x;}
const double Epsilon=1.;

  template<class T>
  void convDif(difference<T>&d,dynamicVector<T>&f,double h,double deltaT,double t){
      for(int j=0; j<d.width(); j++){
	if(t>deltaT/2)f(j)=F(j*h,t-deltaT/2);
	double c=C(j*h,t);
        if(c>0.){
	  d(j,j)=c/h;
	  d(j,j-1)=-c/h;
	  d(j,j+1)=0.;
        }
        else{
	  d(j,j)=-c/h;
	  d(j,j+1)=c/h;
	  d(j,j-1)=0.;
        }
      }
      d += Epsilon/h/h * difference<T>(d.width(),-1.,2.,-1.);
      d(0,0) += d(0,-1);
      d(0,0) -= d(0,-1) * h * Alpha(0,t);
      if(t>deltaT/2){
        f(0) -= d(0,-1) * h * G(0,t-deltaT/2);
        f(d.width()-1) -= d(d.width()-1,d.width())
	      * G(d.width()*h,t-deltaT/2);
      }
  }  //  set the convection-diffusion matrix and right-hand side

  template<class T>
  void march(xtGrid<T>&g, double h, double deltaT){
    difference<T> I(g.width(),0.,1.,0.);
    for(int j=0; j<g.width(); j++)
      g(0,j) = Initial(j*h);
    dynamicVector<T> f(g.width());
    difference<T> previous(g.width());
    convDif(previous,f,h,deltaT,0);
    for(int time=1; time < g.timeSteps(); time++){
      difference<T> current(g.width());
      convDif(current,f,h,deltaT,time*deltaT);
      g(time) = ((I - 0.5 * deltaT * previous) * g[time-1] + deltaT * f)
	      / (I + 0.5 * deltaT * current);
      previous = current;
    }
    print(g[g.timeSteps()-1]);
  }  //  semi-implicit time marching

class domain{
  xtGrid<double> g;
  double Time;
  double Width;
public:
  domain(double T, double L, double accuracy):g((int)(T/accuracy)+1,
      (int)(L/accuracy)+1),Time(T),Width(L){}  //  constructor
  void solveConvDif(){march(g,Width/g.width(),Time/g.timeSteps());}  //  solve
};

template<class T> class dynamicMatrix:public dynamicVector<T>{
  int N;
public:
  dynamicMatrix(int, int, const T&);
  T& operator()(int i, int j){ return component[i*N+j]; }  //(i,j)th element
  const T& operator()(int i, int j, char*) const{
    return component[i*N+j];
  }  //  (i,j)th element (read only)
  int height() const{ return dim()/N; }
  int width() const{ return N; }
  const dynamicMatrix& operator+=(const dynamicMatrix&);
  const dynamicMatrix& operator-=(const dynamicMatrix&);
  const dynamicMatrix& operator*=(const T&);
  const dynamicMatrix& operator/=(const T&);
};

  template<class T>
  dynamicMatrix<T>::dynamicMatrix(int m=0, int n=0, const T&t=0){
    dimension = n*m;
    N = n;
    component = dimension ? new T[dimension] : 0;
    for(int i=0; i<dimension; i++)
      component[i] = t;
  }  //  constructor

  template<class T>
  const dynamicMatrix<T>& dynamicMatrix<T>::operator+=(const dynamicMatrix<T>&v){
      for(int i = 0; i < dimension; i++)
	component[i] += v[i];
      return *this;
  }  //  adding a dynamicMatrix to the current dynamicMatrix

  template<class T>
  const dynamicMatrix<T>& dynamicMatrix<T>::operator-=(const dynamicMatrix<T>&v){
      for(int i = 0; i < dimension; i++)
	component[i] -= v[i];
      return *this;
  }  //  subtracting a dynamicMatrix from the current dynamicMatrix

  template<class T>
  const dynamicMatrix<T>& dynamicMatrix<T>::operator*=(const T& a){
      for(int i = 0; i < dimension; i++)
	component[i] *= a;
      return *this;
  }  //  multiplying the current dynamicMatrix by a scalar

  template<class T>
  const dynamicMatrix<T>& dynamicMatrix<T>::operator/=(const T& a){
      for(int i = 0; i < dimension; i++)
	component[i] /= a;
      return *this;
  }  //  dividing the current dynamicMatrix by a scalar

  template<class T>
  const dynamicMatrix<T> operator+(const dynamicMatrix<T>&u, const dynamicMatrix<T>&v){
    return dynamicMatrix<T>(u) += v;
  }  //  dynamicMatrix plus dynamicMatrix

  template<class T>
  const dynamicMatrix<T> operator-(const dynamicMatrix<T>&u, const dynamicMatrix<T>&v){
    return dynamicMatrix<T>(u) -= v;
  }  //  dynamicMatrix minus dynamicMatrix

  template<class T>
  const dynamicMatrix<T> operator*(const dynamicMatrix<T>&u, const T& a){
    return dynamicMatrix<T>(u) *= a;
  }  //  dynamicMatrix times scalar

  template<class T>
  const dynamicMatrix<T> operator*(const T& a, const dynamicMatrix<T>&u){
    return dynamicMatrix<T>(u) *= a;
  }  //  T times dynamicMatrix

  template<class T>
  const dynamicMatrix<T> operator/(const dynamicMatrix<T>&u, const T& a){
    return dynamicMatrix<T>(u) /= a;
  }  //  dynamicMatrix divided by scalar

  template<class T>
  const dynamicMatrix<T> operator-(const dynamicMatrix<T>&u){
    return dynamicMatrix<T>(u) *= -1.;
  }  //  negative of a dynamicMatrix

  template<class T>
  void print(const dynamicMatrix<T>&v){
    for(int i = 0;i < v.height(); i++){
      for(int j = 0;j < v.width(); j++)
        printf("v[%d,%d]=%f;  ",i,j,v(i,j,"read"));
      printf("\n");
    }
  }  //  printing a dynamicMatrix

  template<class T>
  void switchColumns(dynamicMatrix<T>&A,int j, int k){
    if(j != k)
      for(int i=0; i<A.height(); i++){
        T keep = A(i,k,"read");
        A(i,k) = A(i,j,"read");
        A(i,j) = keep;
      }
  }  //  switch columns j and k

  template<class T>
  int maxRowI(const dynamicMatrix<T>&A,int i){
    int maxI = i;
    for(int j=i+1; j<A.width(); j++)
      if(fabs(A(i,j,"read"))>fabs(A(i,maxI,"read")))
        maxI = j;
    return maxI;
  }  //  column of maximal element in row i

  template<class T>
  const dynamicMatrix<T>
  LUP(const dynamicMatrix<T>&A, dynamicVector<int>&P){
    dynamicMatrix<T> LU = A;
    for(int j=0; j<A.width(); j++){
      P(j) = maxRowI(LU,j);
      switchColumns(LU,j,P[j]);
      for(int i=j+1; i<A.height(); i++){
        LU(i,j) = LU(i,j,"read") / LU(j,j,"read");
        for(int k=j+1; k<A.width(); k++)
	  LU(i,k) -= LU(i,j,"read") * LU(j,k,"read");
      }
    }
    return LU;
  }  //  LUP factorization

  template<class T>
  const T
  det(const dynamicMatrix<T>&LU, const dynamicVector<int>&P){
    T detLU = 1;
    for(int i=0; i<LU.height(); i++){
      detLU *= LU(i,i,"read");
      if(P[i] != i)
        detLU *= -1;
    }
    return detLU;
  }  //  determinant using LU factorization

  template<class T>
  const dynamicVector<T>
  invert(const dynamicMatrix<T>&LU, const dynamicVector<int>&P, const dynamicVector<T>&v){
    dynamicVector<T> x = v;
    for(int i=1; i<v.dim(); i++)
      for(int j=0; j<i; j++)
        x(i) -= LU(i,j,"read") * x[j];
    for(int i=v.dim()-1; i>=0; i--){
      for(int j=v.dim()-1; j>i; j--)
        x(i) -= LU(i,j,"read") * x[j];
      x(i) /= LU(i,i,"read");
        
    }
    for(int i=v.dim()-2; i>=0; i--)
      if(i!=P[i]){
        T keep = x[i];
	x(i) = x[P[i]];
	x(P[i]) = keep;
      }
    return x;
  }  //  invert for some right-hand-side vector

  template<class T>
  const dynamicMatrix<T>
  invert(const dynamicMatrix<T>&LU, const dynamicVector<int>&P){
    dynamicMatrix<T> inverse(LU.height(),LU.width());
    dynamicVector<T> zero(LU.height(),0.);
    for(int j=0; j<LU.width(); j++){
      dynamicVector<T> ej = zero;
      ej(j) = 1.;
      dynamicVector<T> columnJ = invert(LU,P,ej);
      for(int i=0; i<LU.height(); i++)
        inverse(i,j) = columnJ[i];

    }
    return inverse;
  }  //  invert a matrix using its LU factorization

  template<class T>
  const T det(const dynamicMatrix<T>&A){
    if(A.height()==1)
      return A(0,0,"read");
    T detA = 0.;
    int sign = 1;
    for(int j=0; j<A.height(); j++){
      detA += sign * A(0,j,"read") * det(minor(0,j,A));
      sign *= -1;
    }
    return detA;
  }  //  determinant using the original definition

  template<class T>
  const dynamicMatrix<T>  minor(int k, int l, const dynamicMatrix<T>&m){
    dynamicMatrix<T> minorkl(m.height()-1,m.width()-1);
    int ii=-1;
    for(int i=0; i<m.height(); i++)
      if(i!=k){
        ii++;
        int jj=-1;
        for(int j=0; j<m.width(); j++)
          if(j!=l)
	    minorkl(ii,++jj) = m(i,j,"read");
      }
    return minorkl;
  }  //  (k,l)th minor of m

  template<class T>
  const dynamicMatrix<T>
  inverse(const dynamicMatrix<T>&A){
    dynamicMatrix<T> Ainverse(A.height(),A.width());
    for(int i=0; i<A.height(); i++)
      for(int j=0; j<A.width(); j++)
        Ainverse(i,j) = power(-1,i+j) * det(minor(j,i,A));
    return Ainverse/det(A);
  }  //  inverse using Cramer's rule

template<class T> class difference2:public list<dynamicMatrix<T> >{
 public:
  difference2(int,int,const T&,const T&,const T&,const T&,
      const T&,const T&,const T&,const T&,const T&);
  const difference2<T>& operator+=(const difference2<T>&);
  const difference2<T>& operator-=(const difference2<T>&);
  const difference2& operator*=(const T&);
  T& operator()(int i,int j,int k,int l){
    return (*item[(k-i+1)*3+l-j+1])(i,j);
  }  //  (i,j,k,l)th element
  const T& operator()(int i,int j,int k,int l, char*) const{
    return (*item[(k-i+1)*3+l-j+1])(i,j,"read");
  }  //  (i,j,k,l)th element (read only)
  int width() const{ return item[0]->width(); }
  int height() const{ return item[0]->height(); }
};
  template<class T>
  difference2<T>::difference2(int m=0,int n=0,const T&a=0,const T&b=0,
      const T&c=0,const T&d=0,const T&e=0,const T&f=0,const T&g=0,
      const T&h=0,const T&i=0){
    number = 9;
    item = new dynamicMatrix<T>*[9];
    item[0] = new dynamicMatrix<T>(m,n,a);
    item[1] = new dynamicMatrix<T>(m,n,b);
    item[2] = new dynamicMatrix<T>(m,n,c);
    item[3] = new dynamicMatrix<T>(m,n,d);
    item[4] = new dynamicMatrix<T>(m,n,e);
    item[5] = new dynamicMatrix<T>(m,n,f);
    item[6] = new dynamicMatrix<T>(m,n,g);
    item[7] = new dynamicMatrix<T>(m,n,h);
    item[8] = new dynamicMatrix<T>(m,n,i);
  }

  template<class T>
  const difference2<T>& difference2<T>::operator+=(const difference2<T>&d){
    for(int i=0; i<number; i++)
      *item[i] += d[i];
    return *this;
  }  //  adding another difference2 to the current one

  template<class T>
  const difference2<T>& difference2<T>::operator-=(const difference2<T>&d){
    for(int i=0; i<number; i++)
      *item[i] -= d[i];
    return *this;
  }  //  subtracting another difference2 from the current one

  template<class T>
  const difference2<T>& difference2<T>::operator*=(const T&t){
    for(int i=0; i<number; i++)
      *item[i] *= t;
    return *this;
  }  //  multiplying the difference2 operator by a scalar T

  template<class T>
  const difference2<T> operator+(const difference2<T>&d1,
	  const difference2<T>&d2){
    return difference2<T>(d1) += d2;
  }  //  addition of two difference2s

  template<class T>
  const difference2<T> operator-(const difference2<T>&d1,
	  const difference2<T>&d2){
    return difference2<T>(d1) -= d2;
  }  //  subtraction of two difference2s

  template<class T>
  const difference2<T> operator*(const T&t, const difference2<T>&d){
    return difference2<T>(d) *= t;
  }  //  scalar times difference2

  template<class T>
  const difference2<T> operator*(const difference2<T>&d, const T&t){
    return difference2<T>(d) *= t;
  }  //  difference2 times scalar

  template<class T>
  const dynamicMatrix<T> operator*(const difference2<T>&d, const dynamicMatrix<T>&v){
    dynamicMatrix<T> dv(v.height(),v.width(),0);
    for(int i=0; i<v.height(); i++)
      for(int j=0; j<v.width(); j++)
        for(int k=max(0,i-1); k<=min(v.height()-1,i+1); k++)
          for(int l=max(0,j-1); l<=min(v.width()-1,j+1); l++)
	    dv(i,j) += d(i,j,k,l,"read")*v(k,l,"read");
    return dv;
  }  //  difference2 times dynamicMatrix

  template<class T>
  const dynamicMatrix<T> operator/(const dynamicMatrix<T>&f, const difference2<T>&d){
    dynamicMatrix<T> x(f);
    for(int iteration=0; iteration < 100; iteration++)
      for(int i=0; i<f.height(); i++)
        for(int j=0; j<f.width(); j++){
	  double residual = f(i,j,"read");
          for(int k=max(0,i-1); k<=min(f.height()-1,i+1); k++)
            for(int l=max(0,j-1); l<=min(f.width()-1,j+1); l++)
	      residual -= d(i,j,k,l,"read")*x(k,l,"read");
	  x(i,j) += residual/d(i,j,i,j,"read");
        }
    return x;
  }  //  solving d*x=f approximately by 10 GS iterations

template<class T> class xytGrid:public list<dynamicMatrix<T> >{
 public:
  xytGrid(int,int,int,const T&);
  int timeSteps() const{ return size(); }  // number of time steps
  int width() const{ return item[0]->width(); }  // width of grid
  int height() const{ return item[0]->height(); }  // height of grid
  dynamicMatrix<T>& operator()(int i){if(item[i])return *item[i];}//ith plane
  T& operator()(int i,int j,int k){return (*item[i])(j,k); }// (i,j,k)th comp.
};

  template<class T>
  xytGrid<T>::xytGrid(int m=0,int n=0,int l=0,const T&a=0){
    number = m;
    item = m ? new dynamicMatrix<T>*[m] : 0;
    for(int i=0; i<m; i++)
      item[i] = new dynamicMatrix<T>(n,l,a);
  }  //  constructor

double F(double, double, double){return 0.;}
double C1(double, double, double){return 0.;}
double C2(double, double, double){return 0.;}
double Alpha(double, double, double){return 0.;}
double G(double, double, double){return 0.;}
double Initial(double x, double y){return (1.-x*x)*(1.-y*y);}

  template<class T>
  void convDif(difference2<T>&d,dynamicMatrix<T>&f,double hx,
	  double hy,double deltaT,double t){
      for(int k=0; k<d.height(); k++)
       for(int j=0; j<d.width(); j++){
	if(t>deltaT/2)f(k,j)=F(j*hx,k*hy,t-deltaT/2);
	double c1=C1(j*hx,k*hy,t)/hx;
        if(c1>0.){
	  d(k,j,k,j)=c1;
	  d(k,j,k,j-1)=-c1;
        }
        else{
	  d(k,j,k,j)=-c1;
	  d(k,j,k,j+1)=c1;
        }
	double c2=C2(j*hx,k*hy,t)/hy;
        if(c2>0.){
	  d(k,j,k,j)+=c2;
	  d(k,j,k-1,j)=-c2;
        }
        else{
	  d(k,j,k,j)-=c2;
	  d(k,j,k+1,j)=c2;
        }
       }
      d += Epsilon * difference2<T>(d.height(),d.width(),
	      0,-1/hy/hy,0,-1/hx/hx,2/hx/hx+2/hy/hy,-1/hx/hx,0,-1/hy/hy,0);
      for(int k=0; k<d.height(); k++){
        d(k,0,k,0) += d(k,0,k,-1);
        d(k,0,k,0) -= d(k,0,k,-1) * hx * Alpha(0.,k*hy,t);
        if(t>deltaT/2){
          f(k,0) -= d(k,0,k,-1) * hx * G(0,k*hy,t-deltaT/2);
          f(k,d.width()-1) -= d(k,d.width()-1,k,d.width())
	      * G(d.width()*hx,k*hy,t-deltaT/2);
	}
      }
      for(int j=0; j<d.width(); j++){
        d(0,j,0,j) += d(0,j,-1,j);
        d(0,j,0,j) -= d(0,j,-1,j) * hy * Alpha(j*hx,0.,t);
        if(t>deltaT/2){
          f(0,j) -= d(0,j,-1,j) * hy * G(j*hx,0.,t-deltaT/2);
          f(d.height()-1,j) -= d(d.height()-1,j,d.height(),j)
	      * G(j*hx,d.height()*hy,t-deltaT/2);
        }
      }
    }  //  set the convection-diffusion matrix and right-hand side

  template<class T>
  void march(xytGrid<T>&g, double hx, double hy, double deltaT){
    difference2<T> I(g.height(),g.width(),0,0,0,0,1,0,0,0,0);
    for(int k=0; k<g.height(); k++)
      for(int j=0; j<g.width(); j++)
        g(0,k,j) = Initial(j*hx,k*hy);
    dynamicMatrix<T> f(g.height(),g.width());
    difference2<T> previous(g.height(),g.width());
    convDif(previous,f,hx,hy,deltaT,0);
    for(int time=1; time < g.timeSteps(); time++){
      difference2<T> current(g.height(),g.width());
      convDif(current,f,hx,hy,deltaT,time*deltaT);
      g(time) = ((I - 0.5 * deltaT * previous) * g[time-1] + deltaT * f)
	      / (I + 0.5 * deltaT * current);
      previous = current;
    }
    print(g[g.timeSteps()-1]);
  }  //  semi-implicit time marching

class domain2{
  xytGrid<double> g;
  double Time;
  double Width;
  double Height;
public:
  domain2(double T, double Lx, double Ly, double accuracy):
      g((int)(T/accuracy)+1,(int)(Ly/accuracy)+1,(int)(Lx/accuracy)+1),
      Time(T),Width(Lx),Height(Ly){}  //  constructor
  void solveConvDif(){march(g,Width/g.width(),Height/g.height(),
      Time/g.timeSteps());}  //  solve convection-diffusion equation
};

 int main(){
   dynamicMatrix<double> A(3,3);
   matrix<double,3,3> a;
   matrix<double,3,3> W;
   matrix<double,3,3> Z;
   vector<double,3> d;
   A(0,0) = 10.;
   a(0,0,"write") = 10.;
   A(0,1) = 1.2;
   a(0,1,"write") = 1.2;
   A(0,2) = -1.7;
   a(0,2,"write") = -1.7;
   A(1,0) = -5.;
   a(1,0,"write") = -5.;
   A(1,1) = 0.1;
   a(1,1,"write") = 0.1;
   A(1,2) = -2.7;
   a(1,2,"write") = -2.7;
   A(2,0) = -3.;
   a(2,0,"write") = -3.;
   A(2,1) = 10.1;
   a(2,1,"write") = 10.1;
   A(2,2) = -0.7;
   a(2,2,"write") = -0.7;
   dynamicVector<int> P(3,0);
   dynamicMatrix<double> LU = LUP(A,P);
   print(A);
   printf("\n");
   print(inverse(A));
   printf("\n");
   print(invert(LU,P));
   printf("\n");
   printf("det(A)=%f\n",det(A));
   printf("det(A)=%f\n",det(LU,P));
   diagonalize(W,a,Z,d);
   printf("det(A)=%f\n",det(d));
   printf("\n");

   domain D(10.,1.,.1);
   D.solveConvDif();
   domain2 D2(10.,.3,1.,.1);
   D2.solveConvDif();
   return 0;
 }
