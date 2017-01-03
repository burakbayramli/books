//  TAYLOR SCHEME FOR ODE'S (CHAPTER 6)
//  ----------------------------------
//  This program computes the Taylor approximation to
//  1/r at r=2.5. It also computes  the second derivative of 1/r*r
//  at r=2. 
// It also computes the numerical solution by Taylor scheme
//  to the ODE u'=2u at r=0.5 as well as  the discretization
//  error. Finally, it computes  the numerical solution by the Taylor scheme
//   to the Kuramoto-Sivashinski equation at r=0.15.
//  The full explanation is in the book (Chapter 6).

#include<stdio.h>
#include<math.h>

  int factorial(int n){
    return n>1 ? n * factorial(n-1) : 1;
  }  //  compute  n! using recursion

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
  }  //  positive of a vector

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
    matrix(const vector<T,N>&u){
      set(0,u);
    }  //  constructor
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
  void print(const T& d){
    printf("%f; ",d);
  }  //  print a double variable

  template<class T>
  void print(const list<T>&l){
    for(int i=0; i<l.size(); i++){
      printf("i=%d:\n",i);
      print(l[i]);
    }
  }  //  printing a list

  int reversed(int number){
    int result=0;
    while (number){
      result *= 10;
      result += number % 10;
      number /= 10;
    }
    return result;
  }  //  reversing an integer number

  template<class T>
  const T
  power2(const T&x, int n){
    T result = 1;
    T powerOfX = x;
    while(n){
      if(n % 2) result *= powerOfX;
      powerOfX *= powerOfX;
      n /= 2;
    }
    return result;
  }  //  compute a power

  template<class T>
  const T
  power(const T&x, int n){
    return n ? (n%2 ? x * power(x * x,n/2)
		: power(x * x,n/2)) : 1;
  }  //  compute a power

  template<class T>
  const T
  polynomial(const list<T>&a, const T&x){
    T powerOfX = 1;
    T sum=0;
    for(int i=0; i<a.size(); i++){
      sum += a[i] * powerOfX;
      powerOfX *= x;
    }
    return sum;
  }  //  compute a polynomial

  template<class T>
  const T
  Horner(const list<T>&a, const T&x){
    T result = a[a.size()-1];
    for(int i=a.size()-1; i>0; i--){
      result *= x;
      result += a[i-1];
    }
    return result;
  }  //  Horner algorithm to compute a polynomial

  template<class T>
  const list<vector<T,1> >
  deriveExp(const T&r, int n){
    list<vector<T,1> > Exp(n+1,0);
    Exp(0) = vector<T,1>(exp(2.*r));
    for(int i=0; i<n; i++)
      Exp(i+1) = vector<T,1>(2.*Exp[i][0]);
    return Exp;
  }  //  derivatives of r-inverse as list of vectors

  template<class T>
  const list<T>
  deriveRinverse(const T&r, int n){
    list<T> Rinverse(n+1,0);
    Rinverse(0) = 1/r;
    for(int i=0; i<n; i++)
      Rinverse(i+1) = -double(i+1)/r * Rinverse[i];
    return Rinverse;
  }  //  derivatives of r-inverse

  template<class T>
  const T
  Taylor(const list<T>&f, const T&h){
    T powerOfHoverIfactorial = 1;
    T sum=0;
    for(int i=0; i<f.size()-1; i++){
      sum += f[i] * powerOfHoverIfactorial;
      powerOfHoverIfactorial *= h/(i+1);
    }
    return sum;
  }  //  Taylor approximation

  template<class T>
  const T
  HornerTaylor(const list<T>&f, const T&h){
    T result = f[f.size()-2];
    for(int i=f.size()-2; i>0; i--){
      result *= h/i;
      result += f[i-1];
    }
    return result;
  }  //  Horner algorithm to compute Taylor approximation

  template<class T>
  const T
  deriveProduct(const list<T>&f, 
	  const list<T>g, int n){
  const int K=16;
  int triangle[K][K];
  for(int i=0; i<K; i++)
    triangle[i][0]=triangle[0][i]=1;
  for(int i=1; i<K-1; i++)
    for(int j=1; j<=K-1-i; j++)
      triangle[i][j] = triangle[i-1][j] + triangle[i][j-1];
    T sum = 0;
    for(int i=0; i<=n; i++)
      sum += triangle[n-i][i] * f[i] * g[n-i];
    return sum;
  }  //  nth derivative of a product

  template<class T, int N>
  const vector<T,N>
  TaylorScheme(const vector<T,N>&u0,
      const matrix<T,N,N>&S,
      const list<vector<T,N> >&f,
      const T&h){
    T powerOfHoverIfactorial = 1;
    vector<T,N> sum=0;
    vector<T,N> uDerivative = u0;
    for(int i=0; i<f.size(); i++){
      sum += powerOfHoverIfactorial * uDerivative;
      uDerivative = S * uDerivative + f[i];
      powerOfHoverIfactorial *= h/(i+1);
    }
    return sum;
  }  //  Taylor scheme

  template<class T, int N>
  const vector<T,N>
  error(const vector<T,N>&u0,
      const matrix<T,N,N>&S,
      const list<vector<T,N> >&f,
      const T&h){
    T powerOfHoverIfactorial = 1;
    vector<T,N> uDerivative = u0;
    for(int i=0; i<f.size(); i++){
      uDerivative = S * uDerivative + f[i];
      powerOfHoverIfactorial *= h/(i+1);
    }
    return powerOfHoverIfactorial * uDerivative;
  }  //  error in Taylor scheme

  template<class T>
  void deriveKS(const T&c, const T&r,
      list<T>&u, list<T>&v, list<T>&w){
    list<T> Rinverse = deriveRinverse(r,u.size());
    for(int i=0; i<u.size()-1; i++){
      u(i+1)=v[i]-deriveProduct(u,Rinverse,i);
      v(i+1) = w[i];
      w(i+1) = (-0.5)*deriveProduct(u,u,i)
            - deriveProduct(w,Rinverse,i)
            - v[i] + (i ? 0. : (c*c) );
    }
  }  //  derivatives in KS equation

  template<class T>
  const vector<T,3>
  TaylorKS(const T&c, const T&r, const T&h, int n,
      const vector<T,3>&u0,
      const vector<T,3>&bound){
    list<T> u(n,0.);
    list<T> v(n,0.);
    list<T> w(n,0.);
    u(0) = u0[0];
    v(0) = u0[1];
    w(0) = u0[2];
    deriveKS(c,r,u,v,w);
    vector<T,3> result(HornerTaylor(u,h),
       HornerTaylor(v,h), HornerTaylor(w,h));
    u(0) = bound[0];
    v(0) = bound[1];
    w(0) = bound[2];
    deriveKS(c,r,u,v,w);
    vector<T,3> highDerivative(u[n-1],v[n-1],w[n-1]);
    vector<T,3> error = (power(h,n-1)/factorial(n-1)) * highDerivative;
    return result + error;
  }  //  Taylor for KS equation + error

  int main(){
    list<double> Rinverse = deriveRinverse(2.,5);
    list<vector<double,1> > Exp = deriveExp(.4,5);
    printf("1/2.5=%f\n",Taylor(Rinverse,0.5));
    printf("1/2.5=%f\n",HornerTaylor(Rinverse,0.5));
    printf("(1/r*r)''(2)=%f\n",deriveProduct(Rinverse,Rinverse,2));
    printf("0.5*exp(1)=");
    print(TaylorScheme(vector<double,1>(.4*exp(.8)),
	matrix<double,1,1>(vector<double,1>(2.)),Exp,0.1));
    printf("\n");
    printf("error=");
    print(error(vector<double,1>(2.),
	matrix<double,1,1>(vector<double,1>(2.)),Exp,0.1));
    printf("\n");
    printf("Taylor scheme for Kuramoto-SivashinskiS:\n");
    vector<double,3> initial(0.1,-1.85,0.1);
    print(TaylorKS(.5,0.1,0.05,15,initial,initial));
    return 0;
  }
