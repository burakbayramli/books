//  POLYNOMIALS OF 1 AND 2 VARIABLES (CHAPTER 5)
//  --------------------------------------------
//  This program implements polynomials in 1 and 2
//  variables, with multiplication and integration
//  (Chapter 5).  In particular, it computes
//  p+p, p*p, and p(2) for p(x)=1+x+x*x and p*p
//  and its integral over the reference triangle
//  for p(x,y)=1+x+y.
//  The complete documentation is in the book.

#include<stdio.h>
#include<math.h>

int max(int a, int b){return a>b ? a : b;}
int min(int a, int b){return a<b ? a : b;}

double max(double a, double b){return a>b ? a : b;}
double min(double a, double b){return a<b ? a : b;}

double abs(double d){return d > 0. ? d : -d;}  //  absolute value

  void print(double d){
    printf("%f\n",d);
  }  //  print a double variable

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
  ~polynomial(){}
  int degree() const{return number-1;}
};

  template<class T>
  const T
  calculatePolynomial(const polynomial<T>&p, const T&x){
    T powerOfX = 1;
    T sum=0;
    for(int i=0; i<p.size(); i++){
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
const polynomial<T>& operator+=(polynomial<T>& p, const polynomial<T>&q){
  if(p.size() >= q.size())
    for(int i=0; i<q.size(); i++)
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

template<class T>
const polynomial<T>& operator*=(polynomial<T>& p, const T&a){
  for(int i=0; i<p.size(); i++)
      p(i) *= a;
  return p;
}  //  multiplication by scalar

template<class T>
const polynomial<T> operator*(const T&a, const polynomial<T>&p){
  polynomial<T> keep = p;
  return keep *= a;
}  //  scalar times polynomial

  template<class T>
  polynomial<T> 
  operator*(const polynomial<T>&p, const polynomial<T>&q){
    polynomial<T> result(p.degree()+q.degree()+1,0);
    for(int i=0; i<result.size(); i++)
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
  integral(const polynomial<T>&p){
    T sum = 0;
    for(int i=0; i<p.size(); i++)
      sum += (1./(i+1)) * p[i];
    return sum;
  }  //  integral on the unit interval

  template<class T>
  const T
  integral(const polynomial<polynomial<T> >&p){
    polynomial<T> sum(p.size()+1,0);
    polynomial<T> one(1,1);
    polynomial<T> x(2,0);
    x(1) = 1;
    polynomial<T> oneMinusX(2,1);
    oneMinusX(1) = -1;
    list<polynomial<T> > xPowers(p.size(),one);
    list<polynomial<T> > oneMinusXpowers(p.size()+1,one);
    for(int i=1; i<p.size(); i++)
      xPowers(i) = x * xPowers[i-1];
    for(int i=1; i<=p.size(); i++)
      oneMinusXpowers(i) = oneMinusX * oneMinusXpowers[i-1];
    for(int k=p.degree(); k>=0; k--)
      for(int j=0; j<=k; j++)
	sum += (p[k][j]/(j+1))
	    * oneMinusXpowers[j+1] * xPowers[k-j];
    return integral(sum);
  }  //  integral on the truangle

int main(){
  polynomial<double> p(3,1);
  print(p + p);
  print(p * p);
  print(calculatePolynomial(p,2.));
  print(HornerPolynomial(p,2.));
  polynomial<polynomial<double> > 
      p2(2,polynomial<double>(1,1));
  p2(1) = polynomial<double>(2,1);
  print(p2 * p2);
  print(integral(p2 * p2));
  return 0;
}
