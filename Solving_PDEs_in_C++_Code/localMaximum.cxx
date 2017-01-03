//  LOCAL MAXIMUM (cHAPTER 1.20)
//  -------------------------
//  This program finds a local maximum of f(x)=cos(x)
//  in a large x-interval using recursive and nonrecursive
//  versions of the bisection and golden-ratio
//  algorithms (Chapter 1.20).
//  The detailed explanation is in the book.

  #include<stdio.h>
  #include<math.h>

  double f(double x){
    //return x*(1.-x);
    return cos(x);
  }

  double max(double a, double b){
    return a>b ? a : b;
  }  /* maximum of real numbers */

  double bisection(double a, double b){
    double Fa = f(a);
    double Fb = f(b);
    double midpoint = (a+b)/2;
    double Fmidpoint = f(midpoint);
    while(b-a>1.e-6){
      double left = (a+midpoint)/2;
      double Fleft = f(left);
      double right = (midpoint+b)/2;
      double Fright = f(right);
      if(Fmidpoint>max(Fleft,Fright)){
        a = left;
        Fa = Fleft;
        b = right;
        Fb = Fright;
      }
      else{
        if(Fleft>Fright){
          b = midpoint;
          Fb = Fmidpoint;
          midpoint = left;
          Fmidpoint = Fleft;
        }
        else{
          a = midpoint;
          Fa = Fmidpoint;
          midpoint = right;
          Fmidpoint = Fright;
        }
      }
    }
    return midpoint;
  }

  double bisection(double a, double b,
                   double midpoint, double Fa,
                   double Fb, double Fmidpoint){
    double left = (a+midpoint)/2;
    double Fleft = f(left);
    double right = (midpoint+b)/2;
    double Fright = f(right);
    return b-a < 1.e-6 ? midpoint
       : Fmidpoint > max(Fleft,Fright) ?
       bisection(left,right,midpoint,Fleft,Fright,Fmidpoint)
       : Fleft > Fright ? 
       bisection(a,midpoint,left,Fa,Fmidpoint,Fleft)
       : bisection(midpoint,b,right,Fmidpoint,Fb,Fright);
  }  /* local maximum by recursive bisection */

  double goldenRatio(double a, double b){
    double Fa = f(a);
    double Fb = f(b);
    double right = a + (-0.5+sqrt(5.)/2.) * (b-a);
    double Fright = f(right);
    double left = a + b - right;
    double Fleft = f(left);
    while(b-a>1.e-6){
      if(Fleft>Fright){
        b = right;
        Fb = Fright;
        right = left;
        Fright = Fleft;
        left = a + b - right;
        Fleft = f(left);
      }
      else{
        a = left;
        Fa = Fleft;
        left = right;
        Fleft = Fright;
        right = b - (left - a);
        Fright = f(right);
      }
    }
    return (a+b)/2;
  }

  double goldenRatio(double a, double left,
          double right, double b, double Fa,
          double Fleft, double Fright, double Fb){
    return b-a < 1.e-6 ? (a+b)/2
           : Fleft > Fright ?
           goldenRatio(a,a+right-left,left,right,
               Fa,f(a+right-left),Fleft,Fright)
           : goldenRatio(left,right,b+left-right,b,
               Fleft,Fright,f(b+left-right),Fb);
  }

  int main(){
    double a = -100.;
    double b = 10.;
    printf("%f\n",bisection(a,b));
    printf("%f\n",bisection(a,b,(a+b)/2,f(a),f(b),f((a+b)/2)));
    printf("%f\n",goldenRatio(a,b));
    double right = a + (-0.5+sqrt(5.)/2.) * (b-a);
    double left = a + b - right;
    printf("%f\n",goldenRatio(a,left,right,b,
            f(a),f(left),f(right),f(b)));
    return 0;
  }
