//  THE EXPONENT FUNCTION (CHAPTER 1.22)
//  ---------------------------------

//  This program computes the exponent function
//  using Taylor series and diagonal Pade series
//  (Chapter 1.22).
//  The complete explanation is in the book.

      #include<stdio.h>
      #include<math.h>
      double expTaylor(double arg){
	const int K=10;
	double x=arg;
	int m=0;
	while(fabs(x)>0.5){
	  x /= 2.;
	  m++;
	}
	double sum=1.;
	for(int n=K; n>00; n--){
	  sum *= x/n;
	  sum += 1.;
	}
	for(int i=0; i<m; i++)
	  sum *= sum;
	return sum;
      }  //  calculate exp(arg) using Taylor series of length 10

      double expPade(double arg){
	const int K=5;
	double x=arg;
	int m=0;
	while(fabs(x)>0.5){
	  x /= 2.;
	  m++;
	}
	double nom=1.;
	for(int n=K; n>00; n--){
	  nom *= x*(K-n+1)/(n*(2*K-n+1));
	  nom += 1.;
	}
	double denom=1.;
	for(int n=K; n>00; n--){
	  denom *= -x*(K-n+1)/(n*(2*K-n+1));
	  denom += 1.;
	}
	double sum = nom/denom;
	for(int i=0; i<m; i++)
	  sum *= sum;
	return sum;
      }  //  calculate exp(arg) using Pade series of length 10

      main(){
	printf("%f=%f=%f\n",exp(-2.),expTaylor(-2.),expPade(-2.));
      }
