#include <something.h>
void superLibFunc (char* method, float x)
{ printf("method=%s with x=%g",method,x); }

void someFunc ()
{
  /* calling up the super superLibFunc function */
  superLibFunc(a,x);  superLibFunc(qqq,ppp);
  /* other calls of superLibFunc */
  superLibFunc ( method1, method2 );
  superLibFunc(3method /* illegal name! */, method2 ) ;  
  superLibFunc(  _method1,method_2) ;
  /* here is a more challenging call wrt regex construction: */
  superLibFunc (
                method1 /* the first method we have */ ,
            super_method4 /* a special method that
                                 deserves a two-line comment... */
               ) ;
}

