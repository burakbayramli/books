#include <stdlib.h>  /* need atof */
#include <stdio.h>
#include <math.h>

double hw1(double r1, double r2)
{
  double s;
  s = sin(r1 + r2);
  return s;
}

void hw2(double r1, double r2)
{
  double s;
  s = sin(r1 + r2);
  printf("Hello, World! sin(%g+%g)=%g\n", r1, r2, s);
}

/* special version of hw1 where the result is an argument: */
void hw3(double r1, double r2, double *s)
{
  *s = sin(r1 + r2);
}


int main(int argc, char* argv[])
{
  double r1, r2, s;
  if (argc < 3) {  /* need two command-line arguments */
    printf("Usage: %s r1 r2\n", argv[0]);  exit(1);
  }
  r1 = atof(argv[1]);  r2 = atof(argv[2]);
  printf("hw1, result: %g\n", hw1(r1, r2));
  printf("hw2, result: ");
  hw2(r1, r2);
  printf("hw3, result: ");
  hw3(r1, r2, &s);
  printf("%g\n", s);
  return 0; /* success */
}
 
	
