//  From "COMPUTATIONAL PHYSICS", 3rd Ed, Enlarged Python eTextBook  
//  by RH Landau, MJ Paez, and CC Bordeianu
//  Copyright Wiley-VCH Verlag GmbH & Co. KGaA, Berlin;  Copyright R Landau,
//  Oregon State Unv, MJ Paez, Univ Antioquia, C Bordeianu, Univ Bucharest, 2015.
//  Support by National Science Foundation

// pi.c: *Monte-Carlo integration to determine pi

#include <stdio.h>
#include <stdlib.h>

// if you don't have drand48 uncomment the following two lines 
//    #define drand48 1.0/RAND_MAX*rand 
//    #define srand48 srand              

#define max 1000                     // number of stones to be thrown
#define seed 68111                       // seed for number generator

main()  {
	
  int i, pi = 0;
  double x, y, area;
  FILE *output;                              // save data in pond.dat
  output = fopen("pond.dat","w");
  srand48(seed);                         // seed the number generator
  for (i = 1; i<= max; i++) {
    x = drand48()*2-1;                      // creates floats between
    y = drand48()*2-1;                                    // 1 and -1
    if ((x*x + y*y)<1) pi++;                    // stone hit the pond
    area = 4*(double)pi/i;                          // calculate area
    fprintf(output, "%i\t%f\n", i, area);
  }
  printf("data stored in pond.dat\n");
  fclose(output);
}
 
