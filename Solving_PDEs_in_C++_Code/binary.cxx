  //  BINARY REPRESENTATION (CHAPTER 1.18)
  //  ---------------------------------
  //  This program contains recursive and nonrecursive 
  //  versions of the integer logarithm 
  //  (the integer part of the logarithm) and
  //  binary representation of integer number.
  //  It also computes the binary path leading from some inteeger
  //  number to a larger one (list of integers
  //  obtained by adding or subtracting
  //  subsequently larger and larger powers of 2).
  //  The complete explanation is in the book.

  #include<stdio.h>
  void path(int i, int j){
    if(i <= j){
      int go = i;
      int power = 1;
      int increment = 0;
      printf("%d\n",i);
      while(j){
	if(increment = (j%2 - i%2) * power)
	  printf("%d\n", go += increment);
	i /= 2;
	j /= 2;
	power *= 2;
      }
    }
    else
      path(j,i);
  }

  int reversed(int number, int base){
    int result=0;
    while (number){
      result *= 10;
      result += number % base;
      number /= base;
    }
    return result;
  }  /*  reversing an integer number  */

  int binary2(int n){
    int last = 1;
    if(!(n%2)){
      last = 0;
      n += 1;
    }
    return reversed(reversed(n,2),10) - (1-last);
  }

  int log2(int n){
    int log2 = 0;
    while(n>1){
      n /= 2;
      log2++;
    }
    return log2;
  }

  int log(int n){
    return n>1 ? log(n/2)+1 : 0;
  }

  int binary(int n){
   return n>1 ? 10*binary(n/2)+n%2 : n%2;
  }

  void printBinary(int n){
   if(n>1) printBinary(n/2);
   printf("%d",n%2);
  }

  int main(){
    int n = 778;
    printf("log2(%d)=%d\n",n,log2(n));
    printf("log2(%d)=%d\n",1,log2(1));
    printf("log(%d)=%d\n",n,log(n));
    printf("log(%d)=%d\n",1,log(1));
    printf("binary(%d)=%d\n",n,binary(n));
    printf("binary(%d)=%d\n",0,binary(0));
    printf("binary(%d)=%d\n",1,binary(1));
    printBinary(n);
    printf("\n");
    printBinary(0);
    printf("\n");
    printBinary(1);
    printf("\n");
    printf("binary2(%d)=%d\n",n,binary2(n));
    printf("binary2(%d)=%d\n",0,binary2(0));
    printf("binary2(%d)=%d\n",1,binary2(1));
    path(68,1019);
    return 0;
  }
