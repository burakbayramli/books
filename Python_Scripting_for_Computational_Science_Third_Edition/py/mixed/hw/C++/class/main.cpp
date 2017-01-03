#include "HelloWorld.h"
int main(int argc, const char* argv[])
{
  if (argc < 3) {  /* need two command-line arguments */
    std::cout << "Usage: " << argv[0] << " r1 r2\n";  exit(1);
  }
  double r1 = atof(argv[1]);  double r2 = atof(argv[2]);

  HelloWorld hw;
  hw.set(r1, r2);
  std::cout << "hw.message:" << hw;
}

  
