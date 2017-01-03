./tests.verify: test performed on 2003.07.03 


#### Test: ./tests.verify running ./make_module_1.sh 
CPU time of ./make_module_1.sh: 0.8 seconds on hpl-lapx30 i686, Linux


#### Test: ./tests.verify running ./hwa.py 1.2 -1.2
hw1, result: 0.0
hw2, result: Hello, World! sin(1.2+-1.2)=0
 hw3, result: 0.0
CPU time of ./hwa.py: 0.1 seconds on hpl-lapx30 i686, Linux


#### Test: ./tests.verify running ./make_module_2.sh 
running SWIG: swig -python -I.. hw.i
running build_ext
building '_hw' extension
creating build
creating build/temp.linux-i686-2.2
gcc -DNDEBUG -g -O3 -Wall -Wstrict-prototypes -fPIC -I.. -I/work/NO/ext/Linux/include/python2.2 -c ../hw.c -o build/temp.linux-i686-2.2/hw.o
gcc -DNDEBUG -g -O3 -Wall -Wstrict-prototypes -fPIC -I.. -I/work/NO/ext/Linux/include/python2.2 -c hw_wrap.c -o build/temp.linux-i686-2.2/hw_wrap.o
creating build/lib.linux-i686-2.2
gcc -shared build/temp.linux-i686-2.2/hw.o build/temp.linux-i686-2.2/hw_wrap.o -o build/lib.linux-i686-2.2/_hw.so
running SWIG: swig -python -I.. hw.i
running install
running build
running build_ext
skipping '_hw' extension (up-to-date)
running install_lib
copying build/lib.linux-i686-2.2/_hw.so -> .
CPU time of ./make_module_2.sh: 1.3 seconds on hpl-lapx30 i686, Linux


#### Test: ./tests.verify running ./hwa.py 1.2 -1.2
hw1, result: 0.0
hw2, result: Hello, World! sin(1.2+-1.2)=0
 hw3, result: 0.0
CPU time of ./hwa.py: 0.0 seconds on hpl-lapx30 i686, Linux

