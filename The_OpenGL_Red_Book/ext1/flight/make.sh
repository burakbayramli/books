g++ -c landscape.cpp -o landscape.o -lX11 -lGL -lGLU -lglut -g -Wall -O2 
g++ fly.cpp -g -Wall -O2 -o r.exe -lX11 -lGL -lGLU -lglut landscape.o

