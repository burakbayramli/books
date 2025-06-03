CC=g++
CFLAGS=-O2 -DNDEBUG -DWML_USE_GLUT -I./Approximation -I./Containment -I./Curves -I./Distance -I./Geometry -I./Graphics -I./ImageAnalysis -I./Interpolation -I./Intersection -I./Math -I./Meshes -I./Miscellaneous -I./Numerics -I./Physics -I./RationalArithmetic -I./Surfaces -I./System -I./Tessellation

%.o: %.cpp
	$(CC) -c -o $@ $< $(CFLAGS)

#OBJS=Physics/WmlRigidBody.o Physics/Main1.o 

OBJS=Physics/WmlRigidBody2.o Physics/Main2.o 

all: $(OBJS)
#	$(CC) $(OBJS) -o a.exe
	
#app1.exe: $(OBJS)
#	$(CC) $(OBJS) -o a.exe
#
#all: app1.exe
