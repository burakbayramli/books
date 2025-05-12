CC=g++
CFLAGS=-I./Approximation -I./Containment -I./Curves -I./Distance -I./Geometry -I./Graphics -I./ImageAnalysis -I./Interpolation -I./Intersection -I./Math -I./Meshes -I./Miscellaneous -I./Numerics -I./Physics -I./RationalArithmetic -I./Surfaces -I./System -I./Tessellation

TARGET=Physics/WmlRigidBody.o

%.o: %.cpp
	$(CC) -c -o $@ $< $(CFLAGS)
	
all: Physics/WmlRigidBody.o
