CC=g++
CFLAGS=-fpermissive -I./Approximation -I./Containment -I./Curves -I./Distance -I./Geometry -I./Graphics -I./ImageAnalysis -I./Interpolation -I./Intersection -I./Math -I./Meshes -I./Miscellaneous -I./Numerics -I./Physics -I./RationalArithmetic -I./Surfaces -I./System -I./Tessellation

%.o: %.cpp
	$(CC) -c -o $@ $< $(CFLAGS)

OBJS=Physics/WmlRigidBody.o Physics/Main1.o 

all: $(OBJS)

#app1.exe: $(OBJS)
#	$(CC) $(OBJS) -o a.exe
#
#all: app1.exe
