EXEC = jacobi
O = .o

#### Linux
#
LD = mpicc
CC = mpicc
CFLAGS = -O3 -ansi -Wall -c
LDFLAGS = -O3 -o
LIBS = -lm

SOURCES = jacobi.c

OBJECTS = ${SOURCES:.c=$(O)}
.SUFFIXES: .c $(O)

.c$(O):
	$(CC) $(CFLAGS) $*.c

$(EXEC): $(OBJECTS)
	$(LD) $(LDFLAGS) $(EXEC) $(OBJECTS) $(LIBS)
