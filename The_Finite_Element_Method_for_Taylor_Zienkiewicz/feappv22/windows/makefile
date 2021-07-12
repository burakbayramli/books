include ../makefile.in

SOURCES = $(FSOURCE)*.$(FEXT)

archive: $(SOURCES)
	$(AR) $(ARFEAP) $%
	@@echo "--->WINDOWS objects now up-to-date<---"
	rm -f $% *.o

install: $(SOURCES)
	@@echo "--->WINDOWS objects not installed in archive<---"

.f.o:
	$(FF) -c -I$(FINCLUDE) $(FFOPTFLAG) *.f -o $*.o

.c.o:
	$(CC) -c $(CCOPTFLAG) *.c -o $*.o

