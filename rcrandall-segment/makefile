CC=g++
CFLAGS=-c -Wall -O2
SOURCES=ChanVeseSegmentation.cpp Image.cpp ReadWritePBM.cpp ZeroCrossings.cpp TestChanVese.cpp
OBJECTS=$(SOURCES:.cpp=.o)
LIBPNG=-lpng

all: $(SOURCES) segment

segment: $(OBJECTS)
	$(CC) $(OBJECTS) $(LIBPNG) -o $@

.cpp:
	$(CC) $(OBJECTS) $< -o $@

clean:
	rm -rf *.o segment out-*
