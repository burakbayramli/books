#--------------------------------------------------------
# Test of random variable generators
#--------------------------------------------------------
# ME 491/591 Non-equilibrium gas dynamics, Spring 2017
# Alexey N. Volkov, Univesity of Alabama, avolkov1@ua.edu
#--------------------------------------------------------

EXEPATH = .

CPP      = cc
CPPFLAGS =  
LDFLAGS  =

OBJS = RNGTest.o 
EXE = $(EXEPATH)/RNGTest

# compile and load
default:
	@echo " "
	@echo "Compiling Code of Program TNGTest"
	@echo "Test of random variable generators"
	$(MAKE) $(EXE)

$(EXE):	$(OBJS)
	$(CPP) $(CPPFLAGS) $(LDFLAGS) -lm -lg -lstdc++ -o $(EXE) $(OBJS)

.SUFFIXES: .cpp .o
.cpp.o:
	$(CPP) $(CPPFLAGS) -c $*.cpp











