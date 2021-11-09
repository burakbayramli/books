# will put all the build files (*.o, etc.) in a separate directory
odir := _build

sources := $(wildcard *.f90)
objects := $(addprefix $(odir)/, $(sources:.f90=.o))

ALL: euler

# we assume gfortran
FFLAGS := -J $(odir) -I $(odir) -g -fbacktrace -fbounds-check -Wuninitialized -Wunused -ffpe-trap=invalid -finit-real=snan

# automagically find the interdependencies
$(odir)/deps: $(sources)
	@if [ ! -d $(odir) ]; then mkdir -p $(odir); fi
	./util/dep.py --prefix $(odir)/  --search_path . $(sources) > $(odir)/deps

include $(odir)/deps


$(odir)/%.o: %.f90
	@if [ ! -d $(odir) ]; then mkdir -p $(odir); fi
	gfortran $(FFLAGS) -c -o $@ $<

euler: $(objects)
	gfortran -o euler $(objects)


# targets for cleaning up
clean:
	rm -f $(odir)/*.o $(odir)/*.mod $(odir)/deps

realclean: clean
	@if [ -d $(odir) ]; then rmdir $(odir); echo "removing $(odir)"; fi
	rm -f euler
