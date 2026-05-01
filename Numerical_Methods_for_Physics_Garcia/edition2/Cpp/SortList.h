class SortList {

public:

// Class data (sorting lists)
int ncell, npart, *cell_n, *index, *Xref;

// Default Constructor. 
SortList() {
  initLists(1,1);
}

// Regular Constructor. 
SortList(int ncell_in, int npart_in) {
  initLists(ncell_in,npart_in);
}


// Destructor. Called when a SortList object goes out of scope.
~SortList() {
  delete [] cell_n;   // Release allocated memory
  delete [] index;
  delete [] Xref;
}

//*********************************************************

private:

// Initialization routine
void initLists(int ncell_in, int npart_in) {
  ncell = ncell_in;
  npart = npart_in;
  cell_n = new int [ncell+1];  // Allocate memory
  index = new int [ncell+1];
  Xref = new int [npart+1];
  int i;
  for( i=1; i<=ncell; i++ ) {
	cell_n[i] = 0;
	index[i] = 0;
  }
  for( i=1; i<=npart; i++ )
	Xref[i] = 0;
}

}; // Class SortList

