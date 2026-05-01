class SampList {

public:

// Class data (sorting lists)
int ncell, nsamp;
double *ave_n, *ave_ux, *ave_uy, *ave_uz, *ave_T;

// Default Constructor. 
SampList() {
  initLists(1);
}

// Regular Constructor. 
SampList(int ncell_in) {
  initLists(ncell_in);
}


// Destructor. Called when a SampList object goes out of scope.
~SampList() {
  delete [] ave_n;   // Release allocated memory
  delete [] ave_ux;
  delete [] ave_uy;
  delete [] ave_uz;
  delete [] ave_T;
}

//*********************************************************

private:

// Initialization routine
void initLists(int ncell_in) {
  ncell = ncell_in;
  nsamp = 0;
  ave_n = new double [ncell+1];  // Allocate memory
  ave_ux = new double [ncell+1];
  ave_uy = new double [ncell+1];
  ave_uz = new double [ncell+1];
  ave_T = new double [ncell+1];
  int i;
  for( i=1; i<=ncell; i++ ) {
	ave_n[i] = 0;
	ave_ux[i] = 0;
	ave_uy[i] = 0;
	ave_uz[i] = 0;
	ave_T[i] = 0;
  }
}

}; // Class SampList

