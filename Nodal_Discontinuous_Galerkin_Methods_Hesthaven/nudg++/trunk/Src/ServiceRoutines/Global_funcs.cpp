// Global_funcs.cpp
//
// 2007/10/18
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "Global_funcs.h"
#include <math.h>

// for debug printing {DM,IM} and {DV,IV} arrays
#include "MatObj_Type.h"
#include "VecObj_Type.h"


// free unused allocations in the array registries
//---------------------------------------------------------
void NDG_garbage_collect()
//---------------------------------------------------------
{
  DVec dv; dv.compact();  umTRC(2, "*** compacted <double> *** \n");
  IVec iv; iv.compact();  umTRC(2, "*** compacted <int>    *** \n");
}


// return n! as a double
//---------------------------------------------------------
double gamma(int n)
//---------------------------------------------------------
{
  assert (n>=1);
  double tmp = factorial(n-1);
  return tmp;
}
 

// Returns log(gamma(x)) for x > 0.
//---------------------------------------------------------
double lgamma(const double x)
//---------------------------------------------------------
{
  assert (x>0.0);
  static double cof[6] = { 
    76.18009172947146,
   -86.50532032941677,
    24.01409824083091,
    -1.231739572450155,
     0.1208650973866179e-2,
    -0.5395239384953e-5};

  double z=x, y=x, tmp=x+5.5;
  tmp -= (z+0.5)*log(tmp);
  double ser = 1.000000000190015;
  for (int j=0; j<=5; ++j) {
    ser += cof[j]/++y;
  }

  return -tmp+log(2.5066282746310005*ser/z);
}


// Returns gamma(x) for x > 0.
//---------------------------------------------------------
double gamma(const double x)
//---------------------------------------------------------
{
  assert (x>0.0);                 // check input valid
  int ix = int(x);                // check for integral arg
  if (x == ix) {                  // handle integer cases,
    return gamma(ix);             // gamma(n) = factorial(n-1)
  } else {
    double tmp = lgamma(x);       // calculate lgamma(x)
    assert (tmp < log(DBL_MAX));  // check for overflow
    return exp(tmp);              // exponentiate
  }
}


//---------------------------------------------------------
double factorial(int n)
//---------------------------------------------------------
{
  static int ntop=6;
  static double a[33] = {
    // table of precalculated results.
    // 0!   1!   2!   3!    4!     5!     6!
       1.0, 1.0, 2.0, 6.0, 24.0, 120.0, 720.0 }; 

  int j=0;
  assert (n>=0);

  // Note: large values will overflow
  if (n>32) {
    return exp(lgamma(double(n+1.0)));
  }

  // Manage a table of results. Both ntop and a[] 
  // are static, so multiple calls reuse results.

  while (ntop<n) { 
    j=ntop++;
    a[ntop] = a[j]*ntop; // add values to table
  }
  return a[n];
}


// r = [lo:hi] = range(lo,hi);
//---------------------------------------------------------
DVec& range(int lo, int hi)
//---------------------------------------------------------
{
  int N = (hi-lo)+1;  // num entries
  double dlo = double(lo), dhi = double(hi);

  // load vector with [lo:hi], stepping by 1
  DVec *tmp=new DVec(N, 0.0, OBJ_temp);
  for (int i=0; i<N; ++i) {
    (*tmp)[i] = dlo + double(i);
  }
  assert(dhi == (*tmp)(N));
  return (*tmp);
}


// v = ones(N,1) = ones(1,N);
//---------------------------------------------------------
DVec& ones(int N)
//---------------------------------------------------------
{
  char buf[32]={""}; snprintf(buf,(size_t)30,"ones(%d)",N);
  DVec *tmp=new DVec(N, 1.0, OBJ_temp, buf);
  return (*tmp);
}


// A = ones(M,N);
//---------------------------------------------------------
DMat& ones(int M, int N)
//---------------------------------------------------------
{
  char buf[52]={""}; snprintf(buf,(size_t)50,"ones(%d,%d)",M,N);
  DMat *tmp=new DMat(M,N, 1.0, OBJ_temp, buf);
  return (*tmp);
}


// v = zeros(N,1) = zeros(1,N);
//---------------------------------------------------------
DVec& zeros(int N)
//---------------------------------------------------------
{
  char buf[32]={""}; snprintf(buf,(size_t)30,"zeros(%d)",N);
  DVec *tmp=new DVec(N, 0.0, OBJ_temp, buf);
  return (*tmp);
}


// A = zeros(M,N);
//---------------------------------------------------------
DMat& zeros(int M, int N)
//---------------------------------------------------------
{
  char buf[52]={""}; snprintf(buf,(size_t)50,"zeros(%d,%d)",M,N);
  DMat *tmp=new DMat(M,N, 0.0, OBJ_temp, buf);
  return (*tmp);
}


// A = eye(N);
//---------------------------------------------------------
DMat& eye(int N)
//---------------------------------------------------------
{
  DMat *tmp=new DMat("eye", OBJ_temp);
  tmp->identity(N);
  return (*tmp);
}


// v = rand(N);
//---------------------------------------------------------
DVec& rand(int N)
//---------------------------------------------------------
{
  DVec *tmp=new DVec(N, "rand(N)", OBJ_temp);
  tmp->randomize(0.0, 1.0);
  return (*tmp);
}


// A = rand(M,N);
//---------------------------------------------------------
DMat& rand(int M, int N)
//---------------------------------------------------------
{
  DMat *tmp=new DMat(M,N, "rand(M,N)", OBJ_temp);
  tmp->randomize(0.0, 1.0);
  return (*tmp);
}


//---------------------------------------------------------
IVec& ceil(const DVec& V)
//---------------------------------------------------------
{
  int N = V.length();
  IVec *tmp=new IVec(N, "ceil(V)", OBJ_temp);
  for (int i=1; i<=N; ++i) {
    (*tmp)(i) = (int) (::ceil(V(i)));
  }
  if (V.get_mode() == OBJ_temp) { delete (&V); }
  return (*tmp);
}


//---------------------------------------------------------
// Matlab "intersect" operation
//---------------------------------------------------------
IVec& intersect(const IVec& A, const IVec& B)
{
  // return integer values contained in both 
  // A and B, sorted and without duplicates
  IVec *tmp = new IVec("(A)&(B)", OBJ_temp);

  int Na=A.size(), Nb=B.size(), i=0, val=0, sk=0;
  if (Na<1 || Nb<1) {
    // degenerate case: one array is empty
    tmp->resize(0);
  }
  else
  {
    typedef std::set<int>::iterator SetIt;

    // sort and remove duplicates from A and B by loading into sets
    std::set<int> sA, sB, sC;
    for (i=1; i<=Na; ++i) {sA.insert(A(i));} int lenA = (int) sA.size();
    for (i=1; i<=Nb; ++i) {sB.insert(B(i));} int lenB = (int) sB.size();

    // Use set "C" to store unique values from the 
    // smaller set which also occur in larger set
    SetIt it, it2;
    if (lenA <= lenB) {
      //---------------------------------
      // find values in A that occur in B
      //---------------------------------
      for (it=sA.begin(); it!=sA.end(); it++) {
        val = (*it);
        it2=sB.find(val);
        if (it2 != sB.end()) {
          sC.insert(val);
        }
      }
    } else {
      //---------------------------------
      // find values in B that occur in A
      //---------------------------------
      for (it=sB.begin(); it!=sB.end(); it++) {
        val = (*it);
        it2=sA.find(val);
        if (it2 != sA.end()) {
          sC.insert(val);
        }
      }
    }

    int lenC = (int) sC.size(); tmp->resize(lenC);  
    for (it=sC.begin(), sk=1; it!=sC.end(); it++, sk++) {
      (*tmp)(sk) = (*it);
    }
  }
  if (A.get_mode() == OBJ_temp) { delete (&A); }
  if (B.get_mode() == OBJ_temp) { delete (&B); }
  return (*tmp);
}



//---------------------------------------------------------
bool isInf(const DVec& V)
//---------------------------------------------------------
{
  for (int i=1; i<=V.size(); ++i) {

    double Vi = V(i);
    if (isinf(Vi)) {
      return true;    // v has a non-finite element
    }
  }
  return false;       // all elements are finite 
}


//---------------------------------------------------------
bool file_exists(string& fname)
//---------------------------------------------------------
{
  FILE *fp = fopen(fname.c_str(), "r");
  if (!fp) {
    return false;
  } else {
    fclose(fp);
    return true;
  }
}


// check if "bc_name" contains "szbc"
//---------------------------------------------------------
bool match_BC(string& bc_name, const char* szbc)
//---------------------------------------------------------
{
  using std::string;

  // trim
  string sn = trim(bc_name.c_str());
  string st = trim(szbc);

  // convert to lower case
  makelower(sn);
  makelower(st);

  if (sn.find(st.c_str()) != string::npos)
    return true;

  return false;
}



//---------------------------------------------------------
// debug utilities: print arrays to the MSG file
//---------------------------------------------------------

// bump up details
void dumpDMat2(const DMat& M, const char* s) { 
  M.print(g_MSGFile, s, "E", 2, 10, 101,0,100); 
}


// Numerical {DMat,IMat} and {DVec,IVec}
void dumpDMat(const DMat& M, const char* s) { M.print(g_MSGFile, s, "lf", 4, 8, 51,0,50); }
void dumpIMat(const IMat& M, const char* s) { M.print(g_MSGFile, s, "d",  0, 4, 51,0,50); }

void dumpDVec(const DVec& V, const char* s) { V.print(g_MSGFile, s, "lf", 4, 8, true); }
void dumpIVec(const IVec& V, const char* s) { V.print(g_MSGFile, s, "d",  0, 4, true); }

void dumpIMap(const IMap& V, const char* s) { V.print(g_MSGFile, s, "d",  0, 4, true); }


// simple {DM,IM} and {DV,IV} arrays
void dumpDM(const DM& A, const char* s) { 
  DMat M; M.load(A.num_rows(),A.num_cols(), A.data());
  M.print(g_MSGFile, s, "lf", 4, 8, 51,0,50); 
}
void dumpIM(const IM& A, const char* s) { 
  IMat M; M.load(A.num_rows(),A.num_cols(), A.data());
  M.print(g_MSGFile, s, "d",  0, 4, 51,0,50); 
}

void dumpDV(const DV& X, const char* s) { 
  DVec V; V.copy(X.size(), X.data());
  V.print(g_MSGFile, s, "lf", 4, 8, true); 
}
void dumpIV(const IV& X, const char* s) { 
  IVec V; V.copy(X.size(), X.data());
  V.print(g_MSGFile, s, "d",  0, 4, true); 
}



//---------------------------------------------------------
// special functions
//---------------------------------------------------------
#ifdef _MSC_VER
#define _jn jn
#endif

DVec& besselj(int N, const DVec& X)
{
  char buf[22]={""};
  snprintf(buf, (size_t)20, "besselj(%d)", N); 
  int len = X.size();
  DVec* tmp = new DVec(len, 0.0, OBJ_temp, buf);
  for (int i=1; i<=len; ++i) {
    (*tmp)(i) = jn(N, X(i));
  }

  if (X.get_mode() == OBJ_temp) { delete (&X); }

  return (*tmp);
}


#ifdef _MSC_VER
#undef jn
#endif


// tex "table" output
//---------------------------------------------------------
void textable
(
  string&   capt, 
  FILE*     fid, 
  string*   titles, 
  DMat&     entries, 
  string*   form
)
//---------------------------------------------------------
{
  int Nrows=entries.num_rows(), Ncols=entries.num_cols(), n=0,m=0;
  
  fprintf(fid, "\\begin{table} \n");
  fprintf(fid, "\\caption{%s} \n", capt.c_str());

  fprintf(fid, "\\begin{center} \n");
  fprintf(fid, "\\begin{tabular}{|");
  for (n=1; n<=Ncols; ++n) {
    fprintf(fid, "c|");
    if (2==n) {
      fprintf(fid, "|");
    }
  }
  fprintf(fid, "} \\hline \n ");

  for (n=1; n<=(Ncols-1); ++n) {
    fprintf(fid, "%s & ", titles[n].c_str());
  }
  fprintf(fid, " %s \\\\ \\hline \n ", titles[Ncols].c_str());

  for (m=1; m<=Nrows; ++m) {
    for (n=1; n<=(Ncols-1); ++n) {
      fprintf(fid, form[n].c_str(), entries(m,n)); fprintf(fid, " & ");
    }
    if (m<Nrows) {
      fprintf(fid, form[Ncols].c_str(), entries(m,Ncols)); fprintf(fid, " \\\\ \n ");
    } else {
      fprintf(fid, form[Ncols].c_str(), entries(m,Ncols)); fprintf(fid, " \\\\ \\hline \n ");
    }
  }
  fprintf(fid, "\\end{tabular} \n");
  fprintf(fid, "\\end{center} \n"); 
  fprintf(fid, "\\end{table} \n");
}
