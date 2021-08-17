// CS_Cholinc.cpp
// 
// 2007/10/16
//---------------------------------------------------------
#include "NDGLib_headers.h"

#include "CS_Type.h"

#define TRACE_CHOL 0


///////////////////////////////////////////////////////////
//
// Spa : buffer for storing sparse column info
//
///////////////////////////////////////////////////////////


//---------------------------------------------------------
class Spa 
//---------------------------------------------------------
{
public:
  Spa(int n) : length(0), m_status(0) { resize(n); }

  bool ok() const { return (m_status != 0) ? false : true; }
  bool resize(int n);
  void set(CSd& A, int j);
  void scale_add(int j, CSd& A, int k, double alpha);

public:
  int   length, m_status;
  IVec  indices, bitmap;
  DVec  values;
};


//---------------------------------------------------------
bool Spa::resize(int n) 
//---------------------------------------------------------
{
  length = 0;
  indices.resize(n); bitmap.resize(n); values.resize(n);
  if (indices.ok() && bitmap.ok() && values.ok()) 
  {
    bitmap.fill(-1);  // initialize bitmap
    m_status = 0;   return true;
  } else {
    m_status = -1;  return false;
  }
}


//---------------------------------------------------------
void Spa::set(CSd& A, int j)
//---------------------------------------------------------
{
  // initialize info for column L(:,j)
  assert(j < A.n);
  int next=0, i=0; double Aij=0.0;
  for (int ip = A.P[j]; ip < A.P[j+1]; ++ip) 
  {
    i = A.I[ip]; Aij = A.X[ip];
    assert( i >= j );           // A must be lower triangular
    indices[next] = i;
    values [i   ] = Aij;
    bitmap [i   ] = j;
    next++;
  }
  length = next;
}


//---------------------------------------------------------
void Spa::scale_add(int j, CSd& A, int k, double alpha)
//---------------------------------------------------------
{
  assert(k < A.n);

#if (TRACE_CHOL>=5)
  umMSG(1, "spa::scale_add: updating column %d with column %d\n",j,k);
  umMSG(1, "spa::scale_add: colptr %d to %d-1\n",A.P[k],A.P[k+1]);
#endif

  int next=0, i=0, ip=0; double Aik=0.0;
  for (int ip = A.P[k]; ip < A.P[k+1]; ++ip) 
  {
    i = A.I[ip];
    if (i < j) continue;
    Aik = A.X[ip];
    if ((this->bitmap)[i] < j) 
    {
#if (TRACE_CHOL>=3)
      umMSG(1, "fill in (%d,%d)\n",i,j);
#endif
      bitmap [ i ] = j;
      values [ i ] = 0.0;
      indices[length] = i;
      length++;
    }

    values[i] += alpha*Aik;

#if (TRACE_CHOL>=5)
    umMSG(1, "spa::scale_add: A(%d,%d) -= %lg * %lg ==> %lg\n", i,j, alpha, Aik, values[i]);
#endif
  }
}


///////////////////////////////////////////////////////////
//
// RowList : linked lists for mapping row dependencies
//
///////////////////////////////////////////////////////////


//---------------------------------------------------------
class RowList
//---------------------------------------------------------
{
public:

  RowList(int n);
  ~RowList() {}

  int create(int n);
  int add(int i, int j, double v);

  bool    ok() const { return (m_status != 0) ? false : true; }
  int     getfirst (int rl) { return rowlist       [ rl ]; }
  int     getnext  (int rl) { return next  [ rl ]; }
  int     getcolind(int rl) { return colind[ rl ]; }
  double  getvalue (int rl) { return values[ rl ]; }

protected:
  IVec  rowlist, next, colind;
  DVec  values;
  int   rowlist_size, freelist, next_expansion;
  int   m_status;
};


//---------------------------------------------------------
RowList::RowList(int n) 
//---------------------------------------------------------
: rowlist_size(0), freelist(0), next_expansion(0), m_status(0)
{
  // allocate initial rowlist structure
  m_status = create(n);
}


//---------------------------------------------------------
int RowList::create(int n)
//---------------------------------------------------------
{
  freelist = 0;
  rowlist_size = 1000; next_expansion = 1000;

  rowlist.resize(n);    // system is (n,n)

  next.resize  (rowlist_size);  // rowlist_size will grow
  colind.resize(rowlist_size);
  values.resize(rowlist_size);

  if (!rowlist.ok() || !next.ok() || !colind.ok() || !values.ok()) 
  { m_status = -1; return -1; }

  rowlist.fill(-1);  // -1 indicates: no list for row[i]
  
  for (int i=0; i<rowlist_size-1; ++i) { 
    next[i] = i+1; 
  }
  next[rowlist_size-1] = -1;
  
  return 0;
}


//---------------------------------------------------------
int RowList::add(int i, int j, double v)
//---------------------------------------------------------
{
  if ( -1 == freelist ) 
  {
    // expand storage for row info
    int inc = next_expansion, ii=0;

    next_expansion = (int) floor(1.25 * (double) next_expansion);

    int nlen = rowlist_size+inc;
    next.realloc(nlen);    if (!next.ok())   { return -1; }
    colind.realloc(nlen);  if (!colind.ok()) { return -1; }
    values.realloc(nlen);  if (!values.ok()) { return -1; }
    
    freelist = rowlist_size;
    for (int ii=rowlist_size; ii<nlen-1; ++ii) { 
      next[ii] = ii+1;      // initialize new entries
    }
    next[ nlen-1 ] = -1;    // set end marker
    rowlist_size = nlen;    // update current size
  }

  int rl = freelist;
  freelist = next[ freelist ];

  next  [ rl ] = rowlist[ i ];
  colind[ rl ] = j;
  values[ rl ] = v;
  
  rowlist[ i ] = rl;
  return 0;
}



///////////////////////////////////////////////////////////
//
// Incomplete Cholesky factorization
//
// This is a left-looking column-column code using
// row lists.  Performs a drop-tolerance incomplete
// factorization with or without diagonal modification
// to maintain rowsums.
//
///////////////////////////////////////////////////////////

// FIXME: (2007/09/21) "modified" option not yet working


// based on taucs_dccs_factor_llt
//---------------------------------------------------------
CSd& CS_Cholinc
(
  CSd&    A,
  double  droptol, 
  int     modified
)
//---------------------------------------------------------
{
  if (modified) {
    umWARNING("CS_Cholinc", "\"modified\" option not yet working");
    modified = 0;
  }

  CSd *pL = new CSd("L", OBJ_temp); CSd& L = *pL;

  if (! (A.get_shape() & sp_SYMMETRIC)) { umWARNING("CS_Cholinc", "matrix must be symmetric"); return L; }
  if (! (A.get_shape() & sp_LOWER    )) { umWARNING("CS_Cholinc", "tril(A) must be represented\n"); return L; }
  int n = A.num_cols();

  umMSG(1, " ==> CS_Cholinc: n=%d droptol=%0.1e modified? %d\n", n, droptol, modified);

  // Avoid frequent L.realloc() with large inital alloc 
  // TODO: tune initial allocation for incomplete factor:
  int Lnnz = A.size();
  if      (droptol>=9.9e-3) { Lnnz =  1*Lnnz;    }  // L.nnz = 1.0*A.nnz
  else if (droptol>=9.9e-4) { Lnnz = (3*Lnnz)/2; }  // L.nnz = 1.5*A.nnz
  else if (droptol>=9.9e-5) { Lnnz = (9*Lnnz)/5; }  // L.nnz = 1.8*A.nnz
  else if (droptol>=9.9e-6) { Lnnz =  2*Lnnz;    }  // L.nnz = 2.0*A.nnz
  else                      { Lnnz = (5*Lnnz)/2; }  // L.nnz = 2.5*A.nnz
  
  int init_Lnnz = Lnnz;
  L.resize(n,n,Lnnz, 1, 0);
  if (!L.ok()) { return L; }

  // factor is lower triangular
  L.set_shape(sp_TRIANGULAR | sp_LOWER);

  int next=0, Aj_nnz, i,j,k,ip;  double Lkj,pivot,v,norm;
  double flops = 0.0, Lj_nnz=0.0;

  Spa     spa(n);       // allocate buffer for sparse columns
  RowList rowlist(n);   // allocate initial rowlist structure
  DVec    dropped(n);   // allocate buffer for dropped values
  
  if (!spa.ok() || !rowlist.ok() || !dropped.ok()) {
    umWARNING("CS_Cholinc", "out of memory");
    return L;
  }

  umLOG(1, " ==> CS_Cholinc: (n=%d) ", n);
  for (j=0; j<n; ++j) 
  {
    if (! (j%2000)) {umLOG(1, ".");}

    spa.set(A,j);       // load colum j into the accumulation buffer

    for (int rl=rowlist.getfirst(j); rl != -1; rl=rowlist.getnext(rl)) {
      k   = rowlist.getcolind(rl);
      Lkj = rowlist.getvalue(rl);
      spa.scale_add(j,L,k, -(Lkj) );  // L_*j -= L_kj * L_*k
    }

    //-----------------------------------
    // insert the j'th column of L
    //-----------------------------------

    if ( next+(spa.length) > Lnnz ) 
    {
      int inc = std::max((int)floor(1.25*(double)Lnnz), std::max(8192, spa.length));
      Lnnz += inc;

      if (!L.realloc(Lnnz)) {
        return L;
      }
    }
    L.P[j] = next;

    norm = 0.0;
    for (ip=0; ip < spa.length; ++ip) {
      i = (spa.indices)[ip];
      v = (spa.values)[i];
      norm += v*v;
    }
    norm = sqrt(norm);

    Aj_nnz = A.P[j+1] - A.P[j];

    for (ip=0; ip < spa.length; ++ip) {
      i = (spa.indices)[ip];
      v = (spa.values )[i ];

      //###################################################
      // FIXME (a): test if L(i,j) is in pattern of A
      //###################################################

    //if (i==j || fabs(v) > droptol * norm)
      if (i==j || fabs(v) > droptol * norm || ip < Aj_nnz) 
      {
        // nothing
      } 
      else {
        dropped[i] -= v;
        dropped[j] -= v;
      }
    }

    if (modified) {
      pivot = sqrt((spa.values)[j] - dropped[j]);
    } else {
      pivot = sqrt((spa.values)[j]);
    }

#if (TRACE_CHOL>=2)
    umMSG(1, "pivot=%.4e, sqrt=%.4e\n", (spa.values)[j], pivot);
#endif

    if (0.0 == pivot) {
      umLOG(1, " ==> CS_Cholinc: zero pivot in column %d\n",j);
      umLOG(1, " ==> CS_Cholinc: Ajj in spa = %lg dropped[j] = %lg Aj_nnz=%d\n", (spa.values)[j],dropped[j],Aj_nnz);
    } else if (fabs(pivot) < 1e-12) {
      umLOG(1, " ==> CS_Cholinc: small pivot in column %d (%le)\n",j,pivot);
    }

    //-----------------------------------------------------
    // 1st pass: find the diagonal entry for column j then
    // store entry L(j,j) first in each compressed column:
    //-----------------------------------------------------
    for (ip=0; ip < spa.length; ++ip) 
    {
      i = (spa.indices)[ip];
      v = (spa.values )[i ];

      if (i==j) 
      {
        // must include diagonal entry in the droptol factor

        if (modified) v = (spa.values)[j] - dropped[j];

        v /= pivot;
        L.I[next] = i;
        L.X[next] = v;

        next++;
        if (rowlist.add(i,j,v) == -1) {
          return L;
        }
        break;
      }
    }

    //-----------------------------------------------------
    // 2nd pass: build column L(:,j) applying droptol 
    // criteria to manage fill-in below the diagonal
    //-----------------------------------------------------
    for (ip=0; ip < spa.length; ++ip) 
    {
      i = (spa.indices)[ip];
      v = (spa.values )[i ];
      
      if (i==j) continue; // diagonal was set above

      //###################################################
      // FIXME (b): test if L(i,j) is in pattern of A
      //###################################################

    //if (modified && i==j) v = (spa.values)[j] - dropped[j];
      if (i==j || fabs(v) > droptol*norm || ip < Aj_nnz) 
      {
        // include this entry in the droptol factor
        v /= pivot;
        L.I[next] = i;
        L.X[next] = v;

        next++;
        if (rowlist.add(i,j,v) == -1) {
          return L;
        }
      }
    }
    L.P[j+1] = next;                    // set column count
    Lj_nnz = (double)(L.P[j+1]-L.P[j]); // accumulate flop count
    flops += 2.0 * Lj_nnz * Lj_nnz;
  }
  L.P[n] = next;  // finalize column counts

  umLOG(1, "\n");

//umMSG(1, " ==> CS_Cholinc: nnz(L) = %d (init: %d), flops=%.1le\n", L.P[n],init_Lnnz,flops);
  umMSG(1, " ==> CS_Cholinc: nnz(L) = %d (init: %d)\n", L.P[n],init_Lnnz);

  // resize allocation
  L.realloc(0);

  return L;
}
