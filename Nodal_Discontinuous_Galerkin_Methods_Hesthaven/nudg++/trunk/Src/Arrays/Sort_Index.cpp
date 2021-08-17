// Sort_Index.cpp
// enable Matlab's "indexed sort" operation
// 2007/10/14
//---------------------------------------------------------
#include "NDGLib_headers.h"

#include "IndexSort_Type.h"


template <typename T>
class vec_index
{
public:
  T vec;
  int indx;
};


template <class T> bool ascending_compare (T a, T b)
{ return (a < b); }


template <class T> bool descending_compare (T a, T b)
{ return (a > b); }


template <class T>
bool ascending_compare (vec_index<T> *a, vec_index<T> *b)
{
  return (a->vec < b->vec);
}

template <class T>
bool descending_compare (vec_index<T> *a, vec_index<T> *b)
{
  return (a->vec > b->vec);
}


//---------------------------------------------------------
//template <class T>
//void Sort_Index
//(
//  const Vector<T>& A,   // [in] 
//        Vector<T>& S,   // [out]
//        IVec&      IX,  // [out]
//        eDir       dir  // =eAscend
//)
//---------------------------------------------------------
void Sort_Index
(
  const DVec& A,   // [in] 
        DVec& S,   // [out]
        IVec& IX,  // [out]
        eDir  dir  // =eAscend
)
//---------------------------------------------------------
{
  // For equal elements, the indices are such that 
  // the equal elements are listed in the order that 
  // appeared in the original list.

  int ns = A.length(), i=0;
  if (ns < 1) {
    S.resize(0);    // [empty] sorted vector
    IX.resize(0);   // [empty] index array
    return;
  } 

//MLIndex_sort<vec_index<T     > *> indexed_sort;
  MLIndex_sort<vec_index<double> *> indexed_sort;

  if (eAscend == dir) {
    indexed_sort.set_compare (ascending_compare);
  } else {
    indexed_sort.set_compare (descending_compare);
  }

  //OCTAVE_LOCAL_BUFFER (vec_index<T> *, vi, ns);
  //OCTAVE_LOCAL_BUFFER (vec_index<T>, vix, ns);

  vec_index<double>** vi = (vec_index<double>**) calloc(ns, sizeof(vec_index<double>*) );
  vec_index<double>* vix = (vec_index<double>* ) calloc(ns, sizeof(vec_index<double> ) );

  for (i=0; i<ns; ++i) { 
    vi[i] = &vix[i]; 
  }

  for (i=0; i<ns; ++i) {
    vi[i]->vec  = A[i];
    vi[i]->indx = i + 1;    // 1-based indices
  }

  indexed_sort.sort(vi, ns);

  S.resize(ns);             // the sorted vector
  IX.resize(ns);            // the index array

  for (i=0; i<ns; ++i) {
    S[i]  = vi[i]->vec;     // 
    IX[i] = vi[i]->indx;    // FIXME: check base!
  }
}
