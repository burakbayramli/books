// Registry_Type.h
// manage a registry of dense arrays
// 2007/10/06
//---------------------------------------------------------
#ifndef NDG__Registry_Type_H__INCLUDED
#define NDG__Registry_Type_H__INCLUDED


#ifdef _DEBUG
#define SHOW_Reg_ALLOC    1
#define CHECK_Reg_ALLOC   1
#else
#define SHOW_Reg_ALLOC    0
#define CHECK_Reg_ALLOC   0
#endif

#include "LOG_funcs.h"


// Select sizes for the two small, 
// preallocated registries:

const int iReg_small =   16;  // (M,N)=( 4, 4)
//const int iReg_med =  256;  // (M,N)=(16,16)
  const int iReg_med =  512;  // (M,N)=(16,32)
//const int iReg_med = 1024;  // (M,N)=(32,32)

typedef enum {
  umREG_SMALL   = 0,
  umREG_MEDIUM  = 1,
  umREG_GENERAL = 2
} umREG_size;


//---------------------------------------------------------
template <typename T> 
class umRegistry
//---------------------------------------------------------
{
protected:

  T    **dbase;
  int   *curlen;
  int   *maxlen;
  bool  *inuse;

  int   num_alloc; 
  int   MAX_alloc;

  int   m_iNextSlot;
  int   m_iRegSize;
  bool  m_bFixedSize;

public:
  umRegistry (
    int   N     = 200,    // initial number of slots
    bool  bInit = false,  // preallocate all elements?
    int   Nlen  = 0);     // length of preallocate arrays

  virtual ~umRegistry();

  int   size() const { return num_alloc; }
  std::string get_reg_name() const;
  void  release_all();  // release all allocations
  int   compact();      // release unused allocations
  void  expand();
  bool  can_store(int i, int N);
  T*    get_alloc(int N, int& user_id);
  int   select_length(int N);
  int   select_max_length(int N);
  T*    resize_alloc(const T* ptr, const int N, int& user_id);
  int   add_alloc(T *ptr, int N);
  bool  check_alloc(const T* ptr, const int user_id, const int N);
  void  free_alloc  (T *& ptr, int user_id);  // mark as available
  void  free_alloc_2(T *& ptr, int user_id);  // free allocation
  void  show_alloc() const;
};


//---------------------------------------------------------
template <typename T>
umRegistry<T>::umRegistry 
(
  int   N,        // initial number of slots
  bool  bInit,    // preallocate all elements?
  int   Nlen      // length of preallocate arrays
)
//---------------------------------------------------------
: dbase(NULL), curlen(NULL), maxlen(NULL), inuse(NULL),
  num_alloc(0), MAX_alloc(0),
  m_iNextSlot(0), m_iRegSize(0), m_bFixedSize(false)
{
  if      (iReg_small == Nlen) { m_iRegSize = iReg_small;}  // small registry
  else if (iReg_med   == Nlen) { m_iRegSize = iReg_med; }   // medium registry
  else                         { m_iRegSize = 0; }          // arbitrary registry
    
#if (SHOW_Reg_ALLOC)
  // Use printf: Log files not available when static object created.
  std::string msg = get_reg_name();
  if (bInit) { printf("Creating %s  (%d arrays length = %d)\n", msg.c_str(), N, Nlen); } 
  else       { printf("Creating %s  (%d slots available)\n", msg.c_str(), N);}
#endif

  assert(NULL==dbase);  // TODO: singleton object
  MAX_alloc = N;
  num_alloc = 0;
  dbase  = (T ** ) calloc((size_t)N, sizeof(T* ));   assert(dbase);
  curlen = (int* ) calloc((size_t)N, sizeof(int));   assert(curlen);
  maxlen = (int* ) calloc((size_t)N, sizeof(int));   assert(maxlen);
  inuse  = (bool*) calloc((size_t)N, sizeof(bool));  assert(inuse);

  if (!bInit || Nlen<1)
  {
    // set up an empty database

    m_bFixedSize = false;
    for (int i=0; i<MAX_alloc; ++i) {
      dbase [i] = NULL;
      curlen[i] = 0;
      maxlen[i] = 0;
      inuse [i] = false;
    }
  } 
  else 
  {
    // preallocate all elements

    m_bFixedSize = true;
    for (int i=0; i<MAX_alloc; ++i) {
      dbase [i] = (T*) calloc((size_t)Nlen, sizeof(T));   assert(dbase[i]);
      curlen[i] = 0;
      maxlen[i] = Nlen;
      inuse [i] = false;
    }
  }
}
  

//---------------------------------------------------------
template <typename T> 
umRegistry<T>::~umRegistry()
//---------------------------------------------------------
{
#if (SHOW_Reg_ALLOC)
  if (g_TRCFile && num_alloc>0) {
    std::string msg = get_reg_name();
    umTRC(1, "\nReleasing %s  (%d of %d used)\n", msg.c_str(), num_alloc, MAX_alloc);
  }
#endif

#if (SHOW_Reg_ALLOC)
  if (g_TRCFile && num_alloc>0) {
    show_alloc(); 
  }
#endif

  // free all allocations
  int i=0;
  for (i=0; i<num_alloc; ++i) 
  {

#if (SHOW_Reg_ALLOC)
    if (g_TRCFile) {
      int M=0, N=0;
      if (dbase[i]) { N=curlen[i]; M=maxlen[i]; }
      umTRC(5, "deleting obj (%3d/%3d)  size %6d [max:%6d] in use [%3s]\n", i+1, num_alloc, N,M, inuse[i]?"YES":" NO");
    }
#endif

    if (dbase[i]) {
      free (dbase[i]);
      dbase [i] = NULL;
      curlen[i] = 0;
      maxlen[i] = 0;
      inuse [i] = false;
    }
  }

#if (SHOW_Reg_ALLOC)

  if (!m_bFixedSize) {
    // check the remaining GENERAL entries are empty
    for (i=num_alloc; i<MAX_alloc; ++i) {
      if (dbase[i]) 
      { 
        if (g_TRCFile) {
          umTRC(1, "Expected dbase[%3d] to be empty\n", i); 
        } else {
          fprintf(stderr, "Expected dbase[%3d] to be empty\n", i); 
        }
      }
    }
  }

#endif

  // free all unused slots (pre-allocated and GENERAL)
  for (i=num_alloc; i<MAX_alloc; ++i) {
    if (dbase[i]) {
      free(dbase[i]);
      dbase [i] = NULL;
      curlen[i] = 0;
      maxlen[i] = 0;
      inuse [i] = false;
    }
  }

  free(dbase);    dbase = NULL;
  free(curlen);   curlen = NULL;
  free(maxlen);   maxlen = NULL;
  free(inuse);    inuse = NULL;

  num_alloc=0; MAX_alloc=0; m_iNextSlot=0;
}


//---------------------------------------------------------
template <typename T>
std::string umRegistry<T>::get_reg_name() const
//---------------------------------------------------------
{
  static char buf[50] = {""}; 
  static std::string sz1;          sz1 = "GENERAL";
  if      (iReg_small==m_iRegSize) sz1 = "SMALL  ";
  else if (iReg_med  ==m_iRegSize) sz1 = "MEDIUM ";
  sprintf(buf, "umRegistry<%s> %s", typeid(T).name(), sz1.c_str());
  return std::string(buf);
}


//---------------------------------------------------------
template <typename T>
void umRegistry<T>::release_all()
//---------------------------------------------------------
{
  // release all allocations
  umWARNING("umRegistry<T>::release_all()", "not implemented.");
  return this->size();
}


//---------------------------------------------------------
template <typename T>
int umRegistry<T>::compact()
//---------------------------------------------------------
{
  // release unused allocations

  if (m_bFixedSize) 
  {
#ifndef NBEBUG
    // check flags are consistent:
    for (int i=0; i<MAX_alloc; ++i) {
      if (! inuse[i]) {
        assert (0==curlen[i]);  // no current length
        assert (0==maxlen[i]);  // no max length
        assert (! inuse  [i]);  // not in use
      }
    }
#endif
    return this->size();
  }
  
  else
  {
    for (int i=0; i<MAX_alloc; ++i) {
      if (! inuse[i]) {
        free (dbase[i]);      // free this storage
        dbase [i] = NULL;     // reset the pointer
        curlen[i] = 0;        // book-keeping
        maxlen[i] = 0;        // book-keeping
        inuse [i] = false;    // mark slot as free
      } 
    }
    return this->size();
  }
}


//---------------------------------------------------------
template <typename T>
void umRegistry<T>::expand()
//---------------------------------------------------------
{
  int Nold = MAX_alloc;
  
  // Select amount by which to expand this registry:
  int N = MAX_alloc+100;

  if (iReg_small == m_iRegSize) {
    N = 2*MAX_alloc;    // for the "small" registry, double its size
  } else if (iReg_med == m_iRegSize) {
    N = 2*MAX_alloc;    // for the "medium" registry ... 
  }


  bool b1=true, b2=true, b3=true, b4=true;

  assert(dbase);

  dbase  = (T **) realloc(dbase,  N*sizeof(T*  ));  b1=(dbase ?true:false);
  curlen = (int*) realloc(curlen, N*sizeof(int ));  b2=(curlen?true:false);
  maxlen = (int*) realloc(maxlen, N*sizeof(int ));  b3=(maxlen?true:false);
  inuse  = (bool*)realloc(inuse,  N*sizeof(bool));  b4=(inuse ?true:false);

  if (!(b1 && b2 && b3 && b4)) {
    umERROR("umRegistry<T>::expand()", 
      "Problem expanding umRegistry<%s> from %d to %d\n", 
      typeid(T).name(), Nold, N);
  }

  //
  // initialize the new elements
  //
  if (m_bFixedSize)
  {
    // pre-allocate each slot in fixed-size registries
    for (int i=Nold; i<N; ++i) {
      dbase [i] = (T*) calloc((size_t)m_iRegSize, sizeof(T));
      curlen[i] = 0;          // current length of array in use
      maxlen[i] = m_iRegSize;  // actual length of allocation
      inuse [i] = false;
    }
  }
  else
  {
    // initialize the new slots
    for (int i=Nold; i<N; ++i) {
      dbase [i] = NULL;
      curlen[i] = 0;
      maxlen[i] = 0;
      inuse [i] = false;
    }
  }

  // update size of this registry
  MAX_alloc = N;

  std::string msg = get_reg_name();
  umMSG(1, "Expanded %s  (%d available)\n", msg.c_str(), MAX_alloc);
#if (SHOW_Reg_ALLOC)
  umTRC(1, "Expanded %s  (%d available)\n", msg.c_str(), MAX_alloc);
  show_alloc();
#endif
}
  

//---------------------------------------------------------
template <typename T>
bool umRegistry<T>::can_store(int i, int N)
//---------------------------------------------------------
{ 
  // Can allocation[i] store requested array?
  if (!dbase[i]) {
    return false;
  } else {
    return (N <= maxlen[i]) ? true : false;
  }
}


//---------------------------------------------------------
template <typename T>
T* umRegistry<T>::get_alloc(int N, int& user_id)
//---------------------------------------------------------
{

#if (CHECK_Reg_ALLOC)
  if (m_bFixedSize) {
    if      (iReg_small==m_iRegSize && N>iReg_small) { umERROR("umRegistry<%s>::get_alloc(%d)", "SMALL registry limted to %d", typeid(T).name(), N, iReg_small); }
    else if (iReg_med  ==m_iRegSize && N>iReg_med  ) { umERROR("umRegistry<%s>::get_alloc(%d)", "MEDIUM registry limted to %d", typeid(T).name(), N, iReg_med); }
  }
#endif

  // Get an allocation from the registry.
  // If necessary, adds a new allocation.
  assert(dbase);

  int max_idx = num_alloc;
  if (m_bFixedSize) 
    max_idx = MAX_alloc;

  // start search at m_iNextSlot
  for (int i=m_iNextSlot; i<max_idx; ++i) 
  {
    if (!inuse[i]) 
    {
      // Reuse slot [i], and adjust "next slot" marker
      m_iNextSlot = i+1;

      if (m_bFixedSize) { 
        // indicate another fixed allocation is in use
        num_alloc = std::max(num_alloc, i+1); 
      }

      user_id   = i;        // update user's id
      inuse[i]  = true;     // slot[i] is now taken

      if (can_store(i, N)) 
      {
        // garabage collection: if this is not a fixed-length 
        // registry, reallocate vectors that are too large:
        if (!m_bFixedSize) {

        //int MAXRLEN = select_max_length(N);
          int MAXRLEN = N;  // NBN: 2006/12/25

          if (MAXRLEN < maxlen[i]) {
            // reduce size of allocation [i]
            dbase[i]  = (T*) realloc(dbase[i], MAXRLEN*sizeof(T));
            maxlen[i] = MAXRLEN;   // actual length of allocation
          }
        }

        // For fixed-length registries, N <= m_iRegSize

        // allocation dbase[i] is big enough
        curlen[i] = N;      // length of array actually in use.
        return dbase[i];    // return pointer to this slot
      } 
      else 
      {
        int RLEN = N;
        if (!m_bFixedSize) {
          // make block larger than requested
        //RLEN = select_length(N);
          RLEN = N;  // NBN: 2006/12/25
        }
        // expand this allocation with realloc()
        dbase[i]  = (T*) realloc(dbase[i],  RLEN*sizeof(T));
        if (! dbase[i]) { 
          umERROR("umRegistry<T>::get_alloc", 
            "Failed to realloc block (%0.3lf million elements)", 
            double(RLEN)/1e6); 
        }

        curlen[i] = N;      // length of array actually in use.
        maxlen[i] = RLEN;   // actual length of allocation
        return dbase[i];    // return pointer to this slot
      }
    } 
    else 
    {
      m_iNextSlot = i+1;    // slot[i] is taken
    }
  }

  //-------------------------------------------------------
  // No free slots: allocate new object, add to registry
  //-------------------------------------------------------
  if (num_alloc < MAX_alloc)
  {
    user_id = num_alloc++;
    m_iNextSlot = user_id+1;

    int RLEN = N;
    if (!m_bFixedSize) {
      // make block larger than requested to allow reuse
      // for slightly larger requests in the future
    //RLEN = select_length(N);
      RLEN = N;  // NBN: 2006/12/25
    }

    dbase [user_id] = (T*) calloc((size_t)RLEN, sizeof(T));
    curlen[user_id] = N;      // current length of array in use
    maxlen[user_id] = RLEN;   // actual length of allocation
    inuse [user_id] = true;
    return dbase[user_id];
  }
  else
  {
    this->expand();
    return get_alloc(N, user_id);
  }
}



//---------------------------------------------------------
template <typename T>
int umRegistry<T>::select_length(int N)
//---------------------------------------------------------
{
  // Only used by registries with "non-fixed" length
  //
  // Select appropriate length to allocate when a 
  // request is made for N elements.  By making the 
  // allocation larger than requested, this block 
  // can accommodate slightly larger requests later.

  //###########################################
  // TODO: tune sizes as required
  // TODO: Handle usual (larger?) cases first
  //###########################################

  int RLEN = N; double x=0.0;
  if      (N >= 3000) { RLEN = N; }
  else if (N >= 1000) { x = sqrt(double(N))+10.0; RLEN = int(x*x); }
  else if (N >=  500) { x = sqrt(double(N))+10.0; RLEN = int(x*x); }
  else if (N >=  200) { x = sqrt(double(N))+10.0; RLEN = int(x*x); }
  else if (N >=  100) { x = sqrt(double(N))+ 5.0; RLEN = int(x*x); }
  else if (N >=   50) { x = sqrt(double(N))+ 5.0; RLEN = int(x*x); }
  else if (N >=   10) { RLEN = N+20; }
  else                { RLEN = N+10; }
  return RLEN;
}


//---------------------------------------------------------
template <typename T>
int umRegistry<T>::select_max_length(int N)
//---------------------------------------------------------
{
  int RLEN = select_length(N);
  if      (N >= 3000) { RLEN *= 3; }
  else if (N >= 1000) { RLEN *= 3; }
  else if (N >=  500) { RLEN *= 10; }
  else if (N >=  200) { RLEN *= 10; }
  else if (N >=  100) { RLEN *= 10; }
  else if (N >=   50) { RLEN *= 10; }
  else if (N >=   10) { RLEN *= 100; }
  else                { RLEN *= 100; }
  return RLEN;
}


//---------------------------------------------------------
template <typename T>
T* umRegistry<T>::resize_alloc(const T* ptr, const int N, int& user_id)
//---------------------------------------------------------
{
  // Adjust the size of an existing allocation.  If the 
  // pointer is NULL, call get_alloc() and update user_id
  assert(dbase);
  assert(user_id < num_alloc);

  if (m_bFixedSize)
  {
    assert(N <= m_iRegSize);  // caller must "switch registries"
    assert(ptr == dbase[user_id]);
    curlen[user_id] = N;      // update current length of array.
  //maxlen[user_id] = ...;    // no change to fixed-length allocations
    inuse [user_id] = true;   // confirming still in use.
    return dbase[user_id];    // return user's pointer
  }

  //-------------------------------------
  // realloc slot in "dynamic" registry
  //-------------------------------------
  try
  {
    if (user_id < 0)
    {
      // Unexpected:  current block should be registered!
      // Call get_alloc() and update user_id
      assert(NULL==ptr);
      if (ptr) { umWARNING("umRegistry<%s>::resize_alloc()", "expected ptr to be NULL!"); }
      T* pnew = get_alloc(N, user_id);
      if (!pnew) { 
        umERROR("umRegistry<T>::resize_alloc", 
                "Failed to allocate block (%0.3lf million elements)", 
                double(N)/1e6); 
      }
      return pnew;
    }

    // during LU sparse factorizations, large allocations 
    // can be realloc'd to < half original size.
    // check for this condition and recover the memory

    else if ( (N<=  maxlen[user_id]  ) && 
              (N>=2*maxlen[user_id]/3) )
    {
      // this request fits in the current slot and is similar 
      // in size. Re-use allocation and update book-keeping:

      assert(ptr == dbase[user_id]);
      curlen[user_id] = N;      // update length of array actually in use.
    //maxlen[user_id] = ...;    // no change to actual length of allocation
      inuse [user_id] = true;   // confirming still in use.
      return dbase[user_id];    // return user's pointer
    }
    else
    {
      // We need either to "expand" the allocation, 
      // or "reduce" an unnecessarily large block:

      int i = user_id;
      assert(ptr == dbase[i]);

    //int RLEN = select_length(N);
      int RLEN = N;  // NBN: 2007/01/07

      dbase[i] = (T*) realloc(dbase[i],  RLEN*sizeof(T));
      if (! dbase[i]) {umERROR("umRegistry<T>::resize_alloc", "Failed to realloc block (%0.3lf million elements)", double(RLEN)/1e6);}
      curlen[i] = N;      // length of array actually in use.
      maxlen[i] = RLEN;   // actual length of allocation
      inuse[i]  = true;   // confirming still in use.
      return dbase[i];    // return pointer to this slot
    }
  }
  catch (...)
  {
    umWARNING("umRegistry<T>::resize_alloc(%d)", 
              "Exception while allocating block (%d bytes)", 
              N, N*sizeof(T)); 
    return NULL;
  }
}


//---------------------------------------------------------
template <typename T>
int umRegistry<T>::add_alloc(T *ptr, int N)
//---------------------------------------------------------
{
  // Insert a "raw" allocation with length N
  // into first free slot in the database.
  // Return the id of this slot to caller.

  assert(dbase);

  if (m_bFixedSize && (N > m_iRegSize) ) 
  {
    umWARNING("umRegistry<%s>::add_alloc(%d)", 
              "%d-element array added to FIXED-SIZED registry (limit = %d)",
              typeid(T).name(), N, N, m_iRegSize); 
  }

  for (int i=m_iNextSlot; i<num_alloc; ++i) {
    if (!inuse[i]) {
      free(dbase[i]);       // free previous allocation
      dbase [i] = ptr;      // insert raw object
      curlen[i] = N;        // current active length
      maxlen[i] = N;        // actual length of allocation
      inuse [i] = true;     // this slot is now taken
      m_iNextSlot = i+1;    // no free slots before this
      return i;             // Caller needs the id of this slot
    }
  }

  if (num_alloc < MAX_alloc-1)
  {
    int user_id = num_alloc++;
    m_iNextSlot = num_alloc;    // next free slot is next new slot

    // All entries in "fixed-size" registries are non-NULL
    if (m_bFixedSize) {
      free(dbase[user_id]);     // free existing fixed-size allocation
    } else {
      maxlen[user_id] = N;      // update actual length of allocation
      assert(! dbase[user_id]); // assume: pointer has not been used
    }

    dbase [user_id] = ptr;      // insert raw object
    curlen[user_id] = N;        // current active length
    inuse [user_id] = true;     // this slot is now taken
    return  user_id;
  }
  else
  {
    this->expand();
    return add_alloc(ptr, N);
  }
}


//---------------------------------------------------------
template <typename T>
bool umRegistry<T>::check_alloc(const T* ptr, const int user_id, const int N)
//---------------------------------------------------------
{
  // check for invalid ptr/id pairs
  if (user_id < 0) { 
    return false; 
  } else if (m_bFixedSize && (user_id >= MAX_alloc)) {
    return false;
  } else if (!m_bFixedSize && (user_id >= num_alloc)) {
    return false;
  }

  if (dbase) {
    if (ptr == dbase[user_id]) {
      // Pointer matches the allocation in slot[user_id].
      // If this is a fixed length registry, check length:
      if (m_bFixedSize && m_iRegSize<N) 
      {
        umWARNING("umRegistry<%s>::check_alloc()", 
                  "Array (%d) is bigger than fixed size (%d)", 
                  typeid(T).name(), N, m_iRegSize); 
      }
      return true;
    }
    else { return false; }
  } else { return false; }
}


//---------------------------------------------------------
template <typename T>
void umRegistry<T>::free_alloc(T *& ptr, int user_id)
//---------------------------------------------------------
{
  //
  // Mark allocation as available
  //

#if (CHECK_Reg_ALLOC)
  if (m_bFixedSize) { assert(user_id < MAX_alloc); }
  else              { assert(user_id < num_alloc); }
#endif

  if (user_id < 0)
  {
    assert(NULL==ptr);
    if (ptr) { 
      std::string msg = get_reg_name();
      umWARNING("%s - free_alloc()", "expected ptr to be NULL!", msg.c_str()); 
    }
  }
  else if (dbase)
  {
    assert(ptr == dbase[user_id]);
    inuse [user_id] = false;  // this slot is available
    curlen[user_id] = 0;      // update current length
    ptr = NULL;               // zero the caller's pointer

    // slot[user_id] is free; update "first-free-slot" marker
    if (user_id >= 0)
      m_iNextSlot = std::min(m_iNextSlot, user_id);
  }
  else
  {
    // Note: destructors of static allocations 
    // may be called after program termination.
  }
}


//---------------------------------------------------------
template <typename T>
void umRegistry<T>::free_alloc_2(T *& ptr, int user_id)
//---------------------------------------------------------
{
  // Allow user to flush large arrays immediately.
  // free the allocation and mark as available

  if (m_bFixedSize || (user_id < 0)) 
  { 
    // pass to basic version
    free_alloc(ptr, user_id);
    return;
  }

  assert(user_id < num_alloc);

  if (dbase)
  {
    assert(ptr == dbase[user_id]);
    free(dbase[user_id]);     // free allocation
    dbase [user_id] = NULL;   // invalidate pointer
    inuse [user_id] = false;  // this slot is available
    curlen[user_id] = 0;      // update current length
    ptr = NULL;               // zero the caller's pointer

    // slot[user_id] is free; update "first-free-slot" marker
    if (user_id >= 0)
      m_iNextSlot = std::min(m_iNextSlot, user_id);
  }
  else
  {
    // Note: destructors of static allocations 
    // may be called after program termination.
  }
}



//---------------------------------------------------------
template <typename T>
void umRegistry<T>::show_alloc() const
//---------------------------------------------------------
{
  if (!g_TRCFile)
    return;

  assert(dbase);
  bool bExist=false;
  int i=0,N=0,M=0,iUse=0;

  std::string msg = get_reg_name();
  umTRC(1, "--------------------------\n%s\n"
    "  i   Use?    cur    max  \n", msg.c_str());

  if (num_alloc<1) {
    umTRC(1, "[Empty]\n\n");
    return;
  }

  for (i=0; i<num_alloc; ++i) {
    // report state of allocation [i]
    iUse   = inuse[i] ? 1 : 0;
    bExist = dbase[i] ? true : false;
    if (bExist) {
      N = curlen[i];
      M = maxlen[i];
      umTRC(1, "%3d:  [%d] [%6d,%6d]\n", i,iUse,N,M);
    } else {
      umTRC(1, "%3d:  [%d] [ -----, -----]\n", i, iUse);
    }
  }
  umTRC(1, "\n");
}


#undef SHOW_Reg_ALLOC
#undef CHECK_Reg_ALLOC

#endif  // NDG__Registry_Type_H__INCLUDED
