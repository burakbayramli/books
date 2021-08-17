// CS_Utils.cpp
// utility routines used by sparse CS<double> matrices
// 2007/10/08
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "CS_Type.h"



//---------------------------------------------------------
// column counts of LL'=A or LL'=A'A, given parent & post ordering
//---------------------------------------------------------
#define HEAD(k,j) (ata ? head [k] : j)
#define NEXT(J)   (ata ? next [J] : -1)
// helper routine for CS_counts
static void init_ata(const CSd& AT, const IVec& post, int *wd, int **head, int **next)
{
  int m=AT.n, n=AT.m, i=0,k=0,p=0;
  *head = wd+4*n, *next = wd+5*n+1;
  for (k=0; k<n; ++k) wd[post[k]]=k; // invert post
  for (i=0; i<m; ++i) {
    for (k=n, p=AT.P[i]; p<AT.P[i+1]; ++p) {
      k = std::min(k, wd[AT.I[p]]);
    }
    (*next)[i] = (*head)[k]; // place row i in linked list k
    (*head)[k] = i;
  }
}
//---------------------------------------------------------
IVec& CS_counts(const CSd& A, const IVec& parent, const IVec& post, int ata)
//---------------------------------------------------------
{
  IVec* colcount = new IVec("colcount", OBJ_temp);

  if (!A.ok() || !A.is_csc() || !parent.ok() || !post.ok()) 
  { umERROR("CS_counts", "invalid args"); return (*colcount); }

  int i=0, j=0, k=0, J=0, s=0, p=0, q=0, jleaf=0;
  int *head=NULL, *next=NULL;

  int m=A.m, n=A.n;
  s = 4*n + (ata ? (n+m+1) : 0);

  colcount->resize(n);        // allocate result
  IVec& delta = (*colcount);  // reference
  IVec& ccnt  = (*colcount);  // reference

  IVec w(s);                  // workspace
  CSd AT = trans(A, 0);       // AT = A'
  if (!AT.ok() || !colcount->ok() || !w.ok()) 
  { umERROR("CS_counts", "out of memory"); return (*colcount); }
  int *wd=w.data();
  int *ancestor=wd, *maxfirst=wd+n, *prevleaf=wd+2*n, *first=wd+3*n;
  w.fill(-1);                 // clear workspace w [0..s-1]
  for (k=0; k<n; ++k) {       // find first [j]
    j = post[k];              // delta[j]=1 if j is a leaf
    delta[j] = (first[j] == -1)?1:0;
    for ( ; j != -1 && first[j] == -1; j=parent[j]) 
      first[j] = k;
  }

  IVec &ATp=AT.P, &ATi=AT.I;
  if (ata) { init_ata (AT, post, wd, &head, &next); }
  for (i=0; i<n; ++i) ancestor[i] = i;  // each node in its own set
  for (k=0; k<n; ++k)
  {
    j = post[k];          // j is the kth node in postordered etree
    if (parent[j] != -1) delta[parent[j]]-- ;   // j is not a root
    for (J=HEAD(k,j); J != -1; J=NEXT(J))       // J=j for LL'=A case
    {
      for (p=ATp[J]; p<ATp[J+1]; ++p) {
        i = ATi[p] ;
        q = CS_leaf(i, j, first, maxfirst, prevleaf, ancestor, &jleaf);
        if (jleaf >= 1) delta[j]++ ;  // A(i,j) is in skeleton
        if (jleaf == 2) delta[q]-- ;  // account for overlap in q
      }
    }
    if (parent[j] != -1) ancestor[j] = parent[j];
  }
  // sum up delta's of each child
  for (j=0; j<n; ++j) {
    if (parent[j] != -1) 
      ccnt[parent[j]] += ccnt[j];
  }

  AT.reset();           // force deallocation of AT
  return (*colcount);   // success
}

// clear macros
#undef HEAD
#undef NEXT


// p [0..n] = cumulative sum of c[0..n-1],
// and then copy p [0..n-1] into c
//---------------------------------------------------------
double CS_cumsum(IVec& p, IVec& c, int n)
//---------------------------------------------------------
{
  if (!p.ok() || !c.ok()) return -1.0;
  int nz=0;  double nz2=0;
  for (int i=0; i<n; ++i) {
    p[i] = nz;
    nz  += c[i];
    nz2 += c[i];  // also in double to avoid int overflow
    c[i] = p[i];  // also copy p[0..n-1] back into c[0..n-1]
  }
  p[n] = nz ;
  return nz2;     // return sum (c [0..n-1])
}


// apply the ith Householder vector to x
//---------------------------------------------------------
int CS_happly(const CSd& V, int i, double beta, DVec& x)
//---------------------------------------------------------
{
  if (!V.is_csc() || !x.ok()) return 0;
  double tau=0; int p=0;
  for (p=V.P[i]; p<V.P[i+1]; ++p) {
    tau += V.X[p] * x[V.I[p]];      // tau = v'*x
  }
  tau *= beta;                      // tau = beta*(v'*x)
  for (p=V.P[i]; p<V.P[i+1]; ++p) {
    x[V.I[p]] -= V.X[p]*tau;        // x = x - v*tau
  }
  return 1;
}


// create a Householder reflection [v,beta,s]=house(x), 
// overwrite x with v, where (I-beta*v*v')*x = s*x.  
// See Algo 5.1.1, Golub & Van Loan, 3rd ed.
//---------------------------------------------------------
double CS_house(double *x, double *beta, int n)
//---------------------------------------------------------
{
  if (!x || !beta) return -1.0;     // check inputs
  double sigma=0.0, s=0.0;
  for (int i=1; i<n; ++i) sigma += x[i]*x[i];
  if (0.0 == sigma) {
    s = fabs(x[0]);                 // s = |x(0)|
    (*beta) = (x[0]<=0) ? 2.0 : 0.0;
    x[0] = 1.0;
  } else {
    s = sqrt(x[0]*x[0] + sigma) ;  // s = norm(x)
    x[0] = (x[0]<=0) ? (x[0]-s) : (-sigma / (x[0]+s));
    (*beta) = -1.0/(s*x[0]);
  }
  return s;
}


// x(p) = b. If p=NULL, identity is implied. Note that since 
// x is a workspace of arbitrary length, we must not simply
// assign (x=b);  just copy first n elements into workspace.
//---------------------------------------------------------
int CS_ipvec(const IVec& p, const DVec& b, DVec& x, int n)
//---------------------------------------------------------
{
  if (!x.ok() || !b.ok()) {umERROR("CS_ipvec", "invalid args"); return 0;}
  assert( (x.size() >= n) && (b.size() >= n));
  if (p.size()>0) {
    for (int k=0; k<n; ++k) x[p[k]] = b[k]; // permute
  } else {
    x.copy(n, b.data());  // load n elements (identity)
  }
  return 1;
}


// x = b(p). If p=NULL, identity is implied. Note that since 
// x is a workspace of arbitrary length, we must not simply
// assign (x=b);  just copy first n elements into workspace.
//---------------------------------------------------------
int CS_pvec(const IVec& p, const DVec& b, DVec& x, int n)
//---------------------------------------------------------
{
  if (!x.ok() || !b.ok()) {umERROR("CS_ipvec", "invalid args"); return 0;}
  assert( (x.size() >= n) && (b.size() >= n));
  if (p.size()>0) {
    for (int k=0; k<n; ++k) x[k] = b[p[k]];  // permute
  } else {
    x.copy(n, b.data());  // load n elements (identity)
  }
  return 1;
}


// consider A(i,j), node j in ith row subtree 
// and return lca(jprev,j)
//---------------------------------------------------------
int CS_leaf
(
  int i, int j, const int *first, 
  int *maxfirst, int *prevleaf,
  int *ancestor, int *jleaf
)
//---------------------------------------------------------
{
  int q=0, s=0, sparent=0, jprev=0;
  if (!first || !maxfirst || !prevleaf || !ancestor || !jleaf) return (-1);
  *jleaf = 0;
  if (i<=j || first[j] <= maxfirst[i]) return (-1);  // j not a leaf
  maxfirst[i] = first[j];       // update max first[j] seen so far
  jprev = prevleaf[i];          // jprev = previous leaf of ith subtree
  prevleaf[i] = j;
  *jleaf = (jprev == -1)?1:2;   // j is first or subsequent leaf
  if (*jleaf == 1) return (i);  // if 1st leaf, q = root of ith subtree
  for (q=jprev; q!=ancestor[q]; q=ancestor[q]);
  for (s=jprev; s!=q; s=sparent) {
    sparent = ancestor[s];      // path compression
    ancestor[s] = q;
  }
  return (q);  // q = least common ancester (jprev,j)
}


// return permutation vector according to seed:
// seed = -1: return p = n-1:-1:0.
// seed =  0: return p = identity.  
// else       random permutation.
//---------------------------------------------------------
void CS_randperm(int n, int seed, IVec& p)
//---------------------------------------------------------
{
  if (0 == seed) { 
    p.resize(0);
    return;                 // implies identity permutation
  } 

  p.range(n-1,0);           // generate reverse permutation
  assert(p.ok());
  if (-1 == seed) {
    return;                 // return reverse permutation
  }

  int j=0, t=0;             // else: randomize the permutation
  srand (seed);
  for (int k=0; k<n; ++k) {
    j = k+(rand()%(n-k));   // j = rand int in range k to n-1
    t = p[j];               // swap p[k] and p[j]
    p[j] = p[k];
    p[k] = t;
  }
}


// pinv = p', or p = pinv'
//---------------------------------------------------------
IVec& CS_pinv(const IVec& p, int n)
//---------------------------------------------------------
{
  IVec* pinv=new IVec("pinv", OBJ_temp);
  if (!p.ok()) { return (*pinv); }      // p = NULL denotes identity
  pinv->resize(n); assert(pinv->ok());  // allocate result
  int* pid = pinv->data();
  for (int k=0; k<n; ++k) pid[p[k]]=k;  // invert the permutation
  return (*pinv);
}


// post order a forest
//---------------------------------------------------------
IVec& CS_post(const IVec& parent, int n)
//---------------------------------------------------------
{
  IVec* post=new IVec("post", OBJ_temp);
  if (!parent.ok()) {umWARNING("CS_post", "empty parent data"); return (*post);}
  post->resize(n);                    // allocate result
  IVec w(3*n);                        // allocate workspace
  if (!w.ok() || !post->ok()) {umWARNING("CS_post", "out of memory"); return (*post); }
  int *wd=w.data(); int *head=wd, *next=wd+n, *stack=wd+2*n;

  int j=0, k=0;
  for (j=0; j<n; ++j) head[j] = -1;   // empty linked lists
  for (j=n-1; j>=0; j--) {            // traverse nodes in reverse order
    if (parent[j] == -1) continue;    // j is a root
    next[j] = head[parent[j]];        // add j to list of its parent
    head[parent[j]] = j ;
  }
  for (j=0; j<n; ++j) {
    if (parent[j] != -1) continue;    // skip j if it is not a root
    k = CS_tdfs(j, k, head, next, post->data(), stack);
  }
  return (*post);  // success; return post
}


///////////////////////////////////////////////////////////
//
// triangular solves: CS_[l,lt,u,ut]solve
//
///////////////////////////////////////////////////////////

// solve Lx=b where x and b are dense. 
// x=b on input, solution on output.
//---------------------------------------------------------
int CS_lsolve(const CSd& L, DVec& x)
//---------------------------------------------------------
{
  assert( L.ok() && L.is_csc() && x.ok() );

  int n = L.n;
  for (int j=0; j<n; ++j) {
    x[j] /= L.X[L.P[j]];
    for (int p=L.P[j]+1; p<L.P[j+1]; ++p) {
      x[L.I[p]] -= L.X[p] * x[j];
    }
  }
  return 1;
}


// solve L'x=b where x and b are dense.
// x=b on input, solution on output.
//---------------------------------------------------------
int CS_ltsolve(const CSd& L, DVec& x)
//---------------------------------------------------------
{
  assert( L.ok() && L.is_csc() && x.ok() );

  int n = L.n;
  for (int j=n-1; j>=0; j--) {
    for (int p=L.P[j]+1; p<L.P[j+1]; ++p) {
      x[j] -= L.X[p] * x[L.I[p]];
    }
    x[j] /= L.X[L.P[j]];
  }
  return 1;
}


// solve Ux=b where x and b are dense.
// x=b on input, solution on output.
//---------------------------------------------------------
int CS_usolve(const CSd& U, DVec& x)
//---------------------------------------------------------
{
  assert( U.ok() && U.is_csc() && x.ok() );

  int n = U.n;
  for (int j=n-1; j>=0; j--) {
    x[j] /= U.X[U.P[j+1]-1];
    for (int p=U.P[j]; p<U.P[j+1]-1; ++p) {
      x[U.I[p]] -= U.X[p]*x[j] ;
    }
  }
  return 1;
}


// solve U'x=b where x and b are dense.
// x=b on input, solution on output.
//---------------------------------------------------------
int CS_utsolve(const CSd& U, DVec& x)
//---------------------------------------------------------
{
  assert( U.ok() && U.is_csc() && x.ok() );

  int n = U.n;
  for (int j=0; j<n; ++j) {
    for (int p=U.P[j]; p<U.P[j+1]-1; ++p) {
      x[j] -= U.X [p] * x[U.I[p]];
    }
    x[j] /= U.X[U.P[j+1]-1];
  }
  return 1;
}



// solve Gx=b(:,k), where G is either 
// upper (lo=0) or lower (lo=1) triangular
//---------------------------------------------------------
int CS_spsolve
(
  CSd& G, const CSd& B, int k, 
  IVec& xi, DVec& x, const IVec& pinv, int lo
)
//---------------------------------------------------------
{
  assert(G.is_csc() && B.is_csc());
  assert(x.ok() && xi.ok());

  int n=G.n, j=0,J=0,p=0,q=0,px=0;
  IVec &Gp=G.P, &Gi=G.I; DVec &Gx=G.X;
  const IVec &Bp=B.P, &Bi=B.I; const DVec &Bx=B.X;
  int top = CS_reach(G,B,k,xi,pinv);    // xi[top..n-1]=Reach(B(:,k))
  for (p=top; p<n; ++p) x[xi[p]] = 0;   // clear x
  for (p=Bp[k]; p<Bp[k+1]; ++p) 
    x[Bi[p]]=Bx[p];                     // scatter B
  bool bPi= pinv.ok();                  // apply inv. perm?
  for (px=top; px<n; ++px) {
    j = xi[px];                         // x(j) is nonzero
    J = bPi ? (pinv[j]) : j;            // j maps to col J of G
    if (J < 0) continue;                // column J is empty
    x[j] /= Gx[lo?(Gp[J]):(Gp[J+1]-1)]; // x(j) /= G(j,j)
    p = lo ? (Gp[J]+1) : (Gp[J]);       // lo: L(j,j) 1st entry
    q = lo ? (Gp[J+1]) : (Gp[J+1]-1);   // up: U(j,j) last entry
    for (; p < q; p++) {
      x[Gi[p]] -= Gx[p] * x[j];         // x(i) -= G(i,j) * x(j)
    }
  }
  return top;     // return top of stack
}



///////////////////////////////////////////////////////////
//
// utilities...
//
///////////////////////////////////////////////////////////


// xi[top...n-1] = nodes reachable from graph of G*P' via nodes in B(:,k).
// xi[n...2n-1] used as workspace
//---------------------------------------------------------
int CS_reach(CSd& G, const CSd& B, int k, IVec& xi, const IVec& pinv)
//---------------------------------------------------------
{
  assert(G.is_csc() && B.is_csc() && xi.ok());

  IVec &Gp=G.P; const IVec& Bp=B.P, &Bi=B.I;
  int *xid=xi.data();
  int n=G.n, top=G.n, p=0;
  for (p=Bp[k]; p<Bp[k+1]; ++p) {
    if (!CS_MARKED(Gp, Bi[p])) {    // start a dfs at unmarked node i
      top = CS_dfs(Bi[p], G, top, xi, xid+n, pinv.data());
    }
  }
  for (p=top; p<n; ++p) CS_MARK (Gp, xi[p]);  // restore G
  return top;
}


// return 1 if row i is in R2
//---------------------------------------------------------
int CS_rprune(int i, int j, double aij, void *other)
//---------------------------------------------------------
{
  int *rr = (int *) other;
  return (i >= rr[1] && i < rr[2]);
}


//---------------------------------------------------------
// helper routine for CSd::dmperm()
// collect matched rows and columns into p and q
//---------------------------------------------------------
void CS_matched
(
  int n, 
  const int *wj, 
  const int *imatch, 
  IVec& p, IVec& q, 
  int *cc, int *rr, 
  int set, int mark
)
//---------------------------------------------------------
{
  int kc = cc[set];
  int kr = rr[set-1];
  for (int j=0; j<n; ++j) {
    if (wj[j] != mark) continue;    // skip if j is not in C set
    p[kr++] = imatch[j];
    q[kc++] = j;
  }
  cc[set+1] = kc;
  rr[set  ] = kr;
}


//---------------------------------------------------------
// helper routine for CSd::dmperm()
// collect unmatched rows into the permutation vector p
//---------------------------------------------------------
void CS_unmatched (int m, const int *wi, IVec& p, int *rr, int set)
//---------------------------------------------------------
{
  int kr = rr[set];
  for (int i=0; i<m; ++i) {
    if (0==wi[i]) {
      p[kr++] = i;
    }
  }
  rr[set+1] = kr;
}


//---------------------------------------------------------
// helper routine for CSd::dmperm()
// breadth-first search for coarse decomposition (C0,C1,R1 or R0,R3,C3)
//---------------------------------------------------------
bool CS_bfs
(
  const CSd& A, int n, int *wi, int *wj, IVec& queue, 
  const int *imatch, const int *jmatch, int mark
)
//---------------------------------------------------------
{
  assert(A.ok() && A.is_csc());

  int head=0, tail=0, j=0, i=0, p=0, j2=0;
  for (j=0; j<n; ++j)             // place all unmatched nodes in queue
  {
    if (imatch[j]>=0) continue;   // skip j if matched
    wj[j] = 0;                    // j in set C0 (R0 if transpose)
    queue[tail++] = j;            // place unmatched col j in queue
  }
  if (0 == tail) return true;     // quick return if no unmatched nodes

  // select either A or At
  CSd *C = NULL; bool use_trans=false;
  if (1==mark) {
    C = const_cast<CSd*>(&A);
  } else {
    use_trans = true;             // bfs of C=A' to find R3,C3 from R0
    C = new CSd(trans(A,0));      // copy structure only
    if (!C->ok()) { delete C; return false; }
  }

  const IVec& Ap=C->P, &Ai=C->I;  // use references
  while (head < tail)             // while queue is not empty
  {
    j = queue[head++];            // get the head of the queue
    for (p=Ap[j]; p<Ap[j+1]; ++p) {
      i = Ai[p];
      if (wi[i] >= 0) continue;   // skip if i is marked
      wi[i] = mark;               // i in set R1 (C3 if transpose)
      j2 = jmatch[i];             // traverse alternating path to j2
      if (wj[j2] >= 0) continue;  // skip j2 if it is marked
      wj[j2] = mark;              // j2 in set C1 (R3 if transpose)
      queue[tail++] = j2;         // add j2 to queue
    }
  }
  if (use_trans) { delete C; }    // free A' if created

  return true;
}


//---------------------------------------------------------
// helper routine for CSd::SCC()
// depth-first-search of matrix graph, starting at node j
//---------------------------------------------------------
int CS_dfs
(
  int j, CSd& G, int top, IVec& xi, 
  int *pstack, const int *pinv
)
//---------------------------------------------------------
{
  if (!G.ok() || !G.is_csc() || !xi.ok() || !pstack) 
  { umWARNING("CS_dfs", "invalid args"); return -1; }

  int i=0, p=0, p2=0, done=0, jnew=0;
  int head=0;
  xi[0] = j;                // initialize the recursion stack
  while (head >= 0)
  {
    j = xi[head];           // get j from the top of the recursion stack
    jnew = pinv ? (pinv[j]) : j;
    if (! CS_MARKED (G.P,j)) {
      CS_MARK (G.P, j);     // mark node j as visited
      pstack[head] = (jnew<0) ? 0 : CS_UNFLIP (G.P[jnew]);
    }
    done = 1;               // node j done if no unvisited neighbors
    p2 = (jnew<0) ? 0 : CS_UNFLIP (G.P[jnew+1]);
    for (p=pstack[head]; p<p2; ++p)
    {                       // examine all neighbors of j
      i = G.I[p];           // consider neighbor node i
      if ( CS_MARKED (G.P, i)) continue; // skip visited node i
      pstack[head] = p;     // pause depth-first search of node j
      xi[++head] = i;       // start dfs at node i
      done = 0;             // node j is not done
      break;                // break, to start dfs (i)
    }
    if (done) {       // depth-first search at node j is done
      head-- ;        // remove j from the recursion stack
      xi[--top] = j;  // and place in the output stack
    }
  }
  return top;
}

//---------------------------------------------------------
// helper routine for CS_amd()
// depth-first search + postorder of tree rooted at node j
//---------------------------------------------------------
int CS_tdfs
(
  int j, int k, 
  int *head, 
  const int *next, 
  int *post,   // result
  int *stack
)
//---------------------------------------------------------
{
  int top=0, i=0, p=0;
  if (!head || !next || !post || !stack) return -1;  // check inputs

  stack[0] = j ;          // place j on the stack
  while (top >= 0)        // while (stack is not empty)
  {
    p = stack[top];       // p = top of stack
    i = head [p];         // i = youngest child of p
    if (i == -1) {
      top-- ;             // p has no unordered children left
      post[k++] = p;      // node p is the kth postordered node
    } else {
      head [p] = next[i]; // remove i from children of p
      stack[++top] = i;   // start dfs on child node i
    }
  }
  return k;
}


//---------------------------------------------------------
// helper routine for CSd::maxtrans()
//
// find an augmenting path starting at column k 
// and extend the match if found
//---------------------------------------------------------
void CS_augment
(
  int k, const CSd &A, 
  int *jmatch, int *cheap, 
  IVec& w, int *js, int *is, int *ps
)
//---------------------------------------------------------
{
  int found=0, i = -1, head=0, j=0, p=0;
  const IVec &Ap=A.P, &Ai=A.I;
  js[0] = k;              // start with just node k in jstack
  while (head >= 0)
  {
    //-----------------------------------------------------
    // Start (or continue) depth-first-search at node j
    //-----------------------------------------------------
    j = js[head];         // get j from top of jstack
    if (w[j] != k)        // 1st time j visited for kth path
    {
      w[j] = k;           // mark j as visited for kth path
      for (p=cheap[j]; p<Ap[j+1] && !found; ++p) {
        i = Ai[p];        // try a cheap assignment (i,j)
        found = (-1 == jmatch[i]);
      }
      cheap[j] = p;       // start here next time j is traversed
      if (found) {
        is[head] = i;     // column j matched with row i
        break;            // end of augmenting path
      }
      ps[head] = Ap[j];   // no cheap match: start dfs for j
    }
    //-----------------------------------------------------
    // Depth-first-search of neighbors of j
    //-----------------------------------------------------
    for (p=ps[head]; p<Ap[j+1]; ++p) {
      i = Ai[p];                // consider row i
      if (w[jmatch[i]] == k) continue;  // skip jmatch [i] if marked
      ps[head] = p + 1;         // pause dfs of node j
      is[head] = i;             // i will be matched with j if found
      js[++head] = jmatch[i];   // start dfs at column jmatch [i]
      break;
    }
    if (p == Ap[j+1]) head-- ;  // node j is done; pop from stack
  }
  if (found) {                  // augment the match if path found:
    for (p=head; p>=0; p--) { jmatch[is[p]] = js[p]; }
  }
}


//---------------------------------------------------------
// helper routine for CSd::qrsym()
//
// compute nnz(V) = S->lnz, S->pinv, S->leftmost, S->m2 
// from A and S->parent
//---------------------------------------------------------
int CS_vcount(const CSd &A, CSS *S)
//---------------------------------------------------------
{
  int n=A.n, m=A.m;       // dimensions
  S->pinv.resize(m+n);    // allocate pinv,
  S->leftmost.resize(m);  // and leftmost
  IVec w(m+3*n);          // workspace
  if (!S->pinv.ok() || !w.ok() || !S->leftmost.ok()) 
  { umERROR("CS_vcount", "out of memory."); return 0; }
  int *wd = w.data();
  int *next=wd, *head=wd+m, *tail=wd+m+n, *nque=wd+m+2*n;

  int i=0, k=0, p=0, pa=0;
  for (k=0; k<n; ++k) head[k] = -1;   // queue k is empty
  for (k=0; k<n; ++k) tail[k] = -1;
  for (k=0; k<n; ++k) nque[k] = 0;
  for (i=0; i<m; ++i) S->leftmost[i] = -1;
  for (k=n-1; k >=0; k--) {
    for (p=A.P[k]; p<A.P[k+1]; ++p) {
      S->leftmost[A.I[p]] = k;      // leftmost[i] = min(find(A(i,:)))
    }
  }
  for (i=m-1; i>=0; i--) {          // scan rows in reverse order
    S->pinv[i] = -1;                // row i is not yet ordered
    k = S->leftmost[i];
    if (-1 == k) continue;          // row i is empty
    if (nque[k]++ == 0) tail[k]=i;  // first row in queue k
    next[i] = head[k];              // put i at head of queue k
    head[k] = i;
  }
  S->lnz = 0;
  S->m2 = m;
  for (k=0; k<n; ++k) {           // find row permutation and nnz(V)
    i = head[k];                  // remove row i from queue k
    S->lnz++;                     // count V(k,k) as nonzero
    if (i<0) i = S->m2++;         // add a fictitious row
    S->pinv[i] = k;               // associate row i with V(:,k)
    if (--nque[k]<=0) continue;   // skip if V(k+1:m,k) is empty
    S->lnz += nque[k];            // nque [k] is nnz (V(k+1:m,k))
    if ((pa=S->parent[k]) != -1)  // move all rows to parent of k
    {
      if (nque[pa] == 0) tail[pa] = tail[k];
      next[tail [k]] = head[pa];
      head[pa] = next[i];
      nque[pa] += nque[k];
    }
  }
  for (i=0; i<m; ++i) {
    if (S->pinv[i]<0) { 
      S->pinv[i] = k++; 
    }
  }
  return 1;
}



///////////////////////////////////////////////////////////
//
// qr factorization/solve
//
///////////////////////////////////////////////////////////



// x=A\b where A can be rectangular; 
// b is overwritten with solution
//---------------------------------------------------------
bool CS_qrsol(int order, const CSd& A, DVec& b)
//---------------------------------------------------------
{
  if (!A.is_csc()) {umWARNING("CS_qrsol", "expected csc matrix"); return false;}
  if (!b.ok())     {umWARNING("CS_qrsol", "empty rhs"); return false;}

  CSS *S=NULL; CSN *N=NULL;
  bool bOk=false;  int k=0;

  int n=A.n, m=A.m;
  if (m >= n) {
    S = CS_sqr(order, A, 1);      // ordering and symbolic analysis
    N = CS_qr(A, S);              // numeric QR factorization
    DVec x(S ? S->m2 : 1);        // get workspace
    bOk = (S && N && x.ok());
    if (bOk) 
    {
      CS_ipvec(S->pinv, b, x, m); // x(0:m-1) = b(p(0:m-1). x is workspace
      for (k=0; k<n; ++k) {       // apply Householder refl. to x
        CS_happly(N->L, k, N->B[k], x);
      }
      CS_usolve(N->U, x);         // x = R\x
      CS_ipvec(S->Q, x, b, n);    // b(q(0:n-1)) = x(0:n-1)
    }
  } 
  else 
  {
    CSd AT = trans(A,1);          // Ax=b is underdetermined
    S = CS_sqr(order, AT, 1);     // ordering and symbolic analysis
    N = CS_qr(AT, S);             // numeric QR factorization of A'
    DVec x(S?S->m2:1);            // workspace
    bOk = (AT.ok() && S && N && x.ok());
    if (bOk)
    {
      CS_pvec(S->Q, b, x, m);     // x(q(0:m-1)) = b(0:m-1)
      CS_utsolve(N->U, x);        // x = R'\x
      for (k=m-1; k>=0; k--) {    // apply Householder refl. to x
        CS_happly(N->L, k, N->B[k], x);
      }
      CS_pvec(S->pinv, x, b, n);  // b(0:n-1) = x(p(0:n-1))
    }
  }
  if (S) { delete S; }
  if (N) { delete N; }

  return bOk;
}


// symbolic ordering and analysis for QR or LU
//---------------------------------------------------------
CSS* CS_sqr(int order, const CSd& A, int qr)
//---------------------------------------------------------
{
  if (!A.is_csc()) {umERROR("CS_sqr","expected csc matrix"); return NULL;}

  CSS *S=new CSS;               // allocate result S
  if (!S) return NULL;          // out of memory
  int n=A.n, k=0; bool bOk = true;

  S->Q = CS_amd(order, A);      // fill-reducing ordering
  if (order && !S->Q.ok()) {
    umERROR("CS_sqr","amd ordering failed"); 
    delete S; return NULL;
  }

  if (qr)                       // QR symbolic analysis
  {
    CSd *C=NULL; 
    bool freeC=false;
    if (order) {
      C = new CSd("t.CS_sqr");
      freeC = true;             // C is locally allocated
      C->copy(A, 0);
      C->permuteQ(S->Q, 0);
    } else {
      C = const_cast<CSd*>(&A); // C points to external object
    }
    S->parent = C->etree(1);    // etree of C'*C, where C=A(:,q)
    IVec post = CS_post(S->parent, n);
    S->cp = CS_counts((*C), S->parent, post, 1); // col counts chol(C'*C)
    bOk = (C->ok() && S->parent.ok() && S->cp.ok());
    bOk = bOk && CS_vcount((*C),S);
    if (bOk) {
      for (S->unz=0, k=0; k<n; ++k) {
        S->unz += S->cp[k];
      }
    }
    bOk = bOk && (S->lnz>=0 && S->unz>=0);  // int overflow guard
    if (C && freeC) { delete C; }
  }
  else
  {
    S->unz = 4*(A.P[n])+n;  // for LU factorization only,
    S->lnz = S->unz;        // guess nnz(L) and nnz(U)
  }

  if (bOk) {
    return S;       // return result S
  } else {
    delete S; 
    return NULL;    // failed
  }
}


// sparse QR factorization [V,beta,pinv,R] = qr (A)
//---------------------------------------------------------
CSN* CS_qr(const CSd& A, const CSS *S)
//---------------------------------------------------------
{
  if (!A.is_csc()) {umWARNING("CS_qr","expected csc matrix"); return NULL;}
  if (!S)        {umTRC(1, "CS_qr: invalid symbolic data\n"); return NULL;}

  int i=0, k=0, p=0, p1=0, top=0, len=0, col=0;
  int m=A.m, n=A.n, m2=S->m2, vnz=(int)S->lnz, rnz=(int)S->unz;
  const IVec &q=S->Q, &parent=S->parent, &pinv=S->pinv, &leftmost=S->leftmost;
  IVec w(m2+n);  DVec x(m2);  // workspaces
  CSN *N = new CSN;           // allocate result
  if (!w.ok() || !x.ok() || !N) { umERROR("CS_qr","out of memory"); delete N; return NULL; }

  int *wd = w.data();
  int *s = wd + m2;     // s is size n
  x.fill(0.0);          // clear workspace x

  N->L.resize(m2, n, vnz, 1, 0);  // allocate result V
  N->U.resize(m2, n, rnz, 1, 0);  // allocate result R
  N->B.resize(n);                 // allocate result Beta

  if (!N->ok()) {
    umERROR("CS_qr", "failed to allocate CSN structure"); 
    delete N; return NULL;
  }

  // create references into the CSN object
  CSd &V=N->L, &R=N->U;

  IVec &Rp=R.P, &Ri=R.I; DVec &Rx=R.X;
  IVec &Vp=V.P, &Vi=V.I; DVec &Vx=V.X;
  for (i=0; i<m2; ++i) w[i] = -1; // clear w, to mark nodes
  double *Beta = N->B.data();
  double *Vxd = Vx.data();
  
  bool bQ = q.ok();           // check for col permutation
  rnz = 0; vnz = 0;
  for (k=0; k<n; ++k)         // compute V and R
  {
    Rp[k] = rnz;              // R(:,k) starts here
    Vp[k] = p1 = vnz;         // V(:,k) starts here
    w [k] = k;                // add V(k,k) to pattern of V
    Vi[vnz++] = k;
    top = n;
    col = bQ ? q [k] : k;
    for (p=A.P[col]; p<A.P[col+1]; ++p)   // find R(:,k) pattern
    {
      i = leftmost[A.I[p]];   // i = min(find(A(i,q)))
      for (len=0; w[i] != k; i=parent [i]) // traverse up to k
      {
        s [len++] = i;
        w [i] = k;
      }
      while (len > 0) s[--top] = s[--len]; // push path on stack
      i = pinv[A.I[p]];       // i = permuted row of A(:,col)
      x [i] = A.X[p];         // x (i) = A(:,col)
      if (i>k && w[i]<k)      // pattern of V(:,k) = x (k+1:m)
      {
        Vi[vnz++] = i;        // add i to pattern of V(:,k)
        w[i] = k;
      }
    }
    for (p=top; p<n; ++p)     // for each i in pattern of R(:,k)
    {
      i = s[p];               // R(i,k) is nonzero
      CS_happly(V, i, Beta[i], x); // apply (V(i),Beta(i)) to x
      R.I[rnz] = i;           // R(i,k) = x(i)
      R.X[rnz++] = x[i];
      x[i] = 0;  DVec NoX;
      if (parent[i] == k) vnz = V.scatter(i, 0.0, w, NoX, k, V, vnz);
    }
    for (p=p1; p<vnz; ++p)    // gather V(:,k) = x
    {
      Vx[p] = x[Vi[p]];
      x[Vi[p]] = 0;
    }
    R.I[rnz] = k;             // R(k,k) = norm (x)
  //R.X[rnz++] = CS_house(Vx +p1, Beta+k, vnz-p1); // [v,beta]=house(x)
    R.X[rnz++] = CS_house(Vxd+p1, Beta+k, vnz-p1); // [v,beta]=house(x)
  }
  R.P[n] = rnz;     // finalize R
  V.P[n] = vnz;     // finalize V
  
  assert(N->ok());
  return N;  // success
}




///////////////////////////////////////////////////////////
//
// lu factorization/solve
//
///////////////////////////////////////////////////////////


// operator version: x = A\b  (single RHS)
//---------------------------------------------------------
DVec& operator | (const CSd& A, const DVec& b)
//---------------------------------------------------------
{
  int len=A.num_rows();
  assert(A.is_square() && (b.size() == len));

  DVec *x=new DVec(b,OBJ_temp,"A|b"); // copy rhs

  // Select solver depending on shape/symmetry
  int shape = A.get_shape();

  if (sp_NONE == shape) {         // use LU for general matrix
    int    order=2;               // AMD reordering: C = A'*A  (drop dense rows)
    double tol=1.0;               // partial pivoting scale factor
    CS_lusol(order,A,(*x),tol);   // overwrites x with solution
  }
  else if ((sp_LOWER & shape) && (sp_SYMMETRIC & shape)) {
    CSd A2 = trans(A);            // currently expects upper tri
    int order=1;                  // AMD reordering: C = A+A'
    CS_cholsol(order, A2, (*x));  // overwrites x with solution
  }
  else if (sp_SYMMETRIC & shape) {
    CSd& A2 = const_cast<CSd&>(A);  // expects non-const arg
    int order=1;                    // AMD reordering: C = A+A'
    CS_cholsol(order, A2, (*x));    // overwrites x with solution
  }
  else {
    umERROR("operator | (CSd&, DVec&)", 
            "The shape of this sparse matrix is set to %d\n"
            "Please adjust solver to handle this shape.\n", shape);
  }
  return (*x);
}


// operator version: X = A\B  (multiple RHS's)
//---------------------------------------------------------
DMat& operator | (const CSd& A, const DMat& B)
//---------------------------------------------------------
{
  int len=A.num_rows();
  assert(A.is_square() && (B.num_rows() == len));

  DMat *X=new DMat(B,OBJ_temp,"A|B"); // copy rhs
  int    order=2;                     // AMD reordering mode
  double tol=1.0;                     // partial pivoting scale factor
  CS_lusol(order, A, (*X), tol);      // overwrites X with solution
  return (*X);
}




// x=A\b where A is unsymmetric;
// b overwritten with solution  (single RHS)
// tol: partial pivoting "scale factor"
//---------------------------------------------------------
bool CS_lusol(int order, const CSd& A, DVec& b, double tol)
//---------------------------------------------------------
{
  assert(A.ok() && A.is_csc() && A.is_square());
  int n = A.n; CSS *S=NULL; CSN *N=NULL; bool bOk=false;
  
  S = CS_sqr(order, A, 0);    // ordering and symbolic analysis
  N = CS_lu(A, S, tol);       // numeric LU factorization
  DVec x(n);                  // workspace
  bOk = (S && N && x.ok());
  if (bOk) {
    CS_ipvec (N->pinv, b, x, n);  // x = b(p)
    CS_lsolve(N->L, x);           // x = L\x
    CS_usolve(N->U, x);           // x = U\x
    CS_ipvec (S->Q, x, b, n);     // b(q) = x
  }
  if (S) { delete S; }
  if (N) { delete N; }

  return bOk;
}


// X=A\B where A is unsymmetric;
// B overwritten with solution  (multiple RHS's)
// tol: partial pivoting "scale factor"
//---------------------------------------------------------
bool CS_lusol(int order, const CSd& A, DMat& B, double tol)
//---------------------------------------------------------
{
  assert(A.ok() && A.is_csc() && A.is_square());
  int n = A.n; CSS *S=NULL; CSN *N=NULL; bool bOk=false;
  int Nrhs = B.num_cols();
  
  S = CS_sqr(order, A, 0);    // ordering and symbolic analysis
  N = CS_lu(A, S, tol);       // numeric LU factorization
  DVec x(n), b;               // workspace
  bOk = (S && N && x.ok());
  if (bOk) {
    for (int j=1; j<=Nrhs; ++j)
    {
      b.borrow(n, B.pCol(j));       // get B(j) as jth RHS
      CS_ipvec (N->pinv, b, x, n);  // x = b(p)
      CS_lsolve(N->L, x);           // x = L\x
      CS_usolve(N->U, x);           // x = U\x
      CS_ipvec (S->Q, x, b, n);     // b(q) = x
    }
  }
  if (S) { delete S; }
  if (N) { delete N; }

  return bOk;
}



// [L,U,pinv]=lu(A, [q lnz unz]). lnz and unz can be guess
// tol: partial pivoting "scale factor"
//---------------------------------------------------------
CSN* CS_lu(const CSd& A, const CSS *S, double tol)
//---------------------------------------------------------
{
  if (!A.is_square()) {umWARNING("CS_lu","matrix is not square"); return NULL;}
  if (!A.is_csc())    {umWARNING("CS_lu","expected csc matrix"); return NULL;}
  if (!S)             {umTRC(1, "CS_lu: invalid symbolic data"); return NULL;}

  int n = A.n, lnz=(int)S->lnz, unz=(int)S->unz;
  int ipiv=0, k=0, top=0, p=0, i=0, col=0;
  double pivot=0.0, a=0.0, t=0.0;
  const IVec &q=S->Q; DVec x(n); IVec xi(2*n); // {x,xi} are workspaces
  CSN *N = new CSN;     // allocate result
  if (!x.ok() || !xi.ok() || !N) { umERROR("CS_lu","out of memory"); delete N; return NULL; }

  N->L.resize(n, n, lnz, 1, 0);   // allocate result L
  N->U.resize(n, n, unz, 1, 0);   // allocate result U
  N->pinv.resize(n);              // allocate result pinv

  if (!N->ok()) {
    umERROR("CS_lu", "failed to allocate CSN structure"); 
    delete N; N=NULL; return NULL;
  }

  // create references into the CSN object
  CSd &L=N->L, &U=N->U; IVec &pinv=N->pinv, &Lp=L.P, &Up=U.P;

  x.fill(0.0);          // clear workspace
  pinv.fill(-1);        // no rows pivotal yet
  L.P.fill(0);          // no cols of L yet
  bool bQ = q.ok();     // check for col permutation
  lnz = unz = 0;        // reset nnz estimates

  for (k=0; k<n; ++k)   // compute L(:,k) and U(:,k)
  {
    //-----------------------------------------------------
    // Triangular solve
    //-----------------------------------------------------
    Lp[k] = lnz;        // L(:,k) starts here
    Up[k] = unz;        // U(:,k) starts here
    // expand {L,U} if necessary
    if (lnz+n > L.nzmax) { if (!L.realloc(2*L.nzmax+n)) {delete N; return NULL;} }
    if (unz+n > U.nzmax) { if (!U.realloc(2*U.nzmax+n)) {delete N; return NULL;} }
    IVec& Li=L.I, &Ui=U.I;  DVec& Lx=L.X, &Ux=U.X;
    col = bQ ? (q[k]) : k;
    top = CS_spsolve(L, A, col, xi, x, pinv, 1) ;  // x = L\A(:,col)
    //-----------------------------------------------------
    // Find pivot
    //-----------------------------------------------------
    ipiv = -1;
    a = -1.0;
    for (p=top; p<n; ++p) {
      i = xi[p];            // x(i) is nonzero
      if (pinv[i] < 0) {    // row i is not yet pivotal
        if ((t=fabs(x[i])) > a) {
          a = t;            // largest pivot candidate so far
          ipiv = i;
        }
      } else {              // x(i) is the entry U(pinv[i],k)
        Ui[unz] = pinv[i];
        Ux[unz++] = x[i];
      }
    }
    if (ipiv == -1 || a <= 0.0) { 
      umWARNING("CS_lu","no pivot for row %d", k); 
      delete N; return NULL; 
    }
    if (pinv[col]<0 && fabs(x[col]) >= a*tol) {
      ipiv = col;
    }
    //-----------------------------------
    // Divide by pivot
    //-----------------------------------
    pivot = x[ipiv];            // the chosen pivot
    Ui[unz] = k;                // last entry in U(:,k) is U(k,k)
    Ux[unz++] = pivot;
    pinv[ipiv] = k;             // ipiv is the kth pivot row
    Li[lnz] = ipiv;             // first entry in L(:,k) is L(k,k) = 1
    Lx[lnz++] = 1;
    for (p=top; p<n; ++p) {     // L(k+1:n,k) = x/pivot
      i = xi[p] ;
      if (pinv[i] < 0) {        // x(i) is an entry in L(:,k)
        Li[lnz] = i;            // save unpermuted row in L
        Lx[lnz++] = x[i]/pivot; // scale pivot column
      }
      x[i] = 0;                 // x [0..n-1] = 0 for next k
    }
  }


  //-------------------------------------
  // Finalize L and U
  //-------------------------------------
  Lp[n]=lnz;  Up[n]=unz;
  for (p=0; p<lnz; ++p) { L.I[p] = pinv[L.I[p]]; }
  L.realloc(0);  U.realloc(0);  // remove extra space

  return N;   // success
}


///////////////////////////////////////////////////////////
//
// Cholesky factorization/solve
//
///////////////////////////////////////////////////////////


// x=A\b where A is symmetric positive definite;
// b overwritten with solution
//---------------------------------------------------------
bool CS_cholsol(int order, CSd& A, DVec& b)
//---------------------------------------------------------
{
  if (!A.is_square()) { umWARNING("CS_cholsol","matrix is not square"); return false; }
  if (!A.is_csc())    { umWARNING("CS_cholsol","expected csc matrix"); return false; }
  if (!b.ok())        { umWARNING("CS_cholsol","empty rhs"); return false; }

  CSS *S=NULL; CSN *N=NULL;
  bool bOk=false;

  int n = A.n;
  S = CS_schol(order, A);     // ordering and symbolic analysis
  N = CS_chol(A, S);          // numeric Cholesky factorization
  DVec x(n);                  // get workspace
  bOk = (S && N && x.ok());
  if (bOk)
  {
    CS_ipvec  (S->pinv, b, x, n);   // x = P*b
    CS_lsolve (N->L, x);            // x = L\x
    CS_ltsolve(N->L, x);            // x = L'\x
    CS_pvec   (S->pinv, x, b, n);   // b = P'*x
  }
  if (S) { delete S; }
  if (N) { delete N; }

  return bOk;
}


// ordering and symbolic analysis for a Cholesky factorization
//---------------------------------------------------------
CSS* CS_schol(int order, const CSd& A)
//---------------------------------------------------------
{
  if (!A.is_square()) {umERROR("CS_schol","matrix must be square"); return NULL;}
  if (!A.is_csc())    {umERROR("CS_schol","expected csc matrix"); return NULL;}

  CSS *S=new CSS;               // allocate result S
  if (!S) return NULL;          // out of memory
  int n=A.n;
  IVec post,c;

  IVec P = CS_amd(order, A);    // P = amd(A+A'), or natural
  S->pinv = CS_pinv(P, n);      // find inverse permutation
  P.Free();                     // release workspace
  if (order && !S->pinv.ok()) {
    umERROR("CS_schol","error creating amd permutation"); 
    delete S; return NULL;
  }

  CSd C = CS_symperm(A, S->pinv, 0);  // C = spones(triu(A(P,P)))
  S->parent = C.etree(0);             // find etree of C
  post = CS_post(S->parent, n);       // postorder the etree
  c = CS_counts(C,S->parent,post,0);  // find column counts of chol(C)

  S->cp.resize(n+1);                  // allocate result S->cp
  S->lnz = CS_cumsum(S->cp, c, n);    // find column pointers for L
  S->unz = S->lnz;                    // estimate nnz in {L,U}

  C.reset();  // force deallocation

  if (S->lnz >= 0) {
    return S;       // return result S
  } else {
    delete S; 
    return NULL;    // failed
  }
}


// L = chol (A, [pinv parent cp]), pinv is optional
//---------------------------------------------------------
CSN* CS_chol(CSd& A, const CSS *S, bool own_A)
//---------------------------------------------------------
{
  if (!A.is_csc()) {umWARNING("CS_chol","expected csc matrix"); return NULL;}
  if (!S)          {umWARNING("CS_chol","empty symbolic data"); return NULL;}
  if (!S->cp.ok() || !S->parent.ok()) {umWARNING("CS_chol", "symbolic data not ready"); return NULL;}

  double d=0.0, lki=0.0;
  int n=A.n, top=0,i=0,p=0,k=0;
  CSd *C = new CSd("t.CS_chol");
  const IVec &cp=S->cp, &pinv=S->pinv, &parent=S->parent;
  bool bPi = pinv.ok(), freeC=false;

  if (own_A)
  {
    // NBN: attempting to reduce memory overhead.
    // Let C take ownership of A's data, resetting A to empty.
    // Since only the upper triangle is stored, and C 
    // is also deleted once factorization is complete, 
    // larger (symmetric) operators may be factorized.

    freeC = true;           // release C after factorization
    C->own(A);
    if (bPi) {
      C->symperm(pinv,1);   // permute in-place
    }
  }
  else
  {
    if (bPi) {
      (*C) = CS_symperm(A,pinv,1);  // C is locally allocated
      freeC = true;                 // ... so relese after use
    } else {
      C = const_cast<CSd*>(&A);     // C points to external object
    }
  }

  CSN *N = new CSN;             // allocate result
  IVec c(2*n);  DVec x(n);      // get workspaces
  if (!N || !c.ok() || !x.ok() || !C->ok()) {
    umWARNING("CS_chol", "error allocating arrays");
    if (freeC) { delete C; }
    delete N; return NULL;
  }

  umLOG(1, "\n ==> CS_chol: resize N->L(%d,%d), nnz = %d\n", n,n,cp[n]);
  N->L.resize(n, n, cp[n], 1, 0);   // allocate result
//umLOG(1, "*** CS_chol: resized.\n");

  if (!N->L.ok()) {
    umWARNING("CS_chol", "error allocating N->L(%d)", n);
    if (freeC) { delete C; }
    delete N; return NULL;
  }

  CSd& L=N->L;  int *cd=c.data(); int *s=cd+n;
  IVec &Lp=L.P,  &Li=L.I;  DVec &Lx=L.X;
  IVec &Cp=C->P, &Ci=C->I; DVec &Cx=C->X;
  for (k=0; k<n; ++k) { Lp[k] = c[k] = cp[k]; }

  umLOG(1, " ==> CS_chol: (n=%d) ", n);
  for (k=0; k<n; ++k)     // compute L(:,k) for L*L' = C
  {
    if (! (k%1000)) {umLOG(1, ".");}

    //-----------------------------------
    // Nonzero pattern of L(k,:)
    //-----------------------------------
    top = C->ereach(k, parent, s, cd);  // find pattern of L(k,:)
    x[k] = 0;                           // x (0:k) is now zero
    for (p=Cp[k]; p<Cp[k+1]; ++p)       // x = full(triu(C(:,k)))
    {
      if (Ci[p] <= k) x[Ci[p]] = Cx[p];
    }
    d = x[k];                   // d = C(k,k)
    x[k] = 0;                   // clear x for k+1st iteration
    //-----------------------------------
    // Triangular solve
    //-----------------------------------
    for ( ; top<n; ++top)       // solve L(0:k-1,0:k-1) * x = C(:,k)
    {
      i = s[top];               // s [top..n-1] is pattern of L(k,:)
      lki = x[i] / Lx[Lp[i]];   // L(k,i) = x (i) / L(i,i)
      x[i] = 0;                 // clear x for k+1st iteration
      for (p=Lp[i]+1; p<c[i]; ++p) {
        x[Li[p]] -= Lx[p] * lki;
      }
      d -= lki * lki;           // d = d - L(k,i)*L(k,i)
      p = c [i]++;
      Li[p] = k;                // store L(k,i) in column i
      Lx[p] = lki;
    }
    //-----------------------------------
    // Compute L(k,k)
    //-----------------------------------
    if (d <= 0) { umWARNING("CS_chol", "not pos def"); delete N; return NULL; }
    p = c [k]++;
    Li[p] = k;        // store L(k,k) = sqrt (d) in column k
    Lx[p] = sqrt(d);
  }
  Lp[n] = cp[n];      // finalize L

  umLOG(1, "\n\n");

  if (freeC) { 
    umTRC(2, "*** CS_chol: deleting C...\n");
    delete C; 
    umTRC(2, "*** CS_chol: C deleted.\n");
  }

  return N;           // success
}






///////////////////////////////////////////////////////////
//
// matrix reorderings
//
///////////////////////////////////////////////////////////



//---------------------------------------------------------
// symmetric permutation
//---------------------------------------------------------


// C = A(p,p) where A and C are symmetric 
// the upper part stored; uses pinv not p
//---------------------------------------------------------
CSd& CS_symperm(const CSd& A, const IVec& pinv, int values)
//---------------------------------------------------------
{
  CSd *C = new CSd("A(p,p)", OBJ_temp);   // alloc (empty) result
  if (!A.is_csc()) {umERROR("CS_symperm(CSd, ...)", "not csc"); return (*C);}

  int n=A.n, i=0, j=0, p=0, q=0, i2=0, j2=0;
  
  if (!A.m_values) { values=0; }    // override arg if A has no values
  bool bp=(pinv.size()>=n);         // check for (optional) permutation
  IVec w(n);                        // workspace
  C->resize(n,n,A.P[n],values,0);   // resize csc result
  if (!C->ok() || !w.ok()) {umERROR("CS_symperm(CSd, ...)", "out of memory"); return (*C);}
  
  for (j=0; j<n; ++j) {             // count entries in each column of C
    j2 = bp ? pinv[j] : j;          // column j of A is column j2 of C
    for (p=A.P[j]; p<A.P[j+1]; ++p)
    {
      i = A.I[p];
      if (i>j) continue;            // skip lower triangle of A
      i2 = bp ? pinv[i]: i;         // row i of A is row i2 of C
      w[std::max(i2,j2)]++ ;        // column count of C
    }
  }
  C->cumsum(w, n);                  // compute column pointers of C
  for (j=0; j<n; ++j) {
    j2 = bp ? pinv[j] : j;          // column j of A is column j2 of C
    for (p=A.P[j]; p<A.P[j+1]; ++p) 
    {
      i = A.I[p];
      if (i>j) continue;            // skip lower triangle of A
      i2 = bp ? pinv[i] : i;        // row i of A is row i2 of C
      C->I[q=w[std::max(i2,j2)]++] = std::min(i2,j2);
      if (values) C->X[q] = A.X[p];
    }
  }
  return (*C);  // success
}


//---------------------------------------------------------
// AMD reordering
//---------------------------------------------------------

// helper for CS_amd: clear w
static int CS_wclear(int mark, int lemax, int *w, int n)
{
  if (mark < 2 || (mark + lemax < 0)) {
    for (int k=0; k<n; ++k) if (w[k]!=0) w[k]=1;
    mark=2;
  }
  return mark; // at this point, w [0..n-1] < mark holds
}


// p = amd(A+A') if symmetric is true, or amd(A'A) otherwise
// order  0: natural, 
//        1: Chol, 
//        2: LU, 
//        3: QR, 
// NBN:   4: Chol (without A'.  User guarantees symmetry)
//---------------------------------------------------------
IVec& CS_amd(int order, const CSd& A)
//---------------------------------------------------------
{
  IVec* P = new IVec("amd(P)", OBJ_temp);
  if (order <= 0 ) { return (*P); }
  if (order > 4)   { umWARNING("CS_amd(order:%d)", "expected order[0:4]", order); return (*P); }
  if (!A.is_csc() || !A.ok()) {umERROR("CS_amd(order:%d)", "expected csc form", order); return (*P); }

  int i=0, j=0, k=0, k1=0, k2=0, k3=0, mark=0;
  int p=0,p1=0,p2=0,p3=0,p4=0,pj=0,pk=0,pk1=0,pk2=0,pn=0,q=0,t=0;
  int d=0,dk=0,dext=0,e=0,elenk=0,eln=0,jlast=0,ln=0;
  int nvi=0, nvj=0, nvk=0, wnvi=0;
  int lemax=0, mindeg=0, nel=0;
  unsigned int h=0;
  bool bOk=false;

  CSd C("C.amd"), AT("AT.amd");

  if (4 != order) 
  {
    // need to compute A'
    AT = trans(A, 0);
    if (!AT.ok()) {
      umERROR("CS_amd(order:%d)", "failed to create transpose", order);
      return (*P);
    }
  }

  int m=A.m, n=A.n;
  // find dense threshold
  int dense = (int)std::max(16.0, 10*sqrt((double)n));
  dense = std::min(n-2, dense);

  //-------------------------------------
  // assemble matrix C according to order
  //-------------------------------------

  if (1==order && n==m) 
  {
    C = A + AT;             // enforce symmetry
  } 
  else if (2 == order) 
  {
    // drop dense columns from AT
    for (p2=0, j=0; j<m; ++j) {
      p = AT.P[j];          // column j of AT starts here
      AT.P[j] = p2;         // new column j starts here
      if ((AT.P[j+1]-p) > dense) continue;   // skip dense col j
      for ( ; p<AT.P[j+1]; ++p) AT.I[p2++] = AT.I[p];
    }
    AT.P[m] = p2;           // finalize AT
    CSd A2 = trans(AT, 0);  // A2 = AT'
    if (A2.ok()) {
      C = AT*A2;            // C=A'*A with no dense rows
    } else {
      C.reset();            // i.e. "NULL"
    }
    A2.reset();             // free A2 arrays
  } 
  else if (order <= 3) 
  {
    C = AT*A;               // handle natural and QR
  }
  else if (4==order && n==m) 
  {
    // Note: symmetry must be guaranteed by User
    C.copy(A, 0);           // copy structure -- not values
  }
  else
  {
    umERROR("CS_amd(order:%d)", "unexpected order: %d", order);
  }

  AT.reset();               // free AT arrays
  if (!C.ok()) {
    umERROR("CS_amd(order:%d)", "failed to create C", order);
    return (*P);
  }
  C.dropdiag();             // drop diagonal entries

  IVec& Cp = C.P;
  int cnz = Cp[n];
  P->resize(n+1);           // allocate result
  IVec W(8*(n+1));          // workspace
  if (!P->ok() || !W.ok()) {
    umERROR("CS_amd(order:%d)", "out of memory", order);
    return (*P);
  }
  t = cnz + cnz/5 + 2*n;    // expand (nnz) capacity in C
  if (!C.realloc(t)) { 
    umERROR("CS_amd(order:%d)", "failed to realloc C (%g)", order, t);
    return (*P);
  }

  int *wd  =W.data();
  int *len =wd        , *nv   =wd+  (n+1), *next  =wd+2*(n+1);
  int *head=wd+3*(n+1), *elen =wd+4*(n+1), *degree=wd+5*(n+1);
  int *w   =wd+6*(n+1), *hhead=wd + 7*(n+1);
  int *last=P->data();      // use P as workspace for last

  //-------------------------------------------------------
  // Initialize quotient graph
  //-------------------------------------------------------
  for (k=0; k<n; ++k) len[k] = Cp[k+1] - Cp[k];
  len[n] = 0;
  int nzmax = C.nzmax;
  IVec& Ci=C.I;
  for (i = 0; i <= n; i++)
  {
    head  [i] = -1;       // degree list i is empty
    last  [i] = -1;
    next  [i] = -1;
    hhead [i] = -1;       // hash list i is empty
    nv    [i] = 1;        // node i is just one node
    w     [i] = 1;        // node i is alive
    elen  [i] = 0;        // Ek of node i is empty
    degree[i] = len[i];   // degree of node i
  }
  mark = CS_wclear(0, 0, w, n);  // clear w
  elen[n] = -2;           // n is a dead element
  Cp[n] = -1;             // n is a root of assembly tree
  w[n] = 0;               // n is a dead element

  //-------------------------------------------------------
  // Initialize degree lists
  //-------------------------------------------------------
  for (i=0; i<n; ++i)
  {
    d = degree [i];
    if (d == 0)  {          // node i is empty
      elen[i] = -2;         // element i is dead
      nel++;
      Cp[i] = -1;           // i is a root of assembly tree
      w[i] = 0;
    } else if (d>dense) {   // node i is dense
      nv[i] = 0;            // absorb i into element n
      elen[i] = -1;         // node i is dead
      nel++;
      Cp[i] = CS_FLIP (n);
      nv[n]++;
    } else {
      if (head[d] != -1) last[head[d]] = i;
      next[i] = head[d];    // put node i in degree list d
      head[d] = i;
    }
  }

  while (nel < n)           // while (selecting pivots) do
  {
    //-----------------------------------------------------
    // Select node of minimum approximate degree
    //-----------------------------------------------------
    for (k = -1; mindeg<n && (k=head[mindeg]) == -1; ++mindeg);
    if (next[k] != -1) last[next[k]] = -1;
    head[mindeg] = next[k]; // remove k from degree list
    elenk = elen[k];        // elenk = |Ek|
    nvk = nv[k];            // # of nodes k represents
    nel += nvk;             // nv[k] nodes of A eliminated

    //-----------------------------------------------------
    // Garbage collection
    //-----------------------------------------------------
    if (elenk>0 && (cnz+mindeg)>=nzmax) {
      for (j=0; j<n; ++j) {
        if ((p=Cp[j]) >= 0)       // j is a live node or element
        {
          Cp[j] = Ci[p];          // save first entry of object
          Ci[p] = CS_FLIP (j);    // first entry is now CS_FLIP(j)
        }
      }
      for (q=0, p=0; p<cnz;  )    // scan all of memory
      {
        if ((j=CS_FLIP (Ci[p++])) >= 0)  // found object j
        {
          Ci[q] = Cp[j];          // restore first entry of object
          Cp[j] = q++;            // new pointer to object j
          for (k3=0; k3<len[j]-1; ++k3) 
            Ci[q++] = Ci[p++];
        }
      }
      cnz = q;    // Ci [cnz...nzmax-1] now free
    }

    //-----------------------------------------------------
    // Construct new element
    //-----------------------------------------------------
    dk = 0;
    nv[k] = -nvk;                   // flag k as in Lk
    p = Cp [k];
    pk1 = (elenk == 0) ? p : cnz;   // do in place if elen[k] == 0
    pk2 = pk1;

    for (k1=1; k1<=elenk+1; ++k1)
    {
      if (k1 > elenk) {
        e = k;              // search the nodes in k
        pj = p;             // list of nodes starts at Ci[pj]
        ln = len[k]-elenk;  // length of list of nodes in k
      } else {
        e  = Ci[p++];       // search the nodes in e
        pj = Cp[e];
        ln = len[e];        // length of list of nodes in e
      }
      for (k2=1; k2<=ln; ++k2)
      {
        i = Ci[pj++];
        if ((nvi = nv[i]) <= 0) 
          continue;             // node i dead, or seen
        dk += nvi;              // degree[Lk] += size of node i
        nv[i] = -nvi;           // negate nv[i] to denote i in Lk
        Ci[pk2++] = i;          // place i in Lk
        if (next[i] != -1) last [next[i]] = last[i];
        if (last[i] != -1) {    // remove i from degree list
          next[last[i]] = next[i];
        } else {
          head[degree[i]] = next[i];
        }
      }
      if (e != k) {
        Cp[e] = CS_FLIP (k);    // absorb e into k
        w [e] = 0;              // e is now a dead element
      }
    }
    if (elenk != 0) cnz = pk2;  // Ci [cnz...nzmax] is free
    degree[k] = dk;             // external degree of k - |Lk\i|
    Cp  [k] = pk1;              // element k is in Ci[pk1..pk2-1]
    len [k] = pk2 - pk1;
    elen[k] = -2;               // k is now an element
    //-----------------------------------------------------
    // Find set differences
    //-----------------------------------------------------
    mark = CS_wclear(mark, lemax, w, n);  // clear w if necessary
    for (pk=pk1; pk<pk2; ++pk)      // scan 1: find |Le\Lk|
    {
      i = Ci[pk];
      if ((eln=elen[i]) <= 0) continue; // skip if elen[i] empty
      nvi = -nv[i];                 // nv [i] was negated
      wnvi = mark - nvi;
      for (p=Cp[i]; p<=Cp[i]+eln-1; ++p)  // scan Ei
      {
        e = Ci[p];
        if (w[e] >= mark) {
          w[e] -= nvi;              // decrement |Le\Lk|
        } else if (w[e] != 0) {     // ensure e is a live element
          w[e] = degree[e] + wnvi;  // 1st time e seen in scan 1
        }
      }
    }

    //-----------------------------------------------------
    // Degree update
    //-----------------------------------------------------
    for (pk=pk1; pk<pk2; ++pk)   // scan2: degree update
    {
      i = Ci[pk];           // consider node i in Lk
      p1 = Cp[i];
      p2 = p1 + elen[i] - 1;
      pn = p1;
      for (h=0, d=0, p=p1; p<=p2; ++p) // scan Ei
      {
        e = Ci[p];
        if (w[e] != 0)      // e is an unabsorbed element
        {
          dext = w[e]-mark; // dext = |Le\Lk|
          if (dext>0) {
            d += dext;      // sum up the set differences
            Ci[pn++] = e;   // keep e in Ei
            h += e;         // compute the hash of node i
          } else {
            Cp[e] = CS_FLIP (k);  // aggressive absorb. e->k
            w [e] = 0;      // e is a dead element
          }
        }
      }
      elen[i] = pn-p1+1;    // elen[i] = |Ei|
      p3 = pn;
      p4 = p1 + len [i];
      for (p = p2 + 1; p < p4; p++) // prune edges in Ai
      {
        j = Ci [p];
        if ((nvj = nv [j]) <= 0) continue; // node j dead or in Lk
        d += nvj;             // degree(i) += |j|
        Ci [pn++] = j;        // place j in node list of i
        h += j;               // compute hash for node i
      }
      if (d == 0)             // check for mass elimination
      {
        Cp[i] = CS_FLIP (k);  // absorb i into k
        nvi = -nv [i];
        dk  -= nvi;           // |Lk| -= |i|
        nvk += nvi;           // |k| += nv[i]
        nel += nvi;
        nv[i] = 0;
        elen[i] = -1;         // node i is dead
      }
      else
      {
        degree [i] = std::min(degree [i], d); // update degree(i)
        Ci[pn] = Ci[p3];      // move first node to end
        Ci[p3] = Ci[p1];      // move 1st el. to end of Ei
        Ci[p1] = k;           // add k as 1st element in of Ei
        len[i] = pn-p1+1;     // new len of adj. list of node i
        h %= n;               // finalize hash of i
        next[i] = hhead[h];   // place i in hash bucket
        hhead[h] = i;
        last[i] = h;          // save hash of i in last[i]
      }
    } // scan2 is done

    degree[k] = dk;               // finalize |Lk|
    lemax = std::max(lemax, dk);
    mark = CS_wclear(mark+lemax, lemax, w, n);  // clear w
    //-----------------------------------------------------
    // Supernode detection
    //-----------------------------------------------------
    for (pk=pk1; pk<pk2; ++pk)
    {
      i = Ci[pk];
      if (nv[i] >= 0) continue;   // skip if i is dead
      h = last[i];                // scan hash bucket of node i
      i = hhead[h];
      hhead[h] = -1;              // hash bucket will be empty
      for ( ; i != -1 && next[i] != -1; i=next[i], mark++)
      {
        ln = len[i];
        eln = elen[i];
        for (p=Cp[i]+1; p<=Cp[i]+ln-1; ++p) w[Ci[p]] = mark;
        jlast = i;
        for (j=next[i]; j != -1; ) // compare i with all j
        {
          bOk = (len[j] == ln) && (elen[j] == eln);
          for (p=Cp[j]+1; bOk && p<=Cp[j]+ln-1; ++p)
          {
            if (w[Ci[p]] != mark) // compare i and j
              bOk = false;
          }
          if (bOk) {              // i and j are identical
            Cp[j] = CS_FLIP (i);  // absorb j into i
            nv[i] += nv[j];
            nv[j] = 0;
            elen[j] = -1;         // node j is dead
            j = next[j];          // delete j from hash bucket
            next[jlast] = j;
          } else {
            jlast = j;            // j and i are different
            j = next[j];
          }
        }
      }
    }
    //-----------------------------------------------------
    // Finalize new element
    //-----------------------------------------------------
    for (p=pk1, pk=pk1; pk<pk2; ++pk) // finalize Lk
    {
      i = Ci[pk];
      if ((nvi = -nv[i]) <= 0)        // skip if i is dead
        continue; 
      nv[i] = nvi;                    // restore nv[i]
      d = degree[i] + dk - nvi;       // compute external degree(i)
      d = std::min(d, n - nel - nvi);
      if (head[d] != -1) 
        last[head[d]] = i;
      next[i] = head[d];              // put i back in degree list
      last[i] = -1;
      head[d] = i;
      mindeg = std::min(mindeg, d);   // find new minimum degree
      degree[i] = d;
      Ci[p++] = i;                    // place i in Lk
    }

    nv[k] = nvk;                // # nodes absorbed into k
    if ((len[k]=(p-pk1)) == 0)  // length of adj list of element k
    {
      Cp[k] = -1;               // k is a root of the tree
      w[k] = 0;                 // k is now a dead element
    }
    if (elenk != 0) cnz = p;    // free unused space in Lk
  }

  //-------------------------------------------------------
  // Postordering
  //-------------------------------------------------------
  for (i=0; i< n; ++i) Cp[i] = CS_FLIP (Cp[i]); // fix assembly tree
  for (j=0; j<=n; ++j) head[j] = -1;
  for (j=n; j>=0; j--)        // place unordered nodes in lists
  {
    if (nv[j] > 0) continue;  // skip if j is an element
    next[j] = head[Cp[j]];    // place j in list of its parent
    head[Cp[j]] = j;
  }
  for (e=n; e>=0; e--)        // place elements in lists
  {
    if (nv[e] <= 0) continue; // skip unless e is an element
    if (Cp[e] != -1) {
      next[e] = head[Cp[e]];  // place e in list of its parent
      head[Cp[e]] = e;
    }
  }
  for (k=0, i=0; i<=n; ++i)   // postorder the assembly tree
  {
    if (Cp[i] == -1) {
      k = CS_tdfs(i, k, head, next, P->data(), w);
    }
  }

  C.reset();      // force deallocation of C
  return (*P);    // success
}


//---------------------------------------------------------
// Interface to SYMAMD
//---------------------------------------------------------

/*

#define USE_SYMAMD 0
#if (USE_SYMAMD)

#include "D:\_TW\SuiteSparse\COLAMD\Include\colamd.h"

//---------------------------------------------------------
void CS_symamd(CSd& cs_A, IVec& perm, IVec& iperm)
//---------------------------------------------------------
{
  // symmetric matrices: only tril(A) used

  umMSG(1, "CS_symamd -- starting\n");
  int stats[COLAMD_STATS], rc=0;
  int m=cs_A.num_rows(), n=cs_A.num_cols(), nnz=cs_A.P[n];
  IVec &P=cs_A.P, &I=cs_A.I;

  umMSG(1, "CS_symamd -- matrix is (%d,%d), nnz=%d\n", m,n,nnz);
  perm.resize(n+1); assert(perm.ok());

  rc = symamd (m, I.data(), P.data(), // [in ] not changed
               perm.data(),           // [out] permutation
               NULL, stats,           // [out] statistics
               &calloc, &free);       // [in ] mem funcs to use

  if (!rc) {
    // invalidate the return arrays
    perm.destroy(); iperm.destroy();
    umWARNING("CS_symamd", "symamd failed, returned %d\n", rc);
  } else {
    symamd_report(stats);
    iperm.resize(perm.size()); assert(iperm.ok());
    for (int i=0; i<n; ++i) {
      iperm[ perm[i] ] = i;
    }
    umMSG(1, "CS_symamd -- done\n");
  }
}

#endif // USE_SYMAMD

*/


//---------------------------------------------------------
// helper routines to sort the row indices in a csc matrix
//---------------------------------------------------------
#define USE_CSC_SORT 0
#if (USE_CSC_SORT)


// merge [Left,Right] into [S]
// Left[0:nleft-1]+Right[0:nright-1] -> S[0:nleft+nright-1]
//---------------------------------------------------------
static void merge
(
        int S[],          // output of length nleft + nright
  const int Left[],       // left input of length nleft
  const int nleft,
  const int Right[],      // right input of length nright
  const int nright
)
//---------------------------------------------------------
{
  int p=0, pleft=0, pright=0;
  // merge the two inputs, Left and Right, while both inputs exist
  for (p=0, pleft=0, pright=0; pleft<nleft && pright<nright; ++p) {
    if (Left[pleft]<Right[pright]) { S[p] = Left [pleft++]; } 
    else                           { S[p] = Right[pright++]; }
  }
  // either input is exhausted; copy the remaining list into S
  for ( ; pleft  < nleft;  ++p) { S[p] = Left [pleft++]; }
  for ( ; pright < nright; ++p) { S[p] = Right[pright++]; }
}


// SORT(a,b) sorts [a b] in ascending order, so that a < b holds on output
#define SORT(a,b) { if (a > b) { t = a ; a = b ; b = t ; } }

// BUBBLE(a,b) sorts [a b] in ascending order, and sets done to 0 if it swaps
#define BUBBLE(a,b) { if (a > b) { t = a ; a = b ; b = t ; done = 0 ; } }


//---------------------------------------------------------
// mergesort (A, W, n) sorts an int array A of length n 
// in ascending order. W is a workspace array of size n.  
// Used this function for sorting row indices in columns 
// of a compressed (csc-form) sparse matrix.  Lists with
// length <= SMALL are sorted using a "bubble" sort. 
//---------------------------------------------------------

#define SMALL 10

//---------------------------------------------------------
static void mergesort
(
  int A[],	    // array to sort, of size n
  int W[],	    // workspace of size n
  int n
)
//---------------------------------------------------------
{
  if (n <= SMALL)
  {
    //-----------------------------------------------------
    // bubble sort for small lists of length SMALL or less
    //-----------------------------------------------------

    int t=0, done=0;
    switch (n) 
    {

#if SMALL >= 10
    case 10:
      // 10-element bubble sort
      done = 1 ;
      BUBBLE (A [0], A [1]) ;
      BUBBLE (A [1], A [2]) ;
      BUBBLE (A [2], A [3]) ;
      BUBBLE (A [3], A [4]) ;
      BUBBLE (A [4], A [5]) ;
      BUBBLE (A [5], A [6]) ;
      BUBBLE (A [6], A [7]) ;
      BUBBLE (A [7], A [8]) ;
      BUBBLE (A [8], A [9]) ;
      if (done) return ;
#endif

#if SMALL >= 9
    case 9:
      // 9-element bubble sort
      done = 1 ;
      BUBBLE (A [0], A [1]) ;
      BUBBLE (A [1], A [2]) ;
      BUBBLE (A [2], A [3]) ;
      BUBBLE (A [3], A [4]) ;
      BUBBLE (A [4], A [5]) ;
      BUBBLE (A [5], A [6]) ;
      BUBBLE (A [6], A [7]) ;
      BUBBLE (A [7], A [8]) ;
      if (done) return ;
#endif

#if SMALL >= 8
    case 8:
      // 8-element bubble sort
      done = 1 ;
      BUBBLE (A [0], A [1]) ;
      BUBBLE (A [1], A [2]) ;
      BUBBLE (A [2], A [3]) ;
      BUBBLE (A [3], A [4]) ;
      BUBBLE (A [4], A [5]) ;
      BUBBLE (A [5], A [6]) ;
      BUBBLE (A [6], A [7]) ;
      if (done) return ;
#endif

#if SMALL >= 7
    case 7:
      // 7-element bubble sort
      done = 1 ;
      BUBBLE (A [0], A [1]) ;
      BUBBLE (A [1], A [2]) ;
      BUBBLE (A [2], A [3]) ;
      BUBBLE (A [3], A [4]) ;
      BUBBLE (A [4], A [5]) ;
      BUBBLE (A [5], A [6]) ;
      if (done) return ;
#endif

#if SMALL >= 6
    case 6:
      // 6-element bubble sort
      done = 1 ;
      BUBBLE (A [0], A [1]) ;
      BUBBLE (A [1], A [2]) ;
      BUBBLE (A [2], A [3]) ;
      BUBBLE (A [3], A [4]) ;
      BUBBLE (A [4], A [5]) ;
      if (done) return ;
#endif

#if SMALL >= 5
    case 5:
      // 5-element bubble sort
      done = 1 ;
      BUBBLE (A [0], A [1]) ;
      BUBBLE (A [1], A [2]) ;
      BUBBLE (A [2], A [3]) ;
      BUBBLE (A [3], A [4]) ;
      if (done) return ;
#endif

    case 4:
      // 4-element bubble sort
      done = 1 ;
      BUBBLE (A [0], A [1]) ;
      BUBBLE (A [1], A [2]) ;
      BUBBLE (A [2], A [3]) ;
      if (done) return ;

    case 3:
      // 3-element bubble sort
      done = 1 ;
      BUBBLE (A [0], A [1]) ;
      BUBBLE (A [1], A [2]) ;
      if (done) return ;

    case 2:
      // 2-element bubble sort
      SORT (A [0], A [1]) ; 

    case 1:
    case 0:
      // nothing to do
      ;
    }
  }
  else
  {
    //-------------------------------------------
    // recursive mergesort if length A > SMALL
    //-------------------------------------------
    int n1, n2, n3, n4, n12, n34, n123;

    n12 = n   / 2;  n34 = n   - n12;  // split n into n12 and n34
    n1  = n12 / 2;  n2  = n12 - n1;   // split n12 into n1 and n2
    n3  = n34 / 2;  n4  = n34 - n3;   // split n34 into n3 and n4

    n123 = n12 + n3;                  // start of 4th subset = n1 + n2 + n3

    mergesort (A,        W, n1);      // sort A [0    ...   n1-1]
    mergesort (A + n1,   W, n2);      // sort A [n1   ...  n12-1]
    mergesort (A + n12,  W, n3);      // sort A [n12  ... n123-1]
    mergesort (A + n123, W, n4);      // sort A [n123 ...    n-1]

    merge (W,     A,     n1, A+n1,  n2 ); // merge A[0  :  n1-1] and A[n1  :n12-1] into W [0  :n12-1]
    merge (W+n12, A+n12, n3, A+n123,n4 ); // merge A[n12:n123-1] and A[n123:  n-1] into W [n12:  n-1]
    merge (A,     W,     n12,W+n12, n34); // merge W[0  : n12-1] and W[n12 :  n-1] into A [0  :  n-1]
  }
}

#undef SORT
#undef BUBBLE
#undef SMALL

#endif // USE_CSC_SORT
