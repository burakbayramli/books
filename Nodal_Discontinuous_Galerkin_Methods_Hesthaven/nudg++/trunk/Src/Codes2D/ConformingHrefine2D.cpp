// ConformingHrefine2D.m
// function newQ = ConformingHrefine2D(edgerefineflag, Q)
// 2007/02/04
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "NDG2D.h"
#include "VecSort_Type.h"

// helper routine for ConformingHrefine2D
int  countrefinefaces(const IMat& edgerefineflag);

void VertexAngles(const DVec& x1, const DVec& x2, const DVec& x3, 
                  const DVec& y1, const DVec& y2, const DVec& y3, 
                        DVec& a1,       DVec& a2,       DVec& a3);

//---------------------------------------------------------
DMat& NDG2D::ConformingHrefine2D(IMat& edgerefineflag, const DMat& Qin)
//---------------------------------------------------------
{
#if (0)
  OutputNodes(false); // volume nodes
//OutputNodes(true);  // face nodes
#endif


  // function newQ = ConformingHrefine2D(edgerefineflag, Q)
  // Purpose: apply edge splits as requested by edgerefineflag

  IVec v1("v1"), v2("v2"), v3("v3"), tvi;
  DVec x1("x1"), x2("x2"), x3("x3"), y1("y1"), y2("y2"), y3("y3");
  DVec a1("a1"), a2("a2"), a3("a3");

  // count vertices
  assert (VX.size() == Nv);

  // find vertex triplets for elements to be refined
  v1 = EToV(All,1);  v2 = EToV(All,2);  v3 = EToV(All,3);
  x1 = VX(v1);       x2 = VX(v2);       x3 = VX(v3);
  y1 = VY(v1);       y2 = VY(v2);       y3 = VY(v3);

  // find angles at each element vertex (in radians)
  VertexAngles(x1,x2,x3,y1,y2,y3, a1,a2,a3);

  // absolute value of angle size
  a1.set_abs(); a2.set_abs(); a3.set_abs();

  int k=0,k1=0,f1=0,k2=0,f2=0, e1=0,e2=0,e3=0, b1=0,b2=0,b3=0, ref=0;
  IVec m1,m2,m3; DVec mx1, my1, mx2, my2, mx3, my3;

  // create new vertices at edge centers of marked elements 
  // (use unique numbering derived from unique edge number))
  m1 = max(IVec(Nv*(v1-1)+v2+1), IVec(Nv*(v2-1)+v1+1)); mx1=0.5*(x1+x2); my1=0.5*(y1+y2);
  m2 = max(IVec(Nv*(v2-1)+v3+1), IVec(Nv*(v3-1)+v2+1)); mx2=0.5*(x2+x3); my2=0.5*(y2+y3);
  m3 = max(IVec(Nv*(v1-1)+v3+1), IVec(Nv*(v3-1)+v1+1)); mx3=0.5*(x3+x1); my3=0.5*(y3+y1);

  // ensure that both elements sharing an edge are split
  for (k1=1; k1<=K; ++k1) {
    for (f1=1; f1<=Nfaces; ++f1) {
      if (edgerefineflag(k1,f1)) {
        k2 = EToE(k1,f1); 
        f2 = EToF(k1,f1);
        edgerefineflag(k2,f2) = 1;
      }
    }
  }

  // store old data
  IMat oldEToV = EToV;  DVec oldVX = VX, oldVY = VY; 

  // count the number of elements in the refined mesh
  int newK = countrefinefaces(edgerefineflag);
  EToV.resize(newK, Nfaces, true, 0);
  IMat newBCType(newK,3, "newBCType");
  
  //   kold = [];
  IVec kold(newK, "kold");  Index1D KI,KIo;

  int sk=1, skstart=0, skend=0;

  for (k=1; k<=K; ++k)
  {
    skstart = sk;

    e1 = edgerefineflag(k,1); b1 = BCType(k,1);
    e2 = edgerefineflag(k,2); b2 = BCType(k,2);
    e3 = edgerefineflag(k,3); b3 = BCType(k,3);
    ref = e1 + 2*e2 + 4*e3;
    
    switch (ref) {

    case 0: 
      EToV(sk, All) = IVec(v1(k),v2(k),v3(k));    newBCType(sk,All) = IVec(b1, b2, b3); ++sk;
      break;

    case 1:
      EToV(sk, All) = IVec(v1(k),m1(k),v3(k));    newBCType(sk,All) = IVec(b1,  0, b3); ++sk;
      EToV(sk, All) = IVec(m1(k),v2(k),v3(k));    newBCType(sk,All) = IVec(b1, b2,  0); ++sk;
      break;

    case 2:
      EToV(sk, All) = IVec(v2(k),m2(k),v1(k));    newBCType(sk,All) = IVec(b2,  0, b1); ++sk;
      EToV(sk, All) = IVec(m2(k),v3(k),v1(k));    newBCType(sk,All) = IVec(b2, b3,  0); ++sk;
      break;

    case 4:
      EToV(sk, All) = IVec(v3(k),m3(k),v2(k));    newBCType(sk,All) = IVec(b3,  0, b2); ++sk;
      EToV(sk, All) = IVec(m3(k),v1(k),v2(k));    newBCType(sk,All) = IVec(b3, b1,  0); ++sk;
      break;

    case 3:
      EToV(sk, All) = IVec(m1(k),v2(k),m2(k));    newBCType(sk,All) = IVec(b1, b2,  0); ++sk;
      if (a1(k) > a3(k)) { // split largest angle
        EToV(sk, All) = IVec(v1(k),m1(k),m2(k));  newBCType(sk,All) = IVec(b1,  0,  0); ++sk;
        EToV(sk, All) = IVec(v1(k),m2(k),v3(k));  newBCType(sk,All) = IVec( 0, b2, b3); ++sk;
      } else {
        EToV(sk, All) = IVec(v3(k),m1(k),m2(k));  newBCType(sk,All) = IVec( 0,  0, b2); ++sk;
        EToV(sk, All) = IVec(v3(k),v1(k),m1(k));  newBCType(sk,All) = IVec(b3, b1,  0); ++sk;
      }
      break;

    case 5:
      EToV(sk, All) = IVec(v1(k),m1(k),m3(k));    newBCType(sk,All) = IVec(b1,  0, b3); ++sk;
      if (a2(k) > a3(k)) { 
        // split largest angle
        EToV(sk, All) = IVec(v2(k),m3(k),m1(k));  newBCType(sk,All) = IVec( 0,  0, b1); ++sk;
        EToV(sk, All) = IVec(v2(k),v3(k),m3(k));  newBCType(sk,All) = IVec(b2, b3,  0); ++sk;
      } else {
        EToV(sk, All) = IVec(v3(k),m3(k),m1(k));  newBCType(sk,All) = IVec(b3,  0,  0); ++sk;
        EToV(sk, All) = IVec(v3(k),m1(k),v2(k));  newBCType(sk,All) = IVec( 0, b1, b2); ++sk;
      }
      break;

    case 6:
      EToV(sk, All) = IVec(v3(k),m3(k),m2(k));    newBCType(sk,All) = IVec(b3,  0, b2); ++sk;
      if (a1(k) > a2(k)) { 
        // split largest angle
        EToV(sk, All) = IVec(v1(k),m2(k),m3(k));  newBCType(sk,All) = IVec( 0, 0, b3); ++sk;
        EToV(sk, All) = IVec(v1(k),v2(k),m2(k));  newBCType(sk,All) = IVec(b1, b2,  0); ++sk;
      } else {
        EToV(sk, All) = IVec(v2(k),m2(k),m3(k));  newBCType(sk,All) = IVec(b2,  0,  0); ++sk;
        EToV(sk, All) = IVec(v2(k),m3(k),v1(k));  newBCType(sk,All) = IVec( 0 , b3, b1); ++sk;
      }
      break;

    default:
      // split all 
      EToV(sk, All) = IVec(m1(k),m2(k),m3(k)); newBCType(sk, All) = IVec( 0, 0,  0); ++sk;
      EToV(sk, All) = IVec(v1(k),m1(k),m3(k)); newBCType(sk, All) = IVec(b1, 0, b3); ++sk;
      EToV(sk, All) = IVec(v2(k),m2(k),m1(k)); newBCType(sk, All) = IVec(b2, 0, b1); ++sk;
      EToV(sk, All) = IVec(v3(k),m3(k),m2(k)); newBCType(sk, All) = IVec(b3, 0, b2); ++sk;
      break;
    }
    
    skend = sk;

    // kold = [kold; k*ones(skend-skstart, 1)];

    // element k is to be refined into (1:4) child elements.
    // store parent element numbers in array "kold" to help 
    // with accessing parent vertex data during refinement.

    KI.reset(skstart, skend-1); // ids of child elements
    kold(KI) = k;               // mark as children of element k
  }

  // Finished with edgerefineflag.  Delete if OBJ_temp
  if (edgerefineflag.get_mode() == OBJ_temp) { 
    delete (&edgerefineflag); 
  }


  // renumber new nodes contiguously
  // ids = unique([v1;v2;v3;m1;m2;m3]);
  bool unique=true; IVec IDS, ids;
  IDS = concat( concat(v1,v2,v3), concat(m1,m2,m3) );
  ids = sort(IDS, unique);
  Nv = ids.size();

  int max_id = EToV.max_val();
  umMSG(1, "max id in EToV is %d\n", max_id);

  //         M     N   nnz vals triplet
  CSi newids(max_id,1, Nv,  1,    1  );
  //  newids = sparse(max(max(EToV)),1);

  int i=0, j=1;
  for (i=1; i<=Nv; ++i) {
  //     newids(ids)= (1:Nv);
    newids.set1(ids(i),j, i);   // load 1-based triplets
  }          // row   col x
  newids.compress();            // convert to csc form


  // Matlab -----------------------------------------------
  // v1 = newids(v1); v2 = newids(v2); v3 = newids(v3);
  // m1 = newids(m1); m2 = newids(m2); m3 = newids(m3);
  //-------------------------------------------------------

  int KVi=v1.size(), KMi=m1.size();
  // read from copies, overwrite originals 
  
  // 1. reload ids for new vertices
  tvi = v1;  for (i=1;i<=KVi;++i) {v1(i) = newids(tvi(i), 1);}
  tvi = v2;  for (i=1;i<=KVi;++i) {v2(i) = newids(tvi(i), 1);}
  tvi = v3;  for (i=1;i<=KVi;++i) {v3(i) = newids(tvi(i), 1);}

  // 2. load ids for new (midpoint) vertices
  tvi = m1;  for (i=1;i<=KMi;++i) {m1(i) = newids(tvi(i), 1);}
  tvi = m2;  for (i=1;i<=KMi;++i) {m2(i) = newids(tvi(i), 1);}
  tvi = m3;  for (i=1;i<=KMi;++i) {m3(i) = newids(tvi(i), 1);}

  VX.resize(Nv); VY.resize(Nv);
  VX(v1) =  x1; VX(v2) =  x2; VX(v3) =  x3;
  VY(v1) =  y1; VY(v2) =  y2; VY(v3) =  y3;
  VX(m1) = mx1; VX(m2) = mx2; VX(m3) = mx3;
  VY(m1) = my1; VY(m2) = my2; VY(m3) = my3;


  if (newK != (sk-1)) {
    umERROR("NDG2D::ConformingHrefine2D", "Inconsistent element count: expect %d, but sk = %d", newK, (sk-1));
  } else {
    K = newK; // sk-1;
  }

  // dumpIMat(EToV, "EToV (before)");

  // EToV = newids(EToV);
  for (j=1; j<=3; ++j) {
    for (k=1; k<=K; ++k) {
      EToV(k,j) = newids(EToV(k,j), 1);
    }
  }

#if (0)
  dumpIMat(EToV, "EToV (after)");
  // umERROR("Checking ids", "Nigel, check EToV");
#endif


  BCType = newBCType;

  Nv = VX.size();
  // xold = x; yold = y;

  StartUp2D();


#if (1)
  OutputNodes(false); // volume nodes
//OutputNodes(true);  // face nodes
//umERROR("Exiting early", "Check adapted {volume,face} nodes");
#endif


  // allocate return object
  int Nfields = Qin.num_cols();
  DMat* tmpQ = new DMat(Np*K, Nfields, "newQ", OBJ_temp);
  DMat& newQ = *tmpQ;  // use a reference for syntax

  // quick return, if no interpolation is required
  if (Qin.size()<1) {
    return newQ;
  }

  
  DVec rOUT(Np),sOUT(Np),xout,yout,xy1(2),xy2(2),xy3(2),tmp(2),rhs;
  int ko=0,kv1=0,kv2=0,kv3=0,n=0;  DMat A(2,2), interp;
  DMat oldQ = const_cast<DMat&>(Qin);

  for (k=1; k<=K; ++k)
  {
    ko = kold(k); xout = x(All,k); yout = y(All,k);
    kv1=oldEToV(ko,1); kv2=oldEToV(ko,2); kv3=oldEToV(ko,3);
    xy1.set(oldVX(kv1), oldVY(kv1));
    xy2.set(oldVX(kv2), oldVY(kv2));
    xy3.set(oldVX(kv3), oldVY(kv3));
    A.set_col(1, xy2-xy1); A.set_col(2, xy3-xy1);
    
    for (i=1; i<=Np; ++i) {
      tmp.set(xout(i), yout(i));
      rhs = 2.0*tmp - xy2 - xy3;
      tmp = A|rhs;
      rOUT(i) = tmp(1);
      sOUT(i) = tmp(2);
    }

    KI.reset (Np*(k -1)+1, Np*k );  // nodes in new element k
    KIo.reset(Np*(ko-1)+1, Np*ko);  // nodes in old element ko

    interp = Vandermonde2D(N, rOUT, sOUT)*invV;

    for (n=1; n<=Nfields; ++n) 
    {
    //newQ(:,k,n)= interp*  Q(:,ko,n);
      //DVec tm1 = interp*oldQ(KIo,n);
      //dumpDVec(tm1, "tm1");
      newQ(KI,n) = interp*oldQ(KIo,n);
    }
  }
    
  return newQ;
}


// helper routine for ConformingHrefine2D
//---------------------------------------------------------
int countrefinefaces(const IMat& edgerefineflag)
//---------------------------------------------------------
{
  int K = edgerefineflag.num_rows();
  int sk=0,  e1=0,e2=0,e3=0,ref=0;
  for (int k=1; k<=K; ++k)
  {    
    e1=edgerefineflag(k,1);
    e2=edgerefineflag(k,2);
    e3=edgerefineflag(k,3);
    ref = e1 + 2*e2 + 4*e3;

    switch (ref) {
    case 0:  sk += 1; break;  // element not split

    case 1:
    case 2:
    case 4:  sk += 2; break;  // split 1 face: get 2 elements

    case 3:
    case 5:
    case 6:  sk += 3; break;  // split 2 faces: get 3 elements

    default: sk += 4; break;  // split 3 faces: get 4 elements
    }
  }
  return sk;
}




//---------------------------------------------------------
void VertexAngles
(
  const DVec& x1, const DVec& x2, const DVec& x3,
  const DVec& y1, const DVec& y2, const DVec& y3,
        DVec& a1,       DVec& a2,       DVec& a3
)
//---------------------------------------------------------
{

  //------------------------------------------
  // Expand definitions from ElmTools
  //------------------------------------------
  // a1 = acos ( -a23() / sqrt(a22()*a33()) );
  // a2 = acos ( -a13() / sqrt(a11()*a33()) );
  // a3 = acos ( -a12() / sqrt(a11()*a22()) );

  DVec g2x=(y3-y1), g2y=(x1-x3), g3x=(y1-y2), g3y=(x2-x1);
  
  DVec det = g3y*g2x - g3x*g2y;  
  DVec d   = 1.0/det;
  
  g2x *= d;  g2y *= d;  g3x *= d;  g3y *= d;

  DVec g1x =  - g2x - g3x;
  DVec g1y =  - g2y - g3y;

  a1 = acos( -(g2x*g3x + g2y*g3y) / sqrt((sqr(g2x)+sqr(g2y)) * (sqr(g3x)+sqr(g3y)) ));
  a2 = acos( -(g1x*g3x + g1y*g3y) / sqrt((sqr(g1x)+sqr(g1y)) * (sqr(g3x)+sqr(g3y)) ));
  a3 = acos( -(g1x*g2x + g1y*g2y) / sqrt((sqr(g1x)+sqr(g1y)) * (sqr(g2x)+sqr(g2y)) ));

#if (0)
  // check that the angles in each element sum to 180
  int Ni = x1.size(); double sum=0.0;
  umMSG(1, "\nChecking sum of angles in %d element\n", Ni);
  for (int i=1; i<=Ni; ++i) {
    sum = fabs(a1(i)) + fabs(a2(i)) + fabs(a3(i));
    if ( fabs(sum-M_PI) > 1e-15) {
      umMSG(1, "element %4d: %12.5e\n", i, fabs(sum-M_PI)); 
    }
  }
#endif
}
