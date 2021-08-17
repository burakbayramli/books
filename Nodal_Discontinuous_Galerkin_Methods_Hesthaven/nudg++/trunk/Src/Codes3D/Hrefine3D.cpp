// Hrefine3D.cpp
// function Hrefine3D(refineflag)
// 2007/10/13
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "NDG3D.h"


///////////////////////////////////////////////////////////
//
// temporary helper/utility routines
//
///////////////////////////////////////////////////////////

// void Mat_COL<T>::set(int Nr, int Nc, ...)

template <typename T>  void set84
(
  Mat_COL<T>& A, int Nr, int Nc,
  T i11, T i12, T i13, T i14, 
  T i21, T i22, T i23, T i24, 
  T i31, T i32, T i33, T i34, 
  T i41, T i42, T i43, T i44, 
  T i51, T i52, T i53, T i54, 
  T i61, T i62, T i63, T i64, 
  T i71, T i72, T i73, T i74, 
  T i81, T i82, T i83, T i84
)
{
  A.resize(8,4, false);
  A(1,1)=i11;  A(1,2)=i12;  A(1,3)=i13;  A(1,4)=i14;
	A(2,1)=i21;  A(2,2)=i22;  A(2,3)=i23;  A(2,4)=i24;
	A(3,1)=i31;  A(3,2)=i32;  A(3,3)=i33;  A(3,4)=i34;
	A(4,1)=i41;  A(4,2)=i42;  A(4,3)=i43;  A(4,4)=i44;
	A(5,1)=i51;  A(5,2)=i52;  A(5,3)=i53;  A(5,4)=i54;
	A(6,1)=i61;  A(6,2)=i62;  A(6,3)=i63;  A(6,4)=i64;
	A(7,1)=i71;  A(7,2)=i72;  A(7,3)=i73;  A(7,4)=i74;
	A(8,1)=i81;  A(8,2)=i82;  A(8,3)=i83;  A(8,4)=i84;
}


template <typename T>  void set46
(
  Mat_COL<T>& A, int Nr, int Nc,
  T i11, T i12, T i13, T i14, T i15, T i16, 
  T i21, T i22, T i23, T i24, T i25, T i26, 
  T i31, T i32, T i33, T i34, T i35, T i36, 
  T i41, T i42, T i43, T i44, T i45, T i46
)
{
  A.resize(4,6, false);
  A(1,1)=i11;  A(1,2)=i12;  A(1,3)=i13;  A(1,4)=i14;  A(1,5)=i15;  A(1,6)=i16;
	A(2,1)=i21;  A(2,2)=i22;  A(2,3)=i23;  A(2,4)=i24;  A(2,5)=i25;  A(2,6)=i26;
	A(3,1)=i31;  A(3,2)=i32;  A(3,3)=i33;  A(3,4)=i34;  A(3,5)=i35;  A(3,6)=i36;
	A(4,1)=i41;  A(4,2)=i42;  A(4,3)=i43;  A(4,4)=i44;  A(4,5)=i45;  A(4,6)=i46;
}



//---------------------------------------------------------
void NDG3D::Hrefine3D(IVec& refineflag)
//---------------------------------------------------------
{
  DVec lVX("lVX"), lVY("lVY"), lVZ("lVZ");

  int a=1, b=2, c=3, d=4, e=5, 
      f=6, g=7, h=8, i=9, j=10;

  //             a b c d  e    f    g    h    i    j
  lVX.load(10, " 0 1 0 0  0.5  0.5  0.0  0.0  0.5  0.0 ");
  lVY.load(10, " 0 0 1 0  0.0  0.5  0.5  0.0  0.0  0.5 ");
  lVZ.load(10, " 0 0 0 1  0.0  0.0  0.0  0.5  0.5  0.5 ");

  assert(4 == Nfaces);

  IMat lEToV(8,4, "lEToV"), im84(8,4, "im84"), oFnodes(4,6, "oFnodes");

  set84(lEToV, 8,4, 
           a, e, g, h,
	         e, b, f, i,
	         g, f, c, j,
	         h, i, j, d,
	         e, g, h, i,
	         h, i, g, j,
	         e, f, g, i,
	         g, i, f, j);

  set46(oFnodes, 4,6, 
           a, e, b, f, c, g,
	         a, e, b, i, d, h,
	         b, f, c, j, d, i,
	         a, g, c, j, d, h);

  IMat vnum(gRowData, 4,3, "1 2 3  1 2 4  2 3 4  3 1 4");

  int lK = lEToV.num_rows();
  IMat lBCType(lK, Nfaces), tm1, tm2;
  IVec tv(3),tv1,tv2, oFn,vnp,ksids;
  int kk=0,ff=0,oo=0;
  for (kk=1; kk<=lK; ++kk) {
    for (ff=1; ff<=Nfaces; ++ff) {
      for (oo=1; oo<=Nfaces; ++oo) 
      {
        oFn = oFnodes.get_row(oo);
        vnp = vnum.get_row(ff);
        tm1 = lEToV(kk, vnp);  const IVec& knodes = dynamic_cast<const IVec&>(tm1);

      //tv = intersect(lEToV(k, vnum(p,All)), oFnodes(o,All));
        tv = intersect(knodes, oFn);

        if ( tv.length()==3 ) {
	        lBCType(kk, ff) = oo;
        }
      }
    }
  }

  int NV = VX.length()+1;
  int sp_len = NV + NV*NV;

  // sparse buffers   nnz  vals,triplet
  CSd newVX(sp_len,1,  NV,    1,  1);
  CSd newVY(sp_len,1,  NV,    1,  1);
  CSd newVZ(sp_len,1,  NV,    1,  1);

  Index1D II(1, NV-1);
  newVX(II,1) = VX;
  newVY(II,1) = VY;
  newVZ(II,1) = VZ;

  IVec ids("ids");
  int oldK = K, f1=0;

  for (int k1=1; k1<=oldK; ++k1) {
    if (refineflag(k1)) 
    {
      a = EToV(k1,1); 
      b = EToV(k1,2); 
      c = EToV(k1,3); 
      d = EToV(k1,4);

      e = NV + std::max(a*NV+b, b*NV + a);
      f = NV + std::max(b*NV+c, c*NV + b);
      g = NV + std::max(a*NV+c, c*NV + a);
      h = NV + std::max(a*NV+d, d*NV + a);
      i = NV + std::max(b*NV+d, d*NV + b);
      j = NV + std::max(c*NV+d, d*NV + c);

    //ks = [k1, K+1:K+7];
      IVec ks(1, k1);  ks.append(Range(K+1,K+7));

      //--------------------------
      // EToV(ks,:) = [a e g h;
		  //               e b f i;
		  //               g f c j;
		  //               h i j d;
		  //               e g h i;
		  //               h i g j;
		  //               e f g i;
		  //               g i f j];
      //--------------------------

      set84(im84, 8,4, 
            a, e, g, h,
		        e, b, f, i,
		        g, f, c, j,
		        h, i, j, d,
		        e, g, h, i,
		        h, i, g, j,
		        e, f, g, i,
		        g, i, f, j);

    //EToV(ks,All) = im84;
      EToV.merge_rows(ks, im84);

      for (f1=1; f1<=Nfaces; ++f1) 
      {
        tv = lBCType(All,f1);
        ids = find(tv, '!', 0);
        ksids = ks(ids);
        tm1 = lBCType(ids,f1);
        tm2 = BCType(k1, (const IVec&)tm1);

        // expand BCType to accommodate new range
        int maxI=ksids.max_val(), maxR=BCType.num_rows();
        if (maxI>maxR) {BCType.realloc(maxI, BCType.num_cols());}

        BCType(ksids,f1) = trans(tm2);
      }

      K += 7;

      newVX.set1(e,1, 0.5*(VX(a)+VX(b))); 
      newVX.set1(f,1, 0.5*(VX(b)+VX(c)));
      newVX.set1(g,1, 0.5*(VX(c)+VX(a)));
      newVX.set1(h,1, 0.5*(VX(a)+VX(d)));
      newVX.set1(i,1, 0.5*(VX(b)+VX(d)));
      newVX.set1(j,1, 0.5*(VX(c)+VX(d)));

      newVY.set1(e,1, 0.5*(VY(a)+VY(b)));
      newVY.set1(f,1, 0.5*(VY(b)+VY(c)));
      newVY.set1(g,1, 0.5*(VY(c)+VY(a)));
      newVY.set1(h,1, 0.5*(VY(a)+VY(d)));
      newVY.set1(i,1, 0.5*(VY(b)+VY(d)));
      newVY.set1(j,1, 0.5*(VY(c)+VY(d)));

      newVZ.set1(e,1, 0.5*(VZ(a)+VZ(b)));
      newVZ.set1(f,1, 0.5*(VZ(b)+VZ(c)));
      newVZ.set1(g,1, 0.5*(VZ(c)+VZ(a)));
      newVZ.set1(h,1, 0.5*(VZ(a)+VZ(d)));
      newVZ.set1(i,1, 0.5*(VZ(b)+VZ(d)));
      newVZ.set1(j,1, 0.5*(VZ(c)+VZ(d)));
    }
  }

  //-------------------------------------
  // drop duplicates and sort
  //-------------------------------------
  newVX.compress(true);
  newVY.compress(true);
  newVZ.compress(true);

//ids = sort(unique(EToV(:)), 'ascend');
  ids = unique(EToV);


  int len=ids.max_val(); 
  IVec gnum(len);
  gnum(ids) = Range(1,ids.length());

  VX = full( newVX, ids );
  VY = full( newVY, ids );
  VZ = full( newVZ, ids );

  // EToV = gnum(EToV);
  EToV.set_map(EToV, gnum);

  int NV_old = NV-1;        // local counters
  this->Nv = VX.length();   // update member variable

  umLOG(1, "Hrefine3D [%d] : old Nv = %4d, new Nv = %4d\n", ++refine_count, NV_old, Nv);
  umLOG(1, "                old K  = %4d, new K  = %4d\n\n", oldK, K);


#if (0)
  tetramesh(EToV, [VX', VY', VZ'])
#endif

}
