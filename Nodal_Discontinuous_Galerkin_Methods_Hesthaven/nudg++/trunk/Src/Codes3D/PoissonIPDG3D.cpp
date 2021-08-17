// CurvedPoissonIPDG2D.m
// function [OP,MM] = PoissonIPDG3D()
// 2007/10/16
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "NDG3D.h"


#define NDG_New_CHOLINC 1

//---------------------------------------------------------
void NDG3D::PoissonIPDG3D(CSd& spOP, CSd& spMM)
//---------------------------------------------------------
{
  // function [OP,MM] = PoissonIPDG3D()
  //
  // Purpose: Set up the discrete Poisson matrix directly
  //          using LDG. The operator is set up in the weak form


  DVec faceR("faceR"), faceS("faceS"), faceT("faceT");
  DMat V2D;  IVec Fm("Fm");  IVec i1_Nfp = Range(1,Nfp);
  double opti1=0.0, opti2=0.0; int i=0; 

  umLOG(1, "\n ==> {OP,MM} assembly: ");
  opti1 = timer.read(); // time assembly

  // build local face matrices
  DMat massEdge[5]; // = zeros(Np,Np,Nfaces);
  for (i=1; i<=Nfaces; ++i) {
    massEdge[i].resize(Np,Np);
  }

  // face mass matrix 1
  Fm = Fmask(All,1); faceR=r(Fm); faceS=s(Fm); 
  V2D = Vandermonde2D(N, faceR, faceS);
  massEdge[1](Fm,Fm) = inv(V2D*trans(V2D));

  // face mass matrix 2
  Fm = Fmask(All,2); faceR = r(Fm); faceT = t(Fm);
  V2D = Vandermonde2D(N, faceR, faceT);
  massEdge[2](Fm,Fm) = inv(V2D*trans(V2D));

  // face mass matrix 3
  Fm = Fmask(All,3); faceS = s(Fm); faceT = t(Fm);
  V2D = Vandermonde2D(N, faceS, faceT); 
  massEdge[3](Fm,Fm) = inv(V2D*trans(V2D));

  // face mass matrix 4
  Fm = Fmask(All,4); faceS = s(Fm); faceT = t(Fm);
  V2D = Vandermonde2D(N, faceS, faceT); 
  massEdge[4](Fm,Fm) = inv(V2D*trans(V2D));

  // build local volume mass matrix
  MassMatrix = trans(invV)*invV;

  DMat Dx("Dx"),Dy("Dy"),Dz("Dz"), Dx2("Dx2"),Dy2("Dy2"),Dz2("Dz2");
  DMat Dn1("Dn1"),Dn2("Dn2"), mmE("mmE"), OP11("OP11"), OP12("OP12");
  DMat mmE_All_Fm1, mmE_Fm1_Fm1, Dn2_Fm2_All;
  IMat rows1,cols1,rows2,cols2;  int k1=0,f1=0,k2=0,f2=0,id=0;
  Index1D entries, entriesMM, idsM;  IVec fidM,vidM,Fm1,vidP,Fm2;
  double lnx=0.0,lny=0.0,lnz=0.0,lsJ=0.0,hinv=0.0,gtau=0.0;
  double N1N1 = double((N+1)*(N+1)); int NpNp = Np*Np;

  // build DG derivative matrices
  int max_OP = (K*Np*Np*(1+Nfaces));
  int max_MM = (K*Np*Np);

  // "OP" triplets (i,j,x), extracted to {Ai,Aj,Ax}
  IVec OPi(max_OP), OPj(max_OP), Ai,Aj; DVec OPx(max_OP), Ax;
  // "MM" triplets (i,j,x)
  IVec MMi(max_MM), MMj(max_MM); DVec MMx(max_MM);
  IVec OnesNp = Ones(Np);

  // global node numbering
  entries.reset(1,NpNp); entriesMM.reset(1,NpNp);

  OP12.resize(Np,Np);

  for (k1=1; k1<=K; ++k1)
  {
    if (! (k1%250)) { umLOG(1, "%d, ",k1); }

    rows1 = outer( Range((k1-1)*Np+1,k1*Np), OnesNp );
    cols1 = trans(rows1);

    // Build local operators  
    Dx = rx(1,k1)*Dr + sx(1,k1)*Ds + tx(1,k1)*Dt;   
    Dy = ry(1,k1)*Dr + sy(1,k1)*Ds + ty(1,k1)*Dt;
    Dz = rz(1,k1)*Dr + sz(1,k1)*Ds + tz(1,k1)*Dt;

    OP11 = J(1,k1)*(trans(Dx)*MassMatrix*Dx + 
                    trans(Dy)*MassMatrix*Dy + 
                    trans(Dz)*MassMatrix*Dz);

    // Build element-to-element parts of operator
    for (f1=1; f1<=Nfaces; ++f1) {
      k2 = EToE(k1,f1); f2 = EToF(k1,f1); 

      rows2 = outer( Range((k2-1)*Np+1, k2*Np), OnesNp );
      cols2 = trans(rows2);

      fidM  = (k1-1)*Nfp*Nfaces + (f1-1)*Nfp + i1_Nfp;
      vidM = vmapM(fidM); Fm1 = mod(vidM-1,Np)+1;
      vidP = vmapP(fidM); Fm2 = mod(vidP-1,Np)+1;

      id = 1+(f1-1)*Nfp + (k1-1)*Nfp*Nfaces;
      lnx = nx(id);  lny = ny(id);  lnz = nz(id); lsJ = sJ(id); 
      hinv = std::max(Fscale(id), Fscale(1+(f2-1)*Nfp, k2));    

      Dx2 = rx(1,k2)*Dr + sx(1,k2)*Ds + tx(1,k2)*Dt;   
      Dy2 = ry(1,k2)*Dr + sy(1,k2)*Ds + ty(1,k2)*Dt;
      Dz2 = rz(1,k2)*Dr + sz(1,k2)*Ds + tz(1,k2)*Dt;
      
      Dn1 = lnx*Dx  + lny*Dy  + lnz*Dz;
      Dn2 = lnx*Dx2 + lny*Dy2 + lnz*Dz2;

      mmE = lsJ*massEdge[f1];

      gtau = 2.0 * N1N1 * hinv; // set penalty scaling

      if (EToE(k1,f1)==k1) {
        OP11 += ( gtau*mmE - mmE*Dn1 - trans(Dn1)*mmE ); // ok
      }
      else 
      {
        // interior face variational terms
        OP11 += 0.5*( gtau*mmE - mmE*Dn1 - trans(Dn1)*mmE );

        // extract mapped regions:
        mmE_All_Fm1 = mmE(All,Fm1);
        mmE_Fm1_Fm1 = mmE(Fm1,Fm1);
        Dn2_Fm2_All = Dn2(Fm2,All);

        OP12 = 0.0;   // reset to zero
        OP12(All,Fm2)  = -0.5*(       gtau*mmE_All_Fm1 );
        OP12(Fm1,All) -=  0.5*(            mmE_Fm1_Fm1*Dn2_Fm2_All );
      //OP12(All,Fm2) -=  0.5*(-trans(Dn1)*mmE_All_Fm1 );
        OP12(All,Fm2) +=  0.5*( trans(Dn1)*mmE_All_Fm1 );

        // load this set of triplets
#if (1)
        OPi(entries)=rows1; OPj(entries)=cols2, OPx(entries)=OP12;
        entries += (NpNp);
#else
        //###########################################################
        // load only the lower triangle (after droptol test?)
        sk=0; start=entries(1);
        for (int i=1; i<=NpNp; ++i) {
          eid = start+i;
          id=entries(eid); rid=rows1(i); cid=cols2(i);
          if (rows1(rid) >= cid) {          // take lower triangle
            if ( fabs(OP12(id)) > 1e-15) {  // drop small entries
              ++sk; OPi(id)=rid; OPj(id)=cid, OPx(id)=OP12(id);
            }
          }
        }
        entries += sk;
        //###########################################################
#endif
      }
    }

    OPi(entries  )=rows1; OPj(entries  )=cols1, OPx(entries  )=OP11;
    MMi(entriesMM)=rows1; MMj(entriesMM)=cols1; MMx(entriesMM)=J(1,k1)*MassMatrix;
    entries += (NpNp); entriesMM += (NpNp);
  }
  umLOG(1, "\n ==> {OP,MM} to sparse\n");

  entries.reset(1, entries.hi()-Np*Np);

  // Extract triplets from the large buffers. Note: this 
  // requires copying each array, and since these arrays 
  // can be HUGE(!), we force immediate deallocation:

  Ai=OPi(entries);  OPi.Free();
  Aj=OPj(entries);  OPj.Free();
  Ax=OPx(entries);  OPx.Free();
  umLOG(1, " ==> triplets ready (OP) nnz = %10d\n", entries.hi());

  // adjust triplet indices for 0-based sparse operators
  Ai -= 1; Aj -= 1; MMi -= 1; MMj -= 1;  int npk=Np*K;

#if defined(NDG_USE_CHOLMOD) || defined(NDG_New_CHOLINC)
  // load only the lower triangle tril(OP)        free args?
  spOP.load(npk,npk, Ai,Aj,Ax, sp_LT, false,1e-15, true);  // {LT, false} -> TriL
#else
  // select {upper,lower,both} triangles
//spOP.load(npk,npk, Ai,Aj,Ax, sp_LT, true,1e-15,true);   // LT -> enforce symmetry
//spOP.load(npk,npk, Ai,Aj,Ax, sp_All,true,1e-15,true);   // All-> includes "noise"
//spOP.load(npk,npk, Ai,Aj,Ax, sp_UT, false,1e-15,true);  // UT -> triu(OP) only
#endif

  Ai.Free();  Aj.Free();  Ax.Free();

  umLOG(1, " ==> triplets ready (MM) nnz = %10d\n", entriesMM.hi());

  //-------------------------------------------------------
  // The mass matrix operator will NOT be factorised, 
  // Load ALL elements (both upper and lower triangles):
  //-------------------------------------------------------
  spMM.load(npk,npk, MMi,MMj,MMx, sp_All,false,1.00e-15,true);
  MMi.Free(); MMj.Free(); MMx.Free();

  opti2 = timer.read(); // time assembly
  umLOG(1, " ==> {OP,MM} converted to csc.  (%g secs)\n", opti2-opti1);
}
