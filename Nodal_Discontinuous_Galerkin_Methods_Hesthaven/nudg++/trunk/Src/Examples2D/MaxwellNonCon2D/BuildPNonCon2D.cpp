// BuildPNonCon2D.cpp
// function pinfo = BuildPNonCon2D(Norder, pK, pVX, pVY, pEToV, pBCType)
// 2007/08/04
//---------------------------------------------------------
#include "NDGLib_headers.h"
//#include "NDG2D.h"
#include "MaxwellNonCon2D.h"

//#########################################################
// TODO: migrate back to class NDG2D
//#########################################################


//---------------------------------------------------------
void MaxwellNonCon2D::BuildPNonCon2D
(
  int Norder,     // [in] single integer
  int   pK,       // [in]
  DVec& pVX,      // [in]
  DVec& pVY,      // [in]
  IMat& pEToV,    // [in]
  IMat& pBCType,  // [in]
  PInfoV& pinfo   // [out]
)
//---------------------------------------------------------
{
  // load Norder as a list of {N} values
  IVec NList(1); NList(1) = Norder;
  BuildPNonCon2D(NList,pK,pVX,pVY,pEToV,pBCType,pinfo);
}



//---------------------------------------------------------
void MaxwellNonCon2D::BuildPNonCon2D
(
  IVec& Norder,   // [in] integer array
  int   pK,       // [in]
  DVec& pVX,      // [in]
  DVec& pVY,      // [in]
  IMat& pEToV,    // [in]
  IMat& pBCType,  // [in]
  PInfoV& pinfo   // [out]
)
//---------------------------------------------------------
{
  // function pinfo = BuildPNonCon2D(Norder, pK, pVX, pVY, pEToV, pBCType)
  //
  // Purpose: construct info necessary for DGTD on P-nonconforming meshes
  // Globals2D;

  // Find maximum requested polynomial order
  this->Nmax = Norder.max_val();

  // store original boundary conditions
  saveBCType = pBCType;

  // allow for one PInfo object for each order N
  pinfo.resize(Nmax+1, NULL);

  // Mesh details
  Nfaces = 3; NODETOL = 1e-12; VX = pVX; VY = pVY;

  DVec x1,y1,foo,rids1,rids2,sids3, gz,gw, rM,rP,gzr;
  IVec tids,ids,ids1,ids2,ids3,idsM,idsP;
  IVec KR,ksN, va,vb,vc, fN1,fN2,fmask;
  IMat kmap(pK,2), EToV_N, idsPM;  DMat interpM,interpP,mmM;
  DMat_Diag Dgw; 
  int N1=0,N2=0,k=0,offset=0,f1=0,f2=0,k1=0,k2=0;
  int k1orig=0,k2orig=0,pi1Nfp=0;

  // Perform a mini StartUp2D for the elements of each order
  int sk = 1;
  for (N=1; N<=Nmax; ++N) 
  {
    // load N'th order polynomial nodes
    Np = (N+1)*(N+2)/2; Nfp = N+1;
    Nodes2D(N, x1,y1);  xytors(x1,y1, r,s);

    // Find list of N'th order nodes on each face of the reference element
    tids = find(abs(s+1.0), '<', NODETOL);  rids1=r(tids);
    sort(rids1, foo, ids, eAscend);         ids1=tids(ids);

    tids = find(abs(r+s  ), '<', NODETOL);  rids2=r(tids);
    sort(rids2, foo, ids, eDescend);        ids2=tids(ids);

    tids = find(abs(r+1.0), '<', NODETOL);  sids3=s(tids);
    sort(sids3, foo, ids, eDescend);        ids3=tids(ids);

    // Fmask = [ids1,ids2,ids3];
    Fmask.resize(Nfp,3);              // set shape (M,N) before concat()
    Fmask = concat(ids1,ids2,ids3);   // load vector into shaped matrix

    // Build reference element matrices
    V = Vandermonde2D(N,r,s); invV = inv(V);
    MassMatrix = trans(invV)*invV;
    ::Dmatrices2D(N,r,s,V, Dr,Ds);
    Lift2D();    

    // store information for N'th order elements 
    pinfo[N] = new PInfo(Nmax); PInfo& pinfo_N = *(pinfo[N]);
    pinfo_N.Np=Np; pinfo_N.Nfp=Nfp; pinfo_N.Fmask=Fmask; pinfo_N.r=r; pinfo_N.s=s; 
    pinfo_N.Dr=Dr; pinfo_N.Ds=Ds; pinfo_N.LIFT=LIFT; pinfo_N.V=V;

    // Find elements of polynomial order N
    ksN = find(Norder, '=', N);
    K = ksN.length();

    pinfo_N.K = K;
    pinfo_N.ks = ksN; 

    if (K>0) {

      // Use the subset of elements of order N
      EToV_N = pEToV(ksN,All); BCType = saveBCType(ksN,All); KR=Range(1,K);
      kmap(ksN,1) = KR.data(); kmap(ksN,2) = N;

      // Build coordinates of all the nodes
      va = EToV_N(All,1); vb = EToV_N(All,2); vc = EToV_N(All,3);

      x = 0.5 * (-(r+s)*VX(va) + (1.0+r)*VX(vb) + (1.0+s)*VX(vc));
      y = 0.5 * (-(r+s)*VY(va) + (1.0+r)*VY(vb) + (1.0+s)*VY(vc));

      // Calculate geometric factors
      ::GeometricFactors2D(x,y,Dr,Ds,  rx,sx,ry,sy,J);
      Normals2D();
      Fscale = sJ.dd(J(Fmask,All));

      // Calculate element connections on this mesh
      tiConnect2D(EToV_N, EToE,EToF);
      BuildMaps2D();
      BuildBCMaps2D();

      pinfo_N.mapW = mapW;

      //---------------------------------------------------
      // Compute triangulation of N'th order nodes on mesh
      //
      // triN = delaunay(r, s); alltri = [];
      // for (k=1; k<=K; ++k) { alltri = [alltri; triN+(k-1)*Np]; }
      // pinfo_N.tri = alltri;
      //---------------------------------------------------
      IMat triN, alltri;
      Triangulation2D(N, triN);
      for (k=1; k<=K; ++k) {
        // append copies of reference triangulation, 
        // with ids shifted by (k-1)*Np
        alltri.append_rows( triN + (k-1)*Np );
      }
      pinfo_N.tri = alltri;
      //---------------------------------------------------


      // Store geometric information in pinfo struct
      pinfo_N.rx = rx; pinfo_N.sx = sx; pinfo_N.ry = ry; pinfo_N.sy = sy; 
      pinfo_N.nx = nx; pinfo_N.ny = ny; pinfo_N.sJ = sJ; pinfo_N.J  = J; 
      pinfo_N.x  = x; pinfo_N.y = y; pinfo_N.Fscale = Fscale;

      // (current) total number of DG nodes
      max_pinf_id = sk+K*Np-1;

      // Store location of the N'th order nodes in a global vector
      // pinfo_N.ids = reshape(sk:sk+K*Np-1, Np, K);
      pinfo_N.ids.load(Np,K, Range(sk,max_pinf_id));

      sk += K*Np;
    }
  }

  // For each possible order
  for (N1=1; N1<=Nmax; ++N1) {
    // generate face L2projection matrices (from order N2 to order N1 face space)
    for (N2=1; N2<=Nmax; ++N2) {

      // Set up sufficient Gauss quadrature to exactly perform surface integrals
      JacobiGQ(0, 0, std::max(N1,N2), gz, gw);

      const PInfo &piN1 = *(pinfo[N1]), &piN2 = *(pinfo[N2]);
      fN1=piN1.Fmask(All,1); fN2=piN2.Fmask(All,1);

      // All edges have same distribution (note special Fmask)
      rM = piN1.r(fN1);
      rP = piN2.r(fN2);

      // gzr = gz(end:-1:1);
      gzr = gz(Range(gz.size(),1)); Dgw.diag(gw);

      // Build N2 to N1 projection matrices for '+' trace data
      interpM = Vandermonde1D(N1, gz )/Vandermonde1D(N1, rM);
      interpP = Vandermonde1D(N2, gzr)/Vandermonde1D(N2, rP);

      // Face mass matrix used in projection
      mmM = trans(interpM) * Dgw * interpM;
      piN1.interpP[N2] = mmM | ( trans(interpM) * Dgw * interpP);
    }
  }

  // Generate neighbor information for all faces 
  tiConnect2D(pEToV,  EToE, EToF);

  // For each possible polynomial order  
  for (N1=1; N1<=Nmax; ++N1) {

    const PInfo &piN1 = *(pinfo[N1]);

    // Create a set of indexing arrays, one for each possible neighbor order
    for (N2=1; N2<=Nmax; ++N2) {
      (piN1.fmapM[N2]).resize(0);     // used as vector of ids
      (piN1.vmapP[N2]).resize(0,0);   // used as (Nfp,K) matrix
    }

    // Loop through all elements of order N1
    for (k1=1; k1<=piN1.K; ++k1) {

      // Find element in original mesh
      k1orig = piN1.ks(k1);

      // Check all it's faces
      for (f1=1; f1<=Nfaces; ++f1) 
      {
        // Find neighboring element (i.e. it's order and location in N2 mesh)
        k2orig = EToE(k1orig,f1);
        f2     = EToF(k1orig,f1);
        k2     = kmap(k2orig,1);
        N2     = kmap(k2orig,2);
        pi1Nfp = piN1.Nfp;

        // Compute location of face nodes of '-' trace
        offset = (k1-1)*pi1Nfp*Nfaces + (f1-1)*pi1Nfp;
        idsM   = Range(offset+1, offset+pi1Nfp);

        // Find location of volume nodes on '+' trace of (k1orig,f1)
        fmask = pinfo[N2]->Fmask(All,f2);
        idsPM = pinfo[N2]->ids(fmask, k2);  // extract as IMat
        idsP  = idsPM;                      // convert to Ivec

        // Store node locations in cell arrays

        // piN1.fmapM[N2] = [pinfo(N1).fmapM{N2}, idsM];
        // piN1.vmapP[N2] = [pinfo(N1).vmapP{N2}, idsP];

        piN1.fmapM[N2].append(idsM);      // used as vector of ids
        piN1.vmapP[N2].append_col(idsP);  // used as (Nfp,K) matrix
      }
    }
  }

  umMSG(1, "BuildPNonCon2D() complete.\n");
}

