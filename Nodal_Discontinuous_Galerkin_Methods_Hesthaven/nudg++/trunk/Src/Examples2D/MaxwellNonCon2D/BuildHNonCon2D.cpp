// BuildHNonCon2D.m
// function [neighbors] = BuildHNonCon2D(NGauss, tol)
// 2007/08/04
//---------------------------------------------------------
#include "NDGLib_headers.h"
//#include "NDG2D.h"
#include "MaxwellNonCon2D.h"

//#########################################################
// TODO: migrate back to class NDG2D
//#########################################################


//---------------------------------------------------------
void MaxwellNonCon2D::BuildHNonCon2D
(
  int NGauss,       // [in]
  double tol,       // [in]
  FInfoV& fInfo     // [out]
)
//---------------------------------------------------------
{
  // function [fInfo] = BuildHNonCon2D(NGauss, tol)
  // purpose: find element to element connections through
  // non-conforming interfaces (** elements assumed straight sided **)

  // TODO: estimate max_cons
  size_t max_cons=1000+1;
  fInfo.resize(max_cons, NULL);

  DVec gz,gw, xg,yg, vx1,vx2,vy1,vy2, x1,x2,y1,y2, v11(2),v12(2);
  DVec area1, area2, r21, r22, r1,r2,tr1,tr2, flag, rg1,sg1,rg2,sg2;
  IVec evmap1, evmap2, idB, elmtsB, facesB, matches;
  IMat idEF;  DMat xy11,xy12;  DMat_Diag dgw;
  int n=0,k1=0,f1=0,k2=0,f2=0, Nbc=0, b1=0, Nmatches=0;
  double x11=0.0,y11=0.0,x12=0.0,y12=0.0,L=0.0,partsJ;

  // 1. Build Gauss nodes
  JacobiGQ(0.0, 0.0, NGauss-1,  gz, gw);

  evmap1 = concat(EToV.get_col(1), EToV.get_col(2), EToV.get_col(3));
  evmap2 = concat(EToV.get_col(2), EToV.get_col(3), EToV.get_col(1));

  // 1.1 Find location of vertices of boundary faces
  vx1 = VX(evmap1); vx2 = VX(evmap2);
  vy1 = VY(evmap1); vy2 = VY(evmap2);

//idB = find(EToE==((1:K)'*ones(1,Nfaces)));
  idB = find( EToE, '=', outer(Range(1,K), Ones(Nfaces)) );

  x1 = vx1(idB); y1 = vy1(idB);
  x2 = vx2(idB); y2 = vy2(idB);

  // 1.2 Find those element-faces that are on boundary faces
//[elmtsB,facesB] = find(EToE==((1:K)'*ones(1,Nfaces)));
  idEF = find2D(EToE, '=', outer(Range(1,K), Ones(Nfaces)));
  elmtsB = idEF(All,1);  facesB = idEF(All,2);


  Nbc = elmtsB.length();

  int sk = 0;
  // 2.1 For each boundary face
  for (b1=1; b1<=Nbc; ++b1) {

    // 2.2 Find element and face of this boundary face
    k1 = elmtsB(b1); f1 = facesB(b1);

    // 2.3 Find end coordinates of b1'th boundary face
    x11 = x1(b1);  y11 = y1(b1);  x12 = x2(b1);  y12 = y2(b1);

    // 2.4 Compute areas, lengths and face coordinates used in intersection 
    // tests comparing b1'th boundary face with all boundary faces
    area1 = abs((x12-x11)*(y1-y11) - (y12-y11)*(x1-x11)); // scale
    area2 = abs((x12-x11)*(y2-y11) - (y12-y11)*(x2-x11));
    L   = SQ(x12-x11) + SQ(y12-y11);
    r21 = ((2.0*x1-x11-x12)*(x12-x11) + (2.0*y1-y11-y12)*(y12-y11))/L;
    r22 = ((2.0*x2-x11-x12)*(x12-x11) + (2.0*y2-y11-y12)*(y12-y11))/L;

    // 2.5 Find range of local face coordinate (bracketed between -1 and 1)
    r1 = max(-1.0,min(r21,r22)); r2 = min(1.0,max(r21,r22));

    // 2.6 Compute flag for overlap of b1 face with all other boundary faces
    //flag = area1+area2+(r1<= -1 & r2<= -1)+(r1>=1 & r2>=1)+(r2-r1<tol);
    flag = area1+area2 + (r1.le(-1.) && r2.le(-1.)) + 
                         (r1.ge( 1.) && r2.ge( 1.)) + ((r2-r1).lt(tol));

    // 2.7 Find other faces with partial matches
    matches = setdiff(find(flag, '<', tol),b1); 
    Nmatches = matches.length();

    if (Nmatches>0) 
    {
      // 3.1 Find matches
      tr1 = r1(matches); tr2 = r2(matches); 

      v11(1) = x11;   v12(1) = x12;
      v11(2) = y11;   v12(2) = y12;

      // 3.2 Find end points of boundary-boundary intersections
      // xy11 = 0.5*[x11;y11]*(1-r1) +  0.5*[x12;y12]*(1+r1);
      // xy12 = 0.5*[x11;y11]*(1-r2) +  0.5*[x12;y12]*(1+r2);

      xy11 = 0.5*outer(v11,(1.0-tr1)) +  0.5*outer(v12,(1.0+tr1));
      xy12 = 0.5*outer(v11,(1.0-tr2)) +  0.5*outer(v12,(1.0+tr2));

      // 3.3 For each face-face match
      for (n=1; n<=Nmatches; ++n) {

        // add new neighbor info
        fInfo[++sk] = new FInfo;

        // 3.4 Store which elements intersect
        k2 = elmtsB(matches(n)); f2 = facesB(matches(n));

        fInfo[sk]->elmtM = k1; fInfo[sk]->faceM = f1;
        fInfo[sk]->elmtP = k2; fInfo[sk]->faceP = f2;

        // 3.5 Build physical Gauss nodes on face fragment
        xg = 0.5*(1.0-gz)*xy11(1,n) + 0.5*(1.0+gz)*xy12(1,n);
        yg = 0.5*(1.0-gz)*xy11(2,n) + 0.5*(1.0+gz)*xy12(2,n);

        // 3.6 Find local coordinates of Gauss nodes
        FindLocalCoords2D(k1, xg, yg,  rg1,sg1);
        FindLocalCoords2D(k2, xg, yg,  rg2,sg2);

        // 3.7 Build interpolation matrices for volume nodes ->Gauss nodes
        fInfo[sk]->gVM = InterpMatrix2D(rg1,sg1);
        fInfo[sk]->gVP = InterpMatrix2D(rg2,sg2);

        // 3.8 Find face normal 
        fInfo[sk]->nx = nx(1+(f1-1)*Nfp,k1);
        fInfo[sk]->ny = ny(1+(f1-1)*Nfp,k1);

        // 4.0 Build partial face data lift operator
        // 4.1 Compute weights for lifting
        partsJ = 0.5 * sqrt(SQ(xy11(1,n)-xy12(1,n)) + SQ(xy11(2,n)-xy12(2,n)));
        dgw = gw*partsJ/J(1,k1); 
          
        // 4.2 Build matrix to lift Gauss data to volume data
        fInfo[sk]->lift = VVT*(trans(fInfo[sk]->gVM))*dgw;
      }
    }
  }

  Nnoncon = sk;

  // use elements [1:sk], ignore element [0]
  fInfo.resize(sk+1);
}
