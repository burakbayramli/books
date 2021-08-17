// Sample3D.m
// function [sampleweights,sampletet] = Sample3D(xout, yout, zout)
// 2007/06/14
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "NDG3D.h"


void tsearch3D (DVec& X, DVec& Y, DVec& Z, IMat& EtoV, // [in]
                double xout, double yout, double zout, // [in]
                int&  id,     // [out] element number containing vertex
                DVec& bary);  // [out] barycentric coords of vertex in element



//---------------------------------------------------------
void NDG3D::Sample3D
(
  double  xout,           // [in]
  double  yout,           // [in]
  double  zout,           // [in]
  DVec&   sampleweights,  // [out]
  int&    sampletet       // [out]
)
//---------------------------------------------------------
{
  // function [sampleweights,sampletet] = Sample3D(xout, yout, zout)
  // purpose: input = coordinates of output data point
  //          output = number of containing tet and interpolation weights

  DVec sout(1), rout(1), tout(1);

#if (0)
  // find containing tet
  [sampletet,tetbary] = tsearchn([VX', VY', VZ'], EToV, [xout,yout,zout]);
#else
  //#######################################################
  DVec tetbary(4);
  tsearch3D(VX, VY, VZ, EToV, xout, yout, zout, sampletet,tetbary);
  if (sampletet<1) {
    umERROR("NDG3D::Sample3D", "point (%g,%g,%g) not in mesh (%d tets)", xout, yout, yout, EToV.num_rows());
  }
  //#######################################################
#endif

  // Matlab barycentric coordinates -> biunit triangle coordinates
  tout = 2.0*tetbary(4)-1.0;
  sout = 2.0*tetbary(3)-1.0;
  rout = 2.0*tetbary(2)-1.0;

  //-------------------------------------------------------
  // If (xo,yo,zo) is a vertex, then {ro,so,to} should 
  // be in {-1,0,1}.  Here we try to clean numerical noise 
  // before building the generalized Vandermonde matrix
  double bary_tol=1e-10;
  //-------------------------------------------------------
  if      (fabs( sout(1)+1.0 ) < bary_tol) {sout = -1.0;}
  else if (fabs( sout(1)     ) < bary_tol) {sout =  0.0;}
  else if (fabs( sout(1)-1.0 ) < bary_tol) {sout =  1.0;}
  //-------------------------------------------------------
  if      (fabs( rout(1)+1.0 ) < bary_tol) {rout = -1.0;}
  else if (fabs( rout(1)     ) < bary_tol) {rout =  0.0;}
  else if (fabs( rout(1)-1.0 ) < bary_tol) {rout =  1.0;}
  //-------------------------------------------------------
  if      (fabs( tout(1)+1.0 ) < bary_tol) {tout = -1.0;}
  else if (fabs( tout(1)     ) < bary_tol) {tout =  0.0;}
  else if (fabs( tout(1)-1.0 ) < bary_tol) {tout =  1.0;}
  //-------------------------------------------------------

  // build generalized Vandermonde for the sample point
  DMat Vout = Vandermonde3D(N, rout, sout, tout);
       
  // build interpolation matrix for the sample point
  sampleweights = Vout*this->invV;
  // Note: return as a vector

#if (0)
  dumpDMat(Vout, "Vout");
  dumpDVec(sampleweights, "sampleweights");
  umERROR("Testing", "Nigel, check arrays");
#endif

}


//---------------------------------------------------------
void tsearch3D
(
  DVec&   X,          // [in]
  DVec&   Y,          // [in]
  DVec&   Z,          // [in]
  IMat&   EtoV,       // [in]
  double  xout,       // [in]
  double  yout,       // [in]
  double  zout,       // [in]
          int&  id,   // [out] element number containing vertex
          DVec& bary  // [out] barycentric coords of vertex in element
)
//---------------------------------------------------------
{
  // return the index of an element containing the node 
  // xi, together with the barycentric coordinates of 
  // xi in the selected element. If xi is not found in 
  // the mesh, return idx <= 0.

  assert(EtoV.num_cols() == 4);   // 4 vertex ids for each tet.
  int ntet = EtoV.num_rows();     // num elements in mesh
  bool done=false;
  double t_eps =  -1.0e-8;        // set tolerance
//double t_eps =  -sqrt(eps);
  
  id = -1; bary.resize(4); IVec vids; DMat A=ones(4,4);
  DVec q, b(4); b(1)=1.0; b(2)=xout; b(3)=yout; b(4)=zout;

  // find triangle containing point (xout,yout)
  for (int i=1; i<=ntet; ++i) 
  {
    //     [1  x1  y1  z1]
    // A = [1  x2  y2  z2]   update A(:,2) A(:,3) A(:,4)
    //     [1  x3  y3  z3]
    //     [1  x4  y4  z4]

    vids=EtoV.get_row(i);
    A(All,2)=X(vids);
    A(All,3)=Y(vids);
    A(All,4)=Z(vids);

    // Compute barycentric coordinate of each point
    q = b/A;

    // if barycentric coords are all non-negative, then
    // target point lies in element, or on its boundary
    if (q.min_val() > t_eps) {
      id = i;     // set element index
      bary = q;   // set barycentric coordinates
      done=true;  // set flag to indicate succcess
      break;      // finished searching
    }
  }
  if (!done) {
    id = -1;  // force invalid index
    umLOG(1, "tsearch3D: failed to locate point (%g,%g,%g) in mesh (%d tets)", xout, yout, yout, ntet);
  }
}
