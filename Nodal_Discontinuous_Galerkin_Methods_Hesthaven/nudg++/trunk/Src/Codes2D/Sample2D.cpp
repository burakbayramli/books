// Sample2D.m
// function [sampleweights,sampletri] = Sample2D(xout, yout)
// 2007/06/06
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "NDG2D.h"


void tsearch2D (DVec& X, DVec& Y, IMat& EtoV,   // [in]
                double xout, double yout,       // [in]
                int&  id,     // [out] element number containing vertex
                DVec& bary);  // [out] barycentric coords of vertex in element



//---------------------------------------------------------
void NDG2D::Sample2D
(
  double  xout,           // [in]
  double  yout,           // [in]
  DVec&   sampleweights,  // [out]
  int&    sampletri       // [out]
)
//---------------------------------------------------------
{
  // function [sampleweights,sampletri] = Sample2D(xout, yout)
  // purpose: input = coordinates of output data point
  //          output = number of containing tri and interpolation weights
  // [ only works for straight sided triangles ]

  DVec sout(1), rout(1);

  // find containing tri
  // [sampletri,tribary] = tsearchn([VX', VY'], EToV, [xout,yout]);

  DVec tribary(3);
  tsearch2D(VX, VY, EToV, xout, yout, sampletri,tribary);
  if (sampletri<1) {
    umERROR("NDG2D::Sample2D", "point (%g,%g) not in mesh (%d triangles)", xout, yout, EToV.num_rows());
  }

  // Matlab barycentric coordinates -> biunit triangle coordinates
  sout = 2.0*tribary(3)-1.0;
  rout = 2.0*tribary(2)-1.0;

  //-------------------------------------------------------
  // If (xout,yout) is a vertex, then {rout,sout} should 
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

  // build generalized Vandermonde for the sample point
  DMat Vout = Vandermonde2D(N, rout, sout);
       
  // build interpolation matrix for the sample point
  // Note: return as a vector
  sampleweights = Vout*this->invV;
}



//---------------------------------------------------------
void tsearch2D
(
  DVec&   X,          // [in]
  DVec&   Y,          // [in]
  IMat&   EtoV,       // [in]
  double  xout,       // [in]
  double  yout,       // [in]
          int&  id,   // [out] element number containing vertex
          DVec& bary  // [out] barycentric coords of vertex in element
)
//---------------------------------------------------------
{
  // return the index of an element containing the node 
  // xi, together with the barycentric coordinates of 
  // xi in the selected element. If xi is not found in 
  // the mesh, return idx <= 0.

  assert(EtoV.num_cols() == 3);   // 3 vertex ids for each triangle
  int ntri = EtoV.num_rows();     // num elements in mesh
  bool done=false;
  double t_eps =  -1.0e-8;        // set tolerance
//double t_eps =  -sqrt(eps);
  
  id = -1; bary.resize(3); IVec vids; DMat A=ones(3,3);
  DVec q, b(3); b(1)=1.0; b(2)=xout; b(3)=yout;

  // find triangle containing point (xout,yout)
  for (int i=1; i<=ntri; ++i) 
  {
    //     [1  x1  y1 ]
    // A = [1  x2  y2 ]   update A(:,2) A(:,3)
    //     [1  x3  y3 ]

    vids=EtoV.get_row(i);
    A(All,2)=X(vids); A(All,3)=Y(vids);

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
    umLOG(1, "tsearch2D: failed to locate point (%g,%g) in mesh (%d triangles)", xout, yout, ntri);
  }
}
