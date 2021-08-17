// GeometricFactors2D.m
// function [rx,sx,ry,sy,J] = GeometricFactors2D(x,y,Dr,Ds)
// 2007/06/06
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "NDG2D.h"


// overloaded global version: matrix args
//---------------------------------------------------------
void GeometricFactors2D
(
  const DMat& x,  const DMat& y,
  const DMat& Dr, const DMat& Ds,
        DMat& rx, DMat& sx,
        DMat& ry, DMat& sy,
        DMat& J
)
//---------------------------------------------------------
{
  // function [rx,sx,ry,sy,J] = GeometricFactors2D(x,y,Dr,Ds)
  // Purpose  : Compute the metric elements for the local
  //            mappings of the elements

  // Calculate geometric factors
  DMat xr=Dr*x, xs=Ds*x, yr=Dr*y, ys=Ds*y;
  J  =  xr.dm(ys) - xs.dm(yr);
  rx = ys.dd(J); sx = -yr.dd(J); ry = -xs.dd(J); sy = xr.dd(J);
}


// overloaded global version: vector args
//---------------------------------------------------------
void GeometricFactors2D
(
  const DVec& x,  const DVec& y,
  const DMat& Dr, const DMat& Ds,
        DVec& rx, DVec& sx,
        DVec& ry, DVec& sy,
        DVec& J
)
//---------------------------------------------------------
{
  // function [rx,sx,ry,sy,J] = GeometricFactors2D(x,y,Dr,Ds)
  // Purpose  : Compute the metric elements for the local
  //            mappings of the elements

  // Calculate geometric factors
  DVec xr=Dr*x, xs=Ds*x, yr=Dr*y, ys=Ds*y;
  J  = xr.dm(ys) - xs.dm(yr);
  rx =  ys.dd(J);  sx = -yr.dd(J);  ry = -xs.dd(J); sy = xr.dd(J);
}


// member version
//---------------------------------------------------------
void NDG2D::GeometricFactors2D()
//---------------------------------------------------------
{
  // Calculate geometric factors
  DMat xr=Dr*x, xs=Ds*x, yr=Dr*y, ys=Ds*y;
  J  = xr.dm(ys) - xs.dm(yr);
  rx = ys.dd(J); sx = -yr.dd(J); ry = -xs.dd(J); sy = xr.dd(J);
}


// member version (cubature)
//---------------------------------------------------------
void NDG2D::GeometricFactors2D(Cub2D& cub)
//---------------------------------------------------------
{
  // Calculate geometric factors
  DMat xr = cub.Dr*this->x, xs = cub.Ds*this->x, 
       yr = cub.Dr*this->y, ys = cub.Ds*this->y;

  cub.J  =  xr.dm(ys) - xs.dm(yr);
  cub.rx =  ys.dd(cub.J);  cub.sx = -yr.dd(cub.J);
  cub.ry = -xs.dd(cub.J);  cub.sy =  xr.dd(cub.J);
}
