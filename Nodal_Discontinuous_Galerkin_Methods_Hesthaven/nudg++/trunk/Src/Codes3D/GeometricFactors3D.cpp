// GeometricFactors3D.m
// function [rx,sx,tx,ry,sy,ty,rz,sz,tz,J] = ...
//      GeometricFactors3D(x,y,z,Dr,Ds,Dt)
// 2007/10/07
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "NDG3D.h"


// overloaded global version: matrix args
//---------------------------------------------------------
void GeometricFactors3D
(
  const DMat& x,  const DMat& y,  const DMat& z,
  const DMat& Dr, const DMat& Ds, const DMat& Dt,
        DMat& rx,       DMat& sx,       DMat& tx,
        DMat& ry,       DMat& sy,       DMat& ty,
        DMat& rz,       DMat& sz,       DMat& tz,
        DMat& J
)
//---------------------------------------------------------
{
  // function [rx,sx,tx,ry,sy,ty,rz,sz,tz,J] = ...
  //              GeometricFactors3D(x,y,z,Dr,Ds,Dt)
  // Purpose  : Compute the metric elements for the local
  //            mappings of the elements

  // calculate geometric factors
  DMat xr=Dr*x,  xs=Ds*x,  xt=Dt*x;
  DMat yr=Dr*y,  ys=Ds*y,  yt=Dt*y;
  DMat zr=Dr*z,  zs=Ds*z,  zt=Dt*z, tmp;

  J = xr.dm(ys.dm(zt)-zs.dm(yt)) - 
      yr.dm(xs.dm(zt)-zs.dm(xt)) + 
      zr.dm(xs.dm(yt)-ys.dm(xt));

  rx  =  (ys.dm(zt) - zs.dm(yt)).dd(J);
  ry  = -(xs.dm(zt) - zs.dm(xt)).dd(J);
  rz  =  (xs.dm(yt) - ys.dm(xt)).dd(J);
           
  sx  = -(yr.dm(zt) - zr.dm(yt)).dd(J);
  sy  =  (xr.dm(zt) - zr.dm(xt)).dd(J);
  sz  = -(xr.dm(yt) - yr.dm(xt)).dd(J);
           
  tx  =  (yr.dm(zs) - zr.dm(ys)).dd(J);
  ty  = -(xr.dm(zs) - zr.dm(xs)).dd(J);
  tz  =  (xr.dm(ys) - yr.dm(xs)).dd(J);
}


// overloaded global version: vector args
//---------------------------------------------------------
void GeometricFactors3D
(
  const DVec& x,  const DVec& y,  const DVec& z,
  const DMat& Dr, const DMat& Ds, const DMat& Dt,
        DVec& rx,       DVec& sx,       DVec& tx,
        DVec& ry,       DVec& sy,       DVec& ty,
        DVec& rz,       DVec& sz,       DVec& tz,
        DVec& J
)
//---------------------------------------------------------
{
  // function [rx,sx,tx,ry,sy,ty,rz,sz,tz,J] = ...
  //              GeometricFactors3D(x,y,z,Dr,Ds,Dt)
  // Purpose  : Compute the metric elements for the local
  //            mappings of the elements

  // calculate geometric factors
  DVec xr = Dr*x,  xs = Ds*x,  xt = Dt*x;
  DVec yr = Dr*y,  ys = Ds*y,  yt = Dt*y;
  DVec zr = Dr*z,  zs = Ds*z,  zt = Dt*z;

  J = xr.dm(ys.dm(zt)-zs.dm(yt)) - 
      yr.dm(xs.dm(zt)-zs.dm(xt)) + 
      zr.dm(xs.dm(yt)-ys.dm(xt));

  rx  =  (ys.dm(zt) - zs.dm(yt)).dd(J);
  ry  = -(xs.dm(zt) - zs.dm(xt)).dd(J);
  rz  =  (xs.dm(yt) - ys.dm(xt)).dd(J);
  			   
  sx  = -(yr.dm(zt) - zr.dm(yt)).dd(J);
  sy  =  (xr.dm(zt) - zr.dm(xt)).dd(J);
  sz  = -(xr.dm(yt) - yr.dm(xt)).dd(J);
  			   
  tx  =  (yr.dm(zs) - zr.dm(ys)).dd(J);
  ty  = -(xr.dm(zs) - zr.dm(xs)).dd(J);
  tz  =  (xr.dm(ys) - yr.dm(xs)).dd(J);
}



// member version: matrix args
//---------------------------------------------------------
void NDG3D::GeometricFactors3D()
//---------------------------------------------------------
{
  // function [rx,sx,tx,ry,sy,ty,rz,sz,tz,J] = ...
  //              GeometricFactors3D(x,y,z,Dr,Ds,Dt)
  // Purpose  : Compute the metric elements for the local
  //            mappings of the elements

  // calculate geometric factors
  DMat xr=Dr*x,  xs=Ds*x,  xt=Dt*x;
  DMat yr=Dr*y,  ys=Ds*y,  yt=Dt*y;
  DMat zr=Dr*z,  zs=Ds*z,  zt=Dt*z, tmp;

  J = xr.dm(ys.dm(zt)-zs.dm(yt)) - 
      yr.dm(xs.dm(zt)-zs.dm(xt)) + 
      zr.dm(xs.dm(yt)-ys.dm(xt));

  rx  =  (ys.dm(zt) - zs.dm(yt)).dd(J);
  ry  = -(xs.dm(zt) - zs.dm(xt)).dd(J);
  rz  =  (xs.dm(yt) - ys.dm(xt)).dd(J);

  sx  = -(yr.dm(zt) - zr.dm(yt)).dd(J);
  sy  =  (xr.dm(zt) - zr.dm(xt)).dd(J);
  sz  = -(xr.dm(yt) - yr.dm(xt)).dd(J);

  tx  =  (yr.dm(zs) - zr.dm(ys)).dd(J);
  ty  = -(xr.dm(zs) - zr.dm(xs)).dd(J);
  tz  =  (xr.dm(ys) - yr.dm(xs)).dd(J);
}


// member version (cubature)
//---------------------------------------------------------
void NDG3D::GeometricFactors3D(Cub3D& cub)
//---------------------------------------------------------
{
  // Calculate geometric factors
  DMat xr=cub.Dr*this->x,  xs=cub.Ds*this->x,  xt=cub.Dt*this->x;
  DMat yr=cub.Dr*this->y,  ys=cub.Ds*this->y,  yt=cub.Dt*this->y;
  DMat zr=cub.Dr*this->z,  zs=cub.Ds*this->z,  zt=cub.Dt*this->z;

  cub.J = xr.dm(ys.dm(zt) - zs.dm(yt)) - 
          yr.dm(xs.dm(zt) - zs.dm(xt)) + 
          zr.dm(xs.dm(yt) - ys.dm(xt));

  cub.rx =  (ys.dm(zt) - zs.dm(yt)).dd(cub.J);
  cub.ry = -(xs.dm(zt) - zs.dm(xt)).dd(cub.J);
  cub.rz =  (xs.dm(yt) - ys.dm(xt)).dd(cub.J);
           
  cub.sx = -(yr.dm(zt) - zr.dm(yt)).dd(cub.J);
  cub.sy =  (xr.dm(zt) - zr.dm(xt)).dd(cub.J);
  cub.sz = -(xr.dm(yt) - yr.dm(xt)).dd(cub.J);
           
  cub.tx =  (yr.dm(zs) - zr.dm(ys)).dd(cub.J);
  cub.ty = -(xr.dm(zs) - zr.dm(xs)).dd(cub.J);
  cub.tz =  (xr.dm(ys) - yr.dm(xs)).dd(cub.J);
}
