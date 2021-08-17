// dtscale2D.m
// function dtscale = dtscale2D;
// 2007/06/06
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "NDG2D.h"


//---------------------------------------------------------
void NDG2D::dtscale2D(DVec& dtscale)
//---------------------------------------------------------
{
  // function dtscale = dtscale2D;
  // Purpose : Compute inscribed circle diameter as characteristic
  //           for grid to choose timestep

  DMat vx, vy;  DVec len1,len2,len3,sper,s1,s2,s3,Area;
  IVec vmask1, vmask2, vmask3, vmask;

  // Find vertex nodes
  vmask1 = find( abs(s+r+2.0), '<', NODETOL); 
  vmask2 = find( abs(  r-1.0), '<', NODETOL);
  vmask3 = find( abs(  s-1.0), '<', NODETOL);
  vmask = concat(vmask1,vmask2,vmask3);
  assert(vmask.size()==3);  // expect 1 node in each vmask[i]
  vx = x(vmask,All); vy = y(vmask,All);

  // Compute semi-perimeter and area
  len1 = sqrt( sqr(vx(1,All)-vx(2,All)) + sqr(vy(1,All)-vy(2,All)));
  len2 = sqrt( sqr(vx(2,All)-vx(3,All)) + sqr(vy(2,All)-vy(3,All)));
  len3 = sqrt( sqr(vx(3,All)-vx(1,All)) + sqr(vy(3,All)-vy(1,All)));
  sper = 0.5 * (len1 + len2 + len3);

//Area = sqrt( sper.* (sper-len1).* (sper-len2).* (sper-len3) );
//Area = sqrt( sper.dm(sper-len1).dm(sper-len2).dm(sper-len3) );

  s1=(sper-len1); s2=(sper-len2); s3=(sper-len3);
  Area = sqrt( sper.dm(s1.dm(s2.dm(s3))) );

  // Compute scale using radius of inscribed circle
  dtscale = Area.dd(sper);
}
