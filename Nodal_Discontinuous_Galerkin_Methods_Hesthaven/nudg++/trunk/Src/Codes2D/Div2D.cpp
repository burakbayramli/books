// Div2D.m
// function [divu] = Div2D(u,v);
// 2007/06/06
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "NDG2D.h"

//---------------------------------------------------------
void NDG2D::Div2D(const DMat& u, const DMat& v, DMat& divu)
//---------------------------------------------------------
{
  // function [divu] = Div2D(u,v);
  // Purpose: Compute the 2D divergence of the vectorfield (u,v)

  DMat ur=Dr*u, us=Ds*u, vr=Dr*v, vs=Ds*v;
  divu = rx.dm(ur) + sx.dm(us) + ry.dm(vr) + sy.dm(vs);
}
