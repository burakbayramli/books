function timstres(ga,elu,xi,w,dw,s,ds,ne,f3,h,mxelm)
% c     __________________________________________________________________
% c
% c     called in postproc to compute solution and its global derivatives
% c       at nine points (including the nodes) of the timoshenko element
% c
% c        xc........ global (i.e., problem) coordinate
% c        xi ....... local (i.e., element) coordinate
% c        sfl, sfq.. lagrange linear and quadratic shape functions
% c        dsfl,dsfq: first derivative of sf w.r.t. global coordinate
% c        elu....... column vector of generalized displacements
% c        w, dw..... transverse deflection and its derivative
% c        s, ds..... rotation and its derivative
% c     __________________________________________________________________
% c
%       implicit real*8 (a-h,o-z)
%       common/io/in,it
%       dimension elu(9),sfl(2),sfq(3),dsfl(2),dsfq(3),f3(mxelm)

gj =  h*0.5;
%c     interpolation functions for the lagrange linear element
sfl(1) = 0.5*(1.0-xi);
sfl(2) = 0.5*(1.0+xi);
dsfl(1) = -0.5/gj;
dsfl(2) = 0.5/gj;
%c     interpolation functions for the lagrange quadratic element
sfq(1) = -0.5*xi*(1.0-xi);
sfq(2) = 1.0-xi*xi;
sfq(3) = 0.5*xi*(1.0+xi);
dsfq(1) = -0.5*(1.0-2.0*xi)/gj;
dsfq(2) = -2.0*xi/gj;
dsfq(3) = 0.5*(1.0+2.0*xi)/gj;
%c
w3=(3.0*h*f3(ne)/ga + 8.0*(elu(1)+elu(3))+ 2.0*(elu(4)-elu(2))*h)/16.0;
w =  sfq(1)*elu(1) + sfq(2)*w3 + sfq(3)*elu(3);
dw= dsfq(1)*elu(1) +dsfq(2)*w3 +dsfq(3)*elu(3);
s =  sfl(1)*elu(2) + sfl(2)*elu(4);
ds= dsfl(1)*elu(2) +dsfl(2)*elu(4);
%c

end

