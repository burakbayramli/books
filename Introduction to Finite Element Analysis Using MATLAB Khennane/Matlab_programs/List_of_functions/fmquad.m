function[der,fun] = fmquad(samp, ig,jg)
%
% This function returns the vector of the shape function and their derivatives
% with respect to xi and eta at the gauss points for an 8-noded quadrilateral
%
xi=samp(ig,1);
eta=samp(jg,1);
etam=(1.-eta);
etap=(1.+eta);
xim=(1.-xi);
xip=(1.+xi);
%
fun(1) = -0.25*xim*etam*(1.+ xi + eta);
fun(2) = 0.5*(1.- xi^2)*etam;
fun(3) = -0.25*xip*etam*(1. - xi + eta);
fun(4) = 0.5*xip*(1. - eta^2);
fun(5) = -0.25*xip*etap*(1. - xi - eta);
fun(6) = 0.5*(1. - xi^2)*etap;
fun(7) = -0.25*xim*etap*(1. + xi - eta);
fun(8) = 0.5*xim*(1. - eta^2);
%
der(1,1)=0.25*etam*(2.*xi + eta);  der(1,2)=-1.*etam*xi;
der(1,3)=0.25*etam*(2.*xi-eta);  der(1,4)=0.5*(1-eta^2);
der(1,5)=0.25*etap*(2.*xi+eta);  der(1,6)=-1.*etap*xi;
der(1,7)=0.25*etap*(2.*xi-eta);  der(1,8)=-0.5*(1.-eta^2);
%
der(2,1)=0.25*xim*(2.*eta+xi);  der(2,2)=-0.5*(1. - xi^2);
der(2,3)=-0.25*xip*(xi-2.*eta);  der(2,4)=-1.*xip*eta;
der(2,5)=0.25*xip*(xi+2.*eta);  der(2,6)=0.5*(1.-xi^2);
der(2,7)=-0.25*xim*(xi-2.*eta);  der(2,8)=-1.*xim*eta;
%
% end function fmquad
