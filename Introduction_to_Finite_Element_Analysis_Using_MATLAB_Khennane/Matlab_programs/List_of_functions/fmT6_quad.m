function[der,fun] = fmT6_quad(samp, ig)
%
% This function returns the vector of the shape function and their
% derivatives with respect to xi and eta at the gauss points for 
% an 8-noded quadrilateral
%
xi=samp(ig,1);
eta=samp(ig,2);
lambda = 1. - xi - eta;
%
fun(1) = -lambda*(1.-2*lambda);
fun(2) = 4.*xi*lambda;
fun(3) = -xi*(1.-2*xi);
fun(4) = 4.*xi*eta;
fun(5) = -eta*(1.-2*eta);
fun(6) = 4.*eta*lambda;
%
der(1,1)=1.-4*lambda;  der(1,2)=4.*(lambda-xi);
der(1,3)=-1.+4*xi;  der(1,4)=4.*eta;
der(1,5)=0.;  der(1,6)=-4.*eta;
%
der(2,1)=1.-4*lambda;  der(2,2)=-4.*xi;
der(2,3)=0.;  der(2,4)=4.*xi;
der(2,5)=-1.+4.*eta;  der(2,6)=4.*(lambda-eta);
%
% end function fmT6_quad
