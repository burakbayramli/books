function[dee] = formdax(E,vu)
%
% This function forms the elasticity matrix for a plane stess problem
%
v1 = 1. - vu;
c = E/((1. + vu)*(1. - 2.*vu));
%
dee = c*[v1            vu          vu        0;...
         vu            v1          vu        0;...
         vu            vu          v1        0;...
         0.            0.          0.   .5*(1.-vu)];
%
% end function fromdeps
