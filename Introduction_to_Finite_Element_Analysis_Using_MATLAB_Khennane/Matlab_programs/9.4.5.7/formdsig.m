function[dee] = formdsig(E,vu)
%
% This function forms the elasticity matrix for a plane stess problem
%
c=E/(1.-vu*vu);
%
dee=c*[1          vu         0.        ;...
     vu         1           0.        ;...
     0.          0.       .5*(1.-vu)];
%
% end function fromdeps
