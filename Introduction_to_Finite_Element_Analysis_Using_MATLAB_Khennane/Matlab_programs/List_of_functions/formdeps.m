function[dee] = formdeps(E,vu)
%
% This function forms the elasticity matrix for a plane strain problem
%
v1=1.-vu
c=E/((1.+vu)*(1.-2.*vu))
%
dee=[v1*c        vu*c            0.        ;...
     vu*c        v1*c            0.        ;...
     0.           0.       .5*c*(1.-2.*vu)];
%
% end function fromdeps
