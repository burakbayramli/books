function[dees] = formdees(E,vu,thick)
%
% This function forms the elasticity matrix for the shear 
% action in a thick plate element 
%
G= E/(2*(1.+vu));
%
dees=G*[thick     0  ;...
          0     thick];
%
% end function fromdees
