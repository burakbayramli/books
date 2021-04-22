function[deeb] = formdeeb(E,vu,thick)
%
% This function forms the elasticity matrix for a bending 
% action in a plate element 
%
DR= E*(thick^3)/(12*(1.-vu*vu));
%
deeb=DR*[1          vu         0.        ;...
         vu         1           0.        ;...
         0.         0.       (1.-vu)/2]   ;
%
% end function fromdeeb
