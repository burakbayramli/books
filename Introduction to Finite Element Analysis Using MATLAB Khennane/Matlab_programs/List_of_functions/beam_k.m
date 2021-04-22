function[kl] = beam_k(i)
%
% This function forms the element stiffness in local coordinates
%
global nnd nel nne nodof eldof
global geom connec prop nf load Hinge
%
% retrieve the nodes of element i
%
node1=connec(i,1);
node2=connec(i,2);
%
%
% Retrieve the x and y coordinates of nodes 1 and 2
%
x1=geom(node1); x2=geom(node2);
%
% Evaluate length of element i
%
L = abs(x2-x1);
%
%  Retrieve section properties of element i
%
EI = prop(i,1)*prop(i,2);
%
%Calculate element stiffness matrix in its local coordinates
%
if Hinge(i, 1) == 0
    disp('hinge(1,1)=0')
kl=[ 3*EI/L^3      0     -3*EI/L^3     3*EI/L^2  ; ...
         0         0          0             0    ; ...
    -3*EI/L^3      0      3*EI/L^3     -3*EI/L^2 ; ...
     3*EI/L^2      0     -3*EI/L^2      3*EI/L   ];    
    
elseif Hinge(i, 2) == 0
    disp('hinge(1,2)=0')
kl=[ 3*EI/L^3     3*EI/L^2     -3*EI/L^3    0 ; ...
     3*EI/L^2     3*EI/L       -3*EI/L^2    0 ; ...
    -3*EI/L^3    -3*EI/L^2      3*EI/L^3    0 ; ...
         0          0              0        0 ] ;   
else
kl=[ 12*EI/L^3     6*EI/L^2     -12*EI/L^3     6*EI/L^2 ; ...
      6*EI/L^2     4*EI/L        -6*EI/L^2     2*EI/L   ; ...
    -12*EI/L^3    -6*EI/L^2      12*EI/L^3     -6*EI/L^2 ; ...
     6*EI/L^2      2*EI/L        -6*EI/L^2      4*EI/L   ];
end
%
% End function beam_k
 



