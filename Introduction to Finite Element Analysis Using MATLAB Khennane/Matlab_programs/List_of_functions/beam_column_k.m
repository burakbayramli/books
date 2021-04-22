function[kl] = beam_column_k(i)
%
% This function forms the beam-column element stiffness in local coordinates
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
x1=geom(node1,1); y1=geom(node1,2);
x2=geom(node2,1); y2=geom(node2,2);
%
% Evaluate length of element i
%
L = sqrt((x2-x1)^2 + (y2-y1)^2);
%
%  Retrieve section properties of element i
%
E = prop(i,1); A = prop(i,2); I = prop(i,3);
%
EA=E*A; EI=E*I;
%
%Calculate element stiffness matrix in its local coordinates
%
if Hinge(i,1) == 0
kl=[EA/L       0           0     -EA/L        0            0      ; ...
     0      3*EI/L^3       0       0      -3*EI/L^3     3*EI/L^2  ; ...
     0         0           0       0          0            0      ; ...
   -EA/L       0           0      EA/L        0            0      ; ...
     0     -3*EI/L^3       0       0       3*EI/L^3     -3*EI/L^2 ; ...
     0      3*EI/L^2       0       0      -3*EI/L^2      3*EI/L  ];    
    
elseif Hinge(i,2) == 0
kl=[EA/L       0           0           -EA/L        0         0 ; ...
     0      3*EI/L^3     3*EI/L^2        0       -3*EI/L^3    0 ; ...
     0      3*EI/L^2      3*EI/L         0       -3*EI/L^2    0 ; ...
   -EA/L       0             0          EA/L        0         0 ; ...
     0      -3*EI/L^3    -3*EI/L^2       0       3*EI/L^3     0 ; ...
     0         0             0           0          0         0];   
    
else    
kl=[EA/L       0           0           -EA/L        0            0      ; ...
     0      12*EI/L^3     6*EI/L^2       0      -12*EI/L^3     6*EI/L^2 ; ...
     0      6*EI/L^2      4*EI/L         0       -6*EI/L^2     2*EI/L   ; ...
   -EA/L       0             0          EA/L        0            0    ; ...
     0     -12*EI/L^3    -6*EI/L^2       0       12*EI/L^3     -6*EI/L^2 ; ...
     0      6*EI/L^2      2*EI/L         0       -6*EI/L^2      4*EI/L   ];
end
 %
% End function beam_column_k