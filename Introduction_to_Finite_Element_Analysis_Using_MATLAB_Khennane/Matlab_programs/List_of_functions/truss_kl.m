function[kl] = truss_kl(i)
%
% This function forms the element stiffness matrix 
% in local coordinates
%
global geom connec prop 
%
% retrieve the nodes of element i
%
node_1=connec(i,1);
node_2=connec(i,2);
%
%
% Retrieve the x and y coordinates of nodes 1 and 2
%
x1=geom(node_1,1); y1=geom(node_1,2);
x2=geom(node_2,1); y2=geom(node_2,2);
%
% Evaluate length of element i
%
L = sqrt((x2-x1)^2 + (y2-y1)^2);
%
% Retrieve section properties of element i
%
E= prop(i,1); A=prop(i,2);
%
% Calculate element stiffness matrix in its 
% local coordinates
%
kl=[E*A/L 0 -E*A/L 0 ; ...
0 0 0 0 ; ...
-E*A/L 0 E*A/L 0 ; ...
0 0 0 0 ];
%
%%%%%%%%%%%%%%%%%% End function truss_kl%%%%%%%%%%%%