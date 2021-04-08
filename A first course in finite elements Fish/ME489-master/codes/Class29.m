% ----------------------------------------------------------------------
%   Class 29 Matlab Code
%   Element stiffness matrix for the constant strain triangle
%   (Single Triangle Elasticity)
%   Developed by Adrian Buganza
%       email: buganza@purdue.edu
%   Converted by Emily Nadler
%       email: enadler@purdue.edu
%-----------------------------------------------------------------------

node_XY = [0,0; 3,0; 0,3];
elements = [1,2,3];
n_elem = 1;
E = 3e11;
nu = 0.3;

%% Loop over elements
for i = 1:n_elem
    % Get the nodes making up this element
    node1 = elements(i,1);
    node2 = elements(i,2);
    node3 = elements(i,3); 
    % Get the coordinates of the triangle
    x1 = node_XY(node1,1);
    y1 = node_XY(node1,2);
    x2 = node_XY(node2,1);
    y2 = node_XY(node2,2);
    x3 = node_XY(node3,1);
    y3 = node_XY(node3,2);
    

    % Build the element stiffness 
    Ae = Area(x1,y1,x2,y2,x3,y3);
    B = (1/(2*Ae))*[y2-y3,  0  ,y3-y1,  0  ,y1-y2,   0  ;
                         0  ,x3-x2,  0  ,x1-x3,  0  ,x2-x1;
                       x3-x2,y2-y3,x1-x3,y3-y1,x2-x1,y1-y2];
    D = E/(1-nu^2)*[1,nu,0; nu,1,0; 0,0,(1-nu)/2];
    Dstar = [1,nu,0; nu,1,0; 0,0,(1-nu)/2];
    K = Ae*B'*D*B;
    Kstar = Ae*B'*Dstar*B;
    
    fprintf('B')
    disp(B)
    fprintf('K without multiplying by (E/(1-nu^2))')
    disp(Kstar*2)
end % of the for loop

% Only two equations are needed, those corresponding to u2x and u3y 
% equation for u2x
disp(Kstar(3,:))
% equation for u3y
disp(Kstar(end,:)) % -1 in python indicates end in matlab
% NOTE: you can tell from the problem statement that this should be a 
% symmetric solution, i.e. the displacement if u2x and u3y should
% be the same 
Ksmall = E/(1-nu^2)*[0.5,0.15;0.15,0.5];
% The force vector is the integral of the traction over the boundary, 
% note here that the traction is constant, so you don't really need to do 
% the integral, just multiply by le/2 where le is the length of the edge 
force = [45/2,45/2];
fprintf('Displacements')
d = Ksmall'\force';
disp(d);


%% Function Definitions have to go at the end of Matlab scripts
% Area of the triangular element
function A_e = Area(x1,y1,x2,y2,x3,y3)
    edge1 = [x2-x1,y2-y1,0.];
    edge2 = [x3-x1,y3-y1,0.];
    A_e = cross(edge1,edge2);
    A_e = norm(A_e)/2;
end












