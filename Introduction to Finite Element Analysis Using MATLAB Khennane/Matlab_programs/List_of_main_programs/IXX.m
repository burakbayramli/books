% Evaluation of the second moment of area of a geometrical domain 
% Using finite element approximation (8 Nodes isoparametric element elements).
%
clc
clear
format long e
%
global geom connec nel nne nnd RI RE
%
RI = 40;   % Internal radius
RE = 70;   % External radius
%
Eight_Q8        % Load input data
%
%  Number of Gauss points
%
ngp = 3           % The polynomials involved are of degree 5
%
samp = gauss(ngp)  % Gauss abscissae and weights 
%
%
Ixx = 0.; % Initiliase the second moment of area to zero
%
for k=1:nel
    coord = coord_q8(k,nne, geom, connec);   % Retrieve the coordinates of 
                                             % the nodes of element k
    X = coord(:,1);                          % X coordinates of element k
    Y = coord(:,2)                           % Y coordinates of element k
    for i=1:ngp
        xi = samp(i,1);
        WI = samp(i,2);
        for j =1:ngp
        eta = samp(j,1);
        WJ = samp(j,2);
        [der,fun] = fmquad(samp, i,j);  % Form the vector of the shape functions
                                         % and the matrix of their derivatives
        JAC = der*coord;             % Evaluate the Jacobian 
        DET =det(JAC)                % Evaluate determinant of Jacobian matrix                            
        Ixx =Ixx+ (dot(fun,Y))^2*WI*WJ*DET;
        end
    end
end
Ixx