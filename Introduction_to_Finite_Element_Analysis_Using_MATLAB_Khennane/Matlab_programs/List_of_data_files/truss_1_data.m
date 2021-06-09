%               File:    truss_1_data.m
%
% The following variables are declared as global in order
% to be used by all the functions (M-files) constituting 
% the program
%
global nnd nel nne nodof eldof n
global geom connec prop nf load
%
format short e
%
%%%%%%%%%%%%%% Beginning of data input %%%%%%%%%%%%%%%%
%
nnd = 3; % Number of nodes:
nel = 3; % Number of elements:
nne = 2 ; % Number of nodes per element:
nodof =2 ; % Number of degrees of freedom per node
eldof = nne*nodof; % Number of degrees of freedom 
                   % per element
%
% Nodes coordinates X and Y
geom=zeros(nnd,2);
geom = [0.      0.    ; ... % X and Y coord. node 1
        4000.   0.    ; ... % X and Y coord. node 2
        4000.   6000.];     % X and X coord. node 3
%
% Element connectivity
%
connec=zeros(nel,2);
connec = [1   2 ; ... % 1st and 2nd node of element 1
          2   3 ; ... % 1st and 2nd node of element 2
          1   3];     % 1st and 2nd node of element 3
%
% Geometrical properties
%
% prop(1,1) = E; prop(1,2)= A
%
prop=zeros(nel,2);
prop = [200000    2300; ...  % E and A of element 1
        200000    2300; ...  % E and A of element 2
        200000    2300];     % E and A of element 3
%
% Boundary conditions
%
nf = ones(nnd, nodof); % Initialise the matrix nf to 1
nf(1,1) = 0; nf(1,2) =0 ; % Prescribed nodal freedom of node 1
nf(2,2) = 0 ;             % Prescribed nodal freedom of node 3
%
% Counting of the free degrees of freedom
%
n=0;
for i=1:nnd
    for j=1:nodof
        if nf(i,j) ~= 0
        n=n+1;
        nf(i,j)=n;
        end
    end
end
%
% loading
%
load = zeros(nnd, 2);
load(3,:)=[1200. 0]; %forces in X and Y directions at node 3
%
%%%%%%%%%%%%%%%%%%%%%%% End of input %%%%%%%%%%%%%%%%%%%%%%