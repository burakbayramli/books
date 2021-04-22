%               File:    truss_problem_1_data.m
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
nnd = 9; % Number of nodes:
nel = 15; % Number of elements:
nne = 2 ; % Number of nodes per element:
nodof =2 ; % Number of degrees of freedom per node
eldof = nne*nodof; % Number of degrees of freedom 
                   % per element
%
% Nodes coordinates X and Y
geom=zeros(nnd,2);
geom = [0.      0.; ...   % X and Y coord. node 1
        1.      2.; ...   % X and Y coord. node 2
        2.      0.; ...   % X and Y coord. node 3
        3.      2.; ...   % X and Y coord. node 4
        4.      0.; ...   % X and Y coord. node 5
        5.      2.; ...   % X and Y coord. node 6
        6.      0.; ...   % X and Y coord. node 7
        7.      2.; ...   % X and Y coord. node 8
        8.      0.] ;     % X and Y coord. node 9

%
% Element connectivity
%
connec=zeros(nel,2);
connec = [1    2 ; ...    % 1st and 2nd node of element 1
          1    3 ; ...    % 1st and 2nd node of element 2
          2    3 ; ...    % 1st and 2nd node of element 3
          2    4 ; ...    % 1st and 2nd node of element 4
          3    4 ; ...    % 1st and 2nd node of element 5
          3    5 ; ...    % 1st and 2nd node of element 6
          4    5 ; ...    % 1st and 2nd node of element 7
          4    6 ; ...    % 1st and 2nd node of element 8
          5    6 ; ...    % 1st and 2nd node of element 9
          5    7 ; ...    % 1st and 2nd node of element 10
          6    7 ; ...    % 1st and 2nd node of element 11
          6    8 ; ...    % 1st and 2nd node of element 12
          7    8 ; ...    % 1st and 2nd node of element 13
          7    9 ; ...    % 1st and 2nd node of element 14
          8    9 ]  ;     % 1st and 2nd node of element 15
%
% Geometrical properties
%
% prop(1,1) = E; prop(1,2)= A
%
prop=zeros(nel,2);
prop  = [30.e6       0.02  ; ...   % E and A of element 1
         30.e6       0.045 ; ...   % E and A of element 2
         30.e6       0.02  ; ...   % E and A of element 3
         30.e6       0.045 ; ...   % E and A of element 4
         30.e6       0.02  ; ...   % E and A of element 5
         30.e6       0.045 ; ...   % E and A of element 6
         30.e6       0.02  ; ...   % E and A of element 7
         30.e6       0.045 ; ...   % E and A of element 8
         30.e6       0.02  ; ...   % E and A of element 9
         30.e6       0.045 ; ...   % E and A of element 10
         30.e6       0.02  ; ...   % E and A of element 11
         30.e6       0.045 ; ...   % E and A of element 12
         30.e6       0.02  ; ...   % E and A of element 13
         30.e6       0.045 ; ...   % E and A of element 14
         30.e6       0.02  ];      % E and A of element 15
%
% Boundary conditions
%
nf = ones(nnd, nodof); % Initialise the matrix nf to 1
nf(1,1) = 0; nf(1,2) =0 ; % Prescribed nodal freedom of node 1
nf(9,2)= 0 ;              % Prescribed nodal freedom of node 3
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
load(2,:)=[15.   0.];  %forces in X and Y directions at node 2
load(3,:)=[0.   -5.];  %forces in X and Y directions at node 3
load(4,:)=[0.   -7.];  %forces in X and Y directions at node 4
load(7,:)=[0.   -10.]; %forces in X and Y directions at node 7

%
%%%%%%%%%%%%%%%%%%%%%%% End of input %%%%%%%%%%%%%%%%%%%%%%