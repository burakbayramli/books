%               File:    CST_COARSE_MESH_DATA.m
%
% The following variables are declared as global in order
% to be used by all the functions (M-files) constituting 
% the program
%
%
global nnd nel nne nodof eldof n 
global geom connec dee nf Nodal_loads
%
format short e
%
nnd = 21 ;               	% Number of nodes:  				
nel = 24 ;                	% Number of elements: 				
nne = 3 ;                	% Number of nodes per element:			
nodof =2;                	% Number of degrees of freedom per node	
eldof = nne*nodof;       	% Number of degrees of freedom per element 	
%
% Nodes coordinates x and y  
%
geom = zeros(nnd,3);
%
geom = [ 0,    -10    ;  ... % Node 1 
         0,    0      ;    ... % Node 2
         0,    10     ;   ... % Node 3  
         10,    -10   ;  ... % Node 4 
         10,    0     ;    ... % Node 5  
         10,    10    ;   ... % Node 6  
         20,    -10   ;  ... % Node 7  
         20,    0     ;    ... % Node 8  
         20,    10    ;   ... % Node 9  
         30,     -10  ;  ... % Node 10  
         30,     0    ;    ... % Node 11  
         30,     10   ;   ... % Node 12  
         40,    -10   ;  ... % Node 13  
         40,    0     ;    ... % Node 14  
         40,    10    ;   ... % Node 15  
         50,    -10   ;  ... % Node 16  
         50,    0     ;    ... % Node 17  
         50,    10    ;   ... % Node 18  
         60,    -10   ;  ... % Node 19  
         60,    0     ;    ... % Node 20  
         60,    10    ];      % Node 21    
%
% Element connectivity
%
connec=zeros(nel,3);
connec = [ 1,   4,  2;  ...% Element 1
           4,   5,  2;  ...% Element 2
           2,   5,  3;  ...% Element 3
           5,   6,  3;  ...% Element 4
           4,   7,  5;  ...% Element 5
           7,   8,  5;  ...% Element 6
           5,   8,  6;  ...% Element 7
           8,   9,  6;  ...% Element 8
           7,   10, 8;  ...% Element 9
           10,  11, 8;  ...% Element 10
           8,   11, 9;  ...% Element 11
           11,  12, 9;  ...% Element 12
           10,  13, 11; ...% Element 13
           13,  14, 11; ...% Element 14
           11,  14, 12; ...% Element 15
           14,  15, 12; ...% Element 16
           13,  16, 14; ...% Element 17
           16,  17, 14; ...% Element 18
           14,  17, 15; ...% Element 19
           17,  18, 15; ...% Element 20
           16,  19, 17; ...% Element 21
           19,  20, 17; ...% Element 22
           17,  20, 18; ...% Element 23
           20,  21, 18];   % Element 24          
%
% Material 
%
E = 200000.;     % Elastic modulus in MPa
vu = 0.3;       % Poisson's ratio 
thick = 5.;      % Beam thickness in mm
%
% Form the elastic matrix for plane stress 
%
dee = formdsig(E,vu);
%
% Boundary conditions
%
nf = ones(nnd, nodof);    % Initialise the matrix nf to 1
nf(19,1) = 0; nf(19,2) = 0;  % Prescribed nodal freedom of node 19
nf(20,1) = 0; nf(20,2) = 0;  % Prescribed nodal freedom of node 20 
nf(21,1) = 0; nf(21,2) = 0;  % Prescribed nodal freedom of node 21 
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
Nodal_loads= zeros(nnd, 2);
%
Nodal_loads(2,1) = 0.; Nodal_loads(2,2) = -1000.;    % Node 2
%
%%%%%%%%%%%%   End of input            %%%%%%%%%%%% 