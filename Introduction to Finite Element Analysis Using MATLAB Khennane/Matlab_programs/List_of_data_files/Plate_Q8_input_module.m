%
%%%%%%%%%%%%%%%%%%%%%%%%%% Beginning of data input %%%%%%%%%%%%%%%%%%%%%%% 
%
global nnd nel nne nodof eldof  n ngpb ngps 
global geom connec deeb dees nf  load dim

%
dim=2;                     % Dimension
nnd = 21 ;                % Number of nodes:  				
nel = 4;                  % Number of elements: 				
nne = 8 ;                % Number of nodes per element:			
nodof =3;                % Number of degrees of freedom per node
ngpb = 3;                  % number of Gauss points bending
ngps = 2;                 % number of Gauss points shear
eldof = nne*nodof;       % Number of degrees of freedom per element 	
%
% Thickness of the domain
thick = 0.25;
%
% Nodes coordinates x and y  
geom = [0.0    18; ...        %   x and y coordinates of node 1
       0.0     13.5; ...      %   x and y coordinates of node 2
       0.0     9; ...         %   x and y coordinates of node 3
       0.0     4.5; ...       %   x and y coordinates of node 4
       0.0     0; ...         %   x and y coordinates of node 5
       4.5     18; ...        %   x and y coordinates of node 6
       4.5     9.; ...        %   x and y coordinates of node 7
       4.5     0; ...         %   x and y coordinates of node 8
       9       18; ...        %   x and y coordinates of node 9
       9       13.5; ...      %   x and y coordinates of node 10
       9        9; ...        %   x and y coordinates of node 11
       9       4.5; ...       %   x and y coordinates of node 12
       9        0.; ...       %   x and y coordinates of node 13
       13.5     18; ...       %   x and y coordinates of node 14
       13.5     9; ...        %   x and y coordinates of node 15
       13.5     0.; ...       %   x and y coordinates of node 16       
       18       18; ...       %   x and y coordinates of node 17
       18       13.5; ...     %   x and y coordinates of node 18
       18       9; ...        %   x and y coordinates of node 19
       18       4.5; ...      %   x and y coordinates of node 20
       18       0.];          %   x and y coordinates of node 21 
%
disp ('Nodes X-Y coordinates')
geom
%
% Element connectivity
connec= [  1    2   3     7    11   10    9    6;...   % Element 1  
           3    4   5     8    13   12   11    7;...   % Element 2  
           9   10   11    15   19   18   17   14;...   % Element 3 
          11   12   13    16   21   20   19   15];   % Element 4
   
disp ('Elements connectivity')
connec
%
% Material properties
%
E=30.e+6; vu=0.3;          % Young's modulus and Poisson's ration
%
% Form the matrix of elastic properties
%
deeb=formdeeb(E,vu,thick); % Matrix of elastic properties for plate bending
dees=formdees(E,vu,thick); % Matrix of elastic properties for plate shear
%
% Boundary conditions
%
nf = ones(nnd, nodof);              % Initialise the matrix nf to 1
nf(1,1) = 0; nf(1,3)=0; 
nf(2,1) = 0; nf(2,3)=0; 
nf(3,1) = 0; nf(3,3)=0; 
nf(4,1) = 0; nf(4,3)=0;
nf(5,1) = 0; nf(5,2)=0; nf(5,3)=0;
nf(6,3)=0;
nf(8,1)=0; nf(8,2)=0;
nf(9,3)=0; 
nf(13,1)=0; nf(13,2)=0;
nf(14,3)=0
nf(16,1)=0; nf(16,2)=0;
nf(17,2)=0;nf(17,3)=0;
nf(18,2)=0;
nf(19,2)=0;
nf(20,2)=0;
nf(21,1)=0;nf(21,2)=0;
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
disp ('Nodal freedom')
nf
disp ('Total number of active degrees of freedom')
n
%
% loading
%
load = zeros(nnd, 3);
load(17,1) = 1000/4;         % Vertical load of 250 lb on node 17      
%
% End input