%  File:    Q4_COARSE_MESH_DATA
%
global nnd nel nne nodof eldof n ngp
global geom connec dee nf Nodal_loads
%
% To change the size of the mesh, alter the next statements
%    
nnd = 21 ;                % Number of nodes:  				
nel = 12;                  % Number of elements: 				
nne = 4 ;                % Number of nodes per element:			
nodof =2;                % Number of degrees of freedom per node
ngp = 2                  % number of Gauss points
eldof = nne*nodof;       % Number of degrees of freedom per element 	
%
%
% Nodes coordinates x and y  
geom = [0,    -10.0; ...          %   x and y coordinates of node 1
        0.0     0.0; ...           %   x and y coordinates of node 2
        0.0    10.0; ...         %   x and y coordinates of node 3
       10.0   -10.0; ...          %   x and y coordinates of node 4
       10.0     0.0; ...         %   x and y coordinates of node 5
       10.0    10.0; ...         %   x and y coordinates of node 6
       20.0   -10.0; ...         %   x and y coordinates of node 7
       20.0     0.0; ...         %   x and y coordinates of node 8
       20.0    10.0; ...         %   x and y coordinates of node 9
       30.0   -10.0; ...         %   x and y coordinates of node 10
       30.0     0.0; ...         %   x and y coordinates of node 11
       30.0    10.0; ...        %   x and y coordinates of node 12
       40.0   -10.0; ...         %   x and y coordinates of node 13
       40.0     0.0; ...         %   x and y coordinates of node 14
       40.0    10.0; ...         %   x and y coordinates of node 15
       50.0   -10.0; ...         %   x and y coordinates of node 16
       50.0     0.0; ...         %   x and y coordinates of node 17
       50.0    10.0; ...         %   x and y coordinates of node 18
       60.0   -10.0; ...         %   x and y coordinates of node 19
       60.0     0.0; ...         %   x and y coordinates of node 20
       60.0    10.0];            %   x and y coordinates of node 21
%       
%      
%
disp ('Nodes X-Y coordinates')
geom
%
% Element connectivity
connec= [ 1   4    5    2 ;...   % Element 1  
          2   5    6    3 ;...   % Element 2  
          4   7    8    5 ;...   % Element 3 
          5   8    9    6 ;...   % Element 4
          7  10   11    8 ;...   % Element 5  
          8  11   12    9 ;...   % Element 6
         10  13   14   11 ;...   % Element 7
         11  14   15   12 ;...   % Element 8
         13  16   17   14 ;...   % Element 9
         14  17   18   15 ;...   % Element 10
         16  19   20   17 ;...   % Element 11
         17  20   21   18];      % Element 12
%   
%  
disp ('Elements connectivity')
connec
%
E = 200000.;     % Elastic modulus in MPa
vu = 0.3;       % Poisson's ratio 
thick = 5.;      % Beam thickness in mm
%
% Form the elastic matrix for plane stress 
%
dee = formdsig(E,vu);
%
%
% Boundary conditions
%
nf = ones(nnd, nodof);    % Initialise the matrix nf to 1
nf(19,:) = [0   0];       % Node 19 is restrained in the x and y directions
nf(20,:) = [0   0];       % Node 20 is restrained in the x and y directions
nf(21,:) = [0   0];       % Node 21 is restrained in the x and y directions

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
Nodal_loads= zeros(nnd, 2);   % Initialise the matrix of odal loads to 0
%
% Apply a concentrated at the node having x = 0, and y = 0.
%
Force = 1000.;  % N
%
Nodal_loads(1,:) = [0.  -Force]; 